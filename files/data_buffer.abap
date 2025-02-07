REPORT zparallel_material_processing.

"! Material processing class for parallel data retrieval and manipulation
CLASS material DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      "! Structure for material data
      BEGIN OF ty_data,
        material  TYPE bapimathead-material,
        matl_desc TYPE bapi_makt-matl_desc,
      END OF ty_data,
      "! Table type for material data
      tab_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY.

    "! Constructor for material data
    "! parameter im_material Material number
    "! parameter im_desc Material description
    METHODS constructor
      IMPORTING im_material TYPE matnr
                im_desc     TYPE makt-maktx.

    "! Retrieve list of material numbers
    "! returning List of material numbers
    "!
    "! parameter result
    CLASS-METHODS list
      RETURNING VALUE(result) TYPE table_matnr.

    "! Get material details
    "! parameter im_material Material number
    "! returning Material details
    "! parameter result
    CLASS-METHODS get
      IMPORTING im_material   TYPE matnr
      RETURNING VALUE(result) TYPE bapimatdoa.

    "! Change material data
    "! returning Operation result
    "!
    "! parameter result
    METHODS change
      RETURNING VALUE(result) TYPE bapiret2.

  PRIVATE SECTION.
    DATA:
      gs_data        TYPE ty_data,
      gs_header      TYPE bapimathead,
      gt_description TYPE tt_bapi_makt.

    "! Prepare data for BAPI processing
    METHODS fill.

    "! Execute material save BAPI
    "! returning Operation result
    "!
    "! parameter result
    METHODS bapi
      RETURNING VALUE(result) TYPE bapiret2.

ENDCLASS.

CLASS material IMPLEMENTATION.

  METHOD constructor.
    gs_data = VALUE #( material  = im_material
                       matl_desc = im_desc ).
  ENDMETHOD.

  METHOD list.
    SELECT FROM mara
      FIELDS matnr
      INTO TABLE @result
      UP TO 30000 ROWS.
  ENDMETHOD.

  METHOD get.
    DATA:
      material_general_data TYPE bapimatdoa,
      return                TYPE bapireturn.

    CHECK im_material IS NOT INITIAL.

    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
      EXPORTING material              = im_material
      IMPORTING material_general_data = material_general_data
                return                = return.

    IF return-type = if_xo_const_message=>error.
      RETURN.
    ENDIF.

    result = material_general_data.
  ENDMETHOD.

  METHOD change.
    fill( ).

    IF
          gs_header               IS INITIAL
       OR lines( gt_description ) <= 0.
      RETURN.
    ENDIF.

    result = bapi( ).
  ENDMETHOD.

  METHOD fill.
    gs_header = VALUE #( material = |{ gs_data-material ALPHA = OUT }| ).

    gt_description = VALUE #( ( langu     = sy-langu
                                matl_desc = gs_data-matl_desc ) ).
  ENDMETHOD.

  METHOD bapi.
    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING headdata            = gs_header
      IMPORTING return              = result
      TABLES    materialdescription = gt_description.
  ENDMETHOD.

ENDCLASS.

"! Main processing class for parallel task execution
CLASS main DEFINITION.

  PUBLIC SECTION.
    TYPES:
      "! Shared processing record
      BEGIN OF shared_record,
        process_mode TYPE c LENGTH 1,
      END OF shared_record,

      "! Material processing result
      BEGIN OF task_result_material,
        material TYPE mara-matnr,
        data     TYPE bapimatdoa,
      END OF task_result_material.

    "! Execute parallel processing
    CLASS-METHODS process.

ENDCLASS.

"! Parallel task processing class
CLASS single_task DEFINITION INHERITING FROM cl_abap_parallel.

  PUBLIC SECTION.
    METHODS do REDEFINITION.

ENDCLASS.

CLASS main IMPLEMENTATION.

  METHOD process.
    DATA:
      tasks_input            TYPE cl_abap_parallel=>t_in_tab,
      task_input_single      TYPE xstring,
      tasks_input_shared     TYPE xstring,
      shared_record          TYPE shared_record,
      single_record_material TYPE mara-matnr,
      task_result_material   TYPE task_result_material.

    " Set processing mode
    shared_record-process_mode = '2'.

    " Store shared processing mode
    EXPORT buffer_task_shared = shared_record
      TO DATA BUFFER tasks_input_shared.

    " Prepare tasks for parallel processing
    LOOP AT material=>list( ) INTO single_record_material.
      EXPORT buffer_task = single_record_material
        TO DATA BUFFER task_input_single.
      INSERT task_input_single INTO TABLE tasks_input.
    ENDLOOP.

    " Create parallel processing instance
    DATA(parallel) = NEW single_task( p_percentage = 30 ).

    " Execute parallel tasks
    parallel->run( EXPORTING p_in_tab  = tasks_input
                             p_in_all  = tasks_input_shared
                   IMPORTING p_out_tab = DATA(tasks_output) ).

    " Process task results
    LOOP AT tasks_output ASSIGNING FIELD-SYMBOL(<task_output_single>).
      IF <task_output_single>-result IS INITIAL.
        CONTINUE.
      ENDIF.

      IMPORT buffer_result = task_result_material
        FROM DATA BUFFER <task_output_single>-result.

      WRITE: /
        sy-tabix,
        '|',
        task_result_material-material,
        '|',
        task_result_material-data-matl_desc.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS single_task IMPLEMENTATION.

  METHOD do.
    DATA:
      shared_record          TYPE main=>shared_record,
      single_record_material TYPE mara-matnr,
      task_result_material   TYPE main=>task_result_material.

    " Import shared and task-specific data
    IMPORT buffer_task_shared = shared_record
      FROM DATA BUFFER p_in_all.

    IMPORT buffer_task = single_record_material
      FROM DATA BUFFER p_in.

    " Process based on mode
    CASE shared_record-process_mode.
      WHEN '2'.
        task_result_material-material = single_record_material.
        task_result_material-data     = material=>get( im_material = single_record_material ).
    ENDCASE.

    " Export task result
    EXPORT buffer_result = task_result_material
      TO DATA BUFFER p_out.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  main=>process( ).
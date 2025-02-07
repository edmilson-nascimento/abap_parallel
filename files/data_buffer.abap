REPORT parallel.

CLASS material DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_data,
        material  TYPE bapimathead-material,
        matl_desc TYPE bapi_makt-matl_desc,
      END OF ty_data,
      tab_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING im_material TYPE matnr
                im_desc     TYPE makt-maktx.

    CLASS-METHODS list
      RETURNING VALUE(result) TYPE table_matnr.

    CLASS-METHODS get
      IMPORTING im_material   TYPE matnr
      RETURNING VALUE(result) TYPE bapimatdoa.

    METHODS change
      RETURNING VALUE(result) TYPE bapiret2.

  PRIVATE SECTION.

    DATA:
      gs_data        TYPE ty_data,
      gs_header      TYPE bapimathead,
      gt_description TYPE tt_bapi_makt.

    METHODS fill.

    METHODS bapi
      RETURNING VALUE(result) TYPE bapiret2.

ENDCLASS.

CLASS material IMPLEMENTATION.

  METHOD constructor.

    me->gs_data = VALUE #( material  = im_material
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

    IF im_material IS INITIAL.
      RETURN.
    ENDIF.

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

    me->fill( ).

    IF    me->gs_header               IS INITIAL
       OR lines( me->gt_description )  = 0.
      RETURN.
    ENDIF.

    result = me->bapi( ).

  ENDMETHOD.


  METHOD fill.

    me->gs_header      = VALUE #( material = |{ me->gs_data-material ALPHA = OUT }| ).
    me->gt_description = VALUE #( ( langu     = sy-langu
                                    matl_desc = me->gs_data-matl_desc ) ).

  ENDMETHOD.

  METHOD bapi.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING headdata            = me->gs_header
      IMPORTING return              = result
      TABLES    materialdescription = me->gt_description.

  ENDMETHOD.

ENDCLASS.


CLASS main DEFINITION.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF shared_record,
        process_mode TYPE c LENGTH 1,
      END OF shared_record,

      single_record TYPE scarr-carrid,

      BEGIN OF task_result,
        carrid TYPE scarr-carrid,
        count  TYPE i,
      END OF task_result,

      single_record_material TYPE mara-matnr,

      BEGIN OF task_result_material,
        material TYPE mara-matnr,
        data     TYPE bapimatdoa,
      END OF task_result_material.

    CLASS-METHODS process.

ENDCLASS.

CLASS single_task DEFINITION INHERITING FROM cl_abap_parallel.

  PUBLIC SECTION.

    METHODS do REDEFINITION.

ENDCLASS.


CLASS main IMPLEMENTATION.

  METHOD process.

    DATA: tasks_input            TYPE cl_abap_parallel=>t_in_tab,
          task_input_single      TYPE xstring,
*          single_record          TYPE single_record,
          tasks_input_shared     TYPE xstring,
          shared_record          TYPE shared_record,
*          task_result            TYPE task_result,

          single_record_material TYPE single_record_material,
          task_result_material   TYPE task_result_material.

    shared_record-process_mode = 1.
    shared_record-process_mode = 2.

    " Since the process_mode value is shared across all tasks, store
    " it in the shared variable (tasks_input_shared) instead of
    " repeating it for every task input record (tasks_input)

*   To be imported by SINGLE_TASK->DO
*    EXPORT buffer_task_shared = shared_record
*      TO DATA BUFFER tasks_input_shared.
    EXPORT buffer_task_shared = shared_record
           TO DATA BUFFER tasks_input_shared.

*   Get data to be processed.  EXPORT each record and collect them all into
*   table tasks_input.
*    SELECT
*     carrid
*    FROM scarr
*    INTO @single_record.
*
**     To be imported by SINGLE_TASK->DO
*      EXPORT buffer_task = single_record TO DATA BUFFER task_input_single.
*      INSERT task_input_single INTO TABLE tasks_input.
*
*    ENDSELECT.

    " To be imported by SINGLE_TASK->DO
    LOOP AT material=>list( ) INTO single_record_material.
      EXPORT buffer_task = single_record_material TO DATA BUFFER task_input_single.
      INSERT task_input_single INTO TABLE tasks_input.
    ENDLOOP.

    " Create the instance while configuring the resource usage
    DATA(parallel) = NEW single_task( p_num_tasks = 12
*                                      p_timeout   = 200
*                                      p_percentage = 50
*                                      p_num_processes = 20
*                                      p_local_server =
                         ).

    DATA(debug) = ' '.

    " Perform the tasks in parallel
    parallel->run( EXPORTING p_in_tab  = tasks_input
                             p_in_all  = tasks_input_shared
                             p_debug   = debug
                   IMPORTING p_out_tab = DATA(tasks_output) ).

    LOOP AT tasks_output ASSIGNING FIELD-SYMBOL(<task_output_single>).

      " Something went wrong, like a timeout, if the message has a value
      IF <task_output_single>-message IS NOT INITIAL.
        WRITE: / 'Task Error:', <task_output_single>-message.
      ENDIF.

      IF <task_output_single>-result IS NOT INITIAL.
*       Exported from SINGLE_TASK->DO
*        IMPORT buffer_result = task_result
*          FROM DATA BUFFER <task_output_single>-result.

        IMPORT buffer_result = task_result_material
               FROM DATA BUFFER <task_output_single>-result.

        WRITE: / sy-tabix, '-', task_result_material-material, task_result_material-data-matl_desc.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

CLASS single_task IMPLEMENTATION.

  METHOD do.

    " I referenced the MAIN local class instead of using the data dictionary
    " for the ease for copy/paste
    DATA:
      shared_record          TYPE main=>shared_record,
      single_record          TYPE main=>single_record,
      " TODO: variable is assigned but only used in commented-out code (ABAP cleaner)
      task_result            TYPE main=>task_result,

      single_record_material TYPE main=>single_record_material,
      task_result_material   TYPE main=>task_result_material.

    " Exported by MAIN->PROCCESS
    IMPORT buffer_task_shared = shared_record FROM DATA BUFFER p_in_all.

*   Exported by MAIN->PROCCESS
*   IMPORT buffer_task = single_record FROM DATA BUFFER p_in.
    IMPORT buffer_task = single_record_material FROM DATA BUFFER p_in.

    " Other shared values I like to include are a simulation/update flag
    " and a level for the logging detail
    CASE shared_record-process_mode.

      WHEN '1'.

*        DO 40 TIMES.
*          WAIT UP TO 1 SECONDS.
*        ENDDO .

        " Incredibly simplistic example which doesn't warrant parallel processing
        SELECT carrid,
               COUNT( * )
          FROM sflight
          WHERE carrid = @single_record
          GROUP BY carrid
          INTO ( @task_result-carrid, @task_result-count ).

        ENDSELECT.

      WHEN '2'.

        task_result_material-material = single_record_material.
        task_result_material-data     = material=>get( im_material = single_record_material ).

    ENDCASE.

*   To be imported by MAIN->PROCESS
*   EXPORT buffer_result = task_result TO DATA BUFFER p_out.
    EXPORT buffer_result = task_result_material TO DATA BUFFER p_out.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.

  " This is the only statement outside of the classes
  main=>process( ).

*  DATA(list) = material=>list( ).
*  LOOP AT list INTO DATA(material).
*    DATA(data) = material=>get( im_material = material ) .
*    WRITE:/ 'Descr', data-matl_desc, 'hora ', sy-uzeit.
*  ENDLOOP.
REPORT x.

CLASS lcl_material DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_data,
        material  TYPE bapimathead-material,
        matl_desc TYPE bapi_makt-matl_desc,
      END OF ty_data,
      tab_data TYPE STANDARD TABLE OF ty_data WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING im_material TYPE matnr
                im_desc     TYPE makt-maktx.

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

CLASS lcl_material IMPLEMENTATION.

  METHOD constructor.

    me->gs_data = VALUE #( material  = im_material
                           matl_desc = im_desc ).
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



INITIALIZATION .

BREAK-POINT .

  SELECT FROM makt
    FIELDS matnr, maktx
    WHERE spras = @sy-langu
    INTO TABLE @DATA(lt_data)
    UP TO 10 ROWS.

  LOOP AT lt_data INTO DATA(ls_line).

    DATA(ls_messasge) = NEW lcl_material( im_material = ls_line-matnr
                                          im_desc     = ls_line-maktx
    )->change( ).

    WRITE / ls_messasge-message.

  ENDLOOP.


CLASS lcl_process_data DEFINITION.

  PUBLIC SECTION.

    METHODS constructor .

    METHODS execute .

  PRIVATE SECTION.

    METHODS run_parallel
      IMPORTING processes     TYPE cl_abap_parallel=>t_in_inst_tab
      RETURNING VALUE(result) TYPE tty_result.

    METHODS activate_execution
      RETURNING VALUE(lock_record) TYPE ztc4_executions
      RAISING   lcx_calc_aging_equi.

    METHODS start_log
      IMPORTING nr_records  TYPE i
                lock_record TYPE ztc4_executions.

    METHODS build_process
      IMPORTING it_data type lcl_material=>tab_data
      RETURNING VALUE(rt_processes) TYPE bapiret2_t.

    METHODS send_idoc_bi
      IMPORTING equip_result  TYPE lcl_process_data=>tty_result
      RETURNING VALUE(result) TYPE zclc4_equip_idoc=>tty_send_return.

    METHODS disable_execution
      IMPORTING lock_record TYPE ztc4_executions.

    METHODS save_log
      IMPORTING lock_record TYPE ztc4_executions.

    METHODS add_log
      IMPORTING equi_result   TYPE lcl_process_data=>tty_result
                idoc_result   TYPE zclc4_equip_idoc=>tty_send_return
      RETURNING VALUE(result) TYPE lcl_process_data=>tty_result.
    METHODS display_popup
      IMPORTING
        lines         TYPE i
      RAISING   lcx_calc_aging_equi.

ENDCLASS.


CLASS lcl_process_parallel DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_process_data.
  PUBLIC SECTION.
    INTERFACES if_abap_parallel.

    METHODS constructor
      IMPORTING iv_equipment_data TYPE zcdsc4_equipment_aging.

  PRIVATE SECTION.
    METHODS get_result
      RETURNING VALUE(rt_result) TYPE lcl_process_data=>tty_result.

    DATA equipment_data TYPE zcdsc4_equipment_aging.
    DATA mt_result      TYPE lcl_process_data=>tty_result.
ENDCLASS.


CLASS lcl_process_parallel DEFINITION FINAL CREATE PRIVATE FRIENDS lcl_process_data.
  PUBLIC SECTION.
    INTERFACES if_abap_parallel.

    METHODS constructor
      IMPORTING iv_equipment_data TYPE zcdsc4_equipment_aging.

  PRIVATE SECTION.
    METHODS get_result
      RETURNING VALUE(rt_result) TYPE lcl_process_data=>tty_result.

    DATA equipment_data TYPE zcdsc4_equipment_aging.
    DATA mt_result      TYPE lcl_process_data=>tty_result.
ENDCLASS.



CLASS lcl_process_data IMPLEMENTATION.

  METHOD constructor.
  ENDMETHOD.

  METHOD execute.

    DATA(lt_result) = run_parallel( build_process( lt_equip_data ) ).

    result = add_log( equi_result = lt_result
                      idoc_result = send_idoc_bi( lt_result ) ).

  ENDMETHOD.

  METHOD save_log.
  ENDMETHOD.

  METHOD disable_execution.
  ENDMETHOD.

  METHOD build_process.
    rt_processes = VALUE cl_abap_parallel=>t_in_inst_tab( 
    FOR ls_line IN it_data
    ( NEW lcl_material( im_material = ls_line-material
                        im_desc     = ls_line-matl_desc )->change( ) ) ).
  ENDMETHOD.

  METHOD activate_execution.
  ENDMETHOD.

  METHOD run_parallel.
    DATA(lv_debug) = abap_false.
    NEW cl_abap_parallel( p_percentage = 30 )->run_inst( EXPORTING p_in_tab  = processes
                                                                   p_debug   = lv_debug
                                                         IMPORTING p_out_tab = DATA(lt_finished) ).
    LOOP AT lt_finished INTO DATA(ls_finished).
      INSERT LINES OF CAST lcl_process_parallel( ls_finished-inst )->get_result( ) INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.

  METHOD start_log.
  ENDMETHOD.

  METHOD send_idoc_bi.
  ENDMETHOD.

  METHOD add_log.
  ENDMETHOD.

  METHOD display_popup.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_process_parallel IMPLEMENTATION.
  METHOD if_abap_parallel~do.
    INSERT NEW lcl_update_equipment( )->update_equipment( equipment_data ) INTO TABLE mt_result.
  ENDMETHOD.

  METHOD constructor.
    equipment_data = iv_equipment_data.
  ENDMETHOD.

  METHOD get_result.
    rt_result = mt_result.
  ENDMETHOD.
ENDCLASS.




*
*&---------------------------------------------------------------------*
*& Report ZMATERIAL_UPDATE_PARALLEL
*&---------------------------------------------------------------------*
REPORT zmaterial_update_parallel.

*----------------------------------------------------------------------*
* FORWARD DECLARATIONS
*----------------------------------------------------------------------*
CLASS lcl_worker DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* CLASS lcl_material_processor - Orquestrador + Seleção
*----------------------------------------------------------------------*
CLASS lcl_material_processor DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_material_data,
             matnr TYPE mara-matnr,
             werks TYPE marc-werks,
             mtart TYPE mara-mtart,
           END OF ty_material_data.
    TYPES tty_material_data TYPE STANDARD TABLE OF ty_material_data WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_result,
             matnr         TYPE mara-matnr,
             werks         TYPE marc-werks,
             update_status TYPE icon-id,
             message       TYPE bapi_msg,
           END OF ty_result.
    TYPES tty_result     TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

    TYPES range_material TYPE RANGE OF mara-matnr.

    CLASS-METHODS execute
      IMPORTING ir_matnr      TYPE range_material
                ir_werks      TYPE range_t_werks
                iv_mtart      TYPE mara-mtart OPTIONAL
      RETURNING VALUE(result) TYPE tty_result.

  PRIVATE SECTION.
    CLASS-METHODS select_data
      IMPORTING ir_matnr      TYPE range_material
                ir_werks      TYPE range_t_werks
      RETURNING VALUE(result) TYPE tty_material_data.

    CLASS-METHODS build_processes
      IMPORTING it_data       TYPE tty_material_data
      RETURNING VALUE(result) TYPE cl_abap_parallel=>t_in_inst_tab.

    CLASS-METHODS run_parallel
      IMPORTING it_processes  TYPE cl_abap_parallel=>t_in_inst_tab
      RETURNING VALUE(result) TYPE tty_result.

ENDCLASS.


*----------------------------------------------------------------------*
* CLASS lcl_worker - Worker Paralelo + Lógica de Negócio
*----------------------------------------------------------------------*
CLASS lcl_worker DEFINITION FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_abap_parallel.

    METHODS constructor
      IMPORTING is_data TYPE lcl_material_processor=>ty_material_data.

    METHODS get_result
      RETURNING VALUE(result) TYPE lcl_material_processor=>tty_result.

  PRIVATE SECTION.
    DATA ms_data   TYPE lcl_material_processor=>ty_material_data.
    DATA mt_result TYPE lcl_material_processor=>tty_result.

    METHODS update_material
      RETURNING VALUE(result) TYPE lcl_material_processor=>ty_result.

    METHODS create_message
      IMPORTING iv_matnr      TYPE mara-matnr
                iv_werks      TYPE marc-werks
      RETURNING VALUE(result) TYPE sap_bool.

ENDCLASS.


*----------------------------------------------------------------------*
* IMPLEMENTATIONS
*----------------------------------------------------------------------*
CLASS lcl_material_processor IMPLEMENTATION.
  METHOD execute.
    " 1. Seleciona dados
    DATA(lt_data) = select_data( ir_matnr = ir_matnr
                                 ir_werks = ir_werks ).

    IF lt_data IS INITIAL.
      RETURN.
    ENDIF.

    " 2. Constrói e executa processos paralelos
    result = run_parallel( build_processes( lt_data ) ).
  ENDMETHOD.

  METHOD select_data.
    " TODO: parameter IV_MTART is never used (ABAP cleaner)

    SELECT mara~matnr,
           marc~werks,
           mara~mtart
      FROM mara
             INNER JOIN
               marc ON marc~matnr = mara~matnr
      WHERE mara~matnr IN @ir_matnr
        AND marc~werks IN @ir_werks
      INTO TABLE @result
      UP TO 3000 ROWS.

  ENDMETHOD.

  METHOD build_processes.

    result =
      VALUE cl_abap_parallel=>t_in_inst_tab(
         FOR ls_data IN it_data
         ( CAST if_abap_parallel( NEW lcl_worker( is_data = ls_data ) ) ) ).

  ENDMETHOD.

  METHOD run_parallel.

    NEW cl_abap_parallel( p_percentage = 30 )->run_inst( EXPORTING p_in_tab  = it_processes
                                                                   p_debug   = abap_false
                                                         IMPORTING p_out_tab = DATA(lt_finished) ).

    " Coleta resultados
    LOOP AT lt_finished INTO DATA(ls_finished).
      DATA(lo_worker) = CAST lcl_worker( ls_finished-inst ).
      INSERT LINES OF lo_worker->get_result( ) INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_worker IMPLEMENTATION.

  METHOD constructor.
    ms_data = is_data.
  ENDMETHOD.

  METHOD if_abap_parallel~do.

    " Executa atualização e armazena resultado
    INSERT update_material( ) INTO TABLE mt_result.

  ENDMETHOD.

  METHOD get_result.
    result = mt_result.
  ENDMETHOD.

  METHOD update_material.

    result-matnr         = ms_data-matnr.
    result-werks         = ms_data-werks.
    result-update_status = icon_led_yellow.

    " Chama BAPI
    DATA(ls_return_ok) = create_message( iv_matnr = ms_data-matnr
                                         iv_werks = ms_data-werks ).

    " Define status
    IF ls_return_ok = abap_off.
      result-update_status = icon_led_red.
      result-message       = |Error message. { sy-uzeit time = iso  }|.
    ELSE.
      result-update_status = icon_led_green.
      result-message       = |Message ok. { sy-uzeit time = iso }|.
    ENDIF.

  ENDMETHOD.

  METHOD create_message.

    DATA(message_list) = cf_reca_message_list=>create( id_object    = 'BTC'
                                                       id_subobject = 'TEST'
                                                       id_extnumber = 'PARALLEL'
                                                       id_deldate   = CONV #( sy-datum + 1 ) ).
    message_list->add( id_msgty = 'I'
                       id_msgid = '>0'
                       id_msgno = '000'
                       id_msgv1 = |Material { iv_matnr } Plant { iv_werks }. { sy-uzeit }| ).

    message_list->store( EXCEPTIONS error  = 1
                                    OTHERS = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    result = abap_on.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = abap_true.

  ENDMETHOD.

ENDCLASS.

TABLES: marc, mara.

*----------------------------------------------------------------------*
* SELEÇÃO E EXECUÇÃO
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_matnr FOR mara-matnr,
                  s_werks FOR marc-werks.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  DATA(lt_result) = lcl_material_processor=>execute( ir_matnr = s_matnr[]
                                                     ir_werks = s_werks[] ).

  " Exibe resultados
  
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lo_alv)
                              CHANGING  t_table      = lt_result ).

      lo_alv->get_columns( )->set_optimize( abap_true ).
      lo_alv->get_functions( )->set_all( abap_true ).
      lo_alv->display( ).
    CATCH cx_salv_msg.

  ENDTRY.
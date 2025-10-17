*&---------------------------------------------------------------------*
*& Report ZPO_PROCESS_PARALLEL
*&---------------------------------------------------------------------*
REPORT zpo_process_parallel.

*----------------------------------------------------------------------*
* TABLES & DATA DECLARATIONS
*----------------------------------------------------------------------*
TABLES: snwd_po.

*----------------------------------------------------------------------*
* FORWARD DECLARATIONS
*----------------------------------------------------------------------*
CLASS lcl_po_worker DEFINITION DEFERRED.
CLASS lcl_main DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* CLASS lcl_po_processor
*----------------------------------------------------------------------*
CLASS lcl_po_processor DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF ty_po_header,
             node_key      TYPE snwd_po-node_key,
             po_id         TYPE snwd_po-po_id,
             partner_guid  TYPE snwd_po-partner_guid,
             currency_code TYPE snwd_po-currency_code,
             gross_amount  TYPE snwd_po-gross_amount,
             created_at    TYPE snwd_po-created_at,
           END OF ty_po_header.
    
    TYPES: BEGIN OF ty_po_item,
             node_key     TYPE snwd_po_i-node_key,
             parent_key   TYPE snwd_po_i-parent_key,
             po_item_pos  TYPE snwd_po_i-po_item_pos,
             product_guid TYPE snwd_po_i-product_guid,
             gross_amount TYPE snwd_po_i-gross_amount,
           END OF ty_po_item.
    
    TYPES: BEGIN OF ty_result,
             node_key       TYPE snwd_po-node_key,
             po_id          TYPE snwd_po-po_id,
             items_count    TYPE i,
             process_status TYPE icon-id,
             message        TYPE string,
           END OF ty_result.
    
    TYPES: tty_po_header TYPE STANDARD TABLE OF ty_po_header WITH DEFAULT KEY,
           tty_po_item   TYPE STANDARD TABLE OF ty_po_item WITH DEFAULT KEY,
           tty_result    TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.
    
    CLASS-METHODS execute
      IMPORTING it_selected   TYPE tty_po_header
      RETURNING VALUE(result) TYPE tty_result.
      
  PRIVATE SECTION.
    CLASS-METHODS build_processes
      IMPORTING it_selected   TYPE tty_po_header
      RETURNING VALUE(result) TYPE cl_abap_parallel=>t_in_inst_tab.
      
    CLASS-METHODS run_parallel
      IMPORTING it_processes  TYPE cl_abap_parallel=>t_in_inst_tab
      RETURNING VALUE(result) TYPE tty_result.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_po_worker - Worker parallel + business logic
*----------------------------------------------------------------------*
CLASS lcl_po_worker DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES if_abap_parallel.
    
    METHODS constructor
      IMPORTING is_po_header TYPE lcl_po_processor=>ty_po_header.
      
    METHODS get_result
      RETURNING VALUE(result) TYPE lcl_po_processor=>tty_result.
      
  PRIVATE SECTION.
    DATA ms_po_header TYPE lcl_po_processor=>ty_po_header.
    DATA mt_result    TYPE lcl_po_processor=>tty_result.
    
    METHODS process_po
      RETURNING VALUE(result) TYPE lcl_po_processor=>ty_result.
      
    METHODS get_po_items
      IMPORTING iv_parent_key TYPE snwd_po_i-parent_key
      RETURNING VALUE(result) TYPE lcl_po_processor=>tty_po_item.
      
    METHODS create_log_message
      IMPORTING iv_po_id      TYPE snwd_po-po_id
                iv_items      TYPE lcl_po_processor=>tty_po_item
      RETURNING VALUE(result) TYPE sap_bool.
ENDCLASS.

*----------------------------------------------------------------------*
* CLASS lcl_main - Controle da tela e ALV
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION.
  PUBLIC SECTION.
    " Types locais para ranges
    TYPES: ty_range_po_id   TYPE RANGE OF snwd_po-po_id,
           ty_range_currency TYPE RANGE OF snwd_po-currency_code.
    
    CLASS-METHODS start.
    
  PRIVATE SECTION.
    CLASS-DATA: go_alv       TYPE REF TO cl_salv_table,
                gt_po_list   TYPE lcl_po_processor=>tty_po_header,
                gt_results   TYPE lcl_po_processor=>tty_result.
    
    CLASS-METHODS load_po_data
      IMPORTING ir_po_id      TYPE ty_range_po_id
                ir_currency   TYPE ty_range_currency
                iv_maxrow     TYPE i
      RETURNING VALUE(result) TYPE lcl_po_processor=>tty_po_header.
      
    CLASS-METHODS display_selection_list.
    CLASS-METHODS on_user_command FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.
    CLASS-METHODS display_results.
ENDCLASS.

*----------------------------------------------------------------------*
* SELECTION SCREEN
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: so_poid FOR snwd_po-po_id,
                  so_curr FOR snwd_po-currency_code.
  PARAMETERS: p_maxrow TYPE i DEFAULT 500 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* IMPLEMENTATIONS
*----------------------------------------------------------------------*
CLASS lcl_po_processor IMPLEMENTATION.
  METHOD execute.
    IF it_selected IS INITIAL.
      RETURN.
    ENDIF.
    
    " Constrói e executa processos paralelos
    result = run_parallel( build_processes( it_selected ) ).
  ENDMETHOD.
  
  METHOD build_processes.
    result = VALUE cl_abap_parallel=>t_in_inst_tab(
      FOR ls_po IN it_selected
      ( CAST if_abap_parallel( NEW lcl_po_worker( is_po_header = ls_po ) ) )
    ).
  ENDMETHOD.
  
  METHOD run_parallel.
    NEW cl_abap_parallel( p_percentage = 30 )->run_inst(
      EXPORTING
        p_in_tab  = it_processes
        p_debug   = abap_false
      IMPORTING
        p_out_tab = DATA(lt_finished)
    ).
    
    " Coleta resultados
    LOOP AT lt_finished INTO DATA(ls_finished).
      DATA(lo_worker) = CAST lcl_po_worker( ls_finished-inst ).
      INSERT LINES OF lo_worker->get_result( ) INTO TABLE result.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS lcl_po_worker IMPLEMENTATION.
  METHOD constructor.
    ms_po_header = is_po_header.
  ENDMETHOD.
  
  METHOD if_abap_parallel~do.
    " Executa processamento e armazena resultado
    INSERT process_po( ) INTO TABLE mt_result.
  ENDMETHOD.
  
  METHOD get_result.
    result = mt_result.
  ENDMETHOD.
  
  METHOD process_po.
    result-node_key       = ms_po_header-node_key.
    result-po_id          = ms_po_header-po_id.
    result-process_status = icon_led_yellow.
    
    " Busca itens do PO
    DATA(lt_items) = get_po_items( ms_po_header-node_key ).
    result-items_count = lines( lt_items ).
    
    " Simula processamento e criação de log
    IF lt_items IS INITIAL.
      result-process_status = icon_led_red.
      result-message = |PO { ms_po_header-po_id }: No items found!|.
    ELSE.
      DATA(lv_log_ok) = create_log_message( iv_po_id = ms_po_header-po_id
                                            iv_items = lt_items ).
      
      IF lv_log_ok = abap_true.
        result-process_status = icon_led_green.
        result-message = |PO { ms_po_header-po_id }: { result-items_count } items processed at { sy-uzeit TIME = ISO }|.
      ELSE.
        result-process_status = icon_led_red.
        result-message = |PO { ms_po_header-po_id }: Error creating log!|.
      ENDIF.
    ENDIF.
  ENDMETHOD.
  
  METHOD get_po_items.
    SELECT node_key,
           parent_key,
           po_item_pos,
           product_guid,
           gross_amount
      FROM snwd_po_i
      WHERE parent_key = @iv_parent_key
      INTO TABLE @result.
  ENDMETHOD.
  
  METHOD create_log_message.
    " Cria mensagem de log no Application Log
    DATA(message_list) = cf_reca_message_list=>create(
      id_object    = 'BTC'
      id_subobject = 'PO_PROC'
      id_extnumber = |PO_{ iv_po_id }|
      id_deldate   = CONV #( sy-datum + 7 )
    ).
    
    " Adiciona mensagem do header
    message_list->add(
      id_msgty = 'I'
      id_msgid = '>0'
      id_msgno = '000'
      id_msgv1 = |Processing PO { iv_po_id } with { lines( iv_items ) } items at { sy-uzeit TIME = ISO }|
    ).
    
    " Adiciona detalhe dos itens (primeiros 5)
    LOOP AT iv_items INTO DATA(ls_item) TO 5.
      message_list->add(
        id_msgty = 'I'
        id_msgid = '>0'
        id_msgno = '000'
        id_msgv1 = |  Item { ls_item-po_item_pos }: Amount { ls_item-gross_amount }|
      ).
    ENDLOOP.
    
    message_list->store( EXCEPTIONS error = 1 OTHERS = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    
    result = abap_true.
    
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING wait = abap_true.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.
  METHOD start.
    " 1. Carrega lista de POs
    gt_po_list = load_po_data( ir_po_id    = so_poid[]
                               ir_currency = so_curr[]
                               iv_maxrow   = p_maxrow ).
    
    IF gt_po_list IS INITIAL.
      MESSAGE 'No Purchase Orders found!' TYPE 'I'.
      RETURN.
    ENDIF.
    
    " 2. Exibe ALV para seleção
    display_selection_list( ).
  ENDMETHOD.
  
  METHOD load_po_data.
    SELECT node_key,
           po_id,
           partner_guid,
           currency_code,
           gross_amount,
           created_at
      FROM snwd_po
      WHERE po_id IN @ir_po_id
        AND currency_code IN @ir_currency
      INTO TABLE @result
      UP TO @iv_maxrow ROWS.
  ENDMETHOD.
  
  METHOD display_selection_list.
    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = go_alv
          CHANGING  t_table      = gt_po_list
        ).
        
        " Habilita seleção de linhas
        go_alv->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).
        
        " Adiciona botão de processamento
        DATA(lo_functions) = go_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        
        " Adiciona função customizada
        lo_functions->add_function(
          name     = 'PROCESS'
          icon     = CONV #( icon_execute_object )
          text     = 'Process Selected POs'
          tooltip  = 'Process selected Purchase Orders in parallel'
          position = if_salv_c_function_position=>right_of_salv_functions
        ).
        
        " Registra evento
        DATA(lo_events) = go_alv->get_event( ).
        SET HANDLER on_user_command FOR lo_events.
        
        " Otimiza colunas
        go_alv->get_columns( )->set_optimize( abap_true ).
        
        " Exibe
        go_alv->display( ).
        
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_text( ) TYPE 'I'.
    ENDTRY.
  ENDMETHOD.
  
  METHOD on_user_command.
    CASE e_salv_function.
      WHEN 'PROCESS'.
        " Obtém linhas selecionadas
        DATA(lo_selections) = go_alv->get_selections( ).
        DATA(lt_selected_rows) = lo_selections->get_selected_rows( ).
        
        IF lt_selected_rows IS INITIAL.
          MESSAGE 'Please select at least one PO!' TYPE 'I'.
          RETURN.
        ENDIF.
        
        " Monta tabela com POs selecionados
        DATA(lt_selected_pos) = VALUE lcl_po_processor=>tty_po_header(
          FOR ls_row IN lt_selected_rows
          ( gt_po_list[ ls_row ] )
        ).
        
        " Executa processamento paralelo
        gt_results = lcl_po_processor=>execute( lt_selected_pos ).
        
        " Exibe resultados
        display_results( ).
    ENDCASE.
  ENDMETHOD.
  
  METHOD display_results.
    DATA: lo_alv_result TYPE REF TO cl_salv_table.
    
    TRY.
        cl_salv_table=>factory(
          IMPORTING r_salv_table = lo_alv_result
          CHANGING  t_table      = gt_results
        ).
        
        lo_alv_result->get_columns( )->set_optimize( abap_true ).
        lo_alv_result->get_functions( )->set_all( abap_true ).
        
        " Destaca coluna de status como ícone
        TRY.
            DATA(lo_columns) = lo_alv_result->get_columns( ).
            DATA(lo_column) = CAST cl_salv_column_table( lo_columns->get_column( 'PROCESS_STATUS' ) ).
            lo_column->set_icon( if_salv_c_bool_sap=>true ).
          CATCH cx_salv_not_found.
            " Coluna não encontrada, continua sem ícone
        ENDTRY.
        
        lo_alv_result->display( ).
        
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_text( ) TYPE 'I'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* MAIN PROGRAM
*----------------------------------------------------------------------*
START-OF-SELECTION.
  lcl_main=>start( ).
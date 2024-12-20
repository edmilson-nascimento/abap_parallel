CLASS lcx_calc_aging_equi DEFINITION
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    METHODS constructor
      IMPORTING textid    LIKE if_t100_message=>t100key OPTIONAL
                !previous LIKE previous                 OPTIONAL.
ENDCLASS.


CLASS lcx_calc_aging_equi IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_view DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS get_werks
      IMPORTING enterprise    TYPE  range_t_werks
      RETURNING VALUE(result) TYPE ztc4_bu_list-werks.

    CLASS-METHODS show_alv
      CHANGING !output TYPE ANY TABLE.

ENDCLASS.


CLASS lcl_view IMPLEMENTATION.
  METHOD get_werks.
    result = COND #( WHEN lines( enterprise ) = 1 AND enterprise[ 1 ]-option = 'EQ'
                     THEN enterprise[ 1 ]-low
                     ELSE zclc4_util_migrations=>mc_multi_plants ).
  ENDMETHOD.

  METHOD show_alv.
    DATA lo_alv       TYPE REF TO cl_salv_table.
    DATA lo_columns   TYPE REF TO cl_salv_columns.

    DATA lo_functions TYPE REF TO cl_salv_functions_list.

    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv
                                CHANGING  t_table      = output ).
      CATCH cx_salv_msg.
    ENDTRY.
    lo_columns = lo_alv->get_columns( ).
    lo_columns->set_optimize( abap_true ).
    TRY.
        DATA(lo_column) = lo_columns->get_column( 'UPDATE_STATUS' ).
        lo_column->set_short_text( 'EquiST' ).
        lo_column->set_medium_text( 'Equipment Status' ).
        lo_column->set_long_text( 'Equipment Status' ).

        lo_column = lo_columns->get_column( 'IDOC_STATUS' ).
        lo_column->set_short_text( 'IdocSt' ).
        lo_column->set_medium_text( 'Idoc Status' ).
        lo_column->set_long_text( 'Idoc Status' ).

      CATCH cx_salv_not_found.
    ENDTRY.
    lo_functions = lo_alv->get_functions( ).
    lo_functions->set_all( abap_true ).

    lo_alv->display( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_select_data DEFINITION.

  PUBLIC SECTION.
    TYPES ttr_enterprise TYPE RANGE OF ztc4_bu_list-werks.
    TYPES ttr_equnr      TYPE RANGE OF equi-equnr.
    TYPES tty_equip_data TYPE TABLE OF zcdsc4_equipment_aging WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING ir_enterprise TYPE ttr_enterprise
                iv_aedat      TYPE equi-aedat
                ir_equnr      TYPE ttr_equnr
                iv_lines      TYPE i
                iv_new_idoc   TYPE abap_bool.

    METHODS select_data
      RAISING lcx_calc_aging_equi.

    METHODS get_equip_data RETURNING VALUE(result) TYPE tty_equip_data.

    METHODS get_werks
      RETURNING VALUE(result) TYPE ztc4_bu_list-werks.

    METHODS get_idoc_parameter
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS is_execution_running
      RAISING lcx_calc_aging_equi.

    METHODS get_lastmoddate RETURNING VALUE(r_result) TYPE equi-aedat.
    METHODS get_enterprise  RETURNING VALUE(r_result) TYPE lcl_select_data=>ttr_enterprise.

  PRIVATE SECTION.
    DATA enterprise  TYPE ttr_enterprise.
    DATA lastmoddate TYPE equi-aedat.
    DATA equipment   TYPE ttr_equnr.
    DATA lines       TYPE i.
    DATA equip_data  TYPE TABLE OF zcdsc4_equipment_aging.
    DATA new_idoc    TYPE abap_bool.

ENDCLASS.


CLASS lcl_process_data DEFINITION.

  PUBLIC SECTION.
    TYPES : BEGIN OF ty_result,
              equipment            TYPE equi-equnr,
              werks                TYPE ztc4_bu_list-werks,
              zzremaining_life     TYPE equi-zzremaining_life,
              zzequip_age          TYPE equi-zzequip_age,
              zzamdec_vetu         TYPE equi-zzamdec_vetu,
              zzamdec_vetu_comment TYPE equi-zzamdec_vetu_comment,
              zzdureevie           TYPE equi-zzdureevie,
              update_status        TYPE icon-id,
              idoc_status          TYPE icon-id,
              bapi_result          TYPE bapiret2,
            END OF ty_result.
    TYPES tty_result TYPE TABLE OF ty_result WITH EMPTY KEY.

    TYPES : BEGIN OF ty_output,
              equipment            TYPE equi-equnr,
              werks                TYPE ztc4_bu_list-werks,
              zzremaining_life     TYPE equi-zzremaining_life,
              zzequip_age          TYPE equi-zzequip_age,
              zzamdec_vetu         TYPE equi-zzamdec_vetu,
              zzamdec_vetu_comment TYPE equi-zzamdec_vetu_comment,
              zzdureevie           TYPE equi-zzdureevie,
              update_status        TYPE icon-id,
              idoc_status          TYPE icon-id,
            END OF ty_output.
    TYPES tty_output TYPE TABLE OF ty_output WITH EMPTY KEY.

    METHODS constructor
      IMPORTING io_select_data TYPE REF TO lcl_select_data.

    METHODS execute
      RETURNING VALUE(result) TYPE tty_output
      RAISING   lcx_calc_aging_equi.

  PRIVATE SECTION.
    DATA mo_select_data TYPE REF TO lcl_select_data.

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
      IMPORTING it_equip_data       TYPE lcl_select_data=>tty_equip_data
      RETURNING VALUE(rt_processes) TYPE cl_abap_parallel=>t_in_inst_tab.

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


CLASS lcl_update_equipment DEFINITION.
  PUBLIC SECTION.
    TYPES : BEGIN OF ty_read_container,
              equi  TYPE zsc4_equi_append,
              equi2 TYPE zsc4_equi_append2,
            END   OF ty_read_container.

    METHODS update_equipment
      IMPORTING iv_equipment_data TYPE zcdsc4_equipment_aging
      RETURNING VALUE(result)     TYPE lcl_process_data=>ty_result.

  PRIVATE SECTION.
    METHODS get_extensionout
      IMPORTING equipment     TYPE zcdsc4_equipment_aging-equipment
      RETURNING VALUE(result) TYPE bapiparextab.

    METHODS read_container
      IMPORTING !structure    TYPE bapiparex-structure
                im_container  TYPE csequence
      RETURNING VALUE(result) TYPE ty_read_container.

    METHODS call_bapi_equi_change
      IMPORTING equipment     TYPE zcdsc4_equipment_aging-equipment
                !extension    TYPE bapiparextab
      RETURNING VALUE(result) TYPE bapiret2.

    METHODS get_equi_detail
      IMPORTING equipment     TYPE zcdsc4_equipment_aging-equipment
      RETURNING VALUE(result) TYPE zsc4_equi_equipment.

    METHODS read_domain_text_by_value
      IMPORTING domain        TYPE csequence
                !value        TYPE clike
      RETURNING VALUE(result) TYPE equi-zzamdec_vetu_comment.

    METHODS avoid_idoc_generation
      IMPORTING equipment TYPE zcdsc4_equipment_aging-equipment.

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


CLASS lcl_select_data IMPLEMENTATION.
  METHOD constructor.
    enterprise  = ir_enterprise.
    lastmoddate = iv_aedat.
    equipment   = ir_equnr.
    lines       = iv_lines.
    new_idoc    = iv_new_idoc.
  ENDMETHOD.

  METHOD select_data.
    SELECT FROM zcdsc4_equipment_aging
      FIELDS *
      WHERE plant IN @enterprise
        AND lastchangedate <= @lastmoddate
        AND equipment IN @equipment
      ORDER BY lastchangedate
      INTO TABLE @equip_data
      UP TO @lines ROWS.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_calc_aging_equi MESSAGE ID 'ZMC_C4' TYPE 'I' NUMBER '013'.
    ENDIF.
  ENDMETHOD.

  METHOD get_equip_data.
    result = equip_data.
  ENDMETHOD.

  METHOD get_werks.
    result = COND #( WHEN lines( enterprise ) = 1 AND enterprise[ 1 ]-option = 'EQ'
                     THEN enterprise[ 1 ]-low
                     ELSE zclc4_util_migrations=>mc_multi_plants ).
  ENDMETHOD.

  METHOD is_execution_running.
    IF zclc4_executions=>get_instance( )->is_execution_running( iv_plant = get_werks( ) ).
      RAISE EXCEPTION TYPE lcx_calc_aging_equi MESSAGE ID 'ZMC_CS' TYPE 'I' NUMBER '027'.
    ENDIF.
  ENDMETHOD.

  METHOD get_lastmoddate.
    r_result = lastmoddate.
  ENDMETHOD.

  METHOD get_enterprise.
    r_result = enterprise.
  ENDMETHOD.

  METHOD get_idoc_parameter.
    result = new_idoc.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_process_data IMPLEMENTATION.
  METHOD constructor.
    mo_select_data = io_select_data.
  ENDMETHOD.

  METHOD execute.
    DATA(lt_equip_data) = mo_select_data->get_equip_data( ).
    display_popup( lines( lt_equip_data ) ).

    DATA(lock_record) = activate_execution( ).

    start_log( nr_records  = lines( lt_equip_data )
               lock_record = lock_record ).

    DATA(lt_result) = run_parallel( build_process( lt_equip_data ) ).

    result = add_log( equi_result = lt_result
                      idoc_result = send_idoc_bi( lt_result ) ).

    save_log( lock_record ).
    disable_execution( lock_record ).
  ENDMETHOD.

  METHOD save_log.
    zclc4_util_looma=>get_instance( )->save_log( iv_subobject       = CONV #( lock_record-process )
                                                 iv_external_number = CONV #( lock_record-execution ) ).
  ENDMETHOD.

  METHOD disable_execution.
    zclc4_executions=>get_instance( )->disable_execution(
        is_lock         = lock_record
        iv_execution_ok = zclc4_util_looma=>get_instance( )->no_error_messages( ) ).
  ENDMETHOD.

  METHOD build_process.
    rt_processes = VALUE cl_abap_parallel=>t_in_inst_tab(
                             FOR ls_equip_data IN it_equip_data
                             ( NEW lcl_process_parallel( iv_equipment_data = ls_equip_data ) ) ).
  ENDMETHOD.

  METHOD activate_execution.
    lock_record = zclc4_executions=>get_instance( )->activate_execution(
                      iv_execution       = zclc4_executions=>get_instance( )->get_next_execution( )
                      iv_plant           = mo_select_data->get_werks( )
                      iv_key1            = CONV #( mo_select_data->get_lastmoddate( ) )
                      iv_processing_mode = '2' ).
    IF lock_record IS INITIAL.
      RAISE EXCEPTION TYPE lcx_calc_aging_equi MESSAGE ID 'ZMC_C4_LOOMA' TYPE 'I' NUMBER '104'.
    ENDIF.
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
    zclc4_executions=>get_instance( )->initial_log_messages( io_utils      = zclc4_util_looma=>get_instance( )
                                                             is_execution  = lock_record
                                                             iv_nr_records = nr_records  ).

    zclc4_util_looma=>get_instance( )->add_message(
        iv_msgty    = zclc4_util_looma=>gc_msgty_s
        iv_msgid    = zclc4_util_looma=>gc_msgid
        iv_msgno    = '32'
        iv_detlevel = '3'
        iv_msgv1    = CONV #( |{  mo_select_data->get_lastmoddate( ) DATE = ENVIRONMENT }| ) ).
  ENDMETHOD.

  METHOD send_idoc_bi.
    result = zclc4_equip_idoc=>get_instance( )->send_equi_aging_to_bi(
                 equips   = VALUE #( FOR res IN equip_result
                                     ( id_div          = res-werks
                                       id_equi         = res-equipment
                                       id_age_equi     = res-zzequip_age
                                       remaining_life  = res-zzremaining_life
                                       vetu_note_label = res-zzamdec_vetu_comment
                                       vetu_note_value = res-zzamdec_vetu
                                       life_duration   = res-zzdureevie ) )
                 new_idoc = mo_select_data->get_idoc_parameter( ) ).
  ENDMETHOD.

  METHOD add_log.
    LOOP AT equi_result INTO DATA(ls_result).

      APPEND INITIAL LINE TO result ASSIGNING FIELD-SYMBOL(<fs_output>).
      zclc4_util_looma=>get_instance( )->add_message( iv_msgty    = zclc4_util_looma=>gc_msgty_s
                                                      iv_msgid    = zclc4_util_looma=>gc_msgid
                                                      iv_msgno    = '126'
                                                      iv_detlevel = '3'
                                                      iv_msgv1    = CONV #( ls_result-equipment ) ).

      IF ls_result-update_status = icon_led_green.
        zclc4_util_looma=>get_instance( )->add_message( iv_msgty    = zclc4_util_looma=>gc_msgty_s
                                                        iv_msgid    = zclc4_util_looma=>gc_msgid
                                                        iv_msgno    = '127'
                                                        iv_detlevel = '4'
                                                        iv_msgv1    = CONV #( ls_result-equipment ) ).
      ELSEIF ls_result-update_status = icon_led_red.
        zclc4_util_looma=>get_instance( )->add_bapiret_message( is_message  = ls_result-bapi_result
                                                                iv_detlevel = '4' ).

      ELSE.
        zclc4_util_looma=>get_instance( )->add_message( iv_msgty    = zclc4_util_looma=>gc_msgty_s
                                                        iv_msgid    = zclc4_util_looma=>gc_msgid
                                                        iv_msgno    = '155'
                                                        iv_detlevel = '4' ).
      ENDIF.

      TRY.
          DATA(idoc) = VALUE #( idoc_result[ equip = ls_result-equipment ] ).
          IF idoc-send_status = icon_green_light.
            ls_result-idoc_status = icon_led_green.
            zclc4_util_looma=>get_instance( )->add_message( iv_msgty    = zclc4_util_looma=>gc_msgty_s
                                                            iv_msgid    = zclc4_util_looma=>gc_msgid
                                                            iv_msgno    = '247'
                                                            iv_detlevel = '4'
                                                            iv_msgv1    = CONV #( idoc-idoc ) ).
          ELSEIF idoc-send_status = icon_red_light.
            ls_result-idoc_status = icon_led_red.
            zclc4_util_looma=>get_instance( )->add_message( iv_msgty    = zclc4_util_looma=>gc_msgty_s
                                                            iv_msgid    = zclc4_util_looma=>gc_msgid
                                                            iv_msgno    = '082'
                                                            iv_detlevel = '4' ).
          ENDIF.
        CATCH cx_sy_itab_line_not_found.
          CLEAR idoc.
      ENDTRY.
      <fs_output> = CORRESPONDING #( ls_result ).
    ENDLOOP.
  ENDMETHOD.

  METHOD display_popup.
    DATA button TYPE c LENGTH 1.

    IF sy-batch IS NOT INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'POPUP_FOR_INTERACTION'
      EXPORTING headline       = TEXT-t03
                text1          = |{ TEXT-t04 } { lines }|
                button_1       = TEXT-t05
                button_2       = TEXT-t06
      IMPORTING button_pressed = button.
    IF button <> '1'.
      RAISE EXCEPTION TYPE lcx_calc_aging_equi MESSAGE ID 'ZMC_C4' TYPE 'I' NUMBER '016'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_update_equipment IMPLEMENTATION.
  METHOD update_equipment.
    DATA lt_extension TYPE TABLE OF bapiparex.

    result-equipment     = iv_equipment_data-equipment.
    result-werks         = iv_equipment_data-plant.
    result-update_status = icon_led_yellow.

    DATA(lt_extension_out) = get_extensionout( iv_equipment_data-equipment ).

    TRY.
        DATA(extensionout) = lt_extension_out[ structure = 'ZSC4_EQUI_APPEND' ].
        DATA(container) = read_container( structure    = extensionout-structure
                                          im_container = extensionout+30 ).
        IF sy-subrc = 0.
          IF container-equi-zzdureevie IS INITIAL.
            DATA(equipment_detail) = get_equi_detail( iv_equipment_data-equipment ).
          ENDIF.
          container-equi-zzdureevie = COND #( WHEN container-equi-zzdureevie IS NOT INITIAL
                                              THEN container-equi-zzdureevie
                                              ELSE zclc4_techref_dao=>get_instance(
                                                    )->fetch_life_duration( domain_sap  = equipment_detail-zdomain
                                                                            funcion_sap = equipment_detail-function
                                                                            family_sap  = equipment_detail-family ) ).
          DATA(life_duration) = container-equi-zzdureevie.
          result-zzdureevie = life_duration.
          container-equi-zzamdec_vetu = zclc4_equipment=>get_vetuste(
              equip_age     = zclc4_equipment=>get_equip_age( iv_equipment_data-installationdate )
              life_duration = life_duration ).
          result-zzamdec_vetu = container-equi-zzamdec_vetu.
          IF result-zzamdec_vetu IS NOT INITIAL.

            result-zzamdec_vetu_comment = read_domain_text_by_value( domain = 'ZDC4_AMDEC_VETU'
                                                                     value  = result-zzamdec_vetu ).

          ENDIF.
          cl_abap_container_utilities=>fill_container_c( EXPORTING  im_value               = container-equi
                                                         IMPORTING  ex_container           = extensionout+30
                                                         EXCEPTIONS illegal_parameter_type = 1
                                                                    OTHERS                 = 2 ).
          IF sy-subrc = 0.
            APPEND extensionout TO lt_extension.
          ENDIF.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        CLEAR extensionout.
    ENDTRY.

    TRY.
        extensionout = lt_extension_out[ structure = 'ZSC4_EQUI_APPEND2' ].
        container = read_container( structure    = extensionout-structure
                                    im_container = extensionout+30 ).
        IF sy-subrc = 0.
          container-equi2-zzequip_age = zclc4_equipment=>get_equip_age( iv_equipment_data-installationdate ).
          result-zzequip_age = container-equi2-zzequip_age.
          container-equi2-zzremaining_life = zclc4_equipment=>get_remaining_life(
                                                 life_duration     = life_duration
                                                 installation_date = iv_equipment_data-installationdate ).
          result-zzremaining_life = container-equi2-zzremaining_life.
          cl_abap_container_utilities=>fill_container_c( EXPORTING  im_value               = container-equi2
                                                         IMPORTING  ex_container           = extensionout+30
                                                         EXCEPTIONS illegal_parameter_type = 1
                                                                    OTHERS                 = 2 ).
          IF sy-subrc = 0.
            APPEND extensionout TO lt_extension.
          ENDIF.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        CLEAR extensionout.
    ENDTRY.

    IF lt_extension IS NOT INITIAL.
      result-bapi_result = call_bapi_equi_change( equipment = iv_equipment_data-equipment
                                                  extension = lt_extension ).
      IF result-bapi_result-type = 'E'.
        result-update_status = icon_led_red.
      ELSE.
        result-update_status = icon_led_green.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_extensionout.
    CALL FUNCTION 'BAPI_EQUI_GETDETAIL' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        equipment    = equipment
      TABLES
        extensionout = result.
  ENDMETHOD.

  METHOD read_container.
    IF structure = 'ZSC4_EQUI_APPEND'.
      cl_abap_container_utilities=>read_container_c( EXPORTING  im_container           = im_container
                                                     IMPORTING  ex_value               = result-equi
                                                     EXCEPTIONS illegal_parameter_type = 1
                                                                OTHERS                 = 2 ).

    ELSEIF structure = 'ZSC4_EQUI_APPEND2'.
      cl_abap_container_utilities=>read_container_c( EXPORTING  im_container           = im_container
                                                     IMPORTING  ex_value               = result-equi2
                                                     EXCEPTIONS illegal_parameter_type = 1
                                                                OTHERS                 = 2 ).
    ENDIF.
    IF sy-subrc <> 0.
      CLEAR: result.
    ENDIF.
  ENDMETHOD.

  METHOD call_bapi_equi_change.
    DATA data_general   TYPE bapi_itob.
    DATA data_generalx  TYPE bapi_itobx.
    DATA data_specific  TYPE bapi_itob_eq_only.
    DATA data_specificx TYPE bapi_itob_eq_onlyx.

    avoid_idoc_generation( equipment ).

    CALL FUNCTION 'BAPI_EQUI_CHANGE' "#EC CI_USAGE_OK[2438131]
      EXPORTING
        equipment      = equipment
        data_general   = data_general
        data_generalx  = data_generalx
        data_specific  = data_specific
        data_specificx = data_specificx
      IMPORTING
        return         = result
      TABLES
        extensionin    = extension.

    IF result-type = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ENDIF.
  ENDMETHOD.

  METHOD get_equi_detail.
    CALL FUNCTION 'ZFC4_EQUI_EQUIPMENT'
      EXPORTING
        iv_equipment = equipment
      IMPORTING
        es_equipment = result.
  ENDMETHOD.

  METHOD read_domain_text_by_value.
    cl_reca_ddic_doma=>get_text_by_value( EXPORTING  id_name   = domain
                                                     id_value  = value
                                          IMPORTING  ed_text   = result
                                          EXCEPTIONS not_found = 1
                                                     OTHERS    = 2 ).
    IF sy-subrc <> 0.
      CLEAR : result.
    ENDIF.
  ENDMETHOD.

  METHOD avoid_idoc_generation.
    DATA lt_equi      TYPE equnr_tab.
    DATA ls_indx      TYPE indx.
    DATA lv_memory_id TYPE char40.

    lv_memory_id = |{ equipment }_AGI|.
    lt_equi = VALUE #( ( equipment ) ).
    ls_indx-srtfd = space.
    ls_indx-aedat = sy-datum.
    ls_indx-usera = sy-uname.
    ls_indx-pgmid = sy-repid.

    CALL FUNCTION 'ZFCS_EXPORT_TO_DATABASE'
      EXPORTING
        it_table = lt_equi
        is_indx  = ls_indx
        ip_id    = lv_memory_id.
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


CLASS ltcl_view DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    DATA cut TYPE REF TO lcl_select_data.

    METHODS multiple_inputs_returns_mult FOR TESTING RAISING cx_static_check.
    METHODS single_input_rets_same_value FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_view IMPLEMENTATION.
  METHOD multiple_inputs_returns_mult.
    cl_abap_unit_assert=>assert_equals( exp = 'MULT'
                                        act = lcl_view=>get_werks( enterprise = VALUE #( sign   = 'I'
                                                                                         option = 'EQ'
                                                                                         ( low = '9501' )
                                                                                         ( low = '9502' ) ) ) ).
  ENDMETHOD.

  METHOD single_input_rets_same_value.
    cl_abap_unit_assert=>assert_equals( exp = '9501'
                                        act = lcl_view=>get_werks( enterprise = VALUE #( sign   = 'I'
                                                                                         option = 'EQ'
                                                                                         ( low = '9501' ) ) ) ).
  ENDMETHOD.
ENDCLASS.
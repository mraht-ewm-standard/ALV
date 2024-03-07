"! <p class="shorttext synchronized">ALV: Grid</p>
CLASS zial_cl_alv_grid DEFINITION
  PUBLIC
  INHERITING FROM cl_gui_alv_grid
  CREATE PUBLIC
  GLOBAL FRIENDS zial_cl_alv_grid_evt_handler.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_sel_mode,
                 mlt_col_row_sel TYPE lvc_libox VALUE 'A',
                 smp_sel_lstbox  TYPE lvc_libox VALUE 'B',
                 mlt_sel_lstbox  TYPE lvc_libox VALUE 'C',
                 cell_sel        TYPE lvc_libox VALUE 'D',
               END OF mc_sel_mode.

    CONSTANTS: BEGIN OF mc_row_style,
                 col_name  TYPE lvc_cifnm         VALUE 'ROW_STYLE',
                 data_type TYPE zial_de_data_type VALUE 'CHAR4',
                 success   TYPE char4             VALUE space,
                 error     TYPE char4             VALUE 'C600',
               END OF mc_row_style.

    CONSTANTS: BEGIN OF mc_cell_style,
                 col_name  TYPE tabname           VALUE 'CELL_STYLE',
                 data_type TYPE zial_de_data_type VALUE 'LVC_T_STYL',
                 success   TYPE x LENGTH 4        VALUE '00000000',
                 error     TYPE x LENGTH 4        VALUE '00000087',
               END OF mc_cell_style.

    CONSTANTS: BEGIN OF mc_okcode,
                 enter TYPE syucomm VALUE 'ALV_OKCODE_ENTER',
               END OF mc_okcode.

    CONSTANTS: BEGIN OF mc_col_opt,
                 "! Optimization is kept until first interaction
                 first_interact TYPE lvc_colopt VALUE 'X',
                 "! Optimization is kept always
                 always         TYPE lvc_colopt VALUE 'A',
                 "! No optimization
                 none           TYPE lvc_colopt VALUE space,
               END OF mc_col_opt.

    CLASS-METHODS det_fcat_by_ddic
      IMPORTING iv_field_name  TYPE fieldname
                iv_data_type   TYPE zial_de_data_type
                it_text        TYPE zial_tt_alv_fcat_txt OPTIONAL
      RETURNING VALUE(rs_fcat) TYPE lvc_s_fcat
      RAISING   zcx_alv_grid_fcat_not_found.

    METHODS constructor
      IMPORTING io_container     TYPE REF TO cl_gui_container
                iv_appl_events   TYPE abap_bool DEFAULT abap_false
                io_event_handler TYPE REF TO zial_cl_alv_grid_evt_handler
      RAISING   zcx_alv_grid_not_createable.

    METHODS det_selected_rows
      RETURNING VALUE(rt_selected_rows) TYPE lvc_t_row.

    METHODS show
      RAISING cx_static_check.

    METHODS refresh.
    METHODS reset_fcat.

    METHODS set_delay_on_changed_sel
      IMPORTING iv_delay TYPE i DEFAULT 300.

    METHODS set_row_style
      IMPORTING iv_style    TYPE char4 DEFAULT mc_row_style-error
                ir_s_outtab TYPE REF TO data.

    METHODS set_cell_style
      IMPORTING iv_style      TYPE x DEFAULT mc_cell_style-error
                iv_field_name TYPE lvc_fname
                ir_s_outtab   TYPE REF TO data.

    METHODS reset_style
      IMPORTING iv_field_name TYPE lvc_fname OPTIONAL
                ir_s_outtab   TYPE REF TO data.

    "! <p class="shorttext synchronized">Set OKCODE for events</p>
    "! <p>Only works if IV_APPL_EVENTS in constructor was set to abap_false!</p>
    "! @parameter iv_on_enter | OKCODE if user pressed ENTER
    METHODS set_evt_ok_code
      IMPORTING iv_on_enter TYPE syucomm.

    METHODS set_title
      IMPORTING iv_title TYPE lvc_title OPTIONAL.

    METHODS free     REDEFINITION.
    METHODS dispatch REDEFINITION.

    METHODS get_fieldcatalog
      RETURNING VALUE(rt_fieldcatalog) TYPE lvc_t_fcat.

    METHODS set_current_cell_via_id REDEFINITION.
    METHODS enable_cell_style.
    METHODS enable_row_style.

  PROTECTED SECTION.
    DATA mo_container        TYPE REF TO cl_gui_container.
    DATA mr_outtab           TYPE REF TO data.
    DATA ms_variant          TYPE disvariant.
    DATA ms_layout           TYPE lvc_s_layo.
    DATA mt_sort             TYPE lvc_t_sort.
    DATA mt_filter           TYPE lvc_t_filt.
    DATA mt_fieldcatalog     TYPE lvc_t_fcat.
    DATA mt_toolbar_excl     TYPE ui_functions.
    DATA mo_event_handler    TYPE REF TO zial_cl_alv_grid_evt_handler.
    DATA mv_appl_events      TYPE abap_bool.
    DATA mv_ok_code_on_enter TYPE syucomm VALUE mc_okcode-enter.

    METHODS alv_set_fcat
      RAISING cx_static_check.

    METHODS alv_set_outtab
      RAISING cx_static_check.

    METHODS alv_set_sort
      RAISING cx_static_check.

    METHODS alv_set_filter
      RAISING cx_static_check.

    METHODS alv_set_variant
      RAISING cx_static_check.

    METHODS alv_set_layout
      RAISING cx_static_check.

    METHODS alv_set_toolbar_excl
      RAISING cx_static_check.

    METHODS alv_register_events
      IMPORTING iv_on_toolbar               TYPE abap_bool DEFAULT abap_false
                iv_on_double_click          TYPE abap_bool DEFAULT abap_false
                iv_on_user_command          TYPE abap_bool DEFAULT abap_true
                iv_on_value_help            TYPE abap_bool DEFAULT abap_false
                it_value_help               TYPE lvc_t_f4  OPTIONAL
                iv_on_data_changed          TYPE abap_bool DEFAULT abap_false
                iv_on_data_changed_finished TYPE abap_bool DEFAULT abap_false
                iv_on_hotspot               TYPE abap_bool DEFAULT abap_false.

    METHODS alv_set_table
      RAISING zcx_alv_grid_not_createable.

ENDCLASS.


CLASS zial_cl_alv_grid IMPLEMENTATION.

  METHOD constructor.

    super->constructor( EXPORTING  i_parent          = io_container
                                   i_appl_events     = iv_appl_events
                        EXCEPTIONS error_cntl_create = 1
                                   error_cntl_init   = 2
                                   error_cntl_link   = 3
                                   error_dp_create   = 4
                                   OTHERS            = 5 ).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_alv_grid_not_createable
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_container     = io_container.
    mo_event_handler = io_event_handler.
    mo_event_handler->set_alv_grid( me ).
    mv_appl_events = iv_appl_events.

  ENDMETHOD.


  METHOD set_delay_on_changed_sel.

    CHECK iv_delay GT 0.
    set_delay_change_selection( iv_delay ).

  ENDMETHOD.


  METHOD get_fieldcatalog.

    rt_fieldcatalog = mt_fieldcatalog.

  ENDMETHOD.


  METHOD alv_set_outtab.
  ENDMETHOD.


  METHOD alv_set_fcat.
  ENDMETHOD.


  METHOD alv_set_sort.
  ENDMETHOD.


  METHOD alv_set_filter.
  ENDMETHOD.


  METHOD alv_set_variant.

    ms_variant = VALUE #( report   = sy-repid
                          username = sy-uname ).

  ENDMETHOD.


  METHOD alv_set_layout.

    ms_layout = VALUE #( BASE ms_layout
                         edit       = abap_false
                         zebra      = abap_true
                         col_opt    = abap_true
                         sel_mode   = mc_sel_mode-cell_sel
                         no_merging = abap_true ).

  ENDMETHOD.


  METHOD enable_row_style.

    ms_layout-info_fname = mc_row_style-col_name.

    TRY.
        DATA(ls_fcat_row_style) = det_fcat_by_ddic( iv_field_name = mc_row_style-col_name
                                                    iv_data_type  = mc_row_style-data_type ).
        CHECK     ls_fcat_row_style IS NOT INITIAL
          AND NOT line_exists( mt_fieldcatalog[ fieldname = mc_row_style-col_name ] ).

        APPEND VALUE #( BASE CORRESPONDING #( ls_fcat_row_style )
                        tech   = abap_true
                        no_out = abap_true ) TO mt_fieldcatalog.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD enable_cell_style.

    ms_layout-stylefname = mc_cell_style-col_name.

    TRY.
        DATA(ls_fcat_style) = det_fcat_by_ddic( iv_field_name = mc_cell_style-col_name
                                                iv_data_type  = mc_cell_style-data_type ).
        CHECK     ls_fcat_style IS NOT INITIAL
          AND NOT line_exists( mt_fieldcatalog[ fieldname = mc_cell_style-col_name ] ).

        APPEND VALUE #( BASE CORRESPONDING #( ls_fcat_style )
                        tech   = abap_true
                        no_out = abap_true ) TO mt_fieldcatalog.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD set_title.

    CHECK iv_title IS SUPPLIED
      AND iv_title NE ms_layout-grid_title.

    set_gridtitle( iv_title ).
    ms_layout-grid_title = iv_title.

  ENDMETHOD.


  METHOD alv_set_toolbar_excl.
  ENDMETHOD.


  METHOD alv_register_events.

    CHECK mo_event_handler IS BOUND.

    IF iv_on_toolbar EQ abap_true.
      SET HANDLER mo_event_handler->on_toolbar FOR me.
    ENDIF.

    IF iv_on_double_click EQ abap_true.
      SET HANDLER mo_event_handler->on_double_click FOR me.
    ENDIF.

    IF iv_on_user_command EQ abap_true.
      SET HANDLER mo_event_handler->on_user_command FOR me.
    ENDIF.

    IF iv_on_value_help EQ abap_true.
      SET HANDLER mo_event_handler->on_value_help FOR me.

      register_f4_for_fields( it_value_help ).

      " If this event is not registered and one uses a value help, every manual and not yet
      " confirmed input of the user is being cleared. After registering this event SAP is able
      " to save the entered but not yet confirmed value into the global referenced value table.
      register_edit_event( EXPORTING  i_event_id = cl_gui_alv_grid=>mc_evt_modified
                           EXCEPTIONS OTHERS     = 0 ).
    ENDIF.

    IF iv_on_data_changed EQ abap_true.
      SET HANDLER mo_event_handler->on_data_changed FOR me.
    ENDIF.

    IF iv_on_data_changed_finished EQ abap_true.
      SET HANDLER mo_event_handler->on_data_changed_finished FOR me.
    ENDIF.

    IF iv_on_hotspot EQ abap_true.
      SET HANDLER mo_event_handler->on_hotspot FOR me.
    ENDIF.

  ENDMETHOD.


  METHOD show.

    IF mt_fieldcatalog IS INITIAL.

      alv_set_fcat( ).

      alv_set_sort( ).

      alv_set_filter( ).

      alv_set_variant( ).

      alv_set_layout( ).

      set_title( ).

      alv_set_toolbar_excl( ).

      alv_register_events( ).

      alv_set_outtab( ).

      alv_set_table( ).

    ELSE.

      refresh( ).

    ENDIF.

  ENDMETHOD.


  METHOD refresh.

    refresh_table_display( EXPORTING  is_stable      = VALUE #( row = abap_true )
                                      i_soft_refresh = abap_false
                           EXCEPTIONS OTHERS         = 0 ).

  ENDMETHOD.


  METHOD reset_fcat.

    CLEAR mt_fieldcatalog.

  ENDMETHOD.


  METHOD free.

    super->free( EXCEPTIONS OTHERS = 0 ).

    mo_container->free( EXCEPTIONS OTHERS = 0 ).

  ENDMETHOD.


  METHOD set_row_style.

    CHECK ir_s_outtab IS BOUND.

    ASSIGN ir_s_outtab->* TO FIELD-SYMBOL(<ls_outtab_row>).
    CHECK <ls_outtab_row> IS ASSIGNED.

    ASSIGN COMPONENT mc_row_style-col_name OF STRUCTURE <ls_outtab_row> TO FIELD-SYMBOL(<lv_row_style>).
    CHECK <lv_row_style> IS ASSIGNED.

    <lv_row_style> = iv_style.

  ENDMETHOD.


  METHOD set_cell_style.

    FIELD-SYMBOLS <lt_cell_style> TYPE lvc_t_styl.

    CHECK ir_s_outtab IS BOUND.

    ASSIGN ir_s_outtab->* TO FIELD-SYMBOL(<ls_outtab_row>).
    CHECK <ls_outtab_row> IS ASSIGNED.

    ASSIGN COMPONENT mc_cell_style-col_name OF STRUCTURE <ls_outtab_row> TO <lt_cell_style>.
    CHECK <lt_cell_style> IS ASSIGNED.

    ASSIGN <lt_cell_style>[ fieldname = iv_field_name ] TO FIELD-SYMBOL(<ls_cell_style>).
    IF <ls_cell_style> IS NOT ASSIGNED.
      CHECK iv_style NE mc_cell_style-success.
      INSERT VALUE #( fieldname = iv_field_name ) INTO TABLE <lt_cell_style> ASSIGNING <ls_cell_style>.
    ENDIF.

    <ls_cell_style>-style = iv_style.

  ENDMETHOD.


  METHOD alv_set_table.

    FIELD-SYMBOLS <lt_outtab> TYPE STANDARD TABLE.

    ASSIGN mr_outtab->* TO <lt_outtab>.
    CHECK <lt_outtab> IS ASSIGNED.

    activate_display_protocol( abap_false ).

    set_table_for_first_display( EXPORTING  i_bypassing_buffer            = abap_false
                                            i_save                        = zial_cl_alv=>mc_layout_save_mode-both
                                            is_variant                    = ms_variant
                                            is_layout                     = ms_layout
                                            it_toolbar_excluding          = mt_toolbar_excl
                                 CHANGING   it_sort                       = mt_sort
                                            it_filter                     = mt_filter
                                            it_fieldcatalog               = mt_fieldcatalog
                                            it_outtab                     = <lt_outtab>
                                 EXCEPTIONS invalid_parameter_combination = 1
                                            program_error                 = 2
                                            too_many_lines                = 3
                                            OTHERS                        = 4 ).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_alv_grid_not_createable
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.


  METHOD reset_style.

    CHECK ir_s_outtab IS BOUND.

    ASSIGN ir_s_outtab->* TO FIELD-SYMBOL(<ls_outtab>).
    CHECK <ls_outtab> IS ASSIGNED.

    IF iv_field_name IS NOT INITIAL.
      set_cell_style( iv_style      = mc_cell_style-success
                      iv_field_name = iv_field_name
                      ir_s_outtab   = ir_s_outtab ).
    ENDIF.

    set_row_style( iv_style    = mc_row_style-success
                   ir_s_outtab = ir_s_outtab ).

  ENDMETHOD.


  METHOD set_evt_ok_code.

    ASSERT mv_appl_events EQ abap_false.
    mv_ok_code_on_enter = iv_on_enter.

  ENDMETHOD.


  METHOD dispatch.

    super->dispatch( EXPORTING  cargo             = cargo
                                eventid           = eventid
                                is_shellevent     = is_shellevent
                                is_systemdispatch = is_systemdispatch
                     EXCEPTIONS OTHERS            = 0 ).

    IF eventid EQ mc_evt_enter.
      CHECK mv_ok_code_on_enter IS NOT INITIAL.
      cl_gui_cfw=>set_new_ok_code( new_code = mv_ok_code_on_enter ).
    ENDIF.

  ENDMETHOD.


  METHOD det_fcat_by_ddic.

    FIELD-SYMBOLS <ls_text> TYPE zial_s_alv_fcat_txt.

    TRY.
        DATA(ls_fieldinfo) = zial_cl_data_type=>det_info( iv_name = iv_data_type ).
        rs_fcat = CORRESPONDING #( ls_fieldinfo MAPPING ref_table = reftable
                                                        ref_field = reffield
                                                        coltext   = scrtext_l
                                                        seltext   = scrtext_l ).
        rs_fcat-fieldname = iv_field_name.
        rs_fcat-col_opt   = abap_true.

        WHILE <ls_text> IS NOT ASSIGNED.

          DATA(lv_index) = sy-index.
          CASE lv_index.
            WHEN 1.
              ASSIGN it_text[ langu = sy-langu ] TO <ls_text>.

            WHEN 2.
              ASSIGN it_text[ 1 ] TO <ls_text>.

            WHEN OTHERS.
              EXIT.

          ENDCASE.

          CHECK <ls_text> IS ASSIGNED.

          rs_fcat = CORRESPONDING #( BASE ( rs_fcat ) <ls_text> ).

        ENDWHILE.

      CATCH cx_root INTO DATA(lx_error).
        RAISE EXCEPTION TYPE zcx_alv_grid_fcat_not_found
          EXPORTING input_data = VALUE #( ( fnam = 'DATA_TYPE'  low = iv_data_type )
                                          ( fnam = 'FIELD_NAME' low = iv_field_name ) )
                    message    = zial_cl_log=>to_bapiret( iv_msgtx = CONV #( lx_error->get_text( ) ) )
                    previous   = lx_error.

    ENDTRY.

  ENDMETHOD.


  METHOD det_selected_rows.

    WHILE rt_selected_rows IS INITIAL.

      DATA(lv_index) = sy-index.
      CASE lv_index.
        WHEN 1.
          get_selected_rows( IMPORTING et_index_rows = rt_selected_rows ).

        WHEN 2.
          get_selected_cells( IMPORTING et_cell = DATA(lt_cell) ).

          LOOP AT lt_cell ASSIGNING FIELD-SYMBOL(<ls_cell>) WHERE row_id IS NOT INITIAL.
            APPEND <ls_cell>-row_id TO rt_selected_rows.
          ENDLOOP.

        WHEN OTHERS.
          EXIT.

      ENDCASE.

    ENDWHILE.

  ENDMETHOD.


  METHOD set_current_cell_via_id.

    CHECK is_column_id IS NOT INITIAL
      AND (    is_row_id IS NOT INITIAL
            OR is_row_no IS NOT INITIAL ).

    super->set_current_cell_via_id( is_row_id    = is_row_id
                                    is_column_id = is_column_id
                                    is_row_no    = is_row_no ).

  ENDMETHOD.

ENDCLASS.

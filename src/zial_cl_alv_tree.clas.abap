"! <p class="shorttext synchronized">ALV: Tree</p>
CLASS zial_cl_alv_tree DEFINITION
  PUBLIC
  INHERITING FROM cl_gui_alv_tree
  CREATE PUBLIC
  GLOBAL FRIENDS zial_cl_alv_tree_event_handler.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING io_container       TYPE REF TO cl_gui_container
                iv_node_key_fname  TYPE dynfnam   DEFAULT 'NODE_KEY'
                iv_node_sel_mode   TYPE i         DEFAULT cl_gui_simple_tree=>node_sel_mode_single
                iv_no_html_header  TYPE abap_bool DEFAULT abap_true
                iv_single_item_sel TYPE abap_bool DEFAULT abap_false
                io_event_handler   TYPE REF TO zial_cl_alv_tree_event_handler
      RAISING   zcx_alv_grid_not_createable.

    METHODS set_drag_and_drop
      IMPORTING iv_enable TYPE abap_bool DEFAULT abap_true.

    METHODS det_selected_nodes
      RETURNING VALUE(rt_selected_nodes) TYPE lvc_t_nkey.

    METHODS show
      RAISING cx_static_check.

    METHODS reset_fcat.
    METHODS free REDEFINITION.

  PROTECTED SECTION.
    DATA mo_container      TYPE REF TO cl_gui_container.
    DATA mv_node_key_fname TYPE dynfnam.
    DATA mr_outtab         TYPE REF TO data.
    DATA ms_hierarchy_hdr  TYPE treev_hhdr.
    DATA ms_variant        TYPE disvariant.
    DATA mo_toolbar        TYPE REF TO cl_gui_toolbar.
    DATA mt_toolbar_excl   TYPE ui_functions.
    DATA mo_event_handler  TYPE REF TO zial_cl_alv_tree_event_handler.
    DATA mo_dragdrop       TYPE REF TO cl_dragdrop.

    METHODS alv_build_fcat
      RAISING cx_static_check.

    METHODS alv_build_filter
      RAISING cx_static_check.

    METHODS alv_build_variant
      RAISING cx_static_check.

    METHODS alv_build_hierarchy_hdr
      RAISING cx_static_check.

    METHODS alv_build_outtab
      RAISING cx_static_check.

    METHODS alv_build_toolbar_excl
      RAISING cx_static_check.

    METHODS alv_register_events
      IMPORTING iv_on_dflt_context_menu_req TYPE abap_bool DEFAULT abap_false
                iv_on_node_context_menu_req TYPE abap_bool DEFAULT abap_false
                iv_on_double_click          TYPE abap_bool DEFAULT abap_false
                iv_on_drag_drop             TYPE abap_bool DEFAULT abap_false.

    METHODS alv_set_table.
    METHODS alv_build_toolbar.

ENDCLASS.


CLASS zial_cl_alv_tree IMPLEMENTATION.

  METHOD constructor.

    super->constructor( EXPORTING  parent                      = io_container
                                   node_selection_mode         = iv_node_sel_mode
                                   no_html_header              = iv_no_html_header
                                   item_selection              = iv_single_item_sel
                        EXCEPTIONS cntl_error                  = 1
                                   cntl_system_error           = 2
                                   create_error                = 3
                                   lifetime_error              = 4
                                   illegal_node_selection_mode = 5
                                   failed                      = 6
                                   illegal_column_name         = 7
                                   OTHERS                      = 8 ).

    IF sy-subrc NE 0.
      RAISE EXCEPTION TYPE zcx_alv_grid_not_createable
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mo_container     = io_container.
    mo_event_handler = io_event_handler.
    mo_event_handler->set_alv_tree( me ).
    mo_dragdrop       = NEW #( ).
    mv_node_key_fname = iv_node_key_fname.

  ENDMETHOD.


  METHOD det_selected_nodes.

    get_selected_nodes( CHANGING ct_selected_nodes = rt_selected_nodes ).

  ENDMETHOD.


  METHOD set_drag_and_drop.

    CASE iv_enable.
      WHEN abap_true.
        mo_dragdrop->add( EXPORTING  flavor     = zial_cl_alv_tree_event_handler=>mc_dragdrop_flavor
                                     dragsrc    = abap_true
                                     droptarget = abap_true
                                     effect     = cl_dragdrop=>move
                          EXCEPTIONS OTHERS     = 0 ).

      WHEN abap_false.
        mo_dragdrop->remove( EXPORTING  flavor = zial_cl_alv_tree_event_handler=>mc_dragdrop_flavor
                             EXCEPTIONS OTHERS = 0 ).

    ENDCASE.

  ENDMETHOD.


  METHOD show.

    CHECK mt_fieldcatalog IS INITIAL.

    alv_build_fcat( ).

    alv_build_filter( ).

    alv_build_variant( ).

    alv_build_hierarchy_hdr( ).

    alv_build_toolbar_excl( ).

    alv_register_events( ).

    alv_build_outtab( ).

    alv_set_table( ).

    alv_build_toolbar( ).

  ENDMETHOD.


  METHOD alv_build_fcat.
  ENDMETHOD.


  METHOD alv_build_filter.
  ENDMETHOD.


  METHOD alv_build_variant.

    ms_variant = VALUE #( report   = sy-repid
                          username = sy-uname ).

  ENDMETHOD.


  METHOD alv_build_hierarchy_hdr.
  ENDMETHOD.


  METHOD alv_build_toolbar_excl.
  ENDMETHOD.


  METHOD alv_register_events.

    DATA lt_events TYPE cntl_simple_events.

    CHECK mo_event_handler IS BOUND.

    IF iv_on_dflt_context_menu_req EQ abap_true.
      APPEND VALUE #( eventid = cl_gui_simple_tree=>eventid_def_context_menu_req ) TO lt_events.
      SET HANDLER mo_event_handler->on_dflt_context_menu     FOR me.
      SET HANDLER mo_event_handler->on_dflt_context_menu_sel FOR me.
    ENDIF.

    IF iv_on_node_context_menu_req EQ abap_true.
      APPEND VALUE #( eventid = cl_gui_simple_tree=>eventid_node_context_menu_req ) TO lt_events.
      SET HANDLER mo_event_handler->on_node_context_menu     FOR me.
      SET HANDLER mo_event_handler->on_node_context_menu_sel FOR me.
    ENDIF.

    IF iv_on_double_click EQ abap_true.
      APPEND VALUE #( eventid = cl_gui_simple_tree=>eventid_node_double_click ) TO lt_events.
      SET HANDLER mo_event_handler->on_double_click FOR me.
    ENDIF.

    IF iv_on_drag_drop EQ abap_true.
      SET HANDLER mo_event_handler->on_drag          FOR me.
      SET HANDLER mo_event_handler->on_drop          FOR me.
      SET HANDLER mo_event_handler->on_drop_complete FOR me.

    ENDIF.

    IF lt_events IS NOT INITIAL.
      set_registered_events( lt_events ).
    ENDIF.

  ENDMETHOD.


  METHOD alv_build_outtab.
  ENDMETHOD.


  METHOD alv_set_table.

    FIELD-SYMBOLS <lt_outtab> TYPE STANDARD TABLE.

    ASSIGN mr_outtab->* TO <lt_outtab>.
    CHECK <lt_outtab> IS ASSIGNED.

    set_table_for_first_display( EXPORTING i_save               = zial_cl_alv=>mc_layout_save_mode-both
                                           is_variant           = ms_variant
                                           is_hierarchy_header  = ms_hierarchy_hdr
                                           it_toolbar_excluding = mt_toolbar_excl
                                 CHANGING  it_fieldcatalog      = mt_fieldcatalog
                                           it_outtab            = <lt_outtab> ).

  ENDMETHOD.


  METHOD alv_build_toolbar.
  ENDMETHOD.


  METHOD reset_fcat.

    CLEAR mt_fieldcatalog.

  ENDMETHOD.


  METHOD free.

    super->free( EXCEPTIONS OTHERS = 0 ).

    mo_container->free( EXCEPTIONS OTHERS = 0 ).

  ENDMETHOD.

ENDCLASS.

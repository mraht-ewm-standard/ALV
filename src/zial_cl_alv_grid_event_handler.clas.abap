"! <p class="shorttext synchronized">ALV: Grid Event Handler</p>
CLASS zial_cl_alv_grid_event_handler DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_fcode,
                 "! Add/New
                 add       TYPE syst_ucomm VALUE 'ALV_ADD',
                 "! Delete
                 delete    TYPE syst_ucomm VALUE 'ALV_DEL',
                 "! Change
                 change    TYPE syst_ucomm VALUE 'ALV_CHG',
                 "! Confirm
                 confirm   TYPE syst_ucomm VALUE 'ALV_CNF',
                 "! Duplicate
                 duplicate TYPE syst_ucomm VALUE 'ALV_DPL',
                 "! Select
                 select    TYPE syst_ucomm VALUE 'ALV_SEL',
               END OF mc_fcode.

    METHODS get_alv_grid
      RETURNING VALUE(ro_alv_grid) TYPE REF TO zial_cl_alv_grid.

    METHODS set_alv_grid
      IMPORTING io_alv_grid TYPE REF TO zial_cl_alv_grid.

    METHODS on_toolbar ABSTRACT FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object
                e_interactive.

    METHODS on_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS on_double_click FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column
                es_row_no.

    METHODS on_data_changed ABSTRACT FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed
                e_onf4
                e_onf4_before
                e_onf4_after
                e_ucomm.

    METHODS on_data_changed_finished ABSTRACT FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified
                et_good_cells.

    METHODS on_button_clicked ABSTRACT FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id
                es_row_no.

    METHODS on_value_help ABSTRACT FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_fieldname
                e_fieldvalue
                es_row_no
                er_event_data
                et_bad_cells
                e_display.

    METHODS on_changed_selection ABSTRACT FOR EVENT delayed_changed_sel_callback OF cl_gui_alv_grid.

    METHODS on_hotspot ABSTRACT FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no.

  PROTECTED SECTION.
    DATA mo_alv_grid TYPE REF TO zial_cl_alv_grid.

    METHODS handle_user_command ABSTRACT
      IMPORTING iv_ucomm TYPE syst_ucomm.

    METHODS on_add ABSTRACT
      RAISING cx_static_check.

    METHODS on_duplicate ABSTRACT
      IMPORTING it_selected_rows TYPE lvc_t_row
      RAISING   cx_static_check.

    METHODS on_select ABSTRACT
      IMPORTING it_selected_rows TYPE lvc_t_row
      RAISING   cx_static_check.

    METHODS on_change ABSTRACT
      IMPORTING it_selected_rows TYPE lvc_t_row
      RAISING   cx_static_check.

    METHODS on_delete ABSTRACT
      IMPORTING it_selected_rows TYPE lvc_t_row
      RAISING   cx_static_check.

    METHODS on_confirm ABSTRACT
      IMPORTING it_selected_rows TYPE lvc_t_row
      RAISING   cx_static_check.

ENDCLASS.


CLASS zial_cl_alv_grid_event_handler IMPLEMENTATION.

  METHOD get_alv_grid.

    ro_alv_grid = mo_alv_grid.

  ENDMETHOD.


  METHOD set_alv_grid.

    mo_alv_grid = io_alv_grid.

  ENDMETHOD.


  METHOD on_double_click.

    TRY.
        handle_user_command( mc_fcode-select ).

        mo_alv_grid->set_selected_rows( it_row_no = VALUE #( ( es_row_no ) ) ).

      CATCH cx_root.
        " User should redefine HANDLE_USER_COMMAND

    ENDTRY.

  ENDMETHOD.


  METHOD on_user_command.

    TRY.
        handle_user_command( e_ucomm ).
      CATCH cx_root.
        " User should redefine HANDLE_USER_COMMAND

    ENDTRY.

  ENDMETHOD.

ENDCLASS.

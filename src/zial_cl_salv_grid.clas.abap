CLASS zial_cl_salv_grid DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_title         TYPE lvc_title
                iv_start_col     TYPE i
                iv_start_row     TYPE i
                iv_width         TYPE i
                iv_height        TYPE i
                iv_sel_mode      TYPE i DEFAULT if_salv_c_selection_mode=>single
                io_event_handler TYPE REF TO zial_cl_salv_grid_evt_handler OPTIONAL
      RAISING   cx_salv_msg.

    METHODS show
      CHANGING  ct_data                 TYPE table
      RETURNING VALUE(rt_selected_rows) TYPE salv_t_row
      RAISING   cx_salv_msg.

    METHODS get_salv_table RETURNING VALUE(ro_salv_table) TYPE REF TO cl_salv_table.

    METHODS close.

    METHODS get_data
      RETURNING VALUE(rr_t_data) TYPE REF TO data.

  PROTECTED SECTION.
    DATA mo_salv_table    TYPE REF TO cl_salv_table.
    DATA mo_event_handler TYPE REF TO zial_cl_salv_grid_evt_handler.
    DATA mv_grid_title    TYPE lvc_title.
    DATA mv_sel_mode      TYPE i VALUE if_salv_c_selection_mode=>single.
    DATA mv_start_col     TYPE i.
    DATA mv_start_row     TYPE i.
    DATA mv_width         TYPE i.
    DATA mv_height        TYPE i.
    DATA mr_t_data        TYPE REF TO data.

    METHODS register_events.

    METHODS set_title.

    METHODS set_functions.

    METHODS set_selection_mode.

    METHODS create_table
      CHANGING ct_data TYPE table
      RAISING  cx_salv_msg.

    METHODS set_screen.

    METHODS get_selected_rows
      RETURNING VALUE(rt_selected_rows) TYPE salv_t_row.

ENDCLASS.


CLASS zial_cl_salv_grid IMPLEMENTATION.

  METHOD constructor.

    mv_grid_title = iv_title.
    mv_sel_mode   = iv_sel_mode.
    mv_start_col  = iv_start_col.
    mv_start_row  = iv_start_row.
    mv_width      = iv_width.
    mv_height     = iv_height.

    mo_event_handler = io_event_handler.

  ENDMETHOD.


  METHOD register_events.
  ENDMETHOD.


  METHOD show.

    mr_t_data = REF #( ct_data ).

    create_table( CHANGING ct_data = ct_data ).

    set_title( ).

    set_functions( ).

    set_selection_mode( ).

    set_screen( ).

    register_events( ).

    mo_salv_table->display( ).

    rt_selected_rows = get_selected_rows( ).

  ENDMETHOD.


  METHOD get_salv_table.

    ro_salv_table = mo_salv_table.

  ENDMETHOD.


  METHOD close.

    mo_salv_table->close_screen( ).

  ENDMETHOD.


  METHOD set_title.

    mo_salv_table->get_display_settings( )->set_list_header( mv_grid_title ).

  ENDMETHOD.


  METHOD set_functions.

    mo_salv_table->get_functions( )->set_all( abap_true ).

  ENDMETHOD.


  METHOD set_selection_mode.

    mo_salv_table->get_selections( )->set_selection_mode( mv_sel_mode ).

  ENDMETHOD.


  METHOD create_table.

    cl_salv_table=>factory( IMPORTING r_salv_table = mo_salv_table
                            CHANGING  t_table      = ct_data ).

  ENDMETHOD.


  METHOD set_screen.

    mo_salv_table->set_screen_popup( start_column = mv_start_col
                                     end_column   = ( mv_start_col + mv_width )
                                     start_line   = mv_start_row
                                     end_line     = ( mv_start_row + mv_height ) ).

  ENDMETHOD.


  METHOD get_data.

    rr_t_data = mr_t_data.

  ENDMETHOD.


  METHOD get_selected_rows.

    IF    sy-ucomm EQ space
       OR sy-ucomm EQ if_salv_c_function=>continue.
      DATA(lo_selections) = mo_salv_table->get_selections( ).
      rt_selected_rows = lo_selections->get_selected_rows( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

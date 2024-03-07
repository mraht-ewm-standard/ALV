"! <p class="shorttext synchronized">SALV: Grid Event Handler</p>
CLASS zial_cl_salv_grid_evt_handler DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS get_alv_grid
      RETURNING VALUE(ro_alv_grid) TYPE REF TO zial_cl_salv_grid.

    METHODS set_alv_grid
      IMPORTING io_alv_grid TYPE REF TO zial_cl_salv_grid.

    METHODS on_double_click ABSTRACT FOR EVENT double_click OF cl_salv_events_table
      IMPORTING !row
                !column.

    METHODS on_link_click ABSTRACT FOR EVENT link_click OF cl_salv_events_table
      IMPORTING !row
                !column.

  PROTECTED SECTION.
    DATA mo_alv_grid TYPE REF TO zial_cl_salv_grid.

ENDCLASS.


CLASS zial_cl_salv_grid_evt_handler IMPLEMENTATION.

  METHOD get_alv_grid.
    ro_alv_grid = mo_alv_grid.
  ENDMETHOD.


  METHOD set_alv_grid.
    mo_alv_grid = io_alv_grid.
  ENDMETHOD.

ENDCLASS.

"! <p class="shorttext synchronized">ABAP List Viewer</p>
CLASS zial_cl_alv DEFINITION
  PUBLIC FINAL.

  PUBLIC SECTION.
    CONSTANTS: BEGIN OF mc_layout_save_mode,
                 none        TYPE zial_de_alv_layout_save_mode VALUE space,
                 only_global TYPE zial_de_alv_layout_save_mode VALUE 'X',
                 both        TYPE zial_de_alv_layout_save_mode VALUE 'A',
                 only_user   TYPE zial_de_alv_layout_save_mode VALUE 'U',
               END OF mc_layout_save_mode.

ENDCLASS.


CLASS zial_cl_alv IMPLEMENTATION.
ENDCLASS.

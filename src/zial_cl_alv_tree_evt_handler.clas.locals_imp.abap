*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_drag_drop_obj DEFINITION FINAL.

  PUBLIC SECTION.
    DATA: mv_node_key TYPE lvc_nkey ##NEEDED. " NODE_KEY is being set in ON_DROP and read in ON_DRAG.

ENDCLASS.

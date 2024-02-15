"! <p class="shorttext synchronized">ALV: Tree Event Handler</p>
CLASS zial_cl_alv_tree_event_handler DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS mc_dragdrop_flavor TYPE cndd_flavor VALUE 'DragAndDrop'.

    CONSTANTS: BEGIN OF mc_fcode,
                 "! Tree function: Display
                 tree_display    TYPE ui_func VALUE 'TREE_DISPLAY',
                 "! Tree function: New
                 tree_new        TYPE ui_func VALUE 'TREE_NEW',
                 "! Tree function: Edit
                 tree_edit       TYPE ui_func VALUE 'TREE_EDIT',
                 "! Tree function: Delete
                 tree_delete     TYPE ui_func VALUE 'TREE_DELETE',
                 "! Toolbar button: Display
                 tree_tb_display TYPE ui_func VALUE 'TREE_TB_DISPLAY',
                 "! Toolbar button: New
                 tree_tb_new     TYPE ui_func VALUE 'TREE_TB_NEW',
                 "! Toolbar button: Edit
                 tree_tb_edit    TYPE ui_func VALUE 'TREE_TB_EDIT',
                 "! Toolbar button: Delete
                 tree_tb_delete  TYPE ui_func VALUE 'TREE_TB_DELETE',
               END OF mc_fcode.

    METHODS get_alv_tree
      RETURNING VALUE(ro_alv_tree) TYPE REF TO zial_cl_alv_tree.

    METHODS set_alv_tree
      IMPORTING io_alv_tree TYPE REF TO zial_cl_alv_tree.

    METHODS on_dflt_context_menu FOR EVENT default_context_menu_request OF cl_gui_alv_tree
      IMPORTING menu.

    METHODS on_dflt_context_menu_sel FOR EVENT default_context_menu_select OF cl_gui_alv_tree
      IMPORTING fcode.

    METHODS on_node_context_menu FOR EVENT node_context_menu_request OF cl_gui_alv_tree
      IMPORTING menu
                node_key.

    METHODS on_node_context_menu_sel FOR EVENT node_context_menu_selected OF cl_gui_alv_tree
      IMPORTING fcode
                node_key.

    METHODS on_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key.

    METHODS on_drag FOR EVENT on_drag OF cl_gui_alv_tree
      IMPORTING drag_drop_object
                fieldname
                node_key.

    METHODS on_drop FOR EVENT on_drop OF cl_gui_alv_tree
      IMPORTING drag_drop_object
                node_key.

    METHODS on_drop_complete FOR EVENT on_drop_complete OF cl_gui_alv_tree
      IMPORTING drag_drop_object
                fieldname
                node_key.

    METHODS on_function_selected FOR EVENT function_selected OF cl_gui_toolbar
      IMPORTING fcode.

  PROTECTED SECTION.
    DATA mo_alv_tree     TYPE REF TO zial_cl_alv_tree.
    DATA mv_node_key_src TYPE lvc_nkey.
    DATA mv_node_key_dst TYPE lvc_nkey.
    DATA mr_node_src     TYPE REF TO data.
    DATA mr_node_dst     TYPE REF TO data.

    METHODS handle_user_command
      IMPORTING iv_ucomm    TYPE syst_ucomm
                iv_node_key TYPE lvc_nkey OPTIONAL.

    METHODS on_drag_drop_move
      IMPORTING ir_node_src TYPE REF TO data
                ir_node_dst TYPE REF TO data.

ENDCLASS.


CLASS zial_cl_alv_tree_event_handler IMPLEMENTATION.

  METHOD get_alv_tree.

    ro_alv_tree = mo_alv_tree.

  ENDMETHOD.


  METHOD set_alv_tree.

    mo_alv_tree = io_alv_tree.

  ENDMETHOD.


  METHOD on_dflt_context_menu.
  ENDMETHOD.


  METHOD on_dflt_context_menu_sel.

    handle_user_command( fcode ).

  ENDMETHOD.


  METHOD on_node_context_menu.

    menu->clear( ).

  ENDMETHOD.


  METHOD on_node_context_menu_sel.

    handle_user_command( iv_ucomm    = fcode
                         iv_node_key = node_key ).

  ENDMETHOD.


  METHOD on_double_click.

    handle_user_command( iv_ucomm    = mc_fcode-tree_display
                         iv_node_key = node_key ).

  ENDMETHOD.


  METHOD on_drag.

    mv_node_key_src = node_key.

    DATA(lo_drag_drop_obj) = NEW lcl_drag_drop_obj( ).
    lo_drag_drop_obj->mv_node_key = node_key.

    drag_drop_object->object = lo_drag_drop_obj.

  ENDMETHOD.


  METHOD on_drop.

    mv_node_key_dst = node_key.

  ENDMETHOD.


  METHOD on_drop_complete.

    DATA lr_node_src TYPE REF TO data.
    DATA lr_node_dst TYPE REF TO data.

    FIELD-SYMBOLS <lt_outtab> TYPE STANDARD TABLE.

    CHECK drag_drop_object->flavor EQ mc_dragdrop_flavor
      AND drag_drop_object->effect EQ cl_dragdrop=>move.

    CHECK me->mo_alv_tree->mr_outtab IS BOUND.
    ASSIGN me->mo_alv_tree->mr_outtab->* TO <lt_outtab>.
    CHECK <lt_outtab> IS ASSIGNED.

    LOOP AT <lt_outtab> ASSIGNING FIELD-SYMBOL(<ls_outtab>).

      ASSIGN COMPONENT me->mo_alv_tree->mv_node_key_fname OF STRUCTURE <ls_outtab> TO FIELD-SYMBOL(<lv_node_key>).
      CHECK <lv_node_key> IS ASSIGNED.

      CASE <lv_node_key>.
        WHEN mv_node_key_src.
          lr_node_src = REF #( <ls_outtab> ).

        WHEN mv_node_key_dst.
          lr_node_dst = REF #( <ls_outtab> ).

      ENDCASE.

      IF     lr_node_src IS BOUND
         AND lr_node_dst IS BOUND.
        EXIT.
      ENDIF.

      UNASSIGN <lv_node_key>.

    ENDLOOP.

    on_drag_drop_move( ir_node_src = lr_node_src
                       ir_node_dst = lr_node_dst ).

  ENDMETHOD.


  METHOD on_function_selected.

    handle_user_command( fcode ).

  ENDMETHOD.


  METHOD handle_user_command.
  ENDMETHOD.


  METHOD on_drag_drop_move.
  ENDMETHOD.

ENDCLASS.

"! <p class="shorttext synchronized">ALV: Table creator</p>
"! This class proposes alternate methods to avoid the short dump
"! GENERATE_SUBPOOL_DIR_FULL which may occur in the following standard objects:<ul>
"! <li>Method CREATE_DYNAMIC_TABLE of class CL_ALV_TABLE_CREATE</li>
"! <li>Function module REUSE_ALV_TABLE_CREATE</li>
"! <li>Function module LVC_TABLE_CREATE</li>
"! <li>Function module ALV_TABLE_CREATE</li>
"! </ul>
"!
"! <p>The replacement methods have the same names as the replaced object, and the same parameters (as seen in 7.52).
"! Only import parameter iv_STYLE_TABLE has been changed to iv_STYLE_TABNAME to enable the developer to set a custom
"! name for the style table.</p>
"!
"! <p>Here are the call stacks for each method:</p>
"! <ul>
"! <li>CREATE_DYNAMIC_TABLE -&gt; LVC_TABLE_CREATE</li>
"! <li>LVC_TABLE_CREATE -&gt; ALV_TABLE_CREATE</li>
"! <li>REUSE_ALV_TABLE_CREATE -&gt; ALV_TABLE_CREATE</li>
"! <li>ALV_TABLE_CREATE -&gt; FB_TABLE_CREATE_STRING</li>
"! </ul>
"! <p>The main logic to build the internal table is in the method FB_TABLE_CREATE_STRING.</p>
"!
CLASS zial_cl_alv_table_create DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    "! Alternative to method CREATE_DYNAMIC_TABLE of class CL_ALV_TABLE_CREATE,
    "! which avoids the short dump GENERATE_SUBPOOL_DIR_FULL by using RTTC.<br/>
    "! It has exactly the same parameters (as of 7.52), even the exception
    "! GENERATE_SUBPOOL_DIR_FULL but it's never raised.<br/>
    "! (calls LVC_TABLE_CREATE with parameters 'ZCL_ALV_TABLE_CREATE_RTTC' and 'INTERNAL_RECEIVER')
    "!
    "! @parameter iv_style_tabname          | Style table name
    "! @parameter it_fieldcatalog           | Fieldcatalog
    "! @parameter iv_length_in_byte         | Length in bytes
    "! @parameter er_table                  | Table
    "! @parameter ev_style_fname            | Style field name
    "! @exception generate_subpool_dir_full | Dynamic table couldn't be created
    CLASS-METHODS create_dynamic_table
      IMPORTING  iv_style_tabname  TYPE kkblo_tabname OPTIONAL
                 it_fieldcatalog   TYPE lvc_t_fcat
                 iv_length_in_byte TYPE boolean       OPTIONAL
      EXPORTING  er_table          TYPE REF TO data
                 ev_style_fname    TYPE lvc_fname
      EXCEPTIONS generate_subpool_dir_full.

    "! Replacement of function module REUSE_ALV_TABLE_CREATE<br/>
    "! (calls the function module REUSE_ALV_TRANSFER_DATA and then the method FB_TABLE_CREATE_STRING)
    "!
    "! @parameter it_fieldcat               | Fieldcatalog
    "! @parameter iv_callback_program       | Callback report name
    "! @parameter iv_formname               | Form routine name
    "! @exception generate_subpool_dir_full | Dynamic table couldn't be created
    CLASS-METHODS reuse_alv_table_create
      IMPORTING  it_fieldcat         TYPE slis_t_fieldcat_alv
                 iv_callback_program TYPE sy-repid
                 iv_formname         TYPE char30
      EXCEPTIONS generate_subpool_dir_full.

    "! Replacement of function module ALV_TABLE_CREATE
    "! (calls the method FB_TABLE_CREATE_STRING)
    "!
    "! @parameter iv_callback_program       | Callback report name
    "! @parameter iv_formname               | Form routine name
    "! @parameter iv_tabname                | Table name
    "! @parameter iv_style_tabname          | Style table name
    "! @parameter iv_oo_class_reference     | Class reference
    "! @parameter iv_oo_class_name          | Class name
    "! @parameter iv_oo_method              | Method name
    "! @parameter iv_length_in_byte         | Length in bytes
    "! @parameter ct_fieldcat               | Fieldcatalog
    "! @exception generate_subpool_dir_full | Dynamic table couldn't be created
    CLASS-METHODS alv_table_create
      IMPORTING  iv_callback_program   TYPE sy-repid
                 iv_formname           TYPE char30
                 iv_tabname            TYPE kkblo_tabname DEFAULT '1'
                 iv_style_tabname      TYPE kkblo_tabname OPTIONAL
                 iv_oo_class_reference TYPE REF TO object OPTIONAL
                 iv_oo_class_name      TYPE c             OPTIONAL
                 iv_oo_method          TYPE c             OPTIONAL
                 iv_length_in_byte     TYPE boolean       OPTIONAL
      CHANGING   ct_fieldcat           TYPE kkblo_t_fieldcat
      EXCEPTIONS generate_subpool_dir_full.

    "! Replacement of function module LVC_TABLE_CREATE<br/>
    "! (calls the function module LVC_TRANSFER_TO_KKBLO and then the method ALV_TABLE_CREATE)
    "!
    "! @parameter it_fieldcat               | Fieldcatalog
    "! @parameter iv_callback_program       | Callback report name
    "! @parameter iv_formname               | Form routine name
    "! @parameter iv_style_tabname          | Style table name
    "! @parameter iv_oo_class_reference     | Class reference
    "! @parameter iv_oo_class_name          | Class name
    "! @parameter iv_oo_method              | Method name
    "! @parameter iv_length_in_byte         | Length in bytes
    "! @exception generate_subpool_dir_full | Dynamic table couldn't be created
    CLASS-METHODS lvc_table_create
      IMPORTING  it_fieldcat           TYPE lvc_t_fcat
                 iv_callback_program   TYPE sy-repid      OPTIONAL
                 iv_formname           TYPE char30        OPTIONAL
                 iv_style_tabname      TYPE kkblo_tabname OPTIONAL
                 iv_oo_class_reference TYPE REF TO object OPTIONAL
                 iv_oo_class_name      TYPE c             OPTIONAL
                 iv_oo_method          TYPE c             OPTIONAL
                 iv_length_in_byte     TYPE boolean       OPTIONAL
      EXCEPTIONS generate_subpool_dir_full.

    "! Creates the internal table via RTTI (note: the ALV catalog is also completed if needed)<br/>
    "! It's an adaptation of the standard subroutine FB_TABLE_CREATE_STRING of program SAPLSKBH.
    "!
    "! @parameter iv_form                   | Form routine
    "! @parameter iv_program                | Program name
    "! @parameter io_oo_class               | Class reference
    "! @parameter iv_oo_class_name          | Class name
    "! @parameter iv_oo_method              | Method name
    "! @parameter iv_style_tabname          | Style table name
    "! @parameter iv_tabname                | Table name
    "! @parameter iv_length_in_byte         | Length in bytes
    "! @parameter ct_fieldcat               | Fieldcatalog
    "! @exception generate_subpool_dir_full | Dynamic table couldn't be created
    CLASS-METHODS fb_table_create_string
      IMPORTING  iv_form           TYPE c
                 iv_program        TYPE syrepid
                 io_oo_class       TYPE REF TO object
                 iv_oo_class_name  TYPE c
                 iv_oo_method      TYPE c
                 iv_style_tabname  TYPE kkblo_tabname
                 iv_tabname        TYPE kkblo_tabname
                 iv_length_in_byte TYPE boolean
      CHANGING   ct_fieldcat       TYPE kkblo_t_fieldcat
      EXCEPTIONS generate_subpool_dir_full.

  PRIVATE SECTION.
    CONSTANTS mc_class_name TYPE clasname VALUE 'ZIAL_CL_ALV_TABLE_CREATE_RTTC'.

    CLASS-DATA mr_table TYPE REF TO data.

    CLASS-METHODS internal_receiver
      IMPORTING ir_table TYPE REF TO data.

ENDCLASS.


CLASS zial_cl_alv_table_create IMPLEMENTATION.

  METHOD alv_table_create.

    fb_table_create_string( EXPORTING  iv_form                   = iv_formname
                                       iv_program                = iv_callback_program
                                       io_oo_class               = iv_oo_class_reference
                                       iv_oo_class_name          = iv_oo_class_name
                                       iv_oo_method              = iv_oo_method
                                       iv_style_tabname          = iv_style_tabname
                                       iv_tabname                = iv_tabname
                                       iv_length_in_byte         = iv_length_in_byte
                            CHANGING   ct_fieldcat               = ct_fieldcat[]
                            EXCEPTIONS generate_subpool_dir_full = 1 ).
    IF sy-subrc EQ 1.
      RAISE generate_subpool_dir_full.
    ENDIF.

  ENDMETHOD.


  METHOD create_dynamic_table.

    DATA lv_oo_method TYPE c LENGTH 80.

    lv_oo_method = 'INTERNAL_RECEIVER'.

    lvc_table_create( EXPORTING  it_fieldcat               = it_fieldcatalog
                                 iv_style_tabname          = iv_style_tabname
                                 iv_length_in_byte         = iv_length_in_byte
                                 iv_oo_class_name          = mc_class_name
                                 iv_oo_method              = lv_oo_method
                      EXCEPTIONS generate_subpool_dir_full = 1 ).
    IF sy-subrc EQ 1.
      RAISE generate_subpool_dir_full.
    ENDIF.

    er_table       = mr_table.
    ev_style_fname = iv_style_tabname.

  ENDMETHOD.


  METHOD fb_table_create_string.

    DATA ls_fieldcat    TYPE kkblo_fieldcat.
    DATA lv_message     TYPE c LENGTH 240.

    DATA lv_length      TYPE lvc_outlen. " Y9CK020977

    DATA lt_comp        TYPE cl_abap_structdescr=>component_table.
    DATA ls_comp        LIKE LINE OF lt_comp.
    DATA lp_struc_descr TYPE REF TO cl_abap_structdescr.
    DATA lp_tab_descr   TYPE REF TO cl_abap_tabledescr.
    " <<< YI3K081678 get invalid signs to check fieldnames
    DATA lv_space       TYPE string.
    DATA lv_fieldname   TYPE string.
    DATA lv_field_mess  TYPE string.
    DATA lv_subrc       TYPE sy-subrc.
    DATA lv_leng        TYPE lvc_outlen.
    DATA lr_table       TYPE REF TO data.
    DATA lx_root        TYPE REF TO cx_root.

    FIELD-SYMBOLS <lt_gentab> TYPE STANDARD TABLE.

    CALL FUNCTION 'K_KKB_FIELDCAT_COMPLETE'
      EXPORTING  i_tabname   = iv_tabname
      CHANGING   ct_fieldcat = ct_fieldcat[]
      EXCEPTIONS OTHERS      = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lv_space = cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).
    CONCATENATE lv_space ':' INTO lv_space.
    CONCATENATE lv_space ';' INTO lv_space.
    CONCATENATE lv_space '.' INTO lv_space.
    " >>> YI3K081678

    SORT ct_fieldcat BY col_pos ASCENDING.

    LOOP AT ct_fieldcat INTO ls_fieldcat.

      lv_fieldname = ls_fieldcat-fieldname.
      IF lv_fieldname CA lv_space.

        lv_field_mess = lv_fieldname.
        lv_subrc = '1'.

      ELSE.

        ls_comp-name = ls_fieldcat-fieldname.

        IF ls_fieldcat-ref_fieldname IS NOT INITIAL.
          ls_comp-type ?= cl_abap_typedescr=>describe_by_name(
                              ls_fieldcat-ref_tabname && '-' && ls_fieldcat-ref_fieldname ).
        ELSE.
          IF ls_fieldcat-datatype EQ 'CHAR'.                 " Y6AK044662

            " >>Y6AK037383
            IF iv_length_in_byte EQ abap_true.
              ls_fieldcat-intlen = ls_fieldcat-intlen / cl_abap_char_utilities=>charsize.
            ENDIF.
            " <<Y6AK037383

            IF ls_fieldcat-ddic_outputlen GT 0.  ">>B20K8A0Q9G
              lv_length = ls_fieldcat-ddic_outputlen.        " Y6AK044662
            ENDIF.

            IF ls_fieldcat-intlen GT 0.
              IF ls_fieldcat-ddic_outputlen GT ls_fieldcat-intlen.
                lv_length = ls_fieldcat-ddic_outputlen.
              ELSE.
                lv_length = ls_fieldcat-intlen.
              ENDIF.
            ENDIF.

            IF lv_length EQ 0.
              lv_length = ls_fieldcat-outputlen.
            ENDIF.  "<<B20K8A0Q9G

            ls_comp-type = cl_abap_elemdescr=>get_c( lv_length + 0 ).

          ELSEIF ls_fieldcat-datatype EQ 'NUMC'.             " Y6AK044662

            IF iv_length_in_byte EQ abap_true.
              ls_fieldcat-intlen = ls_fieldcat-intlen / cl_abap_char_utilities=>charsize.
            ENDIF.

            IF ls_fieldcat-ddic_outputlen GT 0.
              lv_length = ls_fieldcat-ddic_outputlen.
            ENDIF.

            IF ls_fieldcat-intlen GT 0.
              IF ls_fieldcat-ddic_outputlen GT ls_fieldcat-intlen.
                lv_length = ls_fieldcat-ddic_outputlen.
              ELSE.
                lv_length = ls_fieldcat-intlen.
              ENDIF.
            ENDIF.

            IF lv_length EQ 0.
              lv_length = ls_fieldcat-outputlen.
            ENDIF.  "<<B20K8A0Q9G

            ls_comp-type = cl_abap_elemdescr=>get_n( lv_length + 0 ).

          ELSEIF ls_fieldcat-datatype EQ 'CURR'.

            ls_comp-type = cl_abap_elemdescr=>get_p( p_length   = 8
                                                     p_decimals = 2 ).

          ELSEIF ls_fieldcat-datatype EQ 'INT1'
              OR ls_fieldcat-inttype  EQ 'B'
              OR ls_fieldcat-inttype  EQ 'b'.
            ls_fieldcat-inttype = 'I'.

            ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'INT1' ).

          ELSEIF ls_fieldcat-datatype EQ 'INT2'
              OR ls_fieldcat-inttype  EQ 's'.
            ls_fieldcat-inttype = 'I'.

          ELSE.
            IF     ls_fieldcat-intlen  IS NOT INITIAL
               AND ls_fieldcat-inttype NE 'g'.              " B20K8A0MOD

              IF     ls_fieldcat-inttype EQ 'F'
                 AND ls_fieldcat-intlen  NE 8.
                ls_fieldcat-intlen = 8.
              ENDIF.

              IF     ls_fieldcat-inttype EQ 'D'
                 AND ls_fieldcat-intlen  NE 8.
                ls_fieldcat-intlen = 8.
              ENDIF.
              IF     ls_fieldcat-inttype EQ 'T'              " Y7AK018607
                 AND ls_fieldcat-intlen  NE 6.
                ls_fieldcat-intlen = 6.
              ENDIF.
              IF ls_fieldcat-inttype EQ 'P'.
                IF iv_length_in_byte EQ abap_true.
                  lv_leng = ls_fieldcat-intlen.
                ELSE.
                  lv_leng = ( ls_fieldcat-intlen + 1 ) / 2.
                ENDIF.
              ENDIF.
            ENDIF.

            IF ls_fieldcat-inttype EQ 'g'.                  " B20K8A0MOD

              ls_comp-type = cl_abap_elemdescr=>get_string( ).
            ELSE.
              CASE ls_fieldcat-inttype.
                WHEN 'C'.
                  ls_comp-type = cl_abap_elemdescr=>get_c( ls_fieldcat-intlen + 0 ).
                WHEN 'D'.
                  ls_comp-type = cl_abap_elemdescr=>get_d( ).
                WHEN 'F'.
                  ls_comp-type = cl_abap_elemdescr=>get_f( ).
                WHEN 'I'.
                  ls_comp-type = cl_abap_elemdescr=>get_i( ).
                WHEN 'P'.
                  ls_comp-type = cl_abap_elemdescr=>get_p( p_length   = lv_leng + 0
                                                           p_decimals = 2 ).
                WHEN 'T'.
                  ls_comp-type = cl_abap_elemdescr=>get_t( ).
                WHEN 'X'.
                  ls_comp-type = cl_abap_elemdescr=>get_x( ls_fieldcat-intlen + 0 ).
              ENDCASE.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

      APPEND ls_comp TO lt_comp.

    ENDLOOP.

    " <<< YI3K081678 single (!) message incorrect fieldname
    IF lv_subrc EQ 1.
      MESSAGE i538(0k) WITH lv_field_mess 'FIELDNAME'.
      CLEAR lv_subrc.
    ENDIF.
    " >>> YI3K081678

    IF iv_style_tabname IS NOT INITIAL.
      CLEAR ls_comp.
      ls_comp-name  = iv_style_tabname.
      ls_comp-type ?= cl_abap_typedescr=>describe_by_name( 'LVC_T_STYL' ).
      APPEND ls_comp TO lt_comp.
    ENDIF.

    TRY.
        lp_struc_descr = cl_abap_structdescr=>create( p_components = lt_comp
                                                      p_strict     = ' ' ).

        lp_tab_descr = cl_abap_tabledescr=>create( p_line_type  = lp_struc_descr
                                                   p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA lr_table TYPE HANDLE lp_tab_descr.

      CATCH cx_root INTO lx_root.
        lv_message = lx_root->get_text( ).
        MESSAGE x000(0k) WITH lv_message space space space.
    ENDTRY.

    IF iv_oo_class_name IS INITIAL.
      ASSIGN lr_table->* TO <lt_gentab>.
      PERFORM (iv_form) IN PROGRAM (iv_program) TABLES <lt_gentab>.
    ELSEIF io_oo_class IS BOUND.
      CALL METHOD io_oo_class->(iv_oo_method)
        EXPORTING ir_table = lr_table.
    ELSE.
      CALL METHOD (iv_oo_method)
        EXPORTING ir_table = lr_table.
    ENDIF.

  ENDMETHOD.


  METHOD internal_receiver.

    mr_table = ir_table.

  ENDMETHOD.


  METHOD lvc_table_create.

    DATA lt_fieldcat_lvc TYPE kkblo_t_fieldcat.

    CALL FUNCTION 'LVC_TRANSFER_TO_KKBLO'
      EXPORTING it_fieldcat_lvc   = it_fieldcat
      IMPORTING et_fieldcat_kkblo = lt_fieldcat_lvc.

    alv_table_create( EXPORTING  iv_callback_program       = iv_callback_program
                                 iv_formname               = iv_formname
                                 iv_style_tabname          = iv_style_tabname
                                 iv_length_in_byte         = iv_length_in_byte
                                 iv_oo_class_reference     = iv_oo_class_reference
                                 iv_oo_class_name          = iv_oo_class_name
                                 iv_oo_method              = iv_oo_method
                                 iv_tabname                = '1'
                      CHANGING   ct_fieldcat               = lt_fieldcat_lvc
                      EXCEPTIONS generate_subpool_dir_full = 1 ).
    IF sy-subrc EQ 1.
      RAISE generate_subpool_dir_full.
    ENDIF.

  ENDMETHOD.


  METHOD reuse_alv_table_create.

    DATA lt_fieldcat_lvc TYPE kkblo_t_fieldcat.

    CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'
      EXPORTING  it_fieldcat = it_fieldcat
      IMPORTING  et_fieldcat = lt_fieldcat_lvc
      EXCEPTIONS OTHERS      = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    alv_table_create( EXPORTING  iv_callback_program       = iv_callback_program
                                 iv_formname               = iv_formname
                                 iv_tabname                = '1'
                      CHANGING   ct_fieldcat               = lt_fieldcat_lvc
                      EXCEPTIONS generate_subpool_dir_full = 1 ).
    IF sy-subrc EQ 1.
      RAISE generate_subpool_dir_full.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

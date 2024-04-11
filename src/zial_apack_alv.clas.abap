"! <p class="shorttext synchronized">Metadata: ABAP List Viewer (ALV)</p>
CLASS zial_apack_alv DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_apack_manifest.

    METHODS constructor.

ENDCLASS.


CLASS zial_apack_alv IMPLEMENTATION.

  METHOD constructor.

    if_apack_manifest~descriptor-group_id     = 'c-a-s.de'.
    if_apack_manifest~descriptor-artifact_id  = 'ewm-alv'.
    if_apack_manifest~descriptor-version      = '15.02.2024.001-rc'.
    if_apack_manifest~descriptor-git_url      = 'https://github.com/mraht-ewm-standard/ALV.git' ##NO_TEXT.

    if_apack_manifest~descriptor-dependencies = VALUE #(
        ( group_id       = 'c-a-s.de'
          artifact_id    = 'ewm-dev-basis'
          target_package = 'ZIAL_DEV_BASIS'
          git_url        = 'https://github.com/mraht-ewm-standard/DEV_BASIS.git' )
        ( group_id       = 'c-a-s.de'
          artifact_id    = 'ewm-exc-mgmt'
          target_package = 'ZIAL_EXC_MGMT'
          git_url        = 'https://github.com/mraht-ewm-standard/EXC_MGMT.git' ) ) ##NO_TEXT.

  ENDMETHOD.

ENDCLASS.

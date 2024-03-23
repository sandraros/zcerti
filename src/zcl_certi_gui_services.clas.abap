CLASS zcl_certi_gui_services DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zcl_certi_gui_services.

    METHODS gui_download_asc
      IMPORTING
        file_path           TYPE csequence
        VALUE(file_content) TYPE STANDARD TABLE.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_certi_gui_services IMPLEMENTATION.
  METHOD create.
    result = NEW #( ).
  ENDMETHOD.

  METHOD gui_download_asc.
    cl_gui_frontend_services=>gui_download(
      EXPORTING
        filename = CONV #( file_path )
        filetype = 'ASC' ##NO_TEXT
      CHANGING
        data_tab = file_content
      EXCEPTIONS
        OTHERS   = 17 ).
  ENDMETHOD.
ENDCLASS.

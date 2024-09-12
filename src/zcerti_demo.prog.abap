*&---------------------------------------------------------------------*
*& Report zcerapi_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zcerti_demo.

PARAMETERS p_url TYPE string LOWER CASE DEFAULT `https://github.com`.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_file AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT (60) TEXT-c01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME." TITLE text-B01.
  PARAMETERS p_dir TYPE string MODIF ID rb1 LOWER CASE DEFAULT `C:\temp`.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_strust AS CHECKBOX.
  SELECTION-SCREEN COMMENT (60) TEXT-c02.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME." TITLE text-B02.
  PARAMETERS p_contxt TYPE psecontext MODIF ID rb2 DEFAULT 'SSLC'.
  PARAMETERS p_applic TYPE ssfapplic-applic MODIF ID rb2 DEFAULT 'ANONYM'.
SELECTION-SCREEN END OF BLOCK b02.

START-OF-SELECTION.
  DATA(icm_trace_api) = zcl_certi_icm_trace=>create( ).

  " Get current ICM trace level for later restore
  DATA(original_trace_level) = icm_trace_api->get_trace_level( ).

  " Set the ICM trace level to 3 to obtain the contents of certificates
  icm_trace_api->set_trace_level( '3' ).

  " Clear the trace
  icm_trace_api->delete_trace( ).

  "===================================
  " HTTPS GET
  "===================================
  DATA lo_http_client TYPE REF TO if_http_client.
*  cl_http_client=>create_by_destination( EXPORTING  destination = 'RFC_DESTINATION'
*                                         IMPORTING  client      = lo_http_client
*                                         EXCEPTIONS OTHERS      = 1 ).
  cl_http_client=>create_by_url( EXPORTING  url    = p_url
                                 IMPORTING  client = lo_http_client
                                 EXCEPTIONS OTHERS = 1 ).

  DATA request TYPE REF TO if_http_request.
  request = lo_http_client->request.
  request->set_method( 'GET' ).
  request->set_version( if_http_request=>co_protocol_version_1_1 ). " HTTP 1.0 or 1.1
  lo_http_client->send( EXCEPTIONS OTHERS = 1 ).
  DATA l_return_code TYPE i.
  DATA l_content     TYPE string.
  lo_http_client->receive( EXCEPTIONS OTHERS = 1 ).
  lo_http_client->response->get_status( IMPORTING code = l_return_code ).
  l_content = lo_http_client->response->get_cdata( ).
  lo_http_client->close( EXCEPTIONS OTHERS = 1 ).

  " Get and parse the ICM trace
  DATA(parsed_icm_trace) = icm_trace_api->get_parsed_trace( ).

  " Restore original ICM trace level
  icm_trace_api->set_trace_level( original_trace_level ).

  " Save the certificates
  IF p_file = abap_true.
    DATA(gui_services) = zcl_certi_gui_services=>create( ).
    LOOP AT parsed_icm_trace-certificates REFERENCE INTO DATA(certificate).
      gui_services->gui_download_asc( file_path    = |{ p_dir }\\Certificate_{ sy-tabix }.cer|
                                      file_content = certificate->lines ).
    ENDLOOP.
  ENDIF.

  IF p_strust = abap_true.
    TRY.
        DATA(strust) = NEW zcl_certi_strust( iv_context = p_contxt
                                             iv_applic  = p_applic ).
        " initialize MV_TEMPFILE
        strust->load( ).
        " initialize MV_PROFILE from MV_TEMPFILE
        strust->get_own_certificate( ).
        " get list only after MV_PROFILE is initialized
        DATA(certificates) = strust->get_certificate_list( ).
      CATCH zcx_certi_strust INTO DATA(strust_error).
        MESSAGE strust_error TYPE 'I' DISPLAY LIKE 'E'.
        STOP.
    ENDTRY.
    TRY.
        LOOP AT parsed_icm_trace-certificates REFERENCE INTO certificate.
          strust->add( CONV #( certificate->lines ) ).
        ENDLOOP.
        strust->update( ).
        COMMIT WORK.
      CATCH zcx_certi_strust INTO strust_error.
        MESSAGE strust_error TYPE 'I' DISPLAY LIKE 'E'.
        STOP.
    ENDTRY.
  ENDIF.

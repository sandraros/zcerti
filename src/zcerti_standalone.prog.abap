"! All-in-one copy of https://github.com/sandraros/zcerti
"! Copy/paste this if you can't connect to GitHub
"!
"! MIT License
"!
"! Copyright (c) 2024 sandraros
"!
"! Permission is hereby granted, free of charge, to any person obtaining a copy
"! of this software and associated documentation files (the "Software"), to deal
"! in the Software without restriction, including without limitation the rights
"! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
"! copies of the Software, and to permit persons to whom the Software is
"! furnished to do so, subject to the following conditions:
"!
"! The above copyright notice and this permission notice shall be included in all
"! copies or substantial portions of the Software.
"!
"! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
"! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
"! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
"! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
"! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
"! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
"! SOFTWARE.
REPORT zcerti_standalone.

PARAMETERS p_url TYPE string LOWER CASE DEFAULT `https://github.com`.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_file AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN COMMENT (60) TEXT-c01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME." TITLE text-B01.
  PARAMETERS p_dir TYPE string MODIF ID rb1 LOWER CASE.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF LINE.
  PARAMETERS p_strust AS CHECKBOX.
  SELECTION-SCREEN COMMENT (60) TEXT-c02.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME." TITLE text-B02.
  PARAMETERS p_contxt TYPE psecontext MODIF ID rb2 DEFAULT 'SSLC'.
  PARAMETERS p_applic TYPE ssfapplic-applic MODIF ID rb2 DEFAULT 'ANONYM'.
SELECTION-SCREEN END OF BLOCK b02.


CLASS zcx_certi_strust DEFINITION
  INHERITING FROM cx_static_check
  CREATE PUBLIC.

************************************************************************
* Trust Management Error
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg.
    INTERFACES if_t100_message.

    CLASS-DATA null TYPE string.

    METHODS constructor
      IMPORTING
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL
        !msgv1    TYPE symsgv OPTIONAL
        !msgv2    TYPE symsgv OPTIONAL
        !msgv3    TYPE symsgv OPTIONAL
        !msgv4    TYPE symsgv OPTIONAL.

    "! Raise exception with text
    "! @parameter iv_text | Text
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_certi_strust | Exception
    CLASS-METHODS raise
      IMPORTING
        !iv_text     TYPE clike
        !ix_previous TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_certi_strust.

    "! Raise exception with T100 message
    "! <p>
    "! Will default to sy-msg* variables. These need to be set right before calling this method.
    "! </p>
    "! @parameter iv_msgid | Message ID
    "! @parameter iv_msgno | Message number
    "! @parameter iv_msgv1 | Message variable 1
    "! @parameter iv_msgv2 | Message variable 2
    "! @parameter iv_msgv3 | Message variable 3
    "! @parameter iv_msgv4 | Message variable 4
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_certi_strust | Exception
    CLASS-METHODS raise_t100
      IMPORTING
        VALUE(iv_msgid) TYPE symsgid DEFAULT sy-msgid
        VALUE(iv_msgno) TYPE symsgno DEFAULT sy-msgno
        VALUE(iv_msgv1) TYPE symsgv DEFAULT sy-msgv1
        VALUE(iv_msgv2) TYPE symsgv DEFAULT sy-msgv2
        VALUE(iv_msgv3) TYPE symsgv DEFAULT sy-msgv3
        VALUE(iv_msgv4) TYPE symsgv DEFAULT sy-msgv4
        !ix_previous    TYPE REF TO cx_root OPTIONAL
      RAISING
        zcx_certi_strust.

    "! Raise with text from previous exception
    "! @parameter ix_previous | Previous exception
    "! @raising zcx_certi_strust | Exception
    CLASS-METHODS raise_with_text
      IMPORTING
        !ix_previous TYPE REF TO cx_root
      RAISING
        zcx_certi_strust.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS c_generic_error_msg TYPE string VALUE `An error occured`.

    CLASS-METHODS split_text_to_symsg
      IMPORTING
        !iv_text      TYPE string
      RETURNING
        VALUE(rs_msg) TYPE symsg.

ENDCLASS.



CLASS zcx_certi_strust IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    if_t100_dyn_msg~msgv1 = msgv1.
    if_t100_dyn_msg~msgv2 = msgv2.
    if_t100_dyn_msg~msgv3 = msgv3.
    if_t100_dyn_msg~msgv4 = msgv4.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

  ENDMETHOD.


  METHOD raise.

    DATA:
      lv_text TYPE string,
      ls_msg  TYPE symsg.

    IF iv_text IS INITIAL.
      lv_text = c_generic_error_msg.
    ELSE.
      lv_text = iv_text.
    ENDIF.

    ls_msg = split_text_to_symsg( lv_text ).

    " Set syst variables using generic error message
    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO null.

    raise_t100( ix_previous = ix_previous ).

  ENDMETHOD.


  METHOD raise_t100.

    DATA ls_t100_key TYPE scx_t100key.

    IF iv_msgid IS NOT INITIAL.
      ls_t100_key-msgid = iv_msgid.
      ls_t100_key-msgno = iv_msgno.
      ls_t100_key-attr1 = 'IF_T100_DYN_MSG~MSGV1'.
      ls_t100_key-attr2 = 'IF_T100_DYN_MSG~MSGV2'.
      ls_t100_key-attr3 = 'IF_T100_DYN_MSG~MSGV3'.
      ls_t100_key-attr4 = 'IF_T100_DYN_MSG~MSGV4'.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_certi_strust
      EXPORTING
        textid   = ls_t100_key
        msgv1    = iv_msgv1
        msgv2    = iv_msgv2
        msgv3    = iv_msgv3
        msgv4    = iv_msgv4
        previous = ix_previous.

  ENDMETHOD.


  METHOD raise_with_text.

    raise(
      iv_text     = ix_previous->get_text( )
      ix_previous = ix_previous ).

  ENDMETHOD.


  METHOD split_text_to_symsg.

    CONSTANTS:
      lc_length_of_msgv           TYPE i VALUE 50,
      lc_offset_of_last_character TYPE i VALUE 49.

    DATA:
      lv_text    TYPE c LENGTH 200,
      lv_rest    TYPE c LENGTH 200,
      lv_msg_var TYPE c LENGTH lc_length_of_msgv,
      lv_index   TYPE sy-index.

    lv_text = iv_text.

    DO 4 TIMES.

      lv_index = sy-index.

      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = lc_length_of_msgv
          text   = lv_text
        IMPORTING
          line   = lv_msg_var
          rest   = lv_rest.

      IF lv_msg_var+lc_offset_of_last_character(1) = space OR lv_text+lc_length_of_msgv(1) = space.
        " keep the space at the beginning of the rest
        " because otherwise it's lost
        lv_rest = | { lv_rest }|.
      ENDIF.

      lv_text = lv_rest.

      CASE lv_index.
        WHEN 1.
          rs_msg-msgv1 = lv_msg_var.
        WHEN 2.
          rs_msg-msgv2 = lv_msg_var.
        WHEN 3.
          rs_msg-msgv3 = lv_msg_var.
        WHEN 4.
          rs_msg-msgv4 = lv_msg_var.
      ENDCASE.

    ENDDO.

  ENDMETHOD.
ENDCLASS.


CLASS zcx_certi DEFINITION
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcx_certi IMPLEMENTATION.
ENDCLASS.


CLASS zcl_certi_gui_services DEFINITION
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


*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

"! Constants taken from the standard include ICMDEF
INTERFACE lif_icmdef.

  CONSTANTS dp_plugin_op_chng_param TYPE i VALUE 8.

*---------------------------------------------------------------------
* Rueckgabewerte
* Sind mit icxx.h abzustimmen
* ...
*---------------------------------------------------------------------
  CONSTANTS icmeok       LIKE sy-subrc VALUE 0.
  CONSTANTS icmenotavail LIKE sy-subrc VALUE -6.

ENDINTERFACE.

"! Constants taken from the standard include TSKHINCL
INTERFACE lif_tskhincl.
***INCLUDE TSKHINCL.

*-------------------------------------------------------------------*/
* Constants for calls of the taskhandler-C-functions
*-------------------------------------------------------------------*/


*-------------------------------------------------------------------*/
* reference fields
*-------------------------------------------------------------------*/
  DATA th_opcode(1) TYPE x.
*--------------------------------------------------------------------*/
* Constants for calling function ThSysInfo
*--------------------------------------------------------------------*/
  ##NEEDED
  CONSTANTS opcode_icm LIKE th_opcode VALUE 35.
ENDINTERFACE.


CLASS zcl_certi_icm_trace DEFINITION
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.
    TYPES tt_icm_lines TYPE STANDARD TABLE OF icm_lines WITH DEFAULT KEY.
    TYPES tv_certificate_line  TYPE c LENGTH 64.
    TYPES tt_certificate_line TYPE STANDARD TABLE OF tv_certificate_line WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_certificate,
        lines TYPE tt_certificate_line,
      END OF ts_certificate.
    TYPES tt_certificate TYPE STANDARD TABLE OF ts_certificate WITH DEFAULT KEY.
    TYPES:
      BEGIN OF ts_parsed_trace,
        certificates TYPE tt_certificate,
      END OF ts_parsed_trace.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zcl_certi_icm_trace.

    METHODS delete_trace.

    METHODS get_parsed_trace
      RETURNING
        VALUE(result) TYPE ts_parsed_trace.

    METHODS get_raw_trace
      RETURNING
        VALUE(result) TYPE tt_icm_lines.

    METHODS get_trace_level
      RETURNING
        VALUE(result) TYPE icm_info-trace_lvl.

    METHODS set_trace_level
      IMPORTING
        value TYPE icm_info-trace_lvl DEFAULT 0.

  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES tt_icm_sinfo2 TYPE STANDARD TABLE OF icm_sinfo2 WITH DEFAULT KEY.

ENDCLASS.



CLASS zcl_certi_icm_trace IMPLEMENTATION.


  METHOD create.
    result = NEW #( ).
  ENDMETHOD.


  METHOD delete_trace.
    CALL FUNCTION 'ICM_RESET_TRACE'
      EXCEPTIONS
        icm_op_failed      = 1
        icm_not_authorized = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_parsed_trace.
    TYPES:
      BEGIN OF ts_trace_line,
        thread TYPE string,
        text   TYPE string,
      END OF ts_trace_line.
    TYPES tt_trace_line TYPE STANDARD TABLE OF ts_trace_line WITH EMPTY KEY.

    DATA(raw_trace) = get_raw_trace( ).

    FIND ALL OCCURRENCES OF REGEX `^\[Thr ([^\]]+)\] +([^ ].*)$`
        IN TABLE raw_trace
        RESULTS DATA(matches).

    DATA(trace_lines) = VALUE tt_trace_line( ).
    LOOP AT matches REFERENCE INTO DATA(match).
      DATA(raw_trace_line) = REF #( raw_trace[ match->line ] ).
      INSERT VALUE #( thread = substring( val = raw_trace_line->lines
                                          off = match->submatches[ 1 ]-offset
                                          len = match->submatches[ 1 ]-length )
                      text   = substring( val = raw_trace_line->lines
                                          off = match->submatches[ 2 ]-offset ) )
            INTO TABLE trace_lines.
    ENDLOOP.

    TYPES:
      BEGIN OF ts_certificate_in_trace,
        begin_line TYPE sytabix,
        end_line   TYPE sytabix,
      END OF ts_certificate_in_trace.
    TYPES tt_certificate_in_trace TYPE STANDARD TABLE OF ts_certificate_in_trace WITH EMPTY KEY.

    DATA(certificates_in_trace) = VALUE tt_certificate_in_trace( ).
    LOOP AT trace_lines REFERENCE INTO DATA(trace_line)
        WHERE text = `-----BEGIN CERTIFICATE-----`
           OR text = `-----END CERTIFICATE-----`.
      CASE substring( val = trace_line->text
                      off = 5
                      len = 3 ).
        WHEN 'BEG'.
          DATA(certificate_in_trace) = VALUE ts_certificate_in_trace( begin_line = sy-tabix ).
        WHEN 'END'.
          certificate_in_trace-end_line = sy-tabix.
          INSERT certificate_in_trace INTO TABLE certificates_in_trace.
      ENDCASE.
    ENDLOOP.

    LOOP AT certificates_in_trace ASSIGNING FIELD-SYMBOL(<certificate_in_trace>).
      INSERT VALUE #( lines = REDUCE #( INIT t = VALUE tt_certificate_line( )
                                        FOR <trace_line> IN trace_lines
                                            FROM <certificate_in_trace>-begin_line
                                            TO   <certificate_in_trace>-end_line
                                        NEXT t = VALUE #( BASE t
                                                          ( CONV #( <trace_line>-text ) ) ) ) )
            INTO TABLE result-certificates.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_raw_trace.
    " Credit: subroutine ICMAN_SHOW_TRACE in RSMONICM.
    DATA p_filename      TYPE icm_lines-lines VALUE 'dev_icm'.
    DATA p_first_n_lines TYPE i               VALUE 0.
    DATA p_last_n_lines  TYPE i               VALUE 0.
    CALL FUNCTION 'ICM_READ_TRC_FILE2'
      EXPORTING
        icm_filename      = p_filename
        icm_last_n_lines  = p_last_n_lines
        icm_first_n_lines = p_first_n_lines
      TABLES
        lines             = result
      EXCEPTIONS
        icm_open_failed   = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
  ENDMETHOD.


  METHOD get_trace_level.
    DATA(icm_info_data) = VALUE icm_info( ).
    DATA(dummy_icm_servlist2) = VALUE tt_icm_sinfo2( ).
    CALL FUNCTION 'ICM_GET_INFO2'
      IMPORTING
        info_data   = icm_info_data
      TABLES
        servlist    = dummy_icm_servlist2
*       THRLIST     = ICM_THRLIST
*       SERVLIST3   = ICM_SERVLIST
      EXCEPTIONS
        icm_error   = 1
        icm_timeout = 2
        OTHERS      = 6.
    IF sy-subrc <> 0.
      " TODO
    ENDIF.
    result = icm_info_data-trace_lvl.
  ENDMETHOD.


  METHOD set_trace_level.
    " Credit: subroutine ICMAN_SET_PARAM in RSMONICM.
    DATA p_name  TYPE pfeparname VALUE 'rdisp/TRACE'.
    DATA rc TYPE sysubrc.

    DATA(p_value) = CONV pfepvalue( value ).
    CALL 'ThSysInfo'                                      "#EC CI_CCALL
         ID 'OPCODE'    FIELD lif_tskhincl=>opcode_icm
         ID 'ICMOPCODE' FIELD lif_icmdef=>dp_plugin_op_chng_param
         ID 'PNAME'     FIELD p_name
         ID 'PVALUE'    FIELD p_value.
    IF sy-subrc <> 0.
      " TODO replace with exception
      CASE sy-subrc.
*      WHEN lif_icmdef=>icmeok.
*        MESSAGE s005(icm).
        WHEN lif_icmdef=>icmenotavail.
          MESSAGE i032(icm).
        WHEN OTHERS.
          MESSAGE e006(icm) WITH rc.
      ENDCASE.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS zcl_certi_strust DEFINITION
  FINAL
  CREATE PUBLIC .

************************************************************************
* Trust Management
*
* Copyright 2021 Marc Bernard <https://marcbernardtools.com/>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    CONSTANTS c_version TYPE string VALUE '1.0.0' ##NEEDED.

    TYPES:
      ty_line        TYPE c LENGTH 80,
      ty_certificate TYPE STANDARD TABLE OF ty_line WITH DEFAULT KEY,
      BEGIN OF ty_certattr,
        subject     TYPE string,
        issuer      TYPE string,
        serialno    TYPE string,
        validfrom   TYPE string,
        validto     TYPE string,
        datefrom    TYPE d,
        dateto      TYPE d,
        certificate TYPE xstring,
      END OF ty_certattr,
      ty_certattr_tt TYPE STANDARD TABLE OF ty_certattr WITH DEFAULT KEY.

    METHODS constructor
      IMPORTING
        !iv_context TYPE psecontext
        !iv_applic  TYPE ssfappl
      RAISING
        zcx_certi_strust.

    METHODS load
      IMPORTING
        !iv_create TYPE abap_bool DEFAULT abap_false
        !iv_id     TYPE ssfid OPTIONAL
        !iv_org    TYPE string OPTIONAL
      RAISING
        zcx_certi_strust.

    METHODS add
      IMPORTING
        !it_certificate TYPE ty_certificate
      RAISING
        zcx_certi_strust.

    METHODS get_own_certificate
      RETURNING
        VALUE(rs_result) TYPE ty_certattr
      RAISING
        zcx_certi_strust.

    METHODS get_certificate_list
      RETURNING
        VALUE(rt_result) TYPE ty_certattr_tt
      RAISING
        zcx_certi_strust.

    METHODS remove
      IMPORTING
        VALUE(iv_subject) TYPE string
      RAISING
        zcx_certi_strust.

    METHODS update
      RETURNING
        VALUE(rt_result) TYPE ty_certattr_tt
      RAISING
        zcx_certi_strust.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      mv_context   TYPE psecontext,
      mv_applic    TYPE ssfappl,
      mv_psename   TYPE ssfpsename,
      mv_psetext   TYPE strustappltxt ##NEEDED,
      mv_distrib   TYPE ssfflag,
      mv_tempfile  TYPE localfile,
      mv_id        TYPE ssfid,
      mv_profile   TYPE ssfpab,
      mv_profilepw TYPE ssfpabpw,
      mv_cert_own  TYPE xstring,
      mt_cert_new  TYPE ty_certattr_tt,
      ms_cert_old  TYPE ty_certattr,
      mt_cert_old  TYPE ty_certattr_tt,
      mv_save      TYPE abap_bool.

    METHODS _create
      IMPORTING
        !iv_id  TYPE ssfid OPTIONAL
        !iv_org TYPE string OPTIONAL
      RAISING
        zcx_certi_strust.

    METHODS _lock
      RAISING
        zcx_certi_strust.

    METHODS _unlock
      RAISING
        zcx_certi_strust.

    METHODS _save
      RAISING
        zcx_certi_strust.

ENDCLASS.



CLASS zcl_certi_strust IMPLEMENTATION.


  METHOD add.

    DATA:
      lv_certb64  TYPE string,
      lo_certobj  TYPE REF TO cl_abap_x509_certificate,
      ls_cert_new TYPE ty_certattr.

    FIELD-SYMBOLS:
      <lv_data> TYPE any.

    CONCATENATE LINES OF it_certificate INTO lv_certb64.

    " Remove Header and Footer
    TRY.
        FIND REGEX '-{5}.{0,}BEGIN.{0,}-{5}(.*)-{5}.{0,}END.{0,}-{5}' IN lv_certb64 SUBMATCHES lv_certb64.
        IF sy-subrc = 0.
          ASSIGN lv_certb64 TO <lv_data>.
          ASSERT sy-subrc = 0.
        ELSE.
          zcx_certi_strust=>raise( 'Inconsistent certificate format'(010) ).
        ENDIF.
      CATCH cx_sy_regex_too_complex.
        " e.g. multiple PEM frames in file
        zcx_certi_strust=>raise( 'Inconsistent certificate format'(010) ).
    ENDTRY.

    TRY.
        CREATE OBJECT lo_certobj
          EXPORTING
            if_certificate = <lv_data>.

        ls_cert_new-certificate = lo_certobj->get_certificate( ).

        CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
          EXPORTING
            certificate         = ls_cert_new-certificate
          IMPORTING
            subject             = ls_cert_new-subject
            issuer              = ls_cert_new-issuer
            serialno            = ls_cert_new-serialno
            validfrom           = ls_cert_new-validfrom
            validto             = ls_cert_new-validto
          EXCEPTIONS
            ssf_krn_error       = 1
            ssf_krn_nomemory    = 2
            ssf_krn_nossflib    = 3
            ssf_krn_invalid_par = 4
            OTHERS              = 5.
        IF sy-subrc <> 0.
          _unlock( ).
          zcx_certi_strust=>raise_t100( ).
        ENDIF.

        ls_cert_new-datefrom = ls_cert_new-validfrom(8).
        ls_cert_new-dateto   = ls_cert_new-validto(8).
        APPEND ls_cert_new TO mt_cert_new.

      CATCH cx_abap_x509_certificate.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
    ENDTRY.

  ENDMETHOD.


  METHOD constructor.

    mv_context = iv_context.
    mv_applic  = iv_applic.

    CALL FUNCTION 'SSFPSE_FILENAME'
      EXPORTING
        context       = mv_context
        applic        = mv_applic
      IMPORTING
        psename       = mv_psename
        psetext       = mv_psetext
        distrib       = mv_distrib
      EXCEPTIONS
        pse_not_found = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_certificate_list.

    DATA:
      lt_certlist TYPE ssfbintab,
      ls_cert_old TYPE ty_certattr.

    FIELD-SYMBOLS <lv_certlist> LIKE LINE OF lt_certlist.

    CALL FUNCTION 'SSFC_GET_CERTIFICATELIST'
      EXPORTING
        profile               = mv_profile
        profilepw             = mv_profilepw
      IMPORTING
        certificatelist       = lt_certlist
      EXCEPTIONS
        ssf_krn_error         = 1
        ssf_krn_nomemory      = 2
        ssf_krn_nossflib      = 3
        ssf_krn_invalid_par   = 4
        ssf_krn_nocertificate = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    LOOP AT lt_certlist ASSIGNING <lv_certlist>.

      CLEAR ls_cert_old.

      CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
        EXPORTING
          certificate         = <lv_certlist>
        IMPORTING
          subject             = ls_cert_old-subject
          issuer              = ls_cert_old-issuer
          serialno            = ls_cert_old-serialno
          validfrom           = ls_cert_old-validfrom
          validto             = ls_cert_old-validto
        EXCEPTIONS
          ssf_krn_error       = 1
          ssf_krn_nomemory    = 2
          ssf_krn_nossflib    = 3
          ssf_krn_invalid_par = 4
          OTHERS              = 5.
      IF sy-subrc <> 0.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
      ENDIF.

      ls_cert_old-datefrom = ls_cert_old-validfrom(8).
      ls_cert_old-dateto   = ls_cert_old-validto(8).
      APPEND ls_cert_old TO mt_cert_old.

    ENDLOOP.

    rt_result = mt_cert_old.

  ENDMETHOD.


  METHOD get_own_certificate.

    mv_profile = mv_tempfile.

    CALL FUNCTION 'SSFC_GET_OWNCERTIFICATE'
      EXPORTING
        profile               = mv_profile
        profilepw             = mv_profilepw
      IMPORTING
        certificate           = mv_cert_own
      EXCEPTIONS
        ssf_krn_error         = 1
        ssf_krn_nomemory      = 2
        ssf_krn_nossflib      = 3
        ssf_krn_invalid_par   = 4
        ssf_krn_nocertificate = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'SSFC_PARSE_CERTIFICATE'
      EXPORTING
        certificate         = mv_cert_own
      IMPORTING
        subject             = ms_cert_old-subject
        issuer              = ms_cert_old-issuer
        serialno            = ms_cert_old-serialno
        validfrom           = ms_cert_old-validfrom
        validto             = ms_cert_old-validto
      EXCEPTIONS
        ssf_krn_error       = 1
        ssf_krn_nomemory    = 2
        ssf_krn_nossflib    = 3
        ssf_krn_invalid_par = 4
        OTHERS              = 5.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    ms_cert_old-datefrom = ms_cert_old-validfrom(8).
    ms_cert_old-dateto   = ms_cert_old-validto(8).

    rs_result = ms_cert_old.

  ENDMETHOD.


  METHOD load.

    CLEAR mv_save.

    _lock( ).

    CALL FUNCTION 'SSFPSE_LOAD'
      EXPORTING
        psename           = mv_psename
      IMPORTING
        id                = mv_id
        fname             = mv_tempfile
      EXCEPTIONS
        authority_missing = 1
        database_failed   = 2
        OTHERS            = 3.
    IF sy-subrc <> 0.
      IF iv_create = abap_true.
        _create(
          iv_id  = iv_id
          iv_org = iv_org ).
      ELSE.
        zcx_certi_strust=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD remove.

    FIELD-SYMBOLS:
      <ls_cert_old> LIKE LINE OF mt_cert_old.

    " Remove certificate
    LOOP AT mt_cert_old ASSIGNING <ls_cert_old> WHERE subject = iv_subject.

      CALL FUNCTION 'SSFC_REMOVECERTIFICATE'
        EXPORTING
          profile               = mv_profile
          profilepw             = mv_profilepw
          subject               = <ls_cert_old>-subject
          issuer                = <ls_cert_old>-issuer
          serialno              = <ls_cert_old>-serialno
        EXCEPTIONS
          ssf_krn_error         = 1
          ssf_krn_nomemory      = 2
          ssf_krn_nossflib      = 3
          ssf_krn_invalid_par   = 4
          ssf_krn_nocertificate = 5
          OTHERS                = 6.
      IF sy-subrc <> 0.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
      ENDIF.

      mv_save = abap_true.
    ENDLOOP.

    _save( ).

    _unlock( ).

  ENDMETHOD.


  METHOD update.

    FIELD-SYMBOLS:
      <ls_cert_old> LIKE LINE OF mt_cert_old,
      <ls_cert_new> LIKE LINE OF mt_cert_new.

    " Remove expired certificates
    LOOP AT mt_cert_old ASSIGNING <ls_cert_old>.

      LOOP AT mt_cert_new ASSIGNING <ls_cert_new> WHERE subject = <ls_cert_old>-subject.

        IF <ls_cert_new>-dateto > <ls_cert_old>-dateto.
          " Certificate is newer, so remove the old certificate
          CALL FUNCTION 'SSFC_REMOVECERTIFICATE'
            EXPORTING
              profile               = mv_profile
              profilepw             = mv_profilepw
              subject               = <ls_cert_old>-subject
              issuer                = <ls_cert_old>-issuer
              serialno              = <ls_cert_old>-serialno
            EXCEPTIONS
              ssf_krn_error         = 1
              ssf_krn_nomemory      = 2
              ssf_krn_nossflib      = 3
              ssf_krn_invalid_par   = 4
              ssf_krn_nocertificate = 5
              OTHERS                = 6.
          IF sy-subrc <> 0.
            _unlock( ).
            zcx_certi_strust=>raise_t100( ).
          ENDIF.

          mv_save = abap_true.
        ELSE.
          " Certificate already exists, no update necessary
          DELETE mt_cert_new.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

    " Add new certificates to PSE
    LOOP AT mt_cert_new ASSIGNING <ls_cert_new>.

      CALL FUNCTION 'SSFC_PUT_CERTIFICATE'
        EXPORTING
          profile             = mv_profile
          profilepw           = mv_profilepw
          certificate         = <ls_cert_new>-certificate
        EXCEPTIONS
          ssf_krn_error       = 1
          ssf_krn_nomemory    = 2
          ssf_krn_nossflib    = 3
          ssf_krn_invalid_par = 4
          ssf_krn_certexists  = 5
          OTHERS              = 6.
      IF sy-subrc <> 0.
        _unlock( ).
        zcx_certi_strust=>raise_t100( ).
      ENDIF.

      mv_save = abap_true.
    ENDLOOP.

    _save( ).

    _unlock( ).

    rt_result = mt_cert_new.

  ENDMETHOD.


  METHOD _create.

    DATA:
      lv_license_num TYPE c LENGTH 10,
      lv_id          TYPE ssfid,
      lv_subject     TYPE certsubjct,
      lv_psepath     TYPE trfile.

*   Create new PSE (using RSA-SHA256 2048 which is the default in STRUST in recent releases)
    IF iv_id IS INITIAL.
      CASE mv_applic.
        WHEN 'DFAULT'.
          lv_id = `CN=%SID SSL client SSL Client (Standard), ` &&
                  `OU=I%LIC, OU=SAP Web AS, O=SAP Trust Community, C=DE` ##NO_TEXT.
        WHEN 'ANONYM'.
          lv_id = 'CN=anonymous' ##NO_TEXT.
      ENDCASE.
    ELSE.
      lv_id = iv_id.
    ENDIF.

    CALL FUNCTION 'SLIC_GET_LICENCE_NUMBER'
      IMPORTING
        license_number = lv_license_num.

    REPLACE '%SID' WITH sy-sysid INTO lv_id.
    REPLACE '%LIC' WITH lv_license_num INTO lv_id.
    REPLACE '%ORG' WITH iv_org INTO lv_id.
    CONDENSE lv_id.

    lv_subject = lv_id.

    CALL FUNCTION 'SSFPSE_CREATE'
      EXPORTING
        dn                = lv_subject
        alg               = 'R'
        keylen            = 2048
      IMPORTING
        psepath           = lv_psepath
      EXCEPTIONS
        ssf_unknown_error = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    mv_tempfile = lv_psepath.

    _save( ).

  ENDMETHOD.


  METHOD _lock.

    CALL FUNCTION 'SSFPSE_ENQUEUE'
      EXPORTING
        psename         = mv_psename
      EXCEPTIONS
        database_failed = 1
        foreign_lock    = 2
        internal_error  = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

  ENDMETHOD.


  METHOD _save.

    DATA lv_credname TYPE icm_credname.

    CHECK mv_save = abap_true.

    " Store PSE
    CALL FUNCTION 'SSFPSE_STORE'
      EXPORTING
        fname             = mv_tempfile
        psepin            = mv_profilepw
        psename           = mv_psename
        id                = mv_id
        b_newdn           = abap_false
        b_distribute      = mv_distrib
      EXCEPTIONS
        file_load_failed  = 1
        storing_failed    = 2
        authority_missing = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      _unlock( ).
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

    lv_credname = mv_psename.

    CALL FUNCTION 'ICM_SSL_PSE_CHANGED'
      EXPORTING
        global              = 1
        cred_name           = lv_credname
      EXCEPTIONS
        icm_op_failed       = 1
        icm_get_serv_failed = 2
        icm_auth_failed     = 3
        OTHERS              = 4.
    IF sy-subrc = 0.
      MESSAGE s086(trust).
    ELSE.
      MESSAGE s085(trust).
    ENDIF.

  ENDMETHOD.



  METHOD _unlock.

    " Drop temporary file
    TRY.
        DELETE DATASET mv_tempfile.
      CATCH cx_sy_file_open.
        zcx_certi_strust=>raise( 'Error deleting file'(020) && | { mv_tempfile }| ).
      CATCH cx_sy_file_authority.
        zcx_certi_strust=>raise( 'Not authorized to delete file'(030) && | { mv_tempfile }| ).
    ENDTRY.

    " Unlock PSE
    CALL FUNCTION 'SSFPSE_DEQUEUE'
      EXPORTING
        psename         = mv_psename
      EXCEPTIONS
        database_failed = 1
        foreign_lock    = 2
        internal_error  = 3
        OTHERS          = 4.
    IF sy-subrc <> 0.
      zcx_certi_strust=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.


INITIALIZATION.

  DATA: upload_path   TYPE string,
        download_path TYPE string.

  cl_gui_frontend_services=>get_upload_download_path(
    CHANGING
      upload_path                 = upload_path
      download_path               = download_path
    EXCEPTIONS
      cntl_error                  = 1
      error_no_gui                = 2
      not_supported_by_gui        = 3
      gui_upload_download_path    = 4
      upload_download_path_failed = 5
      OTHERS                      = 6
  ).
  IF sy-subrc = 0.
    p_dir = download_path.
  ENDIF.


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


  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
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

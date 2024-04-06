"! All-in-one copy of https://github.com/sandraros/zcerti
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
*----------------------------------------------------------------------
*
*   INCLUDE ICMDEF
*
*----------------------------------------------------------------------
*

*****************************************************************
*
*     SAP AG Walldorf
*     Systeme, Anwendungen und Produkte in der Datenverarbeitung
*
*     (C) Copyright SAP AG 1999-2012
*
*****************************************************************
*
*  Project:          R/3 ICM Monitor
*  Entwickl.-Stand:  BIN-Datenbank
*
*  Source-Typ:       Include
*
*  Autor:            Oliver Luik, CST Basis
*
*****************************************************************
*
*  Entwicklungslog:
*  29.12.99  ol   Erste Implementierung
*  08.07.07  ol   HTTP Log extension
*
*****************************************************************
*
*  Beschreibung:
*
*   Allgemeine Makros
*
*****************************************************************
*---------------------------------------------------------------------
* Opcodes fuer ThSysinfo
* Sind mit tskhincl.h und dpxxtool.h abzustimmen
* ...
*---------------------------------------------------------------------
  CONSTANTS dp_plugin_op_chng_param TYPE i VALUE 8.

*---------------------------------------------------------------------
* Rueckgabewerte
* Sind mit icxx.h abzustimmen
* ...
*---------------------------------------------------------------------
  CONSTANTS icmeok       LIKE sy-subrc VALUE 0.
  CONSTANTS icmenotavail LIKE sy-subrc VALUE -6.

**----------------------------------------------------------------------
**
**   INCLUDE ICMDEF
**
**----------------------------------------------------------------------
**
*
******************************************************************
**
**     SAP AG Walldorf
**     Systeme, Anwendungen und Produkte in der Datenverarbeitung
**
**     (C) Copyright SAP AG 1999-2012
**
******************************************************************
**
**  Project:          R/3 ICM Monitor
**  Entwickl.-Stand:  BIN-Datenbank
**
**  Source-Typ:       Include
**
**  Autor:            Oliver Luik, CST Basis
**
******************************************************************
**
**  Entwicklungslog:
**  29.12.99  ol   Erste Implementierung
**  08.07.07  ol   HTTP Log extension
**
******************************************************************
**
**  Beschreibung:
**
**   Allgemeine Makros
**
******************************************************************
*
**---------------------------------------------------------------------
** Konstanten fuer Listanzeige
**---------------------------------------------------------------------
*CONSTANTS : disp_icman        TYPE i VALUE 0,
*            disp_trace        TYPE i VALUE 1,
*            disp_info         TYPE i VALUE 2,
*            disp_relinfo      TYPE i VALUE 3,
*            disp_paraminfo    TYPE i VALUE 4,
*            disp_statistic    TYPE i VALUE 5,
*            disp_services     TYPE i VALUE 6,
*            disp_mpiinfo      TYPE i VALUE 7,
*            disp_vecstatistic TYPE i VALUE 8,
*            disp_thrdeta      TYPE i VALUE 9,
*            disp_htab         TYPE i VALUE 10,
*            disp_servdeta     TYPE i VALUE 11,
*            disp_cache        TYPE i VALUE 12,
*            disp_cache_stat   TYPE i VALUE 13,
*            disp_log_stat     TYPE i VALUE 14,
*            disp_log          TYPE i VALUE 15,
*            disp_clog_stat    TYPE i VALUE 16,
*            disp_clog         TYPE i VALUE 17,
*            disp_appservinfo  TYPE i VALUE 18,
*            disp_conns        TYPE i VALUE 19,
*            disp_conndeta     TYPE i VALUE 20,
*            disp_trace_icmbnd TYPE i VALUE 21,
*            disp_trace_seclog TYPE i VALUE 22,
*            disp_seclog_info  TYPE i VALUE 23,
*            disp_ri_hdl_info  TYPE i VALUE 24,
*            disp_j2ee_adm     TYPE i VALUE 25,
*            disp_pconns       TYPE i VALUE 26,
*            disp_pconndeta    TYPE i VALUE 27,
*            disp_loghdl       TYPE i VALUE 28,
*            disp_authhdl      TYPE i VALUE 29,
*            disp_modhdl       TYPE i VALUE 30,
*            disp_mod_rules    TYPE i VALUE 31,
*            disp_permtab      TYPE i VALUE 32,
*            disp_authfile     TYPE i VALUE 33,
*            disp_authstat     TYPE i VALUE 34.
*
*
*
*
*
**---------------------------------------------------------------------
** Protokolle des ICM
** Sind mit icxx.h abzustimmen
**---------------------------------------------------------------------
*CONSTANTS icm_plugin_protocol_none     TYPE i VALUE -1.
*CONSTANTS icm_plugin_protocol_http       TYPE i VALUE 1.
*CONSTANTS icm_plugin_protocol_https    TYPE i VALUE 2.
*CONSTANTS icm_plugin_protocol_nntp       TYPE i VALUE 3.
*CONSTANTS icm_plugin_protocol_smtp       TYPE i VALUE 4.
*CONSTANTS icm_plugin_protocol_ftp        TYPE i VALUE 5.
*CONSTANTS icm_plugin_protocol_monitor  TYPE i VALUE 6.
*CONSTANTS icm_plugin_protocol_telnet   TYPE i VALUE 7.
*CONSTANTS icm_plugin_protocol_lcom     TYPE i VALUE 8.
*
*CONSTANTS icm_plugin_protocol_p4       TYPE i VALUE 9.
*CONSTANTS icm_plugin_protocol_p4sec    TYPE i VALUE 10.
*CONSTANTS icm_plugin_protocol_iiop     TYPE i VALUE 11.
*CONSTANTS icm_plugin_protocol_iiopsec  TYPE i VALUE 12.
*
*CONSTANTS icm_plugin_protocol_enc      TYPE i VALUE 13.
*CONSTANTS icm_plugin_protocol_proxy    TYPE i VALUE 14.
*CONSTANTS icm_plugin_protocol_fcgi     TYPE i VALUE 15.
*CONSTANTS icm_plugin_protocol_router   TYPE i VALUE 16.
*CONSTANTS icm_plugin_protocol_http_jsp  TYPE i VALUE 17.
*CONSTANTS icm_plugin_protocol_https_jsp TYPE i VALUE 18.
*
*CONSTANTS icm_plugin_protocol_webproxy  TYPE i VALUE 19.
*CONSTANTS icm_plugin_protocol_jrfc      TYPE i VALUE 20.
*CONSTANTS icm_plugin_protocol_rmi       TYPE i VALUE 21.
*CONSTANTS icm_plugin_protocol_test      TYPE i VALUE 22.
*CONSTANTS icm_plugin_protocol_controls  TYPE i VALUE 23.
*CONSTANTS icm_plugin_protocol_licserv   TYPE i VALUE 24.
*CONSTANTS icm_plugin_protocol_websocket  TYPE i VALUE 25.
*CONSTANTS icm_plugin_protocol_websockets TYPE i VALUE 26.
*CONSTANTS icm_plugin_protocol_enqueue   TYPE i VALUE 27.
*CONSTANTS icm_plugin_protocol_enqueues  TYPE i VALUE 28.
*CONSTANTS icm_plugin_protocol_tcp       TYPE i VALUE 29.
*CONSTANTS icm_plugin_protocol_tcps      TYPE i VALUE 30.
*CONSTANTS icm_plugin_protocol_h2        TYPE i VALUE 31.
*" PLUGIN_PROTOCOL_CAGENT=32 -> only relevant for Web Dispatcher
*CONSTANTS icm_plugin_protocol_h2c       TYPE i VALUE 33.
*
*CONSTANTS icm_plugin_protocol_last     TYPE i VALUE 34.
*
*
*
*
**---------------------------------------------------------------------
** Opcodes fuer ThSysinfo
** Sind mit tskhincl.h und dpxxtool.h abzustimmen
** ...
**---------------------------------------------------------------------
*CONSTANTS dp_plugin_op_none          TYPE i VALUE 0.
*CONSTANTS dp_plugin_op_connect       TYPE i VALUE 1.
*CONSTANTS dp_plugin_op_trace_up      TYPE i VALUE 2.
*CONSTANTS dp_plugin_op_trace_down    TYPE i VALUE 3.
*CONSTANTS dp_plugin_op_trace_reset   TYPE i VALUE 4.
*CONSTANTS dp_plugin_op_stat_up       TYPE i VALUE 5.
*CONSTANTS dp_plugin_op_stat_down     TYPE i VALUE 6.
*CONSTANTS dp_plugin_op_stat_reset    TYPE i VALUE 7.
*CONSTANTS dp_plugin_op_chng_param    TYPE i VALUE 8.
*CONSTANTS dp_plugin_op_get_relinfo   TYPE i VALUE 9.
*CONSTANTS dp_plugin_op_get_paraminfo TYPE i VALUE 10.
*CONSTANTS dp_plugin_op_get_status    TYPE i VALUE 11.
*CONSTANTS dp_plugin_op_get_thr_status    TYPE i VALUE 12.
*CONSTANTS dp_plugin_op_get_thr_detail    TYPE i VALUE 13.
*CONSTANTS dp_plugin_op_get_statistic     TYPE i VALUE 14.
*CONSTANTS dp_plugin_op_soft_restart      TYPE i VALUE 15.
*CONSTANTS dp_plugin_op_hard_restart      TYPE i VALUE 16.
*CONSTANTS dp_plugin_op_force_restart     TYPE i VALUE 17.
*CONSTANTS dp_plugin_op_continue_wait     TYPE i VALUE 18.
*CONSTANTS dp_plugin_op_serv_deact        TYPE i VALUE 19.
*CONSTANTS dp_plugin_op_serv_act          TYPE i VALUE 20.
*CONSTANTS dp_plugin_op_serv_create       TYPE i VALUE 21.
*CONSTANTS dp_plugin_op_serv_statis       TYPE i VALUE 22.
*CONSTANTS dp_plugin_op_get_mpiinfo       TYPE i VALUE 23.
*CONSTANTS dp_plugin_op_get_hostnametab   TYPE i VALUE 24.
*CONSTANTS dp_plugin_op_reset_hostnametab TYPE i VALUE 25.
*CONSTANTS dp_plugin_op_thr_stop          TYPE i VALUE 26.
*CONSTANTS dp_plugin_op_serv_chg          TYPE i VALUE 27.
*CONSTANTS dp_plugin_op_serv_del          TYPE i VALUE 28.
*CONSTANTS dp_plugin_op_get_conns         TYPE i VALUE 29.
*CONSTANTS dp_plugin_op_get_hostnamtab2   TYPE i VALUE 30.
*CONSTANTS dp_plugin_op_get_seclog_info   TYPE i VALUE 31.
*CONSTANTS dp_plugin_op_pse_changed       TYPE i VALUE 32.
*CONSTANTS dp_plugin_op_serv_ri_dump      TYPE i VALUE 33.
*CONSTANTS dp_plugin_op_enter_maint       TYPE i VALUE 34.
*CONSTANTS dp_plugin_op_leave_maint       TYPE i VALUE 35.
*CONSTANTS dp_plugin_op_reload_conf       TYPE i VALUE 36.
*CONSTANTS dp_plugin_op_get_pconns        TYPE i VALUE 37.
*CONSTANTS dp_plugin_op_reload_acl        TYPE i VALUE 38.
*CONSTANTS dp_plugin_op_set_parameter     TYPE i VALUE 39.
*CONSTANTS dp_plugin_op_get_parameter     TYPE i VALUE 40.
*CONSTANTS dp_plugin_op_get_server_ports  TYPE i VALUE 41.


**---------------------------------------------------------------------
** Opcodes fuer Plugin- und Subhandler-Operationen
** Sind mit icxxextplg.h abzustimmen
** ...
**---------------------------------------------------------------------
*CONSTANTS dp_plugin_op_plginfo           TYPE i VALUE 130.
*CONSTANTS dp_plugin_op_getsubhdl         TYPE i VALUE 131.
*CONSTANTS dp_plugin_op_cache_cont        TYPE i VALUE 132.
*CONSTANTS dp_plugin_op_cache_inval1      TYPE i VALUE 133.
*CONSTANTS dp_plugin_op_cache_invalall    TYPE i VALUE 134.
*CONSTANTS dp_plugin_op_cache_sync        TYPE i VALUE 135.
*CONSTANTS dp_plugin_op_cache_stat        TYPE i VALUE 136.
*CONSTANTS dp_plugin_op_cache_inval       TYPE i VALUE 137.
*CONSTANTS dp_plugin_op_cache_upload      TYPE i VALUE 138.
*
*CONSTANTS dp_plugin_op_sap_upload_url    TYPE i VALUE 140.
*CONSTANTS dp_plugin_op_sap_getinfo       TYPE i VALUE 141.
*CONSTANTS dp_plugin_op_sap_getinfo2      TYPE i VALUE 142.
*
*CONSTANTS dp_plugin_op_mod_stat          TYPE i VALUE 145.
*CONSTANTS dp_plugin_op_mod_get_rules     TYPE i VALUE 146.
*
*CONSTANTS dp_plugin_op_log_stat          TYPE i VALUE 150.
*CONSTANTS dp_plugin_op_log_flush         TYPE i VALUE 151.
*
*CONSTANTS dp_plugin_op_clog_stat         TYPE i VALUE 152.
*CONSTANTS dp_plugin_op_clog_flush        TYPE i VALUE 153.
*
*CONSTANTS dp_plugin_op_log_async_flush   TYPE i VALUE 154.
*
*CONSTANTS dp_plugin_op_adm_stat          TYPE i VALUE 155.
*
*CONSTANTS dp_plugin_op_auth_stat         TYPE i VALUE 156.
*CONSTANTS dp_plugin_op_auth_reload       TYPE i VALUE 157.
*CONSTANTS dp_plugin_op_auth_get_permtab  TYPE i VALUE 158.
*
*CONSTANTS dp_plugin_op_subhdl_act        TYPE i VALUE 160.
*CONSTANTS dp_plugin_op_subhdl_deact      TYPE i VALUE 161.
*CONSTANTS dp_plugin_op_csubhdl_act       TYPE i VALUE 162.
*CONSTANTS dp_plugin_op_csubhdl_deact     TYPE i VALUE 163.
*CONSTANTS dp_plugin_op_get_subhdl_parm   TYPE i VALUE 164.
*CONSTANTS dp_plugin_op_set_subhdl_parm   TYPE i VALUE 165.
*
*CONSTANTS dp_plugin_op_set_session_info  TYPE i VALUE 169.
*
*CONSTANTS dp_plugin_op_ws_send           TYPE i VALUE 170.
*CONSTANTS dp_plugin_op_ws_register       TYPE i VALUE 171.
*
*
*
**---------------------------------------------------------------------
** Destinationen fuer Subhandleroperationen
**
**---------------------------------------------------------------------
*CONSTANTS dp_plugin_dest_all             TYPE i VALUE 1.
*CONSTANTS dp_plugin_dest_spec            TYPE i VALUE 2.
*CONSTANTS dp_plugin_dest_url             TYPE i VALUE 3.
*CONSTANTS dp_plugin_dest_allwithtype     TYPE i VALUE 4.
*
** Alle Subhandler indixes aktivieren
*CONSTANTS icm_subhdlidx_all              TYPE i VALUE -1.
*
*
*
**---------------------------------------------------------------------
** Rollen des ICM
** Sind mit icxx.h abzustimmen
**---------------------------------------------------------------------
*CONSTANTS icm_role_server            TYPE x VALUE 01.
*CONSTANTS icm_role_client            TYPE x VALUE 02.
*
**---------------------------------------------------------------------
** WP Zustand aus Sicht des ICM
**---------------------------------------------------------------------
*CONSTANTS icm_context_stat_init      TYPE i VALUE 0.
*CONSTANTS icm_context_stat_rin       TYPE i VALUE 1.
*CONSTANTS icm_context_stat_rout      TYPE i VALUE 2.
*
*
**---------------------------------------------------------------------
** Zustaende des ICM
** Sind mit icxx.h abzustimmen
**---------------------------------------------------------------------
*CONSTANTS icm_status_none            TYPE i VALUE 0.
*CONSTANTS icm_status_init            TYPE i VALUE 1.
*CONSTANTS icm_status_run             TYPE i VALUE 2.
*CONSTANTS icm_status_shutdown        TYPE i VALUE 3.
*CONSTANTS icm_status_down            TYPE i VALUE 4.
*CONSTANTS icm_status_maint           TYPE i VALUE 5.
*
**---------------------------------------------------------------------
** Zustaende der ICM Threads
** Sind mit icxx.h abzustimmen
**---------------------------------------------------------------------
*CONSTANTS icm_thr_status_none        TYPE i VALUE 0.
*CONSTANTS icm_thr_status_idle        TYPE i VALUE 1.
*CONSTANTS icm_thr_status_run         TYPE i VALUE 2.
*CONSTANTS icm_thr_status_dead        TYPE i VALUE 3.
*
**---------------------------------------------------------------------
** Requesttypen
** Sind mit icxx.h abzustimmen
**---------------------------------------------------------------------
*CONSTANTS icm_req_noop                TYPE i VALUE 0.
*CONSTANTS icm_req_read_request        TYPE i VALUE 1.
*CONSTANTS icm_req_read_response       TYPE i VALUE 2.
*CONSTANTS icm_req_write_request       TYPE i VALUE 3.
*CONSTANTS icm_req_write_response      TYPE i VALUE 4.
*CONSTANTS icm_req_select              TYPE i VALUE 5.
*CONSTANTS icm_req_wait_for_response   TYPE i VALUE 16.
*CONSTANTS icm_req_wait_for_request    TYPE i VALUE 17.
*CONSTANTS icm_req_accept_conn         TYPE i VALUE 64.
*CONSTANTS icm_req_connect_to_serv     TYPE i VALUE 65.
*CONSTANTS icm_req_admmsg              TYPE i VALUE 66.
*CONSTANTS icm_req_shutdown            TYPE i VALUE 67.
*CONSTANTS icm_req_close_connection    TYPE i VALUE 68.
*CONSTANTS icm_req_scheduler           TYPE i VALUE 69.
*CONSTANTS icm_req_javaproxy           TYPE i VALUE 70.
*CONSTANTS icm_req_ssl_init_client     TYPE i VALUE 71.
*CONSTANTS icm_req_ssl_init_server     TYPE i VALUE 72.
*
*CONSTANTS icm_req_ssl_accept_async    TYPE i VALUE 73.
*CONSTANTS icm_req_ssl_conn_async      TYPE i VALUE 74.
*CONSTANTS icm_req_ssl_op_async        TYPE i VALUE 75.
*
**---------------------------------------------------------------------
** Kommunikationspartner des ICM
** Sind mit icxx.h abzustimmen
**---------------------------------------------------------------------
*CONSTANTS icm_partner_none           TYPE i VALUE 0.
*CONSTANTS icm_partner_dp             TYPE i VALUE 1.
*CONSTANTS icm_partner_wp             TYPE i VALUE 2.
*CONSTANTS icm_partner_icmon          TYPE i VALUE 3.
*CONSTANTS icm_partner_icmbnd         TYPE i VALUE 4.
*CONSTANTS icm_partner_webdispclnt    TYPE i VALUE 5.
*CONSTANTS icm_partner_jctrl          TYPE i VALUE 6.
*CONSTANTS icm_partner_jserv          TYPE i VALUE 7.
*CONSTANTS icm_partner_cpic           TYPE i VALUE 8.
*CONSTANTS icm_partner_startsrv       TYPE i VALUE 9.
*
*
**---------------------------------------------------------------------
** Rueckgabewerte
** Sind mit icxx.h abzustimmen
** ...
**---------------------------------------------------------------------
*CONSTANTS icm_ok                     LIKE sy-subrc VALUE 0.
*CONSTANTS icmeok                     LIKE sy-subrc VALUE 0.
*CONSTANTS icmeintern                 LIKE sy-subrc VALUE -1.
*CONSTANTS icmeinval                  LIKE sy-subrc VALUE -2.
*CONSTANTS icmenomem                  LIKE sy-subrc VALUE -3.
*CONSTANTS icmewrongver               LIKE sy-subrc VALUE -4.
*CONSTANTS icmetimeout                LIKE sy-subrc VALUE -5.
*CONSTANTS icmenotavail               LIKE sy-subrc VALUE -6.
*CONSTANTS icmeblockfull              LIKE sy-subrc VALUE -7.
*CONSTANTS icmenierror                LIKE sy-subrc VALUE -8.
*CONSTANTS icmeniclose                LIKE sy-subrc VALUE -9.
*CONSTANTS icmerolledout              LIKE sy-subrc VALUE -10.
*CONSTANTS icmenosession              LIKE sy-subrc VALUE -11.
*CONSTANTS icmemaxconn                LIKE sy-subrc VALUE -12.
*CONSTANTS icmeperm                   LIKE sy-subrc VALUE -13.
*CONSTANTS icmessl                    LIKE sy-subrc VALUE -14.
*CONSTANTS icmenodata                 LIKE sy-subrc VALUE -15.
*CONSTANTS icmewrongdata              LIKE sy-subrc VALUE -16.
*CONSTANTS icmenomoredata             LIKE sy-subrc VALUE -17.
*CONSTANTS icmewronglen               LIKE sy-subrc VALUE -18.
*CONSTANTS icmenosslreq               LIKE sy-subrc VALUE -19.
*CONSTANTS icmeconnfailed             LIKE sy-subrc VALUE -20.
*CONSTANTS icmeproterror              LIKE sy-subrc VALUE -21.
*CONSTANTS icmeinuse                  LIKE sy-subrc VALUE -22.
*CONSTANTS icmeexists                 LIKE sy-subrc VALUE -23.
*CONSTANTS icmeproxyerr               LIKE sy-subrc VALUE -24.
*CONSTANTS icmepartnererr             LIKE sy-subrc VALUE -25.
*CONSTANTS icmedispatcherr            LIKE sy-subrc VALUE -26.
*CONSTANTS icmemaintenance            LIKE sy-subrc VALUE -27.
*CONSTANTS icmenotfound               LIKE sy-subrc VALUE -28.
*CONSTANTS icmeunauth                 LIKE sy-subrc VALUE -29.
*CONSTANTS icmehostunknown            LIKE sy-subrc VALUE -30.
*CONSTANTS icmeservunknown            LIKE sy-subrc VALUE -31.
*CONSTANTS icmenotimpl                LIKE sy-subrc VALUE -32.
*CONSTANTS icmeagain                  LIKE sy-subrc VALUE -33.
*CONSTANTS icmeconflict               LIKE sy-subrc VALUE -34.
*CONSTANTS icmemaxsystems             LIKE sy-subrc VALUE -35.
*CONSTANTS icmeaclcreate              LIKE sy-subrc VALUE -36.
*CONSTANTS icmemaxsessions            LIKE sy-subrc VALUE -37.
*CONSTANTS icmesslrequired            LIKE sy-subrc VALUE -38.
*CONSTANTS icmesslcertmismatch        LIKE sy-subrc VALUE -39.
*
*CONSTANTS icm_open_failed            LIKE sy-subrc VALUE -100.
*
*
**---------------------------------------------------------------------
** Kontexttype
**---------------------------------------------------------------------
*CONSTANTS icm_ctx_type_none          LIKE sy-subrc VALUE 0.
*CONSTANTS icm_ctx_type_sapr3         LIKE sy-subrc VALUE 1.
*CONSTANTS icm_ctx_type_fca           LIKE sy-subrc VALUE 2.
*CONSTANTS icm_ctx_type_proxy         LIKE sy-subrc VALUE 3.
*CONSTANTS icm_ctx_type_net           LIKE sy-subrc VALUE 4.
*
*
*
**---------------------------------------------------------------------
** ICM Fehlercodes für Clientrequests
** HTTP-Header: SAP-ICMCLNTERROR: xxx
** Fehlercode wird im ICF ausgewertet
** Sind mit icxx.h abzustimmen
** ...
**---------------------------------------------------------------------
*
** success: code 200-299
*CONSTANTS icm_http_ok      LIKE sy-subrc VALUE  200.
*
** errors: code 400-599
*CONSTANTS icm_http_connection_failed     LIKE sy-subrc VALUE  400.
*CONSTANTS icm_http_connection_broken     LIKE sy-subrc VALUE  401.
*CONSTANTS icm_http_timeout               LIKE sy-subrc VALUE  402.
*CONSTANTS icm_http_service_unavailable   LIKE sy-subrc VALUE  403.
*CONSTANTS icm_http_no_more_memory        LIKE sy-subrc VALUE  404.
*CONSTANTS icm_http_internal_error        LIKE sy-subrc VALUE  405.
*CONSTANTS icm_http_no_permission         LIKE sy-subrc VALUE  406.
*CONSTANTS icm_http_ssl_error             LIKE sy-subrc VALUE  407.
*CONSTANTS icm_http_sslproxy_error        LIKE sy-subrc VALUE  408.
*CONSTANTS icm_http_not_found             LIKE sy-subrc VALUE  409.
*CONSTANTS icm_http_unauthorized          LIKE sy-subrc VALUE  410.
*CONSTANTS icm_http_connection_refused      LIKE sy-subrc VALUE  411.
*CONSTANTS icm_http_proxy_conn_refused      LIKE sy-subrc VALUE  412.
*CONSTANTS icm_http_proxy_host_unknown      LIKE sy-subrc VALUE  413.
*CONSTANTS icm_http_ssl_cred_not_found      LIKE sy-subrc VALUE  414.
*CONSTANTS icm_http_ssl_cert_mismatch     LIKE sy-subrc VALUE  415.
*CONSTANTS icm_http_proxy_unauthorized    LIKE sy-subrc VALUE  416.
*CONSTANTS icm_http_host_unknown          LIKE sy-subrc VALUE  417.
*CONSTANTS icm_http_ssl_peer_cert_expired LIKE sy-subrc VALUE  418.
*
**---------------------------------------------------------------------
** Subhandlertypen
** Sind mit http_hdl.h abzustimmen
**---------------------------------------------------------------------
*CONSTANTS icm_subhdl_type_illegal    LIKE sy-subrc VALUE 0.
*CONSTANTS icm_subhdl_type_faccess    LIKE sy-subrc VALUE 1.
*CONSTANTS icm_subhdl_type_redirect   LIKE sy-subrc VALUE 2.
*CONSTANTS icm_subhdl_type_cgi        LIKE sy-subrc VALUE 3.
*CONSTANTS icm_subhdl_type_cache      LIKE sy-subrc VALUE 4.
*CONSTANTS icm_subhdl_type_log        LIKE sy-subrc VALUE 5.
*CONSTANTS icm_subhdl_type_sapr3      LIKE sy-subrc VALUE 6.
*CONSTANTS icm_subhdl_type_fcgi       LIKE sy-subrc VALUE 7.
*CONSTANTS icm_subhdl_type_webdisp    LIKE sy-subrc VALUE 8.
*CONSTANTS icm_subhdl_type_j2ee       LIKE sy-subrc VALUE 9.
*CONSTANTS icm_subhdl_type_admin      LIKE sy-subrc VALUE 10.
*CONSTANTS icm_subhdl_type_auth       LIKE sy-subrc VALUE 11.
*CONSTANTS icm_subhdl_type_test       LIKE sy-subrc VALUE 12.
*CONSTANTS icm_subhdl_type_mod        LIKE sy-subrc VALUE 13.
*
*
**---------------------------------------------------------------------
** HTTP Logging: switch_type
**---------------------------------------------------------------------
*CONSTANTS icm_log_switch_none        LIKE sy-subrc VALUE 0.
*CONSTANTS icm_log_switch_hour        LIKE sy-subrc VALUE 1.
*CONSTANTS icm_log_switch_day         LIKE sy-subrc VALUE 2.
*CONSTANTS icm_log_switch_month       LIKE sy-subrc VALUE 3.
*
**---------------------------------------------------------------------
** Defines für Dynprofelder ein/ausschalten
**---------------------------------------------------------------------
*CONSTANTS: intensified TYPE c VALUE 'I',
*           required    TYPE c VALUE 'R',
*           input       TYPE c VALUE 'Y',
*           output      TYPE c VALUE 'O',
*           invisible   TYPE c VALUE 'V',
*           activ       TYPE c VALUE 'A',
*           on          TYPE c VALUE '1',
*           off         TYPE c VALUE '0',
*           true        TYPE c VALUE '1',
*           false       TYPE c VALUE '0',
*           doku        TYPE c VALUE 'D',
*           prop        TYPE c VALUE 'P',
*           create      TYPE c VALUE 'C',
*           delete      TYPE c VALUE 'D',
*           insert(10)  TYPE c VALUE 'INSERT',
*           yes         TYPE c VALUE 'Y',
*           no          TYPE c VALUE 'N'.
*
*
**---------------------------------------------------------------------
** Allgemeine Konstanten
** ...
**---------------------------------------------------------------------
*CONSTANTS micro_sec                   TYPE i VALUE 1000000.
*CONSTANTS milli_sec                   TYPE i VALUE 1000.
*
*CONSTANTS byte                        TYPE i VALUE 1.
*CONSTANTS kilobyte                    TYPE i VALUE 1024.
*CONSTANTS megabyte                    TYPE i VALUE 1048576.
*
** ICM runmodes
*CONSTANTS icm_runmode_normal          TYPE i VALUE 0.
*CONSTANTS icm_runmode_emergency       TYPE i VALUE 1.
*
*CONSTANTS icm_action_none             TYPE i VALUE 0.
*CONSTANTS icm_action_add              TYPE i VALUE 1.
*CONSTANTS icm_action_change           TYPE i VALUE 2.
*CONSTANTS icm_action_delete           TYPE i VALUE 3.
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
****INCLUDE TSKHINCL.
*
**-------------------------------------------------------------------*/
** Constants for calls of the taskhandler-C-functions
**-------------------------------------------------------------------*/
*
*
**-------------------------------------------------------------------*/
** reference fields
**-------------------------------------------------------------------*/
*##NEEDED
*DATA: th_int               LIKE sy-index,
*      th_wptype            TYPE x,
*      th_wpstate           TYPE x,
*      th_opcode(1)         TYPE x,
*      th_vb_phase(1)       TYPE x,
*      th_vb_mode(1)        TYPE x,
*      th_vb_debug(1)       TYPE x,
*      th_vb_v2start(1)     TYPE x,
*      th_vb_delete(1)      TYPE x,
*      th_vb_server_type(1) TYPE x,
*      th_vb_fb_mode(1)     TYPE c,
*      th_vb_rc             LIKE sy-index,
*      th_vb_state          TYPE x,
*      th_vb_cliinfo        TYPE x,
*      th_bool              TYPE x,
*      th_bitvalue          TYPE x,
*      th_semstat           TYPE x,
*      xcom_snc_mode(1)     TYPE c,
*      xcom_cpic_trace(1)   TYPE c,
*      th_bitvalue4(4)      TYPE x,
*
*      " must not be greater as TRCCOMPS_LN in dpxxtool.h
*      th_trccomps(40)      TYPE c.
*
**-------------------------------------------------------------------*/
** Constants for Boolean fields
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_true  LIKE th_bool VALUE 1,
*           th_false LIKE th_bool VALUE 0.
*
**-------------------------------------------------------------------*/
** Constants for unregister test/set function
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_unreg_parameter(8)           VALUE '%UNREG%'.
*
**-------------------------------------------------------------------*/
** Constants for calling function ThVBCall
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: start_vb                     LIKE th_opcode VALUE 1,
*           get_vb_key                   LIKE th_opcode VALUE 2,
*           save_vbparam                 LIKE th_opcode VALUE 3,
*           set_usr_mandt                LIKE th_opcode VALUE 4,
*           vb_write_syslog              LIKE th_opcode VALUE 5,
*           get_debug_info               LIKE th_opcode VALUE 6,
*           activate_vbdebug             LIKE th_opcode VALUE 7,
*           get_vb_rc                    LIKE th_opcode VALUE 8,
*           get_server_names             LIKE th_opcode VALUE 9,
*           rst_vb_dispatching_info      LIKE th_opcode VALUE 10,
*           rst_vb_statistic             LIKE th_opcode VALUE 11,
*           read_vb_statistic            LIKE th_opcode VALUE 12,
*           set_vb_exit_function         LIKE th_opcode VALUE 13,
*           select_vb_server             LIKE th_opcode VALUE 14,
*           vb_activate                  LIKE th_opcode VALUE 15,
*           vb_deactivate                LIKE th_opcode VALUE 16,
*           vb_get_info                  LIKE th_opcode VALUE 17,
*           vb_get_dispatching_info      LIKE th_opcode VALUE 18,
*           reset_vb_enq_key             LIKE th_opcode VALUE 19,
*           vb_set_user_attributes       LIKE th_opcode VALUE 20,
*           vb_get_pseudo_key            LIKE th_opcode VALUE 21,
*           vb_get_context_info          LIKE th_opcode VALUE 22,
*           vb_is_active                 LIKE th_opcode VALUE 23,
*           vb_activate_server           LIKE th_opcode VALUE 24,
*           vb_deactivate_server         LIKE th_opcode VALUE 25,
*           vb_get_upd_key_from_trans_id LIKE th_opcode VALUE 26,
*           vb_in_update_task            LIKE th_opcode VALUE 27,
*           vb_update_modul_processed    LIKE th_opcode VALUE 28,
*           vb_update_request_processsed LIKE th_opcode VALUE 29,
*           vb_start_commit              LIKE th_opcode VALUE 30,
*           vb_commit_processed          LIKE th_opcode VALUE 31,
*           vb_cleanup_arfc              LIKE th_opcode VALUE 32,
*           vb_after_commit              LIKE th_opcode VALUE 33,
*           vb_enq_context_commit        LIKE th_opcode VALUE 34,
*           vb_enq_context_rollback      LIKE th_opcode VALUE 35.
*
**-------------------------------------------------------------------*/
** Constants for field vbphase of structure vbparam
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_v1_normal               LIKE th_vb_phase VALUE 1,
*           vb_v1_test                 LIKE th_vb_phase VALUE 2,
*           vb_v2_normal               LIKE th_vb_phase VALUE 3,
*           vb_mail                    LIKE th_vb_phase VALUE 4,
*           vb_auto_sys_start          LIKE th_vb_phase VALUE 5,
*           vb_auto_sys_continue       LIKE th_vb_phase VALUE 6,
*           vb_auto_dia_single         LIKE th_vb_phase VALUE 7,
*           vb_auto_dia_start          LIKE th_vb_phase VALUE 8,
*           vb_auto_dia_continue       LIKE th_vb_phase VALUE 9,
*           vb_v1_restart_single       LIKE th_vb_phase VALUE 10,
*           vb_v1_restart_start        LIKE th_vb_phase VALUE 11,
*           vb_v1_restart_continue     LIKE th_vb_phase VALUE 12,
*           vb_v2_restart_single       LIKE th_vb_phase VALUE 13,
*           vb_v2_restart_start        LIKE th_vb_phase VALUE 14,
*           vb_v2_restart_continue     LIKE th_vb_phase VALUE 15,
*           vb_delete_single           LIKE th_vb_phase VALUE 16,
*           vb_delete_start            LIKE th_vb_phase VALUE 17,
*           vb_delete_continue         LIKE th_vb_phase VALUE 18,
*           vb_reorg                   LIKE th_vb_phase VALUE 19,
*           vb_delete_sys_start        LIKE th_vb_phase VALUE 20,
*           vb_delete_sys_continue     LIKE th_vb_phase VALUE 21,
*           vb_v2_auto_sys_start       LIKE th_vb_phase VALUE 22,
*           vb_v2_auto_sys_continue    LIKE th_vb_phase VALUE 23,
*           vb_v2_collector_start      LIKE th_vb_phase VALUE 24,
*           vb_v1_sys_restart_start    LIKE th_vb_phase VALUE 25,
*           vb_v1_sys_restart_continue LIKE th_vb_phase VALUE 26,
*           vb_col_restart_single      LIKE th_vb_phase VALUE 27,
*           vb_col_restart_start       LIKE th_vb_phase VALUE 28,
*           vb_col_restart_continue    LIKE th_vb_phase VALUE 29,
*           vb_arfc_clear              LIKE th_vb_phase VALUE 30.
*
**--------------------------------------------------------------------*/
** Constants for field vberronly in vbparam when vbparam is passed to the
** form routine VB_ARFC_CLEAR in RSM13000
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_arfc_cancel TYPE vbparam-vberronly VALUE 1,
*           vb_arfc_delete TYPE vbparam-vberronly VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for parameter vb_server_type of function ThVBCall
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_no_server_type LIKE th_vb_server_type VALUE 0,
*           vb_v1_server      LIKE th_vb_server_type VALUE 1,
*           vb_v2_server      LIKE th_vb_server_type VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for field vbmode of structure vbparam
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: sync_vb  LIKE th_vb_mode VALUE 1,
*           async_vb LIKE th_vb_mode VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for function UPD_GET_REQUEST_INFO
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: upd_not_executed TYPE i VALUE 1,
*           upd_executed     TYPE i VALUE 2,
*           upd_failed       TYPE i VALUE 3.
*
**--------------------------------------------------------------------*/
** Constants for field vbdebug of structure vbparam
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_debug_on  LIKE th_vb_debug VALUE 1,
*           vb_debug_off LIKE th_vb_debug VALUE 2,
*           vb_sdebug_on LIKE th_vb_debug VALUE 3.
*
**--------------------------------------------------------------------*/
** Constants for field vbv2start of structure vbparam
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_v2_start_direct   LIKE th_vb_v2start VALUE 1,
*           vb_v2_start_in_batch LIKE th_vb_v2start VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for field vbdelete of
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_delete_direct   LIKE th_vb_delete VALUE 1,
*           vb_delete_in_batch LIKE th_vb_delete VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for field vbmode of structure vbmod
**/
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_v1_fb              LIKE th_vb_fb_mode VALUE '1',
*           vb_v2_fb              LIKE th_vb_fb_mode VALUE '2',
*           vb_v1_no_upd_again_fb LIKE th_vb_fb_mode VALUE '3',
*           vb_arfc_fb            LIKE th_vb_fb_mode VALUE '4',
*           vb_collector_fb       LIKE th_vb_fb_mode VALUE '5',
*           vb_after_commit_fb    LIKE th_vb_fb_mode VALUE '6'.
*
**--------------------------------------------------------------------*/
** Constants for field vbrc of structure vbhdr
** (values 101 up to 199 are reserved for taskhander)
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_notexecuted       LIKE th_vb_rc VALUE 255,
*           vb_autosysexec       LIKE th_vb_rc VALUE 254,
*           vb_autodiaexec       LIKE th_vb_rc VALUE 253,
*           vb_run_v2            LIKE th_vb_rc VALUE 252,
*           vb_delete            LIKE th_vb_rc VALUE 251,
*           vb_restart_v1        LIKE th_vb_rc VALUE 250,
*           vb_restart_v2        LIKE th_vb_rc VALUE 249,
*           vb_autodelete        LIKE th_vb_rc VALUE 248,
*           vb_executed          LIKE th_vb_rc VALUE 247,
*           vb_run_v1            LIKE th_vb_rc VALUE 246,
*           vb_v2_ok             LIKE th_vb_rc VALUE 245,
*           vb_external_prepared LIKE th_vb_rc VALUE 244,
*           vb_restart_col       LIKE th_vb_rc VALUE 243,
*           vb_run_col           LIKE th_vb_rc VALUE 242,
*           vb_mem_cleared       LIKE th_vb_rc VALUE 241,
*           vb_ok                LIKE th_vb_rc VALUE 0,
*           vb_v1_ok             LIKE th_vb_rc VALUE 1,
*           vb_hdr_insert        LIKE th_vb_rc VALUE 2,
*           vb_hdr_read          LIKE th_vb_rc VALUE 3,
*           vb_dump              LIKE th_vb_rc VALUE 4,
*           vb_commit            LIKE th_vb_rc VALUE 5,
*           vb_modul_read        LIKE th_vb_rc VALUE 6,
*           vb_modul_update      LIKE th_vb_rc VALUE 7,
*           vb_no_memory         LIKE th_vb_rc VALUE 8,
*           vb_sap_exit          LIKE th_vb_rc VALUE 9,
*           vb_no_dynp_memory    LIKE th_vb_rc VALUE 10,
*           vb_bad_mode          LIKE th_vb_rc VALUE 11,
*           vb_run_stopped       LIKE th_vb_rc VALUE 12,
*           vb_del_stopped       LIKE th_vb_rc VALUE 13,
*           vb_wrong_parameter   LIKE th_vb_rc VALUE 14,
*           vb_no_parameter      LIKE th_vb_rc VALUE 15,
*           vb_deadlock          LIKE th_vb_rc VALUE 16,
*           vb_no_server         LIKE th_vb_rc VALUE 17,
*           vb_no_server_list    LIKE th_vb_rc VALUE 18,
*           vb_external_abort    LIKE th_vb_rc VALUE 19,
*           vb_not_running       LIKE th_vb_rc VALUE 20,
*           vb_enq_released      LIKE th_vb_rc VALUE 21,
*           vb_cant_handle_rq    LIKE th_vb_rc VALUE 22,
*           vb_msg_error         LIKE th_vb_rc VALUE 23,
*           vb_commit_error      LIKE th_vb_rc VALUE 24,
*           vb_control_error     LIKE th_vb_rc VALUE 25,
*           vb_v2_err            LIKE th_vb_rc VALUE 200,
*           vb_col_err           LIKE th_vb_rc VALUE 201.
*
*##NEEDED
*CONSTANTS: vb_err_offset        LIKE th_vb_rc VALUE 100.
*
**--------------------------------------------------------------------*/
** Constants for field vbstate of structure vbhdr
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_initial             LIKE th_vb_state VALUE 255,
*           vb_prepared            LIKE th_vb_state VALUE 254,
*           vb_aborted             LIKE th_vb_state VALUE 253,
*           vb_v1_processed        LIKE th_vb_state VALUE 1,
*           vb_v1_and_v2_processed LIKE th_vb_state VALUE 2,
*           vb_after_commit_state  LIKE th_vb_state VALUE 3.
*
**--------------------------------------------------------------------*/
** Constants for field vbcliinfo of structure vbhdr
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_sync_vb            LIKE th_vb_cliinfo VALUE 1,
*           vb_dont_process_again LIKE th_vb_cliinfo VALUE 2,
*           vb_keep_locks         LIKE th_vb_cliinfo VALUE 4,
*           vb_dequeue_atp        LIKE th_vb_cliinfo VALUE 8,
*           vb_with_enqueue       LIKE th_vb_cliinfo VALUE 16,
*           vb_external_commit    LIKE th_vb_cliinfo VALUE 32,
*           vb_external_data      LIKE th_vb_cliinfo VALUE 32,
*           vb_enqueues_released  LIKE th_vb_cliinfo VALUE 64,
*           vb_from_bi            LIKE th_vb_cliinfo VALUE 128.
*
**--------------------------------------------------------------------*/
** Constants for VB_CONTEXT
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_context_db_group  LIKE th_bitvalue VALUE 1,
*           vb_context_code_page LIKE th_bitvalue VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for calling funkction ThUsrInfo
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: opcode_list                    LIKE th_opcode VALUE 2,
*           opcode_mode_count              LIKE th_opcode VALUE 3,
*           opcode_long_blk_info           LIKE th_opcode VALUE 4,
*           opcode_usr_attr                LIKE th_opcode VALUE 5,
*           opcode_long_list               LIKE th_opcode VALUE 6,
*           opcode_blk_info                LIKE th_opcode VALUE 8,
*           opcode_emsg_set                LIKE th_opcode VALUE 9,
*           opcode_emsg_get                LIKE th_opcode VALUE 10,
*           opcode_emsg_out                LIKE th_opcode VALUE 11,
*           opcode_old_email_out           LIKE th_opcode VALUE 12,
*           opcode_email_out               LIKE th_opcode VALUE 13,
*           opcode_create_slave            LIKE th_opcode VALUE 14,
*           opcode_remove_slave            LIKE th_opcode VALUE 15,
*           opcode_get_user_key            LIKE th_opcode VALUE 16,
*           opcode_save_user_key           LIKE th_opcode VALUE 17,
*           opcode_cpic_login              LIKE th_opcode VALUE 18,
*           opcode_async_receive           LIKE th_opcode VALUE 19,
*           opcode_usr_del                 LIKE th_opcode VALUE 20,
*           opcode_usr_reconnect           LIKE th_opcode VALUE 21,
*           opcode_invalidate_cua          LIKE th_opcode VALUE 22,
*           opcode_create_mode             LIKE th_opcode VALUE 23,
*           opcode_delete_mode             LIKE th_opcode VALUE 24,
*           opcode_delete_usr              LIKE th_opcode VALUE 25,
*           opcode_wp_lock_set             LIKE th_opcode VALUE 26,
*           opcode_wplock_free             LIKE th_opcode VALUE 27,
*           opcode_wplock_get              LIKE th_opcode VALUE 28,
*           opcode_activate_user_trace     LIKE th_opcode VALUE 29,
*           opcode_deactivate_user_trace   LIKE th_opcode VALUE 30,
*           opcode_send_pop_up             LIKE th_opcode VALUE 31,
*           opcode_long_usr_info           LIKE th_opcode VALUE 32,
*           opcode_set_enq_info            LIKE th_opcode VALUE 33,
*           opcode_del_enq_info            LIKE th_opcode VALUE 34,
*           opcode_add_atp_enq_info        LIKE th_opcode VALUE 35,
*           opcode_print_enq_info          LIKE th_opcode VALUE 36,
*           opcode_del_all_enq_info        LIKE th_opcode VALUE 37,
*           opcode_clean_rfc_server        LIKE th_opcode VALUE 38,
*           opcode_tx_trans_id             LIKE th_opcode VALUE 39,
*           opcode_tx_prepare              LIKE th_opcode VALUE 40,
*           opcode_tx_commit               LIKE th_opcode VALUE 41,
*           opcode_tx_abort                LIKE th_opcode VALUE 42,
*           opcode_rm_reg_resource         LIKE th_opcode VALUE 43,
*           opcode_rm_unreg_resource       LIKE th_opcode VALUE 44,
*           opcode_delete_trans_id         LIKE th_opcode VALUE 45,
*           opcode_get_context_id          LIKE th_opcode VALUE 46,
*           opcode_test_http_client        LIKE th_opcode VALUE 47,
*           opcode_prepare_debugging       LIKE th_opcode VALUE 48,
*           opcode_set_appl_info           LIKE th_opcode VALUE 49,
*           opcode_del_appl_info           LIKE th_opcode VALUE 50,
*           opcode_get_appl_info           LIKE th_opcode VALUE 51,
*           opcode_usr_info                LIKE th_opcode VALUE 52,
*           opcode_disable_debugging       LIKE th_opcode VALUE 53,
*           opcode_get_debugging_state     LIKE th_opcode VALUE 54,
*           opcode_get_w3c_context_id      LIKE th_opcode VALUE 55,
*           opcode_create_wall_key         LIKE th_opcode VALUE 56,
*           opcode_create_wall_entry       LIKE th_opcode VALUE 57,
*           opcode_delete_wall_entry       LIKE th_opcode VALUE 58,
*           opcode_read_wall_entry         LIKE th_opcode VALUE 59,
*           opcode_modify_wall_entry       LIKE th_opcode VALUE 60,
*           opcode_find_wall_entry         LIKE th_opcode VALUE 61,
*           opcode_get_debug_info          LIKE th_opcode VALUE 62,
*           opcode_reset_debugging         LIKE th_opcode VALUE 63,
*           opcode_wall_entry_exists       LIKE th_opcode VALUE 64,
*           opcode_set_session_id          LIKE th_opcode VALUE 65,
*           opcode_start_ext_debugging     LIKE th_opcode VALUE 66,
*           opcode_stop_ext_debugging      LIKE th_opcode VALUE 67,
*           opcode_get_session_id          LIKE th_opcode VALUE 68,
*           opcode_create_foreign_mode     LIKE th_opcode VALUE 69,
*           opcode_get_client_id           LIKE th_opcode VALUE 70,
*           opcode_set_client_id           LIKE th_opcode VALUE 71,
*           opcode_set_auto_logout         LIKE th_opcode VALUE 72,
*           opcode_reset_ext_debugging     LIKE th_opcode VALUE 73,
*           opcode_activate_res_check      LIKE th_opcode VALUE 74,
*           opcode_deactivate_res_check    LIKE th_opcode VALUE 75,
*           opcode_detailed_blk_info       LIKE th_opcode VALUE 76,
*           opcode_get_trans_id            LIKE th_opcode VALUE 77,
*           opcode_detailed_blk_info_imode LIKE th_opcode VALUE 78,
*           opcode_compare_context_id      LIKE th_opcode VALUE 79,
*           opcode_get_abap_action         LIKE th_opcode VALUE 80,
*           opcode_sap_trace_ctl           LIKE th_opcode VALUE 81,
*           opcode_get_mode_list           LIKE th_opcode VALUE 82,
*           opcode_usr_info_ii             LIKE th_opcode VALUE 83,
*           opcode_cct_test                LIKE th_opcode VALUE 84,
*           opcode_inspect_mode            LIKE th_opcode VALUE 85,
*           opcode_clear_client_id_trc     LIKE th_opcode VALUE 86,
*           opcode_set_client_id_trc       LIKE th_opcode VALUE 87,
*           opcode_reset_imported_epp      LIKE th_opcode VALUE 88,
*           opcode_list_ad                 LIKE th_opcode VALUE 89,
*           opcode_get_remote_debug_key    LIKE th_opcode VALUE 90,
*           opcode_get_security_cont_ref   LIKE th_opcode VALUE 91,
*           opcode_system_message_in       LIKE th_opcode VALUE 92,
*           opcode_get_instance_name       LIKE th_opcode VALUE 93,
*           opcode_get_logon_id            LIKE th_opcode VALUE 94,
*           opcode_get_logon_type          LIKE th_opcode VALUE 95,
*           opcode_debug_session           LIKE th_opcode VALUE 96.
*
**--------------------------------------------------------------------*/
** Constants for field stat of user list
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: usr_stat_slot_free    LIKE th_int VALUE 1,
*           usr_stat_connected    LIKE th_int VALUE 2,
*           usr_stat_nend         LIKE th_int VALUE 3,
*           usr_stat_disconnected LIKE th_int VALUE 4,
*           usr_stat_new_login    LIKE th_int VALUE 5,
*           usr_stat_pooled       LIKE th_int VALUE 6.
*
**--------------------------------------------------------------------*/
** Constants for wall functions
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: wall_not_found       LIKE th_int VALUE 1,
*           wall_fi_error        LIKE th_int VALUE 2,
*           wall_item_truncated  LIKE th_int VALUE 3,
*           wall_alloc_failed    LIKE th_int VALUE 4,
*           wall_bad_item_len    LIKE th_int VALUE 5,
*           wall_no_item         LIKE th_int VALUE 6,
*           wall_bad_appl_id     LIKE th_int VALUE 7,
*           wall_conv_error      LIKE th_int VALUE 8,
*           wall_find_more       LIKE th_int VALUE 9,
*           wall_bad_cleanup_hdl LIKE th_int VALUE 10.
*
**--------------------------------------------------------------------*/
** Constants for calling funkction ThWpInfo
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: opcode_wp_list                LIKE th_opcode VALUE 1,
*           opcode_wp_stop                LIKE th_opcode VALUE 2,
*           opcode_wp_dump                LIKE th_opcode VALUE 3,
*           opcode_wp_restart_on          LIKE th_opcode VALUE 4,
*           opcode_wp_del                 LIKE th_opcode VALUE 5,
*           opcode_wp_debug               LIKE th_opcode VALUE 6,
*           opcode_wp_exit                LIKE th_opcode VALUE 7,
*           opcode_wp_get_id              LIKE th_opcode VALUE 8,
*           opcode_wp_exec_prog           LIKE th_opcode VALUE 9,
*           opcode_wp_trcswitch           LIKE th_opcode VALUE 10,
*           opcode_wp_trcinfo             LIKE th_opcode VALUE 11,
*           opcode_wp_err_get             LIKE th_opcode VALUE 12,
*           opcode_wp_detail              LIKE th_opcode VALUE 13,
*           opcode_wp_trace_on            LIKE th_opcode VALUE 14,
*           opcode_wp_trace_off           LIKE th_opcode VALUE 15,
*           opcode_wp_wait                LIKE th_opcode VALUE 16,
*           opcode_wp_get_enq             LIKE th_opcode VALUE 17,
*           opcode_wp_reset_trace         LIKE th_opcode VALUE 18,
*           opcode_wp_reset_disp_trace    LIKE th_opcode VALUE 19,
*           opcode_wp_full_reset_trace    LIKE th_opcode VALUE 20,
*           opcode_wp_redispatch          LIKE th_opcode VALUE 21,
*           opcode_wp_err_set             LIKE th_opcode VALUE 22,
*           opcode_wp_clr_zombies         LIKE th_opcode VALUE 23,
*           opcode_wp_upd_memory          LIKE th_opcode VALUE 24,
*           opcode_wp_get_info            LIKE th_opcode VALUE 25,
*           opcode_wp_clear_header        LIKE th_opcode VALUE 26,
*           opcode_wp_get_abap_req        LIKE th_opcode VALUE 27,
*           opcode_wp_return_to_java_test LIKE th_opcode VALUE 28,
*           opcode_wp_tinfo_list          LIKE th_opcode VALUE 29,
*           opcode_wp_version             LIKE th_opcode VALUE 30,
*           opcode_wp_clear_err_counter   LIKE th_opcode VALUE 31,
*           opcode_wp_write_trace         LIKE th_opcode VALUE 32,
*           opcode_wp_list_ad             LIKE th_opcode VALUE 33,
*           opcode_wp_get_load_info       LIKE th_opcode VALUE 34.
*
**--------------------------------------------------------------------*/
** Constants for field wp_itype in structure wpinfo
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_no_wp   LIKE th_wptype VALUE 0,
*           th_dia_wp  LIKE th_wptype VALUE 1,
*           th_upd_wp  LIKE th_wptype VALUE 2,
*           th_enq_wp  LIKE th_wptype VALUE 3,
*           th_btc_wp  LIKE th_wptype VALUE 4,
*           th_spo_wp  LIKE th_wptype VALUE 5,
*           th_upd2_wp LIKE th_wptype VALUE 6.
*
**--------------------------------------------------------------------*/
** Constants for field wp_istatus in structure wpinfo
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_wp_slot_free  LIKE th_wpstate VALUE 1,
*           th_wp_wait       LIKE th_wpstate VALUE 2,
*           th_wp_run        LIKE th_wpstate VALUE 4,
*           th_wp_hold       LIKE th_wpstate VALUE 8,
*           th_wp_killed     LIKE th_wpstate VALUE 16,
*           th_wp_shutdown   LIKE th_wpstate VALUE 32,
*           th_wp_restricted LIKE th_wpstate VALUE 64,
*           th_wp_new        LIKE th_wpstate VALUE 128.
*
**--------------------------------------------------------------------*/
** Constants for field wp_semstat in structure wpinfo
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_no_sem_action   LIKE th_semstat VALUE 0,
*           th_wait_for_sem    LIKE th_semstat VALUE 1,
*           th_sem_locked      LIKE th_semstat VALUE 2,
*           th_mtx_locked      LIKE th_semstat VALUE 3,
*           th_mtx_excl_locked LIKE th_semstat VALUE 4,
*           th_mtx_shrd_locked LIKE th_semstat VALUE 5.
*
**--------------------------------------------------------------------*/
** Constants for field SNC_MODE in table xcom
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: xcom_snc_off LIKE xcom_snc_mode VALUE '0',
*           xcom_snc_on  LIKE xcom_snc_mode VALUE '1'.
*
**--------------------------------------------------------------------*/
** Constants for field CPIC_TRACE in table xcom
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: xcom_cpic_trace_off LIKE xcom_cpic_trace VALUE '0',
*           xcom_cpic_trace_on  LIKE xcom_cpic_trace VALUE '1'.
*
**--------------------------------------------------------------------*/
** Constants for calling function ThCPICCall
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: opcode_get_sap_cpic_rc    LIKE th_opcode VALUE 1,
*           opcode_cpic_check_gateway LIKE th_opcode VALUE 2,
*           opcode_test_ne_comm       LIKE th_opcode VALUE 3,
*           opcode_call_cpic_flush    LIKE th_opcode VALUE 4,
*           opcode_set_cpic_security  LIKE th_opcode VALUE 5.
*
**--------------------------------------------------------------------*/
** Constants for calling function ThNoCall
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: opcode_read_hdr            LIKE th_opcode VALUE 1,
*           opcode_read_buffer         LIKE th_opcode VALUE 2,
*           opcode_reset_no_buffer     LIKE th_opcode VALUE 3,
*           opcode_check_buffer        LIKE th_opcode VALUE 4,
*           opcode_deactivate_buffer   LIKE th_opcode VALUE 5,
*           opcode_reset_mutex         LIKE th_opcode VALUE 6,
*           opcode_test_second_db_conn LIKE th_opcode VALUE 7.
*
**--------------------------------------------------------------------*/
** Constants for calling function ThSysInfo
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: opcode_mslst_get               LIKE th_opcode VALUE 1,
*           opcode_mbuf_refresh            LIKE th_opcode VALUE 2,
*           opcode_sys_cminit              LIKE th_opcode VALUE 3,
*           opcode_send_adm_msg            LIKE th_opcode VALUE 4,
*           opcode_saprel                  LIKE th_opcode VALUE 5,
*           opcode_request_queue           LIKE th_opcode VALUE 6,
*           opcode_mbuf_use                LIKE th_opcode VALUE 7,
*           opcode_mbuf_bypass             LIKE th_opcode VALUE 8,
*           opcode_environment             LIKE th_opcode VALUE 9,
*           opcode_arfc_noreq              LIKE th_opcode VALUE 10,
*           opcode_show_comm_adm           LIKE th_opcode VALUE 11,
*           opcode_reset_ni_buffer         LIKE th_opcode VALUE 12,
*           opcode_local_resources         LIKE th_opcode VALUE 13,
*           opcode_setget_arfc_quota       LIKE th_opcode VALUE 14,
*           opcode_add_service             LIKE th_opcode VALUE 15,
*           opcode_sub_service             LIKE th_opcode VALUE 16,
*           opcode_change_parameter        LIKE th_opcode VALUE 17,
*           opcode_set_kern_stat           LIKE th_opcode VALUE 18,
*           opcode_get_kern_stat           LIKE th_opcode VALUE 19,
*           opcode_reset_disp_trace        LIKE th_opcode VALUE 20,
*           opcode_get_plugin_info         LIKE th_opcode VALUE 21,
*           opcode_sys_counter_create      LIKE th_opcode VALUE 22,
*           opcode_sys_counter_delete      LIKE th_opcode VALUE 23,
*           opcode_sys_counter_wait        LIKE th_opcode VALUE 24,
*           opcode_sys_counter_read        LIKE th_opcode VALUE 25,
*           opcode_sys_counter_add         LIKE th_opcode VALUE 26,
*           opcode_sys_counter_sub         LIKE th_opcode VALUE 27,
*           opcode_req_cnt_names           LIKE th_opcode VALUE 28,
*           opcode_req_cnt_activate        LIKE th_opcode VALUE 29,
*           opcode_req_cnt_deactivate      LIKE th_opcode VALUE 30,
*           opcode_sys_counter_get_rc      LIKE th_opcode VALUE 31,
*           opcode_msgserver               LIKE th_opcode VALUE 32,
*           opcode_show_comm_adm_details   LIKE th_opcode VALUE 33,
*           opcode_sys_get_profvalue       LIKE th_opcode VALUE 34,
*           opcode_icm                     LIKE th_opcode VALUE 35,
*           opcode_wpca_blocks             LIKE th_opcode VALUE 36,
*           opcode_appcca_blocks           LIKE th_opcode VALUE 37,
*           opcode_gettimeinfo             LIKE th_opcode VALUE 38,
*           opcode_reset_exp_gui_cnt       LIKE th_opcode VALUE 39,
*           opcode_context_id_name         LIKE th_opcode VALUE 40,
*           opcode_plugins_active          LIKE th_opcode VALUE 41,
*           opcode_change_disp_param       LIKE th_opcode VALUE 42,
*           opcode_server_state            LIKE th_opcode VALUE 43,
*           opcode_get_virt_hostdata       LIKE th_opcode VALUE 44,
*           opcode_get_virt_host           LIKE th_opcode VALUE 45,
*           opcode_cancel_lcom             LIKE th_opcode VALUE 46,
*           opcode_ni                      LIKE th_opcode VALUE 47,
*           opcode_check_arfc_resources    LIKE th_opcode VALUE 48,
*           opcode_test_rpc                LIKE th_opcode VALUE 49,
*           opcode_test                    LIKE th_opcode VALUE 50,
*           opcode_test_msg                LIKE th_opcode VALUE 51,
*           opcode_delay_func_test         LIKE th_opcode VALUE 52,
*           opcode_delayed_func            LIKE th_opcode VALUE 53,
*           opcode_check_icman             LIKE th_opcode VALUE 54,
*           opcode_start_time              LIKE th_opcode VALUE 55,
*           opcode_qrfc_noreq              LIKE th_opcode VALUE 56,
*           opcode_dump_info               LIKE th_opcode VALUE 57,
*           opcode_dump_check              LIKE th_opcode VALUE 58,
*           opcode_dump_convert            LIKE th_opcode VALUE 59,
*           opcode_j2ee_adm                LIKE th_opcode VALUE 60,
*           opcode_get_vsock_list          LIKE th_opcode VALUE 61,
*           opcode_read_time_val           LIKE th_opcode VALUE 62,
*           opcode_dump_status_dp          LIKE th_opcode VALUE 63,
*           opcode_dump_status_wp          LIKE th_opcode VALUE 64,
*           opcode_get_dptimetab           LIKE th_opcode VALUE 65,
*           opcode_set_dptimetab           LIKE th_opcode VALUE 66,
*           opcode_request_queue_ad        LIKE th_opcode VALUE 67,
*           opcode_read_priority_info      LIKE th_opcode VALUE 68,
*           opcode_reset_ca_block_peak_cnt LIKE th_opcode VALUE 69,
*           opcode_handle_acl_file         LIKE th_opcode VALUE 70,
*           opcode_get_overloaded_info     LIKE th_opcode VALUE 71,
*           opcode_get_load_info           LIKE th_opcode VALUE 71,
*           opcode_reset_queue_peak_cnt    LIKE th_opcode VALUE 72,
*           opcode_es_check_and_report     LIKE th_opcode VALUE 73,
*           opcode_es_check_and_repair     LIKE th_opcode VALUE 74,
*           opcode_get_load_info_for_disp  LIKE th_opcode VALUE 75.
*
*
**-------------------------------------------------------------------*/
** Constants for queue type
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: dp_rq_nowp   TYPE i VALUE 0,
*           dp_rq_diawp  TYPE i VALUE 1,
*           dp_rq_updwp  TYPE i VALUE 2,
*           dp_rq_enqwp  TYPE i VALUE 3,
*           dp_rq_btcwp  TYPE i VALUE 4,
*           dp_rq_spowp  TYPE i VALUE 5,
*           dp_rq_upd2wp TYPE i VALUE 6.
*
**-------------------------------------------------------------------*/
** Constants for queue statistic type
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: dp_rq_stat_nowp         TYPE i VALUE 0,
*           dp_rq_stat_diawp        TYPE i VALUE 1,
*           dp_rq_stat_updwp        TYPE i VALUE 2,
*           dp_rq_stat_enqwp        TYPE i VALUE 3,
*           dp_rq_stat_btcwp        TYPE i VALUE 4,
*           dp_rq_stat_spowp        TYPE i VALUE 5,
*           dp_rq_stat_upd2wp       TYPE i VALUE 6,
*           dp_rq_stat_dispatcher   TYPE i VALUE 7,
*           dp_rq_stat_gateway      TYPE i VALUE 8,
*           dp_rq_stat_icman        TYPE i VALUE 9,
*           dp_rq_stat_specific_wp  TYPE i VALUE 10,
*           dp_rq_stat_num_of_types TYPE i VALUE 11.
*
**--------------------------------------------------------------------*/
** Constants for J2ee state
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: dp_j2ee_init      LIKE th_opcode VALUE 0,
*           dp_j2ee_started   LIKE th_opcode VALUE 1,
*           dp_j2ee_connected LIKE th_opcode VALUE 2,
*           dp_j2ee_active    LIKE th_opcode VALUE 3,
*           dp_j2ee_inactive  LIKE th_opcode VALUE 4,
*           dp_j2ee_shutdown  LIKE th_opcode VALUE 5.
*
**--------------------------------------------------------------------*/
** Constants for NI Opcode (47)
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: thni_hosttoaddr   TYPE i VALUE 1,
*           thni_addrtohost   TYPE i VALUE 2,
*           thni_hostaddrlist TYPE i VALUE 3,
*           thni_hostbufdump  TYPE i VALUE 4,
*           thni_hostbufinfo  TYPE i VALUE 5,
*           thni_dpnitabdump  TYPE i VALUE 6,
*           thni_servtono     TYPE i VALUE 7,
*           thni_notoserv     TYPE i VALUE 8.
*
**--------------------------------------------------------------------*/
** Constants for TEST Opcode (50)
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: thtest_noop  TYPE i VALUE 1,
*           thtest_test1 TYPE i VALUE 2,
*           thtest_test2 TYPE i VALUE 3,
*           thtest_test3 TYPE i VALUE 4,
*           thtest_test4 TYPE i VALUE 5,
*           thtest_test5 TYPE i VALUE 6.
*
**--------------------------------------------------------------------*/
** Constants for DELAYED_FUNC Opcode (53)
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: thdel_list_registered     TYPE i VALUE 1,
*           thdel_list_delayed        TYPE i VALUE 2,
*           thdel_get_statistic       TYPE i VALUE 3,
*           thdel_configure_statistic TYPE i VALUE 4.
*
**--------------------------------------------------------------------*/
** Constants resource check, returned by
** TH_ARFC_REQUESTS and
** TH_ARFC_LOCAL_RESOURCES
** defined in tskhincl.h
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_arfc_res_ok                LIKE th_opcode VALUE 0,
*           th_arfc_res_not_activated     LIKE th_opcode VALUE 1,
*           th_arfc_res_waiting_diawp     LIKE th_opcode VALUE 2,
*           th_arfc_res_low_maxcomm_entri LIKE th_opcode VALUE 3,
*           th_arfc_res_maxcomm_entries   LIKE th_opcode VALUE 4,
*           " TH_ARFC_RES_LOCAL_ARFCQUEUE is deprecated.
*           th_arfc_res_local_arfcqueue   LIKE th_opcode VALUE 5,
*           th_arfc_res_low_ownused_wp    LIKE th_opcode VALUE 6,
*           th_arfc_res_ownused_wp        LIKE th_opcode VALUE 7,
*           th_arfc_res_low_reqqueue      LIKE th_opcode VALUE 8,
*           th_arfc_res_reqqueue          LIKE th_opcode VALUE 9,
*           th_arfc_res_reqqueue_error    LIKE th_opcode VALUE 10,
*           th_arfc_res_low_login         LIKE th_opcode VALUE 11,
*           th_arfc_res_login             LIKE th_opcode VALUE 12,
*           th_arfc_res_low_own_login     LIKE th_opcode VALUE 13,
*           th_arfc_res_own_login         LIKE th_opcode VALUE 14,
*           th_arfc_res_server_hibernate  LIKE th_opcode VALUE 15,
*           th_arfc_res_server_shutdown   LIKE th_opcode VALUE 16,
*           th_arfc_res_server_stop       LIKE th_opcode VALUE 17,
*           th_arfc_res_server_starting   LIKE th_opcode VALUE 18,
*           th_arfc_res_server_init       LIKE th_opcode VALUE 19,
*           th_arfc_res_server_unknown    LIKE th_opcode VALUE 20,
*           th_arfc_res_local_low_maxcomm LIKE th_opcode VALUE 21,
*           th_arfc_res_local_maxcomm_ent LIKE th_opcode VALUE 22,
*           th_arfc_res_local_low_maxtask LIKE th_opcode VALUE 23,
*           th_arfc_res_local_maxtasks    LIKE th_opcode VALUE 24,
*           th_arfc_res_not_reachable     LIKE th_opcode VALUE 25,
*           th_arfc_res_exhausted         LIKE th_opcode VALUE 26,
*           th_arfc_res_high_load         LIKE th_opcode VALUE 27,
*
*           th_arfc_res_not_running       LIKE th_opcode VALUE 100,
*           th_arfc_res_wrong_config      LIKE th_opcode VALUE 101,
*           th_arfc_res_unexpected_error  LIKE th_opcode VALUE 102.
*
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_kern_stat_on  LIKE th_int VALUE 1,
*           th_kern_stat_off LIKE th_int VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for analyzing cli_info (see comm_adm)
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: external_client LIKE th_bitvalue VALUE 1,
*           r3_client       LIKE th_bitvalue VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for analyzing the services of an R/3-Server
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: ms_dia   LIKE th_bitvalue VALUE 1,
*           ms_vb    LIKE th_bitvalue VALUE 2,
*           ms_enq   LIKE th_bitvalue VALUE 4,
*           ms_btc   LIKE th_bitvalue VALUE 8,
*           ms_spo   LIKE th_bitvalue VALUE 16,
*           ms_vb2   LIKE th_bitvalue VALUE 32,
*           ms_atp   LIKE th_bitvalue VALUE 64,
*           ms_icman LIKE th_bitvalue VALUE 128.
*
**--------------------------------------------------------------------*/
** Constants for Systemservice parameter in TH_SERVER_LIST
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: ms_vmc  TYPE mssysservice VALUE 1,
*           ms_j2ee TYPE mssysservice VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for analyzing the system services of an R/3-Server
** returns as bit mask in SYSSERVICE0-3
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: ms_sys_vmc               LIKE th_bitvalue VALUE 2,
*           ms_sys_j2ee              LIKE th_bitvalue VALUE 4,
*           ms_sys_kernel_update     LIKE th_bitvalue VALUE 8,
*           ms_sys_server_suspended  LIKE th_bitvalue VALUE 16,
*           ms_sys_reconnect_pending LIKE th_bitvalue VALUE 32,
*           ms_sys_dev_trace_active  LIKE th_bitvalue VALUE 64.
*
**--------------------------------------------------------------------*/
** Constants for field task_state of function TH_USER_INFO
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: task_online           LIKE th_bitvalue4 VALUE 1,
*           task_dialog           LIKE th_bitvalue4 VALUE 2,
*           task_vb               LIKE th_bitvalue4 VALUE 4,
*           task_btc              LIKE th_bitvalue4 VALUE 8,
*           task_spo              LIKE th_bitvalue4 VALUE 16,
*           task_bi               LIKE th_bitvalue4 VALUE 32,
*           task_autoabap         LIKE th_bitvalue4 VALUE 64,
*           task_rolled_in        LIKE th_bitvalue4 VALUE 128,
*           task_remote_server    LIKE th_bitvalue4 VALUE 256,
*           task_term_output_sent LIKE th_bitvalue4 VALUE 512,
*           task_rfc_to_gui       LIKE th_bitvalue4 VALUE 1024,
*           task_gui_error        LIKE th_bitvalue4 VALUE 2048,
*           task_rfc              LIKE th_bitvalue4 VALUE 4096,
*           task_enq              LIKE th_bitvalue4 VALUE 8192,
*           task_bufref           LIKE th_bitvalue4 VALUE 16384.
*
**--------------------------------------------------------------------*/
** Constants for return codes from parameter changes
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: shmprf_ok       LIKE th_int VALUE 0,
*           shmprf_notdyn   LIKE th_int VALUE 101,
*           shmprf_disabled LIKE th_int VALUE 102,
*           shmprf_semerr   LIKE th_int VALUE 103,
*           shmprf_shmerr   LIKE th_int VALUE 104,
*           shmprf_enoent   LIKE th_int VALUE 105,
*           shmprf_enomem   LIKE th_int VALUE 106,
*           shmprf_einval   LIKE th_int VALUE 107,
*           shmprf_einvrcv  LIKE th_int VALUE 108.
*
**--------------------------------------------------------------------*/
** Constants for param stattype of TH_READ_KERNEL_STATISTIC
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_server_statistic  LIKE sy-index VALUE 0,
*           th_client_statistic  LIKE sy-index VALUE 1,
*           th_gateway_statistic LIKE sy-index VALUE 2,
*           th_all_statistic     LIKE sy-index VALUE 3.
*
**--------------------------------------------------------------------*/
** Constants for param mode of TH_MK_INTERVAL_TREE
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_creat_int_tree   LIKE sy-index VALUE 0,
*           th_append_int_tree  LIKE sy-index VALUE 1,
*           th_display_int_tree LIKE sy-index VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for logon types
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_gui_session LIKE th_bitvalue VALUE 4,
*           th_rfc_session LIKE th_bitvalue VALUE 32,
*           th_ext_plugin  LIKE th_bitvalue VALUE 202.
*
**--------------------------------------------------------------------*/
** Constants for opcodes in TH_VB_FUNCTION
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: vb_delete_record TYPE i VALUE 1,
*           vb_update_record TYPE i VALUE 2.
*
**--------------------------------------------------------------------*/
** constants for plugin protocols
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_plugin_protocol_none       TYPE i VALUE -1,
*           th_plugin_protocol_http       TYPE i VALUE 1,
*           th_plugin_protocol_https      TYPE i VALUE 2,
*           th_plugin_protocol_nntp       TYPE i VALUE 3,
*           th_plugin_protocol_smtp       TYPE i VALUE 4,
*           th_plugin_protocol_ftp        TYPE i VALUE 5,
*           th_plugin_protocol_monitor    TYPE i VALUE 6,
*           th_plugin_protocol_saphttp    TYPE i VALUE 24,
*           th_plugin_protocol_saphttps   TYPE i VALUE 23,
*           th_plugin_protocol_sapsmtp    TYPE i VALUE 22,
*           th_plugin_protocol_websocket  TYPE i VALUE 25,
*           th_plugin_protocol_websockets TYPE i VALUE 26.
*
**--------------------------------------------------------------------*/
** Constants for field context_id_type of fm TH_GET_W3C_CONTEXT_ID
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: thrun_context_id TYPE i VALUE 1,
*           thrun_session_id TYPE i VALUE 2.
*
**--------------------------------------------------------------------*/
** Constants for field debugging_type of fm TH_START_EXT_DEBUGGING
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_abap_system_debugging TYPE i VALUE 1,
*           th_abap_normal_debugging TYPE i VALUE 2,
*           th_ext_debugging         TYPE i VALUE 3.
*
**--------------------------------------------------------------------*/
** constants for parameter mode of th_set_appl_info
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_appl_info_mode_stack  TYPE i VALUE 1,
*           th_appl_info_mode_scroll TYPE i VALUE 2.
*
*CONSTANTS: th_appl_info_max_elems    TYPE i VALUE 10,
*           th_appl_info_max_info_len TYPE i VALUE 1000,
*           th_appl_info_max_hide_len TYPE i VALUE 128.
*
**
**-------------------------------------------------------------------*/
** Constants for calling function ThVmInfo
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: opcode_vm_list                 LIKE th_opcode VALUE 1,
*           opcode_vm_exit                 LIKE th_opcode VALUE 2,
*           opcode_vm_cancel               LIKE th_opcode VALUE 3,
*           opcode_vm_debug                LIKE th_opcode VALUE 4,
*           opcode_vm_dump_stack           LIKE th_opcode VALUE 5,
*           opcode_vm_set_trace_flags      LIKE th_opcode VALUE 6,
*           opcode_vm_get_trace_flags      LIKE th_opcode VALUE 7,
*           opcode_vm_info                 LIKE th_opcode VALUE 8,
*           opcode_vm_state                LIKE th_opcode VALUE 9,
*           opcode_vm_set_startup_debug    LIKE th_opcode VALUE 10,
*           opcode_vmc_activate            LIKE th_opcode VALUE 11,
*           opcode_vmc_meminfo             LIKE th_opcode VALUE 12,
*           opcode_vmc_reset               LIKE th_opcode VALUE 13,
*           opcode_vmc_profiling           LIKE th_opcode VALUE 14,
*           opcode_vmc_get_mtx_count       LIKE th_opcode VALUE 15,
*           opcode_vmc_write_memory_dump   LIKE th_opcode VALUE 16,
*           opcode_vm_get_statistics       LIKE th_opcode VALUE 17,
*           opcode_vmc_get_ses_tab         LIKE th_opcode VALUE 18,
*           opcode_vm_get_act_mtx          LIKE th_opcode VALUE 19,
*           opcode_vmc_get_kernel_version  LIKE th_opcode VALUE 20,
*           opcode_vmc_get_file_tab        LIKE th_opcode VALUE 21,
*           opcode_vmc_check_file_tab      LIKE th_opcode VALUE 22,
*           opcode_vmc_check_comm_tab      LIKE th_opcode VALUE 23,
*           opcode_vmc_switch_checks       LIKE th_opcode VALUE 24,
*           opcode_vmc_check_ses_tab       LIKE th_opcode VALUE 25,
*           opcode_vmc_lock_vm             LIKE th_opcode VALUE 26,
*           opcode_vmc_mtx_stat_activate   LIKE th_opcode VALUE 27,
*           opcode_vmc_mtx_stat_read       LIKE th_opcode VALUE 28,
*           opcode_vmc_mtx_stat_reset      LIKE th_opcode VALUE 29,
*           opcode_vmc_check_vm_pool       LIKE th_opcode VALUE 30,
*           opcode_vmc_get_mem_handles     LIKE th_opcode VALUE 31,
*           opcode_vmc_destroy_memory      LIKE th_opcode VALUE 32,
*           opcode_vmc_debug_proxy_start   LIKE th_opcode VALUE 33,
*           opcode_vmc_debug_proxy_stop    LIKE th_opcode VALUE 34,
*           opcode_vmc_debug_proxy_state   LIKE th_opcode VALUE 35,
*           opcode_vmc_stop_ext_timer      LIKE th_opcode VALUE 36,
*           opcode_vmc_get_ext_timer_state LIKE th_opcode VALUE 37,
*           opcode_vmc_reset_gc_counter    LIKE th_opcode VALUE 38,
*           opcode_vm_get_compilation_info LIKE th_opcode VALUE 39,
*           opcode_vm_get_free_debug_port  LIKE th_opcode VALUE 40,
*           opcode_vm_get_monitor_info     LIKE th_opcode VALUE 42.
*
*
**--------------------------------------------------------------------*/
** constants for vm profiling
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_vmc_prof_start              TYPE i VALUE 1,
*           th_vmc_prof_stop               TYPE i VALUE 2,
*           th_vmc_prof_destroy            TYPE i VALUE 3,
*           th_vmc_prof_alloctrace_enable  TYPE i VALUE 4,
*           th_vmc_prof_alloctrace_disable TYPE i VALUE 5,
*           th_vmc_prof_salloctrace_enable TYPE i VALUE 6,
*           th_vmc_prof_salloctrace_disabl TYPE i VALUE 7,
*           th_vmc_prof_sharedlock_enable  TYPE i VALUE 8,
*           th_vmc_prof_sharedlock_disable TYPE i VALUE 9,
*           th_vmc_prof_rolltrace_enable   TYPE i VALUE 10,
*           th_vmc_prof_rolltrace_disable  TYPE i VALUE 11,
*           th_vmc_prof_obj_death_enable   TYPE i VALUE 12,
*           th_vmc_prof_obj_death_disable  TYPE i VALUE 13,
*           th_vmc_prof_method_trc_enable  TYPE i VALUE 14,
*           th_vmc_prof_method_trc_disable TYPE i VALUE 15,
*           th_vmc_prof_timesmplng_enable  TYPE i VALUE 16,
*           th_vmc_prof_timesmplng_disable TYPE i VALUE 17.
*
*##NEEDED
*CONSTANTS: th_vmc_sharedlock_sysspo TYPE i VALUE 1,
*           th_vmc_sharedlock_sysscp TYPE i VALUE 2,
*           th_vmc_sharedlock_sysscv TYPE i VALUE 4,
*           th_vmc_sharedlock_sysscl TYPE i VALUE 8,
*           th_vmc_sharedlock_syssgc TYPE i VALUE 16,
*           th_vmc_sharedlock_sysslt TYPE i VALUE 32,
*           th_vmc_sharedlock_syssit TYPE i VALUE 64,
*           th_vmc_sharedlock_sysstp TYPE i VALUE 128,
*           th_vmc_sharedlock_sysscc TYPE i VALUE 256,
*           th_vmc_sharedlock_l00000 TYPE i VALUE 512,
*           th_vmc_sharedlock_l622b1 TYPE i VALUE 1024,
*           th_vmc_sharedlock_lxxxxx TYPE i VALUE 2048,
*           th_vmc_sharedlock_cxxxxx TYPE i VALUE 4096,
*           th_vmc_sharedlock_xxxxxx TYPE i VALUE 8192.
*
**--------------------------------------------------------------------*/
** Constants for analyzing the client id trace flags
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_cli_trc_sql  LIKE th_bitvalue VALUE 1,
*           th_cli_trc_tbuf LIKE th_bitvalue VALUE 2,
*           th_cli_trc_enq  LIKE th_bitvalue VALUE 4,
*           th_cli_trc_rfc  LIKE th_bitvalue VALUE 8,
*           th_cli_trc_auth LIKE th_bitvalue VALUE 16,
*           th_cli_trc_cmod LIKE th_bitvalue VALUE 32,
*           th_cli_trc_user LIKE th_bitvalue VALUE 64,
*           th_cli_trc_sat  LIKE th_bitvalue VALUE 4,
*           th_cli_trc_ws   LIKE th_bitvalue VALUE 8.
*
**-------------------------------------------------------------------*/
** Constants for taskhandler tests
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_test_msg           TYPE i VALUE 1,
*           th_test_soft_shutdown TYPE i VALUE 2.
*
**-------------------------------------------------------------------*/
** Constants for test TH_TEST_MSG
**-------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: th_test_send_msg_async         TYPE i VALUE 1,
*           th_test_send_msg_sync_and_wait TYPE i VALUE 2,
*           th_test_send_msg_sync_and_roll TYPE i VALUE 3,
*           th_test_send_msg_on_commit     TYPE i VALUE 4.
*
**--------------------------------------------------------------------*/
** Constants for acl file handling
**
** Must be in sync with kernel defines THRT_ACL_... in thxxinfo.c
**--------------------------------------------------------------------*/
*##NEEDED
*CONSTANTS: acl_display_dispatcher_file TYPE i VALUE 1,
*           acl_reload_dispatcher_file  TYPE i VALUE 2,
*           acl_reload_server_files     TYPE i VALUE 3,
*           acl_reload_all_files        TYPE i VALUE 4.
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

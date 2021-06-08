*&---------------------------------------------------------------------*
*& Report ZALV_SABLON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zalv_sablon.

TYPE-POOLS: slis.
INCLUDE <icon>.
DATA: d_ok.
DATA BEGIN OF i_report OCCURS 0 .
DATA : selkz TYPE c,
       matnr LIKE mara-matnr,
*   *       TYPE mara-matnr,
*         color TYPE slis_t_specialcol_alv,
       END OF i_report.
DATA gv_count TYPE i.
DATA: i_color_tab  TYPE slis_t_specialcol_alv.
DATA: wa_color_tab TYPE slis_specialcol_alv.
DATA: g_cell_color TYPE slis_color.
*  Internal table for list catalog
DATA: i_fieldcat  TYPE slis_t_fieldcat_alv,
      gs_fieldcat TYPE LINE OF slis_t_fieldcat_alv.
DATA: d_heading TYPE slis_t_listheader.
DATA: it_events    TYPE slis_alv_event OCCURS 0 WITH HEADER LINE,
      it_excluding TYPE slis_t_extab,
      d_repname    LIKE sy-repid.
DATA: l_layout  TYPE slis_layout_alv.
*  Ranges
************************************************************************
**
*C o n s t a n t s                                                   *
************************************************************************
**
CONSTANTS: formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.
************************************************************************
**
*                    FIELD-SYMBOLS
************************************************************************
**

************************************************************************
*      * Parameters                                                    *
************************************************************************
*  selection screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
*SELECT-OPTIONS :
SELECTION-SCREEN END OF BLOCK b1.
************************************************************************
*  INITIALIZATION
************************************************************************
INITIALIZATION.
  PERFORM initial_values.
************************************************************************
*  AT SELECTION-SCREEN                                                 *
************************************************************************

************************************************************************
*  M a i n  P r o c e s s                                              *
* **********************************************************************
START-OF-SELECTION.
  CALL FUNCTION 'DB_COMMIT'.
  PERFORM main_sel.
************************************************************************
END-OF-SELECTION.
  CALL FUNCTION 'DB_COMMIT'.
  IF d_ok EQ space.
    MESSAGE s208(fz).
    "No data records found for these selection criteria
    EXIT.
  ELSEIF d_ok EQ 'E'.
    MESSAGE s024(zsd).
    "Bayi Sistemine Eri?im Hatasy!
    EXIT.

  ENDIF.

*- ALV output
  PERFORM report.
*&---------------------------------------------------------------------*
*&      Form  INITIAL_VALUES
*&---------------------------------------------------------------------*
FORM initial_values.
  CLEAR: d_ok.
  CLEAR: i_report, i_report[].
  CLEAR: i_fieldcat[], l_layout, d_heading, it_events[], it_excluding[].
ENDFORM.                    " INITIAL_VALUES
*&---------------------------------------------------------------------*
*&      Form  REPORT
*&---------------------------------------------------------------------*

FORM report.
  PERFORM list_layout_specification.
  PERFORM create_field_catalog USING 'I_REPORT'.
  PERFORM call_alv_display TABLES i_report .
*
ENDFORM.                    " REPORT
*&---------------------------------------------------------------------*
*&      Form  create_field_catalog
*&---------------------------------------------------------------------*
FORM create_field_catalog USING  alvtabname TYPE slis_tabname .
  DATA: i_fcat     TYPE slis_t_fieldcat_alv,
        l_fcat     TYPE LINE OF slis_t_fieldcat_alv,
        r_fieldcat TYPE slis_fieldcat_alv,
        d_text(40) TYPE c.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = alvtabname
      i_inclname             = sy-cprog
    CHANGING
      ct_fieldcat            = i_fcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  l_fcat-key = space.
  MODIFY i_fcat FROM l_fcat TRANSPORTING key WHERE key NE space.
  i_fieldcat[] = i_fcat[].
  LOOP AT i_fieldcat INTO r_fieldcat.
    CLEAR d_text.
    CASE r_fieldcat-fieldname .
      WHEN 'SELKZ'.
        r_fieldcat-no_out = 'X'.
    ENDCASE.
    IF d_text NE space.
      MOVE d_text TO: r_fieldcat-seltext_l,
                     r_fieldcat-seltext_m,
                    r_fieldcat-seltext_s,
                  r_fieldcat-reptext_ddic.
    ENDIF.
    MODIFY i_fieldcat FROM r_fieldcat.
  ENDLOOP.
  PERFORM build_header.
  PERFORM build_eventtab.
ENDFORM.                    " create_field_catalog
*---------------------------------------------------------------------*
*       FORM BUILD_HEADER                                             *
*---------------------------------------------------------------------*
FORM build_header.
  DATA: hline    TYPE slis_listheader,
        text(60) TYPE c,
        w_inx    LIKE sy-tabix.
  CLEAR d_heading[].
  CLEAR: hline, text.
  hline-typ  = 'H'.
  WRITE: sy-title(60) TO text CENTERED.
  hline-info = text.
  hline-info = sy-title.
  APPEND hline TO d_heading.
  CLEAR text.
  hline-typ  = 'H'.
*  WRITE: 'User: '(hd1) TO text,
*         sy-uname TO text+6,
*         'Date: '(hd2) TO text+25,
*         sy-datum TO text+31,
*         'Time: '(hd3) TO text+45,
*         sy-uzeit TO text+51.
  DATA lv_count(10) TYPE c.
  lv_count = gv_count.
  CONCATENATE 'Kayit Sayisi:' lv_count
            INTO text SEPARATED BY space.

  hline-info = text.
  APPEND hline TO d_heading.
ENDFORM.                    " BUILD_HEADER
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTTAB
*&---------------------------------------------------------------------*
FORM build_eventtab.
  DATA: ls_event TYPE slis_alv_event,
        ls_exclu TYPE LINE OF slis_t_extab.

  CLEAR: it_events[], it_excluding[].

  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = it_events[].
  READ TABLE it_events WITH KEY name = slis_ev_top_of_page
                           INTO ls_event.
  IF sy-subrc = 0.
    MOVE formname_top_of_page TO ls_event-form.
    APPEND ls_event TO it_events.
  ENDIF.
* Menu Functions to be excluded
  ls_exclu-fcode = '&CRB'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&CRL'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&CRR'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&CRE'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&AQW'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '%SL'. APPEND ls_exclu TO it_excluding.
  ls_exclu-fcode = '&ABC'. APPEND ls_exclu TO it_excluding.
ENDFORM.                    " BUILD_EVENTTAB
*---------------------------------------------------------------------*
*       FORM LIST_LAYOUT_SPECIFICATION                                *
*---------------------------------------------------------------------*
FORM list_layout_specification.
  CLEAR l_layout.
  MOVE:  'X'    TO l_layout-colwidth_optimize,
           'X'    TO l_layout-zebra,
            'X'    TO l_layout-detail_initial_lines.
  l_layout-coltab_fieldname = 'COLOR'. "info_fieldname = 'COLOR'.
  l_layout-box_fieldname = 'SELKZ'.
ENDFORM.                    " list_layout_specification
*&---------------------------------------------------------------------*
*&      Form  CALL_ALV_DISPLAY
*&---------------------------------------------------------------------*
FORM call_alv_display TABLES alvtable TYPE STANDARD TABLE.
  DATA: w_func LIKE tfdir-funcname.
* w_func = 'REUSE_ALV_LIST_DISPLAY'.
  w_func = 'REUSE_ALV_GRID_DISPLAY'.
  CALL FUNCTION w_func
    EXPORTING
      i_callback_program       = sy-cprog
      i_callback_pf_status_set = 'SET_PF_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = l_layout
      i_default                = 'X'
      i_save                   = 'X'
*     IS_VARIANT               = D_VARIANT1
      it_fieldcat              = i_fieldcat[]
      it_excluding             = it_excluding[]
*     IT_SORT                  = I_SORT[]
*     IT_FILTER                = I_FILTER[]
      it_events                = it_events[]
    TABLES
      t_outtab                 = alvtable
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
ENDFORM.                    " CALL_ALV_DISPLAY
*---------------------------------------------------------------------*
*       FORM TOP_OF_PAGE                                              *
*---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = d_heading.
ENDFORM.                    "TOP_OF_PAGE
*----------------------------------------------------------------------*
*&      Form  MAIN_SEL

*----------------------------------------------------------------------*
FORM main_sel .
  PERFORM get_data .
  CHECK NOT i_report[] IS INITIAL.
  d_ok = 'X'.
ENDFORM.                    " MAIN_SEL
*---------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*&      Form  USER_COMMAND
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM user_command
              USING v_ucomm  LIKE sy-ucomm
              v_selfld TYPE slis_selfield.
  CASE v_ucomm.
    WHEN '&IC1'.
    WHEN OTHERS.
  ENDCASE .
*  BREAK-POINT .
ENDFORM .                    "USER_COMMAND
*&--------------------------------------------------------------------
*&      Form  set_pf_status
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
*      -->P_STATUS   text
*---------------------------------------------------------------------*
FORM set_pf_status USING p_status.
  SET PF-STATUS 'STANDARD' EXCLUDING it_excluding.
ENDFORM.                    " SET_PF_STATUS
*&--------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_data .
  PERFORM fill_colour_columns.
  SELECT * FROM mara UP TO 10 ROWS INTO CORRESPONDING FIELDS OF TABLE
      i_report.
*  LOOP AT i_report.
*
**    MOVE i_color_tab TO i_report-color.
*    MODIFY i_report.
*  ENDLOOP.
  DESCRIBE TABLE i_report LINES gv_count.
ENDFORM.                    " GET_DATA
*&---------------------------------------------------------------------*
*&      Form  FILL_COLOUR_COLUMNS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_colour_columns .
  DEFINE color .
    g_cell_color-col = &1.
       g_cell_color-int = &2.
       wa_color_tab-fieldname = &3.
       wa_color_tab-color = g_cell_color.
     APPEND wa_color_tab TO i_color_tab.
  END-OF-DEFINITION.
  color : 7 1 'MENGE' .
  color : 7 1 'RMWWR' .
  color : 7 1 'NETPR' .
  color : 5 0 'BPMNG' .
  color : 4 0 'STOPAJ' .
  color : 4 0 'BAGKUR' .
  color : 4 0 'BORSA' .
  color : 4 0 'MERA' .
  color : 3 0 'WMWST1' .
  color : 3 0 'WRBTR' .


ENDFORM.                    " FILL_COLOUR_COLUMNS

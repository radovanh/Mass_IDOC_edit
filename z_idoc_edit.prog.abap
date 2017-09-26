*&---------------------------------------------------------------------*
*& Report  Z_IDOC_EDIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_idoc_edit.

TABLES: edidc, idocsyn.

SELECTION-SCREEN BEGIN OF BLOCK b1.
SELECT-OPTIONS: so_docnr FOR edidc-docnum,
                so_stat FOR edidc-status,
                so_mesty FOR edidc-mestyp,
                so_docty FOR edidc-idoctp.
SELECT-OPTIONS: so_segty FOR idocsyn-segtyp NO INTERVALS NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) txt_fld FOR FIELD pa_field.
PARAMETERS: pa_field TYPE edi_field OBLIGATORY.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) txt_old FOR FIELD pa_old.
PARAMETERS: pa_old TYPE text30.
SELECTION-SCREEN COMMENT 70(10) txt_any FOR FIELD pa_any.
PARAMETERS: pa_any AS CHECKBOX USER-COMMAND any.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) txt_new FOR FIELD pa_new.
PARAMETERS: pa_new TYPE text30.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) txt_tst FOR FIELD pa_test.
PARAMETERS: pa_test AS CHECKBOX.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.

DATA: gt_docnum TYPE SORTED TABLE OF edidc-docnum WITH UNIQUE KEY table_line.

TYPES: BEGIN OF ty_idocs,
        docnum TYPE edidc-docnum,
        result TYPE xfeld,
        old_value TYPE string,
        msg TYPE massmsg,
       END OF ty_idocs.
DATA: gt_idocs TYPE STANDARD TABLE OF ty_idocs.

INITIALIZATION.
  txt_fld = 'Field Name'.
  txt_old = 'Old Value'.
  txt_any = 'Any value'.
  txt_new = 'New Value'.
  txt_tst = 'Test Run'.

AT SELECTION-SCREEN OUTPUT." ON pa_any.
  IF NOT pa_any IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'PA_OLD'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.


START-OF-SELECTION.

  PERFORM f_get_idocs.
  PERFORM f_process_idocs.
  PERFORM f_display_result.


*&---------------------------------------------------------------------*
*&      Form  f_get_idocs
*&---------------------------------------------------------------------*
FORM f_get_idocs.

  SELECT DISTINCT docnum INTO TABLE gt_docnum
    FROM edidc
    INNER JOIN idocsyn ON edidc~idoctp EQ idocsyn~idoctyp
   WHERE docnum IN so_docnr
     AND status IN so_stat
     AND mestyp IN so_mesty
     AND idoctp IN so_docty
     AND segtyp IN so_segty.

ENDFORM.                    "f_get_idocs
*&---------------------------------------------------------------------*
*&      Form  F_PROCESS_IDOCS
*&---------------------------------------------------------------------*
FORM f_process_idocs .

  FIELD-SYMBOLS: <ls_docnum> LIKE LINE OF gt_docnum.
  FIELD-SYMBOLS: <ls_edidd> TYPE edidd.
  DATA: ls_edidc TYPE edidc.
  DATA: lt_edidd TYPE STANDARD TABLE OF edidd,
        ls_edidd LIKE LINE OF lt_edidd.
  FIELD-SYMBOLS: <ls_data> TYPE ANY.
  FIELD-SYMBOLS: <lv_value> TYPE ANY.
  DATA: lr_data TYPE REF TO data.
  FIELD-SYMBOLS: <ls_idocs> LIKE LINE OF gt_idocs.
  DATA:lv_update TYPE xfeld.

  LOOP AT gt_docnum ASSIGNING <ls_docnum>.

    APPEND INITIAL LINE TO gt_idocs ASSIGNING <ls_idocs>.
    <ls_idocs>-docnum = <ls_docnum>.

    CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_READ'
      EXPORTING
        document_number         = <ls_docnum>
      IMPORTING
        idoc_control            = ls_edidc
      EXCEPTIONS
        document_foreign_lock   = 1
        document_not_exist      = 2
        document_number_invalid = 3
        OTHERS                  = 4.

    IF sy-subrc NE 0.
      MOVE-CORRESPONDING sy TO <ls_idocs>-msg.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'EDI_SEGMENTS_GET_ALL'
      EXPORTING
        document_number         = <ls_docnum>
      TABLES
        idoc_containers         = lt_edidd
      EXCEPTIONS
        document_number_invalid = 1
        end_of_document         = 2
        OTHERS                  = 3.

    IF sy-subrc NE 0.
      MOVE-CORRESPONDING sy TO <ls_idocs>-msg.
      CONTINUE.
    ENDIF.

    READ TABLE lt_edidd ASSIGNING <ls_edidd> WITH KEY segnam = so_segty-low.
    CHECK sy-subrc EQ 0.

    CLEAR ls_edidd.

    UNASSIGN: <ls_data>, <lv_value>.

    CREATE DATA lr_data TYPE (<ls_edidd>-segnam).
    ASSIGN lr_data->* TO <ls_data>.

    IF <ls_data> IS ASSIGNED.
      <ls_data> = <ls_edidd>-sdata.
      ASSIGN COMPONENT pa_field OF STRUCTURE <ls_data> TO <lv_value>.
      IF <lv_value> IS ASSIGNED.
        <ls_idocs>-old_value = <lv_value>.
        IF ( <lv_value> EQ pa_old AND pa_any IS INITIAL ) OR
           ( pa_any IS NOT INITIAL ).
          <lv_value> = pa_new.
          ls_edidd = <ls_edidd>.
          ls_edidd-sdata = <ls_data>.
        ENDIF.
      ENDIF.
    ENDIF.

    IF NOT ls_edidd IS INITIAL.

      CALL FUNCTION 'EDI_DOCUMENT_OPEN_FOR_EDIT'
        EXPORTING
          document_number               = <ls_docnum>
        TABLES
          idoc_data                     = lt_edidd
        EXCEPTIONS
          document_foreign_lock         = 1
          document_not_exist            = 2
          document_not_open             = 3
          status_is_unable_for_changing = 4
          OTHERS                        = 5.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING sy TO <ls_idocs>-msg.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'EDI_CHANGE_DATA_SEGMENT'
        EXPORTING
          idoc_changed_data_record = ls_edidd
        EXCEPTIONS
          idoc_not_open            = 1
          data_record_not_exist    = 2
          OTHERS                   = 3.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING sy TO <ls_idocs>-msg.
        CONTINUE.
      ENDIF.

      IF NOT pa_test IS INITIAL.
        lv_update = space.
      ELSE.
        lv_update = 'X'.
      ENDIF.

      CALL FUNCTION 'EDI_DOCUMENT_CLOSE_EDIT'
        EXPORTING
          document_number = <ls_docnum>
          do_commit       = lv_update
          do_update       = lv_update
        EXCEPTIONS
          idoc_not_open   = 1
          db_error        = 2
          OTHERS          = 3.

      IF sy-subrc NE 0.
        MOVE-CORRESPONDING sy TO <ls_idocs>-msg.
        CONTINUE.
      ENDIF.

      <ls_idocs>-result = 'X'.

    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_PROCESS_IDOCS
*&---------------------------------------------------------------------*
*&      Form  F_DISPLAY_RESULT
*&---------------------------------------------------------------------*
FORM f_display_result .

  FIELD-SYMBOLS: <ls_idocs> LIKE LINE OF gt_idocs.
  DATA: lv_message_text TYPE string.

  FORMAT INTENSIFIED ON.
  WRITE: / 'IDoc number', 20 'Old Value', 50 'Comment'.
  FORMAT INTENSIFIED OFF.

  LOOP AT gt_idocs ASSIGNING <ls_idocs>.
    IF NOT <ls_idocs>-result IS INITIAL.
      WRITE: / <ls_idocs>-docnum, 20 <ls_idocs>-old_value, 50 'IDOC changed successfully'.
    ELSE.
      CALL FUNCTION 'MESSAGE_TEXT_BUILD'
        EXPORTING
          msgid               = <ls_idocs>-msg-msgid
          msgnr               = <ls_idocs>-msg-msgno
          msgv1               = <ls_idocs>-msg-msgv1
          msgv2               = <ls_idocs>-msg-msgv2
          msgv3               = <ls_idocs>-msg-msgv3
          msgv4               = <ls_idocs>-msg-msgv4
        IMPORTING
          message_text_output = lv_message_text.
      WRITE: / <ls_idocs>-docnum, 50 lv_message_text.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " F_DISPLAY_RESULT

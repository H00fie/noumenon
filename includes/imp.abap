*&---------------------------------------------------------------------*
*&  Include           NOUMENON_IMP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_element_remover IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_element_remover IMPLEMENTATION.
  METHOD hide_onli.
    DATA: lt_tab TYPE TABLE OF sy-ucomm.
    APPEND 'ONLI' TO lt_tab.
    CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
      EXPORTING
        p_status        = sy-pfkey
      TABLES
        p_exclude       = lt_tab.
  ENDMETHOD.                    "hide_onli
ENDCLASS.                    "lcl_element_remover IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_visibility_dispenser IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_visibility_dispenser IMPLEMENTATION.
  METHOD make_all_blocks_inv.
    LOOP AT SCREEN.
      IF screen-group1 = 'ID1' OR screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5'.
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "make_all_blocks_inv
ENDCLASS.                    "lcl_visibility_dispenser IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_abap_displayer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_displayer IMPLEMENTATION.
  METHOD pick_random_abap.
    DATA: lv_random_number TYPE i.
    lv_random_number = generate_random( ).
    SELECT SINGLE content
      FROM cs_facts
       INTO lv_abap_fact
        WHERE id = lv_random_number.
  ENDMETHOD.                    "pick_random_abap

  METHOD generate_random.
    DATA lv_result TYPE i.
    CALL FUNCTION 'RANDOM_I4'
      EXPORTING
        RND_MIN         = 1
        RND_MAX         = 3
      IMPORTING
        RND_VALUE       = lv_result.
    r_random = lv_result.
  ENDMETHOD.                    "generate_random
ENDCLASS.                    "lcl_abap_displayer IMPLEMENTATION
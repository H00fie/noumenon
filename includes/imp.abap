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
      IF screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5' OR screen-group1 = 'ID6' OR screen-group1 = 'ID7' OR screen-group1 = 'ID8'.
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "make_all_blocks_inv

  METHOD make_block_visible.
    CASE i_marker.
      WHEN 'ID2'.
        LOOP AT SCREEN.
          IF screen-group1 = 'ID1' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5' OR screen-group1 = 'ID6' OR screen-group1 = 'ID8'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      WHEN 'ID3'.
        LOOP AT SCREEN.
          IF screen-group1 = 'ID1' OR screen-group1 = 'ID2' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5' OR screen-group1 = 'ID6' OR screen-group1 = 'ID8'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      WHEN 'ID4'.
        LOOP AT SCREEN.
          IF screen-group1 = 'ID1' OR screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID5' OR screen-group1 = 'ID6' OR screen-group1 = 'ID8'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      WHEN 'ID5'.
        LOOP AT SCREEN.
          IF screen-group1 = 'ID1' OR screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID6' OR screen-group1 = 'ID8'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      WHEN 'ID6'.
        LOOP AT SCREEN.
          IF screen-group1 = 'ID1' OR screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5' OR screen-group1 = 'ID8'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      WHEN 'ID7'.
        LOOP AT SCREEN.
          IF screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5' OR screen-group1 = 'ID6' OR screen-group1 = 'ID7' OR screen-group1 = 'ID8'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      WHEN 'ID8'.
        LOOP AT SCREEN.
          IF screen-group1 = 'ID1' OR screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5' OR screen-group1 = 'ID6'.
            screen-invisible = '1'.
            screen-input = '0'.
            MODIFY SCREEN.
          ELSE.
            screen-invisible = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
    ENDCASE.
  ENDMETHOD.                    "make_block_visible
ENDCLASS.                    "lcl_visibility_dispenser IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_screen_adjuster IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_screen_adjuster IMPLEMENTATION.
  METHOD constructor.
    me->lo_element_remover = i_lo_element_remover.
    me->lo_visibility_dispenser = i_lo_visibility_dispenser.
  ENDMETHOD.                    "make_all_blocks_inv

  METHOD adjust_screen.
    lo_element_remover->hide_onli( ).
    lo_visibility_dispenser->make_block_visible( decide_marker( ) ).
  ENDMETHOD.                    "adjust_screen

  METHOD decide_marker.
    CASE gv_action_to_perform.
      WHEN 'ABAP'.
        r_marker = 'ID2'.
      WHEN 'CS'.
        r_marker = 'ID3'.
      WHEN 'JAVA'.
        r_marker = 'ID4'.
      WHEN 'Kotlin'.
        r_marker = 'ID5'.
      WHEN 'All'.
        r_marker = 'ID6'.
      WHEN 'Return'.
        r_marker = 'ID7'.
      WHEN 'ABAP_add'.
        r_marker = 'ID8'.
    ENDCASE.
  ENDMETHOD.                    "decide_marker
ENDCLASS.                    "lcl_screen_adjuster IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_abap_displayer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_displayer IMPLEMENTATION.
  METHOD lif_category~add_fact.
    DATA: lwa_zbmierzwitest TYPE zbmierzwitest,
          lv_incremented_id TYPE i.
    lv_incremented_id = check_last_id( ) + 1.
*    lwa_zbmierzwitest-id
*    lwa_zbmierzwitest-category
*    lwa_zbmierzwitest-title
*    lwa_zbmierzwitest-content
  ENDMETHOD.                    "add_abap_fact

  METHOD lif_category~pick_random.
    DATA: lv_random_number TYPE i,
          lv_abap_fact     TYPE string.
    lv_random_number = generate_random( ).
    SELECT SINGLE content
      FROM zbmierzwitest
       INTO lv_abap_fact
        WHERE id = lv_random_number.
  ENDMETHOD.                    "pick_random_abap

  METHOD check_last_id.
    DATA: lv_latest_id TYPE i.
    SELECT MAX( id )
      FROM zbmierzwitest
       INTO lv_latest_id.
    r_latest_id = lv_latest_id.
  ENDMETHOD.                    "check_last_id

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

*----------------------------------------------------------------------*
*       CLASS lcl_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory IMPLEMENTATION.
  METHOD provide_object.
    CASE sy-ucomm.
      WHEN 'FC1'.
        DATA(lo_abap_displayer) = NEW lcl_abap_displayer( ).
        r_o_category = lo_abap_displayer.
    ENDCASE.
  ENDMETHOD.                    "provide_object
ENDCLASS.                    "lcl_factory IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_action_handler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_action_handler IMPLEMENTATION.
  METHOD constructor.
    lo_category = i_o_category.
  ENDMETHOD.                    "constructor

  METHOD decide_action.
      CASE sy-ucomm.
        WHEN 'FC1'.
          gv_action_to_perform = 'ABAP'.
        WHEN 'FC2'.
          gv_action_to_perform = 'CS'.
        WHEN 'FC3'.
          gv_action_to_perform = 'JAVA'.
        WHEN 'FC4'.
          gv_action_to_perform = 'Kotlin'.
        WHEN 'FC5'.
          gv_action_to_perform = 'All'.
        WHEN 'FC15'.
          gv_action_to_perform = 'Return'.
        WHEN 'FC6'.
          gv_action_to_perform = 'ABAP_add'.
      ENDCASE.
  ENDMETHOD.                    "decide_action

  METHOD get_lo_category.
    r_lo_category = lo_category.
  ENDMETHOD.                    "get_lo_category
ENDCLASS.                    "lcl_action_handler IMPLEMENTATION
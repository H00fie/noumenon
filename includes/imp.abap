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
        gv_return_stage = 1.
        set_visibility( i_to_hide = 'ID1ID3ID4ID5ID6ID8' ).
      WHEN 'ID3'.
        gv_return_stage = 1.
        set_visibility( i_to_hide = 'ID1ID2ID4ID5ID6ID8' ).
      WHEN 'ID4'.
        gv_return_stage = 1.
        set_visibility( i_to_hide = 'ID1ID2ID3ID5ID6ID8' ).
      WHEN 'ID5'.
        gv_return_stage = 1.
        set_visibility( i_to_hide = 'ID1ID2ID3ID4ID6ID8' ).
      WHEN 'ID6'.
        gv_return_stage = 1.
        set_visibility( i_to_hide = 'ID1ID2ID3ID4ID5ID8' ).
      WHEN 'ID7'.
        CASE gv_return_stage.
          WHEN 0.
            set_visibility( i_to_hide = 'ID2ID3ID4ID5ID6ID7ID8' ).
          WHEN 1.
            set_visibility( i_to_hide = 'ID1ID3ID4ID5ID6ID8' ).
            gv_return_stage = 0.
        ENDCASE.
      WHEN 'ID8'.
        gv_return_stage = 1.
        set_visibility( i_to_hide = 'ID1ID2ID3ID4ID5ID6' ).
    ENDCASE.
  ENDMETHOD.                    "make_block_visible

  METHOD set_visibility.
    DATA: lv_one   TYPE string,
          lv_two   TYPE string,
          lv_three TYPE string,
          lv_four  TYPE string,
          lv_five  TYPE string,
          lv_six   TYPE string,
          lv_seven TYPE string.
    cut_string( EXPORTING i_to_cut = i_to_hide
                IMPORTING e_one   = lv_one
                          e_two   = lv_two
                          e_three = lv_three
                          e_four  = lv_four
                          e_five  = lv_five
                          e_six   = lv_six
                          e_seven = lv_seven ).
    LOOP AT SCREEN.
      IF strlen( i_to_hide ) = 21.
        IF screen-group1 = lv_one OR screen-group1 = lv_two OR screen-group1 = lv_three OR screen-group1 = lv_four OR screen-group1 = lv_five OR screen-group1 = lv_six OR screen-group1 = lv_seven.
          screen-invisible = '1'.
          screen-input = '0'.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = '0'.
          screen-input = '1'.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = lv_one OR screen-group1 = lv_two OR screen-group1 = lv_three OR screen-group1 = lv_four OR screen-group1 = lv_five OR screen-group1 = lv_six.
          screen-invisible = '1'.
          screen-input = '0'.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = '0'.
          screen-input = '1'.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "set_visibility

  METHOD cut_string.
    e_one   = i_to_cut+0(3).
    e_two   = i_to_cut+3(3).
    e_three = i_to_cut+6(3).
    e_four  = i_to_cut+9(3).
    e_five  = i_to_cut+12(3).
    e_six   = i_to_cut+15(3).
    IF strlen( i_to_cut ) = 21.
      e_seven = i_to_cut+18(3).
    ENDIF.
  ENDMETHOD.                    "cut_string
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
    lwa_zbmierzwitest-id      = lv_incremented_id.
    lwa_zbmierzwitest-title   = p_tit.
    lwa_zbmierzwitest-category   = 'ABAP'.
    lwa_zbmierzwitest-content = p_con.
    INSERT zbmierzwitest FROM lwa_zbmierzwitest.
    IF sy-subrc = 0.
      MESSAGE 'The record has been added.' TYPE 'I'.
    ELSE.
      MESSAGE 'The error has occured.' TYPE 'I'.
    ENDIF.
  ENDMETHOD.                    "add_abap_fact

  METHOD lif_category~pick_random.
    DATA: lv_random_number TYPE i,
          lt_fact TYPE zbmierzwitest.
    lv_random_number = generate_random( ).
    SELECT SINGLE *
      FROM zbmierzwitest
       INTO lt_fact
        WHERE id = lv_random_number.
    set_mt_fact( i_mt_fact = lt_fact ).
  ENDMETHOD.                    "pick_random_abap

  METHOD check_last_id.
    DATA: lv_latest_id TYPE i.
    SELECT MAX( id )
      FROM zbmierzwitest
       INTO lv_latest_id.
    IF sy-subrc <> 0.
      r_latest_id = 1.
    ELSE.
      r_latest_id = lv_latest_id.
    ENDIF.
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

  METHOD get_mt_fact.
    r_mt_fact = mt_fact.
  ENDMETHOD.                    "get_mt_fact

  METHOD set_mt_fact.
    mt_fact = i_mt_fact.
  ENDMETHOD.                    "set_mt_fact
ENDCLASS.                    "lcl_abap_displayer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory IMPLEMENTATION.
  METHOD provide_object.
    CASE sy-ucomm.
      WHEN 'FC16' OR 'FC7'.
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
        WHEN 'FC16'.
          lo_category->add_fact( ).
        WHEN 'FC7'.
          lo_category->pick_random( ).
      ENDCASE.
  ENDMETHOD.                    "decide_action

  METHOD get_lo_category.
    r_lo_category = lo_category.
  ENDMETHOD.                    "get_lo_category
ENDCLASS.                    "lcl_action_handler IMPLEMENTATION
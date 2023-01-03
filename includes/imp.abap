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
      IF screen-group1 = 'ID2' OR screen-group1 = 'ID3' OR screen-group1 = 'ID4' OR screen-group1 = 'ID5' OR screen-group1 = 'ID6' OR screen-group1 = 'ID7' OR screen-group1 = 'ID8' OR screen-group1 = 'ID9'.
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
        adjust_pre_return_action( i_action = 'ID2' ).
        set_visibility( i_to_hide = 'ID1ID3ID4ID5ID6ID8ID9' ).
      WHEN 'ID3'.
        gv_return_stage = 1.
        adjust_pre_return_action( i_action = 'ID3' ).
        set_visibility( i_to_hide = 'ID1ID2ID4ID5ID6ID8ID9' ).
      WHEN 'ID4'.
        gv_return_stage = 1.
        adjust_pre_return_action( i_action = 'ID4' ).
        set_visibility( i_to_hide = 'ID1ID2ID3ID5ID6ID8ID9' ).
      WHEN 'ID5'.
        gv_return_stage = 1.
        adjust_pre_return_action( i_action = 'ID5' ).
        set_visibility( i_to_hide = 'ID1ID2ID3ID4ID6ID8ID9' ).
      WHEN 'ID6'.
        gv_return_stage = 1.
        adjust_pre_return_action( i_action = 'ID6' ).
        set_visibility( i_to_hide = 'ID1ID2ID3ID4ID5ID8ID9' ).
      WHEN 'ID7'.
        CASE gv_return_stage.
          WHEN 1.
            set_visibility( i_to_hide = 'ID2ID3ID4ID5ID6ID7ID8ID9' ).
          WHEN 2.
            CASE pre_return_action.
              WHEN 'ABAP'.
                set_visibility( i_to_hide = 'ID1ID3ID4ID5ID6ID8ID9' ).
                gv_return_stage = 1.
              WHEN 'CS'.
                set_visibility( i_to_hide = 'ID1ID2ID4ID5ID6ID8ID9' ).
                gv_return_stage = 1.
              WHEN 'JAVA'.
                set_visibility( i_to_hide = 'ID1ID2ID3ID5ID6ID8ID9' ).
                gv_return_stage = 1.
              WHEN 'Kotlin'.
                set_visibility( i_to_hide = 'ID1ID2ID3ID4ID6ID8ID9' ).
                gv_return_stage = 1.
              WHEN 'All'.
                set_visibility( i_to_hide = 'ID1ID2ID3ID4ID5ID8ID9' ).
                gv_return_stage = 1.
            ENDCASE.
        ENDCASE.
      WHEN 'ID8'.
        gv_return_stage = 2.
        set_visibility( i_to_hide = 'ID1ID2ID3ID4ID5ID6ID9' ).
      WHEN 'ID9'.
        gv_return_stage = 2.
        set_visibility( i_to_hide = 'ID1ID2ID3ID4ID5ID6ID8' ).
    ENDCASE.
  ENDMETHOD.                    "make_block_visible

  METHOD adjust_pre_return_action.
    CASE i_action.
      WHEN 'ID2'.
        pre_return_action = 'ABAP'.
      WHEN 'ID3'.
        pre_return_action = 'CS'.
      WHEN 'ID4'.
        pre_return_action = 'JAVA'.
      WHEN 'ID5'.
        pre_return_action = 'Kotlin'.
      WHEN 'ID6'.
        pre_return_action = 'All'.
    ENDCASE.
  ENDMETHOD.                    "adjust_pre_return_action

  METHOD set_visibility.
    DATA: lt_id_tab  TYPE STANDARD TABLE OF zidvalues,
          lwa_id_tab TYPE zidvalues,
          lv_counter TYPE i VALUE 0,
          lv_one     TYPE string,
          lv_two     TYPE string,
          lv_three   TYPE string,
          lv_four    TYPE string,
          lv_five    TYPE string,
          lv_six     TYPE string,
          lv_seven   TYPE string,
          lv_eight   TYPE string.
    cut_string( EXPORTING i_to_cut = i_to_hide
                IMPORTING e_id_tab = lt_id_tab ).
    LOOP AT lt_id_tab INTO lwa_id_tab.
      CASE lv_counter.
        WHEN 0.
          lv_one = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
        WHEN 1.
          lv_two = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
        WHEN 2.
          lv_three = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
        WHEN 3.
          lv_four = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
        WHEN 4.
          lv_five = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
        WHEN 5.
          lv_six = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
        WHEN 6.
          lv_seven = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
        WHEN 7.
          lv_eight = lwa_id_tab-value.
          lv_counter = lv_counter + 1.
      ENDCASE.
    ENDLOOP.
    LOOP AT SCREEN.
      IF strlen( i_to_hide ) = str_len_when_back_to_ini_scr.
        IF screen-group1 = lv_one OR screen-group1 = lv_two OR screen-group1 = lv_three OR screen-group1 = lv_four OR screen-group1 = lv_five OR screen-group1 = lv_six OR screen-group1 = lv_seven OR screen-group1 = lv_eight.
          screen-invisible = '1'.
          screen-input = '0'.
          MODIFY SCREEN.
        ELSE.
          screen-invisible = '0'.
          screen-input = '1'.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = lv_one OR screen-group1 = lv_two OR screen-group1 = lv_three OR screen-group1 = lv_four OR screen-group1 = lv_five OR screen-group1 = lv_six OR screen-group1 = lv_seven.
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
    DATA: lt_id_tab  TYPE TABLE OF zidvalues,
          lwa_id_tab TYPE zidvalues.
    CLEAR lwa_id_tab.
    lwa_id_tab-value = i_to_cut+0(3).
    APPEND lwa_id_tab TO lt_id_tab.
    CLEAR lwa_id_tab.
    lwa_id_tab-value = i_to_cut+3(3).
    APPEND lwa_id_tab TO lt_id_tab.
    CLEAR lwa_id_tab.
    lwa_id_tab-value = i_to_cut+6(3).
    APPEND lwa_id_tab TO lt_id_tab.
    lwa_id_tab-value = i_to_cut+9(3).
    APPEND lwa_id_tab TO lt_id_tab.
    CLEAR lwa_id_tab.
    lwa_id_tab-value = i_to_cut+12(3).
    APPEND lwa_id_tab TO lt_id_tab.
    CLEAR lwa_id_tab.
    lwa_id_tab-value = i_to_cut+15(3).
    APPEND lwa_id_tab TO lt_id_tab.
    CLEAR lwa_id_tab.
    lwa_id_tab-value = i_to_cut+18(3).
    APPEND lwa_id_tab TO lt_id_tab.
    CLEAR lwa_id_tab.
    IF strlen( i_to_cut ) = str_len_when_back_to_ini_scr.
      lwa_id_tab-value = i_to_cut+21(3).
      APPEND lwa_id_tab TO lt_id_tab.
      CLEAR lwa_id_tab.
    ENDIF.
    e_id_tab = lt_id_tab.
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
      WHEN 'ABAP_by_id'.
        r_marker = 'ID9'.
    ENDCASE.
  ENDMETHOD.                    "decide_marker
ENDCLASS.                    "lcl_screen_adjuster IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_abap_displayer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_displayer IMPLEMENTATION.
  METHOD constructor.
    me->o_salv = i_o_salv.
  ENDMETHOD.                    "constructor

  METHOD lif_category~add_fact.
    DATA: lwa_zcsfacts TYPE zcsfacts,
          lv_incremented_id TYPE i.
    lv_incremented_id = lif_category~check_last_id( ) + 1.
    lwa_zcsfacts-id       = lv_incremented_id.
    lwa_zcsfacts-title    = p_tit.
    lwa_zcsfacts-category = 'ABAP'.
    lwa_zcsfacts-content  = p_con.
    INSERT zcsfacts FROM lwa_zcsfacts.
    IF sy-subrc = 0.
      MESSAGE 'The record has been added.' TYPE 'I'.
    ELSE.
      MESSAGE 'The error has occured.' TYPE 'I'.
    ENDIF.
  ENDMETHOD.                    "add_fact

  METHOD lif_category~pick_random.
    DATA: lv_random_number TYPE i,
          lt_fact TYPE zcsfacts.
    lv_random_number = lif_category~generate_random( ).
    CLEAR lt_fact.
    SELECT SINGLE *
      FROM zcsfacts
       INTO lt_fact
        WHERE id = lv_random_number.
    set_wa_fact( i_wa_fact = lt_fact ).
    lif_category~display_fact( ).
  ENDMETHOD.                    "pick_random_abap

  METHOD lif_category~pick_by_id.
    DATA: lt_fact        TYPE zcsfacts,
          lv_if_id_found TYPE boolean.
    CLEAR: lt_fact,
           lv_if_id_found.
    lv_if_id_found = lif_category~check_if_id_exists( i_id_to_check = i_id ).
    IF lv_if_id_found = abap_false.
      MESSAGE 'No record of provided ID has been found in ABAP category.' TYPE 'I'.
    ELSE.
      SELECT SINGLE *
        FROM zcsfacts
          INTO lt_fact
            WHERE id = i_id.
      set_wa_fact( i_wa_fact = lt_fact ).
      lif_category~display_fact( ).
    ENDIF.
  ENDMETHOD.                    "pick_by_id

  METHOD lif_category~check_last_id.
    DATA: lv_latest_id TYPE i.
    SELECT MAX( id )
      FROM zcsfacts
       INTO lv_latest_id.
    IF sy-subrc <> 0.
      r_latest_id = 1.
    ELSE.
      r_latest_id = lv_latest_id.
    ENDIF.
  ENDMETHOD.                    "check_last_id

  METHOD lif_category~display_fact.
    DATA: lt_fact TYPE STANDARD TABLE OF zcsfacts.
    APPEND wa_fact TO lt_fact.
    o_salv->display_alv( CHANGING c_lt_tab = lt_fact ).
  ENDMETHOD.                    "display_fact

  METHOD lif_category~generate_random.
    DATA: lv_result         TYPE i,
          lv_record_present TYPE boolean VALUE abap_false.
    WHILE lv_record_present = abap_false.
        CALL FUNCTION 'RANDOM_I4'
          EXPORTING
            RND_MIN         = 1
            RND_MAX         = lif_category~check_the_number_of_records( )
          IMPORTING
            RND_VALUE       = lv_result.
      lv_record_present = lif_category~check_category( i_randomized_id = lv_result ).
    ENDWHILE.
    r_random = lv_result.
  ENDMETHOD.                    "generate_random

  METHOD lif_category~check_category.
    DATA: lv_category(4) TYPE c.
    SELECT SINGLE category
      FROM zcsfacts
        INTO lv_category
          WHERE id = i_randomized_id
          AND category = 'ABAP'.
    IF lv_category IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_category

  METHOD lif_category~check_if_id_exists.
    DATA: lv_found_id TYPE i.
    SELECT SINGLE id
      FROM zcsfacts
        INTO lv_found_id
          WHERE id = i_id_to_check
          AND category = 'ABAP'.
    IF lv_found_id IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_if_id_exists

  METHOD lif_category~check_the_number_of_records.
    DATA: lv_num_of_records TYPE i.
    SELECT COUNT( * )
      FROM zcsfacts
        INTO lv_num_of_records.
    r_num_of_records = lv_num_of_records.
  ENDMETHOD.                    "check_the_number_of_records

  METHOD get_wa_fact.
    r_wa_fact = wa_fact.
  ENDMETHOD.                    "get_mt_fact

  METHOD set_wa_fact.
    wa_fact = i_wa_fact.
  ENDMETHOD.                    "set_mt_fact
ENDCLASS.                    "lcl_abap_displayer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_cs_displayer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cs_displayer IMPLEMENTATION.
  METHOD constructor.
    me->o_salv = i_o_salv.
  ENDMETHOD.                    "constructor

  METHOD lif_category~add_fact.
    DATA: lwa_zcsfacts TYPE zcsfacts,
          lv_incremented_id TYPE i.
    lv_incremented_id = lif_category~check_last_id( ) + 1.
    lwa_zcsfacts-id       = lv_incremented_id.
    lwa_zcsfacts-title    = p_tit.
    lwa_zcsfacts-category = 'CS'.
    lwa_zcsfacts-content  = p_con.
    INSERT zcsfacts FROM lwa_zcsfacts.
    IF sy-subrc = 0.
      MESSAGE 'The record has been added.' TYPE 'I'.
    ELSE.
      MESSAGE 'The error has occured.' TYPE 'I'.
    ENDIF.
  ENDMETHOD.                    "add_fact

  METHOD lif_category~pick_random.
    DATA: lv_random_number TYPE i,
          lt_fact TYPE zcsfacts.
    lv_random_number = lif_category~generate_random( ).
    CLEAR lt_fact.
    SELECT SINGLE *
      FROM zcsfacts
       INTO lt_fact
        WHERE id = lv_random_number.
    set_wa_fact( i_wa_fact = lt_fact ).
    lif_category~display_fact( ).
  ENDMETHOD.                    "pick_random_abap

  METHOD lif_category~pick_by_id.
    DATA: lt_fact        TYPE zcsfacts,
          lv_if_id_found TYPE boolean.
    CLEAR: lt_fact,
           lv_if_id_found.
    lv_if_id_found = lif_category~check_if_id_exists( i_id_to_check = i_id ).
    IF lv_if_id_found = abap_false.
      MESSAGE 'No record of provided ID has been found in CS category.' TYPE 'I'.
    ELSE.
      SELECT SINGLE *
        FROM zcsfacts
          INTO lt_fact
            WHERE id = i_id.
      set_wa_fact( i_wa_fact = lt_fact ).
      lif_category~display_fact( ).
    ENDIF.
  ENDMETHOD.                    "pick_by_id

  METHOD lif_category~check_last_id.
    DATA: lv_latest_id TYPE i.
    SELECT MAX( id )
      FROM zcsfacts
       INTO lv_latest_id.
    IF sy-subrc <> 0.
      r_latest_id = 1.
    ELSE.
      r_latest_id = lv_latest_id.
    ENDIF.
  ENDMETHOD.                    "check_last_id

  METHOD lif_category~display_fact.
    DATA: lt_fact TYPE STANDARD TABLE OF zcsfacts.
    APPEND wa_fact TO lt_fact.
    o_salv->display_alv( CHANGING c_lt_tab = lt_fact ).
  ENDMETHOD.                    "display_fact

  METHOD lif_category~generate_random.
    DATA: lv_result         TYPE i,
          lv_record_present TYPE boolean VALUE abap_false.
    WHILE lv_record_present = abap_false.
        CALL FUNCTION 'RANDOM_I4'
          EXPORTING
            RND_MIN         = 1
            RND_MAX         = lif_category~check_the_number_of_records( )
          IMPORTING
            RND_VALUE       = lv_result.
      lv_record_present = lif_category~check_category( i_randomized_id = lv_result ).
    ENDWHILE.
    r_random = lv_result.
  ENDMETHOD.                    "generate_random

  METHOD lif_category~check_category.
    DATA: lv_category(4) TYPE c.
    SELECT SINGLE category
      FROM zcsfacts
        INTO lv_category
          WHERE id = i_randomized_id
          AND category = 'CS'.
    IF lv_category IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_category

  METHOD lif_category~check_if_id_exists.
    DATA: lv_found_id TYPE i.
    SELECT SINGLE id
      FROM zcsfacts
        INTO lv_found_id
          WHERE id = i_id_to_check
		  AND category = 'CS'.
    IF lv_found_id IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_if_id_exists

  METHOD lif_category~check_the_number_of_records.
    DATA: lv_num_of_records TYPE i.
    SELECT COUNT( * )
      FROM zcsfacts
        INTO lv_num_of_records.
    r_num_of_records = lv_num_of_records.
  ENDMETHOD.                    "check_the_number_of_records

  METHOD get_wa_fact.
    r_wa_fact = wa_fact.
  ENDMETHOD.                    "get_mt_fact

  METHOD set_wa_fact.
    wa_fact = i_wa_fact.
  ENDMETHOD.                    "set_mt_fact
ENDCLASS.                    "lcl_cs_displayer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_java_displayer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_java_displayer IMPLEMENTATION.
  METHOD constructor.
    me->o_salv = i_o_salv.
  ENDMETHOD.                    "constructor

  METHOD lif_category~add_fact.
    DATA: lwa_zcsfacts TYPE zcsfacts,
          lv_incremented_id TYPE i.
    lv_incremented_id = lif_category~check_last_id( ) + 1.
    lwa_zcsfacts-id       = lv_incremented_id.
    lwa_zcsfacts-title    = p_tit.
    lwa_zcsfacts-category = 'JAVA'.
    lwa_zcsfacts-content  = p_con.
    INSERT zcsfacts FROM lwa_zcsfacts.
    IF sy-subrc = 0.
      MESSAGE 'The record has been added.' TYPE 'I'.
    ELSE.
      MESSAGE 'The error has occured.' TYPE 'I'.
    ENDIF.
  ENDMETHOD.                    "add_fact

  METHOD lif_category~pick_random.
    DATA: lv_random_number TYPE i,
          lt_fact TYPE zcsfacts.
    lv_random_number = lif_category~generate_random( ).
    CLEAR lt_fact.
    SELECT SINGLE *
      FROM zcsfacts
       INTO lt_fact
        WHERE id = lv_random_number.
    set_wa_fact( i_wa_fact = lt_fact ).
    lif_category~display_fact( ).
  ENDMETHOD.                    "pick_random_abap

  METHOD lif_category~pick_by_id.
    DATA: lt_fact        TYPE zcsfacts,
          lv_if_id_found TYPE boolean.
    CLEAR: lt_fact,
           lv_if_id_found.
    lv_if_id_found = lif_category~check_if_id_exists( i_id_to_check = i_id ).
    IF lv_if_id_found = abap_false.
      MESSAGE 'No record of provided ID has been found in JAVA category.' TYPE 'I'.
    ELSE.
      SELECT SINGLE *
        FROM zcsfacts
          INTO lt_fact
            WHERE id = i_id.
      set_wa_fact( i_wa_fact = lt_fact ).
      lif_category~display_fact( ).
    ENDIF.
  ENDMETHOD.                    "pick_by_id

  METHOD lif_category~check_last_id.
    DATA: lv_latest_id TYPE i.
    SELECT MAX( id )
      FROM zcsfacts
       INTO lv_latest_id.
    IF sy-subrc <> 0.
      r_latest_id = 1.
    ELSE.
      r_latest_id = lv_latest_id.
    ENDIF.
  ENDMETHOD.                    "check_last_id

  METHOD lif_category~display_fact.
    DATA: lt_fact TYPE STANDARD TABLE OF zcsfacts.
    APPEND wa_fact TO lt_fact.
    o_salv->display_alv( CHANGING c_lt_tab = lt_fact ).
  ENDMETHOD.                    "display_fact

  METHOD lif_category~generate_random.
    DATA: lv_result         TYPE i,
          lv_record_present TYPE boolean VALUE abap_false.
    WHILE lv_record_present = abap_false.
        CALL FUNCTION 'RANDOM_I4'
          EXPORTING
            RND_MIN         = 1
            RND_MAX         = lif_category~check_the_number_of_records( )
          IMPORTING
            RND_VALUE       = lv_result.
      lv_record_present = lif_category~check_category( i_randomized_id = lv_result ).
    ENDWHILE.
    r_random = lv_result.
  ENDMETHOD.                    "generate_random

  METHOD lif_category~check_category.
    DATA: lv_category(4) TYPE c.
    SELECT SINGLE category
      FROM zcsfacts
        INTO lv_category
          WHERE id = i_randomized_id
          AND category = 'JAVA'.
    IF lv_category IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_category

  METHOD lif_category~check_if_id_exists.
    DATA: lv_found_id TYPE i.
    SELECT SINGLE id
      FROM zcsfacts
        INTO lv_found_id
          WHERE id = i_id_to_check
		  AND category = 'JAVA'.
    IF lv_found_id IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_if_id_exists

  METHOD lif_category~check_the_number_of_records.
    DATA: lv_num_of_records TYPE i.
    SELECT COUNT( * )
      FROM zcsfacts
        INTO lv_num_of_records.
    r_num_of_records = lv_num_of_records.
  ENDMETHOD.                    "check_the_number_of_records

  METHOD get_wa_fact.
    r_wa_fact = wa_fact.
  ENDMETHOD.                    "get_mt_fact

  METHOD set_wa_fact.
    wa_fact = i_wa_fact.
  ENDMETHOD.                    "set_mt_fact
ENDCLASS.                    "lcl_java_displayer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_kotlin_displayer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_kotlin_displayer IMPLEMENTATION.
  METHOD constructor.
    me->o_salv = i_o_salv.
  ENDMETHOD.                    "constructor

  METHOD lif_category~add_fact.
    DATA: lwa_zcsfacts TYPE zcsfacts,
          lv_incremented_id TYPE i.
    lv_incremented_id = lif_category~check_last_id( ) + 1.
    lwa_zcsfacts-id       = lv_incremented_id.
    lwa_zcsfacts-title    = p_tit.
    lwa_zcsfacts-category = 'Kotlin'.
    lwa_zcsfacts-content  = p_con.
    INSERT zcsfacts FROM lwa_zcsfacts.
    IF sy-subrc = 0.
      MESSAGE 'The record has been added.' TYPE 'I'.
    ELSE.
      MESSAGE 'The error has occured.' TYPE 'I'.
    ENDIF.
  ENDMETHOD.                    "add_fact

  METHOD lif_category~pick_random.
    DATA: lv_random_number TYPE i,
          lt_fact TYPE zcsfacts.
    lv_random_number = lif_category~generate_random( ).
    CLEAR lt_fact.
    SELECT SINGLE *
      FROM zcsfacts
       INTO lt_fact
        WHERE id = lv_random_number.
    set_wa_fact( i_wa_fact = lt_fact ).
    lif_category~display_fact( ).
  ENDMETHOD.                    "pick_random_abap

  METHOD lif_category~pick_by_id.
    DATA: lt_fact        TYPE zcsfacts,
          lv_if_id_found TYPE boolean.
    CLEAR: lt_fact,
           lv_if_id_found.
    lv_if_id_found = lif_category~check_if_id_exists( i_id_to_check = i_id ).
    IF lv_if_id_found = abap_false.
      MESSAGE 'No record of provided ID has been found in Kotlin category.' TYPE 'I'.
    ELSE.
      SELECT SINGLE *
        FROM zcsfacts
          INTO lt_fact
            WHERE id = i_id.
      set_wa_fact( i_wa_fact = lt_fact ).
      lif_category~display_fact( ).
    ENDIF.
  ENDMETHOD.                    "pick_by_id

  METHOD lif_category~check_last_id.
    DATA: lv_latest_id TYPE i.
    SELECT MAX( id )
      FROM zcsfacts
       INTO lv_latest_id.
    IF sy-subrc <> 0.
      r_latest_id = 1.
    ELSE.
      r_latest_id = lv_latest_id.
    ENDIF.
  ENDMETHOD.                    "check_last_id

  METHOD lif_category~display_fact.
    DATA: lt_fact TYPE STANDARD TABLE OF zcsfacts.
    APPEND wa_fact TO lt_fact.
    o_salv->display_alv( CHANGING c_lt_tab = lt_fact ).
  ENDMETHOD.                    "display_fact

  METHOD lif_category~generate_random.
    DATA: lv_result         TYPE i,
          lv_record_present TYPE boolean VALUE abap_false.
    WHILE lv_record_present = abap_false.
        CALL FUNCTION 'RANDOM_I4'
          EXPORTING
            RND_MIN         = 1
            RND_MAX         = lif_category~check_the_number_of_records( )
          IMPORTING
            RND_VALUE       = lv_result.
      lv_record_present = lif_category~check_category( i_randomized_id = lv_result ).
    ENDWHILE.
    r_random = lv_result.
  ENDMETHOD.                    "generate_random

  METHOD lif_category~check_category.
    DATA: lv_category(4) TYPE c.
    SELECT SINGLE category
      FROM zcsfacts
        INTO lv_category
          WHERE id = i_randomized_id
          AND category = 'Kotlin'.
    IF lv_category IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_category

  METHOD lif_category~check_if_id_exists.
    DATA: lv_found_id TYPE i.
    SELECT SINGLE id
      FROM zcsfacts
        INTO lv_found_id
          WHERE id = i_id_to_check
		  AND category = 'Kotlin'.
    IF lv_found_id IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_if_id_exists

  METHOD lif_category~check_the_number_of_records.
    DATA: lv_num_of_records TYPE i.
    SELECT COUNT( * )
      FROM zcsfacts
        INTO lv_num_of_records.
    r_num_of_records = lv_num_of_records.
  ENDMETHOD.                    "check_the_number_of_records

  METHOD get_wa_fact.
    r_wa_fact = wa_fact.
  ENDMETHOD.                    "get_mt_fact

  METHOD set_wa_fact.
    wa_fact = i_wa_fact.
  ENDMETHOD.                    "set_mt_fact
ENDCLASS.                    "lcl_kotlin_displayer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_all_displayer IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_all_displayer IMPLEMENTATION.
  METHOD constructor.
    me->o_salv = i_o_salv.
  ENDMETHOD.                    "constructor

  METHOD lif_category~pick_random.
    DATA: lv_random_number TYPE i,
          lt_fact TYPE zcsfacts.
    lv_random_number = lif_category~generate_random( ).
    CLEAR lt_fact.
    SELECT SINGLE *
      FROM zcsfacts
       INTO lt_fact
        WHERE id = lv_random_number.
    set_wa_fact( i_wa_fact = lt_fact ).
    lif_category~display_fact( ).
  ENDMETHOD.                    "pick_random_abap

  METHOD lif_category~pick_by_id.
    DATA: lt_fact        TYPE zcsfacts,
          lv_if_id_found TYPE boolean.
    CLEAR: lt_fact,
           lv_if_id_found.
    lv_if_id_found = lif_category~check_if_id_exists( i_id_to_check = i_id ).
    IF lv_if_id_found = abap_false.
      MESSAGE 'No record of provided ID has been found across all categories.' TYPE 'I'.
    ELSE.
      SELECT SINGLE *
        FROM zcsfacts
          INTO lt_fact
            WHERE id = i_id.
      set_wa_fact( i_wa_fact = lt_fact ).
      lif_category~display_fact( ).
    ENDIF.
  ENDMETHOD.                    "pick_by_id

  METHOD lif_category~check_last_id.
    DATA: lv_latest_id TYPE i.
    SELECT MAX( id )
      FROM zcsfacts
       INTO lv_latest_id.
    IF sy-subrc <> 0.
      r_latest_id = 1.
    ELSE.
      r_latest_id = lv_latest_id.
    ENDIF.
  ENDMETHOD.                    "check_last_id

  METHOD lif_category~display_fact.
    DATA: lt_fact TYPE STANDARD TABLE OF zcsfacts.
    APPEND wa_fact TO lt_fact.
    o_salv->display_alv( CHANGING c_lt_tab = lt_fact ).
  ENDMETHOD.                    "display_fact

  METHOD lif_category~generate_random.
    DATA: lv_result         TYPE i,
          lv_record_present TYPE boolean VALUE abap_false.
    WHILE lv_record_present = abap_false.
        CALL FUNCTION 'RANDOM_I4'
          EXPORTING
            RND_MIN         = 1
            RND_MAX         = lif_category~check_the_number_of_records( )
          IMPORTING
            RND_VALUE       = lv_result.
      lv_record_present = lif_category~check_category( i_randomized_id = lv_result ).
    ENDWHILE.
    r_random = lv_result.
  ENDMETHOD.                    "generate_random

  METHOD lif_category~check_category.
    DATA: lv_category(4) TYPE c.
    SELECT SINGLE category
      FROM zcsfacts
        INTO lv_category
          WHERE id = i_randomized_id.
    IF lv_category IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_category

  METHOD lif_category~check_if_id_exists.
    DATA: lv_found_id TYPE i.
    SELECT SINGLE id
      FROM zcsfacts
        INTO lv_found_id
          WHERE id = i_id_to_check.
    IF lv_found_id IS NOT INITIAL.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.                    "check_if_id_exists

  METHOD lif_category~check_the_number_of_records.
    DATA: lv_num_of_records TYPE i.
    SELECT COUNT( * )
      FROM zcsfacts
        INTO lv_num_of_records.
    r_num_of_records = lv_num_of_records.
  ENDMETHOD.                    "check_the_number_of_records

  METHOD get_wa_fact.
    r_wa_fact = wa_fact.
  ENDMETHOD.                    "get_mt_fact

  METHOD set_wa_fact.
    wa_fact = i_wa_fact.
  ENDMETHOD.                    "set_mt_fact
ENDCLASS.                    "lcl_all_displayer IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_salv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_salv IMPLEMENTATION.
  METHOD display_alv.
    prepare_data( CHANGING c_lt_tab = c_lt_tab ).
    change_columns( ).
    alv_table->display( ).
  ENDMETHOD.                    "display_alv

  METHOD prepare_data.
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   = alv_table
          CHANGING
            t_table        = c_lt_tab ).
      CATCH cx_salv_msg .
    ENDTRY.
    alv_table->get_functions( )->set_all( abap_true ).
    alv_table->get_columns( )->set_optimize( abap_true ).
  ENDMETHOD.                    "prepare_data

  METHOD change_columns.
  ENDMETHOD.                    "change_columns

  METHOD change_column_header.
  ENDMETHOD.                    "change_column_header
ENDCLASS.                    "lcl_salv IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_factory IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory IMPLEMENTATION.
  METHOD provide_object.
    DATA(lo_salv) = NEW lcl_salv( ).
    CASE sy-ucomm.
      WHEN 'FC7'.
        DATA(lo_abap_displayer) = NEW lcl_abap_displayer( i_o_salv = lo_salv ).
        r_o_category = lo_abap_displayer.
      WHEN 'FC10'.
        DATA(lo_cs_displayer) = NEW lcl_cs_displayer( i_o_salv = lo_salv ).
        r_o_category = lo_cs_displayer.
      WHEN 'FC13'.
        DATA(lo_java_displayer) = NEW lcl_java_displayer( i_o_salv = lo_salv ).
        r_o_category = lo_java_displayer.
      WHEN 'FC19'.
        DATA(lo_kotlin_displayer) = NEW lcl_kotlin_displayer( i_o_salv = lo_salv ).
        r_o_category = lo_kotlin_displayer.
      WHEN 'FC21'.
        DATA(lo_all_displayer) = NEW lcl_all_displayer( i_o_salv = lo_salv ).
        r_o_category = lo_all_displayer.
      WHEN 'FC16'.
        CASE gv_program_mode.
          WHEN 'ABAP'.
            DATA(lo_abap_displayer2) = NEW lcl_abap_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_abap_displayer2.
          WHEN 'CS'.
            DATA(lo_cs_displayer2) = NEW lcl_cs_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_cs_displayer2.
          WHEN 'JAVA'.
            DATA(lo_java_displayer2) = NEW lcl_java_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_java_displayer2.
          WHEN 'Kotlin'.
            DATA(lo_kotlin_displayer2) = NEW lcl_kotlin_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_kotlin_displayer2.
        ENDCASE.
      WHEN 'FC17'.
        CASE gv_program_mode.
          WHEN 'ABAP'.
            DATA(lo_abap_displayer3) = NEW lcl_abap_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_abap_displayer3.
          WHEN 'CS'.
            DATA(lo_cs_displayer3) = NEW lcl_cs_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_cs_displayer3.
          WHEN 'JAVA'.
            DATA(lo_java_displayer3) = NEW lcl_java_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_java_displayer3.
          WHEN 'Kotlin'.
            DATA(lo_kotlin_displayer3) = NEW lcl_kotlin_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_kotlin_displayer3.
          WHEN 'All'.
            DATA(lo_all_displayer2) = NEW lcl_all_displayer( i_o_salv = lo_salv ).
            r_o_category = lo_all_displayer2.
        ENDCASE.
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
          gv_program_mode = 'ABAP'.
        WHEN 'FC2'.
          gv_action_to_perform = 'CS'.
          gv_program_mode = 'CS'.
        WHEN 'FC3'.
          gv_action_to_perform = 'JAVA'.
          gv_program_mode = 'JAVA'.
        WHEN 'FC4'.
          gv_action_to_perform = 'Kotlin'.
          gv_program_mode = 'Kotlin'.
        WHEN 'FC5'.
          gv_action_to_perform = 'All'.
          gv_program_mode = 'All'.
        WHEN 'FC15'.
          gv_action_to_perform = 'Return'.
        WHEN 'FC6' OR 'FC9' OR 'FC12' OR 'FC18'.
          gv_action_to_perform = 'Add'.
        WHEN 'FC16'.
          lo_category->add_fact( ).
        WHEN 'FC7' OR 'FC10' OR 'FC13' OR 'FC19' OR 'FC21'.
          lo_category->pick_random( ).
        WHEN 'FC8' OR 'FC11' OR 'FC14' OR 'FC20' OR 'FC22'.
          gv_action_to_perform = 'By_id'.
        WHEN 'FC17'.
          lo_category->pick_by_id( i_id = p_id ).
      ENDCASE.
  ENDMETHOD.                    "decide_action

  METHOD get_lo_category.
    r_lo_category = lo_category.
  ENDMETHOD.                    "get_lo_category
ENDCLASS.                    "lcl_action_handler IMPLEMENTATION
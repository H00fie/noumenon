*&---------------------------------------------------------------------*
*&  Include           NOUMENON_DEF
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_element_remover DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_element_remover DEFINITION.
  PUBLIC SECTION.
    METHODS: hide_onli.
ENDCLASS.                    "lcl_element_remover DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_visibility_dispenser DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_visibility_dispenser DEFINITION.
  PUBLIC SECTION.
    METHODS: make_all_blocks_inv,
             make_block_visible IMPORTING i_marker TYPE string.
  PRIVATE SECTION.
    METHODS: set_visibility IMPORTING i_to_hide TYPE string,
             cut_string     IMPORTING i_to_cut TYPE string
                            EXPORTING e_id_tab TYPE ANY TABLE,
             adjust_pre_return_action IMPORTING i_action TYPE string.
    DATA: pre_return_action TYPE string.
ENDCLASS.                    "lcl_visibility_dispenser DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_screen_adjuster DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_screen_adjuster DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING i_lo_element_remover      TYPE REF TO lcl_element_remover
                                   i_lo_visibility_dispenser TYPE REF TO lcl_visibility_dispenser,
             adjust_screen.
  PRIVATE SECTION.
    METHODS: decide_marker RETURNING VALUE(r_marker) TYPE string.
    DATA: lo_element_remover       TYPE REF TO lcl_element_remover,
          lo_visibility_dispenser  TYPE REF TO lcl_visibility_dispenser.
ENDCLASS.                    "lcl_screen_adjuster DEFINITION

*----------------------------------------------------------------------*
*       CLASS lif_category
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_category.
    METHODS: add_fact,
             pick_random,
             generate_random RETURNING VALUE(r_random) TYPE i,
             pick_by_id IMPORTING i_id TYPE i,
             check_last_id RETURNING VALUE(r_latest_id) TYPE i,
             display_fact,
             check_category IMPORTING i_randomized_id TYPE i
                            RETURNING VALUE(r_result) TYPE boolean,
             check_if_id_exists IMPORTING i_id_to_check TYPE i
                            RETURNING VALUE(r_result) TYPE boolean.
ENDINTERFACE.                    "lif_category

*----------------------------------------------------------------------*
*       CLASS lcl_salv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_salv DEFINITION.
  PUBLIC SECTION.
    METHODS: display_alv CHANGING c_lt_tab TYPE ANY TABLE.
  PRIVATE SECTION.
    METHODS: prepare_data CHANGING c_lt_tab TYPE ANY TABLE,
             change_columns,
             change_column_header IMPORTING i_columnname  TYPE c
                                            i_long_text   TYPE c
                                            i_medium_text TYPE c
                                            i_short_text  TYPE c.
    DATA: alv_table   TYPE REF TO cl_salv_table,
          alv_columns TYPE REF TO cl_salv_columns_table,
          alv_column  TYPE REF TO cl_salv_column.
ENDCLASS.                    "lcl_salv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_abap_displayer DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_displayer DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING i_o_salv TYPE REF TO lcl_salv.
    INTERFACES: lif_category.
  PRIVATE SECTION.
    METHODS: get_wa_fact RETURNING VALUE(r_wa_fact) TYPE zcsfacts,
             set_wa_fact IMPORTING i_wa_fact TYPE zcsfacts.
    DATA: wa_fact TYPE zcsfacts,
          o_salv  TYPE REF TO lcl_salv.
ENDCLASS.                    "lcl_abap_displayer DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_cs_displayer DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_cs_displayer DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING i_o_salv TYPE REF TO lcl_salv.
    INTERFACES: lif_category.
  PRIVATE SECTION.
    METHODS: get_wa_fact RETURNING VALUE(r_wa_fact) TYPE zcsfacts,
             set_wa_fact IMPORTING i_wa_fact TYPE zcsfacts.
    DATA: wa_fact TYPE zcsfacts,
          o_salv  TYPE REF TO lcl_salv.
ENDCLASS.                    "lcl_cs_displayer DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_java_displayer DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_java_displayer DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING i_o_salv TYPE REF TO lcl_salv.
    INTERFACES: lif_category.
  PRIVATE SECTION.
    METHODS: get_wa_fact RETURNING VALUE(r_wa_fact) TYPE zbmierzwitest,
             set_wa_fact IMPORTING i_wa_fact TYPE zbmierzwitest.
    DATA: wa_fact TYPE zbmierzwitest,
          o_salv  TYPE REF TO lcl_salv.
ENDCLASS.                    "lcl_java_displayer DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_factory DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_factory DEFINITION.
  PUBLIC SECTION.
    METHODS: provide_object RETURNING VALUE(r_o_category) TYPE REF TO lif_category.
ENDCLASS.                    "lcl_factory DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_action_handler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_action_handler DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor IMPORTING i_o_category TYPE REF TO lif_category,
             decide_action,
             get_lo_category RETURNING VALUE(r_lo_category) TYPE REF TO lif_category.
  PRIVATE SECTION.
    DATA: lo_category TYPE REF TO lif_category.
ENDCLASS.                     "lcl_action_handler DEFINITION
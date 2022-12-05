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
             make_block_visible IMPORTING i_marker  TYPE string.
  PRIVATE SECTION.
    METHODS: set_visibility IMPORTING i_to_hide TYPE string,
             cut_string     IMPORTING i_to_cut TYPE string
                            EXPORTING e_id_tab TYPE ANY TABLE.
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
          lo_visibility_dispenser TYPE REF TO lcl_visibility_dispenser.
ENDCLASS.                    "lcl_screen_adjuster DEFINITION

*----------------------------------------------------------------------*
*       CLASS lif_category
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
INTERFACE lif_category.
    METHODS: add_fact,
             pick_random.
ENDINTERFACE.                    "lif_category

*----------------------------------------------------------------------*
*       CLASS lcl_abap_displayer DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_abap_displayer DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_category.
  PRIVATE SECTION.
    METHODS: generate_random RETURNING VALUE(r_random) TYPE i,
             check_last_id RETURNING VALUE(r_latest_id) TYPE i,
             get_mt_fact RETURNING VALUE(r_mt_fact) TYPE zcsfacts,
             set_mt_fact IMPORTING i_mt_fact TYPE zcsfacts.
    DATA: mt_fact TYPE zcsfacts.
ENDCLASS.                    "lcl_abap_displayer DEFINITION

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
*&---------------------------------------------------------------------*
*& Report NOUMENON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT NOUMENON.

INCLUDE NOUMENON_SEL.
INCLUDE NOUMENON_DEF.
INCLUDE NOUMENON_IMP.

INITIALIZATION.
  DATA(lo_visibility_dispenser) = NEW lcl_visibility_dispenser( ).
  DATA(lo_element_remover) = NEW lcl_element_remover( ).
  DATA(lo_factory) = NEW lcl_factory( ).
  lo_visibility_dispenser->make_all_blocks_inv( ).

AT SELECTION-SCREEN OUTPUT.
  DATA(lo_screen_adjuster) = NEW lcl_screen_adjuster( i_lo_element_remover = lo_element_remover
                                                      i_lo_visibility_dispenser = lo_visibility_dispenser ).
  lo_screen_adjuster->adjust_screen( ).

AT SELECTION-SCREEN.
  DATA(lo_action_handler) = NEW lcl_action_handler( i_o_category = lo_factory->provide_object( ) ).
  lo_action_handler->decide_action( ).
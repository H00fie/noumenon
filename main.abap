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
  lo_visibility_dispenser->make_all_blocks_inv( ).

AT SELECTION-SCREEN OUTPUT.
  DATA(lo_element_remover) = NEW lcl_e
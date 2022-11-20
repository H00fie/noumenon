*&---------------------------------------------------------------------*
*& Report NOUMENON
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT NOUMENON.

INCLUDE NOUMENON_SEL.
INCLUDE NOUMENON_DEF.
INCLUDE NOUMENON_IMP.

AT SELECTION-SCREEN OUTPUT.
  DATA(lo_element_remover) = NEW lcl_element_remover( ).
  lo_element_remover->hide_onli( ).
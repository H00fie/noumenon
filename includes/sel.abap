*&---------------------------------------------------------------------*
*&  Include           NOUMENON_SEL
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE TEXT-000.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-001 USER-COMMAND fc1 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-002 USER-COMMAND fc2 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-003 USER-COMMAND fc3 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-004 USER-COMMAND fc4 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk1.

*TEXT ELEMENTS TO BE INCLUDED IN "TEXTS".
*-----------Text Symbols Sheet-----------
*000 - Category
*001 - ABAP
*002 - Computer Science
*003 - JAVA
*004 - Kotlin
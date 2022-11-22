*&---------------------------------------------------------------------*
*&  Include           NOUMENON_SEL
*&---------------------------------------------------------------------*

*-------------------------Category-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk1 WITH FRAME TITLE TEXT-000.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-001 USER-COMMAND fc1 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-002 USER-COMMAND fc2 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-003 USER-COMMAND fc3 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-004 USER-COMMAND fc4 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-005 USER-COMMAND fc5 MODIF ID id1.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk1.

*-------------------------ABAP-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk2 WITH FRAME TITLE TEXT-006.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-007 USER-COMMAND fc6 MODIF ID id2.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-008 USER-COMMAND fc7 MODIF ID id2.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-009 USER-COMMAND fc8 MODIF ID id2.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk2.

*-------------------------Computer science-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk3 WITH FRAME TITLE TEXT-010.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-007 USER-COMMAND fc9 MODIF ID id3.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-008 USER-COMMAND fc10 MODIF ID id3.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-009 USER-COMMAND fc11 MODIF ID id3.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk3.

*-------------------------JAVA-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk4 WITH FRAME TITLE TEXT-011.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-007 USER-COMMAND fc12 MODIF ID id4.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-008 USER-COMMAND fc13 MODIF ID id4.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-009 USER-COMMAND fc14 MODIF ID id4.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk4.

*-------------------------Kotlin-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk5 WITH FRAME TITLE TEXT-012.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-007 USER-COMMAND fc12 MODIF ID id5.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-008 USER-COMMAND fc13 MODIF ID id5.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-009 USER-COMMAND fc14 MODIF ID id5.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk5.

*-------------------------All-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk6 WITH FRAME TITLE TEXT-013.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-008 USER-COMMAND fc13 MODIF ID id6.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-009 USER-COMMAND fc14 MODIF ID id6.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk6.

SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-014 USER-COMMAND fc15 MODIF ID id7.

*TEXT ELEMENTS TO BE INCLUDED IN "TEXTS".
*-----------Text Symbols Sheet-----------
*000 - Category
*001 - ABAP
*002 - Computer Science
*003 - JAVA
*004 - Kotlin
*005 - All
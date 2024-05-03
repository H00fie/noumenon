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
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-007 USER-COMMAND fc18 MODIF ID id5.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-008 USER-COMMAND fc19 MODIF ID id5.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN PUSHBUTTON 1(25) TEXT-009 USER-COMMAND fc20 MODIF ID id5.
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

*-------------------------Add record-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk7 WITH FRAME TITLE TEXT-015.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) TEXT-016 MODIF ID id8.
    PARAMETERS: p_tit TYPE string MODIF ID id8.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) TEXT-017 MODIF ID id8.
    PARAMETERS: p_con TYPE string MODIF ID id8.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk7.

*-------------------------Display a record by ID-------------------------*
SELECTION-SCREEN BEGIN OF BLOCK bk8 WITH FRAME TITLE TEXT-019.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) TEXT-020 MODIF ID id9.
    PARAMETERS: p_id TYPE i MODIF ID id9.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bk8.

*-------------------------Bottom screen buttons-------------------------*
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-014 USER-COMMAND fc15 MODIF ID id7.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-018 USER-COMMAND fc16 MODIF ID id8.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN PUSHBUTTON 1(20) TEXT-021 USER-COMMAND fc17 MODIF ID id9.
SELECTION-SCREEN END OF LINE.

*TEXT ELEMENTS TO BE INCLUDED IN "TEXTS".
*-----------Text Symbols Sheet-----------
*000 - Category
*001 - ABAP
*002 - Computer Science
*003 - JAVA
*004 - Kotlin
*005 - All
*006 - ABAP section
*007 - Add a fact
*008 - Display a random fact
*009 - Display a fact by ID
*010 - CS section
*011 - JAVA section
*012 - Kotlin section
*013 - Joined section
*014 - Return
*015 - ABAP - add a new record
*016 - Title
*017 - Content
*018 - Add
*019 - Select by id
*020 - Record's ID
*021 - Display record
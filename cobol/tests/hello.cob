      * Simple COBOL test program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       AUTHOR. Claude.
       DATE-WRITTEN. 2025-01-26.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 GREETING PIC X(30) VALUE "Hello from COBOL!".
       01 COUNTER  PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY GREETING.
           MOVE 42 TO COUNTER.
           DISPLAY "Counter: " COUNTER.
           STOP RUN.

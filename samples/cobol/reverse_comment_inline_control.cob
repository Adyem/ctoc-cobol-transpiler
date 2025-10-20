       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE-COMMENT-INLINE-CONTROL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 CONTROL-FLAG PIC X VALUE 'N'.
       PROCEDURE DIVISION.
MAIN.
*> comment before top-level if
       IF CONTROL-FLAG = 'N'
*> comment before then display
           DISPLAY "THEN BRANCH".
       ELSE
*> comment before else display
           DISPLAY "ELSE BRANCH".
       END-IF.
*> comment before stop run
       STOP RUN.
       END PROGRAM REVERSE-COMMENT-INLINE-CONTROL.

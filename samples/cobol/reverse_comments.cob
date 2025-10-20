       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE-COMMENTS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
*> leading comment for flag
       01 STATUS-FLAG PIC X VALUE 'Y'.
*> additional buffer comment
       01 BUFFER-TEXT PIC X(16).
       PROCEDURE DIVISION.
*> paragraph level note
MAIN.
       MOVE 'N' TO STATUS-FLAG. *> inline comment after move
*> comment before display
       DISPLAY STATUS-FLAG.
       STOP RUN.
       END PROGRAM REVERSE-COMMENTS.

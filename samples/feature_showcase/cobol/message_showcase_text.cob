       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHOWCASE-TEXT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TITLE-LENGTH PIC S9(9).
       01 LITERAL-LENGTH PIC S9(9).
       01 NOTE-LENGTH PIC S9(9).
       01 TITLE.
          05 TITLE-LEN PIC 9(4) COMP VALUE 0.
          05 TITLE-CAP PIC 9(4) COMP VALUE 0.
          05 TITLE-PTR USAGE POINTER VALUE NULL.
       01 TITLE-BUF BASED PIC X(19).
       01 NOTE.
          05 NOTE-LEN PIC 9(4) COMP VALUE 0.
          05 NOTE-CAP PIC 9(4) COMP VALUE 0.
          05 NOTE-PTR USAGE POINTER VALUE NULL.
       01 NOTE-BUF BASED PIC X(16).
       PROCEDURE DIVISION.
       SHOWCASE-TEXT.
           MOVE 0 TO TITLE-LEN.
           IF TITLE-CAP < 19
               IF TITLE-PTR NOT = NULL
                   FREE TITLE-PTR
               END-IF
               ALLOCATE 19 CHARACTERS RETURNING TITLE-PTR
               SET ADDRESS OF TITLE-BUF TO TITLE-PTR
               COMPUTE TITLE-CAP = 19
           END-IF.
           MOVE "RENEWAL RISK REVIEW" TO TITLE-BUF.
           COMPUTE TITLE-LEN = 19.
           MOVE 0 TO NOTE-LEN.
           IF NOTE-CAP < 16
               IF NOTE-PTR NOT = NULL
                   FREE NOTE-PTR
               END-IF
               ALLOCATE 16 CHARACTERS RETURNING NOTE-PTR
               SET ADDRESS OF NOTE-BUF TO NOTE-PTR
               COMPUTE NOTE-CAP = 16
           END-IF.
           MOVE SPACES TO NOTE-BUF.
           IF NOTE-CAP < 9
               IF NOTE-PTR NOT = NULL
                   FREE NOTE-PTR
               END-IF
               ALLOCATE 9 CHARACTERS RETURNING NOTE-PTR
               SET ADDRESS OF NOTE-BUF TO NOTE-PTR
               COMPUTE NOTE-CAP = 9
           END-IF.
           SET ADDRESS OF NOTE-BUF TO NOTE-PTR
           MOVE "FAST PATH" TO NOTE-BUF.
           MOVE 9 TO NOTE-LEN.
           IF TITLE-CAP < 1
               IF TITLE-PTR NOT = NULL
                   FREE TITLE-PTR
               END-IF
               ALLOCATE 1 CHARACTERS RETURNING TITLE-PTR
               SET ADDRESS OF TITLE-BUF TO TITLE-PTR
               COMPUTE TITLE-CAP = 1
           END-IF.
           SET ADDRESS OF TITLE-BUF TO TITLE-PTR
           DISPLAY TITLE-BUF(1:TITLE-LEN).
           COMPUTE TITLE-LENGTH = TITLE-LEN.
           DISPLAY "Title chars".
           DISPLAY TITLE-LENGTH.
           COMPUTE LITERAL-LENGTH = 13.
           DISPLAY "Literal chars".
           DISPLAY LITERAL-LENGTH.
           IF NOTE-CAP < 1
               IF NOTE-PTR NOT = NULL
                   FREE NOTE-PTR
               END-IF
               ALLOCATE 1 CHARACTERS RETURNING NOTE-PTR
               SET ADDRESS OF NOTE-BUF TO NOTE-PTR
               COMPUTE NOTE-CAP = 1
           END-IF.
           SET ADDRESS OF NOTE-BUF TO NOTE-PTR
           DISPLAY NOTE-BUF(1:NOTE-LEN).
           COMPUTE NOTE-LENGTH = NOTE-LEN.
           DISPLAY "Note chars".
           DISPLAY NOTE-LENGTH.
           MOVE 0 TO NOTE-LEN.
           IF NOTE-PTR NOT = NULL
               FREE NOTE-PTR
           END-IF.
           SET NOTE-PTR TO NULL.
           MOVE 0 TO NOTE-CAP.
           MOVE 0 TO TITLE-LEN.
           IF TITLE-PTR NOT = NULL
               FREE TITLE-PTR
           END-IF.
           SET TITLE-PTR TO NULL.
           MOVE 0 TO TITLE-CAP.
           GOBACK.

       END PROGRAM SHOWCASE-TEXT.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. SHOWCASE-TEXT.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TITLE-LENGTH PIC S9(9).
       01 NOTE-LENGTH PIC S9(9).
       01 TITLE.
          05 TITLE-LEN PIC 9(4) COMP VALUE 0.
          05 TITLE-CAP PIC 9(4) COMP VALUE 0.
          05 TITLE-PTR USAGE POINTER VALUE NULL.
       01 TITLE-BUF BASED PIC X(15).
       01 NOTE.
          05 NOTE-LEN PIC 9(4) COMP VALUE 0.
          05 NOTE-CAP PIC 9(4) COMP VALUE 0.
          05 NOTE-PTR USAGE POINTER VALUE NULL.
       01 NOTE-BUF BASED PIC X(16).
       PROCEDURE DIVISION.
       SHOWCASE-TEXT.
           MOVE 0 TO TITLE-LEN.
           IF TITLE-CAP < 15
               IF TITLE-PTR NOT = NULL
                   FREE TITLE-PTR
               END-IF
               ALLOCATE 15 CHARACTERS RETURNING TITLE-PTR
               SET ADDRESS OF TITLE-BUF TO TITLE-PTR
               COMPUTE TITLE-CAP = 15
           END-IF.
           MOVE "POLICY SNAPSHOT" TO TITLE-BUF.
           COMPUTE TITLE-LEN = 15.
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
           IF NOTE-CAP < 10
               IF NOTE-PTR NOT = NULL
                   FREE NOTE-PTR
               END-IF
               ALLOCATE 10 CHARACTERS RETURNING NOTE-PTR
               SET ADDRESS OF NOTE-BUF TO NOTE-PTR
               COMPUTE NOTE-CAP = 10
           END-IF.
           SET ADDRESS OF NOTE-BUF TO NOTE-PTR
           MOVE "clean base" TO NOTE-BUF.
           MOVE 10 TO NOTE-LEN.
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


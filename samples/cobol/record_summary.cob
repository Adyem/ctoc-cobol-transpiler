       IDENTIFICATION DIVISION.
       PROGRAM-ID. RECORD-SUMMARY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "records.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  RECORD-STATUS PIC X.
           05  RECORD-AMOUNT PIC 9(6).
       WORKING-STORAGE SECTION.
       01  EOF-FLAG PIC X VALUE 'N'.
       01  TOTAL-AMOUNT PIC 9(7) VALUE 0.
       01  ACCEPTED-COUNT PIC 9(4) VALUE 0.
       PROCEDURE DIVISION.
MAIN.
           OPEN INPUT INPUT-FILE.
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF RECORD-STATUS = "A"
                           ADD 1 TO ACCEPTED-COUNT
                           ADD RECORD-AMOUNT TO TOTAL-AMOUNT
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.
           STOP RUN.

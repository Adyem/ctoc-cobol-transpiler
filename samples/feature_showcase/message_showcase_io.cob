       IDENTIFICATION DIVISION.
       PROGRAM-ID. MESSAGE-SHOWCASE-IO.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO "showcase_transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SUCCESS-FILE ASSIGN TO "showcase_success.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT FAILURE-FILE ASSIGN TO "showcase_failure.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05 TRANSACTION-STATUS         PIC X.
           05 FILLER                     PIC X.
           05 TRANSACTION-AMOUNT         PIC 9(5).

       FD  SUCCESS-FILE.
       01  SUCCESS-RECORD               PIC X(32).

       FD  FAILURE-FILE.
       01  FAILURE-RECORD               PIC X(32).

       WORKING-STORAGE SECTION.
       01  SUCCESS-MARKER               PIC X(32)
               VALUE "ACCEPTED ENTRY".
       01  FAILURE-MARKER               PIC X(32)
               VALUE "REJECTED ENTRY".
       01  SUCCESS-COUNT                PIC 9(4) VALUE 0000.
       01  FAILURE-COUNT                PIC 9(4) VALUE 0000.
       01  TOTAL-AMOUNT                 PIC 9(9) VALUE 000000000.
       01  DISPLAY-BUFFER               PIC X(32).
       01  END-OF-FILE                  PIC X VALUE "N".
       01  NUMERIC-DISPLAY              PIC Z(9).

       PROCEDURE DIVISION.
MAIN.
       DISPLAY "IO SHOWCASE".
       OPEN INPUT TRANSACTION-FILE
            OUTPUT SUCCESS-FILE FAILURE-FILE.
       PERFORM UNTIL END-OF-FILE = "Y"
           READ TRANSACTION-FILE
               AT END
                   MOVE "Y" TO END-OF-FILE
               NOT AT END
                   IF TRANSACTION-STATUS = "A"
                       ADD 1 TO SUCCESS-COUNT
                       ADD TRANSACTION-AMOUNT TO TOTAL-AMOUNT
                       MOVE SUCCESS-MARKER TO SUCCESS-RECORD
                       WRITE SUCCESS-RECORD
                   ELSE
                       ADD 1 TO FAILURE-COUNT
                       MOVE FAILURE-MARKER TO FAILURE-RECORD
                       WRITE FAILURE-RECORD
                   END-IF
           END-READ
       END-PERFORM.
       CLOSE TRANSACTION-FILE
             SUCCESS-FILE
             FAILURE-FILE.
       MOVE SUCCESS-COUNT TO NUMERIC-DISPLAY.
       DISPLAY "SUCCESS COUNT".
       DISPLAY NUMERIC-DISPLAY.
       MOVE FAILURE-COUNT TO NUMERIC-DISPLAY.
       DISPLAY "FAILURE COUNT".
       DISPLAY NUMERIC-DISPLAY.
       MOVE TOTAL-AMOUNT TO NUMERIC-DISPLAY.
       DISPLAY "TOTAL AMOUNT".
       DISPLAY NUMERIC-DISPLAY.
       STOP RUN.
       END PROGRAM MESSAGE-SHOWCASE-IO.

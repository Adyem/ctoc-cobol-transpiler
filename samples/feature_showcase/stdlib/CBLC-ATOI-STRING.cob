       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLC-ATOI-STRING.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 IDX PIC 9(9) VALUE 000000000.
       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.
       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.
       01 START-INDEX PIC 9(9) VALUE 000000001.
       01 END-INDEX PIC 9(9) VALUE 000000000.
       01 REMAINING-INDEX PIC 9(9) VALUE 000000000.
       01 DIGIT-COUNT PIC 9(9) VALUE 000000000.
       01 NEGATIVE-FLAG PIC 9 VALUE 0.
       01 CURRENT-CHAR PIC X VALUE SPACE.
       01 DIGIT-VALUE PIC 9 VALUE 0.
       01 OVERFLOW-FLAG PIC 9 VALUE 0.
       01 ACCUMULATOR PIC S9(36) COMP-3 VALUE 0.
       01 MAX-VALUE PIC S9(36) COMP-3 VALUE 999999999.
       01 MIN-VALUE PIC S9(36) COMP-3 VALUE -999999999.
       LINKAGE SECTION.
       01 LNK-SOURCE.
          05 LNK-SOURCE-LEN PIC 9(4) COMP.
          05 LNK-SOURCE-BUF PIC X(255).
       01 LNK-RESULT PIC S9(9).
       01 LNK-STATUS PIC 9.
       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE
           BY REFERENCE LNK-RESULT BY REFERENCE LNK-STATUS.
       MAIN.
           MOVE 0 TO LNK-STATUS.
           MOVE 0 TO LNK-RESULT.
           MOVE LNK-SOURCE-LEN TO SCAN-LIMIT.
           IF SCAN-LIMIT > 255
               MOVE 255 TO SCAN-LIMIT
           END-IF.
           MOVE 0 TO ACTUAL-LENGTH.
           MOVE 0 TO IDX.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT
               IF LNK-SOURCE-BUF(IDX:1) = LOW-VALUE
                   EXIT PERFORM
               END-IF
               MOVE IDX TO ACTUAL-LENGTH
           END-PERFORM.
           IF ACTUAL-LENGTH = 0
               MOVE SCAN-LIMIT TO ACTUAL-LENGTH
           END-IF.
           MOVE 1 TO START-INDEX.
           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH
               IF LNK-SOURCE-BUF(START-INDEX:1) NOT = SPACE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           IF START-INDEX > ACTUAL-LENGTH
               MOVE 1 TO LNK-STATUS
               GOBACK
           END-IF.
           MOVE LNK-SOURCE-BUF(START-INDEX:1) TO CURRENT-CHAR.
           MOVE 0 TO NEGATIVE-FLAG.
           IF CURRENT-CHAR = "-"
               MOVE 1 TO NEGATIVE-FLAG
               ADD 1 TO START-INDEX
           ELSE
               IF CURRENT-CHAR = "+"
                   ADD 1 TO START-INDEX
               END-IF
           END-IF.
           IF START-INDEX > ACTUAL-LENGTH
               MOVE 1 TO LNK-STATUS
               GOBACK
           END-IF.
           MOVE 0 TO DIGIT-COUNT.
           MOVE 0 TO OVERFLOW-FLAG.
           MOVE 0 TO ACCUMULATOR.
           MOVE 0 TO END-INDEX.
           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH
               MOVE LNK-SOURCE-BUF(IDX:1) TO CURRENT-CHAR
               IF CURRENT-CHAR = SPACE
                   COMPUTE END-INDEX = IDX - 1
                   EXIT PERFORM
               END-IF
               IF CURRENT-CHAR = LOW-VALUE
                   COMPUTE END-INDEX = IDX - 1
                   EXIT PERFORM
               END-IF
               IF CURRENT-CHAR < "0" OR CURRENT-CHAR > "9"
                   MOVE 1 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
               END-IF
               ADD 1 TO DIGIT-COUNT
               MOVE LNK-SOURCE-BUF(IDX:1) TO DIGIT-VALUE
               COMPUTE ACCUMULATOR = ACCUMULATOR * 10
                   ON SIZE ERROR
                       MOVE 1 TO OVERFLOW-FLAG
               END-COMPUTE
               IF OVERFLOW-FLAG = 1
                   MOVE 1 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
               END-IF
               COMPUTE ACCUMULATOR = ACCUMULATOR + DIGIT-VALUE
                   ON SIZE ERROR
                       MOVE 1 TO OVERFLOW-FLAG
               END-COMPUTE
               IF OVERFLOW-FLAG = 1
                   MOVE 1 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
               END-IF
               MOVE IDX TO END-INDEX
           END-PERFORM.
           IF DIGIT-COUNT = 0
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           IF END-INDEX = 0
               MOVE ACTUAL-LENGTH TO END-INDEX
           END-IF.
           COMPUTE REMAINING-INDEX = END-INDEX + 1.
           IF REMAINING-INDEX < START-INDEX
               MOVE START-INDEX TO REMAINING-INDEX
           END-IF.
           PERFORM VARYING IDX FROM REMAINING-INDEX BY 1 UNTIL IDX > ACTUAL-LENGTH
               IF LNK-SOURCE-BUF(IDX:1) = LOW-VALUE
                   EXIT PERFORM
               END-IF
               IF LNK-SOURCE-BUF(IDX:1) NOT = SPACE
                   MOVE 1 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
               END-IF
           END-PERFORM.
           IF NEGATIVE-FLAG = 1
               COMPUTE ACCUMULATOR = 0 - ACCUMULATOR
                   ON SIZE ERROR
                       MOVE 1 TO OVERFLOW-FLAG
               END-COMPUTE
               IF OVERFLOW-FLAG = 1
                   MOVE 1 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
               END-IF
           END-IF.
           IF ACCUMULATOR > MAX-VALUE
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           IF ACCUMULATOR < MIN-VALUE
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           MOVE ACCUMULATOR TO LNK-RESULT.
           GOBACK.
       END PROGRAM CBLC-ATOI-STRING.

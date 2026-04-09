       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLC-DATE-YYYYMMDD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 IDX PIC 9 VALUE 1.
       01 WS-CHAR PIC X.
       01 WS-DATE-DISPLAY PIC 9(8).
       01 WS-REMAINDER PIC 9(8).
       01 WS-YEAR PIC 9(4).
       01 WS-MONTH PIC 9(2).
       01 WS-DAY PIC 9(2).
       01 WS-MAX-DAY PIC 9(2).
       LINKAGE SECTION.
       01 LNK-INPUT PIC X(8).
       01 LNK-YEAR PIC 9(4).
       01 LNK-MONTH PIC 9(2).
       01 LNK-DAY PIC 9(2).
       01 LNK-PACKED PIC 9(8) COMP-3.
       01 LNK-SERIAL PIC S9(9) COMP-5.
       01 LNK-STATUS PIC 9.
       PROCEDURE DIVISION USING BY REFERENCE LNK-INPUT
           BY REFERENCE LNK-YEAR BY REFERENCE LNK-MONTH
           BY REFERENCE LNK-DAY BY REFERENCE LNK-PACKED
           BY REFERENCE LNK-SERIAL BY REFERENCE LNK-STATUS.
       MAIN.
           MOVE 0 TO LNK-STATUS.
           MOVE 0 TO LNK-YEAR.
           MOVE 0 TO LNK-MONTH.
           MOVE 0 TO LNK-DAY.
           MOVE 0 TO LNK-PACKED.
           MOVE 0 TO LNK-SERIAL.
           MOVE 1 TO IDX.
           PERFORM UNTIL IDX > 8
               MOVE LNK-INPUT(IDX:1) TO WS-CHAR
               IF WS-CHAR < "0" OR WS-CHAR > "9"
                   MOVE 1 TO LNK-STATUS
                   GO TO VALIDATION-EXIT
               END-IF
               ADD 1 TO IDX
           END-PERFORM.
           MOVE LNK-INPUT TO WS-DATE-DISPLAY.
           DIVIDE WS-DATE-DISPLAY BY 10000 GIVING WS-YEAR
               REMAINDER WS-REMAINDER.
           DIVIDE WS-REMAINDER BY 100 GIVING WS-MONTH
               REMAINDER WS-DAY.
           IF WS-MONTH < 1 OR WS-MONTH > 12
               MOVE 2 TO LNK-STATUS
               GO TO VALIDATION-EXIT
           END-IF
           MOVE 31 TO WS-MAX-DAY.
           IF WS-MONTH = 4 OR WS-MONTH = 6 OR WS-MONTH = 9
               OR WS-MONTH = 11
               MOVE 30 TO WS-MAX-DAY
           END-IF
           IF WS-MONTH = 2
               MOVE 28 TO WS-MAX-DAY
               IF FUNCTION MOD(WS-YEAR, 4) = 0
                   MOVE 29 TO WS-MAX-DAY
                   IF FUNCTION MOD(WS-YEAR, 100) = 0
                       IF FUNCTION MOD(WS-YEAR, 400) NOT = 0
                           MOVE 28 TO WS-MAX-DAY
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF WS-DAY < 1 OR WS-DAY > WS-MAX-DAY
               MOVE 3 TO LNK-STATUS
               GO TO VALIDATION-EXIT
           END-IF
           MOVE WS-YEAR TO LNK-YEAR.
           MOVE WS-MONTH TO LNK-MONTH.
           MOVE WS-DAY TO LNK-DAY.
           MOVE WS-DATE-DISPLAY TO LNK-PACKED.
           COMPUTE LNK-SERIAL = FUNCTION INTEGER-OF-DATE(WS-DATE-DISPLAY).
       VALIDATION-EXIT.
           IF LNK-STATUS NOT = 0
               MOVE 0 TO LNK-YEAR
               MOVE 0 TO LNK-MONTH
               MOVE 0 TO LNK-DAY
               MOVE 0 TO LNK-PACKED
               MOVE 0 TO LNK-SERIAL
           END-IF.
           GOBACK.
       END PROGRAM CBLC-DATE-YYYYMMDD.

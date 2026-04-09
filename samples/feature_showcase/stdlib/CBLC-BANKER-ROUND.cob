       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLC-BANKER-ROUND.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SCALE-POWER PIC 9(18) COMP-3 VALUE 1.
       01 WS-SCALED PIC S9(18)V9(18) COMP-3 VALUE 0.
       01 WS-INTEGER PIC S9(18) COMP-3 VALUE 0.
       01 WS-FRACTION PIC 9V9(18) COMP-3 VALUE 0.
       01 WS-ABS-INTEGER PIC 9(18) COMP-3 VALUE 0.
       01 WS-REMAINDER PIC 9 COMP-3 VALUE 0.
       01 WS-HALF PIC 9V9 COMP-3 VALUE 0.5.
       01 WS-TWO PIC 9 COMP-3 VALUE 2.
       LINKAGE SECTION.
       01 LNK-OPERAND USAGE COMP-2.
       01 LNK-SCALE PIC S9(4) COMP-5.
       01 LNK-RESULT USAGE COMP-2.
       01 LNK-STATUS PIC 9.
       PROCEDURE DIVISION USING BY REFERENCE LNK-OPERAND
           BY REFERENCE LNK-SCALE BY REFERENCE LNK-RESULT
           BY REFERENCE LNK-STATUS.
       MAIN.
           MOVE 0 TO LNK-STATUS.
           IF LNK-SCALE < 0 OR LNK-SCALE > 18
               MOVE 2 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           COMPUTE WS-SCALE-POWER = 10 ** LNK-SCALE
               ON SIZE ERROR
                   MOVE 2 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
           END-COMPUTE.
           COMPUTE WS-SCALED = LNK-OPERAND * WS-SCALE-POWER
               ON SIZE ERROR
                   MOVE 2 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
           END-COMPUTE.
           COMPUTE WS-INTEGER = FUNCTION INTEGER-PART(WS-SCALED).
           COMPUTE WS-FRACTION = FUNCTION ABS(WS-SCALED - WS-INTEGER).
           MOVE WS-INTEGER TO WS-SCALED.
           IF WS-FRACTION > 0
               MOVE 1 TO LNK-STATUS
           END-IF
           IF WS-FRACTION > WS-HALF
               IF WS-SCALED >= 0
                   COMPUTE WS-SCALED = WS-INTEGER + 1
               ELSE
                   COMPUTE WS-SCALED = WS-INTEGER - 1
               END-IF
           ELSE
               IF WS-FRACTION = WS-HALF
                   COMPUTE WS-ABS-INTEGER = FUNCTION ABS(WS-INTEGER)
                   COMPUTE WS-REMAINDER = FUNCTION MOD(WS-ABS-INTEGER, WS-TWO)
                   IF WS-REMAINDER NOT = 0
                       IF WS-SCALED >= 0
                           COMPUTE WS-SCALED = WS-INTEGER + 1
                       ELSE
                           COMPUTE WS-SCALED = WS-INTEGER - 1
                       END-IF
                   END-IF
               END-IF
           END-IF.
           COMPUTE LNK-RESULT = WS-SCALED / WS-SCALE-POWER
               ON SIZE ERROR
                   MOVE 2 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
           END-COMPUTE.
           GOBACK.
       END PROGRAM CBLC-BANKER-ROUND.

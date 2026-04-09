       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLC-STRTOD.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 IDX PIC 9(9) VALUE 000000000.
       01 SCAN-LIMIT PIC 9(9) VALUE 000000000.
       01 ACTUAL-LENGTH PIC 9(9) VALUE 000000000.
       01 START-INDEX PIC 9(9) VALUE 000000001.
       01 END-INDEX PIC 9(9) VALUE 000000000.
       01 NORMALIZED-LENGTH PIC 9(9) VALUE 000000000.
       01 NORMALIZED-BUFFER PIC X(255) VALUE SPACES.
       01 CURRENT-CHAR PIC X VALUE SPACE.
       01 HAS-ANY-DIGIT PIC 9 VALUE 0.
       01 HAS-DECIMAL PIC 9 VALUE 0.
       01 HAS-EXPONENT PIC 9 VALUE 0.
       01 EXPONENT-DIGITS PIC 9(9) VALUE 000000000.
       01 EXPECT-EXPONENT-SIGN PIC 9 VALUE 0.
       LINKAGE SECTION.
       01 LNK-SOURCE PIC X(255).
       01 LNK-SOURCE-LENGTH PIC S9(9) COMP-5.
       01 LNK-RESULT USAGE COMP-2.
       01 LNK-STATUS PIC 9.
       PROCEDURE DIVISION USING BY REFERENCE LNK-SOURCE
           BY VALUE LNK-SOURCE-LENGTH BY REFERENCE LNK-RESULT
           BY REFERENCE LNK-STATUS.
       MAIN.
           MOVE 0 TO LNK-STATUS.
           MOVE 0 TO LNK-RESULT.
           MOVE LNK-SOURCE-LENGTH TO SCAN-LIMIT.
           IF SCAN-LIMIT > 255
               MOVE 255 TO SCAN-LIMIT
           END-IF.
           MOVE 0 TO ACTUAL-LENGTH.
           MOVE 0 TO IDX.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > SCAN-LIMIT
               IF LNK-SOURCE(IDX:1) = LOW-VALUE
                   EXIT PERFORM
               END-IF
               MOVE IDX TO ACTUAL-LENGTH
           END-PERFORM.
           IF ACTUAL-LENGTH = 0
               MOVE SCAN-LIMIT TO ACTUAL-LENGTH
           END-IF.
           MOVE 1 TO START-INDEX.
           PERFORM VARYING START-INDEX FROM 1 BY 1 UNTIL START-INDEX > ACTUAL-LENGTH
               IF LNK-SOURCE(START-INDEX:1) NOT = SPACE
                   EXIT PERFORM
               END-IF
           END-PERFORM.
           IF START-INDEX > ACTUAL-LENGTH
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           MOVE ACTUAL-LENGTH TO END-INDEX.
           PERFORM UNTIL END-INDEX < START-INDEX
               MOVE LNK-SOURCE(END-INDEX:1) TO CURRENT-CHAR
               IF CURRENT-CHAR NOT = SPACE AND CURRENT-CHAR NOT = LOW-VALUE
                   EXIT PERFORM
               END-IF
               COMPUTE END-INDEX = END-INDEX - 1
           END-PERFORM.
           IF END-INDEX < START-INDEX
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           MOVE 0 TO NORMALIZED-LENGTH.
           MOVE SPACES TO NORMALIZED-BUFFER.
           MOVE 0 TO HAS-ANY-DIGIT.
           MOVE 0 TO HAS-DECIMAL.
           MOVE 0 TO HAS-EXPONENT.
           MOVE 0 TO EXPONENT-DIGITS.
           MOVE 0 TO EXPECT-EXPONENT-SIGN.
           PERFORM VARYING IDX FROM START-INDEX BY 1 UNTIL IDX > END-INDEX
               MOVE LNK-SOURCE(IDX:1) TO CURRENT-CHAR
               IF CURRENT-CHAR = SPACE OR CURRENT-CHAR = LOW-VALUE
                   MOVE 1 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
               END-IF
               EVALUATE TRUE
                   WHEN CURRENT-CHAR >= "0" AND CURRENT-CHAR <= "9"
                       IF NORMALIZED-LENGTH >= 255
                           MOVE 1 TO LNK-STATUS
                           MOVE 0 TO LNK-RESULT
                           GOBACK
                       END-IF
                       ADD 1 TO NORMALIZED-LENGTH
                       MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)
                       MOVE 1 TO HAS-ANY-DIGIT
                       IF HAS-EXPONENT = 1
                           ADD 1 TO EXPONENT-DIGITS
                       END-IF
                       MOVE 0 TO EXPECT-EXPONENT-SIGN
                   WHEN CURRENT-CHAR = "."
                       IF HAS-DECIMAL = 1 OR HAS-EXPONENT = 1
                           MOVE 1 TO LNK-STATUS
                           MOVE 0 TO LNK-RESULT
                           GOBACK
                       END-IF
                       IF NORMALIZED-LENGTH >= 255
                           MOVE 1 TO LNK-STATUS
                           MOVE 0 TO LNK-RESULT
                           GOBACK
                       END-IF
                       ADD 1 TO NORMALIZED-LENGTH
                       MOVE "." TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)
                       MOVE 1 TO HAS-DECIMAL
                   WHEN CURRENT-CHAR = "E" OR CURRENT-CHAR = "e"
                       IF HAS-EXPONENT = 1 OR HAS-ANY-DIGIT = 0
                           MOVE 1 TO LNK-STATUS
                           MOVE 0 TO LNK-RESULT
                           GOBACK
                       END-IF
                       IF NORMALIZED-LENGTH >= 255
                           MOVE 1 TO LNK-STATUS
                           MOVE 0 TO LNK-RESULT
                           GOBACK
                       END-IF
                       ADD 1 TO NORMALIZED-LENGTH
                       MOVE "E" TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)
                       MOVE 1 TO HAS-EXPONENT
                       MOVE 0 TO EXPONENT-DIGITS
                       MOVE 1 TO EXPECT-EXPONENT-SIGN
                   WHEN CURRENT-CHAR = "+" OR CURRENT-CHAR = "-"
                       IF NORMALIZED-LENGTH = 0
                           IF NORMALIZED-LENGTH >= 255
                               MOVE 1 TO LNK-STATUS
                               MOVE 0 TO LNK-RESULT
                               GOBACK
                           END-IF
                           ADD 1 TO NORMALIZED-LENGTH
                           MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)
                       ELSE
                           IF EXPECT-EXPONENT-SIGN = 1
                               IF NORMALIZED-LENGTH >= 255
                                   MOVE 1 TO LNK-STATUS
                                   MOVE 0 TO LNK-RESULT
                                   GOBACK
                               END-IF
                               ADD 1 TO NORMALIZED-LENGTH
                               MOVE CURRENT-CHAR TO NORMALIZED-BUFFER(NORMALIZED-LENGTH:1)
                               MOVE 0 TO EXPECT-EXPONENT-SIGN
                           ELSE
                               MOVE 1 TO LNK-STATUS
                               MOVE 0 TO LNK-RESULT
                               GOBACK
                           END-IF
                       END-IF
                   WHEN OTHER
                       MOVE 1 TO LNK-STATUS
                       MOVE 0 TO LNK-RESULT
                       GOBACK
               END-EVALUATE
           END-PERFORM.
           IF NORMALIZED-LENGTH = 0
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           IF HAS-ANY-DIGIT = 0
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           IF HAS-EXPONENT = 1 AND EXPONENT-DIGITS = 0
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           IF EXPECT-EXPONENT-SIGN = 1
               MOVE 1 TO LNK-STATUS
               MOVE 0 TO LNK-RESULT
               GOBACK
           END-IF.
           COMPUTE LNK-RESULT = FUNCTION NUMVAL(NORMALIZED-BUFFER(1:NORMALIZED-LENGTH))
               ON SIZE ERROR
                   MOVE 1 TO LNK-STATUS
                   MOVE 0 TO LNK-RESULT
                   GOBACK
           END-COMPUTE.
           MOVE 0 TO LNK-STATUS.
           GOBACK.
       END PROGRAM CBLC-STRTOD.

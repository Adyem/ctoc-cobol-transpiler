>>SOURCE FORMAT IS FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLC-DATE-YYYYMMDD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 F-INDEX PIC S9(9) COMP-5.
       01 F-DIGIT PIC S9(9) COMP-5.
       01 F-MONTH-BASE PIC S9(9) COMP-5.
       01 F-MAX-DAY PIC S9(9) COMP-5.
       01 F-LEAP-YEAR PIC S9(9) COMP-5.
       01 F-PREVIOUS-YEAR PIC S9(9) COMP-5.
       01 F-LEAP-COUNT PIC S9(9) COMP-5.
       01 F-CURRENT PIC X(1).
       01 CBLC-PTR-CURSOR USAGE POINTER VALUE NULL.
       01 CBLC-PTR-OFFSET PIC S9(9) VALUE 0.
       01 CBLC-PTR-CHAR BASED PIC X.
       01 CBLC-PTR-INT BASED PIC S9(9).
       01 CBLC-PTR-INT-PTR BASED USAGE POINTER.
       01 CBLC-PTR-CHAR-2 BASED PIC X.
       01 CBLC-PTR-INT-2 BASED PIC S9(9).
       01 CBLC-PTR-INT-PTR-2 BASED USAGE POINTER.
       LINKAGE SECTION.
       01 F-INPUT PIC X(255).
       01 F-YEAR PIC 9(4).
       01 F-MONTH PIC 9(2).
       01 F-DAY PIC 9(2).
       01 F-PACKED PIC 9(8) COMP-3.
       01 F-SERIAL PIC S9(9) COMP-5.
       01 F-STATUS PIC 9.
       PROCEDURE DIVISION USING BY REFERENCE F-INPUT
            BY REFERENCE F-YEAR BY REFERENCE F-MONTH
            BY REFERENCE F-DAY BY REFERENCE F-PACKED
            BY REFERENCE F-SERIAL BY REFERENCE F-STATUS.
       F.
           COMPUTE F-STATUS = 0.
           COMPUTE F-PACKED = 0.
           COMPUTE F-INDEX = 0.
           PERFORM UNTIL NOT (F-INDEX < 8)
           MOVE F-INPUT(F-INDEX + 1:1) TO F-CURRENT
           COMPUTE F-DIGIT = FUNCTION ORD(F-CURRENT) - 49
           COMPUTE F-PACKED = F-PACKED * 10 + F-DIGIT
           COMPUTE F-INDEX = F-INDEX + 1
           END-PERFORM.
           COMPUTE F-YEAR = F-PACKED / 10000.
           COMPUTE F-MONTH-BASE = F-PACKED / 100.
           COMPUTE F-MONTH = F-MONTH-BASE - F-YEAR * 100.
           COMPUTE F-DAY = F-PACKED - F-MONTH-BASE * 100.
           COMPUTE F-MAX-DAY = 31.
           COMPUTE F-LEAP-YEAR = 0.
           IF F-YEAR / 4 * 4 = F-YEAR
           COMPUTE F-LEAP-YEAR = 1
           END-IF.
           IF F-YEAR / 100 * 100 = F-YEAR
           COMPUTE F-LEAP-YEAR = 0
           END-IF.
           IF F-YEAR / 400 * 400 = F-YEAR
           COMPUTE F-LEAP-YEAR = 1
           END-IF.
           IF F-MONTH = 4
           COMPUTE F-MAX-DAY = 30
           END-IF.
           IF F-MONTH = 6
           COMPUTE F-MAX-DAY = 30
           END-IF.
           IF F-MONTH = 9
           COMPUTE F-MAX-DAY = 30
           END-IF.
           IF F-MONTH = 11
           COMPUTE F-MAX-DAY = 30
           END-IF.
           IF F-MONTH = 2
           COMPUTE F-MAX-DAY = 28
           IF F-LEAP-YEAR = 1
           COMPUTE F-MAX-DAY = 29
           END-IF
           END-IF.
           IF F-MONTH < 1
           COMPUTE F-STATUS = 2
           END-IF.
           IF F-MONTH > 12
           COMPUTE F-STATUS = 2
           END-IF.
           IF F-DAY < 1
           COMPUTE F-STATUS = 3
           END-IF.
           IF F-DAY > F-MAX-DAY
           COMPUTE F-STATUS = 3
           END-IF.
           IF F-STATUS NOT = 0
           COMPUTE F-YEAR = 0
           COMPUTE F-MONTH = 0
           COMPUTE F-DAY = 0
           COMPUTE F-PACKED = 0
           COMPUTE F-SERIAL = 0
           END-IF.
           IF F-STATUS = 0
           COMPUTE F-PREVIOUS-YEAR = F-YEAR
           COMPUTE F-PREVIOUS-YEAR = F-PREVIOUS-YEAR - 1
           COMPUTE F-SERIAL = F-YEAR
           COMPUTE F-SERIAL = F-SERIAL - 1601
           COMPUTE F-SERIAL = F-SERIAL * 365
           COMPUTE F-LEAP-COUNT = F-PREVIOUS-YEAR
           COMPUTE F-LEAP-COUNT = F-LEAP-COUNT / 4
           COMPUTE F-LEAP-COUNT = F-LEAP-COUNT - 400
           COMPUTE F-SERIAL = F-SERIAL + F-LEAP-COUNT
           COMPUTE F-LEAP-COUNT = F-PREVIOUS-YEAR
           COMPUTE F-LEAP-COUNT = F-LEAP-COUNT / 100
           COMPUTE F-LEAP-COUNT = F-LEAP-COUNT - 16
           COMPUTE F-SERIAL = F-SERIAL - F-LEAP-COUNT
           COMPUTE F-LEAP-COUNT = F-PREVIOUS-YEAR
           COMPUTE F-LEAP-COUNT = F-LEAP-COUNT / 400
           COMPUTE F-LEAP-COUNT = F-LEAP-COUNT - 4
           COMPUTE F-SERIAL = F-SERIAL + F-LEAP-COUNT
           COMPUTE F-SERIAL = F-SERIAL + F-DAY
           IF F-MONTH > 1
           COMPUTE F-SERIAL = F-SERIAL + 31
           END-IF
           IF F-MONTH > 2
           COMPUTE F-SERIAL = F-SERIAL + 28
           END-IF
           IF F-MONTH > 2 AND F-LEAP-YEAR = 1
           COMPUTE F-SERIAL = F-SERIAL + 1
           END-IF
           IF F-MONTH > 3
           COMPUTE F-SERIAL = F-SERIAL + 31
           END-IF
           IF F-MONTH > 4
           COMPUTE F-SERIAL = F-SERIAL + 30
           END-IF
           IF F-MONTH > 5
           COMPUTE F-SERIAL = F-SERIAL + 31
           END-IF
           IF F-MONTH > 6
           COMPUTE F-SERIAL = F-SERIAL + 30
           END-IF
           IF F-MONTH > 7
           COMPUTE F-SERIAL = F-SERIAL + 31
           END-IF
           IF F-MONTH > 8
           COMPUTE F-SERIAL = F-SERIAL + 31
           END-IF
           IF F-MONTH > 9
           COMPUTE F-SERIAL = F-SERIAL + 30
           END-IF
           IF F-MONTH > 10
           COMPUTE F-SERIAL = F-SERIAL + 31
           END-IF
           IF F-MONTH > 11
           COMPUTE F-SERIAL = F-SERIAL + 30
           END-IF
           END-IF.
           GOBACK.

       END PROGRAM CBLC-DATE-YYYYMMDD.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILTER-PREFIX.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOURCE-FILE ASSIGN TO "source.dat".
           SELECT TARGET-FILE ASSIGN TO "target.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  SOURCE-FILE.
       01  SOURCE-RECORD.
           05  SOURCE-LINE PIC X(256).
       FD  TARGET-FILE.
       01  TARGET-RECORD.
           05  TARGET-LINE PIC X(256).
       WORKING-STORAGE SECTION.
       01  PREFIX PIC X(8) VALUE "ALLOW".
       01  EOF-FLAG PIC X VALUE 'N'.
       PROCEDURE DIVISION.
           OPEN INPUT SOURCE-FILE
                OUTPUT TARGET-FILE.
           PERFORM UNTIL EOF-FLAG = 'Y'
               READ SOURCE-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       IF SOURCE-LINE(1:5) = PREFIX(1:5)
                           MOVE SOURCE-RECORD TO TARGET-RECORD
                           WRITE TARGET-RECORD
                       END-IF
               END-READ
           END-PERFORM.
           CLOSE SOURCE-FILE TARGET-FILE.
           STOP RUN.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. COPY-FILE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.dat".
           SELECT OUTPUT-FILE ASSIGN TO "output.dat".
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD PIC X(256).
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD PIC X(256).
       WORKING-STORAGE SECTION.
       01  EOF-FLAG PIC X VALUE 'N'.
       PROCEDURE DIVISION.
MAIN.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           PERFORM UNTIL EOF-FLAG == 'Y'
               READ INPUT-FILE
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       MOVE INPUT-RECORD TO OUTPUT-RECORD
                       WRITE OUTPUT-RECORD
               END-READ
           END-PERFORM.
           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.
           STOP RUN.

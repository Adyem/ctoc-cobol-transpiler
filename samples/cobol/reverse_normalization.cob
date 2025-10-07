       IDENTIFICATION DIVISION.
       PROGRAM-ID. NORMALIZATION-DEMO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 running-total-value PIC 9(4) VALUE 0005.
       01 status-flag PIC X VALUE 'n'.
       01 scratch-note PIC X(12) VALUE 'raw value'.
       PROCEDURE DIVISION.
ENTRY-PARAGRAPH.
       MOVE 'mixED Case value' TO scratch-note.
       MOVE 0000 TO running-total-value.
       MOVE 'y' TO status-flag.
       STOP RUN.
NORMALIZE-VALUES.
       MOVE 0007 TO running-total-value.
       MOVE 'done' TO scratch-note.
       STOP RUN.

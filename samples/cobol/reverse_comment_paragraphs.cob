       IDENTIFICATION DIVISION.
       PROGRAM-ID. REVERSE-COMMENT-PARAS.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 STATUS-FLAG PIC X VALUE 'Y'.
*> header note before first paragraph
*> secondary description for entry point
       PROCEDURE DIVISION.
MAIN.
       *> comment before assignment
       MOVE 'N' TO STATUS-FLAG. *> inline comment for assignment
       IF STATUS-FLAG = 'N'
           *> comment inside IF
           DISPLAY STATUS-FLAG
       END-IF
       *> trailing note prior to next paragraph
       STOP RUN.
NEXT-PARAGRAPH.
       *> comment nested within next paragraph
       DISPLAY STATUS-FLAG.
       STOP RUN.
       END PROGRAM REVERSE-COMMENT-PARAS.

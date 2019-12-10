       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTPG01.
       DATE-WRITTEN. 12/8/2019.
       AUTHOR. TYLER GRAHAM.
       DATE-COMPILED.
      *-----------------------------------------------------------------
      *    THIS PROGRAM IS TO MAKE A PAINT JOB ESTIMATOR REPORT
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAINT-NUM
               ASSIGN TO "C:\COBOL\PAINTTEST.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT PRTOUT
              ASSIGN TO "C:\COBOL\PAINTRPT.PRT"
              ORGANIZATION IS RECORD SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  PAINT-NUM
           LABEL RECORD IS STANDARD
           DATA RECORD IS I-REC
           RECORD CONTAINS 49 CHARACTERS.
       01      PAINT-REC.
           05  I-PAINT-EST-NO        PIC X(4).        
           05  I-PAINT-DATE.
               10  I-PAINT-YY        PIC 9(4).
               10  I-PAINT-MM        PIC 99.
               10  I-PAINT-DD        PIC 99.
           05  I-PAINT-WALL-SQ-FT    PIC 9(4).
           05  I-PAINT-DOOR-SQ-FT    PIC 9(3).
           05  I-PAINT-PRICE-GAL     PIC 99V99.
         FD   PRTOUT
              LABEL RECORD IS OMITTED
              RECORD CONTAINS 132 CHARACTERS
              DATA RECORD IS PRTLINE
              LINAGE IS 60 WITH FOOTING AT 56.

       01 PRTLINE                      PIC X(140).

       WORKING-STORAGE SECTION.
       01 WORK-AREA.
         05    WORK-ARA1.
           10      THING               PIC X.
           05  C-ESTCTR                PIC 999 VALUE 0.
           05  C-PCTR                  PIC 99 VALUE ZERO.
           05  C-PAINT-WALL-SQ-FT      PIC 9(4).
           05  C-PAINT-PRICE           PIC 99V99.
           05  C-PAINT-GAL-NEED        PIC 999V99.
           05  C-LABOR-COST            PIC 99999V99.
           05  C-TOTAL-COST            PIC 999999V99.
           05  C-TOTAL-SQ-FT           PIC ZZZV9.
           05  C-GT-TOTAL-SQ-FT        PIC ZZ9.
           05  C-GT-PAINT-GAL-NEED     PIC ZZ,ZZZV99.
           05  C-GT-PAINT-PRICE        PIC $$,$$$,$$$V99.
           05  C-GT-LABOR-COST         PIC $$,$$$,$$$V99.
           05  C-GT-TOTAL-COST         PIC $$$,$$$,$$$V99.
           05  MORE-REC                PIC XXX VALUE "YES".

       01 CURRENT-DATE-AND-TIME.
         05 THIS-DATE.
           10  I-YY                     PIC 9(4).
           10  I-MM                     PIC 99.
           10  I-DD                     PIC 99.

       01 COMPANY-TITLE.
           05  FILLER                     PIC X(6) VALUE "DATE:".
           05  O-MM                       PIC 99.
           05  FILLER                     PIC X VALUE '/'.
           05  O-DD                       PIC 99.
           05  FILLER                     PIC X VALUE '/'.
           05  O-YY                       PIC 9(4).
           05  FILLER                     PIC X(36) VALUE SPACES.
           05  FILLER                     PIC X(24)
                                       VALUE 'GRAHAMS PAINT CALCULATOR'.
           05  FILLER                     PIC X(45) VALUE SPACES.
           05  FILLER                     PIC X(6) VALUE "PAGE:".
           05  O-PCTR                     PIC Z9.


       01 COLLUMN-HEADINGS1.
            05  FILLER           PIC X(119) VALUE SPACES.
            05  FILLER              PIC X(13) VALUE "ESTIMATED".

       01 COLLUMN-HEADINGS2.
           05  FILLER             PIC X(13) VALUE "PAINT EST NUM".
           05  FILLER             PIC X(23) VALUE SPACES.
           05  FILLER             PIC X(10) VALUE "PAINT YEAR".
           05  FILLER             PIC X(26) VALUE SPACES.
           05  FILLER             PIC X(11) VALUE "PAINT MONTH".
           05  FILLER             PIC X(26) VALUE SPACES.
           05  FILLER             PIC X(10) VALUE "WALL SQ FT".
           05  FILLER             PIC X(16) VALUE SPACES.
           05  FILLER             PIC X(10) VALUE "DOOR SQ FT".
           05  FILLER             PIC X(16) VALUE SPACES.
           05  FILLER             PIC X(11) VALUE "TOTAL SQ FT".
           05  FILLER             PIC X(16) VALUE SPACES.
           05  FILLER             PIC X(15) VALUE "GALLONS NEEDED".
           05  FILLER             PIC X(16) VALUE SPACES.
           05  FILLER             PIC X(16) VALUE "PRICE PER GALLON".
           05  FILLER             PIC X(16) VALUE SPACES.
           05  FILLER             PIC X(20)
                                  VALUE "TOTAL PAINT ESTIMATE".
           05  FILLER             PIC X(16) VALUE SPACES.
           05  FILLER             PIC X(16) VALUE "LABOR ESTIMATE".
           05  FILLER             PIC X(16) VALUE SPACES.
           05  FILLER             PIC X(16) VALUE "TOTAL ESTIMATE".
                                       
       01 BLANK-LINE2.
         05 FILLER                     PIC X(132) VALUE SPACES.

       01 DETAIL-LINE.
         05  O-PAINT-EST-NO             PIC X(4).
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-PAINT-YY                 PIC 9(4).
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-PAINT-MM                 PIC 99.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-PAINT-DD                 PIC 99.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-PAINT-WALL-SQ-FT         PIC Z,ZZ9.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-PAINT-DOOR-SQ-FT         PIC ZZ9.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-TOTAL-SQ-FT              PIC Z,ZZ9.
         05  FILLER                     PIC X(16) VALUE SPACES.
         05  O-PAINT-GAL-NEED           PIC ZZZ.99.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-PAINT-PRICE-GAL          PIC ZZ.99.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-PAINT-PRICE              PIC $ZZ,ZZZ.99.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-LABOR-COST               PIC $ZZ,ZZZ.99.
         05  FILLER                     PIC X(20) VALUE SPACES.
         05  O-TOTAL-COST               PIC $ZZZ,ZZZ.99.
         05  FILLER                     PIC XX VALUE SPACES.


       01 GT-LINE.
           05  FILLER                 PIC X(54) VALUE SPACES.
           05  FILLER                 PIC X(15) VALUE"TOTAL SQ FT".
           05  O-GT-TOTAL-SQ-FT       PIC ZZ9.???
           05  FILLER                 PIC X(15) VALUE "PAINT JOB COUNT".
           05  O-ESTCTR               PIC ZZ9.
           05  FILLER                 PIC X(60) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INIT.
           PERFORM 2000-MAINLINE
             UNTIL MORE-REC = "NO".
           PERFORM 3000-CLOSING.
           STOP RUN.

       1000-INIT.
           OPEN INPUT PAINT-NUM
           OPEN OUTPUT PRTOUT

           MOVE FUNCTION CURRENT-DATE TO CURRENT-DATE-AND-TIME.
           MOVE I-YY TO O-YY.
           MOVE I-DD TO O-DD.
           MOVE I-MM TO O-MM.

           PERFORM 9000-READ.
           PERFORM 9100-HDGS.

       2000-MAINLINE.
           PERFORM 2100-CALCS.
           PERFORM 2200-OUTPUT.
           PERFORM 9000-READ.

       2100-CALCS.
           ADD 1 TO C-ESTCTR.
           SUBTRACT I-PAINT-DOOR-SQ-FT FROM I-PAINT-WALL-SQ-FT
               GIVING C-PAINT-WALL-SQ-FT.
           DIVIDE 115 INTO C-PAINT-WALL-SQ-FT
               GIVING C-PAINT-GAL-NEED.
           MULTIPLY  C-PAINT-GAL-NEED BY I-PAINT-PRICE-GAL
               GIVING C-PAINT-PRICE.
           COMPUTE C-LABOR-COST = 23.55 * C-PAINT-GAL-NEED.
           ADD C-PAINT-PRICE TO C-LABOR-COST 
               GIVING C-TOTAL-COST.
           COMPUTE C-TOTAL-SQ-FT = C-PAINT-WALL-SQ-FT + 
           I-PAINT-DOOR-SQ-FT.

       2200-OUTPUT.
           MOVE I-PAINT-EST-NO TO O-PAINT-EST-NO.
           MOVE I-PAINT-YY TO O-PAINT-YY.
           MOVE I-PAINT-MM TO O-PAINT-MM.
           MOVE I-PAINT-DD TO O-PAINT-DD.
           MOVE C-PAINT-WALL-SQ-FT TO O-PAINT-WALL-SQ-FT.
           MOVE I-PAINT-DOOR-SQ-FT TO O-PAINT-DOOR-SQ-FT.
           MOVE C-PAINT-PRICE TO O-PAINT-PRICE.
           MOVE C-TOTAL-SQ-FT TO O-TOTAL-SQ-FT.
           MOVE C-PAINT-GAL-NEED TO O-PAINT-GAL-NEED.
           MOVE I-PAINT-PRICE-GAL TO O-PAINT-PRICE-GAL
           MOVE C-LABOR-COST TO O-LABOR-COST.
           MOVE C-TOTAL-COST TO O-TOTAL-COST.

           WRITE PRTLINE FROM DETAIL-LINE
             AFTER ADVANCING 2 lines
               AT EOP
                   PERFORM 9100-HDGS.

       3000-CLOSING.
           MOVE C-ESTCTR TO O-ESTCTR.
           WRITE PRTLINE FROM GT-LINE
             AFTER ADVANCING 3 lines.
           CLOSE PAINT-NUM.
           CLOSE PRTOUT.


       9000-READ.
           READ PAINT-NUM
               AT END
                   MOVE "NO" TO MORE-REC.

       9100-HDGS.
           ADD 1 TO C-PCTR.
           MOVE C-PCTR TO O-PCTR.
           WRITE PRTLINE FROM COMPANY-TITLE
             AFTER ADVANCING PAGE.
           WRITE PRTLINE FROM COLLUMN-HEADINGS1
             AFTER ADVANCING 2 LINES.
           WRITE PRTLINE FROM COLLUMN-HEADINGS2
             AFTER ADVANCING 1 LINE.
          
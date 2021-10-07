        IDENTIFICATION DIVISION.
        PROGRAM-ID. Project1.
        AUTHOR. Martin Funmaker.
      * Project 1
        ENVIRONMENT DIVISION.
        CONFIGURATION SECTION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO 'COB1-EMPLOYEE'.
           SELECT PRNT-FILE ASSIGN TO 'EMPLOYEE-PRNT'.

        DATA DIVISION.

        FILE SECTION.
        FD INPUT-FILE
          BLOCK CONTAINS 0 RECORDS
          LABEL RECORDS ARE STANDARD.
        01 INPUT-REC PIC X(98).

        FD PRNT-FILE
           LABEL RECORDS ARE OMITTED.
        01 PRNT-REC PIC X(125).
        WORKING-STORAGE SECTION.

      **************************************************************
      * LAYOUT FOR THE INPUT FILE *
      **************************************************************
        01 INPUT-DATA.
                03 I-ID PIC X(7).
                03 I-LNAME PIC X(15).
                03 I-FNAME PIC X(15).
                03 I-ETYPE PIC X(02).
                03 I-CLASS PIC X(17).
                03 I-SSN   PIC X(9).
                03 FILLER  PIC X(25) VALUE SPACE.
                03 I-DATE  PIC X(8).
      **************************************************************
      * LAYOUT FOR THE 1ST DATA LINE OF REPORT PRNTING *
      **************************************************************
        01 PRNT-DATA1.
                03 FILLER PIC X(3) VALUE SPACES.
                03 P-SSN  PIC XXXBXXBXXXX.
                03 FILLER PIC X(3) VALUE SPACES.
                03 P-PID PIC X(7).
                03 FILLER PIC X(3) VALUE SPACES.
                03 P-LNAME PIC X(15).
                03 FILLER PIC X(2) VALUE SPACES.
                03 P-FNAME PIC X(15).
                03 FILLER PIC X(10) VALUE SPACES.
                03 P-CLASS PIC X(17).
                03 FILLER PIC X(7) VALUE SPACES.
                03 P-TYPE PIC X(2).
                03 FILLER PIC X(5) VALUE SPACES.
                03 P-DATE PIC 99/99/9999.
                03 FILLER PIC X(15) VALUE SPACES.
      **************************************************************
      * LAYOUT FOR THE HEADING LINES OF REPORT PRNTING *
      **************************************************************
       01 PRNT-HEADING1.
                03 FILLER       PIC X(54) VALUE SPACES.
                03            PIC X(24) VALUE 'IBM ACADEMIC INITIATIVE'.
                03              PIC X(47) VALUE SPACES.
       01 PRNT-HEADING2.
                03 FILLER       PIC X(55) VALUE SPACES.
                03              PIC X(21) VALUE 'INTRODUCTION TO COBOL'.
                03 FILLER       PIC X(49) VALUE SPACES.
       01 PRNT-HEADING3.
                03 FILLER       PIC X(2) VALUE SPACES.
                03              PIC X(3)  VALUE 'SSN'.
                03 FILLER       PIC X(12) VALUE SPACES.
                03              PIC X(6) VALUE 'EMP ID'.
                03 FILLER       PIC X(4) VALUE SPACES.
                03              PIC X(4) VALUE 'LAST'.
                03 FILLER       PIC X(16) VALUE SPACES.
                03              PIC X(5) VALUE 'FIRST'.
                03 FILLER       PIC X(16) VALUE SPACES.
                03              PIC X(5) VALUE 'TITLE'.
                03 FILLER       PIC X(18) VALUE SPACES.
                03              PIC X(4) VALUE 'TYPE'.
                03 FILLER       PIC X(4) VALUE SPACES.
                03              PIC X(4) VALUE 'DATE'.
                03              PIC X(22) VALUE SPACES.
        01 MISC.
      **************************************************************
      * END OF FILE (EOF) SWITCHES *
      * 0 = NOT AT EOF 1 = AT EOF *
      **************************************************************
          03 EOF-I PIC 9 VALUE 0.
      **************************************************************
      * START OF PROCEDURE DIVISION *
      **************************************************************
        PROCEDURE DIVISION.
        000-MAINLINE.
                OPEN INPUT INPUT-FILE
                        OUTPUT PRNT-FILE.
                PERFORM 2000-READ-INPUT.
                PERFORM 1400-PRINT-HEAD.
                PERFORM 1500-LOOP
                   UNTIL EOF-I = 1.
                CLOSE INPUT-FILE
                      PRNT-FILE.
                STOP RUN.
        1400-PRINT-HEAD.
       
          WRITE PRNT-REC FROM PRNT-HEADING1
            AFTER ADVANCING PAGE.
                MOVE SPACES TO PRNT-REC.
                WRITE PRNT-REC
                AFTER ADVANCING 1 LINE.

          WRITE PRNT-REC FROM PRNT-HEADING2.
                MOVE SPACES TO PRNT-REC.
                WRITE PRNT-REC
                AFTER ADVANCING 1 LINE.
          
           WRITE PRNT-REC FROM PRNT-HEADING3.
                MOVE SPACES TO PRNT-REC.
                WRITE PRNT-REC
                AFTER ADVANCING 1 LINE.

        1500-LOOP.
                PERFORM 1600-PRINT-NAMES.
                PERFORM 2000-READ-INPUT.
      **************************************************************
      * PRINTS THE SCHEDULE INFORMATION *
      **************************************************************
        1600-PRINT-NAMES.
           MOVE I-SSN           TO P-SSN.
           INSPECT P-SSN REPLACING ALL ' ' BY '-'.
           MOVE I-ID            TO P-PID.
           MOVE I-LNAME         TO P-LNAME.
           MOVE I-FNAME         TO P-FNAME.
           MOVE I-CLASS         TO P-CLASS.
           MOVE I-ETYPE         TO P-TYPE.
           MOVE I-DATE      TO P-DATE.
           WRITE PRNT-REC FROM PRNT-DATA1
            AFTER ADVANCING 1 LINE.
      **************************************************************
      * READS THE INPUT FILE *
      **************************************************************
        2000-READ-INPUT.
          READ INPUT-FILE INTO INPUT-DATA

                  AT END MOVE 1 TO EOF-I.


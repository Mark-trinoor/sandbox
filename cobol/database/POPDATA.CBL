      * Populate Database
        IDENTIFICATION DIVISION.
         PROGRAM-ID.     popdata.
         AUTHOR.         Mark.
         INSTALLATION.   Ubuntu.
         DATE-WRITTEN.   2024-02-10.
         DATE-COMPILED.  2024-02-10.
      *
      * PROJECT DESC: READ DATA AND POPULATE TABLE
      *
        ENVIRONMENT DIVISION.

        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT INPUT-FILE
      *     ASSIGN TO '/home/mark/mysql/scripts/data' 
            ASSIGN TO './data'
            ORGANIZATION IS LINE SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL. 
        DATA DIVISION.
        FILE SECTION.
        FD INPUT-FILE          RECORD CONTAINS 220 CHARACTERS.
        01 INPUT-RECORD        PIC X(180).

        01 OUTPUT-RECORD.
            05 OUT-SDATE       PIC X(10).
            05 OUT-STIME       PIC X(10).
            05 OUT-CATEGORY    PIC X(35).
            05 OUT-ITEM-NAME   PIC X(40).
            05 OUT-QUANTITY    PIC 9(3).
            05 OUT-SDETAIL     PIC X(45).
            05 OUT-SKU         PIC X(20).
            05 OUT-DISCOUNT    PIC 9(4).
            05 OUT-PRICE       PIC 9(4).
            05 OUT-TRANS-ID    PIC X(40).
            05 OUT-SUNIT       PIC X(4).
            05 OUT-GTIN        PIC X(10).
            05 OUT-COST        PIC 9(4).

        WORKING-STORAGE SECTION.
        01  WS-EOF             PIC X(1)    VALUE 'N'.

        01  TMP-DATA           PIC X(220).

        01  SEPARATED.
            05 SDATE           PIC X(10).
            05 WS-DATE REDEFINES SDATE.
                10 YY      PIC 9(2).
                10 FILLER  PIC X(1) VALUE '-'.
                10 MM      PIC 9(3).
                10 FILLER  PIC X(1) VALUE '-'.
                10 DD      PIC 9(2).
            05 STIME           PIC X(10).
            05 CATEGORY        PIC X(35).
            05 ITEM-NAME       PIC X(40).
            05 QUANTITY        PIC 9(3).
            05 SDETAIL         PIC X(45).
            05 SKU             PIC X(20).
            05 DISCOUNT        PIC 9(4).
            05 PRICE           PIC 9(4).
            05 TRANS-ID        PIC X(40).
            05 SUNIT           PIC X(4).
            05 GTIN            PIC X(10).
            05 COST            PIC 9(4).
 
        01  SQL-FIELDS-STR.
            05 FS              PIC X(1) VALUE ','.

        PROCEDURE DIVISION.
        MAIN-PROCEDURE.

            OPEN INPUT INPUT-FILE
                PERFORM READ-PROCEDURE UNTIL WS-EOF = 'Y'
                CLOSE INPUT-FILE
                STOP RUN.

        READ-PROCEDURE.
            MOVE SPACES TO SEPARATED.
            READ INPUT-FILE INTO INPUT-RECORD
                    AT END MOVE 'Y' TO WS-EOF
                NOT AT END PERFORM DISPLAY-PROCEDURE
            END-READ.

        DISPLAY-PROCEDURE.   
            UNSTRING INPUT-RECORD DELIMITED BY ','
                 INTO SDATE STIME CATEGORY ITEM-NAME QUANTITY
                     SDETAIL SKU DISCOUNT PRICE TRANS-ID
                     SUNIT GTIN COST
            END-UNSTRING.
            MOVE SPACES TO TMP-DATA.
            STRING  ITEM-NAME DELIMITED BY SIZE
                    SPACE
                    QUANTITY
                    SPACE
                    SDETAIL DELIMITED BY SIZE
                    SPACE
                    SKU  DELIMITED BY SIZE
                    INTO        TMP-DATA
            END-STRING.

            DISPLAY TMP-DATA.

        
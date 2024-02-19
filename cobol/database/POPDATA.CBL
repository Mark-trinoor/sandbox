      * Populate Database
        IDENTIFICATION DIVISION.
         PROGRAM-ID.     POPDATA.
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
            ASSIGN TO './data.nosp'
            ORGANIZATION IS LINE SEQUENTIAL
            ACCESS MODE IS SEQUENTIAL. 
        DATA DIVISION.
        FILE SECTION.
        FD INPUT-FILE             RECORD CONTAINS 220 CHARACTERS.
        01 INPUT-RECORD           PIC X(180).

        01 OUTPUT-RECORD.
            05 OUT-SDATE          PIC X(10).
            05 OUT-STIME          PIC X(10).
            05 OUT-CATEGORY       PIC X(35).
            05 OUT-ITEM-NAME      PIC X(40).
            05 OUT-QUANTITY       PIC 9(3).
            05 OUT-SDETAIL        PIC X(45).
            05 OUT-SKU            PIC X(16).
            05 OUT-DISCOUNT       PIC 9(4).
            05 OUT-PRICE          PIC 9(4).
            05 OUT-TRANS-ID       PIC X(40).
            05 OUT-SUNIT          PIC X(4).
            05 OUT-GTIN           PIC X(10).
            05 OUT-COST           PIC 9(4).

        WORKING-STORAGE SECTION.
        01  WS-EOF                PIC X    VALUE 'N'.
        01  WS-OFLOW              PIC X    VALUE 'N'.

        01  WS-PROFIT             PIC 9(3)V99.
        01  WS-TMP-DATA           PIC X(220).

        01  WS-CONVERTED-SPRICE   PIC ZZ9.99.
        01  WS-CONVERTED-OPRICE   PIC ZZ9.99.
        01  WS-CONVERTED-DISCOUNT PIC ZZ9.99.
        01  WS-CONVERTED-COST     PIC ZZ9.99.
        01  WS-CONVERTED-SALE     PIC ZZZ9.99.
        01  WS-CONVERTED-PROFIT   PIC ZZ9.99.
        01  WS-CONVERTED-TCOST    PIC ZZ9.99. 

        01  WS-SEPARATED.
            05 WS-SDATE           PIC X(10).           
            05 WS-STIME           PIC X(10).
            05 WS-CATEGORY        PIC X(35).
            05 WS-ITEM-NAME       PIC X(30).
            05 WS-QUANTITY        PIC 999.
            05 WS-SDETAIL         PIC X(45).
            05 WS-SKU             PIC X(16).
            05 WS-DISCOUNT        PIC 9(2)V99.
            05 WS-PRICE           PIC 9(2)V99.
            05 WS-TRANS-ID        PIC X(40).
            05 WS-SUNIT           PIC X(4).
            05 WS-GTIN            PIC X(10).
            05 WS-COST            PIC 99V99.

        01  WS-SQL-FIELDS-STR.
            05 FS                 PIC X(1) VALUE ','.

        01  WS-TOTAL-COST         PIC 9(3)V99.
        01  WS-TOTAL-SALE         PIC 9(3)V99.
        01  WS-ORIG-PRICE         PIC 9(2)V99.

        PROCEDURE DIVISION.
        MAIN-PROCEDURE.

            OPEN INPUT INPUT-FILE
                PERFORM READ-PROCEDURE UNTIL WS-EOF = 'Y'
                CLOSE INPUT-FILE
                STOP RUN.

        READ-PROCEDURE.
            MOVE SPACES TO WS-SEPARATED.
            READ INPUT-FILE INTO INPUT-RECORD
                    AT END MOVE 'Y' TO WS-EOF
                NOT AT END PERFORM BUILD-PROCEDURE
            END-READ.

        BUILD-PROCEDURE.   
            UNSTRING INPUT-RECORD DELIMITED BY ','
                 INTO WS-SDATE WS-STIME WS-CATEGORY WS-ITEM-NAME 
                    WS-QUANTITY WS-SDETAIL WS-SKU WS-DISCOUNT WS-PRICE
                    WS-TRANS-ID WS-SUNIT WS-GTIN WS-COST
                    ON OVERFLOW MOVE 'Y' TO WS-OFLOW
            END-UNSTRING.
            PERFORM PROCESS-PROCEDURE.
            MOVE SPACES TO WS-TMP-DATA.
            STRING  WS-DISCOUNT
                    SPACE
                    WS-SDATE DELIMITED BY SIZE
                    SPACE
                    WS-GTIN DELIMITED BY SIZE
                    SPACE
                    WS-SKU DELIMITED BY SIZE
                    SPACE
                    'PRC ' WS-CONVERTED-SPRICE
                    SPACE
                    'DSC ' WS-CONVERTED-DISCOUNT
                    SPACE
                    'OPR ' WS-CONVERTED-OPRICE
                    SPACE 
                    'QTY ' WS-QUANTITY
                    SPACE
                    'TOT ' WS-CONVERTED-SALE
                    SPACE
                    'CST ' WS-CONVERTED-COST
                    SPACE 
                    'PFT ' WS-CONVERTED-PROFIT
                    SPACE
                    WS-ITEM-NAME DELIMITED BY SIZE
                    SPACE
                    WS-SDETAIL DELIMITED BY SIZE
                    INTO WS-TMP-DATA
            END-STRING.

            IF WS-OFLOW = 'N'
                PERFORM DISPLAY-PROCEDURE
            ELSE
                DISPLAY 'PROGRAM OVERFLOW'
                DISPLAY INPUT-RECORD
                CLOSE INPUT-FILE
                STOP RUN
            END-IF.

        DISPLAY-PROCEDURE.
            MOVE 'N' TO WS-OFLOW.
            DISPLAY WS-TMP-DATA.

        PROCESS-PROCEDURE.
      *  WS-PRICE IS TOTAL SALE OF ALL ITEMS MINUS DISCOUNT

      *  GET PROFIT GIVEN DISCOUNT AND TOTAL AMOUNT SOLD     
        IF WS-GTIN = '1XX'
            DISPLAY 'PRICE: ' WS-PRICE
            DISPLAY 'TOTAL COST: ' WS-TOTAL-COST
            MULTIPLY WS-COST BY 2
                GIVING WS-COST
        END-IF 

            MULTIPLY WS-COST BY WS-QUANTITY
                GIVING WS-TOTAL-COST.
            SUBTRACT WS-TOTAL-COST FROM WS-PRICE
                GIVING WS-PROFIT.

      *  GET BASE PRICE OF ITEM 
            IF WS-QUANTITY > 0
                DIVIDE WS-PRICE BY WS-QUANTITY
                    GIVING WS-PRICE
            END-IF

            ADD WS-DISCOUNT TO WS-PRICE
                GIVING WS-ORIG-PRICE.

      *  GET TOTAL SALE AMOUNT WITH DISCOUNT              
            MULTIPLY WS-PRICE BY WS-QUANTITY
                GIVING WS-TOTAL-SALE.
            
      *  MOVE ALL NUMERIC TYPES INTO PRINTABLE DOT FORMAT      
            MOVE WS-DISCOUNT TO WS-CONVERTED-DISCOUNT.
            MOVE WS-PRICE TO WS-CONVERTED-SPRICE.
            MOVE WS-ORIG-PRICE TO WS-CONVERTED-OPRICE.
            MOVE WS-COST TO WS-CONVERTED-COST.
            MOVE WS-TOTAL-SALE TO WS-CONVERTED-SALE.
            MOVE WS-PROFIT TO WS-CONVERTED-PROFIT. 

        

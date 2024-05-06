      ******************************************************************
      *  ###                         ##
      *   ##
      *   ##      ####    ##  ##    ###      ####     #####    ####
      *   ##     ##  ##   ##  ##     ##     ##  ##   ##           ##
      *   ##     ######   ##  ##     ##     ##  ##    #####    #####
      *   ##     ##        ####      ##     ##  ##        ##  ##  ##
      *  ####     #####     ##      ####     ####    ######    #####
      *
      ****************************************************************** 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. cpygen.
       AUTHOR.  AlexEtRemi.

      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-INPUT ASSIGN 
           TO "COBOL_FICHIER_MODELE_POUR_CLAUSE_COPY_Exercice.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-INPUT.

           SELECT F-OUTPUT ASSIGN TO "output.cpy"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS FS-OUTPUT.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************

       FILE SECTION.
       FD  F-INPUT
           RECORD CONTAINS 5 TO 1000 CHARACTERS
           RECORDING MODE IS V.
       01  R-INPUT PIC X(215).

       FD  F-OUTPUT
           RECORD CONTAINS 80 CHARACTERS
           RECORDING MODE IS F.
       01  R-OUTPUT PIC X(80).

      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************

       01  FS-INPUT PIC X(02).
           88 FS-INPUT-OK VALUE "00".
           88 FS-INPUT-EOF VALUE "10".

       01  FS-OUTPUT PIC X(02).
           88 FS-OUTPUT-OK VALUE "00".

       01  WS-GROUPE.
           03 WS-COUNTER PIC 9(04) VALUE 1.
           03 WS-ARRAY OCCURS 1 TO 1000 TIMES
                     DEPENDING ON WS-COUNTER
                     INDEXED BY GROUPE-IDX.
               05 WS-VALUE PIC X(20).

       01  VAR                  PIC 9(02) VALUE 1.
       01  WS-INDEX             PIC 9(03).
       01  WS-INDEX-2           PIC 9(03) VALUE 1.
       01 WS-WORD-LENGTH     PIC 9(3).
       01 WS-SPACE-LENGTH    PIC 9(3).
       01 WS-WORD-COUNT      PIC 9(3) VALUE 0.
       01 WS-SPACE-COUNT     PIC 9(3) VALUE 0.

      ****************************************************************** 
       PROCEDURE DIVISION.
      ******************************************************************      
      
       START-MAIN.
           PERFORM START-R-IP THRU END-R-IP.
       END-MAIN.
           STOP RUN.

      ******************************************************************
       START-R-IP.
           OPEN INPUT F-INPUT
                OUTPUT F-OUTPUT.
                CLOSE  F-OUTPUT.
           OPEN EXTEND F-OUTPUT.
              
              
              
              SET VAR TO 0.
           IF FS-INPUT EQUAL "00"
              SET FS-INPUT-OK TO TRUE
                    MOVE "       01 GROUPE." TO R-OUTPUT
                    WRITE R-OUTPUT
              PERFORM UNTIL FS-INPUT-EOF
                 READ F-INPUT 
                 AT END 
                    SET FS-INPUT-EOF TO TRUE
                 NOT AT END 

                    PERFORM START-W-OP THRU END-W-OP
                    
                  END-READ
              END-PERFORM
           ELSE
              DISPLAY "ERREUR :" SPACE FS-INPUT
           END-IF.
           CLOSE F-INPUT.
           CLOSE  F-OUTPUT.
       END-R-IP.
           EXIT.

      *-----------------------------------------------------------------
       START-W-OP.

            PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL
             WS-INDEX > LENGTH OF R-INPUT
            SET VAR TO 0
               IF R-INPUT(WS-INDEX:1) = SPACE
                 PERFORM VARYING WS-INDEX 
                    FROM WS-INDEX BY 1 UNTIL 
                    R-INPUT(WS-INDEX:1) NOT EQUAL SPACE
                    ADD 1 TO VAR
                    END-PERFORM                    
                    STRING "           03  FILLER PIC X(", 
                    VAR, ")." 
                    DELIMITED BY SIZE
                    INTO R-OUTPUT
                    WRITE R-OUTPUT
                    INITIALIZE   R-OUTPUT
               ELSE
                    MOVE 0 TO VAR
                    PERFORM VARYING WS-INDEX 
                    FROM WS-INDEX BY 1 UNTIL 
                    R-INPUT(WS-INDEX:1) = SPACE
                    OR WS-INDEX > LENGTH OF R-INPUT
                    ADD 1 TO VAR
            END-PERFORM
                    STRING "           03  FILLER PIC X(", 
                    VAR, ") VALUE ", R-INPUT(WS-INDEX-2:VAR)  , "." 
                    DELIMITED BY SIZE
                    INTO R-OUTPUT
                    WRITE R-OUTPUT
                    INITIALIZE   R-OUTPUT
               END-IF
                    SUBTRACT 1 FROM WS-INDEX
            ADD var to WS-INDEX-2           
            DISPLAY VAR
           END-PERFORM.

.
       END-W-OP.
           EXIT.
      *-----------------------------------------------------------------

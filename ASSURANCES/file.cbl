       IDENTIFICATION DIVISION.
               PROGRAM-ID. filetest.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA
           ALPHABET ASCII_STR IS STANDARD-2.
               
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REC-ASSU ASSIGN TO 'assurances.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-ASSU-STATUS.

           SELECT RAPPORT ASSIGN TO 'rapport-assurances.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-ASSU-STATUS.

           SELECT REC-ASSUPART1 ASSIGN TO 'assurances-part1.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-ASSU-STATUS1.

           SELECT REC-ASSUPART2 ASSIGN TO 'assurances-part2.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-ASSU-STATUS2.

       DATA DIVISION.
       FILE SECTION.
       FD REC-ASSU 
           RECORD CONTAINS 125 CHARACTERS
           RECORDING MODE IS F.

       01 FILE-REC-ASSU.           
        02 FIELD-REC-ASSU        PIC X(125).

       FD RAPPORT
           LABEL RECORDS ARE STANDARD
           RECORDING MODE IS V.    

       01  RAPPORT-ENTRY     PIC X(125).

       FD REC-ASSUPART1
           RECORD CONTAINS 125 CHARACTERS
           RECORDING MODE IS F.

       01 FILE-REC-ASSUPART1.           
        02 FIELD-REC-ASSUPART1        PIC X(125).

       FD REC-ASSUPART2
           RECORD CONTAINS 125 CHARACTERS
           RECORDING MODE IS F.

       01 FILE-REC-ASSUPART2.           
        02 FIELD-REC-ASSUPART2        PIC X(125).


       WORKING-STORAGE SECTION.
    
       01 REC-ASSU-STATUS      PIC X(2).
       01 REC-ASSU-STATUS1     PIC X(2).
       01 REC-ASSU-STATUS2     PIC X(2).
       01 TIMING               PIC 9(3) VALUE 1.
       01 TIMINGP1             PIC 9(2) VALUE 1.
       01 TIMINGP2             PIC 9(2) VALUE 1.
       01 SOMMEEURO            PIC 9(10) VALUE 0.

       01  DATATABLE.
           02 ASSURANCE  OCCURS 101 TIMES.
            03 WS-ID         PIC A(8) VALUE SPACE.
            03 FILLER        PIC X. 
            03 WS-CONTRAT    PIC X(14) VALUE SPACE.
            03 FILLER        PIC X. 
            03 WS-IRP        PIC X(14) VALUE SPACE.
            03 FILLER        PIC X. 
            03 WS-ENTREPRISE PIC A(41)  VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-ETAT       PIC A(8)  VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-DATA       PIC X(8) VALUE SPACE.
            03 FILLER        PIC X . 
            03 WS-TRENTE     PIC A(8) VALUE SPACE.    
            03 FILLER        PIC X . 
            03 WS-PRIX       PIC 9(9)  VALUE 0.  
            03 FILLER        PIC X . 
            03 EUROS        PIC X(3) VALUE "E".     
                               
       01 TETE      PIC X(125) VALUE
           "                                         RAPPORT ASSURANCE".
       01 RAPPORT-ASSURANCE.
           02 CORP OCCURS 2 TIMES.
               03 DATA-ASSU PIC X(125).
       01 TETEP1      PIC X(125) VALUE
           "                                            RAPPORT  PART1".
       01 TETEP2      PIC X(125) VALUE
           "                                             RAPPORT PART2".


       PROCEDURE DIVISION. 

           OPEN INPUT REC-ASSU.
           SET TIMING TO 1.
            PERFORM UNTIL TIMING = 29
               READ REC-ASSU           
                   NOT AT END
                       MOVE FIELD-REC-ASSU TO ASSURANCE(TIMING)
                       INSPECT ASSURANCE(TIMING) 
                       REPLACING all '*' by '|'
                       DISPLAY FILE-REC-ASSU
                       ADD 1 TO TIMING
               END-READ  
           END-PERFORM.
           CLOSE REC-ASSU.
  
           DISPLAY SPACE.
           DISPLAY SPACE.                      
           DISPLAY "**************************************************".
           DISPLAY "*             UN MESSAGE EXPLICITE               *".
           DISPLAY "**************************************************".
           DISPLAY SPACE.
           DISPLAY "**************************************************".
           DISPLAY  "ID :" WS-ID(3).
           DISPLAY  "TYPE DE CONTRAT :" WS-CONTRAT(3).
           DISPLAY  "IRP MODE :" WS-IRP(3).
           DISPLAY  "ENTREPRISE :" WS-ENTREPRISE(3).
           DISPLAY  "ETAT :" WS-ETAT(3).
           DISPLAY  "DATA :" WS-DATA(3).
           DISPLAY  "TROYE :" WS-TRENTE(3).
           DISPLAY  "PRIX :" WS-PRIX(3).
           DISPLAY  "MONEY :" EUROS(3).
           DISPLAY "**************************************************".
           DISPLAY  "ID :"      WS-ID(7).
           DISPLAY  "TYPE DE CONTRAT :" WS-CONTRAT(7).
           DISPLAY  "IRP MODE :" WS-IRP(7).
           DISPLAY  "ENTREPRISE :" WS-ENTREPRISE(7).
           DISPLAY  "ETAT :" WS-ETAT(7).
           DISPLAY  "DATA :" WS-DATA(7).
           DISPLAY  "TROYE :" WS-TRENTE(7).
           DISPLAY  "PRIX :" WS-PRIX(7).
           DISPLAY  "MONEY :" EUROS(7).
           DISPLAY "**************************************************".
           DISPLAY SPACE.
           DISPLAY "STATUS CODE =" REC-ASSU-STATUS.

           OPEN INPUT REC-ASSUPART1.
           SET TIMING TO 30.  
           PERFORM UNTIL TIMING = 66
               READ REC-ASSUPART1 
                   AT END
                       EXIT PERFORM                         
                   NOT AT END
                       MOVE FIELD-REC-ASSUPART1 TO ASSURANCE(TIMING)
                       INSPECT ASSURANCE(TIMING) 
                       REPLACING all '*' by '|'
                       DISPLAY  TIMING
                       ADD 1 TO TIMING
               END-READ  
           END-PERFORM.
           CLOSE REC-ASSUPART1.

           OPEN INPUT REC-ASSUPART2.
           SET TIMING TO 66.           
           PERFORM UNTIL TIMING = 102
               READ REC-ASSUPART2
                  AT END
                       EXIT PERFORM           
                   NOT AT END
                       MOVE FIELD-REC-ASSUPART2 TO ASSURANCE(TIMING)
                       INSPECT ASSURANCE(TIMING) 
                       REPLACING all '*' by '|'
                       DISPLAY  TIMING
                       ADD 1 TO TIMING
               END-READ  
           END-PERFORM.
           CLOSE REC-ASSUPART2.
           
           OPEN OUTPUT RAPPORT.
           CLOSE RAPPORT.
           OPEN EXTEND RAPPORT.

             MOVE ASSURANCE(3) TO CORP(1).
             MOVE ASSURANCE(7) TO CORP(2).
             MOVE ALL '_' TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY. 
             MOVE TETE TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY.              
             MOVE ALL '-' TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY. 
             MOVE CORP(1) TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY. 
             MOVE CORP(2) TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY. 
             MOVE ALL '-' TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY. 
      

           MOVE ALL '_' TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY. 
             MOVE TETEP1 TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY.              
             MOVE ALL '-' TO RAPPORT-ENTRY.
             WRITE RAPPORT-ENTRY. 
           

           SET TIMING TO 30.
           PERFORM UNTIL TIMING = 65
                      MOVE ASSURANCE(TIMING) TO RAPPORT-ENTRY
                      WRITE RAPPORT-ENTRY
                      DISPLAY  TIMING 
                      ADD 1 TO TIMING                     
           END-PERFORM.
           
           MOVE ALL '-' TO RAPPORT-ENTRY.
           WRITE RAPPORT-ENTRY. 

           MOVE ALL '_' TO RAPPORT-ENTRY.
           WRITE RAPPORT-ENTRY. 
           MOVE TETEP2 TO RAPPORT-ENTRY.
           WRITE RAPPORT-ENTRY.              
           MOVE ALL '-' TO RAPPORT-ENTRY.
           WRITE RAPPORT-ENTRY. 

           SET TIMING TO 65.     
           PERFORM UNTIL TIMING = 102
                 MOVE ASSURANCE(TIMING) TO RAPPORT-ENTRY
                 WRITE RAPPORT-ENTRY
                 DISPLAY  TIMING 
                 ADD 1 TO TIMING
           END-PERFORM.

           MOVE ALL '-' TO RAPPORT-ENTRY.
           WRITE RAPPORT-ENTRY. 

           SET TIMING TO 1
           PERFORM UNTIL TIMING =30
           ADD WS-PRIX(TIMING) TO SOMMEEURO
           ADD 1 TO TIMING
           END-PERFORM.

           MOVE "Somme des montants :" TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY.
           MOVE SOMMEEURO TO RAPPORT-ENTRY
           WRITE RAPPORT-ENTRY. 

           CLOSE  RAPPORT.

           STOP RUN.


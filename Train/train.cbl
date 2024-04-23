       IDENTIFICATION DIVISION.
               PROGRAM-ID. train.
               AUTHOR. AlexEnCode.
      ******************************************************************
       ENVIRONMENT DIVISION.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-TRAIN1 ASSIGN TO 'train1.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-TRAIN1-STATUS.

           SELECT F-TRAIN2 ASSIGN TO 'Train2.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.

           SELECT F-TRAIN3 ASSIGN TO 'Train3.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-TRAIN3-STATUS.

      ******************************************************************
       DATA DIVISION.
      ******************************************************************
       FILE SECTION.
      ****************************************************************** 
       FD  F-TRAIN1
	 	    RECORD CONTAINS 38 CHARACTERS
            RECORDING MODE IS F.

      *  FD  F-TRAIN1	  
      *      RECORD IS VARYING IN SIZE FROM 27 TO 37 CHARACACTERS   
      *      RECORDING MODE IS V.
           
       COPY 'train1-fdescription.txt'.       

       FD  F-TRAIN2
	 	    RECORD CONTAINS 38 CHARACTERS
            RECORDING MODE IS F.

        01 TRAIN-PLANNING2.
            03 RECORD-TYPE2       PIC XXX.
            88 TGV2     VALUE 'TGV'.
            88 CORAIL2  VALUE 'COR'.
            88 TER2     VALUE 'TER'.
            03 STATION-DEPART2    PIC X(18).
            03 TRAIN-TIME2.
               05 TRAIN-TIME-HH2  PIC 99.
               05 TRAIN-TIME-MM2  PIC 99.
            03 TRAIN-NBRE-HEURES2 PIC 99.
            03 TRAIN-HALT-FLAG2   PIC X OCCURS 10 TIMES.
            88 TRAIN-STOPS-HERE2 VALUE 'H'.
            88 TRAIN-SERVICE2    VALUE 'S'.
            88 TRAIN-FRETE2      VALUE 'F'.  

       FD  F-TRAIN3
        RECORDING MODE IS V.
      *     RECORD CONTAINS 27 TO 37 CHARACTERS 
      *     ON FS-TRAIN-PLANNING.

       01 FS-TRAIN-PLANNING.
         02 FS-TRAIN-RECORD OCCURS 46 TIMES.
          03 FS-RECORD-TYPE          PIC XXX.
          88 FS-TGV                  VALUE 'TGV'.
          88 FS-CORAIL               VALUE 'COR'.
          88 FS-TER                  VALUE 'TER'.
          03 FS-STATION-DEPART       PIC X(18).
          03 FS-TRAIN-TIME.
             05 FS-TRAIN-TIME-HH     PIC 99.
             05 FS-TRAIN-TIME-MM     PIC 99.
          03 FS-TRAIN-NBRE-HEURES    PIC 99.
          03 FS-TRAIN-HALT-FLAG      PIC X OCCURS 10 TIMES.
          88 FS-TRAIN-STOPS-HERE     VALUE 'H'.
          88 FS-TRAIN-SERVICE        VALUE 'S'.
          88 FS-TRAIN-FRETE          VALUE 'F'. 

       01  FS-LINE                   PIC X(132) VALUE SPACE.

       01  S-REPORTER.
       	   03 FILLER       PIC X VALUE '|' .
           03 FILLER        PIC x(19) VALUE "Destination       :".
           03 S-DESTINAT    PIC x(18) VALUE SPACE.
       	   03 FILLER       PIC X VALUE '|' .
           03 FILLER        PIC x(19) VALUE "Heure de depart   :".
           03 S-H-DEPART    PIC 99    VALUE ZERO.
       	   03 FILLER       PIC X VALUE '|' .
           03 FILLER        PIC x(19) VALUE "Heure d'arrivée   :".
           03 S-H-ARRIVE    PIC 99    VALUE ZERO.
       	   03 FILLER       PIC X VALUE '|' .
           03 FILLER        PIC x(19) VALUE "Durée du trajet   :".
           03 S-DUREE-T     PIC 99    VALUE ZERO.           
       	   03 FILLER       PIC X VALUE '|' .
           03 FILLER        PIC x(19) VALUE "Nombre d'arret    :".
           03 S-ARRET-NB    PIC x(10) VALUE SPACE.    
       	   03 FILLER       PIC X VALUE '|' .  


      ******************************************************************
       WORKING-STORAGE SECTION.   
      ******************************************************************

       01 TIMING               PIC 9(3)  VALUE 1.
       01 REC-TRAIN1-STATUS    PIC X(02) VALUE 'OK'.
       01 REC-TRAIN3-STATUS    PIC X(02) VALUE 'OK'.
       01 WS-RESULT            PIC 99    VALUE ZERO.
       01 WS-NOMBREDETRAJET    PIC 99    VALUE ZERO.
       01 WS-LINECOUNTER       PIC 9(2)  VALUE 0.
       01 WS-FINITO            PIC X(3)  VALUE 'NON'.
       01 LENGTHOFLINE         PIC 99    VALUE ZERO.
       01 H-COUNTER            PIC 99    VALUE ZERO.

       01 WS-TRAIN-PLANNING.
         02 WS-TRAIN-RECORD OCCURS 46 TIMES.
          03 WS-RECORD-TYPE         PIC XXX.
          88 WS-TGV                          VALUE 'TGV'.
          88 WS-CORAIL                       VALUE 'COR'.
          88 WS-TER                          VALUE 'TER'.
          03 WS-STATION-DEPART      PIC X(18).
          03 WS-TRAIN-TIME.
             05 WS-TRAIN-TIME-HH    PIC 99.
             05 WS-TRAIN-TIME-MM    PIC 99.
          03 WS-TRAIN-NBRE-HEURES   PIC 99.
          03 WS-TRAIN-STOPS         PIC X(10) VALUE SPACE.

       01  REPORTER.
        02 REPORT-CT OCCURS 46 TIMES.
       	   03 FILLER      PIC X(3)    VALUE ' | ' .
           03 FILLER      PIC x(19)   VALUE "Destination     :".
           03 DESTINAT    PIC x(18)   VALUE SPACE.
       	   03 FILLER      PIC X(3)    VALUE ' | ' .
           03 FILLER      PIC x(19)   VALUE "Durée du trajet :".
           03 H-DEPART    PIC 99      VALUE ZERO.
       	   03 FILLER      PIC X(3)    VALUE ' | ' .
           03 FILLER      PIC x(19)   VALUE "Heure de depart :".
           03 DUREE-T     PIC 99      VALUE ZERO. 
       	   03 FILLER      PIC X(3)    VALUE ' | ' .
           03 FILLER      PIC x(19)   VALUE "Heure d'arrivée :".
           03 H-ARRIVE    PIC 99      VALUE ZERO.       
       	   03 FILLER      PIC X       VALUE ' | ' .
           03 FILLER      PIC x(19)   VALUE "Nombre d'arret  :".
           03 ARRET-NB    PIC x(10)   VALUE SPACE.    
       	   03 FILLER      PIC X(3)    VALUE ' | ' .

      ******************************************************************
       PROCEDURE DIVISION. 
      ******************************************************************


      * LIGNE vient compter le nombre de ligne du fichier train1.dat
           PERFORM LIGNE.
      * SAVING-DATA enregistre de ligne du fichier train1.dat
           PERFORM SAVING-DATA.
      * SAVE-TRAIN2 copie fichier train1.dat          
           PERFORM SAVE-TRAIN2.
      * Ouverture du fichier d'écriture enregistre de
      * ligne du fichier train3.dat
           OPEN EXTEND F-TRAIN3.     
           
           PERFORM ENTETE.


           SET TIMING TO 1.
           PERFORM  UNTIL TIMING = WS-LINECOUNTER
           MOVE WS-TRAIN-RECORD(TIMING) TO FS-TRAIN-RECORD(TIMING)
           ADD 1 TO TIMING                    
           END-PERFORM.

           SET TIMING TO 1.
           PERFORM  UNTIL TIMING = WS-LINECOUNTER  
           DISPLAY FS-STATION-DEPART(TIMING)
           ADD 1 TO TIMING   
           END-PERFORM.

           SET TIMING TO 1.
           PERFORM  UNTIL TIMING = WS-LINECOUNTER  

           MOVE WS-STATION-DEPART(TIMING) TO DESTINAT(TIMING)
           MOVE WS-TRAIN-NBRE-HEURES(TIMING) TO DUREE-T(TIMING)
           PERFORM DEPARTURE-TIME
           MOVE WS-RESULT  TO H-DEPART(TIMING)           
           MOVE WS-TRAIN-TIME-HH(TIMING) TO H-ARRIVE(TIMING)
           PERFORM H-COUNTING
           MOVE H-COUNTER 
           TO ARRET-NB(TIMING)
           SET H-COUNTER TO ZERO 
           MOVE REPORT-CT(TIMING) To S-REPORTER
           WRITE S-REPORTER
           ADD 1 TO TIMING                    
           END-PERFORM.

           CLOSE F-TRAIN3.

           STOP RUN.   




      **************************************************************
      *                      PARAGRAPHES                           *
      **************************************************************
       
       DEPARTURE-TIME.
           SUBTRACT WS-TRAIN-TIME-HH(TIMING)
           FROM WS-TRAIN-NBRE-HEURES(TIMING) 
           GIVING WS-RESULT.
           IF WS-RESULT >= 24
            SUBTRACT 24 FROM WS-RESULT
           END-IF.
           EXIT.

       ENTETE.
           MOVE '  -----------------------------------------------' 
           TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE '|             BIENVENUE A LA SNCF                |' 
           TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE '  -----------------------------------------------' 
           TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE SPACE TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE SPACE TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE 'Nombre de trajet à venir :' TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE WS-LINECOUNTER TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE SPACE TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           WRITE FS-TRAIN-PLANNING.
           MOVE '  -----------------------------------------------' 
           TO FS-TRAIN-PLANNING.
           MOVE 'Trajet à venir :'TO FS-TRAIN-PLANNING.
           wRITE FS-TRAIN-PLANNING.
           MOVE ALL '-' TO FS-LINE.           
           wRITE FS-LINE.
           EXIT.


       LIGNE.     
           SET TIMING TO 1.
           OPEN INPUT F-TRAIN1.           
           PERFORM UNTIL WS-FINITO EQUAL 'OUI'
               READ F-TRAIN1
                   AT END
                       MOVE 'OUI' TO WS-FINITO
                       DISPLAY 'OUI'
                       EXIT PERFORM
                   NOT AT END
                       ADD 1 TO WS-LINECOUNTER
                       DISPLAY WS-LINECOUNTER
               END-READ
           END-PERFORM.
           CLOSE F-TRAIN1.
           EXIT.


       SAVING-DATA.
           OPEN INPUT F-TRAIN1.
           PERFORM UNTIL TIMING = WS-LINECOUNTER
               READ F-TRAIN1
                    NOT AT END
                    MOVE TRAIN-PLANNING TO WS-TRAIN-RECORD(TIMING)
                    DISPLAY TRAIN-PLANNING
                    ADD 1 TO TIMING               
               END-READ  01  REPORTER.
           CLOSE F-TRAIN1.             
           OPEN OUTPUT F-TRAIN3.
           CLOSE F-TRAIN3.
           EXIT.


       H-COUNTING.
           INSPECT WS-TRAIN-STOPS(TIMING) 
           TALLYING H-COUNTER FOR ALL 'H'.
           DISPLAY H-COUNTER
           EXIT.
           
       SAVE-TRAIN2.
           OPEN OUTPUT F-TRAIN2.
           CLOSE F-TRAIN2.           
           OPEN EXTEND F-TRAIN2.

           SET TIMING TO 1.
           PERFORM  UNTIL TIMING = WS-LINECOUNTER
           MOVE WS-TRAIN-RECORD(TIMING) TO TRAIN-PLANNING2
           WRITE TRAIN-PLANNING2
           ADD 1 TO TIMING                    
           END-PERFORM.

           CLOSE F-TRAIN2.
           EXIT.           
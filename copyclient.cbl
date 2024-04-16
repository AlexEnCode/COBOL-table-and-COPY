       IDENTIFICATION DIVISION.
               PROGRAM-ID. filetest.

       ENVIRONMENT DIVISION.
              
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT F-EMPLOYE ASSIGN TO 'fichierclient.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-CLIENT-STATUS.

           SELECT F-DEPT ASSIGN TO 'fr-liste-dept.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-DEPT-STATUS.

           SELECT F-CLISOR ASSIGN TO 'affectation.dat'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS REC-AFF-STATUS.

       DATA DIVISION.

       FILE SECTION.

      * Ajout des élément .cpy et changement de texte pour FCLIENT
       COPY 'FCLIENT.cpy' REPLACING ==:CLIENT:== BY ==EMPLOYE==.
       COPY 'FCLISORTIE.cpy'.
       COPY 'FDEPT.cpy'.       
		 
       WORKING-STORAGE SECTION.       
      
      * Les elements de l'en-tête n'ont pas besoin d'être dans FILE SEC 
       COPY 'FRENTETE.cpy'.

       01 REC-AFF-STATUS            PIC X(2) VALUE SPACE.
       01 REC-CLIENT-STATUS         PIC X(2) VALUE SPACE.
       01 REC-DEPT-STATUS           PIC X(2) VALUE SPACE.
       01 TIMING                    PIC 9(3) VALUE 1.
       01 TIMING2                   PIC 9(3) VALUE 1.       
       01 TOTAL-SALAIRE             PIC 9(5) VALUE 0.
       01 TOTAL-SALAIRE-MEP         PIC X(11) VALUE SPACE.
       01 VIRGULE                    PIC X(3) VALUE ",00".     
       01 EUROS                     PIC X(3) VALUE "€".

      * table d'enregistrement des departements
       01 WS-RDEPT.
         02 WS-T-RDEPT OCCURS 101 TIMES. 
           03 WS-RDEPT-ID      PIC X(03) VALUE SPACE.
 	       03 WS-RDEPT-DEP     PIC X(23) VALUE SPACE.
       	   03 WS-RDEPT-REGION  PIC X(26) VALUE SPACE.

      * table d'enregistrement des employés
       01 WS-REMPLOYE.
         02 WS-T-REMPLOYE OCCURS 21 TIMES.
	       03 WS-REMPLOYE-ID      PIC X(8) VALUE SPACE.
           03 WS-REMPLOYE-NOM     PIC X(20) VALUE SPACE.
           03 WS-REMPLOYE-PRENOM  PIC X(20) VALUE SPACE.
           03 WS-REMPLOYE-POSTE   PIC X(14) VALUE SPACE.
           03 WS-REMPLOYE-SALAIRE PIC X(7) VALUE SPACE.
           03 WS-REMPLOYE-AGENCE  PIC X(3) VALUE SPACE.
    
      * table d'enregistrement pour la sortie en txt
       01 WS-R-CLISOR.
        02 WS-T-CLISOR OCCURS 21 TIMES.
           03 WS-RCLISOR-ID      PIC X(10) VALUE SPACE.
       	   03 FILLER          PIC X.
       	   03 WS-RCLISOR-NOM     PIC X(20) VALUE SPACE.
       	   03 FILLER          PIC X.
       	   03 WS-RCLISOR-PRENOM  PIC X(20) VALUE SPACE.
       	   03 FILLER          PIC X.
       	   03 WS-RCLISOR-POSTE   PIC X(20) VALUE SPACE.
       	   03 FILLER          PIC X.
       	   03 WS-RCLISOR-SALAIRE.
       	       06 WS-RCLISOR-SALAIRE-V PIC 9(5) VALUE ZERO.
       	   03 FILLER          PIC X.
       	   03 WS-RCLISOR-AGENCE  PIC X(03) VALUE SPACE.
       	   03 FILLER          PIC X.
       	   03 WS-RCLISOR-DEPART  PIC X(23) VALUE SPACE.
       	   03 FILLER          PIC X.
       	   03 WS-RCLISOR-REGION  PIC X(26) VALUE SPACE.

       PROCEDURE DIVISION.        

      * Perso, je set toujours mes timers juste avant de les utlisés a 1
           SET TIMING TO 1.

      * boucle de sauvegarde du txt dans mon tableau departement 
           OPEN INPUT F-DEPT.
           PERFORM UNTIL TIMING = 101
                   READ F-DEPT
                    NOT AT END
                    MOVE RDEPT TO WS-T-RDEPT(TIMING)
                     ADD 1 TO TIMING               
                   END-READ
           END-PERFORM.
           CLOSE F-DEPT.

           SET TIMING TO 1.

      * boucle de sauvegarde du txt dans mon tableau employe 
           OPEN INPUT F-EMPLOYE.
           PERFORM UNTIL TIMING = 21
                   READ F-EMPLOYE 
                       NOT AT END
                         MOVE REMPLOYE TO WS-T-REMPLOYE(TIMING)
                         ADD 1 TO TIMING
                   END-READ
           END-PERFORM.
           CLOSE F-EMPLOYE.

      * Debut de la phase d'écriture 
           OPEN OUTPUT F-CLISOR.
           CLOSE F-CLISOR.
           OPEN EXTEND F-CLISOR.

      * Ecriture de l'en-tête            
           MOVE R-ENTETE TO R-CLISOR.
           WRITE R-CLISOR.

      * Set des timers pour la double boucle
           SET TIMING2 TO 1.
           SET TIMING TO 1.

      * La première partie de la boucle enregistre les elements
      * du tableau employé du tableau de sortie
           PERFORM  UNTIL TIMING = 21
           MOVE  WS-REMPLOYE-ID(TIMING) TO WS-RCLISOR-ID(TIMING)
           MOVE  WS-REMPLOYE-NOM(TIMING) TO WS-RCLISOR-NOM(TIMING)
           MOVE  WS-REMPLOYE-PRENOM(TIMING) TO WS-RCLISOR-PRENOM(TIMING)
           MOVE  WS-REMPLOYE-POSTE(TIMING) TO WS-RCLISOR-POSTE(TIMING)
           MOVE  WS-REMPLOYE-SALAIRE(TIMING) 
           TO WS-RCLISOR-SALAIRE(TIMING)
           MOVE  WS-REMPLOYE-AGENCE(TIMING) 
           TO WS-RCLISOR-AGENCE(TIMING)
            DISPLAY  WS-T-REMPLOYE(TIMING)
      
      *La boucle dans la boucle vient injecter les données se trouvant
      *dans le tableau de departement pour faire la jointure      
           PERFORM UNTIL TIMING2 = 101          
           IF WS-REMPLOYE-AGENCE(TIMING) = WS-RDEPT-ID(TIMING2) 
             MOVE WS-RDEPT-REGION(TIMING2) 
             TO WS-RCLISOR-REGION(TIMING)
             MOVE WS-RDEPT-DEP(TIMING2) 
             TO WS-RCLISOR-DEPART(TIMING)
             SET TIMING2 TO 101
           ELSE
            ADD 1 TO TIMING2    
           END-IF
           END-PERFORM
           ADD 1 TO TIMING
           SET TIMING2 TO 1                     
           END-PERFORM.

      * Boucle d'écriture dans le tableau de sortie 
           SET TIMING TO 1.
           PERFORM  UNTIL TIMING = 21
           MOVE WS-T-CLISOR(TIMING) TO R-CLISOR   
           WRITE R-CLISOR
           ADD 1 TO TIMING                    
           END-PERFORM.
           
           SET TIMING TO 1.
      
      * Boucle pour calculer la somme des salaires         
           MOVE 0 TO TOTAL-SALAIRE.
           PERFORM  UNTIL TIMING = 21
               MOVE WS-REMPLOYE-SALAIRE(TIMING) 
               TO WS-RCLISOR-SALAIRE-V(TIMING)
               ADD WS-RCLISOR-SALAIRE-V(TIMING) 
               TO TOTAL-SALAIRE
               ADD 1 TO TIMING
           END-PERFORM.
           
      * Mise en page des salaires, puis l'écriture
           STRING TOTAL-SALAIRE VIRGULE EUROS DELIMITED BY 
           SIZE INTO TOTAL-SALAIRE-MEP.

           MOVE ALL '_' TO R-CLISOR.
           WRITE R-CLISOR. 

           MOVE "La somme des salaires de tous les employés est de : " 
           TO R-CLISOR.
           WRITE R-CLISOR.  

           MOVE TOTAL-SALAIRE-MEP TO R-CLISOR.
           WRITE R-CLISOR. 

           MOVE ALL '_' TO R-CLISOR.
           WRITE R-CLISOR. 

           CLOSE F-CLISOR.

      * Voilà! finito pipo!
           STOP RUN.   

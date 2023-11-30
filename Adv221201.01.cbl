       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2022-12-01.
       AUTHOR. MARC BRASSART.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE1 ASSIGN 
            TO "C:\Users\MarcBrassart\Documents\AdventOfCode\2022\files\
      -        "d1.input.txt"
            FILE STATUS IS FILE1-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE1.
         01 INPUTRECORD1 PIC X(9).
       WORKING-STORAGE SECTION.
       
      * FILE STATUS 
         01 FILE1-STATUS PIC 9(02).
            88 FILE1-STATUS-OK  VALUE 00.
            88 FILE1-STATUS-EOF VALUE 10.
            
      * WORKING VARIABLE
         01 CURRENT-CALORIES PIC 9(09).
         01 SUM-CALORIES PIC 9(09).
         01 MAX-CALORIES PIC 9(09).

       PROCEDURE DIVISION.
       
           PERFORM MAIN-PROCESSING
           STOP RUN
           .
       
       MAIN-PROCESSING.
           PERFORM OPEN-FILE1
           
           MOVE ZERO TO SUM-CALORIES
           MOVE ZERO TO MAX-CALORIES
           
           PERFORM READ-FILE1
           
           PERFORM UNTIL FILE1-STATUS-EOF 
                  OR NOT FILE1-STATUS-OK
              
              IF INPUTRECORD1 = SPACES
                  IF SUM-CALORIES > MAX-CALORIES
                       MOVE SUM-CALORIES TO MAX-CALORIES
                  END-IF
                  MOVE ZERO TO SUM-CALORIES
              ELSE
                 MOVE INPUTRECORD1 TO CURRENT-CALORIES
                 ADD CURRENT-CALORIES TO SUM-CALORIES
              END-IF
              PERFORM READ-FILE1
              
           END-PERFORM
           
           DISPLAY 'MAX CALORIES CARRIED : ' MAX-CALORIES
           
           PERFORM CLOSE-FILE1            
           .

       OPEN-FILE1.
           OPEN INPUT INPUTFILE1
           IF NOT FILE1-STATUS-OK
               DISPLAY 'OPEN ERROR ON FILE 1'            
               DISPLAY 'FILE STATUS : ' FILE1-STATUS 
               STOP RUN
           END-IF
           .
           
       READ-FILE1.
           READ INPUTFILE1
               AT END 
                   CONTINUE
                NOT AT END 
                   IF NOT FILE1-STATUS-OK
                       DISPLAY 'READ ERROR ON FILE 1'            
                       DISPLAY 'FILE STATUS : ' FILE1-STATUS   
                       STOP RUN
                   END-IF
           END-READ
           .
           
       CLOSE-FILE1.
           CLOSE INPUTFILE1
           IF NOT FILE1-STATUS-OK
               DISPLAY 'CLOSE ERROR ON FILE 1'            
               DISPLAY 'FILE STATUS : ' FILE1-STATUS  
               STOP RUN
           END-IF
           .

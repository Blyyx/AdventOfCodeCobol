       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2023-12-02.
       AUTHOR. MARC BRASSART.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE1 ASSIGN
            TO "C:\Users\MarcBrassart\Documents\AdventOfCode\2023\files\
      -        "d2.input.txt"
            FILE STATUS IS FILE1-STATUS
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
         FD INPUTFILE1.
         01 INPUTRECORD1 PIC X(200).
       WORKING-STORAGE SECTION.

      * FILE STATUS
         01 FILE1-STATUS PIC 9(02).
            88 FILE1-STATUS-OK  VALUE 00.
            88 FILE1-STATUS-EOF VALUE 10.

      * WORKING VARIABLE
         01 SET-STATE                      PIC X.
            88 CORRECT-SET                 VALUE 'O'.
            88 INCORRECT-SET               VALUE 'N'.

         01 GAME-NUMBER                    PIC 9(03) COMP-5.
         01 SUM-GAME-NUMBER                PIC 9(05) COMP-5.

         01 POWER-SET                      PIC 9(08) COMP-5.
         01 SUM-POWER-SET                  PIC 9(08) COMP-5.

         01 CARAC-TO-SKIP                  PIC 9(02) COMP-5.

         01 SET-NBR                        PIC 9(02) COMP-5.
         01 SET-TAB-NBR                    PIC 9(03) COMP-5.
         01 SET-TAB.
            05 LSET-TAB                    OCCURS 100.
               10 SET-VALUE                PIC X(150).

         01 DRAW-NBR                       PIC 9(02) COMP-5.
         01 DRAW-TAB-NBR                   PIC 9(03) COMP-5.
         01 DRAW-TAB.
            05 LDRAW-TAB                   OCCURS 100.
               10 DRAW-VALUE               PIC X(40).

         01 STARTING-POINT                 PIC 9(5) COMP-3.

         01 CARAC-BFO-GREEN                PIC 9(02) COMP-3.
         01 CARAC-BFO-BLUE                 PIC 9(02) COMP-3.
         01 CARAC-BFO-RED                  PIC 9(02) COMP-3.

         01 CUBE-NUMBER                    PIC 9(03).

         01 GREEN-CUBE-NEEDED              PIC 9(03) COMP-3.
         01 BLUE-CUBE-NEEDED               PIC 9(03) COMP-3.
         01 RED-CUBE-NEEDED                PIC 9(03) COMP-3.

       PROCEDURE DIVISION.

           PERFORM MAIN-PROCESSING
           STOP RUN
           .

       MAIN-PROCESSING.
           PERFORM OPEN-FILE1

           MOVE ZERO TO GAME-NUMBER

           PERFORM READ-FILE1

           PERFORM UNTIL FILE1-STATUS-EOF
                  OR NOT FILE1-STATUS-OK

               ADD 1               TO GAME-NUMBER
               SET CORRECT-SET     TO TRUE
               MOVE ZERO           TO GREEN-CUBE-NEEDED
                                      BLUE-CUBE-NEEDED
                                      RED-CUBE-NEEDED

               DISPLAY 'INPUTRECORD1 = ' INPUTRECORD1

               PERFORM SPLIT-SET
               PERFORM VARYING SET-NBR FROM 1 BY 1
                         UNTIL SET-NBR > SET-TAB-NBR
                   PERFORM SPLIT-DRAW
                   PERFORM CHECK-DRAW
               END-PERFORM

               IF CORRECT-SET
                  ADD GAME-NUMBER TO SUM-GAME-NUMBER
                  DISPLAY GAME-NUMBER ' CORRECT  !'
               ELSE
                  DISPLAY ' INCORRECT  !'
               END-IF

               COMPUTE POWER-SET =  GREEN-CUBE-NEEDED
                                  * BLUE-CUBE-NEEDED
                                  * RED-CUBE-NEEDED
               END-COMPUTE

               DISPLAY 'POWER SET = ' POWER-SET
               DISPLAY ' WITH ' GREEN-CUBE-NEEDED ' GREEN CUBES'
               DISPLAY '      ' BLUE-CUBE-NEEDED  ' BLUE CUBES'
               DISPLAY '      ' RED-CUBE-NEEDED   ' RED CUBES'

               ADD POWER-SET TO SUM-POWER-SET

              PERFORM READ-FILE1

           END-PERFORM

           DISPLAY SPACE
           DISPLAY '---------------------------------------------'
           DISPLAY ' RESULT'
           DISPLAY '---------------------------------------------'
           DISPLAY ' SUM CORRECT GAME NUMBER = ' SUM-GAME-NUMBER
           DISPLAY ' SUM POWER SET = ' SUM-POWER-SET

           PERFORM CLOSE-FILE1
           .

       SPLIT-SET.
           MOVE 0 TO CARAC-TO-SKIP
           INSPECT INPUTRECORD1 TALLYING CARAC-TO-SKIP
                                              FOR CHARACTERS BEFORE ':'
           ADD 2 TO CARAC-TO-SKIP

           INITIALIZE SET-NBR SET-TAB SET-TAB-NBR
           MOVE 1 TO STARTING-POINT

           INSPECT INPUTRECORD1 TALLYING SET-NBR FOR ALL ';'
           ADD 1 TO SET-NBR

           PERFORM SET-NBR TIMES
               ADD 1 TO SET-TAB-NBR

               UNSTRING INPUTRECORD1(CARAC-TO-SKIP:) DELIMITED BY ';'
                   INTO SET-VALUE(SET-TAB-NBR)
                   WITH POINTER STARTING-POINT
               END-UNSTRING

           END-PERFORM
           .

       SPLIT-DRAW.
           INITIALIZE DRAW-NBR DRAW-TAB DRAW-TAB-NBR
           MOVE 1 TO STARTING-POINT

           INSPECT SET-VALUE(SET-NBR) TALLYING DRAW-NBR FOR ALL ','
           ADD 1 TO DRAW-NBR

           PERFORM DRAW-NBR TIMES
               ADD 1 TO DRAW-TAB-NBR

               UNSTRING SET-VALUE(SET-NBR) DELIMITED BY ','
                   INTO DRAW-VALUE(DRAW-TAB-NBR)
                   WITH POINTER STARTING-POINT
               END-UNSTRING

           END-PERFORM
           .

       CHECK-DRAW.

           PERFORM VARYING DRAW-NBR FROM 1 BY 1
                     UNTIL DRAW-NBR > DRAW-TAB-NBR

               MOVE ZERO TO CUBE-NUMBER

               MOVE ZERO TO CARAC-BFO-GREEN CARAC-BFO-BLUE CARAC-BFO-RED
               INSPECT DRAW-VALUE(DRAW-NBR) TALLYING CARAC-BFO-GREEN
                                          FOR CHARACTERS BEFORE 'green'
               INSPECT DRAW-VALUE(DRAW-NBR) TALLYING CARAC-BFO-BLUE
                                          FOR CHARACTERS BEFORE 'blue'
               INSPECT DRAW-VALUE(DRAW-NBR) TALLYING CARAC-BFO-RED
                                          FOR CHARACTERS BEFORE 'red'

                EVALUATE TRUE

                   WHEN CARAC-BFO-GREEN < 40
                      MOVE DRAW-VALUE(DRAW-NBR)(2:CARAC-BFO-GREEN - 2)
                        TO CUBE-NUMBER
                      IF CUBE-NUMBER > 13
                         SET INCORRECT-SET TO TRUE
                         DISPLAY 'GREEN ERROR'
                      END-IF
                      IF CUBE-NUMBER > GREEN-CUBE-NEEDED
                         MOVE CUBE-NUMBER  TO GREEN-CUBE-NEEDED
                      END-IF

                   WHEN CARAC-BFO-BLUE < 40
                      MOVE DRAW-VALUE(DRAW-NBR)(2:CARAC-BFO-BLUE - 2)
                        TO CUBE-NUMBER
                      IF CUBE-NUMBER > 14
                         SET INCORRECT-SET TO TRUE
                         DISPLAY 'BLUE ERROR'
                      END-IF
                      IF CUBE-NUMBER > BLUE-CUBE-NEEDED
                         MOVE CUBE-NUMBER  TO BLUE-CUBE-NEEDED
                      END-IF

                   WHEN CARAC-BFO-RED < 40
                      MOVE DRAW-VALUE(DRAW-NBR)(2:CARAC-BFO-RED - 2)
                        TO CUBE-NUMBER
                      IF CUBE-NUMBER > 12
                         SET INCORRECT-SET TO TRUE
                         DISPLAY 'RED ERROR'
                      END-IF
                      IF CUBE-NUMBER > RED-CUBE-NEEDED
                         MOVE CUBE-NUMBER  TO RED-CUBE-NEEDED
                      END-IF

                END-EVALUATE

           END-PERFORM
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

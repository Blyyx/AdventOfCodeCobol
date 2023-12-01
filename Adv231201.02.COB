       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC-2022-12-01.
       AUTHOR. MARC BRASSART.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUTFILE1 ASSIGN
            TO "C:\Users\MarcBrassart\Documents\AdventOfCode\2023\files\
      -        "d1.input.txt"
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
         01 CALIBRATION  PIC 9(02).
         01 CALIBRATION-SUM PIC 9(09).

         01 i PIC 9(2).

         01 DIGIT          PIC 9.
            88 FIRST-DIGIT VALUE 1.
            88 LAST-DIGIT  VALUE 2.

         01 CARAC-TYPE      PIC 9.
            88 ISNUMBER     VALUE 1.
            88 ISNOTNUMBER  VALUE 2.

         01 CURRENT-NUMBER PIC 9.

       PROCEDURE DIVISION.

           PERFORM MAIN-PROCESSING
           STOP RUN
           .

       MAIN-PROCESSING.
           PERFORM OPEN-FILE1

           MOVE ZERO TO CALIBRATION-SUM

           PERFORM READ-FILE1

           PERFORM UNTIL FILE1-STATUS-EOF
                  OR NOT FILE1-STATUS-OK

              MOVE ZERO        TO CALIBRATION
              SET FIRST-DIGIT  TO TRUE
              PERFORM VARYING i FROM 1 BY 1
                        UNTIL i > 200
                           OR INPUTRECORD1(i:1) = SPACE
                 PERFORM CONTROL-CARACTER-TYPE

                 IF ISNUMBER
                    IF FIRST-DIGIT
                       MOVE CURRENT-NUMBER     TO CALIBRATION(1:1)
                       MOVE CURRENT-NUMBER     TO CALIBRATION(2:1)
                       SET LAST-DIGIT          TO TRUE
                    ELSE
                       MOVE CURRENT-NUMBER     TO CALIBRATION(2:1)
                    END-IF

                 END-IF

              END-PERFORM

              DISPLAY 'CALIBRATION OF ' INPUTRECORD1(1:i) ' IS '
              CALIBRATION

              ADD CALIBRATION TO CALIBRATION-SUM

              PERFORM READ-FILE1

           END-PERFORM

           DISPLAY 'SUM CALIBRATION VALUES : ' CALIBRATION-SUM

           PERFORM CLOSE-FILE1
           .

       CONTROL-CARACTER-TYPE.
           SET ISNOTNUMBER             TO TRUE

           IF INPUTRECORD1(i:1) IS NUMERIC
              SET ISNUMBER             TO TRUE
              MOVE INPUTRECORD1(i:1)   TO CURRENT-NUMBER
           ELSE
              MOVE ZERO                TO CURRENT-NUMBER
              EVALUATE TRUE
                 WHEN INPUTRECORD1(i:3) = 'one'
                    MOVE 1             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:3) = 'two'
                    MOVE 2             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:5) = 'three'
                    MOVE 3             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:4) = 'four'
                    MOVE 4             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:4) = 'five'
                    MOVE 5             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:3) = 'six'
                    MOVE 6             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:5) = 'seven'
                    MOVE 7             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:5) = 'eight'
                    MOVE 8             TO CURRENT-NUMBER
                 WHEN INPUTRECORD1(i:4) = 'nine'
                    MOVE 9             TO CURRENT-NUMBER
              END-EVALUATE

              IF CURRENT-NUMBER NOT = ZERO
                 SET ISNUMBER          TO TRUE
              END-IF
           END-IF
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

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
                 IF INPUTRECORD1(i:1) IS NUMERIC
                    IF FIRST-DIGIT
                       MOVE INPUTRECORD1(i:1) TO CALIBRATION(1:1)
                       MOVE INPUTRECORD1(i:1) TO CALIBRATION(2:1)
                       SET LAST-DIGIT         TO TRUE
                    ELSE
                       MOVE INPUTRECORD1(i:1) TO CALIBRATION(2:1)
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

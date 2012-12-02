  !***********************************************************************
  !    Copyright (C) 1995-
  !        Roy A. Walters, R. Falconer Henry
  !
  !        rawalters@shaw.ca
  !
  !    This file is part of TQGG, Triangle-Quadrilateral Grid Generation,
  !    a grid generation and editing program.
  !
  !    TQGG is free software; you can redistribute it and/or
  !    modify it under the terms of the GNU General Public
  !    License as published by the Free Software Foundation; either
  !    version 3 of the License, or (at your option) any later version.
  !
  !    This program is distributed in the hope that it will be useful,
  !    but WITHOUT ANY WARRANTY; without even the implied warranty of
  !    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  !    General Public License for more details.
  !
  !    You should have received a copy of the GNU General Public
  !    License along with this program; if not, write to the Free Software
  !    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
  !    USA, or see <http://www.gnu.org/licenses/>.
  !***********************************************************************

  !***********************************************************************
C     Module contains 3 sets of routines:
C
C     a) CSORT >> QCSORT >> CPART >> CSWAP
C              >> -------------------CSWAP
C
C     b) ISORT >> QISORT >> IPART >> ISWAP
C              >> -------------------ISWAP
C
C     c)  SORT >>  QSORT >>  PART >>  RSWAP, ISWAP
C              >> ------------------- RSWAP, ISWAP
C
C----------------------------------------------------------------------
C
C     SUBROUTINE CSORT - SORTS AN ARRAY OF CHARACTERS INTO ASCENDING OR
C        DESCENDING ORDER. QUICKSORT (SUBROUTINE QCSORT) IS USED 
C
C     WRITTEN BY KIN WU ,JUNE 1986
C     REVISED BY LYNDA WILLIAMS, SEPT 1986 TO USE CHARACTER ARRAYS
C
C     ARGUMENT LIST : ARR - GIVEN ARRAY TO BE SORTED.
C              NUM - NUMBER OF ENTRIES IN THE GIVEN ARRAY.
C              ASC - LOGICAL VARIABLE, TRUE FOR ASCENDING,
C                    FALSE FOR DESCENDING ORDER.
C              ERR - LOGICAL VARIBLE TO INDICATE AN ERROR OCCURS.
C
      SUBROUTINE CSORT(ARR,NUM,ASC,ERR)
C
      INTEGER NUM,I,J,K
      CHARACTER*8 ARR(NUM)
      LOGICAL ASC,ERR
C
C      INVALID VALUE OF NUM.
C
      ERR = .FALSE.
C
      IF (NUM .LE. 0) THEN
         ERR = .TRUE.
         PRINT *,'In CSORT:INVALID NUMBER OF ENTRIES IN THE ARRAY !'
         RETURN
      ENDIF
C
      IF (NUM .EQ. 1) THEN
         RETURN
      ENDIF
C
      CALL QCSORT(ARR,NUM)
      IF (.NOT. ASC) THEN
         J = NUM
         K = INT(NUM/2)
         DO 10 I = 1,K
            CALL CSWAP(ARR(I),ARR(J-I+1))
10       CONTINUE
      ENDIF
      END
C*****************************************************************
C     SUBROUTINE QCSORT - SORTS AN ARRAY OF NUMBER INTO ASCENDING OR
C     DESCENDING ORDER USING PARTITION-EXCHANGE (QUICK SORT) METHOD
C
C     ARGUMENT LIST : ARR - GIVEN ARRAY TO BE SORTED.
C              NUM - NUMBER OF ENTRIES IN THE GIVEN ARRAY.
C                    FALSE FOR DESCENDING ORDER.
C
C     LOCAL VARIABLES :
C        L/R - LEFT/RIGHT INDICES OF THE ARRAY TO BE PARTITIONED.
C        L1/R1,L2/R2 - LEFT/RIGHT INDICES OF THE TWO SUBARRYS
C                      AFTER PARTITIONED.
C        LST/RST - STACKS OF LEFT/RIGHT INDICES OF THE ARRAYS.
C        TOP - POINTER POINTS TO THE TOP OF THE STACKS(LST/RST).
C
      SUBROUTINE QCSORT(ARR,NUM)
C
      INTEGER NUM
      CHARACTER*8 ARR(NUM)
      INTEGER TOP,LST(20),RST(20)
      INTEGER L,R,L1,R1,L2,R2
C
C     INITIALIZE THE STACKS
C
      TOP = 1
      LST(TOP) = 1
      RST(TOP) = NUM
C
C     WHILE STACK IS NOT EMPTY, DO THE PARTITIONING.
C
100   IF (TOP .GT. 0) THEN
         L = LST(TOP)
         R = RST(TOP)
         TOP = TOP - 1
C
C     IF MORE THAN ONE ENTRY THEN CALL CPART.
C
         IF (L .LT. R) THEN
           CALL CPART(ARR,NUM,L,R,L1,R1,L2,R2)
C
C     STACK THE LEFT/RIGHT INDICES OF THE TWO SUBARRAYS,
C     STACK THE LONGER SUBARRAY'S INDICES FIRST.
C
           IF ((R1-L1) .GT. (R2-L2)) THEN
            TOP = TOP + 1
            LST(TOP) = L1
            RST(TOP) = R1
            TOP = TOP + 1
            LST(TOP) = L2
            RST(TOP) = R2
           ELSE
            TOP = TOP + 1
            LST(TOP) = L2
            RST(TOP) = R2
            TOP = TOP + 1
            LST(TOP) = L1
            RST(TOP) = R1
           ENDIF
C
         ENDIF
C
         GOTO 100
      ENDIF
C
      END
C
C
C     SUBROUTINE CPART - PERFORMS PARTITION ON THE GIVEN ARRAY
C                INTO TWO SUBARRAYS. THE FIRST ENTRY OF THE GIVEN ARRAY
C                 IS USED AS THE KEY. ALL THE ENTRIES IN THE LEFT
C                 SUBARRAY IS LESS THAN OR EQUAL TO THE KEY. ALL
C                 THE ENTRIES IN THE RIGHT SUBARRY IS GREATER
C                 THAN THE KEY.
C
C
      SUBROUTINE CPART(ARR,NUM,L,R,L1,R1,L2,R2)
C
      INTEGER NUM,L,R,L1,R1,L2,R2,I,J
C
      CHARACTER*8 KEY,ARR(NUM)
C
C     INITIALIZE THE SCAN.
C
      I = L
      J = R + 1
      KEY = ARR(L)
C
100   IF (I .LT. J) THEN
C
C     SCAN FROM THE RIGHT, LOOK FOR THE ENTRY LESS THAN OR EQUAL
C     TO THE KEY.
C
         J = J -1
300      IF (ARR(J) .GT. KEY) THEN
            J = J - 1
            GO TO 300
         ENDIF
C
C     SCAN FROM THE RIGHT, LOOK FOR THE ENTRY GREATER THAN OR
C     EQUAL TO THE KEY.
C
         I = I + 1
500      IF (ARR(I) .LT. KEY .AND. I .LT. J) THEN
            I = I + 1
            GO TO 500
         ENDIF
C
C     INTERCHANGE THE TWO ENTRIES IF THE SCAN HAVE NOT OVERLAPPED
C
         IF (I .LT. J) THEN
            CALL CSWAP(ARR(I),ARR(J))
         ENDIF
C
         GO TO 100
C
      ENDIF
C
C     PLACE THE KEY IN THE FINAL POSITION
C
      CALL CSWAP(ARR(L),ARR(J))
C
C     COMPUTES THE LEFT/RIGHT INDICES OF THE TWO SUBARRAYS.
C
      L1 = L
      R1 = J - 1
      L2 = J + 1
      R2 = R
      END
C
C ***************************************************************
C
C
C
C ***************************************************************
C ***************************************************************
C     SUBROUTINE ISORT - SORTS AN ARRAY OF INTEGERS INTO ASCENDING OR
C        DESCENDING ORDER. USING QUICKSORT (SUBROUTINE QISORT)
C
C     WRITTEN BY KIN WU ,JUNE 1986
C     REVISED BY LYNDA WILLIAMS, JULY 1986 TO FOR INTEGER ARRAYS
C
C     ARGUMENT LIST : ARR - GIVEN ARRAY TO BE SORTED.
C              NUM - NUMBER OF ENTRIES IN THE GIVEN ARRAY.
C              ASC - LOGICAL VARIABLE, TRUE FOR ASCENDING,
C                    FALSE FOR DESCENDING ORDER.
C              ERR - LOGICAL VARIBLE TO INDICATE AN ERROR OCCURS.
C
      SUBROUTINE ISORT(ARR,NUM,ASC,ERR)
C
      INTEGER NUM,I,J,K
      INTEGER ARR(NUM)
      LOGICAL ASC,ERR
C
C      INVALID VALUE OF NUM.
C
      ERR = .FALSE.
C
      IF (NUM .LE. 0) THEN
         ERR = .TRUE.
         PRINT *,'In ISORT:INVALID NUMBER OF ENTRIES IN THE ARRAY !'
         RETURN
      ENDIF
C
      IF (NUM .EQ. 1) THEN
         RETURN
      ENDIF
C
      CALL QISORT(ARR,NUM)
      IF (.NOT. ASC) THEN
         J = NUM
         K = INT(NUM/2)
         DO 10 I = 1,K
            CALL ISWAP(ARR(I),ARR(J-I+1))
10       CONTINUE
      ENDIF
      END
C*****************************************************************
C     SUBROUTINE QISORT - SORTS AN ARRAY OF NUMBER INTO ASCENDING OR
C     DESCENDING ORDER USING PARTITION-EXCHANGE (QUICK SORT) METHOD
C
C     ARGUMENT LIST : ARR - GIVEN ARRAY TO BE SORTED.
C              NUM - NUMBER OF ENTRIES IN THE GIVEN ARRAY.
C                    FALSE FOR DESCENDING ORDER.
C
C     LOCAL VARIBLES :
C        L/R - LEFT/RIGHT INDICES OF THE ARRAY TO BE PARTITIONED.
C        L1/R1,L2/R2 - LEFT/RIGHT INDICES OF THE TWO SUBARRYS
C                      AFTER PARTITIONED.
C        LST/RST - STACKS OF LEFT/RIGHT INDICES OF THE ARRAYS.
C        TOP - POINTER POINTS TO THE TOP OF THE STACKS(LST/RST).
C
      SUBROUTINE QISORT(ARR,NUM)
C
      INTEGER NUM
      INTEGER ARR(NUM)
      INTEGER TOP,LST(20),RST(20)
      INTEGER L,R,L1,R1,L2,R2
C
C     INITIALIZE THE STACKS
C
      TOP = 1
      LST(TOP) = 1
      RST(TOP) = NUM
C
C     WHILE STACK IS NOT EMPTY, DO THE PARTITIONING.
C
100   IF (TOP .GT. 0) THEN
         L = LST(TOP)
         R = RST(TOP)
         TOP = TOP - 1
C
C     IF MORE THAN ONE ENTRY THEN CALL IPART.
C
         IF (L .LT. R) THEN
           CALL IPART(ARR,NUM,L,R,L1,R1,L2,R2)
C
C     STACK THE LEFT/RIGHT INDICES OF THE TWO SUBARRAYS,
C     STACK THE LONGER SUBARRY'S INDICES FIRST.
C
           IF ((R1-L1) .GT. (R2-L2)) THEN
            TOP = TOP + 1
            LST(TOP) = L1
            RST(TOP) = R1
            TOP = TOP + 1
            LST(TOP) = L2
            RST(TOP) = R2
           ELSE
            TOP = TOP + 1
            LST(TOP) = L2
            RST(TOP) = R2
            TOP = TOP + 1
            LST(TOP) = L1
            RST(TOP) = R1
           ENDIF
C
         ENDIF
C
         GOTO 100
      ENDIF
C
      END
C
C
C     SUBROUTINE IPART - PERFORMS PARTITION ON THE GIVEN ARRAY
C                INTO TWO SUBARRAYS. THE FIRST ENTRY OF THE GIVEN ARRAY
C                 IS USED AS THE KEY. ALL THE ENTRIES IN THE LEFT
C                 SUBARRAY IS LESS THAN OR EQUAL TO THE KEY. ALL
C                 THE ENTRIES IN THE RIGHT SUBARRY IS GREATER
C                 THAN THE KEY.
C
C
      SUBROUTINE IPART(ARR,NUM,L,R,L1,R1,L2,R2)
C
      INTEGER NUM,L,R,L1,R1,L2,R2,I,J
C
      INTEGER KEY,ARR(NUM)
C
C     INITIALIZE THE SCAN.
C
      I = L
      J = R + 1
      KEY = ARR(L)
C
100   IF (I .LT. J) THEN
C
C     SCAN FROM THE RIGHT, LOOK FOR THE ENTRY LESS THAN OR EQUAL
C     TO THE KEY.
C
         J = J -1
300      IF (ARR(J) .GT. KEY) THEN
            J = J - 1
            GO TO 300
         ENDIF
C
C     SCAN FROM THE RIGHT, LOOK FOR THE ENTRY GREATER THAN OR
C     EQUAL TO THE KEY.
C
         I = I + 1
500      IF (ARR(I) .LT. KEY .AND. I .LT. J) THEN
            I = I + 1
            GO TO 500
         ENDIF
C
C     INTERCHANGE THE TWO ENTRIES IF THE SCAN HAVE NOT OVERLAPPED
C
         IF (I .LT. J) THEN
            CALL ISWAP(ARR(I),ARR(J))
         ENDIF
C
         GO TO 100
C
      ENDIF
C
C     PLACE THE KEY IN THE FINAL POSITION
C
      CALL ISWAP(ARR(L),ARR(J))
C
C     COMPUTES THE LEFT/RIGHT INDICES OF THE TWO SUBARRAYS.
C
      L1 = L
      R1 = J - 1
      L2 = J + 1
      R2 = R
      END
C
C ***************************************************************
C ***************************************************************
C
C
C
C ***************************************************************
C ***************************************************************
C     SUBROUTINE SORT - SORTS AN ARRAY OF REAL NUMBERS INTO 
C                       ASCENDING OR DESCENDING ORDER USING 
C                       QUICKSORT METHOD (SUBROUTINE QSORT)
C
C     WRITTEN BY KIN WU ,JUNE 1986
C     REVISED BY LYNDA WILLIAMS, JULY 1986 TO OUTPUT INDEX TO
C                ORIGINAL MATRIX(ARRAY IARR)
C
C     ARGUMENT LIST : ARR - GIVEN ARRAY TO BE SORTED.
C              IARR- ARRAY CONTAINING INDICES OF ARR
C                    THEY POINT TO LOCATION IN ORIGINAL ARRAY
C              NUM - NUMBER OF ENTRIES IN THE GIVEN ARRAY.
C              ASC - LOGICAL VARIABLE, TRUE FOR ASCENDING,
C                    FALSE FOR DESCENDING ORDER.
C              ERR - LOGICAL VARIABLE TO INDICATE AN ERROR OCCURS.
C
      SUBROUTINE SORT(ARR,IARR,NUM,ASC,ERR)
C
      INTEGER NUM,I,J,K
      INTEGER IARR(NUM)
      REAL ARR(NUM)
      LOGICAL ASC,ERR
C
C      INVALID VALUE OF NUM.
C
      ERR = .FALSE.
C
      IF (NUM .LE. 0) THEN
         ERR = .TRUE.
         PRINT *,'INVALID NUMBER OF ENTRIES IN THE ARRAY !'
         RETURN
      ENDIF
C
      IF (NUM .EQ. 1) THEN
         RETURN
      ENDIF
C
      CALL QSORT(ARR,IARR,NUM)
      IF (.NOT. ASC) THEN
         J = NUM
         K = INT(NUM/2)
         DO 10 I = 1,K
            CALL RSWAP(ARR(I),ARR(J-I+1))
            CALL ISWAP(IARR(I),IARR(J-I+1))
10       CONTINUE
      ENDIF
      END
C*****************************************************************
C     SUBROUTINE QSORT - SORTS AN ARRAY OF NUMBER INTO ASCENDING OR
C     DESCENDING ORDER USING PARTITION-EXCHANGE (QUICK SORT) METHOD
C
C     ARGUMENT LIST : ARR - GIVEN ARRAY TO BE SORTED.
C              NUM - NUMBER OF ENTRIES IN THE GIVEN ARRAY.
C                    FALSE FOR DESCENDING ORDER.
C
C     LOCAL VARIBLES :
C        L/R - LEFT/RIGHT INDICES OF THE ARRAY TO BE PARTITIONED.
C        L1/R1,L2/R2 - LEFT/RIGHT INDICES OF THE TWO SUBARRYS
C                      AFTER PARTITIONED.
C        LST/RST - STACKS OF LEFT/RIGHT INDICES OF THE ARRAYS.
C        TOP - POINTER POINTS TO THE TOP OF THE STACKS(LST/RST).
C
      SUBROUTINE QSORT(ARR,IARR,NUM)
C
      INTEGER NUM
      INTEGER IARR(NUM)
      REAL ARR(NUM)
      INTEGER TOP,LST(20),RST(20)
      INTEGER L,R,L1,R1,L2,R2
C
C     INITIALIZE THE STACKS
C
      TOP = 1
      LST(TOP) = 1
      RST(TOP) = NUM
C
C     WHILE STACK IS NOT EMPTY, DO THE PARTITIONING.
C
100   IF (TOP .GT. 0) THEN
         L = LST(TOP)
         R = RST(TOP)
         TOP = TOP - 1
C
C     IF MORE THAN ONE ENTRY THEN CALL PART.
C
         IF (L .LT. R) THEN
           CALL PART(ARR,IARR,NUM,L,R,L1,R1,L2,R2)
C
C     STACK THE LEFT/RIGHT INDICES OF THE TWO SUBARRAYS,
C     STACK THE LONGER SUBARRY'S INDICES FIRST.
C
           IF ((R1-L1) .GT. (R2-L2)) THEN
            TOP = TOP + 1
            LST(TOP) = L1
            RST(TOP) = R1
            TOP = TOP + 1
            LST(TOP) = L2
            RST(TOP) = R2
           ELSE
            TOP = TOP + 1
            LST(TOP) = L2
            RST(TOP) = R2
            TOP = TOP + 1
            LST(TOP) = L1
            RST(TOP) = R1
           ENDIF
C
         ENDIF
C
         GOTO 100
      ENDIF
C
      END
C
C
C     SUBROUTINE PART - PERFORMS PARTITION ON THE GIVEN ARRAY
C                INTO TWO SUBARRAYS. THE FIRST ENTRY OF THE GIVEN ARRAY
C                 IS USED AS THE KEY. ALL THE ENTRIES IN THE LEFT
C                 SUBARRAY IS LESS THAN OR EQUAL TO THE KEY. ALL
C                 THE ENTRIES IN THE RIGHT SUBARRY IS GREATER
C                 THAN THE KEY.
C
C
      SUBROUTINE PART(ARR,IARR,NUM,L,R,L1,R1,L2,R2)
C
      INTEGER NUM,L,R,L1,R1,L2,R2,I,J
C
      INTEGER IARR(NUM)
      REAL KEY,ARR(NUM)
C
C     INITIALIZE THE SCAN.
C
      I = L
      J = R + 1
      KEY = ARR(L)
C
100   IF (I .LT. J) THEN
C
C     SCAN FROM THE RIGHT, LOOK FOR THE ENTRY LESS THAN OR EQUAL
C     TO THE KEY.
C
         J = J -1
300      IF (ARR(J) .GT. KEY) THEN
            J = J - 1
            GO TO 300
         ENDIF
C
C     SCAN FROM THE RIGHT, LOOK FOR THE ENTRY GREATER THAN OR
C     EQUAL TO THE KEY.
C
         I = I + 1
500      IF (ARR(I) .LT. KEY .AND. I .LT. J) THEN
            I = I + 1
            GO TO 500
         ENDIF
C
C     INTERCHANGE THE TWO ENTRIES IF THE SCAN HAVE NOT OVERLAPPED
C
         IF (I .LT. J) THEN
            CALL RSWAP(ARR(I),ARR(J))
            CALL ISWAP(IARR(I),IARR(J))
         ENDIF
C
         GO TO 100
C
      ENDIF
C
C     PLACE THE KEY IN THE FINAL POSITION
C
      CALL RSWAP(ARR(L),ARR(J))
      CALL ISWAP(IARR(L),IARR(J))
C
C     COMPUTES THE LEFT/RIGHT INDICES OF THE TWO SUBARRAYS.
C
      L1 = L
      R1 = J - 1
      L2 = J + 1
      R2 = R
      END
C
C****************************************************************
C
C     SUBROUTINE CSWAP - INTERCHANGES VALUES BETWEEN TWO VARIABLES
C
      SUBROUTINE CSWAP(EX1,EX2)
C
      CHARACTER*8 EX1,EX2,TEMP
C
      TEMP = EX1
      EX1 = EX2
      EX2 = TEMP
      END
C****************************************************************
C
C     SUBROUTINE ISWAP - INTERCHANGES VALUES BETWEEN TWO VARIABLES
C
      SUBROUTINE ISWAP(EX1,EX2)
C
      INTEGER EX1,EX2,TEMP
C
      TEMP = EX1
      EX1 = EX2
      EX2 = TEMP
      END
C****************************************************************
C
C     SUBROUTINE RSWAP - INTERCHANGES VALUES BETWEEN TWO VARIABLES
C
      SUBROUTINE RSWAP(EX1,EX2)
C
      REAL EX1,EX2,TEMP
C
      TEMP = EX1
      EX1 = EX2
      EX2 = TEMP
      END
C****************************************************************

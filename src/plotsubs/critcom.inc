!-----------------------------------------------------------------------*
!                                                                       *
!                       CRITCOM.INC                                     *
!       Common area used to control the display of vertex markers.      *
!                                                                       *
!-----------------------------------------------------------------------*

!     - Maximum number of criteria which are currently implemented in the 
!     -- current version of the editor. Array bound for TheCriteria() below.
!     -- Mar/91 (JDM), MaxCrit increased from 15 to 16 for new option C= .
      INTEGER MaxCrit
      PARAMETER( MaxCrit = 19 )

!     - Flag which is set TRUE if any criteria is to be displayed.
      LOGICAL DrawVCrit

!     - Flags which keep track of which criteria is currently being displayed
!     -- Mar/91 (JDM, flag TheCriteria(16) added for option C= which allows
!     -- placement of markers where code is equal to a user entered value
!     -- ( USER_CODE ).
!     -- array TheCriteria(1->MaxCrit) contents:
!     -- [1] CO [2] C1 [3] C2 [4] C3 [5] C4 [6] C5 [7] C6 [8] NOTC0
!     -- [9] DLT [10] DGT [11] DBTW [12] NBGT [13] NBLT [14] NBE [15] EXT
!     -- [16] C=
      LOGICAL TheCriteria(MaxCrit)

!     - Following variables for various criteria function calculations.
!     -- Mar/91 (JDM), USER_CODE added for option C= ( TheCriteria(16) )

      REAL USER_COUNT, USER_COUNT1, LOWERD, UPPERD
      INTEGER USER_NCOUNT, USER_NCOUNT1, USER_NCOUNT2, USER_CODE

!     - Common block used to control the criteria.
      COMMON /VerControl/ USER_COUNT, USER_COUNT1, LOWERD, UPPERD,              &
     &         USER_NCOUNT, USER_NCOUNT1, USER_NCOUNT2,  USER_CODE,             &
     &         TheCriteria, DrawVCrit

!------------------END CRITCOM.INC--------------------------------------*

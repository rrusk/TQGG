!-------------------------
! File:  NOTICOM.INC
!-------------------------

! Common block for noti system:
!     var name        type                     meaning
!     --------     -----------       -----------------------------------------
!     NumOfTests    integer           # of unique tests available
!     NumOfColours  integer           # of unique colours available
!     NumOfBndryPts integer           Maximun number of boundary points
!                                    allowed for each unique test
!     Interval     array of real      Boundary points for the test intervals
!     TestColour   array of integer   For each interval for each test :
!                                    the color for each interval
!     NumTest      array of integer   For each test: the number of boundary
!                                     points
!     Colour       array of string    The colours available

      INTEGER NumOfTests, NumofColours, NumOfBndryPts
      PARAMETER (NumOfTests = 7,NumofColours = 16,NumOfBndryPts = 15)
!      N.B.
!      when changing NumOfTests here must change MaxCrit correpondingly
!      in /trigrid/includes/TRSCOM.SET

      REAL Interval(1:NumOfTests,1:NumOfBndryPts)

      integer TestColour(1:NumOfTests,0:NumofBndryPts)
      INTEGER NumTest(1:NumOfTests)

      !CHARACTER*7 Colour(0:NumofColours)

      COMMON /NOTI_C/ Interval, TestColour, NumTest  !, Colour

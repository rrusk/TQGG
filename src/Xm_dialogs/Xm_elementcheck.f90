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

!---------------------------------------------------------------------------*
      SUBROUTINE FlagsTriangles_Init(change)

! Purpose: To set up RH Panel for FlagsTriangles routine.
! Givens : None
! Returns: None
! Effects: RH Panel is set up with 6 options to be used by FlasTriangles
!          routine.

      use MainArrays

!     - PASSED PARAMETERS
      LOGICAL change

!     - INCLUDES
      INCLUDE '../includes/graf.def'

!     - COMMON AREAS
      LOGICAL fullcolour
      COMMON /SHADE/ fullcolour

      LOGICAL TrHiOff
      COMMON /TH/ TrHiOff

!     - LOCAL VARIABLES
      integer hitnum

!---------- BEGIN --------------


!     - Load triangle lists
      call PigPutMessage('Forming triangle list-please wait')      
!      call LdTrLt(change)
        if(change) then
          call RemoveNotExist(itot,code,nbtot,nl)
          call Element_Lister(CHANGE, .FALSE. , &
     &          itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode,  &
     &          x0off,y0off,scaleX,scaleY,igridtype)
          change = .false.
        endif
      call PigEraseMessage

      call FlagsTriangles_Ehandler(change, Hitnum)

      END

!---------------------------------------------------------------------------*
      SUBROUTINE FlagsTriangles_Ehandler( change, Hitnum)

! Purpose: Dispatch routine for the colouring of TRIANGLES according to
!          various user defined criteria.
! Givens : None
! Returns: CHANGE - TRUE IF any changes
! Effects: Coloring/marking of triangles by criteria may be set ON or OFF.
! Written: Steve Prestage and Daphne Connolly May 1989
! Modified: OCT91 - JDM - RH Panel implemented to replace prompts. Segments
!                         removed.

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'noticom.inc'

!     - PASSED PARAMETERS
      LOGICAL change

!     - COMMON AREAS
      LOGICAL fullcolour
      COMMON /SHADE/ fullcolour

!      integer Test
!      COMMON /CRITER/ Test

      LOGICAL TrHiOff
      COMMON /TH/ TrHiOff
      
!     - LOCAL VARIABLES
      character*1 ans
      integer hitnum
      LOGICAL start
      DATA start /.TRUE./

!--------------BEGIN-------------
!              - change from none to full color
      IF (start) THEN
        call NUP_Init(.TRUE.)
        start = .FALSE.
      ENDIF

      TrHiOff = .false.
      call PigMessageYesNo ('Full colour (or symbols)? ',ans)
      if(ans(1:1).eq.'Y') then
        fullcolour = .true.
      else
        fullcolour = .false.
      endif

! tests: 1=eql, 2=dep, 3=a2d, 4=ccw, 5=g90
      call PigPrompt('Enter test number (1-5): ', ans )
      read(ans,'(i1)') hitnum

      call Nup_FCriteria_Ehandler ( change, hitnum)


      END

!---------------------------------------------------------------------------*
!                         NUP.FOR                                           *
!     This module is concerned with the colouring of triangles              *
!     by criteria tests, from an external file or a colour table.           *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
      SUBROUTINE NUP_Init( RESETFLAG )

! Purpose: Initializes the NOTI_USER_PROMPTS module
! Givens : RESETFLAG - If TRUE then colour table will explicitly be
!                      initialized.
!                    - If FALSE, then colour table will only be
!                      initialized if this is the first time calling
!                      this routine.
! Effects : COLOUR array is loaded and NumTest array is initialized.
! Modified: Steve Prestage  May 1989

      INCLUDE 'noticom.inc'

! Passed variables
      LOGICAL ResetFlag

! Local Variables
      integer I

      LOGICAL Start
      DATA Start /.TRUE./

      if ((.not. start) .and. (.not. resetflag)) goto 999

      Start = .FALSE.

!     Set colours
! NOTE **  the following colours exist for GSS-GKS only.
! Following colours may not be used anywhere, but removing these
! statements causes array size error in ERASEPERMMARKER in PERMMARK.FOR
! when Editor is loaded 
      Colour(0) = 'NONE'
      Colour(1) = 'WHITE'
      Colour(2) = 'RED'
      Colour(3) = 'GREEN'
      Colour(4) = 'BLUE'
      Colour(5) = 'YELLOW'
      Colour(6) = 'CYAN'
      Colour(7) = 'MAGENTA'

!     Initialize the current number of interval boundary points for each test
      DO 10 I = 1 , NumOfTests
        NumTest(I) = 0
10    CONTINUE

999   continue
      END

!---------------------------------------------------------------------------*
      SUBROUTINE Nup_FCriteria_Ehandler ( change, hitnum)

! Purpose : Equivalent to NUP_Criteria.
!           PigPrompts for test type and tolerances
! Given   : nrec - # of nodes
!           change = .true. if new triangle list required
!                    .false. otherwise
! Effects : Nil
! Modified: OCT91 - JDM - prompts reduced by RH Panel implementation.
!           FEB 93 - RFH - various cleanup changes


!     - INCLUDES
      INCLUDE 'noticom.inc'
      INCLUDE '../includes/graf.def'

!     - COMMON AREAS
      integer TEST
      COMMON /CRITER/TEST

!     - PASSED VARIABLES
      INTEGER hitnum
      LOGICAL change
!     - LOCAL VARIABLES
      LOGICAL prompt

!------------BEGIN---------------

!     - get criteria selection
      test = hitnum
!      call Nup_Ftest_Prompt(Hitnum, done)
!      if(.not.done) return
      prompt = .true.
      call Nup_Fparams( prompt )
!     return

!     entry Nup_FCriteria_Reset(nrec, change)
!     make options from FlagsTriangles available
!      call ReenablePanelHit(1)
!      call ReenablePanelHit(2)
!     - make criteria unavailable
!      do i=11,17
!      call DisablePanelHit(i)
!      end do
      call DrwFig (.false., change)
      END

!---------------------------------------------------------------------------*

      SUBROUTINE Nup_Ftest_Prompt(hitnum, DONE)

! Purpose : New version reading character strings introduced 22/6/88
!           equivalent to NUP_MENU
! Given   : None
! Effects : Returns COMMON integer TEST indicating criterion required.
! Modified: Steve Prestage  May 1989
! Modified: OCT91 - JDM - RH Panel implementation.

!     - INCLUDES
      INCLUDE 'noticom.inc'
      INCLUDE '../includes/graf.def'

!     - COMMON AREAS
      integer TEST
      COMMON /CRITER/TEST

!     - LOCAL VARIABLES
      integer hitnum
      LOGICAL ok, done

!----------BEGIN----------

        done = .TRUE.
    IF ( hitnum .eq. 11 ) THEN
!         - EQL
      test = 1
    ELSEIF ( hitnum .eq. 12 ) THEN
!         - DEP
      test = 2
    ELSEIF ( hitnum .eq. 13 ) THEN
!         - A2D
      test = 3
    ELSEIF ( hitnum .eq. 14 ) THEN
!         - CCW
      test = 4
    ELSEIF ( hitnum .eq. 15 ) THEN
!         - G90
      test = 5
    ELSEIF ( hitnum .eq. 17 ) THEN
!         - EXT
      test = 6
    ELSEIF ( hitnum .eq. 16 ) THEN
!         - EXT
          test = 7
      call RdXCriF( ok )
      IF ( .NOT. ok ) THEN
        done = .FALSE.
      ENDIF
    ELSE
      call PigPutMessage('Invalid Test in Nup_Ftest_Prompt - setting to 1')
      test = 1
    ENDIF

      END

!---------------------------------------------------------------------------*
      SUBROUTINE NUP_FPARAMS(prompt)

! Purpose : Equivalent to NUP_PARAMS
! Effects : resets number of interval points for triangle test(s);
!           resets the interval point values for triangle test(s)
! Modified: Steve Prestage  May 1989
      logical prompt

      INCLUDE 'noticom.inc'
      INCLUDE '../includes/graf.def'

      integer Test
      COMMON /CRITER/Test

      CHARACTER*80 ans
      character PigCursYesNo
      LOGICAL Ok, Success
      
      REAL Default_Interval(1:NumOfTests,1:NumOfBndryPts)
      integer I
      integer Default_TestColour(1:NumOfTests,0:NumofColours)
      INTEGER Default_NumTest(1:NumOfTests)

      integer testsaved

      data Default_Numtest/3,5,4,1,1,6,0/
      data (Default_TestColour(1,I),I=0,15)/black,blue,yellow,red,12*0/
      data (Default_TestColour(2,I),I=0,15) /red,yellow,green,cyan,blue,violet,10*black/
      data (Default_TestColour(3,I),I=0,15) /blue,cyan,green,yellow,red,11*black/
      data (Default_TestColour(4,I),I=0,15)/red,15*black/
      data (Default_TestColour(5,I),I=0,15)/red,15*black/
      data (Default_TestColour(6,I),I=0,15) /black,red,yellow,green,cyan,blue,violet,9*black/
      data (Default_TestColour(7,I),I=0,15)/16*black/

      data (Default_Interval(1,I),I=1,15)/1.2,1.4,2.0,0.,0.,10*0./
      data (Default_Interval(2,I),I=1,15)/10.,20.,30.,40.,50.,10*0./
      data (Default_Interval(3,I),I=1,15)/.003,.005,.007,.008,0.,10*0./
      data (Default_Interval(4,I),I=1,15)/0.0,0.0,0.0,0.0,0.0,10*0./
      data (Default_Interval(5,I),I=1,15)/0.0,0.0,0.0,0.0,0.0,10*0./
      data (Default_Interval(6,I),I=1,15)/0.1,1.1,2.1,3.1,4.1,5.1,9*0./
      data (Default_Interval(7,I),I=1,15)/0.0,0.0,0.0,0.0,0.0,10*0./

!-----------BEGIN-------------
! save and restore value of test, to prevent crash during refresh...
! which can occur if the PigCursYesNo dialog box is moved, or the screen
! is otherwise refreshed.
      testsaved = test
      test = 0
      prompt = .false.
      if(prompt)then
          ans = PigCursYesNo ('Set or change color scale ?:')
      else
          ans = 'N'
      endif
      test = testsaved
      if (ANS(1:1) .eq. 'N') then
      if(NumTest(Test).eq.0)then

         Numtest(Test) = Default_Numtest(Test)
         do i=1,Numtest(Test)
           Interval(Test,i) = Default_Interval(Test,i)
         enddo
         do i=0,Numtest(Test)
           TestColour(Test,i) = Default_TestColour(Test,i)
         enddo
! 
         call PigPutMessage('Using default color scale for this test..')
      endif
        return
      endif

      NumTest(Test) = 0
4     continue
      testsaved = test
      test = 0
      ans = PigCursYesNo ('Use prepared colour table ?:')
      test = testsaved

      if (ANS(1:1) .eq. 'Y') then
         call AUTO_FSHADING( Ok )
         if ( .not. ok ) goto 4
         goto 1000
      elseif ( ans(1:1) .eq. 'N' ) then
         CALL NUP_FSHADING
6          continue
         testsaved = test
         test = 0
         call PigPrompt('Enter boundary value.. (<CR> = no bound):', ans )
         call PigEraseMessage
         test = testsaved

!          WHILE (ans(1:1) <> ' ') DO
10         CONTINUE

         IF (ans(1:1) .EQ. ' ') GOTO 100
           NumTest(Test) = NumTest(Test) + 1

           call PigReadReal( ans, Interval(Test, NumTest(Test)),Success)
           if (.not. Success) goto 6

!        check boundary point before asking for colour
           IF (NumTest(Test) .eq. 1) THEN
             CALL NUP_FSHADING
           ELSEIF ( Interval(Test,NumTest(Test)).gt. Interval(Test,NumTest(Test)-1)) THEN
               CALL NUP_FSHADING
           ELSE
             NumTest(Test) = NumTest(Test) - 1
           ENDIF

          IF (NumTest(Test) .NE. NumOfBndryPts) THEN
             call PigPrompt('Enter boundary value (<CR> = no bound):', ans )
           ELSE
             ans(1:1) = ' '
           ENDIF
!        END WHILE
         GOTO 10
      endif
100   CONTINUE

1000  continue
      END


!---------------------------------------------------------------------------*
      SUBROUTINE RdXCriF( Ok )

!           called by S/R NUP_FTEST_PROMPT
!
! Effect:   Reads in file in EXTCRI format
!           - each record contains a triangle number followed
!           by value of user's triangle criterion.

      use MainArrays

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

! Passed parameters
      LOGICAL Ok

!      integer Test
!      COMMON /CRITER/ Test

      integer I, anslen
      logical PigOpenFile

!   Following are used in file read
!       END = Y/N end of file
      CHARACTER*(80) ans

!      Initialize CritLt(1),.. CritLt(MREC*2) to nul value
!      recognised by module TrHiLite.FOR
      DO 100 I = 1, MREC*2
100   CritLt(I) = 999999.
! ------------------ Reading external file ---------------------------
!     *get the filename from the user

      ok = .false.

!      if(PigGetOpenFileName('Open triangle data file', ans,
      if(PigOpenFile(23,'Open triangle data file', ans, &
     &    'Triangle Files (*.tri),*.tri;All Files (*.*),*.*;')) then
        anslen = len_trim( ans )
        Ok = .TRUE.
        TCritName = Fblank
        TCritName = ans(1:anslen)
!        open (unit=23,file=ans(1:anslen),status='OLD',
!     +      access='SEQUENTIAL',iostat=IOS)
        do while(.true.)
            READ(23,*,ERR=52,END=200) I, CritLt(I)
        end do
        GOTO 53
52      continue
           call PigPutMessage('ERROR - reading external triangle criteria file..')
           ok = .FALSE.
           close(23)
           return
53       continue
      else
       Ok = .FALSE.
      endif
200   CONTINUE
      close(23)
      END

!---------------------------------------------------------------------------*
      SUBROUTINE NUP_FSHADING

!           equivalent to NUP_ISHADING
! Effects : loads a valid colour number into TestColour(Test,NumTest(Test))
! Modified: Steve Prestage  May 1989

      INCLUDE 'noticom.inc'
      INCLUDE '../includes/graf.def'

      integer Test
      COMMON /CRITER/Test

      integer ColourNum
      CHARACTER*80 ans
      integer testsaved

9     continue
      testsaved = test
      test = 0
      call PigPrompt('Enter colour for range:',ans)
      test = testsaved
      ColourNum = (ICHAR(ans(1:1))-ICHAR('0'))

      IF ( ColourNum .GE. 0 .AND. ColourNum .LE. NumOfColours) THEN
       TestColour(Test, NumTest(Test)) = ColourNum
      ELSE
       GOTO 9
      ENDIF
!     UNTIL 0 <= ColourNum <= NumOfColours
      END


!---------------------------------------------------------------------------*
      Subroutine AUTO_FSHADING( Ok )

!           equivalent to AUTO_SHADING
! Purpose : to input the test criteria (the colors and boundary values)
!           with a preset color table defined by the user in a file to
!           be read in rather than manually entering the values
! Given   : Test - the test to be performed (ie. Area to Depth Ratio)
! Input   : Colors and boundary values from user supplied file
! Output  : Error messages to the user

!     Comment : Performs same task as S/R AUTO_SHADING, but is geared to
!               use during with header messages during graphics sessions.
!     Modified: Steve Prestage  May 1989


      INCLUDE 'noticom.inc'
      INCLUDE '../includes/graf.def'

! Passed Variables
      LOGICAL Ok

      integer Test
      COMMON /CRITER/Test

      REAL BNDVAL
!        - interval value from the input file
      integer UserColour
!        - colour value from the input file
      CHARACTER*256 ans
!        - file name entered by the user
      LOGICAL FOUND
!        - loop control
      integer anslen

      logical PigOpenFile

! *** START SUBROUTINE ***

!     *get the filename from the user

      if(PigOpenFile(27,'Open colour data file', ans,  &
     &    'Colour Files (*.col),*.col;All Files (*.*),*.*;')) then
      ok = .TRUE.
      anslen = len_trim( ans )

      read (27,*,ERR=21,END=21) UserColour, BNDVAL
      goto 23
21      continue
      call PigMessageOK('ERROR - reading external color file')
!      call PigUWait(2.0)
      Ok = .FALSE.
      close ( Unit=27 )
      goto 9999
23      continue

      FOUND = .false.
      if (UserColour .gt. NumOfColours) goto 55
      TestColour(Test, NumTest(Test)) = UserColour
      FOUND = .true.

55      continue
      if (.not. FOUND) then
        call PigMessageOK('Color in file not allowed.. Restart session')
!        call PigUWait (2.5)
      endif

30      continue
      if (BNDVAL .eq. 9999) then
        close (unit=27)
        goto 9999
      else
        NumTest(Test) = NumTest(Test) + 1
        Interval (Test,NumTest(Test)) = BNDVAL
      endif

      if ((NumTest(Test) .eq. 1) .or.(Interval(Test,NumTest(Test)) .gt. &
     &    Interval(Test,NumTest(Test)-1))) then
        read (27,*,ERR=21,END=21) UserColour, BNDVAL

        FOUND = .false.
        if (UserColour .gt. NumOfColours) goto 125
        TestColour(Test,NumTest(Test)) = UserColour
        FOUND = .true.

125         continue
        if (.not. FOUND) then
           call PigMessageOK('Color in file not allowed.. Restart session')
!           call PigUWait (2.5)
        endif
      else
        NumTest(Test) = NumTest(Test) - 1
      endif

      if (NumTest(Test) .eq. NumOfBndryPts) BNDVAL = 9999
      goto 30
      else
     ok = .TRUE.
      endif

9999  continue
      end

!---------------------- END NUP.FOR ----------------------------------------*

!---------------------------------------------------------------------------*
!                            TRIPROP.FOR                                    *
!       This module is the triangle data manipulation module.               *
!---------------------------------------------------------------------------*
! Note that module TRIFNS.FOR used by program TRISTATS contains real        *
!      functions CalEqlat, CalDepth, TrPerim, CalA2D, TrDepth, TrArea       *
!      extracted this module
!---------------------------------------------------------------------------*

    REAL FUNCTION CalCrit( Crit, T, Status)

! Purpose : To dispatch the correct criteria test
! Given   : A criteria index to a triangle.
    integer   Crit, T
! Returns : The calculation of the specified criteria
!           and a status code: 1 = infinity; -1 = -infinity;
!                            -99 = bad criteria
        integer   Status, Eqlat, Dep, A2D, CCW, G90, TriCode

!  Following line added  March 27,89     S.P.
    REAL CalA2D, CalEqlat, CalDepth, CalCCW, CalG90, CalTCode

    PARAMETER ( Eqlat = 1, Dep = 2, A2D = 3, CCW = 4, G90 = 5, TriCode = 6)

        Status = 0
    IF (Crit .eq. A2D) THEN
      CalCrit = CalA2D( T, Status)
    ELSEIF (Crit .eq. Eqlat) THEN
      CalCrit = CalEqlat( T, Status)
    ELSEIF (Crit .eq. Dep) THEN
      CalCrit = CalDepth( T, Status)
    ELSEIF (Crit .eq. CCW) THEN
      CalCrit = CalCCW( T, Status)
    ELSEIF (Crit .eq. G90) THEN
      CalCrit = CalG90( T, Status)
    ELSEIF (Crit .eq. TriCode) THEN
      CalCrit = CalTCode( T, Status)
!  more elseif's for more criteria ...
    ELSE
      Status = -99
    END IF
    END

!---------------------------------------------------------------------------*
    REAL FUNCTION CalA2D( T, Stat)

! Given  :An index to a triangle
        integer      T, Stat
! Returns:The area / mean depth ratio for the triangle
!         and a status code:  1 = infinity

    REAL Depth, Trdepth, TrArea

    Depth = Trdepth( T)
    IF ( Depth .eq. 0) THEN
      Stat = 1
    ELSE
      CalA2D = TrArea( T) / Depth
      Stat = 0
    END IF

    END

!---------------------------------------------------------------------------*
    REAL FUNCTION TrDepth( T)

! Given  : An index to a triangle
! Returns: The depth of triangle: the average depth of its vertices

      use MainArrays

      integer        T


    TrDepth = ( DEPTH(ListTr(1,T)) + DEPTH(ListTr(2,T)) + DEPTH(ListTr(3,T)) ) / 3
    END

!---------------------------------------------------------------------------*
    REAL FUNCTION TrArea(T)

! Given  : An index to a triangle in ListTr
! Returns: The area of the indexed triangle

      use MainArrays

      integer      T

    REAL X(3), Y(3)
    integer I

    DO 10 I = 1, 3
      X(I) = DXRAY(ListTr(I,T))
      Y(I) = DYRAY(ListTr(I,T))
10      CONTINUE
    TrArea = 0.5 * ABS( (X(1)-X(3))*(Y(2)-Y(3)) -(X(2)-X(3))*(Y(1)-Y(3)) )       
    END

!---------------------------------------------------------------------------*

    REAL FUNCTION CalEqlat( T, Stat)

! Given  : An index to a triangle
! Returns:The equilateral calculation of a triangle
!          and a status code:  1 = infinity

    integer        T, Stat
    REAL FourRootThree , Denom, TrArea, TrPerim
    Parameter (FourRootThree=6.928032)

    Denom = TrArea( T) * FourRootThree
    IF ( Denom .eq. 0 ) THEN
      Stat = 1
    ELSE
      CalEqlat = TrPerim( T) / Denom
      Stat = 0
    END IF
    END

!---------------------------------------------------------------------------*
    REAL FUNCTION CalCCW( T, DUMMY)

! Given:   An index to a triangle
! Returns: CalCCW = -1 if triangle vertices 1,2,3 are clockwise
!                 = +1 if triangle vertices  are counterclockwise

      use MainArrays

! *** Passed variables ***

      INTEGER T, DUMMY

! *** Local variables
      REAL X1 , X2 , X3 , Y1 , Y2 , Y3 , A , B , U , V
      

      X1 = DXRAY(ListTr(1,T))
      Y1 = DYRAY(ListTr(1,T))     
      X2 = DXRAY(ListTr(2,T))
      Y2 = DYRAY(ListTr(2,T))     
      X3 = DXRAY(ListTr(3,T))
      Y3 = DYRAY(ListTr(3,T))     

      A = X2 - X1
      B = Y2 - Y1

      U = X3 - X1
      V = Y3 - Y1
      
      CalCCW = -1.
      IF (A*V-B*U.gt.0) THEN
        CalCCW = +1.
      ENDIF

      DUMMY = 0

      END

!---------------------------------------------------------------------------*
    REAL FUNCTION CalG90( T, DUMMY)

! Given:   An index to a triangle
! Returns: CalG90 = -1 if any internal angle of triangle .gt. 90 deg
!                  = +1 if all internal angles .le. 90 deg

      use MainArrays

! *** Passed variables ***

      integer T, DUMMY
! *** Local variables ***

      Real X1 , X2 , X3 , Y1 , Y2 , Y3 , ASQ, BSQ, CSQ
!      - ASQ,BSQ, CSQ are squares of lengths of sides

      X1 = DXRAY(ListTr(1,T))
      Y1 = DYRAY(ListTr(1,T))     
      X2 = DXRAY(ListTr(2,T))
      Y2 = DYRAY(ListTr(2,T))     
      X3 = DXRAY(ListTr(3,T))
      Y3 = DYRAY(ListTr(3,T))     

      ASQ = (X1-X2)**2 + (Y1-Y2)**2
      BSQ = (X2-X3)**2 + (Y2-Y3)**2
      CSQ = (X3-X1)**2 + (Y3-Y1)**2
! Apply test based on cosine rule - only in triangle with one angle 
! more than 90 deg can the square of one side be greater than the 
! sum of the squares of the other two sides.      
      CalG90 = 1.
      IF ((CSQ.GT.ASQ+BSQ) .OR. (BSQ.GT.ASQ+CSQ).OR. (ASQ.GT.BSQ+CSQ)) THEN
        CalG90 = -1.
      ENDIF

      DUMMY = 0

      END

!---------------------------------------------------------------------------*
    REAL FUNCTION CalDepth( T, Stat)

! Given:  an index to a triangle
    integer      T, Stat
! Returns:the mean depth of the triangle
!         and a status code:  1 = "infinite" depth or depth < 1.0

    REAL Depth, Trdepth

    Depth = Trdepth( T)
    IF ( Depth .gt. 99999 .or. Depth .lt. -1) THEN
      Stat = 1
    ELSE
      CalDepth =  Depth
      Stat = 0
    END IF

    END


!---------------------------------------------------------------------------*
    REAL FUNCTION CalTCode( T, Stat)

      use MainArrays

! Given:  an index to a triangle

      integer      T, Stat

      CalTCode =  TCode(T)
      Stat = 0

    END

!---------------------------------------------------------------------------*
    REAL FUNCTION TrPerim( T)

! Given:   An index to a triangle
! Returns: The "square perimeter" of the triangle

      use MainArrays

      integer        T

      REAL X1,Y1, X2,Y2, X3,Y3

    X1 = DXRAY(ListTr(1,T))
    Y1 = DYRAY(ListTr(1,T))

    X2 = DXRAY(ListTr(2,T))
    Y2 = DYRAY(ListTr(2,T))

    X3 = DXRAY(ListTr(3,T))
    Y3 = DYRAY(ListTr(3,T))

    TrPerim = ( X2-X1 )**2 + ( Y2-Y1 )**2 + ( X3-X2 )**2 + ( Y3-Y2 )**2 &
            + ( X1-X3 )**2 + ( Y1-Y3 )**2

    END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!                          TRHILITE.FOR                                     *
!       The purpose of this module is to provide highlighting               *
!       for the triangles, by criteria.                                     *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
    SUBROUTINE HiLtTrs( CHANGE )

!  Purpose : To highlight triangles.
!  Givens  : CHANGE - TRUE  if the triangle list needs to be re-generated.
!                   - FALSE otherwise
!  Returns : None
!  Effects : None
!  Modified: Steve Prestage    June 1989.

      use MainArrays

! Passed parameters
    LOGICAL CHANGE

! Local Parameters
    LOGICAL NeedNewListTr
    integer MaxCrit

      INCLUDE 'noticom.inc'

    LOGICAL TrHiOff
    COMMON /TH/ TrHiOff

    integer Tr,Cr,I
    REAL TVal, CalCrit

    integer Test, S
    COMMON /CRITER/Test

    logical start
!    character*(80) msg

    data start/.true./

    if (start) then
      start = .false.
      test=0
    endif

    NeedNewListTr = CHANGE
    MaxCrit = NumOfTests

    Cr = Test
    IF(Cr.eq.0)go to 21
    IF(NumTest(Cr).eq.0)go to 21
    IF( .NOT. TrHiOff) THEN
!     if (CHANGE) then
!       call LdTrLt(CHANGE)
!     endif
        if(change) then
          call RemoveNotExist(itot,code,nbtot,nl)
          call Element_Lister(CHANGE, .FALSE. ,  &
     &          itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
     &          x0off,y0off,scaleX,scaleY,igridtype)
          change = .false.
        endif

! Following 'if' structure added to fix external file criterion problem
! Steve Prestage   June 1989.
!          write(msg,'(a,i6,a)') 'Colouring',TotTr,' triangles'
!          call PigPutMessage(msg)
      if ((Cr.eq.MaxCrit).and.(NeedNewListTr)) then
         TrHiOff = .TRUE.
         call PigPutMessage('WARNING - External criterion file'// &
     &           ' is no longer valid.. Grid has been changed.')
      else
         DO 20 Tr = 1, TotTr
        IF(Cr.GE.0.and.Cr.LT.MaxCrit) then
          TVal = CalCrit(Cr,Tr,S)
        ELSE IF(Cr.EQ.MaxCrit) then
          TVal = CritLt(Tr)
        ELSE
          TVal = 0
        ENDIF

        IF((Cr.eq.MaxCrit).and.(TVal.eq.999999.))go to 20
        IF( TVal .lt. Interval(Cr,1)) THEN
          CALL ShdTr(Tr,TestColour(Cr,0))
        ELSEIF( TVal .ge. Interval(Cr,NumTest(Cr))) THEN
          CALL ShdTr(Tr,TestColour(Cr,NumTest(Cr)))
        ELSE
          DO 10 I = 1, NumTest(Cr)-1
            IF( TVal .ge. Interval(Cr,I) .and. TVal .lt. Interval(Cr,I+1) ) THEN
              CALL ShdTr(Tr,TestColour(Cr,I))
            ENDIF
10                CONTINUE
        ENDIF
20           CONTINUE
      endif
    ENDIF
21      CONTINUE
    END


!---------------------------------------------------------------------------*
    SUBROUTINE ShdTr( t, c )

!  Purpose : To colour shade the triangles by criteria.
!  Given   : Triangle number and colour code.
!  Returns : None
!  Effect  : Triangles matching a specified criteria are coloured.
!  Modified: Steve Prestage  May 1989
!  Modified: OCT91 - JDM - segments removed.

      use MainArrays

!     - INCLUDES
      INCLUDE 'noticom.inc'

!     - PASSED PARAMETERS
      integer t
      integer c

      REAL     cwxl,cwxh,cwyl,cwyh
      COMMON  /CURWIN/ CWXL,CWXH,CWYL,CWYH

      LOGICAL fullcolour, in_box
      COMMON /SHADE/ fullcolour

!     - LOCAL VARABLES
      integer j,numfp
      REAL x(4), y(4), xcen, ycen
      LOGICAL inview

!------------BEGIN-------------

    IF ( (c .ne. 0) .AND. (ABS(c) .le. NumOfColours).AND. (ListTr(1, T) .ne. 0) )  THEN
      if(ListTr(4,t).gt.0) then
        numfp = 4
      else
        numfp = 3
      endif
      DO j = 1, numfp
        x(j) = dxray(ListTr(j,t))
        y(j) = dyray(ListTr(j,t))
      ENDDO

!         - Following tests added 25 Feb 91 to check if triangle is totally
!         -- above, below, to right or to left of screen window
      inview = .TRUE.
      IF ( (y(1) .gt. cwyh) .AND. (y(2) .gt. cwyh) .AND. (y(3) .gt. cwyh) ) THEN
        inview = .FALSE.
      ENDIF
      IF ( (y(1) .lt. cwyl) .AND. (y(2) .lt. cwyl) .AND. (y(3) .lt. cwyl) ) THEN
        inview = .FALSE.
      ENDIF
      IF ( (x(1) .gt. cwxh) .AND. (x(2) .gt. cwxh) .AND. (x(3) .gt. cwxh) ) THEN
        inview = .FALSE.
      ENDIF
      IF ( (x(1) .lt. cwxl) .AND. (x(2) .lt. cwxl) .AND. (x(3) .lt. cwxl) ) THEN
        inview = .FALSE.
      ENDIF
      IF ( inview ) THEN
        if(numfp.eq.4) then
          xcen = ( x(1) + x(2) + x(3) + x(4) ) / 4.0
          ycen = ( y(1) + y(2) + y(3) + y(4) ) / 4.0
        else
          xcen = ( x(1) + x(2) + x(3) ) / 3.0
          ycen = ( y(1) + y(2) + y(3) ) / 3.0
        endif
!           - check if full colour or spot colour shading required
        IF ( FullColour ) THEN
          call PigFillPly( numfp, x, y, IABS(c) )
        ELSE
          IF ( c .gt. 0 ) THEN
        IF ( in_box(xcen,ycen) ) THEN
          IF ( Colour(c) .ne. 'NONE' ) THEN
            call PutMarker( xcen, ycen, 3, c )
          ENDIF
        ENDIF
          ENDIF
        ENDIF               
!             - ( FullColour )
      ENDIF
!           - ( inview )
    ENDIF

    RETURN
    END

!---------------------------------------------------------------------------*
    SUBROUTINE THiOn

    LOGICAL TrHiOff
    COMMON /TH/ TrHiOff
      
    TrHiOff = .FALSE.
          
    RETURN    
    END       

!---------------------------------------------------------------------------*
    SUBROUTINE THiOff
 
    LOGICAL TrHiOff
    COMMON /TH/ TrHiOff
      
    TrHiOff = .TRUE.
          
    RETURN    
    END

!---------------------------------------------------------------------------*


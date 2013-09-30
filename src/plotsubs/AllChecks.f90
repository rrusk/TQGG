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
!---------------------------------------------------------------------------*
!     This module contains the dialogs associated with the                  *
!     quality tests for elements.                                           *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
      
      SUBROUTINE FlagsTriangles_Init()

! Purpose: To set up dialog for FlagsTriangles routine.
! Givens : None
! Returns: None
! Effects: Dialog is set up with options to be used by FlagsTriangles routine.

      use MainArrays

      implicit none

!---------- BEGIN --------------

      call WPigElementCheck ()

      END

!---------------------------------------------------------------------------*

      subroutine ElementCheck( ntest, cmode )
      
! Purpose: Entry point for element tests and coloring.
! Givens : number of test and coloring mode.
! Returns: None
! Effects: Results of test displayed in color with grid.

      use MainArrays, only: TotTr,CritLt

      implicit none
      
!     - PASSED PARAMETERS
      integer :: ntest
      logical :: cmode

!     - COMMON AREAS
      LOGICAL fullcolour
      COMMON /SHADE/ fullcolour
      integer Test
      COMMON /CRITER/ Test
      LOGICAL TrHiOff
      COMMON /TH/ TrHiOff

! Local Variables
      LOGICAL :: Start=.TRUE.
      CHARACTER*256 fle
      integer j, nunit,istat !,Fnlen
      logical PigOpenFileCD
      
!--------------BEGIN-------------

      test = ntest
      fullcolour = cmode
      TrHiOff = .false.

      IF (start) THEN
        call Nup_Fparams()
        start = .FALSE.
      ENDIF
      
      if(test.eq.7) then
        if(.not.PigOpenFileCD(nunit,'Open Ext File', fle,&
     &     'External criteria (*.dat),*.dat;All Files (*.*),*.*;')) then
!          fnlen = len_trim( Fle )
!          call PigMessageOK('Error opening file '//fle(:fnlen),'OpenExt')
          return
        endif
        
        do j=1,TotTr
          read(nunit,*,IOSTAT=istat) CritLt(j)
          if(istat.lt.0) then
            call PigMessageOK('ERROR premature end of file','ReadExt')
            close(nunit)
            return
          elseif(istat.gt.0) then
            call PigMessageOK('ERROR reading Ext file: Most likely a format error','ReadExt')
            close(nunit)
            return
          endif 
        enddo
        close(nunit)     
      endif

      call ReDrawOnly()

      return
      end

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!     This module contains the dialogs associated with the              *
!     criteria tests for vertices.                                          *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE FlagsVertices()

! Purpose: Dispatch routine for the placement and deletion of vertice markers.
! Givens : CHANGE - TRUE IF triangle list needs updating.
! Returns: None
! Effects: None

      IMPLICIT NONE
      
      INCLUDE 'critcom.inc'
      
      logical ans

      DrawVCrit = .true.

      call WPigNodeCheck(ans, TheCriteria, MaxCrit)
      
      END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!                         NUP.FOR                                           *
!     This module is concerned with the colouring of triangles              *
!     by criteria tests, from  a colour table.                              *
!---------------------------------------------------------------------------*

      SUBROUTINE NUP_FPARAMS()

! Purpose : Equivalent to NUP_PARAMS
! Effects : resets number of interval points for triangle test(s);
!           resets the interval point values for triangle test(s)

      implicit none

      INCLUDE 'noticom.inc'
      INCLUDE '../includes/graf.def'

      integer iTest, i
!      COMMON /CRITER/Test
      
      REAL Default_Interval(1:NumOfTests,1:NumOfBndryPts)
      integer Default_TestColour(1:NumOfTests,0:NumofColours)
      INTEGER Default_NumTest(1:NumOfTests)

      data Default_Numtest/3,5,4,1,1,15,15/
      data (Default_TestColour(1,I),I=0,15)/black,blue,yellow,red,12*black/
      data (Default_TestColour(2,I),I=0,15) /red,yellow,green,cyan,blue,violet,10*black/
      data (Default_TestColour(3,I),I=0,15) /blue,cyan,green,yellow,red,11*black/
      data (Default_TestColour(4,I),I=0,15)/red,15*black/
      data (Default_TestColour(5,I),I=0,15)/red,15*black/
      data (Default_TestColour(6,I),I=0,15) /black,red,yellow,green,cyan,blue,violet,dkcyan,&
                                   dkgreen,vioblue,orange,dkgray,dkblue,ltgray,dkred,white/
      data (Default_TestColour(7,I),I=0,15)/black,red,dkgreen,blue,dkcyan,yellow,dkblue,cyan,&
                                   vioblue,green,orange,dkgray,ltgray,dkred,violet,white/

      data (Default_Interval(1,I),I=1,15)/1.2,1.4,2.0,0.,0.,10*0./
      data (Default_Interval(2,I),I=1,15)/10.,20.,30.,40.,50.,10*0./
      data (Default_Interval(3,I),I=1,15)/.003,.005,.007,.008,0.,10*0./
      data (Default_Interval(4,I),I=1,15)/0.0,0.0,0.0,0.0,0.0,10*0./
      data (Default_Interval(5,I),I=1,15)/0.0,0.0,0.0,0.0,0.0,10*0./
      data (Default_Interval(6,I),I=1,15)/0.1,1.1,2.1,3.1,4.1,5.1,9*0./ !not used
      data (Default_Interval(7,I),I=1,15)/0.0,0.0,0.0,0.0,0.0,10*0./ !not used

!-----------BEGIN-------------

      DO iTest = 1 , NumOfTests
        Numtest(iTest) = Default_Numtest(iTest)
        do i=1,Numtest(iTest)
          Interval(iTest,i) = Default_Interval(iTest,i)
        enddo
        do i=0,Numtest(iTest)
          TestColour(iTest,i) = Default_TestColour(iTest,i)
        enddo
! 
!        call PigPutMessage('Using default color scale for this test..')
      enddo

      return
      END

!---------------------- END NUP.FOR ----------------------------------------*

!---------------------------------------------------------------------------*
!                            TRIPROP.FOR                                    *
!       This module is the triangle data manipulation module.contains real  *
!      functions CalEqlat, CalDepth, TrPerim, CalA2D, TrDepth, TrArea       *
!---------------------------------------------------------------------------*

    REAL FUNCTION CalCrit( Crit, T, Status)

! Purpose : To dispatch the correct criteria test
! Given   : A criteria index to a triangle.

    integer   Crit, T
    integer   Status  !, Eqlat, Dep, A2D, CCW, G90, TriCode
    integer, PARAMETER :: Eqlat = 1, Dep = 2, A2D = 3, CCW = 4, G90 = 5, TriCode = 6
    REAL CalA2D, CalEqlat, CalDepth, CalCCW, CalG90, CalTCode

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
! Returns:The area / mean depth ratio for the triangle
!         and a status code:  1 = infinity

    integer T, Stat
    REAL Depth, Trdepth, TrArea

    Depth = Trdepth( T)
    IF ( Depth .le. 0) THEN
      CalA2D = 0.
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

    integer T

    TrDepth = ( DEPTH(ListTr(1,T)) + DEPTH(ListTr(2,T)) + DEPTH(ListTr(3,T)) ) / 3
    
    END

!---------------------------------------------------------------------------*
    REAL FUNCTION TrArea(T)

! Given  : An index to a triangle in ListTr
! Returns: The area of the indexed triangle

    use MainArrays

    integer T

    REAL X(3), Y(3)
    integer I

    DO I = 1, 3
      X(I) = DXRAY(ListTr(I,T))
      Y(I) = DYRAY(ListTr(I,T))
    enddo
    TrArea = 0.5 * ABS( (X(1)-X(3))*(Y(2)-Y(3)) -(X(2)-X(3))*(Y(1)-Y(3)) )       
    
    END

!---------------------------------------------------------------------------*

    REAL FUNCTION CalEqlat( T, Stat)

! Given  : An index to a triangle
! Returns:The equilateral calculation of a triangle
!          and a status code:  1 = infinity

    integer T, Stat
    REAL Denom, TrArea, TrPerim
    real, Parameter :: FourRootThree=6.928032

    Denom = TrArea( T) * FourRootThree
    IF ( Denom .eq. 0 ) THEN
      CalEqlat = 0.
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
! Returns:the mean depth of the triangle
!         and a status code:  1 = "infinite" depth or depth < 1.0

    integer      T, Stat
    REAL Depth, Trdepth

    Depth = Trdepth( T)
    IF ( Depth .gt. 99999 .or. Depth .lt. -1) THEN
      CalDepth =  0.
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
!  Effects : Displays element tests

    use MainArrays

    implicit none
    
! Passed parameters
    LOGICAL CHANGE

    INCLUDE 'noticom.inc'

    LOGICAL TrHiOff
    COMMON /TH/ TrHiOff

    integer Test
    COMMON /CRITER/Test

! Local Parameters
    LOGICAL NeedNewListTr
    integer MaxCrit, S
    integer Tr,Cr,I
    REAL TVal, CalCrit
    logical  :: start=.true., retro=.false.

!------------BEGIN-------------

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
      if(change) then
        call RemoveNotExist(itot,code,nbtot,nl)
        call Element_Lister(CHANGE,retro,itot,nbtot,dxray,dyray,depth,&
             nl,TotTr,ListTr,Tcode,x0off,y0off,scaleX,scaleY,igridtype)
        change = .false.
      endif

! Following 'if' structure added to fix external file criterion problem
      if ((Cr.eq.MaxCrit).and.(NeedNewListTr)) then
         TrHiOff = .TRUE.
         call PigPutMessage('WARNING - External criterion file'// &
                 ' is no longer valid.. Grid has been changed.')
      else
        DO Tr = 1, TotTr
          IF(Cr.GE.0.and.Cr.LT.MaxCrit) then
            TVal = CalCrit(Cr,Tr,S)
          ELSE IF(Cr.EQ.MaxCrit) then
            TVal = CritLt(Tr)
          ELSE
            TVal = 0
          ENDIF

          IF((Cr.eq.MaxCrit).and.(TVal.eq.999999.)) cycle
          IF(Cr.ge.MaxCrit-1) then
            I = mod(NINT(Tval),16)
            CALL ShdTr(Tr,TestColour(Cr,I))
          ELSEIF( TVal .lt. Interval(Cr,1)) THEN
            CALL ShdTr(Tr,TestColour(Cr,0))
          ELSEIF( TVal .ge. Interval(Cr,NumTest(Cr))) THEN
            CALL ShdTr(Tr,TestColour(Cr,NumTest(Cr)))
          ELSE
            DO I = 1, NumTest(Cr)-1
              IF( TVal .ge. Interval(Cr,I) .and. TVal .lt. Interval(Cr,I+1) ) THEN
                CALL ShdTr(Tr,TestColour(Cr,I))
              ENDIF
            enddo
          ENDIF
        enddo
      endif
    ENDIF

21  CONTINUE
    
    END

!---------------------------------------------------------------------------*
    
      SUBROUTINE ShdTr( t, c )

!  Purpose : To colour shade the triangles by criteria.
!  Given   : Triangle number and colour code.
!  Returns : None
!  Effect  : Triangles matching a specified criteria are coloured.

      use MainArrays

      implicit none
      
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
!           - check if full colour or spot colour shading required
        IF ( FullColour ) THEN
          call PigFillPly( numfp, x, y, IABS(c) )
        ELSE
          IF ( c .gt. 0 ) THEN
            if(numfp.eq.4) then
              xcen = ( x(1) + x(2) + x(3) + x(4) ) / 4.0
              ycen = ( y(1) + y(2) + y(3) + y(4) ) / 4.0
            else
              xcen = ( x(1) + x(2) + x(3) ) / 3.0
              ycen = ( y(1) + y(2) + y(3) ) / 3.0
            endif
            IF ( in_box(xcen,ycen) ) THEN
!              IF ( Colour(c) .ne. 'NONE' ) THEN
              call PutMarker( xcen, ycen, 3, IABS(c) )
!              ENDIF
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
!    Section for Vertex checks or tests - part of info routines
!---------------------------------------------------------------------------*

      SUBROUTINE Depth_Greater( nrec )

! Purpose : To mark points that have DEPTH >= user supplied
!           number
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i
!        - number that user enters for comparison test
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!--------------BEGIN--------------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
          if ( DEPTH(i) .gt. USER_COUNT1 ) then
!              *put a marker at that depth
             if ( IN_BOX(DXRAY(i),DYRAY(i)) ) THEN
              call PUTMARKER( DXRAY(i), DYRAY(i), 4, cdgt )
             endif
          endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      
      SUBROUTINE Depth_Less( nrec )

! Purpose : To mark points that have DEPTH <= user supplied
!           number
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i
!        - number that user enters for comparison test
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN----------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
          if ( DEPTH(I) .lt. USER_COUNT ) then
!              *put a marker at that depth
             if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
              call PUTMARKER( DXRAY(i), DYRAY(i), 4, cdlt )
             endif
          endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      
      SUBROUTINE Depth_Between( nrec )

! Purpose : To mark points with LOWERD<= DEPTH <= UPPERD
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i
!        - numbers that user enters for comparison test
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN--------------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
          if ( (DEPTH(i) .ge. LOWERD) .AND.(DEPTH(i) .le. UPPERD) ) then
!              *put a marker at the vertex
             if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
              call PUTMARKER( DXRAY(i), DYRAY(i), 4, cdbtw )
             endif
          endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      
      SUBROUTINE Mark_Nbs_Over( nrec )

! Purpose : To mark points that have more than 'n' number
!           of neighbors
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE 'critcom.inc'
      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i, j
!       - counters
      integer nb_count
!       - number of neighbors for any record
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN-----------------

      do 1000 i = 1, NREC
       if ( code(i).ge.0 ) then
       nb_count = 0
       do 900 j = 1, NBTOTR
          if ( NL(j,i) .ne. 0 ) then
             nb_count = nb_count + 1
          endif
900      continue
       if ( nb_count .gt. USER_NCOUNT ) then
          if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
             call PUTMARKER( DXRAY(i), DYRAY(i), 4, cnbgt )
          endif
       endif
       endif
1000  continue

      RETURN
      END

!---------------------------------------------------------------------------*
      
      SUBROUTINE Mark_Nbs_Equal( nrec )

! Purpose : To mark points that have 'n' neighbors
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE 'critcom.inc'
      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i, j
!       - counters
      integer nb_count
!       - number of neighbors for any record
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
       nb_count = 0
       do 200 j = 1, NBTOTR
          if ( NL(j,i) .ne. 0 ) then
             nb_count = nb_count + 1
          endif
200      continue
       if ( nb_count .eq. USER_NCOUNT2 ) then
          if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
             call PUTMARKER( DXRAY(i), DYRAY(i), 4, cnbe )
          endif
       endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      
      SUBROUTINE Mark_Nbs_Under( nrec )

! Purpose : To mark points that have less than 'n' number
!           of neighbors
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i, j
!       - counters
      integer nb_count
!       - number of neighbors for any record
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN------------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
       nb_count = 0
       do 200 j = 1, NBTOTR
          if ( NL(j,i) .ne. 0 ) then
             nb_count = nb_count + 1
          endif
200      continue
       if ( nb_count .lt. USER_NCOUNT1 ) then
          if ( IN_BOX(DXRAY(i),DYRAY(i)) ) THEN
             call PUTMARKER( DXRAY(i), DYRAY(i), 4, cnblt)
          endif
       endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
!                       END CRITERIA.FOR                                    *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!              VERTMARK.FOR                     *
!     This module contains the subroutines that control the vertex      *
!     markers.                                  *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
      
      SUBROUTINE InitVertexMarkers

! Purpose: Initialize all vertex markers to OFF
! Givens : None
! Returns: None
! Effects: Common Block VerControl in 'critcom.inc' is initialized.

      INCLUDE 'critcom.inc'

! Local variables
      integer i

! Turn off all criteria
      do 10 i = 1, MaxCrit
     TheCriteria(i) = .FALSE.
10    continue

! Turn off criteria drawing
      !call VMarkOff
      DrawVCrit = .FALSE.

      RETURN
      END


!---------------------------------------------------------------------------*
      
      SUBROUTINE VMarkOn0

! Purpose: Turn on Vertex markers.
! Givens : None
! Returns: None
! Effects: Vertex marker drawing is enabled.

      INCLUDE 'critcom.inc'

      DrawVCrit = .TRUE.

      RETURN
      END


!---------------------------------------------------------------------------*
      
      SUBROUTINE VMarkOff

! Purpose: Turn on Vertex markers.
! Givens : None
! Returns: None
! Effects: Vertex marker drawing is disabled.

      INCLUDE 'critcom.inc'

      DrawVCrit = .FALSE.

      RETURN
      END


!---------------------------------------------------------------------------*
      
      SUBROUTINE DrwVertMarkers()

! Purpose : Draw vertex markers using specified criterion.
! Givens  : nrec - number of points in the current grid
!       All required information in common blocks.
! Returns : None
! Effects : Markers are drawn according to the specified criterion

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/defaults.inc'
      INCLUDE 'critcom.inc'
      INCLUDE 'critcol.def'

!     - PASSED PARAMETERS
      integer nrec

!     - LOCAL VARIABLES
      integer i
      LOGICAL In_Box

!-----------------BEGIN------------------

!     - Perform vertex marking only if vertex marking has been turned on.
      if ( .NOT. DrawVCrit ) goto 999

      nrec = itot

      if (TheCriteria(15)) then
!   - EXT, put markers at vertices specified in external file
!       -- each record of which contains vertex number followed by
!       -- number of colour required. Record format is (1X,I5,1X,I2)
!    call EXTERNAL
      else
    if ( TheCriteria(1) ) then
!     - C0, code = 0
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 0 ) then
!       - put a marker wherever the code is 0
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc0 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 0 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(1) )

    if ( TheCriteria(8) ) then
!     - !C0, code not equal to 0
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .gt. 0 ) then
!           - put a marker wherever the code is NOT 0
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cnotc0 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) gt 0 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec)
    endif
!     - ( TheCriteria(8) )

    if ( TheCriteria(2) ) then
!     - C1, code = 1
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 1 ) then
!       - put a marker wherever the code is 1
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PUTMARKER ( DXRAY(I), DYRAY(I), 4, cc1 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 1 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(2) )

    if ( TheCriteria(3) ) then
!         - C2, code = 2
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 2 ) then
!       - put a marker wherever the code is 2
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker ( DXRAY(i), DYRAY(i), 4, cc2 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 2 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( (TheCriteria(3) )

    if ( TheCriteria(4) ) then
!     - C3, code = 3
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 3 ) then
!       - put a marker wherever the code is 3
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc3 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 3 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(4) )

    if ( TheCriteria(5) ) then
!     - C4, code = 4
      do I = 1, NREC
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 4 ) then
!       - put a marker wherever the code is 4
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc4 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 4 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(5) )

    if ( TheCriteria(6) ) then
!     - C5, code = 5
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 5 ) then
!       - put a marker wherever the code is 5
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc5 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 5 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(6) )

    if ( TheCriteria(7) ) then
!     - C6, code = 7
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 7 ) then
!       - put a marker wherever the code is 6
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc6 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 7 )
        endif
!         - code(i).ge.0
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(7) )

!   - Mar/91 (JDM), new check added for TheCriteria(16)
    if ( TheCriteria(16) ) then
!     - C=?, code = USER_CODE
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. USER_CODE ) then
!       - put a marker wherever the code is USER_CODE
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, ccuser )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq USER_CODE )
        endif
!         - code(i).ge.0
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(16) )

    if ( TheCriteria(10) ) then
!     - DGT, depth greater than USER_COUNT
      call Depth_Greater( nrec )
    endif

    if ( TheCriteria(9) ) then
!     - DLT, depthless than USER_COUNT
        call Depth_Less( nrec )
    endif

    if ( TheCriteria(11) ) then
!     - DBTW, depth between UPPERD and LOWERD
      call Depth_Between( nrec )
    endif

    if ( TheCriteria(12) ) then
!     - NBGT, neighbours greater than USER_NCOUNT
      call Mark_Nbs_Over( nrec )
    endif

    if ( TheCriteria(14) ) then
!     - NBE, neighbours equal to USER_NCOUNT
      call Mark_Nbs_Equal( nrec )
    endif

    if ( TheCriteria(13) ) then
!     - NBLT, neighbours less than USER_NCOUNT
      call Mark_Nbs_Under( nrec )
    endif

      endif
!   - ( TheCriteria(15) )
999   continue

      RETURN
      END

!---------------------------------------------------------------------------*


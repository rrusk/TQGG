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
      
!      logical ans

      DrawVCrit = .true.

      call WPigNodeCheck()
      
      END
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!     This module contains the dialogs associated with the              *
!     boundary tests                                          *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE BoundaryCheck()

! Purpose: Check boundaries for orientation and errors.
! Returns: None
! Effects: Display boundary info

      USE MainArrays

      IMPLICIT NONE


! - LOCAL VARIABLES
      integer ierr,i2,tmpc(100000), iouter
      integer istrt, iend,i,j,k,tmpi
      logical bndCW(TotBndys)
      logical flipIsl,closedIsl,OK
      real bndsum, tmpx(100000),tmpy(100000),tmpd(100000)
      integer imin
      CHARACTER*80 cstr, ans
      CHARACTER*1 PigCursYesNo

!*-------------------START ROUTINE-------------------------------------

      ierr = 0
      i2 = 2



!     Find the outer boundary and make sure that it is first in the arrays
      imin = MINLOC(dyray(1:TotCoords),1)

      IF( (imin.le.PtsThisBnd(1)).and.(imin.gt.0) ) THEN !Bnd is the first bnd
        call PigStatusMessage('First boundary is outer boundary.')
!       Set iouter to contain the index of the outer boundary
        iouter = 1
     
      ELSE ! The outer bnd is not the first boundary - ask about moving it
        cstr = 'Outer boundary is not the first boundary. Move to first posistion?'
        ans = PigCursYesNo (cstr)
        IF ( ans(1:1) .eq. 'Y' ) then ! Move it

!         Loop through bnd to find the correct segment to be moved move it
          iend = 0
          DO i=1,TotBndys
!           Set start and end of bnd
            istrt = iend+1
            iend = istrt+PtsThisBnd(i)-1
!           Break the loop if we are on the right segment
            IF ((imin.ge.istrt).and.(imin.le.iend)) THEN
              EXIT
            END IF
          END DO

!         Now we have i, istrt and iend of the segment that should be moved to start
!         Using imin to hold number of nodes in the bnd
          imin = iend - istrt + 1

!         Put bnd in tmp arrays
          do k=istrt,iend 
            tmpi = k-istrt+1
            tmpx(tmpi) = dxray(k)
            tmpy(tmpi) = dyray(k)
            tmpd(tmpi) = depth(k)
            tmpc(tmpi) = code(k)
          end do

!         Shift arrays to close the gap
          do k=istrt-1,1,-1 
            dxray(k+imin) = dxray(k)
            dyray(k+imin) = dyray(k)
            depth(k+imin) = depth(k)
            code(k+imin) = code(k)
          end do

!         Insert the tmp array in front
          do k = 1,imin
            dxray(k) = tmpx(k)
            dyray(k) = tmpy(k) 
            depth(k) = tmpd(tmpi)
            code(k) = tmpc(tmpi)
          end do

!         Shift and update PtsThisBnd
          do k=i,2,-1
            PtsThisBnd(k) = PtsThisBnd(k-1)
          end do
          PtsThisBnd(1) = imin
!         Set iouter to contain the index of the outer boundary
          iouter = 1

        ELSE  ! User does not want to move bnd to to start

!         Find index of the outer boundary
          iend = 0
          DO i=1,TotBndys
!           Set start and end of bnd
            istrt = iend+1
            iend = istrt+PtsThisBnd(i)-1
!           Break the loop if we are on the right segment
            IF ((imin.ge.istrt).and.(imin.le.iend)) THEN
              iouter = i
              EXIT
            END IF
          END DO

        END IF ! Move?
      END IF ! Bnd is first bnd?
        



!     Check for bnd with less than three point and ask to delete them

      imin = MINVAL(PtsThisBnd(1:TotBndys),1)
      IF (imin.lt.3) THEN ! There are bnd with less points than three
        cstr = 'There are boundaries with less points than 3. Delete them?'
        ans = PigCursYesNo (cstr)

        IF ( ans(1:1) .eq. 'Y' ) then ! Delete them
          iend = 0
          DO i=1,TotBndys
!           Set start and end of bnd
            istrt = iend + 1
            iend = istrt+PtsThisBnd(i)-1

            IF (PtsThisBnd(i).lt.3) THEN ! Delete the current boundary
              CALL DelIslUpdate ( i, OK, .false. )
            END IF

          END DO
        END IF
      END IF







!     Loop over bnds to determine direction
      iend = 0
      flipisl = .false.
      do i=1,TotBndys

!       Set start and end of bnd
        istrt = iend + 1
        iend = istrt+PtsThisBnd(i)-1

!       Check if the current bnds orientation
        bndCW(i) = .true.

!       Check bnd orientation
        bndsum = 0
        do j=istrt,iend-1
          bndsum = bndsum + ( dxray(j+1)-dxray(j) )*( dyray(j+1)+dyray(j) )
        end do
        bndsum = bndsum + ( dxray(istrt)-dxray(iend) )*( dyray(istrt)+dyray(iend) )

        if (bndsum.lt.0) bndCW(i) = .false.

!       If this is not the outer boundary and the direction is not CW the island is defined the wrong way around
        if (( i.ne.iouter ).and.( .not.bndCW(i) ).and.( PtsThisBnd(i).gt.2 ))THEN ! Detected a island that is defined the wrong way
          flipIsl = .true.
        end if

      end do


!
!     Now bndCW is true for the bnds that are cw, and false for CCW.
!     READY TO PLOT!
!



!     Determine if outer boundary is defined right
      IF (bndCW(iouter)) THEN ! No - prompt user to reverse
        cstr = 'Outer boundary is defined CW. Reverse?'
        ans = PigCursYesNo (cstr)

        IF ( ans(1:1) .eq. 'Y' ) then ! Reverse outer bnd

!         Define start and end index of the outer boundary
          iend = 0
          DO j=1,iouter
            iend = iend + PtsThisBnd(j)
          END DO
          istrt = iend - PtsThisBnd(iouter) + 1

!         Put bnd in tmp arrays
          do k=istrt,iend 
            tmpi = k-istrt+1
            tmpx(tmpi) = dxray(k)
            tmpy(tmpi) = dyray(k)
            tmpd(tmpi) = depth(k)
            tmpc(tmpi) = code(k)
          end do

!         Put it back reversed
          do k=istrt,iend 
            tmpi = iend-k+1 ! Reverse order
            dxray(k) = tmpx(tmpi)
            dyray(k) = tmpy(tmpi)
            depth(k) = tmpd(tmpi)
            code(k) = tmpc(tmpi) 
          end do

        END IF ! Reverse?
      END IF ! Is bnd CW?


!     If there are reversed islands, ask the user to correct them
      IF (flipIsl) THEN ! There are flipped islands
        cstr = 'Detected islands that are defined CW. Reverse?'
        ans = PigCursYesNo (cstr)

        IF ( ans(1:1) .eq. 'Y' ) then ! Reverse islands
          iend = 0
          DO i=1,TotBndys
!           Set start and end of bnd
            istrt = iend + 1
            iend = istrt+PtsThisBnd(i)-1

            IF (( i.ne.iouter ).and.( .not.bndCW(i) )) THEN ! The current island is reversed

!             Put bnd in tmp arrays
              do k=istrt,iend 
                tmpi = k-istrt+1
                tmpx(tmpi) = dxray(k)
                tmpy(tmpi) = dyray(k)
                tmpd(tmpi) = depth(k)
                tmpc(tmpi) = code(k)
              end do

!             Put it back reversed
              do k=istrt,iend 
                tmpi = iend-k+1 ! Reverse order
                dxray(k) = tmpx(tmpi)
                dyray(k) = tmpy(tmpi)
                depth(k) = tmpd(tmpi)
                code(k) = tmpc(tmpi) 
              end do


            END IF ! Current boundary reversed?
          END DO ! Loop bnds
        END IF ! Reverse islands?
      END IF ! Flipped islands?






!     Reset all nodecodes
      iend = 0
      DO i=1,TotBndys
!       Set start and end of bnd
        istrt = iend + 1
        iend = istrt+PtsThisBnd(i)-1

        IF (i.eq.iouter) THEN
          tmpc(1) = 1
        ELSE
          tmpc(1) = 2
        END IF

        DO j=istrt,iend
          code(j) = tmpc(1)
        END DO

      END DO





!     Check for coincident nodes on boundary start/end
      iend = 0
      closedIsl = .false.
      DO i=1,TotBndys
!       Set start and end of bnd
        istrt = iend + 1
        iend = istrt+PtsThisBnd(i)-1

        tmpx(1) = sqrt( (dxray(iend)-dxray(iend-1))**2 + (dyray(iend)-dyray(iend-1))**2 )
        tmpy(1) = sqrt( (dxray(istrt+1)-dxray(istrt))**2 + (dyray(istrt+1)-dyray(istrt))**2 )
        tmpx(10) = (tmpx(1) + tmpy(1)) / 20 

!       Find distance between ends
        tmpx(20) = sqrt( (dxray(iend)-dxray(istrt))**2 + (dyray(iend)-dyray(istrt))**2 )

        IF (tmpx(20).lt.tmpx(10)) THEN   ! Coincident start and end nodes on islands
          closedIsl = .true.
          EXIT
        END IF  
      END DO

      IF (closedIsl) THEN ! There are closed islands Ask the user to remove coincident nodes
        cstr = 'Detected closed boundaries. Open them up?'
        ans = PigCursYesNo (cstr)

        IF ( ans(1:1) .eq. 'Y' ) then
          istrt = TotCoords+1 
          DO i=TotBndys,1,-1
!           Set start and end of bnd
            iend = istrt-1
            istrt = iend-PtsThisBnd(i)+1

!           Get 10% of average distance at ends of segment
            tmpx(1) = sqrt( (dxray(iend)-dxray(iend-1))**2 + (dyray(iend)-dyray(iend-1))**2 )
            tmpy(1) = sqrt( (dxray(istrt+1)-dxray(istrt))**2 + (dyray(istrt+1)-dyray(istrt))**2 )
            tmpx(10) = (tmpx(1) + tmpy(1)) / 20 

!           Find distance between ends
            tmpx(20) = sqrt( (dxray(iend)-dxray(istrt))**2 + (dyray(iend)-dyray(istrt))**2 )

            IF (tmpx(20).lt.tmpx(10)) THEN   ! Coincident start and end nodes on islands
!             Move all points from iend to TotCoods one step back
              DO k=iend,TotCoords-1
                dxray(k) = dxray(k+1)
                dyray(k) = dyray(k+1)
                depth(k) = depth(k+1)
                code(k) = code(k+1)
              END DO

!             Correct TotCoords and PtsThisBnd
              TotCoords = TotCoords - 1
              PtsThisBnd(i) = PtsThisBnd(i)-1

            END IF



          END DO ! Loop bndys      

        END IF ! Open up islands?
      END IF ! There are closed isl



!     Check for self-intersecting bnd   

      cstr = 'Check for selfintersecting boundaries? (Slow)'
      ans = PigCursYesNo (cstr)

      IF ( ans(1:1) .eq. 'Y' ) then

!       Loop through bnd to find the correct segment to be moved move it
        iend = 0
   
        OK = .true.

        DO i=1,TotBndys
!         Set start and end of bnd
          istrt = iend+1
          iend = istrt+PtsThisBnd(i)-1
          
          CALL simplePolygon(dxray(istrt:iend),dyray(istrt:iend),iend-istrt+1,imin)
!         If imin is returned anything other than 0, then it holds index of one of the intersecting bndsegments points
          IF (imin.ne.0) THEN
            CALL DrawPMark( dxray(imin+istrt-1),  dyray(imin+istrt-1) )
            OK = .false.
          END IF
        END DO

        IF (.not.OK) THEN
          call PigMessageOK('Selfintersecting boundaries found.')
        END IF
      
      END IF


!     CHECKS YET TO BE INCLUDED
!     Determine if there are nodes outside the outer boundary bnd.
!     Check for nodes inside islands



!     End of subroutine
      return
      
      END

!---------------------------------------------------------------------------*
!                         Check for self-intersecting polygon               *
!---------------------------------------------------------------------------*
!     
!     Shamos-Hoey Algorithm from http://geomalgorithms.com/a09-_intersect-3.html#simple_Polygon()

! Purpose: Check for crossing boundaries
! Returns: isSimple (index of intersecting segment point) or 0
! Effects: None

      SUBROUTINE simplePolygon(X,Y,N,isSimple)
!     Input
!       X = x-coordinates of points on polygon
!       Y = y-coordinates of points on polygon
!       N = number of points on polygon

!     Output
!       isSimple - index of point on polygon that has a segment that intersects with another
!                  returns zero if no intersection

!     Input/output variables
      integer N
      real X(N), Y(N)
      integer isSimple

!     Internal variables

!     LS = line segments
      real LSsx(N),LSsy(N),LSex(N),LSey(N),tempx(N)

!     EQ is event queue = all segment endpoints - sorted list
      integer EQ((N)*2)
      integer EQs((N)*2)
      logical EQleft(N*2),EQsreg(N)

!     SL = sweep line list of all the  
      integer IARRsx(N),IARRex(N),i,j,sI,eI,imin,SL(N),SLnum
      real rmin

      logical intersect,OK

! ***********************************************
      isSimple = 0

!     Create list of line segments
      DO i = 1,N-1
        IF(X(i).lt.X(i+1)) THEN ! To find the most left point
          LSsx(i) = X(i)
          LSsy(i) = Y(i)
          LSex(i) = X(i+1)
          LSey(i) = Y(i+1)
        ELSE
          LSsx(i) = X(i+1)
          LSsy(i) = Y(i+1)
          LSex(i) = X(i)
          LSey(i) = Y(i)
        END IF
      END DO
!     Add last segment to close up polygon
      IF(X(N).lt.X(1)) THEN
        LSsx(N) = X(N)
        LSsy(N) = Y(N)
        LSex(N) = X(1)
        LSey(N) = Y(1)
      ELSE
        LSsx(N) = X(N)
        LSsy(N) = Y(N)
        LSex(N) = X(1)
        LSey(N) = Y(1)
      END IF
      

!     Sorted the linesegment start and end-points along the x-axis
!     This part should be excanged because this sorting algorithm is slow


      tempx = LSsx

      DO i =1,N
       IARRsx(i)=i
       IARRex(i)=i
      END DO

!     Sort start points, but only keep index
      DO i=1,N-1

        rmin = 999999 
        DO j=i,N
          IF (tempx(j).lt.rmin) THEN
            rmin = tempx(j)
            imin = j
          END IF
        END DO

!       Minimum found, swap and register
        tempx(imin) = tempx(i)
        tempx(i) = rmin

        j = IARRsx(imin)
        IARRsx(imin) = IARRsx(i)
        IARRsx(i) = j
        
      END DO

!     Sort end points, but only keep index
      tempx = LSex
      DO i=1,N-1

        rmin = 999999 
        DO j=i,N
          IF (tempx(j).lt.rmin) THEN
            rmin = tempx(j)
            imin = j
          END IF
        END DO

!       Minimum found, swap and register
        tempx(imin) = tempx(i)
        tempx(i) = rmin

        j = IARRex(imin)
        IARRex(imin) = IARRex(i)
        IARRex(i) = j
        
      END DO



!     Register all events (start and end of line segments)
      sI = 1
      eI = 1
      i = 1
      EQsreg(1:N) = .false.
      OK = .false.

      DO WHILE (i.le.N*2)

!       Register the end point
        IF (( ( LSex(IARRex(eI)).lt.LSsx(IARRsx(sI)) ).and.EQsreg(IARRex(eI)) ).or.(OK)) THEN
          EQ(i) = i
          EQs(i) = IARRex(eI)
          EQleft(i) = .false.
          eI=eI+1
          i=i+1

!       Register the start point
        ELSE
          EQ(i) = i
          EQs(i) = IARRsx(sI)
          EQleft(i) = .true.
          EQsreg(IARRsx(sI)) = .true. ! To know that starts are registered before ends
          sI=sI+1
          IF (sI.gt.N) THEN
            OK = .true.
            sI = N
          END IF
          i=i+1
        END IF
      END DO


!     Init list of segments that are currently crossed by sweep-line
      SL(1:N) = 0
      SLnum = 0


!     Loop events and keep track of segments that the sweeping line intersects
      DO i=1,N*2

!       Keep track of segments that are intersected with the sweep line
        IF ( EQleft(i) ) THEN ! This is the leftmost end - add the segment
          SLnum = SLnum + 1 
          SL(SLnum) = EQs(i)

!         Check for line intersections with the line allready in the SL list.
          DO j=1,SLnum-1
            CALL LineIntersect(LSsx(SL(SLnum)),LSsy(SL(SLnum)),LSex(SL(SLnum)),LSey(SL(SLnum)), &
                  LSsx(SL(j)),LSsy(SL(j)),LSex(SL(j)),LSey(SL(j)), intersect )
            IF (intersect) THEN ! Intersection detected
              isSimple = EQs(i)
              RETURN
            END IF
          END DO

        ELSE ! Remove line segment from SL list
          OK = .false. ! Have we passed the one to remove?

!         Loop through list to remove the linesegment to be removed
          DO j=1,SLnum-1
            IF ( SL(j).eq.EQs(i) ) THEN
              OK = .true.
            END IF

            IF ( OK ) THEN
              SL(j)=SL(j+1)
            END IF
          END DO

          SLnum = SLnum - 1
        END IF
      END DO
          
!     End rutine        
      RETURN ! isSimple is 0 by default
      END

!---------------------------------------------------------------------------*
!                         Check for self-intersecting polygon               *
!---------------------------------------------------------------------------*

      SUBROUTINE LineIntersect(x1,y1,x2,y2,x3,y3,x4,y4,intersect)

! Purpose: Check if two lines intersect each other.
! Given: x and y coordinates of to lines.
! Returns: intersect (true or false)
! Effects: None      
!     Point 1 and 2 is line 1. Point 2 and 3 is line 2

!     Input/output variables
      real x1,y1,x2,y2,x3,y3,x4,y4
      logical intersect

!     Internal variables
      real a1,a2,b1,b2,xint

      intersect=.false.

!     Check for intersecting bounding boxes first
!     http://martin-thoma.com/how-to-check-if-two-line-segments-intersect/
      IF (.not.( MIN(x1,x2).lt.MAX(x3,x4) ).and. &
         ( MAX(x1,x2).gt.MIN(x3,x4) ).and. &
         ( MAX(y1,y2).lt.MIN(y3,y4) ).and. &
         ( MIN(y1,y2).gt.MAX(y3,y4) )) THEN ! Does not intersect
        intersect=.false.
        RETURN
      END IF

!     IF they share endpoint, it does not count
      IF ( ((x1.eq.x3).and.(y1.eq.y3)).or. &
           ((x1.eq.x4).and.(y1.eq.y4)).or. &
           ((x2.eq.x3).and.(y2.eq.y3)).or. &
           ((x2.eq.x4).and.(y2.eq.y4)) ) THEN
        intersect=.false.
        RETURN
      END IF
           

!     Find line equation variables
      a1 = (y2-y1)/(x2-x1)
      a2 = (y4-y3)/(x4-x3)
      b1 = y1 - a1*x1
      b2 = y3 - a2*x3

!     Return false if slope is equal
      IF (a1.eq.a2) THEN
        intersect=.false.
        RETURN
      END IF

!     Determine crossing point
      xint = (b2-b1)/(a1-a2)

!     If it is within the limits of the segments it is intersecting
      IF ( (xint.gt.MIN(x1,x2)).and. &
           (xint.lt.MAX(x1,x2)).and. &
           (xint.gt.MIN(x3,x4)).and. &
           (xint.lt.MAX(x3,x4)) ) THEN ! It intersects
        intersect=.true.
        RETURN
      ELSE
        intersect=.false.
        RETURN
      END IF

!     End of routine
      RETURN
      END


!---------------------------------------------------------------------------*
!------------- ans, TheCriteria, MaxCrit  ----------------------------------*
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

      if (TheCriteria(19)) then
!   - EXT, put markers at vertices specified in external file
!       -- each record of which contains vertex number followed by
!       -- number of colour required. Record format is (1X,I5,1X,I2)
!    call EXTERNAL
      else
        if ( TheCriteria(1) ) then
!         - C0, code = 0
          do i = 1, nrec
            if ( code(i).eq.0 ) then
!           - put a marker wherever the code is 0
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc0 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(11) ) then
!         - !C0, code not equal to 0
          do i = 1, nrec
            if ( CODE(i) .ne. 0 ) then
!               - put a marker wherever the code is NOT 0
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cnotc0 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(2) ) then
!         - C1, code = 1
          do i = 1, nrec
            if ( CODE(i) .eq. 1 ) then
!           - put a marker wherever the code is 1
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PUTMARKER ( DXRAY(I), DYRAY(I), 4, cc1 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(3) ) then
!         - C1, code = 2
          do i = 1, nrec
            if ( CODE(i) .eq. 2 ) then
!           - put a marker wherever the code is 2
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PUTMARKER ( DXRAY(I), DYRAY(I), 4, cc2 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(4) ) then
!         - C3, code = 3
          do i = 1, nrec
            if ( CODE(i) .eq. 3 ) then
!           - put a marker wherever the code is 3
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc3 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(5) ) then
!         - C4, code = 4
          do I = 1, NREC
            if ( CODE(i) .eq. 4 ) then
!           - put a marker wherever the code is 4
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc4 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(6) ) then
!         - C5, code = 5
          do i = 1, nrec
            if ( CODE(i) .eq. 5 ) then
!           - put a marker wherever the code is 5
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc5 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(7) ) then
!         - C6, code = 6
          do i = 1, nrec
            if ( CODE(i) .eq. 6 ) then
!           - put a marker wherever the code is 6
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc6 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(8) ) then
!         - C7, code = 7
          do i = 1, nrec
            if ( CODE(i) .eq. 7 ) then
!           - put a marker wherever the code is 7
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc7 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(9) ) then
!         - C8, code = 8
          do i = 1, nrec
            if ( CODE(i) .eq. 8 ) then
!           - put a marker wherever the code is 8
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc8 )
              endif
            endif
          enddo
        endif


        if ( TheCriteria(10) ) then
!         - C6, code = 9
          do i = 1, nrec
            if ( CODE(i) .eq. 9 ) then
!           - put a marker wherever the code is 9
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, cc9 )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(12) ) then
!         - C=?, code = USER_CODE
          do i = 1, nrec
            if ( CODE(i) .eq. USER_CODE ) then
!           - put a marker wherever the code is USER_CODE
              if ( In_Box(DXRAY(i),DYRAY(i)) ) then
                call PutMarker( DXRAY(i), DYRAY(i), 4, ccuser )
              endif
            endif
          enddo
        endif

        if ( TheCriteria(13) ) then
!         - DLT, depthless than USER_COUNT
            call Depth_Less( nrec )
        endif

        if ( TheCriteria(14) ) then
!         - DGT, depth greater than USER_COUNT
          call Depth_Greater( nrec )
        endif

        if ( TheCriteria(15) ) then
!         - DBTW, depth between UPPERD and LOWERD
          call Depth_Between( nrec )
        endif

        if ( TheCriteria(16) ) then
!         - NBLT, neighbours less than USER_NCOUNT
          call Mark_Nbs_Under( nrec )
        endif

        if ( TheCriteria(17) ) then
!         - NBGT, neighbours greater than USER_NCOUNT
          call Mark_Nbs_Over( nrec )
        endif

        if ( TheCriteria(18) ) then
!         - NBE, neighbours equal to USER_NCOUNT
          call Mark_Nbs_Equal( nrec )
        endif

      endif

999   continue

      RETURN
      END

!---------------------------------------------------------------------------*

      SUBROUTINE SetUserValue(ntest,check)

! Purpose : To place markers at vertices if certain
!           criteria are satisfied
! Given   : None
! Returns : Ok - TRUE if a redraw is needed

      implicit none

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../plotsubs/critcom.inc'
      INCLUDE '../includes/graf.def'

! critcom.inc:  array TheCriteria(1->MaxCrit) contents
! [1] CO [2] C1 [3] C2 [4] C3 [5] C4 [6] C5 [7] C6 [8] NC0
! [9] DLT [10] DGT [11] DBTW [12] NBGT [13] NBLT [14] NBE [15] EXT

! - PASSED VARIABLES
      integer ntest

! - LOCAL VARIABLES
      integer :: ntest_local
      real rval
      logical Success,check
      character*80 ans

!----------BEGIN----------------------

!      write(*,*) ' ntest,check=',ntest,check
      
      if(ntest.ge.1.and.ntest.le.11) then  !codes
        TheCriteria(ntest) = check
      
      elseif(ntest.eq.12) then !C=?
        TheCriteria(ntest) = check
        if(check) then
          call PigPrompt('Enter integer code:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_CODE = nint(rval)
          endif
        endif

      elseif(ntest.eq.13) then  !DLT
        ntest_local = 9
        TheCriteria(ntest) = check
        if(check) then
          call PigPrompt('Enter depth for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid number','ncheck')
          else
            USER_COUNT=rval
          endif
        endif
        
      elseif(ntest.eq.14) then !DGT
        ntest_local = 10
        TheCriteria(ntest) = check
        if(check) then
          call PigPrompt('Enter depth for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid number','ncheck')
          else
            USER_COUNT1=rval
          endif
        endif
      
      elseif(ntest.eq.15) then !DBTW
        ntest_local = 11
        TheCriteria(ntest) = check
        if(check) then
          call PigPrompt('Enter lower depth for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid number','ncheck')
          else
            lowerd=rval
            call PigPrompt('Enter upper depth for comparison:',ans )
            call PigReadReal( ans, rval, Success )
            IF ( .NOT. Success ) THEN
              call PigMessageOK('Invalid number','ncheck')
            else
              upperd=rval
            endif
          endif
        endif
      
      elseif(ntest.eq.16) then !NLT
        ntest_local = 13
        TheCriteria(ntest) = check
        if(check) then
          call PigPrompt('Enter integer count for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_NCOUNT1 = nint(rval)
          endif
        endif
      
      elseif(ntest.eq.17) then !NGT
        ntest_local = 12
        TheCriteria(ntest) = check
        if(check) then
          call PigPrompt('Enter integer count for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_NCOUNT = nint(rval)
          endif
        endif
      
      elseif(ntest.eq.18) then !NBE
        ntest_local = 14
        TheCriteria(ntest) = check
        if(check) then
          call PigPrompt('Enter integer count for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_NCOUNT2 = nint(rval)
          endif
        endif
      
!      elseif(ntest.eq.19) then !EXT
      
      
      endif

      return
      END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*


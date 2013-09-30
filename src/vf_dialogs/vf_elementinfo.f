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

C*--------------------------------------------------------------------------*
C                           TRIINFO.FOR                                     *
C     This module controls the right hand display panel for the displaying  *
C     of information about triangles and allows the user to change values   *
C     for triangle types, aka material types.                               *
C*--------------------------------------------------------------------------*
C*--------------------------------------------------------------------------*
      SUBROUTINE Init_TriInfo
C
C Purpose : To initialize the area to the right of the grid which
C           is the INFO screen.
C Returns : None.

      INCLUDE '../includes/graf.def'

C     - PASSED VARIABLES

C--------BEGIN--------------
C       - initialize as needed
      return
	END

C*--------------------------------------------------------------------------*

      SUBROUTINE GetTVal_MW_Ehandler(Xinp, Yinp, Index)

C Purpose : To get all the information for a triangle in the grid
C Given   : nrec = the number of points read in from the data file
C           Xinp = x position to locate node
C           Yinp = y position to locate node
C Returns : index = index of the triangle

      use MainArrays

      INCLUDE '../includes/graf.def'
c
C - PASSED VARIABLES
      real xinp, yinp
      integer index
!      integer nrec
!      CHARACTER*3 mmode
C
C *** LOCAL VARIABLES ***
C
      integer j
      real xm0, xm1, ym0, ym1
      real x0, x1, x2, x3, y0, y1, y2, y3
      real delta, da1, da2, da3, da4

C     - get the point
c
        do j=1,TotTr
c
          xm0 = min(dxray(ListTr(1,j)),dxray(ListTr(2,j)))
          xm0 = min(xm0,dxray(ListTr(3,j)))
          xm1 = max(dxray(ListTr(1,j)),dxray(ListTr(2,j)))
          xm1 = max(xm1,dxray(ListTr(3,j)))
          ym0 = min(dyray(ListTr(1,j)),dyray(ListTr(2,j)))
          ym0 = min(ym0,dyray(ListTr(3,j)))
          ym1 = max(dyray(ListTr(1,j)),dyray(ListTr(2,j)))
          ym1 = max(ym1,dyray(ListTr(3,j)))
c
          if(xinp.gt.xm0.and.xinp.lt.xm1.and.
     +       yinp.gt.ym0.and.yinp.lt.ym1) then
c
            x0 = xinp
            y0 = yinp
            X1 = DXRAY(ListTr(1,j))
            X2 = DXRAY(ListTr(2,j))
            X3 = DXRAY(ListTr(3,j))
            Y1 = DYRAY(ListTr(1,j))
            Y2 = DYRAY(ListTr(2,j))
            Y3 = DYRAY(ListTr(3,j))
C
C CHECK TO SEE IF POINT IS INSIDE TRIANGLE
C
C TO DETERMINE IF POINT LIES INSIDE THE TRIANGLE,
C CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE
C
            DA1=ABS((X1-X0)*(Y2-Y0)
     *             -(X2-X0)*(Y1-Y0))
            DA2=ABS((X2-X0)*(Y3-Y0)
     *             -(X3-X0)*(Y2-Y0))
            DA3=ABS((X3-X0)*(Y1-Y0)
     *             -(X1-X0)*(Y3-Y0))
            DA4=ABS((X2-X1)*(Y3-Y1)
     *             -(X3-X1)*(Y2-Y1))
c           INSIDE = .FALSE.
            IF (DA4.GT.0.) THEN
              DELTA=DA1+DA2+DA3-DA4
              IF (DELTA.EQ.0..OR.
     *           (DELTA.NE.0..AND.DELTA.LE.DA4*1.E-6)) THEN
c               INSIDE=.TRUE.
                index = j
		        call PutTriMarker(index)
                call Put_TriVal( index )
                go to 999
              endif
            endif
          endif
        enddo
999   continue
      END

C*--------------------------------------------------------------------------*
	
	SUBROUTINE PutTriMarker(index)

      use MainArrays

      integer index

      INCLUDE '../includes/graf.def'

	real xcent, ycent

	xcent = 0.
	ycent = 0.
        if(ListTr(4,index).gt.0) then
          numcn = 4
        else
          numcn = 3
        endif
        do i=1,numcn
          xcent = xcent + dxray(ListTr(i,index))/float(numcn)
          ycent = ycent + dyray(ListTr(i,index))/float(numcn)
        enddo
	call PutMarker( xcent, ycent, 4, yellow )
	end

C*--------------------------------------------------------------------------*

      SUBROUTINE Put_TriVal( index )

C Purpose : To put the values on the info screen for a
C           specified vertex point
C Given   : index = index to data arrays of point.

      use MainArrays
      
      implicit none

C - PASSED VARIABLES
      integer index

	integer	last_triangle_index
	common	/last_triangle/ last_triangle_index

C---------BEGIN------------------

       last_triangle_index = index
       
       call InitEleInfo(index)
       return 

      END

!*--------------------------------------------------------------------------*
      
      SUBROUTINE InfoTriangle(CHANGE)

! Purpose: Future implementation to display triangle info in the right hand
!          display window.
! Givens : CHANGE
! Returns: CW setup
! Effects: Updates triangle list if CHANGE=TRUE

      use MainArrays

      logical CHANGE

! Update triangle list beforehand.

      if(change) then
          call RemoveNotExist(itot,code,nbtot,nl)
          call Element_Lister(CHANGE, .FALSE. ,
     &          itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode,
     &          x0off,y0off,scaleX,scaleY,igridtype)
        change = .false.
      endif

      call INIT_TriInfo

      END

!*--------------------------------------------------------------------------*

      SUBROUTINE GetElementInfo( index,xc,yc,zc,ec,nv )
!
! Purpose : To get the values for the info dialog for a
!           specified vertex point
! Given   : index = index to data arrays of point.

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer, intent(in) :: index
      integer, intent(out) :: ec,nv(4)
      real, intent(out) :: xc,yc,zc


! - LOCAL VARIABLES
      integer i,numcn

!---------BEGIN------------------

      ec = TCode(index)
      
      xc = 0.
      yc = 0.
      zc = 0.
      if(ListTr(4,index).gt.0) then
        numcn = 4
      else
        numcn = 3
      endif
      do i=1,numcn
        xc = xc + dxray(ListTr(i,index))/float(numcn)
        yc = yc + dyray(ListTr(i,index))/float(numcn)
        zc = zc + depth(ListTr(i,index))/float(numcn)
      enddo

      do i = 1,4
        nv(i) = ListTr(i,index)
      end do

      END

!*--------------------------------------------------------------------------*

      SUBROUTINE SetElementInfo( index,ec )
!
! Purpose : To get the values for the info dialog for a
!           specified vertex point
! Given   : index = index to data arrays of point.

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer, intent(in) :: index,ec

!---------BEGIN------------------

      TCode(index) = ec

      END

!-----------------------------------------------------------------------*
!                       END INFO.FOR                                    *
!-----------------------------------------------------------------------*

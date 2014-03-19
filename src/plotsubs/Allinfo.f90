  !-**********************************************************************
  !    Copyright (C) 1995-
  !        Roy A. Walters, R. Falconer Henry
  !
  !        TQGridGen@gmail.com
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
  !-**********************************************************************

!---------------------------------------------------------------------------*
!                           TRIINFO.FOR                                     *
!     This module controls the right hand display panel for the displaying  *
!     of information about triangles and allows the user to change values   *
!     for triangle codes, aka material types.                               *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE LocateElement(Xinp, Yinp, Index, ierr)

! Purpose : To get all the information for a triangle in the grid
! Given   : Xinp = x position to locate node
!           Yinp = y position to locate node
! Returns : index = index of the triangle

      use MainArrays
      
      implicit none

! - PASSED VARIABLES
      real, intent(in) :: xinp, yinp
      integer, intent(out) :: index, ierr

! *** LOCAL VARIABLES ***
      integer j
      real xm0, xm1, ym0, ym1
      real x0, x1, x2, x3, y0, y1, y2, y3
      real delta, da1, da2, da3, da4

!     - get the element

        ierr = 1
        do j=1,TotTr

          xm0 = min(dxray(ListTr(1,j)),dxray(ListTr(2,j)))
          xm0 = min(xm0,dxray(ListTr(3,j)))
          xm1 = max(dxray(ListTr(1,j)),dxray(ListTr(2,j)))
          xm1 = max(xm1,dxray(ListTr(3,j)))
          ym0 = min(dyray(ListTr(1,j)),dyray(ListTr(2,j)))
          ym0 = min(ym0,dyray(ListTr(3,j)))
          ym1 = max(dyray(ListTr(1,j)),dyray(ListTr(2,j)))
          ym1 = max(ym1,dyray(ListTr(3,j)))

          if(xinp.gt.xm0.and.xinp.lt.xm1.and.yinp.gt.ym0.and.yinp.lt.ym1) then

            x0 = xinp
            y0 = yinp
            X1 = DXRAY(ListTr(1,j))
            X2 = DXRAY(ListTr(2,j))
            X3 = DXRAY(ListTr(3,j))
            Y1 = DYRAY(ListTr(1,j))
            Y2 = DYRAY(ListTr(2,j))
            Y3 = DYRAY(ListTr(3,j))

! CHECK TO SEE IF POINT IS INSIDE TRIANGLE

! TO DETERMINE IF POINT LIES INSIDE THE TRIANGLE,
! CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE

            DA1=ABS((X1-X0)*(Y2-Y0) -(X2-X0)*(Y1-Y0))
            DA2=ABS((X2-X0)*(Y3-Y0) -(X3-X0)*(Y2-Y0))
            DA3=ABS((X3-X0)*(Y1-Y0) -(X1-X0)*(Y3-Y0))
            DA4=ABS((X2-X1)*(Y3-Y1) -(X3-X1)*(Y2-Y1))
!           INSIDE = .FALSE.
            IF (DA4.GT.0.) THEN
              DELTA=DA1+DA2+DA3-DA4
              IF (DELTA.EQ.0..OR.(DELTA.NE.0..AND.DELTA.LE.DA4*1.E-6)) THEN
!               INSIDE=.TRUE.
                index = j
!                call PutTriMarker(index)
!                call WPigElementInfo(index)
                ierr = 0
                go to 999
              endif
            endif
          endif
        enddo
999   continue
      END

!---------------------------------------------------------------------------*

      SUBROUTINE PutNodeMarker(index)

      use MainArrays
      
      implicit none

      integer, intent(in) :: index

      INCLUDE '../includes/graf.def'

      real xcent, ycent

      xcent = dxray(index)
      ycent = dyray(index)
            
      call PutMarker( xcent, ycent, 4, yellow )
      
      end

!---------------------------------------------------------------------------*

      SUBROUTINE PutTriMarker(index)

      use MainArrays
      
      implicit none

      integer, intent(in) :: index

      INCLUDE '../includes/graf.def'

      integer i, numcn
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

!---------------------------------------------------------------------------*

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

      if(index.gt.0.and.index.le.TotTr) then
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

      else
        call PigMessageOK('Invalid element index','GetElementInfo')
        ec = -999      
        xc = 0.
        yc = 0.
        zc = 0.
        nv = 0
      endif

      END

!---------------------------------------------------------------------------*

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

      if(index.gt.0.and.index.le.TotTr) then
        TCode(index) = ec
      else
        call PigMessageOK('Invalid element index','SetElementInfo')
      endif

      END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
      
      SUBROUTINE GetNodeInfo( index,xc,yc,zc,ec,numngh,nv )
!
! Purpose : To get the values for the info dialog for a
!           specified vertex point
! Given   : index = index to data arrays of point.

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer, intent(in) :: index
      integer, intent(out) :: ec,numngh,nv(nbtotr)
      real, intent(out) :: xc,yc,zc


! - LOCAL VARIABLES
      integer i

!---------BEGIN------------------

      if(index.gt.0.and.index.le.itot) then
        xc = dxray(index)
        yc = dyray(index)
        zc = depth(index)
        ec = Code(index)
        numngh = 0
        nv = 0
        do i = 1,nbtotr
          if(NL(i,index).gt.0) then
            if(code(NL(i,index)).ne.-9) then
              numngh = numngh + 1
              nv(numngh) = NL(i,index)
            endif
          endif
        end do
      else
        call PigMessageOK('Invalid node index','GetNodeInfo')
        xc = 0.
        yc = 0.
        zc = 0.
        ec = -999
        numngh = 0
        nv = 0
      endif

      END

!---------------------------------------------------------------------------*

      SUBROUTINE SetNodeInfo( index,ec,zc )
!
! Purpose : To get the values for the info dialog for a
!           specified vertex point
! Given   : index = index to data arrays of point.

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer, intent(in) :: index,ec
      real, intent(in) :: zc

!---------BEGIN------------------

      if(index.gt.0.and.index.le.itot) then
        depth(index) = zc
        Code(index) = ec
      else
        call PigMessageOK('Invalid node index','SetNodeInfo')
      endif

      END

!-----------------------------------------------------------------------*
!                       END INFO                                        *
!-----------------------------------------------------------------------*
!-----------------------------------------------------------------------*

  !***********************************************************************
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
  !***********************************************************************

!***********************************************************************

    Module MainArrays

!     Variable names

!  maxnpts   - maximum no. of vertices (nodes) allowed in grid
!  maxNBTOT  - max. no. of neighbours allowed for each node

!    Associated variables which are used in routines:
!         NREC   - actual no. of vertices in grid
!         NBTOTR - actual max. no. of neighbours

!  (I        - vertex index number      I = 1,..,NREC)
!  (J        - neighbour number         J = 1,..,NBTOTR)
!  DXRAY(I) - x-coord. of vertex I 
!  DYRAY(I) - y-coord.
!  CODE(I)  - computational code
!  DEPTH(I) - water depth at vertex
!  NB(J,I) - index nos. of neighbours
!  ITOT = NREC 

      integer, parameter :: maxnpts=1000000, nbtot=20
      integer, parameter :: mrec=maxnpts, maxtri=2*maxnpts+1
      integer, parameter :: MAXSTRNODES = 60000, MAXMARKS = 50
      integer, parameter :: maxpts = mrec
      
      INTEGER ITOT  
      INTEGER NBTOTR

      INTEGER NL(NBTOT,maxnpts+1)
      REAL    DEPTH(maxnpts+1)
!      LOGICAL EXIST(maxnpts+1)
      INTEGER CODE(maxnpts+1)
      REAL DXRAY(maxnpts+4)
      REAL DYRAY(maxnpts+4)

!-----------------------------------------------------------------------*
! - DEFINITIONS
!       TotBndys = total # of boundaries in data.
!       PtsThisBnd(Maxnnb) = # of points on a given boundary in data.
!       Maxnnb = max # of boundaries allowed 
!       TotIntPts = total # of interior ( non-boundary ) points in data.
!       TotCoords = total # of coordinates ( boundary & interior )
!                   in data.
!-----------------------------------------------------------------------*

      integer, parameter :: maxnnb=10000
      integer PtsThisBnd(Maxnnb), BndryIndex(Maxnnb)
      integer TotBndys,TotIntBndys,TotIntPts,TotCoords

!-----------------------------------------------------------------------*

! - DEFINITIONS
!        x0off = offset in longitude for grid file
!        y0off = offset in latitude for grid file
!        xlong0 = longitude of display center for polar coordinates
!-----------------------------------------------------------------------*
      real*8 x0off, y0off, scaleX, scaleY
      real xlong0, xlongsum, xlongmin, xlongmax
      integer :: igridtype,izup,iUTMzone=0
      character(3) :: UTMzone='   '
!      common /polarxf/ x0off,y0off,xlong0,xlongsum,xlongmin,xlongmax,       &
!     &                  scaleX, scaleY,igridtype

! ***************************************************************
!          TotTr = total number of triangles
! was this...TrList( , ) = vertices of triangles in ccw order
!          ListTr( , ) = vertices of triangles in ccw order
!          TCode( )    = material type (code) for a triangle
!          CritLt( )   = storage for values of triangle criteria
! ***************************************************************

      INTEGER      TotTr, ListTr(4,maxtri), TCode(maxtri)
      REAL         CritLt(maxtri)

! ***************************************************************

    End Module

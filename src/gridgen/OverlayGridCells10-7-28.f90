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

!-----------------------------------------------------------------------*
!                       OverlayGrid.f90                                 *
!       This module contains routines for the generation of a regular   *
!       square FE or FD grid using the active polygon limits.           *
!       ROUTINES: GenRegular, GenHex, GenMixed                          *
!-----------------------------------------------------------------------*

      SUBROUTINE GenHexCells(maxnp,np,xg,yg,ncode,maxngh,numngh,nbrs,maxne,ne,nen)

! PURPOSE: To generate a regular equilateral triangular grid over the  
!          current grid within the currently active polygon
!   GIVEN: A NEIGH format depth grid has previously been read in to memory
!          to be used in the mesh generation algorithm.
! RETURNS: quit = TRUE if user quits operation, else = FALSE and a depth 
!          mesh is created over the active polygon domain 
!------------------------------------------------------------------------*
      
      implicit none

! *** passed variables
      integer :: maxnp,np,maxngh,numngh,maxne,ne
      integer :: ncode(maxnp),nbrs(maxngh,maxnp),nen(4,maxne)
      real :: xg(maxnp),yg(maxnp)

! *** local variables
      integer :: i,j,k,nip,istat,ipoints,nxmax,nymax,nbmax,nn,ierr
      real :: dx,dy,xmin,xmax,ymin,ymax
      real :: da1,da2,da3,da4,delta,x0,y0,x1,y1,x2,y2,x3,y3
      real, allocatable :: xr(:), yr(:)
      character(80) cstr, ans

! *** Start

      nbmax = 8
      
      xmin = minval(xg(1:np))
      xmax = maxval(xg(1:np))
      ymin = minval(yg(1:np))
      ymax = maxval(yg(1:np))
      
      cstr ='Enter number (yes) or size (no) of elements ?:'
      call PigMessageYesNo(cstr, ans)
      IF ( ans(1:1) .eq. 'Y' ) THEN
10      call PigPrompt( 'Enter number of elements across:',ans)
        read(ans,*,iostat=ierr) nxmax
        if(ierr.ne.0.or.nxmax.le.0) go to 10
        nymax = nint(float(nxmax)*(2./sqrt(3.))*(ymax-ymin)/(xmax-xmin))
        dx = (xmax-xmin)/float(nxmax)
        dy = (ymax-ymin)/float(nymax)
      else
20      call PigPrompt( 'Enter edge length for element:',ans)
        read(ans,*,iostat=ierr) dx
        if(ierr.ne.0.or.dx.le.0.) go to 20
        dy = dx*sqrt(3.)*0.5
        nxmax = ceiling((xmax-xmin)/dx)
        nymax = ceiling((ymax-ymin)/dy)
      endif

!      nxmax = 20
      
      nip = (nymax+2)*(nxmax+2)
          
      allocate( xr(nip), yr(nip), STAT = istat )
      if(istat.ne.0) then
        call PigPutMessage('FATAL ERROR: Cannot allocate point storage arrays')
        stop
      endif
          
      call PigPutMessage('generating points...')

      ipoints = 0
      do j=1,nymax+1
        do i=1,nxmax + mod(j+1,2)
          ipoints = ipoints+1
          xr(ipoints) = xmin + dx*float(i-1) + 0.5*dx*float(mod(j,2))
!          xr(ipoints) = amin1(xmax,amax1(xmin,xr(ipoints))) !for straight edge
          yr(ipoints) = ymin + dy*float(j-1)
        enddo
      enddo
      
          
      call PigPutMessage('eliminating outliers...')

      ncode = -9
      do nn=1,ne
        x1 = xg(nen(1,nn))
        x2 = xg(nen(2,nn))
        x3 = xg(nen(3,nn))
        y1 = yg(nen(1,nn))
        y2 = yg(nen(2,nn))
        y3 = yg(nen(3,nn))
        xmax = amax1(xg(nen(1,nn)),xg(nen(2,nn)),xg(nen(3,nn)))
        xmin = amin1(xg(nen(1,nn)),xg(nen(2,nn)),xg(nen(3,nn)))
        ymax = amax1(yg(nen(1,nn)),yg(nen(2,nn)),yg(nen(3,nn)))
        ymin = amin1(yg(nen(1,nn)),yg(nen(2,nn)),yg(nen(3,nn)))
        do j=1,ipoints  !determine who is in or out
          if(xr(j).le.xmax) then
            if(xr(j).ge.xmin) then
              if(yr(j).le.ymax) then
                if(yr(j).ge.ymin) then
! CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE
                  x0 = xr(j)
                  y0 = yr(j)
                  DA1=ABS((X1-X0)*(Y2-Y0)-(X2-X0)*(Y1-Y0))
                  DA2=ABS((X2-X0)*(Y3-Y0)-(X3-X0)*(Y2-Y0))
                  DA3=ABS((X3-X0)*(Y1-Y0)-(X1-X0)*(Y3-Y0))
                  DA4=ABS((X2-X1)*(Y3-Y1)-(X3-X1)*(Y2-Y1))
                  IF (DA4.GT.0) THEN
                    DELTA=DA1+DA2+DA3-DA4
                    IF (DELTA.EQ.0..or.DELTA.LE.DA4*1.E-6) THEN
                      ncode(j) =  0
                    ENDIF
                  ENDIF
                endif
              endif
            endif
          endif
        enddo
      enddo

          
      call PigPutMessage('generating ngh list...')

      ipoints = 0
      nbrs = 0
      do j=1,nymax + 1  !generate ngh list
        do i=1,nxmax + mod(j+1,2)
          ipoints = ipoints+1
          if(ncode(ipoints).eq.-9) cycle
          if(j.gt.1) then
            if(i.gt.mod(j+1,2)) then
              if(ncode(ipoints-nxmax-1).ne.-9) then
                do k=1,maxngh
                  if(nbrs(k,ipoints).eq.0) then
                    nbrs(k,ipoints) = ipoints-nxmax-1
                    exit
                  endif
                enddo
              endif
            endif
            if(i.lt.nxmax+1) then
              if(ncode(ipoints-nxmax).ne.-9) then
                do k=1,maxngh
                  if(nbrs(k,ipoints).eq.0) then
                    nbrs(k,ipoints) = ipoints-nxmax
                    exit
                  endif
                enddo
              endif
            endif
          else
            ncode(ipoints) = 1
          endif
          if(j.lt.nymax+1) then
            if(i+mod(j,2).gt.1) then
              if(ncode(ipoints+nxmax).ne.-9) then
                do k=1,maxngh
                  if(nbrs(k,ipoints).eq.0) then
                    nbrs(k,ipoints) = ipoints+nxmax
                    exit
                  endif
                enddo
              endif
            endif
            if(i.lt.nxmax+1) then
              if(ncode(ipoints+nxmax+1).ne.-9) then
                do k=1,maxngh
                  if(nbrs(k,ipoints).eq.0) then
                    nbrs(k,ipoints) = ipoints+nxmax+1
                    exit
                  endif
                enddo
              endif
            endif
          else
            ncode(ipoints) = 1
          endif
          if(i.gt.1) then
            if(ncode(ipoints-1).ne.-9) then
               do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints-1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
          if(i.lt.nxmax+mod(j+1,2)) then
            if(ncode(ipoints+1).ne.-9) then
              do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints+1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
        enddo
      enddo
      
      np = ipoints
! *** eliminate nodes with one ngh
      call RemoveDanglingNodes (np,maxngh,nbrs)

! *** move to main arrays
      numngh = 6
      do j=1,np
        xg(j) = xr(j)
        yg(j) = yr(j)
      enddo

!      call DoCheckEdges ()      
      call PigEraseMessage
      
      return
          end

!**********************************************************************************

      SUBROUTINE GenQuadCells (maxnp,np,xg,yg,ncode,maxngh,numngh,nbrs,maxne,ne,nen)

! PURPOSE: To generate a regular quad grid over the current grid within the 
!          currently active polygon
!   GIVEN: A NEIGH format depth grid has previously been read in to memory
!          to be used in the mesh generation algorithm.
! RETURNS: quit = TRUE if user quits operation, else = FALSE and a depth 
!          mesh is created over the active polygon domain 
!------------------------------------------------------------------------*
      
      implicit none

! *** passed variables
      integer :: maxnp,np,maxngh,numngh,maxne,ne
      integer :: ncode(maxnp),nbrs(maxngh,maxnp),nen(4,maxne)
      real :: xg(maxnp),yg(maxnp)

! *** local variables
      integer :: i,j,k,nip,istat,ipoints,nxmax,nymax,nbmax,nn,ierr
      real :: dx,dy,xmin,xmax,ymin,ymax
      real :: da1,da2,da3,da4,delta,x0,y0,x1,y1,x2,y2,x3,y3
      real, parameter :: tol=0.000001
      real, allocatable :: xr(:), yr(:)
      character(80) ans

! *** Start

      nbmax = 8
      
      xmin = minval(xg(1:np))
      xmax = maxval(xg(1:np))
      ymin = minval(yg(1:np))
      ymax = maxval(yg(1:np))
      
10    call PigPrompt( 'Enter number of elements across:',ans)
      read(ans,*,iostat=ierr) nxmax
      if(ierr.ne.0.or.nxmax.le.0) go to 10

!      nxmax = 20
      nymax = int(float(nxmax)*(ymax-ymin)/(xmax-xmin))
      
      dx = (xmax-xmin)/float(nxmax)
      dy = (ymax-ymin)/float(nymax)
      
      nip = (nymax+2)*(nxmax+2)
          
          allocate( xr(nip), yr(nip), STAT = istat )
      if(istat.ne.0) then
            call PigPutMessage('FATAL ERROR: Cannot allocate point storage arrays')
            stop
          endif
          
          call PigPutMessage('working...')

      ipoints = 0
      do j=1,nymax + 1 !generate points
        do i=1,nxmax + 1
          ipoints = ipoints+1
          xr(ipoints) = xmin + dx*float(i-1)
          yr(ipoints) = ymin + dy*float(j-1)
        enddo
      enddo
            
      ncode = -9
      do nn=1,ne
        x1 = xg(nen(1,nn))
        x2 = xg(nen(2,nn))
        x3 = xg(nen(3,nn))
        y1 = yg(nen(1,nn))
        y2 = yg(nen(2,nn))
        y3 = yg(nen(3,nn))
        xmax = amax1(xg(nen(1,nn)),xg(nen(2,nn)),xg(nen(3,nn)))
        xmin = amin1(xg(nen(1,nn)),xg(nen(2,nn)),xg(nen(3,nn)))
        ymax = amax1(yg(nen(1,nn)),yg(nen(2,nn)),yg(nen(3,nn)))
        ymin = amin1(yg(nen(1,nn)),yg(nen(2,nn)),yg(nen(3,nn)))
        do j=1,ipoints  !determine who is in or out
          if(xr(j).le.xmax+tol) then
            if(xr(j).ge.xmin-tol) then
              if(yr(j).le.ymax+tol) then
                if(yr(j).ge.ymin-tol) then
! CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE
                  x0 = xr(j)
                  y0 = yr(j)
                  DA1=ABS((X1-X0)*(Y2-Y0)-(X2-X0)*(Y1-Y0))
                  DA2=ABS((X2-X0)*(Y3-Y0)-(X3-X0)*(Y2-Y0))
                  DA3=ABS((X3-X0)*(Y1-Y0)-(X1-X0)*(Y3-Y0))
                  DA4=ABS((X2-X1)*(Y3-Y1)-(X3-X1)*(Y2-Y1))
                  IF (DA4.GT.0) THEN
                    DELTA=DA1+DA2+DA3-DA4
                    IF (DELTA.EQ.0..or.DELTA.LE.DA4*1.E-6) THEN
                      ncode(j) =  0
                    ENDIF
                  ENDIF
                endif
              endif
            endif
          endif
        enddo
      enddo

      nbrs = 0
      ipoints = 0
      do j=1,nymax + 1  !generate ngh list
        do i=1,nxmax + 1
          ipoints = ipoints+1
          if(ncode(ipoints).eq.-9) cycle
          if(j.gt.1) then
            if(ncode(ipoints-nxmax-1).ne.-9) then
              do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints-nxmax-1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
          if(j.lt.nymax+1) then
            if(ncode(ipoints+nxmax+1).ne.-9) then
              do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints+nxmax+1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
          if(i.gt.1) then
            if(ncode(ipoints-1).ne.-9) then
               do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints-1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
          if(i.lt.nxmax+1) then
            if(ncode(ipoints+1).ne.-9) then
              do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints+1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
        enddo
      enddo

      np = ipoints
! *** eliminate nodes with one ngh
      call RemoveDanglingNodes (np,maxngh,nbrs)      
      
! *** move to main arrays
      numngh = 4
      do j=1,np
        xg(j) = xr(j)
        yg(j) = yr(j)
      enddo
      
      call PigEraseMessage
      
      return
      end

!**********************************************************************

      SUBROUTINE GenMixedCells (maxnp,np,xg,yg,ncode,maxngh,numngh,nbrs,maxne,ne,nen)

! PURPOSE: To generate a regular quad grid over the current grid within the 
!          currently active polygon and with triangles at the boundary.
!   GIVEN: A NEIGH format depth grid has previously been read in to memory
!          to be used in the mesh generation algorithm.
! RETURNS: quit = TRUE if user quits operation, else = FALSE and a depth 
!          mesh is created over the active polygon domain 
!------------------------------------------------------------------------*
      
      implicit none

! *** passed variables
      integer :: maxnp,np,maxngh,numngh,maxne,ne
      integer :: ncode(maxnp),nbrs(maxngh,maxnp),nen(4,maxne)
      real :: xg(maxnp),yg(maxnp)

! *** local variables
      integer :: i,j,k,nip,istat,ipoints,nxmax,nymax,nbmax,nn,ierr
      real :: dx,dy,xmin,xmax,ymin,ymax
      real :: da1,da2,da3,da4,delta,x0,y0,x1,y1,x2,y2,x3,y3
      real, allocatable :: xr(:), yr(:)
      character(80) ans

! *** Start

      nbmax = 8
      
      xmin = minval(xg(1:np))
      xmax = maxval(xg(1:np))
      ymin = minval(yg(1:np))
      ymax = maxval(yg(1:np))
      
10    call PigPrompt( 'Enter number of elements across:',ans)
      read(ans,*,iostat=ierr) nxmax
      if(ierr.ne.0.or.nxmax.le.0) go to 10

!      nxmax = 20
      nymax = int(float(nxmax)*(ymax-ymin)/(xmax-xmin))
      
      dx = (xmax-xmin)/float(nxmax)
      dy = (ymax-ymin)/float(nymax)
      
      nip = (nymax+2)*(nxmax+2)
          
          allocate( xr(nip), yr(nip), STAT = istat )
      if(istat.ne.0) then
            call PigPutMessage('FATAL ERROR: Cannot allocate point storage arrays')
            stop
          endif
          
          call PigPutMessage('working...')

      ipoints = 0
      do j=1,nymax + 1 !generate points
        do i=1,nxmax + 1
          ipoints = ipoints+1
          xr(ipoints) = xmin + dx*float(i-1)
          yr(ipoints) = ymin + dy*float(j-1)
        enddo
      enddo
      
      ncode = -9
      do nn=1,ne
        x1 = xg(nen(1,nn))
        x2 = xg(nen(2,nn))
        x3 = xg(nen(3,nn))
        y1 = yg(nen(1,nn))
        y2 = yg(nen(2,nn))
        y3 = yg(nen(3,nn))
        xmax = amax1(xg(nen(1,nn)),xg(nen(2,nn)),xg(nen(3,nn)))
        xmin = amin1(xg(nen(1,nn)),xg(nen(2,nn)),xg(nen(3,nn)))
        ymax = amax1(yg(nen(1,nn)),yg(nen(2,nn)),yg(nen(3,nn)))
        ymin = amin1(yg(nen(1,nn)),yg(nen(2,nn)),yg(nen(3,nn)))
        do j=1,ipoints  !determine who is in or out
          if(xr(j).le.xmax) then
            if(xr(j).ge.xmin) then
              if(yr(j).le.ymax) then
                if(yr(j).ge.ymin) then
! CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE
                  x0 = xr(j)
                  y0 = yr(j)
                  DA1=ABS((X1-X0)*(Y2-Y0)-(X2-X0)*(Y1-Y0))
                  DA2=ABS((X2-X0)*(Y3-Y0)-(X3-X0)*(Y2-Y0))
                  DA3=ABS((X3-X0)*(Y1-Y0)-(X1-X0)*(Y3-Y0))
                  DA4=ABS((X2-X1)*(Y3-Y1)-(X3-X1)*(Y2-Y1))
                  IF (DA4.GT.0) THEN
                    DELTA=DA1+DA2+DA3-DA4
                    IF (DELTA.EQ.0..or.DELTA.LE.DA4*1.E-6) THEN
                      ncode(j) =  0
                    ENDIF
                  ENDIF
                endif
              endif
            endif
          endif
        enddo
      enddo

      nbrs = 0
      ipoints = 0
      do j=1,nymax + 1  !generate ngh list
        do i=1,nxmax + 1
          ipoints = ipoints+1
          if(ncode(ipoints).eq.-9) cycle
          if(j.gt.1) then
            if(ncode(ipoints-nxmax-1).ne.-9) then
              do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints-nxmax-1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
          if(j.lt.nymax+1) then
            if(ncode(ipoints+nxmax+1).ne.-9) then
              do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints+nxmax+1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
          if(i.gt.1) then
            if(ncode(ipoints-1).ne.-9) then
               do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints-1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
          if(i.lt.nxmax+1) then
            if(ncode(ipoints+1).ne.-9) then
              do k=1,maxngh
                if(nbrs(k,ipoints).eq.0) then
                  nbrs(k,ipoints) = ipoints+1
                  exit
                endif
              enddo
            endif
          else
            ncode(ipoints) = 1
          endif
        enddo
      enddo
      
      np = ipoints
! *** add tris
!      call RemoveDanglingNodes (np,maxngh,nbrs)      

! *** move to main arrays
      numngh = 4
      do j=1,np
        xg(j) = xr(j)
        yg(j) = yr(j)
      enddo
      
      call PigEraseMessage
      
      return
      end

!**********************************************************************


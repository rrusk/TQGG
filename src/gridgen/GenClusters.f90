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

! **********************************************************

    Module MeshArrays

    integer, allocatable:: meshIndex(:,:)
    integer, allocatable:: agrid(:,:)
    real, allocatable:: mesh(:,:), center(:,:)  !, RefLength(:)
    REAL MESHSTEP
    INTEGER XSIZE,YSIZE
    integer :: NumMeshIntervals=100
    REAL XMSMIN,XMSMAX,YMSMIN,YMSMAX

    logical :: displtris=.true.
    logical :: displcells=.true.
    logical :: displclust=.true.

    End Module

! **********************************************************

    Module ClusterParams

    integer :: numr, sqnum, numsq, minnumsq
    REAL :: percnt, ratio, area0, rat2, dxb, dyb

    End Module

! **********************************************************

    SUBROUTINE MakeMeshAndClusters(N,NPTS,NTRI,ELIST,NCB,V,T,X,Y, &
                                   DMAX,XMIN,YMIN,LIST,Reflength,Quit)

    USE MeshArrays

!      PURPOSE: Generate background mesh for cluster generation              
!      --------   and generate internal nodes via clusters

    logical quit  !,displtris,displcells
    character(128) cstr
    character(1) ans
    integer ival, lencstr

!   passed variables
    real DMAX,XMIN,YMIN
    integer npts,n,ntri
    integer v(4,2*npts+1)
    integer ncb, elist(2,n)
    real x(npts+3),y(npts+3)
    real Reflength(npts)

!   Re node insertion
    INTEGER T(3,2*NPTS+1)
    INTEGER LIST(NPTS)

! *** generate RefLength for boundary points
!      ALLOCATE (RefLength(n), STAT = istat )
!      if(istat.ne.0) then
!        call PigMessageOK('out of memory - Cannot allocate front storage arrays','front')
!        AutoGenFlag = -1
!        return
!      endif

!      RefLength = 1.

    cstr = 'Read a ngh reference file?'
    call PigMessageYesNo (cstr, ans)
    IF (ans(1:1).EQ.'Y') THEN
      call GetRefGrid( Quit )
!      call ScaleRefGrid(N, NTRI, X, Y, RefLength, V)
    else
      call SetDefaultRefGrid(N, NTRI, X, Y, RefLength, V, DMAX, XMIN, YMIN)
    endif

    do !loop until OK

! *** set mesh parameters
      call SetMeshStepSize

! *** create mesh
      call CreateMesh(N,NPTS,NTRI,ELIST,NCB,V,X,Y,DMAX,XMIN,YMIN)

      cstr = 'Look OK? Continue?'
      call PigMessageYesNo (cstr, ans)
      IF (ans(1:1).EQ.'Y') THEN
        call PigEraseMainwin ()
!          call PigSetWindowNum ( MAINWIN )
!          call DisplayNodes ()
        exit
      ELSE
! *** go back and reset meshstep
        call PigEraseMainwin ()
!          call PigSetWindowNum ( MAINWIN )
!          call DisplayNodes ()
! *** DEAllocate arrays
        DEALLOCATE (Mesh, MeshIndex,  STAT = istat )
        if(istat.ne.0) then
          call PigMessageOK('ERROR: Cannot DEallocate Mesh storage arrays','mesh')
        endif
!          read NumMeshIntervals
        call PigPrompt('Enter NumMeshIntervals:',cstr)
        READ( cstr, FMT = '(I4)') ival
        lencstr = LEN_TRIM( cstr )
        if ( lencstr .ge. 1 ) then
          NumMeshIntervals = ival
        endif
      endif
    enddo

    call CloseRefGrid( Quit )

    call CreateClusterPoints(N,NPTS,NTRI,ELIST,NCB,V,T,X,Y, &
                                              LIST,DMAX,XMIN,YMIN)

! *** DEAllocate arrays
    if(allocated(Mesh)) DEALLOCATE (Mesh)
    if(allocated(MeshIndex)) deallocate(MeshIndex)
    if(allocated(agrid)) deallocate(agrid)
    if(allocated(center)) deallocate(center)
!    if(allocated(RefLength)) deallocate(RefLength)

    return
    end

! **********************************************************

    SUBROUTINE SetMeshStepSize

    use MeshArrays
   
    IMPLICIT NONE

! PURPOSE: To set mesh step value to default value for current active 
!          polygon, or to latest step value entered by user ( if any ).
! GIVEN:   
! RETURNS: 
! EFFECTS: value for meshstep is set.
!-----------------------------------------------------------------------*

! - LOCAL VARIABLES
    integer ToMatx2, istat
    REAL xminp, xmaxp, yminp, ymaxp, xlimp, ylimp

!---------------START ROUTINE-----------------------------------

!         - calculate size of mesh with default step value
    call PolyLimits ( xminp, xmaxp, yminp, ymaxp )

    xmsmin = xminp
    xmsmax = xmaxp
    ymsmin = yminp
    ymsmax = ymaxp

    xlimp = ABS ( xmaxp - xminp )
    ylimp = ABS ( ymaxp - yminp )

!     NumMeshIntervals must not exceed nrow = ncol, set up automatic selection here
!     NumMeshIntervals = 100

    MeshStep = amax1(xlimp,ylimp)/NumMeshIntervals
!     Note: above quantities are in original grid coords, not normalized coords

!     calculate size of mesh
!     xsize, ysize are integers independent of coordinates used
    xsize = ToMatx2 ( xmsmax, xmsmin, meshstep )
    ysize = ToMatx2 ( ymsmax, ymsmin, meshstep )


! *** Allocate arrays
    ALLOCATE (Mesh(xsize,ysize), MeshIndex(xsize,ysize), STAT = istat )
    if(istat.ne.0) then
      call PigMessageOK('FATAL ERROR: Cannot allocate Mesh storage arrays','mesh')
      stop
    endif


    END

! **********************************************************

    SUBROUTINE CreateMesh(N,NPTS,NTRI,ELIST,NCB,V,X,Y,DMAX,XMIN,YMIN)

    USE MeshArrays

!! ----------------------------------------------------------------------

! PURPOSE: This routine loops through the triangle list and calculates
!          the depth for each mesh cell that lies within some triangle of
!          reference grid
! GIVEN:   Neigh file depth grid data in RefGrid,
!          Triangle list data in RefGrid,
!          Active polygon data in file POLYSTOR.INC,
!          Mesh parameters set in SetMeshStepSize
!          Display flags set as needed.
! RETURNS: In module MeshArrays;
!               mesh( , ) = depths set where appropriate in mesh over active
!                           polygon with 
!               meshIndex( , ) = -1 in mesh squares outside reference grid
!                              =  0 in usable mesh squares 
! EFFECTS: Depths are calculated for the mesh. Mesh cells and/or triangles 
!          are colored and displayed according to the results if displcells 
!          and/or displtris are TRUE.
! WRITTEN: July 1990 by JDM for NODER, based on Meshpta by Lynda Williams
! MODIFIED: December 2006 by Roy Walters and Falconer Henry  
!-----------------------------------------------------------------------*


! - "INCLUDES"
    include '../includes/edpolys.inc'

!    LOGICAL displcells, displtris

!   passed variables
    
    integer npts,n,ntri
    integer v(4,2*npts+1)
    real x(npts+3),y(npts+3),DMAX,XMIN,YMIN
    integer ncb, elist(2,n)

! - LOCAL VARIABLES
! ii - x index of mesh
! jj - y index of mesh
! mm - index of triangle we are searching
! iaa1-ida4 - temp values
! delta - if ~= 0 then point lies within triangle
! a,b,c - used to find depth
! x0-z3 - real values- x,y co-ordinates, z=depth at (x,y)
! ToMatx2,ToMsh2 - functions
! nout - output unit number
! aa,bb,cc - depth at (x0,y0) = aa * x0 + bb * y0 + cc
! aa1,aa2,b1,c1 -  used to calculate aa, bb, cc
! delta,da1,da2,da3,da4 - used to see if point inside triangle
! txmin,txmax,tymin,tymax - min/max coords for individual triangle
! xminlimit,xmaxlimit,yminlimit,ymaxlimit - mesh limits for one triangle
! xb,yb - co-ordinates of boundary vertex if only 1
! xb1,yb1,xb2,yb2 - co-ordinates of boundary vertices when 2 
! delver - delta for 1 boundary vertex
! delbnd - delta for 2 boundary vertices
! halfstep - half step size
! inside - logical flag

    integer ii, jj, mm, ToMatx2, nex, npx
    REAL ToMsh2, aa, bb, cc, delta, txmin, txmax, tymin, tymax
    integer xminlimit, xmaxlimit, yminlimit, ymaxlimit
    REAL x0, x1, x2, x3, y0, y1, y2, y3, z0, z1, z2, z3
    REAL b1, c1, aa1, aa2, da1, da2, da3, da4
    REAL  halfstep
    LOGICAL inside
    LOGICAL didpltris
    INTEGER N1, N2, N3, Elelist(4) ,CRef(4)
    REAL xp(4), yp(4), xRef(4), yRef(4), ZRef(4)

!    data void/-1.e30/

!---------------START ROUTINE--------------------------------------

!       - prepare active polygon for calls to InPoly
!      call PrepPoly ( actvpoly )

!      displtris = .true.   

    step = meshstep
    halfstep = step / 2.0
!     Note: above quantities are in original grid coords, not normalized coords

    call PigPutMessage ( 'Evaluating mesh...' )

!       - initialize mesh to zero
    DO jj = 1, ysize
      DO ii = 1, xsize
        mesh(ii,jj) = 0.0000
        meshIndex(ii,jj) = -1
      END DO
    END DO

!       - loop for each triangle in reference grid triang file
       call GetRefGridLimits( nex, npx )
    DO 700 mm = 1, nex
      m1 = mm
      call GetRefGridElementInfo( m1, ncnx, Elelist, xRef, yRef, zRef,CRef)
      if(m1.le.0) cycle
      N1 = EleList(1)
      N2 = EleList(2)  !ListTr(2,mm)
      N3 = EleList(3)  !ListTr(3,mm)
!         - find x, y, depth for each vertex
      x1 = xRef(1)  !dxray(N1)
      x2 = xRef(2)  !dxray(N2)
      x3 = xRef(3)  !dxray(N3)
      y1 = yRef(1)  !dyray(N1)
      y2 = yRef(2)  !dyray(N2)
      y3 = yRef(3)  !dyray(N3)
      z1 = zRef(1)  !depth(N1)
      z2 = zRef(2)  !depth(N2)
      z3 = zRef(3)  !depth(N3)

!         - calculate a,b,c for depth triangle where
!         -- depth at (x0,y0) = (a * x0) + (b * y0) + c
      aa1 = (z1 * y2) - (z2 * y1) + (z3 * y1) - (z1 * y3) + (z2 * y3) - (z3 * y2)
      aa2 = (x1 * y2) - (x2 * y1) + (x3 * y1) - (x1 * y3) + (x2 * y3) - (x3 * y2)
      b1 = (x2 * z3) - (z2 * x3) + (x3 * z1) - (x1 * z3) + (x1 * z2) - (x2 * z1)
      c1 = x1 * (y2 * z3 - y3 * z2) + x2 * (y3 * z1 - y1 * z3) + x3 * (y1 * z2 - y2 * z1)
      IF ( aa2 .ne. 0 ) THEN 
        aa = aa1 / aa2
        bb = b1 / aa2
        cc = c1 / aa2
      ELSE
        aa = 0
        bb = 0
        cc = 0
      ENDIF
!           - ( aa2 ne 0 )
!         - calculate max and min for this triangle
      txmin = MIN ( x1, x2, x3 )
      txmax = MAX ( x1, x2, x3 )
      tymin = MIN ( y1, y2, y3 )
      tymax = MAX ( y1, y2, y3 )

!         - determine indices for these values
      xminlimit = ToMatx2 ( txmin, xmsmin, step )
      xmaxlimit = ToMatx2 ( txmax, xmsmin, step )
      yminlimit = ToMatx2 ( tymin, ymsmin, step )
      ymaxlimit = ToMatx2 ( tymax, ymsmin, step )

!         - if triangle completely outside mesh, skip to next triangle
      IF ( (xminlimit .lt. 1) .AND. (xminlimit .gt. xsize) .AND.&
               (xmaxlimit .lt. 1) .AND. (xmaxlimit .gt. xsize) .AND. &
               (yminlimit .lt. 1) .AND. (yminlimit .gt. ysize) .AND. &
               (ymaxlimit .lt. 1) .AND. (ymaxlimit .gt. ysize) ) THEN
        IF ( displtris ) THEN
          xp(1) = x1
          yp(1) = y1
          xp(2) = x2
          yp(2) = y2
          xp(3) = x3
          yp(3) = y3
          xp(4) = xp(1)
          yp(4) = yp(1)
!          call PigSetWindowNum ( MAINWIN )
!          call PigSetLineColour ( red )
          call PigDrawline ( 4, xp, yp, 4 )
        ENDIF
!             - ( displtris )
        GOTO 699
      ENDIF

!         - triangle not skipped over yet so must be one used in calculations
      didpltris = .true.
         IF ( displtris ) THEN
        xp(1) = x1
        yp(1) = y1
        xp(2) = x2
        yp(2) = y2
        xp(3) = x3
        yp(3) = y3
        xp(4) = xp(1)
        yp(4) = yp(1)
!        call PigSetWindowNum ( MAINWIN )
!        call PigSetLineColour ( white )
        call PigDrawline ( 4, xp, yp, 1 )
      ENDIF
!             - ( displtris )

!         - these values determine area of mesh that will be searched
!         -- for this triangle (ie. all points of triangle lie within the
!         -- rectangle defined by (xminlimit,yminlimit),(xmaxlimit,ymaxlimit)

!         - loop through this rectangle
      DO 698 jj = yminlimit, ymaxlimit
        DO 698 ii = xminlimit, xmaxlimit
!             - if point lies outside mesh, skip it
          IF ( (ii .gt. xsize) .OR. (ii .lt. 1) .OR.(jj .gt. ysize) .OR. (jj .lt. 1) ) THEN
            GOTO 698
          ENDIF

!             - want to check center of square, so must add half of step size
          x0 = ToMsh2 ( ii, xmsmin, step ) + halfstep
          y0 = ToMsh2 ( jj, ymsmin, step ) + halfstep

!             - if point not in polygon then skip it
!xx          call InPoly ( actvpoly, x0, y0, inply )
!xx              IF ( .NOT. inply ) THEN
!xx                GOTO 698
!xx              ENDIF


!               - check to see if point is inside triangle
!               - to determine if mesh(ii,jj) lies inside the triangle,
!               - calculate delta. if delta >0 then point is outside triangle
          da1 = ABS((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0))
          da2 = ABS((x2 - x0) * (y3 - y0) - (x3 - x0) * (y2 - y0))
          da3 = ABS((x3 - x0) * (y1 - y0) - (x1 - x0) * (y3 - y0))
          da4 = ABS((x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1))
          inside = .FALSE.
          IF ( da4 .gt. 0 ) THEN
            delta = da1 + da2 + da3 - da4
            IF ( delta .le. (da4 * 1.0E-6) )  inside = .TRUE.
          ENDIF
!                 - ( da4 > 0 )
!               - if point lies inside triangle, evaluate depth
          IF ( inside ) THEN
            z0 = aa * x0 + bb * y0 + cc
            mesh(ii,jj) = z0
                  meshIndex(ii,jj) = 0
          ENDIF
!                 - ( inside )

!       - bottom of DO loops
698     CONTINUE
699   CONTINUE
700 CONTINUE

    call PigEraseMessage

    call ReserveSquares(N,NPTS,NTRI,ELIST,NCB,V,X,Y,DMAX,XMIN,YMIN)

!      ReserveSquares set meshIndex(i,j) = 2 for mesh squares whose centres 
!      lie within specified distances from existing boundary or front-related 
!      nodes. These 'reserved squares' are then unavailable for forming clusters.

!    displcells = .true.
!    call ColorInfo ( displcells, displtris )
    IF ( displcells ) THEN
!       - select transformation 1
!      call PigSetWindowNum ( MAINWIN )
!      call PigSetSymbolNumber ( cross )

      DO mrow = 1, ysize
        DO mcol = 1, xsize
          xp(1) = ToMsh2 ( mcol, xmsmin, step ) + ( 0.5 * step )
          yp(1) = ToMsh2 ( mrow, ymsmin, step ) + ( 0.5 * step )
          IF ( meshIndex(mcol,mrow) .lt. 0 ) THEN
!             - square is outside reference grid and is unusable
!            call PigSetSymbolColour ( red )
            call PigDrawClrSymbol ( xp(1), yp(1), 'R' )
          ELSE 
!           - ( mesh(mcol,mrow) >= 0 ) -
            if( meshIndex(mcol,mrow).eq.2) then
!             square reserved for 'cluster' around bdy or front-related node             
!              call PigSetSymbolColour ( blue )
              call PigDrawClrSymbol ( xp(1), yp(1), 'B' )
            else
!             square not reserved, meshIndex(mrow,mcol) = 0
!             call PigSetSymbolColour ( yellow )
              call PigDrawClrSymbol ( xp(1), yp(1), 'Y' )
            endif
          ENDIF
!             - ( mesh(mcol,mrow) = 0 )
        END DO
      END DO
    ENDIF

    END

! **********************************************************

    SUBROUTINE ReserveSquares(N,NPTS,NTRI,ELIST,NCB,V,X,Y,DMAX,XMIN,YMIN)

      USE MeshArrays


!     PURPOSE: 
!     -------- 
!     Reserves mesh squares attributed to virtual cluster associated
!     with each boundary and front-related node by setting 
!     meshIndex(i,j) = 2 .  The virtual cluster around each
!     boundary or front-related node consists of those mesh squares
!     whose centres lie within a circle of given radius depending on
!     local node spacing


    include '../includes/edpolys.inc'

!   passed variables
    
       integer npts,n,ntri
       integer v(4,2*npts+1)
       integer ncb, elist(2,n)
       real x(npts+3),y(npts+3),DMAX,XMIN,YMIN 
!      ncb = number of boundary nodes

!   local variables
       integer i,j,k
       logical inply
       real Radk, Radsq, Rad2sq,factor1, factor2
       real xcm, ycm,xmodel,ymodel,xsqtem,ysqtem
!      Radk is the square of the radius of the virtual cluster to be
!      placed around node k and is some factor times the square 
!      of a typical length associated with node k. 
!      At present, for boundary nodes, use 
!          radius**2 = average of squares of distances to node's two neighbours 
!                      on the same boundary (evaluated in real function Radsq)
!      for frontal nodes, use 
!          radius**2 = average of squares of distances to 3 nearest neighbours    
!                              see Rad2sq
!
!      factor1 is arbitrary factor for boundary node radius calculation
!      factor2 is arbitrary factor for frontal node radius calculation
       factor1 = 0.5
       factor2 = 0.5
!      factor1, factor2 values will be based on experiment

!    call PrepPoly ( actvpoly )

       do k = 1, n !bfs
!       Reserve mesh squares only for boundary nodes or front-related nodes
!       which lie within the active polygon (which is defined in model coords)
!       Location x(k),y(k) is currently in normal coordinates, so convert to
!       model coordinates xmodel, ymodel

         xmodel = xmin + x(k)*DMAX
         ymodel = ymin + y(k)*DMAX
!         call InPoly ( actvpoly, xmodel, ymodel, inply )
         inply = .true.
         if(inply) then
!       Find mesh squares whose centres are within specified distance 
!       [distance)**2<= Radsq] of node k
           if(k.le.ncb) then
!          node k is a boundary node
             Radk = factor1*Radsq(k,N,NPTS,ELIST,NCB,X,Y)
           elseif(k.gt.ncb.and.k.le.n) then !bfs) then
!          node k is a front-related node
             Radk = factor2*Rad2sq(k,NPTS,NTRI,V,X,Y)
           endif

!       
      do i = 1, xsize-1
        do j = 1, ysize-1
          IF(meshIndex(i,j).eq.0) THEN
!           mesh square centre is usable and has not been reserved yet.        
!           Get centre of mesh square i,j in normalised grid coords
!           NB: xcm, xmssim, etc and meshstep are in original grid coords
!           x( ),y( ), Radk are in normalised coordinates
!        To convert position of center of mesh square to normalised coordinates
!        first transform to model coordinates
            xcm = xmsmin + (i-0.5)*meshstep
            ycm = ymsmin + (j-0.5)*meshstep
!        then transform to normalised coordinates            
            xcm = (xcm-xmin)/DMAX
            ycm = (ycm-ymin)/DMAX
 
            xsqtem = (xcm-x(k))**2
            ysqtem = (ycm-y(k))**2

            if( (xsqtem + ysqtem).le.Radk ) then
!??              rcount = rcount + 1
!             reserve square
              meshIndex(i,j) = 2 
         endif
          ENDIF
        end do ! j
      end do ! i
    endif ! in
  end do !  for node k
      

      END
!! **********************************************************

     REAL FUNCTION Rad2sq(NodeNum,NPTS,NTRI,V,X,Y)

!     PURPOSE: 
!     -------- 
!      Finds square of typical length scale for front-related node
!      On return, Rad2sq equals average of squares of distances to
!      three nearest neighbours of node in question

!
       integer npts,ntri,NodeNum
       integer v(4,2*npts+1)
       real x(npts+3),y(npts+3)

!      local variables
       integer i, vert1, vert2
       real  distsq1,distsq2,distsq3,dsq1,dsq2
       logical newtrifound

!      Find squares of distances to NodeNum's nearest three neighbours

!       Initialise distsq1,distsq2,distsq3 to large value
        distsq1 = 1.E30
        distsq2 = 1.E30
        distsq3 = 1.E30

        do i = 1, ntri
          newtrifound = .false. 
          if  (v(1,i).eq.NodeNum) then
            vert1 = v(2,i)
            vert2 = v(3,i)
            newtrifound = .true. 
          elseif(v(2,i).eq.NodeNum) then
            vert1 = v(3,i)
            vert2 = v(1,i)
            newtrifound = .true. 
          elseif(v(3,i).eq.NodeNum) then
            vert1 = v(1,i)
            vert2 = v(2,i)
            newtrifound = .true. 
          endif

          if(newtrifound) then
!           calculate squares of distances from NodeNum to vert1,vert2
            dsq1 = (x(NodeNum)-x(vert1))**2 + (y(NodeNum)-y(vert1))**2 
            dsq2 = (x(NodeNum)-x(vert2))**2 + (y(NodeNum)-y(vert2))**2 
            if(dsq1.lt.distsq1) then
              distsq3 = distsq2
              distsq2 = distsq1
              distsq1 = dsq1
            elseif(dsq1.lt.distsq2) then
              distsq3 = distsq2
              distsq2 = dsq1
            elseif(dsq1.lt.distsq3) then
              distsq3 = dsq1
            endif

            if(dsq2.lt.distsq1) then
              distsq3 = distsq2
              distsq2 = distsq1
              distsq1 = dsq2
            elseif(dsq2.lt.distsq2) then
              distsq3 = distsq2
              distsq2 = dsq2
            elseif(dsq2.lt.distsq3) then
             distsq3 = dsq2
            endif
          endif ! newtrifound

        end do ! i     

        Rad2sq = (distsq1 + distsq2 + distsq3)/3.

      END

!! **********************************************************

     REAL FUNCTION Radsq(NodeNum,N,NPTS,ELIST,NCB,X,Y)

!     PURPOSE: 
!     -------- 
!      Finds square of length typical of distances along boundary to
!      node NodeNum 's two neighbours

!
       integer npts,n,NodeNum, nbr1, nbr2
       integer ncb, elist(2,n)
       real nbr1distsq, nbr2distsq
       real x(npts+3),y(npts+3)

!      npts = max possible number of nodes in triangulation
!      n    = number of nodes

!      local variables
       integer i

!      Find NodeNum's two neighbours nbr1,, nbr2 from elist

       nbr1 = 0
       nbr2 = 0

       do i = 1, ncb         !  finding nbr1
         if(nbr1.eq.0) then
           if(elist(1,i).eq.NodeNum) then
             nbr1 = elist(2,i)
           elseif(elist(2,i).eq.NodeNum) then
             nbr1 = elist(1,i)
           endif
         endif
       end do 

       do i = 1,ncb         !  finding nbr2
         if(nbr1.ne.0) then
           if(elist(1,i).eq.NodeNum) then
             nbr2 = elist(2,i)
           elseif(elist(2,i).eq.NodeNum) then
             nbr2 = elist(1,i)
           endif
         endif
       end do

!      Squares of distances from NodeNum to nbr1 and nbr2 are
       nbr1distsq = (x(NodeNum)-x(nbr1))**2 + (y(NodeNum)-y(nbr1))**2 
       nbr2distsq = (x(NodeNum)-x(nbr2))**2 + (y(NodeNum)-y(nbr2))**2 

!      Radsq is is normalised coordinates
       Radsq = (nbr1distsq + nbr2distsq)/2

      END

!! **********************************************************

    SUBROUTINE CreateClusterPoints(N,NPTS,NTRI,ELIST,NCB,V,T,X,Y, &
                                                     LIST,DMAX,XMIN,YMIN)

      USE MeshArrays
      USE ClusterParams

!     PURPOSE: Generate clusters and points for grid generation.
!     --------

! *** parameters
      integer, parameter :: maxn = 2000, maxseeds=1000000


! - PASSED VARIABLES
      real DMAX,XMIN,YMIN
      LOGICAL aborted

! *** local
      integer :: lencstr
      real rval,x1(2),y1(2)
      character(128) cstr
      character(1) ans

!   Re node insertion
      real xp,yp
      integer v1,v2,v3,j
      INTEGER N,NPTS,NTRI,NCB
      INTEGER T(3,2*NPTS+1),V(4,2*NPTS+1)
      INTEGER LIST(NPTS)
      INTEGER ELIST(2,*)
      REAL X(NPTS+3),Y(NPTS+3)

! *** seed arrays
      INTEGER SEARCH(2,maxn)
      INTEGER nseed,maxsd,SEED1(MAXSEEDS),SEED2(MAXSEEDS)
      REAL SEED3(MAXSEEDS)
        
! *** Allocate arrays
      ALLOCATE (center(2,npts), STAT = istat )
      if(istat.ne.0) then
        call PigMessageOK('FATAL ERROR: Cannot allocate center storage arrays','center')
        stop
      endif

      maxsq = 2000

      ALLOCATE (agrid(2,maxsq), STAT = istat )
      if(istat.ne.0) then
        call PigMessageOK('FATAL ERROR: Cannot allocate agrid storage arrays','agrid')
        stop
      endif

!       - initialize grid to zero
       agrid = 0

      do  !do until OK
      
        maxsd = min((xsize+1)*(ysize+1),maxseeds)  !maxseeds
        call SetUpClusters (npts, nseed, maxsd, seed1, seed2, seed3, aborted )

        maxsrch = maxn
        call SetUpSearch (maxsrch, search)

        call PigPutMessage ( 'Forming Clusters...' )
      
        call Create2 ( nseed, maxsd, seed1, seed2, maxsrch, search, aborted )

! *** convert centroids into model coordinates and display nodes, then edges

!        call PigSetWindowNum ( MAINWIN )
!        call PigSetSymbolNumber ( cross )
!        call PigSetSymbolColour ( yellow )

        DO i = 1, sqnum
          x1(1) = center(1,i)
          y1(1) = center(2,i)
          call PigDrawClrSymbol ( x1(1), y1(1), 'Y' )
        END DO

        do i=1,ncb
          x1(1) = x(elist(1,i))*dmax + xmin
          y1(1) = y(elist(1,i))*dmax + ymin
          x1(2) = x(elist(2,i))*dmax + xmin
          y1(2) = y(elist(2,i))*dmax + ymin
!       call PigSetLineColour ( red )
       call PigDrawline ( 2, x1, y1, 4 )
        enddo
 !       call PigSetSymbolColour ( green )
        do i=1,n
          x1(1) = x(i)*dmax + xmin
          y1(1) = y(i)*dmax + ymin
          call PigDrawClrSymbol ( x1(1), y1(1), 'G' )
        enddo
          
!        call PigSetSymbolNumber ( point )

        cstr = 'Look OK? Continue?'
        call PigMessageYesNo (cstr, ans )
        IF (ans(1:1).EQ.'Y') THEN       
!!      this is better place to put call GetDepth2 -
!!      having it called in Convrt2 means unnecessary calculation
!!      in cases where tentative new nodes are rejected
          exit
        ELSE
!          read A0
           call PigPrompt('Enter A0:',cstr)
           READ( cstr, FMT = '(F8.0)') rval
           lencstr = LEN_TRIM( cstr )
           if ( lencstr .ge. 1 ) then
            area0 = rval
           endif
!          read A1
           call PigPrompt('Enter A1:',cstr)
           READ( cstr, FMT = '(F8.0)') rval
           lencstr = LEN_TRIM( cstr )
           if ( lencstr .ge. 1 ) then
            ratio = rval
       endif
          call PigEraseMainwin ()
        endif
      enddo

!     Batch of sqnum new nodes formed via clusters has been approved
!     Now insert them into Sloan triangulation. 
!     Convert2 converts the new nodes positions into model coordinates, 
!     i.e. center(1,*),center(2,*) [DEFN2.SET] are in model coordinates
!     having been converted in FIND. Sloan needs normalised coodinates

!     Inserting new cluster-nodes into ongoing Sloan triangulation
      do i = 1, sqnum
        n=n+1
! *** must first convert from model coordinates to normalised coordinates
        x(n)= (center(1,i)-xmin)/DMAX
        y(n)= (center(2,i)-ymin)/DMAX

! *** insert new pt
        xp=x(n)
        yp=y(n)
        list(n)=n

        call FINDTRI(XP,YP,J,T,V,NPTS,NTRI,V1,V2,V3,X,Y,NCB)
        if(j.eq.0) then
          n = n-1
          cycle
        endif
!        call insertp(npts,n,x,y,v,t,ntri,v1,v2,v3,xp,yp,n,j)
        call insertp(npts,x,y,v,t,ntri,v1,v2,v3,xp,yp,n,j)

      end do
    
      END

! **********************************************************

    SUBROUTINE SetUpClusters ( npts, nseed, maxsd, seed1, seed2, seed3, aborted )

      USE MeshArrays
      USE ClusterParams

! PURPOSE: This routine gets values for mesh square depths 
!          It also sets up array seed to contain indices of live mesh squares
!          sorted in descending order of depth. Called from CreateClusterPoints
!          mesh(i,j) contains mesh depth values
!          MeshIndex(i,j) contains mesh occupancy info
!                        = -1 outside domain, mesh square not to be used
!                        =  0 mesh square available, not yet used in cluster
!                        =  1 mesh square already used in some cluster
!                        =  2 mesh square reserved for virtual cluster around
!                             boundary node or front-related node
!-----------------------------------------------------------------------*

! - PARAMETERS - (constants)
    REAL*4 seconds
    PARAMETER ( seconds = 2.0 )

! - "INCLUDES"
!    include '../includes/defn5.set'

! - PASSED VARIABLES
! *** seed arrays
    INTEGER npts,nseed,maxsd,SEED1(MAXSD),SEED2(MAXSD)
    REAL SEED3(MAXSD)
    LOGICAL aborted

! - LOCAL VARIABLES
    integer ii, jj, ll, num
    CHARACTER*80 cstr
    LOGICAL asc, err, start/.true./

! -----------START ROUTINE---------------------------------------- 

    aborted = .FALSE.
!       - initialize defaults
    defarea0 = 0.0
    defratio = 0.8
    defrat2 = 0.0
    defdxb = 0.0
    defdyb = 0.0
    defminnumsq = 1
    defpercnt = 60
    defnumr = npts ! MaxPts !- existnodes

!       - set values to defaults
      if(start) then
        area0 = defarea0
      ratio = defratio
      rat2 = defrat2
      dxb = defdxb 
      dyb = defdyb
      minnumsq = nint(defminnumsq)
      percnt = defpercnt
      numr = nint(defnumr)
         start = .false.
      endif

!       maxsq = 2000

! *** Allocate arrays
!       ALLOCATE (agrid(2,maxsq), STAT = istat )
!       if(istat.ne.0) then
!      call PigPutMessage('FATAL ERROR: Cannot allocate agrid storage arrays')
!      stop
!    endif

!       - initialize grid to zero
       agrid = 0

!   prepare to sort usable mesh squares in descending order of depth
    ll = 0
    DO jj = 1, ysize
      DO ii = 1, xsize 
        IF ( MeshIndex(ii,jj) .eq. 1 ) THEN
          MeshIndex(ii,jj) = 0
        endif
        IF ( mesh(ii,jj) .gt. 0. ) THEN
          ll = ll + 1
          seed1(ll) = jj + 1000 * ii
          seed2(ll) = 0
          seed3(ll) = mesh(ii,jj)
        ENDIF
      END DO
!           - ( ii = 1, xsize )
    END DO
!         - ( jj = 1, ysize )


!!--------------------------------------------------------------------------        
!! Comments FH 2006   seed1( ), seed2( ), seed( ) are
!! assigned dimension MAXSEEDS = NCOL * NROW, where NCOL and NROW are
!! presently both set to 100. The line: seed1(ll) = jj + 1000 * ii above is
!! in preparation for depth-based sort of seed3( ) as 1-dimensional array.
!! i,j of any square can be recovered in GetNxt2 from seed1( ) so long 
!! as jj<= 999
!!--------------------------------------------------------------------------

!    invrat = 1 / ratio

!       -  sort seed array
!       -- seed1 = contains i,j indices for each square, these will be
!                  sorted so deepest locations come first
!       -- seed2 = initialized to zero, will be set to 1 as locations
!                  are used
!       - seed3 = contains depth info, this is used to sort seed1
    err = .FALSE.
    asc = .FALSE.
!   asc = .FALSE.  = sort in descending order
!!--------------------------------------------------------------------------        
!! Comments FH 2006  Here ll = number of mesh squares with +ve depths
!!                           = actual length of seed1(),seed2(),seed3()
!!--------------------------------------------------------------------------
    num = ll
    nseed = num
!!--------------------------------------------------------------------------        
!! Comments FH  No need to sort seed2( ) since all values zero at this point     
!!--------------------------------------------------------------------------        

        call Sort ( seed3, seed1, num, asc, err )
        IF ( err ) THEN
          cstr = 'Sort In Error In SetUpCluster, Execution Terminated.'
          call PigMessageOK(cstr, 'Error')
          aborted = .TRUE.
        ENDIF
!         - ( err )

    END

! **********************************************************

    SUBROUTINE SetUpSearch (maxsrch, search)

! PURPOSE: This routine initializes the search table used for spiral search.
!           The table has pairs of values defining a spiral search
!           pattern, it starts:
!               (1,0),  (1,1),   (0,1),  (-1,1),
!               (-1,0), (-1,-1), (0,-1), (1,-1)
!           at this point one is added to the first index
!           and the sequence is continued (see *** in listing)
!           ie: (2,-1), (2,0), (2,1), (2,2) etc...
!           At any point only one of search(1,ii) or search(2,ii) is
!           incremented. To keep track of this, count and notcnt
!           are used. Their values are either 1 or 2.
!           Called from Nnodit.

!-----------------------------------------------------------------------*

! *** passed variiables
    INTEGER maxsrch, SEARCH(2,maxsrch)

! - LOCAL VARIABLES
    integer ii, jj, l, m, count, notcnt, index
    integer savcnt, limit, num

! ii,jj = counters
! l,m   = store values of search table
!          l = search(1,ii)
!          m = search(2,ii)
! count,notcnt = keep track of which index of search table is to be incremented
! index = either 1 or -1, switches every time l equals m
! search = table of indices that map out a spiral around (0,0)
! savcnt = used to save value in count when switching counters
! limit = keeps track of when to increment index
! num = counts how many times limit has been incremented

!---------------START ROUTINE----------------------------------------

    count = 2
    notcnt = 1
    index = 1
    search(1,1) = 1
    search(2,1) = 0
    ii = 0
    limit = 8
    num = 2
    DO 200 jj = 1, maxsrch -1
      ii = ii + 1
      IF ( ii .eq. limit ) THEN
        limit = limit + 8 * num
        num = num + 1
! ***       here is where index is incremented (see *** above)
        search(1,ii+1) = search(1,ii) + 1
        search(2,ii+1) = search(2,ii)
!           - switch counter
        savcnt = count
        count = notcnt
        notcnt = savcnt
        GO TO 200
      ELSE
        l = search(1,ii)
        m = search(2,ii)
        IF ( IABS(l) .eq. IABS(m) ) THEN
!             - switch counter
          savcnt = count
          count = notcnt
          notcnt = savcnt
!             - switch index
          IF ( l .eq. m ) index = -index
        ENDIF
!             - ( IABS(l) = IABS(m) )
      ENDIF
!           - ( ii = limit )

!         - add index (either 1 or -1) to search value being incremented
      search(count,ii+1) = search(count,ii) + index
      search(notcnt,ii+1) = search(notcnt,ii)

!       END DO - ( jj = 1, maxsrch - 1 )
200     CONTINUE


    END

! **********************************************************

    SUBROUTINE Create2 ( nseed, maxsd, seed1, seed2, maxsrch, search, aborted )

       USE ClusterParams


! PURPOSE: This routine creates clusters. A cluster is a compact
!          group of squares from the underlying mesh. The # of squares
!          in a cluster is related to the depth at the first (also called
!          seed) location. Called from CreateClusterPoints
!!----------------------------------------------------------------------
!! Comments FH Nov 2006. 
!!    CREATE2 calls FIND2 to make each individual
!!    cluster. Number of good clusters sqnum formed with current mesh is
!!    incremented in FIND2, which also returns the centroid of each new
!!    cluster in [center(1,sqnum),center(2,sqnum)]. center( , ) is
!!    dimensioned in defn2.set .        FIND also updates
!!    occupancy indicator meshIndex(i,j) as it uses up mesh squares.
!!
!-----------------------------------------------------------------------*


! - PARAMETERS - (constants)
    REAL*4 seconds
    PARAMETER ( seconds = 2.0 )

! - "INCLUDES"
!    include '../includes/defn5.set'

! - PASSED VARIABLES
    INTEGER maxsrch, SEARCH(2,maxsrch)
    INTEGER nseed,maxsd,SEED1(MAXSD),SEED2(MAXSD)
    LOGICAL aborted

! - LOCAL VARIABLES
    integer ierr, err, countr, istart, jstart
       integer :: totsd, totuse
    CHARACTER*80 cstr

! err, ierr = flags
! countr = counter
! istart,jstart = indices of seed location

!------------START ROUTINE--------------------------------------

    aborted = .FALSE.
    sqnum = 0
    totuse = 0
    totsd = 0
    countr = -1

    DO WHILE ( sqnum .le. maxsd )  !maxcls )
      sqnum = sqnum + 1
      totsd = totsd + 1
      ierr = 0
!         - get next seed location
!          call PigPutMessage ( 'Calculating next seed location...' )
      call GetNxt2 ( countr, istart, jstart, err, nseed, maxsd, seed1, seed2 )
      IF ( err .eq. 0 ) THEN
!            call PigPutMessage ( 'Calculating find...' )
        call Find2 ( istart, jstart, ierr, maxsrch, search )
        IF ( ierr .eq. 0 ) THEN
!              call PigPutMessage ( 'Calculating boundaries...' )
!             - calculate boundaries for this cluster

!         REGION2 calculates cluster outlines and plots them
          call Region2
          totuse = totuse + numsq
         ENDIF
!              - ( ierr = 0 )
          ELSE
!            - all locations used up
             call PigMessageOK ( 'All Locations Used.','gen' )
!            call PigUwait ( seconds )
             call PigEraseMessage
             sqnum = sqnum - 1
             totsd = totsd - 1
             GO TO 350
      ENDIF
!            - ( err = 0 )

!          - check if user specified max # clusters created yet
          IF ( sqnum .eq. numr ) THEN
!            - output result
             call PigPutMessage ( 'Some Unused Seeds Remain.' )
             GO TO 350
          ENDIF
!            - ( sqnum = numr )
    END DO
!         - ( sqnum <=  maxcls )

        CONTINUE
        cstr = 'Max # Clusters Exceeded, Execution Terminated'
        call PigMessageOK(cstr, 'Error')
!       call PigPutMessage ( cstr )
!       call PigUwait ( seconds )
!       call PigEraseMessage
    aborted = .TRUE.

350     CONTINUE


    END

! **********************************************************

    SUBROUTINE GetNxt2 ( countr, newx, newy, err, nseed, maxsd, seed1, seed2 )

      USE MeshArrays
      USE ClusterParams

! PURPOSE: This routine gets next seed location. All locations have been
!          sorted by depth (deepest first). This routine searches for 
!          next unused location. 
! RETURNS: err = 0 if next seed location found
!              = 1 if all locations used
!-----------------------------------------------------------------------*


! - "INCLUDES"
!    include '../includes/defn5.set'

    LOGICAL defstart

! - PASSED VARIABLES
    integer countr, err, newx, newy
    INTEGER nseed,maxsd,SEED1(MAXSD),SEED2(MAXSD)

! - LOCAL VARIABLES
    integer, save :: start, end, istart, jstart, ii
    integer, save :: lastsd
! countr = # seeds tried since last display
! size = size of array mesh
! mesh(i,j) = array containing depths
! meshIndex(i,j) = array containing mesh occupancy info
! newx, newy = new seed location
! err = 1 if all locations used
! start, end = temp values
! ii, istart, jstart = indices

!---------------START ROUTINE----------------------------------------

    defstart = .TRUE.
    sizex = xsize
    sizey = ysize
!    lastsd = 0

    IF ( countr .eq. -1 ) THEN
!       - this is first time
      err = 0
      countr = 1
      IF ( .NOT. defstart ) THEN
!           - user entered starting position
!        newx = xpos
!        newy = ypos
        lastsd = 0
      ELSE
!           - use deepest location ( seed1(1) )
        newx = INT ( seed1(1) / 1000 )
        newy = seed1(1) - 1000 * newx
        seed2(1) = 1
        lastsd = 1
      ENDIF
!           - ( defstart )
      numsq = nint(area0 + ratio*mesh(newx,newy) &
                    + rat2*(mesh(newx,newy)**2) &
                    + dxb*newx/sizex + dyb*newy/sizey)
      IF ( numsq .lt. minnumsq )  numsq = minnumsq
      RETURN
    ENDIF
!         - ( countr = -1 )

!       - this is not the first time
    err = 0
    end = nseed
    start = lastsd + 1
    DO 200 ii = start, end
!         - has location already been used ?
      IF ( seed2(ii) .eq. 1 )  GO TO 200
      istart = INT ( seed1(ii) / 1000 )
      jstart = seed1(ii) - 1000 * istart
      IF ( meshIndex(istart,jstart) .eq. 0 )  GO TO 220
200     CONTINUE
!       END DO - ( 200 ii = start, end )

!       - all locations used
    err = 1
    RETURN

220     CONTINUE

!       - empty location found
    newx = istart
    newy = jstart
    countr = countr + 1
    numsq = nint(area0 + ratio * mesh(newx,newy) &
                  + rat2 * ( mesh(newx,newy)**2 ) &
                  + dxb*newx/sizex + dyb*newy/sizey)
    IF ( numsq .lt. minnumsq )  numsq = minnumsq
    lastsd = ii


    END

!-----------------------------------------------------------------------*

    SUBROUTINE Check2 ( newx, newy, ok, icount )

       USE MeshArrays

! PURPOSE: This routine checks if a new square is shoulder to shoulder
!          (not diagonal) with the previous squares. Two squares are
!          shoulder to shoulder if the sum of the absolute value of the
!          difference between old x and newx, and old y  and newy is 1.
!          Called from Find2.
!-----------------------------------------------------------------------*


! - "INCLUDES"
!    include '../includes/defn5.set'

! - PASSED VARIABLES
    integer newx, newy, icount
    CHARACTER*1 ok

! - LOCAL VARIABLES
    integer diff1, diff2, ii, sum

! diff1, diff2 = differences between old and new co-ordinates
! icount, ii = indices
! newx, newy = coordinates of new square
! ok = flag, 'Y' if new square is shoulder to shoulder 
! sum = temp value

!-----------------START ROUTINE---------------------------------

    ok = 'N'
    diff1 = agrid(1,icount) - newx
    diff2 = agrid(2,icount) - newy
    sum = IABS(diff1) + IABS(diff2)
    IF ( sum .eq. 1 ) THEN
!         - they are shoulder to shoulder so return
         ok = 'Y'
         RETURN
    ELSE
!         - the latest square is not shoulder to shoulder to the new one
!         -- so must search all squares found so far
      DO ii = 1, icount
        diff1 = agrid(1,ii) - newx
        diff2 = agrid(2,ii) - newy
        sum = IABS(diff1) + IABS(diff2)
        IF ( sum .eq. 1 ) THEN
          ok = 'Y'
          RETURN
        ENDIF
!             - ( sum = 1 )
      END DO
!           - ( ii = 1, icount )
    ENDIF
!         - ( sum = 1 )

    END

!-----------------------------------------------------------------------*

    SUBROUTINE Find2 ( istart, jstart, ierr, maxsrch, search )

       USE MeshArrays
       USE ClusterParams


! PURPOSE: This routine finds and fills unoccupied squares.
!          Called from Create.
! RETURNS: ierr = 0 when successful find
!               = 1 when search table is empty
!               = 2 when no good square found after iquit tries
!-----------------------------------------------------------------------*


! - "INCLUDES"
!    include '../includes/defn5.set'

! - LOCAL VARIABLES
    integer newi, newj, icount, ii
    integer ncount, iquit
    REAL sumi, sumj, sumisq, sumjsq, depsum, invsum, k
    integer deli, delj
    REAL a2, b, centx, centy, howfull
    CHARACTER*1 ok

! istart, jstart = indices of starting square
! centx, centy = x,y co-ordinates of centroid (relative to (0,0))
! a2 = 2nd moment of area about centroid
! b = normalized 2nd moment
! deli, delj = delta i, delta j
! sumi, sumj = sum of delta i, sum of delta j
! sumisq, sumjsq = sum of (delta I)**2, sum of (delta j)**2
! k = # squares in cluster so far 
! newi, newj = indices for next location to look at
! icount = keeps track of how many squares have been filled
! ncount = # squares searched since last usable location
! iquit = max # squares to search
! - PASSED VARIABLES
    integer istart, jstart, ierr
    INTEGER maxsrch, SEARCH(2,maxsrch)

!         iquit = 2 + 3*(number of squares in cluster so far)
! ierr = 1 when search table is empty
!      = 2 when no good square found after iquit tries
! ii = index
! ok = y/n if new square ok to use
! depsum = sum of depths in squares currently in cluster
! invsum = current icount * (1/ratio)

!--------------START ROUTINE-------------------------------------

    sumi = 0
    sumj = 0
    sumisq = 0
    sumjsq = 0
    ierr = 1
    newi = istart
    newj = jstart
    depsum = 0.0
    invsum = 0.0
    ncount = 0
    icount = 0
    sizex = xsize
    sizey = ysize


    DO 200 ii = 1, maxsrch !maxn
!         - search for empty locations

!         -- reject if new index is <= 0 
      IF ( newi .le. 0 .OR. newj .le. 0 ) GO TO 190
!         -- reject if new index is > size of array (mesh grid size)
      IF ( newi .gt. sizex .OR. newj .gt. sizey ) GO TO 190

      IF ( meshIndex(newi,newj) .eq. 0 ) THEN
!           - empty cell
        IF ( icount .gt. 0 ) THEN
          call Check2 ( newi, newj, ok, icount )
        ELSE
          ok = 'Y'
        ENDIF
!             - ( icount > 0 )
        IF ( ok .eq. 'Y' ) THEN
!          meshIndex(newi,newj) = sqnum  ! i.e. number of cluster to which
!          mesh square is assigned was stored in meshIndex. This was probably 
!          for some purpose now unnecessary
!          Line was replaced 18 Dec 06 with following
          meshIndex(newi,newj) = 1
!  
          icount = icount + 1
          ncount = 0
          agrid(1,icount) = newi
          agrid(2,icount) = newj
          depsum = depsum + mesh(newi,newj)
!          invrat = 1 / ratio
          invsum = invsum + 1./ratio   !invrat
          IF ( ii .gt. 1 ) THEN
!               - calculate centroid and 2nd moment of area about centroid
        deli = search(1,ii-1)
        delj = search(2,ii-1)
        sumi = sumi + deli
        sumj = sumj + delj
        sumisq = sumisq + deli*deli
        sumjsq = sumjsq + delj*delj
        k = icount
        centx = sumi / k
        centy = sumj / k
        a2 = sumisq + sumjsq - k * (centx*centx + centy*centy)
        b = 6 * a2 / (k*(k-1))
!               - following gears cluster size to mean depth of cluster
!               --stop adding to cluster if cluster large enough i.e. test if
!               -- icount >= (updated numsq)
        IF ( icount .ge. minnumsq ) THEN
          IF ( icount .ge. (area0 + ratio*(depsum/icount)+ rat2*(depsum/icount)**2+ dxb*centx/sizex+ dyb*centy/sizey) ) THEN
            ierr = 0
            GO TO 400
          ENDIF 
!                   - ( icount >= big eq'n )
        ENDIF
!                 - ( icount >= minnumsq )

!               - stop adding to cluster if b is greater than or equal to 1.5
!               -- ( after first 5 squares added )
        IF ( (ii .gt. 5) .AND. (b .ge. 1.5) ) THEN
!                 - reject if cluster is less than  required percent of size
          howfull = ( REAL(icount) / REAL(numsq) ) * 100
          IF ( howfull .lt. percnt ) THEN
            GO TO 250
          ELSE
            ierr = 0
            GO TO 400
          ENDIF
!                   - ( howfull < percnt )
        ENDIF
!                 - ( ii > 5  &  b >= 1.5 )
          ENDIF
!               - ( ii > 1 )
          IF ( icount .eq. numsq ) THEN
        ierr = 0
        GO TO 400
          ENDIF
!               - ( icount = numsq )
        ELSE
          GO TO 190
        ENDIF
!             - ( icount > 0 )
      ELSE
!           - not empty or useable cell
        ncount = ncount + 1
        iquit = 3*icount + 2
        IF ( ncount .gt. iquit ) THEN
          ierr = 2
          GO TO 250
        ENDIF
!             - ( ncount > iquit )
      ENDIF
!           - ( meshIndex(newi,newj) = 0 )
190       CONTINUE
!         - get next cell from spiral search table
      newi = istart + search(1,ii)
      newj = jstart + search(2,ii)
!       END DO - ( ii = 1, maxsrch ) !maxn )
200     CONTINUE

250     CONTINUE
!     - err encountered, either search table is exhausted or cluster rejected
!     -- because b (normalized 2nd moment of area about centroid) became too
!     -- big (ie: > 1.5) or no usable square found after iquit tries.
!     -- must not use this cluster

    DO ii = 1, icount
      newi = agrid(1,ii)
      newj = agrid(2,ii)
      agrid(1,ii) = 0
      agrid(2,ii) = 0
      meshIndex(newi,newj) = 0
    END DO
!         - ( ii = 1, icount )
    sqnum = sqnum - 1
    RETURN

400    CONTINUE
!      - to get here the routine must have found numsq usable locations
!      -- calculate centroid
!      -- must add 0.5 to both i and j because i,j is origin of unit
!      -- square and must use center of unit square to calculate centroid

     
!     CORRECTION 
!     The "I,J" coordinates of a cluster centroid were calculated 
!     until 3 Dec 06 using the incorrect formulas
!       center(1,sqnum) = istart + 0.5 + sumi/icount      !(Ia)
!       center(2,sqnum) = jstart + 0.5 + sumj/icount      !(Ib)
!     The correct formulas are 
       center(1,sqnum) = istart - 0.5 + sumi/icount       !(IIa)
       center(2,sqnum) = jstart - 0.5 + sumj/icount       !(IIb)
!     At this point, the position of the centroid is expressed as
!     its column number "I" and row number "J" in the mesh, these
!     numbers in general not being integer. For example, a cluster 
!     of the two squares 2,3 and 2,4 would have centroid 2,3.5

!      - convert to model co-ordinates

!     Until 3 Dec 06, the error in equations (I) was compensated for in
!     converting to model coordinates  by using the formulas
!       center(1,sqnum) = (center(1,sqnum)-1)*xoff + xstart
!       center(2,sqnum) = (center(2,sqnum)-1)*yoff + ystart
!     whereas the correct formulas (which go with Equations (II) are
!XX      center(1,sqnum) = center(1,sqnum)*xoff + xstart
!XX      center(2,sqnum) = center(2,sqnum)*yoff + ystart
      center(1,sqnum) = center(1,sqnum)*meshstep + xmsmin
      center(2,sqnum) = center(2,sqnum)*meshstep + ymsmin
!       xoff = yoff = meshstep
!       xstart = xmsmin, ystart = ymsmin
!     At this point center(1,*), center(2,*) are in model coordinates
!
!     Note that in GetDepth2, the calculation of the mesh square in 
!     which the centroid lies appears to be wrong. It should lie in
!     square with indices
!      INT[(center(1,*)-xstart)/xoff],INT[(center(2,*)-ystart)/yoff]
!     while GetDepth2 uses values 1 greater.
!     So long as depth of a cluster centroid is put equal to the depth  
!     in the mesh square where the centroid lies, above problem can be
!     preempted by setting cluster depth in FIND2 by
!       zdata(*) = mesh(1,ii,jj) = depth in mesh square with indices
!     ii=INT[istart - 0.5 + sumi/icount],jj=INT[jstart - 0.5 + sumj/icount]
!     and eliminating GetDepth2
!

       END

!-----------------------------------------------------------------------*

    SUBROUTINE Region2

       USE MeshArrays


! PURPOSE: This routine displays the region boundaries. It calculates 
!          boundaries by assuming index of square defines the square's
!          origin, then generates corners of each square. Edges of each
!          square are calculated and packed into variable Edge. These edges
!          are sorted. When there are duplicates, both occurances are deleted.
!          The remaining edges are unpacked and their pairs of coordinates
!          are stored in variable Cluster.
!-----------------------------------------------------------------------*


! - PARAMETERS - (constants)
    integer, parameter :: maxn = 2000
       REAL, PARAMETER ::  seconds = 2.0 

!       - EDGES stores packed indices of endpoints of each edge of one cluster
    CHARACTER*8 Edge(4*maxn)

!       - display flag for clusters
!    LOGICAL displclust

! - LOCAL VARIABLES
    integer ll, mm
    integer point1, point2, point3, point4
    CHARACTER*4 temp1(4*maxn+5), temp2(4*maxn+5)
    CHARACTER*1 pack1x(4*maxn+5), pack2x(4*maxn+5)
    CHARACTER*1 pack1y(4*maxn+5), pack2y(4*maxn+5)
    CHARACTER*3 iflag(4*maxn+5)
    common /regiontemp/ temp1, temp2, pack1x, pack2x, pack1y, pack2y, iflag
    integer j, last
    integer k1, k2, k3, k4
    integer tempx(4*maxn+5), tempy(4*maxn+5)
    common /regiontemp2/ tempx, tempy
    REAL edgex(2), edgey(2), ToMsh2

! point1 -> point4 = temp storage
! ll, mm = indices
! temp1, temp2 = store edges of one cluster
! pack1x, 2x, 1y, 2y = temp storage
! i, j, last = indices
! k1 -> k4 = temp
! tempx, tempy = temp
! iflage = flags mirror image

!----------------START ROUTINE-----------------------------------------

!    displclust = .TRUE.
       maxsq = maxn

    k4 = 4
    DO 200 j = 1, maxsq
      IF ( agrid(1,j) .eq. 0 ) THEN
        last = k4
        GO TO 220
      ENDIF
!           - ( agrid(1,j) = 0 )

!         - calculate corners for each box
      k1 = 1 + ( j - 1 ) * 4
      k2 = k1 + 1
      k3 = k1 + 2
      k4 = k1 + 3
      tempx(k1) = agrid(1,j)
      tempx(k2) = agrid(1,j) + 1
      tempx(k3) = agrid(1,j) + 1
      tempx(k4) = agrid(1,j)
      tempy(k1) = agrid(2,j)
      tempy(k2) = agrid(2,j)
      tempy(k3) = agrid(2,j) + 1
      tempy(k4) = agrid(2,j) + 1

!         - convert to base 127 using characters since indices are > 99
      DO mm = k1, k4
        pack1x(mm) = CHAR ( tempx(mm) / 127 )
        pack2x(mm) = CHAR ( MOD ( tempx(mm), 127 ) )
        pack1y(mm) = CHAR ( tempy(mm) / 127 )
        pack2y(mm) = CHAR ( MOD ( tempy(mm), 127 ) )
      END DO
!           - ( mm = k1, k4 )

!         - pack points into edge notation
          IF ( (j/2) * 2 .eq. j ) THEN
!           - switch order of points if even
            DO mm = k1, k3
              edge(mm) = pack1x(mm) // pack2x(mm) // pack1y(mm) // pack2y(mm) // &
                      pack1x(mm+1) // pack2x(mm+1) // pack1y(mm+1) // pack2y(mm+1)   
            END DO
!             - ( mm = k1, k3 )
            edge(k4) = pack1x(k4) // pack2x(k4) // pack1y(k4) // pack2y(k4) // &
                      pack1x(k1) // pack2x(k1) // pack1y(k1) // pack2y(k1)
          ELSE
            DO mm = k1, k3
              edge(mm) = pack1x(mm+1) // pack2x(mm+1) // pack1y(mm+1) // &
                    pack2y(mm+1) // pack1x(mm) // pack2x(mm) // pack1y(mm) // pack2y(mm)
            END DO
!             - ( mm = k1, k3 )
            edge(k4) = pack1x(k1) // pack2x(k1) // pack1y(k1) // pack2y(k1) // &
                    pack1x(k4) // pack2x(k4) // pack1y(k4) // pack2y(k4)
          ENDIF
!           -  ( (j/2) * 2 = j )
200     CONTINUE
    last = k4
220   CONTINUE

!       - sort edges, eliminate duplicates
       nedg = 4*maxn
    call SrtEdg2 (nedg, edge, last )
!       - unpack points, iflag flags mirror images so both occurances can
!       -- be deleted
    DO mm = 1, last
      temp1(mm) = edge(mm)(1:4)
      temp2(mm) = edge(mm)(5:8)
      iflag(mm) = 'OFF'
    END DO
!         - ( mm = 1, last )

!       - check if any pairs are mirror images
!       - ie: { temp1(i), temp2(i) = temp2(j), temp1(j) }
!       - if so, then discard both occurances
    ll = 0
    DO mm = 1, last
      DO j = 1, last
        IF ( temp1(mm) .eq. temp2(j) .AND.temp2(mm) .eq. temp1(j) ) THEN
          iflag(j) = 'ON'
          GO TO 460
        ELSE
          IF ( iflag(mm) .eq. 'ON' )  GO TO 460
        ENDIF
!             - ( temp1(mm) = temp2(j) & temp2(mm) = temp1(j) )
      END DO
!           - ( j = 1, last )
      ll = ll + 1
      temp1(ll) = temp1(mm)
      temp2(ll) = temp2(mm)
460       CONTINUE
    END DO
!         - ( mm = 1, last )
    last = ll
    DO mm = 1, last
      point1 = ICHAR ( temp1(mm)(1:1) ) * 127 + ICHAR ( temp1(mm)(2:2) )
      point2 = ICHAR ( temp1(mm)(3:3) ) * 127 + ICHAR ( temp1(mm)(4:4) )
      point3 = ICHAR ( temp2(mm)(1:1) ) * 127 + ICHAR ( temp2(mm)(2:2) )
      point4 = ICHAR ( temp2(mm)(3:3) ) * 127 + ICHAR ( temp2(mm)(4:4) )

      IF ( displclust ) THEN
           edgex(1) = ToMsh2 ( point1, xmsmin, meshstep )
        edgey(1) = ToMsh2 ( point2, ymsmin, meshstep )
        edgex(2) = ToMsh2 ( point3, xmsmin, meshstep )
        edgey(2) = ToMsh2 ( point4, ymsmin, meshstep )
!           call PigSetWindowNum ( MAINWIN )
!        call PigSetLineColour ( white )
        call PigDrawline ( 2, edgex, edgey, 3 )
      ENDIF
!           - ( displclust )
    END DO
!         - ( mm = 1, last )

!       - initialize array before next use (otherwise stray lines result)

       agrid = 0

    END

!-----------------------------------------------------------------------*

    SUBROUTINE SrtEdg2 (nedg, edge, num )

! PURPOSE: This routine sorts the edges and eliminates duplicates.
!          Called from Region2.
!-----------------------------------------------------------------------*


! - PARAMETERS - (constants)
    REAL*4 seconds
    PARAMETER ( seconds = 2.0 )

! *** passed variables
!       - edge contains packed indices of endpoints of each edge of 1 cluster
    integer nedg, num
       CHARACTER*8 edge(nedg)

! - LOCAL VARIABLES
    CHARACTER*8 oldnum
    CHARACTER*3 iflag
    CHARACTER*80 cstr
    LOGICAL asc, err
    integer mm, last, itest

! oldnum = temp value
! iflag = flag
! asc = TRUE for ascending order sort
! err = FALSE if no errors in sort
! num = # items to sort
! mm, last = indices

!---------------START ROUTINE----------------------------------------
        if(num.ge.nedg) then
                call PigPutMessage('SRTEDG: sorting edges')
        endif

        asc = .TRUE.
    err = .FALSE.

        call CSort ( edge, num, asc, err )
        IF ( err ) THEN
          cstr = 'CSort In Error, Execution Terminated.'
          call PigMessageOK(cstr, 'Error')
!         call PigPutMessage ( cstr )
!         call PigUwait ( seconds )
!         call PigEraseMessage
      RETURN
    ENDIF
!         - ( err )

!       - discard duplicates
    last = 1
    iflag = 'OFF'

      itest = 1
      if(itest.eq.0) then  

    do mm=2,num
        if(edge(last).ne.edge(mm)) then
            last = last + 1
        endif
        if(last.ne.mm) then
            edge(last) = edge(mm)
        endif
    end do
    num = last
      else
!***
!*** agd mar 94 - there is probably a bug here. The following code 
!*** will ALWAYS discard the contents of EDGE(1)!!! - replaced with above code
!***
!       - must create dummy final record (very large character)
        if(num+1.le.nedg) then
                edge(num+1) = '}}}}}}}}'
        endif
        oldnum = edge(1)
        DO mm = 2, min(num+1,nedg)
          IF ( edge(mm) .ne. oldnum ) THEN
            IF ( iflag .eq. 'OFF') THEN
              edge(last) =edge(mm-1)
              oldnum = edge(mm)
              last = last + 1
            ELSE
              iflag = 'OFF'
              oldnum = edge(mm)
            ENDIF
!             - ( iflag = OFF )
          ELSE
            iflag = 'ON'
          ENDIF
!           - ( edge(mm) ne oldnum )
        END DO
!         - ( mm = 2, num+1 )
        last = last - 1
        num = last
      endif

        if(num.ge.nedg) then
                call PigPutMessage('SRTEDG: sorting edges')
        endif
        END

! ********************************************************************

    REAL FUNCTION ToMsh2 ( s, base, step )

! PURPOSE: Given an index (row or column) in mesh grid matrix, return
!          X or Y coordinate of matrix entry.
!   GIVEN: s = an index (column or row) of an entry in rect.
!          base = offset.
!          step = step size of the grid.
! RETURNS: ToMsh2 = the X or Y coordinate of the entry
! WRITTEN: July 1990 by JDM for NODER, from ToMsh in NGHMESH.FOR.
!-----------------------------------------------------------------------*


! - PASSED VARIABLES
    REAL base, step
    integer s

!------------START FUNCTION--------------------------------------

    ToMsh2 = base + FLOAT (s - 1) * step

    RETURN
    END

! ********************************************************************

    integer FUNCTION ToMatx2 ( s, base, step )

! PURPOSE: To match the X or Y coordinate of a point to the entry in mesh 
!          and return the index (column,row) of the entry.
!   GIVEN: s = co-ordinate (x or y) of a point in the grid.
!          base = offset.
!          step = step size of the grid.
! RETURNS: ToMatx2 = the index (column or row) of the entry.
! WRITTEN: July 1990 by JDM for NODER, from ToMatx in NGHMESH.FOR
!-----------------------------------------------------------------------*


! - PASSED VARIABLES
    REAL s, base, step

!------------START FUNCTION--------------------------------------

    ToMatx2 = NINT ( (s - base) / step ) + 1
    END

! ********************************************************************

      subroutine SetGenDisplayOptions

      USE MeshArrays

      character(80) cstr
      character(1) ans

      cstr = 'Display Mesh?'
      call PigMessageYesNo (cstr, ans )
      IF (ans(1:1).EQ.'Y') THEN
        displcells = .true.
      else
        displcells = .false.
      endif

      cstr = 'Display Ref Grid?'
      call PigMessageYesNo (cstr, ans )
      IF (ans(1:1).EQ.'Y') THEN
        displtris = .true.
      else
        displtris = .false.
      endif

      cstr = 'Display Clusters?'
      call PigMessageYesNo (cstr, ans )
      IF (ans(1:1).EQ.'Y') THEN
        displclust = .true.
      else
        displclust = .false.
      endif

      end

! ********************************************************************


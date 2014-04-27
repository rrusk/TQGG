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

!*--------------------------------------------------------------------------*

    SubRoutine DeKite2(np,numngh,maxngh,xg,yg,ncode,nbrs,polylist)

!     Purpose:  To remove unrestrained nodes that have
!              four neighbours. Carried out by merging
!              node with closest neighbour. 
     
    IMPLICIT NONE

! - PASSED VARIABLES
      integer np,maxngh,numngh
      integer :: ncode(np),nbrs(maxngh,np)
      real xg(np),yg(np)
      logical polylist(np)
    
! - LOCAL VARIABLES! - LOCAL VARIABLES
      integer ndx, nb_count
!    real PXRAY1(numngh), PYRAY1(numngh)
!    real PXRAY2(numngh), PYRAY2(numngh)
    real dist1, dist2
    integer NBARRAY(4), NT1, NT2, NT3, NT4, NUMNBRS
    ! - counters
    integer j2, j3, j4, j5
      integer  k5
    logical INPOL


!------------------BEGIN------------------

      DO  ndx = 1, np
! *** Check that the node exists in the current grid state
!        IF ( EXIST(ndx) ) THEN
          INPOL = polylist(ndx)
          IF(nCODE(ndx).EQ.0.AND.INPOL) THEN
!    initialse counter and count number of neighbours
            nb_count = 0
            DO  k5 = 1, maxngh
!    count number of neighbours to this node
              IF ( nbrs(k5,ndx) .ne. 0 ) THEN
                nb_count = nb_count + 1
              ENDIF
            ENDDO
            IF ( nb_count .eq. 4 ) THEN !dekite
              IF ( ncode(ndx) /= 0 ) CYCLE  !boundary point
    ! SORT NEIGHBOURS OF OLD VERTEX (NDX) INTO CCW ORDER FROM EAST
              NUMNBRS=4
              call CCWSORT2(NDX,NUMNBRS,NBARRAY,np,maxngh,xg,yg,nbrs)
! Should have array NBARRAY that is of size (1,4) -the four entries
! containing the node indices of the four neighbours of the centre
! point of the kite...
              NT1 = NBARRAY(1)
              NT2 = NBARRAY(2)
              NT3 = NBARRAY(3)
              NT4 = NBARRAY(4)
              DIST1 = (xg(NT1)-xg(NT3))**2 + (yg(NT1)-yg(NT3))**2
              DIST2 = (xg(NT2)-xg(NT4))**2 + (yg(NT2)-yg(NT4))**2          

                      DO  j2 = 1,4 ! Remove ndx node from neighbour 
                                                        ! list of four neighbours
                        DO  j3 = 1,numngh
                              IF ( nbrs(j3,NBARRAY(j2)) .eq. ndx ) THEN
                                nbrs(j3,NBARRAY(j2))=0
                              ENDIF
                            ENDDO
              ENDDO

              IF ( dist1<dist2 ) THEN ! Set array to draw new line across shortest
                                    ! diagonal of kite 
                 DO  j4 = 1,numngh ! Add new connection in neighbour lists
                  IF ( nbrs(j4,NBARRAY(1)) == 0 ) THEN
                    nbrs(j4,NBARRAY(1)) = NBARRAY(3)
                      IF ( nbrs(j4,NBARRAY(1)) == NBARRAY(3) ) EXIT
                  ENDIF
                ENDDO
                DO  j5 = 1,numngh
                  IF ( nbrs(j5,NBARRAY(3)) == 0 ) THEN
                    nbrs(j5,NBARRAY(3)) = NBARRAY(1)
                    IF ( nbrs(j5,NBARRAY(3)) == NBARRAY(1) ) EXIT
                  ENDIF
                ENDDO
              ELSE
                DO  j4 = 1,numngh ! Add new connection in neighbour lists
                  IF ( nbrs(j4,NBARRAY(2)) == 0 ) THEN
                    nbrs(j4,NBARRAY(2)) = NBARRAY(4)
                    IF ( nbrs(j4,NBARRAY(2)) == NBARRAY(4) ) EXIT
                  ENDIF
                ENDDO
                DO  j5 = 1,numngh
                  IF ( nbrs(j5,NBARRAY(4)) == 0 ) THEN
                    nbrs(j5,NBARRAY(4)) = NBARRAY(2)
                    IF ( nbrs(j5,NBARRAY(4)) == NBARRAY(2) ) EXIT
                  ENDIF
                ENDDO
              ENDIF

              !- flag the point for deletion
              nbrs(:,ndx) = 0
              ncode(ndx)=-9
            ENDIF
          ENDIF
!        ENDIF
      ENDDO
    
      RETURN
      END
    
!*--------------------------------------------------------------------------*

    SubRoutine ChangeNodeCode (ncode1,ncode2,np,ncode,polylist)

!     Purpose:  

    IMPLICIT NONE

! - PASSED VARIABLES
      integer ncode1,ncode2,np
      integer :: ncode(np)
      logical polylist(np)
    
      integer j

      DO j=1,np
        if(ncode(j).eq.ncode1.and.polylist(j)) then
          ncode(j) = ncode2
        endif
      enddo

      RETURN
      END
    
!*--------------------------------------------------------------------------*

    SubRoutine ChangeElementCode (iecode1,iecode2,np,ne,ncn,nen,IECode,polylist)

!     Purpose:  

    IMPLICIT NONE

! - PASSED VARIABLES
      integer np,ne,ncn,iecode1,iecode2
      integer :: IECode(ne),nen(4,ne)
      logical polylist(np)

      integer j,k,n,ncn2

      neloop: DO j=1,ne
        if(iecode(j).eq.iecode1) then
          ncn2 = ncn -1 + min0(1,nen(ncn,j))
          do k=1,ncn2
            n = nen(k,j)
            if(.not.polylist(n)) cycle neloop
          enddo
          IECode(j) = iecode2
        endif
      enddo neloop
    
      RETURN
      END
    
!*--------------------------------------------------------------------------*
      SUBROUTINE RESHAPE2(np,maxngh,xg,yg,ncode,nbrs,polylist)

!  Purpose :    To improve equilateralness of triangles after grid 
!               editing. 
!               Only interior vertices (CODE=0) are moved; each such
!               vertex is moved to the mean position of its immediate
!               neighbours. The vertices are moved in order of their
!               index numbers. A fixed number MAXPASS of adjustment 
!               passes is made through the grid.
!               If IND = 1 , only nodes in working polygon are moved.
!               if IND = 2 , only nodes in current window are moved.   
!                  The depth at each vertex moved is recalculated by
!               linear interpolation after each move,
!                . The recalculated depths are 
!               only approximate, due to accumulation of interpolation
!               error, and depths throughout the adjusted grid should
!               be re-evaluated from a reference depth grid after the 
!               editing session, using program REDEP .
!                  Following a mod made 1 De! 88, a vertex is left where
!               it is if the proposed move (del X + del Y) is less than
!               a specified fraction EPS of the sum of the linear
!               dimensions of the polygon formed by its neighbours.

      implicit none


! *** Passed Variables ***
      integer np,maxngh
      integer ncode(np),nbrs(maxngh,np)
      real :: xg(np),yg(np),zg(np)
      logical polylist(np)

! *** Local Variables ***
      integer NUMPASS, I, J, TNBM, NBM(maxngh), VERT(3), MVNDX
      REAL XSUM, YSUM, XNEW, YNEW
      REAL x0,y0,X1, Y1, X2, Y2, X3, Y3, DA1, DA2, DA3,DA4, DELTA
      REAL A,B,C,Z1,Z2,Z3,AA1,AA2,B1,C1
      REAL XMIN, YMIN, XMAX, YMAX
      LOGICAL INPOL!, IN_BOX
      integer, parameter :: MAXPASS=3
      REAL, parameter :: EPS=0.01  

! *** LOGICAL FUNCTION
      LOGICAL aTr2

      DO 100 NUMPASS = 1, MAXPASS
!     ADJUST POSITIONS OF ELIGIBLE VERTICES
        DO 101 MVNDX = 1, np
!          if(EXIST(MVNDX)) then
          INPOL = polylist(mvndx)
!     Note: nodes with code = 90 are not moved
          IF(nCODE(MVNDX).EQ.0.AND.INPOL)THEN
            TNBM = 0
            XSUM = 0.
            YSUM = 0.
            XMIN = 1.E10
            YMIN = 1.E10
            XMAX = -1.E10
            YMAX = -1.E10
            DO J = 1, maxngh
              IF(nbrs(J,MVNDX).NE.0) THEN
                  TNBM = TNBM + 1
                  XSUM = XSUM + xg(nbrs(J,MVNDX))-xg(MVNDX)
                  YSUM = YSUM + yg(nbrs(J,MVNDX))-yg(MVNDX)
                  NBM(TNBM)=nbrs(J,MVNDX)
                  IF(xg(nbrs(J,MVNDX)).LT.XMIN)XMIN=xg(nbrs(J,MVNDX))
                  IF(yg(nbrs(J,MVNDX)).LT.YMIN)YMIN=yg(nbrs(J,MVNDX))
                  IF(xg(nbrs(J,MVNDX)).GT.XMAX)XMAX=xg(nbrs(J,MVNDX))
                  IF(yg(nbrs(J,MVNDX)).GT.YMAX)YMAX=yg(nbrs(J,MVNDX))
              ENDIF
            enddo
! *** Compute new position for node = mean position of neighbours 
            if(tnbm.gt.0) then
              XNEW = XSUM/TNBM
              YNEW = YSUM/TNBM
                  x0 = xnew  ! + xg(MVNDX)
                  y0 = ynew  ! + yg(MVNDX)
                else
!                     call PigPutMessage(' A node has no neighbors' )
!                     call PigUWait( 2. )
                      return
                endif
! *** Skip move if proposed change in position is too small
!           IF((ABS(XNEW-xg(MVNDX))+ABS(YNEW-yg(MVNDX)))
            IF((ABS(XNEW)+ABS(YNEW)).LT.EPS*((XMAX-XMIN)+(YMAX-YMIN))) GO TO 101
! *** Begin depth reevaluation for moved point
            VERT(1) = MVNDX
            X1 = 0. !xg(MVNDX)
            Y1 = 0. !yg(MVNDX)
! *** Find valid triangles around centre point (not yet moved)
            DO I = 1, TNBM -1
              VERT(2) = NBM(I)
              X2 = xg(VERT(2)) - xg(MVNDX)
              Y2 = yg(VERT(2)) - yg(MVNDX)
              DO J = I+1, TNBM
                VERT(3) = NBM(J)

! ***   Check if vert(1),vert(2),vert(3) form valid triangle
                if (aTr2(VERT,np,maxngh,nbrs)) then
                  X3 = xg(VERT(3)) - xg(MVNDX)
                  Y3 = yg(VERT(3)) - yg(MVNDX)
! ***   Check if new location is in this triangle
                  DA1=ABS((X1-X0)*(Y2-Y0)-(X2-X0)*(Y1-Y0))
                  DA2=ABS((X2-X0)*(Y3-Y0)-(X3-X0)*(Y2-Y0))
                  DA3=ABS((X3-X0)*(Y1-Y0)-(X1-X0)*(Y3-Y0))
                  DA4=ABS((X2-X1)*(Y3-Y1)-(X3-X1)*(Y2-Y1))

! ***     If delta >0 then point is outside triangle
                  IF (DA4.GT.0) THEN
                    DELTA=DA1+DA2+DA3-DA4
                    IF (DELTA.LE.DA4*1.E-6) THEN
! ***     Calculate linear depth fit coeffts. for this triangle
                      Z1 = zg(VERT(1))          
                      Z2 = zg(VERT(2))          
                      Z3 = zg(VERT(3))          
                      AA1 = Z1*Y2 - Z2*Y1 + Z3*Y1 - Z1*Y3 + Z2*Y3 - Z3*Y2
                      AA2 = X1*Y2 - X2*Y1 + X3*Y1 - X1*Y3 + X2*Y3 - X3*Y2
                      B1  = X2*Z3 - Z2*X3 + X3*Z1 - X1*Z3 + X1*Z2 - X2*Z1
                      C1  = X1*(Y2*Z3-Y3*Z2) + X2*(Y3*Z1-Y1*Z3) + X3*(Y1*Z2-Y2*Z1)
                      IF (AA2.NE.0) THEN 
                        A = AA1/AA2
                        B = B1/AA2
                        C = C1/AA2
                      ELSE
                        A = 0
                        B = 0
                        C = 0
                      ENDIF
                      zg(MVNDX) = A * X0 + B * Y0 + C  
                      GO TO 600
                    ENDIF
                  ENDIF
                endif
              enddo
            enddo
600         CONTINUE
            xg(MVNDX) = x0 + xg(MVNDX)
            yg(MVNDX) = y0 + yg(MVNDX)
          ENDIF
!          ENDIF
101     CONTINUE
100   CONTINUE

      return
      END


!*--------------------------------------------------------------------------*
! ***************************************************************************

        SUBROUTINE Refine_by2(maxnp,np,maxngh,maxne,ne,xg,yg,zg,ncode,Polylist,nbrs,nen)
! 
! Purpose : Refine a triangular grid by adding a vertex on the triangle 
!               edges (1 triangle => 4 triangles). Refines inside a
!               polygon or other criterion.
! Returns : New neighbour file.
!
! Written : 15 June 2010 by Roy Walters.

        IMPLICIT NONE

! *** passed variables
     integer :: maxnp,np,maxngh,maxne,ne
     integer :: nen(4,maxne),ncode(maxnp),nbrs(maxngh,maxnp)
     real :: xg(maxnp),yg(maxnp),zg(maxnp)
     logical Polylist(maxnp)

! Local Variables
    LOGICAL colorit, present_flag
    INTEGER i, j, count_not, jj, jjj, k1, k2, c3
    INTEGER idx, nots(2), np1, np2
    INTEGER ndx, ierr, new_pt(3), n1, splitTri_pt, cleve_node
    REAL xNew, yNew, dist, range

! ******************* START CODE HERE *************************

!       - mark all points in polygon & color triangles totally in polygon
    DO i = 1, ne
!         - see if triangle totally in poly, if so color it
      colorit = .TRUE.
      count_not = 3 ! number of nodes per triangle NOT in polygon        
      DO j = 1, 3
        idx = nen(j,i)
        IF ( .NOT. PolyList(idx) ) THEN
          colorit = .FALSE.
          count_not = count_not-1
        ENDIF
      ENDDO

      IF ( colorit ) THEN
! inject nodes...
        DO j = 1,3 ! Loop creating list of nodes to inject
          np1  = nen(j,i) ! Get two points from TriList
          np2  = nen(mod(j,3)+1,i)
          xNew = (xg(np1)+xg(np2))*.5 ! Find mid point of side
          yNew = (yg(np1)+yg(np2))*.5
          dist = (xg(np1)-xg(np2))**2 + (yg(np1)-yg(np2))**2
          range = sqrt(dist)*0.05
          ierr=1
          do jj=1,np
            if(ncode(jj).ge.0) then    
              if(xnew.gt.(xg(jj)-range).and.xnew.lt.(xg(jj)+range)) then
                if(ynew.gt.(yg(jj)-range).and.ynew.lt.(yg(jj)+range)) then
                  ierr=0
                  ndx = jj
                  exit
                endif
              endif
            endif
          enddo
          IF ( ierr == 0 ) THEN ! 
            new_pt(j) = ndx
          ELSE  ! Set new node parameters
            new_pt(j) = np+1
            np = np+1
            xg(np)=xNew
            yg(np)=yNew
            zg(np) = (zg(np1)+zg(np2))*.5
            IF ( ncode(np1)==1.and.ncode(np2)==1 ) THEN
              ncode(np) = 1
            ELSEIF ( ncode(np1)==2.and.ncode(np2)==2 ) THEN
              ncode(np) = 2
            ELSEIF ( ncode(np1)==5.OR.ncode(np1)==6.AND.ncode(np2)==5.OR.ncode(np2)==6 ) THEN
              ncode(np) = 5
            ELSEIF ( ncode(np1)==5.OR.ncode(np1)==6.AND.ncode(np2)==10.OR.ncode(np2)==11 ) THEN
              ncode(np) = 10
            ELSEIF ( ncode(np1)==10.OR.ncode(np1)==11.AND.ncode(np2)==5.OR.ncode(np2)==6 ) THEN
              ncode(np) = 10
            ELSEIF ( ncode(np1)==7.OR.ncode(np1)==8.AND.ncode(np2)==7.OR.ncode(np2)==8 ) THEN
              ncode(np) = 7
            ELSEIF ( ncode(np1)==7.OR.ncode(np1)==8.AND.ncode(np2)==10.OR.ncode(np2)==11 ) THEN
              ncode(np) = 10
            ELSEIF ( ncode(np1)==10.OR.ncode(np1)==11.AND.ncode(np2)==7.OR.ncode(np2)==8 ) THEN
              ncode(np) = 10
            ELSEIF ( ncode(np1)==10.OR.ncode(np1)==11.AND.ncode(np2)==10.OR.ncode(np2)==11 ) THEN
              ncode(np) = 10
            ELSEIF ( ncode(np1)==90.and.ncode(np2)==90 ) THEN
              ncode(np) = 90
            ELSE
              ncode(np) = 0
            ENDIF
          ENDIF
        ENDDO ! At this point should have array new_pt(3) that has the node numbers
              ! of the new pts (may be existing or new). Pts 1,2,3 are rel to the 
              ! order of the nodes in the TriList. i.e. new_pt(1) between nen(1,i)
              ! and nen(2,i).

        DO j = 1,3 ! Loop updating neighbour lists for the 
            n1=nen(j,i) ! three original nodes of triangle
            jj=mod(j,3)+1
            jjj=mod(jj,3)+1
            DO k1 = 1,maxngh
                IF ( nbrs(k1,n1)==nen(jj,i) ) THEN
                    nbrs(k1,n1)=new_pt(j)
                ENDIF
                IF ( nbrs(k1,n1)==nen(jjj,i) ) THEN
                    nbrs(k1,n1)=new_pt(jjj)
                ENDIF
            ENDDO
        ENDDO
        DO j = 1,3 ! Loop updating/creating neighbours lists
            n1=new_pt(j) ! for the three injected nodes
            jj=mod(j,3)+1
            jjj=mod(jj,3)+1
            DO k1 = 1,maxngh
                IF ( nbrs(k1,n1)==0 ) THEN
                    nbrs(k1,n1)=new_pt(jj)
                    EXIT
                ENDIF
            ENDDO
            DO k1 = 1,maxngh
                IF ( nbrs(k1,n1)==0 ) THEN
                    nbrs(k1,n1)=new_pt(jjj)
                    EXIT
                ENDIF
            ENDDO
            present_flag=.FALSE.
            DO k1 = 1,maxngh ! check if node connection present, if not add it
                IF ( nbrs(k1,n1)==nen(j,i) ) THEN
                                        present_flag=.TRUE.
                                ENDIF
                        ENDDO
                        IF ( present_flag.eqv..FALSE. ) THEN
                                DO k2 = 1,maxngh
                                        IF ( nbrs(k2,n1)==0 ) THEN
                                                nbrs(k2,n1)=nen(j,i)
                        EXIT
                    ENDIF
                ENDDO
            ENDIF
            present_flag=.FALSE.
            DO k1 = 1,maxngh ! 
                IF ( nbrs(k1,n1)==nen(jj,i) ) THEN
                                        present_flag=.TRUE.
                                ENDIF
                        ENDDO
                        IF ( present_flag.eqv..FALSE. ) THEN
                                DO k2 = 1,maxngh
                                        IF ( nbrs(k2,n1)==0 ) THEN
                                                nbrs(k2,n1)=nen(jj,i)
                        EXIT
                    ENDIF
                ENDDO
            ENDIF
        ENDDO
      ENDIF

! Following code (still in 'i' loop) deals with triangles  
    IF ( count_not==2 ) THEN ! with only TWO nodes in poly
        c3=0
        DO j = 1,3 ! Find the node number that belongs to TRI but not in poly
            IF ( .NOT. PolyList(nen(j,i)) ) THEN
                cleve_node=nen(j,i)
            ELSE
                c3=c3+1
                nots(c3)=nen(j,i)
            ENDIF
        ENDDO
        xNew = (xg(nots(1))+xg(nots(2)))*.5
        yNew = (yg(nots(1))+yg(nots(2)))*.5
        dist = (xg(nots(1))-xg(nots(2)))**2 + (yg(nots(1))-yg(nots(2)))**2
        range = sqrt(dist)*0.05
        ierr=1
        do jj=1,np
          if(xnew.gt.(xg(jj)-range).and.xnew.lt.(xg(jj)+range)) then
            if(ynew.gt.(yg(jj)-range).and.ynew.lt.(yg(jj)+range)) then
              ierr=0
              ndx = jj
              exit
            endif
          endif
        enddo
        IF ( ierr == 0 ) THEN ! 
            splitTri_pt = ndx
        ELSE  ! Set new node parameters
            splitTri_pt = np+1
            np = np+1
            xg(np)=xNew
            yg(np)=yNew
            zg(np) = (zg(nots(1))+zg(nots(2)))*.5
            IF ( ncode(nots(1))==1.and.ncode(nots(2))==1 ) THEN
                ncode(np) = 1
            ELSEIF ( ncode(nots(1))==2.and.ncode(nots(2))==2 ) THEN
                ncode(np) = 2
            ELSEIF ( ncode(nots(1))==5.OR.ncode(nots(1))==6.AND.ncode(nots(2))==5.OR.ncode(nots(2))==6 ) THEN
                ncode(np) = 5
            ELSEIF ( ncode(nots(1))==5.OR.ncode(nots(1))==6.AND.ncode(nots(2))==10.OR.ncode(nots(2))==11 ) THEN
                ncode(np) = 10
            ELSEIF ( ncode(nots(1))==10.OR.ncode(nots(1))==11.AND.ncode(nots(2))==5.OR.ncode(nots(2))==6 ) THEN
                ncode(np) = 10
            ELSEIF ( ncode(nots(1))==7.OR.ncode(nots(1))==8.AND.ncode(nots(2))==7.OR.ncode(nots(2))==8 ) THEN
                ncode(np) = 7
              ELSEIF ( ncode(nots(1))==7.OR.ncode(nots(1))==8.AND.ncode(nots(2))==10.OR.ncode(nots(2))==11 ) THEN
                  ncode(np) = 10
              ELSEIF ( ncode(nots(1))==10.OR.ncode(nots(1))==11.AND.ncode(nots(2))==7.OR.ncode(nots(2))==8 ) THEN
                  ncode(np) = 10
            ELSEIF ( ncode(nots(1))==10.OR.ncode(nots(1))==11.AND.ncode(nots(2))==10.OR.ncode(nots(2))==11 ) THEN
                ncode(np) = 10
            ELSEIF ( ncode(nots(1))==90.and.ncode(nots(2))==90 ) THEN
                ncode(np) = 90
            ELSE
                ncode(np) = 0
            ENDIF
        ENDIF

! Update neighbour lists
        n1 = nots(1)
        DO k1 = 1,maxngh
            IF ( nbrs(k1,n1)==nots(2) ) THEN
                nbrs(k1,n1)=splitTri_pt
            ENDIF
        ENDDO
        n1 = nots(2)
        DO k1 = 1,maxngh
            !jj=j+1
            !IF ( jj>3 ) jj=1
            IF ( nbrs(k1,n1)==nots(1) ) THEN
                nbrs(k1,n1)=splitTri_pt
            ENDIF
        ENDDO
        n1 = cleve_node
        DO k1 = 1,maxngh
            !jj=j+1
            !IF ( jj>3 ) jj=1
            IF ( nbrs(k1,n1)==0 ) THEN
                nbrs(k1,n1)=splitTri_pt
                EXIT
            ENDIF
        ENDDO
! Now make a nbrs for the new splitTri_pt
        present_flag=.FALSE.
        DO k1 = 1,maxngh ! check if node connection present, if not add it
            IF ( nbrs(k1,splitTri_pt)==nots(1) ) THEN
                                present_flag=.TRUE.
                        ENDIF
                ENDDO
                IF ( present_flag.eqv..FALSE. ) THEN
                        DO k2 = 1,maxngh
                                IF ( nbrs(k2,splitTri_pt)==0 ) THEN
                                        nbrs(k2,splitTri_pt)=nots(1)
                    EXIT
                ENDIF
            ENDDO
        ENDIF
        present_flag=.FALSE.
        DO k1 = 1,maxngh ! 
            IF ( nbrs(k1,splitTri_pt)==nots(2) ) THEN
                                present_flag=.TRUE.
                        ENDIF
                ENDDO
                IF ( present_flag.eqv..FALSE. ) THEN
                        DO k2 = 1,maxngh
                                IF ( nbrs(k2,splitTri_pt)==0 ) THEN
                                        nbrs(k2,splitTri_pt)=nots(2)
                    EXIT
                ENDIF
            ENDDO
        ENDIF
        present_flag=.FALSE.
        DO k1 = 1,maxngh ! 
            IF ( nbrs(k1,splitTri_pt)==cleve_node ) THEN
                                present_flag=.TRUE.
                        ENDIF
                ENDDO
                IF ( present_flag.eqv..FALSE. ) THEN
                        DO k2 = 1,maxngh
                                IF ( nbrs(k2,splitTri_pt)==0 ) THEN
                                        nbrs(k2,splitTri_pt)=cleve_node
                    EXIT
                ENDIF
            ENDDO
        ENDIF
    ENDIF

    ENDDO

    END

!-----------------------------------------------------------------------*

      SUBROUTINE DeleteGridInPoly2 (np,maxngh,ncode,nbrs,polylist)

!     PURPOSE : To handle the deletion of a subgrid
!     GIVEN : nrec - number of records in the original data file
!     RETURNS :
!--------------------------------------------------------------------

      IMPLICIT NONE

! *** PASSED VARIABLES ***
      INTEGER np,maxngh
      integer ncode(np),nbrs(maxngh,np)
      logical polylist(np)

! *** LOCAL VARIABLES ***
      INTEGER j,nb,k,kk

!--------------- BEGIN --------------------
        
      do j=1,np
        if(polylist(j)) ncode(j) = -9
      enddo

! *** remove j from nbrs list
      do j=1,np
        if(ncode(j).eq.-9) then
          do k=1,maxngh
            nb = nbrs(k,j)
            if(nb.gt.0) then
              if(ncode(nb).ge.0) ncode(nb)=90
              do kk = 1,maxngh
                if(nbrs(kk,nb).eq.j) then
                  nbrs(kk,nb) = 0
                endif
              enddo
            endif
          enddo
! *** remove nbrs of j
          nbrs(:,j) = 0
        endif
      enddo

! *** eliminate nodes with one ngh
      call RemoveDanglingNodes (np,maxngh,nbrs)
      
      return
      end

!-----------------------------------------------------------------------*

      SUBROUTINE SplitGridInPoly (np,maxngh,ncode,nbrs,polylist)

!     PURPOSE : To handle the removal of a subgrid from a grid.
!       Mark outside nodes with ncode=-9 and negate outside nbrs. Write.
!       Then mark inside nodes with ncode=-9 and negate inside nbrs. Write.
!     RETURNS :
!--------------------------------------------------------------------

      IMPLICIT NONE

! *** PASSED VARIABLES ***
      INTEGER np,maxngh
      integer ncode(np),nbrs(maxngh,np)
      logical polylist(np)

! *** LOCAL VARIABLES ***
      INTEGER j,nb,k,iunit
      integer nindex(0:np)
      character(256) fname
          logical ResOK, PigOpenfile
          character ans*1

      do j=1,np
        if(.not.polylist(j)) then
          ncode(j) = ncode(j) - 9000
        else
! *** remove outside nodes from nbrs list
          do k=1,maxngh
            nb = nbrs(k,j)
            if(nb.gt.0) then
              if(.not.polylist(nb)) then
                nbrs(k,j) = -nb
                ncode(j)=91
              endif
            endif
          enddo
        endif
      enddo
      
      iunit = 21
      call PigMessageYesNo( ' Write grid inside polygon?', ans)
      if(ans(1:1).eq.'Y'.or.ans(1:1).eq.'y') then
        ResOK = PigOpenFile(iunit,'Open NGH Ouput File',fname, &
         "Output file(*.ngh),*.ngh;All files(*.*),*.*;")

        if(.not.resOK) then
          call PigMessageOK(' Error opening ngh output file','ERROR')
        else
          call write_ngh_file(iunit,nindex)
          close (iunit)
        endif
      endif

      
! *** clean up
      do j=1,np
        if(ncode(j).lt.-8000) then
          ncode(j) = ncode(j) + 9000
        endif
      enddo
      nbrs = iabs(nbrs)

! *** now do grid outside polygon with cross links.
      do j=1,np
        if(polylist(j)) then
          if(ncode(j).ne.91) ncode(j) = ncode(j) - 9000
        else
! *** remove inside nodes from nbrs list
!          do k=1,maxngh
!            nb = nbrs(k,j)
!            if(nb.gt.0) then
!              if(polylist(nb)) then
!                nbrs(k,j) = -nb
!                ncode(j)=91
!              endif
!            endif
!          enddo
        endif
      enddo

      call PigMessageYesNo( ' Write grid outside polygon?', ans)
      if(ans.eq.'Y'.or.ans.eq.'y') then
        ResOK = PigOpenFile(iunit,'Open NGH Ouput File',fname, &
         "Output file(*.ngh),*.ngh;All files(*.*),*.*;")

        if(.not.resOK) then
          call PigMessageOK(' Error opening ngh output file','ERROR')
        else
          call write_ngh_file(iunit,nindex)
          close (iunit)
        endif
      endif
      
      return
      end

!-----------------------------------------------------------------------*

      SUBROUTINE CutGridInPoly (np,maxngh,ncode,nbrs,zg,zlimit,zlow,polylist)

!     PURPOSE : To handle the deletion of a subgrid by elevation cutoff
!     GIVEN : nrec - number of records in the original data file
!     RETURNS :
!--------------------------------------------------------------------

      IMPLICIT NONE

! *** PASSED VARIABLES ***
      INTEGER np,maxngh
      integer ncode(np),nbrs(maxngh,np)
      real :: zlimit,zlow,zg(np)
      logical polylist(np)

! *** LOCAL VARIABLES ***
      INTEGER i,j,nb,k,kk

!--------------- BEGIN --------------------

!       - now determine points that are inside currently active poly
        do i=1,np
          if(zg(i).le.zlimit.and.zg(i).ge.zlow) then
            PolyList(i) = .FALSE.
          endif
        ENDDO

        
      do j=1,np
        if(polylist(j)) ncode(j) = -9
      enddo

! *** remove j from nbrs list
      do j=1,np
        if(ncode(j).eq.-9) then
          do k=1,maxngh
            nb = nbrs(k,j)
            if(nb.gt.0) then
              if(ncode(nb).ge.0) ncode(nb)=90
              do kk = 1,maxngh
                if(nbrs(kk,nb).eq.j) then
                  nbrs(kk,nb) = 0
                endif
              enddo
            endif
          enddo
! *** remove nbrs of j
          nbrs(:,j) = 0
        endif
      enddo

! *** eliminate nodes with one ngh
      call RemoveDanglingNodes (np,maxngh,nbrs)
!      call RemoveDanglingLinks (np,maxngh,nbrs,ne,nen)
            
      return
      end

! ***********************************************************************

    SUBROUTINE ReDepth2 (np,xg,yg,zg,polylist)

!    Purpose - This program re-evaluates the depths of a model grid
!    in NEIGH format. Usually used to correct depths after a grid
!    has been edited. 
!
!    Method - The model grid depths are obtained by interpolating
!    linearly within the triangles of a depth grid (also in NEIGH format).
!
! ***********************************************************************
!      REDEP outputs a log file REDEP.LOG containing information about
!      any model nodes found to be outside the depth grid or very close 
!      to the boundary. 
! *********************************************************************

      IMPLICIT NONE

! *** passed variables
      integer np
      real :: xg(np),yg(np),zg(np)
      logical polylist(np)

! *** local variables
      LOGICAL Quit

        call GetRefGrid( Quit )

        call InterpRefGrid( np, xg, yg, zg, polylist)

        call CloseRefGrid( Quit )

      END

! ***********************************************************************

    SUBROUTINE SetDepth2 (np,zscale,zlimit,zg,polylist)

!    Purpose - This program sets the depths within a polygon.

      IMPLICIT NONE

! *** passed variables
      integer np
      real :: zg(np),zscale,zlimit
      logical polylist(np)

! *** local variables
      integer j

      DO j=1,np
        if(polylist(j)) then
          zg(j) = zscale*zg(j) + zlimit
        endif
      enddo

      END

! *********************************************************************

    SUBROUTINE RemoveDanglingNodes (np,maxngh,nbrs)

!    Purpose - This s/r removes nodes with 1 neighbour

      IMPLICIT NONE

! *** passed variables
      INTEGER np,maxngh
      integer nbrs(maxngh,np)

! *** local variables
      integer j,k,nongh,nb,nb1,nb2
      
      do j=1,np
        nb = j
        nongh = 0
        do k=1,maxngh
          if(nbrs(k,j).gt.0) then
            nongh = nongh +1
            nb1 = nbrs(k,j)
          endif
        enddo
        do
          if(nongh.ne.1) exit
          nbrs(:,nb) = 0
          nongh = 0
          do k=1,maxngh
            if(nbrs(k,nb1).eq.nb) then
              nbrs(k,nb1) = 0
            elseif(nbrs(k,nb1).gt.0) then
              nongh = nongh +1
              nb2 = nbrs(k,nb1)
            endif
          enddo
          nb = nb1
          nb1 = nb2
          nb2 = 0
        enddo
      enddo
      
      END

! *********************************************************************

    SUBROUTINE RemoveDanglingLinks (np,maxngh,numngh,nbrs,ne,nen)

!    Purpose - This s/r removes edges that are not in an element.

      IMPLICIT NONE

! *** passed variables
      INTEGER np,maxngh,numngh,ne
      integer nbrs(maxngh,np),nen(4,ne)

! *** local variables
      integer nonbrs(np)
!   CUVX - current vertex being considered
      INTEGER CUVX
!   CUNBR - current neighbour being considered
      INTEGER CUNBR
      INTEGER II,JJ,LL,MM
      LOGICAL pass1, NEWNBR
      character cstr*80

!   Starting neighbor list

!   Set count of nbrs to zero for all nodes
      
      nonbrs = 0
      nbrs = 0
      
! *** Check each triangle and check that each vertex is in each of 
! *** the other two vertices' neighbour lists

        pass1 = .true.
        DO JJ = 1, ne
!            IECode(JJ) = 1 
! *** Check each vertex in current triangle
          DO II = 1, 3
! *** Choose current vertex
            CUVX = nen(II,JJ)
! *** Take other two vertices in turn
            DO LL = 1,3
              IF(LL.eq.II) cycle
!              Choose current neighbour of chosen vertex 
              CUNBR = nen(LL,JJ)
!              Check if CUNBR is already in neighbour list of CUVX
              NEWNBR = .TRUE.
              IF(NONBRS(CUVX).gt.0) then
                DO MM = 1, NONBRS(CUVX)
                  IF(CUNBR.eq.nbrs(MM,CUVX)) NEWNBR = .FALSE.
                enddo
              endif
!              If CUNBR is new neighbour of CUVX, add to list
              IF(NEWNBR) THEN     
                if(nonbrs(cuvx).ge.maxngh) then
                  if(pass1) then
                    cstr =' Too many neighbor points - truncating:'
                        call PigMessageOK(cstr, 'Error')
                    pass1 = .false.
                  endif
                else
                  NONBRS(CUVX) = NONBRS(CUVX) + 1
                  nbrs(NONBRS(CUVX),CUVX) = CUNBR
                endif
              ENDIF
            enddo
          enddo 
        enddo

!       Find max number of neighbours
        numngh = 0
        DO II = 1, np
          IF(NONBRS(II).gt.numngh) numngh = NONBRS(II)
        enddo
                
      END

! *********************************************************************

!**********************************************************************

      SUBROUTINE RemoveNotExist(np,ncode,maxngh,nbrs)

      Implicit none

! *** passed variables
      integer :: np,maxngh,ncode(np),nbrs(maxngh,np)
!      logical exist(np)
      
! *** local variables
      integer :: nb,j,k,kk

!      do j=1,np
!        if(.not.exist(j)) then
!          ncode(j) = -9
!        endif
!      enddo

! *** remove j from nbrs list
      do j=1,np
        if(ncode(j).eq.-9) then
          do k=1,maxngh
            nb = nbrs(k,j)
            if(nb.gt.0) then
!              if(ncode(nb).ge.0) ncode(nb)=90
              do kk = 1,maxngh
                if(nbrs(kk,nb).eq.j) then
                  nbrs(kk,nb) = 0
                endif
              enddo
            endif
          enddo
! *** remove nbrs of j
          nbrs(:,j) = 0
        endif
      enddo
      
      return
      end
! *********************************************************************

!**********************************************************************
!-----------------------------------------------------------------------*
!                       NODEPOLY.FOR                                    *
!       This module contains procedures for defining a subset of the    *
!       node data with a polygon. Nodes in the polygon may be           *
!       manipulated.                                                    *
!       ROUTINES - DelPolyNodes, CleanNodes, ThinoutNodes               *
!-----------------------------------------------------------------------*

      SUBROUTINE DelPolyNodes (deltype,polylist,TotCoords,Totbndys,&
                               TotIntpts,PtsThisBnd,dxray,dyray,depth,code)
      IMPLICIT NONE

! PURPOSE: To delete all specified type nodes ( interior, boundary or
!          both ) in the currently active polygon.
! GIVEN:   deltype = type of nodes to delete,
!                    'I' = delete interior nodes only,
!                    'B' = delete boundary nodes only,
!                    'A' = delete all nodes (interior & boundary),
!                    'Q' = user quit, delete no nodes.
! RETURNS: None.
! EFFECTS: Specified nodes in active polygon are deleted, if confirmed.
!----------------------------------------------------------------------*

! - PASSED VARIABLES
      CHARACTER*1 deltype
      integer TotCoords,Totbndys,TotIntpts
      logical polylist(totcoords)
      integer PtsThisBnd(Totbndys),code(TotCoords)
      real dxray(TotCoords),dyray(TotCoords),depth(TotCoords)

! - LOCAL VARIABLES
      CHARACTER*80 cstr 
      CHARACTER*1 ans
      integer i, j,jj
      REAL xmark, ymark
      LOGICAL Quitflag

!----------------------START ROUTINE----------------------------------
      
      IF ( deltype .eq. 'Q' ) THEN
!         - user wants to quit
        Quitflag = .TRUE.
      else
        Quitflag = .FALSE.
      ENDIF

      IF ( .NOT. Quitflag ) THEN

        IF ( deltype .eq. 'I' ) THEN
           polylist(1:TotCoords-TotIntPts) = .false.
        elseif ( deltype .eq. 'B' ) THEN
          polylist(TotCoords-TotIntPts+1: TotCoords) = .false.
        endif

        DO j = 1, TotCoords
          if(polylist(j)) then
            xmark = dxray (j)
            ymark = dyray (j)
            call PigDrawModifySymbol ( xmark, ymark )
          endif
         END DO

!         - now get deletion confirmation
        cstr = 'Delete these nodes ?:'
        call PigMessageYesNo (cstr, ans)

        IF ( ans(1:1) .eq. 'Y' ) THEN
!           - confirmed nodes to be deleted, remove highlited nodes
          DO j = 1, TotCoords
            if(polylist(j)) then
              code(j) = code(j) - 100000
              xmark = dxray (j)
              ymark = dyray (j)
              call PigEraseModifySymbol ( xmark, ymark )
            endif
          END DO

!           - now delete the actual nodes
          call CleanNodes (deltype,polylist,TotCoords,Totbndys,TotIntpts, &
                             PtsThisBnd,dxray,dyray,depth)
          code = 0
          code(1:PtsThisBnd(1)) = 1
          i = PtsThisBnd(1)
          do j = 2, TotBndys
            do jj = 1, PtsThisBnd(j)
              i = i+1
              code(i) = 2
            enddo
          end do
          TotIntPts = Totcoords - i 

        ELSE
!           - user does not want to delete nodes shown, remove highlited nodes
          IF ( deltype .eq. 'I' .or. deltype .eq. 'A' )  then
            DO j = TotCoords-TotIntPts+1, TotCoords
              if(polylist(j)) then
                xmark = dxray (j)
                ymark = dyray (j)
                call PigDrawIntSymbol ( xmark, ymark )
              endif  
            enddo
          endif

          IF ( deltype .eq. 'B' .or. deltype .eq. 'A' )  then
            DO j = 1,TotCoords-TotIntPts
              if(polylist(j)) then
                xmark = dxray (j)
                ymark = dyray (j)
                call PigDrawBndSymbol ( xmark, ymark )
              endif  
            enddo
          endif

        ENDIF
!           - ( ans = Y )
      ENDIF
      
      END

! ********************************************************

      SUBROUTINE CleanNodes (deltype,polylist,TotCoords,Totbndys,TotIntpts, &
                             PtsThisBnd,dxray,dyray,depth)

      IMPLICIT NONE

! PURPOSE: To update dxray(), dyray(), depth() during a successful call to
!          DelNode.
! GIVEN:   deltype = type of nodes to delete,
!                    'I' = delete interior nodes only,
!                    'B' = delete boundary nodes only,
!                    'A' = delete all nodes (interior & boundary),
!                    'Q' = user quit, delete no nodes.
! RETURNS: Updated Node data 
! EFFECTS: dxray(), dyray(), depth() are updated so that specified
!          nodes are removed. TotCoords, PtsThisBnd(), and
!          TotIntPts are updated as required.
!----------------------------------------------------------------------*

! - PASSED VARIABLES
      CHARACTER*1 deltype
      integer TotCoords,Totbndys,TotIntpts
      logical polylist(totcoords)
      integer PtsThisBnd(Totbndys)
      real dxray(TotCoords),dyray(TotCoords),depth(TotCoords)


! - LOCAL VARIABLES
      integer count, countbpt, countbpt0, NewTotBndys
      integer NewPtsThisBnd(Totbndys)
      integer i, j

!--------------------START ROUTINE--------------------------------------*

      IF ( deltype.eq.'A'.or.deltype.eq.'B') THEN  !do boundary nodes

        count = 0
        countbpt0 = 0
        NewTotBndys = 0
        NewPtsThisBnd = 0

        do i=1,TotBndys
          countbpt = 0
          do j=1,PtsThisBnd(i)
            if(.not.polylist(j+count)) then
              countbpt = countbpt + 1
              dxray(countbpt+countbpt0) = dxray(j+count)
              dyray(countbpt+countbpt0) = dyray(j+count)
              depth(countbpt+countbpt0) = depth(j+count)
            endif
          END DO
            
          if(countbpt.gt.2) then
            NewTotBndys = NewTotBndys + 1
            NewPtsThisBnd(NewTotBndys) = countbpt
            countbpt0 = countbpt0 + countbpt
          else
            continue
          endif
            
          count = count + PtsThisBnd(i)

        enddo

      ELSE  !interior only
        count = 0                  ! added 12 Feb 09
        NewTotBndys = TotBndys
        NewPtsThisBnd = PtsThisBnd
        do j=1,TotBndys
          count = count + PtsThisBnd(j)
        enddo
        countbpt0 = count
      endif

      countbpt = 0
      do j = count + 1, TotCoords  ! interior points
        if(.not.polylist(j)) then
          countbpt = countbpt + 1
          dxray(countbpt+countbpt0) = dxray(j)
          dyray(countbpt+countbpt0) = dyray(j)
          depth(countbpt+countbpt0) = depth(j)
        endif
      enddo

      TotIntPts = countbpt
      TotBndys = NewTotBndys
      PtsThisBnd = 0
      do j=1,TotBndys
        PtsThisBnd(j) = NewPtsThisBnd(j)
      enddo

      TotCoords = countbpt0 + countbpt

      RETURN
      END

!***********************************************************************

      SUBROUTINE PolyReSampleNodes ( polylist )

      USE MainArrays 

      IMPLICIT NONE

! PURPOSE: To resample all nodes in the current polygon against a
!          specified distance.
! GIVEN:   variables
! RETURNS: resampled nodes.
! EFFECTS: Specified nodes in active polygon are sampled, if confirmed.
!----------------------------------------------------------------------*

! - PASSED VARIABLES
      logical polylist(totcoords)




! - LOCAL VARIABLES
      character(80) :: cstr,answ
      character(1) :: ans
      integer :: j,jj,k,i,bndSum,indxs(2),istat
      real :: xmark,ymark,res,newbx(100000), newby(100000), newbd(100000), dist
      real :: tmpx,tmpy,tmpd
      integer :: bndstart(10000), bndend(10000),bnd(10000), numBndSeg,nnew
      logical :: segFlag

!----------------------START ROUTINE----------------------------------

!     Check for correct igridtype
      IF (.not.((igridtype.eq.0).or.(igridtype.eq.1).or.(igridtype.eq.2))) then
        call PigMessageOK('Unknown grid type. ReSample only works with UTM, LatLon and Cartesian. (0,1 and 2)','Projection')
        return
      END IF

!     Single out bnd nodes     
      polylist(TotCoords-TotIntPts+1: TotCoords) = .false.
      
!     Single out all bnd segments
      numBndSeg = 1
      bndSum = 1
      DO j = 1, Totbndys

        segFlag = .false.
        DO k = bndSum,bndSum+PtsThisBnd(j)-1
          if ( polylist(k).and..not.segFlag ) then
            segFlag = .true.
            bndstart(numBndSeg) = k
            bnd(numBndSeg) = j
          endif
          if( (( .not.polylist(k) ).or.( k.eq.(bndSum+PtsThisBnd(j))-1 )).and.segFlag ) then
            segFlag = .false.
            bndend(numBndSeg) = k - 1
            if( k.eq.(bndSum+PtsThisBnd(j))-1 ) then
              bndend(numBndSeg) = k
            end if
            numBndSeg = numBndSeg + 1
          endif
        END DO

        bndSum = bndSum + PtsThisBnd(j)

      END DO

!     Correct number of bnd segments selected
      numBndSeg = numBndSeg - 1

!     Show the bnd nodes
      DO j = 1, numBndSeg
        DO k = bndstart(j),bndend(j)
          xmark = dxray (k)
          ymark = dyray (k)
          call PigDrawModifySymbol ( xmark, ymark )
        END DO
      END DO


!     Prompt user to continue
      cstr = 'Resample these boundary segments?:'
      call PigMessageYesNo (cstr, ans)
      if (ans .ne. 'Y') then
        return
      endif


!     Prompt user for resample distance

      call PigPrompt('Enter boundary resolution : ',answ)
      read(answ,*,iostat=istat)res
      if(istat.ne.0) return




!     Resample from the back to the front      
      DO jj = numBndSeg,1,-1

!       Set bnd indexes
        indxs(1) = bndStart(jj)
        indxs(2) = bndEnd(jj)

!       Create the new bnd with equal spacing
!       Set number one
        i = 1
        newbx(1) = dxray(indxs(1))
        newby(1) = dyray(indxs(1))
        newbd(1) = depth(indxs(1))

        DO j=indxs(1),indxs(2)-1

!         Get distance between the point and the next
!         nnew is the number of 1m segments along the line.

!         Calcualte distance based on grid type
          IF (igridtype.eq.0) then ! latlon
            call haversine(dyray(j),dxray(j),dyray(j+1),dxray(j+1), dist)
          ELSE
            dist = sqrt((dxray(j+1)-dxray(j))**2 + (dyray(j+1)-dyray(j))**2)
          END IF

          nnew = floor(dist)-1

          IF( nnew.lt.2 ) nnew = 2

          DO k=1,nnew
!           Calculate position of temporary node
            tmpx = dxray(j) + k * ((dxray(j+1)-dxray(j)) / (nnew + 1))
            tmpy = dyray(j) + k * ((dyray(j+1)-dyray(j)) / (nnew + 1))
            tmpd = depth(j) + k * ((depth(j+1)-depth(j)) / (nnew + 1))

!           Get distance from the current node (i) to the new temporary node
!           Calcualte distance based on grid type
            IF (igridtype.eq.0) then !latlon
              call haversine(tmpy,tmpx,newby(i),newbx(i),dist)
            ELSE
              dist = sqrt((tmpx-newbx(i))**2 + (tmpy-newby(i))**2)
            END IF


            IF (dist >= res) then
              i = i + 1
              newbx(i) = tmpx
              newby(i) = tmpy
              newbd(i) = tmpd
              IF ( i.gt.99998 ) then
                call PigMessageOK('The boundary segment is to long/resolution to small','ReSample')
                return
              END IF
            END IF

          END DO

        END DO

!       Put the last node in the array
        i = i + 1
        newbx(i) = dxray(indxs(2))
        newby(i) = dyray(indxs(2))
        newbd(i) = depth(indxs(2))


!       Assign number of added nodes to nnew
        nnew = i - (indxs(2)-indxs(1)) - 1

!       Check if the new bnd is less than three nodes - delete if it is
        IF ((PtsThisBnd(bnd(jj)) + nnew).lt.3) THEN
          nnew = -PtsThisBnd(bnd(jj)) ! Assign to remove all nodes
        END IF



!       Move the array to make room for the new array
        IF (nnew.lt.0) THEN ! Less nodes
          DO j=indxs(2)+1,TotCoords
            dxray(j+nnew) = dxray(j)
            dyray(j+nnew) = dyray(j)
            depth(j+nnew) = depth(j)
            code(j+nnew) = code(j)
          END DO
        ELSE IF (nnew.gt.0) THEN ! More nodes
          DO j=TotCoords,indxs(2)+1,-1
            dxray(j+nnew) = dxray(j)
            dyray(j+nnew) = dyray(j)
            depth(j+nnew) = depth(j)
            code(j+nnew) = code(j)
          END DO
        END IF

!       Add the number of added nodes to the bnd segment, ITOTs and TotCoords
        PtsThisBnd(bnd(jj)) = PtsThisBnd(bnd(jj))+nnew
        TotCoords = TotCoords + nnew

!       Check if the bnd is empty, delete the marker if it is.
        IF (PtsThisBnd(bnd(jj)).eq.0) THEN
          DO j=bnd(jj),TotBndys-1
            PtsThisBnd(j)=PtsThisBnd(j+1)
          END DO
          TotBndys = TotBndys - 1
        ELSE ! Input new array
          dxray(indxs(1):indxs(1)+i-1)=newbx(1:i)
          dyray(indxs(1):indxs(1)+i-1)=newby(1:i)
          depth(indxs(1):indxs(1)+i-1)=newbd(1:i)
          code(indxs(1)+1:indxs(1)+i-1)=code(indxs(1))
        END IF


      END DO ! End of loop over bnd segs

      return
      end
!-----------------------------------------------------------------------*

      SUBROUTINE haversine(lat1,lon1,lat2,lon2,dist)

! PURPOSE: Calculate the distance from one point to another in latlon coordinates
! GIVEN:   lat and lon for 2 points
! RETURNS: dist between points
! EFFECTS: -
!----------------------------------------------------------------------*

          ! great circle distance -- adapted from Matlab
          implicit none

          real :: lat1,lon1,lat2,lon2
          real :: a,c,rdlat,rdlon,rlat1,rlat2,pi,dist
          real,parameter :: radius = 6371000


!         Generate PI
          pi = 4*atan(1.0)

!         Convert to radians
          rdlat = (lat2-lat1)*pi/180.0
          rdlon = (lon2-lon1)*pi/180.0
          rlat1 = lat1*pi/180.0
          rlat2 = lat2*pi/180.0

          a = (sin(rdlat/2))**2 + cos(rlat1)*cos(rlat2)*(sin(rdlon/2))**2
          c = 2*atan2(sqrt(a),sqrt(1-a))
          dist = radius*c

      END
     
! ---------------------------------------------------------------------*
!------------------END NODEPOLY.FOR-------------------------------------*

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

!*--------------------------------------------------------------------------*
!                            NEWEDITS.FOR                                   *
!         - CONTAINS THE ADVANCED EDITING FUNCTIONS.                        *
!*--------------------------------------------------------------------------*
!*--------------------------------------------------------------------------*

      SUBROUTINE NEARLINE(XNEW,YNEW,NX1,NX2,NX3,NX4,IND)

!       Purpose: To determine some line connecting two vertices in the
!                display window which is near point (XNEW,YNEW) in the
!                sense that the mid-point of the connecting line lies 
!                within a box of side length 2*n*RANGE centred at the 
!                point (XNEW,YNEW). Note that the subroutine returns
!                the first such suitable line. Failure to detect the 
!                correct line indicates that RANGE should be reduced;
!                alternatively, recompile with a smaller factor n

!       Given:   Coordinates of point - XNEW,YNEW
!                Number of records in grid file - NREC
!       Returns: NX1 - Index of vertex at one end of grid line
!                      near enough to (XNEW,YNEW)
!                NX2 - Index of vertex at other end of same line
!                NX3 - First vertex forming triangle with NX1, NX2
!                NX4 - Second vertex forming triangle with NX1, NX2
!                       (there may be no second such vertex)
!                IND = 1  if only one triangle includes NX1 and NX2
!                    = 2  if two triangles include NX1 and NX2
!                    = 0  if no line found by cursor (ERROR)
!                    = 3  if <1 or >2 triangles include NX1 and NX2
!                                                             (ERROR)

      use MainArrays

! *** PASSED VARIABLES ***
      integer NX1, NX2, NX3, NX4, IND
      REAL XNEW,YNEW

! *** LOCAL VARIABLES ***
      integer I, J, K, NUMTRIANGS
      LOGICAL IN_BOX, FOUNDLINE
      REAL XMID, YMID, HXLIM, LXLIM, HYLIM, LYLIM, ERROR
      real diffdist, diffnew

      NX3 = 0
      NX4 = 0
      IND = 0
      diffdist = 1.E30
      FOUNDLINE = .false.

! *** FIND LINE NEAR (XNEW,YNEW) ***************************
      DO I = 1, itot
!        IF(EXIST(I)) THEN
        IF(code(I).ge.0) THEN
!          IS VERTEX IN DISPLAY WINDOW?
          IF(IN_BOX(DXRAY(I),DYRAY(I))) THEN
!               CHECK ONLY NEIGHBOURS WITH HIGHER INDEX NUMBER
            DO J = 1, NBTOTR
              IF(NL(J,I).GT.I) THEN
                XMID=(DXRAY(I)+DXRAY(NL(J,I)))/2.
                YMID=(DYRAY(I)+DYRAY(NL(J,I)))/2.
                ERROR = 0.1*(ABS(DXRAY(I)-DXRAY(NL(J,I)))+ABS(DYRAY(I)-DYRAY(NL(J,I))))
                HXLIM = XMID + ERROR
                LXLIM = XMID - ERROR
                IF((XNEW.LE.HXLIM).AND.(XNEW.GE.LXLIM)) THEN
                  HYLIM = YMID + ERROR
                  LYLIM = YMID - ERROR
                  IF((YNEW.LE.HYLIM).AND.(YNEW.GE.LYLIM)) THEN
                    diffnew = (xnew-xmid)**2 + (ynew-ymid)**2
                    if(diffnew.lt.diffdist) then
                      NX1 = I
                      NX2 = NL(J,I)
                      FOUNDLINE = .true.
                    ENDIF
                  ENDIF
                ENDIF
              endif
            enddo
          ENDIF
        ENDIF
      enddo

      IF(.not.FOUNDLINE) THEN
        IND = 0
        return
      ENDIF

! ** FIND TRIANGLES TO WHICH BOTH NX1 AND NX2 BELONG
      NUMTRIANGS = 0
!     SEARCH NEIGHBOURS OF NX1, EXCEPT NX2 
      DO J = 1, NBTOTR
        IF(NL(J,NX1).NE.NX2.and.NL(J,NX1).NE.0) THEN
!          CHECK IF THIS NEIGHBOUR IS ALSO A NEIGHBOUR OF NX2
          DO K = 1, NBTOTR
            IF(NL(K,NX2).EQ.NL(J,NX1)) THEN
              NUMTRIANGS = NUMTRIANGS + 1
              IF(NUMTRIANGS.EQ.1) THEN
                NX3 = NL(J,NX1)
                IND = 1
              ELSEIF(NUMTRIANGS.EQ.2) THEN
                NX4 = NL(J,NX1)
                IND = 2
              ENDIF
            ENDIF
          enddo
        ENDIF
      enddo

      IF(NUMTRIANGS.EQ.0.or.NUMTRIANGS.GT.2) THEN
!        ERROR CONDITION - NO TRIANGLES FOUND or TOO MANY
        IND = 3
      ENDIF

      RETURN
      END

!*--------------------------------------------------------------------------*

      SUBROUTINE Insert( nrec, xnew, ynew, ierr )

! Purpose : To add an extra vertex plus appropriate connections
!           in the following two situations:
!           a) where one diagonal connection exists among four
!              vertices and a second one is required (this implies
!              creation of a new vertex at the crossing point)
!           b) where a new line is required from an interior point
!              to a neighbouring boundary (this implies creation
!              of a new vertex at the target point on the boundary).
! Returns : change - TRUE if changes made require new triangle list.
!           ind -  see below
!*--------------------------------------------------------------------
! Notes:
!     Variables passed to routine NearLine
!       xnew,ynew : Coordinates of point
!     Variables returned by routine NearLine
!       nx1 - Index of vertex at one end of grid line
!             near enough to (xnew,ynew)
!       nx2 - Index of vertex at other end of same line
!       nx3 - First vertex forming triangle with nx1, nx2
!       nx4 - Second vertex forming triangle with nx1, nx2
!             (there may be no second such vertex)
!       ind = 1  if only one triangle includes nx1 and nx2
!           = 2  if two triangles include nx1 and nx2
!           = 0  if no line found by cursor (error)
!           = 3  if 0 or >2 triangles include nx1 AND nx2 (error)
!*--------------------------------------------------------------------

      use MainArrays

!     - PASSED PARAMETERS
      LOGICAL change
      integer nrec, ierr

!     - INCLUDES
      INCLUDE '../includes/graf.def'

!      integer delete(mrec)
!      integer count
!      COMMON /NOTIFY0/ delete, count

!     - LOCAL VARIABLES
      integer nx1, nx2, nx3, nx4, ind, icnt, n1, i, j
      integer ians, nnx, nx(4), idrr, savcode, countdep
      REAL xnew, ynew, pxray(5), pyray(5), savdep, depsum
      REAL xa, xb, xc, ya, yb, yc
      CHARACTER*80 ans
      character cstr*80, PigCursYesNo*1
      LOGICAL inx(4), Success, possible, space1, space2, space3, space4
      LOGICAL LISTSPACE

!*------------ BEGIN -----------
      ierr = 0

!     - find required location of new point (mid-pt. of line)
!      call NewPt( xnew, ynew, 1, ierr )
      IF ( ierr .eq. 999 ) return

!     - find triangle(s) associated with new point
      call NearLine( xnew, ynew, nx1, nx2, nx3, nx4, ind )
!     - check no error condition
      IF ((ind.eq.1).or.(ind.eq.2)) then
!         this line is appropriate, and satisfies either case a) or b) above
      ELSE
!         some error condition
      IF ( ind .eq. 0 ) THEN
!           - no line found by cursor
          ierr = 1
          call PigPutMessage('No line located.')
      ELSEIF( ind .eq. 3 ) THEN
!           - 2 vertices in 0 or more than 2 triangles
          ierr = 2
          call PigPutMessage('More than 2 triangles detected. No insertion possible.')
      else
!           - some other error
          call PigPutMessage('Error in grid detected. No insertion possible.')
          ierr = ind
      endif
      return
      endif
!     - ok: 2 vertices in 1 or 2 triangles; insertion may be possible
      ierr = 0
!     Must add check here that existing nodes which are neighbours of
!     new node do not already have full neighbour lists. If so, abort.
!     check nx1,nx2,nx3 have empty space in their neighbour lists
      space1 = listspace(nx1)
      space2 = listspace(nx2)
      space3 = listspace(nx3)
      space4 = .true.
      IF (ind.eq.2) THEN
!         check nx4 has empty space in its neighbour list
      space4 = listspace(nx4)
      ENDIF
!     now check all nodes connected to new node have space in their lists
      possible = space1.and.space2.and.space3.and.space4
      IF(.not.possible) THEN
!        put up 'not possible' message and abort
     call PigPutMessage('Insert creates too many neighbours.')
     ierr = 4
       return
      ENDIF
!     - erase existing line betweeen vertices nx1 and nx2
      pxray(1) = dxray(nx1)
      pyray(1) = dyray(nx1)
      pxray(2) = dxray(nx2)
      pyray(2) = dyray(nx2)
      call ReDraw( pxray, pyray, 2, 0 )

!     - now draw in new connections
      pxray(1) = xnew
      pyray(1) = ynew
      pxray(2) = dxray(nx1)
      pyray(2) = dyray(nx1)
      pxray(3) = dxray(nx2)
      pyray(3) = dyray(nx2)
      pxray(4) = dxray(nx3)
      pyray(4) = dyray(nx3)
      icnt = 4
      IF ( ind .eq. 2 ) THEN
      pxray(5) = dxray(nx4)
      pyray(5) = dyray(nx4)
      icnt = 5
      ENDIF
      call ReDraw( pxray, pyray, icnt, 1 )
      IF ( IND .EQ. 1 ) then
!       - check if new point lies outside old boundary
!       -- using vector cross-product test:
!        ( xb*ya - yb*xa ) * ( xb*yc - yb*xc ) < 0
      xa = dxray(nx3) - dxray(nx1)
      ya = dyray(nx3) - dyray(nx1)
      xb = dxray(nx2) - dxray(nx1)
      yb = dyray(nx2) - dyray(nx1)
      xc = xnew - dxray(nx1)
      yc = ynew - dyray(nx1)
!       - place yellow marker at new point if outside former boundary
      IF ( (xb * ya - yb * xa) * (xb * yc - yb * xc) .lt. 0 ) THEN
        call PutMarker( xnew, ynew, 4, yellow )
      ENDIF           
      ENDIF

!     - ask confirmation
      cstr='Is this change OK ?:'
      ans = PigCursYesNo ( cstr)
      IF ( ans(1:1) .eq. 'Y' ) THEN
      change = .TRUE.
!       - increase # of valid vertices by 1
      itot = itot + 1
      n1 = nrec
      nrec = nrec + 1
      !exist(n1) = .TRUE.
!       - set depth at new vertex
60      CONTINUE
        cstr = 'Set depth automatically ?:'
        ans = 'Y'  !PigCursYesNo ( cstr )
      IF ( ans(1:1) .eq. 'Y' ) THEN
        IF ( ind .eq. 1 ) THEN
          depth(n1) = (depth(nx1) + depth(nx2))/2.
        ELSEIF ( ind .eq. 2 ) THEN
!           - find nature of 4 vertices surrounding N1
          nnx = 0
          nx(1) = nx1
          nx(2) = nx2
          nx(3) = nx3
          nx(4) = nx4
          DO i = 1, 4
            inx(i) = .FALSE.
            IF ( code(nx(i)) .ne. 0 ) THEN
            inx(i) = .TRUE.
            nnx = nnx + 1
            ENDIF
          ENDDO
          IF ( nnx .le. 1 ) GO TO 61
          IF ( nnx .eq. 2 ) THEN
!             - opposite points on boundary?
            IF ( inx(1) .AND. inx(2) ) THEN
            depth(n1) = ( depth(nx3) + depth(nx4) ) / 2.0
            GO TO 62
            ELSEIF ( inx(3).AND.inx(4) ) THEN
            depth(n1) =( depth(nx1) + depth(nx2) ) / 2.0
            GO TO 62
!               - adjacent points on boundary
            ELSE
            GO TO 61
            ENDIF
          ELSEIF ( nnx .eq. 3 ) THEN
            countdep = 0
            depsum = 0.
            DO i = 1, 4
            IF ( depth(nx(i)) .ne. 0 ) THEN
              depsum = depsum + depth(nx(i))
              countdep = countdep + 1
            ENDIF
            ENDDO
            IF ( countdep .eq. 0 ) countdep = 1
            depth(n1) = depsum/countdep
            ENDIF
61            CONTINUE
!             - normal depth interpolation
            savdep = depth(nx1)
            savcode = code(nx1)
            code(nx1) = 0
            call MovDep( nx1, xnew, ynew, idrr)
            call PigPutMessage('New depth set by interpolation.' )
            depth(n1) = depth(nx1)
            depth(nx1) = savdep
            code(nx1) = savcode
62           CONTINUE
         ENDIF
       ELSEIF ( ans(1:1) .eq. 'N' ) THEN
         Success = .FALSE.
         DO WHILE ( .NOT. Success )
           call PigPrompt('Enter depth for the new point:',ans )
           call PigReadReal( ans, DEPTH(N1), Success )
         ENDDO
       ELSE
         GO TO 60
       ENDIF
!        - nx1 , nx2  no longer neighbours
        DO j = 1, nbtotr
          IF ( NL(j,nx1) .eq. nx2 ) THEN
            NL(j,nx1) = 0
          ENDIF
          IF ( NL(j,nx2) .eq. nx1 ) THEN
            NL(j,nx2) = 0
          ENDIF
        ENDDO
!         - add new vertex to appropriate neighbour lists
        call NBinsert( nx1, n1, ierr )
        call NBinsert( nx2, n1, ierr )
        call NBinsert( nx3, n1, ierr )
        IF ( ind .eq. 2 ) THEN
          call NBinsert( nx4, n1, ierr )
        ENDIF        
!          if ( ierr .ne. 0 ) - no error test carried out here because
!          check was carried out above that neighbouring nodes have
!          empty space(s) available in their neighbour lists

!         - neighbour list for new vertex
        NL(1,n1) = nx1
        NL(2,n1) = nx2
        NL(3,n1) = nx3
        IF ( ind .eq. 2 ) THEN
          NL(4,n1) = nx4
        ENDIF
!         - fill in data for new vertex
        dxray(n1) = xnew
        dyray(n1) = ynew
        IF ( ind .eq. 2 ) THEN
!           - new vertex is in interior
          code(n1) = 0
        ELSE
!           - new vertex is on boundary
          IF ( code(nx1) .eq. code(nx2) ) THEN
!             - both neighbours have same code
            code(n1) = code(nx1)
          ELSE
!             - ask user for CODE for new vertex
            ians = 0
            DO WHILE ( ians .lt. 49 )
            call PigPrompt('Enter Non-Zero code for boundary point:', ans )
            ians = ichar(ans(1:1))
            ENDDO
            code(n1) = ians - 48
          ENDIF
        ENDIF
      ELSE
!       - not satisfactory
      call PigPutMessage( 'No update..' )
!       - erase the new
      call ReDraw( pxray, pyray, icnt, 0 )
!       - and replace the old
      pxray(1) = dxray(nx1)
      pyray(1) = dyray(nx1)
      pxray(2) = dxray(nx2)
      pyray(2) = dyray(nx2)
      call ReDraw( pxray, pyray, 2, 1 )
      ENDIF
      END

!*--------------------------------------------------------------------------*

      LOGICAL FUNCTION LISTSPACE (INODE)

!     Purpose:
!     LISTSPACE = .true. if node INODE has empty space(s) in its
!                 neighbour list, i.e one of places 1 to NBTOT is empty
!               = .false. otherwise

      use MainArrays

! *** passed variables
      INTEGER INODE
!       INODE is index of node examined

! *** local variables
      INTEGER j
      LISTSPACE = .false.
      do j = nbtot,1,-1
      if(NL(j,INODE).eq.0) then
         LISTSPACE = .true.
         return
      endif
      enddo
      end
!*--------------------------------------------------------------------------*

      SUBROUTINE NBINSERT(NNROW,NPUTIN,IERR)

!     Purpose : To insert neighbour index NPUTIN into first empty
!               space in neighbour list of vertex NNROW
!     Returns : IERR = 0 if space found
!                   = 1 if no space available
!     MODIFIED: STEVE PRESTAGE AND DAPHNE CONNOLLY  MAY 1989
!     Modified 5 July 95 by RFH to update maximum number of neighbours
!     (of existing nodes) when a node is inserted. Also to give error 
!     warning if all NBTOT neighbour places are already full.

      use MainArrays

! *** PASSED VARIABLES ***
      integer NNROW, NPUTIN, IERR
!       NNROW - vertex whose nbr list is being added to 
!       NPUTIN - index of neighbour being added
!       IERR - error indicator

! *** LOCAL VARIABLES ***
      integer I
      logical placefound

!       I - counter

      IERR = 1

      I = 1
      placefound = .false.

       do while (I.le.NBTOT.and..not.placefound)
      IF(NL(I,NNROW).eq.0)THEN
        NL(I,NNROW) = NPUTIN
        IERR = 0
        placefound = .true.
        if (I.gt.NBTOTR) NBTOTR = I
      ELSE
        I = I + 1
      ENDIF
       enddo

      RETURN
      END

!*--------------------------------------------------------------------------*

      SUBROUTINE XCHANGE (xnew, ynew,IERR)

!     Purpose : To erase a diagonal connection in a quadrilateral
!               and insert connection across other diagonal
!     Returns : CHANGE - true if changes have been made
!                        which require new triangle list
!               IND -  see below 

      use MainArrays

! *** PASSED VARIABLES ***
      LOGICAL  CHANGE
      integer  IERR

      INCLUDE '../includes/graf.def'

!      integer DELETE(mrec)
!      integer COUNT
!      COMMON /NOTIFY0/DELETE,COUNT

! *** Variables passed to S/R NEARLINE    
!                Coordinates of point XNEW,YNEW
! *** Variables returned by S/R NEARLINE
!                NX1 - Index of vertex at one end of grid line
!                      near enough to (XNEW,YNEW)
!                NX2 - Index of vertex at other end of same line
!                NX3 - First vertex forming triangle with NX1, NX2
!                NX4 - Second vertex forming triangle with NX1, NX2
!                       (there may be no second such vertex)
!                IND = 1  if only one triangle includes NX1 and NX2
!                    = 2  if two triangles include NX1 and NX2
!                    = 0  if no line found by cursor (ERROR)
!                    = 3  if 0 or >2 triangles include NX1 and NX2
!                                                             (ERROR)
! *** Local variables ***
      integer NX1, NX2, NX3, NX4, IND, J
      REAL XNEW, YNEW, PXRAY(5), PYRAY(5)
      CHARACTER*80 ANS
      character cstr*80, PigCursYesNo*1

!     FIND TRIANGLE(S) ASSOCIATED WITH NEW POINT
      call NEARLINE(XNEW,YNEW,NX1,NX2,NX3,NX4,IND)

!     CHECK NO ERROR CONDITION
      IF(IND.EQ.2) THEN
!       O.K. 2 triangles
      IERR = 0
      ELSE
!         some error condition
        IF ( ind .eq. 0 ) THEN
!           - no line found by cursor
            ierr = 1
            call PigPutMessage('No line located.')
        ELSEIF( ind .eq. 1 ) THEN
!           - 2 vertices in only 1 triangle
            ierr = 2
            call PigPutMessage('Only one triangle detected. No exchange possible.')
        ELSEIF( ind .ge. 3 ) THEN
!           - 2 vertices in more than 2 triangles
            ierr = 2
            call PigPutMessage('More than 2 triangles detected. No exchange possible.')
        else
!           - some other error
            call PigPutMessage('Error in grid detected. No exchange possible.')
            ierr = ind
        endif
        return
      endif
!      ** ERASE EXISTING LINE BETWEEEN VERTICES NX1 AND NX2
      PXRAY(1) = DXRAY(NX1)
      PYRAY(1) = DYRAY(NX1)
      PXRAY(2) = DXRAY(NX2)
      PYRAY(2) = DYRAY(NX2)
      call ReDraw( pxray, pyray, 2, 0 )
!      ** NOW DRAW IN NEW LINE BETWEEN NX3 AND NX4 **
      PXRAY(1) = DXRAY(NX3)
      PYRAY(1) = DYRAY(NX3)
      PXRAY(2) = DXRAY(NX4)
      PYRAY(2) = DYRAY(NX4)
      call ReDraw( pxray, pyray, 2, 1 )
!        ** ASK CONFIRMATION **
      ans = 'x'
      cstr = 'Is this change OK ?:'
       ans = PigCursYesNo( cstr )
      if (ANS(1:1) .eq. 'Y') then
          CHANGE = .true.
!   ** NX1 , NX2  no longer neighbours
        DO 400 J = 1, NBTOTR 
          IF(NL(J,NX1).EQ.NX2) NL(J,NX1) = 0
          IF(NL(J,NX2).EQ.NX1) NL(J,NX2) = 0
400       CONTINUE
!   ** ADD NX3,NX4 TO ONE ANOTHER'S NEIGHBOUR LISTS
        call NBINSERT(NX3,NX4,IERR)
        call NBINSERT(NX4,NX3,IERR)
!         if(IERR.ne.0) no error test included yet
      else
!           if not satisfactory -
          Call PigPutMessage('No update..')
!           erase the new
          call ReDraw( pxray, pyray, 2, 0 )
!           and replace the old
          PXRAY(1) = DXRAY(NX1)
          PYRAY(1) = DYRAY(NX1)
          PXRAY(2) = DXRAY(NX2)
          PYRAY(2) = DYRAY(NX2)
          call ReDraw( pxray, pyray, 2, 1 )
      endif
      END

!*--------------------------------------------------------------------------*

      SUBROUTINE Cleave( nrec, xold,yold, ierr )
 
! Purpose : To insert an extra vertex into a grid by deleting
!           a designated vertex and replacing it by two new
!           vertices.
! Returns : change - logical, flag tells changes have been made
!                    which require new triangle list
!           ierr = 0 - if successful cleave performed.
!                = 1 - can't find node indicated by mouse
!                = 2 - boundary node, or fixed node (code = 90)
!                           not to be cleaved
!                = 3 - less than 5 neighbours, not to be cleaved

      use MainArrays

!     - PASSED PARAMETERS
      LOGICAL change
      integer nrec, ierr

!     - INCLUDES
      INCLUDE '../includes/graf.def'

      integer  ndx
      COMMON  /PICK/ ndx

!      integer delete(mrec)
!      integer count
!      COMMON /NOTIFY0/ delete,count

      integer n1, n2, icnt1, icnt2, nb1(nbtot), nb2(nbtot)
      REAL px1(nbtot+1), px2(nbtot+1), py1(nbtot+1), py2(nbtot+1)
      COMMON /CLEEV/ n1, n2, icnt1, icnt2, nb1, nb2, px1, px2, py1, py2
!     - n1, n2 - indices of the two new vertices
!     - icnt1, icmt2 - number of neighbours + 1 for new vertices
!     - nb1, nb2 - lists of indices of 2 new vertices
!     - px1, py1 - x,y coordinates of first new vertex and neighbours
!     - px2, py2 - x,y coordinates of second new vertex and neighbours

!     - LOCAL VARIABLES
      REAL pxray(nbtot+1), pyray(nbtot+1)
      integer icnt, ier, ier2, idrr, i, mvndx
!      REAL PXRAY(5), PYRAY(5)
!              - holds x y coords for the original point neighbors
      CHARACTER*80  ans
      character cstr*80, PigCursYesNo*1 
!              - original number of neighbors + 1
      REAL xold, yold
!              - X,Y coordinates of vertex being cleaved
      REAL olddep
!              - original depth at cleft vertex
      logical delupok
!*------- BEGIN --------
   
      ierr = 0
      ier = 0
!     - get the point for cleave operation
!      call NewPt( xold, yold, 3, ier )
      IF ( ier .eq. 0 ) THEN
!       - input detected, check if existing vertex
        call ChkPt( xold, yold, ndx, ier2 )
        IF ( ier2 .eq. 1 ) THEN
!         - vertex does not exist
          call PigMessageOK('ERROR - Invalid point.',' ' )
!         call PigUWait( 2.0 )
!         call PigEraseMessage
          ierr = 1
        ELSEIF ( ier2 .eq. 0 ) THEN
!         - vertex exists, check if boundary point
        IF ( code(ndx) .ne. 0 ) THEN
!           - flag as boundary or fixed node, not to be cleaved
          ierr = 2
        ENDIF
        IF ( ierr .eq. 2 ) THEN
!           - boundary point
          call PigPutMessage('ERROR - Boundary or fixed node, cleave not allowed')
        ELSE
!           - valid vertex, put marker at location ChkPt returned
          pxray(1) = xold
          pyray(1) = yold
          icnt = 1
!           - get vertex's neighbors (the X,Y locations)
          call RelPt2( pxray, pyray, ndx, icnt, ierr)
!           - Abort if point to cleave has less than five neighbours
          IF ( icnt .lt. 6 ) THEN
            call PigPutMessage('ERROR - Less than 5 neighbours, cleave not allowed')
            ierr = 3
          ELSE
!             - still valid vertex, with enough neighbours
            n1 = nrec
            n2 = nrec + 1
!             - subroutine ReBuild works out position of 2 new vertices and
!             -- lists of neighbours to which each is connected and
!             -- respective coords
            call ReBuild( ndx )
!             - erase the old point and lines to neighbors
            call ReDraw( pxray, pyray, icnt, 0 )
!             - now draw in new vertices' connections
            call ReDraw( px1, py1, icnt1, 1 )
            call ReDraw( px2, py2, icnt2, 1 )
!             - ask confirmation 
            cstr = 'Is this change OK ?:'
            ans = PigCursYesNo( cstr)
            IF ( ans(1:1) .eq. 'Y' ) THEN
!               - satisfactory change
            change = .TRUE.
!               - increase no. of valid vertices by 2
            !exist(n1) = .TRUE.
            !exist(n2) = .TRUE.
!               - routine MovDep can be used to find depths at new vertices 
!               -- provided grid file has not yet been updated. MOVDEP puts 
!               -- new value of depth in depth(ndx)
            olddep = depth(ndx)
            call MovDep( ndx, px1(1), py1(1), idrr)
            depth(n1) = depth(ndx)
            dxray(n1) = px1(1)
            dyray(n1) = py1(1)
            code(n1)  = 0
            DO i = 1, icnt1-1
              NL(i,n1) = nb1(i)     
            ENDDO
            depth(ndx) = olddep
            mvndx = ndx
            call MovDep( mvndx, px2(1), py2(1), idrr)
!                call PigPutMessage(
!     +                 'New depths set by interpolation.' )
            depth(n2) = depth(ndx)
            dxray(n2) = px2(1)
            dyray(n2) = py2(1)
            code(n2)  = 0
            DO i = 1, icnt2-1
              NL(i,n2) = nb2(i) 
            ENDDO
!               - mark old vertex for deletion and update its neighbours' lists
            mvndx = ndx
            call DelUp(mvndx,DelUpOK)
!               - add new vertices selectively to nbr lists of old 
!               -- vertex's nbrs
            DO i = 1, icnt1-2
              call NBinsert( NL(i,n1), n1, ierr )
!                 - if ( ierr.ne.0 ) error test not yet implemented 
            ENDDO
            DO i = 1, icnt2-2
              call NBinsert( NL(i,n2), n2, ierr )
!                 if ( ierr .ne. 0 ) error test not yet implemented 
            ENDDO
            nrec = nrec + 2
            itot = itot + 2
            ELSE
!               - ans = 'N', not satisfactory change
            call PigPutMessage('No update..')
!               - erase the new
            call ReDraw( px1, py1, icnt1, 0 )
            call ReDraw( px2, py2, icnt2, 0 )
!               - and replace the old
            call ReDraw( pxray, pyray, icnt, 1 )
            ENDIF
!               - ( satisfactory change )
          ENDIF
!             - ( enough neighbours )
        ENDIF
!           - ( boundary point )
      ENDIF
!         - ( vertex not exist )
      ENDIF
!       - ( input detected )
      END

!*--------------------------------------------------------------------------*
      SUBROUTINE REBUILD(NDX)

!     Purpose:  To return indices, nos. of neighbours, indices of
!               neighbours, coordinates of neighbours, of the two
!               new vertices replacing index NDX in cleave operation.
!     MODIFIED: STEVE PRESTAGE AND DAPHNE CONNOLLY  MAY 1989

      use MainArrays

! *** PASSED VARIABLES ***
!      integer NREC, NDX
      integer NDX
!       NREC - total number of vertices currently in vertex list
!       NDX  - index of vertex to be replaced

      integer N1, N2, ICNT1, ICNT2, NB1(NBTOT), NB2(NBTOT)
      REAL PX1(NBTOT+1), PX2(NBTOT+1), PY1(NBTOT+1), PY2(NBTOT+1)
      COMMON /CLEEV/ N1, N2, ICNT1, ICNT2, NB1, NB2, PX1, PX2, PY1, PY2
!       N1, N2 - indices of two new vertices
!       ICNT1  - (no. of nbrs of N1) + 1
!       ICNT2  - (no. of nbrs of N2) + 1
!       NB1(1) - index of first neighbour common to N1 and N2
!       NB2(1) - index of first neighbour common to N1 and N2
!       NB1(2) - index of second neighbour common to N1 and N2
!       NB2(2) - index of second neighbour common to N1 and N2
!       NB1(3) to NB1(ICNT1-2) ordinary neighbours of N1
!       NB2(3) to NB2(ICNT2-2) ordinary neighbours of N2
!       NL(ICNT1-1) = N2
!       NL(ICNT2-1) = N1
!       PX1(1),PY1(1) - coords of N1
!       PX2(1),PY2(1) - coords of N2
!       PX1(2),...PX1(ICNT1-1) - X-coords of NB1(1),...NB1(ICNT1-1)
!       PY1(2),...PY1(ICNT1-1) - Y-coords of NB1(1),...NB1(ICNT1-1)
!       PX2(2),...PX2(ICNT1-1) - X-coords of NB2(1),...NB2(ICNT1-1)
!       PY2(2),...PY2(ICNT1-1) - Y-coords of NB2(1),...NB2(ICNT1-1)

! *** ARGUMENTS IN SUBORDINATE SUBROUTINES ***
      integer NUMNBRS, NBARRAY(NBTOT)
!       NUMNBRS - no. of neighbours of NDX (old vertex)
!       NBARRAY - neighbours of NDX sorted counterclockwise 

! *** LOCAL VARIABLES ***
      LOGICAL EVEN
      integer NT1, NT2, I, NBY2, NB1PLACE, NB2PLACE, IPLACE
      REAL DIST, DMIN, XSUM, YSUM

!    SORT NEIGHBOURS OF OLD VERTEX (NDX) INTO CCW ORDER FROM EAST
      call CCWSORT(NDX,NUMNBRS,NBARRAY)

!    CHECK IF NUMBER OF NEIGHBOURS IS EVEN OR ODD
      EVEN = .TRUE.
      NBY2 = NUMNBRS/2
      IF(NBY2*2.NE.NUMNBRS) EVEN = .FALSE.

!    FIND OPPOSED PAIR OF NEIGHBOURS MINIMUM DISTANCE APART
      DMIN = 1.E20
      DO 100 I = 1, NBY2
      NT1 = NBARRAY(I)
      NT2 = NBARRAY(I + NBY2)
      DIST = (DXRAY(NT1)-DXRAY(NT2))**2 + (DYRAY(NT1)-DYRAY(NT2))**2
      IF(DIST.LE.DMIN) THEN
        DMIN = DIST
        NB1(1) = NT1
        NB1(2) = NT2
        NB1PLACE = I
        NB2PLACE = I + NBY2
      ENDIF
      IF(.not.EVEN) THEN
        NT2 = NBARRAY(I + NBY2 + 1)
        DIST = (DXRAY(NT1)-DXRAY(NT2))**2 + (DYRAY(NT1)-DYRAY(NT2))**2
        IF(DIST.LE.DMIN) THEN
          DMIN = DIST
          NB1(1) = NT1
          NB1(2) = NT2
! following line added 14 Sept 90
          NB1PLACE = I
          NB2PLACE = I + NBY2 + 1
        ENDIF
      ENDIF
100   CONTINUE

!    NOTE : THE TWO NEW VERTICES REPLACING THE OLD VERTEX (NDX)
!           BOTH HAVE THE CLOSEST PAIR OF OPPOSED VERTICES I.E.
!           FINAL VALUES OF NT1, NT2 ABOVE, AS NEIGHBOURS.
!            I.E. NB2(1) = NB1(1) = best NT1  
!                 NB2(2) = NB1(2) = best NT2 
      NB2(1) = NB1(1)
      NB2(2) = NB1(2)
      PX1(2) = DXRAY(NB1(1))    
      PY1(2) = DYRAY(NB1(1))    
      PX1(3) = DXRAY(NB1(2))    
      PY1(3) = DYRAY(NB1(2))    
      PX2(2) = DXRAY(NB2(1))    
      PY2(2) = DYRAY(NB2(1))    
      PX2(3) = DXRAY(NB2(2))    
      PY2(3) = DYRAY(NB2(2))    

!   ALLOT NEIGHBOURS TO NEW VERTICES

!       FIRST NEW VERTEX'S NEIGHBOURS AND THEIR COORDINATES
!        ( FIRST AND SECOND NEIGHBOURS ARE ALREADY ALLOTTED)

      ICNT1 = 3
      DO 101 I = NB1PLACE + 1, NB1PLACE + 1 + NBY2
      IPLACE = I
      IF(IPLACE.GT.NUMNBRS) IPLACE = IPLACE - NUMNBRS
      IF(NBARRAY(IPLACE).EQ.NB1(2)) GO TO 102
      ICNT1 = ICNT1 + 1
      NB1(ICNT1-1) = NBARRAY(IPLACE)
      PX1(ICNT1) = DXRAY(NB1(ICNT1-1))
      PY1(ICNT1) = DYRAY(NB1(ICNT1-1))
101   CONTINUE
102   CONTINUE

!       SECOND NEW VERTEX'S NEIGHBOURS AND THEIR COORDINATES
      ICNT2 = 3
      DO 103 I = NB2PLACE + 1, NB2PLACE + 1 + NBY2
      IPLACE = I
      IF(IPLACE.GT.NUMNBRS) IPLACE = IPLACE - NUMNBRS
      IF(NBARRAY(IPLACE).EQ.NB1(1)) GO TO 104
      ICNT2 = ICNT2 + 1
      NB2(ICNT2-1) = NBARRAY(IPLACE)
      PX2(ICNT2) = DXRAY(NB2(ICNT2-1))
      PY2(ICNT2) = DYRAY(NB2(ICNT2-1))
103   CONTINUE
104   CONTINUE

!    FIND TWO NEW VERTEX POSITIONS
!       PLACE FIRST NEW VERTEX AT MEAN POSITION OF OLD NBRS ALLOTTED
!       (INCLUDE ORIGINAL VERTEX IF IT HAD MORE THAN 4 NEIGHBOURS)
      XSUM = 0.
      YSUM = 0.
      if((ICNT1+ICNT2).gt.8)then
!        in cases where original point has more than 4 neighbours
       DO 105 I = 2, ICNT1
          XSUM = XSUM + PX1(I)
          YSUM = YSUM + PY1(I)
105      CONTINUE
       XSUM = XSUM + DXRAY(NDX)
       YSUM = YSUM + DYRAY(NDX)
       PX1(1) = XSUM/(ICNT1)
       PY1(1) = YSUM/(ICNT1)
      else
!        in case where original point has 4 neighbours
       DO 108 I = 2, ICNT1
          XSUM = XSUM + PX1(I)
          YSUM = YSUM + PY1(I)
108      CONTINUE
       PX1(1) = XSUM/(ICNT1-1)
       PY1(1) = YSUM/(ICNT1-1)
      endif

!       PLACE SECOND NEW VERTEX AT MEAN POSITION OF OLD NBRS ALLOTTED
!       (INCLUDE ORIGINAL VERTEX IF IT HAD MORE THAN 4 NEIGHBOURS)
      XSUM = 0.
      YSUM = 0.
      if((ICNT1+ICNT2).gt.8)then
!        in cases where original point has more than 4 neighbours
       DO 106 I = 2, ICNT2
         XSUM = XSUM + PX2(I)
         YSUM = YSUM + PY2(I)
106      CONTINUE
       XSUM = XSUM + DXRAY(NDX)
       YSUM = YSUM + DYRAY(NDX)
       PX2(1) = XSUM/(ICNT2)
       PY2(1) = YSUM/(ICNT2)
      else
!        in case where original point has 4 neighbours
       DO 107 I = 2, ICNT2
         XSUM = XSUM + PX2(I)
         YSUM = YSUM + PY2(I)
107      CONTINUE
       PX2(1) = XSUM/(ICNT2-1)
       PY2(1) = YSUM/(ICNT2-1)
      endif

!    COMPLETE NEIGHBOUR LISTS, COUNTS AND POSITIONS
      ICNT1 = ICNT1 + 1
      ICNT2 = ICNT2 + 1
      PX1(ICNT1) = PX2(1)
      PY1(ICNT1) = PY2(1)
      PX2(ICNT2) = PX1(1)
      PY2(ICNT2) = PY1(1)
      NB1(ICNT1-1) = N2
      NB2(ICNT2-1) = N1
      RETURN
      END


!*--------------------------------------------------------------------------*

      SUBROUTINE CCWSORT(NDX,NUMNBRS,NBARRAY)

!     Purpose : To sort neighbours of vertex NDX into c/clockwise order
!     Returns : NUMNBRS - number of neighbours of NDX
!               NBARRAY - indices of neighbours of NDX in ccw order
!     Comment : Sorting by angle is relatively inefficient and should
!               be replaced by methods buried in S/R Get2Nb
!     MODIFIED: STEVE PRESTAGE AND DAPHNE CONNOLLY  MAY 1989

      use MainArrays

! *** PASSED VARIABLES ***
      integer  NDX, NUMNBRS, NBARRAY(NBTOT)

! *** LOCAL VARIABLES ***
      integer I, J, NCT, JMIN, NBTEMP(NBTOT)
      REAL PigANGLE, XANGLE(NBTOT), X, Y, ANGMIN

!  Count and stack neighbours and evaluate angular positions
      NUMNBRS = 0   
      DO 100 J = 1, NBTOT
      IF(NL(J,NDX).GT.0) THEN
         NUMNBRS = NUMNBRS + 1
         NBTEMP(NUMNBRS) = NL(J,NDX)
         X = DXRAY(NL(J,NDX)) - DXRAY(NDX)
         Y = DYRAY(NL(J,NDX)) - DYRAY(NDX)
         XANGLE(NUMNBRS) = PigANGLE(X,Y)
      ENDIF
100   CONTINUE

!  Sort neighbours into ccw order and store in NBARRAY
      NCT = 0
      DO 101 I = 1, NUMNBRS
!     Find unused neighbour with minimum angle
      ANGMIN = 6.2831853
      DO 102 J = 1, NUMNBRS
        IF(XANGLE(J).LT.ANGMIN) THEN
           ANGMIN = XANGLE(J)
           JMIN = J
        ENDIF
102     CONTINUE 
      NCT = NCT + 1
      NBARRAY(NCT) = NBTEMP(JMIN)
      XANGLE(JMIN) = 999.
101   CONTINUE      
      RETURN
      END

!*--------------------------------------------------------------------------*
      
      FUNCTION PigANGLE(X,Y)

!  Purpose: Return angle in radians.
!  Givens : real x
!           real y
!  Returns: Angle in radians, between 0 and 2*PI,equaling
!           ATAN(Y/X), measured counterclockwise from east
!  Effects: None

      REAL PigAngle
      REAL X, Y, TWOPI
      TWOPI = 6.2831853
      PigANGLE = ATAN2(Y,X)
      IF(PigANGLE.LT.0) PigANGLE = PigANGLE + TWOPI
      END

!*******************************************************************************

      LOGICAL FUNCTION aTr(Tr)

!       Purpose: Returns aTr .true. if nodes Tr( ) form a triangle
!       Note: Essentially same as routine of same name in old TRILED

      integer    TR(3)
      LOGICAL aNb
      aTr = aNb(Tr(1),Tr(2)).and.aNb(Tr(2),Tr(3)).and.aNb(Tr(3),Tr(1)) 
      END

!*******************************************************************************

      LOGICAL FUNCTION aNb(Nb1, Nb2)

!     Purpose: Returns aNb .FALSE. if Nb1, Nb2 are not neighbours
!     Note: Essentially same as routine of same name in old TRILED 

      use MainArrays

      integer   I,   Nb1, Nb2
!   
      aNb = .FALSE.
      if (Nb2.ne.0) then
        do 10 i = 1,nbtot
          if (NL(i,Nb2).eq.Nb1) then
            aNb = .TRUE.
          end if
10        continue
      end if
      END

!*--------------------------------------------------------------------------*

      SubRoutine DeKite(xold, yold)

!     Purpose:  To remove unrestrained nodes that have
!                    four neighbours. Carried out by merging
!                  node with closest neighbour.      

      use MainArrays

      IMPLICIT NONE

! - PASSED VARIABLES
      logical CHANGE

!      integer delete(mrec)
!      integer count
!      COMMON /NOTIFY0/ delete, count

! - LOCAL VARIABLES
      integer ierr, ier, ier2, ndx, nb_count
      real xold, yold 
      real PXRAY1(NBTOTR), PYRAY1(NBTOTR)
      real PXRAY2(NBTOTR), PYRAY2(NBTOTR)
      real dist1, dist2
      integer NBARRAY(NBTOT), NT1, NT2, NT3, NT4, NUMNBRS
      ! - counts
      integer j, j2, j3, j4, j5, j7

!------------------BEGIN------------------

      ierr = 0
      ier = 0
      PXRAY1=0
      PYRAY1=0
      PXRAY2=0
      PYRAY2=0
!     - get the point for cleave operation
!      call NewPt( xold, yold, 3, ier )
      IF ( ier .eq. 0 ) THEN
!       - input detected, check if existing vertex
          call ChkPt( xold, yold, ndx, ier2 )
          IF ( ier2 .eq. 1 ) THEN
!           - vertex does not exist
            call PigMessageOK('ERROR - Invalid point.',' ' )
!           call PigUWait( 2.0 )
!           call PigEraseMessage
            ierr = 1
            ELSEIF ( ier2 .eq. 0 ) THEN !vertex exists, check node has 4 neighbours...
          !     initialse counter
          nb_count = 0
          DO 277 j = 1, NBTOTR
          !      count number of neighbours to this node
            IF ( NL(j,ndx) .ne. 0 ) THEN
              nb_count = nb_count + 1
!                  IF ( nb_count <= 4 ) THEN
!                    Neighs(nb_count)=NL(j,ndx)
!                  ENDIF
            ENDIF
277       CONTINUE
          IF ( nb_count .eq. 4 ) THEN
            !      If number of neighbours = 4 then do "**DeKite-ing**"
!            if(count.ge.mrec) then
!            ! too many deletions...save file and edit again...
!             call PigPutMessage('Too many deletions. Save file and edit again.')  
!             return
!            endif
            call PigPutMessage('DeKite active' )
      ! SORT NEIGHBOURS OF OLD VERTEX (NDX) INTO CCW ORDER FROM EAST
                  NUMNBRS=4
              call CCWSORT(NDX,NUMNBRS,NBARRAY)
! Should have array NBARRAY that is of size (1,4) -the four entries
! containing the node indices of the four neighbours of the centre
! point of the kite...
              NT1 = NBARRAY(1)
              NT2 = NBARRAY(2)
              NT3 = NBARRAY(3)
              NT4 = NBARRAY(4)
              DIST1 = (DXRAY(NT1)-DXRAY(NT3))**2 + (DYRAY(NT1)-DYRAY(NT3))**2
              DIST2 = (DXRAY(NT2)-DXRAY(NT4))**2 + (DYRAY(NT2)-DYRAY(NT4))**2              

              PXRAY1(1)=DXRAY(ndx)
              PYRAY1(1)=DYRAY(ndx)
            DO  j7 = 1,4
                PXRAY1(j7+1)=DXRAY(NBARRAY(j7))
                PYRAY1(j7+1)=DYRAY(NBARRAY(j7))
              ENDDO

              DO  j2 = 1,4 ! Remove ndx node from neighbour 
                                          ! list of four neighbours
                  DO  j3 = 1,NBTOTR
                    IF ( NL(j3,NBARRAY(j2)) == ndx ) THEN
                      NL(j3,NBARRAY(j2))=0
                    ENDIF
                  ENDDO
              ENDDO

              call ReDraw(PXRAY1,PYRAY1,5,0) ! Remove all the current
                                                              ! connections (graphics)
              IF ( dist1<dist2 ) THEN ! Set array to draw new line across shortest
                                                      ! diagonal of kite 
                  PXRAY2(1)=DXRAY(NBARRAY(1))            
                PYRAY2(1)=DYRAY(NBARRAY(1))
                PXRAY2(2)=DXRAY(NBARRAY(3))            
                PYRAY2(2)=DYRAY(NBARRAY(3))
                  DO  j4 = 1,NBTOTR ! Add new connection in neighbour lists
                IF ( NL(j4,NBARRAY(1)) == 0 ) THEN
                  NL(j4,NBARRAY(1)) = NBARRAY(3)
                        IF ( NL(j4,NBARRAY(1)) == NBARRAY(3) ) EXIT
                    !RETURN
                    ENDIF
                  ENDDO
                  DO  j5 = 1,NBTOTR
                  IF ( NL(j5,NBARRAY(3)) == 0 ) THEN
                        NL(j5,NBARRAY(3)) = NBARRAY(1)
                        IF ( NL(j5,NBARRAY(3)) == NBARRAY(1) ) EXIT
                   ! RETURN
                    ENDIF
                  ENDDO
              ELSE
                  PXRAY2(1)=DXRAY(NBARRAY(2))            
                PYRAY2(1)=DYRAY(NBARRAY(2))
                PXRAY2(2)=DXRAY(NBARRAY(4))            
                PYRAY2(2)=DYRAY(NBARRAY(4))
                  DO  j4 = 1,NBTOTR ! Add new connection in neighbour lists
                IF ( NL(j4,NBARRAY(2)) == 0 ) THEN
                  NL(j4,NBARRAY(2)) = NBARRAY(4)
                  IF ( NL(j4,NBARRAY(2)) == NBARRAY(4) ) EXIT
                    !RETURN
                    ENDIF
                  ENDDO
                  DO  j5 = 1,NBTOTR
                  IF ( NL(j5,NBARRAY(4)) == 0 ) THEN
                        NL(j5,NBARRAY(4)) = NBARRAY(2)
                        IF ( NL(j5,NBARRAY(4)) == NBARRAY(2) ) EXIT
                    !RETURN
                    ENDIF
                  ENDDO
              ENDIF
              call ReDraw(PXRAY2,PYRAY2,2,1)
                        ! Remove all the current connections (graphics)

            !- flag the point for deletion
            !exist(ndx) = .FALSE.
            code(ndx) = -9
!            count = count + 1
!            delete(count) = ndx
            change = .true.
            ELSE !node does not have 4 neightbours
              call PigPutMessage('Error - node must have 4 neighbours only' )
          ENDIF
        ENDIF
      ENDIF

      RETURN
      END

!*******************************************************************************
!*---------------------- END NEWEDITS.FOR ----------------------------------*

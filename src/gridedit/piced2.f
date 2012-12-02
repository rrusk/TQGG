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
C                             PICED2.FOR                                    *
C     This module is concerned with the updating of edited vertices and     *
C     line segments and includes move/merge operations.                     *
C*--------------------------------------------------------------------------*
C*--------------------------------------------------------------------------*
      subroutine DelUp(mvndx, DelUpOKp)
      integer mvndx
      logical DelUpOKp
      logical DelUpOK
      DelUpOKp = DelUpOK(mvndx)
      end
      
      logical function DelupOK(mvndx)
C
C PURPOSE : To update the screen arrays (mark for deletion)
C           and to update all the neighborhoods
C UPDATES : exist - at location mvndx
C           nl - the neighborhood array

      use MainArrays

C     - INCLUDES
      INCLUDE '../includes/graf.def'

      integer mvndx
c      COMMON /PICK/ mvndx

!      integer delete(mrec)
!      integer count
!      COMMON /NOTIFY0/ delete, count

C     - LOCAL VARIABLES 
      integer i,j
      integer temndx
C              - temp index to the data file
      CHARACTER*80 ans
      integer ians, msymbol1
      LOGICAL boundary
C              - set true if the deleted point was a boundary point

C---------BEGIN-------------
      delupOK = .true.
      msymbol1 = 3
!      if(count.ge.mrec) then
!*         too many deletions...save file and edit again...
!        call PigPutMessage(
!     +        'Too many deletions. Save file and edit again.')
!        delupOK = .false.
!        return
!      endif
      if    (    (mvndx.lt.1)
     +      .or. (mvndx.gt.MREC)
     +      ) then
*           invalid mvndx
          call PigMessageOK('ERROR:Illegal mvndx in DelUpOK()','delup')
          delupOK = .false.
        return
      endif

      IF ( code(mvndx) .ne. 0) THEN
        boundary = .TRUE.
      ELSE
        boundary = .FALSE.
      ENDIF
C     - flag the point for deletion
      !exist(mvndx) = .FALSE.
      code(mvndx) = -9
!      count = count + 1
!      delete(count) = mvndx
C     - now update all the neighborhood points
      DO i = 1,nbtotr
        temndx = NL(i,mvndx)
        IF (boundary) THEN
          IF (NL(i,mvndx) .NE. 0) THEN
            IF (code(temndx) .EQ. 0) THEN
              ians = 0
              DO WHILE ( ians .lt. 48 )
                call Putmarker( dxray(temndx),
     +                          dyray(temndx), msymbol1, red )
                call PigPrompt('Enter a Non-Negative'//
     +             ' code for this boundary point:', ans)
                ians = ichar (ans(1:1))
              ENDDO
              code(temndx) = ians - 48
C             - remove marker
              call Putmarker( dxray(temndx),
     +                        dyray(temndx), msymbol1, backgr )
            ENDIF
          ENDIF
        ENDIF
        IF ( temndx .ne. 0 ) THEN
          DO j = 1, nbtotr
            IF (NL(j,temndx) .eq. mvndx) THEN
              NL(j,temndx) = 0
            ENDIF
          ENDDO
        ENDIF
      ENDDO
      END

C*--------------------------------------------------------------------------*

      SUBROUTINE LnChg3( xmov, ymov, FirstPoint, NextPoint )

C PURPOSE : To change line segment(s).
C GIVEN   : change  - logical, flag tells changes have been made
C                     which require new triangle list
C RETURNS : ierr - variable flag for errors and quitting

      use MainArrays, only : nbtot

C     - PASSED PARAMETERS
      LOGICAL change, FirstPoint, NextPoint

C     - INCLUDES
      INCLUDE '../includes/graf.def'

      integer  mvndx
      COMMON  /PICK/ mvndx

C     - LOCAL VARIABLES
      REAL, save :: pxray(nbtot), pyray(nbtot)
C              - holds the x y coordinates for the point and it's neighbors
      CHARACTER*80 ans

      integer, save :: icnt
C              - the number of neighbors to a specific point
      REAL, save :: xold, yold
C              - the old x y locations
      REAL xnew, ynew, xmov, ymov
C              - and its new location
      LOGICAL, save :: trans
C              - set true if a transparent delete (merge) is to be
C              -- performed, rather than a simple move 
      LOGICAL, save :: tnear
C              - set true if point is moved near a neighbour, but
C              -- not near enough for a merge
      integer new_index
C              - index of a point that the original point might be 
C              -- moved to
      integer idrr
      integer ierr 
      integer, save :: msymbol2
C              - error indicator for depth calculation
c     following added 6 April 93 for use of PigCursYesNo
      character*80 cstr
      character*1 PigCursYesNo

C--------- BEGIN -----------

      msymbol2 = 4
      trans = .FALSE.
      tnear = .FALSE.

C     - get the point to be moved
      if(FirstPoint) then
        ierr = 1
        xold = xmov
        yold = ymov
        call ChkPt( xold, yold, mvndx, ierr)
        IF ( ierr .eq. 0 ) THEN
C         - existing vertex, put a red marker at the spot that the checkpoint
C         -- routine returns
          call PutMarker( xold, yold, msymbol2, red )
          pxray(1) = xold
          pyray(1) = yold
          icnt = 1
C         - get all the neighbors to that point (the X,Y locations)
          call RelPt2( pxray, pyray, mvndx, icnt, ierr )
          IF ( ierr .eq. 0 ) THEN
C           - neighbours found, now get the new location to move to
          else !cleanup
C           - neighbours not found, erase marker at original location
            call PutMarker( xold, yold, msymbol2, backgr )
            call PigPutMessage( 'ERROR Locating Neighbours!!')
            return
          endif
          FirstPoint = .false.
          NextPoint = .true.
        else 
          return
        endif
      elseif(NextPoint) then
        xnew = xmov
        ynew = ymov
!XXXXXXX                call NewPt( xnew, ynew, 4, ierr )
        ierr = 0
        IF ( ierr .eq. 0 ) THEN
C         - see if new location falls into error range of
C         -- any neighbors of the original point
          call ChkTrans( xnew, ynew, mvndx, new_index,
     +                trans, tnear)
C         - erase the marker at the original location
          call PutMarker( xold, yold, msymbol2, backgr )
C         - erase the old point and line segments to neighbors
          call ReDraw( pxray, pyray, icnt, backgr )
          pxray(1) = xnew
          pyray(1) = ynew
C         - now redraw the point at new location and its neighbors
          call ReDraw( pxray, pyray, icnt, 1 )
C         - ask confirmation
          IF ( trans ) THEN
             cstr='Is this MERGE ok ?:'
             ans=PigCursYesNo (cstr)
          ELSE
!            IF ( tnear ) THEN
!              call PigMessageOK('This is a move NOT a merge', ' ')
!              call PigUWait(2.0)
!            ENDIF
            cstr='Is this MOVE ok ?:'
            ans=PigCursYesNo (cstr)
          ENDIF
          IF ( ans(1:1) .eq. 'Y' ) THEN
C           - see if there is to be a transparent delete
            IF ( trans ) THEN
              call Transparent( mvndx, new_index )
              mvndx = new_index
              change = .TRUE.
            ELSE
              call MovDep( mvndx, xnew, ynew, idrr)
              call PigPutMessage( 
     +             'New depth set by interpolation.' )
            ENDIF
            call Update( mvndx, xnew, ynew )
          ELSE
C               - if not satisfactory
            call PigPutMessage( 'No update..')
C           - erase the new
            call ReDraw( pxray, pyray, icnt, 0 )
C           - and replace the old
            pxray(1) = xold
            pyray(1) = yold
            call ReDraw( pxray, pyray, icnt, 1 )
            IF ( trans ) THEN
              mvndx = new_index
              pxray(1) = xnew
              pyray(1) = ynew
              icnt = 1
              call RelPt2( pxray, pyray, mvndx, icnt, ierr )
              call ReDraw( pxray, pyray, icnt, 1 )
            ENDIF
          ENDIF
          FirstPoint = .true.
          NextPoint = .false.
        ENDIF
C             - ( input detected )
      ENDIF
C       - ( input detected )
      END

C*--------------------------------------------------------------------------*

      SUBROUTINE ChkTrans( xnew, ynew, mvndx, new_index, trans,tnear)

C Purpose :  to see if the moved point falls in the error range
C            of any one of its neighbours. If it does, then it
C            will be flagged so that a merge (transparent delete)
C            can be carried out rather than a simple move
C Given    : XNEW, YNEW - Locations of the new point picked by user
C            MVNDX - index of the point being moved
C Returns  : NEW_INDEX - index of the point which falls within range
C            TRANS - set true if a point does fall within range
C            TNEAR - set true if point is near but not near enough
C                    for a merge
C Modifies : XNEW, YNEW - if there is a point that the user is
C            obviously moving to, the X and Y locations are taken
C            from point that is being moved to.

      use MainArrays

C *** PASSED VARIABLES
      integer MVNDX, NEW_INDEX
      REAL XNEW, YNEW
      LOGICAL TRANS, TNEAR

C *** LOCAL VARIABLES ***
      integer J
      real dist1
      REAL HXLIM, HYLIM, LXLIM, LYLIM
C       - upper limits for checking for error range

! *** START SUBROUTINE ***

      TRANS = .false.
      TNEAR = .false.
      DO J = 1, NBTOTR
        IF(NL(J,MVNDX).gt.0) THEN
          dist1 = sqrt((DXRAY(MVNDX)-DXRAY(NL(J,MVNDX)))**2
     &                 +(DYRAY(MVNDX)-DYRAY(NL(J,MVNDX)))**2)
!        check if moved point is near one of its neighbours
          HXLIM = DXRAY(NL(J,MVNDX)) + .07*dist1  !5*RANGE
          LXLIM = DXRAY(NL(J,MVNDX)) - .07*dist1  !5*RANGE
          if ((XNEW .le. HXLIM) .and. (XNEW .ge. LXLIM)) then
            HYLIM = DYRAY(NL(J,MVNDX)) + .07*dist1  !5*RANGE
            LYLIM = DYRAY(NL(J,MVNDX)) - .07*dist1  !5*RANGE
            IF ((YNEW .le. HYLIM) .and. (YNEW .ge. LYLIM)) THEN
              TNEAR = .true.
              XNEW = DXRAY(NL(J,MVNDX))
              YNEW = DYRAY(NL(J,MVNDX))
              NEW_INDEX = NL(J,MVNDX)
              TRANS = .true.
              TNEAR = .false.
              exit
            ENDIF
          endif
        ENDIF
      enddo

      return
      end


C*--------------------------------------------------------------------------*

      SUBROUTINE MergeNode( xmov, ymov, FirstPoint, NextPoint )

C PURPOSE : To merge nodes.
C GIVEN   : change  - logical, flag tells changes have been made
C                     which require new triangle list
C RETURNS : ierr - variable flag for errors and quitting

      use MainArrays

C     - INCLUDES
      INCLUDE '../includes/graf.def'

C     - PASSED PARAMETERS
      LOGICAL change, FirstPoint, NextPoint
      REAL xmov, ymov

C     - COMMON AREAS
      integer  mvndx
      COMMON  /PICK/ mvndx

C     - LOCAL VARIABLES
      REAL, save :: pxray(nbtot), pyray(nbtot)
C              - holds the x y coordinates for the point and it's neighbors
      CHARACTER*80 ans

      integer, save :: icnt
C              - the number of neighbors to a specific point
      REAL, save :: xold, yold
C              - the old x y locations
      REAL xnew, ynew
C              - and its new location
      LOGICAL, save :: trans
C              - set true if a transparent delete (merge) is to be
C              -- performed, rather than a simple move 
      LOGICAL, save :: tnear
C              - set true if point is moved near a neighbour, but
C              -- not near enough for a merge
      integer, save :: move_index, new_index
C              - index of a point that the original point might be 
C              -- moved to
      integer ierr 
      integer, save :: msymbol2
C              - error indicator for depth calculation
c     following added 6 April 93 for use of PigCursYesNo
      character*80 cstr
      character*1 PigCursYesNo

C--------- BEGIN -----------

      msymbol2 = 4
      trans = .FALSE.
      tnear = .FALSE.

C     - get the point to be moved
      if(FirstPoint) then
        ierr = 1
        xold = xmov
        yold = ymov
        call ChkPt( xold, yold, move_index, ierr)
        IF ( ierr .eq. 0 ) THEN
          mvndx = move_index
C         - existing vertex, put a red marker at the spot 
          call PutMarker( xold, yold, msymbol2, red )
          pxray(1) = xold
          pyray(1) = yold
          icnt = 1
C         - get all the neighbors to that point (the X,Y locations)
          call RelPt2( pxray, pyray, move_index, icnt, ierr )
          IF ( ierr .eq. 0 ) THEN
C           - neighbours found, now get the new location to move to
          else !cleanup
C           - neighbours not found, erase marker at original location
            call PutMarker( xold, yold, msymbol2, backgr )
            call PigPutMessage( 'ERROR Locating Neighbours!!')
            return
          endif
          FirstPoint = .false.
          NextPoint = .true.
        else 
          return
        endif
      elseif(NextPoint) then
        xnew = xmov
        ynew = ymov
        call ChkPt( xnew, ynew, new_index, ierr)
        IF ( ierr .eq. 0 ) THEN
          if(move_index.eq.new_index) return
          if(code(move_index).ne.0.and.code(new_index).ne.0) then
            trans = .true.
          else
            DO J = 1, NBTOTR
              IF(NL(J,move_index).eq.new_index) THEN
                trans = .true.
                exit
              endif
            enddo
          endif
          IF ( trans ) THEN
C         - erase the marker at the original location
            call PutMarker( xold, yold, msymbol2, backgr )
C         - erase the old point and line segments to neighbors
            call ReDraw( pxray, pyray, icnt, backgr )
            pxray(1) = xnew
            pyray(1) = ynew
C         - now redraw the point at new location and its neighbors
            call ReDraw( pxray, pyray, icnt, 1 )

C         - ask confirmation
            cstr='Is this MERGE ok ?:'
            ans=PigCursYesNo (cstr)
          ELSE
            return
          ENDIF

          IF ( ans(1:1) .eq. 'Y' ) THEN
C           - see if there is to be a transparent delete
            IF ( trans ) THEN
              call Transparent( move_index, new_index )
              mvndx = new_index
              change = .TRUE.
            ENDIF
            call Update( new_index, xnew, ynew )
          ELSE
C               - if not satisfactory
            call PigPutMessage( 'No update..')
C           - erase the new
            call ReDraw( pxray, pyray, icnt, 0 )
C           - and replace the old
            pxray(1) = xold
            pyray(1) = yold
            call ReDraw( pxray, pyray, icnt, 1 )
            IF ( trans ) THEN
              mvndx = new_index
              pxray(1) = xnew
              pyray(1) = ynew
              icnt = 1
              call RelPt2( pxray, pyray, new_index, icnt, ierr )
              call ReDraw( pxray, pyray, icnt, 1 )
            ENDIF
          ENDIF
          FirstPoint = .true.
          NextPoint = .false.
        ENDIF
C             - ( input detected )
      ENDIF
C       - ( input detected )
      END

C*--------------------------------------------------------------------------*

      SUBROUTINE Transparent( from_index, to_index )

C Purpose : To do a transparent delete operation when a vertex is
C           moved to within the error radius of another vertex
C           **NOTE** No safeguard to see if space available for
C           possible new neighbours
C Given   : FROM_INDEX - index of the point the user is moving
C           TO_INDEX - index of the point (a neighbour to the one
C           that is being moved) the user is moving to
C Modifies: TO_INDEX - will become the current index and
C           FROM_INDEX is slated to become deleted.
C           Therefore, the code for TO_INDEX must be
C           updated. Also other neighbours of
C           FROM_INDEX must be transferred to become
C           neighbours of TO_INDEX.

      use MainArrays

C *** PASSED VARIABLES ***
      integer FROM_INDEX, TO_INDEX

C *** COMMON AREA ***
!      integer DELETE(mrec)
!      integer COUNT
!      COMMON /NOTIFY0/DELETE,COUNT
C
C *** LOCAL VARIABLES ***
      integer I, J, K
      integer NEIGH
C       - a new neighbour for point that is being moved to (TO_INDEX)
      LOGICAL FOUND
      integer DOUBLE_COUNT

C *** START SUBROUTINE ***

!      if(count.ge.mrec) then
!         too many deletions...save file and edit again...
!        call PigPutMessage(
!     +        'Too many deletions. Save file and edit again.')
!        return
!      endif

C     *update the codes
      if ((CODE(TO_INDEX) .eq. 0) .and. (CODE(FROM_INDEX) .ne. 0)) then
        CODE(TO_INDEX) = CODE(FROM_INDEX)
      endif
C
C     *change the FROM_INDEX neighbour in TO_INDEX to zero
      FOUND = .false.
      I = 1
C     do while ((I .le. NBTOTR) .and. (.not. FOUND))
10    IF ((I .gt. NBTOTR) .or. (FOUND)) THEN
        goto 15
      ENDIF
      if (NL(I,TO_INDEX) .eq. FROM_INDEX) then
        NL(I,TO_INDEX) = 0
        FOUND = .true.
      endif
      I = I + 1
      goto 10
15    continue  
C     enddo

C     *go to neighbour of TO_INDEX and see if any of their neighbours
C      are FROM_INDEX. If so, change that value to TO_INDEX
      I = 1
C     do while (I .le. NBTOTR)
30    IF (I .gt. NBTOTR) THEN
        goto 35
      ENDIF
      NEIGH = NL(I,TO_INDEX)
      if(neigh.eq.0) go to 27
      J = 1
      FOUND = .false.
C       do while ((J .le. NBTOTR) .and. (.not. FOUND))
20    IF ((J .gt. NBTOTR) .or. (FOUND)) THEN
        goto 23
      ENDIF
      if (NL(J,NEIGH) .eq. FROM_INDEX) then
        NL(J,NEIGH) = TO_INDEX
        FOUND = .true.
      endif
      J = J + 1
      goto 20
23    continue
C     enddo

C        *now check for repeaters (having TO_INDEX as a neighbour more
C        *than once).
       K = 1
       DOUBLE_COUNT = 0
C      do while (K .le. NBTOTR)
25       IF(K .gt. NBTOTR) THEN
            goto 27
         ENDIF     
         if (NL(K,NEIGH) .eq. TO_INDEX) then
             DOUBLE_COUNT = DOUBLE_COUNT + 1
         endif
         if (DOUBLE_COUNT .gt. 1) then
           NL(K,NEIGH) = 0
           DOUBLE_COUNT = DOUBLE_COUNT - 1
        endif
        K = K + 1
        goto 25
27       continue
C        enddo
        I = I + 1
        goto 30
35      continue        
C    enddo

C     *go to neighbours of FROM_INDEX and see if any of their neighbours
C     *are FROM_INDEX. If so, change the value to TO_INDEX
      I = 1
C     do while (I .le. NBTOTR)
50    IF (I .gt. NBTOTR) THEN
      goto 55
      ENDIF     
      NEIGH = NL(I,FROM_INDEX)
      if(neigh.eq.0) go to 47
      J = 1
      FOUND = .false.
C        do while ((J .le. NBTOTR) .and. (.not. FOUND))
40       IF ((J .gt. NBTOTR) .or. (FOUND)) THEN
         goto 43
      ENDIF
         if (NL(J,NEIGH) .eq. FROM_INDEX) then
            NL(J,NEIGH) = TO_INDEX
            FOUND = .true.
         endif
         J = J + 1
         goto 40
43       continue
C        enddo
C        *now check for repeaters again (having TO_INDEX as 
C        *a neighbor more than once)
      K = 1
      DOUBLE_COUNT = 0
C        do while (K .le. NBTOTR)
45       IF (K .gt. NBTOTR) THEN
         goto 47
      ENDIF
         if (NL(K,NEIGH) .eq. TO_INDEX) then
            DOUBLE_COUNT = DOUBLE_COUNT + 1
         endif
         if (DOUBLE_COUNT .gt. 1) then
            NL(K,NEIGH) = 0
            DOUBLE_COUNT = DOUBLE_COUNT - 1
         endif
         K = K + 1
         goto 45
47       continue
C        enddo
      I = I + 1
      goto 50
55    continue
C     enddo

C     * transfer neighbors of FROM_INDEX to TO_INDEX
      I = 1
C     do while (I .le. NBTOTR)
70    IF (I .gt. NBTOTR) THEN
      goto 75
      ENDIF     
      if ((NL(I,FROM_INDEX) .ne. 0) .and. 
     +   (NL(I,FROM_INDEX) .ne. TO_INDEX)) then
C           *compare against NL list in TO_INDEX
         J = 1
         FOUND = .false.
C           do while ((J .le. NBTOTR) .and. (.not. FOUND))
60          IF ((J .gt. NBTOTR) .or. (FOUND)) THEN
            goto 63
         ENDIF
            if (NL(I,FROM_INDEX) .eq. NL(J,TO_INDEX)) then
            FOUND = .true.
            endif
            J = J + 1
         goto 60
63          continue
C           enddo
C           *if no duplicates found, then find a zero spot 
C           and transfer
         if (.not. FOUND) then
            K = 1
C              do while ((K .le. NBTOTR) .and. (.not. FOUND))
65             IF ((K .gt. NBTOTR) .or. (FOUND)) THEN
            goto 67
            ENDIF
            if (NL(K,TO_INDEX) .eq. 0) then
               FOUND = .true.
               NL(K,TO_INDEX) = NL(I,FROM_INDEX)
            endif
            K = K + 1
            goto 65
67             continue
C              enddo
         endif
      endif
      I = I + 1
      goto 70
75    CONTINUE
C     enddo

C     *flag the point (FROM_INDEX) for deletion
      !EXIST(FROM_INDEX) = .false.
      code(FROM_INDEX) = -9
!      COUNT = COUNT + 1
!      DELETE(COUNT) = FROM_INDEX

      end


C*--------------------------------------------------------------------------*
      SUBROUTINE MovDep( mvndx, xnew, ynew, ierr )

C Purpose : to set depth at point moved;
C           - by linear interpolation if interior point
C           - by user if boundary point
C Given   : xnew, ynew - new coordinates of moved point
C           mvndx      - index to DXRAY(), DYRAY() of moved point
C Modifies: DEPTH(mvndx) = depth at moved point
C           ierr = 0 if depth O.K. (manual or automatic)
C                = 1 if point is not in any triangle around mvndx
C                  ( depth is set equal to 0 )

      use MainArrays

      INCLUDE '../includes/graf.def'
!      INCLUDE 'info.inc'

C     - PASSED VARIABLES
      integer mvndx, ierr
      REAL xnew, ynew

C     - LOCAL VARIABLES
      CHARACTER*80 ans

      character cstr*80, PigCursYesNo*1
      integer nbm(nbtot), tnbm, vert(3), i, j
      REAL x1, y1, x2, y2, x3, y3, da1, da2, da3, da4, delta, tdepth
      REAL a, b, c, z1, z2, z3, aa1, aa2, b1, c1
      LOGICAL Success, aTr

C----------------------BEGIN----------------------

      ierr = 0
C     - check if moved pt. is boundary point
      if ( CODE(mvndx) .ne. 0 ) go to 500
C     - begin procedure for INTERIOR point
c      call PigPutMessage( 'New depth set by interpolation.' )
      VERT(1) = mvndx
      x1 = DXRAY(mvndx)
      y1 = DYRAY(mvndx)
C     - find neighbours of moved point, tnbm = no. of neighbours
      tnbm = 0
      do i = 1, NBTOT
      if ( NL(i,mvndx) .gt. 0 ) then
        tnbm = tnbm + 1
        nbm(tnbm) = NL(i,mvndx)
      endif
      enddo
C       - ( i = 1 to NBTOT )
C     - find valid triangles around moved point
      do i = 1, tnbm - 1
      VERT(2) = nbm(i)
      x2 = DXRAY(VERT(2))
      y2 = DYRAY(VERT(2))
      do j = i + 1, tnbm
        VERT(3) = nbm(j)
C         - check if vert(1), vert(2), vert(3) form valid triangle
        if ( aTr(VERT) ) then
          x3 = DXRAY( VERT(3) )
          y3 = DYRAY( VERT(3) )
          da1 = ABS( (x1-xnew)*(y2-ynew)-(x2-xnew)*(y1-ynew) )
          da2 = ABS( (x2-xnew)*(y3-ynew)-(x3-xnew)*(y2-ynew) )
          da3 = ABS( (x3-xnew)*(y1-ynew)-(x1-xnew)*(y3-ynew) )
          da4 = ABS( (x2-x1)*(y3-y1)-(x3-x1)*(y2-y1) )
C           - if delta > 0 then point is outside triangle
          if ( da4 .gt. 0 ) then
            delta =da1 + da2 + da3 - da4
            if ( delta .le. (da4 * 1.0E-6) ) then
C               - calculate linear depth fit coeffts. for this triangle
            z1 = DEPTH( VERT(1) )
            z2 = DEPTH( VERT(2) )
            z3 = DEPTH( VERT(3) )
            aa1 = z1*y2 - z2*y1 + z3*y1 - z1*y3 + z2*y3 - z3*y2
            aa2 = x1*y2 - x2*y1 + x3*y1 - x1*y3 + x2*y3 - x3*y2
            b1 = x2*z3 - z2*x3 + x3*z1 - x1*z3 + x1*z2 - x2*z1
            c1 = x1*(y2*z3-y3*z2) + x2*(y3*z1-y1*z3) + 
     |               x3*(y1*z2-y2*z1)
            if ( aa2 .ne. 0 ) then 
              a = aa1/aa2
              b = b1/aa2
              c = c1/aa2
            else
              a = 0
              b = 0
              c = 0
            endif
C                 - ( aa2 ne 0 )
            DEPTH(mvndx) = a * xnew + b * ynew + c  
            go to 600
            endif
C               - ( delta le (da4 * 1.0E-6) )
          endif
C             - ( da4 gt 0 )
        endif
C           - ( aTr(VERT) )
      enddo
C         - ( j = i + 1 to tnbm )
      enddo
C       - ( i = 1 to tnbm - 1 )
C     - no triangle found
      DEPTH(MVNDX) = 0.0
      ierr = 1
      go to 600
C     - manual setting of depth at moved point
500   cstr = 'Present depth OK ?:'
      ans = PigCursYesNo (cstr)
      if ( ans(1:1) .eq. 'N' ) then
C       - prompt till valid depth
510     continue
      call PigPrompt( 'Enter new depth :', ans )
        call PigReadReal( ans, tdepth, Success )
        if ( .NOT. Success ) goto 510
        if ( tdepth .lt. 0.0 ) then
                call PigMessageOK('Depths must be positive.', ' ')
!               call PigUWait(2.0)
                goto 510
        endif
        DEPTH(mvndx) = tdepth
      else
      call PigPutMessage( 'Depth not changed.')
      endif
C       - ( ans = N )
600   continue
C     - Following code added to allow immediate update of depth if it is 
C     -- changed when point is moved. S.P. May 21/89
!      if ( UpdateFlag ) then
C       - change the value on the screen
!      call PigSetTextColour( HitColor )
!      WRITE( rnum, '(F12.3)' ) DEPTH(mvndx)
!      call PanelHit( 8, 7, 7, rnum, 12 )
!      endif
C       - ( UpdateFlag )
C     - End of modification, S.P. May 21/89
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE Update( mvndx, xnew, ynew )
C
C Purpose : To update screen data arrays.
C Given   : mvndx = index to data arrays DXRAY() and DYRAY() of point
C                   whose (x,y) location is being moved.
C           xnew, ynew = new coordinate for point.
C Effects : DXRAY(mvndx) = xnew
C         : DYRAY(mvndx) = ynew

      use MainArrays

C     - PASSED VARIABLES
      integer  mvndx
      REAL xnew, ynew

C------------BEGIN------------------

      DXRAY(mvndx) = xnew
      DYRAY(mvndx) = ynew

      RETURN
      END

!*--------------------------------------------------------------------------*

      SUBROUTINE GridAddLine(xpt1,ypt1,FirstPoint,LastPoint,change)

C Purpose : To add a line segment between two vertices. The
C           user will be prompted for the two vertices. A
C           line segment will be drawn between the two points,
C           and the neighbor lists will be updated.
C Given   : nrec   - the number of points in the data file.
C Returns : change - a logical set to true if any lines were added.
C Effects:  Modifies neighborhood lists if a line is added.

      use MainArrays

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

C     - PASSED PARAMETERS
      LOGICAL FirstPoint,LastPoint,change
      real xpt1, ypt1

      integer MVNDX
      COMMON /PICK/MVNDX

C     - LOCAL VARIABLES
      REAL, save :: xpt0, ypt0, xpt2, ypt2
      character cstr*80, PigCursYesNo*1
      integer, save :: index1, index2
      integer ierr
      REAL strtx(2), strty(2)
      LOGICAL Space
C       - used to see if there is space available in the neighborhood
C         list of the two points chosen for the add
      integer msymbol2

C---------------BEGIN----------------------------------

      msymbol2 = 4

!        index = .FALSE.
!        xy = .FALSE.
!        valid = .TRUE.
!        xhrs = .TRUE.

C               - see if point exists
      if(FirstPoint) then
        call ChkPt( xpt1, ypt1, index1, ierr )
        if ( ierr .eq. 1 ) then
          return
        else
          xpt0 = DXRAY(index1)
          ypt0 = DYRAY(index1)
C        - now check if available space for a new neighbour
          if (.NOT. Space(index1) ) then
            call PutMarker( xpt0, ypt0, msymbol2, green )
            call PigPutMessage(
     +      'ERROR - Point already has maximum number of neighbors')
            call PutMarker( xpt0, ypt0, msymbol2, backgr )
            return
          endif
!        - put a marker at the spot
          call PutMarker( xpt0, ypt0, msymbol2, red )
          FirstPoint = .false.
          LastPoint = .true.
          return
        endif
      elseif(LastPoint) then
!        - now get the second point
        xpt2 = xpt1
        ypt2 = ypt1
!            call NewPt( xpt2, ypt2, 4, ierr )
        call ChkPt( xpt2, ypt2, index2, ierr )
        if ( ierr .eq. 1 ) then
          call PigPutMessage( 'ERROR - Invalid point..' )
          return
        else
          xpt2 = DXRAY(index2)
          ypt2 = DYRAY(index2)

C       - now check if available space for a new neighbour
          if ( .NOT. Space(index2) ) then
            call PutMarker( xpt2, ypt2, msymbol2, green )
            call PigPutMessage(
     +      'ERROR - Point already has maximum number of neighbors')
            call PutMarker( xpt2, ypt2, msymbol2, backgr )
            call PutMarker( xpt0, ypt0, msymbol2, backgr )
            return
          endif
C       - all OK for join of the two vertices, put marker at second point
          call PutMarker( xpt2, ypt2, msymbol2, red )
C       - draw a line between the two vertices
          strtx(1) = xpt0
          strty(1) = ypt0
          strtx(2) = xpt2
          strty(2) = ypt2
          call REDRAW (strtx,strty,2,1)
        endif
        FirstPoint = .true.
        LastPoint = .false.
      endif

C       - now see if the addition is satisfactory
        cstr = 'Accept this change ?:'
        if(PigCursYesNo (cstr) .eq. 'Y') then
C         - update the neighbours list
          MVNDX = index1
          call Naybor( index2 )
          MVNDX = index2
          call Naybor( index1 )
          change = .TRUE.
C         - erase markers and redraw line
          call PutMarker( xpt2, ypt2, msymbol2, backgr )
          call PutMarker( xpt0, ypt0, msymbol2, backgr )
          strtx(1) = xpt0
          strty(1) = ypt0
          strtx(2) = xpt2
          strty(2) = ypt2
          call REDRAW (strtx,strty,2,1)
        else
C         - ans = N, erase the line segment and markers
          strtx(1) = xpt0
          strty(1) = ypt0
          strtx(2) = xpt2
          strty(2) = ypt2
          call REDRAW (strtx,strty,2,0)
          call PutMarker( xpt2, ypt2, msymbol2, backgr )
          call PutMarker( xpt0, ypt0, msymbol2, backgr )
        endif

      END

C*-----------------------END PICED2.FOR---------------------------------*

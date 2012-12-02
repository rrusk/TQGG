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

!-----------------------------------------------------------------------*
C                       STRAIGHT.FOR                                    *
C       This module contains procedures for node manipulation.          *
C       ROUTINES - StraightBnd, StringNodes, StrBndUpdate,              *
C                  StrSpecUpdate, PickBndHalf.              *
!-----------------------------------------------------------------------*
!-----------------------------------------------------------------------*

      SUBROUTINE StraightBnd (nrec,MouseX,MouseY,FirstPoint,NextPoint)

C PURPOSE: To change a specified section of a boundary to a straight
C          line of boundary nodes.
C   INPUT: Interactive input from user
C   GIVEN: None.
C RETURNS: In Common NODES;
C               dxray(), dyray(), depth(), TotCoords, PtsThisBnd() =
C                 updated to reflect changes, if any.
C EFFECTS: User specifies the start and end of a boundary section, then
C          the nodes on the section are deleted and replaced by a straight
C          line of 'n' boundary nodes between the two nodes specified. User
C          picks the integer value for 'n' in StringNodes.
!-----------------------------------------------------------------------*

      use MainArrays

! *** passed variables
      integer nrec
      real MouseX, MouseY
      logical FirstPoint, NextPoint

C - COMMON BLOCKS
C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C       - AUTODPTH stores depths of two delimiting nodes of straight boundary
C       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

C       - UPDATE3 stores indication of whether StrBndUpdate or StrSpecUpdate
C       -- was called by StraightBnd or SaveReSel
      LOGICAL reselcall
      COMMON /UPDATE3/ reselcall

C - LOCAL VARIABLES
      REAL lnx(2), lny(2)
      integer ierr, ndx, nodetype
      integer, save :: indxs(2), bnd, contighalf
      LOGICAL success, valid

!------------------START ROUTINE----------------------------------------

C       - get 2 delimiting nodes to mark section
      if(FirstPoint) then
        ierr = 0
        call CheckNode ( MouseX, MouseY, ndx, ierr, nodetype )
        IF ( ierr .eq. 1 ) THEN
C               - valid node chosen, save it & put marker
          indxs(1) = ndx
          call PigDrawModifySymbol( MouseX, MouseY)
          FirstPoint = .false.
          NextPoint = .true.
          return
        else
          return
        endif
      elseif(NextPoint) then
        ierr = 0
        call CheckNode ( MouseX, MouseY, ndx, ierr, nodetype )
        IF ( (ierr .eq. 1).and.(ndx .ne. indxs(1)) ) THEN
C               - valid node chosen, save it & put marker
          indxs(2) = ndx
          call PigDrawModifySymbol( MouseX, MouseY)
          FirstPoint = .false.
          NextPoint = .false.
          call Check2Nodes( bnd, indxs, valid )
          if(.not.valid) then
            return
          endif
        else
          return
        endif
      else
        return
      endif

      IF ( indxs(1) .gt. indxs(2) ) THEN
        ndx = indxs(1)
        indxs(1) = indxs(2)
        indxs(2) = ndx
      ENDIF

C         - nodes are valid, determine which half of boundary to specify
C         - store depths of delimiting vertices in case depth by interpolation
      dpth1 = depth ( indxs(1) )
      dpth2 = depth ( indxs(2) )
      call PickBndHalf ( bnd, indxs, contighalf )
      IF ( contighalf .ne. 0 ) THEN
C           - user picked a half as correct one, draw straight line between
C           - nodes & get user input for # nodes to lay out between them
        lnx(1) = dxray ( indxs(1) )
        lny(1) = dyray ( indxs(1) ) 
        lnx(2) = dxray ( indxs(2) )
        lny(2) = dyray ( indxs(2) ) 
        success = .FALSE.
        call StringNodes ( lnx, lny, success )
        IF ( success ) THEN
C             - user decided to replace nodes, update NODES variables
          IF ( contighalf .eq. 1 ) THEN
C               - cotiguous half is correct half
            reselcall = .FALSE.
            call StrBndUpdate ( indxs )
          ELSE
C               - non-cotiguous (straddling) half is correct half
            reselcall = .FALSE.
            call StrSpecUpdate ( indxs )
          ENDIF
C               - ( contighalf = 1 )
        ENDIF
C             - ( success )
      ENDIF
C           - ( contighalf ne 0 )
      nrec = TotCoords + 1

      RETURN
      END

!-----------------------------------------------------------------------*

      SUBROUTINE Check2Nodes ( bound, indx, validmarks )

C PURPOSE: To prompt the user to pick 2 nodes on the same boundary that
C          will delimit a stretch of boundary.
C   INPUT: Interactive input from user - mouse input to pick nodes.
C   GIVEN: None.
C RETURNS: indx(1) & indx(2) = indices to dxray(), dyray() of the
C                              2 delimiting nodes picked, sorted so that
C                              indx(1) is the smaller index.
C          bound = index to PtsThisBnd() of boundary section # where
C                  indx(1) & indx(2) are located.
C          validmarks = .TRUE. if 2 valid nodes were selected. To be valid
C                       nodes must be boundary nodes on the same boundary.
C EFFECTS: If validmarks is True, then 2 nodes are picked to delimit a
C          stretch of boundary. There is still ambiguity as to which stretch
C          of boundary is intended for use ( ie: clockwise from indx(1) to
C          indx(2), or counter-clockwise from indx(1) to indx(2) ).
!-----------------------------------------------------------------------*

      use MainArrays

C - "INCLUDES"
      include '../includes/defaults.inc'

C - PASSED VARIABLES
      LOGICAL validmarks
      integer indx(2), bound

C - LOCAL VARIABLES
      REAL ptx(2), pty(2), xtemp, ytemp
      integer nodetype, bound_tmp(2)
      integer i, k, tempnum
      CHARACTER*80 cstr

!------------------START ROUTINE----------------------------------------

C       - set attributes
      PendMType = 4
      nodetype = 3
ccc nodetype = 2    changed to 3  17/3/96

C       - get 2 valid nodes to delimit boundary section
      validmarks = .FALSE.
C       - set invalid indices to crash code using this routine if
C       - it does not check and use validmarks correctly.
!      IF ( picked1 .AND. picked2 ) THEN
C           - make sure both delimiting markers are on same boundary
      do k=1,2
C             - determine boundary section # for 1st marker
        tempnum = 0
        i = 0

        pTStHISbND (tOTbNDYS + 1) = tOTiNTpTS

        DO WHILE ( i .lt. TotBndys + 1 )
          i = i + 1
          tempnum = tempnum + PtsThisBnd(i)
          IF ( indx(k) .le. tempnum ) THEN
            bound_tmp(k) = i
            i = 9999
          ENDIF
        END DO
      END DO
!           - compare bound1 & bound2
      IF ( bound_tmp(1) .ne. bound_tmp(2) ) THEN
!             - delimiting markers are on 2 different boundaries
        cstr ='Error: nodes on different boundaries.'
        call PigMessageOK( cstr, 'BndLine' )
        validmarks = .FALSE.
!             - delete 2 delimiting markers & redraw nodes they marked
        DO i = 1, 2
          xtemp = ptx(i)
          ytemp = pty(i)
          call PigDrawBndSymbol( xtemp, ytemp )
        END DO
!               - ( i = 1, 2 )
      ELSE
!            - delimiting markers are on same boundary
          validmarks = .TRUE.
          bound = bound_tmp(1)
      ENDIF

      END

!-----------------------------------------------------------------------*

      SUBROUTINE PickBndHalf ( bndry, indx, contig )

C PURPOSE: To prompt the user to choose 'half' of a boundary. 
C   INPUT: Interactive input from user - Y/N answers to prompts.
C   GIVEN: Prior to this procedure, the user has indicated 2 boundary
C          nodes on the same boundary. The 2 nodes divide the boundary into
C          (uneven) halves. The two halves are: 1) a contiguous half from
C          the lower index ( of indx(1) & indx(2) ) to the higher index,
C          2) a non-contiguous half from the higher index to the last node
C          (index) of the boundary & from the first node (index) to the 
C          lower index ( of indx(1) & indx(2) ), this half straddles the
C          first & last nodes of the boundary.
C          indx() = 2 element array containing indices to dxray(), dyray()
C                   of nodes that delimit boundary halves to choose from.
C          bndry = index to PtsThisBnd() of boundary # where indx(1) &
C                  indx(2) are located.
C RETURNS: contig = 1, if selected half is contiguous section of boundary
C                   from indx(1) to indx(2).
C                 = 2, if selected half is non-contiguous section of
C                   boundary straddling the 1st & last nodes of boundary.
C                 = 0, if neither half was selected.
C EFFECTS: User answers Y or N to each half of the boundary as it is 
C          hilighted.
!-----------------------------------------------------------------------*

      use MainArrays

C - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

C - PASSED VARIABLES
      integer bndry, indx(2), contig

C - COMMON BLOCKS
C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C - LOCAL VARIABLES
      REAL  xarr(maxstrnodes), yarr(maxstrnodes)
      common /maxlocal/ xarr, yarr, numinhalf
      integer numinhalf, i, j, tmpndx
      CHARACTER*80 cstr, ans
      CHARACTER*1 PigCursYesNo

!------------------START ROUTINE----------------------------------------

C       - first show nodes that don't straddle first & last nodes
      IF ( indx(1) .gt. indx(2) ) THEN
C         - make indx(1) smallest of the two
        tmpndx = indx(1)
        indx(1) = indx(2)
        indx(2) = tmpndx
      ENDIF
C         - ( indx(1) > indx(2) )

C       - fill array of nodes to hilite for replacement
      j = 1
      DO i = indx(1), indx(2)
        xarr(j) = dxray(i)
        yarr(j) = dyray(i)
        j = j + 1
      END DO
C         - ( i = indx(1), indx(2) )
      numinhalf = indx(2) - indx(1) + 1

C       - now hilite the nodes
      call PigSetSymbolColour( ModifColor )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetWindowNum ( MAINWIN )
      do i=1,numinhalf
            call PigDrawSymbols ( 1, xarr(i), yarr(i))
      end do
c        next line replaced by above loop agd 93/nov/16
c        call PigDrawSymbols ( numinhalf, xarr, yarr)
      call PigSetSymbolColour( NodeBColor )

C       - enquire if correct half of boundary is hilited
      cstr = 'Correct BOUNDARY section ?:'
        ans = PigCursYesNo (cstr)
C       - remove hilite markers, regardless of answer
      call PigSetSymbolColour ( backgr )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetWindowNum ( MAINWIN )
      do i=1,numinhalf
            call PigDrawSymbols ( 1, xarr(i), yarr(i))
      end do
c        next line replaced by above loop agd 93/nov/16
c        call PigDrawSymbols ( numinhalf, xarr, yarr)
      call PigSetSymbolColour( NodeBColor )
      IF ( ans(1:1) .eq. 'Y' ) THEN
C         - contiguous half is correct half
        contig = 1
      ELSE
C         - contiguous half is not correct half, try straddling half
C         - restore original nodes
        call PigSetSymbolColour( NodeBColor )
        call PigSetWindowNum ( MAINWIN )
        call PigSetSymbolNumber ( NodeMType )
      do i=1,numinhalf
            call PigDrawSymbols ( 1, xarr(i), yarr(i))
      end do
c        next line replaced by above loop agd 93/nov/16
c          call PigDrawSymbols ( numinhalf, xarr, yarr)

C         - reset xarr, yarr
        DO i = 1, maxstrnodes
          xarr(i) = 0.0
          yarr(i) = 0.0
        END DO
C           - ( i = 1, maxstrnodes )

C         - non-contiguous half requires 2 more delimiting indices
C         - straddidx(1) pairs with idx(1) marking section from start of
C         - boundary to 1st index
        IF ( bndry .eq. 1 ) THEN
C           - 1st node in boundary is node index 1
          straddidx(1) = 1
        ELSE IF ( bndry .eq. 2 ) THEN
C           - determine 1st node in boundary 2
          straddidx(1) = PtsThisBnd(1) + 1
        ELSE
C           - bndry is 3 or greater, determine 1st node in boundary
          straddidx(1) = 0
          DO i = 1, bndry - 1
C             - total all boundry nodes before start of bndry
            straddidx(1) = straddidx(1) + PtsThisBnd(i)
          END DO
        straddidx(1) = straddidx(1) + 1
      ENDIF
C           - ( bndry = 1 )
C         - straddidx(2) pairs with idx(2) marking section from 2nd index
C         - to end of boundary
C         - last node of bndry is straddidx(2)

          if ( bndry.eq.TotBndys+1 ) PtsThisBnd(bndry) = TotIntPts
      straddidx(2) = straddidx(1) + PtsThisBnd(bndry) - 1

C         - mark remaining nodes, fill arrays of nodes to hilite
C         - insert nodes from straddidx(1) to idx(1) first
      j = 1
      DO i = straddidx(1), indx(1)
        xarr(j) = dxray(i)
        yarr(j) = dyray(i)
        j = j + 1
      END DO
C           - ( i = 1, indx(1) )
C         - insert nodes from idx(2) to straddidx(2) second
      DO i = indx(2), straddidx(2)
        xarr(j) = dxray(i)
        yarr(j) = dyray(i)
        j = j + 1
      END DO
C           - ( i = indx(2), PtsThisBnd )
C         - add 2 to following so count is correct after subtractions
      numinhalf = ( indx(1) - straddidx(1) ) +
     +                ( straddidx(2) - indx(2) ) + 2
      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolColour ( ModifColor )
      do i=1,numinhalf
        call PigDrawSymbols ( 1, xarr(i), yarr(i))
      end do
c        next line replaced by above loop agd 93/nov/16
c          call PigDrawSymbols ( numinhalf, xarr, yarr)
      call PigSetSymbolColour ( NodeBColor )
C         - see if boundary half that straddles 1st & last nodes is correct one
      cstr = 'Correct BOUNDARY section ?:'
        ans = PigCursYesNo (cstr)
C         - remove hilite markers, regardless of answer
      call PigSetSymbolColour ( backgr )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetWindowNum ( MAINWIN )
      do i=1,numinhalf
        call PigDrawSymbols ( 1, xarr(i), yarr(i))
      end do
c        next line replaced by above loop agd 93/nov/16
c          call PigDrawSymbols ( numinhalf, xarr, yarr)
      call PigSetSymbolColour ( NodeBColor )
      IF ( ans(1:1) .eq. 'Y' ) THEN
C           - non-contiguous (straddling) half is correct half
        contig = 2
      ELSE
C           - neither contiguous half nor non-contiguous half is correct half
        contig = 0
C           - restore original nodes
        call PigSetSymbolColour ( NodeBColor )
        call PigSetWindowNum ( MAINWIN )
      do i=1,numinhalf
        call PigDrawSymbols ( 1, xarr(i), yarr(i))
      end do
c        next line replaced by above loop agd 93/nov/16
c            call PigDrawSymbols ( numinhalf, xarr, yarr)
      ENDIF
C           - ( ans = Y )
      ENDIF
C         - ( ans = Y ) - for 1st boundary half checked

      RETURN
      END

!-----------------------------------------------------------------------*

      SUBROUTINE StrBndUpdate ( indxs )

C PURPOSE: To Update data arrays after a successful call to StraightBnd.
C          This update procedure is for updating the contiguous boundary 
C          half.
C   GIVEN: indxs() = 2 element array containing delimiting indices of
C                    nodes in dxray(), dyray(), depth() to remove.
C          in Common /STRBND/;
C               numnodes = # nodes to insert, starting at indxs(1).
C               strx() = array of X coordinates of nodes to insert.
C               stry() = array of Y coordinates of nodes to insert.
C RETURNS: None.
C EFFECTS: Data arrays dxray(), dyray(), depth() are updated. Original
C          nodes along boundary half specified in PickBndHalf are removed
C          and replaced by nodes stored in Common STRBND by StringNodes.
!-----------------------------------------------------------------------*

      use MainArrays

C - PASSED VARIABLES
      integer indxs(2)

C - "INCLUDES"
      include '../includes/defaults.inc'

C - COMMON BLOCKS
C       - STRBND stores straight boundary values for updating
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C       - AUTODPTH stores depths of two delimiting nodes of straight boundary
C       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

C       - UPDATE3 stores indication of whether StrBndUpdate or StrSpecUpdate
C       -- was called by StraightBnd or SaveReSel
      LOGICAL reselcall
      COMMON /UPDATE3/ reselcall

C       - STRADDLE stores 2 extra indices for non-contiguous boundary half,
C       -- used here as flag for DepthUpdate procedure ( both = 0 )
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C - LOCAL VARIABLES
      integer numdel, numrepl, numdiff, i, j, tempnum, cbound
      integer shuffidx, tmp1, tmp2, tmploc,  nodetype
      real tmpdpth

!------------------START ROUTINE--------------------------------------

C       - determine boundary section # where changes are to occur
      tempnum = 0
      i = 0
        pTStHISbND (tOTbNDYS + 1) = tOTiNTpTS
      DO WHILE ( i .lt. TotBndys + 1 )
        i = i + 1
        tempnum = tempnum + PtsThisBnd(i)
        IF ( indxs(2) .le. tempnum ) THEN
          cbound = i
C           - now end the loop
          i = 9999
        ENDIF
      END DO
C         - ( i > TotBndys + 1 )

C       - numdel = # existing nodes to remove
      numdel = ABS ( indxs(2) - indxs(1) ) + 1
C       - numrepl = # new nodes to insert
      numrepl = numnodes
      IF ( numdel .ge. numrepl ) THEN
C         - room in deleted space for new nodes, shuffle down
        numdiff = numdel - numrepl
C         - insert new nodes
        j = 1
        tmp1 = indxs(1)
        tmp2 = indxs(1) + numrepl - 1
        DO i = tmp1, tmp2
          dxray(i) = strx(j)
          dyray(i) = stry(j)
            depth(i) = strz(j)
          j = j + 1
        END DO
C           - ( i = tmp1, tmp2 )
C         - how many spots are left over ( numdiff ) ?
        IF ( numdiff .gt. 0 ) THEN
C           - shuffle those spots away
            shuffidx = indxs(1) + numrepl
            DO WHILE ( (shuffidx + numdiff ) .le. TotCoords)
            dxray(shuffidx) = dxray(shuffidx + numdiff)
            dyray(shuffidx) = dyray(shuffidx + numdiff)
            depth(shuffidx) = depth(shuffidx + numdiff)
            shuffidx = shuffidx + 1
            END DO
            TotCoords = TotCoords - numdiff

              IF ( CBOUND.EQ.tOTbNDYS+1 ) pTStHISbND(CBOUND) = tOTiNTpTS
            PtsThisBnd(cbound) = PtsThisBnd(cbound) - numdiff
              IF ( CBOUND.EQ.tOTbNDYS+1 ) tOTiNTpTS = pTStHISbND(CBOUND)

        ENDIF
C           - ( numdiff > 0 )
      ELSE
C         - not enough room in deleted space for new nodes, shuffle up
        numdiff = numrepl - numdel
C         - make room for new nodes by shuffling up
        shuffidx = TotCoords + numdiff
        DO WHILE ( (shuffidx - numdiff ) .ge. indxs(1) )
          dxray(shuffidx) = dxray(shuffidx - numdiff)
          dyray(shuffidx) = dyray(shuffidx - numdiff)
          depth(shuffidx) = depth(shuffidx - numdiff)
          shuffidx = shuffidx - 1
        END DO
C         - insert new nodes
        j = 1
        tmp1 = indxs(1)
        tmp2 = indxs(1) + numrepl - 1
        DO i = tmp1, tmp2
          dxray(i) = strx(j)
          dyray(i) = stry(j)
           depth(i) = strz(j)
          j = j + 1
        END DO
C           - ( i = tmp1, tmp2 )
        TotCoords = TotCoords + numdiff

          IF ( CBOUND.EQ.tOTbNDYS+1 ) pTStHISbND(CBOUND) = tOTiNTpTS
        PtsThisBnd(cbound) = PtsThisBnd(cbound) + numdiff
          IF ( CBOUND.EQ.tOTbNDYS+1 ) tOTiNTpTS = pTStHISbND(CBOUND)

      ENDIF
C         - ( numdel >= numrepl )
C       - nodetype = boundary nodes
      nodetype = 2
C       - tmp1 matches to dpth1, tmp2 matches to dpth2
C       - make tmp1 smallest of tmp1 & tmp2, keeping matching depths
      IF ( tmp1 .gt. tmp2 ) THEN
        tmploc = tmp1
        tmp1 = tmp2
        tmp2 = tmploc
        tmpdpth = dpth1
        dpth1 = dpth2
        dpth2 = tmpdpth
      ENDIF
C         - ( tmp1 > tmp2 )
C       - set straddidx(1) & (2) to flag contiguous update
      straddidx(1) = 0
      straddidx(2) = 0
C       - 2nd argument to DepthUpdate 'types' calling routine
      IF ( reselcall ) THEN
C         - called by SaveReSel ( 5 )
        call DepthUpdate ( nodetype, 5, tmp1, tmp2 )
      ELSE
C         - called by StraightBnd ( 3 )
        call DepthUpdate ( nodetype, 3, tmp1, tmp2 )
      ENDIF
C         - ( reselcall )

      RETURN
      END

!-----------------------------------------------------------------------*

      SUBROUTINE StrSpecUpdate ( indxs )

C PURPOSE: To Update data arrays after a successful call to StraightBnd.
C          This update procedure is for updating the non-contiguous boundary 
C          half.
C   GIVEN: indxs() = 2 element array containing delimiting indices of
C                    nodes in dxray(), dyray(), depth() to remove.
C          in Common /STRBND/;
C               numnodes = # nodes to insert, starting at indxs(1).
C               strx() = array of X coordinates of nodes to insert.
C               stry() = array of Y coordinates of nodes to insert.
C          in Common STRADDLE;
C                straddidx(1) = index of first node in boundary section
C                               being updated, pairs with nodes(1) to
C                               define half of non-contig boundary section,
C                straddidx(2) = index of last node in boundary section
C                               being updated, pairs with nodes(2) to
C                               define half of non-contig boundary section,
C          in Common AUTODPTH;
C                dpth1 = depth at nodes(1) prior to StraightBnd,
C                dpth2 = depth at nodes(2) prior to StraightBnd.
C RETURNS: In Common in file NODESTOR.INC;
C               updated arrays dxray(), dyray(), depth(), and updated counts
C               TotCoords, PtsThisBnd().
C EFFECTS: Data arrays dxray(), dyray(), depth() are updated. Original
C          nodes along boundary half specified in PickBndHalf are removed
C          and replaced by nodes stored in Common STRBND by StringNodes.
!-----------------------------------------------------------------------*

      use MainArrays

C - PASSED VARIABLES
      integer indxs(2)

C - "INCLUDES"
      include '../includes/defaults.inc'

C - COMMON BLOCKS
C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - AUTODPTH stores depths of two delimiting nodes of straight boundary
C       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

C       - UPDATE3 stores indication of whether StrBndUpdate or StrSpecUpdate
C       -- was called by StraightBnd or SaveReSel
      LOGICAL reselcall
      COMMON /UPDATE3/ reselcall

C       - BRK stores index to STRBND arrays of break in non-contig reselection
      integer ncbreak
      COMMON /BRK/ ncbreak

C - LOCAL VARIABLES
      integer numdel1, numdel2, numrepl1, numrepl2
      integer numdiff1, numdiff2, i, j
      integer shuffidx, tmp1, tmp2, nodetype, tempnum, cbound
      integer strt1, end1, strt2, end2, updt1, updt2, tmploc
      REAL ncratio, tmpdpth

!------------------START ROUTINE--------------------------------------

C       - organize indices
C       - 1st node in boundary
      strt1 = straddidx(1)
C       - lower indexed endpoint
      end1 = indxs(1)
C       - higher indexed endpoint
      strt2 = indxs(2)
C       - last point in boundary
      end2 = straddidx(2)

C       - determine boundary section # where changes are to occur
      tempnum = 0
      i = 0

        pTStHISbND (tOTbNDYS + 1) = tOTiNTpTS

      DO WHILE ( i .lt. TotBndys + 1 )
        i = i + 1
        tempnum = tempnum + PtsThisBnd(i)
        IF ( indxs(1) .le. tempnum ) THEN
          cbound = i
C           - now end the loop
          i = 9999
        ENDIF
      END DO
C         - ( i > TotBndys )

C       - numdel1 = # existing nodes to remove between strt1 & end1
      numdel1 =  end1 - strt1 + 1

C       - numdel2 = # existing nodes to remove between strt2 & end2
      numdel2 =  end2 - strt2 + 1

      IF ( .NOT. reselcall ) THEN
C         - estimate break, get a ratio
        ncratio = FLOAT(numdel1) / FLOAT(numdel1 + numdel2)
C         - use ratio to divide straight line into halves
        ncbreak = INT ( ncratio * numnodes )
      ENDIF
C         - ( NOT reselcall )

C       - numrepl2 = # new nodes to insert between strt2 & end2
      numrepl2 = numnodes - ncbreak

C       - numrepl1 = # new nodes to insert between strt1 & end1
      numrepl1 = ncbreak

C       - insert nodes from strt2 to end2
      IF ( numdel2 .ge. numrepl2 ) THEN
C         - room between strt2 & end2 for numrepl2 nodes, shuffle down
        numdiff2 = numdel2 - numrepl2
C         - insert new nodes
        j = numnodes
        tmp1 = strt2
        tmp2 = strt2 + numrepl2 - 1
        DO i = tmp1, tmp2
          dxray(i) = strx(j)
          dyray(i) = stry(j)
          j = j - 1
        END DO
C           - ( i = tmp1, tmp2 )
C         - how many spots are left over ( numdiff2 ) ?
        IF ( numdiff2 .gt. 0 ) THEN
C           - shuffle those spots away
          shuffidx = strt2 + numrepl2
          DO WHILE ( (shuffidx + numdiff2 ) .le. TotCoords)
            dxray(shuffidx) = dxray(shuffidx + numdiff2)
            dyray(shuffidx) = dyray(shuffidx + numdiff2)
            depth(shuffidx) = depth(shuffidx + numdiff2)
            shuffidx = shuffidx + 1
          END DO
          TotCoords = TotCoords - numdiff2

            IF ( CBOUND.EQ.tOTbNDYS+1 ) pTStHISbND(CBOUND) = tOTiNTpTS
          PtsThisBnd(cbound) = PtsThisBnd(cbound) - numdiff2
            IF ( CBOUND.EQ.tOTbNDYS+1 ) tOTiNTpTS = pTStHISbND(CBOUND)

cccccccccccccc
          straddidx(2) = straddidx(2) - numdiff2
cccccccccccccc
        ENDIF
C           - ( numdiff2 > 0 )
      ELSE
C         - not enough room between strt2 & end2 for numrepl2 nodes, shuffle up
        numdiff2 = numrepl2 - numdel2
C         - make room for new nodes by shuffling up
        shuffidx = TotCoords + numdiff2
        DO WHILE ( (shuffidx - numdiff2 ) .ge. end2 )
          dxray(shuffidx) = dxray(shuffidx - numdiff2)
          dyray(shuffidx) = dyray(shuffidx - numdiff2)
          depth(shuffidx) = depth(shuffidx - numdiff2)
          shuffidx = shuffidx - 1
        END DO
C         - insert new nodes
        j = numnodes
        tmp1 = strt2
        tmp2 = strt2 + numrepl2 - 1
        DO i = tmp1, tmp2
          dxray(i) = strx(j)
          dyray(i) = stry(j)
          j = j - 1
        END DO
C           - ( i = tmp1, tmp2 )
        TotCoords = TotCoords + numdiff2

          IF ( CBOUND.EQ.tOTbNDYS+1 ) pTStHISbND(CBOUND) = tOTiNTpTS
        PtsThisBnd(cbound) = PtsThisBnd(cbound) + numdiff2
          IF ( CBOUND.EQ.tOTbNDYS+1 ) tOTiNTpTS = pTStHISbND(CBOUND)

cccccccccccccc
          straddidx(2) = straddidx(2) + numdiff2
cccccccccccccc
      ENDIF
C         - ( numdel2 >= numrepl2 )

C       - set indices for call to DepthUpdate
C       - updt2: 1st node of section from strt2 -> end2
      updt2 = strt2
C       - straddidx(2): last node of section from strt2 -> end2
ccccc   straddidx(2) = tmp2

C       - insert nodes from strt1 to end1
      IF ( numdel1 .ge. numrepl1 ) THEN
C         - room between strt1 & end1 for new nodes, shuffle down
        numdiff1 = numdel1 - numrepl1
C         - insert new nodes
        j = ncbreak
        tmp1 = strt1
        tmp2 = strt1 + numrepl1 - 1
        DO i = tmp1, tmp2
          dxray(i) = strx(j)
          dyray(i) = stry(j)
          j = j - 1
        END DO
C           - ( i = tmp1, tmp2 )
C         - how many spots are left over ( numdiff1 ) ?
        IF ( numdiff1 .gt. 0 ) THEN
C           - shuffle those spots away
            shuffidx = strt1 + numrepl1
            DO WHILE ( (shuffidx + numdiff1 ) .le. TotCoords)
            dxray(shuffidx) = dxray(shuffidx + numdiff1)
            dyray(shuffidx) = dyray(shuffidx + numdiff1)
            depth(shuffidx) = depth(shuffidx + numdiff1)
            shuffidx = shuffidx + 1
            END DO
            TotCoords = TotCoords - numdiff1

              IF ( CBOUND.EQ.tOTbNDYS+1 ) pTStHISbND(CBOUND) = tOTiNTpTS
            PtsThisBnd(cbound) = PtsThisBnd(cbound) - numdiff1
              IF ( CBOUND.EQ.tOTbNDYS+1 ) tOTiNTpTS = pTStHISbND(CBOUND)

C             - update updt2 & straddidx(2) after indices have been altered
            updt2 = updt2 - numdiff1
            straddidx(2) = straddidx(2) - numdiff1
        ENDIF
C           - ( numdiff1 > 0 )
      ELSE
C         - not enough room between strt1 & end1 for new nodes, shuffle up
        numdiff1 = numrepl1 - numdel1
C         - make room for new nodes by shuffling up
        shuffidx = TotCoords + numdiff1
        DO WHILE ( (shuffidx - numdiff1 ) .ge. indxs(1) )
          dxray(shuffidx) = dxray(shuffidx - numdiff1)
          dyray(shuffidx) = dyray(shuffidx - numdiff1)
          depth(shuffidx) = depth(shuffidx - numdiff1)
          shuffidx = shuffidx - 1
        END DO
C         - insert new nodes
        j = ncbreak
        tmp1 = strt1
        tmp2 = strt1 + numrepl1 - 1
        DO i = tmp1, tmp2
          dxray(i) = strx(j)
          dyray(i) = stry(j)
          j = j - 1
        END DO
C           - ( i = tmp1, tmp2 )
        TotCoords = TotCoords + numdiff1

          IF ( CBOUND.EQ.tOTbNDYS+1 ) pTStHISbND(CBOUND) = tOTiNTpTS
        PtsThisBnd(cbound) = PtsThisBnd(cbound) + numdiff1
          IF ( CBOUND.EQ.tOTbNDYS+1 ) tOTiNTpTS = pTStHISbND(CBOUND)

C         - update updt2 & straddidx(2) after indices have been altered
        updt2 = updt2 + numdiff1
        straddidx(2) = straddidx(2) + numdiff1
      ENDIF
C         - ( numdel1 >= numrepl1 )

C       - set indices for call to DepthUpdate
C       - straddidx(1) matches to dpth1, updt2 matches to dpth2
C       - updt1: last node of section from strt1 -> end1
      updt1 = tmp2
C       - straddidx(1): 1st node of section from strt1 -> end1
      straddidx(1) = strt1

C       - nodetype = boundary nodes
      nodetype = 2
C       - updt1 matches to dpth1, updt2 matches to dpth2
C       - make updt1 smallest of updt1 & updt2, keeping matching depths
      IF ( updt1 .gt. updt2 ) THEN
        tmploc = updt1
        updt1 = updt2
        updt2 = tmploc
        tmpdpth = dpth1
        dpth1 = dpth2
        dpth2 = tmpdpth
      ENDIF
C         - ( updt1 > updt2 )
C       - 2nd argument to DepthUpdate 'types' calling routine
        IF ( reselcall ) THEN
C         - called by SaveReSel ( 5 )
! bug in updt1 when node number 2 is chosen. Index ends up 0
!         call DepthUpdate ( nodetype, 5, updt1, updt2 )
        ELSE
C         - called by StraightBnd ( 3 )
!         call DepthUpdate ( nodetype, 3, updt1, updt2 )
        ENDIF
C         - ( reselcall )

      RETURN
      END

!-----------------------------------------------------------------------*

      SUBROUTINE StringNodes ( lnx, lny, success )

C PURPOSE: To insert a specified number of nodes along a straight line.
C   INPUT: Interactive input from user;
C          numnodes = # of nodes to insert along the straight line.
C   GIVEN: lnx = 2 element array of start and end X coordinates of
C          straight line section.
C          lny = 2 element array of start and end Y coordinates of
C          straight line section.
C RETURNS: success = TRUE if user specifies number of nodes to insert
C                    and does not abort, else FALSE.
!-----------------------------------------------------------------------*

      use MainArrays, only : maxstrnodes

C - "INCLUDES"
      include '../includes/defaults.inc'

C - PASSED VARIABLES
      REAL lnx(2), lny(2)
      LOGICAL success

C - COMMON BLOCKS
C       - STRBND stores straight boundary values for updating, requires
C         parameter maxstrnodes to be set.
        integer numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C - LOCAL VARIABLES
        integer i
      REAL nratio, tmpval, xdiff, ydiff, tmpreal
      CHARACTER*1 PigCursYesNo
      CHARACTER*80 ch_ans, ans, cstr
      LOGICAL left, down, donestr, convOK

!*-----------------START ROUTINE--------------------------------------

!       - draw a straight line between the two end nodes
      call PigDrawPolyline ( 2, lnx, lny )
      success = .FALSE.
      donestr = .FALSE.
      DO WHILE ( .NOT. donestr )
        cstr='How many NEW nodes nodes (Q - to abort)?'
        call PigPrompt ( cstr, ch_ans )
        call PigEraseMessage
        IF ( (ch_ans(1:1) .eq. 'Q') .OR. (ch_ans(1:1) .eq. 'q') ) THEN
!         - aborting
          success = .FALSE.
          donestr = .TRUE.
        ELSE
!        - not aborting
          call PigReadReal ( ch_ans, tmpreal, convOK )
          numnodes = INT ( tmpreal )
!        - next line added agd 95jan23 for new question above
          numnodes = numnodes + 2
          if (numnodes.lt.2) then
            call PigPutMessage('Must select positive number of nodes')
            success = .FALSE.
            donestr = .TRUE.
          else
!         - determine X direction of line
            IF ( lnx(2) .lt. lnx(1) ) THEN
              left = .TRUE.
              xdiff = lnx(1) - lnx(2)
            ELSE
              left = .FALSE.
              xdiff = lnx(2) - lnx(1)
            ENDIF
!           - ( lnx(2) < lnx(1) )
!         - determine Y direction of line
            IF ( lny(2) .lt. lny(1) ) THEN
              down = .TRUE.
              ydiff = lny(1) - lny(2)
            ELSE
              down = .FALSE.
              ydiff = lny(2) - lny(1)
            ENDIF
!           - ( lny(2) > lny(1) )
            tmpval = FLOAT ( numnodes )
            nratio = 1.0 / ( tmpval - 1.0 )
            strx(1) = lnx(1)
            stry(1) = lny(1)
            strx(numnodes) = lnx(2) 
            stry(numnodes) = lny(2)
            DO i = 2, numnodes-1
              IF ( left ) THEN
                strx(i) = lnx(1) - ( (i-1) * nratio * xdiff )
              ELSE
                strx(i) = lnx(1) + ( (i-1) * nratio * xdiff )
              ENDIF
!             - ( left )
              IF ( down ) THEN
                stry(i) = lny(1) - ( (i-1) * nratio * ydiff )
              ELSE
                stry(i) = lny(1) + ( (i-1) * nratio * ydiff )
              ENDIF
!             - ( down )
            END DO
!           - ( i = 2, numnodes-1 )
            do i=1,numnodes
              call PigDrawModifySymbol (strx(i), stry(i))
            end do

            cstr = 'Acceptable node density ?:'
            ans = PigCursYesNo (cstr)
            call PigEraseMessage
            IF ( ans(1:1) .eq. 'Y' ) THEN
!           - acceptable density
              success = .TRUE.
              donestr = .TRUE.
            ELSE
!           - not acceptable density
              success = .FALSE.
              donestr = .FALSE.
!           - remove displayed nodes
              do i=1,numnodes
                call PigEraseModifySymbol (strx(i), stry(i))
              end do
            ENDIF
!           - ( ans = Y )
          endif
        ENDIF
!       - ( ch_ans = Q ) - if quitting
      END DO
!       - ( NOT donestr )

!       - remove line between delimiting nodes
      call PigDrawline ( 2, lnx, lny, 2 )

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE StraightInt(nrec,MouseX,MouseY,FirstPoint,NextPoint)

C PURPOSE: To add a straight line of interior nodes.
C   INPUT: Interactive input from user
C   GIVEN: None.
C RETURNS: In Common NODES;
C               dxray(), dyray(), depth(), TotCoords, PtsThisBnd() =
C                 updated to reflect changes, if any.
C EFFECTS: User specifies the start and end of a  section, then a straight
C          line of 'n' nodes is added between the two nodes specified. User
C          picks the integer value for 'n' in StringNodes.
!-----------------------------------------------------------------------*

      use MainArrays

! *** passed variables
      integer nrec
      real MouseX, MouseY
      logical FirstPoint, NextPoint

! - COMMON BLOCKS

!       - STRBND stores straight boundary values for updating, requires
!         parameter maxstrnodes to be set.
      integer numnodes
      REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

!       - AUTODPTH stores depths of two delimiting nodes of straight boundary
!       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

! - LOCAL VARIABLES
      REAL lnx(2), lny(2)
      integer ierr, ndx, nodetype
      integer, save :: indxs(2)
      LOGICAL success

!------------------START ROUTINE----------------------------------------

!       - get 2 delimiting nodes to mark section
      if(FirstPoint) then
        ierr = 0
        nodetype = 1
        call CheckNode ( MouseX, MouseY, ndx, ierr, nodetype )
        IF ( ierr .eq. 1 ) THEN
!               - valid node chosen, save it & put marker
          indxs(1) = ndx
          call PigDrawCoinSymbol( MouseX, MouseY)
          FirstPoint = .false.
          NextPoint = .true.
          return
        else
          return
        endif
      elseif(NextPoint) then
        ierr = 0
        nodetype = 1
        call CheckNode ( MouseX, MouseY, ndx, ierr, nodetype )
        IF ( (ierr .eq. 1).and.(ndx .ne. indxs(1)) ) THEN
!               - valid node chosen, save it & put marker
          indxs(2) = ndx
          call PigDrawCoinSymbol( MouseX, MouseY)
          FirstPoint = .false.
          NextPoint = .false.
        else
          return
        endif
      else
        return
      endif

      IF ( indxs(1) .gt. indxs(2) ) THEN
        ndx = indxs(1)
        indxs(1) = indxs(2)
        indxs(2) = ndx
      ENDIF
!         - nodes are valid, 
!         - store depths of delimiting vertices in case depth by interpolation
      dpth1 = depth ( indxs(1) )
      dpth2 = depth ( indxs(2) )
      lnx(1) = dxray ( indxs(1) )
      lny(1) = dyray ( indxs(1) ) 
      lnx(2) = dxray ( indxs(2) )
      lny(2) = dyray ( indxs(2) ) 

      success = .FALSE.
      call StringNodes ( lnx, lny, success )
      IF ( success ) THEN
        call StrIntUpdate ( indxs )
      ENDIF

      nrec = TotCoords + 1

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE StrIntUpdate ( indxs )

! PURPOSE: To Update data arrays after a successful call to StraightInt.
!   GIVEN: indxs() = 2 element array containing delimiting indices of
!                    nodes in dxray(), dyray(), depth() to remove.
!          in Common /STRBND/;
!               numnodes = # nodes to insert, starting at indxs(1).
!               strx() = array of X coordinates of nodes to insert.
!               stry() = array of Y coordinates of nodes to insert.
! RETURNS: None.
! EFFECTS: Data arrays dxray(), dyray(), depth() are updated.
!-----------------------------------------------------------------------*

      use MainArrays

! - PARAMETERS (constants)
      real minallowdpth, maxallowdpth
      parameter ( minallowdpth=-1.e20, maxallowdpth=1.e20 )

! - PASSED VARIABLES
      integer indxs(2)

! - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

! - COMMON BLOCKS
!       - STRBND stores straight boundary values for updating
      integer numnodes
      REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

!       - AUTODPTH stores depths of two delimiting nodes of straight boundary
!       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2


! - LOCAL VARIABLES
      integer i, j
      integer tmp1, tmp2
      real dist, dist1, depdiff, xtemp, ytemp

!------------------START ROUTINE--------------------------------------


!         - insert new nodes and interpolate depth

      tmp1 = indxs(1)
      tmp2 = indxs(2)
      dpth1 = depth(tmp1)
      dpth2 = depth(tmp2)
!  roll up interior nodes by numnodes
      DO i = 1, TotIntPts !need limit by TotIntPts
!        dxray(TotCoords+numnodes+1-i) = dxray(TotCoords+1-i)
!        dyray(TotCoords+numnodes+1-i) = dyray(TotCoords+1-i)
!        depth(TotCoords+numnodes+1-i) = depth(TotCoords+1-i)
!        exist(TotCoords+numnodes+1-i) = .true.
      END DO

!  insert new nodes as line constraint
      j = numnodes
      dist = sqrt((strx(j)-strx(1))**2+(stry(j)-stry(1))**2)
      depdiff = dpth2 - dpth1
      j = 2
      tmp1 = TotCoords + 1
      tmp2 = TotCoords + numnodes - 2
      DO i = tmp1, tmp2
        dxray(i) = strx(j)
        dyray(i) = stry(j)
        dist1 = sqrt((strx(j)-strx(1))**2+(stry(j)-stry(1))**2)
        depth(i) = dpth1 + depdiff*dist1/dist
        !exist(i) = .true.
        j = j + 1
      END DO
!           - ( i = tmp1, tmp2 )
!           - delete 2 delimiting markers & redraw nodes they marked
      DO i = 1, numnodes, numnodes-1
        xtemp = strx(i)
        ytemp = stry(i)
        call PigSetSymbolNumber ( PendMType )
        call RedrawMark( xtemp, ytemp, backgr )
        call PigSetSymbolNumber ( NodeMType )
      END DO
!             - ( i = 1, 2 )
      TotIntPts = TotIntPts + numnodes - 2
      TotCoords = TotCoords + numnodes - 2
      itot = TotCoords

!   Clean up
      call PigSetSymbolColour ( ModifColor )
      do i=1,numnodes
        call PigDrawSymbols ( 1, strx(i), stry(i))
      end do
!             - ( i = 1, numnodes )

      RETURN
      END

!-----------------------------------------------------------------------*
!----------------------END STRAIGHT.FOR---------------------------------*


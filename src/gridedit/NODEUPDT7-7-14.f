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

!----------------------------------------------------------------------*
!                       NODEUPDT.FOR                                    *
!       This module contains procedures for updating data arrays        *
!       after node editing operations.                                  *
!       ROUTINES - DelUpdate, MoveUpdate, AddUpdate, DepthUpdate,       *
!                  AutoDpthMove, AutoDpthAdd, DelIslUpdate.             *
!*----------------------------------------------------------------------*
!*----------------------------------------------------------------------*

      SUBROUTINE  DepthUpdate ( nodetype, optype, loc1, loc2 )

! PURPOSE: To update depth() at loc1, or at all positions from
!          loc1 to loc2, or from straddidx(1) to loc1 and from loc2 to
!          straddidx(2).
!   GIVEN: nodetype = type of node whose depth is to be updated.
!          loc1 = index to depth() of node whose depth to update, or
!                 1st index in range of nodes whose depths to update.
!          loc2 = end of range of indices to depth(), if nodes from
!                 loc1 to loc2 are to have depths updated.
!          optype = type of operation performed which made the call to 
!                   DepthUpdate; 1 = Move, 2 = Add, 3 = StraightBnd,
!                                4 = ChangeNodeType, 5 = SaveReSel
!          In Common STRADDLE;
!               straddidx(1) & (2) = both 0 (zero) if contiguous half to be 
!                                    updated, else two extra indices for 
!                                    non-contig boundary half.
! RETURNS: None.
! EFFECTS: If optype = 3 or 5, depth(loc1) to depth(loc2) are updated,
!          else depth(loc1) is updated
!*----------------------------------------------------------------------*

      use MainArrays

! - PASSED VARIABLES
      integer nodetype, loc1, loc2, optype

! - PARAMETERS (constants)
      REAL maxallowdpth, minallowdpth
      REAL*4 seconds
      PARAMETER ( seconds = 2.0 )
      PARAMETER ( maxallowdpth = 10.0 E20 )
      PARAMETER ( minallowdpth = -10.0 E20 )

! - "INCLUDES"
      include '../includes/defaults.inc'
      include '../includes/graf.def'

! - COMMON BLOCKS
!       - FDEP stores depth from DIGIT file for SaveReSel
      REAL filedepth
      logical deptype
      COMMON /FDEP/ filedepth, deptype

!       - STRADDLE stores 2 extra indices for boundary half that straddles
!       - 1st & last nodes of non-contiguous boundary half.
      integer straddidx(2)
      COMMON /STRADDLE/ straddidx

! - LOCAL VARIABLES
      CHARACTER*80 cstr, ch_dpth, ans
      CHARACTER*1 tmpchar
!      character*4 optsarray(4)
      REAL real_depth, xreal, yreal, dfltdepth
      integer ii, tmpint1, tmpint2
      LOGICAL auto, convOK, updtdone

C*----------------START ROUTINE-------------------------------------

      PendMType = 4
      updtdone = .FALSE.
      DO WHILE ( .NOT. updtdone )
        IF ( (optype .eq. 1) .OR. (optype .eq. 2) ) THEN
!           - called by "move" or "add", single node to update
          auto = .FALSE.
          IF ( nodetype .eq. 2 ) THEN
!             - only boundary nodes have 'interpolate' option for add & move
!             -- prompt for choice of auto or manual depth
!            cstr = 'Interpolate DEPTH ?'
!            ans = PigCursYesNo (cstr)
            ans(1:1) = 'Y'
            IF ( ans(1:1) .eq. 'Y'.or.ans(1:1) .eq. 'I' ) THEN
              auto = .TRUE.
            ELSE
              auto = .FALSE.
            ENDIF
!               - ( ans = I )
          ENDIF
!             - ( nodetype = 2 )
          IF ( .NOT. auto ) THEN
C             - manual entry, get depth
            convOK = .FALSE.
            DO WHILE ( .NOT. convOK )
C               - set default depth & prompt string
              IF ( optype .eq. 1 ) THEN
C                 - default depth for move is old depth
                dfltdepth = depth(loc1)
                cstr = 'Enter DEPTH at this node' //
     +                   ' (<RTN> = old depth):'
              ELSE
C                 - default depth for add is 0.0
                dfltdepth = 0.0
                cstr = 'Enter DEPTH at this node (<RTN> = 0.0):'
              ENDIF
C                 - ( optype = 1 )
              call PigPrompt ( cstr, ch_dpth )
              call PigReadReal ( ch_dpth, real_depth, convOK )
              IF ( (real_depth .ge. minallowdpth) .AND.
     +               (real_depth .le. maxallowdpth) .AND.
     +               (convOK) ) THEN
                IF ( ch_dpth .eq. ' ' ) THEN
!                   - <RTN> only entered, depth = default
                  depth(loc1) = dfltdepth
                ELSE
                  depth(loc1) = real_depth
                ENDIF
!                   - ( ch_dpth = ' ' )
                updtdone = .TRUE.
                call PigEraseMessage
              ELSE
                cstr = 'INVALID DEPTH, try again.'
                call PigMessageOK ( cstr, ' ' )
!                call PigUwait ( seconds )
!                call PigEraseMessage
                convOK = .FALSE.
              ENDIF
C               - ( max < real_depth > min )
            END DO
C               - ( NOT convOK )
          ELSE
C             - automatic depth evaluation
            call AutoDpthInterp ( nodetype, loc1 )
            updtdone = .TRUE.
          ENDIF
C             - ( NOT auto )
        ELSE IF ( (optype .eq. 3) .OR. (optype .eq. 5) ) THEN
C           - called by "StraightBnd" or "SaveReSel"
C           - prompt for auto or manual depths
          ans(1:1) = 'z'
          IF ( optype .eq. 3 ) THEN
            cstr = 'Assign DEPTH  <M>anually  or <I>nterpolate ?:'
c             cstr = 'Assign DEPTHS Manually or Interpolate (M/I)?'
            tmpchar(1:1) = 'Manually'
          ELSE
C             - optype = 5
            cstr = 'Assign DEPTHS from <F>ile or <I>nterpolate ?:'
            cstr = 'Assign DEPTHS from File or Interpolate (F/I)?'
            tmpchar(1:1) = 'File'
          ENDIF
C             - ( optype = 3 )
          ans(1:1) = 'z'
 !         optsarray(2) = 'Interpolate'
            cstr = 'Interpolate DEPTH ?'
            ans = 'Y' !PigCursYesNo (cstr)
          IF ( ans(1:1) .eq. 'Y'.or.ans(1:1) .eq. 'I' ) THEN
            auto = .TRUE.
          ELSE
            auto = .FALSE.
          ENDIF
C             - ( ans = I )
          IF ( (.NOT. auto) .AND. ( optype .eq. 3) ) THEN
C             - optype = 3, manual depth entry, default = 0.0 for StraightBnd
            dfltdepth = 0.0
            IF ( (straddidx(1) .eq. 0) .AND. 
     +             (straddidx(2) .eq. 0) ) THEN
C               - contig half
C               -- get depths for section from loc1 -> loc2
            tmpint1 = loc1
            tmpint2 = loc2
            ELSE
C               - non-contig half
C               -- get depths for section from loc2 -> straddidx(2)
            tmpint1 = loc2
            tmpint2 = straddidx(2)
            ENDIF
C               - ( straddidx(1) & (2) = 0 )
            DO ii = tmpint1, tmpint2
C               - highlite node whose depth to update
            xreal = dxray(ii)
            yreal = dyray(ii)
            call PigSetSymbolNumber ( PendMType )
            call RedrawMark( xreal, yreal, white )
            call PigSetSymbolNumber ( NodeMType )
C               - get user to enter a depth
            convOK = .FALSE.
            DO WHILE ( .NOT. convOK )
              cstr = 'Enter DEPTH at this node (<RTN> = 0.0):'
              call PigPrompt ( cstr, ch_dpth )
              call PigReadReal ( ch_dpth, real_depth, convOK )
C                 - validate depth
              IF ( (real_depth .ge. minallowdpth) .AND.
     +                 (real_depth .le. maxallowdpth) .AND.
     +                 (convOK) ) THEN
C                   - valid depth entered, insert new depth
                IF ( ch_dpth .eq. ' ' ) THEN
C                     - <RTN> only entered, depth = default
                  depth(ii) = dfltdepth
                ELSE
                  depth(ii) = real_depth
                ENDIF
C                     - ( ch_dpth = ' ' )
                  ELSE
C                   - invalid depth entered
                    cstr = 'INVALID DEPTH, try again.'
                    call PigMessageOK ( cstr, ' ' )
!                   call PigUwait ( seconds )
!                   call PigEraseMessage
                    convOK = .FALSE.
                  ENDIF
C                   - ( max < real_depth > min )
            END DO 
C                 - ( NOT convOK )
C               - un-highlite node, redraw original node
            call PigSetSymbolNumber ( PendMType )
            call RedrawMark( xreal, yreal, backgr )
            call PigSetSymbolNumber ( NodeMType )
            call RedrawMark( xreal, yreal, ModifColor )
            END DO
C               - ( i = tmpint1, tmpint2 )
            IF ( (straddidx(1) .ne. 0) .AND. 
     +             (straddidx(2) .ne. 0) ) THEN
C               - non-contig half
C               -- get depths for section from straddidx(1) -> loc1
            tmpint1 = straddidx(1)
            tmpint2 = loc1
            DO ii = tmpint1, tmpint2
C                 - highlite node whose depth to update
              xreal = dxray(ii)
              yreal = dyray(ii)
              call PigSetSymbolNumber ( PendMType )
              call RedrawMark( xreal, yreal, white )
              call PigSetSymbolNumber ( NodeMType )
C                 - get user to enter a depth
              convOK = .FALSE.
              DO WHILE ( .NOT. convOK )
                cstr = 'Enter DEPTH at this node (<RTN> = 0.0):'
                call PigPrompt ( cstr, ch_dpth )
                call PigReadReal ( ch_dpth, real_depth, convOK )
C                   - validate depth
                IF ( (real_depth .ge. minallowdpth) .AND.
     +                   (real_depth .le. maxallowdpth) .AND.
     +                   (convOK) ) THEN
C                     - valid depth entered, insert new depth
                  IF ( ch_dpth .eq. ' ' ) THEN
C                       - <RTN> only entered, depth = default
                  depth(ii) = dfltdepth
                  ELSE
                  depth(ii) = real_depth
                  ENDIF
C                       - ( ch_dpth = ' ' )
                    ELSE
C                     - invalid depth entered
                      cstr = 'INVALID DEPTH, try again.'
                      call PigMessageOK ( cstr, ' ' )
!                     call PigUwait ( seconds )
!                     call PigEraseMessage
                      convOK = .FALSE.
                    ENDIF
C                     - ( max < real_depth > min )
              END DO 
C                   - ( NOT convOK )
C                 - un-highlite node, redraw original node
              call PigSetSymbolNumber ( PendMType )
              call RedrawMark( xreal, yreal, backgr )
              call PigSetSymbolNumber ( NodeMType )
              call RedrawMark( xreal, yreal, ModifColor)
            END DO
C                 - ( i = tmpint1, tmpint2 )
            ENDIF
C               - ( straddidx(1) & (2) ne 0 )
            updtdone = .TRUE.
          ELSE IF ( (.NOT. auto) .AND. (optype .eq. 5) ) THEN
C             - assign depths from file for SaveReSel
            IF ( (straddidx(1) .eq. 0) .AND. 
     +             (straddidx(2) .eq. 0) ) THEN
C               - contig half
C               -- assign depths from loc1 -> loc2
            tmpint1 = loc1
            tmpint2 = loc2
            ELSE
C               - non-contig half
C               -- assign depths from loc2 -> straddidx(2)
            tmpint1 = loc2
            tmpint2 = straddidx(2)
            DO ii = tmpint1, tmpint2
                  if(deptype) depth(ii) = filedepth
            END DO
C                 - ( ii = tmpint1, tmpint2 )
C               -- then from straddidx(1) -> loc1
            tmpint1 = straddidx(1)
            tmpint2 = loc1
            ENDIF
C               - ( straddidx(1) & (2) = 0 )
            DO ii = tmpint1, tmpint2
            if(deptype) depth(ii) = filedepth
            END DO
C               - ( ii = tmpint1, tmpint2 )
            updtdone = .TRUE.
          ELSE
C             - auto depth evaluation for StraightBnd or SaveReSel
            IF ( (straddidx(1) .eq. 0) .AND. 
     +             (straddidx(2) .eq. 0) ) THEN
C               - contig
cc following line had last parameter as TRUE, should be 1
            call InterpDepths ( loc1, loc2, 1 )
            ELSE
C               - non-contig
cc following line had last parameter as FALSE, should be 2
            call InterpDepths ( loc1, loc2, 2 )
            ENDIF
C               - ( straddidx(1) & (2) = 0 )
            updtdone = .TRUE.
          ENDIF
C             - ( NOT auto )
        ELSE IF ( optype .eq. 4 ) THEN
C           - called by ChangeNodeType
          auto = .FALSE.
          IF ( nodetype .eq. 2 ) THEN
C             -  prompt for auto or manual depths
            ans(1:1) = 'z'
            cstr = 'Assign DEPTHS <M>anually or <I>nterpolate ?:'
!            optsarray(1) = 'Manually'
!            optsarray(2) = 'Interpolate'
            ans = "I" 
            IF ( ans(1:1) .eq. 'I' ) THEN
            auto = .TRUE.
            ELSE
            auto = .FALSE.
            ENDIF
C               - ( ans = I )
          ENDIF
C             - ( nopdetype = 2 )
          IF ( .NOT. auto ) THEN
C             - manual entry, default depth = old depth for ChangeNodeType
            dfltdepth = depth(loc1)
C             - get depth
            convOK = .FALSE.
            DO WHILE ( .NOT. convOK )
            cstr = 'Enter DEPTH at this node (<RTN> = old depth):'
            call PigPrompt ( cstr, ch_dpth )
            call PigReadReal ( ch_dpth, real_depth, convOK )
            IF ( (real_depth .ge. minallowdpth) .AND.
     +               (real_depth .le. maxallowdpth) .AND.
     +               (convOK) ) THEN
              IF ( ch_dpth .eq. ' ' ) THEN
C                   - <RTN> only entered, depth = default
                depth(loc1) = dfltdepth
              ELSE
                depth(loc1) = real_depth
              ENDIF
C                   - ( ch_dpth = ' ' )
              updtdone = .TRUE.
                  call PigEraseMessage
                ELSE
                  cstr = 'INVALID DEPTH, try again.'
                  call PigMessageOK ( cstr, ' ' )
!                 call PigUwait ( seconds )
!                 call PigEraseMessage
                  convOK = .FALSE.
                ENDIF
C               - ( max < real_depth > min )
            END DO
C               - ( NOT convOK )
          ELSE
C             - automatic depth evaluation
            call AutoDpthInterp ( nodetype, loc1 )
            updtdone = .TRUE.
          ENDIF
C             - ( NOT auto )
        ENDIF
C           - ( optype = 1 OR 2 )
      END DO
C         - ( NOT updtdone )
      call PigEraseMessage

      RETURN
      END

C*----------------------------------------------------------------------*

      SUBROUTINE AutoDpthInterp ( nodetype, loc )

C PURPOSE: To automatically assign a depth to a moved node.
C   GIVEN: nodetype = type (interior or boundary) of node to assign depth to.
C          loc = index in depth() of node to assign depth to.
C RETURNS: depth(loc) = depth interpolated as below.
C EFFECTS: Node depth(loc) is automatically assigned a depth. Depth is
C          calculated by:
C       r1 = SQRT ( ((x1-x0) * (x1-x0)) + ((y1-y0) * (y1-y0)) )
C       r2 = SQRT ( ((x2-x0) * (x2-x0)) + ((y2-y0) * (y2-y0)) )
C       d0 = ( (r2*d1) + (r1*d2) ) / ( r1 + r2 )
C       where: (x1,y1) = coordinates of 1st neighbour of node at loc.
C              (x2,y2) = coordinates of 2nd neighbour of node at loc.
C              (x0,y0) = coordinates of node at loc.
C              r1 = distance between loc and 1st neighbour.
C              r2 = distance between loc and 2nd neighbour.
C              d1 = depth of 1st neighbour of node at loc.
C              d2 = depth of 2nd neighbour of node at loc.
C              d0 = interpolated depth for node at loc
C*----------------------------------------------------------------------*

      use MainArrays

C - PASSED VARIABLES
      integer nodetype, loc

C - LOCAL VARIABLES
      integer nbr1, nbr2, tempnum, i, strtbnd, endbnd, bound
      REAL d0, d1, d2, r1, r2, x1, y1, x2, y2, x0, y0

C*----------------------------------------------------------------------*

      IF ( nodetype .eq. 2 ) THEN
C       - determine boundary where loc is located (bound)
      tempnum = 0
      i = 0

        pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

      DO WHILE ( i .lt. TotBndys + 1 )
        i = i + 1
        tempnum = tempnum + PtsThisBnd(i)
        IF ( loc .le. tempnum ) THEN
          bound = i
C           - now end the loop
          i = 9999
        ENDIF
      END DO
C         - ( i > TotBndys + 1 )

C       - determine start & end indices of boundary (strtbnd, endbnd)

        pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

      IF ( bound .eq. 1 ) THEN
        strtbnd = 1
        endbnd = PtsThisBnd(bound)
      ELSE
        strtbnd = 0
        DO i = 1, bound - 1
          strtbnd = strtbnd + PtsThisBnd(i)
        END DO
        strtbnd = strtbnd + 1
        endbnd = strtbnd + PtsThisBnd(bound)
      ENDIF
C         - ( bound = 1 )

C       - determine neighbour indices to node being updated (nbr1, nbr2)
      IF ( loc .eq. strtbnd ) THEN
C         - node is 1st node in boundary
        nbr1 = endbnd
        nbr2 = loc + 1
      ELSE IF ( loc .eq. endbnd ) THEN
C         - node is last node in boundary
        nbr1 = loc - 1
        nbr2 = strtbnd
      ELSE
C         - just a node along the way
        nbr1 = loc - 1
        nbr2 = loc + 1
      ENDIF

C       - determine depths at neighbours (d1, d2)
      d1 = depth(nbr1)
      d2 = depth(nbr2)
      d0 = 0.0

C       - determine distances between loc & each neighbour (r1, r2)
      x0 = dxray(loc)
      y0 = dyray(loc)
      x1 = dxray(nbr1)
      y1 = dyray(nbr1)
      x2 = dxray(nbr2)
      y2 = dyray(nbr2)
      r1 = SQRT ( ((x1-x0) * (x1-x0)) + ((y1-y0) * (y1-y0)) )
      r2 = SQRT ( ((x2-x0) * (x2-x0)) + ((y2-y0) * (y2-y0)) )
      d0 = ( (r2*d1) + (r1*d2) ) / ( r1 + r2 )

C       - assign interpolated depth
      depth(loc) = d0
      ENDIF
C       - ( nodetype = 2 )

      RETURN
      END

C*----------------------------------------------------------------------*

      SUBROUTINE InterpDepths ( loc1, loc2, cntig )

C PURPOSE: To automatically assign depths to all nodes in depth() indexed
C          from loc1 to loc2, OR from location loc2 to straddidx(2) and 
C          from straddidx(1) to loc1. Nodes are assumed to be boundary type.
C   GIVEN: cntig = 1 if contiguous half of boundary is to have depths 
C                  interpolated,
C                = 2 if non-contiguous half of boundary is to have depths
C                  interpolated,
C          loc1 = if cntig = 1, then starting index of nodes to assign depths
C                 to, else ending index of nodes to assign depths to from 
C                 straddidx(1) to loc1,
C          loc2 = if cntig = 1, then ending index of nodes to assign depths to,
C                 else starting index of nodes to assign depths to from loc2
C                 to straddidx(2),
C          In Common STRADDLE ( valid if cntig = 2 );
C               straddidx(1) = starting index of nodes to assign depths to from
C                              straddidx(1) to loc1,
C               straddidx(2) = ending index of nodes to assign depths to from
C                              loc2 to straddidx(2).
C          In Common AUTODPTH:
C               dpth1 = orginal depth at loc1.
C               dpth2 = orginal depth at loc2.
C RETURNS: Updated depth()
C EFFECTS: Nodes in depth array depth() from straddidx(1) to loc1 and from
C          loc2 to straddidx(2) have depths automatically assigned to them. 
C          Depths are calculated by linear interpolation between loc1 & loc2.
C          Interpolation is as follow;
C               depth(loc1) = dpth1, 
C               depth(loc2) = dpth2. 
C               If dpth1 = dpth2, then all depths to be interpolated are 
C               assigned depth = depth1. If dpth1 <> dpth2, then depths 
C               are interpolated as follow;
C
C       depth(loc 'm') = { (em * depthN) + ((eN - em) * depth1) } / eN
C
C          where the stretch of boundary nodes goes from loc '1' to loc 'N',
C          m is a location (node) between 1 & N,
C          em = distance from loc '1' to loc 'm',
C          eN = distance from loc '1' to loc 'N',
C          depthN = depth at loc 'N' ( = dpth2 in Common AUTODPTH),
C          depth1 = depth at loc '1' ( = dpth1 in Common AUTODPTH).
C*----------------------------------------------------------------------*

      use MainArrays

C - PASSED VARIABLES
      INTEGER loc1, loc2
      INTEGER cntig

C - COMMON BLOCKS
C       - AUTODPTH stores depths of two delimiting nodes of straight boundary
C       -- line prior to StraightBnd operation
      REAL dpth1, dpth2
      COMMON /AUTODPTH/ dpth1, dpth2

C       - STRADDLE stores 2 extra indices for boundary half that straddles
C       - 1st & last nodes of non-contiguous boundary half.
      INTEGER straddidx(2)
      COMMON /STRADDLE/ straddidx

C       - UPDATE3 stores indication of whether StrBndUpdate or StrSpecUpdate
C       -- was called by StraightBnd or SaveReSel
      LOGICAL reselcall
      COMMON /UPDATE3/ reselcall

C       - STRBND stores arrays that will be used here for temp storage of
C       -- nodes whose depths are to be interpolated
        INTEGER numnodes
        REAL strx(maxstrnodes), stry(maxstrnodes), strz(maxstrnodes)
        COMMON /STRBND/ strx, stry, strz, numnodes 

C - LOCAL VARIABLES
        REAL tmpdpths(maxstrnodes)
      common /tmplocal/ tmpdpths
      REAL eN, em, distn
      REAL intx1, inty1, intx2, inty2
      integer numlocs1, numlocs2, totlocs, i, j
      integer tmpint1, tmpint2

C*----------------------------------------------------------------------*

C       - see if user wants to change depths at endpoints
      intx1 = dxray(loc1)
      inty1 = dyray(loc1)
      intx2 = dxray(loc2)
      inty2 = dyray(loc2)
!      call InterpInfo ( intx1, inty1, intx2, inty2 )
C       - find number of nodes that need depths assigned, incl. end points
      IF ( cntig .eq. 1 ) THEN
        numlocs1 = 0
        numlocs2 = 0
        totlocs = loc2 - loc1 + 1
      ELSE
        numlocs1 = loc1 - straddidx(1) + 1
        numlocs2 = straddidx(2) - loc2 + 1
        totlocs = numlocs1 + numlocs2
      ENDIF
C         - ( cntig = 1 )
      IF ( dpth1 .eq. dpth2 ) THEN
C         - all depths will be the same
        IF ( totlocs .ge. 2 ) THEN
C           - assign depths via do loops
          IF ( cntig .eq. 1 ) THEN
C             - update from loc1 to loc2
            tmpint1 = loc1
            tmpint2 = loc2
          ELSE
C             - update from straddidx(1) to loc1
            tmpint1 = straddidx(1)
            tmpint2 = loc1
            DO i = tmpint1, tmpint2
            depth(i) = dpth1
            END DO
C             - then from loc2 to straddidx(2)
            tmpint1 = loc2
            tmpint2 = straddidx(2)
          ENDIF
C             _ ( cntig = 1 )
          DO i = tmpint1, tmpint2
            depth(i) = dpth1
          END DO
        ELSE
C           - totlocs < 2, only 1 node to assign
          depth(loc1) = dpth1
        ENDIF
C           - ( numlocs >= 2 )
      ELSE
C         - dpth1 ne dpth2, depths must be interpolated
C         - copy nodes whose depths are to be interpolated to Common STRBND
        numnodes = 0
        IF ( cntig .eq. 1 ) THEN
C           - copy nodes from loc1 to loc2
          tmpint1 = loc1
          tmpint2 = loc2
          j = 0
          DO i = tmpint1, tmpint2
            j = j + 1
            strx(j) = dxray(i)
            stry(j) = dyray(i)
          END DO
          numnodes = j
        ELSE
C           - copy nodes backwards from loc1 to straddidx(1)
          tmpint1 = loc1
          tmpint2 = straddidx(1)
          j = 0
          DO i = tmpint1, tmpint2, -1
            j = j + 1
            strx(j) = dxray(i)
            stry(j) = dyray(i)
          END DO
          numnodes = j
C           - then copy nodes backwards from straddidx(2) to loc2
          tmpint1 = straddidx(2)
          tmpint2 = loc2
          j = numnodes
          DO i = tmpint1, tmpint2, -1
            j = j + 1
            strx(j) = dxray(i)
            stry(j) = dyray(i)
          END DO
          numnodes = j
        ENDIF
C           - ( cntig = 1 )
C         - now interpolate the depths
C         -- calculate eN (total distance between endpoints)
        eN = 0
        distn = 0
        DO i = 1, numnodes - 1
          distn = SQRT ( ( (strx(i) - strx(i+1)) * 
     |                       (strx(i) - strx(i+1)) ) +
     |                     ( (stry(i) - stry(i+1)) *
     |                       (stry(i) - stry(i+1)) ) )
          eN = eN + distn
        END DO
C           - ( i = 1, numnodes - 1 )
C         - assign endpoint depths from Common AUTODPTH
        tmpdpths(1) = dpth1
        tmpdpths(numnodes) = dpth2
        DO i = 2, numnodes - 1
C           - move through nodes assigning depths
C           -- calculate em (distance from loc1 to loc 'i')
          em = 0
          distn = 0
          DO j = 1, i - 1
            distn = SQRT ( ( (strx(j) - strx(j+1)) * 
     |                         (strx(j) - strx(j+1)) ) +
     |                       ( (stry(j) - stry(j+1)) *
     |                         (stry(j) - stry(j+1)) ) )
            em = em + distn
          END DO
C             - ( j = 1, i - 1 )
C           - calculate & assign depth at loc 'i'
          tmpdpths(i) = ( (em * dpth2) + ((eN - em) * dpth1) )
     |                                   / eN
        END DO
C           - ( i = 2, numnodes - 1 )
C         - now copy depths to depth()
        IF ( cntig .eq. 1 ) THEN
C           - copy nodes from loc1 to loc2
          tmpint1 = loc1
          tmpint2 = loc2
          j = 0
          DO i = tmpint1, tmpint2
            j = j + 1
            depth(i) = tmpdpths(j)
          END DO
        ELSE
C           - copy nodes backwards from loc1 to straddidx(1)
          tmpint1 = loc1
          tmpint2 = straddidx(1)
          j = 0
          DO i = tmpint1, tmpint2, -1
            j = j + 1
            depth(i) = tmpdpths(j)
          END DO
C           - then copy nodes backwards from straddidx(2) to loc2
          tmpint1 = straddidx(2)
          tmpint2 = loc2
          DO i = tmpint1, tmpint2, -1
            j = j + 1
            depth(i) = tmpdpths(j)
          END DO
        ENDIF
C           - ( cntig = 1 )
      ENDIF
C         - ( dpth1 = dpth2 )

      close(23)

      RETURN
      END

C*----------------------------------------------------------------------*
C*--------------------END NODEUPDT.FOR-----------------------------------*

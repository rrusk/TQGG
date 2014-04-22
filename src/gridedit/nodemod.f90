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

!*----------------------------------------------------------------------*
!                       NODEMOD.FOR                                     *
!       This module contains procedures for node manipulation.          *
!       ROUTINES - MovTypNode, AddInNode, AddBdNode, AddUpdate          *
!                  DelNode, DelUpdate, DelIsland, DelIslUpdate          *
!                  PlaceMarker, EraseMarkers                            *
!*----------------------------------------------------------------------*

!*----------------------------------------------------------------------*
      SUBROUTINE MovTypNode2 (  xmov, ymov, FirstPoint, NextPoint   )  !, nodetype )
! PURPOSE: To move an interior or boundary node to specified new location.
!   INPUT: Interactive input from user.
!   GIVEN: nodetype = 1, to move interior nodes
!                   = 2, to move boundary nodes
!                   = ( <1 or >2 ), to move any type of point
! RETURNS: dxray(movidx), dyray(movidx) = updated to reflect move,
!          In Common MOVEDIT;
!               movidx = index of node moved,
!                      = 0 if unsuccessful move.
! EFFECTS: No segments created.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - PASSED VARIABLES
      REAL xmov, ymov

! - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

! - LOCAL VARIABLES
      integer, save :: movidx, nodetype, origcolor
      REAL movedx, movedy
      REAL, save :: xnew, ynew, xold, yold
      real xtemp, ytemp
      REAL xclose(1), yclose(1), bx(3), by(3)
      integer ierr, ndx, mbound, bpfst, bplst
      integer closendx, TypeOfNode, BndNodePos
      CHARACTER*80 ans, cstr
      CHARACTER*1 PigCursYesNo
      logical  FirstPoint, NextPoint  

!------------------START ROUTINE----------------------------------------

!       - Marker type for pending moves = o
      PendMType = 4
      PendColor = white

      if(FirstPoint) then
        movidx = 0
        ierr = 0
        nodetype = 0
        call CheckNode ( xmov, ymov, movidx, ierr, nodetype )

        IF ( ierr .eq. 1 ) THEN
!           - valid node to move selected, put a marker at exact location
          IF ( movidx .gt. TotCoords - TotIntPts ) THEN
            nodetype = 1 
            origcolor = NodeIColor
          ELSE
            nodetype = 2 
            origcolor = NodeBColor
          ENDIF

          xold = dxray(movidx)
          yold = dyray(movidx)
!           - temporarily set marker type for pending move
          call PigSetSymbolNumber ( PendMType )
          call RedrawMark( xold, yold, PendColor )
          call PigSetSymbolNumber ( NodeMType )
          FirstPoint = .false.
          NextPoint = .true.
          return
        else
          FirstPoint = .true.
          NextPoint = .false.
          return
        endif

      elseif(NextPoint) then

        xnew = xmov
        ynew = ymov
!        call NewPt( xnew, ynew, 4, ierr )
        ierr=1
        IF ( ierr .eq. 1 ) THEN
!             - location to move to picked ok
!             - put a marker at point to move to
          call RedrawMark( xnew, ynew, ModifColor )
!             - check all nodes for a 'close move'
          ierr = 0
!             - save xnew, ynew values
          xtemp = xnew
          ytemp = ynew
          call CheckNode ( xtemp, ytemp, ndx, ierr, 3 )
          xclose(1) = xtemp
          yclose(1) = ytemp
          closendx = ndx
!             - make sure node not flagged as too close to itself
          IF ( (xold .eq. xtemp) .AND. (yold .eq. ytemp) ) ierr = 0
!               - ans set to 'Y' to enter 'ELSE (if move was close but OK)'
!               -- in case following IF not entered
          ans(1:1) = 'Y'

          ierr = 0
          IF ( ierr .eq. 1 ) THEN
!                 - hilite close node
            call PigSetSymbolColour ( red )
            call PigSetSymbolNumber ( NodeMType )
            call PigSetWindowNum ( MAINWIN )
            call PigDrawSymbols ( 1, xclose, yclose )
!                 - move was close to another node, get confirmation
            cstr = 'Move is CLOSE to RED node;  OK ?:'
            ans = PigCursYesNo (cstr)
!                 - un-hilite close node
            call PigSetSymbolNumber ( NodeMType )
            IF ( TypeOfNode(closendx) .eq. 1 ) THEN
              call PigSetSymbolColour ( NodeIColor )
            ELSE
              call PigSetSymbolColour ( NodeBColor )
            ENDIF
            call PigSetWindowNum ( MAINWIN )
            call PigDrawSymbols ( 1, xclose, yclose )
          ENDIF  !     - ( ierr = 1 ) 

          IF ( ans(1:1) .eq. 'N' ) THEN
!                 - close move NOT ok, remove marker at location to move to
            call PigSetSymbolNumber ( NodeMType )
            call RedrawMark( xnew, ynew, backgr )
!                 - remove pendmarker
            call PigSetSymbolNumber ( PendMType )
            call RedrawMark( xold, yold, backgr )
            call PigSetSymbolNumber ( NodeMType )
!                 - restore original marker
            call RedrawMark( xold, yold, origcolor )
!            movidx = 0
          ELSE
!                 - if move was close it's OK
!                 -- show boundary connections if boundary node moved
            IF ( nodetype .eq. 2 ) THEN
!                   - must determine if first or last node in boundary
              IF ( BndNodePos(movidx,mbound,bpfst,bplst).eq. 0 ) THEN
!                     - a middle node
                bx(1) = dxray(movidx-1)
                by(1) = dyray(movidx-1)
                bx(2) = xnew
                by(2) = ynew
                bx(3) = dxray(movidx+1)
                by(3) = dyray(movidx+1)
              ELSEIF(BndNodePos(movidx,mbound,bpfst,bplst).eq.1) THEN
!                     - first node
                bx(1) = dxray(bplst)
                by(1) = dyray(bplst)
                bx(2) = xnew
                by(2) = ynew
                bx(3) = dxray(movidx+1)
                by(3) = dyray(movidx+1)
              ELSEIF(BndNodePos(movidx,mbound,bpfst,bplst).eq.2) THEN
!                     - last node
                bx(1) = dxray(movidx-1)
                by(1) = dyray(movidx-1)
                bx(2) = xnew
                by(2) = ynew
                bx(3) = dxray(bpfst)
                by(3) = dyray(bpfst)
              ENDIF  ! - ( BndNodePos(movidx,mbound,bpfst,bplst) = 0 )
              call PigSetLineColour ( red )
              call PigSetWindowNum ( MAINWIN )
              call PigDrawPolyline ( 3, bx, by )
              bx(2) = xold
              by(2) = yold
              call PigSetLineColour ( backgr )
              call PigDrawPolyline ( 3, bx, by )
            ENDIF   ! - ( nodetype = 2 )
!                   - move acceptable, remove pending marker
            call PigSetSymbolNumber ( PendMType )
            call RedrawMark( xold, yold, backgr )
            call PigSetSymbolNumber ( NodeMType )
!                 - remove old node from display too
            call RedrawMark( xold, yold, backgr )
!                 - Move was OK store new point in common MOVEDIT
!                 - movidx stored in common by CheckNode call above
            movedx = xnew
            movedy = ynew
            dxray(movidx) = movedx
            dyray(movidx) = movedy
!  ** leave depth as is
!            call DepthUpdate ( nodetype, 1, movidx, 0 )
!            call AutoDpthInterp ( nodetype, movidx )
            FirstPoint = .true.
            NextPoint = .false.
          ENDIF !   - ( ans = N ), 'close move' OK
        ENDIF  !   - ( ierr = 1), for xmov, ymov in CheckNode
      ENDIF  !  - nextpoint

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE AddInNode (xadd, yadd )

! PURPOSE: To add an interior node at specified new location.
!   INPUT: Interactive input from user.
!   GIVEN: None.
! RETURNS: In Common NODES;
!               TotIntPts = updated to include new node, if created,
!               TotCoords = updated to include new node, if created,
!               dxray(), dyray(), depth() = updated to include new node,
!                                            if created.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

! - LOCAL VARIABLES
      integer addedidx
      REAL addedx, addedy
      REAL xadd, yadd, xtemp, ytemp, xclose(1), yclose(1)
      integer ierr, ndx, nodetype, i3
      integer closendx, TypeOfNode
      LOGICAL confirm
      CHARACTER*80 ans, cstr
      CHARACTER*1 PigCursYesNo

!------------------START ROUTINE----------------------------------------

      ierr = 0
      i3 = 3
      nodetype = 1

!           - node to add picked ok, put a marker there
      call PigSetSymbolNumber ( NodeMType )
      call RedrawMark( xadd, yadd, ModifColor )
!           - check all nodes for a 'close add'
      xtemp = xadd
      ytemp = yadd
      call CheckNode ( xtemp, ytemp, ndx, ierr, i3 )
      xclose(1) = xtemp
      yclose(1) = ytemp
      closendx = ndx
!           - ans set to 'Y' to enter 'ELSE (if add was close it's OK...)'
!           -- in case following IF not entered
      ans(1:1) = 'Y'
      ierr = 0
      IF ( ierr .eq. 1 ) THEN
!             - hilite close node
        call PigSetSymbolColour ( red )
        call PigSetSymbolNumber ( NodeMType )
        call PigSetWindowNum ( MAINWIN )
        call PigDrawSymbols ( 1, xclose, yclose )
!             - add was close to another node, get confirmation
        cstr = 'Added node is CLOSE to RED node; OK ?:'
        ans = PigCursYesNo (cstr)
!             - un-hilite close node
        call PigSetSymbolNumber ( NodeMType )
        IF ( TypeOfNode(closendx) .eq. 1 ) THEN
          call PigSetSymbolColour ( NodeIColor )
        ELSE
          call PigSetSymbolColour ( NodeBColor )
        ENDIF
        call PigSetWindowNum ( MAINWIN )
        call PigDrawSymbols ( 1, xclose, yclose )
      ENDIF  !   - ( ierr = 1 )
 
!      call PigEraseMessage
      IF ( ans(1:1) .eq. 'N' ) THEN
!             - close add is .NOT. OK, remove add marker
        call RedrawMark( xadd, yadd, backgr )
      ELSE
!             - if add was close it's OK, ask if acceptable add
!             - add acceptable, store new point in common ADDEDIT
        addedx = xadd
        addedy = yadd
        call AddUpdate ( nodetype, addedx, addedy, addedidx,confirm, .FALSE. )
      ENDIF  !  - ( ans = N ), 'close add' OK

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE AddBdNode ( xadd, yadd )

! PURPOSE: To add a boundary node at specified new location.
!   INPUT: Interactive input from user
!   GIVEN: None.
! RETURNS: In Common NODES;
!               dxray(), dyray(), depth(), TotCoords, PtsThisBnd =
!                 updated to reflect new node, if added.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

! - LOCAL VARIABLES
      integer addedidx
      REAL addedx, addedy
      REAL xadd, yadd, xtemp, ytemp, xclose(1), yclose(1)
      integer ierr, ndx, nodetype, i3
      integer closendx, TypeOfNode
      LOGICAL confirm
      CHARACTER*80 ans, cstr
      CHARACTER*1 PigCursYesNo

!------------------START ROUTINE----------------------------------------

      ierr = 1
      i3 = 3
      nodetype = 2

!           - node to add picked ok, put a marker there
      call PigSetSymbolNumber ( NodeMType )
      call RedrawMark( xadd, yadd, ModifColor )
!           - check all nodes for a 'close add'
      ierr = 0
      xtemp = xadd
      ytemp = yadd
      call CheckNode ( xtemp, ytemp, ndx, ierr, 3 )
      xclose(1) = xtemp
      yclose(1) = ytemp
      closendx = ndx
!           - ans set to 'Y' to enter 'ELSE (if add was close it's OK...)'
!           -- in case following IF not entered
      ans(1:1) = 'Y'
      ierr = 0
      IF ( ierr .eq. 1 ) THEN
!             - hilite close node
        call PigSetSymbolColour ( red )
        call PigSetSymbolNumber ( NodeMType )
        call PigSetWindowNum ( MAINWIN )
        call PigDrawSymbols ( 1, xclose, yclose )
!             - add was close to another node, get confirmation
        cstr = 'Added node is CLOSE to RED node; OK ?:'
        ans = PigCursYesNo (cstr)
!             - un-hilite close node
        call PigSetSymbolNumber ( NodeMType )
!             - restore close node
        IF ( TypeOfNode(closendx) .eq. 1 ) THEN
          call PigSetSymbolColour ( NodeIColor )
        ELSE
          call PigSetSymbolColour ( NodeBColor )
        ENDIF
        call PigSetWindowNum ( MAINWIN )
        call PigDrawSymbols ( 1, xclose, yclose )
      ENDIF
!            - ( ierr = 1 ) 
      IF ( ans(1:1) .eq. 'N' ) THEN
!             - close add is .NOT. OK, remove add marker
        call RedrawMark( xadd, yadd, backgr )
      ELSE
!             - if add was close it's OK, store new point in common ADDEDIT
        addedx = xadd
        addedy = yadd
        confirm = .FALSE.
        call AddUpdate ( nodetype,addedx, addedy, addedidx,confirm, .FALSE. )
        IF ( confirm ) THEN
        ELSE
!               - add not confirmed, remove added node from display
          call RedrawMark( xadd, yadd, backgr )
        ENDIF  !   - ( confirm )
      ENDIF  !    - ( ans = N ), 'close add' OK

      RETURN
      END

!*--------------------------------------------------------------------------*

      SUBROUTINE AddUpdate ( nodetype,addedx, addedy, addedidx,confirm, updepth )

! PURPOSE: To update display arrays dxray(), dyray(), depth() after an
!           acceptable add operation.
!   GIVEN: In Common ADDEDIT;
!               - addedx, addedy = coordinates of node to be added at
!                                  dxray(addedidx), dyray(addedidx).
!          nodetype = type (interior: 1,  or boundary : 2) of node 
!                     to be added.
!          updepth = .TRUE. if depth to be updated on add
!                  = .FALSE. if calling routine to handle depth
! RETURNS: In Common NODES;
!               - TotCoords = TotCoords + 1.
!               - TotIntPts = TotIntPts + 1, IF interior node added.
!               - PtsThisBnd(i) = PtsThisBnd(i) + 1 for boundary i, 
!                                 IF boundary node added.
!          addedidx = index node added at
! EFFECTS: dxray(), dyray(), depth() are updated to reflect addition.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer nodetype
      LOGICAL confirm, updepth

!       - CHGTYP stores new index of interior node switched to boundary
      integer chgtypndx
      COMMON /CHGTYP/ chgtypndx

! - LOCAL VARIABLES
      integer locidx, shuffidx, boundnum
      integer addedidx
      REAL addedx, addedy

!------------------START ROUTINE----------------------------------------

      IF ( nodetype .eq. 1 ) THEN 
!           - new node will be stored as last interior node
        TotCoords = TotCoords + 1
        TotIntPts = TotIntPts + 1
        itot = TotCoords
        !exist(itot) = .true.
        addedidx = TotCoords
        dxray(addedidx) = addedx
        dyray(addedidx) = addedy
        IF ( updepth ) THEN
!             - 2nd argument 'types' calling routine
          call DepthUpdate ( nodetype, 2, addedidx, 0 )
        ENDIF
      ELSE IF ( nodetype .eq. 2 ) THEN
!           - find location where new boundary node will be stored
        call LocateBndPos(addedx,addedy,locidx,boundnum,confirm)
!           - if found, shuffle  dxray(), dyray(), depth() down 1 position
        IF ( confirm ) THEN
          TotCoords = TotCoords + 1
          itot = TotCoords
          !exist(itot) = .true.
          shuffidx = TotCoords
          DO WHILE (shuffidx .gt. locidx)
            dxray(shuffidx) = dxray(shuffidx - 1)
            dyray(shuffidx) = dyray(shuffidx - 1)
            depth(shuffidx) = depth(shuffidx - 1)
            shuffidx = shuffidx - 1
          END DO
!             - now store the new point
          dxray(locidx) = addedx
          dyray(locidx) = addedy
!             - store index found for node to CHGTYP var for ChangeNodeType proc.
          chgtypndx = locidx

              pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

          PtsThisBnd(boundnum) = PtsThisBnd(boundnum) + 1

              tOTiNTpTS =  pTStHISbND(tOTbNDYS+1)

          IF ( updepth ) THEN
!               - 2nd argument 'types' calling routine
            call DepthUpdate ( nodetype, 2, locidx, 0 )
          ENDIF
        ELSE
!             - position was not confirmed for new node
          confirm = .FALSE.
        ENDIF
      ELSE
!           - an erroneous nodetype has crept in somehow
        confirm = .FALSE.
      ENDIF
!           - (nodetype = 1)

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE DelNode (  xdel, ydel )

! PURPOSE: To delete an interior node specified by user.
!   INPUT: Interactive input from user.
!   GIVEN: nodetype = type (boundary or interior) of node to delete:
!                     1 = interior,
!                     2 = boundary
!                     It is important not to change this value and confuse
!                     the DelUpdate() routine into performing data update
!                     on wrong node type. Nodetype is set in NODEMENU.FOR
!                     originally by Interior & Boundary menus.
! RETURNS: In Common NODES;
!               dxray(), dyray(), depth(), TotCoords, TotIntPts,
!               PtsThisBnd() = updated to reflect deleted node, if any,
!          In Common DELEIT;
!               delidx = index of deleted node before it was deleted,
!                      = 0 if no node deleted.
! EFFECTS: Node confirmed to delete by user is removed. DelUpdate is called
!          to update Common NODES.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - PASSED VARIABLES
      REAL xdel, ydel

!- "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

! - LOCAL VARIABLES
      integer nodetype
      integer delidx
      REAL xpend, ypend
      REAL deletx, delety
      integer origcolor, ierr

!--------------------START ROUTINE--------------------------------------*

!       - index to data arrays of any nodes to delete, set to 0
!       -- to avoid any erroeus deletes
      delidx = 0
!       - Marker type for pending moves = o
      PendMType = 4
      PendColor = white

      ierr = 0
      nodetype = 0
      call CheckNode ( xdel, ydel, delidx, ierr, nodetype )

      IF ( ierr .eq. 1 ) THEN
!           - valid node to delete selected, put a marker at exact location
        IF ( delidx .gt. TotCoords - TotIntPts ) THEN
          nodetype = 1 
          origcolor = NodeIColor
        ELSE
          nodetype = 2 
          origcolor = NodeBColor
        ENDIF

        xpend = dxray(delidx)
        ypend = dyray(delidx)
        call PigSetSymbolNumber ( NodeMType )
        call RedrawMark( xdel, ydel, backgr )

        deletx = dxray(delidx)
        delety = dyray(delidx)
        call DelUpdate( nodetype, delidx )

      ELSE     !   - valid node to delete NOT selected
        delidx = 0
      ENDIF   !   - ( ierr .eq. 1 ) - valid node to delete selected

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE DelUpdate ( nodetype, delidx )

! PURPOSE: To update dxray(), dyray(), depth() during a successful call to
!          DelNode.
!   GIVEN: nodetype = type (2=boundary or 1=interior) of node to delete,
!          In Common DELETIT;
!               deletx = x coordinate of node to delete,
!               delety = y coordinate of node to delete,
!               delidx = index of node to delete.
! RETURNS: Updated Node data in Common NODES in NODESTOR.INC.
! EFFECTS: dxray(), dyray(), depth() are updated so that node specified
!          in Common DELETIT is removed. TotCoords, PtsThisBnd(), and
!          TotIntPts are updated as required.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer nodetype

! - LOCAL VARIABLES
      integer tempnum, tempnum2, i, shuffidx
      integer delidx
      CHARACTER*80 cstr

!--------------------START ROUTINE--------------------------------------*

      IF ( nodetype .eq. 1 ) THEN
!           - interior node
        tempnum = ( TotCoords - TotIntPts ) + 1
        IF ( delidx .lt. tempnum ) THEN
!             - error nodetype was interior, but delidx is in boundary range
          cstr='NODETYPE/INDEX conflict in DelUpdate - I'
          call PigMessageOK(cstr,'DelUpdate')
        ELSE
!             - index checks out OK for interior node
!             - shuffle  dxray(), dyray(), depth() down 1 position
          shuffidx = delidx
          DO WHILE (shuffidx .lt. TotCoords)
            dxray(shuffidx) = dxray(shuffidx + 1)
            dyray(shuffidx) = dyray(shuffidx + 1)
            depth(shuffidx) = depth(shuffidx + 1)
            shuffidx = shuffidx + 1
          END DO
          TotCoords = TotCoords - 1
          itot = TotCoords
          !exist(itot+1) = .false.
          code(itot+1) = -9
          TotIntPts = TotIntPts - 1
        ENDIF
!             - ( delidx < tempnum )
      ELSE IF ( nodetype .eq. 2 ) THEN
!           - boundary node
        tempnum = ( TotCoords - TotIntPts )
        IF ( delidx .gt. tempnum ) THEN
!             - error nodetype was boundary, but delidx is in interior range
          cstr='NODETYPE/INDEX conflict in DelUpdate - B'
          call PigMessageOK(cstr,'DelUpdate')

        ELSE
!             - index checks out OK for boundary node
!             - update PtsThisBnd(i) for correct 'i'
          tempnum2 = 0
          i = 0

              pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

          DO WHILE ( i .lt. TotBndys + 1 )
            i = i + 1
            tempnum2 = tempnum2 + PtsThisBnd(i)
            IF ( delidx .le. tempnum2 ) THEN
          PtsThisBnd(i) = PtsThisBnd(i) - 1
!                 - now end the loop
          i = 9999
            ENDIF
          END DO
!             - shuffle  dxray(), dyray(), depth() down 1 position
          shuffidx = delidx
          DO WHILE (shuffidx .lt. TotCoords)
            dxray(shuffidx) = dxray(shuffidx + 1)
            dyray(shuffidx) = dyray(shuffidx + 1)
            depth(shuffidx) = depth(shuffidx + 1)
            shuffidx = shuffidx + 1
          END DO
          TotCoords = TotCoords - 1
          itot = TotCoords
          !exist(itot+1) = .false.
          code(itot+1) = -9
        ENDIF
!             - ( delidx > tempnum )
      ENDIF
!           - ( nodetype = 1 )

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE DelIsland (islx, isly)

! PURPOSE: To delete an entire boundary section which defines an island.
!          *Note: The first boundary section, ie: the outer boundary
!                 cannot be deleted by this routine.
!   GIVEN: Interactive input from user.
! RETURNS: In Common NODES;
!               dxray(), dyray(), depth(), TotCoords, PtsThisBnd() =
!                 updated to reflect deleted island, if any.
! EFFECTS: The specified island boundary is deleted.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - LOCAL VARIABLES
      CHARACTER*80 cstr
      REAL islx, isly
      integer island, ierr, i, islndx, tempnum
      LOGICAL islfound, islOK

!*-------------------START ROUTINE-------------------------------------

      islfound = .FALSE.
      call CheckNode ( islx, isly, islndx, ierr, 2 )
      IF ( ierr .eq. 1 ) THEN
!           - a boundary node was selected, determine boundary # for islndx
        tempnum = 0
        i = 0
        DO WHILE ( i .lt. TotBndys )
          i = i + 1
         tempnum = tempnum + PtsThisBnd(i)
          IF ( islndx .le. tempnum ) THEN
            island = i
!               - now end the loop
            i = 9999
          ENDIF
!               - ( islndx <= tempnum )
        ENDDO
!             - ( i > TotBndys )
        IF ( island .eq. 1 ) THEN
!             - outer boundary chosen, invalid
          islfound = .FALSE.
          cstr = 'Outer Boundary Is Not Valid For This Operation.'
          call PigMessageOK(cstr,'DelIsland')
        ELSE
!             - a valid island was chosen
          islfound = .TRUE.
        ENDIF
!             - ( island = 1 )
      ENDIF
!           - ( ierr = 1 ) for CheckNode

      IF ( islfound ) THEN
!           - island to delete located
!           - APR93 - add 3rd parameter, True = prompt for confirmation
        call DelIslUpdate ( island, islOK, .TRUE. )
!             - ( NOT islOK )
      ENDIF
!           - ( islfound )

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE DelIslUpdate ( isl, OK, interact )

! PURPOSE: To remove all data concerning specified island from arrays
!          dxray(), dyray(), depth(), and from NODESTOR storage.
!          *Note: The first boundary section, ie: the outer boundary
!                 cannot be deleted by this routine.
!   GIVEN: Interactive input from user to confirm, if interact
!          isl = boundary section # of island.
!          interact = .TRUE. if user confirmation sought interactively
!                   = .FALSE. if deletion to procede without user confirmation
! RETURNS: OK = TRUE if valid boundary section # defining an island
!               was passed in isl, else FALSE. *Note: OK depends only
!               on a valid boundary section in isl, whether user
!               confirms deletion or not does not affect OK.
! EFFECTS: The specified island boundary data is removed from storage
!          if confirmed by user.
! WRITTEN: June 1990 by JDM for NODER.
! MODIFIED: APR93 - JDM - Parameter "interact" introduced.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer isl
      LOGICAL OK, interact

! - LOCAL VARIABLES
      integer i, j, islstart, islend, isldiff,n2
      real x2,y2
!      REAL xmark(halfmrec), ymark(halfmrec)
!      common /xylocal/ xmark, ymark
      CHARACTER*80 cstr, ans
      CHARACTER*1 PigCursYesNo

!*-------------------START ROUTINE-------------------------------------

      IF ( (isl .gt. 1) .AND. (isl .le. TotBndys) ) THEN
!           - valid island specified
        OK = .TRUE.
!           - get delimiting indices to island
        islstart = 0

        PtsThisBnd(totbndys+1) = totintpts

        DO i = 1, isl - 1
         islstart = PtsThisBnd(i) + islstart
        END DO
        islstart = islstart + 1
        islend = islstart + PtsThisBnd(isl) - 1
        isldiff = islend - islstart + 1
!           - highlite island nodes and get confirmation to delete
!        call PigSetWindowNum ( MAINWIN )
!        call PigSetSymbolColour ( ModifColor )
!        k = 1
!        DO j = islstart, islend
!          x2 = dxray(j)
!          y2 = dyray(j)
!          call PigDrawModifySymbol ( x2, y2 )
!          k = k + 1
!        END DO
!             - ( j = islstart, islend )
        n2 =  PtsThisBnd(isl)
!        write(*,*) 'n2,islstart=',n2,islstart
        call PigDrawModifySymbols (n2, dxray(islstart), dyray(islstart) )
        ans(1:1) = 'z'
        cstr = 'Delete this island ?:'
        IF ( interact ) THEN
          ans = PigCursYesNo (cstr)
        ELSE
          ans(1:1) = 'Y'
        ENDIF
        IF ( ans(1:1) .eq. 'Y' ) THEN
!             - confirmed deletion, delete the nodes
!          DO j = islstart, islend
!            x2 = dxray(j)
!            y2 = dyray(j)
!            call PigEraseModifySymbol ( x2, y2 )
!          END DO
          call PigEraseModifySymbols (n2, dxray(islstart), dyray(islstart) )

          DO i = islstart, TotCoords - isldiff
!               - shuffle down to delete
            dxray(i) = dxray ( i + isldiff )
            dyray(i) = dyray ( i + isldiff )
            depth(i) = depth ( i + isldiff )
          END DO
!               - ( i = islstart, TotCoords )
!             - now update NODESTOR info
          if(isl.gt.TotBndys-TotIntBndys) TotIntBndys = TotIntBndys-1
          TotBndys = TotBndys - 1
          TotCoords = TotCoords - isldiff
          itot = TotCoords
          do i=totcoords+1,totcoords+isldiff
            !exist(i) = .false.
            code(i) = -9
          enddo
          DO i = isl,  TotBndys
            PtsThisBnd(i) = PtsThisBnd( i + 1 )
          END DO
!          call PigSetSymbolColour ( backgr )
        ELSE
!             - not confirmed delete
!          call PigSetSymbolColour ( NodeBColor)
          DO j = islstart, islend
            x2 = dxray(j)
            y2 = dyray(j)
            call PigDrawBndSymbol ( x2, y2 )
          END DO
        ENDIF
!             - ( ans = Y )
!           - now redraw island nodes in color set above;
!           -- boundary color if not deleted, background if deleted
!        call PigSetWindowNum ( MAINWIN )
!        call PigDrawSymbols ( 1, x2, y2 )
      ELSE
!           - an invalid island # has been passed
        OK = .FALSE. 
      ENDIF
!             - ( isl > 1  AND  isl <= TotBndys )

      RETURN
      END

!*----------------------------------------------------------------------*

      SUBROUTINE ReverseBoundary ( islx, isly )

! PURPOSE: To reverse a boundary node sequence
!   GIVEN: Interactive input from user.
! RETURNS: In Common NODES; PtsThisBnd() =
!                 updated to reflect reversed order
! EFFECTS: The specified island boundary is reversed.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - LOCAL VARIABLES
      integer ierr, islndx, tempnum, non2, i2, je, js, n2
      integer i, j, islstart, islend
      REAL islx, isly, xtmp, ytmp, dtmp
      CHARACTER*80 cstr, ans
      CHARACTER*1 PigCursYesNo

!*-------------------START ROUTINE-------------------------------------

      ierr = 0
      i2 = 2
!         - a location was specified, determine node
      call CheckNode ( islx, isly, islndx, ierr, i2 )
      IF ( ierr .eq. 1 ) THEN
!           - a boundary node was selected, determine boundary # for islndx
        tempnum = 0
        i = 0
        DO WHILE ( i .lt. TotBndys )
          i = i + 1
          tempnum = tempnum + PtsThisBnd(i)
          IF ( islndx .le. tempnum ) THEN  ! reverse nodes here
            islstart = tempnum - PtsThisBnd(i)
            islend = tempnum + 1
!         - highlite island nodes and get confirmation to delete
!            call PigSetWindowNum ( MAINWIN )
!            call PigSetSymbolColour ( ModifColor )
!            do j=islstart+1,islend-1
!              xtmp = dxray(j)
!              ytmp = dyray(j)
!              call PigDrawSymbols ( 1, dxray(j), dyray(j) )	          
!            enddo
            n2 = PtsThisBnd(i)
            call PigDrawModifySymbols ( n2, dxray(islstart+1), dyray(islstart+1) )           
            cstr = 'Reverse this boundary ?:'
            ans = PigCursYesNo (cstr)
            IF ( ans(1:1) .eq. 'Y' ) THEN
              non2 = PtsThisBnd(i)/2
              do j=1,non2
                js = islstart + j
                je = islend - j
                xtmp = dxray(js)
                ytmp = dyray(js)
                dtmp = depth(js)
                dxray(js) = dxray(je)
                dyray(js) = dyray(je)
                depth(js) = depth(je)
                dxray(je) = xtmp
                dyray(je) = ytmp
                depth(je) = dtmp
              enddo
!              doneisl = .TRUE.
              exit
            else
              call PigDrawBndSymbols ( n2, dxray(islstart+1), dyray(islstart+1) )           
              exit
            endif
          ENDIF
        END DO
      ENDIF

      RETURN
      END

!-----------------------------------------------------------------------*

    SUBROUTINE JoinBoundaries ( islx, isly, FirstPoint, NextPoint )

! PURPOSE: To join boundaries
!   GIVEN: Interactive input from user.
! RETURNS: In Common NODES; PtsThisBnd() =
!                 updated to reflect joining
! EFFECTS: The specified island boundaries are joined.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - LOCAL VARIABLES
      integer ierr, islndx2, isl2, isl2start, isl2end, isl, islstart, islend
      integer, save :: islndx
      integer j, jj, k, knum, n, n2, i2
      REAL islx, isly
      real, save :: px(2), py(2)
      CHARACTER*1 PigCursYesNo, ans
      logical  FirstPoint, NextPoint,flip,flip2

!*-------------------START ROUTINE-------------------------------------

      i2 = 2 ! Only boundary nodes

      if(FirstPoint) then
        ierr = 0
        call CheckNode ( islx, isly, islndx, ierr, i2 )

!       Pick first node
        IF ( ierr .eq. 1 ) THEN
!         Valid node to move selected, put a marker at exact location

!         Loop through bnds to find bnds indexes
          jj = 0
          do j=1,TotBndys
            jj = jj + PtsThisBnd(j)
            IF (( islndx.le.jj ).and. (islndx.ge.jj-PtsThisBnd(j)+1)) THEN
              isl = j
              islstart = jj - PtsThisBnd(j) + 1
              islend = jj
              n = PtsThisBnd(j)
            END IF
          END DO

!         Reselect the nearest endnode by finding the closes end-node (in terms of index)
          IF ((islndx-islstart).lt.(islend-islndx)) THEN
            islndx = islstart
          ELSE
            islndx = islend
          END IF


!          
          call PigDrawModifySymbol ( dxray(islndx), dyray(islndx) )
          px(1) = dxray(islndx)
          py(1) = dyray(islndx)
          FirstPoint = .false.
          NextPoint = .true.
          return
        else
          FirstPoint = .true.
          NextPoint = .false.
          return
        endif

!     Pick second node
      elseif(NextPoint) then
        ierr=0
        call CheckNode ( islx, isly, islndx2, ierr, i2 )

        IF ( ierr .eq. 1 ) THEN
!         Loop through bnds to find bnds indexes
          jj = 0
          do j=1,TotBndys
            jj = jj + PtsThisBnd(j)
            IF (( islndx2.le.jj ).and. (islndx2.ge.jj-PtsThisBnd(j)+1)) THEN
              isl2 = j
              isl2start = jj - PtsThisBnd(j) + 1
              isl2end = jj
              n2 = PtsThisBnd(j)
            END IF
            IF (( islndx.le.jj ).and. (islndx.ge.jj-PtsThisBnd(j)+1)) THEN !Duplicate to get rid of warning
              isl = j
              islstart = jj - PtsThisBnd(j) + 1
              islend = jj
              n = PtsThisBnd(j)
            END IF
          END DO

!         Reselect the nearest endnode by finding the closes end-node (in terms of index)
          IF ((islndx2-isl2start).lt.(isl2end-islndx2)) THEN
            islndx2 = isl2start
          ELSE
            islndx2 = isl2end
          END IF

!         Check it is the same boundary as nr 1
          IF (isl2.eq.isl) then
            call PigMessageOK('Cannot join the same boundary','DelUpdate')
            FirstPoint = .true.
            NextPoint = .false.
            return
          END IF
          write(*,*)islndx, isl,islstart,islend, islndx2, isl2,isl2start, isl2end

          call PigDrawModifySymbol ( dxray(islndx2), dyray(islndx2) )
          px(2) = dxray(islndx2)
          py(2) = dyray(islndx2)

!         Draw the joining line
          call PigDrawLine( 2, px, py, 1 )

!         Prompt user for joining
          ans = PigCursYesNo ('Join these boundaries ?:')
          IF ( ans(1:1) .eq. 'Y' ) THEN

!     *** find lowest boundary
            if(isl2.lt.isl) then
              k = isl2
              isl2 = isl
              isl = k
              k = isl2start
              isl2start = islstart
              islstart = k
              k = isl2end
              isl2end = islend
              islend = k
              k = islndx2
              islndx2 = islndx
              islndx = k
              k = n2
              n2 = n
              n = k
            endif

!           Determine if bnds should be flipped
            flip = .false.
            flip2 = .false.

            IF ( (islndx.eq.islstart).and.(islndx2.eq.isl2start) ) THEN
              flip = .true.
              flip2 = .false.
            ELSEIF ( (islndx.eq.islstart).and.(islndx2.eq.isl2end) ) THEN
              flip = .true.
              flip2 = .true.
            ELSEIF ( (islndx.eq.islend).and.(islndx2.eq.isl2end) ) THEN
              flip = .false.
              flip2 = .true.
            END IF


!   *** move up the second segment to the back of array
            knum = 0
            do k=isl2start,isl2end
              knum = knum+1
              dxray(TotCoords+knum) = dxray(k)
              dyray(TotCoords+knum) = dyray(k)
              depth(TotCoords+knum) = depth(k)
              code(TotCoords+knum) = code(k)
            enddo

!      Shuffle nodes between first and second back to make room for second
            do k=isl2start-1,islend+1,-1
              dxray(k+n2) = dxray(k)
              dyray(k+n2) = dyray(k)
              depth(k+n2) = depth(k)
              code(k+n2) = code(k)
            enddo
         
!  *** Put second bnd segement after the first one
            do k = 1, n2
              IF(flip2) THEN ! Flip it
                jj = n2-k+1
              ELSE
                jj = k
              END IF
               dxray(islend+k) = dxray(TotCoords+jj)
               dyray(islend+k) = dyray(TotCoords+jj)
               depth(islend+k) = depth(TotCoords+jj)
               code(islend+k) = code(TotCoords+jj)
            enddo

!       Flip first segment if needed by moving to back, then flip on way back
            IF(flip) then

              knum = 0
              do k=islstart,islend
                knum = knum+1
                dxray(TotCoords+knum) = dxray(k)
                dyray(TotCoords+knum) = dyray(k)
                depth(TotCoords+knum) = depth(k)
                code(TotCoords+knum) = code(k)
              enddo

              do k = 1, n
                jj = n-k+1
                dxray(islstart-1+k) = dxray(TotCoords+jj)
                dyray(islstart-1+k) = dyray(TotCoords+jj)
                depth(islstart-1+k) = depth(TotCoords+jj)
                code(islstart-1+k) = code(TotCoords+jj)
               enddo
            END IF ! flip
     

!       Clean up bnd register
            PtsThisBnd(isl) = PtsThisBnd(isl) + PtsThisBnd(isl2)                
            do k = isl2, TotBndys-1
              PtsThisBnd(k) = PtsThisBnd(k+1)
            enddo

            TotBndys = TotBndys-1
          else ! - Join bnd prompt
!           Remove the joining line
            call PigDrawLine( i2, px, py, 2 )
          endif ! - Join bnd prompt

          FirstPoint = .true.
          NextPoint = .false.
        ENDIF  !   - ( ierr = 1), for xmov, ymov in CheckNode
      ENDIF  !  - nextpoint

      RETURN
      END

!-----------------------------------------------------------------------*

    SUBROUTINE SplitBoundaries ( islx, isly, FirstPoint )

! PURPOSE: To split boundaries
!   GIVEN: Interactive input from user.
! RETURNS: In Common NODES; PtsThisBnd() =
!                 updated to reflect splitting
! EFFECTS: The specified island boundaries are joined.
!*----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - LOCAL VARIABLES
      integer j, ierr, islndx2, idif1, idif2
      integer, save :: islndx,upDown
      integer i2, icnt,iend,rdflag, ibnd
      REAL islx, isly, islx1, isly1, islx2, isly2
      real, save :: px(2), py(2)
      CHARACTER*1 PigCursYesNo, ans
      logical  FirstPoint,onlychoice  

!*-------------------START ROUTINE-------------------------------------

      i2 = 2

      if(FirstPoint) then
        ierr = 0
        call CheckNode ( islx, isly, islndx, ierr, i2 )

        IF ( ierr .ne. 1 ) THEN
          FirstPoint = .true.
          return

        else  !IF ( ierr .eq. 1 ) THEN
!           - valid node, put a marker at exact location
          islx1 = dxray(islndx)
          isly1 = dyray(islndx)
          call PigDrawModifySymbol ( islx1, isly1 )
          px(1) = islx1
          py(1) = isly1

! need some error checks here to determine which adjacent segment to use
! loop over boundaries to find node
          icnt = 1
          do j=1,TotBndys
            iend = icnt+PtsThisBnd(j)-1
            if(islndx.eq.icnt.or.islndx.eq.iend) then !endpoint
              call PigMessageOK('Cannot split endpoint','SplitBnd')
              call PigDrawBndSymbol( islx1, isly1 )
              return
            elseif(islndx.lt.iend) then !found node
              idif1 = islndx - icnt
              idif2 = iend - islndx
              ibnd = j
              exit
            endif
            icnt = iend + 1
          enddo

! Determine if the first try should be up or down.


          if(idif1.eq.1) then !near start
            upDown = 1
            onlychoice = .true.
          elseif(idif2.eq.1) then !near end 
            upDown = -1
            onlychoice = .true.
          else ! Determine by distance - longest first
            upDown = -1
            if(sqrt( (dxray(islndx)-dxray(islndx-1))**2 + (dyray(islndx)-dyray(islndx-1))**2 ).lt. &
              sqrt( (dxray(islndx)-dxray(islndx+1))**2 + (dyray(islndx)-dyray(islndx+1))**2 )) upDown = 1
            onlychoice = .false.
          end if


! first try

          islndx2 = islndx + upDown
          islx2 = dxray(islndx2)
          isly2 = dyray(islndx2)
          call PigDrawModifySymbol ( islx2, isly2 )

          rdflag = 2
          px(2) = islx2
          py(2) = isly2
          call PigDrawLine( i2, px, py, rdflag )

          ans = PigCursYesNo ('Split this boundary segment ?:')
          IF ( ans(1:1) .eq. 'Y' ) THEN
            do j=TotBndys,ibnd+1,-1
              PtsThisBnd(j+1) = PtsThisBnd(j)
            enddo
            TotBndys = TotBndys + 1
            PtsThisBnd(ibnd) = max(islndx,islndx2) - icnt
            PtsThisBnd(ibnd+1) = iend - min(islndx,islndx2)
          else
            call PigDrawBndSymbol( islx2, isly2 )
            rdflag = 4
            call PigDrawLine( i2, px, py, rdflag )
            
            if(onlychoice) then
              call PigDrawBndSymbol( islx1, isly1 )
              return
            endif

! Switch upDown for second try
            upDown = upDown*(-1)

! second try
            islndx2 = islndx  + upDown
            islx2 = dxray(islndx2)
            isly2 = dyray(islndx2)
            call PigDrawModifySymbol ( islx2, isly2 )
            rdflag = 2
            px(2) = islx2
            py(2) = isly2
            call PigDrawLine( i2, px, py, rdflag )
            
            ans = PigCursYesNo ('Split THIS boundary segment ?:')
            IF ( ans(1:1) .eq. 'Y' ) THEN
              do j=TotBndys,ibnd+1,-1
                PtsThisBnd(j+1) = PtsThisBnd(j)
              enddo
              TotBndys = TotBndys + 1
              PtsThisBnd(ibnd) = max(islndx,islndx2) - icnt
              PtsThisBnd(ibnd+1) = iend - min(islndx,islndx2)
            else  !no split
              call PigDrawBndSymbol( islx1, isly1 )
              call PigDrawBndSymbol( islx2, isly2 )
              rdflag = 4
              call PigDrawLine( i2, px, py, rdflag )          
            endif
          endif
        ENDIF  !   - ( ierr = 1), for xmov, ymov in CheckNode
      ENDIF  !  - nextpoint
      FirstPoint = .true.

      RETURN
      END

!-----------------------------------------------------------------------*

      SUBROUTINE ReSampleBndNodes(MouseX,MouseY,FirstPoint,NextPoint)

! PURPOSE: Control routine for boundary node reselection routines.
!   GIVEN: None.
! RETURNS: None.
! EFFECTS: The chosen boundary segment is resamples to have increasing/decreasing
!          resolution based on the resolution at the ends. The boundary is first
!          resampled to 1m resolution before beeing resampled again.
!-----------------------------------------------------------------------*

      use MainArrays

! *** passed variables
      real MouseX, MouseY
      logical FirstPoint, NextPoint

! - LOCAL VARIABLES
      integer ierr, ndx, nodetype,i,j,k,nnew,nspc,newbind(1000000)
      integer, save :: indxs(2), bnd, contighalf
      LOGICAL valid
      real :: newbx(1000000), newby(1000000), newbd(1000000), dist,dist1,dist2

!     nexb* - stores the new boundary (1 m resampled at first, then the one with increasing space)
!     newbind - for indexing the nodes that should be in the final bnd segment
!     indxs - the chosen end nodes
!     i,j,k - used for looping and temp storage of integers
!     dist* - for distance between nodes and also increasing distance in the final segment
!     nnew - number of new nodes (both in 1m resampling and in the final segment)
!     nspc - number of spaces/lines (in the new segment)

!-----------------START ROUTINE----------------------------------------

!     Check for correct igridtype
      IF (.not.((igridtype.eq.0).or.(igridtype.eq.1).or.(igridtype.eq.2))) then
        call PigMessageOK('Unknown grid type. ReSample only works with UTM, LatLon and Cartesian. (0,1 and 2)','Projection')
        return
      END IF

!       - get 2 delimiting nodes to mark section
      if(FirstPoint) then
        ierr = 0
        call CheckNode ( MouseX, MouseY, ndx, ierr, nodetype )
        IF ( ierr .eq. 1 ) THEN
!               - valid node chosen, save it & put marker
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
!               - valid node chosen, save it & put marker
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
      endif

      IF ( indxs(1) .gt. indxs(2) ) THEN
        ndx = indxs(1)
        indxs(1) = indxs(2)
        indxs(2) = ndx
      ENDIF


      call PickBndHalf ( bnd, indxs, contighalf )

!     Move end nodes one 'in' on the boundary, to keep initial end spacing
      indxs(1) = indxs(1)+1
      indxs(2) = indxs(2)-1

!     Create a 1m spaced bnd in newbnd
      i = 1
      DO j=indxs(1),indxs(2)-1
!       Get distance between the point and the next

        IF (igridtype.eq.0) then ! latlon
          call haversine(dyray(j),dxray(j),dyray(j+1),dxray(j+1), dist)
        ELSE
          dist = sqrt((dxray(j+1)-dxray(j))**2 + (dyray(j+1)-dyray(j))**2)
        END IF

        nnew = floor(dist)-1

!       If i exceeds limit of 1000000 nodes, error and return
        IF ( (i + nnew).gt. 1000000 ) THEN
          call PigMessageOK('The boundary segment is to long','ReSample')
          return
        END IF

        newbx(i) = dxray(j)
        newby(i) = dyray(j)
        newbd(i) = depth(j)

        DO k=1,nnew
          newbx(i+k) = dxray(j) + k * ((dxray(j+1)-dxray(j)) / (nnew + 1))
          newby(i+k) = dyray(j) + k * ((dyray(j+1)-dyray(j)) / (nnew + 1))
          newbd(i+k) = depth(j) + k * ((depth(j+1)-depth(j)) / (nnew + 1))
        END DO

        i = i + nnew + 1


      END DO

!     Put the last node in the array
      newbx(i) = dxray(indxs(2))
      newby(i) = dyray(indxs(2))
      newbd(i) = depth(indxs(2))

!     Calculate spacing at start and end of segment
      IF (igridtype.eq.0) then ! latlon
        call haversine(dyray(indxs(1)-1),dxray(indxs(1)-1),dyray(indxs(1)),dxray(indxs(1)), dist1)
        call haversine(dyray(indxs(2)+1),dxray(indxs(2)+1),dyray(indxs(2)),dxray(indxs(2)), dist2)
      ELSE
        dist1 = sqrt((dxray(indxs(1)-1)-dxray(indxs(1)))**2 + (dyray(indxs(1)-1)-dyray(indxs(1)))**2)
        dist2 = sqrt((dxray(indxs(2))-dxray(indxs(2)+1))**2 + (dyray(indxs(2))-dyray(indxs(2)+1))**2)
      END IF


!     Pick out the indexes to use in the increasing spacing
!     And the increase

      nspc = floor((i-1) / ((dist1+dist2)/2)) ! Number of spacings between the two nodes
      dist = (dist2-dist1) / (nspc) ! Increase of distance between each node
      newbind(1) = 1
      k=1 ! Stores the amount of increments to add
      newbind(nspc+1) = i

      IF (dist2.gt.dist1 ) THEN ! The distance is increasing - go from the start
        DO j=2,nspc-1
          newbind(j)=floor(dist1*(j-1)+dist*k)
          k = k + j
        END DO
        newbind(nspc) = floor(( newbind(nspc-1)+newbind(nspc+1) )/2.0)

      ELSE ! The distance is decreasing - start from the end of the segment
        DO j=2,nspc-1
          newbind(nspc+2-j)=i-floor(dist2*(j-1)-dist*k)
          k = k + j
        END DO
        newbind(2) = floor(( newbind(1)+newbind(3) )/2.0)
      END IF
      

!     Reassign the new bnd
      DO i=1,nspc+1
        newbx(i) = newbx(newbind(i))
        newby(i) = newby(newbind(i))
        newbd(i) = newbd(newbind(i))
      END DO

      i = nspc+1

!     Assign number of added nodes to nnew
      nnew = i - (indxs(2)-indxs(1)) - 1

!     Move the array to make room for the new array
      IF (nnew.lt.0) THEN ! Less nodes
        DO j=indxs(2)+1,ITOT
          dxray(j+nnew) = dxray(j)
          dyray(j+nnew) = dyray(j)
          depth(j+nnew) = depth(j)
          code(j+nnew) = code(j)
        END DO
      ELSE IF (nnew.gt.0) THEN ! More nodes
        DO j=ITOT,indxs(2)+1,-1
          dxray(j+nnew) = dxray(j)
          dyray(j+nnew) = dyray(j)
          depth(j+nnew) = depth(j)
          code(j+nnew) = code(j)
        END DO
      END IF


!     Input new array
      dxray(indxs(1):indxs(1)+i-1)=newbx(1:i)
      dyray(indxs(1):indxs(1)+i-1)=newby(1:i)
      depth(indxs(1):indxs(1)+i-1)=newbd(1:i)
      code(indxs(1)+1:indxs(1)+i-1)=code(indxs(1))


!     Add the number of added nodes to the bnd segment, ITOTs and TotCoords
      PtsThisBnd(bnd) = PtsThisBnd(bnd)+nnew
      ITOT = ITOT + nnew
      TotCoords = TotCoords + nnew


!     Draw the new lines and symbols
      call PigDrawBndSymbols( i , newbx, newby )
      call PigDrawLine( i , newbx, newby, 3 )
      call PigDrawClrSymbol(dxray(indxs(1)-1), dyray(indxs(1)-1), 'B')
      call PigDrawClrSymbol(dxray(indxs(1)+i), dyray(indxs(1)+i), 'B')

      RETURN
      END

!-----------------------------------------------------------------------*
!*--------------------END NODEMOD.FOR-----------------------------------*

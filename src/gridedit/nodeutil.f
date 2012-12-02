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

C*----------------------------------------------------------------------*
C                       NODEUTIL.FOR                                    *
C       This module contains procedures for node manipulation, mostly   *
C       as utility routines for operations in NodeMod.For               *
C       ROUTINES - CheckNode, ManualBdPos, RedrawMark,                  *
C                  LocateBndPos, AddBdConfirm,                          *
C       FUNCTIONS - TypeOfNode, BndNodePos.                             *
C*----------------------------------------------------------------------*
C*----------------------------------------------------------------------*
      SUBROUTINE LocateBndPos (newbdx,newbdy,dataidx,boundnum,confirm)
C PURPOSE: To locate the position in dxray(), dyray(), depth() that a new
C          boundary node should be stored. Nodes already existing at indices
C          'dataidx' to 'TotCoords' must be shuffled ahead one index position
C          each in dxray(), dyray(), depth() to allow room for new node to 
C          be stored at index 'dataidx'. This is not done here.
C   GIVEN: newbdx, newbdy = (x,y) coordinates of new node whose index position
C                           is to be found.
C RETURNS: dataidx = the index to dxray(), dyray(), depth() where the new node
C                    should be stored. *NOTE* The node is not stored yet.
C                  = 0 if location not found
C          boundnum = boundary section number where dataidx occurs.
C                   = 0 if location not found
C          confirm = TRUE if location found & accepted by user, else = FALSE
C----------------------------------------------------------------------------

      use MainArrays

C - "INCLUDES"
      include '../includes/graf.def'

C - PASSED VARIABLES 
      REAL newbdx, newbdy
      integer dataidx, boundnum
      LOGICAL confirm

C - LOCAL VARIABLES
      REAL curx1, cury1, curx2, cury2, xctr, yctr
      REAL dist, tpside, btside, rtside, lfside
      integer ndx1, ndx2, tmpndx, tmp1, tmp2
      integer endbounds, curbound, curstart, curbndpts
      LOGICAL posfound, bndchange, firstlast
      CHARACTER*80 mess

C------------------START ROUTINE---------------------------------------

      boundnum = 0
      dataidx = 0
      ndx1 = 0
      ndx2 = 1
      endbounds = TotCoords - TotIntPts
      curbound = 1
      curstart = 1
      curbndpts = 1
      firstlast = .FALSE.
      bndchange = .FALSE.
      posfound = .FALSE.
      DO WHILE ( ( .NOT. posfound ) .AND. ( ndx2 .lt. endbounds ) )
        ndx2 = ndx2 + 1
        ndx1 = ndx2 - 1
        curbndpts = curbndpts + 1
        curx1 = dxray(ndx1)
        cury1 = dyray(ndx1)
        curx2 = dxray(ndx2)
        cury2 = dyray(ndx2)
C         - define a box based on the approx. distance between 2 bdy nodes
c         dist = ( ABS(curx1-curx2) + ABS(cury1-cury2) ) / 2.0
        dist = ( ABS(curx1-curx2) + ABS(cury1-cury2) ) * 0.45
        IF ( bndchange ) THEN
C           - ndx1 & ndx2 are straddling boundary change, curbound = new bdy
C           - assume change to island boundary, check first & last
          tmpndx = ndx2
          ndx2 = ndx1
          ndx1 = curstart
C           - now checking first & last nodes of boundary section
          firstlast = .TRUE.
          curx1 = dxray(ndx1)
          cury1 = dyray(ndx1)
          curx2 = dxray(ndx2)
          cury2 = dyray(ndx2)
          tmp1 = ndx1
          tmp2 = ndx2
C           - redefine distance
c           dist = ( ABS(curx1-curx2) + ABS(cury1-cury2) ) / 2.0
          dist = ( ABS(curx1-curx2) + ABS(cury1-cury2) ) * 0.45
          curstart = tmpndx
          ndx2 = tmpndx
          curbndpts = 1
          bndchange = .FALSE.
        ENDIF
C           - ( bndchange )
        xctr = ( curx1 + curx2 ) / 2.0
        yctr = ( cury1 + cury2 ) / 2.0
        tpside = yctr + dist
        btside = yctr - dist
        rtside = xctr + dist
        lfside = xctr - dist
        IF ( (newbdx .le. rtside) .AND. (newbdx .ge. lfside) ) THEN
          IF ( (newbdy .le. tpside) .AND. (newbdy .ge. btside) ) THEN
C             - new node is within the defined box
            IF ( firstlast ) THEN
c               firstlast = .FALSE.
            curbound = curbound - 1
            ndx1 = tmp1
            ndx2 = tmp2
            ENDIF
            confirm = .FALSE.
            call AddBdConfirm(ndx1,ndx2,newbdx,newbdy,confirm)
            IF ( confirm ) THEN
            dataidx = ndx2
            IF ( firstlast ) dataidx = ndx1
            boundnum = curbound
            posfound = .TRUE.
            ELSE
            boundnum = 0
                dataidx = 0
      mess = 'Unable to connect new node to neighbours;'//
     +   ' Try a closer Add, then Move node ?:'
      call PigMessageOK( mess, 'BndAdd' )
!               mess = 'Pick location manually ?:'
!                 ans = PigCursYesNo ( mess )
!               call PigEraseMessage
!            IF ( ans(1:1) .eq. 'Y' ) THEN 
!              confirm = .FALSE.
!              call ManualBdPos(newbdx,newbdy,ndx1,ndx2,
!     +                                    curbound,confirm)
!              dataidx = ndx2
!              boundnum = curbound
!              posfound = .TRUE.
!            ELSE
              posfound = .TRUE.
              confirm = .FALSE.
!            ENDIF
C                 - ( ans = Y )
            ENDIF
C               - ( confirm )
          ENDIF
C             - ( newbdx in defined box )
        ENDIF
C           - ( newbdy in defined box )

          pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

        IF ( curbndpts .eq. PtsThisBnd(curbound) ) THEN
          bndchange = .TRUE.
          curbound = curbound + 1
        ENDIF
        firstlast = .FALSE.
      END DO
C         - ( NOT posfound AND ndx2 lt endbounds )

      IF ( .NOT. posfound ) THEN
        confirm = .FALSE.
        boundnum = 0
          dataidx = 0
      mess = 'Unable to connect new node to neighbours;'//
     +   ' Try a closer Add, then Move node ?:'
      call PigMessageOK( mess, 'BndAdd' )
!           ans = PigCursYesNo (mess)
!         call PigEraseMessage
!         IF ( ans(1:1) .eq. 'Y' ) THEN 
!          confirm = .FALSE.
!          call ManualBdPos(newbdx,newbdy,ndx1,ndx2,curbound,confirm)
!          dataidx = ndx2
!          boundnum = curbound
!        ENDIF
      ENDIF

      RETURN
      END

C-----------------------------------------------------------------------*

      SUBROUTINE AddBdConfirm(idx1,idx2,cnfX,cnfY,confirm)

C PURPOSE: To confirm with the user that located position for new boundary
C          node is OK.
C   GIVEN: idx1 = index to dxray(), dyray() of first neighbour to new node.
C          idx2 = index to dxray(), dyray() of second neighbour to new node.
C          cnfX, cnfY = (x,y) coordinates of new boundary node whose
C                       location is to be confirmed.
C RETURNS: confirm = TRUE if user confirms location is OK.
C-----------------------------------------------------------------------*

      use MainArrays

C - PASSED VARIABLES
      REAL cnfX, cnfY
      integer idx1, idx2
      LOGICAL confirm

C - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'

C - LOCAL VARIABLES
      REAL xmarks(3), ymarks(3), tempx(2), tempy(2)
      integer curlineclr
      CHARACTER*80 ans, cstr
      CHARACTER*1 PigCursYesNo

C----------------------START ROUTINE------------------------------------*

      confirm = .FALSE.
      xmarks(1) = dxray(idx1)
      ymarks(1) = dyray(idx1)
      xmarks(2) = cnfX
      ymarks(2) = cnfY
      xmarks(3) = dxray(idx2)
      ymarks(3) = dyray(idx2)

C       - select main window
      call PigSetWindowNum ( MAINWIN )
C       - remember current color setting
      call PigGetLineColour (curlineclr)

C       - draw connection lines to neighbours
      call PigSetLineColour ( orange )
      call PigDrawPolyline ( 3, xmarks, ymarks )

C       - get user confirmation
      cstr = 'Correct connections for new node ?:'
        ans = PigCursYesNo (cstr)
      IF ( ans(1:1) .eq. 'Y' ) THEN
C         - connection OK
C         -- draw line to erase original boundary connection
        call PigSetWindowNum ( MAINWIN )
        call PigSetLineColour ( backgr )
        tempx(1) = xmarks(1)
        tempy(1) = ymarks(1)
        tempx(2) = xmarks(3)
        tempy(2) = ymarks(3)
        call PigDrawPolyline ( 2, tempx, tempy )
        confirm = .TRUE.
        call PigEraseMessage
      ELSE
C         - connection NOT OK
        confirm = .FALSE.
        call PigEraseMessage
C         - remove the connection lines
        call PigSetWindowNum ( MAINWIN )
        call PigSetLineColour ( backgr )
        call PigDrawPolyline ( 3, xmarks, ymarks )
      ENDIF
C       - set colors back to current values
      call PigSetLineColour ( curlineclr )

      RETURN
      END

C-----------------------------------------------------------------------*

      SUBROUTINE CheckNode ( xpt, ypt, indx, ierr, nodetype )

C PURPOSE: To validate the existence of a node.
C          Consideration for existence:
C               within 'coinrng' of (xpt,ypt), units in screen co-ordinates.
C          If given (xpt,ypt) exists, return actual co-ordinate
C           i.e. dxray(i),dyray(i).
C   GIVEN: (xpt,ypt) = point to be checked in screen co-ordinate.
C          dxray(),dyray() = arrays containing displayed (x,y) co-ordinates.
C          nodetype = 1, check interior nodes
C                     2, check boundary nodes
C                     ( <1 or >2 ), check all nodes
C          TotCoords = total number of data points on screen.
C RETURNS: ierr = 1 if data point exist (node found), 0 if not.
C          (xpt,ypt) = the exact stored co-ordinates, if found
C          indx = index of (xpt,ypt) in dxray(), dyray(),
C               = 0 if not exist
C----------------------------------------------------------------------------

      use MainArrays

C - PASSED VARIABLES 
      REAL xpt, ypt
      integer ierr, indx, nodetype

C - COMMON BLOCKS
C       - NODECHECK stores indication of direction of search thru arrays,
C       -- toggles each call so that no nodes are hard to locate with cursor.
C        = TRUE if search is from high index to low
C        = FALSE if search is from low index to high
      LOGICAL SrchSwitch
      COMMON /NODECHECK/ SrchSwitch

C - LOCAL VARIABLES
      REAL hxlim, lxlim, hylim, lylim,xpt0,ypt0
C          - the x,y high and low limits
      REAL CWXL, CWXH, CWYL, CWYH,nrange 
      integer i, Startidx, Endidx
C          - counter and index limits for loop
      LOGICAL in_box

C - SAVED VALUES
*        SAVE /NODECHECK/

C------------------START ROUTINE---------------------------------------

!     - toggle search direction
      SrchSwitch = .false.  !.NOT. SrchSwitch

!     - determine indices for type of nodes to check
      IF ( nodetype .eq. 1 ) THEN
!       - check interior nodes only
        Startidx = ( TotCoords - TotIntPts ) + 1
        Endidx = TotCoords
      ELSE IF ( nodetype .eq. 2 ) THEN
!       - check boundary nodes only
        Startidx = 1
        Endidx = ( TotCoords - TotIntPts )
      ELSE
!       - check all nodes as default
        Startidx = 1
        Endidx = TotCoords
      ENDIF

      indx = 0
      ierr  = 0
!      i = Startidx
      xpt0 = xpt
      ypt0 = ypt
      pdist0 = 1.E30
!     - set upper and lower limits on the search      
      call PigGetWorldCoordinates(CWXL, CWXH, CWYL, CWYH)
      nrange = 0.05*(CWXH-CWXL)
      hxlim = xpt + nrange
      lxlim = xpt - nrange
      hylim = ypt + nrange
      lylim = ypt - nrange
      
      DO i = Startidx, Endidx
        IF( IN_BOX(dxray(i),dyray(i)) ) THEN
          IF ((dxray(i) .le. hxlim) .AND. (dxray(i) .ge. lxlim)) THEN
            IF ((dyray(i) .le. hylim) .AND. (dyray(i) .ge. lylim)) THEN
              pdist = (xpt0-dxray(i))**2 + (ypt0-dyray(i))**2
              if(pdist.lt.pdist0) then
                pdist0 = pdist
                xpt = dxray(i)
                ypt = dyray(i)
                indx = i
                ierr = 1
              endif
            ENDIF
          ENDIF
        ENDIF
      END DO

      END

C*--------------------------------------------------------------------------*
      integer FUNCTION TypeOfNode ( indx )
C PURPOSE: To return the type (interior or boundary) of a node given
C          the nodes index.
C   GIVEN: indx = index of node to type
C RETURNS: TypeOfNode = 1 if interior node,
C                     = 2 if boundary node.
C                     (= 0 if error calculating indexes)
C*----------------------------------------------------------------------*

      use MainArrays

C - PASSED VARIABLES
      integer indx

C------------------START ROUTINE---------------------------------

      IF ( indx .gt. (TotCoords - TotIntPts) ) THEN
C         - interior node
        TypeOfNode = 1
      ELSE

cc extra check
cc              ELSEIF ( (indx .ge. 1) .AND.
cc     +             (indx .le. (TotCoords-TotIntPts)) ) THEN

C         - boundary node
        TypeOfNode = 2

cc error flag
cc        ELSE
cc           TypeOfNode = 0

      ENDIF
C         - ( indx > TotCoords - TotIntPts )

      RETURN
      END

C-----------------------------------------------------------------------*
      integer FUNCTION BndNodePos ( ndx, bound, bpfirst, bplast )
C PURPOSE: To determine if a given boundary node is the first, last, or
C          a middle node of a boundary.
C   GIVEN: ndx = the index to data arrays of the node,
C          Node data in Common NODES in NODESTOR.INC.
C RETURNS: BndNodePos = 0 if a middle node in boundary,
C                     = 1 if first node in boundary,
C                     = 2 if last node in boundary,
C          bound = boundary section # where node is located,
C          bpfirst = index of first node in boundary,
C          bplast = index of last node in boundary.
C EFFECTS: None.
C WRITTEN: Sept 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays

C - PASSED VARIABLES
      integer ndx, bound, bpfirst, bplast

C - COMMON BLOCKS

C - LOCAL VARIABLES
      integer tempval, i

C-----------------START ROUTINE----------------------------------------

C       - determine boundary section where node is located
      tempval = 0
      i = 0

        pTStHISbND(tOTbNDYS+1) = tOTiNTpTS

      DO WHILE ( i .lt. TotBndys + 1 )
        i = i + 1
        tempval = tempval + PtsThisBnd(i)
        IF ( ndx .le. tempval ) THEN
          bound = i
C           - now end the loop
          i = 9999
        ENDIF
      END DO
C         - ( i > TotBndys + 1 )
C       - determine first & last node in boundary
      IF ( bound .lt. 2 ) THEN
C         - outer boundary
        bpfirst = 1
        bplast = PtsThisBnd(1)
      ELSE
C         - island boundary
        bpfirst = 0
        DO i = 1, bound - 1
          bpfirst = bpfirst + PtsThisBnd(i)
        END DO
        bpfirst = bpfirst + 1
        bplast = bpfirst + PtsThisBnd(bound) - 1
      ENDIF
C         - ( bound < 2 )
C       - test if node is 1st or last in boundary
      IF ( ndx .eq. bpfirst ) THEN
C         - first node
        BndNodePos = 1
      ELSE IF ( ndx .eq. bplast ) THEN
C         - last node
        BndNodePos = 2
      ELSE
C         - a middle node
        BndNodePos = 0
      ENDIF
C         - ( ndx = bpfirst )

      RETURN
      END

C*----------------------------------------------------------------------*
C*---------------------END NODEUTIL.FOR---------------------------------*

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
C                       NODEDISP.FOR                                    *
C       This module contains routines to display the nodes.             *
C       ROUTINES: DisplayNodes.                                         *
C*----------------------------------------------------------------------*
C*---------------------------------------------------------------------------*
      SUBROUTINE DisplayNodes ( )
C PURPOSE: This routine displays NODES read from node file .
C          If reset is true, window will be set to start-up default value.
C          If reset is false, newly entered window value, from common
C          area /WLIMIT/, will be used.
C          (If outlineonly, only boundary nodes will be displayed)
C          If outlineonly, only boundaries will be displayed
C GIVEN:   In Common;
C          dxray(), dyray(), depth() = arrays of coordinates & depths
C          TotCoords = number of nodes
C          TotBndys = number of boundaries
C          PtsThisBnd() = number of nodes in each boundary
C          TotIntPts = number of non-boundary nodes
C          LOGICAL reset = .TRUE. if first display
C RETURNS: Nodes are displayed on screen.
C*----------------------------------------------------------------------*

      use MainArrays

C  - PARAMETERS (constants)
        integer limwin
        REAL*4 seconds
        PARAMETER ( limwin = 50 )
        PARAMETER ( seconds = 2.0 )

C  - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'
      include '../includes/edpolys.inc'

C - COMMON BLOCKS
C   - PolyDisplay indicates polygons to display on redraw.
C       PolyDisplay = 0 = display active polygon only.
C                   = 1 = display all polygons.
C                   = 2 = display NO polygons.
      integer PolyDisplay
      COMMON /POLYSTATUS/ PolyDisplay

C    - current window limits
      REAL   cwxl, cwxh, cwyl, cwyh
      COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh
C
      REAL   wxl(limwin), wxh(limwin), wyl(limwin), wyh(limwin)
        integer level
        COMMON /WLIMIT/ wxl, wxh, wyl, wyh, level

C       - MARKERS stores markers placed by user
        integer NumMarks
        LOGICAL MarksOn
      REAL MarksX(maxmarks), MarksY(maxmarks)
      COMMON /MARKERS/ NumMarks, MarksOn, MarksX, MarksY

C - COINBOX stores coincident node range and location of range box display
      REAL coinrng, boxlocx, boxlocy, rngbox(5), rngboy(5)
      COMMON /COINBOX/ coinrng, boxlocx, boxlocy, rngbox, rngboy

C - COINDISPLAY stores indication of whethter to draw CoinArray & coinrangebox
      LOGICAL showcoin, showbox
      COMMON /COINDISPLAY/ showcoin, showbox

C -  (Only boundary nodes are displayed when OUTLINEONLY is .TRUE.)
C -  Only boundaries are displayed when OUTLINEONLY is .TRUE.
      LOGICAL OUTLINEONLY
      COMMON /OUTLINE/ OUTLINEONLY

      logical FlagN
      logical FlagG
      logical FlagD
      logical FlagC
      common /MenuDrawFlags/ FlagN,FlagG,FlagC,FlagD

C  - LOCAL VARIABLES
c      REAL x1, x2, y1, y2,
      REAL nodex(1), nodey(1)
      integer ii, jj, SecClrStrt, seccolor, markidx
      integer intcolor, bndcolor, marktype, PrevLineColour
      LOGICAL IN_BOX
c        logical start
c        DATA    start /.FALSE./

C --------------------START ROUTINE-----------------------------------

      call PigGetLineColour(PrevLineColour)

      marktype = NodeMType

      intcolor = NodeIColor
      bndcolor = NodeBColor
      seccolor = NodeSColor

C    - set index for secondary color to start
      SecClrStrt = NodeSIndex

      IF (.not.outlineonly) THEN
C      - set marker type
       call PigSetSymbolNumber ( marktype )

       IF ( level .eq. 0 ) THEN
C       - display boundary nodes
C       - set marker color for boundary nodes
      call PigSetSymbolColour ( bndcolor )
      markidx = 0
      DO jj = 1, TotBndys
          DO ii = 1, PtsThisBnd(jj)
c           - add protection from array bounds error if PtsThisBnd(jj) too big
            if( (markidx+ii).le.MaxPts) then
             nodex(1) = dxray(markidx + ii)
             nodey(1) = dyray(markidx + ii)
             IF ( In_Box(nodex(1),nodey(1)) ) THEN
            IF ((markidx + ii) .ge. SecClrStrt)
     +            call PigSetSymbolColour(seccolor)
            call DepthClrCode ( 2, depth(markidx + ii) )
            call PigDrawSymbols ( 1, nodex, nodey )
             ENDIF
            endif
        END DO
        markidx = markidx + PtsThisBnd(jj)
      END DO
C       - display interior nodes
C       - set marker color for interior nodes
      call PigSetSymbolColour ( intcolor )
      DO ii = 1, TotIntPts
c         - add protection from array bounds error if PtsThisBnd(jj) too big
          if( (markidx+ii).le.MaxPts) then
           nodex(1) = dxray(markidx + ii)
           nodey(1) = dyray(markidx + ii)
           if (IN_BOX(nodex(1),nodey(1))) then
            IF ((markidx + ii) .ge. SecClrStrt)
     +             call PigSetSymbolColour(seccolor)
            call DepthClrCode ( 1, depth(markidx + ii) )
            call PigDrawSymbols ( 1, nodex, nodey )
           endif
          endif
      END DO
      ELSE
C       - display boundary nodes
C       - set marker color for boundary nodes
      call PigSetSymbolColour ( bndcolor )
        markidx = 0
      DO jj = 1, TotBndys
        DO ii = 1, PtsThisBnd(jj)
c          - add protection from array bounds error if PtsThisBnd(jj) too big
           if( (markidx+ii).le.MaxPts) then
            nodex(1) = dxray(markidx + ii)
          nodey(1) = dyray(markidx + ii)
            if(IN_BOX(nodex(1),nodey(1))) then
              IF ((markidx + ii) .ge. SecClrStrt)
     +            call PigSetSymbolColour(seccolor)
            call DepthClrCode ( 2, depth(markidx + ii) )
              call PigDrawSymbols ( 1, nodex, nodey )
            endif
           endif
        END DO
C           - ( ii = 1, PtsThisBnd(jj) )
        markidx = markidx + PtsThisBnd(jj)
        END DO

C     - set marker color for interior nodes
      call PigSetSymbolColour ( intcolor )
C     - display interior nodes
      DO ii = 1, TotIntPts
c         - add protection from array bounds error if PtsThisBnd(jj) too big
          if( (markidx+ii).le.MaxPts) then
           nodex(1) = dxray(markidx + ii)
         nodey(1) = dyray(markidx + ii)
         if(IN_BOX(nodex(1),nodey(1))) then
            IF ((markidx + ii) .ge. SecClrStrt)
     +              call PigSetSymbolColour(seccolor)
             call DepthClrCode ( 1, depth(markidx + ii) )
             call PigDrawSymbols ( 1, nodex, nodey )
           ENDIF
          endif
      END DO
       ENDIF
      endif

C     - DISPLAY OTHER FEATURES AS REQUIRED...

C     - contours & boundaries
      if(FlagD) then
        call DispContBound
      endif

C     - polygons
         do j=1,numpolys
           jj = j
           call DisplayPoly ( jj )
         enddo

!      IF ( reset ) PolyDisplay = 0
!      IF ( PolyDisplay .eq. 0 ) THEN
C       - display active polygon if any
!      call DisplayPoly ( actvpoly )
!      ELSE IF ( PolyDisplay .eq. 1 ) THEN
C       - display all polygons
!      call AllPolys ( .TRUE. )
!      ELSE IF ( PolyDisplay .eq. 2 ) THEN
C       - remove all polygons from display
!      call AllPolys ( .FALSE. )
!      ENDIF
C       - ( PolyDisplay = 0 )

C     - tooclose node markers
      IF ( showcoin ) THEN
c      call MarkCoins
      ENDIF
      IF ( showbox ) THEN
c      call DisplayRangeBox( .TRUE. )
      ENDIF

C     - boundary connections
      call ConnectBnd

C     - markers
      IF ( MarksOn ) THEN
C         - set marker type & display markers
        call PigSetSymbolNumber ( MarkMType )
        call PigSetSymbolColour ( MarkColor )
*       Why is the symbol colour being set specifically to yellow, right
*       after being set to MarkColor? commented out by (AM) June93
*          call PigSetSymbolColour ( YELLOW )
        call PigSetWindowNum ( MAINWIN )
        call PigDrawSymbols ( NumMarks, MarksX, MarksY )
      ENDIF
      call PigSetLineColour(PrevLineColour)
      END

C*----------------------------------------------------------------------*
      SUBROUTINE ConnectBnd
C PURPOSE: To display the connections between boundary nodes.
C   GIVEN: In Common BNDCONN;
C               showbndconn = TRUE if boundary connections are to be shown,
C                           = FALSE if boundary connections not to be shown,
C          Boundary data in NODESTOR.INC.
C RETURNS: None.
C EFFECTS: If showbndconn is TRUE  connection lines are drawn between boundary
C          nodes of the same boundary.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*

      use MainArrays


C - "INCLUDES"
      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

C - COMMON BLOCKS
C       - BNDCONN stores indication of when to show boundary connections
        LOGICAL showbndconn
!       COMMON /BNDCONN/ showbndconn

C - LOCAL VARIABLES
        integer i, bndsofar
      integer PrevColour

      integer linecolours(8)
      integer mod

C---------------START ROUTINE--------------------------------------

      call PigGetLineColour(PrevColour)
      linecolours(1) = YELLOW
      linecolours(2) = RED
      linecolours(3) = GREEN
      linecolours(4) = CYAN
      linecolours(5) = LTGRAY
      linecolours(6) = WHITE
      linecolours(7) = ORANGE
      linecolours(8) = DKGREEN
      showbndconn = .true.
      IF ( showbndconn ) THEN
        call PigSetWindowNum ( MAINWIN )
        call PigSetLineColour ( LTGRAY )
      bndsofar = 0
      DO i = 1, TotBndys
          call PigSetLineColour ( linecolours(MOD(i,8)+1) )
c ensure do not overrun arrays dxray and dyray
          if  (    (bndsofar+1.le.MaxPts)
     +        .and.(PtsThisBnd(i).gt.0)
     +        ) then
              call PigDrawPolyline(
     +            MIN(PtsThisBnd(i),MaxPts-(bndsofar+1)),
     +            dxray(bndsofar+1), dyray(bndsofar+1)
     +            )
          endif
        bndsofar = bndsofar + PtsThisBnd(i)
      END DO
      ENDIF
      call PigSetLineColour(PrevColour)
      END
C-----------------------------------------------------------------------*

      SUBROUTINE DepthClrCode ( ntype, ndep )

C PURPOSE: To be called before each node is drawn in DisplayNodes. Checks
C          if color coding by depths is turned on for that node type 
C          ( interior or boundary ) and if so then sets PolyMarker color
C          to the appropriate color. If color coding for only one type of
C          node is on, then nodes of the other type are drawn in green.
C   GIVEN: ntype = 1 if calling routine is currently drawing interior nodes,
C                = 2 if calling routine is currently drawing boundary nodes,
C          ndep = depth at node about to be displayed by calling routine,
C          In Common DPTHCOLORS;
C               rngc() = array of colors to use for matching depth values
C                        in rngv(),
C               rngv() = array of upper limits to depth range values that
C                        determine color code.
C               inton = TRUE if depth color coding of interior nodes is ON,
C                       else FALSE,
C               bndon = TRUE if depth color coding of boundary nodes is ON,
C                       else FALSE,
C RETURNS: PolyMarker color may be set.
C EFFECTS: If ntype = 1 and inton is TRUE, OR if ntype = 2 and bndon is TRUE
C          then a call to GSS/GKS graphics routine PigSetSymbolColour is made to set poly
C          marker color to match node depth to depth range color code.
C WRITTEN: Aug 1990 by JDM for NODER.
C-----------------------------------------------------------------------*


C - "INCLUDES"
      include '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

C - PASSED VARIABLES
      REAL ndep
      integer ntype
C       -- 1 = interior, 2 = boundary

C - COMMON BLOCKS
C       - DPTHCOLORS stores parameters for color coding nodes by depth
      integer rngc(16)
      REAL rngv(16)
      LOGICAL inton, bndon
      COMMON /DPTHCOLORS/ rngc, rngv, inton, bndon

C - LOCAL VARIABLES
      integer i, ncon
      REAL lowlim

C-----------------START ROUTINE----------------------------------------

      ncon = NumCntValues(DataType)
C       - set depth code color if on
      do j=1,ncon
        rngc(j) = CntColors(DataType,j)
        rngv(j) = CntValues(DataType,j)
      enddo
        
      IF ( ntype .eq. 1 ) THEN
C         - interior nodes, check if color coding ON
        IF ( inton ) THEN
C           - determine color for node's depth value
          lowlim = -9999
          DO i = 1, ncon-1
            IF ( (ndep .le. rngv(i)) .AND. (ndep .gt. lowlim) )
     +                  call PigSetSymbolColour ( rngc(i) )
            lowlim = rngv(i)
          END DO
          IF (ndep.gt.rngv(ncon)) call PigSetSymbolColour(rngc(ncon))
        ENDIF
C           - ( inton )
      ELSE
C         - boundary nodes, check if color coding ON
        IF ( bndon ) THEN
C           - determine color for node's depth value
          lowlim = -9999
          DO i = 1, ncon-1
            IF ( (ndep .le. rngv(i)) .AND. (ndep .gt. lowlim) )
     +                  call PigSetSymbolColour ( rngc(i) )
            lowlim = rngv(i)
          END DO
          IF (ndep.gt.rngv(ncon)) call PigSetSymbolColour(rngc(ncon))
        ENDIF
C           - ( bndon )
      ENDIF
C         - ( ntype = 1 )

C       - if ntype = boundary & inton = T & bndon = F, then color green
      IF ( (ntype .eq. 2) .AND. inton .AND. (.NOT. bndon) )
     +          call PigSetSymbolColour ( green )

C       - if ntype = interior & inton = F & bndon = T, then color green
      IF ( (ntype .eq. 1) .AND. bndon .AND. (.NOT. inton) )
     +          call PigSetSymbolColour ( green )

      RETURN
      END

C-----------------------------------------------------------------------*

C*-------------------END NODEDISP.FOR-----------------------------------*

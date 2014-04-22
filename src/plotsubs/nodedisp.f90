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

!*----------------------------------------------------------------------*
!                       NODEDISP.F90                                    *
!       This module contains routines to display the nodes.             *
!       ROUTINES: DisplayNodes, ConnectBnd,  DepthClrCode                                      *
!*----------------------------------------------------------------------*
!*---------------------------------------------------------------------------*
      SUBROUTINE DisplayNodes ( )
! PURPOSE: This routine displays NODES read from node file .
!          If outlineonly, only boundaries will be displayed
! GIVEN:   In Common;
!          dxray(), dyray(), depth() = arrays of coordinates & depths
!          TotCoords = number of nodes
!          TotBndys = number of boundaries
!          PtsThisBnd() = number of nodes in each boundary
!          TotIntPts = number of non-boundary nodes
! RETURNS: Nodes are displayed on screen.
!*----------------------------------------------------------------------*

      use MainArrays
      
      implicit none

!  - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'
      include '../includes/edpolys.inc'

!      integer PolyDisplay
!      COMMON /POLYSTATUS/ PolyDisplay

!       - MARKERS stores markers placed by user
!      integer NumMarks
!      LOGICAL MarksOn
!      REAL MarksX(maxmarks), MarksY(maxmarks)
!      COMMON /MARKERS/ NumMarks, MarksOn, MarksX, MarksY

! - COINBOX stores coincident node range and location of range box display
!      REAL coinrng, boxlocx, boxlocy, rngbox(5), rngboy(5)
!      COMMON /COINBOX/ coinrng, boxlocx, boxlocy, rngbox, rngboy

! - COINDISPLAY stores indication of whethter to draw CoinArray & coinrangebox
!      LOGICAL showcoin, showbox
!      COMMON /COINDISPLAY/ showcoin, showbox

! -  Only boundaries are displayed when OUTLINEONLY is .TRUE.
      LOGICAL OUTLINEONLY
      COMMON /OUTLINE/ OUTLINEONLY

      logical FlagN
      logical FlagG
      logical FlagD
      logical FlagC
      common /MenuDrawFlags/ FlagN,FlagG,FlagC,FlagD

!  - LOCAL VARIABLES
      REAL nodex(1), nodey(1)
      integer ii, j, jj, SecClrStrt, seccolor, markidx
      integer intcolor, bndcolor, marktype, PrevLineColour
      LOGICAL IN_BOX

! --------------------START ROUTINE-----------------------------------

      call PigGetLineColour(PrevLineColour)

      marktype = NodeMType

      intcolor = NodeIColor
      bndcolor = NodeBColor
      seccolor = NodeSColor

!    - set index for secondary color to start
      SecClrStrt = NodeSIndex

!     - boundary connections
      call ConnectBnd

!       - set marker type and color for boundary nodes
      call PigSetSymbolNumber ( marktype )
      call PigSetSymbolColour ( bndcolor )

      markidx = 0
      DO jj = 1, TotBndys
        nodex(1) = dxray(markidx + 1) !first node
        nodey(1) = dyray(markidx + 1)
        IF ( In_Box(nodex(1),nodey(1)) ) THEN
          call PigSetSymbolNumber ( square )
          call PigSetSymbolColour ( white )
          call PigDrawSymbols ( 1, nodex, nodey )
          call PigSetSymbolNumber ( marktype )
          call PigSetSymbolColour ( bndcolor )
        endif

        IF (.not.outlineonly) THEN  !do the rest        
          DO ii = 2, PtsThisBnd(jj)
!           - add protection from array bounds error if PtsThisBnd(jj) too big
            if( (markidx+ii).le.MaxPts) then
              nodex(1) = dxray(markidx + ii)
              nodey(1) = dyray(markidx + ii)
              IF ( In_Box(nodex(1),nodey(1)) ) THEN
                if(FlagC) then
                  call DepthClrCode ( 2, depth(markidx + ii) )
                elseif ((markidx + ii) .eq. SecClrStrt) then
                  call PigSetSymbolColour(seccolor)
                endif
                call PigDrawSymbols ( 1, nodex, nodey )
              ENDIF
            endif
          END DO
        endif
        markidx = markidx + PtsThisBnd(jj)
      END DO
!       - display interior nodes
!       - set marker color for interior nodes
      IF (.not.outlineonly) THEN
        call PigSetSymbolNumber ( marktype )
        call PigSetSymbolColour ( intcolor )
        DO ii = 1, TotIntPts
!         - add protection from array bounds error if PtsThisBnd(jj) too big
          if( (markidx+ii).le.MaxPts) then
            nodex(1) = dxray(markidx + ii)
            nodey(1) = dyray(markidx + ii)
            if (IN_BOX(nodex(1),nodey(1))) then
              IF ((markidx + ii) .ge. SecClrStrt) call PigSetSymbolColour(seccolor)
              if(FlagC) call DepthClrCode ( 1, depth(markidx + ii) )
              call PigDrawSymbols ( 1, nodex, nodey )
            endif
          endif
        END DO        
      endif

!     - DISPLAY OTHER FEATURES AS REQUIRED...

!     - contours & boundaries
      if(FlagD) then
        call DispContBound
      endif

!     - polygons
      do j=1,numpolys
        jj = j
        call DisplayPoly ( jj )
      enddo

!     - markers
!      IF ( MarksOn ) THEN
!         - set marker type & display markers
!        call PigSetSymbolNumber ( MarkMType )
!        call PigSetSymbolColour ( MarkColor )
!        call PigSetWindowNum ( MAINWIN )
!        call PigDrawSymbols ( NumMarks, MarksX, MarksY )
!      ENDIF
      call PigSetLineColour(PrevLineColour)

      END

!*----------------------------------------------------------------------*
      
      SUBROUTINE ConnectBnd
      
! PURPOSE: To display the connections between boundary nodes.
!   GIVEN: In Common BNDCONN;
!               showbndconn = TRUE if boundary connections are to be shown,
!                           = FALSE if boundary connections not to be shown,
!          Boundary data in NODESTOR.INC.
! RETURNS: None.
! EFFECTS: If showbndconn is TRUE  connection lines are drawn between boundary
!          nodes of the same boundary.
! WRITTEN: Aug 1990 by JDM for NODER.
!-----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - "INCLUDES"
      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

! - COMMON BLOCKS
!       - BNDCONN stores indication of when to show boundary connections
      LOGICAL showbndconn
!     COMMON /BNDCONN/ showbndconn

! - LOCAL VARIABLES
      integer i, ii, startidx, endidx, bndsofar
      integer PrevColour
      real nx, ny
      integer linecolours(8)
      logical inside
      LOGICAL IN_BOX

!---------------START ROUTINE--------------------------------------

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
          if(BoundCCW) then
            call PigSetLineColour ( linecolours(BndryIndex(i)+2) )
          else
            call PigSetLineColour ( linecolours(MOD(i,8)+1) )
          endif
          startidx = 0
          inside = .false.
          DO ii = 1, PtsThisBnd(i)
            nx = dxray(bndsofar + ii)
            ny = dyray(bndsofar + ii)
            if (IN_BOX(nx,ny)) then
              if(startidx.eq.0) startidx = max(1,ii-1)+bndsofar
              endidx = min(PtsThisBnd(i),ii+1)+bndsofar
              inside = .true.
!              exit
            endif
          enddo
! ensure do not overrun arrays dxray and dyray
          if(inside) then
            if  (    (bndsofar+1.le.MaxPts).and.(PtsThisBnd(i).gt.0)) then
              call PigDrawPolyline( MIN(endidx-startidx+1,MaxPts-(bndsofar+1)),&
                                    dxray(startidx), dyray(startidx))
!              call PigDrawPolyline( MIN(PtsThisBnd(i),MaxPts-(bndsofar+1)),&
!                                    dxray(bndsofar+1), dyray(bndsofar+1))
            endif
          endif
          bndsofar = bndsofar + PtsThisBnd(i)
        END DO
      ENDIF
      
      call PigSetLineColour(PrevColour)
      
      END
!-----------------------------------------------------------------------*

      SUBROUTINE DepthClrCode ( ntype, ndep )

! PURPOSE: To be called before each node is drawn in DisplayNodes. Checks
!          if color coding by depths is turned on for that node type 
!          ( interior or boundary ) and if so then sets PolyMarker color
!          to the appropriate color. If color coding for only one type of
!          node is on, then nodes of the other type are drawn in green.
!   GIVEN: ntype = 1 if calling routine is currently drawing interior nodes,
!                = 2 if calling routine is currently drawing boundary nodes,
!          ndep = depth at node about to be displayed by calling routine,
!          In Common DPTHCOLORS;
!               rngc() = array of colors to use for matching depth values
!                        in rngv(),
!               rngv() = array of upper limits to depth range values that
!                        determine color code.
!               inton = TRUE if depth color coding of interior nodes is ON,
!                       else FALSE,
!               bndon = TRUE if depth color coding of boundary nodes is ON,
!                       else FALSE,
! RETURNS: PolyMarker color may be set.
! EFFECTS: If ntype = 1 and inton is TRUE, OR if ntype = 2 and bndon is TRUE
!          then a call to graphics routine PigSetSymbolColour is made to set poly
!          marker color to match node depth to depth range color code.
!-----------------------------------------------------------------------*

      implicit none

! - "INCLUDES"
!      include '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

! - PASSED VARIABLES
      REAL ndep
      integer ntype
!       -- 1 = interior, 2 = boundary


! - LOCAL VARIABLES
      integer i, j, ncon
      integer rngc(20)
      REAL rngv(20)
      REAL lowlim

!-----------------START ROUTINE----------------------------------------

      ncon = NumCntValues(DataType)
!       - set depth code color
      do j=1,ncon
        rngc(j) = CntColors(DataType,j)
        rngv(j) = CntValues(DataType,j)
      enddo
        
      IF ( ntype .eq. 1 ) THEN
!         - interior nodes, check if color coding ON
!        IF ( inton ) THEN
!           - determine color for node's depth value
          lowlim = -1.e30
          DO i = 1, ncon
            IF ( (ndep .le. rngv(i)) .AND. (ndep .gt. lowlim) ) then
              call PigSetSymbolColour ( rngc(i) )
            endif
            lowlim = rngv(i)
          END DO
          IF (ndep.gt.rngv(ncon)) call PigSetSymbolColour(rngc(ncon))
!        ENDIF
!           - ( inton )
      ELSE
!         - boundary nodes, check if color coding ON
!        IF ( bndon ) THEN
!           - determine color for node's depth value
          lowlim = -1.e30
          DO i = 1, ncon
            IF ( (ndep .le. rngv(i)) .AND. (ndep .gt. lowlim) ) then
              call PigSetSymbolColour ( rngc(i) )
            endif
            lowlim = rngv(i)
          END DO
          IF (ndep.gt.rngv(ncon)) call PigSetSymbolColour(rngc(ncon))
!        ENDIF
!           - ( bndon )
      ENDIF
!         - ( ntype = 1 )

      RETURN
      END

!-----------------------------------------------------------------------*

!*-------------------END NODEDISP.F90-----------------------------------*

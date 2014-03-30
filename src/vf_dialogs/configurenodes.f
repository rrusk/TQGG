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
C                           NODECONF.FOR                                    *
C     This module contains the controlling functions for the configuration  *
C     options. The setting of colors for boundaries, contours, and grid     *
C     lines and the display and toggling of configuration options in the    *
C     right hand display panel.                                             *
C       ROUTINES: ConfigNodes, ConfigBoundaries, ConfigContours, ConfigXhr. *
C*--------------------------------------------------------------------------*
C*--------------------------------------------------------------------------*
      SUBROUTINE ConfigNodes_Init
C PURPOSE: Toggles the node default attributes.
C GIVENS : None
C RETURNS: None
C EFFECTS: Defaults are set.
C--------------------------------------------------------------------------*


C     - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'
cnodedef.inc'

C     - LOCAL VARIABLES
      integer           temp, hitnum
c      LOGICAL           Accepted
      CHARACTER*8       tmp
      CHARACTER*80      ans
C     - OCT91 - for GSS/GKS GPM() routine
c      REAL              gpmX, gpmY
      integer prev_win
c      include '../../pigsrc/pig/panelmod.inc'
C-----------------START ROUTINE-----------------------------------------
      call PigGetWindowNum(prev_win)
      call PigSetWindowNum(CONTROLWIN)
C     - Right window definition T9
      call InitRHPanel

C     - display all labels
      call PigSetTextColour ( LabelColor )
      call PanelText( 3, 1, 'NODE CONFIGURATION', 18 )
      call PanelText( 1, 3, 'Node File Name:', 15 )
      call PanelText( 1, 6, 'Next Interim Save File:', 23 )
      call PanelText( 1, 9, 'Node Colors', 11 )
      call PanelText( 3, 10, 'Interior:', 9 )
      call PanelText( 3, 11, 'Boundary:', 9 )
      call PanelText( 3, 12, '  Second:', 9 )
      call PanelText( 3, 13, '  Modify:', 9 )
      call PanelText( 3, 14, ' Markers:', 9 )
      call PanelText( 1, 16, 'Secondary Color', 15 )
      call PanelText( 1, 17, ' Index:', 7 )
      call PanelText( 1, 19, '  Node Type:', 12 )
      call PanelText( 1, 20, 'Marker Type:', 12 )
!      call PanelText( 10, 23, 'CLOSE', 5 )

C     - display all unchangeable text
      call PigSetTextColour( NoHitColor )
      call PanelText( 1, 4, NodeRName, 24 )
      call PanelText( 1, 7, NodeIName, 24 )

C     - display all changeable text
      call PigSetTextColour( HitColor )

C     - REDRAW option ( hit # 10 )
!      call PanelHit( 10, 22, 10, 'REDRAW', 6 )

C     - ACCEPT option ( hit # 1 )
      call PanelHit( 10, 22, 1, 'CLOSE', 5 )

C     - Interior Node color option ( hit # 2 )
      call PanelHitBox( 13, 10, 2, 6, NodeIColor )

C     - Boundary Node color option ( hit # 3 )
      call PanelHitBox( 13, 11, 3, 6, NodeBColor )

C     - Secondary color option ( hit # 4 )
      call PanelHitBox( 13, 12, 4, 6, NodeSColor )

C     - Modification color option ( hit # 5 )
      call PanelHitBox( 13, 13, 5, 6, ModifColor )

C     - Marker color option ( hit # 6 )
      call PanelHitBox( 13, 14, 6, 6, MarkColor )

C     - Index for second colour to begin ( hit # 7 )
      tmp = ' '
      WRITE ( tmp, FMT='(I6)' ) NodeSIndex
      call PanelHit( 8, 17, 7, tmp(1:6), 6 )

C     - for following two hit options a polymarker is placed on top of
C     -- a dummy (backgr color) panel hit box, calculation of coordinates
C     -- for GPM() routine uses formula from PANELMOD.FOR routine PanelText()
C     -- as follows:
C       xcoord = Pxmax - ( (25 - colt) * 1.6 )
C       ycoord = Pymax - ( rowt * 4.1 )
C     -         -       -       -       - add a little to center it
c changed agd aug 94 to use statement function defined in panelmod.inc

C     - Node Type option ( hit # 8 )
      call PanelHitSym( 15, 19, 8, NodeMType, NodeIColor )
c      call PanelHitBox( 15, 19, 8, 2, backgr )
c      call PigSetSymbolNumber( NodeMType )
c      call PigSetSymbolColour ( NodeIColor )
c     call PigSetSymbolSize ( 1.0 )
c      gpmX = (coltox(15) + coltox(16))/2.0
c      gpmY = ( rowtoy(18) + 2.*rowtoy(19) )/3.
c      call PigDrawSymbols ( 1, gpmX, gpmY )

C     - Marker Type option ( hit # 9 )
c      gpmX = (coltox(15) + coltox(16))/2.0
c      gpmY = ( rowtoy(19) + 2.*rowtoy(20) )/3.
c     gpmY = rowtoy(20)
      call PanelHitSym( 15, 20, 9, MarkMType, MarkColor )
c      call PanelHitBox( 15, 20, 9, 2, backgr )
c      call PigSetSymbolNumber ( MarkMType )
c     call PigSetSymbolSize ( 1.0 )
c      call PigSetSymbolColour ( MarkColor )
c      call PigDrawSymbols ( 1, gpmX, gpmY )
      call PigSetWindowNum(prev_win)
      return

      entry ConfigNodes_Ehandler(hitnum)

      call PigGetWindowNum(prev_win)
      call PigSetWindowNum(CONTROLWIN)
      IF ( hitnum .eq. 1 ) THEN
	     call ClearRHPanel
      ELSE IF ( hitnum .eq. 2 ) THEN
	     IF ( NodeIColor .eq. MaxColour ) THEN
	       NodeIColor = 0
	     ELSE
	       NodeIColor = NodeIColor + 1
	     ENDIF
	     call PanelHitBox( 13, 10, 2, 6, NodeIColor )
      ELSE IF ( hitnum .eq. 3 ) THEN
	     IF ( NodeBColor .eq. MaxColour ) THEN
	       NodeBColor = 0
	     ELSE
	       NodeBColor = NodeBColor + 1
	     ENDIF
	     call PanelHitBox( 13, 11, 3, 6, NodeBColor )
      ELSE IF ( hitnum .eq. 4 ) THEN
	     IF ( NodeSColor .eq. MaxColour ) THEN
	       NodeSColor = 0
	     ELSE
	       NodeSColor = NodeSColor + 1
	     ENDIF
	     call PanelHitBox ( 13, 12, 4, 6, NodeSColor )
      ELSE IF ( hitnum .eq. 5 ) THEN
	     IF ( ModifColor .eq. MaxColour ) THEN
	       ModifColor = 0
	     ELSE
	       ModifColor = ModifColor + 1
	     ENDIF
	     call PanelHitBox( 13, 13, 5, 6, ModifColor )
      ELSE IF ( hitnum .eq. 6 ) THEN
	     IF ( MarkColor .eq. MaxColour ) THEN
	       MarkColor = 0
	     ELSE
	       MarkColor = MarkColor + 1
	     ENDIF
	     call PanelHitBox( 13, 14, 6, 6, MarkColor )
      ELSE IF ( hitnum .eq. 7 ) THEN
30           CONTINUE
	     call PigPrompt (
     +          'Enter index to start secondary color:', ans )
	     READ ( ans, FMT = '(I6)', ERR = 30 ) temp
	     call PigEraseMessage
	     call PigSetTextColour ( HitColor )
	     NodeSIndex = temp
	     tmp = '        '
	     WRITE ( tmp, FMT = '(I6)' ) NodeSIndex
	     call PanelHit( 8, 17, 7, tmp(1:6), 6 ) 
      ELSE IF ( hitnum .eq. 8 ) THEN
c *** Erase old marker
c	     call PigSetSymbolNumber ( NodeMType )
c            call PigSetSymbolSize ( 1.0 )
c             call PigSetSymbolColour ( Backgr )
c	     gpmX = (coltox(15) + coltox(16))/2.0
c             gpmY = ( rowtoy(18) + 2.*rowtoy(19) )/3.
c	     call PigDrawSymbols ( 1, gpmX, gpmY )
c
	     IF ( NodeMType .eq. MaxMarkType ) THEN
	       NodeMType = MinMarkType
	     ELSE
	       NodeMType = NodeMType + 1
	       IF ( NodeMType .eq. 0 )  NodeMType = NodeMType + 1
	     ENDIF
	     call PanelHitSym( 15, 19, 8, NodeMType, NodeIColor )
c	     call PanelHitBox( 15, 19, 8, 2, backgr )
c	     call PigSetSymbolNumber ( NodeMType )
c            call PigSetSymbolSize ( 1.0 )
c             call PigSetSymbolColour ( NodeIColor )
c	     gpmX = (coltox(15) + coltox(16))/2.0
c             gpmY = ( rowtoy(18) + 2.*rowtoy(19) )/3.
c	     call PigDrawSymbols ( 1, gpmX, gpmY )
      ELSE IF ( hitnum .eq. 9 ) THEN
c *** Erase old marker
c	     call PigSetSymbolNumber ( MarkMType )
c            call PigSetSymbolSize ( 1.0 )
c             call PigSetSymbolColour ( Backgr )
c	     gpmX = (coltox(15) + coltox(16))/2.0
c             gpmY = ( rowtoy(19) + 2.*rowtoy(20) )/3.
c	     call PigDrawSymbols ( 1, gpmX, gpmY )
	     IF ( MarkMType .eq. MaxMarkType ) THEN
	       MarkMType = MinMarkType
	     ELSE
	       MarkMType = MarkMType + 1
	       IF ( MarkMType .eq. 0 )  MarkMType = MarkMType + 1
	     ENDIF
	     call PanelHitSym( 15, 20, 9, MarkMType, MarkColor )
c	     call PanelHitBox( 15, 20, 9, 2, backgr )
c	     call PigSetSymbolNumber ( MarkMType )
c            call PigSetSymbolSize ( 1.0 )
c             call PigSetSymbolColour ( MarkColor )
c	     gpmX = (coltox(15) + coltox(16))/2.0
c             gpmY = ( rowtoy(19) + 2.*rowtoy(20) )/3.
c	     call PigDrawSymbols ( 1, gpmX, gpmY )
      ELSE IF ( hitnum .eq. 10 ) THEN
	     call DisplayNodes()
      ENDIF
      call PigSetWindowNum(prev_win)
      END

C*--------------------------------------------------------------------------*
C*--------------------END NODECONF.FOR--------------------------------------*

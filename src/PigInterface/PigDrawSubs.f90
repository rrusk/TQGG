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
!                            Drawing                                      *
!*--------------------------------------------------------------------------*

      subroutine PigSetWorldCoordinates(x1, x2, y1, y2)

!  Call Sequence:
!        call PigSetWorldCoordinates(x1, x2, y1, y2)
!  Purpose:  Set the world coordinates for the graphic window.
!  Givens :  real x1, x2, y1, y2     are in WC.  (x1 < x2) and (y1 < y2)
!  Returns:  None
!  Effects:  Changes the MAIN window world coordinates
! 
! save world coordinates
      real x1, x2, y1, y2
      real, save :: wx1, wx2, wy1, wy2

      if ((x1 .ne. x2) .and. (y1 .ne. y2)) then
         wx1 = x1
         wx2 = x2
         wy1 = y1
         wy2 = y2
         call WPigSetWorldCoordinates(x1,x2,y1,y2)
      endif

      return

! ------------------------------------------------------------------------- *
      entry PigGetWorldCoordinates(x1, x2, y1, y2)

!  Call Sequence:
!        call PigGetWorldCoordinates(x1, x2, y1, y2)
!  Purpose:  Return the current world coordinates for the graphic window.
!            this routine may need to query the hardware
!  Givens :  None
!  Returns:  real        x1, x2   : in WC    (x1 < x2)
!            real        y1, y2   : in WC    (y1 < y2)
!  Effects:  Returns the current world coordinates

!      x1 = wx1
!      x2 = wx2
!      y1 = wy1
!      y2 = wy2
      call WPigGetWorldCoordinates(x1,x2,y1,y2)

      return

      end

!*--------------------------------------------------------------------------*
!                       Draw attributes                                     *
!*--------------------------------------------------------------------------*

      subroutine PigSetLineColour(NewColour)

!  Call Sequence:
!        call PigSetLineColour(NewColour)
!  Purpose:  Set the current line colour for use to NewColour.  Lines
!            drawn are in the current line colour.
!  Givens :  integer      NewColour: the new colour to change to
!  Returns:  None
!  Effects:  Changes current line colour to colour passed in

      integer, save :: LineColour = 1
      integer   NewColour, CurColour

      if (NewColour.lt.0) then
           call PigMessageOK('Illegal colour number, must be > 0 ','color')

      else
        LineColour = mod (NewColour,16)
        call WPigSetLineColour (LineColour)
      endif
      return

!*--------------------------------------------------------------------------*

      entry PigGetLineColour(CurColour)

!  Call Sequence:
!        call PigGetLineColour(CurColour)
!  Purpose:  Returns the current line colour
!  Givens :  None
!  Returns:  integer     CurColour: the current new line colour
!  Effects:  resets the line colour

      CurColour = LineColour

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigSetLineWidth(NewWidth)

!  Call Sequence:
!        call PigSetLineWidth(NewWidth)
!  Purpose:  Set the current line Width for use to NewWidth.  Lines
!            drawn are in the current line Width.
!  Givens :  integer      NewWidth: the new Width to change to
!  Returns:  None
!  Effects:  Changes current line Width to Width passed in

      real, save :: LineWidth = 1.0
      real   NewWidth, CurWidth

      if (NewWidth.lt.0) then
        call PigMessageOK('Illegal Width number, must be > 0','width')

      else
        LineWidth = NewWidth
!        call WPigSetLineWidth (LineWidth)
      endif
      return

!*--------------------------------------------------------------------------*

      entry PigGetLineWidth(CurWidth)

!  Call Sequence:
!        call PigGetLineWidth(CurWidth)
!  Purpose:  Returns the current line Width
!  Givens :  None
!  Returns:  integer     CurWidth: the current new line Width
!  Effects:  resets the line Width
! 
      CurWidth = LineWidth

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigSetFillColour(NewColour)

!  Call Sequence:
!        call PigSetFillColour(NewColour)
!  Purpose:  Set the current fill colour for use to NewColour.  Polylines
!            are filled in the current fill colour.
!  Givens :  integer     NewColour: the new fill colour
!  Returns:  None
!  Effects:  changes the current fill colour to NewColour

      integer, save :: FillColour = 1
      integer   NewColour, CurColour

      if (NewColour.lt.0) then
        call PigMessageOK('Illegal colour number, must be > 0 ','color')
      else
        FillColour = mod(NewColour,16)
        call WPigSetFillColour (FillColour)
      endif
      return

!*--------------------------------------------------------------------------*

      entry PigGetFillColour(CurColour)

!  Call Sequence:
!        call PigGetFillColour(CurColour)
!  Purpose:  Returns the current fill colour.
!  Givens :  None
!  Returns:  integer     CurColour: the current fill colour
!  Effects:  Resets the fill colour

      CurColour = FillColour

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigSetSymbolColour(NewColour)

!  Call Sequence:
!        call PigSetSymbolColour(NewColour)
!  Purpose:  Set the current symbol colour for use to NewColour.  Symbols
!            drawn are in the current symbol colour.
!  Givens :  integer     NewColour: the new colour to change to
!  Returns:  None
!  Effects:  Changes the current symbol colour to NewColour

      integer, save :: SymbolColour = 1
      integer   NewColour, CurColour

      if (NewColour.lt.0) then
        call PigMessageOK('Illegal colour number, must be > 0 ','color')
      endif

      SymbolColour = mod(NewColour,16)

      call WPigSetSymbolColour (SymbolColour)
      return

! ------------------------------------------------------------------------- *

      entry PigGetSymbolColour(CurColour)

!  Call Sequence:
!        call PigGetSymbolColour(CurColour)
!  Purpose:  Returns the current symbol colour.
!  Givens :  None
!  Returns:  integer     CurColour: the current symbol colour
!  Effects:  resets the symbol colour
 
      CurColour = SymbolColour

      return
      end

! ------------------------------------------------------------------------- *

      subroutine PigSetSymbolNumber(NewSymbolNumber)

!  Call Sequence:
!        call PigSetSymbolNumber(NewSymbolNumber)
!  Purpose:  Set the current symbol for use to NewSymbolNumber.  Symbols
!            drawn are the current symbol.
!  Givens :  integer     NewSymbolNumber: the new symbol to change to
!  Returns:  None
!  Effects:  Changes the current symbol to NewSymbolNumber

      integer, save :: SymbolNumber = 1
      integer   NewSymbolNumber, CurSymbolNumber

      SymbolNumber = NewSymbolNumber

      call WPigSetSymbolNumber (SymbolNumber)
      return

! ------------------------------------------------------------------------- *

    entry PigGetSymbolNumber(CurSymbolNumber)

!  Call Sequence:
!        call PigGetSymbolNumber(CurSymbolNumber)
!  Purpose:  Returns the current symbol number.
!  Givens :  None
!  Returns:  integer     CurSymbolNumber: the current symbol colour
!  Effects:  Resets the symbol number

    CurSymbolNumber = SymbolNumber

    return
    end

!*--------------------------------------------------------------------------*

      subroutine PigSetSymbolSize(NewSymbolSize)

!  Call Sequence:
!        call PigSetSymbolSize(NewSymbolSize)
!  Purpose:  Set the size of the current symbol to NewSymbolSize.
!  Givens :  real       NewSymbolSize: the new symbol size to change to
!  Returns:  None
!  Effects:  Changes the current symbol size to NewSymbolSize

      real, save :: SymbolSize = 1.
      real   NewSymbolSize, CurSymbolSize

      SymbolSize = NewSymbolSize

      call WPigSetSymbolSize (SymbolSize)
      return

!*--------------------------------------------------------------------------*

      entry PigGetSymbolSize(CurSymbolSize)

!  Call Sequence:
!        call PigGetSymbolSize(NewSymbolSize)
!  Purpose:  Get the current symbol size.
!  Givens :  real       NewSymbolSize: the new symbol size to change to
!  Returns:  None
!  Effects:  None

      CurSymbolSize = SymbolSize

      return
      end

!*--------------------------------------------------------------------------*
!                       Drawing routines                                    *
!*--------------------------------------------------------------------------*

    subroutine PigDrawLine( icnt, pxray, pyray, rdflag )

!  Call Sequence:
!        call PigDrawLine( icnt, pxray, pyray, rdflag )
!  Purpose: To draw a line from (x1, y1) to (x2, y2) in
!           the current line colour.
!  Given   : pxray - real array contains x co-ordinates,
!           pyray - real array contains y co-ordinates,
!           icnt  - total no. of paired co-ordinates,
!           rdflag = 0 if grid color to be used for lines,
!                  = 1 if Modification color to be used for lines,
!                  = 2 if back ground color is used.
!  Effects: Draws a line from (x1, y1) to (x2, y2).


      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

!     - PASSED PARAMETERS
      integer icnt, rdflag
      REAL pxray(icnt), pyray(icnt)

!     - LOCAL VARIABLES
      integer   prev_tn, PrevColour

!------------------BEGIN------------------

      call PigGetWindowNum( prev_tn )
      call PigSetWindowNum( MAINWIN )
      call PigGetLineColour(PrevColour)

      if ( rdflag .eq. 0 ) then
        call PigSetLineColour( GridPColour )
      elseif ( rdflag .eq. 1 ) then
        call PigSetLineColour( ModificationColour )
      elseif ( rdflag .eq. 2 ) then
        call PigSetLineColour( backgr )
      elseif ( rdflag .eq. 3 ) then
        call PigSetLineColour( orange )
      elseif ( rdflag .eq. 4 ) then
        call PigSetLineColour( red )
      else  !if ( rdflag .eq. 5 ) then
        call PigSetLineColour( foregr )
      endif

      call WPigDrawPolyLine(icnt, pxray, pyray )

      call PigSetWindowNum( prev_tn )
      call PigSetLineColour(PrevColour)

      END

!*--------------------------------------------------------------------------*

      SUBROUTINE PigDrawGridLine( icnt, pxray, pyray, rdflag )

! Purpose : To draw line section which paired in pxray and pyray
!           arrays. All elememts are in screen co-ordinates.
!           The first element of the two arrays is the starting
!           point of each line, icnt is the number of elements
!           in an array; icnt - 1 is the total number of lines to
!           be drawn. For example :
!                if icnt = 3,  then 2 lines will be drawn.
!                line 1 : (pxray(1),pyray(1)) to (pxray(2),pxray(2))
!                line 2 : (pxray(1),pyray(1)) to (pxray(3),pyray(3))
! Given   : pxray - real array contains x co-ordinates,
!           pyray - real array contains y co-ordinates,
!           icnt  - total no. of paired co-ordinates,
!           rdflag = 0 if grid color to be used for lines,
!                  = 1 if Modification color to be used for lines,
!                  = 2 if back ground color is used.
! Returns : None.



      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

!     - PASSED PARAMETERS
      REAL pxray(*), pyray(*)
      integer icnt, rdflag

!     - COMMON BLOCKS
!      REAL CWXL, CWXH, CWYL, CWYH
!      COMMON /CURWIN/ CWXL, CWXH, CWYL, CWYH

!     - LOCAL VARIABLES
      integer   i
      REAL      strtx(2),strty(2)
!               - the start and endpoints of a line section
      integer   prev_tn, PrevColour

!------------------BEGIN------------------

      call PigGetWindowNum( prev_tn )
      call PigSetWindowNum( MAINWIN )
      call PigGetLineColour(PrevColour)

      if ( rdflag .eq. 0 ) then
        call PigSetLineColour( GridPColour )
      elseif ( rdflag .eq. 1 ) then
        call PigSetLineColour( ModificationColour )
      else
        call PigSetLineColour( backgr )
      endif

!     - start out at the new point
      strtx(1) = pxray(1)
      strty(1) = pyray(1)
      do i = 2, icnt
        strtx(2) = pxray(I)
        strty(2) = pyray(I)

!       - connect neighbors
        call PigDrawPolyline( 2, strtx, strty )
      enddo

      call PigSetWindowNum( prev_tn )
      call PigSetLineColour(PrevColour)

      END

!*--------------------------------------------------------------------------*

      SUBROUTINE PutMarker( xloc, yloc, markersymbol, markerclr )

! Purpose : To put a marker at an x,y location on the grid
! Given   : xloc, yloc   - the x y location where the marker is to go
!           markersymbol - the marker symbol to be used
!           markerclr - the colour for the marker

!     - PASSED PARAMETERS
      integer markersymbol
      integer markerclr
      REAL xloc, yloc

!     - INCLUDES
	include '../includes/graf.def'
!	INCLUDE '../includes/pmarker.inc'

      real, parameter :: PMSize = 7.

!     - LOCAL VARIABLES
      integer        markc, marks, i1
      integer        prev_tn, prev_symb_colour
      integer        prev_symb_number
      real           prev_symb_size, x(1),y(1)
!--------- BEGIN --------

      markc = markerclr
      marks = markersymbol
      i1 = 1
      x(1) = xloc
      y(1) = yloc

! save current states        
      call PigGetSymbolNumber(prev_symb_number)
      call PigGetSymbolSize(prev_symb_size)
      call PigGetWindowNum( prev_tn )
      call PigGetSymbolColour (prev_symb_colour)

      call PigSetSymbolNumber( marks )
      call PigSetSymbolSize( 0.15 * pmsize )

      call PigSetSymbolColour( markc )
      call PigSetWindowNum( MAINWIN )
      call PigDrawSymbols( 1, x(1), y(1) )

! reset states
      call PigSetWindowNum( prev_tn )
      call PigSetSymbolColour (prev_symb_colour)
      call PigSetSymbolNumber(prev_symb_number)
      call PigSetSymbolSize(prev_symb_size)
      END

! ------------------------------------------------------------------------- *

      subroutine PigDrawClrSymbol(x, y, clr)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x,y
      character(1) clr

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )

      if ( clr(1:1) .eq. 'W' ) then
      call PigSetLineColour( white )
      elseif ( clr(1:1) .eq. 'R' ) then
      call PigSetLineColour( red )
      elseif ( clr(1:1) .eq. 'G' ) then
      call PigSetLineColour( green )
      elseif ( clr(1:1) .eq. 'B' ) then
      call PigSetLineColour( blue )
      elseif ( clr(1:1) .eq. 'Y' ) then
      call PigSetLineColour( yellow )
      endif


      np = 1
      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigDrawBndSymbol(x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x,y

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( NodeBColor )

      np = 1
      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigDrawBndSymbols (np, x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x(np),y(np)

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( NodeBColor )

      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigDrawIntSymbol(x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x,y

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( NodeIColor )

      np = 1
      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigDrawIntSymbols (np, x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x(np),y(np)

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( NodeIColor )

      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigDrawModifySymbol(x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x,y

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( ModifColor )

      np = 1
      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigEraseModifySymbol(x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x,y

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( backgr )

      np = 1
      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigDrawModifySymbols (np, x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x(np),y(np)

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( ModifColor )

      write(*,*) 'np=',np
      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigEraseModifySymbols (np, x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
      include '../includes/defaults.inc'

      integer np
      real x(np),y(np)

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( NodeMType )
      call PigSetSymbolColour ( backgr )

      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigDrawCoinSymbol(x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
!      include '../includes/defaults.inc'

      integer np
      real x,y

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( square )
      call PigSetSymbolColour ( violet )

      np = 1
      call WPigDrawSymbols (np, x, y)

      return
      end

!*--------------------------------------------------------------------------*

      subroutine PigEraseCoinSymbol(x, y)

!  Purpose:  Draw modify symbol in modifyColour at 1 point in x, y
!  Givens :  real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      implicit none

      include '../includes/graf.def'
!      include '../includes/defaults.inc'

      integer np
      real x,y

      call PigSetWindowNum ( MAINWIN )
      call PigSetSymbolNumber ( square )
      call PigSetSymbolColour ( backgr )

      np = 1
      call WPigDrawSymbols (np, x, y)

      return
      end

!*-----------------------------------------------------------------------*

      SUBROUTINE RedrawMark ( xmark, ymark, mcolor )

! PURPOSE: To draw a polymarker of current type at xmark,ymark.
!          All elememts are in screen co-ordinates.
!   GIVEN: xmark = x co-ordinate of polmarker location
!          ymark = y co-ordinate of polmarker location
!          mcolor = indicates color to use
! RETURNS: none.
! EFFECTS: A polymarker of the current type, with color = mcolor is drawn
!          at (xmark,ymark). 
! WRITTEN: May 1990 - JDM for NODER, based on REDRAW in PICSP.FOR
! MODIFIED: OCT91 - JDM - 3rd arguement for segment to create removed. All
!                         calls to this routine throughout NODER changed to
!                         remove old arguement 3, not documented except for
!                         here.
!-----------------------------------------------------------------------*

! - PASSED VARIABLES
      REAL xmark, ymark
      integer mcolor

! - "INCLUDES"
      include '../includes/graf.def'
      include '../includes/defaults.inc'


! - COMMON BLOCKS
!      REAL cwxl, cwxh, cwyl, cwyh
!      COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh

! - LOCAL VARIABLES
      REAL xm(1), ym(1)

      integer PrevColour, p_win

!----------------------START ROUTINE--------------------------------
      call PigGetWindowNum (p_win)
      call PigGetSymbolColour (PrevColour)

!     - select main window
      call PigSetWindowNum( MAINWIN )
      call PigSetSymbolColour ( mcolor )
      xm(1) = xmark
      ym(1) = ymark
!     - draw the marker
      call PigDrawSymbols ( 1, xm, ym )
       
      call PigSetSymbolColour (PrevColour)
      call PigSetWindowNum (p_win)

      END

!*--------------------------------------------------------------------------*

      SUBROUTINE DrawPMark( xwc, ywc )

! Purpose: Output of a permanent marker symbol on active graphic display device.
!          Location is in world coordinates (WC) in the Main window (1).
! Given: xwc - x WC coordinates of marker location
!        ywc - y WC coordinates of marker location
!        mcolor - color to display marker in
! Returns: None.
! Effects: Transformation 1 is selected, marker is displayed. Marker type
!          is set to PermMarkerType.

!       - INCLUDES
!      INCLUDE '../includes/pmarker.inc'
      include '../includes/graf.def'

!       - PASSED PARAMETERS
      REAL xwc, ywc

!       - Local variables
      integer mcolor, i1
      integer        prev_tn, prev_symb_colour
      integer        prev_symb_number
      real           prev_symb_size
      real xwc1(1),ywc1(1)

      integer, parameter :: PermMarkerColour = 15
      integer, parameter :: PermMarkerType = 3
      REAL, parameter :: PMSize = 7.

!--------- BEGIN --------

! save current states        
      call PigGetSymbolNumber(prev_symb_number)
      call PigGetSymbolSize(prev_symb_size)
      call PigGetWindowNum( prev_tn )
      call PigGetSymbolColour (prev_symb_colour)

      mcolor = white
      call PigSetSymbolNumber ( PermMarkerType )
      call PigSetSymbolSize( 0.15 * PMSize )
      call PigSetWindowNum( MAINWIN )
      call PigSetSymbolColour( PermMarkerColour )
      i1 = 1
      xwc1(1) = xwc
      ywc1(1) = ywc
      call PigDrawSymbols( i1, xwc1(1), ywc1(1) )

! reset previous states
      call PigSetSymbolNumber(prev_symb_number)
      call PigSetSymbolSize(prev_symb_size)
      call PigSetWindowNum( prev_tn )
      call PigSetSymbolColour (prev_symb_colour)

!      END
      return
!*-----------------------------------------------------------------------*

!      SUBROUTINE ErasePMark( xwc, ywc )

! Purpose: Output of a permanent marker symbol on active graphic display device.
!          Location is in world coordinates (WC) in the Main window (1).
! Given: xwc - x WC coordinates of marker location
!        ywc - y WC coordinates of marker location
!        mcolor - color to display marker in
! Returns: None.
! Effects: Transformation 1 is selected, marker is displayed. Marker type
!          is set to PermMarkerType.

!       - INCLUDES
!      INCLUDE '../includes/pmarker.inc'
!      include '../includes/graf.def'

!       - PASSED PARAMETERS
!      REAL xwc, ywc

!       - Local variables
!      integer mcolor
!      integer        prev_tn, prev_symb_colour
!      integer        prev_symb_number
!      real           prev_symb_size

!--------- BEGIN --------
      entry ErasePMark( xwc, ywc )

! save current states        
      call PigGetSymbolNumber(prev_symb_number)
      call PigGetSymbolSize(prev_symb_size)
      call PigGetWindowNum( prev_tn )
      call PigGetSymbolColour (prev_symb_colour)

      mcolor = white
      call PigSetSymbolNumber ( PermMarkerType )
      call PigSetSymbolSize( 0.15 * PMSize )
      call PigSetWindowNum( MAINWIN )
      call PigSetSymbolColour( backgr )
      i1 = 1
      xwc1(1) = xwc
      ywc1(1) = ywc
      call PigDrawSymbols( i1, xwc1(1), ywc1(1) )

! reset previous states
      call PigSetSymbolNumber(prev_symb_number)
      call PigSetSymbolSize(prev_symb_size)
      call PigSetWindowNum( prev_tn )
      call PigSetSymbolColour (prev_symb_colour)

      END

!*--------------------------------------------------------------------------*
!                       Drawing primitives                                  *
!*--------------------------------------------------------------------------*

      subroutine PigDrawPolyline(n, x, y)

!  Purpose:  Draw a line in the current LineColour.
!  Givens :  integer     n      :    number of points
!            real        x(n)   :    X coordinates of points in WC
!            real        y(n)   :    Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws n number of lines of x,y coordinates

      integer n
      real x(n),y(n)

!      call WPigSetWindowNum( MAINWIN )

      call WPigDrawPolyLine(n, x, y)

      return
      end

!* ------------------------------------------------------------------------- *
      subroutine PigDrawFilledPolygon(n, x, y)

!  Purpose: Draws a filled polygon of n points of x,y coordinates
!  Givens :  integer     n     :   number of points
!            real        x(n)  :    X coordinates of points in WC
!            real        y(n)  :    Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws a filled polgon

      integer n
      real x(n),y(n)

      call WPigDrawFilledPolygon(n, x, y)

      return
      end

!* ------------------------------------------------------------------------- *
      subroutine PigDrawSymbols(n, x, y)

!  Purpose:  Draw current symbol in current FillColour at n points in x, y
!  Givens :  integer     n      :     number of points
!            real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      integer n
      real x(n),y(n)

      call WPigDrawSymbols (n, x, y)
      return
      end
!*--------------------------------------------------------------------------*

      SUBROUTINE PigFillPly(N,XP,YP,Iclr)

!  Purpose:  To fill a polygon.
!  Givens :  
!       integer N : The number of points
!       real XP(N): their X coordinates
!       real YP(N): their Y coordinates
!       integer Iclr: the fill colour.
!  Returns : None
!  Effects:  The given polygon is filled in with the given colour.

      integer N, Iclr
      REAL XP(1), YP(1)
      integer   PrevColour

      call PigGetFillColour(PrevColour)
      call PigSetFillColour(Iclr)
      call PigDrawFilledPolygon(N,XP,YP)
      call PigSetFillColour(PrevColour)
      END

!*--------------------------------------------------------------------------*
!*--------------------------------------------------------------------------*

      subroutine PigSetWindowNum(WinNum)

!  Purpose:  Change the current window number to WinNum
!  Givens :  integer     WinNum  : window number to change to
!  Returns:  None
!  Effects:  Changes the window number

!      include 'ipig.def'

      integer, save :: LastWinNum
      integer WinNum

      LastWinNum = WinNum
      call WPigSetWindowNum(LastWinNum)
      return

! ------------------------------------------------------------------------- *
      entry PigGetWindowNum(WinNum)

!  Purpose:  Get the current window number
!  Givens :  None
!  Returns:  integer     WinNum  : the current window number
!  Effects:  None

      WinNum = LastWinNum
      return

      end

! ========================================================================= *



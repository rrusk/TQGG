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

*----------------------------------------------------------------------*
C                                                                       *
C                       PANELMOD.FOR                                    *
C Modified handling of the hitnumber. The routines which set up panel
C hit areas are now free to use any integer value for the hit number.
C This is designed to make it easier for simultaneously active menus
C and right hand panels.
C A single array called hitnums(MAXHITS) was added to the common block
C in file panelmod.inc.
C 
C The positioning of hit areas in the right hand panel was recoded, to
C use statement functions (in panelmod.inc) to define four transformations,
C for {x,y} to/from {col,row}. This should make it simpler to change the
C coordinate system used in the CONTROLWIN, which will probably be necessary
C when porting to other GUI platforms.
C                                                                       *
C       Contains routines for using right hand panel                    *
C       as a menu area in a standard format. Panel is subdivided into   *
C       24 row and 24 columns, creating 576 squares. Each square is     *
C       intended to be one character high and one character wide. With  *
C       OCT91 changes, a text character need not be the same size as    *
C       a square, however the closer the two the better. Using routines *
C       in this module, the right hand panel can be addressed in row    *
C       and column coordinates (1:24,1:24). Active hit areas may be     *
C       declared as text or colored boxes. A hit may exists on only one *
C       row, but more than one hit may be on the same row. Each hit may *
C       exist in several contiguous squares.                            *
C Routines available:                                                   *
C       - InitRHPanel() : set up right hand panel parameters            *
C       - ClearRHPanel(): clears righthand panel and draws outline box, *
C       - PanelText()   : output text label at given row & column,      *
C       - PanelTextCentre()   : output text label at given row & column,*
C                         using centre justification for the position.  *
C       - PanelTextRight()   : output text label at given row & column, *
C                         using right justification for the position.   *
C       - PanelHit()    : output text at given row & column, text is    *
C                         then a selectable hit area using the mouse,   *
C                         May be called repeatedly with the same hitnum *
C       - PanelHitBox() : output a colored box at given row & column,   *
C                         box is then a selectable hit area using the   *
C                         mouse.                                        *
C                         May be called repeatedly with the same hitnum *
C       - GetPanelHit() : retrieve any valid hits selected in the panel,*
C       - PanelGetHitnum(MouseX, MouseY, Hitnum) : Retrieve Hitnum      *
C                         corresponding to MouseX and MouseY (in        *
C                         controlwin)                                   *
C       - ISetNumHits( ): declare the number of hit areas defined       *
C                         in the panel. Normally should be set to 0,    *                                 
C                         before any hits declared. PanelHit and        *
C                         PanelHitBox increment the stored value.       *
C                         Is set to zero by InitRHPanel and ClearRHPanel*
C       - IGetNumHits( ): obtain the number of hit areas defined        *
C                         in the panel.                                 *
C       - SetNumHits()  : Dummy routine, to suppress old code where users
C                         did set these values themselves. If called, it
C                         is immediately fatal, with a deliberate array
C                         bounds error to generate a stack trace.
C       - GetNumHits()  : Dummy routine, to suppress old code. Should never
C                         be called. Fatal if called.
C Internal Routines:                                                    *
C       - SetPanelText() : set text attributes.                         *
C       - BlankArea() : clear an area before writting text.             *
C Requirements:                                                         *
C       - Following PigSetWindowNum definition has been made in INIT.FOR         *
C         The coordinate system for the CONTROLWIN IS
C          0.0=<X<=40.0, 0.0<=Y<=100.0
C       - Character attributes contained in PANELMOD.INC                *
C*----------------------------------------------------------------------*

      SUBROUTINE InitRHPanel()

C Purpose: To initialize the right hand panel.
C Given  : None.
C Returns: None.
C Effects: PigSetWindowNum( CONTROLWIN ) is defined and made active, text attributes are set,
C          right hand panel area is cleared, and an outline box is drawn.

!	INCLUDE 'ipig.def'

C----------------------BEGIN------------------------

	call ClearRHPanel
c        call PigErase(CONTROLWIN)
C       - Initialize text
	call SetPaneltext()
	call ISetNumHits(0)
	END

C*----------------------------------------------------------------------*

      SUBROUTINE ClearRHPanel()

C Purpose: To clear the right hand panel.
C Given  : None.
C Returns: None.
C Effects: The right hand panel area is cleared, and WriteID is called,
C          MessStr line is cleared, PigSetWindowNum( CONTROLWIN ) is left active. Number of hits
C          defined is set = 0. NOTE: WriteID, when linked in, should be
C          appropriate title screen routine for program using this module,
C          not necessarily WriteID from MAINGR.FOR.

	INCLUDE 'ipig.def'

C----------------------BEGIN------------------------

!	call PigGetLineColour(PrevColour)
C       - reset # of hit areas
	call ISetNumHits( 0 )

	call PigErase(CONTROLWIN)

!	call PigSetLineColour(PrevColour)
	call PigEraseMessage
	END

C*----------------------------------------------------------------------*

      SUBROUTINE PanelGetHitnum(MouseX, MouseY, Hitnum)

C Purpose: To test an input location, and
C          determine if a valid hit has been made in the right hand panel,
C          and to return the hit number of any valid hit.
C          If there is no valid hit, then Hitnum is returned as 0.
C Assumes: ISetNumHits() has been called to define the number of active hit
C          ares, PanelHit() and/or PanelHitBox() have been called
C          as many times as ISetNumHits() was set to. This is because a
C          contiguous search is made from 1 to numhits in looking for hits.
C          All array values in COMMON /PHITS/ in PANELMOD.INC from 1 to
C          numhits should be valid.
C Given  : None.
C Returns: hitnum = 0 if a valid hit was NOT made,
C                 = number of hit made if hit was valid, this is the same
C                   number that was supplied as 'hith' in PanelHit() or
C                   PanelHitBox().
C Effects: A search is made from 1 to numhits to find a hit.
C---------------------------------------------------------------------------*

	 INCLUDE 'ipig.def'

C       - PASSED PARAMETERS
	real MouseX, MouseY
	integer hitnum

C       - LOCAL PARAMETERS
	integer i, row, col
	integer xtocol, ytorow
	REAL xinp, yinp
*       LOGICAL rfound
*       logical cfound
	logical hfound
C       - (OCT91
*       integer lnth
c	integer  prev_tn, PrevLineColour, PrevTextColour

	INCLUDE 'panelmod.inc'
C------------------BEGIN----------------------

	  hitnum = 0
	  hfound = .FALSE.
	  xinp = MouseX
	  yinp = MouseY

C         - determine option # (hit) chosen
	  IF    (       (xinp .ge. Pxmin)
     +          .AND.   (xinp .le. Pxmax)
     +          .and.   (yinp .ge. Pymin)
     +          .and.   (yinp .le. Pymax)
     +          ) THEN
C            - hit was inside panel hit area, check rows (y) from top to bottom
	     row = ytorow(yinp)
	     col = xtocol(xinp)
	     i = 1
	     hfound = .FALSE.
	     DO WHILE    (       (i.le. numhits)
     +                  .and.   (.NOT. hfound)
     +                  )
		IF ( row .eq. hitrows(i) ) THEN
C               - row found, determine column
c                  IF (      (col .ge. hitcols(i))
c     +               .and.  (col .le. (hitcols(i) + hitlengs(i)))
c     +               ) THEN
		  IF (      (col .ge. hitcols(i))
     +               .and.  (col .le. (hitcols(i) + hitlengs(i) - 1))
     +               ) THEN
C                 - column found
		    if(hitenabled(i)) then
*
* next line changed agdolling June 1993. This change is to support use
* of a large number of hit events, using parameters, similar to the menu
* handling.
*
			 hitnum = hitnums(i)
			 hfound = .TRUE.
			 call PigEraseMessage
		      endif
		  ENDIF
		ENDIF
		i = i + 1
	    ENDDO
	  ENDIF
	  if(.not. hfound) then
		if(numhits.gt.0)
     +      call PigPutMessage(
     +      'Invalid hit. Please select a blue highlight '//
     +      'region from control panel.')
	  endif
	if (       (numhits.gt.0) 
     +     .and.   (hfound)
     +     ) then
	endif
      end
C*----------------------------------------------------------------------*

      SUBROUTINE PanelTextPrevJust( colt, rowt, string, length_p )

C Purpose: To display a text string in the right hand panel using row and
C          column numbers for location.
C Assumes: Character attributes (suggested):
C               expansion = Cexpansion (1.0)
C               spacing = Cspace (0.0)
C               height = Cheight (2.2)
C               width = charwidth (1.3)
C               text color = set as desired
C         * ( these attributes are set in SetPanelText() )
C Given  : colt = column location for text, from 1 to 24,
C          rowt = row location for text, from 1 to 24,
C          string = text string to display,
C          length = number of characters of text in string, should not
C                   exceed 25 - colt. - readonly (AGD)
C Returns: None.
C Effects: Text is displayed at given row, column location. Arguement 'length'
C          may be modified if it is greater than '25 - colt', ie: the text  
C          will not fit in the space between the column given and the edge of
C          the panel.

      include 'ipig.def'
C       - PASSED PARAMETERS
      integer rowt, colt, length_p
      CHARACTER*(*) string

C       - LOCAL VARIABLES
      REAL xcoord, ycoord
      integer length
      integer  prev_tn, PrevColour
      integer prev_just
      INCLUDE 'panelmod.inc'
C------------------BEGIN----------------------

      length = length_p
      length = len(string)
      call PigGetWindowNum( prev_tn )
      call PigSetWindowNum( CONTROLWIN )
      call PigGetJustification(prev_just)
      call PigGetLineColour(PrevColour)
	
      call SetPanelText()
      call PigSetJustification(prev_just)
      if(prev_just.eq.LEFT_JUSTIFY) then
	  call BlankArea(colt, rowt, length_p)
          xcoord = coltox(colt)
          ycoord = rowtoy(rowt-1)
      elseif(prev_just.eq.RIGHT_JUSTIFY) then
	  call BlankArea(colt-length_p, rowt, length_p)
          xcoord = coltox(colt-length)
          ycoord = rowtoy(rowt-1)
      elseif(prev_just.eq.CENTRE_JUSTIFY) then
	  call BlankArea(colt-(length_p/2), rowt, length_p)
          xcoord = coltox(colt-length/2)
          ycoord = rowtoy(rowt-1)
      else
!	  call PigFatal('Invalid justification in PanelTextPrevJust')
      endif

C     - write string
      call PigDrawText( xcoord, ycoord, string(:length) )

C     - refresh outline box of RH Panel in case text interferes with it
      call PigSetLineColour( foregr )
!      call PigDrawBox( 0.0, 40.0, 0.0, 100.0 )
      call PigSetWindowNum( prev_tn )
	
      call PigSetJustification(prev_just)
      call PigSetLineColour(PrevColour)
      END

C*----------------------------------------------------------------------*

      SUBROUTINE PanelText( colt, rowt, string, length_p )

C Purpose: To display a text string in the right hand panel using row and
C          column numbers for location.
C Assumes: Character attributes (suggested):
C               expansion = Cexpansion (1.0)
C               spacing = Cspace (0.0)
C               height = Cheight (2.2)
C               width = charwidth (1.3)
C               text color = set as desired
C         * ( these attributes are set in SetPanelText() )
C Given  : colt = column location for text, from 1 to 24,
C          rowt = row location for text, from 1 to 24,
C          string = text string to display,
C          length = number of characters of text in string, should not
C                   exceed 25 - colt. - readonly (AGD)
C Returns: None.
C Effects: Text is displayed at given row, column location. Arguement 'length'
C          may be modified if it is greater than '25 - colt', ie: the text  
C          will not fit in the space between the column given and the edge of
C          the panel.

      include 'ipig.def'

C       - PASSED PARAMETERS
	integer rowt, colt, length_p
	CHARACTER*(*) string

C       - LOCAL VARIABLES
	integer  prev_just

C------------------BEGIN----------------------
!	entry PanelText( colt, rowt, string, length_p )
	call PigGetJustification(prev_just)
	call PigSetJustification(LEFT_JUSTIFY)
	call PanelTextPrevJust( colt, rowt, string, length_p )
	call PigSetJustification(prev_just)
	END

C*----------------------------------------------------------------------*

      SUBROUTINE PanelTextRight( colt, rowt, string, length_p )

      include 'ipig.def'

C       - PASSED PARAMETERS
	integer rowt, colt, length_p
	CHARACTER*(*) string

C       - LOCAL VARIABLES
	integer  prev_just

C------------------BEGIN----------------------
	call PigGetJustification(prev_just)
	call PigSetJustification(RIGHT_JUSTIFY)
	call PanelTextPrevJust( colt, rowt, string, length_p )
	call PigSetJustification(prev_just)
	END
C*----------------------------------------------------------------------*

      SUBROUTINE SetPanelText( )

C Purpose: To initialize text character attributes for any text displayed
C          in RH Panel.
C Assumes: Character attributes supplied in PANELMOD.INC.
C   Given: None.
C Returns: None.
C Effects:  PigSetWindowNum( CONTROLWIN ) is selected and text attributes are set.

      include 'ipig.def'
	integer  prev_tn
	INCLUDE 'panelmod.inc'

C------------------BEGIN----------------------
        call PigGetWindowNum( prev_tn )
        call PigSetWindowNum( CONTROLWIN )

!       call PigSetTextFontPrec( Pfont, Pprec )
	  Palhorz = LEFT_JUSTIFY
	  Palvert = 4
        call IPigSetTextAlignment( Palhorz, Palvert )
        call PigSetWindowNum( prev_tn )
        END

C*----------------------------------------------------------------------*

      SUBROUTINE BlankArea( colb, rowb, length )

C Purpose: To clear an area in RH Panel where a string would be written.
C Assumes: none.
C Given  : colb = column location of string start, from 1 to 24,
C          rowb = row location of string start, from 1 to 24,
C          length = number of characters in string.
C Returns: None.
C Effects: An area in the RH Panel is cleared to background color. The
C          area starts at rowb, colb and continues for the length and
C          height that the string would take if written. PigGetTextExtent
C          is used to determine where area string would be written 
C          (area to blank). If PigGetTextExtent call fails area is 
C          not blanked. A dummy string filled, with 'B's, is used for 
C          PigGetTextExtent call. Clears only area where string would write,
C          existing text outside that area will not be cleared.

      include 'ipig.def'

C       - PASSED PARAMETERS
	INTEGER rowb, colb, length

C       - LOCAL VARIABLES
	INTEGER i
C       - (OCT91
	REAL xlo, ylo
c	, cpx, cpy, xext(4), yext(4)
	real xhi, yhi
	CHARACTER*(80) string
c	, msg
	integer prev_win

	INCLUDE 'panelmod.inc'

C------------------BEGIN---------------------- 

	do i=1,min(length,len(string))
* too wide for win prop fonts            string(i:i) = 'A'
	    string(i:i) = 'e'
	end do

	call PigGetWindowNum(prev_win)
	call PigSetWindowNum(CONTROLWIN)

C       - determine WC coords of start of string
	xlo = coltox(colb)
	ylo = rowtoy(rowb)
        xhi = coltox(colb+length)
        yhi = rowtoy(rowb-1)
        call PigFillArea(xlo,xhi,ylo,yhi, backgr)

        call PigSetWindowNum(prev_win)
        END

C*----------------------------------------------------------------------*

      SUBROUTINE PanelHit( colh, rowh, hitnum, string, length )

C Purpose: To create and display text for a hit area in the right hand panel
C          that will be detectable by GetPanelHit().
C Given  : colh = column location for hit text, from 1 to 24,
C          rowh = row location for hit text, from 1 to 24,
C          hitnum = the number to identify the hit area by, will be returned
C                 by GetPanelHit() when the hit area is selected, must be
C                 in the set 1...numhits.
C          string = text string to display,
C          length = number of characters of text in string, should not
C                   exceed 25 - colt.
C Returns: None.
C Effects: Text is displayed at given row, column location.
C          Text justification is the current setting. The hit area is set
C          accordingly.
C          Data in
C          PANELMOD.INC is updated to include the new hit area, row, column,
C          and length are stored. Arguement 'length' may be modified if it
C          is greater than '25 - colt', ie: the text will not fit in the space
C          between the column given and the edge of the panel. Text color is
C          set to Hitcolor.
C          If the hitnum has already been used, then the slot is reassigned.
C          The screen area previously used is unchanged - i.e. not blanked.

	INCLUDE 'ipig.def'

C       - PASSED PARAMETERS
	integer       rowh, colh, hitnum, length
	CHARACTER*(*) string
	integer       prev_tn, hith
	integer i, thishith
	integer prev_justify
	INCLUDE 'panelmod.inc'
*       SAVE
C------------------BEGIN----------------------
	call PigGetWindowNum( prev_tn )
	call PigSetWindowNum( CONTROLWIN )

	call IGetNumHit(hith)
c - look for this hitnum in hitnum array.        
	thishith = hith + 1
	do i=1,hith
		if (hitnums(i) .eq. hitnum) then
c                       - match found
			thishith = i
		endif
	end do
	if(thishith .gt. hith) then
		hith = hith + 1
		call ISetNumHits(hith)
	else
		hith = thishith
	endif
	call PigGetJustification(prev_justify)
C       - Initialize text
	call SetPaneltext()
C       - output the text
	call PigSetJustification(prev_justify)
	call PanelTextPrevJust( colh, rowh, string, length )

C       - store the hit information
	if(prev_justify .eq. LEFT_JUSTIFY) then
	     hitcols(hith) = colh
	else if(prev_justify .eq. CENTRE_JUSTIFY) then
	     hitcols(hith) = colh - length / 2
	else if(prev_justify .eq. RIGHT_JUSTIFY) then
	     hitcols(hith) = colh - length
	endif
	hitrows(hith) = rowh
	hitlengs(hith) = length
	hitnums(hith) = hitnum
	hitenabled(hith) = .true.
	hitistext(hith) = .true.
	hittext(hith) = string
	call PigSetJustification(prev_justify)
	call PigSetWindowNum( prev_tn )
	END

C*----------------------------------------------------------------------*

      SUBROUTINE PanelHitBox( colh, rowh, hitnum, length, hcolor)

C Purpose: To create and display a colored box for a hit area in the right
C          hand panel that will be detectable by GetPanelHit().
C Assumes: PigSetWindowNum( CONTROLWIN ) is active,
C Given  : colh = column location for hit box, from 1 to 24,
C          rowh = row location for hit box, from 1 to 24,
C          hitnum = the number to identify the hit area by, will be returned
C                 by GetPanelHit() when the hit area is selected, must be
C                 in the set 1...numhits.
C          length = number of columns in lengthof the box, should not
C                   exceed 25 - colt,
C          hcolor = color to fill box with.
C Returns: None.
C Effects: Box is displayed at given row, column location. Data in
C          PANELMOD.INC is updated to include the new hit area, row, column,
C          and length are stored. Arguement 'length' may be modified if it
C          is greater than 25 - colt, ie: the box will not fit in the space
C          between the column given and the edge of the panel.
C          If the hitnum has already been used, then the slot is reassigned.
C          The screen area previously used is unchanged - i.e. not blanked.

	INCLUDE 'ipig.def'

C       - PASSED PARAMETERS
	integer rowh, colh, hitnum, length
	integer hcolor

C       - LOCAL VARIABLES
	REAL xbl, xbh, ybl, ybh
	integer   prev_tn, PrevColour, hith
	integer i, ThisHitH
	INCLUDE 'panelmod.inc'
C------------------BEGIN----------------------

	call PigGetLineColour(PrevColour)

	call IGetNumHit(hith)
c - look for this hitnum in hitnum array.        
	thishith = hith + 1
	do i=1,hith
		if (hitnums(i) .eq. hitnum) then
c                       - match found
			thishith = i
		endif
	end do
	if(thishith .gt. hith) then
		hith = hith + 1
		call ISetNumHits(hith)
	else
		hith = thishith
	endif

	call PigGetWindowNum( prev_tn )
	call PigSetWindowNum( CONTROLWIN )

C       - define the box smaller than entire area of squares in hit area
	xbl = coltox(colh)
	ybl = rowtoy(rowh)
	xbh = coltox(colh + length)
	ybh = (rowtoy(rowh-1) + ybl) / 2.0
c        xbh = xbl + ( length * xdelta ) - 1.0
c        ybh = ybl + ydelta - 2.0

	call PigFillArea( xbl, xbh, ybl, ybh, hcolor )

C       - store the hit information
	hitcols(hith) = colh
	hitrows(hith) = rowh
	hitlengs(hith) = length
	hitnums(hith) = hitnum
	hitenabled(hith) = .true.
	hitistext(hith) = .false.
	call PigSetWindowNum( prev_tn )
	call PigSetLineColour(PrevColour)
	END

C*----------------------------------------------------------------------*

      SUBROUTINE PanelHitSym( colh, rowh, hitnum, SymbolNum, hcolor)

C Purpose: To create and display a colored symbol for a hit area in the right
C          hand panel that will be detectable by GetPanelHit().
C Assumes: PigSetWindowNum( CONTROLWIN ) is active,
C Given  : colh = column location for hit box, from 1 to 24,
C          rowh = row location for hit box, from 1 to 24,
C          hitnum = the number to identify the hit area by, will be returned
C                 by GetPanelHit() when the hit area is selected, must be
C                 in the set 1...numhits.
C          SymbolNum = number of the symbol
C          hcolor = color to draw with..
C Returns: None.
C Effects: Symbol is displayed at given row, column location. Data in
C          PANELMOD.INC is updated to include the new hit area, row, column,
C          and length are stored. 
C          If the hitnum has already been used, then the slot is reassigned.
C          The screen area previously used is unchanged - i.e. not blanked.

	INCLUDE 'ipig.def'

C       - PASSED PARAMETERS
	integer rowh, colh, hitnum, SymbolNum
	integer hcolor

C       - LOCAL VARIABLES
	REAL xbl(1), ybl(1)
	integer   prev_tn, PrevColour
	INCLUDE 'panelmod.inc'
C------------------BEGIN----------------------

	call PigGetSymbolColour(PrevColour)
	call PigGetWindowNum( prev_tn )

	call PigSetWindowNum( CONTROLWIN )

C       - define the box
        call PanelHitBox(colh, rowh, hitnum, 2, backgr )
        call PigSetSymbolNumber( SymbolNum )
        call PigSetSymbolColour( hcolor )
	xbl = coltox(colh)+1.5
	ybl = 0.3*rowtoy(rowh-1) + 0.7*rowtoy(rowh)

	call PigDrawSymbols( 1, xbl(1), ybl(1) )

	call PigSetWindowNum( prev_tn )
	call PigSetSymbolColour(PrevColour)
	END

C*----------------------------------------------------------------------*

      SUBROUTINE ISetNumHits( num )

C Purpose: To define the number of hit areas active in the right hand panel.
C Given  : num = number of hit areas.
C Returns: None.
C Effects: if num < MaxHits then numhits (in PANELMOD.INC) = num,
C          else numhits = MaxHits
C          GetPanelHit() will look for hits from 1 to num when determining if
C          a valid hit has been made.

C       - PASSED PARAMETERS
	integer num
	INCLUDE 'panelmod.inc'
*       SAVE
C------------------BEGIN----------------------

	numhits = num
	if(numhits .gt. MaxHits) numhits = MaxHits
	return

	entry IGetNumHit(num)
	num = numhits

	END

*--------------------------------------------------------
*
* define statement functions for calculating row, col to/from x,y
*
	real FUNCTION coltox(ipar)
      include 'panelmod.inc'
	integer ipar
	coltox = (ipar-1)*xdelta + pxmin
      return
      end

	real FUNCTION rowtoy(ipar)
      include 'panelmod.inc'
	integer ipar
	rowtoy = (25-1-ipar)*ydelta + pymin
      return
      end

	integer FUNCTION xtocol(rpar)
      include 'panelmod.inc'
	real    rpar 
	xtocol = int((rpar - pxmin)/xdelta)+1
      return
      end

	integer FUNCTION ytorow(rpar)
      include 'panelmod.inc'
	real    rpar 
c	intrinsic int
	ytorow = 25-1 + int((Pymin - rpar)/Ydelta)
      return
      end

C*--------------------------------------------------------------------------*

      SUBROUTINE PigFillArea( xmin, xmax, ymin, ymax, colour )

*+ Purpose:  To fill a rectangle
*+ Givens :  
*+      real Xmin, Xmax: the range in X coordinates
*+      real Ymin, Ymax: the range in Y coordinates
*+      integer Colour: the fill colour.
*+ Returns : None
*+ Effects:  The given rectangle is filled in with the given colour.

      REAL xmin, xmax, ymin, ymax
      integer colour
      REAL x(5), y(5)

      x(1) = xmin
      y(1) = ymin
      x(2) = xmax
      y(2) = ymin
      x(3) = xmax
      y(3) = ymax
      x(4) = xmin
      y(4) = ymax
      x(5) = xmin
      y(5) = ymin

      call PigFillPly(5, x, y, colour)
      END

* ------------------------------------------------------------------------- *
 
      subroutine PigSetJustification(NewJustification)

*+ Purpose:  Set the current Justification to NORMAL, LEFT, CENTRE, or RIGHT.
*+           All text printed is with the current Justification.
*+ Givens :  integer     NewJustification: the new text justification
*+ Returns:  None
*+ Effects:  Changes the current text justification to NewJustification

	include 'ipig.def'

      integer, save :: Justification = 1
      integer   NewJustification, CurJustification


      Justification = NewJustification
      call IPigSetTextAlignment(NewJustification, BOTTOM_JUSTIFY)
      return

* ------------------------------------------------------------------------- *

      entry PigGetJustification(CurJustification)

*+ Purpose:  Returns the current Justification.
*+ Givens :  None
*+ Returns:  ineteger    CurJustification: the current text justification
*+ Effects:  Resets the text justification

      CurJustification = Justification

      return

      end

* ========================================================================= *

        subroutine IPigSetTextAlignment(Horizontal, Vertical)

*+ Purpose: To set the text aligment both horizontally and vertically.
*+ Givens : integer      Horizontal   : horizontal alignment
*+          integer      Vertical     : vertical alignment
*+ Returns: None
*+ Effects: Changes text alignment.

	include 'ipig.def'

	integer           Horizontal, Vertical
        integer           Horizontalp
        integer           Verticalp
        character*80 Msg

        data    Horizontalp /LEFT_JUSTIFY/
        data    Verticalp   /BOTTOM_JUSTIFY/
        Verticalp = BOTTOM_ALIGN
        if (Horizontal .eq. LEFT_JUSTIFY) then
           Horizontalp = LEFT_ALIGN
	elseif (Horizontal .eq. CENTRE_JUSTIFY) then
	   Horizontalp = CENTRE_ALIGN
	elseif (Horizontal .eq. RIGHT_JUSTIFY) then
           Horizontalp = RIGHT_ALIGN
        else
           Horizontalp = LEFT_ALIGN
!          call WPigSetTextAlignment(Horizontalp, Verticalp)
           write(Msg,'(a,i8)')
     +    'Invalid setting for Horizontal justification:',Horizontal
           call PigMessageOK(Msg, ' ')
!          call PigUWait(2.0)
           Horizontalp = LEFT_ALIGN
        endif
        if (Vertical .eq. BASE_JUSTIFY) then
	   Verticalp = BASE_ALIGN
	elseif (Vertical .eq. BOTTOM_JUSTIFY) then
	   Verticalp = BOTTOM_ALIGN
	elseif (Vertical .eq. TOP_JUSTIFY) then
           Verticalp = TOP_ALIGN
        else
           Verticalp = BOTTOM_ALIGN
!          call WPigSetTextAlignment(Horizontalp, Verticalp)
           write(Msg,'(a,i8)')
     +    'Invalid setting for Vertical justification:',Vertical
           call PigMessageOK(Msg, ' ')
!           call PigUWait(2.0)
        endif
        
!       call WPigSetTextAlignment(Horizontalp, Verticalp)

        return
        end

CC*----------------------------------------------------------------------*
C                       END PANELMOD.FOR                                *
C*----------------------------------------------------------------------*

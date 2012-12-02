  !***********************************************************************
  !    Copyright (C) 1995-2007,
  !        Roy A. Walters, R. Falconer Henry, Adrian Dolling
  !
  !        rawalters@shaw.ca
  !
  !    This file is part of TriGrid2, a grid generation and editing program.
  !
  !    TriGrid2 is free software; you can redistribute it and/or
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
C                                                                       *
C                       PANELMOD.INC                                    *
C       Contains parameters for routines in PANELMOD.FOR which control  *
C       menuing in the right hand panel.                                *
C $Header: C:/TRIGRID/PIGSRC/PIG/RCS/PANELMOD.INC 5.5 95/09/04 09:51:58 ROOT_DOS Exp Locker: ROOT_DOS $
C
C $Log:	PANELMOD.INC $
C Revision 5.5  95/09/04  09:51:58  ROOT_DOS
C Release for XPig conversion
C 
C Revision 5.4  94/10/26  08:58:45  ROOT_DOS
C After many mods to make work in WPig, and GPig. Changed include pathnames.
C 
C Revision 5.3  94/06/23  17:53:44  ROOT_DOS
C Prior to shipping to Roy Walters
C 
C Revision 5.2  94/04/14  00:13:11  ROOT_DOS
C About to give copy of code to 1994 Camosun team
C 
C Revision 5.1  94/04/02  18:30:01  ROOT_DOS
C Prior to shipping to Roy for testing. Test2 compiles fine under HP unix.
C 
C Revision 5.0  93/07/08  15:02:29  ROOT_DOS
C After large scale changes to prompting routines, removed string lengths.
C 
C Revision 1.1  93/05/18  16:43:46  ROOT_DOS
C Initial revision
C 
C       Written: Mar/91 (JDM).                                          *
C       Included only in PANELMOD.INC, INIT.FOR for text attribute      *
C       initialization, and NODER/NODECONF.FOR for Pxmax & Pymax        *
C       values used in calculations for GPM() calls.                    *
C*----------------------------------------------------------------------*
C
C       Definition of RH Panel: 
C               RH Panel is 24 rows by 24 columns. Around this area is
C       a small border (0.8 x 0.8 WC units). RH Panel is defined in
C       GSELNT( 9 ) by:
C          GSWN definition = ( 9,  0.0,   40.0, 0.0, 100.0 ) WC
C          GSVP definition + ( 9, 0.655, 0.995, 0.0,  0.65 ) NDC
C 
C       COLUMNS:1 col = 1.6 WC units wide
C               24 cols
C               2 borders = 0.8 WC units each
C               1.6 * 24 = 38.4 
C               2 * 0.8 + 38.4 = 40 = x dimension of WC window
C               WC range  = 0.0 to 40.0 = 40 units
C               NDC range = 0.655 to 0.995 = .34 NDC units
C
C          ROWS:1 row = 4.1 WC units high
C               24 rows
C               2 borders = 0.8 WC units each
C               4.1 * 24 = 98.4
C               2 * 0.8 + 98.4 = 100 = y dimension of WC window
C               WC range  = 0.0 to 100.0 = 100 units
C               NDC range = 0.0 to 0.65 = .65 NDC units
C
C       Translation NDC coords to WC:
C         ( GRQLC routine returns NDC coords, not WC as per documentation )
C              WCx = ( NDCx - 0.655 ) * ( 40.0 / 34.0 ) * 100
C              WCy = NDCy * ( 100.0 / 65.0 ) * 100
C
C*----------------------------------------------------------------------*
C       - Hit Information Storage
C       -- MaxHits : maximum number of hit areas that may be defined at one 
C                    time, dimension of following arrays.
C       -- numhits : the actual number of currently defined hit areas.
C       -- hitrows() : stores row number for each defined hit area, rows
C                      start at the top of the screen (1) and increase
C                      towards the bottom (24). Rows are each 1 character
C                      in height.
C       -- hitcols() : stores starting (left-most) column number for each 
C                      defined hit area, columns start at the left side of 
C                      the screen (1) and increase towards the right (24).
C                      Columns are each 1 character in width.
C       -- hitlengs() : stores length, in number of columns (or characters), 
C                       for each defined hit area. For a given hit area, X, 
C                       hitcols(X) + hitlengs(X) = ending (right-most) column 
C                       number of the hit area.
C       -- hitnums()  : stores the number of the hit for each hit area. This
C                       was added to provide for unique hit numbers across
C                       all screens, in order to make a generic event handler
C                       mechanism for the CONTROLWIN screens.
C                       changed by agdolling june 1993

	INTEGER RHPMAXROWS
	INTEGER RHPMAXCOLS
	PARAMETER (RHPMAXROWS = 24, RHPMAXCOLS = 24)
	INTEGER MaxHits
	PARAMETER( MaxHits = RHPMAXROWS*2 )

	INTEGER numhits
	INTEGER hitrows(MaxHits), hitcols(MaxHits), hitlengs(MaxHits)
	INTEGER hitnums(MaxHits)
	LOGICAL hitenabled(MaxHits), hitistext(Maxhits)
	COMMON /PHITS/ numhits, hitrows, hitcols, hitlengs, hitnums,
     +    hitenabled, hitistext
	integer MAXHITLENGTH
	parameter (MAXHITLENGTH=RHPMAXCOLS)
	character*(MAXHITLENGTH) hittext(MaxHits)
	common /PHITS_TEXT/ hittext

C*----------------------------------------------------------------------*
C       - Text attribute limits (may be platform specific):

	REAL MaxExp, MaxSpc, MaxHgt, MaxWth
	REAL MinExp, MinSpc, MinHgt, MinWth
	INTEGER Maxfnt, MaxPrec, MaxHrz, MaxVert
	INTEGER MinFnt, MinPrec, MinHrz, MinVert

C       - GSS-GKS version 2.12 values
C       - no max expansion in manual, set reasonable limit
	PARAMETER( MaxExp = 100.0 )
	PARAMETER( MinExp = 0.0 )
C       - no max or min spacing in manual, set reasonable limits
	PARAMETER( MaxSpc = 100.0 )
	PARAMETER( MinSpc = 0.0 )
C       - no max or min height in manual, set reasonable limits
	PARAMETER( MaxHgt = 100.0 )
	PARAMETER( MinHgt = 0.0 )
C       - width not used in GSS-GKS v 2.12, make 0.0 only valid value
	PARAMETER( MaxWth = 0.0 )
	PARAMETER( MinWth =  0.0 )
C       - no max or min font in manual, set reasonable limits
	PARAMETER( MaxFnt = 100 )
	PARAMETER( MinFnt = -100 )
C       - valid precision values from manual are 0, 1, 2
	PARAMETER( MaxPrec = 2 )
	PARAMETER( MinPrec = 0 )
C       - valid horiz align values from manual are 0, 1, 2, 3
	PARAMETER( MaxHrz = 3 )
	PARAMETER( MinHrz = 0 )
C       - valid vert align values from manual are  0, 1, 2, 3, 4, 5
	PARAMETER( MaxVert = 5 )
	PARAMETER( MinVert = 0 )

C*----------------------------------------------------------------------*
C       - Text attributes (recommended on IBM compatible PC):
C               expansion = Pexp = (1.0)
C               spacing = Pspace = (0.0)
C               height = Pheight = (2.2)
C               width = Pwidth = (1.3)
C               font = Pfont = (1)
C               precision = Pprec = (1) = character precision
C               horizontal alignment = Palhorz = (1) = left
C               vertical alignment = Palvert = (4) = top

C       - Following values must be set in initialization routine
	REAL Pexp, Pspace, Pheight, Pwidth
	INTEGER Pfont, Pprec, Palhorz, Palvert
	COMMON /RHattribs/ Pexp, Pspace, Pheight, Pwidth, Pfont, 
     +                     Pprec, Palhorz, Palvert
c        namelist /txtattr_nml/ Pexp, Pspace, Pheight, Pwidth, Pfont,
c     +                  Pprec, Palhorz, Palvert
c       namelist /txtattr_nml/ Pexp, Pspace, Pheight, Pwidth, Pfont,
c     +                 Pprec, Palhorz, Palvert
C*----------------------------------------------------------------------*
C       - Define max & min limits to panel hit area.
	REAL Pxmin, Pxmax, Pymin, Pymax

	PARAMETER( Pxmin = 0.8 )
C       - below = 0.8 + ( 24.0 * 1.6 )
	PARAMETER( Pxmax = 39.2 )
	PARAMETER( Pymin = 0.8 )
	PARAMETER( Pymax = 99.2 )
	real    xdelta, ydelta
	parameter (xdelta = 1.6, ydelta = 4.1)
*
* define statement functions for calculating row, col to/from x,y
*
c	real coltox
c	real rowtoy
c	integer xtocol
c	integer ytorow
c	integer ipar
c	real    rpar 
c	intrinsic int
c	coltox(ipar) = (ipar-1)*xdelta + pxmin
c	rowtoy(ipar) = (25-1-ipar)*ydelta + pymin
c	xtocol(rpar) = int((rpar - pxmin)/xdelta)+1
c	ytorow(rpar) = 25-1 + int((Pymin - rpar)/Ydelta)

C*----------------------------------------------------------------------*
C                       END PANELMOD.INC                                *
C*----------------------------------------------------------------------*
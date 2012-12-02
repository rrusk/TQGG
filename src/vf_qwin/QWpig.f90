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

!******************************************************************************
!                  PIG Application Interface for Visual Fortran (DVF, CVF6, IFC)
!******************************************************************************
!+ Purpose: The PIG.FOR and PIG1.FOR modules contains all necessary subroutines to
!+          interface with a graphics system.  Many specific subroutines
!+          will need to be changed as PIGS is ported from one graphical
!+          environment to another.  All subroutines starting with an I, like
!+          IPIGSetVP are internal calls and are not available for PUBLIC use.
!+
!+          This module also contains routines to open, initialize, and close
!+          a graphics system.
!+
!******************************************************************************
	subroutine IPigOpenClose

!       PRIVATE
!-
!-      subroutine IPigOpenClose
!- Call Sequence:
!-       Not callable
!- Purpose: To open the graphis package, set defaults and windows
!- Givens : None
!- Returns: None
!- Effects: Not directly callable

	include 'ipig.def'
	logical         pig_open
	real            x1, x2, y1, y2


	data		pig_open        /.false./
!        save

!	call PigFatal('Cannot call IPigOpenClose directly.')
	return

! ------------------------------------------------------------------------- *
	entry PigOpenGraphicPkg(x1, x2, y1, y2)

!       PUBLIC
!+
!+      entry PigOpenGraphicPkg(x1, x2, y1, y2)
!+ Call Sequence:
!+       call PigOpenGraphicPkg(x1, x2, y1, y2)
!+ Purpose:  To initialize a graphics package, open and connect all graphics
!+           devices that are required for the Trigrid program to utilize
!+           a graphic system.
!+ Givens :  real        x1, x2, y1, y2 : world coordinates of (any) two
!+                                        diagonally oposite corners
!+ Returns:  None
!+ Effects:  Four default Window and Viewport transformations will be defined:
!+                    1 = MENUWIN, 2 = STATUSWIN, 3 = CONTROLWIN, and 4 = MAINWIN
!+           All devices initialized and error logging enabled.  If an error
!+           file already exists, it is deleted before the new error file
!+           is created.

      if(pig_open) then
!	    call PigFatal('Call to PigOpenGraphicPkg detected when already open.')
      endif

      call WPigOpenGraphicPkg(x1, x2, y1, y2)

      call PigSetWindowNum(MAINWIN)
!      call WPigSetForegrColour(Foregr)
!      call PigSetBackgroundColour(BLACK)


!***** sets up colour and text defaults
 !     call IPigSetDefaults


! set fill style to solid
!	 call IPigSetFillInteriorStyle(1)

! Set line type and colour then draw a box around each window/viewport.
	 call PigSetLineColour(FOREGR)
	 call PigSetTextColour(FOREGR)
	 call PigSetFillColour(FOREGR)

	 call PigSetWorldCoordinates(x1,x2,y1,y2)
      call PigSetWindowNum( MAINWIN )

	 call PigErase(MAINWIN)
	 call PigErase(STATUSWIN)
!	 call PigErase(MENUWIN)
	 call PigErase(CONTROLWIN)


! Make the MAINWIN window current and set the priority of the 0 viewport lower
! than all other viewports.
! Window priority order:     MAINWIN = CONTROLWIN = STATUSWIN = MENUWIN > DEFAULT
! Equivalent integer values:   1  =    2    =   3    =  4   >    0

! Set the active window      
      call PigSetWindowNum( MAINWIN )
      pig_open = .true.
      RETURN

! ------------------------------------------------------------------------- *
	entry PigCloseGraphicPkg
!       PUBLIC
!+
!+      entry PigCloseGraphicPkg
!+ Call Sequence:
!+       call PigCloseGraphicPkg
!+ Purpose: To shut down GKS and return to DOS
!+ Givens : None
!+ Returns: None
!+ Effects: Terminates workstation, and returns to DOS

      call WPigCloseGraphicPkg

      pig_open = .false.
      END

! END IPigOpenClose

!------------------------------------------------------------------*
	subroutine IPigMouseRoutines

!       PRIVATE
!-
!-      subroutine IPigMouseRoutines
!- Call Sequence:
!-      NOT CALLABLE
!- Purpose: To contain all mouse handling routines
!- Givens : None
!- Returns: None
!- Effects: Not directly callable

	include 'ipig.def'
	integer         Window

	integer         MouseButton
	real            ipx, ipy

	integer         PrevMouseWindow, PrevWindow
	real            PrevMouseX, PrevMouseY

	integer		curWindow
	
!        save

	return

! ------------------------------------------------------------------------- *
	entry PigGetMouseAndButton(Window, MouseButton, ipx, ipy)

!       PUBLIC
!+
!+      entry PigGetMouseAndButton(Window, MouseButton, ipx, ipy)
!+ Call Sequence:
!+       call PigGetMouseAndButton(Window, MouseButton, ipx, ipy)
!+ Purpose:  To activate the mouse. PigGetMouse returns the world
!+           coordinates (ipx, ipy) and the current normalization
!+           transformation number (Window number) of the current
!+           mouse location when the mouse button is released. To
!+           ensure the mouse cursor doesn't 'fly away' each time
!+           the mouse is clicked, PigGetMouse should be called
!+           with the same values as it returned on the previous
!+           call.
!+ Givens :  None
!+ Returns:  integer     Window: the normalization transformation number
!+                               where the mouse was clicked
!+           integer     MouseButton: Button number, from (0..n)
!+           real        ipx   : the x world coordinate of the mouse location
!+           real        ipy   : the y world coordinate of the mouse location
!+ Effects:  Routine waits for mouse input

! Move the window number into an integer*2 var. and re-initialize the
! last x/y coords.
	  
	  ipx = PrevMouseX
	  ipy = PrevMouseY

	  call WPigGetMouseAndButton (Window, MouseButton, ipx, ipy)

	  PrevMouseWindow = Window
	  PrevMouseX = ipx
	  PrevMouseY = ipy

	return

	entry PigGetRubberMouseAndButton(Window, MouseButton, ipx, ipy)

!       PUBLIC
!+
!+      entry PigGetRubberMouseAndButton(Window, MouseButton, ipx, ipy)
!+ Call Sequence:
!+       call PigGetRubberMouseAndButton(Window, MouseButton, ipx, ipy)
!+ Purpose:  To activate the mouse.
!+           Identical to PigGetMouseAndButton, except that a rubber line is drawn
!+           following the mouse, until clicked.
!+           PigGetRubberMouseAndButton returns the world
!+           coordinates (ipx, ipy) and the current normalization
!+           transformation number (Window number) of the current
!+           mouse location when the mouse button is released. To
!+           ensure the mouse cursor doesn't 'fly away' each time
!+           the mouse is clicked, PigGetMouseAndButton should be called
!+           with the same values as it returned on the previous
!+           call.
!+ Givens :  None
!+ Returns:  integer     Window: the normalization transformation number
!+                               where the mouse was clicked
!+           integer     MouseButton: Button number, from (0..n)
!+           real        ipx   : the x world coordinate of the mouse location
!+           real        ipy   : the y world coordinate of the mouse location
!+ Effects:  Routine waits for mouse input

! Move the window number into an integer*2 var. and re-initialize the
! last x/y coords.
	  

	  call PigGetWindowNum(PrevWindow)
	  call PigSetWindowNum(Window)
	  call PigGetWindowNum(CurWindow)
	  Window = -1
	  PrevMouseX = ipx
	  PrevMouseY = ipy
	  do while(Window.ne.CurWindow)
	  	ipx = PrevMouseX
	  	ipy = PrevMouseY
 	    call WPigGetRubberMouseAndButton(Window, MouseButton, ipx, ipy)
	  end do

	  PrevMouseWindow = Window
	  PrevMouseX = ipx
	  PrevMouseY = ipy

	  call PigSetWindowNum(PrevWindow)
	return

! ------------------------------------------------------------------------- *
	entry PigSetMouse(Window, ipx, ipy)

!       PUBLIC
!+
!+      entry PigSetMouse(Window, ipx, ipy)
!+ Call Sequence:
!+       call PigSetMouse(Window, ipx, ipy)
!+ Purpose:  To set the saved mouse position.
!+ Givens :  integer     Window: the normalization transformation number
!+                               in which to position the mouse
!+           real        ipx   : the x world coordinate of the mouse location
!+           real        ipy   : the y world coordinate of the mouse location
!+ Returns:  None
!+ Effects:  Routine waits for mouse input
!+
! Written:  Adrian Dolling -- June 1993

! Move the window number into an integer*2 var. and re-initialize the
! last x/y coords.
	PrevMouseWindow = Window
	PrevMouseX = ipx
	PrevMouseY = ipy
	return

! ------------------------------------------------------------------------- *
	entry PigGetMousePrev(Window, ipx, ipy)

!       PUBLIC
!+
!+      entry PigGetMousePrev(Window, ipx, ipy)
!+ Call Sequence:
!+       call PigGetMousePrev(Window, ipx, ipy)
!+ Purpose:  To retrieve the last known position of the mouse. 
!+           PigGetMousePrev returns the world
!+           coordinates (ipx, ipy) and the current normalization
!+           transformation number (Window number) of the last known
!+           mouse location when the mouse button was released. 
!+ Givens :  None
!+ Returns:  integer     Window: the normalization transformation number
!+                               in which to position the mouse
!+           real        ipx   : the x world coordinate of the mouse location
!+           real        ipy   : the y world coordinate of the mouse location
!+ Effects:  None.
!+
! Written:  Adrian Dolling -- June 1993

! Move the window number into an integer*2 var. and re-initialize the
! last x/y coords.
	Window = PrevMouseWindow
	ipx = PrevMouseX
	ipy = PrevMouseY
	return
      end
! ------------------------------------------------------------------------- *
	subroutine PigGetMouse(Window, ipx, ipy)

!       PUBLIC
!+
!+      subroutine PigGetMouse(Window, ipx, ipy)
!+ Call Sequence:
!+       call PigGetMouse(Window, ipx, ipy)
!+ Purpose:  To activate the mouse. PigGetMouse returns the world
!+           coordinates (ipx, ipy) and the current normalization
!+           transformation number (Window number) of the current
!+           mouse location when the mouse button is released. To
!+           ensure the mouse cursor doesn't 'fly away' each time
!+           the mouse is clicked, PigGetMouse should be called
!+           with the same values as it returned on the previous
!+           call.
!+ Givens :  None
!+ Returns:  integer     Window: the normalization transformation number
!+                               where the mouse was clicked
!+           real        ipx   : the x world coordinate of the mouse location
!+           real        ipy   : the y world coordinate of the mouse location
!+ Effects:  Routine waits for mouse input

	integer Window
	integer MouseButton
	real    ipx, ipy
	call PigGetMouseAndButton(Window, MouseButton, ipx, ipy)
	end

	subroutine PigGetRubberMouse(Window, ipx, ipy)

!       PUBLIC
!+
!+      subroutine PigGetRubberMouse(Window, ipx, ipy)
!+ Call Sequence:
!+       call PigGetRubberMouse(Window, ipx, ipy)
!+ Purpose:  To activate the mouse. 
!+           Identical to PigGetMouse, except that a rubber line is drawn
!+           following the mouse, until clicked.
!+           PigGetRubberMouse returns the world
!+           coordinates (ipx, ipy) and the current normalization
!+           transformation number (Window number) of the current
!+           mouse location when the mouse button is released. To
!+           ensure the mouse cursor doesn't 'fly away' each time
!+           the mouse is clicked, PigGetRubberMouse should be called
!+           with the same values as it returned on the previous
!+           call.
!+ Givens :  None
!+ Returns:  integer     Window: the normalization transformation number
!+                               where the mouse was clicked
!+           real        ipx   : the x world coordinate of the mouse location
!+           real        ipy   : the y world coordinate of the mouse location
!+ Effects:  Routine waits for mouse input

	integer Window
	integer MouseButton
	real    ipx, ipy
	call PigGetRubberMouseAndButton(Window, MouseButton, ipx, ipy)
	end

! ========================================================================= *

      SUBROUTINE DisplayIn2( mousex, mousey, Redrw )

! Purpose: Allows the display window to be defined one window level deeper.
! Givens : None
! Returns: Redrw - TRUE if window has been redefined and grid needs to be
!                  redrawn
!                - FALSE otherwise
! Effects: Window is redefined and contents redrawn.
      
      INCLUDE '../includes/graf.def'

      LOGICAL Redrw

      integer LIMWIN
      PARAMETER( LIMWIN = 50 )

! *** COMMON BLOCK VARIABLES ***
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL

      real mousex, mousey

!     BEGIN
      call PigSetLineColour(FOREGR)
      if ( LEVEL .ge. LIMWIN ) then
        call PigPutMessage( 'You are already at the lowest level..')
        Redrw = .FALSE.
      else
        call DEFWIN2(  mousex, mousey, Redrw )
!        if ( Redrw ) call WINBOX
      endif

      END

! ========================================================================= *

      SUBROUTINE DEFWIN2 ( xL, yL, Redrw )

! PURPOSE : Calls PigGetZoomArea to define the new WINDOW
! INPUT   : XL - LEFT WINDOW BOUNDARY
!           XH - RIGHT WINDOW BOUNDARY
!           YL - BOTTOM WINDOW BOUNDARY
!           YH - TOP WINDOW BOUNDARY
!
! RETURN  : Redrw - TRUE if a valid window was selected.
!                 - FALSE otherwise.
!                   COMMON AREA /WLIMIT/
!                   WXL - AS XL
!                   WXH - AS XH
!                   WYL - AS YL
!                   WYH - AS YH

      include '../includes/graf.def'

      LOGICAL Redrw

      integer LIMWIN
      PARAMETER( LIMWIN = 50 )

! *** COMMON AREA ***
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL,WXH,WYL,WYH,LEVEL

      REAL   CWXL, CWXH, CWYL, CWYH 
      COMMON /CURWIN/ CWXL,CWXH,CWYL,CWYH   

      REAL    xL, yL, xH, yH
      
      Redrw = .FALSE.

      call WPigGetZoomArea2(xL, yL, Xh, yH)

      if(LEVEL.gt.0) then
	    if((xL.eq.WXL(LEVEL)).and.(xH.eq.WXH(LEVEL))&
     &          .and.(yL.eq.WYL(LEVEL)).and.(yH.eq.WYH(LEVEL))) then
	      call PigPutMessage('No change in display window detected. Not redrawing!')
	      return
	    endif
      endif
      Redrw = .TRUE.
      LEVEL = LEVEL + 1
      WXL(LEVEL) = xL
      WXH(LEVEL) = xH
      WYL(LEVEL) = yL
      WYH(LEVEL) = yH
      cwxl = wxl(level)
      cwxh = wxh(level)
      cwyl = wyl(level)
      cwyh = wyh(level)

      END

! ========================================================================= *

      SUBROUTINE DisplayPan2( mousex, mousey, Redrw )

! Purpose: Redefine the center position of the current window to the same
!          or a different world coordinate.
! Givens : Multiple = 1 for move centre to cursor position
!                     2 for double move 
! Returns: Redrw  - TRUE  if the window definition changes
!                 - FALSE otherwise
! Effects: Window is redefined using the current LOCATOR device.

      INCLUDE '../includes/graf.def'

! Passed Parameters
      LOGICAL Redrw

      integer LIMWIN
      PARAMETER( LIMWIN = 50 )


! *** COMMON BLOCK VARIABLES ***
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL

      REAL CWXL, CWXH, CWYL, CWYH 
      COMMON /CURWIN/CWXL, CWXH, CWYL, CWYH

      integer win
      REAL xoffset, yoffset,mousex, mousey

!     BEGIN

      call PigSetLineColour(FOREGR)

      Redrw = .FALSE.

!      call PigPutMessage('Drag point to new location')
      
      xoffset = mousex
      yoffset = mousey
      
!      call PigPan2(xoffset, yoffset)
      call WPigDragRubberMouse(win, xoffset, yoffset)

      Redrw = .TRUE.
      LEVEL = LEVEL + 1
      if(level-1.le.0) then
        WXL(LEVEL) = CWXL + xoffset
        WXH(LEVEL) = CWXH + xoffset
        WYL(LEVEL) = CWYL + yoffset
        WYH(LEVEL) = CWYH + yoffset
      else
        WXL(LEVEL) = WXL(LEVEL-1) + xoffset
        WXH(LEVEL) = WXH(LEVEL-1) + xoffset
        WYL(LEVEL) = WYL(LEVEL-1) + yoffset
        WYH(LEVEL) = WYH(LEVEL-1) + yoffset
      endif
      cwxl = wxl(level)
      cwxh = wxh(level)
      cwyl = wyl(level)
      cwyh = wyh(level)

      return
      END

! ========================================================================= *

        subroutine PigErase(WinNum)

!  Purpose:  Blanks out a defined viewport by calling PigDrawFilledPolygon
!            with the fill colour set to the background colour. This gives
!            the effect of erasing an are on the screen. Called before text
!            is output to a viewport.
!  Givens :  integer        WinNum      :  window to erase
!  Returns:  None
!  Effects:  Erases an entire window, then draws the corresponding
!            border outline.

        include  'ipig.def'
        integer         N
        parameter       (N = 5)
        integer         PrevLineColour, PrevFillColour,WinNum, PrevWin

        real    x3(3),y3(3)

        real    CtrlX(N), CtrlY(N) 

        Data    CtrlX(1), CtrlX(2), CtrlX(3), CtrlX(4), CtrlX(5)&
     &          /XCtrlMin, XCtrlMin, XCtrlMax, XCtrlMax, XCtrlMin/
        Data    CtrlY(1), CtrlY(2), CtrlY(3), CtrlY(4), CtrlY(5)&
     &          /YCtrlMin, YCtrlMax, YCtrlMax, YCtrlMin, YCtrlMin/


! save current states
        call PigGetWindowNum(PrevWin)
        call PigGetLineColour(PrevLineColour)
        call PigGetFillColour(PrevFillColour)


        call PigSetLineColour(white)
        call PigSetFillColour(BACKGR)
! Next line is added for testing the controlwin layouts       

      IF (WinNum .EQ. CONTROLWIN) THEN
! draw filled polygon then an outline box
         call PigSetWindowNum(CONTROLWIN)
         call PigSetFillColour(BACKGR)
         call PigDrawFilledPolygon (N, CtrlX, CtrlY)
!         call PigGetWorldCoordinates(x1, x2, y1, y2)
         x3(1) = XCtrlMin
         x3(2) = XCtrlMin
         y3(1) = YCtrlMin         
         y3(2) = YCtrlMax            
         call PigSetLineColour(white)
         call PigDrawPolyline (2, x3, y3)

      ELSEIF (WinNum .EQ. STATUSWIN) THEN
! draw filled polygon then an outline box
!         call PigGetWorldCoordinates(x1, x2, y1, y2)
         call PigPutMessage (' ')
         call PigSetWindowNum(STATUSWIN)
         x3(1) = XStatusMin
         x3(2) = XStatusMax
         y3(1) = YStatusMin  
         y3(2) = YStatusMin          
         call PigSetLineColour(white)
         call PigDrawPolyline (2, x3, y3)

      ELSEIF (WinNum .EQ. MAINWIN) THEN
        call WPigEraseMain ()
      ELSE
!           call PigFatal('Invalid window requested...')
      ENDIF

! restore states
        call PigSetFillColour(PrevFillColour)
        call PigSetLineColour(PrevLineColour)
        call PigSetWindowNum(PrevWin)
        end

! ========================================================================= * 
!******************************************************************************
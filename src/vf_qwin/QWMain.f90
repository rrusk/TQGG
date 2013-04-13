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

!     Interface to subroutine WPigSetWorldCoordinates[C] (x1,x2,y1,y2)
!         real x1        
!         real x2
!         real y1
!         real y2
!     End

!     Interface to subroutine WPigGetWorldCoordinates[C] (x1,x2,y1,y2)
!         real x1 [reference]
!         real x2 [reference]
!         real y1 [reference]
!         real y2 [reference]
!     End

!     Interface to subroutine WPigSetLineColour[C] (LineColour)
!         integer LineColour
!     End

!     Interface to subroutine WPigSetLineWidth[C] (LineWidth)
!         real LineWidth
!     End

!     Interface to subroutine WPigSetSymbolColour[C] (SymbolColour)
!         integer SymbolColour
!     End

!     Interface to subroutine WPigSetSymbolNumber[C] (SymbolNumber)
!         integer SymbolNumber
!     End

!     Interface to subroutine WPigSetSymbolSize[C] (SymbolSize)
!         real SymbolSize
!     End

!     Interface to subroutine WPigSetTextColour[C] (TextColour)
!         integer TextColour
!     End

!     Interface to subroutine WPigSetFillColour[C] (FillColour)
!         integer FillColour
!     End

!     Interface to subroutine WPigSetBackgrColour[C] (BackgrColour)
!         integer BackgrColour
!     End

!     Interface to subroutine WPigSetForegrColour[C] (ForegrColour)
!         integer ForegrColour
!     End

!     Interface to subroutine WPigDrawSymbols [C] (np, x, y)
!         integer   np
!         real      x(np)
!         real      y(np)
!     End

!     Interface to subroutine WPigDrawPolyLine[c] (n, x, y)
!         integer n
!         real x [reference] (n)
!         real y [reference] (n)
!     End

!     Interface to subroutine WPigDrawFilledPolygon[c] (n, x, y)
!         integer n
!         real x [reference] (n)
!         real y [reference] (n)
!     End

!     Interface to subroutine WPigSetWindowNum[c] (WinNum)
!         integer*2 WinNum
!     End

!     Interface to subroutine WPigDrawText[C] ( PosX, PosY, Text)
!         real PosX
!         real PosY
!         character*(*) Text [far, reference]
!     End

!     Interface to subroutine WPigGetMouseAndButton[C](Window, Button, x, y)
!         integer     Window  [far, reference]
!         integer     Button  [far, reference]
!         real        x       [far, reference]
!         real        y       [far, reference]
!     End

!     Interface to subroutine WPigEraseMain[C] ()
!     End

!       Interface to subroutine WPigGetString[C] (PromptString,
!     +           CharLen, RetString)
!           character*(*) PromptString [far, reference]
!           integer       CharLen [far, reference]
!           character*(*) RetString [far, reference]
!       End

!       Interface to character*1 function WPigCursYesNo[C] (messg)
!           character*(*)  messg [far, reference]
!       End

!       Interface to subroutine WPigGetZoomArea[C] (x1, x2, y1, y2)
!           real  x1 [reference]
!           real  x2 [reference]
!           real  y1 [reference]
!           real  y2 [reference]
!       End

!       Interface to logical function
!     +    WPigGetOpenFileName[C] (prompt, name, template)
!          character*(*)  prompt [far, reference]
!          character*(*)  name [far, reference]
!          character*(*)  template [far, reference]
!       End

! ===================================================================== *

!+ Purpose:  Initialise PIG graphics, control execution and interaction 
!+           with menu items, mouse and keyboard, then on exit, close the
!+           PIG graphics system.
!+           Control program execution awaiting and handling user events.
!+           The precise nature of interaction is system dependent, some
!+           systems are unable to simultaneously await mouse and keyboard
!+           inputs. The user may control the next expected action, or
!+           exit PigMain by setting the return value in one of the
!+           UserXXXEventHandler user supplied functions.
!+ Givens :
!+       integer function UserInitialiser()
!+              A user supplied function with the above declaration, that
!+              provides the required actions prior to any events being
!+              detected. It is called after the graphics package hass
!+              been opened, and before menus are initialized.
!+              It must initialise the menus, display any default 
!+              information, and set the MAINWIN world coordinates.
!+              UserInitialiser() must return a value of MOUSE_EVENT,
!+              KEYBOARD_EVENT or QUIT_EVENT.
!+              The returned value controls the first kind of event sought.
!+              It will normally be set to MOUSE_EVENT, which is also used
!+              to return MENU_EVENTs.
!+       integer function UserMenuEventHandler(integer MenuItem)
!+              A user supplied function with the above declaration, that
!+              provides the required actions in response to the user
!+              choosing a menu item.
!+              UserInitialiser() must return a value of MOUSE_EVENT,
!+              KEYBOARD_EVENT or QUIT_EVENT.
!+              The returned value controls the next kind of event sought.
!+              It will normally be set to MOUSE_EVENT
!+       integer function UserMouseEventHandler
!+               (integer Window, integer MouseButton,
!+                       real MouseX, real MouseY)
!+              A user supplied function with the above declaration, that
!+              provices the required actions in response to the user
!+              taking an action with the mouse. At present the detected
!+              actions are limited to button releases (GKS restriction),
!+              and no detection of which button was pressed is implemented
!+              (non portable in GKS).
!+              UserMouseEventHandler() must return a value of MOUSE_EVENT,
!+              KEYBOARD_EVENT or QUIT_EVENT.
!+              The returned value controls the next kind of event sought.
!+              It will normally be set to MOUSE_EVENT
!+       integer function UserStringEventHandler
!+               (integer Clen, character*(Clen) String)
!+              A user supplied function with the above declaration, that
!+              provides the required actions in response to the user taking
!+              an action with the keyboard.
!+              UserStringEventHandler() must return a value of MOUSE_EVENT,
!+              KEYBOARD_EVENT or QUIT_EVENT.
!+       integer FirstInputType
!+              Must be set to either MOUSE_EVENT or KEYBOARD_EVENT.
!+              This argument controls the first kind of event sought. It
!+              will normally be set to MOUSE_EVENT
!+ Returns:  None
!+ Effects:  Controls interaction with user, passing next events to one of
!+           the three user supplied functions. Does not return until one of
!+           the user supplied functions returns QUIT_EVENT.
!+|========================================================================
!******************************************************************************
 
      Program Main
 

        USE DFLIB
 
        include 'ipig.def'

        integer RetVal
        integer EventType
        integer MenuItem
        integer Window
        integer MouseButton
        real    MouseX
        real    MouseY
        integer Clen
        character*(120) Message
        character*(80) String
!        logical a
 
        call PigInitFont
 
        call WPigOpenGraphicPkg(0.0,1.0,0.0,1.0)

!*******************************
!
! call user initialiser routine
!
!*******************************
!       RetVal must be set to MENU_EVENT, MOUSE_EVENT, KEYBOARD_EVENT or QUIT_EVENT
 
        call Initialiser()
        retval = MOUSE_EVENT
 
        String = ' '
 
!******************************
!
! CATCH AND THROW EVENTS UNTIL DONE
!
!******************************
        do while(RetVal .ne. QUIT_EVENT)
!*******************************
!
! CATCH NEXT EVENT
!
!*******************************
          if((RetVal .eq. MOUSE_EVENT).OR.(RetVal .eq. MENU_EVENT)) then
            EventType = NULL_EVENT
!            result = WAITONMOUSEEVENT (MOUSE$LBUTTONDOWN,keystate, moux, mouy)
!            call MouseEventCB(unit,mouseevent,keystate,moux,mouy)
!            call GetMouseOption(Window,MouseButton,MouseX, MouseY)
!            EventType = MOUSE_EVENT
!             exit
             do while(EventType .eq. NULL_EVENT)
              call sleepqq(100)
!              call GetMenuOption(MenuItem)
              MenuItem = NULL_EVENT
              if(MenuItem .eq. NULL_EVENT) then
                call GetMouseOption(Window,MouseButton,MouseX, MouseY)
                if(MouseButton.ne.Null_Event) then
                  EventType = MOUSE_EVENT
                endif
              else
                 EventType = MENU_EVENT
              endif
            end do
!               !have a valid mouse event

          elseif(RetVal .eq. KEYBOARD_EVENT)then
!               !possibly the string device should be initialized first?
                String = ' '
                Clen = len_trim(String)
                EventType = STRING_EVENT
          elseif(RetVal .eq. QUIT_EVENT) then
!               !QUIT_EVENT should never occur here, but we are really looking
!               !for non QUIT_EVENT errors
          elseif(RetVal .eq. PRINT_EVENT) then
                EventType = PRINT_EVENT
                call PigPutMessage('PRINT_EVENT not implemented yet.')
          else
                write(message,'(a,i6)')'Unrecognised type of next event', RetVal
!                call PigFatal(message)
          endif
!*******************************
!
! THROW EVENT TO A USER EVENT HANDLER
!
!*******************************
!           !policy here, clear the message line prior to throwing any events
          call PigEraseMessage
!*******************************
!
! THROW MENU EVENT
!
!*******************************
          if (EventType .eq. MENU_EVENT) then
            call MNU_MainMenuDisable
!                RetVal = EditorMenuEHandler(MenuItem)
!*******************************
!
! THROW MOUSE EVENT
!
!*******************************
          elseif (EventType .eq. MOUSE_EVENT) then
!               !policy here...delete the menu when a selected item is active
!                RetVal=EditorMouseEHandler(Window,MouseButton,MouseX,MouseY)
            call MouseEHandler(Window,MouseButton,MouseX,MouseY)
          else
!               !catch all, no change from previous...
                RetVal = RetVal
          endif
!          call MNU_MainMenuEnable
!          if(RetVal.eq.NodeMenuType) then
!            call MNU_GridMenuDisable
!            call MNU_NodeMenuEnable
            RetVal = MOUSE_EVENT
!          elseif(RetVal.eq.GridMenuType) then
!            call MNU_NodeMenuDisable
!            call MNU_GridMenuEnable
!            RetVal = MOUSE_EVENT
!          endif
        end do

      end
!!  ======================================================================== *

      subroutine WPigExit()
      
      USE DFLIB
      
      integer status
      
! Clear the workstation
      call clearscreen( $GCLEARSCREEN )
      status = SETEXITQQ(QWIN$EXITNOPERSIST)

      stop
      
      end

!!  ======================================================================== *
!!                         open/close routines                               *
!* ========================================================================= *

      subroutine IWPigOpenClose

        USE DFLIB

!       PRIVATE

!       subroutine IPigOpenClose
!  Call Sequence:
!        Not callable
!  Purpose: To open the graphis package, set defaults and windows
!  Givens : None
!  Returns: None
!  Effects: Not directly callable

      EXTERNAL MouseEventCB

      integer moustatus
      logical statusmode
	integer result
	type (qwinfo) winfo
      type (windowconfig) vc

	include 'ipig.def'

	integer         status, numrows
	real            aspect
	real            x1, y1, x2, y2
	real ystatus, xright, ybot, xrightmw
    REAL    xdcmax, ydcmax
!        save

	return

! ------------------------------------------------------------------------- *
	entry WPigOpenGraphicPkg(x1, x2, y1, y2)


!       PUBLIC
! 
!       entry WPigOpenGraphicPkg(x1, x2, y1, y2)
!  Call Sequence:
!        call WPigOpenGraphicPkg(x1, x2, y1, y2)
!  Purpose:  To initialize a graphics package, open and connect all graphics
!            devices that are required for the Trigrid program to utilize
!            a graphic system.
!  Givens :  real        x1, x2, y1, y2 : world coordinates of (any) two
!                                         diagonally oposite corners
!  Returns:  None
!  Effects:  Four default Window and Viewport transformations will be defined:
!                     1 = MENUWIN, 2 = STATUSWIN, 3 = CONTROLWIN, and 4 = MAINWIN
!            All devices initialized and error logging enabled.  If an error
!            file already exists, it is deleted before the new error file
!            is created.
! 
! *** open display for Visual Fortran
      
!      winfo.TYPE = QWIN$MAX
!      result = SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)

!      modestatus = setvideomode( $VRES16COLOR )
      vc.numxpixels  = -1
      vc.numypixels  = -1
      vc.numtextcols = -1
!      vc.numtextrows = -1
!      vc.numcolors   = -1
      vc.fontsize    = #000C0018     !16x24 pixels
      vc.title  = "main"C
      statusmode = setwindowconfig ( vc )
      if(.NOT. statusmode) statusmode = setwindowconfig( vc )

      statusmode = getwindowconfig ( vc )
      xdcmax = vc.numxpixels - 1
      if(xdcmax.lt.750) then
	  vc.fontsize = #0007000C
        statusmode = setwindowconfig ( vc )
        if(.NOT. statusmode) statusmode = setwindowconfig( vc )
	endif

      statusmode = getwindowconfig ( vc )
      xdcmax = vc.numxpixels - 1
      ydcmax = vc.numypixels - 1
      xright = 0.25*xdcmax
	numrows = vc.numtextrows
	if(numrows.le.0) numrows = 25
	ystatus = ydcmax/numrows
!  compensate for window border
      if(xdcmax.ge.850) then
	  ydcmax = (1.- 0.08)*ydcmax
	elseif(xdcmax.ge.500) then
	  ydcmax = (1.-2.5/22.5)*ydcmax
      else
 	  ydcmax = (1.-3.5/22.5)*ydcmax
      endif

! Calculate the aspect ratio for this device.
      if(xdcmax.gt.0.) then
	  aspect = (ydcmax-ystatus)/xdcmax
	else
	  aspect = .75
	endif
      if(aspect .lt. .75) then
	  xrightmw = ydcmax - ystatus
	  xright = xdcmax - 0.4*ydcmax
	  ybot = ydcmax
      else
	  xright = (1. - 0.3)*xdcmax
	  ybot = 0.75*xdcmax
      endif

!**** sets up colour and text defaults
!      call IPigSetDefaults


! Main Window definition T1 (where all grids are drawn)
      call IPigSetWN(MAINWIN, x1, x2, y1, y2)
      call IPigSetVP(MAINWIN,	0., xright-1., ydcmax, ystatus+1.)
!	 XMinMainView*scale1,
!     +                        XMaxMainView*scale1,
!     +                        (0.75-YMinMainView)*scale,
!     +                        (0.75-YMaxMainView)*scale )

! Right window definition T2 ( Used for RH Panel window )
      call IPigSetWN(CONTROLWIN, XCtrlMin, XCtrlMax, YCtrlMin, YCtrlMax)
      call IPigSetVP(CONTROLWIN, xright, xdcmax, ybot, ystatus+1.)
!	XMinControlView*scale1,
!     +                           XMaxControlView*scale1,
!     +                           (0.75-YMinControlView)*scale,
!     +                           (0.75-YMaxControlView)*scale)

! Right window definition T?( Used for RH Panel window )
!      call IPigSetWN(PROFILEWIN, XCtrlMin, XCtrlMax, YCtrlMin, YCtrlMax)
!      call IPigSetVP(PROFILEWIN,  xright, xdcmax, ybot, ystatus+1.)
!	XMinControlView*scale1,
!     +                           XMaxControlView*scale1,
!     +                           (0.75-YMinControlView)*scale,
!     +                           (0.75-YMaxControlView)*scale)

! Vertical STATUSWIN window T3 (Old T3 (=3) Used by plot model program only)
      call IPigSetWN(STATUSWIN, XStatusMin, XStatusMax,YStatusMin, YStatusMax)
      call IPigSetVP(STATUSWIN, 0., xdcmax, ystatus, 0.)
!	XMinStatusView*scale1,
!     +                          XMaxStatusView*scale1,
!     +                          (0.75-YMinStatusView)*scale,
!     +                          (0.75-YMaxStatusView)*scale)

! Top window definition T4
!      call IPigSetWN(MENUWIN, XMenuMin, XMenuMax, YMenuMin, YMenuMax  )
!      call IPigSetVP(MENUWIN, 0., xdcmax, 1., 0.)
!	XMinMenuView*scale1,
!     +                        XMaxMenuView*scale1,
!     +                        (0.75-YMinMenuView)*scale,
!     +                        (0.75-YMaxMenuView)*scale)

! set fill style to solid
!      call IPigSetFillInteriorStyle(1)

! Set line type and colour then draw a box around each window/viewport.
!      call GSPLI(1)
      call PigSetLineColour(FOREGR)
      call PigSetTextColour(FOREGR)
      call PigSetFillColour(FOREGR)

      call PigSetWorldCoordinates(x1,x2,y1,y2)
      call PigSetWindowNum( MAINWIN )
      call PigErase(MAINWIN)
      call PigErase(STATUSWIN)
!      call PigErase(MENUWIN)
      call PigErase(CONTROLWIN)

! Set the active window      
      call PigSetWindowNum( MAINWIN )
! Position mouse - default is in centre of MAINWIN
    !!  call PigSetMouse(MAINWIN, ((x1+x2)/2.), ((y1+y2)/2.))

! Register the mouse events
      moustatus = REGISTERMOUSEEVENT(0,MOUSE$LBUTTONDOWN,MouseEventCB)
	if(moustatus.lt.0) then
	  status = MESSAGEBOXQQ('Bad mouse unit'C,' 'C,MB$OK)
	endif

      winfo.TYPE = QWIN$MAX
      result = SETWSIZEQQ(0, winfo)
      result = SETWSIZEQQ(0, winfo)

      RETURN

      END

!*******************************
!
! Window routines
!
!*******************************

      subroutine IPigDefineWindows

        USE DFLIB


!       subroutine IPigDefineWindows
!  Call Sequence:
!                Not directly callable
!  Purpose:  Houses different window routines
!  Givens :  None
!  Returns:  None
!  Effects:  Not directly callable

        include 'ipig.def'
        integer ::  WinNum, GetNum
        integer*2   status

        real ::     x1, x2, y1, y2
        real*8, save ::          wx1(5), wx2(5), wy1(5), wy2(5)
        integer*2, save ::       nx1(5), nx2(5), ny1(5), ny2(5)

        return

! ------------------------------------------------------------------------- *

        entry IPigSetWN(WinNum, x1, x2, y1, y2)

!       PRIVATE

!       entry IPigSetWN(WinNum, x1, x2, y1, y2)
!  Call Sequence:
!        call IPigSetWN(WinNum, x1, x2, y1, y2)
!  Purpose:  Sets world coordinates for Window WinNum
!  Givens :  integer     WinNum       : window number to set up
!            real        x1, x2          : x coordinates in WC (x1 < x2)
!            real        y1, y2          : y coordinates in WC (y1 < y2)
!  Returns:  None
!  Effects:  Defines WinNum area in WC

        wx1(WinNum) = x1
        wx2(WinNum) = x2
        wy1(WinNum) = y1
        wy2(WinNum) = y2

        return

! ------------------------------------------------------------------------- *

        entry PigGetWN(GetNum, x1, x2, y1, y2)

!       PRIVATE

!       entry PigGetWN(WinNum, x1, x2, y1, y2)
!  Call Sequence:
!        call PigGetWN(WinNum, x1, x2, y1, y2)
!  Purpose:  Sets world coordinates for Window WinNum
!  Givens :  integer     WinNum       : window number to set up
!            real        x1, x2          : x coordinates in WC (x1 < x2)
!            real        y1, y2          : y coordinates in WC (y1 < y2)
!  Returns:  None
!  Effects:  Gets WinNum area in WC


        x1 = wx1(GetNum)
        x2 = wx2(GetNum)
        y1 = wy1(GetNum)
        y2 = wy2(GetNum)

        return

! ------------------------------------------------------------------------- *

        entry WPigSetWindowNum (WinNum)
          if(.not.( (WinNum.eq.MAINWIN).or. (WinNum.eq.MENUWIN)&
            .or. (WinNum.eq.STATUSWIN).or. (WinNum.eq.CONTROLWIN)) ) then
	        WinNum = MAINWIN
          endif

          call setviewport( nx1(WinNum), ny2(WinNum),nx2(WinNum), ny1(WinNum) )
          status = setwindow( .TRUE., wx1(WinNum), wy1(WinNum),wx2(WinNum), wy2(WinNum) )
        return

! ------------------------------------------------------------------------- *

	entry IPigSetVP(WinNum, x1, x2, y1, y2)

!       PRIVATE

!       entry IPigSetVP(WinNum, x1, x2, y1, y2)
!  Call Sequence:
!        call IPigSetVP(WinNum, x1, x2, y1, y2)
!  Purpose:  Sets device coordinates for viewport WinNum
!  Givens :  integer     WinNum       : viewport number to set up
!            real        x1, x2          : x coordinates in NDC (x1 < x2)
!            real        y1, y2          : y coordinates in NDC (y1 < y2)
!  Returns:  None
!  Effects:  Defines viewprt are in NDC


        nx1(WinNum) = int2(x1)
        nx2(WinNum) = int2(x2)
        ny1(WinNum) = int2(y1)
        ny2(WinNum) = int2(y2)

	return

! ------------------------------------------------------------------------- *

	entry PigGetVP(GetNum, x1, x2, y1, y2)

!       PRIVATE

!       entry PigGetVP(WinNum, x1, x2, y1, y2)
!  Call Sequence:
!        call PigGetVP(WinNum, x1, x2, y1, y2)
!  Purpose:  Sets device coordinates for viewport WinNum
!  Givens :  integer     WinNum       : viewport number to set up
!            real        x1, x2          : x coordinates in NDC (x1 < x2)
!            real        y1, y2          : y coordinates in NDC (y1 < y2)
!  Returns:  None
!  Effects:  Gets viewprt in NDC

        x1 = nx1(GetNum)
        x2 = nx2(GetNum)
        y1 = ny1(GetNum)
        y2 = ny2(GetNum)

        return

! ------------------------------------------------------------------------- *

        entry WPigSetWorldCoordinates (x1,x2,y1,y2)

          wx1(1) = x1
          wx2(1) = x2
          wy1(1) = y1
          wy2(1) = y2

!   find world coordinate aspect ratio
          if(x2.gt.x1) then
	      aspectW = (y2-y1)/(x2-x1)
          else
	      aspectW=1.
          endif

          vx1 = nx1(1)
          vx2 = nx2(1)
          vy1 = ny1(1)
          vy2 = ny2(1)
!          call PigGetVP(1,vx1,vx2,vy1,vy2)
          if(vx2.gt.vx1) then
	      aspectVP = (vy1-vy2)/(vx2-vx1)
          else
	      aspectVP=1.
          endif

          aspectW = aspectW/aspectVP
          If(aspectW.gt.1.) then
	      range = y2-y1
  	      wy2(1) = y2 + 0.02*range
	      wy1(1) = y1 - 0.02*range
	      wx2(1) = (x2+x1)/2. + 0.52*range/aspectVP
	      wx1(1) = (x2+x1)/2. - 0.52*range/aspectVP
          else
	      range = x2-x1
	      wx2(1) = x2 + 0.02*range
	      wx1(1) = x1 - 0.02*range
	      wy2(1) = (y2+y1)/2. + aspectVP*0.52*range
	      wy1(1) = (y2+y1)/2. - aspectVP*0.52*range
          endif

        return

! ------------------------------------------------------------------------- *

        entry WPigGetWorldCoordinates (x1,x2,y1,y2)

          x1 = wx1(1)
          x2 = wx2(1)
          y1 = wy1(1)
          y2 = wy2(1)
        return

! ------------------------------------------------------------------------- *

!        entry WPigSetProfileCoordinates (x1,x2,y1,y2)
!
!          wx1(4) = x1
!          wx2(4) = x2
!          wy1(4) = y1
!          wy2(4) = y2

!        return

! ------------------------------------------------------------------------- *

!        entry WPigGetProfileCoordinates (x1,x2,y1,y2)
!
!          x1 = wx1(4)
!          x2 = wx2(4)
!          y1 = wy1(4)
!          y2 = wy2(4)

!        return

      end

      subroutine WPigEraseMain ()
	  include  'ipig.def'
	  integer n
	  real	MainX(5), MainY(5)

! draw filled polygon then an outline box
	  call PigSetWindowNum(MAINWIN)
	  call PigGetWorldCoordinates(x1, x2, y1, y2)
	  MainX(1) = x2
	  MainY(1) = y2
	  MainX(2) = x1
	  MainY(2) = y2
	  MainX(3) = x1
	  MainY(3) = y1
	  MainX(4) = x2
	  MainY(4) = y1
	  MainX(5) = x2
	  MainY(5) = y2
	  n = 5
	  call PigSetFillColour(BACKGR)
	  call PigDrawFilledPolygon(N, MainX, MainY)
!	  call PigDrawPolyline (5, MainX, MainY)
!           call PigGetWorldCoordinates(x1, x2, y1, y2)	  
      End
 
!*******************************
!
! Drawing routines
!
!*******************************

        subroutine WPigSetLineColour (LineColour)
          USE DFLIB
          integer LineColour
          integer*2 LineColour2, status
          LineColour2 = INT(LineColour)
          status = setcolor(LineColour2)
        End

        subroutine WPigSetLineWidth (LineWidth)
            real LineWidth,dummy
            dummy=LineWidth
        End

        subroutine WPigSetSymbolColour (SymbolColour)
          USE DFLIB
          integer SymbolColour
          integer*2 LineColour2, status
          LineColour2 = INT(SymbolColour)
          status = setcolor(LineColour2)
        End

        subroutine WPigSetSymbolNumber (SymbolNumber)
            integer SymbolNumber,dummy
            dummy =SymbolNumber
        End

        subroutine WPigSetSymbolSize (SymbolSize)
            real SymbolSize,dummy
            dummy = SymbolSize
        End

        subroutine WPigSetTextColour (TextColour)
          USE DFLIB
          integer TextColour
          integer*2 LineColour2, status
          LineColour2 = INT(TextColour)
          status = settextcolor(LineColour2)
          status = setcolor(LineColour2)
        End

        subroutine WPigSetFillColour (FillColour)
          USE DFLIB
          integer FillColour
          integer*2 LineColour2, status
          LineColour2 = INT(FillColour)
          status = setcolor(LineColour2)
        End

        subroutine WPigSetBackgrColour (BackgrColour)
          USE DFLIB
          integer BackgrColour
          integer*2 LineColour2, status
          LineColour2 = INT(BackgrColour)
          status = setcolor(LineColour2)
        End

        subroutine WPigSetForegrColour (ForegrColour)
          USE DFLIB
          integer ForegrColour
          integer*2 LineColour2, status
          LineColour2 = INT(ForegrColour)
          status = setcolor(LineColour2)
        End

! ========================================================================= *

      subroutine IWPigDrawPackage

        USE DFLIB

!       subroutine IWPigDrawPackage
!  Call Sequence:
!        call IWPigDrawPackage
!  Purpose:  Houses various drawing routines
!  Givens :  None
!  Returns:  None
!  Effects:  Not directly callable

        record /xycoord/ vxy
        record /wxycoord/ wxy
        record /wxycoord/ wpoly(20)

        include 'ipig.def'

        integer         n, WinNum

	real            x1, y1, x3(2),y3(2)
	real            x(*), y(*)
        real*8          xdbl, ydbl
	real	StatX(5), StatY(5)
	Data    StatX(1), StatX(2), StatX(3), StatX(4), StatX(5) &
     &          /XStatusMin, XStatusMin, XStatusMax,XStatusMax, XStatusMin/
	Data    StatY(1), StatY(2), StatY(3), StatY(4), StatY(5) &
     &          /YStatusMin, YStatusMax, YStatusMax,YStatusMin, YStatusMin/
	character *(*)  Text

! local copies of saved variables
!	integer*2       LastWinNum
!       real            MainwinTextHeight, MenuwinTextHeight
!       real            ControlwinTextHeight, StatuswinTextHeight
! temporary variables        
	integer         i, status4, SymbolNum, NewColour
        integer*2       TextColour
	integer*2       np, status, vcx, vcy, vx, vy, dx, dl
        real SymbolSize, DefaultSize
!       save
	return

! ------------------------------------------------------------------------- *
	entry WPigDrawPolyline(n, x, y)


!      PUBLIC

!       entry PigDrawPolyline(n, x, y)
!  Call Sequence:
!        call PigDrawPolyline(n, x, y)
!  Purpose:  Draw a line in the current LineColour.
!  Givens :  integer     n      :    number of points
!            real        x(n)   :    X coordinates of points in WC
!            real        y(n)   :    Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws n number of lines of x,y coordinates

	  xdbl = x(1)
	  ydbl = y(1)    
        call moveto_w( xdbl, ydbl, wxy )

	  do i=2,n
	      xdbl = x(i)
	      ydbl = y(i)    
	      status = lineto_w( xdbl, ydbl )
	  end do

	return

! ------------------------------------------------------------------------- *
	entry WPigDrawFilledPolygon(n, x, y)

!       PUBLIC

!       entry PigDrawFilledPolygon(n, x, y)
!  Call Sequence:
!        call PigDrawFilledPolygon(n, x, y)
!  Purpose: Draws a filled polygon of n points of x,y coordinates
!  Givens :  integer     n     :   number of points
!            real        x(n)  :    X coordinates of points in WC
!            real        y(n)  :    Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws a filled polgon
 
	  np = n
	  do i=1,n
	    wpoly(i).wx = x(i)
	    wpoly(i).wy = y(i)
	  enddo
	         
	  status = polygon_w( $GFILLINTERIOR, wpoly, np )

	return

! ------------------------------------------------------------------------- *
	entry WPigDrawSymbols(n, x, y)

!       PUBLIC

!       entry PigDrawSymbols(n, x, y)
!  Call Sequence:
!        call PigDrawSymbols(n, x, y)
!  Purpose:  Draw current symbol in current FillColour at n points in x, y
!  Givens :  integer     n      :     number of points
!            real        x(n)   :     X coordinates of points in WC
!            real        y(n)   :     Y coordinates of points in WC
!  Returns:  None
!  Effects:  Draws the current symbol

      call PigGetSymbolNumber(SymbolNum)
      call PigGetSymbolSize(SymbolSize)

      DefaultSize = 5.
      dx = int2(DefaultSize*SymbolSize+0.5)
      dl = int2(0.707*DefaultSize*SymbolSize+0.5)
     
      do i=1,n

        xdbl = x(i)
        ydbl = y(i)
        call moveto_w( xdbl, ydbl, wxy )
        status4 = setpixel_w( xdbl, ydbl )    
        call GetViewCoord_w( xdbl, ydbl, vxy )
        vcx = vxy.xcoord
        vcy = vxy.ycoord

        if(SymbolNum.eq.2.or.SymbolNum.eq.3) then
          vy = vcy + dx
          call moveto( vcx, vy, vxy )
          vy = vcy - dx
          status = lineto( vcx, vy )
          vx = vcx + dx
          call moveto( vx, vcy, vxy )
          vx = vcx - dx
          status = lineto( vx, vcy )
        endif
        if(SymbolNum.eq.3.or.SymbolNum.eq.5) then
          vx = vcx + dl
          vy = vcy + dl
          call moveto( vx, vy, vxy )
          vx = vcx - dl
          vy = vcy - dl
          status = lineto( vx, vy )
          vx = vcx - dl
          vy = vcy + dl
          call moveto( vx, vy, vxy )
          vx = vcx + dl
          vy = vcy - dl
          status = lineto( vx, vy )
        endif

        if(SymbolNum.eq.4) then
          vx = vcx + dx
          vy = vcy - dx
          vcx = vcx - dx
          vcy = vcy + dx
          status = rectangle( $GBORDER, vcx,vcy,vx,vy )
        elseif(SymbolNum.eq.6) then
          vy = vcy + dx
          call moveto( vcx, vy, vxy )
          vx = vcx + dx
          status = lineto( vx, vcy )
          vy = vcy - dx
          status = lineto( vcx, vy )
          vx = vcx - dx
          status = lineto( vx, vcy )
          vy = vcy + dx
          status = lineto( vcx, vy )
        elseif(SymbolNum.eq.7) then
          vx = vcx + dx
          vy = vcy - dx
          vcx = vcx - dx
          vcy = vcy + dx
          status = ellipse( $GBORDER, vcx,vcy,vx,vy )
        endif

      end do

      return

! ------------------------------------------------------------------------- *

      entry WPigDrawText(x1, y1, Text)
!       PUBLIC

!       entry PigDrawText(x1, y1, Text)
!  Call Sequence:
!        call PigDrawText(x1, y1, Text)
!  Purpose:  Draw Text string at x1, y1, with current TextColour and Justification
!  Givens :  real        x1     :    X coordinate of Text position in WC
!            real        y1     :    Y coordinate of Text position in WC
!            character *(*) Text:    string of characters
!  Returns:  None
!  Effects:  Draws Text at given x,y coordinates in current text color
! 



        call PigGetTextColour(NewColour)
        if(NewColour.lt.0) call PigPutMessage('Illegal colour number, must be > 0')
        TextColour = mod(NewColour,16)
        status = setcolor(TextColour)

        call PigGetWindowNum(WinNum)
        If(WinNum.eq.STATUSWIN) then
          call PigSetFillColour(BACKGR)
          call PigDrawFilledPolygon (5, StatX, StatY)
          call WPigPutText(1,3,Text)
          x3(1) = XStatusMin
          x3(2) = XStatusMax
          y3(1) = YStatusMin  
          y3(2) = YStatusMin          
          call PigSetLineColour(white)
          call PigDrawPolyline (2, x3, y3)
        else
          xdbl = x1
          ydbl = y1    
          call moveto_w( xdbl, ydbl, wxy )
          call outgtext( Text )
        endif
        return
      end

! ========================================================================= *

      subroutine PigDrawText(x1, y1, Text)
!+ Purpose:  Draw Text string at x1, y1, with current TextColour and Justification
!+ Givens :  real        x1     :    X coordinate of Text position in WC
!+           real        y1     :    Y coordinate of Text position in WC
!+           character *(*) Text:    string of characters
!+ Returns:  None
!+ Effects:  Draws Text at given x,y coordinates in current text color

      real x1, y1
      character *(*)  Text
      character*(256) aStr
      integer lenstr

	aStr = Text
	lenstr = LEN_trim(Text)
	lenstr = min(255,lenstr)
	aStr(lenstr+1:) = char(0)
	call WPigDrawText(x1, y1, aStr)
	return

	end

! ------------------------------------------------------------------------- *

      Subroutine WPigPutText(row,col,Text)

        USE DFLIB

!       Subroutine PigPutText(row,col,Text)
!  Call Sequence:
!        call PigPutText(row,col,Text)
!  Purpose:  Puts text (Text) at set locations (row,col)
!  Givens :  character*(*)    Text   :  a variable length character string
!  Returns:  None
!  Effects:  Prints Text (trailing blanks are
!            removed) in the current text colour

	include 'ipig.def'

	type (rccoord) rc

	character*(*)   Text
        integer         row, col
        integer*2       row2, col2
        integer         PrevColour

        call PigGetTextColour(PrevColour)
        call PigSetTextColour(MESSAGE_COLOR)

        row2 = row
        col2 = col
        call SetTextPosition(row2,col2,rc)
        call outtext( Text  ) !(:Len) )

        call PigSetTextColour(PrevColour)

	end

! ------------------------------------------------------------------------- *

      subroutine PigSetTextColour(NewColour)

!  Purpose:  Set the current text colour for use to NewColour.  Text are
!            drawn in the current text colour.
!  Givens :  integer     NewColour: the new text colour
!  Returns:  None
!  Effects:  changes the text colour to NewColour

      integer, save ::   TextColour=15
      integer   NewColour, CurColour

      if (NewColour.lt.0) then
        call PigPutMessage('Illegal colour number, must be > 0')
      else
        TextColour = mod(NewColour,16)
        call WPigSetTextColour(TextColour)
      endif

      return
! ------------------------------------------------------------------------- *
      entry PigGetTextColour(CurColour)

!  Purpose:  Returns the current text colour
!  Givens :  None
!  Returns:  integer     CurColour: the current text colour
!  Effects:  Resets the text colour

      CurColour = TextColour
      return

      end

! ------------------------------------------------------------------------- *

      subroutine WPigDragRubberMouse (Win, xdif, ydif)

        USE DFLIB
        include 'ipig.def'

        type (xycoord) t2
        integer(2) ipx1,ipy1,ipx2,ipy2,stat2
        integer result,mevent,keystate,ipx,ipy
        integer win
        REAL Xend, Yend, Xinit, Yinit, Xdif, Ydif

        call PigSetWindowNum( MAINWIN )

!  establish viewport
        Win = MAINWIN
        call PigGetWN( Win, x1, x2, y1, y2 )
        dxWN = x2 - x1
        dyWN = y2 - y1
        Xdif = Xdif - x1
        Ydif = Ydif - y1

        call PigGetVP( Win, x1, x2, y1, y2 )
        dxVP = x2 - x1
        dyVP = y2 - y1
        Xdif = Xdif*dxVP/dxWN + x1
        Ydif = Ydif*dyVP/dyWN + y1

!        call PigPutMessage('Drag point to new location')
        call PigSetLineColour(RUBBER_COLOR)

      Xinit = xdif
      Yinit = ydif

      keystate = MOUSE$KS_LBUTTON

	ipx1 = Xinit-x1
	ipy1 = Yinit-y2
	Xend = Xinit
	Yend = Yinit
	mevent=MOUSE$LBUTTONUP.OR.MOUSE$MOVE
	do while (keystate.eq.MOUSE$KS_LBUTTON)
	  call sleepqq(50)
          result=WaitOnMouseEvent(mevent,keystate,ipx,ipy)
	  stat2 = setwritemode( $GXOR )
	  call moveto(ipx1,ipy1,t2)
	  ipx2 = Xend-x1
	  ipy2 = Yend-y2
	  stat2 = lineto(ipx2,ipy2)
	  Xend = ipx
	  Yend = ipy
	  ipx2 = Xend-x1
	  ipy2 = Yend-y2
	  call moveto(ipx1,ipy1,t2)
	  stat2 = lineto(ipx2,ipy2)
	enddo
      Xdif = Xinit-Xend
	  Ydif = Yinit-Yend

! convert to world coordinates
      Xdif = Xdif*dxWN/dxVP
      Ydif = Ydif*dyWN/dyVP

	  stat2 = setwritemode( $GPSET )
!        call PigSetMouse(win, XLoc, YLoc)

        End

! ------------------------------------------------------------------------- *

      subroutine WPigRubberLine(Xnew, Ynew, Xinit, Yinit)

        USE DFLIB

        implicit none

        include 'ipig.def'

        type (xycoord) t2
        integer(2) ipx1,ipy1,ipx2,ipy2,stat2
        integer result,mevent,keystate,ipx,ipy
        integer win
!        integer PrevWin, PrevColour
        real x1,x2,y1,y2,xW1,yW1,dxWN,dyWN,dxVP,dyVP
        REAL Xend, Yend
        REAL  Xnew, Ynew, Xinit, Yinit

!        call PigGetMouse( win, Xnew, Ynew )
!        return

        call PigSetWindowNum( MAINWIN )

!  establish viewport
        Win = MAINWIN
        call PigGetWN( Win, xW1, x2, yW1, y2 )
        dxWN = x2 - xW1
        dyWN = y2 - yW1
        call PigGetVP( Win, x1, x2, y1, y2 )
        dxVP = x2 - x1
        dyVP = y2 - y1

!        call PigPutMessage('Drag point to new location')
        call PigSetLineColour(RUBBER_COLOR)
        ipx1 = x1 + (Xinit-xW1)*dxVP/dxWN
        ipy1 = y1 - y2 + (Yinit-yW1)*dyVP/dyWN
        Xend = ipx1 !Xinit
        Yend = ipy1 + y2 !Yinit
        mevent=MOUSE$LBUTTONDOWN.OR.MOUSE$MOVE
        do !while (keystate.ne.MOUSE$KS_LBUTTON)
          call sleepqq(50)
          result=WaitOnMouseEvent(mevent,keystate,ipx,ipy)
          stat2 = setwritemode( $GXOR )
          call moveto(ipx1,ipy1,t2)
          ipx2 = Xend-x1
          ipy2 = Yend-y2
          stat2 = lineto(ipx2,ipy2)
          Xend = ipx
          Yend = ipy
          ipx2 = Xend-x1
          ipy2 = Yend-y2
          call moveto(ipx1,ipy1,t2)
          stat2 = lineto(ipx2,ipy2)
          if (keystate.eq.MOUSE$KS_LBUTTON) exit
        enddo

! convert to world coordinates
        Xnew= xW1 + (Xend-x1)*dxWN/dxVP
        Ynew= yW1 + (Yend-y1)*dyWN/dyVP

        stat2 = setwritemode( $GPSET )

      end

!!  ======================================================================== *
!!                              mouse event routines                               *
!* ========================================================================= *
        
        subroutine IPigMouseEventRoutines

      USE DFLIB

!       PRIVATE

!       subroutine IPigMouseRoutines
!  Call Sequence:
!       NOT CALLABLE
!  Purpose: To contain all mouse handling routines
!  Givens : None
!  Returns: None
!  Effects: Not directly callable


        include 'ipig.def'
! *** used by mouse event handler
      integer MouseItem, MousePointer
        real MouseX, MouseY, px, py
        data px,py/0.,0./
      integer unit,dummy

        integer  ::       Window
        integer :: Window2=1

!        integer         MouseButton
        integer    mouseevent,keystate, moux, mouy
!        , result
!        real            ipx, ipy
        real            x1, x2, y1, y2

!        integer         PrevMouseWindow
!        real            PrevMouseX, PrevMouseY
!        character*(80)  Message
        

!        data  PrevMouseWindow /MAINWIN/
!        data  PrevMouseX, PrevMouseY /0.0, 0.0/
        data  MousePointer/Null_Event/

        return

! ------------------------------------------------------------------------- *
        entry MouseEventCB(unit,mouseevent,keystate,moux,mouy)

        dummy = unit
! *** register CB, set MousePointer, MouseX, MouseY

        MousePointer = mouseevent
          px = moux
          py = mouy
! *** find correct window and scale x,y
!          Window2 = MENUWIN
!          call PigGetVP( Window2, x1, x2, y1, y2 )
!          if(py.le.y1) then
!            dx = x2 - x1
!            dy = y2 - y1
!            px = px - x1
!            py = py - y1
!            call PigGetWN( Window2, x1, x2, y1, y2 )
!            px = x1 + px*(x2-x1)/dx
!            py = y1 + py*(y2-y1)/dy
!          else
            Window2 = STATUSWIN
            call PigGetVP( Window2, x1, x2, y1, y2 )
            if(py.le.y1) then
              dx = x2 - x1
              dy = y2 - y1
              px = px - x1
              py = py - y1
              call PigGetWN( Window2, x1, x2, y1, y2 )
              px = x1 + px*(x2-x1)/dx
              py = y1 + py*(y2-y1)/dy
            else
              Window2 = MAINWIN
              call PigGetVP( Window2, x1, x2, y1, y2 )
              if(px.le.x2) then
                dx = x2 - x1
                dy = y2 - y1
                px = px - x1
                py = py - y1
                call PigGetWN( Window2, x1, x2, y1, y2 )
                px = x1 + px*(x2-x1)/dx
                py = y1 + py*(y2-y1)/dy
              else
                Window2 = CONTROLWIN
                call PigGetVP( Window2, x1, x2, y1, y2 )
                dx = x2 - x1
                dy = y2 - y1
                px = px - x1
                py = py - y1
                call PigGetWN( Window2, x1, x2, y1, y2 )
                px = x1 + px*(x2-x1)/dx
                py = y1 + py*(y2-y1)/dy
              endif
            endif
!          endif
          return

        entry GetMouseOption(Window,MouseItem,MouseX,MouseY)
          if(MousePointer.ne.Null_Event) then
            MouseItem = MousePointer
            MousePointer = Null_Event
              Window = Window2
              MouseX = px
              MouseY = py
          else
            MouseItem = Null_Event
          endif
        return

        entry ResetMouseOption !(Window,MouseItem,MouseX,MouseY)
          MousePointer = Null_Event
!          MouseItem = Null_Event
        return

        end
! -------------------End IPigMouseEventRoutines---------------------------- *

!*******************************
!
! String routines
!
!*******************************

        subroutine PigInitFont

        USE DFLIB

!       PUBLIC

!       subroutine PigInitFont
!  Call Sequence:
!        call PigInitFont
!  Purpose: To set the initial text font.
!  Returns: None
!  Effects: Registers the font.

!        character*10 fontname /"t'helv'"/
!        character*30 list
        integer*2  numfonts, Fontp

!        if(registerfonts( 'c:\f32\lib\helvb.fon' ).lt.0) then
!          call PigFatal('Cannot locate font file')
!        endif

        numfonts = INITIALIZEFONTS() 

!        list = fontname // 'h15w7b'
        Fontp = setfont('t''fixedsys''h15w7b')

        return
        end

! ========================================================================= *

      SUBROUTINE WPigGetString( PromptString, nchar, RetString )

!+ Purpose: Prompt the user to input a string from the keyboard.
!+ Givens:  character*(*) PromptString : String to use as the prompt.
!+ Returns: character*80  RetString    : character return string.  Blank if an
!+                                       error occurred.
!+ Effects: User is prompted for appropriate input.

      include 'ipig.def'

      CHARACTER*(*) PromptString
      CHARACTER*(*) RetString
      integer       nchar
      integer PrevColour

!------------------BEGIN--------------------

      call PigGetTextColour(PrevColour)
      call PigSetTextColour(PROMPT_COLOR)

      call PigPutMessage(PromptString)
!      call WPigGetString0(PromptString, nchar, RetString)
      read(*,'(a)') RetString
      nchar = len_trim( RetString )

      if(nchar.eq.-1) then  !cancel
        RetString(1:1)= char(0)
      elseif(nchar.eq.0) then
	    RetString = ' '
      endif
      call PigEraseMessage
      call PigSetTextColour(PrevColour)
      END

! ========================================================================= *

!      subroutine WPigGetString0 (PromptString,CharLen, RetString)
!        character*(*) PromptString
!        integer       CharLen
!        character*(*) RetString

!        read(*,'(a)') RetString
!        CharLen = len_trim( RetString )

!      End

! ========================================================================= *

      Subroutine WPigPutMessage(Text)

!- Purpose:  Draws a text message (Text) at set locations in the STATUSWIN
!-           window, clears STATUSWIN first then draws
!- Givens :  character*(*)    Text   :  a variable length character string
!- Returns:  None
!- Effects:  Blanks the STATUSWIN, then prints Text (trailing blanks are
!-           removed) in the current text colour in the STATUSWIN window

        include 'ipig.def'

        character(*)   Text
        integer         PrevWin
        integer         len

        len = len_trim(Text)
        call PigGetWindowNum(PrevWin)
        call PigSetWindowNum(STATUSWIN)

        call PigDrawText(STATUSX, STATUSY, Text(:Len))
        call PigSetWindowNum(PrevWin)
        

        end

!- ========================================================================= *

      subroutine WPigEraseMessage

!- Purpose:  Blanks out a defined viewport by calling IPigErase(WinNum)
!-           with the fill colour set to the background colour. This gives
!-           the effect of erasing an are on the screen. Called before text
!-           is output to a viewport.
!- Givens :  None
!- Returns:  None
!- Effects:  Erases the entire status window, then draws the corresponding
!-           border outline.

      include 'ipig.def'

      call PigErase(STATUSWIN)
      end

!***********************************************************************

      Subroutine WPigStatusMessage(Text)

!       Subroutine WPigMessageOK(Text, title)
!       Call Sequence:
!         call WPigMessageOK(Text,title)
!       Purpose:  Creates a message window with (Text) and the caption (title).
!            Exit with the button OK.
!       Givens :  character*(*)    Text   :  a variable length character string
!            character*(*)    Title  :  a variable length character string
!       Returns:  None
!       Effects:  Prints Text (trailing blanks are
!            removed) in a message window.

        character*(*)   Text
        integer(4)      len1

        len1 = len_trim(text)
        call PigPutMessage(text(:len1))

      end

!***********************************************************************

      Subroutine WPigMessageOK(Text,title)

        USE DFLIB

!       Subroutine WPigMessageOK(Text, title)
!       Call Sequence:
!         call WPigMessageOK(Text,title)
!       Purpose:  Creates a message window with (Text) and the caption (title).
!            Exit with the button OK.
!       Givens :  character*(*)    Text   :  a variable length character string
!            character*(*)    Title  :  a variable length character string
!       Returns:  None
!       Effects:  Prints Text (trailing blanks are
!            removed) in a message window.

        character*(*)   Text,title
        integer(4)      len1, len2, result

        len1 = len_trim(text)
        len2 = len_trim(title)
        result = MessageBoxQQ(text(:len1)//char(0),title(:len2)//char(0),MB$OK)

      end

!***********************************************************************

      subroutine WPigCursYesNo (reply, messg)

        USE DFLIB

!---------------------------------------------------------------------*
!       subroutine WPigCursYesNo (reply, messg )

!       PUBLIC

!       subroutine WPigCursYesNo (reply, messg)
!       Call Sequence:
!         call WPigCursYesNo (reply, messg)
!       Purpose: Provide ability to respond to yes/no prompt using cursor input.
!       Given  : messg = prompt message to respond to
!       Returns: reply = Y or N
!       Effects: messg is displayed in dialog box.
!       Usage: To use this routine, pass the appropriate parameters, and assign
!          return values to "ans" & test "ans(1:1)", as in Prompt() calls.
!
!---------------------------------------------------------------------*

! - PASSED PARAMETERS
        CHARACTER(*) messg
        character*1 reply

! - LOCAL VARIABLES
        integer result,length

! --------- BEGIN--------
        length = LEN_TRIM(messg)
        result = MESSAGEBOXQQ(messg(:length)//char(0),''C, MB$ICONQUESTION.OR.MB$YESNO)
        if(result.eq.MB$IDNO) then
          reply = 'N'
        else
          reply = 'Y'
        endif
      End

!***********************************************************************

      subroutine WPigCursYesNoCancel (reply, messg)

        USE DFLIB

!       subroutine WPigCursYesNoCancel (reply,messg, messglen)

!       PUBLIC

!       subroutine WPigCursYesNoCancel (reply,messg)
!       Call Sequence:
!         call WPigCursYesNoCancel (reply,messg)
!       Purpose: Provide ability to respond to yes/no/cancel prompt using cursor input.
!       Given  : messg = prompt message to respond to
!       Returns: reply = Y, N, or C
!       Effects: messg is displayed in dialog box.
!       Usage: To use this routine, pass the appropriate parameters, and assign
!           return values to "ans" & test "ans(1:1)", as in Prompt() calls.

!! ---------------------------------------------------------------------*

! - PASSED PARAMETERS
        CHARACTER*(*) messg
        character*1 reply

! - LOCAL VARIABLES
        integer result,length

! --------- BEGIN--------
        length = LEN_TRIM(messg)
        result = MESSAGEBOXQQ(messg(:length)//char(0),''C, MB$ICONQUESTION.OR.MB$YESNOCANCEL)
        if(result.eq.MB$IDNO) then
          reply = 'N'
        elseif(result.eq.MB$IDYES) then
          reply = 'Y'
        elseif(result.eq.MB$IDCANCEL) then
          reply = 'C'
        endif

      end

!***********************************************************************

        subroutine WPigGetZoomArea0 (XLow, XHigh, YLow, YHigh)

          USE DFLIB

          include 'ipig.def'

!          type (xycoord) t2
          integer(2) ipx1,ipy1,ipx2,ipy2,stat2
          integer result,mevent,keystate,ipx,ipy
          integer win
          REAL Xend, Yend, Xinit, Yinit, Xdif, Ydif
          REAL XLow, XHigh, YLow, YHigh

          call PigSetWindowNum( MAINWIN )
          call PigSetLineColour(RUBBER_COLOR)

!  establish viewport
          Win = MAINWIN
          call PigGetWN( Win, xW1, x2, yW1, y2 )
          dxWN = x2 - xW1
          dyWN = y2 - yW1
          call PigGetVP( Win, x1, x2, y1, y2 )
          dxVP = x2 - x1
          dyVP = y2 - y1

!          call PigPutMessage('Drag point to new location')

          mevent=MOUSE$LBUTTONDOWN
          Xinit = -9999.
          Yinit = -9999.
          do while(Xinit.lt.x1.or.Xinit.gt.x2.or.Yinit.lt.y2.or.Yinit.gt.y1)
            result=WaitOnMouseEvent(mevent,keystate,ipx,ipy)
            Xinit = ipx
            Yinit = ipy
          ENDDO

          ipx1 = Xinit-x1
          ipy1 = Yinit-y2
          Xend = Xinit
          Yend = Yinit
          mevent=MOUSE$LBUTTONUP.OR.MOUSE$MOVE
          do while (keystate.eq.MOUSE$KS_LBUTTON)
            result=WaitOnMouseEvent(mevent,keystate,ipx,ipy)
            stat2 = setwritemode( $GXOR )
            ipx2 = Xend-x1
            ipy2 = Yend-y2
            stat2 = rectangle($GBORDER,ipx1,ipy1,ipx2,ipy2)
            Xend = ipx
            Yend = ipy
            ipx2 = Xend-x1
            ipy2 = Yend-y2
            stat2 = rectangle($GBORDER,ipx1,ipy1,ipx2,ipy2)
          enddo
          Xdif = Xinit-Xend
          Ydif = Yinit-Yend
! convert to world coordinates
          Xdif = Xdif*dxWN/dxVP
          Ydif = Ydif*dyWN/dyVP
          Xlow = xW1 + min(Xinit-x1,Xend-x1)*dxWN/dxVP
!         Ylow = yW1 + min(y1-Yinit,y1-Yend)*dxWN/dxVP
          Ylow = yW1 - min(y1-Yinit,y1-Yend)*dyWN/dyVP
          XHigh = Xlow + abs(Xdif)
          YHigh = YLow + abs(Ydif)

          stat2 = setwritemode( $GPSET )
    !!      call PigSetMouse(win, XLoc, YLoc)
          return

        End

!***********************************************************************

        subroutine WPigGetZoomArea2 (XLow, YLow, XHigh, YHigh)

          USE DFLIB

          include 'ipig.def'

!          type (xycoord) t2
          integer(2) ipx1,ipy1,ipx2,ipy2,stat2
          integer result,mevent,keystate,ipx,ipy
          integer win
          REAL Xend, Yend, Xinit, Yinit, Xdif, Ydif
          REAL XLow, XHigh, YLow, YHigh

          call PigSetWindowNum( MAINWIN )
          call PigSetLineColour(RUBBER_COLOR)

!  establish viewport
          Win = MAINWIN
          call PigGetWN( Win, xW1, x2, yW1, y2 )
          dxWN = x2 - xW1
          dyWN = y2 - yW1
          Xdif = XLow - xW1
          Ydif = YLow - yW1

          call PigGetVP( Win, x1, x2, y1, y2 )
          dxVP = x2 - x1
          dyVP = y2 - y1
          Xdif = Xdif*dxVP/dxWN + x1
          Ydif = Ydif*dyVP/dyWN + y1

!          call PigPutMessage('Drag point to new location')

          Xinit = xdif
          Yinit = ydif

          keystate = MOUSE$KS_LBUTTON

          ipx1 = Xinit-x1
          ipy1 = Yinit-y2
          Xend = Xinit
          Yend = Yinit
          mevent=MOUSE$LBUTTONUP.OR.MOUSE$MOVE
          do while (keystate.eq.MOUSE$KS_LBUTTON)
            result=WaitOnMouseEvent(mevent,keystate,ipx,ipy)
            stat2 = setwritemode( $GXOR )
            ipx2 = Xend-x1
            ipy2 = Yend-y2
            stat2 = rectangle($GBORDER,ipx1,ipy1,ipx2,ipy2)
            Xend = ipx
            Yend = ipy
            ipx2 = Xend-x1
            ipy2 = Yend-y2
            stat2 = rectangle($GBORDER,ipx1,ipy1,ipx2,ipy2)
          enddo
          Xdif = Xinit-Xend
          Ydif = Yinit-Yend

! convert to world coordinates
          Xdif = Xdif*dxWN/dxVP
          Ydif = Ydif*dyWN/dyVP
          Xlow = xW1 + min(Xinit-x1,Xend-x1)*dxWN/dxVP
          Ylow = yW1 - min(y1-Yinit,y1-Yend)*dyWN/dyVP
          XHigh = Xlow + abs(Xdif)
          YHigh = YLow + abs(Ydif)

          stat2 = setwritemode( $GPSET )

          return

        End

!***********************************************************************

        logical function WPigGetOpenFileName (prompt, name, template)

          USE DFLIB

          character*(*)  prompt 
          character*(*)  name 
          character*(*)  template

          call PigPutMessage(prompt)

          call setmessageqq(template,QWIN$MSG_FILEOPENDLG)
          open(93,file=' ')
          inquire(unit=93,name=Name)
          close(93,status='keep')
          WPigGetOpenFileName = .true.

          call PigEraseMessage

        End

!***********************************************************************

        logical function WPigOpenFileCD(nunit, prompt, fname, template)

        USE DFLIB

!        logical function PigOpenFileCD(nunit, prompt, name, template)
!  Call Sequence:
!               open(unit=lun, file=name, status='unknown',...)
!  Purpose:  To select the name of an existing or new file, open the file,
!            and change the default directory
!  Givens :  integer            nunit - logical unit number
!            character*(*)      prompt - contains a message displayed with
!                               the request for a file name.
!            character*(*)      template - contains file name template
!                               including wild cards etc. 
!                               May require system dependent implementation.
!                               May be ignored completely.
!  Returns : logical            PigOpenFileCD 
!                                       is .TRUE. if the user selected a 
!                                       valid filename, which is returned
!                                       in name. The file must exist.
!                                       is .FALSE. if the user cancelled
!                                       the operation.
!            character*(*)      name - contains a valid filename of an
!                               existing file.
!  Effects:  Opens a file 

        integer nunit
        character *(*) fname, prompt, template
!     Following 7 lines for Windows browsing
        integer(4) len, len2, fnlen
        logical(4) resOK
        character(256) pathbuf
        character(40) fdrive
        character(256) fdir
        character(256) fname8
        character(40) fext

        call PigPutMessage(prompt)

        call setmessageqq( template, QWIN$MSG_FILEOPENDLG )
        open(nunit, file=' ')
        inquire(unit=nunit,name=fName)

        len = fullpathqq(fname, pathbuf)
        len2 = splitpathqq(pathbuf,fdrive,fdir,fname8,fext)
        fnlen = LEN_TRIM(fname8)
        lenext = LEN_TRIM(fext)
        lendrv = LEN_TRIM(fdrive)
        fname = fname8(:fnlen)//fext(:lenext)
        resOK = changedirqq(fdrive(:lendrv)//fdir(:len2))
        WPigOpenFileCD = resOK

        call PigEraseMessage

        return
        end
      
        logical function WPigCWD(fname)

        USE DFLIB

        character *(*) fname
!     Following 7 lines for Windows browsing
        integer(4) len, len2, fnlen
        logical(4) resOK
        character(256) pathbuf
        character(40) fdrive
        character(256) fdir
        character(256) fname8
        character(40) fext

        len = fullpathqq(fname, pathbuf)
        len2 = splitpathqq(pathbuf,fdrive,fdir,fname8,fext)
        fnlen = LEN_TRIM(fname8)
        lenext = LEN_TRIM(fext)
        lendrv = LEN_TRIM(fdrive)
!        fname = fname8(:fnlen)//fext(:lenext)
        resOK = changedirqq(fdrive(:lendrv)//fdir(:len2))
        WPigCWD = resOK
 
        return
        end

!***********************************************************************
     
        logical function WPigOpenFile(nunit, prompt, fname, template)

        USE DFLIB

!        logical function PigOpenFile(nunit, prompt, name, template)
!  Call Sequence:
!               open(unit=lun, file=name, status='unknown',...)
!  Purpose:  To select the name of an existing or new file, open the file,
!  Givens :  integer            nunit - logical unit number
!            character*(*)      prompt - contains a message displayed with
!                               the request for a file name.
!            character*(*)      template - contains file name template
!                               including wild cards etc. 
!                               May require system dependent implementation.
!                               May be ignored completely.
!  Returns : logical            PigOpenFile 
!                                       is .TRUE. if the user selected a 
!                                       valid filename, which is returned
!                                       in name. The file must exist.
!                                       is .FALSE. if the user cancelled
!                                       the operation.
!            character*(*)      name - contains a valid filename of an
!                               existing file.
!  Effects:  Opens a file 

        integer nunit
        character *(*) fname, prompt, template
!     Following 7 lines for Windows browsing
        integer(4) len, len2, fnlen
        character(256) pathbuf
        character(2) fdrive
        character(256) fdir
        character(8) fname8
        character(4) fext

        call PigPutMessage(prompt)

        call setmessageqq( template, QWIN$MSG_FILEOPENDLG )
        open(nunit, file=' ')
        inquire(unit=nunit,name=fName)

        len = fullpathqq(fname, pathbuf)
        len2 = splitpathqq(pathbuf,fdrive,fdir,fname8,fext)
        fnlen = Len_Trim(fname8)
        fname = fname8(:fnlen)//fext
        WPigOpenFile = .true.

        call PigEraseMessage

        return
        end
 
!******************************************************************************
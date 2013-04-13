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

      SUBROUTINE DefPoly ( xpos, ypos, success )

! PURPOSE: To let the user map out a polygon. This polygon will be the
!          active area for node manipulation. The only restriction is
!          that the polygon can have no more than 'maxvert' sides.
!   INPUT: Interactive input from user,
!   GIVEN: Previously existing polygons defined in Common POLYDEFS.
! RETURNS: success = TRUE if user defines and confirms a polygon,
!                    else FALSE
!          In Common POLYDEFS;
!               actvpoly = polygon defined, if success, else no change.
! EFFECTS: Polygon is drawn as created, and saved in POLYDEFS. User can choose 
!          cursor, keyboard entry, or mixed (prompt at each vertex for cursor 
!          or keyboard) as method of defining a polygon. If entering the 4th 
!          or greater vertex by keyboard entry, then prompt for coordinate
!          includes '<C>lose' to automatically join up with the 1st vertex and 
!          close the polygon.
! BUG: Mar94 . It is possible to create more polygons than MAXPOLYS,
!          or at least for some of the arrays declared in size as MAXPOLYS
!          to be accessed outide their bounds.
!-----------------------------------------------------------------------*

! - PASSED VARIABLES
       LOGICAL success

! - "INCLUDES"
       include '../includes/defaults.inc'
       include '../includes/edpolys.inc'

! - COMMON BLOCKS
!   - PolyDisplay indicates polygons to display on redraw.
!       PolyDisplay = 0 = display active polygon only.
!                   = 1 = display all polygons.
!                   = 2 = display NO polygons.
       INTEGER PolyDisplay
       COMMON /POLYSTATUS/ PolyDisplay

       REAL cwxl, cwxh, cwyl, cwyh
       COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh

!       - display array needs 1 extra point to connect 1st & last points
       REAL vx(maxvert+1), vy(maxvert+1)
       COMMON /VLOCAL/ vx, vy

! - LOCAL VARIABLES
       integer, parameter :: ii1=1,i2=2
       REAL xpos, ypos
       INTEGER i, j,jj, ierr, tmpactv
       CHARACTER cstr*80, ans*1
       CHARACTER*3 char_nump, char_scrnp
       LOGICAL polydone, In_Box
       REAL v1xmin, v1xmax, v1ymin, v1ymax, prange
!          to define range box around 1st polyon vertex to close polygon
       INTEGER InpType
!       - InpType is method chosen to define polygon: 
!               1 = cursor, 2 = keyboard, 3 = mixed
!       - InpChoice, if InpType = 3, then choice of method for specifying
!                    current vertex: 1 = cursor, 2 = keyboard

!------------------START ROUTINE----------------------------------------

       if(numpolys .ge. MAXPOLYS) then
         cstr='Too many polygons already defined. Please delete some.'
         call PigMessageOK( cstr, 'defpoly' )
         return
       endif

!       - store currently active polygon, if any
       tmpactv = 0
       IF ( PolyDisplay .lt. 2 ) THEN
!         - other polygons may be displayed
         IF ( actvpoly .gt. 0 ) THEN
!           - there is an active polygon now, draw it in red
           tmpactv = actvpoly
           actvpoly = 0
           call DisplayPoly ( tmpactv )
         ENDIF
!           - ( actvpoly > 0 )
       ENDIF
!         - ( PolyDisplay < 2 )

!       - get method of defining polygon
       InpType = 1
       ans(1:1) = 'C'

!       - start new polygon
 !      call PigSetLineColour ( yellow )
       i = 1
       success = .FALSE.
       polydone = .FALSE.
       call PigDrawModifySymbol (xpos, ypos)
       DO WHILE ( .NOT. polydone )
!         - get a polygon vertex location
         IF ( InpType .eq. 1) THEN
!           - cursor input
           ierr = 0
           cstr = 'Pick Vertices, pick first again to close polygon.'
           IF ( i .eq. 1 ) THEN
!             - first vertex, no rubberband
             vx(1) = xpos
             vy(1) = ypos
             ierr = 1
             i = 2
             cycle
           ELSE
!             - not first vertex, use rubber band
             call RubberNode( xpos, ypos, vx(i-1), vy(i-1),cstr, ierr)
           ENDIF
!            - ( i = 1 )
         ENDIF
!           - ( InpType = 1 )
         IF ( ierr .eq. 1 ) THEN
!           - vertex chosen successfully, check if valid
           IF ( In_Box(xpos,ypos) ) THEN
             vx(i) = xpos
             vy(i) = ypos
             prange = 0.1*sqrt((xpos-vx(i-1))**2 + (ypos-vy(i-1))**2)
!             - check if polygon is to be closed now
             v1xmin = vx(1) - prange
             v1xmax = vx(1) + prange
             v1ymin = vy(1) - prange
             v1ymax = vy(1) + prange
             i1=max0(1,i-1)
             IF (((i.gt.1).AND.(vx(i).ge.v1xmin).AND.(vx(i).le.v1xmax)&
                          .AND.(vy(i).ge.v1ymin).AND.(vy(i).le.v1ymax))&
               .OR.((i .gt. 2).AND.(vx(i) .eq. vx(i1)).AND.(vy(i).eq.vy(i1)))&
               .OR.  (i .eq. maxvert))  THEN
!               - close polygon
              IF ( i .eq. maxvert ) THEN
!                 - put marker and line for maxvert
                call PigDrawLine ( i, vx, vy, ii1 )
                i = i + 1
              ENDIF
!                 - ( i = maxvert )
!               - connect last to first
              vx(i) = vx(1)
              vy(i) = vy(1)
              call PigDrawLine ( i, vx, vy, ii1 )
              IF ( i .lt. 4 ) THEN
!                 - reject polygon for too few vertices
                cstr = 'Invalid polygon, 3 vertices minimum.'
                call PigMessageOK ( cstr , 'polygon')
!                call PigUWait ( seconds )
!                 - remove from display
                call PigDrawLine ( i, vx, vy, i2 )
                i = 1
                polydone = .FALSE.
              ELSE
!                 - enough vertices in polygon
                cstr ='Polygon OK ?:'
                call PigMessageYesNo(cstr, ans)
                IF ( ans(1:1) .eq. 'Y' ) THEN
!                   - polygon OK
                  polydone = .TRUE.
                  success = .TRUE.
                ELSE
!                   - polygon rejected, remove from display
                  call PigDrawLine ( i, vx, vy, i2 )
                  polydone = .TRUE.
                  success = .FALSE.
                  i = 1
                ENDIF
!                   - ( ans = Y ) polygon OK
              ENDIF
!                 - ( i < 4 )
             ELSE
!               - not closing polygon, put a marker at location
!              call RedrawMark( vx(i), vy(i), yellow )
              IF ( i .gt. 1 ) THEN
!                 - more than 1 vertex, draw a line connecting to prev. vertex
                call PigDrawLine ( i, vx, vy, ii1 )
              ENDIF
!                 - ( i > 1 )
              i = i + 1
             ENDIF
!               - ( closing polygon )
           ELSE
!             - invalid point, not in current window
             cstr = 'Invalid point, pick again.'
             call PigPutMessage ( cstr )
           ENDIF
!             - ( In_Box(xpos, ypos) )
         ELSE
!           - error in choosing a vertex
           cstr = 'Invalid point, pick again.'
           call PigPutMessage ( cstr )
         ENDIF
!           - ( ierr =  1 ) NewNode
       END DO
!         - ( NOT donepoly )

       IF ( success ) THEN
!         - a new polygon has been created
         numpolys = numpolys + 1
!         call PigDrawLine ( i, vx, vy, ii1 )
         vnum = i !- 1
         DO j = 1, vnum
           vertx(numpolys,j) = vx(j)
           verty(numpolys,j) = vy(j)
         END DO
         vertcnt(numpolys) = vnum
         PolyCode(numpolys) = numpolys
         actvpoly = numpolys
!         scrnpoly = curpoly
         curpoly = curpoly + 1
         do j=1,numpolys
           jj = j
           call DisplayPoly ( jj )
         enddo
         char_nump = '   '
         char_scrnp = '   '
         WRITE ( char_nump, '(I3)' ) numpolys
         WRITE ( char_scrnp, '(I3)' ) actvpoly
!          - put up message
         cstr = 'Polygon '//char_scrnp//' of '// char_nump // '.'
         call PigStatusMessage ( cstr )
       ELSE
!         - not success, restore original active poly if there was one
         IF ( tmpactv .ne. 0 ) THEN
!           - tmpactv was used to store previously active poly
           actvpoly = tmpactv
           call DisplayPoly ( actvpoly )
         ELSE
!           - there was no previously active poly
           actvpoly = 0
         ENDIF
!           - ( tmpactv ne 0 )
       ENDIF
!         - ( success )

       END

!-----------------------------------------------------------------------*
 
      SUBROUTINE RubberNode ( xn, yn, xinit, yinit, mess, ierr )

! PURPOSE: To ask user to pick a location for a point.
!   INPUT: xn,yn = (x,y) co-ordinates by user,
!   GIVEN: mess = message to prompt with,
!          xinit, yinit = coordinates of initial cursor location,
! RETURNS: (xn,yn) as input by user
!            ierr  = 1  if there is input
!                  = 0 if no input, or invalid input.
! EFFECTS: New point is selected, and a "rubber band" line is drawn
!          connecting point (xinit,yinit) to cursor while point is chosen.
! WRITTEN: May 1990 by JDM, for NODER.
!----------------------------------------------------------------------*


! - "INCLUDES"
       include '../includes/graf.def'

! - PASSED VARIABLES
      REAL xn, yn, xinit, yinit
      integer ierr
      CHARACTER*(*) mess

! - COMMON BLOCKS
      REAL cwxl, cwxh, cwyl, cwyh
      COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh

       xn = ( cwxl+ cwxh)/2.0
       yn = ( cwyl+ cwyh)/2.0
!        ierr = 0

!      DO WHILE ( ierr .eq. 0 )
       call PigPutMessage( mess )
       call PigRubberLine(xn, yn, xinit, yinit)
!        IF (tnr .eq. MAINWIN) ierr = 1
!      END DO
       ierr = 1
      END

!-----------------------------------------------------------------------*

      subroutine PigRubberLine(Xnew, Ynew, Xinit, Yinit)

!+ Purpose:  Displays a rubberline around Xinit, Yinit.  Used to
!+           define any polygon area.
!+ Givens :  real       Xinit   : initial x location in WC
!+           real       Yinit   : initial y location in WC
!+ Returns:  real       XNew    : final x location in WC
!+           real       YNew    : final y location in WC
!+ Effects:  Draws a rubberline from Xinit, Yinit to Xnew,Ynew until
!+           the mouse is clicked.

      REAL  Xnew, Ynew, Xinit, Yinit

      Xnew = Xinit
      Ynew = Yinit

      call WPigRubberLine(Xnew, Ynew, Xinit, Yinit)  !used in Visual Fortran version

      end

!*--------------------------------------------------------------------------*
!******************************************************************************
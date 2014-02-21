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
!                            Windowing                                      *
!*--------------------------------------------------------------------------*

      SUBROUTINE BlankMap
!   
! PURPOSE : To blank map screen.
! Given   : None. 
! Returns : None.

!     - INCLUDES
      INCLUDE '../includes/graf.def'
!     - PARAMETERS
      integer LIMWIN
      PARAMETER( LIMWIN = 50 )
!     - COMMON BLOCKS
      REAL   CWXL, CWXH, CWYL, CWYH 
      COMMON /CURWIN/ CWXL,CWXH,CWYL,CWYH   
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON  /MAXRG/ XMAX,YMAX,XMIN,YMIN

!     - LOCAL VARIABLES
      REAL x1,x2,y1,y2
!--------------BEGIN----------------------

      if (level.eq.0) then
        CWXL = XMIN
        CWXH = XMAX
        CWYL = YMIN
        CWYH = YMAX
        x1 = XMIN
        x2 = XMAX
        y1 = YMIN
        y2 = YMAX
      else
        CWXL = WXL(LEVEL)
        CWXH = WXH(LEVEL)
        CWYL = WYL(LEVEL)
        CWYH = WYH(LEVEL)
        x1 = WXL(LEVEL)
        x2 = WXH(LEVEL)
        y1 = WYL(LEVEL)
        y2 = WYH(LEVEL)
      endif

      call PigSetWorldCoordinates( x1, x2, y1, y2 )
      call PigEraseMainWin

      END

! --------------------------------------------------------------------------*

      subroutine PigEraseMainWin

      call WPigEraseMain
    
      end

!*--------------------------------------------------------------------------*

      LOGICAL FUNCTION IN_BOX (X, Y)

! Purpose : To see if the X and Y locations given fall within
!           the current window limits
! Given   : X, Y, the coordinates to be tested
! Returns : IN_BOX - true if X and Y fall in the window limits or
!                    false if outside window limits

! *** passed variables ***
      REAL X, Y

! *** common variables ***
      REAL CWXL, CWXH, CWYL, CWYH 

! *** Start Logical Function ***

      call PigGetWorldCoordinates(CWXL, CWXH, CWYL, CWYH)
      if ((X .lt. CWXL).or.(X .gt. CWXH).or.(Y .lt. CWYL).or.(Y .gt. CWYH)) THEN
        IN_BOX = .FALSE.
      else
        IN_BOX = .TRUE.
      endif

      end

! --------------------------------------------------------------------------*

      SUBROUTINE Fullsize ( xl, yl, xh, yh )

! PURPOSE : Resets window to full sizeW
! INPUT   : XL - LEFT WINDOW BOUNDARY
!           XH - RIGHT WINDOW BOUNDARY
!           YL - BOTTOM WINDOW BOUNDARY
!           YH - TOP WINDOW BOUNDARY
! RETURN  :         COMMON AREA /WLIMIT/
!                   WXL - AS XL
!                   WXH - AS XH
!                   WYL - AS YL
!                   WYH - AS YH

      implicit none      

      real xl,xh,yl,yh

      integer LIMWIN
      PARAMETER( LIMWIN = 50 )

! *** COMMON AREA ***
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL,WXH,WYL,WYH,LEVEL

      REAL   CWXL, CWXH, CWYL, CWYH 
      COMMON /CURWIN/ CWXL,CWXH,CWYL,CWYH   

      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON /MAXRG/ XMAX,YMAX,XMIN,YMIN

      LEVEL = 0
      XMIN = xl
      XMAX = xh
      YMIN = yl
      YMAX = yh
      WXL(1) = XMIN
      WXH(1) = XMAX
      WYL(1) = YMIN
      WYH(1) = YMAX
      cwxl = wxl(1)
      cwxh = wxh(1)
      cwyl = wyl(1)
      cwyh = wyh(1)

      END


!***********************************************************************

      SUBROUTINE DisplayIn3(  xL, yL, xH, yH, Redrw )

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

      REAL    xL, yL, xH, yH

!     BEGIN
      call PigSetLineColour(FOREGR)
      if ( LEVEL .ge. LIMWIN ) then
        call PigPutMessage( 'You are already at the lowest level..')
        Redrw = .FALSE.
      else
      
        Redrw = .TRUE.
        LEVEL = LEVEL + 1
        WXL(LEVEL) = min(xL,xH)
        WXH(LEVEL) = max(xL,xH)
        WYL(LEVEL) = min(yL,yH)
        WYH(LEVEL) = max(yL,yH)
        cwxl = wxl(level)
        cwxh = wxh(level)
        cwyl = wyl(level)
        cwyh = wyh(level)

!        if ( Redrw ) call WINBOX
      endif

      END

!*--------------------------------------------------------------------------*

      SUBROUTINE DisplayOut( Redrw )

! Purpose: 'Zooms' by 1.25.
! Givens : None
! Returns: Redrw - TRUE if window has been redefined and grid needs to be
!                  redrawn
!                - FALSE otherwise
! Effects: Window is zoomed out.

      INCLUDE '../includes/graf.def'

      LOGICAL Redrw

      integer LIMWIN
      PARAMETER( LIMWIN = 50 )

! *** COMMON BLOCK VARIABLES ***
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL

      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON /MAXRG/ XMAX,YMAX,XMIN,YMIN

      REAL CWXL, CWXH, CWYL, CWYH 
      COMMON /CURWIN/CWXL, CWXH, CWYL, CWYH

      REAL MEANX, MEANY, DELX, DELY

      Redrw = .TRUE.
      LEVEL = LEVEL + 1
      if(level-1.le.0) then
        meanx = (xmin+xmax)/2
        meany = (ymin+ymax)/2
        delx = (xmax-xmin)/2
        dely = (ymax-ymin)/2
        delx = delx * 1.25
        dely = dely * 1.25
        WXL(LEVEL) =  meanx - delx
        WXH(LEVEL) =  meanx + delx
        WYL(LEVEL) =  meany - dely
        WYH(LEVEL) =  meany + dely
      elseif ( LEVEL .ge. LIMWIN ) then
        do j=2,LIMWIN
          WXL(j-1) = WXL(j)
          WXH(j-1) = WXH(j)
          WYL(j-1) = WYL(j)
          WYH(j-1) = WYH(j)
        enddo          

        LEVEL = LIMWIN

        meanx = (WXL(LEVEL-1)+WXH(LEVEL-1))/2
        meany = (WYH(LEVEL-1)+WYL(LEVEL-1))/2

        delx = (WXH(LEVEL-1)-WXL(LEVEL-1))/2
        dely = (WYH(LEVEL-1)-WYL(LEVEL-1))/2

        delx = delx * 2
        dely = dely * 2

        WXL(LEVEL) =  meanx - delx
        WXH(LEVEL) =  meanx + delx
        WYL(LEVEL) =  meany - dely
        WYH(LEVEL) =  meany + dely  

      else

        meanx = (WXL(LEVEL-1)+WXH(LEVEL-1))/2
        meany = (WYH(LEVEL-1)+WYL(LEVEL-1))/2

        delx = (WXH(LEVEL-1)-WXL(LEVEL-1))/2
        dely = (WYH(LEVEL-1)-WYL(LEVEL-1))/2

        delx = delx * 2
        dely = dely * 2

        WXL(LEVEL) =  meanx - delx
        WXH(LEVEL) =  meanx + delx
        WYL(LEVEL) =  meany - dely
        WYH(LEVEL) =  meany + dely 
 
      endif
      cwxl = wxl(level)
      cwxh = wxh(level)
      cwyl = wyl(level)
      cwyh = wyh(level)

      END

!***********************************************************************

      SUBROUTINE DisplayPan3( mousex, mousey, Redrw )

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

      REAL xoffset, yoffset,mousex, mousey
      integer j

!     BEGIN

      call PigSetLineColour(FOREGR)

!      Redrw = .FALSE.

!      call PigPutMessage('Drag point to new location')
      
      xoffset = -mousex
      yoffset = -mousey
      
!      call PigPan2(xoffset, yoffset)

      Redrw = .TRUE.
      LEVEL = LEVEL + 1
      if(level-1.le.0) then
        WXL(LEVEL) = CWXL + xoffset
        WXH(LEVEL) = CWXH + xoffset
        WYL(LEVEL) = CWYL + yoffset
        WYH(LEVEL) = CWYH + yoffset
      elseif(LEVEL.ge.LIMWIN) then
        do j=2,LIMWIN
          WXL(j-1) = WXL(j)
          WXH(j-1) = WXH(j)
          WYL(j-1) = WYL(j)
          WYH(j-1) = WYH(j)
        enddo          
        LEVEL = LIMWIN
        WXL(LEVEL) = WXL(LEVEL-1) + xoffset
        WXH(LEVEL) = WXH(LEVEL-1) + xoffset
        WYL(LEVEL) = WYL(LEVEL-1) + yoffset
        WYH(LEVEL) = WYH(LEVEL-1) + yoffset
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

!*--------------------------------------------------------------------------*

      SUBROUTINE DisplayLast( Redrw )

! Purpose: 'Zooms' out N window levels.
! Givens : None
! Returns: Redrw - TRUE if window has been redefined and grid needs to be
!                  redrawn
!                - FALSE otherwise
! Effects: Window is redefined to a previous window level.

      INCLUDE '../includes/graf.def'

      LOGICAL Redrw

      integer LIMWIN
      PARAMETER( LIMWIN = 50 )

! *** COMMON BLOCK VARIABLES ***
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL

      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON /MAXRG/ XMAX,YMAX,XMIN,YMIN

      REAL MEANX, MEANY, DELX, DELY

      if ( LEVEL .eq. 0 ) then
        meanx = (xmin+xmax)/2
        meany = (ymin+ymax)/2
        delx = (xmax-xmin)/2
        dely = (ymax-ymin)/2
        delx = delx * 1.25
        dely = dely * 1.25
        xmin = meanx - delx
        xmax = meanx + delx
        ymin = meany - dely
        ymax = meany + dely
        Redrw = .TRUE.
      elseif ( LEVEL .gt. 0 ) then
        LEVEL = LEVEL - 1
        Redrw = .TRUE.
      endif

      END

!*--------------------------------------------------------------------------*



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

!---------------------------------------------------------------------------*
!                           CONFIG.FOR                                      *
!     This module contains the controlling functions for the configuration  *
!     option.  The setting of colours for boundaries, contours, and grid    *
!     lines and the display and toggling of configuration options in the    *
!     right hand display panel.                                             *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE ConfigLines_init
!      (hitnumdummy)

! Purpose: Toggles the grid default attributes.
! Givens : None
! Returns: None
! Effects: Defaults are set.

      implicit none

      call PigMessageOK('Unavailable','configgrid')

      END

!---------------------------------------------------------------------------*

      SUBROUTINE ConfigBoundaries_Init()

! Purpose: Toggles the attributes for the display of boundary information.
! Givens : None
! Returns: None
! Effects: None

      implicit none

      call PigMessageOK('Unavailable','configbnd')


      END

!---------------------------------------------------------------------------*

      SUBROUTINE ConfigContours_Init()

! Purpose: Toggles the attributes for the display of contour information.
! Givens : None
! Returns: None
! Effects: None

      implicit none


      END

!---------------------------------------------------------------------------*

      SUBROUTINE ConfigXhr(range)

! Purpose: Allow user to change the cursor sensitivity RANGE
! Givens : None
! Returns: None
! Effects: Range is updated only if a value was entered > 0.0 .

      implicit none


      INCLUDE '../includes/graf.def'

      REAL RANGE

      CHARACTER*80 ans
      CHARACTER*13 Oldr
      REAL temp
      LOGICAL Success

10    continue
      oldr = '             '
      write(oldr,'(F12.5)') RANGE

      call PigPrompt('RANGE (' //oldr// ') Enter new Range:',ans )

      call PigReadReal( ans, temp, Success )
      if ( .not. Success ) goto 10
      if ( temp .gt. 0.0 ) then
        RANGE = temp
      endif

      END


!---------------------------------------------------------------------------*

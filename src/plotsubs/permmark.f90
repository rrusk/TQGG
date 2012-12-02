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

!*----------------------------------------------------------------------
!                         PERMMARK.FOR                                  *
!     The routines of this module are those concerned with markers      *
!     that are placed during an editing session that must survive       *
!     redraws and windowing until specifically turned off or erased.    *
! Requires: PMARKER.INC storage for marker information.                 *
!
! Routines: PutPermMarker - routine to draw marker on display device.   *
!           RemoveLastMarker - erase the last marker placed.            *
!           ErasePermMarkers - erase all markers.                       *
!           DisplayPermMarkers - refresh display of all markers.        *
!*----------------------------------------------------------------------*

!*--------------------------------------------------------------------------*

      module PMarksData
      
      integer, parameter :: MaxPermMarkers = 100
      integer :: LastMarker
      real :: Xperm(MaxPermMarkers),Yperm(MaxPermMarkers)

      end module

!*--------------------------------------------------------------------------*

      SUBROUTINE PutPermMarker( Xloc, Yloc, Success )

! Purpose: Display a marker at the given location
!          on the active display device, and add marker location to
!          the list of permanent markers.
! Givens : Xloc - array of x coordinates (WC).
!          Yloc - array of y coordinates (WC).
! Assumes: Markertype, MarkerColour previously set.
! Returns: Success - TRUE if marker was added to the list.
! Effects: Marker is added to the permanent marker list.

      use PMarksData

      implicit none

!     - PASSED VARIABLES
      REAL  xloc, yloc
      LOGICAL Success

!      INCLUDE 'pmarker.inc'

!----------- BEGIN ---------

      IF ( LastMarker .eq. MaxPermMarkers ) THEN
        Success = .FALSE.
      ELSE
        LastMarker = LastMarker + 1
        Xperm(LastMarker) = Xloc
        Yperm(LastMarker) = Yloc
        Success = .TRUE.
        call DrawPMark( xloc, yloc )
      ENDIF

      RETURN
      END

!*--------------------------------------------------------------------------*

      subroutine ErasePermMarkers

! Purpose: Erases all permanent markers (Routine may be used for init also).
! Givens : PMARKER.INC storage of marker locations.
! Returns: None
! Effects: Permanent marker table is cleared, and markers are erased
!          from screen.

      use PMarksData

      implicit none

!     - LOCAL VARIABLES
      integer i
      REAL xloc, yloc

!------- BEGIN --------

      DO i=1,LastMarker  !WHILE ( LastMarker .ne. 0 )
        xloc = Xperm(LastMarker)
        yloc = Yperm(LastMarker)
        Xperm(LastMarker) = 0.0
        Yperm(LastMarker) = 0.0
        LastMarker = LastMarker - 1
        call ErasePMark( xloc, yloc)
      ENDDO

      RETURN
      END

!*--------------------------------------------------------------------------*

      subroutine RemoveLastMarker( Success )

! Purpose: Erases the last marker which was added to the Marker table.
! Givens : None
! Returns: Success - TRUE if there was a marker to delete.
! Effects: PermMarker table is updated, and marker is removed from the screen.

      use PMarksData

      implicit none

!     - PASSED PARAMETERS
      LOGICAL Success

!     - LOCAL PARAMETERS
      REAL xloc, yloc

!------------BEGIN--------

      Success = .FALSE.
      IF ( LastMarker .gt. 0 ) THEN
        Success = .TRUE.
        xloc = Xperm( LastMarker )
        yloc = Yperm( LastMarker )
        LastMarker = LastMarker - 1
        call ErasePMark( xloc, yloc  )
      ENDIF

      RETURN
      END

!*--------------------------------------------------------------------------*

      subroutine DisplayPermMarkers

! Purpose: Displays all recorded permanent markers within the current window.
! Givens : None passed, PMARKER.INC included.
! Returns: None
! Effects: Markers are displayed on the screen in the default permanent marker
!          colour and type.

      use PMarksData

      implicit none

!     - LOCAL pARAMETERS
      REAL xloc, yloc
      integer i

!---------- BEGIN --------

!      i = LastMarker
      DO i=1,LastMarker  !WHILE ( i .gt. 0 )
        xloc = Xperm( i )
        yloc = Yperm( i )
        call DrawPMark( xloc, yloc )
!        i = i - 1
      ENDDO

      END

!*-----------------------------------------------------------------------*
!*---------------------- END PERMMARK.FOR ----------------------------------*

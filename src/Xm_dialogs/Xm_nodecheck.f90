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
!                          CRITERIA.FOR                                     *
!     This module contains the subroutines associated with the              *
!     criteria tests for vertices.                                          *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE FlagsVertices(CHANGE)

! Purpose: Dispatch routine for the placement and deletion of vertice markers.
! Givens : CHANGE - TRUE IF triangle list needs updating.
! Returns: None
! Effects: None

! Passed variables.
      LOGICAL CHANGE
!       - TRUE IF triangle list needs to be updated.

! Local variables
      LOGICAL Ok

      Ok = .FALSE.
      call CRITERIA(Ok)
      IF (Ok) THEN
        call DrwFig(.FALSE., CHANGE)
      ENDIF
      
      END

!---------------------------------------------------------------------------*

      SUBROUTINE Criteria( OkDummy )

! Purpose : To place markers at vertices if certain
!           criteria are satisfied
! Given   : None
! Returns : Ok - TRUE if a redraw is needed

      INCLUDE '../includes/defaults.inc'
      INCLUDE 'critcom.inc'
      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'

! critcom.inc:  array TheCriteria(1->MaxCrit) contents
! [1] CO [2] C1 [3] C2 [4] C3 [5] C4 [6] C5 [7] C6 [8] NC0
! [9] DLT [10] DGT [11] DBTW [12] NBGT [13] NBLT [14] NBE [15] EXT

! - PASSED VARIABLES
      LOGICAL OkDummy

! - PARAMETERS
      CHARACTER*11 lblank
      PARAMETER ( lblank = '           ' )

! - LOCAL VARIABLES
!      integer i, j  !, row
!      integer hitnum !, anslen
!      LOGICAL  Success, IEXIS
!      logical, save :: Accepted, Erase
      CHARACTER*80 ans
!      CHARACTER*6 inum
!      CHARACTER*11 rnum
!      logical PigGetOpenFileName

!----------BEGIN----------------------

      OkDummy = .true.
      DrawVCrit = .true.

      call PigMessageYesNo ('Code (yes) or neighbor (no) checks? ',ans)
      if(ans(1:1).eq.'Y') then
        TheCriteria = .false.
        TheCriteria(2) = .true.
        TheCriteria(3) = .true.
        TheCriteria(6) = .true.
        TheCriteria(7) = .true.
        TheCriteria(8) = .true.
      else
        TheCriteria = .false.
        TheCriteria(12) = .true.
        USER_NCOUNT = 8
        TheCriteria(13) = .true.
        USER_NCOUNT1 = 4
        TheCriteria(14) = .true.
        USER_NCOUNT2 = 4
      endif

      return

!      entry criteria_ehandler(hitnum)

      END

!---------------------------------------------------------------------------*
      SUBROUTINE TurnOffOtherCriteria

! Purpose: Turn off all other criteria when external criteria is enabled.
! Givens : None
! Returns: None
! Effects: Criteria other than EXT are turned OFF and the screen is updated
!          accordingly.

      RETURN
      END

!---------------------------------------------------------------------------*
      SUBROUTINE Depth_Greater( nrec )

! Purpose : To mark points that have DEPTH >= user supplied
!           number
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i
!        - number that user enters for comparison test
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!--------------BEGIN--------------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
          if ( DEPTH(i) .gt. USER_COUNT1 ) then
!              *put a marker at that depth
             if ( IN_BOX(DXRAY(i),DYRAY(i)) ) THEN
              call PUTMARKER( DXRAY(i), DYRAY(i), 4, cdgt )
             endif
          endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      SUBROUTINE Depth_Less( nrec )

! Purpose : To mark points that have DEPTH <= user supplied
!           number
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i
!        - number that user enters for comparison test
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN----------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
          if ( DEPTH(I) .lt. USER_COUNT ) then
!              *put a marker at that depth
             if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
              call PUTMARKER( DXRAY(i), DYRAY(i), 4, cdlt )
             endif
          endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      SUBROUTINE Depth_Between( nrec )

! Purpose : To mark points with LOWERD<= DEPTH <= UPPERD
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i
!        - numbers that user enters for comparison test
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN--------------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
          if ( (DEPTH(i) .ge. LOWERD) .AND.(DEPTH(i) .le. UPPERD) ) then
!              *put a marker at the vertex
             if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
              call PUTMARKER( DXRAY(i), DYRAY(i), 4, cdbtw )
             endif
          endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      SUBROUTINE Mark_Nbs_Over( nrec )

! Purpose : To mark points that have more than 'n' number
!           of neighbors
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE 'critcom.inc'
      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i, j
!       - counters
      integer nb_count
!       - number of neighbors for any record
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN-----------------

      do 1000 i = 1, NREC
       if ( code(i).ge.0 ) then
       nb_count = 0
       do 900 j = 1, NBTOTR
          if ( NL(j,i) .ne. 0 ) then
             nb_count = nb_count + 1
          endif
900      continue
       if ( nb_count .gt. USER_NCOUNT ) then
          if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
             call PUTMARKER( DXRAY(i), DYRAY(i), 4, cnbgt )
          endif
       endif
       endif
1000  continue

      RETURN
      END

!---------------------------------------------------------------------------*
      SUBROUTINE Mark_Nbs_Equal( nrec )

! Purpose : To mark points that have 'n' neighbors
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE 'critcom.inc'
      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i, j
!       - counters
      integer nb_count
!       - number of neighbors for any record
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
       nb_count = 0
       do 200 j = 1, NBTOTR
          if ( NL(j,i) .ne. 0 ) then
             nb_count = nb_count + 1
          endif
200      continue
       if ( nb_count .eq. USER_NCOUNT2 ) then
          if ( IN_BOX(DXRAY(i),DYRAY(i)) ) then
             call PUTMARKER( DXRAY(i), DYRAY(i), 4, cnbe )
          endif
       endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
      SUBROUTINE Mark_Nbs_Under( nrec )

! Purpose : To mark points that have less than 'n' number
!           of neighbors
! Given   : NREC - number of points in the grid.
! Returns : None

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
      INCLUDE 'critcom.inc'

! - PASSED VARIABLES
      integer NREC

! - LOCAL VARIABLES
      integer i, j
!       - counters
      integer nb_count
!       - number of neighbors for any record
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits

!------------------BEGIN------------------------

      do 100 i = 1, NREC
       if ( code(i).ge.0 ) then
       nb_count = 0
       do 200 j = 1, NBTOTR
          if ( NL(j,i) .ne. 0 ) then
             nb_count = nb_count + 1
          endif
200      continue
       if ( nb_count .lt. USER_NCOUNT1 ) then
          if ( IN_BOX(DXRAY(i),DYRAY(i)) ) THEN
             call PUTMARKER( DXRAY(i), DYRAY(i), 4, cnblt)
          endif
       endif
       endif
100   continue

      RETURN
      END

!---------------------------------------------------------------------------*
!                       END CRITERIA.FOR                                    *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!              VERTMARK.FOR                     *
!     This module contains the subroutines that control the vertex      *
!     markers.                                  *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
      SUBROUTINE InitVertexMarkers

! Purpose: Initialize all vertex markers to OFF
! Givens : None
! Returns: None
! Effects: Common Block VerControl in 'critcom.inc' is initialized.

      INCLUDE 'critcom.inc'

! Local variables
      integer i

! Turn off all criteria
      do 10 i = 1, MaxCrit
     TheCriteria(i) = .FALSE.
10    continue

! Turn off criteria drawing
      call VMarkOff

      RETURN
      END


!---------------------------------------------------------------------------*
      SUBROUTINE VMarkOn

! Purpose: Turn on Vertex markers.
! Givens : None
! Returns: None
! Effects: Vertex marker drawing is enabled.

      INCLUDE 'critcom.inc'

      DrawVCrit = .TRUE.

      RETURN
      END


!---------------------------------------------------------------------------*
      SUBROUTINE VMarkOff

! Purpose: Turn on Vertex markers.
! Givens : None
! Returns: None
! Effects: Vertex marker drawing is disabled.

      INCLUDE 'critcom.inc'

      DrawVCrit = .FALSE.

      RETURN
      END


!---------------------------------------------------------------------------*
      SUBROUTINE DrwVertMarkers()

! Purpose : Draw vertex markers using specified criterion.
! Givens  : nrec - number of points in the current grid
!       All required information in common blocks.
! Returns : None
! Effects : Markers are drawn according to the specified criterion

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/defaults.inc'
      INCLUDE 'critcom.inc'
      INCLUDE 'critcol.def'

!     - PASSED PARAMETERS
      integer nrec

!     - LOCAL VARIABLES
      integer i
      LOGICAL In_Box

!-----------------BEGIN------------------

!     - Perform vertex marking only if vertex marking has been turned on.
      if ( .NOT. DrawVCrit ) goto 999

      nrec = itot

      if (TheCriteria(15)) then
!   - EXT, put markers at vertices specified in external file
!       -- each record of which contains vertex number followed by
!       -- number of colour required. Record format is (1X,I5,1X,I2)
!    call EXTERNAL
      else
    if ( TheCriteria(1) ) then
!     - C0, code = 0
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 0 ) then
!       - put a marker wherever the code is 0
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc0 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 0 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(1) )

    if ( TheCriteria(8) ) then
!     - !C0, code not equal to 0
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .gt. 0 ) then
!           - put a marker wherever the code is NOT 0
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cnotc0 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) gt 0 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec)
    endif
!     - ( TheCriteria(8) )

    if ( TheCriteria(2) ) then
!     - C1, code = 1
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 1 ) then
!       - put a marker wherever the code is 1
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PUTMARKER ( DXRAY(I), DYRAY(I), 4, cc1 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 1 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(2) )

    if ( TheCriteria(3) ) then
!         - C2, code = 2
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 2 ) then
!       - put a marker wherever the code is 2
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker ( DXRAY(i), DYRAY(i), 4, cc2 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 2 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( (TheCriteria(3) )

    if ( TheCriteria(4) ) then
!     - C3, code = 3
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 3 ) then
!       - put a marker wherever the code is 3
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc3 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 3 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(4) )

    if ( TheCriteria(5) ) then
!     - C4, code = 4
      do I = 1, NREC
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 4 ) then
!       - put a marker wherever the code is 4
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc4 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 4 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(5) )

    if ( TheCriteria(6) ) then
!     - C5, code = 5
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 5 ) then
!       - put a marker wherever the code is 5
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc5 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 5 )
        endif
!         - ( code(i).ge.0 )
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(6) )

    if ( TheCriteria(7) ) then
!     - C6, code = 7
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 7 ) then
!       - put a marker wherever the code is 6
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc6 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 7 )
        endif
!         - code(i).ge.0
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(7) )

!   - Mar/91 (JDM), new check added for TheCriteria(16)
    if ( TheCriteria(16) ) then
!     - C=?, code = USER_CODE
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. USER_CODE ) then
!       - put a marker wherever the code is USER_CODE
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, ccuser )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq USER_CODE )
        endif
!         - code(i).ge.0
      enddo
!       - ( i = 1 to nrec )
    endif
!     - ( TheCriteria(16) )

    if ( TheCriteria(10) ) then
!     - DGT, depth greater than USER_COUNT
      call Depth_Greater( nrec )
    endif

    if ( TheCriteria(9) ) then
!     - DLT, depthless than USER_COUNT
        call Depth_Less( nrec )
    endif

    if ( TheCriteria(11) ) then
!     - DBTW, depth between UPPERD and LOWERD
      call Depth_Between( nrec )
    endif

    if ( TheCriteria(12) ) then
!     - NBGT, neighbours greater than USER_NCOUNT
      call Mark_Nbs_Over( nrec )
    endif

    if ( TheCriteria(14) ) then
!     - NBE, neighbours equal to USER_NCOUNT
      call Mark_Nbs_Equal( nrec )
    endif

    if ( TheCriteria(13) ) then
!     - NBLT, neighbours less than USER_NCOUNT
      call Mark_Nbs_Under( nrec )
    endif

      endif
!   - ( TheCriteria(15) )
999   continue

      RETURN
      END

!---------------------------------------------------------------------------*

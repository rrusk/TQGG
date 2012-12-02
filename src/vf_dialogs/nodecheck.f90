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

!      INCLUDE '../includes/defaults.inc'
!      INCLUDE '../includes/critcom.inc'

! Passed variables.
      LOGICAL CHANGE
!       - TRUE IF triangle list needs to be updated.

! Local variables
      LOGICAL Ok

      Ok = .FALSE.
      call CRITERIA(Ok)
      IF (Ok) THEN
      call DrwFig(CHANGE)
      ENDIF
      END

!---------------------------------------------------------------------------*

      SUBROUTINE Criteria( OkDummy )

! Purpose : To place markers at vertices if certain
!           criteria are satisfied
! Given   : None
! Returns : Ok - TRUE if a redraw is needed
! Written : Steve Prestage and Daphne Connolly June 1989
! Modified: Mar/91 (JDM), all segments removed, option 'C=?' (TheCriteria(16))
!           added, where code is equal to user entered value USER_CODE.



      INCLUDE '../includes/defaults.inc'
      INCLUDE 'critcom.inc'
      INCLUDE '../includes/graf.def'
      INCLUDE 'critcol.def'
!      INCLUDE '../includes/criteria.inc'

! critcom.inc:  array TheCriteria(1->MaxCrit) contents
! [1] CO [2] C1 [3] C2 [4] C3 [5] C4 [6] C5 [7] C6 [8] NC0
! [9] DLT [10] DGT [11] DBTW [12] NBGT [13] NBLT [14] NBE [15] EXT

! - PASSED VARIABLES
      LOGICAL OkDummy

! - PARAMETERS
      CHARACTER*11 lblank
      PARAMETER ( lblank = '           ' )

! - LOCAL VARIABLES
      integer i, j, row
      integer hitnum, anslen
      LOGICAL  Success, IEXIS
      logical, save :: Accepted, Erase
      CHARACTER*80 ans
      CHARACTER*6 inum
      CHARACTER*11 rnum
      logical PigGetOpenFileName

!----------BEGIN----------------------

     okdummy = .false.

!       - Right window definition T9
	call InitRHPanel

!       - display labels
	call PigSetTextColour( LabelColor )

	call PanelText( 10, 1, 'VERTEX', 6 )
	call PanelText( 9, 2,  'CRITERION', 9 )

	call PanelText( 1, 4, 'Display Flag:', 13 )

	call PanelText( 2, 6,  'C0', 2)
	call PanelText( 2, 7,  'C1', 2 )
	call PanelText( 2, 8,  'C2', 2 )
	call PanelText( 2, 9,  'C3', 2 )
	call PanelText( 2, 10, 'C4', 2 )
	call PanelText( 2, 11, 'C5', 2 )
	call PanelText( 2, 12, 'C6', 2 )
	call PanelText( 2, 13, 'NC0', 3 )
	call PanelText( 2, 14, 'C=?', 3 )
	call PanelText( 11, 6, 'DLT', 3 )
	call PanelText( 11, 7, 'DGT', 3 )
	call PanelText( 11, 8, 'DBTW', 4 )
	call PanelText( 11, 10, 'NBGT', 4 )
	call PanelText( 11, 11, 'NBLT', 4 )
	call PanelText( 11, 12, 'NBE', 3 )
!	call PanelText( 11, 13, 'EXT', 3 )
!	call PanelText( 2, 16, 'External Criterion File:', 24 )

!       - set text color for modifiable values ( hit areas )
	call PigSetTextColour( HitColor )

!        - Display flag
	IF ( DrawVCrit ) THEN
	  call PanelHit( 15, 4, 1, 'ON ', 3 )
	ELSE
	  call PanelHit( 15, 4, 1, 'OFF', 3 )
	ENDIF

!       - display criterion status values
      row = 5
      DO i = 1, 8
	 row = row + 1
	 IF ( TheCriteria(i) ) THEN
!           - set text color for "ON" status
	    IF ( i .eq. 1 ) THEN
	       call PigSetTextColour( cc0 )
	    ELSE IF ( i .eq. 2 ) THEN
	       call PigSetTextColour( cc1 )
	    ELSE IF ( i .eq. 3 ) THEN
	       call PigSetTextColour( cc2 )
	    ELSE IF ( i .eq. 4 ) THEN
	       call PigSetTextColour( cc3 )
	    ELSE IF ( i .eq. 5 ) THEN
	       call PigSetTextColour( cc4 )
	    ELSE IF ( i .eq. 6 ) THEN
	       call PigSetTextColour( cc5 )
	    ELSE IF ( i .eq. 7 ) THEN
	       call PigSetTextColour( cc6 )
	    ELSE IF ( i .eq. 8 ) THEN
	       call PigSetTextColour( cnotc0 )
	    ENDIF
	    call PanelHit( 6, row, i + 1, 'ON ', 3 )
	 ELSE
	    call PigSetTextColour( HitColor )
	    call PanelHit( 6, row, i + 1, 'OFF', 3 )
	 ENDIF
	ENDDO
!        - option 'C=?' initialization (20)
      if ( TheCriteria(16) ) then
	call PigSetTextColour( ccuser )
	WRITE( inum, '(I3)' ) USER_CODE
	call PanelHit( 7, 14, 20, inum, 3 )
      else
	call PigSetTextColour( HitColor )
	call PanelHit( 6, 14, 20, 'OFF', 3 )
      endif

      row = 5
      DO i = 1, 7
	 row = row + 1
	 IF ( TheCriteria(i+8) ) THEN
!           - set text color for "ON" status
	    IF ( i .eq. 1 ) then
	       call PigSetTextColour( cdlt )
	       WRITE( rnum, '(F11.3)' ) USER_COUNT
	       call PanelHit( 16, row, 10, rnum, 11 )
	    ELSE IF ( i .eq. 2 ) THEN
	       call PigSetTextColour( cdgt )
	       WRITE( rnum, '(F11.3)' ) USER_COUNT1
	       call PanelHit( 16, row, 11, rnum, 11 )
	    ELSE IF ( i .eq. 3 ) THEN
	       call PigSetTextColour( cdbtw )
	       WRITE( rnum, '(F11.3)' ) LOWERD
	       call PanelHit( 16, row, 12, rnum, 11 )
	       row = row + 1
	       WRITE( rnum, '(F11.3)' ) UPPERD
	       call PanelHit( 16, row, 21, rnum, 11 )
	    ELSE IF ( i .eq. 4 ) THEN
	       call PigSetTextColour( cnbgt )
	       WRITE( inum, '(I6)' ) USER_NCOUNT
	       call PanelHit( 18, row, 13, inum, 6 )
	    ELSE IF ( i .eq. 5 ) THEN
	       call PigSetTextColour( cnblt )
	       WRITE( inum, '(I6)' ) USER_NCOUNT1
	       call PanelHit( 18, row, 14, inum, 6 )
	    ELSE IF ( i .eq. 6 ) THEN
	       call PigSetTextColour( cnbe )
	       WRITE( inum, '(I6)' ) USER_NCOUNT2
	       call PanelHit( 18, row, 15, inum, 6 )
	    ELSE IF ( i .eq. 7 ) THEN
	       call PigSetTextColour( HitColor )
!	       call PanelHit( 18, row, 16, '    ON', 6 )
	    ENDIF
	 ELSE
	   call PigSetTextColour( HitColor )
	   if(i.ne.7) call PanelHit( 17, row, i + 9, '   OFF', 6 )
	   IF ( i .eq. 3 ) then
!            - NBTW option different from rest
	     row = row + 1
	     call PanelHit( 17, row, 21, '      ', 6 )
	   ENDIF
	 ENDIF
       ENDDO

!     - set text color for selectable (hit) areas
      call PigSetTextColour( HitColor )

!     - External criteria filename (17)
!      call PanelHit( 1, 17, 17, VCritName, 24 )

!     - ACCEPT button (18)
!      call PanelHit( 10, 20, 18, 'ACCEPT', 6 )

!     - QUIT button (19)
!      call PanelHit( 11, 22, 19, 'QUIT', 4 )

!     - Print the instructional message
!      call PigPutMessage(
!     +          'Select criterion, "QUIT" when done.')
!     +          'Select criterion, "ACCEPT" or "QUIT" when done')
      call PigSetTextColour( HitColor )

!    - dont erase right hand panel at end unless "QUIT" selected
      Erase = .FALSE.
      Accepted = .FALSE.
      return

      entry criteria_ehandler(hitnum)
      IF (    ( (hitnum .ge. 2) .AND. (hitnum .le. 15) )  &
     &   .OR. ( (hitnum .eq. 20) .OR. (hitnum .eq. 21) )) THEN
!             - selection  other than EXT, turn EXT OFF
	  IF ( TheCriteria(15) ) THEN
	      TheCriteria(15) = .FALSE.
!	      call PanelHit( 14, 13, 16, '   OFF', 6 )
	  ENDIF
      ENDIF

      IF ( hitnum .eq. 18 ) THEN
!           - ACCEPT
	    Accepted = .TRUE.
      ELSEIF ( hitnum .eq. 19 ) THEN
!           - QUIT
	    Accepted = .TRUE.
	    Erase = .TRUE.
      ELSE IF ( (hitnum .ge. 2) .and. (hitnum .le. 9) ) THEN
!           - hit in column 1, C0 -> NC0
!            ok = .TRUE.
!           - set index to TheCriteria()
	    j = hitnum - 1
!           - Toggle the criteria flag
	    IF ( TheCriteria(j) ) THEN
	      TheCriteria(j) = .FALSE.
	      call PigSetTextColour( HitColor )
	      call PanelHit( 6, 5 + j, hitnum, 'OFF', 3 )
	    ELSE
	      TheCriteria(j) = .TRUE.
!             - set color for ON value according to criteria
	      IF ( j .eq. 1 ) THEN
		call PigSetTextColour( cc0 )
	      ELSE IF ( j .eq. 2 ) THEN
		call PigSetTextColour( cc1 )
	      ELSE IF ( j .eq. 3 ) THEN
		call PigSetTextColour( cc2 )
	      ELSE IF ( j .eq. 4 ) THEN
		call PigSetTextColour( cc3 )
	      ELSE IF ( j .eq. 5 ) THEN
		call PigSetTextColour( cc4 )
	      ELSE IF ( j .eq. 6 ) THEN
		call PigSetTextColour( cc5 )
	      ELSE IF ( j .eq. 7 ) THEN
		call PigSetTextColour( cc6 )
	      ELSE IF ( j .eq. 8 ) THEN
		call PigSetTextColour( cnotc0 )
	      ENDIF
	      call PanelHit( 6, 5 + j, hitnum, 'ON ', 3 )
	      call PigSetTextColour( HitColor )
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, 'ON ', 3 )
	      ENDIF
	    ENDIF
!             - ( TheCriteria(j) )
      ELSEIF ( hitnum .eq. 1 ) THEN
!           - DISPLAY FLAG
!            ok = .TRUE.
	    IF ( DrawVCrit ) THEN
	      DrawVCrit = .FALSE.
	      call PanelHit( 15, 4, 1, 'OFF', 3 )
	    ELSE
	      DrawVCrit = .TRUE.
	      call PanelHit( 15, 4, 1, 'ON ', 3 )
	    ENDIF
      ELSEIF ( hitnum .eq. 10 ) THEN
!           - DLT
!            ok = .TRUE.
	    IF ( TheCriteria(9) ) THEN
	      TheCriteria(9) = .FALSE.
	      call PanelHit( 17, 6, 10, '   OFF     ', 11 )
	    ELSE
2             continue
	      call PigPrompt('Enter depth for comparison:',ans )
	      call PigReadReal( ans, USER_COUNT, Success )
	      IF ( .NOT. Success ) THEN
		GOTO 2
	      ENDIF
	      TheCriteria(9) = .TRUE.
	      call PigSetTextColour( cdlt )
	      WRITE( rnum, '(F11.3)' ) USER_COUNT
	      call PanelHit( 16, 6, 10, rnum, 11 )
	      call PigSetTextColour( HitColor )
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, ' ON', 3 )
	      ENDIF
	    ENDIF
      ELSEIF ( hitnum .eq. 11 ) THEN
!           - DGT
!            ok = .TRUE.
	    IF ( TheCriteria(10) ) THEN
	      TheCriteria(10) = .FALSE.
	      call PanelHit( 17, 7, 11, '   OFF     ', 11 )
	    ELSE
4             continue
	      call PigPrompt('Enter depth for comparison:',ans )
	      call PigReadReal( ans, USER_COUNT1, Success )
	      IF ( .NOT. Success) THEN
		goto 4
	      ENDIF
	      TheCriteria(10) = .TRUE.
	      call PigSetTextColour( cdgt )
	      WRITE( rnum, '(F11.3)' ) USER_COUNT1
	      call PanelHit( 16, 7, 11, rnum, 11 )
	      call PigSetTextColour( HitColor )
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, ' ON', 3 )
	      ENDIF
	    ENDIF
      ELSEIF ( (hitnum .eq. 12) .OR. (hitnum .eq. 21) ) THEN
!           - DBTW
!            ok = .TRUE.
	    IF ( TheCriteria(11) ) THEN
	      TheCriteria(11) = .FALSE.
	      call PanelHit( 17, 8, 12, '   OFF     ', 11 )
!             - second line ( UPPERD line )
	      call PanelHit( 16, 9, 21, '           ', 11 )
	    ELSE
6             continue
	      call PigPrompt('Enter lower depth limit:',ans )
	      call PigReadReal( ans, LOWERD, Success )
	      IF ( .NOT. Success ) THEN
		GOTO 6
	      ENDIF
8             continue
		call PigPrompt('Enter upper depth limit:',ans )
	      call PigReadReal( ans, UPPERD, Success )
	      IF ( .NOT. Success ) THEN
		GOTO 8
	      ENDIF
	      call PigSetTextColour( cdbtw )
	      WRITE( rnum, '(F11.3)' ) LOWERD
	      call PanelHit( 16, 8, 12, rnum, 11 )
	      WRITE( rnum, '(F11.3)' ) UPPERD
	      call PanelHit( 16, 9, 21, rnum, 11 )
	      call PigSetTextColour( HitColor )
	      TheCriteria(11) = .TRUE.
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, ' ON', 3 )
	      ENDIF
	    ENDIF
      ELSEIF ( hitnum .eq. 13 ) THEN
!           - NBGT
!            ok = .TRUE.
	    IF ( TheCriteria(12) ) THEN
	      TheCriteria(12) = .FALSE.
	      call PanelHit( 17, 10, 13, '   OFF', 6 )
	    ELSE
11            continue
	      call PigPrompt('Enter number of neighbors for test:',ans )
	      read( ans, FMT = '(I6)', ERR = 11 ) USER_NCOUNT
	      IF ( USER_NCOUNT .lt. 0 ) THEN
		GOTO 11
	      ENDIF
	      TheCriteria(12) = .TRUE.
	      call PigSetTextColour( cnbgt )
	      WRITE( inum, '(I6)' ) USER_NCOUNT
	      call PanelHit( 17, 10, 13, inum, 6 )
	      call PigSetTextColour( HitColor )
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, ' ON', 3 )
	      ENDIF
	    ENDIF
      ELSEIF ( hitnum .eq. 14 ) THEN
!           - NBLT
!            ok = .TRUE.
	    IF ( TheCriteria(13) ) THEN
	      TheCriteria(13) = .FALSE.
	      call PanelHit( 17, 11, 14, '   OFF', 6 )
	    ELSE
13            continue
	      call PigPrompt('Enter number of neighbors for test:',ans )
!	      ans = PigUpperS( ans )
	      read( ans, FMT = '(I6)', ERR = 13 ) USER_NCOUNT1
	      IF ( USER_NCOUNT1 .lt. 0 ) THEN
		GOTO 13
	      ENDIF
	      TheCriteria(13) = .TRUE.
	      call PigSetTextColour( cnblt )
	      WRITE( inum, '(I6)' ) USER_NCOUNT1
	      call PanelHit( 17, 11, 14, inum, 6 )
	      call PigSetTextColour( HitColor )
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, ' ON', 3 )
	      ENDIF
	    ENDIF
      ELSEIF ( hitnum .eq. 15 ) THEN
!           - NBE
!            ok = .TRUE.
	    IF ( TheCriteria(14) ) THEN
	      TheCriteria(14) = .FALSE.
	      call PanelHit( 17, 12, 15, '   OFF', 6 )
	    ELSE
15            continue
		call PigPrompt('Enter number of neighbors for test:',ans )
!	      ans = PigUpperS( ans )
	      read( ans, FMT = '(I6)', ERR = 15 ) USER_NCOUNT2
	      IF ( USER_NCOUNT2 .lt. 0 ) THEN
		GOTO 15
	      ENDIF
	      TheCriteria(14) = .TRUE.
	      call PigSetTextColour( cnbe )
	      WRITE( inum, '(I6)' ) USER_NCOUNT2
	      call PanelHit( 17, 12, 15, inum, 6 )
	      call PigSetTextColour( HitColor )
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, ' ON', 3 )
	      ENDIF
	    ENDIF
      ELSEIF ( hitnum .eq. 16 ) THEN
!           - EXT
	    IF ( TheCriteria(15) ) THEN
	      TheCriteria(15) = .FALSE.
!	      call PanelHit( 17, 13, 16, '   OFF', 6 )
	    ELSE
	      IF ( VCritName(1:4) .ne. 'NONE' ) THEN
		TheCriteria(15) = .TRUE.
!		call PanelHit( 14, 13, 16, '   ON', 6 )
		IF ( .NOT. DrawVCrit ) THEN
		  DrawVCrit = .TRUE.
		  call PanelHit( 15, 4, 1, ' ON', 3 )
		ENDIF
		call TurnOffOtherCriteria()
	      ELSE
		hitnum = 17
	      endif
	    endif
      ENDIF
      IF ( hitnum .eq. 17 ) THEN
		if(PigGetOpenFileName('Open Vertex Data File',ans, &
     &      'Data Files (*.dat),*.dat;All Files (*.*),*.*;')) then
		    anslen = len_trim( ans )
		    iexis  = .FALSE.
		    VCritName = ans(1:anslen)
		    TheCriteria(15) = .TRUE.
		    call PanelHit( 17, 13, 16, '   ON', 6 )
		    call PanelHit( 1, 17, 17, VCritName, 24 )
		    IF ( .NOT. DrawVCrit ) THEN
			DrawVCrit = .TRUE.
			call PanelHit( 15, 4, 1, ' ON', 3 )
		    ENDIF
		    call TurnOffOtherCriteria()
		ENDIF
      ELSEIF ( hitnum .eq. 20 ) THEN
!           - C=?, TheCriteria(16)
	    IF ( TheCriteria(16) ) THEN
	      TheCriteria(16) = .FALSE.
	      call PanelHit( 6, 14, 20, 'OFF', 3 )
	    ELSE
33            continue
		call PigPrompt( 'Enter CODE for test:', ans )
!	      ans = PigUpperS( ans )
	      read( ans, FMT = '(I3)', ERR = 33 ) USER_CODE
	      IF ( USER_CODE .lt. 0 ) THEN
		GOTO 33
	      ENDIF
	      TheCriteria(16) = .TRUE.
	      call PigSetTextColour( ccuser )
	      WRITE( inum, '(I3)' ) USER_CODE
	      call PanelHit( 6, 14, 20, inum(1:3), 3 )
	      call PigSetTextColour( HitColor )
	      IF ( .NOT. DrawVCrit ) THEN
		DrawVCrit = .TRUE.
		call PanelHit( 15, 4, 1, ' ON', 3 )
	      ENDIF
	    ENDIF
!             - ( TheCriteria(16) )
      ENDIF

      IF ( Erase ) THEN
!       - clear right hand panel
        call ClearRHPanel
      ENDIF

      END

!---------------------------------------------------------------------------*
      SUBROUTINE TurnOffOtherCriteria

! Purpose: Turn off all other criteria when external criteria is enabled.
! Givens : None
! Returns: None
! Effects: Criteria other than EXT are turned OFF and the screen is updated
!          accordingly.
! Written: Steve Prestage    June 1989
! Modified: Mar/91 (JDM), routines from PANELMOD.FOR used.



      INCLUDE 'critcom.inc'
      INCLUDE '../includes/graf.def'

! Local Variables
      integer i, row

!------------------BEGIN------------------------------

      call PigSetTextColour( HitColor )

!     - Turn off all criteria status except EXT
!     - column 1
      row = 5
      DO i = 1, 8
	 row = row + 1
	 TheCriteria(i) = .FALSE.
	 call PanelHit( 6, row, i + 1, 'OFF', 3 )
      ENDDO

!     - turn off 'C=?'
      TheCriteria(16) = .FALSE.
      call PanelHit( 6, 14, 20, 'OFF', 3 )

!     - column 2
      row = 5
      DO i = 9, 14
	 row = row + 1
	 TheCriteria(i) = .FALSE.
	 call PanelHit( 17, row, i + 1, '   OFF     ', 11 )
	 if ( i .eq. 11 ) then
	   row = row + 1
	   call PanelHit( 17, row, 21, '           ', 11 )
	 endif
      ENDDO

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
      SUBROUTINE External

! Purpose : To place coloured markers at vertices read in from
!           external file.
! Given   : File in EXTVER format (1X,I5,1X,I2) where each record
!           contains:
!               First entry  = vertex number
!               Second entry = number of colour required
!                              (for colour table, see S/R NUP_Init)
! Returns : None

      use MainArrays

      INCLUDE 'critcom.inc'
      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

! - LOCAL VARIABLES
      LOGICAL IN_BOX
!       - function set true if X and Y coordinates fall within
!         the current window limits
      integer i, v1, mmrec
      integer v2
!----------------------BEGIN--------------------------

!     - open file
      OPEN( UNIT = 23, FILE = VCritName, STATUS ='OLD', ERR = 20 )
      goto 30

!     - ERROR trap while reading from file..
40    continue
      close( 23 )

!     - ERROR trap when opening external file..
20    continue
      call PigPutMessage('ERROR - Reading external vertex critera file.')
      goto 300

30    continue
      mmrec = MREC * 5
      do 100 i = 1, mmrec
	 READ( 23, *, ERR = 40, END = 999 ) v1, v2
	 if ( IN_BOX(DXRAY(v1),DYRAY(v1)) ) then
	   call PUTMARKER( DXRAY(v1), DYRAY(v1), 4, v2 )
	 endif
100   continue
999   continue
      close (23)
300   continue

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
! Written: Steve Prestage   June 1989


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
! Written: Steve Prestage May 1989

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
! Written: Steve Prestage May 1989

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

      nrec = itot
!     - Perform vertex marking only if vertex marking has been turned on.
      if ( .NOT. DrawVCrit ) goto 999

      if (TheCriteria(15)) then
!   - EXT, put markers at vertices specified in external file
!       -- each record of which contains vertex number followed by
!       -- number of colour required. Record format is (1X,I5,1X,I2)
    call EXTERNAL
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
!     - C6, code = 6
      do i = 1, nrec
        if ( code(i).ge.0 ) then
          if ( CODE(i) .eq. 6 ) then
!       - put a marker wherever the code is 6
        if ( In_Box(DXRAY(i),DYRAY(i)) ) then
          call PutMarker( DXRAY(i), DYRAY(i), 4, cc6 )
        endif
!         - ( In_Box(DXRAY(i),DYRAY(i)) )
          endif
!       - ( CODE(i) eq 6 )
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

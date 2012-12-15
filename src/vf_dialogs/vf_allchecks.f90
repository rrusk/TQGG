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
!---------------------------------------------------------------------------*
!     This module contains the subroutines associated with the              *
!     quality tests for elements.                                           *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE FlagsTriangles_Init()

! Purpose: To set up RH Panel for FlagsTriangles routine.
! Givens : None
! Returns: None
! Effects: RH Panel is set up with 6 options to be used by FlasTriangles
!          routine.

!      use MainArrays
      implicit none 
      
!     - PASSED PARAMETERS
!      LOGICAL change

!     - INCLUDES
      INCLUDE '../includes/graf.def'

!     - COMMON AREAS
      LOGICAL fullcolour
      COMMON /SHADE/ fullcolour

!      LOGICAL TrHiOff
!      COMMON /TH/ TrHiOff

!     - LOCAL VARIABLES
      integer prev_colour

!---------- BEGIN --------------

      call PigGetTextColour(prev_colour)

!     - set up RH Panel
      call InitRHPanel

!     - display labels
      call PigSetTextColour(LabelColor)
      call Paneltext(4, 1, 'COLOR TRIANGLES', 15)
      call Paneltext(4, 2, '  BY CRITERIA  ', 15)
      call PanelText(1, 4, 'Coloring Mode:', 14)

!     - define options
      call PigSetTextColour(HitColor)
      fullcolour=.true.
      IF (fullcolour) THEN
        call PanelHit(4, 5, 1, 'FULL COLOR  ', 12)
      ELSE
        call PanelHit(4, 5, 1, 'COLOR MARKER', 12)
      ENDIF

      call PanelHit(  4,  7, 11, 'EQL', 3 )
      call PanelHit(  4,  8, 12, 'DEP', 3 )
      call PanelHit(  4,  9, 13, 'A2D', 3 )
      call PanelHit(  4, 10, 14, 'CCW', 3 )
      call PanelHit(  4, 11, 15, 'G90', 3 )
      call PanelHit(  4, 12, 16, 'COD', 3 )

      call PanelHit(  10, 15, 5, 'close', 6 )

      call PigSetTextColour(prev_colour)
      
      END

!---------------------------------------------------------------------------*

      SUBROUTINE FlagsTriangles_Ehandler(change, Hitnum)

! Purpose: Dispatch routine for the colouring of TRIANGLES according to
!          various user defined criteria.
! Givens : None
! Returns: CHANGE - TRUE IF any changes
! Effects: Coloring/marking of triangles by criteria may be set ON or OFF.

      use MainArrays
      
      implicit none

      INCLUDE '../includes/graf.def'
!      INCLUDE 'noticom.inc'

!     - PASSED PARAMETERS
      LOGICAL change

!     - COMMON AREAS
      LOGICAL fullcolour
      COMMON /SHADE/ fullcolour

!      integer Test
!      COMMON /CRITER/ Test

      LOGICAL TrHiOff
      COMMON /TH/ TrHiOff
      
!     - LOCAL VARIABLES
      integer hitnum
      integer PrevTextColour
      LOGICAL Accepted, cmode
!      logical :: start=.TRUE.

!--------------BEGIN-------------

      call PigGetTextColour(PrevTextColour)

!     - Load triangle lists
      call PigPutMessage('Forming triangle list-please wait')      
!      call LdTrLt(change)
      if(change) then
        call RemoveNotExist(itot,code,nbtot,nl)
        call Element_Lister(CHANGE, .FALSE. ,  &
     &          itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode,  &
     &          x0off,y0off,scaleX,scaleY,igridtype)
        change = .false.
      endif
      call PigEraseMessage

      Accepted = .FALSE.
      IF (hitnum .EQ. 1) THEN
!         - Coloring Mode (COLOR MARKER/FULL COLOR)
        call PigSetTextColour(HitColor)
        fullcolour = .not. fullcolour
        IF (fullcolour) THEN
          call PanelHit(4, 5, 1, 'FULL COLOR  ', 12)
        ELSE
          call PanelHit(4, 5, 1, 'COLOR MARKER', 12)
        ENDIF
      ELSEIF (hitnum .EQ. 5) THEN
!         - CLOSE
        Accepted = .TRUE.
        TrHiOff = .TRUE.
        call DrwFig(change)
      ELSE
        TrHiOff = .false.
!        call Nup_FCriteria_Ehandler (change, hitnum)
        cmode = fullcolour
        call ElementCheck(hitnum-10,cmode)
      ENDIF

      if(Accepted)then
!         - restore RH Panel to title screen
        call ClearRHPanel
      endif
      call PigSetTextColour(PrevTextColour)
      
      END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
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
      INCLUDE '../plotsubs/critcom.inc'
      INCLUDE '../includes/graf.def'
      INCLUDE '../plotsubs/critcol.def'
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



      INCLUDE '../plotsubs/critcom.inc'
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

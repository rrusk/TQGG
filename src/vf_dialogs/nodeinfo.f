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

C*--------------------------------------------------------------------------*
C                              INFO.FOR                                     *
C     This module controls the right hand display panel for the displaying  *
C     of information about vertices and allows the user to change values    *
C     associated with those vertices. The Text file option is also handled  *
C     in this module, along with a proposed module to display triangle info.*
C*--------------------------------------------------------------------------*
C*--------------------------------------------------------------------------*
      SUBROUTINE Init_Info()
C
C Purpose : To initialize the area to the right of the grid which
C           is the INFO screen.
C Given   : mmode = the info mode choice
C                   'INF' = info mode,
C                   'CHG' = change mode.
C Returns : None.

      INCLUDE '../includes/graf.def'

C--------BEGIN--------------

C       - initialize righthand panel
	call InitRHPanel

C       - Display Info Screen
	call Up_Info

	call PigSetTextColour( LabelColor )

	call PanelText( 5, 1, 'NODE INFORMATION', 16 )

	call PigPutMessage('Choose node in main window...')

	END

C*-------------------------------------------------------------------------*
      SUBROUTINE Up_Info
C
C Purpose : To create screen window for INFO.
C Assume  : InitRHPanel() has been called.
C Given   : None.
C Returns : None.



      INCLUDE '../includes/graf.def'

C--------BEGIN--------------

C       - define number of hit areas (options)
c        call SetNumHits( 0 )
C       - display labels
	call PigSetTextColour( LabelColor )
c       call PanelText( 6, 2, 'INFORMATION', 11 )
	call PanelText( 2, 4, 'INDEX:', 6 )
	call PanelText( 2, 5, '    X:', 6 )
	call PanelText( 2, 6, '    Y:', 6 )
	call PanelText( 2, 7, 'DEPTH:', 6 )
	call PanelText( 2, 8, ' CODE:', 6 )

	call PanelText( 5, 10, 'NEIGHBOUR LIST', 14 )
	call PanelTextRight( 4, 11, '1:', 2 )
	call PanelTextRight( 4, 12, '2:', 2 )
	call PanelTextRight( 4, 13, '3:', 2 )
	call PanelTextRight( 4, 14, '4:', 2 )
	call PanelTextRight( 4, 15, '5:', 2 )
	call PanelTextRight( 4, 16, '6:', 2 )
	call PanelTextRight( 4, 17, '7:', 2 )
	call PanelTextRight( 4, 18, '8:', 2 )

	call PanelTextRight( 16, 11, ' 9:', 3 )
	call PanelTextRight( 16, 12, '10:', 3 )
	call PanelTextRight( 16, 13, '11:', 3 )
	call PanelTextRight( 16, 14, '12:', 3 )
	call PanelTextRight( 16, 15, '13:', 3 )
	call PanelTextRight( 16, 16, '14:', 3 )
	call PanelTextRight( 16, 17, '15:', 3 )
	call PanelTextRight( 16, 18, '16:', 3 )

C       - create options
	call PigSetTextColour( HitColor )

C       - QUIT option (1)
        call PanelHit( 10, 21, 1, 'QUIT', 4 )

C       - CURSOR option (2)
c	call PanelHit( 6, 19, 2, 'CURSOR', 6 )

C       - INDEX option (3)
	call PanelHit( 9, 19, 3, 'INDEX', 5 )

C       - COORD option (4)
!	call PanelHit( 9, 21, 4, 'COORD', 5 )

C       - ERASE ALL option (5)
!	call PanelHit( 6, 23, 5, 'ERASE SOME', 10 )

c        call info_set_active(1003)

	END

C*--------------------------------------------------------------------------*
      SUBROUTINE ChgVal( changev, index, field )
C
C Written :   Russ Kirby,  May 1987
C Purpose : To change the value for the code, the XY location, or the depth
C           field for any point on the grid.
C Assume  : User has made a selection in routine GetVal() indicating the
C           point and the field to change.
C Given   : nrec   =  the number of points read in from the data file
C           index  =  index to data arrays of point whose field to change.
C           field  =  field to be changed, 1 = (X,Y)
C                                          2 = DEPTH
C                                          3 = CODE
C Return  : changev = logical set TRUE if any changes take place
C                     to the data that is to be written back
C                     to the files.
C Effects: Indicated field is updated with user entered value.
C          NOTE: routine MovDep() is called here, Movdep contains a call
C                to PanelHit() for writing new depth value to screen.

      use MainArrays

      INCLUDE '../includes/graf.def'

C - PASSED VARIABLES
      LOGICAL changev
      integer index, field
C
!      REAL    MININ,MAXIN,SCUNIT
!      COMMON  /MAXMIN/ MININ,MAXIN,SCUNIT

      integer  MVNDX
      COMMON   /PICK/ MVNDX

C - LOCAL VARIABLES
      REAL pxray(20), pyray(20)
C          - To temporarily hold the information for a point
      CHARACTER*80 ans
      character cstr*80, PigCursYesNo*1
C          _ For user prompts
      integer icode
C          - temporarily holds a new code entered by the user
      integer ierr, idrr
C          - error variables
      integer icnt
C          - the number of neighbours for a specific point
      REAL xnew, ynew
C          - old and new X and Y locations for a point
      REAL tdepth
C          - temporarily holds new depth entered by user
      LOGICAL tnear, trans, Success
C          -  marking proximity to neighbour
      integer new_index
C          -  index of near neighbour (dummy)
      REAL prevdepth
      integer anslen, prevcode
c      CHARACTER*6 num
c      CHARACTER*13 rnum
C-----------------BEGIN------------------------

      MVNDX = index
      if ( field .eq. 3 ) then
C       - change the CODE field
	prevcode = CODE(index)
	Success = .FALSE.
	do while ( .NOT. Success )
10        continue
	  call PigPrompt('Enter a Non-Negative code for this'//
     +                ' point:',ans)
C         - get a new code from the user
	  READ( ans, FMT = '(I4)', err = 10 ) icode
	  anslen = LEN_TRIM( ans )
	  if ( anslen .lt. 1 ) then
C           - restore original code if only <RTN> entered
	    icode = prevcode
	  endif
	  if ( icode .ge. 0 ) then
	    Success = .TRUE.
	  endif
	enddo
C         - ( NOT Success )
	CODE(index) = icode
C       - set the change variable
	changev = .TRUE.
      elseif ( field .eq. 2 ) then
C       - change the DEPTH field
	prevdepth = DEPTH(index)
	Success = .FALSE.
	do while ( .NOT. Success )
	  call PigPrompt('Enter a new depth for this point:',ans )
C         - get a new depth from the user
	  call PigReadReal( ans, tdepth, Success )
	  anslen = LEN_TRIM( ans )
	  if ( anslen .lt. 1 ) then
	    Success = .TRUE.
	    tdepth = prevdepth
	  endif
!	  if ( tdepth .lt. 0.0 ) then
!	    Success = .FALSE.
!	  endif
	enddo
C         - ( NOT Success )
	DEPTH(index) = tdepth
C       - set the change variable
	changev = .TRUE.
      elseif ( field .eq. 1 ) then
C       - change the X,Y location of a point
C       -- following changed MAY92, don't prompt for method of change
C       -- assume entering coordinates, code commented out for ease of
C       -- changing back to original method
	ans(1:1) = 'X'
	anslen = 1
	if ( ans(1:1) .ne. 'Q' ) then
C         - put a marker on the point that the user wants to change
c         call PUTMARKER( DXRAY(index), DYRAY(index), 4, yellow)
C         - store point in location 1 of temp arrays
	  pxray(1) = DXRAY(index)
	  pyray(1) = DYRAY(index)
	  icnt = 1
C         - get all the neighbours to that point
	  call RELPT2( pxray, pyray, mvndx, icnt, ierr )
	  if ( ierr .eq. 999 ) then
	    call PigPutMessage( 'ERROR determining neighbors...ABORTED' )
	    ans(1:1) = 'Q'
	  endif
C           - ( ierr eq 999 )
	endif
C         - ( ans ne Q )
	if ( ans(1:1) .eq. 'C' ) then
C         - get new location by cursor
	  call NEWPT( xnew, ynew, 2, ierr )
	  if ( ierr .eq. 999 ) then
	    call PigPutMessage( 'ERROR locating point...ABORTED' )
	    ans(1:1) = 'Q'
	  endif
	elseif ( ans(1:1) .eq. 'X' ) then
C         - get new location by prompting for X and Y
	  Success = .FALSE.
	  do while ( .NOT. Success )
C           - get X location
	    call PigPrompt( 'Enter the X coordinate:',ans )
	    call PigReadReal( ans, xnew, Success )
	  enddo
C         - ( NOT Success )
	  Success = .FALSE.
	  do while ( .NOT. Success )
C           - get the Y location
	    call PigPrompt('Enter the Y coordinate:',ans)
	    call PigReadReal( ans, ynew, Success )
	  enddo
C           - ( NOT Success )
	endif
C         - ( ans = C )
	if ( ans(1:1) .ne. 'Q' ) then
C         - erase old point & lines to it's neighbours
	  call PigSetWindowNum( MAINWIN )
	  call ReDraw( pxray, pyray, icnt, 0 )
	  pxray(1) = xnew
	  pyray(1) = ynew
C         - redraw the point
	  call ReDraw( pxray, pyray, icnt, 1 )
	  call PigSetWindowNum( CONTROLWIN )
	  tnear = .FALSE.
	  trans = .FALSE.
          call CHKTRANS( xnew, ynew, MVNDX, new_index, trans, 
     +                   tnear)
          if ( trans .OR. tnear ) then
            call PigMessageOK('N.B. - This is a move, NOT a merge.',' ')
!           call PigUWait( 2.0 )
          endif
C         - get user confirmation, loop till valid response
          Success = .FALSE.
	  cstr = 'Is this move satisfactory ?:'
	  do while ( .NOT. Success )
	    ans = PigCursYesNo (cstr)
	    if ( ans(1:1) .eq. 'Y' ) then
C             - satisfactory move
	      Success = .TRUE.
C             - update data files
	      call MovDep( mvndx, xnew, ynew, idrr)
	      call PigPutMessage(
     +                 'New depth set by interpolation.' )
	      call Update( mvndx, xnew, ynew )
C             - set the change variable
	      changev = .TRUE.
C             - put new values on screen
*              call PigSetTextColour( HitColor )
*              WRITE( rnum, '(f12.3)' ) xnew
*              call PanelHit( 8, 5, 6, rnum, 12 )
*              WRITE( rnum, '(f12.3)' ) ynew
*              call PanelHit( 8, 6, 7, rnum, 12 )
	    elseif ( ans(1:1) .eq. 'N' ) then
	      Success = .TRUE.
C             - not satisfactory
	      call PigPutMessage( 'No update..' )
C             - erase the new
	      call PigSetWindowNum( MAINWIN )
	      call ReDraw( pxray, pyray, icnt, 0 )
C             - replace the old
	      pxray(1) = DXRAY(index)
	      pyray(1) = DYRAY(index)
	      call ReDraw( pxray, pyray, icnt, 1 )
	      call PigSetWindowNum( CONTROLWIN )
	    endif
C             - ( ans(1:1) = Y )
	  enddo
C           - ( NOT Success )
	endif
C         - ( ans(1:1) ne Q )
      endif
C       - ( field = 3 )

      END


C*--------------------------------------------------------------------------*
      SUBROUTINE GetVal_CW_ehandler( index, nrec,mmode, Hitnum)

C Purpose : To get all the information for a point on the grid
C Given   : nrec = the number of points read in from the data file
C           mmode = whether operation is for Information
C                   or Change a value
C Returns : index = index of the point we will be examining,

      use MainArrays

      INCLUDE '../includes/graf.def'

C - PASSED VARIABLES
      integer index
      integer nrec
      CHARACTER*3 mmode
      LOGICAL changev

      REAL     CWXL,CWXH,CWYL,CWYH
      COMMON  /CURWIN/ CWXL,CWXH,CWYL,CWYH

!      REAL    MININ,MAXIN,SCUNIT
!      COMMON  /MAXMIN/ MININ,MAXIN,SCUNIT

      integer last_val_node
      common /val_node/ last_val_node

C - LOCAL VARIABLES
	LOGICAL Done
        logical okpoint,success
	integer hitnum
	CHARACTER*80 ans
c       data okpoint /.FALSE./
c       data okpoint /.TRUE./
C--------------BEGIN-----------------

       changev = .FALSE.

       Done = .FALSE.
       okpoint = .TRUE.
	if ( hitnum .eq. 1 ) then
C         - 'QUIT'
	  INDEX = 0
	  Done = .TRUE.
	  mmode = 'QIT'
        okpoint = .FALSE.
!	elseif ( hitnum .eq. 2 ) then
C         - 'CURSOR'
!	  ok = .FALSE.
!	  call NEWPT( xpos, ypos, 0, ierr )
C         - see if the point exists
!	  call CHKPT( xpos, ypos, INDEX, ierr )
!	  if ( ierr .eq. 1 ) then
!	    call PigPutMessage('ERROR - Invalid point..')
!            okpoint = .FALSE.
!	  endif
	elseif ( hitnum .eq. 3 ) then
C         - 'INDEX'
7         continue
	  call PigPrompt('Enter index:', ans )
	  READ( ans, FMT = '(I6)', ERR = 7 ) INDEX
C         - see if point chosen is legal
	  if ( (INDEX .lt. 1) .or. (INDEX .gt. NREC) ) then
	    call PigPutMessage('ERROR - Invalid point..')
            okpoint = .FALSE.
      else
        call PigSetWindowNum( MAINWIN )
        call PutPermMarker( DXRAY(index), DYRAY(index), Success )
        call PigSetWindowNum( CONTROLWIN )
      endif
!	elseif ( hitnum .eq. 4 ) then
C         - 'COORD'
!	  ok = .FALSE.
!	  do while ( .NOT. ok )
!	    call PigPrompt('Enter the X coordinate:',ans)
!	    call PigReadReal( ans, xpos, ok )
!	  enddo
C           - ( NOT ok )
!	  ok = .FALSE.
!	  do while ( .NOT. ok )
!	    call PigPrompt('Enter the Y coordinate:',ans)
!	    call PigReadReal( ans, ypos, ok )
!	  enddo
C           - ( NOT ok )
C         - see if point exists
!	  call CHKPT( xpos, ypos, INDEX, ierr )
!	  if ( ierr .eq. 1 ) then
!	    call PigPutMessage('ERROR - Invalid point..')
!            okpoint = .FALSE.
!	  endif
!	elseif ( hitnum .eq. 5 ) then
C         - 'ERASE ALL'
!	  call DrwFig(changev)
	endif
C         - ( hitnum = 0 )
        if ( okpoint ) then
C         - a point has been selected, if this point exists (hasn't been
C         -- flagged for deletion, etc) then put up all the values for
C         -- this point on the screen
!	  if ( Exist(index) ) then
	  if ( code(index).ge.0 ) then
	    call PigSetWindowNum( MAINWIN )
	    call PutMarker( DXRAY(index), DYRAY(index), 4, yellow )
	    call PigSetWindowNum( CONTROLWIN )
	    call Put_Val( index, mmode )
	    if ( mmode .eq. 'CHG' ) then
C             - check for a CHANGE option hit
	      if ( (hitnum .eq. 6) .OR. (hitnum .eq. 7) ) then
C               - CHANGE (X,Y)
		    call ChgVal( changev, index, 1 )
	      elseif ( hitnum .eq. 8 ) then
C               - CHANGE DEPTH
		    call ChgVal( changev, index, 2 )
	      elseif ( hitnum .eq. 9 ) then
C               - CHANGE CODE
		    call ChgVal( changev, index, 3 )
	      endif
	      if(changev) call Put_Val( index, mmode )
	    endif
	  else
C           - an invalid point was selected
c            okpoint = .FALSE.
	  endif
C           - ( Exist(index) )
       endif
C         - ( okpoint )
       if(Done) then
	  call ClearRHPanel

      endif
      END

      SUBROUTINE GetVal_MW_Ehandler(mmode, Xinp, Yinp, Index)

C Purpose : To get all the information for a point on the grid
C Given   : nrec = the number of points read in from the data file
C           mmode = whether operation is for Information
C                   or Change a value
C           Xinp = x position to locate node
C           Yinp = y position to locate node
C Returns : index = index of the point we will be examining,
C           changev = logical set TRUE if any changes take place
C                     to the data that is to be written back
C                     to the files.

      use MainArrays

      INCLUDE '../includes/graf.def'
C - PASSED VARIABLES
      real xinp, yinp
      integer index
      CHARACTER*3 mmode
C
C *** LOCAL VARIABLES ***
C
      integer ierr

C     - see if the point exists
      call CHKPT( xinp, yinp, INDEX, ierr )
      if ( ierr .eq. 1 ) then
	  call PigPutMessage('ERROR - Invalid point..')
!      else if ( Exist(index) ) then
      else if ( code(index).ge.0 ) then
	  call PutMarker( DXRAY(index), DYRAY(index), 4, yellow)
	  call Put_Val( index, mmode )
      endif
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE Repaint_Info
C
C Purpose : To repaint the node info screen with saved node info
C Given   : last_val_node in common
C Effects : refreshes the node info screen and updates with the 
C	    information for the saved node

      use MainArrays

      INCLUDE '../includes/graf.def'

      integer last_val_node
      common /val_node/ last_val_node

	integer index

	index = last_val_node

	call Init_Info()
	if	(	(index .gt. 0)
     +		.and.	(index .le. MREC)
     +		) then
		call PutMarker( DXRAY(index), DYRAY(index), 4, yellow)
		call Put_Val( index, 'CHG')
	endif
      end
C*--------------------------------------------------------------------------*
      SUBROUTINE Put_Val( index, mmode )
C
C Purpose : To put the values on the info screen for a
C           specified vertex point
C Given   : index = index to data arrays of point.
C           mmode = 'INF' if info mode, = 'CHG' if change mode

      use MainArrays

      INCLUDE 'ipig.def'

C - PASSED VARIABLES
      integer index
      CHARACTER*3 mmode

!      REAL    MININ,MAXIN,SCUNIT
!      COMMON  /MAXMIN/ MININ,MAXIN,SCUNIT

      integer last_val_node
      common /val_node/ last_val_node

C - LOCAL VARIABLES
      integer i, nbt, prev_justify
      CHARACTER num*6, rnum*12
c      CHARACTER num*9, rnum*18

C---------BEGIN------------------
C	- save node number for refresh operations
        last_val_node = index

C     - set text color according to mode
      call PigGetJustification(prev_justify)
      if ( mmode .eq. 'CHG' ) then
        call PigSetJustification(RIGHT_JUSTIFY)
        call PigSetTextColour( HitColor )
C       - x field
        WRITE( rnum, '(f12.3)' ) DXRAY(index)
	call PanelHit( 20, 5, 6, rnum, 12 )
C       - y field
	WRITE( rnum, '(f12.3)' ) DYRAY(index)
	call PanelHit( 20, 6, 7, rnum, 12 )
C       - depth field
	WRITE( rnum, '(f12.3)' ) DEPTH(index)
	call PanelHit( 20, 7, 8, rnum, 12 )
C       - code field
        WRITE( num, '(I6)' ) CODE(index)
        call PanelHit( 20, 8, 9, num, 4 )
        call PigSetJustification(LEFT_JUSTIFY)
      else
        call PigSetTextColour( NoHitColor )
C       - x field
	WRITE( rnum, '(f12.3)' ) DXRAY(index)
	call PanelTextRight( 20, 5, rnum, 12 )
C       - y field
	WRITE( rnum, '(f12.3)' ) DYRAY(index)
	call PanelTextRight( 20, 6, rnum, 12 )
C       - depth field
	WRITE( rnum, '(f12.3)' ) DEPTH(index)
	call PanelTextRight( 20, 7, rnum, 12 )
C       - code field
	WRITE( num, '(I6)' ) CODE(index)
	call PanelTextRight( 20, 8, num, 4 )
      endif

C     - set color for non-selectable fields
      call PigSetTextColour( NoHitColor )

C     - index field
      WRITE( num, '(I6)' ) index
      call PanelTextRight( 20, 4, num, 6 )

C   Set nbt equal to position of highest non-zero neighbour
      nbt = 0
      do i = 1, NBTOTR
	if ( NL(i,index) .ne. 0 ) nbt = i
      enddo
      do i = 1,8
	if(i.le.NBT)then
		write( num, '(I6)' ) NL(i,index)
	else
		num = ' '
	end if
	  call PanelTextRight( 10, (10+i), num, 6)
      end do
      do i = 9,16
	if(i.le.NBT)then
		write( num, '(I6)' ) NL(i,index)
	else
		num = ' '
        end if
          call PanelTextRight( 24, (10+i-8), num, 6)
      end do
      call PigSetJustification(prev_justify)
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE InfoFiles
C
C Purpose : Displays the default filenames currently in use during the
C           current interactive session.
C Givens  : None
C Returns : None
C Effects : None

      INCLUDE '../includes/defaults.inc'

      character*256 cstr
	integer len1, len2, len3, len4
C-----------BEGIN------------------

      if(DispNodes) then
        len1 = len_trim(NodeRName)
      else
        len1 = len_trim(GridRName)
	endif
      len2 = len_trim(LastInterim)
      len3 = len_trim(ContFName)
      len4 = len_trim(BoundFName)
!      len5 = len_trim(VCritName)
!      len6 = len_trim(TCritName)

      if(DispNodes) then
        cstr = 
     +'Node File: '// NodeRName(:len1)//char(13)//
     +'Last Interim Node File: '// LastInterim(:len2)//char(13)//
     +'Contours File: '// ContFName(:len3)//char(13)//
     +'Boundary File: '// BoundFName(:len4)//char(0)  !//
!     +'Vertex Criterion File: '// VCritName(:len5)//char(13)//
!     +'Triangle Criterion File: '// TCritName(:len6)//char(0)
	else
        cstr = 
     +'Grid File: '// GridRName(:len1)//char(13)//
     +'Last Interim Grid File: '// LastInterim(:len2)//char(13)//
     +'Contours File: '// ContFName(:len3)//char(13)//
     +'Boundary File: '// BoundFName(:len4)//char(0)  !//
!     +'Vertex Criterion File: '// VCritName(:len5)//char(13)//
!     +'Triangle Criterion File: '// TCritName(:len6)//char(0)
      endif

	call PigMessageOK(cstr, 'FILES')

      END
C-----------------------------------------------------------------------*
C                       END INFO.FOR                                    *
C-----------------------------------------------------------------------*
C-----------------------------------------------------------------------*

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
C                           CONFIG.FOR                                      *
C     This module contains the controlling functions for the configuration  *
C     option.  The setting of colours for boundaries, contours, and grid    *
C     lines and the display and toggling of configuration options in the    *
C     right hand display panel.                                             *
C*--------------------------------------------------------------------------*
C*--------------------------------------------------------------------------*
      subroutine config_ehandler(hitnum)
c
      implicit none
c
      integer hitnum
      integer idp
      integer active_id
      integer prev_win
      data active_id /-1/

      call PigGetWindowNum(prev_win)
      if(hitnum.gt.0) then

	  if(active_id.eq.1) then
	      call configlines_ehandler(hitnum)
	  else if(active_id.eq.2) then
	      call configboundaries_ehandler(hitnum)
	  else if(active_id.eq.3) then
	      call configcontours_ehandler(hitnum)
	  else if(active_id.eq.-1) then
c              - null option...
	  else
	  endif
      endif
      call PigSetWindowNum(prev_win)
      return

      entry config_set_active(idp)
      active_id = idp
      end

      SUBROUTINE ConfigLines_init
c      (hitnumdummy)
C
C Purpose: Toggles the grid default attributes.
C Givens : None
C Returns: None
C Effects: Defaults are set.
C Written: Steve Prestage and Daphne Connolly   May 1989
C Modified: Mar/91 (JDM), all segments removed
c
      implicit none
c
 
c      integer hitnumdummy

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

C     - LOCAL VARIABLES
      integer temp, hitnum
      LOGICAL Accepted
      CHARACTER*8 tmp
      CHARACTER*80 ans

C-----------------BEGIN-------------------------

C     - Right window definition T9
      call InitRHPanel

C     - display all labels
      call PigSetTextColour( LabelColor )
      call PanelText( 3, 1,  'GRID CONFIGURATION', 18 )
      call PanelText( 1, 3,  'File:', 5 )
      call PanelText( 1, 6,  'Next Interim Save File:', 23 )
      call PanelText( 1, 9,  'Grid Colour', 11 )
      call PanelText( 3, 10, 'First :', 7 )
      call PanelText( 3, 11, 'Second:', 7 )
      call PanelText( 3, 12, 'Modify:', 7 )
      call PanelText( 1, 14, 'Secondary colour Index:', 23 )

C     - display all unchangeable text
      call PigSetTextColour( NoHitColor )
      call PanelText( 1, 4, GridRName, 12 )
      call PanelText( 1, 7, GridIName, 12 )

C     - display all changeable text
      call PigSetTextColour( HitColor )

C     - ACCEPT option ( hit # 1 )
      call PanelHit( 10, 20, 1, 'CLOSE', 5 )

C     - Index for second colour to begin ( hit # 2 )
      write ( tmp, FMT = '(I6)' ) GridSIndex
      call PanelHit( 3, 15, 2, tmp(1:6), 6 )

C     - Grid Primary colour ( hit # 3 )
      call PanelHitBox( 12, 10, 3, 5, GridPColour )

C     - Grid Secondary colour ( hit # 4 )
      call PanelHitBox( 12, 11, 4, 5, GridSColour )

C     - Grid Modification colour ( hit # 5 )
      call PanelHitBox( 12, 12, 5, 5, ModificationColour )

C     - loop getting user hits until ACCEPT selected
C      hitnum = 0
      Accepted = .FALSE.
C       - print Instructions
C        call PigPutMessage('Pick option to toggle.  ACCEPT to accept'//
C     +                    ' the defaults.')

      call config_set_active(1)
      return

      entry configlines_ehandler(hitnum)
c      DO WHILE ( .NOT. Accepted )
	call PigSetTextColour( HitColor )

C       - get user selection
c        call GetPanelHit( hitnum )
	 if ( hitnum .eq. 1 ) then
	    Accepted = .TRUE.
C           - clean up righthand panel and message line
	    call config_set_active(-1)
	    call ClearRHPanel
	 elseif ( hitnum .eq. 3) then
	    if ( GridPColour .eq. MaxColour ) then
	      GridPColour = 0
	    else
	      GridPColour = GridPColour + 1
	    endif
	    call PanelHitBox( 12, 10, 3, 5, GridPColour )
	  else if ( hitnum .eq. 4 ) then
	    if ( GridSColour .eq. MaxColour ) then
	      GridSColour = 0
	    else
	      GridSColour = GridSColour + 1
	    endif
	    call PanelHitBox( 12, 11, 4, 5, GridSColour )
	  else if ( hitnum .eq. 5 ) then
	    if ( ModificationColour .eq. MaxColour ) then
	      ModificationColour = 0
	    else
	      ModificationColour = ModificationColour + 1
	    endif
	    call PanelHitBox( 12, 12, 5, 5, ModificationColour)
	  else if ( hitnum .eq. 2 ) then
30          continue
	    call PigPrompt('Enter index of vertex where'//
     +                  ' second colour is to begin:', ans)
	    call PigPutMessage(ans)
	    read( ans, FMT = '(I6)' , ERR = 30 ) temp
	    call PigSetTextColour( HitColor )
	    GridSIndex = temp
	    tmp = ' '
	    write( tmp, FMT = '(I6)' ) GridSIndex
	    call PanelHit( 3, 15, 2, tmp(1:6), 6 )
	  endif
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE ConfigBoundaries_Init()
C
C Purpose: Toggles the attributes for the display of boundary information.
C Givens : None
C Returns: None
C Effects: None
C Written: Steve Prestage and Daphne Connolly  May 1989
C Modified: Mar/91 (JDM), all segments removed
C
c
      implicit none
c
      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

C - LOCAL VARIABLES
      integer anslen, hitnum
      LOGICAL Valid, Accepted
      CHARACTER*80 prompt,template
      character(256) ans
c      CHARACTER*4 test
      logical PigGetOpenFileName

C------------------BEGIN------------------

C     - Clear right hand panel.
      call InitRHPanel

C     - display all labels
      call PigSetTextColour( LabelColor )
      call PanelText( 9, 1, 'BOUNDARY', 8 )
      call PanelText( 7, 2, 'CONFIGURATION', 13 )
      call PanelText( 1, 4, 'Display Flag:', 13 )
      call PanelText( 1, 6, 'Boundary File:', 14 )
      call PanelText( 1, 9, 'Boundary Colour:', 16 )
      call PanelText( 9, 12, 'CONTOURS', 8 )
      call PanelText( 7, 13, 'CONFIGURATION', 13 )
      call PanelText( 1, 15, 'Display Flag:', 13 )
      call PanelText( 1, 17, 'Contours File:', 14 )
      call PanelText( 1, 20, 'Contours Colour:', 16 )

C     - display all changeable text
      call PigSetTextColour( HitColor )

C     - Create Filename option ( hit # 4 )
      call PanelHit( 1, 7, 4, BoundFName, 24 )

C     - Create Filename option ( hit # 7 )
      call PanelHit( 1, 18, 7, ContFName, 24 )

C     - ACCEPT option ( hit # 1 )
      call PanelHit( 10, 22, 1, 'CLOSE', 5 )

C     - Create Display Flag option ( hit # 2 )
      if ( DispBound ) then
	 call PanelHit( 15, 4, 2, ' ON', 3 )
      else
	 call PanelHit( 15, 4, 2, 'OFF', 3 )
      endif

C     - Create Display Flag option ( hit # 5 )
      if ( DispCont ) then
	 call PanelHit( 15, 15, 5, ' ON', 3 )
      else
	 call PanelHit( 15, 15, 5, 'OFF', 3 )
      endif

C     - Create Colour option ( hit # 3 )
      call PanelHitBox( 17, 9, 3, 5, BoundColour )

C     - Create Colour option ( hit # 6 )
      call PanelHitBox( 17, 20, 6, 5, ContColour )

      call config_set_active(2)
      return

      entry ConfigBoundaries_ehandler(hitnum)

      if ( hitnum .eq. 1 ) then
	  Accepted = .TRUE.
	  call config_set_active(-1)
C           - clear righthand panel
	  call ClearRHPanel
      elseif ( hitnum .eq. 3 ) then
	  if ( BoundColour .eq. MaxColour ) then
	      BoundColour = backgr
	  else
	      BoundColour = BoundColour + 1
	  endif
	  call PanelHitBox( 17, 9, 3, 5, BoundColour )
      elseif ( hitnum .eq. 2 ) then
	  if ( DispBound ) then
	    call PigSetTextColour( HitColor )
	    call PanelHit( 15, 4, 2, 'OFF', 3 )
	    DispBound = .FALSE.
	  else
	    if ( BoundFName(1:4) .ne. 'NONE' ) then
		  DispBound = .TRUE.
		  call PigSetTextColour( HitColor )
		  call PanelHit( 15, 4, 2, ' ON', 3 )
	    else
c                 force file name specification too...
		  hitnum = 4
	    endif
	  endif
      elseif ( hitnum .eq. 6 ) then
	  if ( ContColour .eq. MaxColour ) then
	    ContColour = backgr
	  else
	    ContColour = ContColour + 1
	  endif
	  call PanelHitBox( 17, 20, 6, 5, ContColour )
      elseif ( hitnum .eq. 5 ) then
	  if ( DispCont ) then
	    call PigSetTextColour( HitColor )
	    call PanelHit( 15, 15, 5, 'OFF', 3 )
	    DispCont = .FALSE.
	  else
	    if ( ContFName(1:4) .ne. 'NONE' ) then
	      DispCont = .TRUE.
		  call PigSetTextColour( HitColor )
		  call PanelHit( 15, 15, 5, ' ON', 3 )
	    else
		  hitnum = 7
	    endif
	  endif
      endif
      if(hitnum.eq.4) then
*             change name of boundary file
        prompt = 'Select Boundary File'
        template ='Digit File (*.dig),*.dig;Node File (*.nod),*.nod;'
     +         //'All Files (*.*),*.*'
	  if(PigGetOpenFileName(prompt, ans, template)) then
          anslen = len_trim( ans )
		DispBound = .TRUE.
		BoundFName = Fblank
		BoundFName = ans(1:anslen)
		call PigSetTextColour( HitColor )
		call PanelHit( 15, 4, 2, ' ON', 3 )
		Valid = .TRUE.
		call PanelHit( 1, 7, 4, BoundFName, 24 )
	  endif
      elseif ( hitnum .eq. 7 ) then
	  if(PigGetOpenFileName('Open Contour File', ans,
     +      'Digit File (*.dig),*.dig;Node File (*.nod),*.nod;'
     +       //'All Files (*.*),*.*')   ) then
		  anslen = len_trim( ans )
		  DispCont = .TRUE.
		  ContFName = ans(:anslen)
		  call PigSetTextColour( HitColor )
		  call PanelHit(15, 15, 5, ' ON', 3 )
		  call PanelHit( 1, 18, 7, ContFName, 24 )
	    endif
      endif
	
	END

C*--------------------------------------------------------------------------*
      SUBROUTINE ConfigContours_Init()
C
C Purpose: Toggles the attributes for the display of contour information.
C Givens : None
C Returns: None
C Effects: None
C Written: Steve Prestage and Daphne Connolly  May 1989
C Modified: Mar/91 (JDM), all segments removed.

c
      implicit none
c


      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'


C - LOCAL VARIABLES
      integer hitnum, anslen
      LOGICAL Accepted
      character*256 ans
      logical PigGetOpenFileName

C-------------------BEGIN------------------------------

C     - Create right hand panel.
      call InitRHPanel

C     - display all labels
      call PigSetTextColour( LabelColor )
      call PanelText( 9, 1, 'CONTOURS', 8 )
      call PanelText( 7, 2, 'CONFIGURATION', 13 )
      call PanelText( 1, 4, 'Display Flag:', 13 )
      call PanelText( 1, 6, 'Contours File:', 14 )
      call PanelText( 1, 9, 'Contours Colour:', 16 )

C     - display all changeable text
      call PigSetTextColour( HitColor )

C     - Create Filename option ( hit # 4 )
      call PanelHit( 1, 7, 4, ContFName, 24 )

C     - Create Display Flag option ( hit # 2 )
      if ( DispCont ) then
	 call PanelHit( 15, 4, 2, ' ON', 3 )
      else
	 call PanelHit( 15, 4, 2, 'OFF', 3 )
      endif

C     - Create Colour option ( hit # 3 )
      call PanelHitBox( 8, 10, 3, 5, ContColour )

C     - Create ACCEPT option ( hit # 1 )
C      call Panel Hit( 10, 16, 1, 'ACCEPT', 6 )

      call config_set_active(3)
      return

      entry ConfigContours_ehandler(hitnum)
      if ( hitnum .eq. 1 ) then
	    Accepted = .TRUE.
	    call config_set_active(-1)
	    call ClearRHPanel
      elseif ( hitnum .eq. 3 ) then
	    if ( ContColour .eq. MaxColour ) then
	      ContColour = backgr
	    else
	      ContColour = ContColour + 1
	    endif
	    call PanelHitBox( 8, 10, 3, 5, ContColour )
      elseif ( hitnum .eq. 2 ) then
	    if ( DispCont ) then
	      call PigSetTextColour( HitColor )
	      call PanelHit( 15, 4, 2, 'OFF', 3 )
	      DispCont = .FALSE.
	    else
	      if ( ContFName(1:4) .ne. 'NONE' ) then
		DispCont = .TRUE.
		call PigSetTextColour( HitColor )
		call PanelHit( 15, 4, 2, ' ON', 3 )
	      else
		hitnum = 4
	      endif
	    endif
      endif
      if ( hitnum .eq. 4 ) then
	    if(PigGetOpenFileName('Open Contour File', ans,
     +         'Digit File (*.dig),*.dig;Data File (*.dat),*.dat;'
     +         //'All Files (*.*),*.*')
     +        ) then
		  anslen = len_trim( ans )
		  DispCont = .TRUE.
		  ContFName = ans(:anslen)
		  call PigSetTextColour( HitColor )
		  call PanelHit(15, 4, 2, ' ON', 3 )
		  call PanelHit( 1, 7, 4, ContFName, 24 )
	    endif
      endif
      END

C*--------------------------------------------------------------------------*

      SUBROUTINE ConfigXhr(range)

C Purpose: Allow user to change the cursor sensitivity RANGE
C Givens : None
C Returns: None
C Effects: Range is updated only if a value was entered > 0.0 .

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


C*--------------------------------------------------------------------------*

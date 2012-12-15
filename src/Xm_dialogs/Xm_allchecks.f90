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
!     This module contains the dialogs associated with the                  *
!     quality tests for elements.                                           *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
      
      SUBROUTINE FlagsTriangles_Init(change)

! Purpose: To set up dialog for FlagsTriangles routine.
! Givens : None
! Returns: None
! Effects: Dialog is set up with options to be used by FlagsTriangles routine.

      use MainArrays

      implicit none

!     - PASSED PARAMETERS
      LOGICAL change

!     - LOCAL VARIABLES
      integer hitnum
      character*1 ans
      LOGICAL cmode
      LOGICAL :: retro=.false.

!---------- BEGIN --------------

!     - Load triangle lists
      call PigPutMessage('Forming triangle list-please wait')      

      if(change) then
        call RemoveNotExist(itot,code,nbtot,nl)
        call Element_Lister(CHANGE,retro,itot,nbtot,dxray,dyray,depth,&
             nl,TotTr,ListTr,Tcode,x0off,y0off,scaleX,scaleY,igridtype)
        change = .false.
      endif
      call PigEraseMessage

      call PigMessageYesNo ('Full colour (or symbols)? ',ans)
      if(ans(1:1).eq.'Y') then
        cmode = .true.
      else
        cmode = .false.
      endif

!     tests: 1=eql, 2=dep, 3=a2d, 4=ccw, 5=g90, 6=code
      call PigPrompt('Enter test number (1-5): ', ans )
      read(ans,'(i1)') hitnum

      call ElementCheck(hitnum,cmode)

      END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!     This module contains the dialogs associated with the              *
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
      INCLUDE '../plotsubs/critcom.inc'
      INCLUDE '../includes/graf.def'
!      INCLUDE 'critcol.def'

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


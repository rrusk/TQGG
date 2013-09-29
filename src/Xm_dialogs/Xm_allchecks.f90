  !***********************************************************************
  !    Copyright (C) 1995-
  !        Roy A. Walters, R. Falconer Henry
  !
  !        TQGridGen@gmail.com
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
!     This module contains the dialogs associated with the              *
!     criteria tests for vertices.                                          *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE SetUserValue(ntest)

! Purpose : To place markers at vertices if certain
!           criteria are satisfied
! Given   : None
! Returns : Ok - TRUE if a redraw is needed

      implicit none

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../plotsubs/critcom.inc'
      INCLUDE '../includes/graf.def'

! critcom.inc:  array TheCriteria(1->MaxCrit) contents
! [1] CO [2] C1 [3] C2 [4] C3 [5] C4 [6] C5 [7] C6 [8] NC0
! [9] DLT [10] DGT [11] DBTW [12] NBGT [13] NBLT [14] NBE [15] EXT

! - PASSED VARIABLES
      integer ntest

! - LOCAL VARIABLES
      real rval
      logical Success
      character*80 ans

!----------BEGIN----------------------

      if(ntest.eq.9) then      !DLT
        call PigPrompt('Enter depth for comparison:',ans )
        call PigReadReal( ans, rval, Success )
        IF ( .NOT. Success ) THEN
          call PigMessageOK('Invalid number','ncheck')
        else
          USER_COUNT=rval
        endif
        
      elseif(ntest.eq.10) then !DGT
        call PigPrompt('Enter depth for comparison:',ans )
        call PigReadReal( ans, rval, Success )
        IF ( .NOT. Success ) THEN
          call PigMessageOK('Invalid number','ncheck')
        else
          USER_COUNT1=rval
        endif
      
      elseif(ntest.eq.11) then !DBTW
        call PigPrompt('Enter lower depth for comparison:',ans )
        call PigReadReal( ans, rval, Success )
        IF ( .NOT. Success ) THEN
          call PigMessageOK('Invalid number','ncheck')
        else
          lowerd=rval
          call PigPrompt('Enter upper depth for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid number','ncheck')
          else
            upperd=rval
          endif
        endif
      
      elseif(ntest.eq.12) then !NGT
        call PigPrompt('Enter integer count for comparison:',ans )
        call PigReadReal( ans, rval, Success )
        IF ( .NOT. Success ) THEN
          call PigMessageOK('Invalid integer','ncheck')
        else
          USER_NCOUNT = nint(rval)
        endif
      
      elseif(ntest.eq.13) then !NLT
        call PigPrompt('Enter integer count for comparison:',ans )
        call PigReadReal( ans, rval, Success )
        IF ( .NOT. Success ) THEN
          call PigMessageOK('Invalid integer','ncheck')
        else
          USER_NCOUNT1 = nint(rval)
        endif
      
      elseif(ntest.eq.14) then !NBE
        call PigPrompt('Enter integer count for comparison:',ans )
        call PigReadReal( ans, rval, Success )
        IF ( .NOT. Success ) THEN
          call PigMessageOK('Invalid integer','ncheck')
        else
          USER_NCOUNT2 = nint(rval)
        endif
      
!      elseif(ntest.eq.15) then !EXT
      
      elseif(ntest.eq.16) then !C=?
        call PigPrompt('Enter integer code:',ans )
        call PigReadReal( ans, rval, Success )
        IF ( .NOT. Success ) THEN
          call PigMessageOK('Invalid integer','ncheck')
        else
          USER_CODE = nint(rval)
        endif
      
      endif

      return
      END

!---------------------------------------------------------------------------*


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

      SUBROUTINE SetUserValue(ntest,check)

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
      integer :: ntest_local
      real rval
      logical Success,check
      character*80 ans

!----------BEGIN----------------------

      if(ntest.ge.1.and.ntest.le.7) then  !codes
        TheCriteria(ntest) = check

      elseif(ntest.eq.11) then      !NCO
        ntest_local = 8
        TheCriteria(ntest_local) = check
      
      elseif(ntest.eq.12) then !C=?
        ntest_local = 16
        TheCriteria(ntest_local) = check
        if(check) then
          call PigPrompt('Enter integer code:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_CODE = nint(rval)
          endif
        endif

      elseif(ntest.eq.13) then  !DLT
        ntest_local = 9
        TheCriteria(ntest_local) = check
        if(check) then
          call PigPrompt('Enter depth for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid number','ncheck')
          else
            USER_COUNT=rval
          endif
        endif
        
      elseif(ntest.eq.14) then !DGT
        ntest_local = 10
        TheCriteria(ntest_local) = check
        if(check) then
          call PigPrompt('Enter depth for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid number','ncheck')
          else
            USER_COUNT1=rval
          endif
        endif
      
      elseif(ntest.eq.15) then !DBTW
        ntest_local = 11
        TheCriteria(ntest_local) = check
        if(check) then
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
        endif
      
      elseif(ntest.eq.16) then !NLT
        ntest_local = 13
        TheCriteria(ntest_local) = check
        if(check) then
          call PigPrompt('Enter integer count for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_NCOUNT1 = nint(rval)
          endif
        endif
      
      elseif(ntest.eq.17) then !NGT
        ntest_local = 12
        TheCriteria(ntest_local) = check
        if(check) then
          call PigPrompt('Enter integer count for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_NCOUNT = nint(rval)
          endif
        endif
      
      elseif(ntest.eq.18) then !NBE
        ntest_local = 14
        TheCriteria(ntest_local) = check
        if(check) then
          call PigPrompt('Enter integer count for comparison:',ans )
          call PigReadReal( ans, rval, Success )
          IF ( .NOT. Success ) THEN
            call PigMessageOK('Invalid integer','ncheck')
          else
            USER_NCOUNT2 = nint(rval)
          endif
        endif
      
!      elseif(ntest.eq.15) then !EXT
      
      
      endif

      return
      END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

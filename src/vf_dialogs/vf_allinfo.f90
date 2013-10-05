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

!*--------------------------------------------------------------------------*
!                           TRIINFO.FOR                                     *
!     This module controls the right hand display panel for the displaying  *
!     of information about triangles and allows the user to change values   *
!     for triangle codes, aka material types.                               *
!*--------------------------------------------------------------------------*
!*--------------------------------------------------------------------------*

!*--------------------------------------------------------------------------*
      
      SUBROUTINE InfoFiles

! Purpose : Displays the default filenames currently in use during the
!           current interactive session.
! Givens  : None
! Returns : None
! Effects : None

      INCLUDE '../includes/defaults.inc'

      character*256 cstr
      integer len1, len2, len3, len4

!-----------BEGIN------------------

      if(DispNodes) then
        len1 = len_trim(NodeRName)
      else
        len1 = len_trim(GridRName)
      endif
      len2 = len_trim(LastInterim)
      len3 = len_trim(ContFName)
      len4 = len_trim(BoundFName)

      if(DispNodes) then
        cstr = 'Node File: '// NodeRName(:len1)//char(13)//&
               'Last Interim Node File: '// LastInterim(:len2)//char(13)//&
               'Contours File: '// ContFName(:len3)//char(13)//&
               'Boundary File: '// BoundFName(:len4)//char(0)
      else
        cstr = 'Grid File: '// GridRName(:len1)//char(13)//&
               'Last Interim Grid File: '// LastInterim(:len2)//char(13)//&
               'Contours File: '// ContFName(:len3)//char(13)//&
               'Boundary File: '// BoundFName(:len4)//char(0)
      endif

      call PigMessageOK(cstr, 'FILES')

      END

!-----------------------------------------------------------------------*
!                       END INFO.FOR                                    *
!-----------------------------------------------------------------------*

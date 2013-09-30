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
C Given   : none
C Returns : None.

C--------BEGIN--------------
C       - initialize if needed
      
      return
	END

C*-------------------------------------------------------------------------*

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
      else if ( code(index).ge.0 ) then
	  call PutMarker( DXRAY(index), DYRAY(index), 4, yellow)
	  call Put_Val( index, mmode )
      endif
      END

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

      integer last_val_node
      common /val_node/ last_val_node

C---------BEGIN------------------
C	- save node number for refresh operations
        last_val_node = index
        
      call InitNodeInfo(index)
      
      return
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
!*--------------------------------------------------------------------------*

      SUBROUTINE GetNodeInfo( index,xc,yc,zc,ec,numngh,nv )
!
! Purpose : To get the values for the info dialog for a
!           specified vertex point
! Given   : index = index to data arrays of point.

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer, intent(in) :: index
      integer, intent(out) :: ec,numngh,nv(nbtotr)
      real, intent(out) :: xc,yc,zc


! - LOCAL VARIABLES
      integer i

!---------BEGIN------------------

      xc = dxray(index)
      yc = dyray(index)
      zc = depth(index)
      ec = Code(index)
      numngh = nbtotr
      do i = 1,numngh
        nv(i) = NL(i,index)
      end do

      END

!*--------------------------------------------------------------------------*

      SUBROUTINE SetNodeInfo( index,ec,zc )
!
! Purpose : To get the values for the info dialog for a
!           specified vertex point
! Given   : index = index to data arrays of point.

      use MainArrays

      implicit none

! - PASSED VARIABLES
      integer, intent(in) :: index,ec
      real, intent(in) :: zc

!---------BEGIN------------------

      depth(index) = zc
      Code(index) = ec

      END

!-----------------------------------------------------------------------*
C-----------------------------------------------------------------------*
C                       END INFO.FOR                                    *
C-----------------------------------------------------------------------*
C-----------------------------------------------------------------------*

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

!-----------------------------------------------------------------------*
      SUBROUTINE About(RevisionP)
!      SUBROUTINE About(ProgramP, RevisionP, Nrec, Nbtotr, NNB)
! Purpose: Write the version number and opening title for TRIGRID.
! Givens : ProgramP - character*(*) program name
!          RevisionP - character*(*) program revision number
!          Nrec - number of nodes used
!          Nbtotr - number of neighbours used
!          NNB - number of boundaries used
! Returns: None
! Effects: Window displays Title and version number. 
!----------------------------------------------------------------

	USE DFLIB

!     - INCLUDES
!      INCLUDE '..\includes\graf.def'

	character*(*) RevisionP
!	character*(*) ProgramP
	integer tmplen
      character*(30) Revision
!      character*(30) Program
!      character*(30) Platform

!-----------------START ROUTINE------------------------------------

!      Program = ProgramP
!      RevisionP = '$Revision: 2.11.5 $' !RevisionP
      Revision = RevisionP

! display program identifications

!      call PigSetTextColour (TitleColor)
!      tmplen = len_trim(Program)
!      call PigSetTextColour (foregr)
! remove the $ signs from the RCS Revision string above
!      Revision = '$Revision: 12.7 $'  !RevisionP
      Revision = Revision(2:)
      Revision(len_trim(Revision):) = ' '
      tmplen = len_trim(Revision)

      tmplen1 = aboutboxqq(&
       'TQGG  '//char(13)//&
        Revision(:tmplen)//char(13)//&
       'Triangle and Quadrilateral Grid Generation and Editing,'//char(13)//&
       'Developed from programs GridGen and Trigrid by Roy Walters.'//char(13)//&
       'Base programs developed by Roy Walters, R.F. Henry, and others.'//char(13)//&
       'Roy A. Walters'//char(13)//&
       'rwalters@shaw.ca'//char(13)//&       
       'R F. Henry'//char(13)//&
       'rfhenry@shaw.ca'C)

      return

      END

!-----------------------------------------------------------------------*

       Subroutine Limits(nrec,mrec,nbtot,maxnnb)

	USE DFLIB

      integer tmplen,tmplen1,tmplen2
      integer nrec,mrec,nbtot,maxnnb
      character*80 msg,msg1,msg2

      write(msg,'(a,i7,a,i7)') 'Nodes used/allocated=',nrec,'/',MREC
      tmplen = len_trim(msg)
      write(msg1,'(a,i3)') 'Max Neighbours:', NBTOT
      tmplen1 = len_trim(msg1)
      write(msg2,'(a,i5)') 'Max Boundaries:', MAXNNB
      tmplen2 = len_trim(msg2)
      msg2 = msg2(:tmplen2)//char(0)

      tmplen = messageboxqq(msg(:tmplen)//char(13)//msg1(:tmplen1)//char(13)//msg2,'Limits'C,MB$OK)

      return
      end
!*--------------------------------------------------------------------------*

      SUBROUTINE GridGenHelp

!  Purpose : The purpose of this routine is to display a help file
!            in a child window
!  Given   : None.
!  Returns : None.
!  Effect  : A help file is opened and the data read and displayed

      USE DFLIB

      integer*2 result

!----------BEGIN--------------

      result = RUNQQ( 'winhlp32','C:\TQGG\doc\gridgen.hlp' )

      END
!-----------------------------------------------------------------------*

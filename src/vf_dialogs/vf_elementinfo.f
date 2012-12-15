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
C                           TRIINFO.FOR                                     *
C     This module controls the right hand display panel for the displaying  *
C     of information about triangles and allows the user to change values   *
C     for triangle types, aka material types.                               *
C*--------------------------------------------------------------------------*
C*--------------------------------------------------------------------------*
      SUBROUTINE Init_TriInfo
C
C Purpose : To initialize the area to the right of the grid which
C           is the INFO screen.
C Returns : None.

      INCLUDE '../includes/graf.def'

C     - PASSED VARIABLES

C--------BEGIN--------------

C       - initialize righthand panel
	call InitRHPanel

C       - Display Info Screen
	call Up_TriInfo

	call PigSetTextColour( LabelColor )

	call PanelText( 3, 2, 'ELEMENT INFORMATION', 20)

	call PigPutMessage('Choose element in main window...')
	END

C*-------------------------------------------------------------------------*
      SUBROUTINE Up_TriInfo
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
	call PanelText( 2, 4, 'INDEX:', 6 )
	call PanelText( 2, 5, 'XCENT:', 6 )
	call PanelText( 2, 6, 'YCENT:', 6 )
	call PanelText( 2, 7, 'DEPTH:', 6 )
	call PanelText( 2, 8, ' TYPE:', 6 )

	call PanelText( 5, 10, 'NODE LIST', 9 )
	call PanelTextRight( 5, 11, '1:', 2 )
	call PanelTextRight( 5, 12, '2:', 2 )
	call PanelTextRight( 5, 13, '3:', 2 )
	call PanelTextRight( 5, 14, '4:', 2 )

C       - create options
	call PigSetTextColour( HitColor )

C       - CURSOR option (1)
	call PanelHit( 10, 22, 1, 'QUIT', 4 )

C       - coord option (3)
	call PanelHit( 10, 20, 4, 'Coord', 5 )

C       - INDEX option (3)
	call PanelHit( 10, 19, 2, 'INDEX', 5 )

	END

C*--------------------------------------------------------------------------*
      SUBROUTINE ChgTVal( changev, index, field )
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

      use MainArrays

      INCLUDE 'ipig.def'

C - PASSED VARIABLES
      LOGICAL changev
      integer index, field
C
!      REAL    MININ,MAXIN,SCUNIT
!      COMMON  /MAXMIN/ MININ,MAXIN,SCUNIT

      integer  MVNDX
      COMMON   /PICK/ MVNDX

C - LOCAL VARIABLES
      CHARACTER*80 ans
      integer icode
C          - temporarily holds a new code entered by the user
      integer prev_justify
c      LOGICAL tnear, trans
      LOGICAL Success
C          -  marking proximity to neighbour
c      integer new_index
C          -  index of near neighbour (dummy)
      integer anslen, prevcode
      CHARACTER*6 num
C-----------------BEGIN------------------------

      MVNDX = index
      if ( field .eq. 3 ) then
C       - change the CODE field
	prevcode = TCODE(index)
	Success = .FALSE.
	do while ( .NOT. Success )
10        continue
	  call PigPrompt('Enter a Non-Negative code for this'//
     +                ' triangle:',ans)
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
        TCODE(index) = icode
c
        call PigGetJustification(prev_justify)
        call PigSetJustification(RIGHT_JUSTIFY)
c
        call PigSetTextColour( HitColor )
C       - code field
        WRITE( num, '(I6)' ) TCODE(index)
        call PanelHit( 20, 8, 3, num, 4 )
       call PigSetJustification(prev_JUSTIFY)
c
C       - set the change variable
        changev = .TRUE.

      endif
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE GetTVal_CW_ehandler( index,mmode, Hitnum)

C Purpose : To get all the information for a triangle in the grid
C Given   : nrec = the number of points read in from the data file
C Returns : index = index of the point we will be examining,
C           changev = logical set TRUE if any changes take place
C                     to the data that is to be written back
C                     to the files.

      use MainArrays

      INCLUDE '../includes/graf.def'

C - PASSED VARIABLES
      integer index
      CHARACTER*3 mmode
      LOGICAL changev

      REAL     CWXL,CWXH,CWYL,CWYH
      COMMON  /CURWIN/ CWXL,CWXH,CWYL,CWYH

!      REAL    MININ,MAXIN,SCUNIT
!      COMMON  /MAXMIN/ MININ,MAXIN,SCUNIT

C - LOCAL VARIABLES
        LOGICAL Done,success
C        logical okpoint
c        REAL xpos, ypos
        REAL xcent, ycent
        integer hitnum, i
	CHARACTER*80 ans
C--------------BEGIN-----------------
        changev = .FALSE.
        Done = .FALSE.
        if ( hitnum .eq. 1 ) then
C         - 'QUIT'
	  INDEX = 0
	  mmode = 'QIT'
	  Done = .TRUE.
	elseif ( hitnum .eq. 2 ) then
C         - 'INDEX'
7         continue
	  call PigPrompt('Enter index:', ans )
	  READ( ans, FMT = '(I6)', ERR = 7 ) INDEX
C         - see if point chosen is legal
	  if ( (INDEX .lt. 1) .or. (INDEX .gt. tottr) ) then
	    call PigPutMessage('ERROR - Invalid point..')
	  else
            xcent = 0.
            ycent = 0.
            if(ListTr(4,index).gt.0) then
              numcn = 4
            else
              numcn = 3
            endif
            do i=1,numcn
              xcent = xcent + dxray(ListTr(i,index))/float(numcn)
              ycent = ycent + dyray(ListTr(i,index))/float(numcn)
            enddo
c
	    call PigSetWindowNum( MAINWIN )
	    call PutMarker( xcent, ycent, 4, yellow )
	    call PigSetWindowNum( CONTROLWIN )
	    call Put_TriVal( index )
C             - check for a CHANGE option hit
          endif
	elseif ( hitnum .eq. 3 ) then
C              - CHANGE CODE
	  call ChgTVal( changev, index, 3 )
	elseif ( hitnum .eq. 4 ) then
71         continue
	  call PigPrompt('Enter x coordinate:', ans )
	  READ( ans, *, ERR = 71 ) xcent
72         continue
	  call PigPrompt('Enter y coordinate:', ans )
	  READ( ans, *, ERR = 72 ) ycent
	  call PigSetWindowNum( MAINWIN )
!	  call PutMarker( xcent, ycent, 4, yellow )
        call PutPermMarker( xcent, ycent, Success )
        call GetTVal_MW_Ehandler(Xcent, Ycent, Index)
	  call PigSetWindowNum( CONTROLWIN )

	else
C           - an invalid point was selected
c            okpoint = .FALSE.
	endif
c
       if(Done) then
	  call ClearRHPanel
       endif
c
      END

C*--------------------------------------------------------------------------*

      SUBROUTINE GetTVal_MW_Ehandler(Xinp, Yinp, Index)

C Purpose : To get all the information for a triangle in the grid
C Given   : nrec = the number of points read in from the data file
C           Xinp = x position to locate node
C           Yinp = y position to locate node
C Returns : index = index of the triangle

      use MainArrays

      INCLUDE '../includes/graf.def'
c
C - PASSED VARIABLES
      real xinp, yinp
      integer index
!      integer nrec
!      CHARACTER*3 mmode
C
C *** LOCAL VARIABLES ***
C
      integer j
      real xm0, xm1, ym0, ym1
      real x0, x1, x2, x3, y0, y1, y2, y3
      real delta, da1, da2, da3, da4

C     - get the point
c
        do j=1,TotTr
c
          xm0 = min(dxray(ListTr(1,j)),dxray(ListTr(2,j)))
          xm0 = min(xm0,dxray(ListTr(3,j)))
          xm1 = max(dxray(ListTr(1,j)),dxray(ListTr(2,j)))
          xm1 = max(xm1,dxray(ListTr(3,j)))
          ym0 = min(dyray(ListTr(1,j)),dyray(ListTr(2,j)))
          ym0 = min(ym0,dyray(ListTr(3,j)))
          ym1 = max(dyray(ListTr(1,j)),dyray(ListTr(2,j)))
          ym1 = max(ym1,dyray(ListTr(3,j)))
c
          if(xinp.gt.xm0.and.xinp.lt.xm1.and.
     +       yinp.gt.ym0.and.yinp.lt.ym1) then
c
            x0 = xinp
            y0 = yinp
            X1 = DXRAY(ListTr(1,j))
            X2 = DXRAY(ListTr(2,j))
            X3 = DXRAY(ListTr(3,j))
            Y1 = DYRAY(ListTr(1,j))
            Y2 = DYRAY(ListTr(2,j))
            Y3 = DYRAY(ListTr(3,j))
C
C CHECK TO SEE IF POINT IS INSIDE TRIANGLE
C
C TO DETERMINE IF POINT LIES INSIDE THE TRIANGLE,
C CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE
C
            DA1=ABS((X1-X0)*(Y2-Y0)
     *             -(X2-X0)*(Y1-Y0))
            DA2=ABS((X2-X0)*(Y3-Y0)
     *             -(X3-X0)*(Y2-Y0))
            DA3=ABS((X3-X0)*(Y1-Y0)
     *             -(X1-X0)*(Y3-Y0))
            DA4=ABS((X2-X1)*(Y3-Y1)
     *             -(X3-X1)*(Y2-Y1))
c           INSIDE = .FALSE.
            IF (DA4.GT.0.) THEN
              DELTA=DA1+DA2+DA3-DA4
              IF (DELTA.EQ.0..OR.
     *           (DELTA.NE.0..AND.DELTA.LE.DA4*1.E-6)) THEN
c               INSIDE=.TRUE.
                index = j
		call PutTriMarker(index)
                call Put_TriVal( index )
                go to 999
              endif
            endif
          endif
        enddo
999   continue
      END

C*--------------------------------------------------------------------------*
	SUBROUTINE PutTriMarker(index)

      use MainArrays

      integer index

      INCLUDE '../includes/graf.def'

	real xcent, ycent

	xcent = 0.
	ycent = 0.
        if(ListTr(4,index).gt.0) then
          numcn = 4
        else
          numcn = 3
        endif
        do i=1,numcn
          xcent = xcent + dxray(ListTr(i,index))/float(numcn)
          ycent = ycent + dyray(ListTr(i,index))/float(numcn)
        enddo
	call PutMarker( xcent, ycent, 4, yellow )
	end

C*--------------------------------------------------------------------------*
      SUBROUTINE Repaint_TriInfo
C
C Purpose : To repaint put the values on the info screen for a
C           specified triangle point
C Given   : last_triangle_index in common

      use MainArrays, only : maxtri

	integer	last_triangle_index
	common	/last_triangle/ last_triangle_index

	call Init_TriInfo

	if	(	(last_triangle_index .gt. 0)
     +		.and.	(last_triangle_index .le. MAXTRI)
     +		) then
		call PutTriMarker(last_triangle_index)
		call Put_TriVal(last_triangle_index)
	endif
	end

C*--------------------------------------------------------------------------*
      SUBROUTINE Put_TriVal( index )
C
C Purpose : To put the values on the info screen for a
C           specified vertex point
C Given   : index = index to data arrays of point.

      use MainArrays

      INCLUDE 'ipig.def'

C - PASSED VARIABLES
      integer index
C
!      REAL    MININ,MAXIN,SCUNIT
!      COMMON  /MAXMIN/ MININ,MAXIN,SCUNIT

	integer	last_triangle_index
	common	/last_triangle/ last_triangle_index
C - LOCAL VARIABLES
      real xcent,ycent,dcent
      integer i, prev_justify
      CHARACTER num*6, rnum*12

C---------BEGIN------------------

        last_triangle_index = index

C     - set text color according to mode
       call PigGetJustification(prev_justify)
       call PigSetJustification(RIGHT_JUSTIFY)
c
        call PigSetTextColour( HitColor )
C       - code field
        WRITE( num, '(I6)' ) TCODE(index)
        call PanelHit( 20, 8, 3, num, 4 )
        call PigSetJustification(LEFT_JUSTIFY)
c
        xcent = 0.
        ycent = 0.
        dcent = 0.
        if(ListTr(4,index).gt.0) then
          numcn = 4
        else
          numcn = 3
        endif
        do i=1,numcn
          xcent = xcent + dxray(ListTr(i,index))/float(numcn)
          ycent = ycent + dyray(ListTr(i,index))/float(numcn)
          dcent = dcent + depth(ListTr(i,index))/float(numcn)
        enddo

	call PigSetTextColour( NoHitColor )
C       - x field
	WRITE( rnum, '(f12.3)' ) xcent
	call PanelTextRight( 20, 5, rnum, 12 )
C       - y field
	WRITE( rnum, '(f12.3)' ) ycent
	call PanelTextRight( 20, 6, rnum, 12 )
C       - depth field
	WRITE( rnum, '(f12.3)' ) Dcent
	call PanelTextRight( 20, 7, rnum, 12 )

C     - set color for non-selectable fields
      call PigSetTextColour( NoHitColor )

C     - index field
      WRITE( num, '(I6)' ) index
      call PanelTextRight( 20, 4, num, 6 )

      do i = 1,4
	write( num, '(I6)' ) ListTr(i,index)
        call PanelTextRight( 14, (10+i), num, 6)
      end do

      call PigSetJustification(prev_justify)
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE InfoTriangle(CHANGE)
C
C Purpose: Future implementation to display triangle info in the right hand
C          display window.
C Givens : CHANGE
C Returns: CW setup
C Effects: Updates triangle list if CHANGE=TRUE

      use MainArrays

        logical CHANGE
c
C Update triangle list beforehand.

!        if(CHANGE) then
!          call PigPutMessage('Forming triangle list')
!          call LdTrLt(CHANGE)
!          call PigEraseMessage
!        endif
        if(change) then
          call RemoveNotExist(itot,code,nbtot,nl)
          call Element_Lister(CHANGE, .FALSE. ,
     &          itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode,
     &          x0off,y0off,scaleX,scaleY,igridtype)
          change = .false.
        endif

        call INIT_TriInfo

	END

C-----------------------------------------------------------------------*
C                       END INFO.FOR                                    *
C-----------------------------------------------------------------------*
C-----------------------------------------------------------------------*

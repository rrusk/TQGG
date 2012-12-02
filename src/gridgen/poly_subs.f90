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
!                       EDPOLY.FOR                                      *
!       This module contains procedures for defining a subset of the    *
!       grid data with a polygon. Points in the polygon may be          *
!       manipulated.                                                    *
!-----------------------------------------------------------------------*
!       ROUTINES:                                                       *
!         DefPoly, DeletePoly, CyclePoly, AllPolys, ActivatePoly,       *
!         DisplayPoly, PolyLimits, WholePoly, SaveWhole,                *
!         ReadEpoly, SaveEPoly                                          *
!-----------------------------------------------------------------------*

      SUBROUTINE DefPoly2 ( FirstPoint, NextPoint, xpos, ypos, success )

! PURPOSE: To let the user map out a polygon. This polygon will be the
!          active area for node manipulation. The only restriction is
!          that the polygon can have no more than 'maxvert' sides.
!   INPUT: Interactive input from user,
!   GIVEN: Previously existing polygons defined in Common POLYDEFS.
! RETURNS: success = TRUE if user defines and confirms a polygon,
!                    else FALSE
!          In Common POLYDEFS;
!               actvpoly = polygon defined, if success, else no change.
! EFFECTS: Polygon is drawn as created, and saved in POLYDEFS. User can choose 
!          cursor, keyboard entry, or mixed (prompt at each vertex for cursor 
!          or keyboard) as method of defining a polygon. If entering the 4th 
!          or greater vertex by keyboard entry, then prompt for coordinate
!          includes '<C>lose' to automatically join up with the 1st vertex and 
!          close the polygon.
! BUG: Mar94 . It is possible to create more polygons than MAXPOLYS,
!          or at least for some of the arrays declared in size as MAXPOLYS
!          to be accessed outide their bounds.
!-----------------------------------------------------------------------*

! - "INCLUDES"
       include '../includes/defaults.inc'
       include '../includes/edpolys.inc'

! - COMMON BLOCKS
!   - PolyDisplay indicates polygons to display on redraw.
!       PolyDisplay = 0 = display active polygon only.
!                   = 1 = display all polygons.
!                   = 2 = display NO polygons.
       INTEGER PolyDisplay
       COMMON /POLYSTATUS/ PolyDisplay

       REAL cwxl, cwxh, cwyl, cwyh
       COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh

!       - display array needs 1 extra point to connect 1st & last points
       REAL vx(maxvert+1), vy(maxvert+1)
       COMMON /VLOCAL/ vx, vy

! - PASSED VARIABLES
       LOGICAL success, FirstPoint, NextPoint
       REAL xpos, ypos

! - LOCAL VARIABLES
       integer, parameter :: ii1=1,i2=2
       INTEGER, save :: i
       integer j,jj, tmpactv
       CHARACTER cstr*80, ans*1
       CHARACTER*3 char_nump, char_scrnp
       LOGICAL polydone
       REAL v1xmin, v1xmax, v1ymin, v1ymax, prange
!          to define range box around 1st polyon vertex to close polygon
       INTEGER InpType
!       - InpType is method chosen to define polygon: 
!               1 = cursor, 2 = keyboard, 3 = mixed
!       - InpChoice, if InpType = 3, then choice of method for specifying
!                    current vertex: 1 = cursor, 2 = keyboard

!------------------START ROUTINE----------------------------------------

       if(numpolys .ge. MAXPOLYS) then
         cstr='Too many polygons already defined. Please delete some.'
         call PigMessageOK( cstr, 'defpoly' )
         return
       endif

!       ENDIF
!         - ( PolyDisplay < 2 )

!       - get method of defining polygon
       InpType = 1
       ans(1:1) = 'C'
       success = .FALSE.
       polydone = .FALSE.

!       - start new polygon
 !      call PigSetLineColour ( yellow )
       if(FirstPoint) then
         i = 1
         vx(i) = xpos
         vy(i) = ypos
         call PigDrawModifySymbol (xpos, ypos)
         FirstPoint = .false.
         NextPoint = .true.
!       - store currently active polygon, if any
         tmpactv = 0
         IF ( actvpoly .gt. 0 ) THEN
!           - there is an active polygon now, draw it in red
           tmpactv = actvpoly
           actvpoly = 0
           call DisplayPoly ( tmpactv )
         ENDIF
!           - ( actvpoly > 0 )
         return
         
       elseif(NextPoint) then
         i = i + 1 
         vx(i) = xpos
         vy(i) = ypos
         prange = 0.1*sqrt((xpos-vx(i-1))**2 + (ypos-vy(i-1))**2)
         call PigDrawModifySymbol (xpos, ypos)
!         - check if polygon is to be closed now
         v1xmin = vx(1) - prange
         v1xmax = vx(1) + prange
         v1ymin = vy(1) - prange
         v1ymax = vy(1) + prange
         i1=max0(1,i-1)
         IF (        (       (i .gt. 1)&
     &                    .AND.       (vx(i) .ge. v1xmin)&
     &                    .AND.       (vx(i) .le. v1xmax)&
     &                    .AND.       (vy(i) .ge. v1ymin)&
     &                    .AND.       (vy(i) .le. v1ymax)& 
     &                     )&
     &                .OR.       (       (i .gt. 2)&
     &                    .AND.       (vx(i) .eq. vx(i1))&
     &                    .AND.       (vy(i) .eq. vy(i1))&
     &                     )&
     &                .OR.  (i .eq. maxvert) )  THEN
!               - close polygon
           IF ( i .eq. maxvert ) THEN
!              - put marker and line for maxvert
             call PigDrawLine ( i, vx, vy, ii1 )
             i = i + 1
           ENDIF
!                 - ( i = maxvert )
!               - connect last to first
           vx(i) = vx(1)
           vy(i) = vy(1)
           call PigDrawLine ( i, vx, vy, ii1 )
           IF ( i .lt. 4 ) THEN
!             - reject polygon for too few vertices
             cstr = 'Invalid polygon, 3 vertices minimum.'
             call PigMessageOK ( cstr , 'polygon')
!             - remove from display
             call PigDrawLine ( i, vx, vy, i2 )
             i = 1
             polydone = .FALSE.
             FirstPoint = .true.
             NextPoint = .false.
             return
           ELSE
!             - enough vertices in polygon
             cstr ='Polygon OK ?:'
             call PigMessageYesNo(cstr, ans)
             IF ( ans(1:1) .eq. 'Y' ) THEN
!               - polygon OK
               polydone = .TRUE.
               success = .TRUE.
               FirstPoint = .false.
               NextPoint = .false.
             ELSE
!               - polygon rejected, remove from display
               call PigDrawLine ( i, vx, vy, i2 )
               do j=1,i
                  call PigEraseModifySymbol (vx(j), vy(j))
               enddo
               polydone = .TRUE.
               success = .FALSE.
               i = 1
               FirstPoint = .false.
               NextPoint = .false.
               return
             ENDIF
!               - ( ans = Y ) polygon OK
           ENDIF
!                 - ( i < 4 )
         ELSE
!               - not closing polygon, put a marker at location
!              call RedrawMark( vx(i), vy(i), yellow )
           IF ( i .gt. 1 ) THEN
!                 - more than 1 vertex, draw a line connecting to prev. vertex
             call PigDrawLine ( i, vx, vy, ii1 )
           ENDIF
!                 - ( i > 1 )
!              i = i + 1
         ENDIF
       endif
!               - ( closing polygon )
!           ELSE
!             - invalid point, not in current window
!             cstr = 'Invalid point, pick again.'
!             call PigPutMessage ( cstr )
!           ENDIF
!             - ( In_Box(xpos, ypos) )
!         ELSE
!           - error in choosing a vertex
!           cstr = 'Invalid point, pick again.'
!           call PigPutMessage ( cstr )
!         ENDIF
!           - ( ierr =  1 ) New Node
!       END DO
!         - ( NOT donepoly )

       IF ( success ) THEN
!         - a new polygon has been created
         numpolys = numpolys + 1
!         call PigDrawLine ( i, vx, vy, ii1 )
         vnum = i !- 1
         DO j = 1, vnum
           vertx(numpolys,j) = vx(j)
           verty(numpolys,j) = vy(j)
           call PigEraseModifySymbol (vx(j), vy(j))
         END DO
         vertcnt(numpolys) = vnum
         PolyCode(numpolys) = numpolys
         actvpoly = numpolys
!         scrnpoly = curpoly
!         curpoly = curpoly + 1
         do j=1,numpolys
           jj = j
           call DisplayPoly ( jj )
         enddo
         char_nump = '   '
         char_scrnp = '   '
         WRITE ( char_nump, '(I3)' ) numpolys
         WRITE ( char_scrnp, '(I3)' ) actvpoly
!          - put up message
         cstr = 'Polygon '//char_scrnp//' of '// char_nump // '.'
         call PigStatusMessage ( cstr )
       ELSEIF(polydone) then
!         - not success, restore original active poly if there was one
         IF ( tmpactv .ne. 0 ) THEN
!           - tmpactv was used to store previously active poly
           actvpoly = tmpactv
           call DisplayPoly ( actvpoly )
         ELSE
!           - there was no previously active poly
           actvpoly = 0
         ENDIF
!           - ( tmpactv ne 0 )
       ENDIF
!         - ( success )

       END

!----------------------------------------------------------------------*

      SUBROUTINE DeletePoly

! PURPOSE: To delete the currently active polygon.
!   GIVEN: Data in Common POLYDEFS.
! RETURNS: Updated data in Common POLYDEFS with one less polygon
! EFFECTS: The currently active polygon (actvpoly) is deleted, and there
!          is no new active polygon ( actvpoly = 0 ).
! WRITTEN: Jul92 - JDM - based on similar NODER routine.
!----------------------------------------------------------------------*

! - "INCLUDES"
      include '../includes/edpolys.inc'

! - LOCAL VARIABLES
      REAL vx(maxvert+1), vy(maxvert+1)
!         - display array needs 1 extra point to connect 1st & last points
      integer i, j

      CHARACTER cstr*80, ans*1
      CHARACTER*3 char_nump, char_scrnp

!----------------------START ROUTINE----------------------------------

      IF ( actvpoly .gt. 0 ) THEN
!       - now get confirmation to delete hilited polygon
        cstr = 'DELETE active Polygon ?:'
        call PigMessageYesNo (cstr, ans)
        IF ( ans(1:1) .eq. 'Y' ) THEN
!       - remove hilited poly
          i2 = 2
          DO j = 1, vertcnt(actvpoly)+1
            vx(j) = vertx(actvpoly,j)
            vy(j) = verty(actvpoly,j)
          END DO
          call PigDrawline ( vertcnt(actvpoly) + 1, vx, vy, i2 )
!         - Compress list of polygons
!         - shuffle down polys following poly being deleted by 1 position
          DO i = actvpoly, numpolys-1
            DO j = 1, vertcnt(i+1)
              vertx(i,j) = vertx(i+1,j)
              verty(i,j) = verty(i+1,j)
            END DO
            vertcnt(i) = vertcnt(i+1)
            PolyCode(i) = PolyCode(i+1)
          END DO
          vertcnt(numpolys) = 0
          PolyCode(numpolys) = 0
          numpolys = numpolys - 1
          actvpoly = numpolys
          IF ( numpolys.gt.0 ) then
            call DisplayPoly ( actvpoly )
          endif
          curpoly = curpoly - 1
        ELSE
!         - ans = N, do not delete this polygon, redraw active poly
          call DisplayPoly ( actvpoly )
        ENDIF
!         - ( ans = Y )
        char_nump = '   '
        char_scrnp = '   '
        WRITE ( char_nump, '(I3)' ) numpolys
        WRITE ( char_scrnp, '(I3)' ) actvpoly
!         - put up message
        cstr = 'Polygon '//char_scrnp//' of '// char_nump // '.'
        call PigStatusMessage ( cstr )
      ELSE
!       - no active polygon to delete
        cstr = 'Please Activate Polygon To Delete.'
        call PigMessageOK ( cstr, 'delpoly' )
      ENDIF
!       - ( actvpoly > 0 )

      END

!-----------------------------------------------------------------------*

      SUBROUTINE CyclePoly

! PURPOSE: To cycle through polygons.
!   GIVEN: None.
! RETURNS: In Common CYCLESTATUS:
!               showall,shownone = status of 1st two events in cycle
!          In Common POLYSTATUS:
!               PolyDisplay = indicates polygons to display on redraw
!          In Common POLYDEFS:
!               scrnpoly = currently displayed polygon
! EFFECTS: Display cycle of polygons is incremented by one each time
!          this procedure is called. Display cycle works as follow:
!               1) all polygons are displayed
!               2) no polygons are displayed
!               3) the first polygon is displayed
!               .
!               .
!               n) the nth polygon is displayed
!             n+1) cycle starts at 1) again
!----------------------------------------------------------------------*

! - "INCLUDES"
      include '../includes/edpolys.inc'

! - COMMON BLOCKS
!       - CYCLESTATUS stores indicators of when to show all or no polygons
!       -- during cycle
!      LOGICAL showall, shownone
!      COMMON /CYCLESTATUS/ showall, shownone

!   - PolyDisplay indicates polygons to display on redraw.
!       PolyDisplay = 0 = display active polygon only.
!                   = 1 = display all polygons.
!                   = 2 = display NO polygons.
      integer PolyDisplay
      COMMON /POLYSTATUS/ PolyDisplay

! - LOCAL VARIABLES

      integer  j, jj
      integer, parameter :: i2=2
!      REAL vx(maxvert+1), vy(maxvert+1)
!         - display array needs 1 extra point to connect 1st & last points
      CHARACTER cstr*80
      CHARACTER*3 char_nump, char_scrnp

!----------------------START ROUTINE----------------------------------

      IF ( numpolys .gt. 0 ) THEN
        IF ( numpolys .gt. 1 ) THEN
          actvpoly = mod(actvpoly,numpolys) + 1
          do j=1,numpolys
            jj = j
            call DisplayPoly ( jj )
          enddo
          call DisplayPoly ( actvpoly )
!             - get character values for numpolys & scrnpoly

            char_nump = '   '
            char_scrnp = '   '
            WRITE ( char_nump, '(I3)' ) numpolys
            WRITE ( char_scrnp, '(I3)' ) actvpoly
!             - put up message
            cstr = 'Polygon '//char_scrnp//' of '// char_nump // '.'
            call PigStatusMessage ( cstr )

        ELSE
!           - only one polygon is defined
!           - put up message
          cstr = 'Only One Polygon Currently Defined.'
          call PigPutMessage ( cstr )
        ENDIF
!           - ( numpolys > 0 )
      ELSE
!         - no polygons are defined
        cstr = 'No Polygons Currently Defined.'
        call PigPutMessage ( cstr )
      ENDIF
!         - ( numpolys > 0 )

      END

!----------------------------------------------------------------------*

       SUBROUTINE DisplayPoly ( polynum )

! PURPOSE: To display a specified polygon.
!   GIVEN: polynum = id number of polygon to display
! RETURNS: In Common POLYDEFS;
!               scrnpoly = polynum.
! EFFECTS: Specified polygon is displayed in red if inactive, in yellow
!          if active. If polynum = 0, then no polygon is displayed.
!----------------------------------------------------------------------*

! - "INCLUDES"
       include '../includes/edpolys.inc'

! - PASSED VARIABLES
       integer polynum

! - LOCAL VARIABLES
       integer j
       integer, parameter :: i1=1,i4=4

       REAL vx(maxvert+1), vy(maxvert+1)
!         - display array needs 1 extra point to connect 1st & last points

!----------------------START ROUTINE----------------------------------

       IF ( polynum .ne. 0 ) THEN
         DO j = 1, vertcnt(polynum)
           vx(j) = vertx(polynum,j)
           vy(j) = verty(polynum,j)
         END DO
         j = vertcnt(polynum) + 1
         vx(j) = vertx(polynum,1)
         vy(j) = verty(polynum,1)
         IF ( polynum .eq. actvpoly ) THEN
!           - active polygon color
           call PigDrawline ( j, vx, vy, i1 )  !yellow
         ELSE
!           - inactive polygon color
           call PigDrawline ( j, vx, vy, i4 )  !red
         ENDIF
!           - ( polynum = actvpoly )
       ENDIF
!         - ( polynum ne 0 )

       END

!-----------------------------------------------------------------------*

       SUBROUTINE PolyLimits ( xminp, xmaxp, yminp, ymaxp )

! PURPOSE: To determine the maximum & minimum limits of the active polygon.
!   GIVEN: Polygon data in file edpolys.INC. An active polygon must exist
!          prior to a call to this routine.
! RETURNS: xminp, xmaxp, xminy, xmaxy = the polygon maximum & minimum limits
! WRITTEN: Jul92 - JDM - based on similar NODER routine.
!----------------------------------------------------------------------*

! - "INCLUDES"
       include '../includes/edpolys.inc'

! - PASSED VARIABLES
       REAL xminp, xmaxp, yminp, ymaxp

! - LOCAL VARIABLES
       integer i

!------------------START ROUTINE------------------------------

!       - determine max, min coordinates for active polygon
       xminp = vertx(actvpoly,1)
       xmaxp = vertx(actvpoly,1)
       yminp = verty(actvpoly,1)
       ymaxp = verty(actvpoly,1)
       DO i = 2, vertcnt(actvpoly)
         IF ( vertx(actvpoly,i) .lt. xminp )  xminp = vertx(actvpoly,i)
         IF ( vertx(actvpoly,i) .gt. xmaxp )  xmaxp = vertx(actvpoly,i)
         IF ( verty(actvpoly,i) .lt. yminp )  yminp = verty(actvpoly,i)
         IF ( verty(actvpoly,i) .gt. ymaxp )  ymaxp = verty(actvpoly,i)
       END DO
!         - ( i = 1, vertcnt )

       END

!-----------------------------------------------------------------------*

       SUBROUTINE WholePoly( ok )

! PURPOSE: To define a polygon that just covers to whole
!          grid (node) area.
!   GIVEN: displayit = TRUE if created polygon is to be displayed
!                    = FALSE if polygon is to be created but not displayed.
!          In Common DATAMAX, max & min node coordinates.
!          In Common in edpolys.INC
!            numpolys = # polygons currently defined
!            maxpolys = max # polygons that may be defined 
! RETURNS: ok = TRUE if there is room for another polygon to be added to
!               polygon list, else FALSE
! EFFECTS: If there is room in the list for another polygon, it is drawn
!          but not added to the list.
!----------------------------------------------------------------------*

      use MainArrays

! - PASSED PARAMETERS
       LOGICAL ok

! - "INCLUDES"
       include '../includes/edpolys.inc'

! - COMMON BLOCKS
       REAL Dmaxx, Dmaxy, Dminx, Dminy
!       COMMON /DATAMAX/ Dmaxx, Dmaxy, Dminx, Dminy

! - LOCAL VARIABLES
       CHARACTER cstr*80
       CHARACTER*3 char_nump, char_scrnp
       REAL ptx(5), pty(5), dxy
       
       integer j,jj

!----------------START ROUTINE------------------------------------------

      Dminx = minval(dxray(1:itot))
      Dminy = minval(dyray(1:itot))
      Dmaxx = maxval(dxray(1:itot))
      Dmaxy = maxval(dyray(1:itot))
      dxy = 0.01*(Dmaxx - Dminx + Dmaxy - Dminy)

       IF ( numpolys .lt. maxpolys ) THEN
         ptx(1) = Dminx - dxy
         pty(1) = Dminy - dxy
         ptx(2) = Dminx - dxy
         pty(2) = Dmaxy + dxy
         ptx(3) = Dmaxx + dxy
         pty(3) = Dmaxy + dxy
         ptx(4) = Dmaxx + dxy
         pty(4) = Dminy - dxy
         ptx(5) = ptx(1)
         pty(5) = pty(1)

         numpolys = numpolys + 1
         vnum = 5
         DO j = 1, vnum
           vertx(numpolys,j) = ptx(j)
           verty(numpolys,j) = pty(j)
         END DO
         vertcnt(numpolys) = vnum
         PolyCode(numpolys) = numpolys
         actvpoly = numpolys
         curpoly = curpoly + 1
         do j=1,numpolys
           jj = j
           call DisplayPoly ( jj )
         enddo
         char_nump = '   '
         char_scrnp = '   '
         WRITE ( char_nump, '(I3)' ) numpolys
         WRITE ( char_scrnp, '(I3)' ) actvpoly
!          - put up message
         cstr = 'Polygon '//char_scrnp//' of '// char_nump // '.'
         call PigStatusMessage ( cstr )
         ok = .TRUE.
       ELSE
         call PigPutMessage('MaxPolygons already defined, delete one')
         ok = .FALSE.
       ENDIF

       END

!-----------------------------------------------------------------------*

       SUBROUTINE ReadEPoly ( success )

! PURPOSE: To read in a file of polygons, using file unit 8 as input
!          file unit.  
!   GIVEN: Interactive input from user.
! RETURNS: Data read into Common POLYDEFS.
!          success = .TRUE. if polygon(s) read , else .FALSE.
!          In Common IOVALUES ; IOvalue
!                               0 = OK.
!                               1 = user "quit" at filename.
!                               others = F77L3 specific.
! EFFECTS: Polygon(s) defined in user named file areread into
!          Common POLYDEFS.
! WRITTEN: June 1990 by JDM for NODER.
!-----------------------------------------------------------------------*
 
! - PARAMETERS (constants)
!       REAL*4 seconds
!       PARAMETER ( seconds = 2.0 )

! - PASSED VARIABLES
       LOGICAL success

! - COMMON AREA 
!   - IOVALUES is for I/O status storage
       integer IOvalue
       COMMON /IOVALUES/ IOvalue

! - "INCLUDES"
       include '../includes/edpolys.inc'

! - LOCAL VARIABLES
       integer fnlen, j,jj, i
       CHARACTER*80 cstr
       CHARACTER*80 Flename
       LOGICAL Exists, Quitting, named

!       integer PrevColour

       logical PigOpenFile

!----------------START ROUTINE------------------------------------------

!       call PigGetLineColour (PrevColour)

       success = .FALSE.
       named = .FALSE.
       Exists = .FALSE.
       Quitting = .FALSE.
       Flename = '                    '

       if(PigOpenFile(12,'Open Polygon File',flename,&
             'Polygon File (*.ply),*.ply;All Files (*.*),*.*;')&
             ) then
!             - file Exists
             fnlen = len_trim( Flename )
             named = .TRUE.
             Quitting = .FALSE.
      ELSE
           Quitting = .TRUE.
           named = .TRUE.
           success = .FALSE.
           IOvalue = 1
      ENDIF

       IF ( .NOT. Quitting ) THEN
!         - start reading...
         cstr = 'Reading File...[POLY] format.'
         call PigPutMessage ( cstr )
         i = numpolys + 1
         DO WHILE ( .TRUE. )
!    read ( 12, fmt = 90, err = 99, end = 89, iostat = IOvalue)
           read ( 12, *, err = 99, end = 89, iostat = IOvalue) vertcnt(i), PolyCode(i)
           DO j = 1, vertcnt(i)
!      read ( 12, fmt = 91, err = 99, end = 89,iostat = IOvalue)
             read ( 12, *, err = 99, end = 89,iostat = IOvalue) vertx(i,j), verty(i,j)
           END DO
!             - ( j = 1, vertcnt(i) )
           i = i + 1
         END DO
!           - ( TRUE )
89        continue
         numpolys = i
         actvpoly = numpolys-1
         success = .TRUE.
         CLOSE ( 12, status = 'KEEP', err = 99, iostat = IOvalue )
       ENDIF
!         - ( NOT Quitting )

       do j=1,numpolys
         jj = j
         call DisplayPoly ( jj )
       enddo

       GOTO 100
!       - line 99 is error trap for I/O failure
99     call PigMessageOK ('IOStat error with file','writepoly')
!       print*,'IOstat = ', MOD ( IOvalue, 256 )

100     CONTINUE

       END

!-----------------------------------------------------------------------*

       SUBROUTINE SaveEPoly ( success )

! PURPOSE: To write currently displayed polygon, or all defined polygons
!          to file, using file unit 12 as output file unit.  
!   GIVEN: Data in Common POLYDEFS = data to write.
! RETURNS: success = .TRUE. if polygon(s) saved , else .FALSE.
!          In Common IOVALUES ; IOvalue
!                               0 = OK.
!                               1 = user "quit" at filename.
!                               others = F77L3 specific.
! EFFECTS: Polygon(s) defined by vertx(actvpoly,), verty(actvpoly,) are
!          written to user named file, or all defined polygons are written.
!-----------------------------------------------------------------------*
 

! - PARAMETERS (constants)
       integer funit
       PARAMETER ( funit = 12 )

! - PASSED VARIABLES
       LOGICAL success

! - COMMON AREA 
!   - IOVALUES is for I/O status storage
       integer IOvalue
       COMMON /IOVALUES/ IOvalue

! - "INCLUDES"
       include '../includes/edpolys.inc'

! - LOCAL VARIABLES
       integer fnlen, i, j
       CHARACTER cstr*80, ans*1
       CHARACTER*80 Flename
       LOGICAL Exists, Quitting, named, all, polys
       logical PigOpenFile

!----------------START ROUTINE------------------------------------------

!     - see if any polygons to save
      IF ( numpolys .gt. 0 ) THEN
!       - polygons exist to save
        polys = .TRUE.
      ELSE
!       - no polygons exist to save
        polys = .FALSE.
        cstr = 'No Polygons Defined To Save.'
        call PigPutMessage ( cstr )
      ENDIF
!       - ( numpolys > 0 )

      IF ( polys ) THEN
!       - save all or displayed polygons only
       cstr = 'Save ACTIVE Polygon only ?:'
        call PigMessageYesNo(cstr, ans)
       IF ( ans(1:1) .eq. 'N' ) THEN
!         - save all defined polygons
           all = .TRUE.
       ELSE
!         - save only actvpoly
         all = .FALSE.
       ENDIF
!         - ( ans = O )

       success = .FALSE.
       named = .FALSE.
       Exists = .FALSE.
       Quitting = .FALSE.
       Flename = ' '
       if(PigOpenFile(funit,'Save Polygon File',flename,&
             'Polygon File (*.ply),*.ply;All Files (*.*),*.*;')) then
             fnlen = len_trim( Flename )
!             - file does not exist, write to new file
             named = .TRUE.
             Quitting = .FALSE.
       ELSE
           Quitting = .TRUE.
           named = .TRUE.
           success = .FALSE.
           IOvalue = 1
       ENDIF
      ELSE
!       - no polys to save
       Quitting = .TRUE.
       success = .FALSE.
      ENDIF
!       - ( polys )

       IF ( .NOT. Quitting ) THEN
!         - start writing...
         cstr = 'Writing File...[POLY] format.'
         call PigPutMessage ( cstr )
         IF ( all ) THEN
!           - write all polygons
           DO i = 1, numpolys
!             - write vertex count for this polygon
             WRITE ( funit, *, err = 99, iostat = IOvalue ) vertcnt(i),PolyCode(i)
             DO j = 1, vertcnt(i)
!               - write coordinates for this polygon
              WRITE ( funit, *, err = 99, iostat = IOvalue ) vertx(i,j), verty(i,j)
             END DO
!               - ( j = 1, vertcnt(i) )
           END DO
!             - ( i = 1, numpolys )
         ELSE
!           - write actvpoly only, write vertex count
           WRITE ( funit, *, err = 99, iostat = IOvalue ) vertcnt(actvpoly),PolyCode(actvpoly)
           DO j = 1, vertcnt(actvpoly)
!             - write coordinates
             WRITE ( funit, *, err = 99, iostat = IOvalue ) vertx(actvpoly,j), verty(actvpoly,j)
           END DO
!             - ( j = 1, vertcnt(i) )
         ENDIF
!           - ( all )
       CLOSE ( funit, status = 'KEEP', err = 99, iostat = IOvalue )
       success = .TRUE.
       ENDIF
!         - ( NOT Quitting )
       GOTO 100
99     call PigMessageOK ('IOStat error with file','writepoly')
!       print*,'IOstat = ', MOD ( IOvalue, 256 )
100     CONTINUE

       END

!----------------------------------------------------------------------*

    SUBROUTINE PrepPoly2 ( numvert,vertx,verty )

! PURPOSE: To determine certain polygon information in preparation for
!          a call to InPoly.
!   GIVEN: polyid  = the id number of polygon to use in Common in EDPOLYS.INC.
! RETURNS: In Common /SLOP/:
!             slope = array of slopes of polygon sides
!             a, b, c = arrays of line equation components of polygon sides
!             pmaxx, pmaxy, pminx, pminy = min/max x,y polygon coordinates
!-----------------------------------------------------------------------*


!    USE Poly_arrays
    
    implicit none
    
! - PASSED VARIABLES
    INTEGER numvert
    real :: vertx(numvert),verty(numvert)

! - LOCAL VARIABLES

    INTEGER i

! - COMMON BLOCKS
!       - SLOP stores polygon information for InPoly calls
    integer, parameter :: maxvert = 1000
    REAL pmaxx, pmaxy, pminx, pminy
    REAL slope(maxvert+1), a(maxvert+1), b(maxvert+1), c(maxvert+1)
!         - array needs 1 extra point to connect 1st & last points
    COMMON /SLOP/ slope, a, b, c, pmaxx, pmaxy, pminx, pminy


!--------------------START ROUTINE-----------------------------------


!       - get slopes & line equations for each polygon boundary
    DO i = 1, numvert-1
      call PointSlope2 ( slope(i), vertx(i),vertx(i+1), &
               verty(i),verty(i+1), a(i), b(i), c(i) )
    END DO
!         - ( i = 1, numvert-1 )
    call PointSlope2 ( slope(numvert), vertx(numvert),vertx(1),&
     verty(numvert),verty(1), a(numvert),b(numvert), c(numvert) )

!       - calculate the max x,y's of polygon
    pmaxx = vertx(1)
    pmaxy = verty(1)
    DO i = 1, numvert
      IF (vertx(i) .gt. pmaxx)  pmaxx = vertx(i)
      IF (verty(i) .gt. pmaxy)  pmaxy = verty(i)
    END DO

!       - calculate the min x,ys of polygon
    pminx = vertx(1)
    pminy = verty(1)
    DO i = 1, numvert
      IF (vertx(i) .lt. pminx)  pminx = vertx(i)
      IF (verty(i) .lt. pminy)  pminy = verty(i)
    END DO

    END

!-----------------------------------------------------------------------*

    SUBROUTINE PointSlope2 ( slup, vertxa, vertxb, vertya,vertyb, ax, by, cnstnt )

! PURPOSE: To get the slope and general equations of lines.
!   GIVEN: vertxa, vertxb, vertya, vertyb = endpoints of line section
!                                           to operate on.
! RETURNS: slup = slope of the line section
!          ax, by, cnstnt = general equation of the line section.
!-----------------------------------------------------------------------*


! - PASSED VARIABLES
    REAL vertxa, vertxb, vertya, vertyb, slup
    REAL ax, by, cnstnt

! - LOCAL VARIABLES
      REAL rise, run

!----------------START ROUTINE------------------------------------------

    rise = vertyb - vertya
    run = vertxb - vertxa
    IF ( run .eq. 0 ) THEN
      slup = 9999
    ELSE
      slup = rise / run
    ENDIF
    IF ( ABS(slup) .lt. 0.001 ) THEN
      slup = 0.0
    ENDIF
    IF ( slup .eq. 0 ) THEN
      ax = slup
      by = 1
      cnstnt = vertya
    ELSEIF ( slup .eq. 9999 ) THEN
      ax = 1
      by = 0
      cnstnt = vertxa
    ELSE
      ax = slup
      by = -1
      cnstnt = ( slup * vertxa - vertya )
    ENDIF

      END

!-----------------------------------------------------------------------*

    SUBROUTINE ListInPoly2(numvert,vertx,verty,maxnp,np,xg,yg,polylist)

! PURPOSE: Determines, for each triangle in the triangle list, which vertices
!          are in the active polygon and which are not.
!   GIVEN: Polygon data in Common in EDPOLYS.INC.
!          Grid data
!          Triangle list in Common in INCLUDES/TRSCOM.SET
! RETURNS:  PListTr(j,i) = TRUE if ListTr(j,i) is in polygon, else FALSE
!-----------------------------------------------------------------------*

    implicit none
    
! - PASSED VARIABLES
    integer numvert,maxnp,np
    real :: xg(maxnp),yg(maxnp)
    real :: vertx(numvert), verty(numvert)
    logical polylist(maxnp)

! - COMMON BLOCKS
    integer, parameter :: maxvert = 1000
    REAL pmaxx, pmaxy, pminx, pminy
    REAL slope(maxvert+1), a(maxvert+1), b(maxvert+1), c(maxvert+1)
!         - array needs 1 extra point to connect 1st & last points
    COMMON /SLOP/ slope, a, b, c, pmaxx, pmaxy, pminx, pminy

! - LOCAL VARIABLES
    INTEGER i,idx
    REAL xtmp, ytmp
    LOGICAL inp

!------------------START ROUTINE---------------------------------------

    call PrepPoly2 (numvert,vertx, verty)

    PolyList = .FALSE.

!       - now determine points that are inside currently active poly

    idx = 0
    DO i = 1, np
      xtmp = xg(i)
      ytmp = yg(i)
      call InPoly2 ( xtmp, ytmp, inp,numvert,vertx,verty )
!           - mark vertex as in/out polygon
        PolyList(i) = inp

        if(inp) idx=idx+1
    ENDDO

    END

!-----------------------------------------------------------------------*

    SUBROUTINE InPoly2 (xpoint,ypoint,in,numvert,vertx,verty )

! PURPOSE: To see if a point is inside or outside of the specified
!           polygon.
!   GIVEN: Polygon data in Common SLOP, this data must be obtained by a 
!          call to PrepPoly prior to any calls to InPoly referencing the
!          same polygon. When polyid changes between calls to InPoly,
!          PrepPoly must be called to obtain the new info for the new polygon.
!          Passed Arguments;
!          polyid  = the id number of polygon to use in Common POLYDEFS
!                    in PolyStor.Inc.
!          xpoint, ypoint = x,y coordinates of point to test.
! RETURNS: in = TRUE if point is in polygon, else FALSE.
!-----------------------------------------------------------------------*

!    USE Poly_arrays

    implicit none

! - PASSED VARIABLES
    integer numvert
    REAL xpoint, ypoint
    real :: vertx(numvert), verty(numvert)
    LOGICAL in

! - COMMON BLOCKS
    integer, parameter :: maxvert = 1000
    REAL pmaxx, pmaxy, pminx, pminy
    REAL slope(maxvert+1), a(maxvert+1), b(maxvert+1), c(maxvert+1)
!         - array needs 1 extra point to connect 1st & last points
    COMMON /SLOP/ slope, a, b, c, pmaxx, pmaxy, pminx, pminy

! - LOCAL VARIABLES
    REAL xpt, x, y, evenodd
    integer cross, j

!--------------------START ROUTINE-----------------------------------

!       - store coordinates of point to test
    x = xpoint
    y = ypoint
!       - get the number of crossing with the polygon boundary
    cross = 0
!       - see if point falls in the max and min range of polygon
    IF ( x .le. pmaxx ) THEN
      IF ( x .ge. pminx ) THEN
        IF ( y .le. pmaxy ) THEN
          IF ( Y .ge. pminy ) THEN
!               - yes, step through the polygon boundaries
        DO j = 1, numvert
!                 - see if slope = 9999 and if point is on same y axis
          IF ( slope(j) .eq. 9999 ) THEN
            IF ( x .ge. vertx(j) ) THEN
              IF ( j .eq. numvert ) THEN
            IF ( ( (y .le. verty(numvert) ) .AND.&
               (y .ge. verty(1)       ) ).OR.&
               ( (y .ge. verty(numvert) ) .AND.&
               (y .le. verty(1)       ) ) ) THEN
!                         - it has crossed the polygon boundary
              cross = cross + 1
            ENDIF
!                         - ( y test )
              ELSEIF ( ( (y .le. verty(j)   ) .AND.&
                 (y .ge. verty(j+1) ) ) .OR.&
                 ( (y .ge. verty(j)   ) .AND.&
                 (y .le. verty(j+1) ) ) ) THEN
!                       - it has crossed the polygon boundary
            cross = cross + 1
              ENDIF
!                       - ( j = numvert )
            ENDIF
!                     - ( x >= vertx(j) )
!                   - see if normal slope (+ or -), and if point is not
!                   - higher or lower than y endpoints of the vertices
          ELSEIF ( slope(j) .ne. 0 ) THEN
            xpt = ( c(j) + y ) / a(j) 
            IF ( j .eq. numvert ) THEN
              IF ( ( (xpt .le. vertx(numvert) ) .AND.&
                (xpt .ge. vertx(1)       ) ) .OR.&
                ( (xpt .ge. vertx(numvert) ) .AND.&
                (xpt .le. vertx(1)       ) ) ) THEN
            IF ( x .ge. xpt) THEN
!                         - it has crossed the polygon boundary
              cross = cross + 1
            ENDIF
!                         - ( x >= xpt )
              ENDIF
!                       - ( xpt test )
            ELSEIF ( ( (xpt .le. vertx(j)   ) .AND.&
                   (xpt .ge. vertx(j+1) ) ) .OR.&
                   ( (xpt .ge. vertx(j)   ) .AND.&
                   (xpt .le. vertx(j+1) ) ) ) THEN
              IF ( x .ge. xpt ) THEN
!                       - it has crossed the polygon boundary
            cross = cross + 1
              ENDIF
!                       - ( x >= xpt )
            ENDIF
!                     - ( j = numvert )
          ENDIF
!                   - ( xpt test )
        END DO
!                 - ( j = 1, numvert )
!               - decide how many times scanline crossed poly bounds
        evenodd = AMOD ( ( cross * 1.0 ), 2.0 )
        IF ( evenodd .ne. 0 ) THEN
!                 - point is in polygon
          in = .TRUE.
        ELSE
          in = .FALSE.
        ENDIF
!                 - ( evenodd ne 0 )
          ELSE
        in = .FALSE.
          ENDIF
!               - ( y >= pminy )
        ELSE
          in = .FALSE.
        ENDIF
!             - ( y <= pmaxy )
      ELSE
        in = .FALSE.
      ENDIF
!           - ( x >= pminx )
    ELSE
      in = .FALSE.
    ENDIF
!         - ( x <= pmaxx )
    END

!*******************************************************************************

      SUBROUTINE CCWSORT2(NDX,NUMNBRS,NBARRAY,np,maxngh,xg,yg,nbrs)

!     Purpose : To sort neighbours of vertex NDX into c/clockwise order
!     Returns : NUMNBRS - number of neighbours of NDX
!               NBARRAY - indices of neighbours of NDX in ccw order
!     Comment : Sorting by angle is relatively inefficient and should
!               be replaced by methods buried in S/R Get2Nb
!-----------------------------------------------------------------------*

      implicit none

! *** PASSED VARIABLES ***
      integer np,maxngh,nbrs(maxngh,np)
      integer  NDX, NUMNBRS, NBARRAY(maxngh)
      real xg(np),yg(np)

! *** LOCAL VARIABLES ***

      integer I, J, NCT, JMIN, NBTEMP(maxngh)
      REAL GetANGLE, XANGLE(maxngh), X, Y, ANGMIN

!  Count and stack neighbours and evaluate angular positions
      NUMNBRS = 0   
      DO J = 1, maxngh
        IF(nbrs(J,NDX).GT.0) THEN
          NUMNBRS = NUMNBRS + 1
          NBTEMP(NUMNBRS) = nbrs(J,NDX)
          X = xg(nbrs(J,NDX)) - xg(NDX)
          Y = yg(nbrs(J,NDX)) - yg(NDX)
          XANGLE(NUMNBRS) = GetANGLE(X,Y)
        ENDIF
      enddo
!100   CONTINUE

!  Sort neighbours into ccw order and store in NBARRAY
      NCT = 0
      DO I = 1, NUMNBRS
!     Find unused neighbour with minimum angle
        ANGMIN = 6.2831853
        DO J = 1, NUMNBRS
          IF(XANGLE(J).LT.ANGMIN) THEN
            ANGMIN = XANGLE(J)
            JMIN = J
          ENDIF
        enddo
!102     CONTINUE 
        NCT = NCT + 1
        NBARRAY(NCT) = NBTEMP(JMIN)
        XANGLE(JMIN) = 999.
      enddo
!101   CONTINUE      

      RETURN
      END

!*******************************************************************************
      
      FUNCTION GetANGLE(X,Y)

!  Purpose: Return angle in radians.
!  Givens : real x
!           real y
!  Returns: Angle in radians, between 0 and 2*PI,equaling
!           ATAN(Y/X), measured counterclockwise from east
!-----------------------------------------------------------------------*

      REAL GetANGLE
      REAL X, Y, TWOPI

      TWOPI = 6.2831853
      GetANGLE = ATAN2(Y,X)
      IF(GetANGLE.LT.0) GetANGLE = GetANGLE + TWOPI

      END

!*******************************************************************************

    LOGICAL FUNCTION aTr2(Tr,np,maxngh,nbrs)

!       Purpose: Returns aTr .true. if nodes Tr( ) form a triangle
!-----------------------------------------------------------------------*

! *** passed variables
    integer np,maxngh, TR(3),nbrs(maxngh,np)
    
    LOGICAL aNb2

    aTr2 = aNb2(Tr(1),Tr(2),np,maxngh,nbrs).and.&
       aNb2(Tr(2),Tr(3),np,maxngh,nbrs).and.aNb2(Tr(3),Tr(1),np,maxngh,nbrs) 

    END

!*******************************************************************************

      LOGICAL FUNCTION aNb2(Nb1, Nb2,np,maxngh,nbrs)

!     Purpose: Returns aNb .FALSE. if Nb1, Nb2 are not neighbours
!-----------------------------------------------------------------------*

! *** passed variables
      integer Nb1, Nb2
      integer np,maxngh,nbrs(maxngh,np)

      integer   I
   
    aNb2 = .FALSE.
    if (Nb2.ne.0) then
      do i = 1,maxngh
        if (nbrs(i,Nb2).eq.Nb1) then
          aNb2 = .TRUE.
        end if
      enddo
    end if

    END

!*--------------------------------------------------------------------------*
!------------------END poly_subs---------------------------------------*
!---------------------------------------------------------------------------*

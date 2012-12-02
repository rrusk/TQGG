!-----------------------------------------------------------------------*

      SUBROUTINE NewNode ( xn, yn, mess, ierr )

!  PURPOSE: To ask user to pick a location for a new node.
!    INPUT: xnew,ynew = (x,y) co-ordinates by user
!    GIVEN: mess = message to prompt with, deleted after selection
!  RETURNS: (xn,yn) as input by user
!            ierr  = 1  if there is input
!                  = 0 if no input, or invalid input.
!  WRITTEN: May 1990 by JDM, for NODER.
!----------------------------------------------------------------------*


! - "INCLUDES"
       include '../includes/graf.def'

! - PASSED VARIABLES
      REAL xn, yn
      integer ierr
      CHARACTER*(*) mess

! - COMMON BLOCKS
!      REAL cwxl, cwxh, cwyl, cwyh
!      COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh

      integer tnr
      
!-----------------START ROUTINE----------------------------------

      ierr = 0
      call PigPutMessage( mess )
      DO WHILE ( ierr .eq. 0 )
*       xn = ( cwxl+ cwxh)/2.0
*       yn = ( cwyl+ cwyh)/2.0
       call PigGetMouse(tnr, xn, yn)
       IF ( tnr .eq. MAINWIN ) then
              ierr = 1
       else
              call PigPutMessage('Select point from graphics window.')
       endif
      END DO
      call PigEraseMessage
      END

C*--------------------------------------------------------------------------*

      SUBROUTINE NewPt( xnew, ynew, imess, ierr )
C
C Purpose : To ask user to pick a location
C Input   : xnew, ynew = (x,y) co-ordinates
C Given   : imess = 0 if message is to be "Pick a point...",
C                   1 if message is "Pick the mid-side point.."
C                   2 if message is "Pick a NEW point..."
C                   3 if message is "Pick an EXISTING point..."
C                   4 if message is "Pick a NEW point..." using a rubber line
c                   OTHER, message is "Pick a point..."
C Return  : (xnew,ynew) as input
C            ierr  = 0 if there is input
C                  = 999 if want to quit
C Modified: Steve Prestage and Daphne Connolly  May 1989



C     - PASSED PARAMETERS
      REAL xnew, ynew, x0, y0
      integer ierr, imess

C     - COMMON BLOCKS
*      REAL cwxl, cwxh, cwyl, cwyh
*      COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh

C     - LOCAL VARIABLES
*      integer istat
C             - used for graphics input call : returns status of
C             -- pick and ascii decimal equiv. of key hit.

C--------------BEGIN---------------------
	character*(80) mess
C     - get the co-ordinates of the new point
      IF ( imess .eq. 0 ) THEN
	mess = 'Pick a point..'
      ELSEIF (imess .eq. 1) then
	mess = 'Pick the mid-side point..'
      elseif (imess .eq. 2) then
	mess = 'Pick a NEW point..'
      elseif (imess .eq. 3) then
	mess = 'Pick an EXISTING point..'
      elseif (imess .eq. 4) then
	mess = 'Pick a NEW point..'
      else
	mess = 'Pick a point..'
      ENDIF
      if(imess.eq.4)then
	 call PigPutMessage(mess)
	 x0=xnew
	 y0=ynew
	 call PigRubberLine(xnew,ynew,x0,y0)
	 ierr = 0
      else
	 call NewNode(xnew, ynew,mess,ierr)
	 if(ierr.eq.1) then
	     ierr = 0
	 else
	     ierr = 1
	 endif
      end if
      call PigEraseMessage
      END

!-----------------------------------------------------------------------*
 
      SUBROUTINE RubberNode ( xn, yn, xinit, yinit, mess, ierr )

! PURPOSE: To ask user to pick a location for a point.
!   INPUT: xn,yn = (x,y) co-ordinates by user,
!   GIVEN: mess = message to prompt with,
!          xinit, yinit = coordinates of initial cursor location,
! RETURNS: (xn,yn) as input by user
!            ierr  = 1  if there is input
!                  = 0 if no input, or invalid input.
! EFFECTS: New point is selected, and a "rubber band" line is drawn
!          connecting point (xinit,yinit) to cursor while point is chosen.
! WRITTEN: May 1990 by JDM, for NODER.
!----------------------------------------------------------------------*


! - "INCLUDES"
       include '../includes/graf.def'

! - PASSED VARIABLES
      REAL xn, yn, xinit, yinit
      integer ierr
      CHARACTER*(*) mess

! - COMMON BLOCKS
      REAL cwxl, cwxh, cwyl, cwyh
      COMMON /CURWIN/ cwxl, cwxh, cwyl, cwyh

       xn = ( cwxl+ cwxh)/2.0
       yn = ( cwyl+ cwyh)/2.0
*        ierr = 0

*      DO WHILE ( ierr .eq. 0 )
       call PigPutMessage( mess )
       call PigRubberLine(xn, yn, xinit, yinit)
*        IF (tnr .eq. MAINWIN) ierr = 1
*      END DO
       ierr = 1
      END

!-----------------------------------------------------------------------*

	subroutine PigRubberLine(Xnew, Ynew, Xinit, Yinit)

*+ Purpose:  Displays a rubberline around Xinit, Yinit.  Used to
*+           define any polygon area.
*+ Givens :  real       Xinit   : initial x location in WC
*+           real       Yinit   : initial y location in WC
*+ Returns:  real       XNew    : final x location in WC
*+           real       YNew    : final y location in WC
*+ Effects:  Draws a rubberline from Xinit, Yinit to Xnew,Ynew until
*+           the mouse is clicked.

	REAL  Xnew, Ynew, Xinit, Yinit

	Xnew = Xinit
	Ynew = Yinit

      call WPigRubberLine(Xnew, Ynew, Xinit, Yinit)  !used in Visual Fortran version

      end

!*--------------------------------------------------------------------------*

      SUBROUTINE DefPoly ( xpos, ypos, success )

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

! - PASSED VARIABLES
       LOGICAL success

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

! - LOCAL VARIABLES
       integer, parameter :: ii1=1,i2=2
       REAL xpos, ypos
       INTEGER i, j,jj, ierr, tmpactv
       CHARACTER cstr*80, ans*1
       CHARACTER*3 char_nump, char_scrnp
       LOGICAL polydone, In_Box
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

!       - store currently active polygon, if any
       tmpactv = 0
       IF ( PolyDisplay .lt. 2 ) THEN
!         - other polygons may be displayed
         IF ( actvpoly .gt. 0 ) THEN
!           - there is an active polygon now, draw it in red
           tmpactv = actvpoly
           actvpoly = 0
           call DisplayPoly ( tmpactv )
         ENDIF
!           - ( actvpoly > 0 )
       ENDIF
!         - ( PolyDisplay < 2 )

!       - get method of defining polygon
       InpType = 1
       ans(1:1) = 'C'

!       - start new polygon
 !      call PigSetLineColour ( yellow )
       i = 1
       success = .FALSE.
       polydone = .FALSE.
       call PigDrawModifySymbol (xpos, ypos)
       DO WHILE ( .NOT. polydone )
!         - get a polygon vertex location
         IF ( InpType .eq. 1) THEN
!           - cursor input
           ierr = 0
           cstr = 'Pick Vertices, pick first again to close polygon.'
           IF ( i .eq. 1 ) THEN
!             - first vertex, no rubberband
             vx(1) = xpos
             vy(1) = ypos
             ierr = 1
             i = 2
             cycle
           ELSE
!             - not first vertex, use rubber band
             call RubberNode( xpos, ypos, vx(i-1), vy(i-1),cstr, ierr)
           ENDIF
!            - ( i = 1 )
         ENDIF
!           - ( InpType = 1 )
         IF ( ierr .eq. 1 ) THEN
!           - vertex chosen successfully, check if valid
           IF ( In_Box(xpos,ypos) ) THEN
             vx(i) = xpos
             vy(i) = ypos
             prange = 0.1*sqrt((xpos-vx(i-1))**2 + (ypos-vy(i-1))**2)
!             - check if polygon is to be closed now
             v1xmin = vx(1) - prange
             v1xmax = vx(1) + prange
             v1ymin = vy(1) - prange
             v1ymax = vy(1) + prange
             i1=max0(1,i-1)
             IF (        (       (i .gt. 1)
     &                    .AND.       (vx(i) .ge. v1xmin)
     &                    .AND.       (vx(i) .le. v1xmax)
     &                    .AND.       (vy(i) .ge. v1ymin)
     &                    .AND.       (vy(i) .le. v1ymax) 
     &                     )
     &                .OR.       (       (i .gt. 2)
     &                    .AND.       (vx(i) .eq. vx(i1))
     &                    .AND.       (vy(i) .eq. vy(i1))
     &                     )
     &                .OR.  (i .eq. maxvert)
     &               )  THEN
!               - close polygon
              IF ( i .eq. maxvert ) THEN
!                 - put marker and line for maxvert
                call PigDrawLine ( i, vx, vy, ii1 )
                i = i + 1
              ENDIF
!                 - ( i = maxvert )
!               - connect last to first
              vx(i) = vx(1)
              vy(i) = vy(1)
              call PigDrawLine ( i, vx, vy, ii1 )
              IF ( i .lt. 4 ) THEN
!                 - reject polygon for too few vertices
                cstr = 'Invalid polygon, 3 vertices minimum.'
                call PigMessageOK ( cstr , 'polygon')
!                call PigUWait ( seconds )
!                 - remove from display
                call PigDrawLine ( i, vx, vy, i2 )
                i = 1
                polydone = .FALSE.
              ELSE
!                 - enough vertices in polygon
                cstr ='Polygon OK ?:'
                call PigMessageYesNo(cstr, ans)
                IF ( ans(1:1) .eq. 'Y' ) THEN
!                   - polygon OK
                  polydone = .TRUE.
                  success = .TRUE.
                ELSE
!                   - polygon rejected, remove from display
                  call PigDrawLine ( i, vx, vy, i2 )
                  polydone = .TRUE.
                  success = .FALSE.
                  i = 1
                ENDIF
!                   - ( ans = Y ) polygon OK
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
              i = i + 1
             ENDIF
!               - ( closing polygon )
           ELSE
!             - invalid point, not in current window
             cstr = 'Invalid point, pick again.'
             call PigPutMessage ( cstr )
           ENDIF
!             - ( In_Box(xpos, ypos) )
         ELSE
!           - error in choosing a vertex
           cstr = 'Invalid point, pick again.'
           call PigPutMessage ( cstr )
         ENDIF
!           - ( ierr =  1 ) NewNode
       END DO
!         - ( NOT donepoly )

       IF ( success ) THEN
!         - a new polygon has been created
         numpolys = numpolys + 1
!         call PigDrawLine ( i, vx, vy, ii1 )
         vnum = i !- 1
         DO j = 1, vnum
           vertx(numpolys,j) = vx(j)
           verty(numpolys,j) = vy(j)
         END DO
         vertcnt(numpolys) = vnum
         PolyCode(numpolys) = numpolys
         actvpoly = numpolys
!         scrnpoly = curpoly
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
       ELSE
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

!-----------------------------------------------------------------------*

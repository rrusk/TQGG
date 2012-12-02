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

C*----------------------------------------------------------------------*
C                                                                       *
C                           PICED1.FOR                                  *
C      This Module contains routines that perform editing and updating  *
C      functions for additions and deletions of vertices and lines.     *
C                                                                       *
C*----------------------------------------------------------------------*
C*----------------------------------------------------------------------*
      SUBROUTINE Expand( nrec )


C     Purpose  : To expand number of neighbours to max permitted (NBTOT)
C                and set added neighbours to 0
C     Given    : NREC - number of points in the data file
C     Modifies : the neighbour array NL

      use MainArrays

C     - PASSED PARAMETERS
      integer nrec

C     - LOCAL VARIABLES
      integer i, j, nn
      integer nrecl
C--------BEGIN--------------
c     adjust for the value of nrec pointing to next available slot,
c     not the actual count...see ReData subroutine
c      call PigPutMessage('Initializing unused neighbour arrays...')
      nrecl = nrec - 1
      nn = nbtotr + 1
C     - expand neighbour spaces to nbtot
      nbtotr = nbtot
C     - set added neighbours to zero
      DO i = 1,nrecl
      DO j = nn,nbtot
          NL(j,i) = 0
      ENDDO
      ENDDO
c      call PigPutMessage('Initializing unused node arrays...')
C temporary fix changed agd 27 jan 94. Array bounds error on startup with
C small value of MREC. Limited to min of MREC and (nrec+mrec)
C     Ensure whole neighbour array is empty, needed in case of many
C     additions to file, e.g. in refining grid
C      DO i = nrec + 1, min(MREC,nrec + mrec)
      DO i = nrecl+1, MREC
       !exist(i) = .false.
       code(i) = -9
      end do
c      DO i = nrecl+1, MREC
c           dxray(i) = 0.0
c           dyray(i) = 0.0
c           DEPTH(I) = 0.0
c           exist(i) = .false.
c           code(i) = 0
C
C the nl array indices are backwards for efficient memory access. So here
C try reversing the access order. - that made things quite bad...
C
c      DO i = nrecl+1, MREC
c           DO j = 1, nbtot
cc      DO i = nrecl+1, MREC
c               NL(j,i) = 0
c           ENDDO
c      ENDDO
c      call PigPutMessage('Done Initializing arrays...')
c      call PigEraseMessage
      END

C*--------------------------------------------------------------------------*
      LOGICAL FUNCTION Space( ndx )

C Purpose : To check for space in the neighbour list of a particular point.
C Given   : ndx - the point to check

      use MainArrays

C     - PASSED PARAMETERS
      integer ndx
C           - the point that will be checked

C     - LOCAL VARIABLES
      integer i

C-----------BEGIN----------

      Space = .FALSE.
      DO i = 1,nbtotr
        IF ( NL(i,ndx) .eq. 0 ) THEN
          Space = .TRUE.
          return
        ENDIF
      ENDDO
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE Naybor( ndx )
C Purpose : To update the neighborhood arrays.
C Given   : ndx - index to neighbor that will be updated.

      use MainArrays

C     - PASSED PARAMETERS
      integer ndx

      integer mvndx
      COMMON /PICK/ mvndx
C     - LOCAL VARIABLES
      integer i
C-----------BEGIN----------
      DO i = 1, nbtotr
      IF ( NL(i,ndx) .eq. 0 ) THEN
        NL(i,ndx) = mvndx
        return
      ENDIF
      ENDDO
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE ChkPt( xpt, ypt, ndx, ierr )
C
C PURPOSE : To validate the existence of data point.
C           consideration for existence :
C           within (range), units in screen co-ordinates
C           if given (xpt,ypt) exists, return actual co-ordinate
C           i.e. dxray(i),dxray(i).
C GIVEN   : (xpt,ypt),    points to be checked in screen co-ordinate.
C           (dxray,dyray) arrays contain displayed (x,y) co-ordinates.
C           *** note ***  the arrays, dxray and dyray, only contain
C                         data points which appear on screen.
C           itot          total number of data points on screen.
C RETURNS : ierr -- 0 if data point exist, 1 if not.
C           (xpt,ypt) -- the exact stored co-ordinates
C            ndx -- index where (xpt,ypt) in dxray and dyray

      use MainArrays

C     - PASSED PARAMETERS
      REAL xpt, ypt, xpt0, ypt0
      integer ierr
      integer ndx

C     - LOCAL VARIABLES
      integer i
      LOGICAL in_box
      REAL hxlim, lxlim, hylim, lylim, nrange
      real diffdist, diffnew
      REAL CWXL, CWXH, CWYL, CWYH 

C------------BEGIN-----------

      ndx = 0
      ierr  = 1
      diffdist= 1.E30
      xpt0 = xpt
      ypt0 = ypt
      call PigGetWorldCoordinates(CWXL, CWXH, CWYL, CWYH)
      nrange = 0.05*(CWXH-CWXL)
      hxlim = xpt + nrange
      lxlim = xpt - nrange
      hylim = ypt + nrange
      lylim = ypt - nrange

      DO i = 1,itot
!        IF ( exist(i) ) THEN
        IF ( code(i).ge.0 ) THEN
        IF ( In_Box( dxray(i), dyray(i) ) ) THEN
          IF ((dxray(i) .le. hxlim) .AND. (dxray(i) .ge. lxlim)) THEN
            IF ((dyray(i) .le. hylim) .AND. (dyray(i) .ge. lylim)) THEN
              ierr = 0
              diffnew = (xpt0-dxray(i))**2 + (ypt0-dyray(i))**2
              if(diffnew.lt.diffdist) then
                diffdist=diffnew
                xpt = dxray(i)
                ypt = dyray(i)
                ndx = i
              endif
            ENDIF
          ENDIF
        ENDIF
        ENDIF
      enddo

      END

C*--------------------------------------------------------------------------*

      SUBROUTINE PickNeighs( pxray, pyray, icnt )
C
C Purpose : To ask user to manually pick all relevant points (neighbours)
C           which are to connect to coordinate in ( pxray(1), pyray(1) ).
C           Does not provide a fool-proof check on inputs.
C Assumes:  AddVertexPanel has been called to set up options.
C Input   : (xloc,yloc) = real co-ordinates
C Returns : icnt = total number of elements in each array pxray and pyray.
C           pxray(), pyray() = arrays of x & y coords of selected neighbours.
C           ierr = error indicator;
C                  999 to terminate program
C                  0 normal exit.
C  Effects: Creates but leaves no markers displayed on return.

      use MainArrays

C     - INCLUDES
      INCLUDE '../includes/graf.def'

C     - PASSED PARAMETERS
      REAL pxray(nbtot), pyray(nbtot)
      integer icnt

      integer ndummy(nbtot)
      integer cnt
      COMMON /ADDITN/ ndummy, cnt

      integer  i,j,ncn2,n(4)
      real :: xm0,xm1,ym0,ym1
      real :: x0,y0,x1,y1,x2,y2,x3,y3,DA4,delta

C----------------BEGIN----------------------

        do j=1,TotTr
          ncn2 = 3 + min0(1,ListTr(4,j))
          do i=1,ncn2
            n(i) = ListTr(i,j)
          enddo
          xm0 = minval(dxray(n(1:ncn2)))
          xm1 = maxval(dxray(n(1:ncn2)))
          ym0 = minval(dyray(n(1:ncn2)))
          ym1 = maxval(dyray(n(1:ncn2)))

          x0 = pxray(1)
          y0 = pyray(1)

          if(x0.gt.xm0.and.x0.lt.xm1.and.y0.gt.ym0.and.y0.lt.ym1) then
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
            IF (DA4.GT.0.) THEN
              DELTA=DA1+DA2+DA3-DA4
              IF (DELTA.EQ.0..OR.
     *           (DELTA.NE.0..AND.DELTA.LE.DA4*1.E-6)) THEN
                index = j
                icnt = ncn2+1
                cnt = ncn2
                do i=1,ncn2
                  ndummy(i) = n(i)
                  pxray(i+1)=DXRAY(ListTr(i,j))
                  pyray(i+1)=DyRAY(ListTr(i,j))
                enddo
                exit
              endif
            endif
          endif
        enddo

      END

C*--------------------------------------------------------------------------*
      SUBROUTINE AddPro( xold, yold, change, nrec )
C
C Purpose : To add a point and define its neighbors.
C Given   : nrec = number of points in grid.
C Returns : change = TRUE if any additions did take place.
C  Effects: Creates but leaves no markers or lines displayed on return.

      use MainArrays

C     - INCLUDES
      INCLUDE '../includes/graf.def'

C     - PASSED PARAMETERS
      LOGICAL change
C             - set to true if any additions of points took place
      integer nrec
C             - the number of active records

      integer ndummy(nbtot)
      integer cnt
      COMMON /ADDITN/ ndummy, cnt

C     - LOCAL VARIABLES
      REAL xold, yold
C          - the point supplied by the user
      REAL txray(nbtot), tyray(nbtot)
C          - temp array to hold the data points
      CHARACTER*80 ans
      character cstr*80, PigCursYesNo*1
C          - user supplied answers
      LOGICAL error
C          - set true when the neighbors to the point have too many neighbors
      LOGICAL error1
C          - set true when user picks too many neighbors for the point
      integer mark(nbtot)
C          - holds location index for points of error
      integer i, m, z, msymbol1, msymbol2
      integer ierr
C          - error flag
      REAL xloc, yloc
      integer icnt
C          - number of neighbors to a point

C--------------BEGIN--------------------------

C     - marker symbol for new point to add
      msymbol1 = 3
C     - marker symbol for neighbours picked
      msymbol2 = 4

      error1 = .FALSE.
      error = .FALSE.
      cnt = 0
      DO i = 1, nbtot
        ndummy(i) = 0
      ENDDO
      ierr = 0
C     - get the new point
!      call NewPt( xold, yold, 2, ierr )
!      IF ( ierr .eq. 999 ) GOTO 999
C     - put a marker at the spot
      xloc = xold
      yloc = yold
      call PutMarker( xloc, yloc, msymbol1, foregr )
C     - and store the new point
      txray(1) = xold
      tyray(1) = yold
      icnt = 1
!     - now get all the neighbour points, PickNeighs leaves no markers
      call Pickneighs( txray, tyray, icnt )

C     - draw out the new polygon
      call ReDraw( txray, tyray, icnt, 1 )

      cstr ='Is this addition satisfactory ?:'
      ans = PigCursYesNo (cstr)
      IF ( ans(1:1) .eq. 'Y' ) THEN
        call AddUp(txray,tyray,nrec,error,mark,m,error1)
        IF ( (.NOT. error) .AND. (.NOT. error1) ) THEN
          change = .TRUE.
          call PigPutMessage( 'Addition complete..' )
        ELSEIF ( error ) THEN
C         - if neighbors to the point have too many neighbors
          DO i = 1, m
            z = mark(i)
            call PutMarker( dxray(z), dyray(z),
     +                         msymbol2, yellow)
          ENDDO
          call PigPutMessage('ERROR - too many neighbours')
          call ReDraw( txray, tyray, icnt, 0 )
        ELSEIF ( error1 ) THEN 
C         - too many nodes to add a new one
          call PigPutMessage('ERROR - grid full, cannot add new nodes')
        ELSE     
C         - too many points were picked by user
          call PigPutMessage('ERROR - Too many neighbours picked..')
          call ReDraw( txray, tyray, icnt, 0 )
        ENDIF
      ELSE
C       - not satisfactory
        call PigPutMessage( 'No update..' )
        call ReDraw( txray, tyray, icnt, 0 )
      ENDIF

      END

C*--------------------------------------------------------------------------*

      SUBROUTINE AddUp(txray,tyray,nrec,error,mark,m,error1)

C Purpose : To update the screen arrays which may
C           later be written to the data file
C Given   : txray,tyray - the x,y coordinates of the new point
C                         and all its neighbors.
C           nrec - used to calculate the master index mvndx
C Returns : error - set to true when too many neighbors for a point
C           error1 - ?
C           mrec - the upper limit to the number of points
C           nbtot - the maximum number of neighbors
C Effects :

      use MainArrays

C     - INCLUDES
      INCLUDE '../includes/graf.def'

C     - PASSED PARAMETERS
      REAL txray(nbtot),tyray(nbtot)
      integer nrec, m, mark(nbtot)
      LOGICAL error, error1
      
C     -  COMMON AREAS
      integer ndummy(nbtot)
C             - the number of additions that have occured
      integer cnt
C             - used to store the index of the neighbors
      COMMON /ADDITN/ ndummy, cnt

      integer mvndx
C             - the master index
      COMMON /PICK/ mvndx
C     - LOCAL VARIABLES
      integer temdex
C            - temp value for mvndx
      integer i,j, msymbol1, msymbol2
      LOGICAL fndspt
C               - set true when we have found a spot to insert
C               -- the point in the neighbor list

C---------- BEGIN -----------

C     - marker symbol for new point
      msymbol1 = 3
C     - marker symbol for neighbours picked
      msymbol2 = 4
      m = 0
      error1 = .FALSE.
      error = .FALSE.
      temdex = itot + 1
      !exist(temdex) = .TRUE.
c *** nbtotr already expanded to nbtot
c     IF ( ndummy(nbtotr+1) .ne. 0) THEN
      IF ( cnt .gt. nbtot) THEN
C       - the user has picked more than nbtotr neighbors
        error1 = .TRUE.
      ELSE
C       - see if neighbors to the point can include the
C       -- point as one of its neighbors
        DO i = 1, cnt
          fndspt = .FALSE.
          DO j = 1, nbtot
            IF ( .NOT. fndspt ) THEN
              IF ( NL(j,ndummy(i)) .eq. 0 ) THEN
                NL(j,ndummy(i)) = temdex
                fndspt = .TRUE.
              ENDIF
            ENDIF
          ENDDO
          IF ( .NOT. fndspt ) THEN
C           - no empty spot was found, therefore the neighbour
C           -- already has 7 neighbours - keep track of this neighbour
            error = .TRUE.
            m = m + 1
            mark(m) = ndummy(i)
          ENDIF
        ENDDO
      ENDIF

      IF ( (.NOT. error) .AND. (.NOT. error1) ) THEN
        nrec = nrec + 1
        itot = itot + 1
        mvndx = itot
        DO i = 1, nbtotr
          NL(i,mvndx) = ndummy(i)
        ENDDO
        dxray(mvndx) = txray(1)
        dyray(mvndx) = tyray(1)
        depavg = 0.
        do i=1,cnt
          depavg = depavg + depth(ndummy(i))
        enddo
        DEPTH(MVNDX) = depavg/float(max(1,cnt))
        code(mvndx) = 0
      ELSE
C       - ( error  OR  error1 )
        IF ( error ) THEN
          DO i = 1, cnt
            DO j = 1, nbtotr
              IF ( NL(j,ndummy(i)) .eq. temdex ) THEN
                NL(j,ndummy(i)) = 0
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C       - ( NOT error  AND  NOT error1 )
C     - remove marker at new point
      call PutMarker (txray(1), tyray(1), msymbol1, backgr )
      END

C*--------------------------------------------------------------------------*
      SUBROUTINE DelSeg( XNEW,YNEW, change, ierr )

C Purpose : To eliminate a connection between 2 vertices
C Comment : Replaces earlier DELSEG by Russ Kirby in which whole
C           process is manual
C Returns : CHANGE  - true if changes have been made
C                     which require new triangle list
C           IERR = 0 if a line is identified for deletion
C                  1 if ROUTINE NEARLINE finds no line
C
C *** Variables passed to S/R 
C                Coordinates of point XNEW,YNEW
C *** Variables returned by S/R NEARLINE
C                NX1 - Index of vertex at one end of grid line
C                      near enough to (XNEW,YNEW)
C                NX2 - Index of vertex at other end of same line
C                NX3 - Vertex forming triangle with NX1, NX2
C                IND = 1  if only one triangle includes NX1 and NX2
C                               i.e. line is on boundary
C                    = 2  if two triangles include NX1 and NX2
C                    = 0  if no line found by cursor (ERROR)
C                    = 3  if 0 or >2 triangles include NX1 and NX2

      use MainArrays

C     - INCLUDES
      INCLUDE '../includes/graf.def'

C     - PASSED VARIABLES 
      LOGICAL  change
      integer  ierr

!      integer DELETE(mrec)
!      integer COUNT
!      COMMON /NOTIFY0/DELETE,COUNT

C     - LOCAL VARIABLES 
      integer nx1, nx2, nx3, nx4, ind, j, msymbol2
      REAL xnew, ynew, pxray(5), pyray(5)
      CHARACTER*80 ans
      character cstr*80, PigCursYesNo*1
C
C------- BEGIN ----------

      msymbol2 = 4

C     - locate mid-point of line
!      call NEWPT(XNEW,YNEW,1,IERR)
!      IF (IERR .eq. 999) go to 999

C     - find triangle(s) associated with new point
      call NEARLINE(XNEW,YNEW,NX1,NX2,NX3,NX4,IND)

C     - check no error condition
      IF ( IND .EQ. 0 ) THEN
C       - no line found by cursor
      IERR = 1
      GO TO 999
CC    ELSEIF ( IND .EQ. 3 ) THEN
CC      - 2 vertices in 0 or more than 2 triangles
CC      IERR = 2
CC      GO TO 999
      ELSE
C       - O.K. 2 triangles
      IERR = 0
      ENDIF
C     - erase existing line betweeen vertices nx1 and nx2
      pxray(1) = dxray(nx1)
      pyray(1) = dyray(nx1)
      pxray(2) = dxray(nx2)
      pyray(2) = dyray(nx2)
      call ReDraw( pxray, pyray, 2, 0 )

C     - ask confirmation

      cstr='Is this change OK ?:'
      ans = PigCursYesNo (cstr)
      IF ( ans(1:1) .eq. 'Y') THEN
      change = .TRUE.
C       - NX1 , NX2  no longer neighbours
      DO j = 1, nbtotr
        IF ( NL(J,NX1) .eq. NX2 ) THEN
          NL(J,NX1) = 0
        ENDIF
        IF ( NL(J,NX2) .eq. NX1 ) THEN
          NL(J,NX2) = 0
        ENDIF
      ENDDO
C       - PigPrompt user to supply computational code if deletion
C       - leaves formerly interior point on boundary
      IF ( IND .eq. 1 ) THEN
C         - place temporary marker at NX3
        call PutMarker( dxray(nx3), dyray(nx3), msymbol2, violet )
402       CONTINUE
C           - get a code from the user
          call PigPrompt('Enter a computational code for this'//
     +                  ' point:', ans )
          read (ans,FMT='(I4)',ERR=402) CODE(NX3)
         call PutMarker( dxray(nx3), dyray(nx3), msymbol2, backgr )
       ENDIF
      ELSE
C       - if not satisfactory -
      call PigPutMessage( 'No update..' )
C       - replace old connection
      call ReDraw( pxray, pyray, 2, 1 )
      ENDIF
!      cstr = 'Delete more lines ?:'
!      ans = PigCursYesNo (cstr)
!      IF ( ans(1:1) .eq. 'Y') THEN
!      GOTO 10
!      ENDIF
999   CONTINUE
      END


C*--------------------------------------------------------------------------*
      SUBROUTINE DelPro(XOLD, YOLD, change )
C
C PURPOSE : To delete a point and all the line sections leading
C           from it
C RETURNS : change - logical, set to true if any deletions
C                    did take place

      use MainArrays


C *** PASSED VARIABLES ***
      LOGICAL CHANGE

C *** COMMON AREA ***
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/defaults.inc'

      integer MVNDX
      COMMON /PICK/MVNDX
C
!      integer DELETE(mrec)
!      integer COUNT
!      COMMON /NOTIFY0/DELETE,COUNT
C
C
C *** LOCAL VARIABLES ***
      integer IERR
C                 - USED FOR ERROR CHECKING
      REAL XOLD, YOLD
C                 - THE USER SUPPLIED POINT TO BE DELETED
      REAL TXRAY(nbtot), TYRAY(nbtot)
C                 - TEMP ARRAY TO HOLD THE POINT TO BE DELETED AND
C                   ITS NEIGHBORS
      integer ICNT
C                 - COUNTER TO THE TEMP ARRAY
      logical delupOKp, delupOK
      CHARACTER*80 ANS
      character cstr*80, PigCursYesNo*1
C                 - YES OR NO ANSWER FROM PROMPTS TO THE USER
C
C *** START DELETION ROUTINE
C
!      if (COUNT .ne. mrec) then
      IERR = 0
C       * get the point to be deleted
!      call NEWPT(XOLD,YOLD,3,IERR)
      if (IERR .eq. 999) goto 999
C       * see if it is a valid point
      call CHKPT (XOLD, YOLD, MVNDX, IERR)
      if (IERR .eq. 1) then
        call PigPutMessage( 'ERROR - Invalid point..' )
      else
C           * carry on with the deletion process
C           * store point to be deleted in a temp array
C           * and keep a counter to that array
          TXRAY(1) = XOLD
          TYRAY(1) = YOLD
          ICNT = 1
            if(DispNodes) then
              call PigSetSymbolNumber( NodeMType )
              call ReDrawMark( xold, yold, Backgr )
            else
C             * now get all the neighbouring points
            call RELPT2 (TXRAY, TYRAY, mvndx, ICNT, IERR)
            if (IERR .eq. 999) goto 999
C             - Modified by S.P   March 29, 89
C             call VANISH (TXRAY, TYRAY, ICNT, OLDSEG)
            call ReDraw( txray, tyray, icnt, 0 )
            endif
C           * ask user for confirmation
          cstr = 'Is this deletion satisfactory ?:'
            ans = PigCursYesNo (cstr)
          if (ANS(1:1) .eq. 'Y') then
             CHANGE = .true.
             delupOKp = DELUPOK(mvndx)
          else
C           if not satisfactory -
            call PigPutMessage( 'No update..' )
C              - if deletion not satisfactory, redraw line
              if(DispNodes) then
                call PigSetSymbolNumber( NodeMType )
                if(code(mvndx).gt.0.and.Code(mvndx).ne.90) then
                  call ReDrawMark( xold, yold, NodeBColor )
                else
                  call ReDrawMark( xold, yold, NodeIColor )
                endif
              else
              call Redraw( txray, tyray, icnt, 1 )
              endif
          endif
       endif
!      else
!      call PigPutMessage('ERROR - Deletion buffer full..' )
!      endif

!      cstr = 'Delete more nodes ?:'
!      ans = PigCursYesNo (cstr)
!      IF ( ans(1:1) .eq. 'Y') THEN
!      GOTO 10
!      ENDIF
999   continue
      end
C
C
C*--------------------------------------------------------------------------*
      SUBROUTINE RelPt2( pxray, pyray, mvndx, icnt, ierr )
C
C PURPOSE : THIS ROUTINE PICKS ALL RELEVANT POINTS WHICH
C           ARE CONNECTED TO CO-ORDINATE STORED IN (PXRAY(1),
C           PYRAY(1)).
C
C INPUT   : (XLOC,YLOC) -- REAL, CO-ORDINATES
C           IDAT        -- ADE OF CHARACTER TO DECIDE CASES
C
C RETURNS : ICNT -- TOTAL NUMBER OF ELEMENTS IN EACH ARRAYS
C                   PXRAY AND PYRAY.
C           IERR -- ERROR INDICATOR.
C                   999 TO TERMINATE PROGRAM
C                   0  NORMAL EXIT.

      use MainArrays

C *** PASSED VARIABLES ***
      REAL   PXRAY(*), PYRAY(*)
      integer ICNT, MVNDX, IERR

      integer I
C
      ierr = 0
      if    (    (mvndx.lt.1)
     +      .or. (mvndx.gt.MREC)
     +      ) goto 997
!      if(.not.EXIST(mvndx).or.code(mvndx).lt.0) goto 999
      if(code(mvndx).lt.0) goto 999

      if (ICNT .LT. 1) goto 998
C
      do I = 1,NBTOTR
C        get the index of the neighbours
       if (NL(I,MVNDX) .ne. 0) then
          ICNT = ICNT + 1
C           and get the X,Y locations for that index
          PXRAY(ICNT) = DXRAY(NL(I,MVNDX))
          PYRAY(ICNT) = DYRAY(NL(I,MVNDX))
       endif
      end do
      goto 1000
C
997   continue
      call PigPutMessage('ERROR - Invalid mvndx in call to RelPt2')
      goto 999

998   continue
      call PigPutMessage('ERROR - No reference point picked..')
C
999   continue
      IERR = 999
C
1000  continue
c      write (msg,'(a,i5,a,i3,a,i4,a)') 
c     +    'RelPt2(,,',mvndx,',',icnt,',',ierr,')'
c      call PigPutMessage(msg)
      end
C*--------------------------------------------------------------------------*
      SUBROUTINE ReDraw( pxray, pyray, icnt, rdflag )

C Purpose : To draw line section which paired in pxray and pyray
C           arrays. All elememts are in screen co-ordinates.
C           The first element of the two arrays is the starting
C           point of each line, icnt is the number of elements
C           in an array; icnt - 1 is the total number of lines to
C           be drawn. For example :
C                if icnt = 3,  then 2 lines will be drawn.
C                line 1 : (pxray(1),pyray(1)) to (pxray(2),pxray(2))
C                line 2 : (pxray(1),pyray(1)) to (pxray(3),pyray(3))
C Given   : pxray - real array contains x co-ordinates,
C           pyray - real array contains y co-ordinates,
C           icnt  - total no. of paired co-ordinates,
C           rdflag = 1 if Modification color to be used for lines,
C                    else back ground color is used.
C Returns : None.
C Modified: Steve Prestage and Daphne Connolly  May 1989
C Modified: OCT91 (JDM), segments no longer used.



      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/graf.def'

C     - PASSED PARAMETERS
      REAL pxray(*), pyray(*)
      integer icnt, rdflag

C     - COMMON BLOCKS
      REAL CWXL, CWXH, CWYL, CWYH
      COMMON /CURWIN/ CWXL, CWXH, CWYL, CWYH

C     - LOCAL VARIABLES
      integer   i
      REAL      strtx(2),strty(2)
C               - the start and endpoints of a line section
      integer   prev_tn, PrevColour

C------------------BEGIN------------------

      call PigGetWindowNum( prev_tn )
      call PigSetWindowNum( MAINWIN )

      call PigGetLineColour(PrevColour)

      if ( rdflag .eq. 1 ) then
      call PigSetLineColour( ModificationColour )
      else
      call PigSetLineColour( backgr )
      endif

C     - start out at the new point
      strtx(1) = pxray(1)
      strty(1) = pyray(1)
      do i = 2, icnt
      strtx(2) = pxray(I)
      strty(2) = pyray(I)

C       - connect neighbors
      call PigDrawPolyline( 2, strtx, strty )
      enddo
      call PigSetWindowNum( prev_tn )
      
      call PigSetLineColour(PrevColour)
      END

C--------------------------END PICED1.FOR------------------------------*

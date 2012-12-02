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

C-----------------------------------------------------------------------*
C                                                                       *
C                       PLOTSUBS.FOR                                    *
C       Routines:       DrwCon                                          *
C                       DrwConFill                                     *
C                                                                       *
C-----------------------------------------------------------------------*
C-----------------------------------------------------------------------*
      SUBROUTINE DrwContours ()

C PURPOSE: Display contours with attributes as set in Common in cntcfg.inc
C-----------------------------------------------------------------------*
      INCLUDE '../includes/cntcfg.inc'

      If(ScalePhase(1).eq.'L') then
        call DrwConLine()
      else
        call DrwConFill()
      endif

      end

C-----------------------------------------------------------------------*
      SUBROUTINE DrwConLine ()

C PURPOSE: Display contours with attributes as set in Common in cntcfg.inc
C   GIVEN: prtit = TRUE if output to printer
C          pltit = TRUE if output to screen
C
C                         idf   iphase  LL      ncon          Scale or Phase
C                         ---   ------  --      ----          --------------
C     DateType: 1) Depth   0      0      1      NumCntValues    Scale only

C-----------------------------------------------------------------------*

      use MainArrays


C     - PASSED PARAMETERS
      LOGICAL TransOn

C     - INCLUDES
      INCLUDE '../includes/cntcfg.inc'

      integer nptstart,nptend,TotTr2
      common /trans2/ nptstart,nptend,TotTr2

C     - vcon will hold contour range values from CntValues
      REAL vcon
      DIMENSION var(9), xx(9,3), vcon(20,2)

C     - LOCAL VARIABLES
C       idf = id # to indicate data type being plotted, 0..7
C       in_box = function that returns TRUE if point is inside current window
C       ncon = # of contour colors set

      LOGICAL inside, in_box
      INTEGER idf, iphase, ncon, k, k2, L, nsw, node, cntr, j, LL, ncn 
      INTEGER*4 icolor, icnt
      integer first, last
      REAL v3, vlmt, var, xx, v1, xcon, ycon, xp(5), yp(5)
!      REAL plmt, p0lmt

!      DATA plmt  /150.0/
!      DATA p0lmt /260.0/
      DATA ncn   /3/
      DATA vcon /21*0.0, 30.0, 60.0, 90.0, 120.0, 150.0, 180.0,
     +                210.0, 240.0, 270.0, 300.0, 330.0, 8*0.0/


C--------------BEGIN---------------
      DataType = 1
      netype = 0
      TransOn = .false.

C     - from table above:
C     - Depth, Hamp, Uamp, Vamp, or Speed
      LL = 1
      iphase = 0
      ncon = NumCntValues(DataType)

      idf = Datatype - 1
      DO j = 1, ncon
        vcon(j,LL) = CntValues(DataType,j)
      ENDDO

      if(TransOn) then
        first = TotTr +1
        last = TotTr2
      else
        first = 1
        last = TotTr
      endif

C     - for each contour...
      DO cntr = 1, Ncon
        call PigSetWindowNum( 1 )
C        - set polyline width scale factor
!        call PigSetLineWidth( Plwscale1 )
C        - set contour color as set in cntcfg.inc
        icolor = CntColors(DataType,cntr)
        call PigSetLineColour( icolor )
c       DO L = 1, TotTr
        DO L = first,last
          if(TCode(L).le.0) go to 999
          nsw = 0
          inside = .FALSE.
          if(ListTr(4,L).gt.0) then
            ncn = 4
          else
            ncn = 3
          endif
          DO j = 1, ncn
            node = ListTr(j,L)
            xx(j,1) = dxray( node )
            xx(j,2) = dyray( node )
            xx(j,3) = Depth( node )
            inside = In_Box( xx(j,1), xx(j,2) )
            vlmt = 0.0
            IF ( idf .eq. 0 ) THEN
C                 - depth data type, var(j) = depth at point
                var(j) = xx(j,3)
            ELSE
C                 - non-depth data type
c                var(j) = vel(node,idf)
            ENDIF
          END DO
          IF ( .NOT. inside ) GO TO 999
          IF ( vcon(cntr,LL) .eq. var(1) ) THEN
C              - if equal to depth, then plot it
            nsw = 1
            icnt = 1
            xp(1) = xx(1,1)
            yp(1) = xx(1,2)
          ENDIF
          DO k = 1, ncn
            k2 = k + 1
            IF ( k2 .gt. ncn ) THEN
                k2 = 1
            ENDIF
            IF ( var(k) .gt. var(k2) ) THEN
                GO TO 610
            ENDIF
            IF ( vcon(cntr,LL) .lt. var(k) .OR.
     +              vcon(cntr,LL) .gt. var(k2) ) THEN
                GO TO 649
            ENDIF
            GO TO 611
610         IF ( vcon(cntr,LL) .lt. var(k2) .OR.
     +           vcon(cntr,LL) .gt. var(k) ) THEN
                GO TO 649
            ENDIF
611         v3 = var(k2) - var(k)
            v1 = 1.0
            IF ( ABS(v3) .gt. 1.0 E-7) THEN
C                 - v1 is a multiple
c data: v1 computed from vcon, cntr, var, k, v3
                v1 = ( vcon(cntr,LL) - var(k) ) / v3
            ENDIF

C              - xcon & ycon will be plotted
C              --  computed from v1, xx, k2, k
            xcon = v1 * ( xx(k2,1) - xx(k,1) ) + xx(k,1)
            ycon = v1 * ( xx(k2,2) - xx(k,2) ) + xx(k,2)
            IF ( nsw .eq. 1 ) THEN
C                 - icnt # point to plot
                Icnt = Icnt + 1
                xp(Icnt) = xcon
                yp(Icnt) = ycon
            ELSE
C                 - first point to plot
                nsw = 1
                Icnt = 1
                xp(1) = xcon
                yp(1) = ycon
            ENDIF
C - goto's exist for following line
649         CONTINUE
          END DO
          IF ( (nsw .eq. 1) .AND. (icnt .gt. 1) ) THEN
c data: xp, yp, icnt
            call PigDrawPolyLine( icnt, xp, yp )
          ENDIF
999       CONTINUE
        END DO
        IF ( LabelsOn(DataType) ) THEN
!          call ConLab()
        ENDIF
c999     continue
      END DO
c      if(.not.TransOn) call LBound
      call PigSetWindowNum( 1 )

      RETURN
      END

C-----------------------------------------------------------------------*
      SUBROUTINE DrwConFill()

C PURPOSE: Display contours with attributes as set in Common in cntcfg.inc
C   GIVEN: prtit = TRUE if output to printer
C          pltit = TRUE if output to screen
C WRITTEN: ?
C MODIFIED: June92 - JDM - Configuration set in cntcfg.inc by ConfigCntrs
C                    is used. Values set are:
C
C                         idf   LL      ncon          Scale or Phase
C                         ---   --      ----          --------------
C     DateType: 1) Depth   0     1      NumCntValues    Scale only
C-----------------------------------------------------------------------*

      use MainArrays


C     - PASSED VARIABLES
      LOGICAL TransOn

C     - INCLUDES
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

      integer nptstart,nptend,TotTr2
      common /trans2/ nptstart,nptend,TotTr2

C - LOCAL VARIABLES
      LOGICAL inside, in_box
      REAL vcon
      INTEGER idf, ncon, irnbw(16)
      INTEGER npt, indxp0
      integer first, last
      REAL v1, t1, t2, v3, xp, yp, t, x, y
      REAL tmin, tmax
      INTEGER node, ncn, j, LL, L, cntr, j2, k

      DIMENSION T(9),X(4),y(4),VCON(20,2)
      DIMENSION xp(10),yp(10)
      DATA vcon /21*0.,30.,60.,90.,120.,150.,180.,
     *           210.,240.,270.,300.,330.,8*0./
      DATA ncn/3/

C --------------- BEGIN ----------------

      DataType = 1
      netype = 0
      TransOn = .false.

      DO j = 1, 16
        irnbw(j) = CntColors(DataType,j)
      ENDDO

C     - from table above:
C     - Depth, Hamp, Uamp, Vamp, or Speed
      LL = 1
      ncon = NumCntValues(DataType)

      idf = Datatype - 1
      DO j = 1, ncon
        vcon(j,LL) = CntValues(DataType,j)
      ENDDO

      call PigSetWindowNum(MAINWIN)

      if(TransOn) then
        first = TotTr +1
        last = TotTr2
        indxp0 = 1
      else
        first = 1
        last = TotTr
        indxp0 = netype
      endif

      DO 653 cntr = 1, ncon-1
        IF ( irnbw(cntr) .eq. 0 ) GO TO 653
        call PigSetWindowNum(MAINWIN)
C       - set polyline width scale factor
!        call PigSetLineWidth( Plwscale1 )
        tmin = vcon(cntr,LL)
        tmax = vcon(cntr+1,LL)
c        DO 651 L = 1, TotTr
        DO 651 L = first,last
          if(TCode(L).le.0) go to 651
          inside = .FALSE.
          if(ListTr(4,L).gt.0) then
            ncn = 4
          else
            ncn = 3
          endif
          DO J=1,NCN
            NODE= ListTr(j,L)
            x(j) = dxray(node)
            y(j) = dyray(node)
            IF ( in_box( x(j), y(j) ) ) inside = .TRUE.
            IF ( idf .eq. 0 ) THEN
              t(j)= Depth(node)
            elseif(idf.eq.1.and.indxp0.eq.0) then
              node = L
c             t(j) = vel(node,1)
            ELSE
c              t(j)=vel(node, idf)
            ENDIF
          enddo
          IF ( .NOT. inside ) GO TO 651
          DO K = 1, 10
            xp(k)=0.0
            yp(k)=0.0
          ENDDO
          npt = 0
          DO 155 j=1,ncn
            j2 = j+1
            IF ( j2 .gt. ncn ) j2 = 1
            IF ( t(j) .gt. tmax .AND. t(j2) .gt. tmax ) GO TO 155
            IF ( t(j) .lt. tmin .AND. t(j2) .lt. tmin ) GO TO 155
            IF ( t(j) .ge. tmin .AND. t(j)  .le. tmax ) THEN
              npt = npt + 1
              xp(npt) = x(j)
              yp(npt) = y(j)
              IF ( t(j2) .ge. tmin .AND. t(j2) .le. tmax ) THEN
                  GO TO 155
              ELSE
                  v3 = t(j2) - t(j)
                  IF ( t(j2) .gt. tmax ) THEN
                    v1 = (tmax - t(j)) / v3
                  ELSE
                    v1 = (tmin - t(j)) / v3
                  ENDIF
                  npt = npt +1
                  xp(npt) = v1 * (x(j2) - x(j)) + x(j)
                  yp(npt) = v1 * (y(j2) - y(j)) + y(j)
              ENDIF
            ELSEIF ( t(j2) .ge. tmin .AND. t(j2) .le. tmax ) THEN
              v3 = t(j2) - t(j)
              IF ( t(j) .gt. tmax ) THEN
                  v1 = (tmax - t(j)) / v3
              ELSE
                  v1 = (tmin - t(j)) / v3
              ENDIF
              npt = npt + 1
              xp(npt) = v1 * (x(j2) - x(j)) + x(j)
              yp(npt) = v1 * (y(j2) - y(j)) + y(j)
            ELSE
              IF ( t(j) .lt. tmin ) THEN
                  t1 = tmin
                  t2 = tmax
              ELSE
                  t1 = tmax
                  t2 = tmin
              ENDIF
              v3 = t(j2) - t(j)
              v1 = (t1 - t(j)) / v3
              npt = npt + 1
              xp(npt) = v1 * (x(j2) - x(j)) + x(j)
              yp(npt) = v1 * (y(j2) - y(j)) + y(j)
              v1 = (t2 - t(j)) / v3
              npt = npt + 1
              xp(npt) = v1 * (x(j2) - x(j)) + x(j)
              yp(npt) = v1 * (y(j2) - y(j)) + y(j)
            ENDIF
155       CONTINUE
          IF ( npt .gt. 0 ) THEN
C           - set fill area color index
            call PigSetFillColour(irnbw(cntr))
C           - call fill area to create polygons
            call PigDrawFilledPolygon( npt, xp, yp )
          ENDIF
651     CONTINUE
653   CONTINUE

c      if(.not.TransOn) call LBound
      call PigSetWindowNum(MAINWIN)

      RETURN
      END

C------------------------END PLOTSUBS.FOR ------------------------------*

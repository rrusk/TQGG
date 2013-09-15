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

!-----------------------------------------------------------------------*
!-----------------------------------------------------------------------*
      SUBROUTINE DrwContours ()

!-----------------------------------------------------------------------*
! PURPOSE: Display contours with attributes as set in Common in cntcfg.inc
!-----------------------------------------------------------------------*

      INCLUDE '../includes/cntcfg.inc'

      If(ScalePhase(1).eq.'L') then
        call DrwConLine()
      else
        call DrwConFill()
      endif

      end

!-----------------------------------------------------------------------*
      SUBROUTINE DrwConLine ()

!-----------------------------------------------------------------------*
! PURPOSE: Display contour lines with attributes as set in cntcfg.inc
!-----------------------------------------------------------------------*

      use MainArrays

!     - INCLUDES
      INCLUDE '../includes/cntcfg.inc'

!     - vcon will hold contour range values from CntValues
      REAL vcon(20), var(9), xx(9,3)

!     - LOCAL VARIABLES
!       idf = id # to indicate data type being plotted, 0..7
!       in_box = function that returns TRUE if point is inside current window
!       ncon = # of contour colors set

      LOGICAL inside, in_box
      INTEGER ncon, k, k2, L, nsw, node, cntr, j, ncn2 
      INTEGER*4 icolor, icnt
      integer first, last
      REAL v3, vlmt, v1, xcon, ycon, xp(5), yp(5)
      
!--------------BEGIN---------------

      DataType = 1

      ncon = NumCntValues(DataType)

      DO j = 1, ncon
        vcon(j) = CntValues(DataType,j)
      ENDDO

      first = 1
      last = TotTr

!     - for each contour...
      DO cntr = 1, Ncon
!        call PigSetWindowNum( 1 )
!        - set polyline width scale factor
!        call PigSetLineWidth( Plwscale1 )
!        - set contour color as set in cntcfg.inc
        icolor = CntColors(DataType,cntr)
        call PigSetLineColour( icolor )
!       DO L = 1, TotTr
        DO L = first,last
          if(TCode(L).le.0) go to 999
          nsw = 0
          inside = .FALSE.
          if(ListTr(4,L).gt.0) then
            ncn2 = 4
          else
            ncn2 = 3
          endif
          DO j = 1, ncn2
            node = ListTr(j,L)
            xx(j,1) = dxray( node )
            xx(j,2) = dyray( node )
            xx(j,3) = Depth( node )
            inside = In_Box( xx(j,1), xx(j,2) )
            vlmt = 0.0
            var(j) = xx(j,3)
          END DO
          IF ( .NOT. inside ) GO TO 999
          IF ( vcon(cntr) .eq. var(1) ) THEN
!              - if equal to depth, then plot it
            nsw = 1
            icnt = 1
            xp(1) = xx(1,1)
            yp(1) = xx(1,2)
          ENDIF
          DO k = 1, ncn2
            k2 = k + 1
            IF ( k2 .gt. ncn2 ) THEN
                k2 = 1
            ENDIF
            IF ( var(k) .gt. var(k2) ) THEN
                GO TO 610
            ENDIF
            IF ( vcon(cntr) .lt. var(k) .OR.vcon(cntr) .gt. var(k2) ) THEN
                GO TO 649
            ENDIF
            GO TO 611
610         IF ( vcon(cntr) .lt. var(k2) .OR.vcon(cntr) .gt. var(k) ) THEN
                GO TO 649
            ENDIF
611         v3 = var(k2) - var(k)
            v1 = 1.0
            IF ( ABS(v3) .gt. 1.0E-7) THEN
!                 - v1 is a multiple data: v1 computed from vcon, cntr, var, k, v3
                v1 = ( vcon(cntr) - var(k) ) / v3
            ENDIF

!              - xcon & ycon will be plotted
!              --  computed from v1, xx, k2, k
            xcon = v1 * ( xx(k2,1) - xx(k,1) ) + xx(k,1)
            ycon = v1 * ( xx(k2,2) - xx(k,2) ) + xx(k,2)
            IF ( nsw .eq. 1 ) THEN
!                 - icnt # point to plot
                Icnt = Icnt + 1
                xp(Icnt) = xcon
                yp(Icnt) = ycon
            ELSE
!                 - first point to plot
                nsw = 1
                Icnt = 1
                xp(1) = xcon
                yp(1) = ycon
            ENDIF
! - goto's exist for following line
649         CONTINUE
          END DO
          IF ( (nsw .eq. 1) .AND. (icnt .gt. 1) ) THEN
! data: xp, yp, icnt
            call PigDrawPolyLine( icnt, xp, yp )
          ENDIF
999       CONTINUE
        END DO
!        IF ( LabelsOn(DataType) ) THEN
!          call ConLab()
!        ENDIF

      END DO

!      call PigSetWindowNum( 1 )

      RETURN
      END

!-----------------------------------------------------------------------*

      SUBROUTINE DrwConFill()

!-----------------------------------------------------------------------*
! PURPOSE: Display contours with attributes as set in Common in cntcfg.inc
!-----------------------------------------------------------------------*

      use MainArrays

!     - INCLUDES
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

! - LOCAL VARIABLES
      LOGICAL inside, in_box
      REAL vcon(20)
      INTEGER ncon, irnbw(16)
      INTEGER npt
      integer first, last
      REAL v1, t1, t2, v3, xp(10),yp(10), T(9),X(4),y(4)
      REAL tmin, tmax
      INTEGER node, ncn2, j, L, cntr, j2, k

! --------------- BEGIN ----------------

      DataType = 1

      DO j = 1, 16
        irnbw(j) = CntColors(DataType,j)
      ENDDO

      ncon = NumCntValues(DataType)

      DO j = 1, ncon
        vcon(j) = CntValues(DataType,j)
      ENDDO

!      call PigSetWindowNum(MAINWIN)

      first = 1
      last = TotTr

      DO 653 cntr = 1, ncon-1
        IF ( irnbw(cntr) .eq. 0 ) GO TO 653
!        call PigSetWindowNum(MAINWIN)
!       - set polyline width scale factor
!        call PigSetLineWidth( Plwscale1 )
        tmin = vcon(cntr)
        tmax = vcon(cntr+1)
!        DO 651 L = 1, TotTr
        DO 651 L = first,last
          if(TCode(L).le.0) go to 651
          inside = .FALSE.
          if(ListTr(4,L).gt.0) then
            ncn2 = 4
          else
            ncn2 = 3
          endif
          DO J=1,ncn2
            NODE= ListTr(j,L)
            x(j) = dxray(node)
            y(j) = dyray(node)
            IF ( in_box( x(j), y(j) ) ) inside = .TRUE.
            t(j)= Depth(node)
          enddo
          IF ( .NOT. inside ) GO TO 651
          DO K = 1, 10
            xp(k)=0.0
            yp(k)=0.0
          ENDDO
          npt = 0
          DO 155 j=1,ncn2
            j2 = j+1
            IF ( j2 .gt. ncn2 ) j2 = 1
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
!           - set fill area color index
            call PigSetFillColour(irnbw(cntr))
!           - call fill area to create polygons
            call PigDrawFilledPolygon( npt, xp, yp )
          ENDIF
651     CONTINUE
653   CONTINUE

!      call PigSetWindowNum(MAINWIN)

      RETURN
      END

!------------------------END PLOTSUBS.FOR ------------------------------*

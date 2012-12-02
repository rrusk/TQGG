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

!*-------------------------------------------------------------------*

!*-------------------------------------------------------------------*

      SUBROUTINE ScaleOrShift(Control)

!     Purpose : To shift or scale x,y coordinates of all nodes
!         Control = 1 - scale coordinates
!                 = 2 - shift coordinates
!                 = 3 - rotate coordinates
!                 = 4 - scale depths
!                 = 5 - shift depths

      use MainArrays

      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/edpolys.inc'
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON /MAXRG/ XMAX,YMAX,XMIN,YMIN

      integer PolyDisplay
      common /polystatus/PolyDisplay

      integer LIMWIN
      PARAMETER( LIMWIN = 10 )
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL

! *** PASSED VARIABLES ***
      integer CONTROL

! *** LOCAL VARIABLES ***
      REAL XSHIFT, YSHIFT
      REAL XSCF, YSCF
      REAL DSCF, DSHIFT
      REAL THETA, STH, CTH, xx, xmid, ymid
      integer JJ, kk
      LOGICAL SUCCESS
      character*(80)    ans
      character*(80)    msg

      IF (CONTROL .EQ. 1) THEN
!     Scaling coordinates

      success = .false.
      do while(.not.success)
        call PigPrompt('Enter scale factor for x coordinates :',ans)
        call PigReadReal(ans, xscf, Success)
        if(xscf.eq.0.) xscf = 1.
      end do

      success = .false.
      do while(.not.success)
        call PigPrompt('Enter scale factor for y coordinates :',ans)
        call PigReadReal(ans, yscf, Success)
        if(yscf.eq.0.) yscf = 1.
      enddo

      success = .false.
         do while(.not.success)
           call PigPrompt('Enter scale factor for depths : ',ans)
           call PigReadReal(ans, dscf, Success)
!          if(dscf.eq.0.) dscf = 1.
         end do
         write(msg,'(a,3f8.3)') 'Working. xscf,yscf,dscf:',xscf,yscf,dscf
         call PigPutMessage(msg)

      DO JJ = 1, ITOT
        DXRAY(JJ) = DXRAY(JJ) * xscf
        DYRAY(JJ) = DYRAY(JJ) * yscf
        DEPTH(JJ) = DEPTH(JJ) * dscf
      enddo
      xcent = (XMAX+XMIN)/2.
      dxc   = (XMAX-XMIN)/2.
      ycent = (YMAX+YMIN)/2.
      dyc   = (YMAX-YMIN)/2.
      scf = amax1(xscf,yscf)
      XMAX=xcent*xscf + dxc*scf 
      YMAX=ycent*yscf + dyc*scf 
      XMIN=xcent*xscf - dxc*scf
      YMIN=ycent*yscf - dyc*scf
      do jj = 1, limwin
          WXL(jj) = WXL(jj) * xscf
          WXH(jj) = WXH(jj) * xscf
          WYL(jj) = WYL(jj) * yscf
          WYH(jj) = WYH(jj) * yscf
      enddo

!rfh  code added by RFH on June 27 1995 to scale polygons with grid
!     note 'including' of EDPOLYS.INC to this subroutine (above)
      do jj = 1, numpolys
        do kk = 1, vertcnt(jj)           
          vertx(jj,kk) = vertx(jj,kk) * xscf
          verty(jj,kk) = verty(jj,kk) * yscf
        enddo
      enddo
!      arrange to display all polygons on next REDRAW
      if(numpolys.gt.0) Polydisplay = 1
!rfh end of inserted code

      ELSEIF (CONTROL .EQ. 2) THEN
!     Shifting coordinates   
      success = .false.
      do while(.not.success)
        call PigPrompt('Enter amount to add to x coordinates :',ans)
        call PigReadReal(ans, xshift, Success)
      enddo

      success = .false.
      do while(.not.success)
        call PigPrompt('Enter amount to add to y coordinates :',ans)
        call PigReadReal(ans, yshift, Success)
      enddo

      success = .false.
      do while(.not.success)
        call PigPrompt('Enter amount to add to depths : ',ans)
        call PigReadReal(ans, dshift, Success)
      end do

      write(msg,'(a,3f8.3)') 'Working.. xs,ys,ds:',xshift,yshift,dshift
      call PigPutMessage(msg)
      DO JJ = 1, ITOT
        DXRAY(JJ) = DXRAY(JJ) + xshift
        DYRAY(JJ) = DYRAY(JJ) + yshift
        DEPTH(JJ) = DEPTH(JJ) + dshift
      enddo
!      shift zoom limits with grid
      XMAX=XMAX + xshift 
      YMAX=YMAX + yshift 
      XMIN=XMIN + xshift
      YMIN=YMIN + yshift
      do jj = 1, limwin
          WXL(jj) = WXL(jj) + xshift
          WXH(jj) = WXH(jj) + xshift
          WYL(jj) = WYL(jj) + yshift
          WYH(jj) = WYH(jj) + yshift
      enddo
!      to shift polygons with grid
      do jj = 1, numpolys
        do kk = 1, vertcnt(jj)           
          vertx(jj,kk) = vertx(jj,kk) + xshift
          verty(jj,kk) = verty(jj,kk) + yshift
        enddo
      enddo
!      arrange to display all polygons on next REDRAW
      if(numpolys.gt.0) Polydisplay = 1
!rfh end of inserted code

      ELSEIF (CONTROL .EQ. 3) THEN
!     Rotating coordinates   
      success = .false.
      do while(.not.success)
        call PigPrompt('How many degrees to rotate grid (c/w) : ',ans)
        call PigReadReal(ans, theta, Success)
      enddo
      write(msg,'(a,3f8.3)') 'Working... theta:',theta
      call PigPutMessage(msg)
      sth = sin(-theta*3.14159265/180.)
      cth = cos(-theta*3.14159265/180.)
! *** rotate about mid-point of window to avoid grid rotating out of sight
      xmid = (xmin + xmax)/2.
      ymid = (ymin + ymax)/2.

! this is probably in error, since it prevents the user from controlling
! the exact transformation. So, for now, rotate about 0,0. Later could add
! ability for user to select rotation point by cursor or by coordinate.
! but not for now...
     xmid = 0.0
     ymid = 0.0

      DO JJ = 1, ITOT
       xx = DXRAY(JJ)
       DXRAY(JJ) =(DXRAY(JJ)-xmid)*cth - (DYRAY(JJ)-ymid)*sth +xmid
       DYRAY(JJ) =(xx-xmid)*sth + (DYRAY(JJ)-ymid)*cth + ymid
      enddo
!      rotate polygons with grid
      do jj = 1, numpolys
        do kk = 1, vertcnt(jj)           
          xx = vertx(jj,kk)
          vertx(jj,kk) = (xx-xmid)*cth - (verty(jj,kk)-ymid)*sth+xmid
          verty(jj,kk) = (xx-xmid)*sth + (verty(jj,kk)-ymid)*cth+ymid
        enddo
      enddo
!      arrange to display all polygons on next REDRAW
      if(numpolys.gt.0) Polydisplay = 1
!rfh end of inserted code

      ELSEIF (CONTROL .EQ. 4) THEN
!     scaling depths
      success = .false.
      do while(.not.success)
        call PigPrompt('Enter scale factor for depths : ',ans)
        call PigReadReal(ans, dscf, Success)
      enddo
      write(msg,'(a,3f8.3)') 'Working... dscf:',dscf
      call PigPutMessage(msg)
      DO JJ = 1, ITOT
        DEPTH(JJ) = DEPTH(JJ) * dscf
      enddo
      ELSEIF (CONTROL .EQ. 5) THEN
! *** shifting depths
      success = .false.
      do while(.not.success)
        call PigPrompt('Enter amount to add to depths : ',ans)
        call PigReadReal(ans, dshift, Success)
      enddo
      write(msg,'(a,3f8.3)') 'Working... dshift:',dshift
      call PigPutMessage(msg)
      DO JJ = 1, ITOT
        DEPTH(JJ) = DEPTH(JJ) + dshift
      enddo
      ENDIF
      
      call PigEraseMessage

      END

!***************************************************************

      SUBROUTINE PolarTransform

!     Purpose : To transform coordinateds to polar from xy

      use MainArrays

      integer LIMWIN
      PARAMETER( LIMWIN = 10 )
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON  /MAXRG/ XMAX,YMAX,XMIN,YMIN

      character(80) text

      d2r = acos(-1.)/180.

! *** check data for proper limits
      DO JJ = 1, ITOT
        If((DXRAY(JJ).lt.-360.).or.(DXRAY(JJ).gt.360.)) then
          text='Longitude out of range -360<long<360'
          call PigMessageOK(Text,'Longitude Limits')
          exit
        endif
      enddo
      DO JJ = 1, ITOT
        If((DYRAY(JJ)+y0off.lt.-89.).or.(DYRAY(JJ)+y0off.gt.89.)) then
          text='Latitude out of range -89<lat<89'
          call PigMessageOK(Text,'Latitude Limits')
          return
        endif
      enddo

      if (level.eq.0) then
        xlong0 = 0.5*(XMIN + XMAX)
        xlongmin = xmin
        xlongmax = xmax
        xlongsum = xlong0
        xmin = xmin - xlong0
        xmax = xmax - xlong0
      else
        xlong0 = 0.5*(WXL(LEVEL)+WXH(LEVEL))
        xlongsum = xlong0
        WXL(LEVEL) = WXL(LEVEL) - xlong0
        WXH(LEVEL) = WXH(LEVEL) - xlong0
      endif

      ScaleX = xlongsum
      ScaleY = -999.

      DO JJ = 1, ITOT
        xscf = cos((dyray(jj)+y0off)*d2r)
        DXRAY(JJ) = (DXRAY(JJ) - xlong0) * xscf
      enddo

      END

!***************************************************************

      SUBROUTINE PolarShift

!     Purpose : To transform coordinateds to polar from xy

      use MainArrays

      integer LIMWIN
      PARAMETER( LIMWIN = 10 )
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON  /MAXRG/ XMAX,YMAX,XMIN,YMIN

      xlongold = xlong0
      d2r = acos(-1.)/180.

      if (level.eq.0) then
        xlong0 = 0.5*(XMIN + XMAX)
        xlongsum = xlongsum + xlong0
        xmin = xmin - xlong0
        xmax = xmax - xlong0
      else
        xlong0 = 0.5*(WXL(LEVEL)+WXH(LEVEL))
        xlongsum = xlongsum + xlong0
        WXL(LEVEL) = WXL(LEVEL) - xlong0
        WXH(LEVEL) = WXH(LEVEL) - xlong0
      endif
      ScaleX = xlongsum
      ScaleY = -999.

      DO JJ = 1, ITOT
        xscf = cos((dyray(jj)+y0off)*d2r)
        DXRAY(JJ) = DXRAY(JJ) - xlong0*xscf
      enddo

      END

!***************************************************************

      SUBROUTINE XYTransform

!     Purpose : To transform coordinates from polar to xy

      use MainArrays

      integer LIMWIN
      PARAMETER( LIMWIN = 10 )
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON  /MAXRG/ XMAX,YMAX,XMIN,YMIN

      d2r = acos(-1.)/180.

      if (level.eq.0) then
        xmin = xlongmin
        xmax = xlongmax
      else
        WXL(LEVEL) = WXL(LEVEL) + xlongsum
        WXH(LEVEL) = WXH(LEVEL) + xlongsum
      endif

      ScaleX = 1.
      ScaleY = 1.

      DO JJ = 1, ITOT
        xscf = cos((dyray(jj)+y0off)*d2r)
        DXRAY(JJ) = (DXRAY(JJ)+xlongsum*xscf)/xscf
      enddo
      xlong0 = 0.

      END

!***************************************************************

      SUBROUTINE MercTransform

!     Purpose : To transform coordinateds to UTM from polar(lin)

      use MainArrays

      integer LIMWIN
      PARAMETER( LIMWIN = 10 )
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON  /MAXRG/ XMAX,YMAX,XMIN,YMIN

      real, parameter ::    PI = 3.1415926536
      real, save :: R, d2r, yref

      character(80) text

      R = 1.
      d2r = acos(-1.)/180.

! *** check data for proper limits
      DO JJ = 1, ITOT
        If((DXRAY(JJ).lt.-180.).or.(DXRAY(JJ).gt.180.)) then
          text='Longitude out of range -180<long<100'
          call PigMessageOK(Text,'Longitude Limits')
          return
        endif
      enddo

      DO JJ = 1, ITOT
        If((DYRAY(JJ)+y0off.lt.-89.).or.(DYRAY(JJ)+y0off.gt.89.)) then
          text='Latitude out of range -89<lat<89'
          call PigMessageOK(Text,'Latitude Limits')
          return
        endif
      enddo


      DO JJ = 1, ITOT
        DXRAY(JJ) = R*DXRAY(JJ)
        yref = y0off
        DYRAY(JJ) = R*alog(tan(0.25*PI + 0.5*d2r*(DYRAY(JJ)+yref)))/d2r
!       scalek = sec(dyday(jj))
      enddo

      yref = y0off
      yref = R*alog(tan(0.25*PI + 0.5*d2r*yref))/d2r
      if (level.eq.0) then
        ymin = ymin + yref
        ymax = ymax + yref
      else
        WYL(LEVEL) = WYL(LEVEL) + yref
        WYH(LEVEL) = WYH(LEVEL) + yref
      endif
      return

      Entry InverseMercTransform

      if (level.eq.0) then
        ymin = ymin - yref
        ymax = ymax - yref
      else
        WYL(LEVEL) = WYL(LEVEL) - yref
        WYH(LEVEL) = WYH(LEVEL) - yref
      endif

      DO JJ = 1, ITOT
        DXRAY(JJ) = DXRAY(JJ)/R
        DYRAY(JJ) = atan(sinh(d2r*DYRAY(JJ)/R))/d2r - y0off
      enddo

      END

!***************************************************************

      SUBROUTINE TMTransform

!     Purpose : To transform coordinateds to UTM from polar(lin)

      use MainArrays

      integer LIMWIN
      PARAMETER( LIMWIN = 10 )
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON  /MAXRG/ XMAX,YMAX,XMIN,YMIN

      real, save :: R, d2r, scaleK0
      character(80) text

      R = 1.
      scaleK0 = 1.
      d2r = acos(-1.)/180.

!      call PigPutMessage('Transverse Mercator Transform not available')
!      return

! *** check data for proper limits
      DO JJ = 1, ITOT
        If((DXRAY(JJ).lt.-180.).or.(DXRAY(JJ).gt.180.)) then
          text='Longitude out of range -180<long<180'
          call PigMessageOK(Text,'Longitude Limits')
          return
        endif
      enddo
      DO JJ = 1, ITOT
        If((DYRAY(JJ)+y0off.lt.-89.).or.(DYRAY(JJ)+y0off.gt.89.)) then
          text='Latitude out of range -89<lat<89'
          call PigMessageOK(Text,'Latitude Limits')
          return
        endif
      enddo

      DO JJ = 1, ITOT
        B = cos(d2r*(dyray(jj)+y0off))*sin(d2r*dxray(jj))
        DYRAY(JJ) = R*scaleK0*(atan(tan(d2r*(dyray(jj)+y0off))/cos(d2r*dxray(jj))) - d2r*y0off)/d2r
        DXRAY(JJ) = 0.5*R*scaleK0*alog((1+B)/(1.-B))/d2r
!       scaleK = scaleK0/sqrt(1.-B**2)
      enddo

      return

      Entry InverseTMTransform

      DO JJ = 1, ITOT
        D = d2r*(dyray(jj)/(R*scaleK0) + y0off)
        DYRAY(JJ)=asin(sin(D)/cosh(d2r*DXRAY(JJ)/(R*scaleK0)))/d2r-y0off
        DXRAY(JJ) = atan(sinh(d2r*DXRAY(JJ)/(R*scaleK0))/cos(D))/d2r
      enddo

      END

!***************************************************************

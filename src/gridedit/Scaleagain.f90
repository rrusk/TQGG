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

!     Purpose : To transform coordinateds to UTM from lat/lon

      use MainArrays

      integer LIMWIN
      PARAMETER( LIMWIN = 10 )
      REAL   WXL(LIMWIN), WXH(LIMWIN), WYL(LIMWIN), WYH(LIMWIN)
      integer LEVEL
      COMMON /WLIMIT/ WXL, WXH, WYL, WYH, LEVEL
      REAL    XMAX, YMAX, XMIN, YMIN
      COMMON  /MAXRG/ XMAX,YMAX,XMIN,YMIN

      character(2) zonestr
      character(20) utmchars
      character(80) ans,text
      real :: tmpx, tmpy, gxmax, gxmin, gxc
      integer grid_zone(2)
      logical success
      real (kind=8) lambda0


! *** check data for proper limits
      DO JJ = 1, ITOT
        If((DXRAY(JJ).lt.-180.).or.(DXRAY(JJ).gt.180.)) then
          text='Longitude out of range -180<long<100'
          call PigMessageOK(Text,'Longitude Limits')
          return
        endif
      enddo

      DO JJ = 1, ITOT
        If((DYRAY(JJ).lt.-89.).or.(DYRAY(JJ).gt.89.)) then
          text='Latitude out of range -89<lat<89'
          call PigMessageOK(Text,'Latitude Limits')
          return
        endif
      enddo


!     Find center of grid and find central meridan (in essence the UTM zone representer by the centerline)

      gxmax = MAXVAL(dxray(1:ITOT))
      gxmin = MINVAL(dxray(1:ITOT))
      gxc = (gxmax + gxmin) / 2 ! Center
      gymax = MAXVAL(dyray(1:ITOT))
      gymin = MINVAL(dyray(1:ITOT))
      gyc = (gymax + gymin) / 2 ! Center

!     Figure out the UTM zone, as well as lambda0
      utmchars = "CDEFGHJKLMNPQRSTUVWX"

      call get_grid_zone (gxc, gyc, grid_zone, lambda0)
      write(zonestr,'(I0.2)') grid_zone(1)

!     Prompt user for UTM zone
      success = .false.
      do while(.not.success)
        call PigPrompt('Enter UTM zone number ('//zonestr//utmchars(grid_zone(2):grid_zone(2)) &
            //'?): ',ans)

          call PigReadReal(ans(1:2), tmpx, Success)
          grid_zone(1) = nint(tmpx)

!         Check for grid lat zone
          DO JJ = 1,20
            IF(ans(3:3).eq.utmchars(JJ:JJ)) then
              grid_zone(2) = JJ
            END IF
          END DO

      enddo

!     Get new central meridan (lambda0) and validate.
      call get_lambda0 (grid_zone, lambda0, ierr)
      if (ierr .NE. 0) then
        write (*,*) 'Unable to translate UTM to LL'
        return
      endif



!     Loop through all nodes and transform from lat/lon to UTM

      DO JJ = 1, ITOT

        CALL ll2utm (dxray(jj), dyray(jj), tmpx, tmpy, lambda0, 3)
        dyray(jj) = tmpy
        dxray(jj) = tmpx

      enddo

!     Redraw fullsize
      xmin = minval(dxray(1:itot))
      xmax = maxval(dxray(1:itot))
      ymin = minval(dyray(1:itot))
      ymax = maxval(dyray(1:itot))
      call fullsize(xmin,ymin,xmax,ymax)

      RETURN



      Entry InverseTMTransform

!
!!     Inverse transformation
!

!     Prompt user for UTM zone
      success = .false.
      grid_zone(2) = 0
      utmchars = "CDEFGHJKLMNPQRSTUVWX"
      do while(.not.success)
        call PigPrompt('Enter UTM zone number (##S): ',ans)

          call PigReadReal(ans(1:2), tmpx, Success)
          grid_zone(1) = nint(tmpx)

!         Check for grid lat zone
          DO JJ = 1,20
            IF(ans(3:3).eq.utmchars(JJ:JJ)) then
              grid_zone(2) = JJ
            END IF
          END DO

          IF(grid_zone(2).eq.0) then
            success = .false.
          END IF
      enddo

!     Loop through all nodes and transform from UTM to lat/lon

      DO JJ = 1, ITOT

        CALL utm2ll(dxray(jj),dyray(jj),tmpx,tmpy,grid_zone,3)
        dyray(jj) = tmpy
        dxray(jj) = tmpx
        
      enddo

!     Redraw fullsize
      xmin = minval(dxray(1:itot))
      xmax = maxval(dxray(1:itot))
      ymin = minval(dyray(1:itot))
      ymax = maxval(dyray(1:itot))
      call fullsize(xmin,ymin,xmax,ymax)

      END

!*************************************************************************
!/*
! * Peter Daly
! * MIT Ocean Acoustics
! * pmd@mit.edu
! * 25-MAY-1998
! * 
! Revisions:
!   Jan. 25, 1999 DHG  Port to Fortran 90
!   Mar. 23, 1999 DHG  To add Lewis Dozier's fix to "rr1" calculation 
!   Mar. 26, 2014 KLO  Edited ll2utm so that lambda0 is an input rather than output
! * 
! Description:
! * 
! * These routines convert UTM to Lat/Longitude and vice-versa,
! * using the WGS-84 (GPS standard) or Clarke 1866 Datums.
! * 
! * The formulae for these routines were originally taken from
! * Chapter 10 of "GPS: Theory and Practice," by B. Hofmann-Wellenhof,
! * H. Lictenegger, and J. Collins. (3rd ed) ISBN: 3-211-82591-6,
! * however, several errors were present in the text which
! * made their formulae incorrect.
! *
! * Instead, the formulae for these routines was taken from
! * "Map Projections: A Working Manual," by John P. Snyder
! * (US Geological Survey Professional Paper 1395)
! *
! * Copyright (C) 1998 Massachusetts Institute of Technology
! *               All Rights Reserved
! *
! * RCS ID: $Id: convert_datum.c,v 1.2 1998/06/04 20:50:47 pmd Exp pmd $
! */
!
!*************************************************************************
!
subroutine get_grid_zone (longitude, latitude, grid_zone, lambda0)

IMPLICIT NONE

real (kind=8) longitude, latitude
integer       grid_zone(2)
real (kind=8) lambda0

   integer  zone_long, zone_lat

   real (kind=8) M_PI
!!!   parameter (M_PI = 3.141592654)

!-------------------------------------------------------------------------

  m_pi = ACOS (-1.0)

!  /* Solve for the grid zone, returns the central meridian */

  zone_long = INT ((longitude + 180.0) / 6.0) + 1
  zone_lat = NINT ((latitude + 80.0) / 8.0)
  grid_zone(1) = zone_long
  grid_zone(2) = zone_lat

!  /* First, let's take care of the polar regions */

  if ((latitude < -80.0) .OR. (latitude > 84.0)) then
     lambda0 = 0.0 * M_PI / 180.0
     return
  endif

!  /* Now the special "X" grid */

  if (latitude .GT. 72.0 .AND. &
      longitude .GT. 0.0 .AND. longitude .LT. 42.0) then
     if (longitude .LT. 9.0) then
        lambda0 = 4.5 * M_PI / 180.0
     elseif (longitude .LT. 21.0) then
        lambda0 = 15.0 * M_PI / 180.0
     elseif (longitude .LT. 33.0) then
        lambda0 = 27.0 * M_PI / 180.0
     elseif (longitude .LT. 42.0) then
        lambda0 = 37.5 * M_PI / 180.0
     endif
     return
  endif

!  /* Handle the special "V" grid */

  if (latitude .GT. 56.0 .AND. latitude .LT. 64.0 .AND. &
      longitude .GT. 0.0 .AND. longitude .LT. 12.0) then
     if (longitude .LT. 3.0) then
        lambda0 = 1.5 * M_PI / 180.0
     elseif (longitude .LT. 12.0) then
        lambda0 = 7.5 * M_PI / 180.0
     endif
     return
  endif

!  /* The remainder of the grids follow the standard rule */

  lambda0 = (FLOAT (zone_long - 1) * 6.0 + (-180.0) + 3.0) * M_PI / 180.0
  
  return
  end

!*************************************************************************

subroutine get_lambda0 (grid_zone, lambda0, ierr)

IMPLICIT NONE

integer       grid_zone(2)
real (kind=8) lambda0
integer       ierr

   integer zone_long
   integer zone_lat
   real (kind=8) latitude, longitude

   real (kind=8) M_PI
!!!   parameter (M_PI = 3.141592654)
 
!---------------------------------------------------------------------------


  m_pi = ACOS (-1.0)

  !/* Given the grid zone, then set the central meridian, lambda0 */

  !/* Check the grid zone format */

  zone_long = grid_zone(1)
  zone_lat = grid_zone(2)
  if ((zone_long .LT. 1) .OR. (zone_long .GT. 61)) then
    write (*,*) 'Invalid grid zone format: ', zone_long, zone_lat
    ierr = -1
    return 
  endif

  longitude = (FLOAT (zone_long - 1) * 6.0) - 180.0
  latitude = (FLOAT (zone_lat) * 8.0) - 80.0

  !/* Take care of special cases */

  if ((latitude .LT. -80.0) .OR. (latitude .GT. 84.0)) then
    lambda0 = 0.0
    ierr = 0
    return 
  endif

  if (latitude .GT. 56.0 .AND. latitude .LT. 64.0 .AND. &
      longitude .GT. 0.0 .AND. longitude .LT. 12.0) then
     if (longitude .LT. 3.0) then
        lambda0 = 1.5 * M_PI / 180.0
     elseif (longitude .LT. 12) then
        lambda0 = 7.5 * M_PI / 180.0
     endif
     ierr = 0
     return
  endif
  
  if (latitude .GT. 72.0 .AND. &
      longitude .GT. 0.0 .AND. longitude < 42.0) then
     if (longitude .LT. 9.0) then
        lambda0 = 4.5 * M_PI / 180.0
     elseif (longitude .LT. 21.0) then
        lambda0 = 15.0 * M_PI / 180.0
     elseif (longitude .LT. 33.0) then
        lambda0 = 27.0 * M_PI / 180.0
     elseif (longitude .LT. 42.0) then
        lambda0 = 37.5 * M_PI / 180.0
     endif
     ierr = 0
     return
  endif

  !/* Now handle standard cases */

  lambda0 = (FLOAT (zone_long - 1) * 6.0 + (-180.0) + 3.0) * M_PI / 180.0

  !/* All done */

  ierr = 0
  return
  end

!*************************************************************************
subroutine ll2utm (longitude, latitude, utm_x, utm_y, lambda0, datum)

IMPLICIT NONE

real (kind=8) latitude, longitude
real (kind=8) utm_x, utm_y
integer       datum

   real (kind=8)  a, b, f, e, e2, e4, e6
   real (kind=8)  phi, lambda, lambda0, phi0, k0
   real (kind=8)  t, rho, x, y, mm, mm0
   real (kind=8)  aa, aa2, aa3, aa4, aa5, aa6
   real (kind=8)  ep2, nn, tt, cc

   real (kind=8) M_PI
!!!   parameter (M_PI = 3.141592654)

   integer CLARKE_1866_DATUM
   parameter (CLARKE_1866_DATUM = 1)
   integer GRS_80_DATUM
   parameter (GRS_80_DATUM = 2)
   integer WGS_84_DATUM
   parameter (WGS_84_DATUM = 3)

!---------------------------------------------------------------------------


  m_pi = ACOS (-1.0)

  !/* Converts lat/long to UTM, using the specified datum */

  if (datum == CLARKE_1866_DATUM) then      ! CLARKE_1866_DATUM:
    a = 6378206.4
    b = 6356583.8
  elseif (datum == GRS_80_DATUM) then      ! GRS_80_DATUM:
    a = 6378137
    b = 6356752.3
  elseif (datum == WGS_84_DATUM) then      ! WGS_84_DATUM:
    a = 6378137.0           !/* semimajor axis of ellipsoid (meters) */
    b = 6356752.31425       !/* semiminor axis of ellipsoid (meters) */
  else
    write (*,*) 'Unknown datum: ', datum
    return
  endif

  !/* Calculate flatness and eccentricity */

  f = 1 - (b / a)
  e2 = 2 * f - f * f
  e = sqrt (e2)
  e4 = e2 * e2
  e6 = e4 * e2

  !/* Convert latitude/longitude to radians */
  
  phi = latitude * M_PI / 180.0
  lambda = longitude * M_PI / 180.0

  phi0 = 0.0

  !/* See if this will use UTM or UPS */

  if (latitude .GT. 84.0) then

    !/* use Universal Polar Stereographic Projection (north polar aspect) */

    k0 = 0.994

    t = sqrt ( ((1 - sin (phi)) / (1 + sin (phi))) * &
           (((1 + e * sin (phi)) / (1 - e * sin (phi))) ** e) )
    rho = 2.0 * a * k0 * t / sqrt ( ((1.0 + e) ** (1.0 + e)) * ((1.0 - e) ** (1.0 - e)) )
    !!! Not needed (dhg) m = cos (phi) / sqrt (1.0 - e2 * sin (phi) * sin (phi))

    x = rho * sin (lambda - lambda0)
    y = -rho * cos (lambda - lambda0)
    !!! Not needed (dhg) k = rho * a * m

    !/* Apply false easting/northing */

    x = x + 2000000.0
    y = y + 2000000.0

  elseif (latitude .LT. -80.0) then

    !/* use Universal Polar Stereographic Projection (south polar aspect) */

    phi = -phi
    lambda = -lambda
    lambda0 = -lambda0

    k0 = 0.994

    t = sqrt (((1.0 - sin (phi)) / (1.0 + sin (phi))) * &
         ( ( (1.0 + e * sin (phi)) / (1.0 - e * sin (phi)) ** e) ) )
    rho = 2.0 * a * k0 * t / sqrt ( ((1+e) ** (1+e)) * ((1-e) ** (1-e)) )
    !!! Not needed (dhg) m = cos (phi) / sqrt (1.0 - e2 * sin (phi) * sin (phi))

    x = rho * sin (lambda - lambda0)
    y = -rho * cos (lambda - lambda0)
    !!! Not needed (dhg) k = rho * a * m

    x = -x
    y = -y

    !/* Apply false easting/northing */

    x = x + 2000000.0
    y = y + 2000000.0

  else

    !/* Use UTM */

    !/* set scale on central median (0.9996 for UTM) */
    
    k0 = 0.9996

    mm = a * ((1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0) * phi - &
          (3.0*e2/8.0 + 3.0*e4/32.0 + 45.0*e6/1024.0) * sin (2.0*phi) + &
          (15.0*e4/256.0 + 45.0*e6/1024.0) * sin (4.0*phi) - &
          (35.0*e6/3072.0) * sin (6.0*phi))

    mm0 = a * ((1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0) * phi0 - &
           (3.0*e2/8.0 + 3.0*e4/32.0 + 45.0*e6/1024.0) * sin (2.0*phi0) + &
           (15.0*e4/256.0 + 45.0*e6/1024.0) * sin (4.0*phi0) - &
           (35.0*e6/3072.0) * sin (6.0*phi0))

    aa = (lambda - lambda0) * cos(phi)
    aa2 = aa * aa
    aa3 = aa2 * aa
    aa4 = aa2 * aa2
    aa5 = aa4 * aa
    aa6 = aa3 * aa3

    ep2 = e2 / (1.0 - e2)
    nn = a / sqrt (1.0 - e2 * sin (phi) * sin (phi))
    tt = tan (phi) * tan (phi)
    cc = ep2 * cos (phi) * cos (phi)

    !!! Not needed (dhg) k = k0 * (1 + (1+cc)*aa2/2 + (5-4*tt+42*cc+13*cc*cc-28*ep2) * aa4 / 24.0 + &
    !!! Not needed (dhg)          (61-148*tt+16*tt*tt) * aa6 / 720.0)
    x = k0 * nn * (aa + (1-tt+cc) * aa3 / 6 + &
              (5-18*tt+tt*tt+72*cc-58*ep2) * aa5 / 120.0)
    y = k0 * (mm - mm0 + nn * tan (phi) * &
             (aa2 / 2 + (5-tt+9*cc+4*cc*cc) * aa4 / 24.0 + &
         (61 - 58*tt + tt*tt + 600*cc - 330*ep2) * aa6 / 720))

    !/* Apply false easting and northing */

    x = x + 500000.0
    if (y .LT. 0.0) then
       y = y + 10000000.0
    endif
  endif

  !/* Set entries in UTM structure */

  utm_x = x
  utm_y = y

  !/* done */

  return
  end

!*************************************************************************
subroutine utm2ll (utm_x, utm_y, longitude, latitude, grid_zone, datum)

IMPLICIT NONE

real (kind=8) utm_x, utm_y
real (kind=8) latitude, longitude
integer       grid_zone(2)
integer       datum

  integer ierr
  real (kind=8)  a, b, f, e, e2, e4, e6, e8
  real (kind=8)  lambda0, x, y, k0, rho, t, chi, phi, phi1, phit
  real (kind=8)  lambda, phi0, e1, e12, e13, e14
  real (kind=8)  mm, mm0, mu, ep2, cc1, tt1, nn1, rr1
  real (kind=8)  dd, dd2, dd3, dd4, dd5, dd6

   real (kind=8) M_PI
!!!   parameter (M_PI = 3.141592654)
   real (kind=8) LOWER_EPS_LIMIT
   parameter (LOWER_EPS_LIMIT = 1.0e-14)
   real (kind=8) M_PI_2

   integer CLARKE_1866_DATUM
   parameter (CLARKE_1866_DATUM = 1)
   integer GRS_80_DATUM
   parameter (GRS_80_DATUM = 2)
   integer WGS_84_DATUM
   parameter (WGS_84_DATUM = 3)

!---------------------------------------------------------------------------


   m_pi = ACOS (-1.0)

   M_PI_2 = M_PI * 2.0

  !/* Converts UTM to lat/long, using the specified datum */

  if (datum == CLARKE_1866_DATUM) then      ! CLARKE_1866_DATUM:
    a = 6378206.4
    b = 6356583.8
  elseif (datum == GRS_80_DATUM) then       ! GRS_80_DATUM:
    a = 6378137
    b = 6356752.3
  elseif (datum == WGS_84_DATUM) then       ! WGS_84_DATUM:
    a = 6378137.0             !/* semimajor axis of ellipsoid (meters) */
    b = 6356752.31425         !/* semiminor axis of ellipsoid (meters) */
  else
    write (*,*) 'Unknown datum: ', datum
    return
  endif

  !/* Calculate flatness and eccentricity */

  f = 1.0 - (b / a)
  e2 = (2.0 * f) - (f * f)
  e = sqrt (e2)
  e4 = e2 * e2
  e6 = e4 * e2
  e8 = e4 * e4

  !/* Given the UTM grid zone, generate a baseline lambda0 */

  call get_lambda0 (grid_zone, lambda0, ierr)
  if (ierr .NE. 0) then
    write (*,*) 'Unable to translate UTM to LL'
    return
  endif

  latitude = (FLOAT (grid_zone(2)) * 8.0) - 80.0

  !/* Take care of the polar regions first. */

  if (latitude .GT. 84.0) then !/* north polar aspect */

    !/* Subtract the false easting/northing */

    x = utm_x - 2000000.0
    y = utm_y - 2000000.0

    !/* Solve for inverse equations */

    k0 = 0.994
    rho = sqrt (x*x + y*y)
    t = rho * sqrt ( ((1+e) ** (1+e)) * ((1-e) ** (1-e)) ) / (2*a*k0)

    !/* Solve for latitude and longitude */

    chi = M_PI_2 - 2 * atan (t)
    phit = chi + (e2/2 + 5*e4/24 + e6/12 + 13*e8/360) * sin(2*chi) + &
                 (7*e4/48 + 29*e6/240 + 811*e8/11520) * sin(4*chi) + &
                 (7*e6/120 + 81*e8/1120) * sin(6*chi) + &
                 (4279*e8/161280) * sin(8*chi)

    do while (ABS (phi-phit) .GT. LOWER_EPS_LIMIT)
      phi = phit
      phit = M_PI_2 - 2 * atan ( t * (((1 - e * sin (phi)) / (1 + e * sin (phi))) ** (e / 2)) )
    enddo

    lambda = lambda0 + atan2 (x, -y)

  elseif (latitude .LT. -80.0) then !/* south polar aspect */

    !/* Subtract the false easting/northing */

    x = -(utm_x - 2000000)
    y = -(utm_y - 2000000)

    !/* Solve for inverse equations */

    k0 = 0.994
    rho = sqrt (x*x + y*y)
    t = rho * sqrt ( ((1+e) ** (1+e)) * ((1-e) ** (1-e)) ) / (2*a*k0)

    !/* Solve for latitude and longitude */

    chi = M_PI_2 - 2 * atan (t)
    phit = chi + (e2/2 + 5*e4/24 + e6/12 + 13*e8/360) * sin (2*chi) + &
           (7*e4/48 + 29*e6/240 + 811*e8/11520) * sin (4*chi) + &
           (7*e6/120 + 81*e8/1120) * sin (6*chi) + &
           (4279*e8/161280) * sin (8*chi)

    do while (ABS (phi-phit) .GT. LOWER_EPS_LIMIT)
      phi = phit;
      phit = M_PI_2 - 2 * atan (t * ( ((1-e*sin(phi)) / (1+e*sin(phi)) ) ** (e/2)))
    enddo

    phi = -phi
    lambda = -(-lambda0 + atan2 (x , -y))

  else

    !/* Now take care of the UTM locations */

    k0 = 0.9996

    !/* Remove false eastings/northings */

    x = utm_x - 500000.0
    y = utm_y

    if (latitude .LT. 0.0) then  !/* southern hemisphere */
      y = y - 10000000.0
    endif

    !/* Calculate the footpoint latitude */

    phi0 = 0.0
    e1 = (1.0 - sqrt (1.0-e2)) / (1.0 + sqrt (1.0-e2))
    e12 = e1 * e1
    e13 = e1 * e12
    e14 = e12 * e12

    mm0 = a * ((1.0-e2/4.0 - 3.0*e4/64.0 - 5.0*e6/256.0) * phi0 - &
           (3.0*e2/8.0 + 3.0*e4/32.0 + 45.0*e6/1024.0) * sin (2.0*phi0) + &
           (15.0*e4/256.0 + 45.0*e6/1024.0) * sin (4.0*phi0) - &
           (35.0*e6/3072.0) * sin (6.0*phi0))
    mm = mm0 + y/k0;
    mu = mm / (a * (1.0-e2/4.0-3.0*e4/64.0-5.0*e6/256.0))

    phi1 = mu + (3.0*e1/2.0 - 27.0*e13/32.0) * sin (2.0*mu) + &
           (21.0*e12/16.0 - 55.0*e14/32.0) * sin (4.0*mu) + &
           (151.0*e13/96.0) * sin (6.0*mu) + &
           (1097.0*e14/512.0) * sin (8.0*mu)

    !/* Now calculate lambda and phi */

    ep2 = e2 / (1.0 - e2)
    cc1 = ep2 * cos (phi1) * cos (phi1)
    tt1 = tan (phi1) * tan (phi1)
    nn1 = a / sqrt (1.0 - e2 * sin (phi1) * sin (phi1))
    !!!DHG Old Code rr1 = a * (1.0 - e2) / ((1.0 - e2 * sin (phi) * sin (phi)) ** 1.5)
    !!!DHG L.Dozier's fix is next
    rr1 = a * (1.0 - e2) / ((1.0 - e2 * sin (phi1) * sin (phi1)) ** 1.5)
    dd = x / (nn1 * k0)

    dd2 = dd * dd
    dd3 = dd * dd2
    dd4 = dd2 * dd2
    dd5 = dd3 * dd2
    dd6 = dd4 * dd2

    phi = phi1 - (nn1 * tan (phi1) / rr1) * &
          (dd2/2.0 - (5.0+3.0*tt1+10.0*cc1-4.0*cc1*cc1-9.0*ep2) * dd4 / 24.0 + &
          (61.0+90.0*tt1+298.0*cc1+45.0*tt1*tt1-252.0*ep2-3.0*cc1*cc1) * dd6 / 720.0)
    lambda = lambda0 + &
             (dd - (1.0+2.0*tt1+cc1) * dd3 / 6.0 + &
             (5.0-2.0*cc1+28.0*tt1-3.0*cc1*cc1+8.0*ep2+24.0*tt1*tt1) * dd5 / 120.0) / cos (phi1)
  endif

  !/* Convert phi/lambda to degrees */
  
  latitude = phi * 180.0 / M_PI
  longitude = lambda * 180.0 / M_PI
  
  !/* All done */

  return
  end

!*************************************************************************
!/* END OF CODE
! * Peter Daly
! * MIT Ocean Acoustics
! */
! *************************************************************************

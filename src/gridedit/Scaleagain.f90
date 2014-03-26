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

      character(4) cmpstr
      character(2) zonestr
      character(80) ans,text
      real :: tmpx, tmpy, gxmax, gxmin, gxc
      integer :: tmpint,cmp,zone

      logical success


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

!     Find zone
      zone = FLOOR(gxc / 6) + 1
      IF( zone.lt.1 ) then
        zone = FLOOR((gxc + 180) / 6) + 1
      ENDIF
      write(zonestr,'(I0.2)') zone

!     Find central meridan
      cmp = (FLOOR(gxc / 6) * 6) + 3
      write(cmpstr,'(I4)') cmp

!     Prompt user for central meridan or UTM zone number
      success = .false.
      do while(.not.success)
        call PigPrompt('Enter UTM central meridan ('//cmpstr &
            //'?) or zone number in format Z** (Z'//zonestr//'?): ',ans)

        IF (ans(:1).eq.'Z') then ! Zone input - calculate central meridan
          read(ans(2:3),*)tmpint

          cmp = nint(tmpint*6.0-3) - 180
          success = .true.
        ELSE ! Not ZONE input
          call PigReadReal(ans, tmpx, Success)
          cmp = nint(tmpx)
        ENDIF

      enddo


!     Loop through all nodes and transform from lat/lon to UTM
      DO JJ = 1, ITOT
        CALL GEOUTM(to_radians(dyray(jj)),to_radians(dxray(jj)),to_radians(real(cmp)),tmpy,tmpx,tmpint)
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
!     Inverse transformation
!

      success = .false.
      do while(.not.success)
        call PigPrompt('Enter UTM central meridan or zone number in format Z**: ',ans)

        IF (ans(:1).eq.'Z') then
          read(ans(2:3),*)tmpint
          cmp = nint(tmpint*6.0-3) - 180
          success = .true.
        ELSE
          call PigReadReal(ans, tmpx, Success)
          cmp = nint(tmpx)
        ENDIF
      enddo


!     Loop through all nodes and transform from UTM to lat/lon

      DO JJ = 1, ITOT

        CALL UTMGEO(tmpy,tmpx,to_radians(real(cmp)),dyray(jj),dxray(jj),tmpint)
        dyray(jj) = to_degrees(tmpy)
        dxray(jj) = to_degrees(tmpx)
        
      enddo

!     Redraw fullsize
      xmin = minval(dxray(1:itot))
      xmax = maxval(dxray(1:itot))
      ymin = minval(dyray(1:itot))
      ymax = maxval(dyray(1:itot))
      call fullsize(xmin,ymin,xmax,ymax)

      END



!***************************************************************

      SUBROUTINE GEOUTM(LAT,LONGP,CMP,NORTH,EAST,ZONE)

!
!    COPYRIGHT 1991, DEPARTMENT OF FISHERIES AND OCEANS,
!            GOVERNMENT OF CANADA.
!    INSTITUTE OF OCEAN SCIENCES, SIDNEY, B.C.
!
!    SOFTWARE PRODUCED OR MODIFIED UNDER CONTRACT BY 
!            CHANNEL CONSULTING, 2167 GUERNSEY ST., 
!            VICTORIA, B.C., CANADA.
!            (604) 598-9500
!
!    AUTHOR          :    ADRIAN DOLLING, but see below.
!    PROJECT         :    fractals
!    SCIENTIFIC AUTH :    Rick Thomson
!    DATE CREATED    :    25 June 1991
!    MODULE NAME     :    geoutm
!    PURPOSE         :    Convert geographic coordinates to UTM coordinates.
!   No changes here, just adding documentation to
!   routine inherited from Sherman Oraas (CHS)
!   ONE CHANGE INTRODUCED:  Original routine had sign
!   convention localized for W.Coast N. Am.  The longitude
!   increased in value going to the west.  Normal convention
!   is to increase to the east.  The change here leaves the
!   following convention:
!       latitude  : N is +ve, S is -ve
!       longitude : E is +ve, W is -ve.
!    CAUTIONS      :  This routine uses the central meridian as supplied.
!   the utm grid has a set of standard zones, which are at
!   6 degree intervals.  The returned zone is the zone number
!   Zone 1 is centered on -177 deg and covers -180 to -174 
!   deg.  Zone numbers increase by 1 for each 6 degree zone
!   to the east.
!    METHODS         :
!    EXTERNAL REFS.  :  US Army technical manual TM 5-241-8.  
!   Universal Transverse Mercator Grid.  1958.
!
!modification history:
!code    date        author        purpose
!agd01   25 june 91  agd     remove implicit typing
!agd02   28June 91   agd     Modify sign convention as above
!agd03   ?       agd     Diagnostic printing (normally commented out)
!agd04   ?       agd     Modified calculation of zone number. Reason
!         unknown, but related to precision/rounding.
!         (agd 93/oct/18)
!
!
!
! GEOGRAPHI ! TO TRANSVERSE MERCATOR POSITIONS
!    
! FORMULAS FROM GEODESY, THIRD EDITION, BY G.BOMFORD
!
! WRITTEN BY KALMAN CZOTTER JULY 1978
!

!***************************************************************

!   Changes by Kristoffer Lorentsen,
!   Cascadia Coast Research
!   March 2014

!   Changes:
!      Removed input parameters for a nd f 


!***************************************************************


!agd01 IMPLICIT READ (A-Z)
!agd01 INTEGER ZONE,ICM,IMIN
      INTEGER ZONE !output - UTM zone
!agd01 - start added definitions
      REAL lat !radians north of equator - input
      REAL longp !radians east of Greenwich - input
      REAL long !radians west of Greenwich - internal
      REAL cmp !radians - input central meridian 
   !(longitude) east of Greenwich
      REAL cm !radians - internal central meridian 
   !(longitude) west of Greenwich
      REAL north !?? - output northing
      REAL east !?? - output easting
      REAL :: a = 6378137.000 !metres - ellipse semi-major axis - input - use 6378137.000
      REAL :: f = 0.003352810681 !flatenning ... input - use 0.003352810681 - is the same
   !as (1-eccentricity)
!
!to obtain ellipse and flatenning parameters, run the program
!ccs$hydrog:[oraas1.microvax_dua0.prod]convert.exe
!and select the NAD83 geoid.
!
!agd01 - end of input/output parameters
      REAL M0 !
      REAL EC2 !
      REAL EC4 !
      REAL EC6 !
      REAL EP !
      REAL V !
      REAL VP !
      REAL VP2 !
      REAL A0 !
      REAL A2 !
      REAL A4 !
      REAL A6 !
      REAL M !
      REAL W !
      REAL W2 !
      REAL W3 !
      REAL W4 !
      REAL W5 !
      REAL W6 !
      REAL W7 !
      REAL W8 !
      REAL T !
      REAL T2 !
      REAL T4 !
      REAL T6 !
      REAL VSIN     !
      REAL NORTH1     !
      REAL NORTH2     !
      REAL NORTH3     !
      REAL NORTH4     !
      REAL EAST1     !
      REAL EAST2     !
      REAL EAST3     !
      REAL sec !seconds - internal - UTM zone central meridian ?
! REAL rad_to_deg
!agd01 - end added definitions
      INTEGER ICM !degrees - internal - UTM zone central meridian ?
      INTEGER IMIN !minutes - internal - UTM zone central meridian ?
!
! integer nint
!agd02 start
 !change coordinate system from +ve E to +ve W.
      cm = - cmp
      long = -longp
!agd02 end

      M0=.9996
      EC2=F*(2-F)
      EC4=EC2**2
      EC6=EC2**3
      EP=EC2/(1-EC2)
      V=A*(1+.5*EC2*SIN(LAT)**2+3/8*EC4*SIN(LAT)**4 &
        +5/16*EC6*SIN(LAT)**6)
      VP=1+EP*COS(LAT)**2
      VP2=VP*VP
      A0=1-1/4*EC2-3/64*EC4-5/256*EC6
      A2=3/8*(EC2+1/4*EC4+15/128*EC6)
      A4=15/256*(EC4+3/4*EC6)
      A6=35/3072*EC6
      M=A*(A0*LAT-A2*SIN(2*LAT)+A4*SIN(4*LAT) &
        -A6*SIN(6*LAT))
      W=CM-LONG
      W2=W*W
      W3=W2*W
      W4=W3*W
      W5=W4*W
      W6=W5*W
      W7=W6*W
      W8=W7*W
      T=SIN(LAT)/COS(LAT)
      T2=T*T
      T4=T2**2
      T6=T2**3
      VSIN=V*SIN(LAT)
      NORTH1=VSIN*W2/2*COS(LAT)
      NORTH2=VSIN*W4/24*COS(LAT)**3*(4*VP2+VP-T2)
      NORTH3=VSIN*W6/720*COS(LAT)**5*(8*VP**4*(11-24*T2) &
        -28*VP**3*(1-6*T2)+VP2*(1-32*T2)-VP*(2*T2)+T4)
      NORTH4=VSIN*W8/40320*COS(LAT)**7*( &
        1385-3111*T2+543*T4-T6)
      NORTH=M0*(M+NORTH1+NORTH2+NORTH3+NORTH4)
      EAST1=V*W*COS(LAT)+V*W**3/6*COS(LAT)**3*(VP-T2)
      EAST2=V*W**5/120*COS(LAT)**5*(4*VP**3*(1-6*T2) &
        +VP2*(1+8*T2)-VP*(2*T2)+T4)
      EAST3=V*W7/5040*COS(LAT)**7*(61-479*T2+179*T4-T6)
      EAST=M0*(EAST1+EAST2+EAST3)
      EAST=500000+EAST
      CALL RADDMS(CM,ICM,IMIN,SEC)
! ZONE=(183-ICM)/6.
      ZONE=ifix(float(183-ICM)/6.) !agd04
!agd03
! rad_to_deg = 57.295779513082321D0
! print  *,cmp*rad_to_deg,zone,cm*rad_to_deg,icm,imin,sec
! if  (nint(cmp*rad_to_deg).ne.(-183+6*zone))
!    1   print *,'Warning: central meridian not a standard meridian',
!    1   nint(cmp*rad_to_deg),' should use cm=',-183+6*zone
!agd03
      RETURN
      END


!***************************************************************


      SUBROUTINE UTMGEO(LAT,LONGp,CMp,NORTH,EAST,ZONE)
!      Purose: convert from UTM to GEO
!      For information, look at GEOUT description above.
!
!      Copied from TriGrid Code March 2014 by Kristoffer Lorentsen,
!      Cascadia Coast Research Ltd
!
!      TRANSVERSE MERCATOR TO GEOGRAPHIC POSITIONS
!      
!      FORMULAS FROM GEODESY, THIRD EDITION, BY G.BOMFORD
!
!      WRITTEN BY KALMAN CZOTTER SEPTEMBER 1978
!
!agd 6 Nov 97 - added explicit typing
      IMPLICIT REAL (A-Z)
      INTEGER ZONE
      REAL LAT, LONGP, CMP, NORTH, EAST
      REAL CM, SMALL, M0, M03, M05, M07, EC2, EC4, EC6, E, E2, E3
      REAL E4, E5, E6, A0, A2, A4, A6, M, LAT0, V, V3, V5
      REAL W, W2, W3, W4, T, T2, T4, T6
      REAL SLAT, TM0P, LAT1, LAT2, LAT3, LAT4, LONG1, LONG2, LONG3
      REAL LONG, LATP, P
      REAL :: a = 6378137.000 !metres - ellipse semi-major axis - input - use 6378137.000
      REAL :: f = 0.003352810681 !flatenning ... input - use 0.003352810681 - is the same

!cagd01-start
      cm = -cmp
!cagd01-end


      SMALL=1D-11

      M0=.9996
      M03=M0**3
      M05=M0**5
      M07=M0**7
      EC2=F*(2-F)
      EC4=EC2**2
      EC6=EC2**3
      E=EAST-5E5
      E2=E*E
      E3=E**3
      E4=E**4
      E5=E**5
      E6=E**6
      A0=1-1/4*EC2-3/64*EC4-5/256*EC6
      A2=3/8*(EC2+1/4*EC4+15/128*EC6)
      A4=15/256*(EC4+3/4*EC6)
      A6=35/3072*EC6
      LATP=0
      M=NORTH/M0

!      ITERATION TO CALCULATE LATITUDE PRIME

99      LAT0=LATP
      LATP=(M/A+A2*SIN(2*LAT0)-A4*SIN(4*LAT0) &
         +A6*SIN(6*LAT0))/A0
      IF(ABS(LATP-LAT0).GT.SMALL) GO TO 99
!      
!      CALCULATES CONSTANTS FOR PRIME LATITUDE

      V=A*(1+.5*EC2*SIN(LATP)**2+3/8*EC4* &
         SIN(LATP)**4+5/16*EC6*SIN(LATP)**6)
      V3=V**3
      V5=V**5
      P=A*(1-EC2)*(1+3/2*EC2*SIN(LATP)**2+ &
         15/8*EC4*SIN(LATP)**4+35/16*EC6*SIN(LATP)**6)
      W=V/P
      W2=W*W
      W3=W**3
      W4=W2*W2
      T=SIN(LATP)/COS(LATP)
      T2=T**2
      T4=T**4
      T6=T**6
      SLAT=1/COS(LATP)
      TM0P=T/(M0*P)

!      CALCULATE LATITUDE

      LAT1=TM0P*E2/(2*M0*V)
      LAT2=TM0P*E4/(24*M03*V3)*(-4*W2+9*W* &
         (1-T2)+12*T2)
      LAT3=TM0P*E6/(720*M05*V5)*(8*W4*(11-24*T2)- &
         12*W3*(21-71*T2)+15*W2*(15-98*T2+15*T4)+ &
         180*W*(5*T2-3*T4)+360*T4)
      LAT4=TM0P*(E/V)**7*E/(40320*M07)* &
         (1385+3633*T2+4095*T4+1575*T6)
      LAT=LATP-LAT1+LAT2-LAT3+LAT4

!      CALCULATES LONGITUDE

      LONG1=SLAT*E/(M0*V)-SLAT*E3/(6*M03*V3)*(W+2*T2)
      LONG2=SLAT*E5/(120*M05*V5)*(-4*W3*(1-6*T2)+ &
         W2*(9-68*T2)+72*W*T2+24*T4)
      LONG3=SLAT*(E/V)**7/(5040*M07)*(61+662*T2+1320*T4+720*T6)
      LONG=CM-(LONG1+LONG2-LONG3)

!      CALCULATES ZONE

!cagd01-start
      longp = -long
!cagd01-end
      ZONE=INT((183-CM*57.29577951)/6)
      RETURN
      END



!***************************************************************

      FUNCTION to_radians(deg)
!
!       Convert from degrees to radians
!
        REAL deg
        REAL :: pi
        pi = 3.1415926536
        to_radians = deg*pi/180.0
      END FUNCTION to_radians

!***************************************************************

      FUNCTION to_degrees(rad)
!
!       Convert from radians to degrees
!
        REAL rad
        REAL :: pi
        pi = 3.1415926536
        to_degrees = (rad*180.0)/pi
      END FUNCTION to_degrees


!***************************************************************

      SUBROUTINE RADDMS (RAD,DEG,MIN,SEC)

!     Copied from TriGrid code to be used with UTMGEO/GEOUTM

        REAL RAD
        INTEGER DEG,MIN
        REAL SEC
!
!	RADIANS ARE CONVERTED TO DEGREES, MINUTES, SECONDS
!
        REAL SMALL
        REAL FRC
        DATA SMALL/1D-8/
!
        FRC=RAD*57.295779513082321
        FRC=FRC+360.0
        DEG=INT(FRC+SMALL)
        FRC=(FRC-DEG)*60.0+SMALL
        MIN=INT(FRC)
        SEC=(FRC-MIN)*60.0
        DEG=MOD(DEG,360)
      END

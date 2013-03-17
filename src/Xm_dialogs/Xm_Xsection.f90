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

      SUBROUTINE XSection( Etype, Quit )

! PURPOSE: To control the information screen in the right hand panel for 
!          entering the cross section parameters, 
!          and to perform the interpolation.
!   GIVEN: In Common in file xseccom.inc;
! RETURNS: In Common XSecCom;
!            XSFname = name of file to select from,
!            sdist = cross section sample distance, if selecting by distance,
!            srate = number of points in cross section, if by points,
!            sdist2= downstream sample distance, if selecting by distance,
!            srate2= number of points between cross sections, if by points,
!            Nth = TRUE if selection by every nth point,
!                = FALSE if selection by distance between points.
!            Nth2= TRUE if selection by every nth point,
!                = FALSE if selection by distance between points.
! EFFECTS: An entry screen is created in the right hand panel for the user
!          to enter parameters. Default values 
!          for all parameters are given initially. Interpolation may be 
!          performed.
!----------------------------------------------------------------------*

      use MainArrays

! - PASSED PARAMETERS
        integer AutoGenFlag, EType
        logical Quit

! - "INCLUDES"
      include '../includes/defaults.inc'
      include '../includes/graf.def'

!----------------------------------------------------------------------
!       - XSecCom stores parameters set in XSection.
      CHARACTER*80 :: XSFname='NONE'
      REAL :: sdist=1., sdist2=1.
      integer :: srate=3, srate2=1
      LOGICAL :: Nth=.true., Nth2=.true.
      COMMON /XSecCom/ XSFname,sdist,sdist2,srate,srate2,Nth,Nth2

!       - XSecCom stores parameters set in XSection.
!      CHARACTER*80 XSFname
!      REAL sdist, sdist2
!      integer srate, srate2
!      LOGICAL Nth, Nth2
!      COMMON /XSecCom/ XSFname, sdist, sdist2, srate, srate2,Nth, Nth2

!----------------------------------------------------------------------

! - LOCAL VARIABLES
      CHARACTER*80 cstr, Fname, ans
      CHARACTER*1 vartype
      REAL dumr
      real :: defsdist=1.0
      integer fnlen
      integer, save :: defsrate=3, defsrate2=0
      LOGICAL, save :: filein=.FALSE.
      logical PigGetOpenFileName

      !------------------START ROUTINE---------------------------------------

      Quit = .FALSE.
      Fname = XSFname

!     - get input file name
      quit = .true.
      if(PigGetOpenFileName('Open XSection File', FName,&
                'XSec Files (*.xsc),*.xsc;All Files (*.*),*.*;')) then
        quit = .false.
        XSFname = Fname
        fnlen = len_trim(XSFname)
        filein = .TRUE.
      else
        return
      ENDIF

!      distance sampling does not work
!      call PigMessageYesNo ('Sample by number (yes) or distance (no)? ',ans)
      ans = 'Y'

      if(ans(1:1).eq.'Y') then
!             - get srate
        cstr = 'Enter number of points ACROSS section:'
        vartype = 'I'
        call InputRealInt( srate, dumr, vartype, cstr )
        IF ( vartype .eq. 'D' ) THEN
          srate = defsrate
        ENDIF
        srate = max(2,srate)
        Nth = .TRUE.
        
      else
!             - get sdist
         cstr = 'Enter Distance Between ReSelected Points:'
         vartype = 'R'
         call InputRealInt( dumi, sdist, vartype, cstr )
         IF ( vartype .eq. 'D' ) THEN
           sdist = defsdist
         ENDIF
         Nth = .FALSE.
       endif
        
      if(ans(1:1).eq.'Y') then
!             - get srate
        cstr = 'Enter number of points BETWEEN sections::'
        vartype = 'I'
        call InputRealInt( srate2, dumr, vartype, cstr )
        IF ( vartype .eq. 'D' ) THEN
          srate2 = defsrate2
        ENDIF
        srate2 = max(0,srate2)
        Nth2 = .TRUE.
            
      else
!             - get sdist
        cstr = 'Enter distance increment between sections:'
        vartype = 'R'
        call InputRealInt( dumi, sdist2, vartype, cstr )
        IF ( vartype .eq. 'D' ) THEN
          sdist2 = defsdist
        ENDIF
        Nth2 = .FALSE.
      endif
            
      call PigPrompt('Enter type: 0=nodes, 1=triangles, 2=quads', ans )
      read(ans,'(i1)') EType
      Etype = min(2,max(0,etype))

!             - "ACCEPT" button hit
      IF ( filein ) THEN
        call PigEraseMessage
        cstr = 'Reading File...[XSec] format.'
        call PigPutMessage ( cstr )
!         Fname = XSFname
        call ReadXSecFile ( Quit, nx, ny )
        call PigEraseMessage
        if(EType.eq.0) then
        elseif(EType.eq.1.and.itot.gt.3) then
          AutoGenFlag = 0 !.false.
          call Gridit2(mrec,itot,dxray,dyray,depth,code,nbtot,&
     &  nbtotr,NL,maxtri,TotTr,ListTr,TCode,TotBndys,TotIntBndys,&
     &  PtsThisBnd,Quit,AutoGenFlag)
        elseif(EType.eq.2) then
          call GenerateQuads( Quit, nx, ny )
        endif
        quit = .true.
      ELSE IF ( .NOT. filein ) THEN
        cstr = 'Please Specify a File Name.'
        call PigMessageOK ( cstr,'XSection' )
      ENDIF
      Quit = .false.

      RETURN
      END

!-----------------------------------------------------------------------*

      subroutine ReadXSecFile( Quit, nx, ny )

      use MainArrays

!   Passed variables
      logical Quit
      integer nx, ny

      include '../includes/defaults.inc'
      INCLUDE '../includes/cntcfg.inc'

!----------------------------------------------------------------------

!       - XSecCom stores parameters set in XSection.
      CHARACTER*80 XSFname
      REAL sdist, sdist2
      integer srate, srate2
      LOGICAL Nth, Nth2
!      COMMON /XSecCom/ XSFname, sdist, sdist2, srate, srate2,Nth, Nth2
      COMMON /XSecCom/ XSFname,sdist,sdist2,srate,srate2,Nth,Nth2

!----------------------------------------------------------------------

      REAL    XMAX, YMAX, XMIN, YMIN

      real pdum(1001),yz(1001),ddum(1001),zorg(1001,50) &
     &,xc(1001),yc(1001),yzy(1001),slt(1001),yzx(1001),ww(1001)
      common/are/wt(1001),thet(1001),zz(1001,50),rds(1001) &
     &,tct(1001),xct(1001),yct(1001),twg(1001,3),zws(1001)
      integer maxnscs, maxnpx

!      print *,'enter discharge in m**3/s, slope'
!  
!  James P. Bennett 960415
!  Program uses tensioned splines to interpolate nint
!  cross-sections between each pair of the nxp input
!  cross-sections.  Each new x-section (including the
!  input sections) ends up with np equally spaced points.
!  Input file is xsdat and output is top1

!  Local variables
        integer indx
        character cstr*80
        integer ni,np,ncl,nxp,ns,nint,nscs,i,j,im,ip,iss
        real wt,thet,zz,rds
        real tct,xct,yct,twg,zws
        real spten,pi,xl,yl,xr,yr,zref,zro,sigm,dw,w,dep,slp,dx,dy
        real sigx,sigy,xint,stry,xx,yy,thz,rho,xo,yo,twgt,ds,s,dxs,dys
        real dt,ts,ss,tt,xf,xm,tp,xp,yp,zp
        integer fnlen
        data maxnscs,maxnpx/1001,50/

! -------------------START ROUTINE-----------------------------------

      GridSIndex = 9999999

      Quit = .TRUE.

      spten=.5
      ni=24
      fnlen = len_trim(XSFname)
      open(80,status='OLD',file=XSFname(:fnlen))
!open(81,file='xsout')
!open(82,file='top1')
      pi=acos(-1.)
!      print *,pi,ncl
!      read(80,*,err=9901)nxp,ns,nint,ni
      read(80,*,err=9901)nxp
      if(nxp.gt.maxnscs) then
        call PigMessageOK('Number of input crossections too large','ReadXsec') 
        go to 9903
      endif
      ni = srate-1
      nint = srate2
!  ns=number of points in input section, assumed here to be
!  constant, but easily read for each x-section by moving
!  its input to inside the do loop.
!      print *,nxp,ns,nint,ni
      nscs=(nxp-1)*(nint+1)+1
      np=ni+1
      if(nscs.gt.maxnscs) then
        call PigMessageOK('Interpolated number of crossections too large','ReadXsec') 
          go to 9903
      endif
      if(np.gt.maxnpx) then
        call PigMessageOK('Interpolated number of points in crossection too large','ReadXsec') 
          go to 9903
      endif
      ncl=(np+1)/2
      slt(1)=0.
      do 10 i=1,nxp
!  note that xl,yl are absolute coordinates for left-bank-end of
!  cross-section measurement reference line
        read(80,*,err=9901)ns,xl,yl,xr,yr,zref
        if(ns.gt.maxnpx) then
          call PigMessageOK('Number of points in input crossection too large','ReadXsec') 
          go to 9903
        endif
        if(i.eq.1)zro=zref
!  pdum is distance from right-bank end of reference line and
!  ddum is distance below zref
        read(80,*,err=9901)(pdum(j),ddum(j),j=1,ns)
!       print *,i,(pdum(j),ddum(j),j=1,ns)
        call splnfit(ns,spten,0.,0.,sigm,pdum,ddum,yz)
        dw=(pdum(ns)-pdum(1))/ni
!       print *,dw
        zorg(i,1)=zref-ddum(1)
        zorg(i,np)=zref-ddum(ns)
        do 50 j=2,ni
          w=(j-1)*dw
          call splndf(w          ,sigm,pdum,ddum,yz,dep  ,slp)
          zorg(i,j)=zref-dep
50      continue
!2       format(10f7.2)
!       print *,pdum(ns),depth,slp
        dx=xl-xr
        dy=yl-yr
!  store incremental width as width scale, ww=totwidth/ni
        ww(i)=sqrt(dx**2+dy**2)/ni
        xc(i)=(xl+xr)/2.
        yc(i)=(yl+yr)/2.
        if(i.gt.1)then
          im=i-1
          dx=xc(i)-xc(im)
          dy=yc(i)-yc(im)
          slt(i)=slt(im)+sqrt(dx**2+dy**2)
        end if
!       write(81,*)i,slt(i),xc(i),yc(i),ww(i)
!       write(*,*)i,slt(i),xc(i),yc(i),ww(i)
!      ii=ii+nint+1
10    continue
      call splnfit(nxp,spten,0.,0.,sigx,slt,xc,yzx)
      call splnfit(nxp,spten,0.,0.,sigy,slt,yc,yzy)
!  S&M(84) metric in this coordinate system
!     xmet=1./(1.-ww(itry)*(jtry-ncl)/rho)
      rho = 1.
      xint=1./(nint+1)
      stry=.01
      call LOCGEOM(stry,sigx,sigy,slt,xc,yc,yzx,yzy,xx,yy,thz) !,rho)
      tct(1)=stry
      xct(1)=xx
      yct(1)=yy
      thet(1)=thz
      rds(1)=rho
      wt(1)=ww(1)
      do j=1,np
        zz(nscs,j)=zorg(nxp,j)
        zz(1,j)=zorg(1,j)
      enddo
!  find thalweg length
      xo=xx
      yo=yy
      twgt=0.
      ds=slt(nxp)/500.
      s=ds
      do i=2,500
        call splndf(s,sigx,slt,xc,yzx,xx,dxs)
        call splndf(s,sigy,slt,yc,yzy,yy,dys)
        twgt=twgt+sqrt((xx-xo)**2+(yy-yo)**2)
        s=s+ds
        if(i.eq.500)s=s-.01
        xo=xx
        yo=yy
      enddo
!       print *,twgt,slt(nxp)
!  interpolate x-sections into equal increments along thalweg
      dt=twgt/(nscs-1)
      ts=0.
      ss=.01
      xo=xct(1)
      yo=yct(1)
      tt=dt
      ds=dt/100.
      im=1
      ip=2
      iss=0
      do 20 i=2,nscs
21      ss=ss+ds
        if(ss.ge.slt(nxp))then
          ss=slt(nxp)-.01
          iss=1
        end if
        call splndf(ss,sigx,slt,xc,yzx,xx,dxs)
        call splndf(ss,sigy,slt,yc,yzy,yy,dys)
        ts=ts+sqrt((xx-xo)**2+(yy-yo)**2)
        xo=xx
        yo=yy
        if(iss.gt.0)goto 22
        if(ts-tt)21,22,22
22      continue
!       print *,dt,ds,tt,ts
        if(ss.gt.slt(ip))then
          im=ip
          ip=ip+1
          goto 22
        endif
        call LOCGEOM(ss,sigx,sigy,slt,xc,yc,yzx,yzy,xx,yy,thz) !,rho)
        xf=(ss-slt(im))/(slt(ip)-slt(im))
        xm=1.-xf
        tct(i)=ss
        xct(i)=xx
        yct(i)=yy
        thet(i)=thz
        wt(i)=xf*ww(ip)+xm*ww(im)
        rds(i)=rho
        do 40 j=1,np
40      zz(i,j)=xf*zorg(ip,j)+xm*zorg(im,j)
!       write(81,1)i,ss,xx,yy,thz,rho,wt(i),zz(i,ncl)
!1       format(i3,8f9.3)
!       write(*,1)i,ss,xx,yy,thz,rho,wt(i),zz(i,ncl)
        tt=tt+dt
20    continue

      TotIntPts = 0
      TotBndys = 1
      PtsThisBnd(1) = 2*np + 2*nscs -4
      IF ( np*nscs .gt. Mrec ) THEN
        cstr = 'INPUT HAS TOO MANY NODES'
        call PigMessageOK ( cstr, 'ReadXsec' )
        Quit = .true.
        return
      ENDIF

      do 25 i=1,nscs
        tp=thet(i)+pi/2.
        dx=wt(i)*cos(tp)
        dy=wt(i)*sin(tp)
        do 26 j=1,np
          xp=xct(i)+(ncl-j)*dx
          yp=yct(i)+(ncl-j)*dy
          zp=zro-zz(i,j)
!  This version writes interpolated points for cross-section i
!  where cross-sections are in upstream to downstream order
!  at point j in x-y-z order where j increases right bank to left bank
!  x is  positive generally in the downstream direction and
!  y is  positive rt bank to lt bank 
!  zp is depth below the reference elev. for the first x-section
!  write(82,*)i,j,xp,yp,zp
          if(i.eq.1) then
            indx = j
            code(indx) = 1
          elseif(i.eq.nscs) then
            indx = 2*np + nscs - 1 - j
            code(indx) = 1
          elseif(j.eq.np) then
            indx = np + i - 1
            code(indx) = 1
          elseif(j.eq.1) then
            indx = 2*np + 2*nscs - 2 - i
            code(indx) = 1
          else
            TotIntPts = TotIntPts + 1
            indx = TotIntPts + PtsThisBnd(1)
            code(indx) = 0
          endif

          dxray(indx) = xp
          dyray(indx) = yp
          depth(indx) = zp
!          exist(indx) = .true.
26      continue
25    continue

      nx = np
      ny = nscs

      Quit = .false.
      TotCoords = PtsThisBnd(1) + TotIntPts
!      DispNodes = .true.
      itot = TotCoords
      nbtotr = 0

      close(80)

      xmin = minval(dxray(1:itot))
      xmax = maxval(dxray(1:itot))
      ymin = minval(dyray(1:itot))
      ymax = maxval(dyray(1:itot))
      call fullsize(xmin,ymin,xmax,ymax)

      !     - determine max/min depths (datatype 1)
      SMaxVal(1) = depth(1)
      SMinVal(1) = depth(1)
      do i=2,itot
        IF ( SMaxVal(1) .lt. Depth(i) ) THEN
        SMaxVal(1) = Depth(i)
        ENDIF
        IF ( SMinVal(1) .gt. Depth(i) ) THEN
        SMinVal(1) = Depth(i)
        ENDIF
      enddo

      return

9901  continue
!     error reading TotCoords
      close(80)
      call PigMessageOK('Error reading cross-section file','ReadXsec')
      Quit = .true.

9903  continue
!     error reading TotCoords
      close(80)
      Quit = .true.

      return
      end
!----------------------------------------------------------------------

      SUBROUTINE SPCCTR(i,im,pi2,c)

        integer i,im,ip
        real wt,thet,zz,rds,tct,xct,yct,twg,zws
        real pi2,c,tp,xctr,yctr

      common/are/wt(1001),thet(1001),zz(1001,50),rds(1001) &
     &,tct(1001),xct(1001),yct(1001),twg(1001,3),zws(1001)
!      common/are/wt(100),thet(100),zz(100,50),rds(100)
!     &,tct(100),xct(100),yct(100),twg(100,3)

      tp=thet(i)-pi2
      xctr=xct(i)+c*cos(tp)
      yctr=yct(i)+c*sin(tp)
      twg(i,3)=0.
      twg(i,1)=xctr
      twg(i,2)=yctr
      if(i.eq.im)return
      ip=i+1
      twg(i,3)=sqrt((twg(ip,1)-xctr)**2+(twg(ip,2)-yctr)**2)
      return
      end

!----------------------------------------------------------------------

      SUBROUTINE LOCGEOM(stry,sigx,sigy,slt,xc,yc,yzx,yzy,xx,yy,thz) !,rho)

!      real stry,sigx,sigy,xx,yy,thz,rho,dstry,dxs,dys,str,x2,y2,th2
      real stry,sigx,sigy,xx,yy,thz,dstry,dxs,dys,str,x2,y2,th2
      real slt(1001),xc(1001),yc(1001),yzx(1001),yzy(1001)

      dstry=.01
      call splndf(stry     ,sigx,slt,xc,yzx,xx,dxs)
      call splndf(stry     ,sigy,slt,yc,yzy,yy,dys)
      thz=atan2(dys,dxs)
      str=stry+dstry
      call splndf(str     ,sigx,slt,xc,yzx,x2,dxs)
      call splndf(str     ,sigy,slt,yc,yzy,y2,dys)
      th2=atan2(dys,dxs)
!     rho=dstry/(th2-thz)

      return
      end

!----------------------------------------------------------------------

      SUBROUTINE AREA(i,jf,zw,t,a,c)

      integer i, jf,jc,j,jm
      real zw,t,a,c
      real ww,thet,zz,rds,tct,xct,yct,twg,zws
      real wm,zm,zj,zd,tj,aj,cj

      common/are/ww(1001),thet(1001),zz(1001,50),rds(1001) &
     &,tct(1001),xct(1001),yct(1001),twg(1001,3),zws(1001)

!     common/are/ww(100),thet(100),zz(100,50),rds(100)

!1     format(13f6.2)
!     write(*,1)(zz(i,j),j=1,jf)
      t=0.
      a=0.
      c=0.
      wm=ww(i)
      jc=jf/2+1
      zm=zw-zz(i,1)
      do 10 j=2,jf
        zj=zw-zz(i,j)
        jm=j-1
        zd=abs(zj-zm)
        if(zj.ge.0.)then
          if(zm.ge.0.)then
            tj=1.
            aj=(zm+zj)/2.
            cj=(2.*zj+zm)/6.+aj*(jm-1)
            goto 20
          else
            tj=zj/zd
            aj=zj*tj/2.
            cj=aj*(jm-tj/3.)
            goto 20
          end if
        else
          if(zm.ge.0.)then
            tj=zm/zd
            aj=zm*tj/2.
            cj=aj*(jm-1.+tj/3.)
            goto 20
          else
            aj=0.
            tj=0.
            cj=0.
            goto 20
          end if
        end if
20      continue
        zm=zj
        a=a+aj
        t=t+tj
        c=c+cj
10    continue
      a=a*wm
      t=t*wm
      c=wm*(wm*c/a-jc+1)
!     print *,i,wm,t,a,c

      return
      end
!-----------------------------------------------------------------------*

      subroutine GenerateQuads(Quit,nx,ny)

      use MainArrays

! *** Passed variables
      logical Quit
      integer nx, ny

      include '../includes/defaults.inc'

      nbtotr = 4
      do i = 1, mrec
        do j=1,nbtot
          nL(j,i) = 0
        enddo
      enddo

! *** corner points
      nL(1,1) =  2
      nL(1,nx) =  nx+1
      nL(1,nx+ny-1) = nx+ny
      nL(1,2*nx+ny-2) = 2*nx+ny-1
      nL(2,1) =  2*nx+2*ny-4
      nL(2,nx) =  nx-1
      nL(2,nx+ny-1) = nx+ny-2
      nL(2,2*nx+ny-2) = 2*nx+ny-3
      if(ny.eq.2) then
        nL(1,2*nx+ny-2) = 1
      endif

! *** interior
      if(nx.gt.2.and.ny.gt.2) then
        do k=2,ny-1
          do j=2,nx-1
            i = 2*nx + 2*ny - 3 + (j-2) + (k-2)*(nx-2)
            nL(1,i) = i-nx+2
            nL(2,i) = i+1
            nl(3,i) = i+nx-2
            nl(4,i) = i-1
          enddo
        enddo
      endif

! *** sides
      if(nx.gt.2) then
        if(ny.eq.2) then
            do j=2,nx-1
            nL(1,j) = j+1
            nL(2,j) = j-1
            i= 2*nx + ny -j -1
            nL(1,i) = i+1
            nL(2,i) = i-1
            nL(3,j) = i
            nL(3,i) = j
          enddo
        else
            do j=2,nx-1
            nL(1,j) = j+1
            nL(2,j) = j-1
            ii = (j-2) + 2*nx + 2*ny -3
            nl(3,j) = ii
            nL(1,ii) = j
            i= j + nx + ny -2
            nL(1,i) = i+1
            nL(2,i) = i-1
            ii = -(j-2) + nx*ny
            nL(3,i) = ii
            nL(3,ii) = i
          enddo
        endif
      endif
      if(ny.gt.2) then
        if(nx.eq.2) then
          do j=3,ny
            nL(1,j) = j+1
            nL(2,j) = j-1
            i= 2*ny -j+3
            if(j.eq.3) then
              nL(1,i) = 1
            else
              nL(1,i) = i+1
            endif
            nL(2,i) = i-1
            nL(3,j) = i
            nL(3,i) = j
          enddo
        else
          do j=2,ny-1
            i = j+nx-1
            nL(1,i) = i+1
            nL(2,i) = i-1
            ii = (j-2)*(nx-2) + 2*nx + 2*ny -4 + nx - 2
            nl(3,i) = ii
            nL(2,ii) = i
            i= j + 2*nx + ny -3
            if(i.eq.2*nx+2*ny-4) then
              nL(1,i) = 1
            else
              nL(1,i) = i+1
            endif
            nL(2,i) = i-1
            ii = -(j-2)*(nx-2) + nx*ny - nx + 3
            nl(3,i) = ii
            nL(4,ii) = i
          enddo
        endif
      endif
      quit = .false.

      return
      end

!----------------------------------------------------------------------


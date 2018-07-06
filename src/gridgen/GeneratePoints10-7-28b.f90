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

! **********************************************************

      SUBROUTINE GenPoints(NPTS,N,X,Y,ncode,LIST,V,T,NTRI,ELIST,NCB,NCE, &
                              DMAX,XMIN,YMIN,AutoGenFlag)

!      PURPOSE:
!      --------
!
!      Driver routine for node insertions routines
!      Called only from CONTRI, just after initial triangulation of boundary nodes

      implicit none 

! *** passed variables
      integer :: n,ncb,nce, AutoGenFlag
      integer :: npts,ntri
      integer :: t(3,2*npts+1),v(4,2*npts+1),ncode(npts)
      integer :: list(npts), elist(2,n)
      real :: x(npts+3),y(npts+3)
      real :: DMAX,XMIN,YMIN      !  needed in s/r CreateClusterPoints

! *** local variables
      real, allocatable, save :: Reflength(:)
      logical :: Quit
      integer :: istat,np0

      if(.not.allocated(Reflength)) then
        ALLOCATE (RefLength(npts), STAT = istat )
        if(istat.ne.0) then
          call PigMessageOK('out of memory - Cannot allocate Reflength storage array','Reflength')
          AutoGenFlag = -1
          return
        endif
      endif

      if(AutoGenFlag.eq.1) then  !add a front 
        call NODEBFRONT2(NPTS,N,X,Y,ncode,DMAX,XMIN,YMIN,&
                         LIST,V,T,NTRI,ELIST,NCB,NCE,Reflength,AutoGenFlag)
      elseif(AutoGenFlag.eq.2) then  !add another front
        call NODEBFRONT2(NPTS,N,X,Y,ncode,DMAX,XMIN,YMIN,&
                         LIST,V,T,NTRI,ELIST,NCB,NCE,Reflength,AutoGenFlag)
      elseif(AutoGenFlag.eq.4) then  !add all fronts front
        AutoGenFlag = 1
        call NODEBFRONT2(NPTS,N,X,Y,ncode,DMAX,XMIN,YMIN,&
                         LIST,V,T,NTRI,ELIST,NCB,NCE,Reflength,AutoGenFlag)
        AutoGenFlag = 2
        do
          np0 = n
          call NODEBFRONT2(NPTS,N,X,Y,ncode,DMAX,XMIN,YMIN,&
                           LIST,V,T,NTRI,ELIST,NCB,NCE,Reflength,AutoGenFlag)
          if(n.le.np0) exit
          if(AutoGenFlag.ne.2) then
            call PigMessageOK('ERROR: Front too long, check for overrefinement','newnf')
            exit
          endif
        enddo
        deallocate(RefLength)
      elseif(AutoGenFlag.eq.3) then  !add points from clusters
!        call NODEBFRONT2(NPTS,N,X,Y,ncode,LIST,V,T,NTRI,ELIST,NCB,NCE,Reflength,AutoGenFlag)
        call MakeMeshAndClusters(N,NPTS,NTRI,ELIST,NCB,V,T,X,Y,DMAX,XMIN,YMIN,LIST,Reflength,Quit)
        deallocate(Reflength)
      endif

      return
      end

! **********************************************************

    subroutine NODEBFRONT2(NPTS,N,X,Y,ncode,DMAX,XMIN,YMIN,&
                           LIST,V,T,NTRI,ELIST,NCB,NCE,Reflength,AutoGenFlag)


!     PURPOSE:  Boundary front triangulation
!     --------
!
!      Given a set of boundary nodes, (with an initial boundary triangulation),
!    find the small angles between boundary nodes and find the number of 
!    triangles associated with this node. Add the extra node (or nodes) to 
!    construct the triangles. Use an 'advancing front' method to propagate inwards.

!      Multiple boundaries, ie islands are allowed. One option with
!    this version is to run for a specified number of iterations (found by trial
!    and error), then the remaining space filled by using cluster insertion. 
!
!     INPUT:
!     ------
!
!     NPTS   - Total number of points in data set
!     N      - Total number of points to be triangulated (N le NPTS)
!     X      - X-coords of all points in data set
!            - X-coord of point I given by X(I)
!            - If point is in LIST, coordinate must be normalised
!              such that X=(X-XMIN)/DMAX
!            - Last three locations are used to store x-coords of
!              supertriangle vertices in subroutine delaun
!     Y      - Y-coords of all points in data set
!            - Y-coord of point I given by Y(I)
!            - If point is in LIST, coordinate must be normalised
!              such that Y=(Y-YMIN)/DMAX
!            - Last three locations are used to store y-coords of
!              supertriangle vertices in subroutine delaun
!     V      - Vertex array for triangulation
!            - Vertices listed in anticlockwise sequence
!            - Vertices for triangle J are found in V(I,J) for I=1,2,3
!              and J=1,2,...,NTRI
!            - First vertex is at point of contact of first and third
!              adjacent triangles
!     T      - Adjacency array for triangulation
!            - Triangles adjacent to J are found in T(I,J) for I=1,2,3
!              J=1,2,...,NTRI
!            - Adjacent triangles listed in anticlockwise sequence
!            - Zero denotes no adjacent triangle
!     NTRI   - Number of triangles in triangulation, including those
!              formed with the supertriangle vertices
!            - NTRI = 2*N+1
!     LIST   - List of points to be triangulated
!
!    OUTPUT
!     ------
!     P      - point to be inserted
!     XP, YP - normalised coords of P
!     J      - triangle encompassing P
!     V1,V2,V3 - vertices of J

!     Local variables
! x = list; x-location of all nodes
! y = list; y-location of all nodes
! n = total number of all nodes
! nb = total number of boundary nodes (= inital front)
! f = list of nodes in ad. front
! nf = number of nodes in ad. front
! ii = index in f of current node
! p = number (value) of current node
! i = general index
! istart= node number for start of boundary segment
! iend = number of node for end of boundary segment

        
    implicit none 

! *** passed variables
      integer :: n, ncb, nce, AutoGenFlag
      integer :: npts,ntri,v1,v2,v3
      integer :: t(3,2*npts+1),v(4,2*npts+1),ncode(npts)
      integer :: list(npts), elist(2,n)
      real :: xp,yp,zp,zp0
      real :: x(npts+3),y(npts+3),DMAX,XMIN,YMIN
      real :: Reflength(npts)
    
!  *** local
      integer :: ncb00, ncb0 = 0
      integer :: j,jj,nn,n1,ia, istat,ErrorFlag
      integer ::  p,tp,i,ii,numinb
      integer ::  newnf, newnf0, newnbreak, breaki, fi
      integer, save :: nf, nbreak
      integer, allocatable, save :: break(:), f(:), oldf(:)
      integer :: newf(2*nce), newbreak(nce)
!      real, allocatable, save :: reflength(:)
      real :: angle, theta, alpha,vax,vbx,vay,vby,Lb,l,Lbavg,snum,Lavg,snum1
      real :: Lba,Lbb,Lbmin,Lbmax
      real :: xt,yt,dxt,dyt,ang,dxn,dyn,w,dx1,dy1
      real :: pi,distv1,distv2,distv3

!  *** used for ref grid stuff
      INTEGER :: nex,npx,i1
      REAL :: refdepth(1),factor
      Real :: d2r

      pi = 3.1415927
      d2r = pi/180.

!      ACTVPOLY=0
       
! *** Allocate arrays
      if(ncb0.eq.0) then         
        ALLOCATE (break(nce), f(2*nce), oldf(2*nce), STAT = istat )   !RefLength(npts), STAT = istat )
        if(istat.ne.0) then
          call PigMessageOK('out of memory - Cannot allocate front storage arrays','front')
          AutoGenFlag = -1
          return
        endif
        ncb0 = nce
      elseif(AutoGenFlag.eq.2.and.nce.ne.ncb0) then
        call PigPutMessage('ERROR: Grid has changed since last front')
        AutoGenFlag = -2
        return
      elseif(Autogenflag.eq.1) then
        ncb00 = ncb0
        ncb0 = nce
        if(nce.gt.ncb00) then
          if(allocated(break)) Deallocate(break)
          if(allocated(f)) deallocate(f)
          ALLOCATE (break(nce), f(2*nce), STAT = istat )
          if(istat.ne.0) then
            call PigMessageOK('out of memory - Cannot allocate front storage arrays','front')
            AutoGenFlag = -1
            return
          endif
        endif
!      elseif(Autogenflag.eq.3) then
!        return !already set up
      endif
      
      if(AutoGenFlag.eq.1) then !.or.AutoGenFlag.eq.3) then  !set reflength

        nbreak =1
        breaki = 1
        fi = 1
        f(1) = elist(1,1)
        break(1) = 1

! *** find boundary segments
        do i=2,nce 
          fi=fi+1
          f(fi)=elist(1,i)
          if (elist(2,i-1).ne.elist(1,i)) then
            if(i-1.gt.ncb) then
!              fi = fi+1
              f(fi) = elist(2,i-1)
              nn = fi-break(breaki)-1
              do ii=1,nn
                fi = fi+1
                f(fi) = f(fi-2*ii)
              enddo
              fi=fi+1
              f(fi)=elist(1,i)  
            endif
            breaki=breaki+1
            break(breaki)=fi
            nbreak=nbreak+1
          elseif (i.eq.nce.and.i.gt.ncb) then
            if(elist(2,i).ne.elist(1,f(break(breaki)))) then
              fi = fi+1
              f(fi) = elist(2,i)
            endif
            nn = fi-break(breaki)
            do ii=1,nn-1
              fi = fi+1
              f(fi) = f(fi-2*ii)
            enddo
          endif
        end do
        break(nbreak+1)=fi+1
        nf=fi

!  *** set reference length scale
        snum=0.
        Lbavg = 0.
        do nn=1,nbreak
          snum1=0.
          Lavg = 0.
          n1 = break(nn)
          nf = break(nn+1)-1
          do i=n1,nf
            if (i.eq.n1) then
              vax=x(f(nf))- x(f(i))
              vay=y(f(nf))- y(f(i))
              Lba=sqrt(vax**2 + vay**2)
              vbx=x(f(i+1))- x(f(i))
              vby=y(f(i+1))- y(f(i))
              Lbb=sqrt(vbx**2 + vby**2)
            elseif (i.eq.nf) then
!              vax=x(f(i-1))- x(f(i))
!              vay=y(f(i-1))- y(f(i))
              Lba = Lbb
              vbx=x(f(n1))- x(f(i))
              vby=y(f(n1))- y(f(i))
              Lbb=sqrt(vbx**2 + vby**2)
            else
!              vax=x(f(i-1))- x(f(i))
!              vay=y(f(i-1))- y(f(i))
              Lba = Lbb
              vbx=x(f(i+1))- x(f(i))
              vby=y(f(i+1))- y(f(i))
              Lbb=sqrt(vbx**2 + vby**2)
            end if
            Lb= 0.5*(Lba+Lbb)  !average
!            Lb = amax1(Lba,Lbb) !max
            RefLength(f(i)) = Lb
            snum1=snum1+1.
            Lavg = Lavg + Lb
            snum=snum+1.
            Lbavg = Lbavg + Lb
          enddo

          if(break(nn+1)-break(nn).ge.5) then !running average
            numinb = break(nn+1)-break(nn)
            do i=n1,nf
              Lbavg =0.
              Lbmin = Reflength(f(i))
              Lbmax = Reflength(f(i))
              do j=1,5
                jj = n1 + mod(i-n1+j-3+numinb,numinb)
                Lbavg = Lbavg + Reflength(f(jj))
                Lbmin = amin1(Lbmin,Reflength(f(jj)))
                Lbmax = amax1(Lbmax,Reflength(f(jj)))
              enddo
              Reflength(f(i)) = (Lbavg-Lbmin-Lbmax)/3.
            enddo
          else
            do i=n1,nf
              Reflength(f(i)) = Lavg/snum1  !avg by segments at node
            enddo
          endif
        enddo
!        Lbavg = Lbavg/snum  !global average
         
!         call CloseRefGrid (Quit)
!         call SetDefaultRefGrid(N, NTRI, X, Y, RefLength, V, DMAX, XMIN, YMIN)

                
! *** ref length scale set. Add new values as new nodes are created.
! *** an option here is to read a ref grid and scale it with Reflength.

      elseif(AutoGenFlag.eq.2) then ! add another front, set reflength
        call GetRefGridLimits( nex, npx )
        refdepth(1)=1.
        if(npx.gt.0) then
          i1 = 1
          do i=1,nf
            xp = dmax*x(f(i)) + xmin
            yp = dmax*y(f(i)) + ymin
            call InterpRefGrid2( xp, yp, zp )
!            factor = zp
            xp = dmax* x(oldf(i)) + xmin
            yp = dmax* y(oldf(i)) + ymin
            call InterpRefGrid2( xp, yp, zp0 )
            if(zp0.eq.0.) then
              factor = 1.
            else
              factor = zp/zp0
            endif
!            factor = factor/max(zp,1.)  !1.25
            factor = max(0.8,min(1.25,factor))
            RefLength(f(i)) = factor*RefLength(f(i))
          enddo
        endif
      else
        call PigMessageOK('Invalid Autogen option','GenPoints')
        return
      endif

! *** loop through nodes in front all in sequence
!        nf = nf0
      newnbreak = 1
      newbreak(newnbreak)=1
      newnf = 0
      do nn=1,nbreak
        newnf0 = newnf
        n1 = break(nn)
        nf = break(nn+1)-1
        if((nf-n1).lt.3) cycle
        do i=n1,nf
          if (i.eq.n1) then        !  coast, islands
            vbx=x(f(i+1))- x(f(i))
            vby=y(f(i+1))- y(f(i))
            if(AutoGenFlag.eq.1) then
              vax=x(f(nf))- x(f(i))
              vay=y(f(nf))- y(f(i))
            else
              vax=-vbx
              vay=-vby
            endif
          else if (i.eq.nf) then
            vax=x(f(i-1))- x(f(i))
            vay=y(f(i-1))- y(f(i))
            if(AutoGenFlag.eq.1) then
              vbx=x(f(n1))- x(f(i))
              vby=y(f(n1))- y(f(i))
            else
              vbx=-vax
              vby=-vay
            endif
          else
            vax=x(f(i-1))- x(f(i))
            vay=y(f(i-1))- y(f(i))
            vbx=x(f(i+1))- x(f(i))
            vby=y(f(i+1))- y(f(i))
          end if

          Lbavg = Reflength(f(i))  !nn)

          theta=atan2(vay,vax)-atan2(vby,vbx)
          if (theta.lt.0.) then
            theta=(2*pi)+theta
          elseif(theta.lt..1) then
            theta=2*pi
          end if
          angle=theta

          p=f(i) ! this is the number of the current node
          ii=i   ! this is the index in f of the current node

! *** number of triangles associated with this node
          tp=nint(angle/(60.*pi/180.))

! *** use this to write to trierrors.txt for debug
!          write(76,'(//,''iter: '',i4,''  node: '', i4,''  angle: '',f7.2,''  ntri: '',i4)')count,p,angle*180/pi,tp

!          if (angle.ge.270*pi/180.) then
!          ! angle greater than 270 deg - break
!            write(76,'('' Warning: Angle greater than 270 deg'')')
!            if (tp.gt.4) tp=4
!          end if

!          if(ii.gt.ncb) then
!            if (ii .eq. n1) tp = 6
!            if (ii .eq. nf) tp = 0
!          endif
      
          if (tp.le.1) then  ! we have 1 triangle, no nodes in newf

          else if (tp.eq.2) then    !we have 2 triangles, add 1  node to newf
        ! find location of new node by angles
        ! intermidate point t
            if (ii .eq. n1) then
              if(AutoGenFlag.eq.1) then
                xt=0.5*(x(f(nf))+x(f(ii+1)))
                yt=0.5*(y(f(nf))+y(f(ii+1)))
              else
                xt = x(f(ii))
                yt = y(f(ii))
              endif
            elseif (ii .eq. nf) then
              if(AutoGenFlag.eq.1) then
                xt=0.5*(x(f(ii-1))+x(f(n1)))
                yt=0.5*(y(f(ii-1))+y(f(n1)))
              else
                xt = x(f(ii))
                yt = y(f(ii))
              endif
            else
              xt=0.5*(x(f(ii-1))+x(f(ii+1)))
              yt=0.5*(y(f(ii-1))+y(f(ii+1)))
            endif
            dxt=xt-x(f(ii))
            dyt=yt-y(f(ii))
        ! new point

            L=Lbavg
            ang=atan2(dyt,dxt)
            dxn=L*cos(ang)
            dyn=L*sin(ang)
            xp=dxn+x(f(ii))
            yp=dyn+y(f(ii))

! *** find which triangle new node is in, hence dis to other nodes
            call FINDTRI(xp,yp,j,t,v,npts,ntri,V1,V2,V3,x,y,ncb)
            if (j.eq.0) goto 90        ! if outside domain then skip

            distv1= sqrt((x(v1)-xp)**2 + (y(v1)-yp)**2)
            distv2= sqrt((x(v2)-xp)**2 + (y(v2)-yp)**2)
            distv3= sqrt((x(v3)-xp)**2 + (y(v3)-yp)**2)
            if (amin1(distv1,distv2,distv3).lt.0.8*L) goto 90        ! too close, then skip 

            if(newnf+1.ge.2*nce) then !we have a problem Houston
              AutoGenFlag = 0
              return
            endif
            n=n+1
! *** put new node into newf
            newnf=newnf+1
            newf(newnf)=n
            oldf(newnf) = f(ii)

! *** add node to domain
            x(n)=xp
            y(n)=yp
! *** insert new pt
!            xp=x(n)
!            yp=y(n)

! *** set reflength at new node if no reference grid
            call InterpolateElement( npts,x,y,RefLength,v,xp,yp,zp,j)
            RefLength(n) = zp

            list(n)=n
!            call insertp(npts,n,x,y,v,t,ntri,v1,v2,v3,xp,yp,n,j)
            call insertp(npts,x,y,v,t,ntri,v1,v2,v3,xp,yp,n,j,ErrorFlag)

90    continue


           else if (tp.le.6) then        !we have 3 triangles, 2 add nodes

! work round from p-1, finding angle from +x axis
            if (ii .eq. n1) then
              dxt=x(f(nf))-x(f(ii))
              dyt=y(f(nf))-y(f(ii))
            else
              dxt=x(f(ii-1))-x(f(ii))
              dyt=y(f(ii-1))-y(f(ii))
            end if
            w=atan2(dyt,dxt)
            if (w.lt.0) then
              w=w+(2*pi)
            end if

! ***  loop over 2 pt to insert
            do ia=1,tp-1
              alpha = w-((float(ia)/float(tp))*angle)
              L=Lbavg

! *** adjusted point
              dx1=L*cos(alpha)
              dy1=L*sin(alpha)
              xp=dx1+x(f(ii))
              yp=dy1+y(f(ii))

! ***  find which triangle new node is in, hence dis to other nodes
              call FINDTRI(xp,yp,j,t,v,npts,ntri,V1,V2,V3,x,y,ncb)
              if (j.eq.0) goto 91        ! if outside domain then skip

              distv1= sqrt((x(v1)-xp)**2 + (y(v1)-yp)**2)
              distv2= sqrt((x(v2)-xp)**2 + (y(v2)-yp)**2)
              distv3= sqrt((x(v3)-xp)**2 + (y(v3)-yp)**2)
              if (amin1(distv1,distv2,distv3).lt.0.8*L) goto 91        ! skip

              if(newnf+1.ge.2*nce) then !we have a problem Houston
                AutoGenFlag = 0
                return
              endif
! ***  put new node into newf
              n=n+1
              newnf=newnf+1
              newf(newnf)=n
              oldf(newnf) = f(ii)
        ! add node to domain
              x(n)=xp
              y(n)=yp
!              xp=x(n)
!              yp=y(n)

! *** set reflength at new node if no reference grid
              call InterpolateElement( npts,x,y,RefLength,v,xp,yp,zp,j)
              RefLength(n) = zp

              list(n)=n
!              call insertp(npts,n,x,y,v,t,ntri,v1,v2,v3,xp,yp,n,j)
              call insertp(npts,x,y,v,t,ntri,v1,v2,v3,xp,yp,n,j,ErrorFlag)

91              continue
            end do  !ia loop

          end if   !tp
    
        end do        ! end loop through front ni to nf
        if(newnf.gt.newnf0) then 
          newnbreak = newnbreak + 1
          newbreak(newnbreak) = newnf + 1
        endif
      
      enddo   !nbreak

! *** make newf into the current f
      nf=newnf
      nbreak = newnbreak-1
      do ii=1, newnf
        f(ii)=newf(ii)
      end do
      do ii=1, newnbreak
        break(ii)=newbreak(ii)
      end do

      newnf=0
      newnbreak=1

!    end do            ! end loop throu layers count

      do ii=1, nf
        ncode( f(ii) ) = 3
      end do

!     Check consistency of triangulation

      IF(NTRI.NE.2*N+1)THEN
       ! Error 13
        WRITE(76,'(//,''***ERROR IN generatepoints***'', &
     &             /,''INCORRECT NUMBER OF TRIANGLES FORMED(NTRI.NE.2*N+1)'')')
        write(76,*) ' ntri=',ntri,' n=',n
!        STOP
      ENDIF

    end

! **********************************************************

    SUBROUTINE FINDTRI(XP,YP,J,T,V,NPTS,NTRI,V1,V2,V3,X,Y,NCB)

! *** Given a point xp,yp, find the triangle in which this point lies
! *** Also tests to see if point is outside of domain - returns J=0 if so

    IMPLICIT NONE

    INTEGER J,V1,V2,V3,NPTS,NTRI
       INTEGER T(3,2*NPTS+1),V(4,2*NPTS+1)
    integer vj(3),ncb,MYLOC(1)
       integer L,count,lplus, Lmax(1), Lmin(1), Lnext
    REAL X(NPTS+3),Y(NPTS+3)
       REAL XP,YP

    J=NTRI

 90    CONTINUE
      V1=V(1,J)
      V2=V(2,J)
      IF((Y(V1)-YP)*(X(V2)-XP).GT.(X(V1)-XP)*(Y(V2)-YP))THEN
        J=T(1,J)
        GOTO 90
      ENDIF
      V3=V(3,J)
      IF((Y(V2)-YP)*(X(V3)-XP).GT.(X(V2)-XP)*(Y(V3)-YP))THEN
        J=T(2,J)
        GOTO 90
      ENDIF
      IF((Y(V3)-YP)*(X(V1)-XP).GT.(X(V3)-XP)*(Y(V1)-YP))THEN
        J=T(3,J)
        GOTO 90
      ENDIF

!    test to see if triangle is outside domain
    VJ=V(1:3,J)
    IF(MAXVAL(VJ).GT.NPTS) then        ! triangle is part of supertri - skip
        J=0
        GOTO 100
    end if

!    Test for triangle orientation (ie is it inside boundary?)
!    (vertex in the next index should be higher)
!    This test only applies to triangles having all vertices on the same boundary
!    ie, no internal pts.

!    maxelist = MAXVAL(ELIST)
    IF (MAX(VJ(1),VJ(2),VJ(3)).LE.NCB) THEN  !all points on boundary
      Lmax = MAXLOC(VJ)
         Lmin = MINLOC(VJ)
         Lnext = mod(Lmin(1),3)+1
         If(VJ(Lnext).eq.VJ(Lmax(1))) then !outside boundary- then check for same boundary
           j=0
           go to 100
         endif

    END IF
    
100    continue
       return

! *** old code 
              MYLOC=MINLOC(VJ)
        L=MYLOC(1)
        DO 15 COUNT=1,2
            IF (L.EQ.3) THEN
                LPLUS=1
            ELSE
                LPLUS=L+1
            END IF
            IF (VJ(LPLUS).LT.VJ(L)) THEN    ! Skip this triangle
!                J=0
!                GOTO 100
            END IF
            L=LPLUS
15        CONTINUE


    END

! ***********************************************************************

      SUBROUTINE InterpolateElement(npts,x,y,z,v,xp,yp,zp,nn)

      IMPLICIT NONE

! THIS SUBROUTINE INTERPOLATES ZRef IN THE DESIGNATED ELEMENT,
! AND RETURNS THE INTERPOLATED VALUE.

! *** passed variables
      integer :: nn
      INTEGER NPTS
      INTEGER V(4,2*NPTS+1)
      REAL X(NPTS+3),Y(NPTS+3),z(npts)
      real :: xp,yp,zp

! *** local variables
      integer I1,I2,I3,j
      real*8 ac(3),acsum
      real dx(3),dy(3),ar


! *** write(*,*) 'Evaluating at node,nn=',node,nn,xp,yp

      I1=(V(1,nn))
      I2=(V(2,nn))
      I3=(V(3,nn))
      DX(1)=(X(I2)-X(I3))
      DX(2)=(X(I3)-X(I1))
      DX(3)=(X(I1)-X(I2))
      DY(1)=(Y(I2)-Y(I3))
      DY(2)=(Y(I3)-Y(I1))
      DY(3)=(Y(I1)-Y(I2))
      AR=0.5*(X(I1)*DY(1)+X(I2)*DY(2)+X(I3)*DY(3))     

      ac(1) = 0.5*((X(I2)-xp)*(Y(I3)-yp) -(X(I3)-xp)*(Y(I2)-yp))/ar
      ac(2) = 0.5*((xp-X(I1))*(Y(I3)-Y(I1)) -(yp-Y(I1))*(X(I3)-X(I1)))/ar
      ac(3) = 0.5*((X(I2)-X(I1))*(yp-Y(I1)) -(xp-X(I1))*(Y(I2)-Y(I1)))/ar

      if((maxval(ac).gt.1.).or.(minval(ac).lt.0.)) then
        do j=1,3
          ac(j) = min(1.,max(ac(j),0.))
        enddo
        acsum = sum(ac)
        ac = ac/acsum
      endif

      zp = Z(I1)*ac(1) +Z(I2)*ac(2) +Z(I3)*ac(3)

         
      RETURN
      END

! **********************************************************

      subroutine SetFrontGenOptions

      character(80) cstr
      character(1) ans
      logical quit

      cstr = 'Read a ngh reference file?'
      call PigMessageYesNo (cstr, ans )
      IF (ans(1:1).EQ.'Y') THEN
        call GetRefGrid( Quit )
!        call ScaleRefLength(N, NTRI, X, Y, RefLength, V)
      else
        call CloseRefGrid(quit)
      endif

      end

! ********************************************************************

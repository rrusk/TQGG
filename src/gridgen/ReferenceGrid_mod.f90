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

! ***********************************************************************

      MODULE RefGridData

      SAVE

! Scalar data
      integer ::  NERef=0, NPRef=0, MaxNbRef, igridtypeR
      real*8 x0offR,y0offR,scaleXR,scaleYR

! 1D data
      integer, allocatable ::  CodeRef(:), ECRef(:)
      real, allocatable ::  XRef(:), YRef(:), ZRef(:)
      logical, allocatable :: ExRef(:)

! 2D data
      integer, allocatable ::  NbRef(:,:), NENRef(:,:)

      END MODULE

! ***********************************************************************

      SUBROUTINE GetRefGrid( Quit )

      USE RefGridData

! Purpose: Prompts user for the input grid file.
! Givens : None
! Returns: None
! Effects: User is prompted for a grid file name.
!          Grid file is opened, points are read,and grid file is closed.

      IMPLICIT NONE

      LOGICAL Quit
      CHARACTER*256 fle
      integer Fnlen, istat
      logical ResOK,PigOpenfile,change
!------------------BEGIN------------------

      Quit = .FALSE.
      ResOK = PigOpenFile(8,'Open Reference File', fle, &
            'Reference File (*.ngh),*.ngh;All Files (*.*),*.*;')
          if(.not.resOK) then
            Quit = .TRUE.
        return
      endif
      fnlen = len_trim( Fle )
!      write(*,*) ('Reading file '//fle(:fnlen))
      call ReadReferenceGrid (Quit)
      close( 8 )
      if(quit) return

! *** Allocate element arrays
      ALLOCATE (NENRef(4,2*NPRef),ECRef(2*NPRef), STAT = istat )
      if(istat.ne.0) then
!        write(*,*)('out of memory - Cannot allocate Ref element arrays')
!        stop
        Quit = .TRUE.
        return
      endif

      NERef = 0
      NENRef = 0
      ECRef = 0
!      write(*,*)('Forming triangle list-please wait')
      call Element_Lister(CHANGE, .FALSE. ,&
     &         NPRef,MaxNbRef,XRef,YRef,ZRef,NbRef,NERef,NENRef,ECRef, &
               x0offR,y0offR,scaleXR,scaleYR,igridtypeR)

      END

! ***********************************************************************

      SUBROUTINE ReadReferenceGrid (quit)

      USE RefGridData
      use mainarrays, only : x0off, y0off, scaleY, igridtype, UTMzone, xlong0
   
! Purpose : To read reference grid data into memory

      IMPLICIT NONE

!     - PASSED VARIABLES
      LOGICAL :: Quit

!     - LOCAL VARIABLES
      INTEGER :: i , j, istat
!        - counters
      INTEGER :: irec, nrec
      real :: d2r, xscf
      character(120) :: Firstline
      character(3) :: UTMzoneR
      character :: PigCursYesNo*1, ans1*1
      logical :: PTneeded
      character*1 :: newline
      INTEGER MarkType1
      COMMON /MoreDefaults/ MarkType1,newline

!------------------BEGIN------------------


      QUIT = .FALSE.

      d2r = acos(-1.)/180.
      PTneeded=.false.
      
!     - initialize 
      nrec = 1

!      call PigPutMessage( 'Reading Reference file.. [NEIGH] format' )

      READ(8,'(a)', err=9999, end=99999) Firstline

      if(firstline(1:4).eq."#NGH") then  !neigh grid file
        do
          READ(8,'(a)', err=9999, end=99999) Firstline
          if(firstline(1:1).ne."#") then    !comment lines
!           following line is internal read of firstline          
! - read offsets, scale factors, coordinate type
            READ(firstline, *, err = 9999, end=99999 ) x0offR, y0offR, scaleXR, scaleYR, igridtypeR
! *** check that igridtype is the same as main grid
            if(igridtypeR.eq.0.and.igridtype.lt.0) then
               ans1 = PigCursYesNo('Ref grid requires Polar Transform. Continue?')
               if(ans1.eq.'Y') then
                 PTneeded = .true.
               else
                 Quit = .true.
                 return
               endif
            elseif(igridtypeR.ne.igridtype) then
               ans1 = PigCursYesNo('Ref grid is of different type. Continue?')
               if(ans1.eq.'N') then
                 Quit = .true.
                 return
               endif
            endif
            if(igridtypeR.eq.1) then
! *** read UTMzone, check that it is the same as main grid
              j = len_trim(firstline)
              READ( firstline(j-2:j), '(a)', IOSTAT=istat ) UTMzoneR(1:3)
              if(UTMzoneR(1:3).ne.UTMzone(1:3)) then
                ans1 = PigCursYesNo('Ref grid has different UTMzone. '//newline//&
                                    'UTM='//UTMzone(1:3)//' UTMR='//UTMzoneR(1:3)//' Continue?')
                if(ans1.eq.'N') then
                  Quit = .true.
                  return
                endif
              endif
            endif
            exit
          endif
        enddo

        READ( 8, *, ERR = 9999, end=99999 ) NPRef
        READ( 8, *, err = 9999, end=99999 ) MaxNbRef

!        write(message,'(a,i6,a,i3,a)') 'Reading Grid File with ',NPRef,' nodes, ', &
!               MaxNbRef,' neighbours'
!        call PigPutMessage(message)

! *** Allocate arrays
        ALLOCATE (XRef(NPRef),YRef(NPRef),CODERef(NPRef),ZRef(NPRef), &
                            EXRef(NPRef),NbRef(MaxNbRef,NPRef+1), STAT = istat )
        if(istat.ne.0) then
!          call PigPutMessage('FATAL ERROR: Cannot allocate Ref storage arrays')
!          call PigMessageOK('out of memory - Cannot allocate Ref element arrays','ref'C)
!          stop
          Quit = .TRUE.
          return
        endif

      else

        READ( firstline, *, ERR = 9999, end=99999 ) NPRef
        READ( 8, *, err = 9999, end=99999 ) MaxNbRef
! - read ranges
        READ( 8, *, err = 9999, end=99999 ) x0offR,y0offR,scaleXR,scaleYR
        igridtypeR = 0

!        write(message,'(a,i6,a,i3,a)') 'Reading Grid File with ',NPRef,' nodes, ', &
!               MaxNbRef,' neighbours'
!        call PigPutMessage(message)

! *** Allocate arrays
        ALLOCATE (XRef(NPRef),YRef(NPRef),CODERef(NPRef),ZRef(NPRef), &
                            EXRef(NPRef),NbRef(MaxNbRef,NPRef+1), STAT = istat )
        if(istat.ne.0) then
!          call PigPutMessage('FATAL ERROR: Cannot allocate Ref storage arrays')
!          call PigMessageOK('out of memory - Cannot allocate Ref element arrays','ref'C)
!          stop
          Quit = .TRUE.
          return
        endif

      endif

      XRef = 0.
      YRef = 0.
      ZRef = 0.
      CodeRef = 0
      EXRef = .false.
      NbRef = 0


      do i = 1,NPRef  
        READ( 8, *, end = 999, err = 9999 ) IREC,XRef(i),YRef(i),CODERef(i),ZRef(i), &
                       ( NbRef(j,i),j = 1, MaxNbRef)

        XRef(i) = (XRef(i)+x0offR)*scaleXR - x0off !set same units and offset as main grid
        YRef(i) = (YRef(i)+y0offR)*scaleXR - y0off
        ZRef(i) = ZRef(i)*scaleYR*scaleY !set up/down in same sense as main grid
        NREC = i
        EXRef(i) = .TRUE.   

        do j=1,MaxNbRef
          if(     (NbRef(j,i) .lt. 0).OR.(NbRef(j,i) .gt. NPRef)) then
!                set illegal number to zero - that should 
!                delete any lines to points out of grid
!                write(message,'(a,i8,a,i8)')'Eliminated connection from node ',i,' to node ',NbRef(j,i)
!                call PigPutMessage(message)
!                call PigUWait(2.0)
                NbRef(j,i) = 0
          endif
        end do
      
      enddo
      
      if(PTneeded) then
! *** check data for proper limits
        DO J = 1, NPRef
          If((XRef(J).lt.-360.).or.(XRef(J).gt.360.)) then
!            text='Longitude out of range -360<long<360'
            call PigMessageOK('Ref Longitude out of range -360<long<360','Longitude Limits')
            return
          endif
        enddo
        DO J = 1, NPRef
          If((YRef(J)+y0off.lt.-89.9).or.(YRef(J)+y0off.gt.89.9)) then
!            text='Latitude out of range -89<lat<89'
            call PigMessageOK('Ref Latitude out of range -89<lat<89','Latitude Limits')
            return
          endif
        enddo
!        write(*,*) 'ref',maxval(XRef(1:NPRef)),minval(XRef(1:NPRef)),maxval(YRef(1:NPRef)),minval(YRef(1:NPRef))
        DO J = 1, NPRef
          xscf = cos((YRef(j)+y0off)*d2r)
          XRef(J) = (XRef(J) - xlong0) * xscf
        enddo
!        write(*,*) 'refPT',maxval(XRef(1:NPRef)),minval(XRef(1:NPRef)),maxval(YRef(1:NPRef)),minval(YRef(1:NPRef))
      endif

      RETURN

   
999   continue
      if(nrec.ne.NPRef) then
!        write(message,'(a,i6,a,i6,a)') 'WARNING: Premature end of file. Only ', &
!                nrec,' of expected ',NPRef,' nodes read in'
!        call PigPutMessage(message)
!        call PigUWait(3.0)
        if(NREC.eq.0) then
          quit = .true.
        endif
      endif

      RETURN
   
9999  continue  
!      call PigPutMessage('ERROR reading grid file: Most likely a format error' )
!      call PigUWait( 3.0 )
      Quit = .TRUE.
      return
      
99999 continue  
!      call PigPutMessage('ERROR reading grid file: Premature end of file in header.' )
!      call PigUWait( 3.0 )
      Quit = .TRUE.
      return

      END

! ***********************************************************************

      SUBROUTINE GetRefGridLimits( nex, npx )

      USE RefGridData

! Purpose: Provide reference grid information
! Givens : None
! Returns: NeRef and NpRef
! Effects: None

       integer nex, npx

       nex = NERef
       npx = NPRef

       end

! ***********************************************************************

      SUBROUTINE InterpRefGrid( n, Xdata, Ydata, Zdata, polylist)

      USE RefGridData

! THIS SUBROUTINE GOES THROUGH THE INPUT DATA VERTEX BY VERTEX, 
! FINDS THE REFERENCE GRID TRIANGLE IT BELONGS IN, INTERPOLATES THE DEPTH,
! AND REPLACES THE DEPTH VALUE WITH THE INTERPOLATED DEPTH.
! If IND=0, interpolates all points
!    IND>0, interpolates inside active polygon

      IMPLICIT NONE

      INTEGER n,II,JJ,count,negcnt,outcnt
      Real Xdata(n),Ydata(n),Zdata(n)
      REAL X0,X1,X2,X3,Y0,Y1,Y2,Y3,Z0,Z1,Z2,Z3,dmax
      REAL DELTA,DA1,DA2,DA3,DA4
      LOGICAL INSIDE,FIRST,polylist(n)
      REAL A,B,C
      REAL B1,C1,AA1,AA2


!   A,B,C - DEPTH AT (X0,Y0)=A*X0 + B*Y0 + C
!   AA1,AA2,B1,C1 -  USED TO CALCULATE A,B,C
!   X0,Y0 - CO-ORDINATES OF POINT TO BE EVALUATED
!   X1,Y1,X2,Y2,X3,Y3 - VERTICES OF TRIANGLE BEING LOOKED AT
!   COUNT - HOW MANY TIMES DELTA IS NOT = 0 AND IS LE 1.E-6 IE. POINT
!                IS MARGINALLY INSIDE TRIANGLE
!------------------BEGIN------------------

      FIRST = .TRUE.
      COUNT = 0
         NEGCNT = 0
         OUTCNT = 0

! EVALUATE DEPTHS

!         call PigPutMessage('Evaluating Depths')

! LOOP THROUGH EACH VERTEX OF THE MODEL GRID AND RE-EVALUATE ITS DEPTH

      DO 200 II=1,n
      
        if(.not.polylist(II)) cycle

! CHECK IF THIS POINT IS ALSO IN DEPTH GRID

        dmax = 1.e30
        DO JJ = 1,NPRef
          x0 = xdata(ii)
          y0 = ydata(ii)
          if( X0.eq.XRef(JJ).and.Y0.eq.YRef(JJ)) then 
            Zdata(II) = ZRef(JJ)
            cycle
          ENDIF
        enddo

! LOOP THROUGH REFERENCE TRIANGLES UNTIL POINT IS FOUND

        DO JJ = 1,NERef
! Use relative coordinates to eliminate truncation problems
          X1 = XRef(NENRef(1,JJ))
          X2 = XRef(NENRef(2,JJ))-x1
          X3 = XRef(NENRef(3,JJ))-x1
          Y1 = YRef(NENRef(1,JJ))
          Y2 = YRef(NENRef(2,JJ))-y1
          Y3 = YRef(NENRef(3,JJ))-y1
          x0 = xdata(ii)-x1
          y0 = ydata(ii)-y1
          x1 = 0.
          y1 = 0.
          Z1 = ZRef(NENRef(1,JJ))
          Z2 = ZRef(NENRef(2,JJ))
          Z3 = ZRef(NENRef(3,JJ))
        
! CHECK TO SEE IF POINT IS INSIDE TRIANGLE

! TO DETERMINE IF x,y LIES INSIDE THE TRIANGLE,
! CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE
! 
          DA1=ABS((X1-X0)*(Y2-Y0)-(X2-X0)*(Y1-Y0))
          DA2=ABS((X2-X0)*(Y3-Y0)-(X3-X0)*(Y2-Y0))
          DA3=ABS((X3-X0)*(Y1-Y0)-(X1-X0)*(Y3-Y0))
          DA4=ABS((X2-X1)*(Y3-Y1)-(X3-X1)*(Y2-Y1))
          INSIDE = .FALSE.
          IF (DA4.GT.0) THEN
            DELTA=DA1+DA2+DA3-DA4
            IF (DELTA.EQ.0) INSIDE=.TRUE.
            IF (DELTA.NE.0.AND.DELTA.LE.DA4*1.E-6) THEN
                INSIDE=.TRUE.
                COUNT = COUNT + 1
            ENDIF
          ENDIF

! IF POINT LIES INSIDE TRIANGLE, EVALUATE DEPTH

          IF (INSIDE) THEN

! CALCULATE A,B,C FOR THIS TRIANGLE WHERE
!    value AT (X0,Y0) z0 = A*z1 + B*z2 + C*z3

            AA2 = X1*Y2 - X2*Y1 + X3*Y1 - X1*Y3 + X2*Y3 - X3*Y2
               AA1 = x2*y3-x3*y2+(y2-y3)*x0+(x3-x2)*y0
               b1  = x3*y1-x1*y3+(y3-y1)*x0+(x1-x3)*y0
               c1  = x1*y2-x2*y1+(y1-y2)*x0+(x2-x1)*y0

            IF (AA2.NE.0.) THEN 
              A = AA1/AA2
              B = B1/AA2
              C = C1/AA2
            ELSE
              A = 0
              B = 0
              C = 0
            ENDIF

            Z0 = A*z1 + B*z2 + C*z3
!                   write(25,*) ' Depth=',z1,z2,z3,z0

            Zdata(II) = Z0

! DEPTH HAS BEEN FOUND, GET OUT OF THIS LOOP AND LOOK FOR NEXT POINT

            cycle
!            GO TO 200
          ENDIF
        enddo

! AT THIS POINT NO TRIANGLE HAS BEEN FOUND THAT CONTAINS THIS POINT

          if (first) then 
!               WRITE(25,*) 'The following points are outside the grid'
            first = .false.
          endif
!          WRITE(25,*) 'INDEX= ',II,'X = ',Xdata(II),'Y = ',Ydata(II),' Z = ',Z0
             outcnt = outcnt + 1
200    CONTINUE

!       WRITE(25,*) '# points outside reference grid = ',OUTCNT
!       WRITE(25,*) '# of marginal points (included in grid) = ',COUNT

    RETURN
    END

! ***********************************************************************

      SUBROUTINE InterpRefGrid2( Xdata, Ydata, Zdata)

      USE RefGridData

! THIS SUBROUTINE GOES THROUGH THE INPUT DATA VERTEX BY VERTEX, 
! FINDS THE REFERENCE GRID TRIANGLE IT BELONGS IN, INTERPOLATES THE DEPTH,
! AND REPLACES THE DEPTH VALUE WITH THE INTERPOLATED DEPTH.
! If IND=0, interpolates all points
!    IND>0, interpolates inside active polygon

      IMPLICIT NONE

      INTEGER JJ,count,negcnt,outcnt
      Real Xdata,Ydata,Zdata
      REAL X0,X1,X2,X3,Y0,Y1,Y2,Y3,Z0,Z1,Z2,Z3,dmax
      REAL DELTA,DA1,DA2,DA3,DA4
      LOGICAL INSIDE,FIRST
      REAL A,B,C
      REAL B1,C1,AA1,AA2


!   A,B,C - DEPTH AT (X0,Y0)=A*X0 + B*Y0 + C
!   AA1,AA2,B1,C1 -  USED TO CALCULATE A,B,C
!   X0,Y0 - CO-ORDINATES OF POINT TO BE EVALUATED
!   X1,Y1,X2,Y2,X3,Y3 - VERTICES OF TRIANGLE BEING LOOKED AT
!   COUNT - HOW MANY TIMES DELTA IS NOT = 0 AND IS LE 1.E-6 IE. POINT
!                IS MARGINALLY INSIDE TRIANGLE
!------------------BEGIN------------------

      FIRST = .TRUE.
      COUNT = 0
      NEGCNT = 0
      OUTCNT = 0

! EVALUATE DEPTHS

!         call PigPutMessage('Evaluating Depths')

! LOOP THROUGH EACH VERTEX OF THE MODEL GRID AND RE-EVALUATE ITS DEPTH

!      DO 200 II=1,n
      
!        if(.not.polylist(II)) cycle

! CHECK IF THIS POINT IS ALSO IN DEPTH GRID

        dmax = 1.e30
           DO JJ = 1,NPRef
          x0 = xdata
          y0 = ydata
          if( X0.eq.XRef(JJ).and.Y0.eq.YRef(JJ)) then 
            Zdata = ZRef(JJ)
            return
             ENDIF
        enddo

! LOOP THROUGH REFERENCE TRIANGLES UNTIL POINT IS FOUND

        DO JJ = 1,NERef
! Use relative coordinates to eliminate truncation problems
          X1 = XRef(NENRef(1,JJ))
          X2 = XRef(NENRef(2,JJ))-x1
          X3 = XRef(NENRef(3,JJ))-x1
          Y1 = YRef(NENRef(1,JJ))
          Y2 = YRef(NENRef(2,JJ))-y1
          Y3 = YRef(NENRef(3,JJ))-y1
          x0 = xdata-x1
          y0 = ydata-y1
          x1 = 0.
          y1 = 0.
          Z1 = ZRef(NENRef(1,JJ))
          Z2 = ZRef(NENRef(2,JJ))
          Z3 = ZRef(NENRef(3,JJ))
        
! CHECK TO SEE IF POINT IS INSIDE TRIANGLE

! TO DETERMINE IF x,y LIES INSIDE THE TRIANGLE,
! CALCULATE DELTA. IF DELTA >0 THEN POINT IS OUTSIDE TRIANGLE
! 
             DA1=ABS((X1-X0)*(Y2-Y0)-(X2-X0)*(Y1-Y0))
             DA2=ABS((X2-X0)*(Y3-Y0)-(X3-X0)*(Y2-Y0))
             DA3=ABS((X3-X0)*(Y1-Y0)-(X1-X0)*(Y3-Y0))
          DA4=ABS((X2-X1)*(Y3-Y1)-(X3-X1)*(Y2-Y1))
          INSIDE = .FALSE.
          IF (DA4.GT.0) THEN
            DELTA=DA1+DA2+DA3-DA4
            IF (DELTA.EQ.0) INSIDE=.TRUE.
            IF (DELTA.NE.0.AND.DELTA.LE.DA4*1.E-6) THEN
                INSIDE=.TRUE.
                COUNT = COUNT + 1
            ENDIF
          ENDIF

! IF POINT LIES INSIDE TRIANGLE, EVALUATE DEPTH

          IF (INSIDE) THEN

! CALCULATE A,B,C FOR THIS TRIANGLE WHERE
!    value AT (X0,Y0) z0 = A*z1 + B*z2 + C*z3

            AA2 = X1*Y2 - X2*Y1 + X3*Y1 - X1*Y3 + X2*Y3 - X3*Y2
               AA1 = x2*y3-x3*y2+(y2-y3)*x0+(x3-x2)*y0
               b1  = x3*y1-x1*y3+(y3-y1)*x0+(x1-x3)*y0
               c1  = x1*y2-x2*y1+(y1-y2)*x0+(x2-x1)*y0

            IF (AA2.NE.0.) THEN 
              A = AA1/AA2
              B = B1/AA2
              C = C1/AA2
            ELSE
              A = 0
              B = 0
              C = 0
            ENDIF

            Z0 = A*z1 + B*z2 + C*z3
!                   write(25,*) ' Depth=',z1,z2,z3,z0

            Zdata = Z0

! DEPTH HAS BEEN FOUND, GET OUT OF THIS LOOP AND LOOK FOR NEXT POINT

            cycle
!            GO TO 200
          ENDIF
        enddo

! AT THIS POINT NO TRIANGLE HAS BEEN FOUND THAT CONTAINS THIS POINT

          if (first) then 
!               WRITE(25,*) 'The following points are outside the grid'
            first = .false.
          endif
!          WRITE(25,*) 'INDEX= ',II,'X = ',Xdata(II),'Y = ',Ydata(II),' Z = ',Z0
             outcnt = outcnt + 1

!       WRITE(25,*) '# points outside reference grid = ',OUTCNT
!       WRITE(25,*) '# of marginal points (included in grid) = ',COUNT

    RETURN
    END

! ***********************************************************************

      SUBROUTINE InterpRefElement( nn,xp,yp,zp)

      USE RefGridData

      IMPLICIT NONE

! THIS SUBROUTINE INTERPOLATES ZRef IN THE DESIGNATED ELEMENT,
! AND RETURNS THE INTERPOLATED VALUE.

! *** passed variables
      integer :: nn
      real :: xp,yp,zp

! *** local variables
      integer I1,I2,I3,j
      real*8 ac(3),acsum
      real dx(3),dy(3),ar

! *** write(*,*) 'Evaluating at node,nn=',node,nn,xp,yp

      I1=(NenRef(1,nn))
      I2=(NenRef(2,nn))
      I3=(NenRef(3,nn))
      DX(1)=(XRef(I2)-XRef(I3))
      DX(2)=(XRef(I3)-XRef(I1))
      DX(3)=(XRef(I1)-XRef(I2))
      DY(1)=(YRef(I2)-YRef(I3))
      DY(2)=(YRef(I3)-YRef(I1))
      DY(3)=(YRef(I1)-YRef(I2))
      AR=0.5*(XRef(I1)*DY(1)+XRef(I2)*DY(2)+XRef(I3)*DY(3))     

      ac(1) = 0.5*((XRef(I2)-xp)*(YRef(I3)-yp) -(XRef(I3)-xp)*(YRef(I2)-yp))/ar
      ac(2) = 0.5*((xp-XRef(I1))*(YRef(I3)-YRef(I1)) -(yp-YRef(I1))*(XRef(I3)-XRef(I1)))/ar
      ac(3) = 0.5*((XRef(I2)-XRef(I1))*(yp-YRef(I1)) -(xp-XRef(I1))*(YRef(I2)-YRef(I1)))/ar

      if((maxval(ac).gt.1.).or.(minval(ac).lt.0.)) then
        do j=1,3
          ac(j) = min(1.,max(ac(j),0.))
        enddo
        acsum = sum(ac)
        ac = ac/acsum
      endif

      zp = ZRef(I1)*ac(1) +ZRef(I2)*ac(2) +ZRef(I3)*ac(3)

         
      RETURN
      END

! ***********************************************************************

      subroutine SetDefaultRefGrid(NP, NE, X, Y, Z, V,DMAX,XMIN,YMIN)

      USE RefGridData

! Purpose: Generate default reference grid
! Givens : Limits
! Returns: 2 triangles with vertices at limit, depth = Dmaxz
! Effects: None.
      
      integer :: np,ne,v(4,ne)
      REAL :: x(np),y(np),z(np)
      REAL :: DMAX,XMIN,YMIN


      NPRef = np
      MaxNbRef = 1
! *** Allocate arrays
      ALLOCATE (XRef(NPRef),YRef(NPRef),ZRef(NPRef), STAT = istat )
      if(istat.ne.0) then
!        call PigPutMessage('FATAL ERROR: Cannot allocate Ref storage arrays')
!        call PigMessageOK('out of memory - Cannot allocate Ref element arrays','ref'C)
!        stop
        return
      endif

      XRef(1:np) = xmin + X(1:np)*DMAX
      YRef(1:np) = ymin + Y(1:np)*DMAX
      ZRef(1:np) = Z(1:np)

      NERef = ne
! *** Allocate element arrays
      ALLOCATE (NENRef(4,NERef), STAT = istat )
      if(istat.ne.0) then
!        call PigPutMessage('FATAL ERROR: Cannot allocate Ref element arrays')
!        call PigMessageOK('out of memory - Cannot allocate Ref element arrays','ref'C)
!        stop
        return
      endif

      NENRef = 0
      NENRef(1,1:ne) = v(1,1:ne)
      NENRef(2,1:ne) = v(2,1:ne)
      NENRef(3,1:ne) = v(3,1:ne)
      NENRef(4,1:ne) = 0

      RETURN
      END

! ***********************************************************************

      SUBROUTINE GetRefGridElementInfo( nex, ncnx, Elelist, x1, y1, z1, C1) 

      USE RefGridData

! Purpose: Provide reference triangle information
! Givens : triangle number
! Returns: vertex node numbers, x, y, z 
! Effects: None

       integer nex, ncnx
       integer Elelist(4),C1(4), n1
       real x1(4), y1(4), z1(4)

       ncnx = 4
       Elelist(1:4) = NENRef(1:4,nex)

       nemax = maxval(EleList)
       if (nemax.gt.NPRef) then
         nex = 0
         return
       endif

       do j=1,4
         n1 = Elelist(j)
         if(n1.gt.0) then
           x1(j) = XRef(n1)
           y1(j) = YRef(n1)
           z1(j) = ZRef(n1)
           C1(j) = 1
         else
           x1(j) = 0.
           y1(j) = 0.
           z1(j) = 0.
           C1(j) = 0
         endif
       enddo

       end

! ***********************************************************************

      SUBROUTINE GetZRefLimits( ZRefmax, ZRefmin )

      USE RefGridData

! Purpose: Return limits on ZRef
! Givens : None
! Returns: Limits
! Effects: None.

      IMPLICIT NONE

      Real ZRefmax, ZRefmin

      ZRefmax = maxval(ZRef)
      ZRefmin = minval(ZRef)

      RETURN
      END

! ***********************************************************************

      SUBROUTINE CloseRefGrid( Quit )

      USE RefGridData

      IMPLICIT NONE

! Purpose: Deallocates reference grid
! Givens : None
! Returns: error indicator
! Effects: arrays deallocated

      LOGICAL Quit

!------------------BEGIN------------------

      Quit = .FALSE.
      NERef = 0
      NPRef = 0

! *** Deallocate arrays
      if(ALLOCATED(XRef)) DEALLOCATE (XRef)
      if(ALLOCATED(YRef)) DEALLOCATE (YRef)
      if(ALLOCATED(ZRef)) DEALLOCATE (ZRef)
      if(ALLOCATED(CodeRef)) DEALLOCATE (CodeRef)
      if(ALLOCATED(EXRef)) DEALLOCATE (EXRef)
      if(ALLOCATED(NbRef)) DEALLOCATE (NbRef)
      if(ALLOCATED(NENRef)) DEALLOCATE (NENRef)
      if(ALLOCATED(ECRef)) DEALLOCATE (ECRef)

      END

! ***********************************************************************

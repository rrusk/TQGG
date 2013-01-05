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

  !***********************************************************************

      Subroutine SAMPLE(quit)

!       Purpose: Permits selection of boundary nodes from the digitized
!                boundary data file in DIGIT format and selection of
!                internal nodes for the depth grid from the digitized
!                contour data file (DIGIT format) and/or from a file
!                of depth soundings in NODE format.
!
!       All boundaries should have have been digitized in the counter-
!       clockwise direction. An optional test is included to permit the
!       user to check if each boundary has been digitized in the coun-
!       terclockwise direction. The nodes selected from the outer boun-
!       dary are output in counter-clockwise order, but island boundary
!       nodes are output in clockwise order, as required for NODE
!       format files. The program permits the user to select every
!       Nth point from data strings in the boundary and contour files.
!       Different values of N can be used for land or sea portions of
!       the boundary. The outer boundary is assumed to consist of
!       alternate stretches of land and sea boundary, the first block
!       being land boundary. Each island boundary is assumed to be a
!       single stretch of land boundary. All points in the soundings
!       file are added to the list of internal nodes.

!       UNITS:
!       x,y coordinates in the DIGIT and NODE format files are assumed
!       to be in problem length units.
! *************************************************************************

      use MainArrays

      implicit none

      INCLUDE '../includes/defaults.inc'

!     - passed variable
      logical quit,change

!     - local variables
      integer gridcode
      character PigCursYesNo*1
      CHARACTER*80 ans, cstr

! *** start

! *** open, parse, and read data file
      call Sample_Input (gridcode,quit)
      if(quit) return

! *** join segments
      if(gridcode.eq.1) call Join_Segments ()

      If(TotCoords.gt.0) then
        dispnodes = .true.
        change = .false.
        call DrwFig(change)
      else
        return
      endif

! *** define resolution
      cstr= 'Add points to set spacing ?:'
      ans = PigCursYesNo (cstr)
      if (ANS(1:1) .ne. 'Y') then
        quit = .true.
      endif
!      call Set_Resolution ()

! *** sample from points
!      call Sample_Boundary ()

      END subroutine

! ********************************************************************

      subroutine Sample_Input (gridcode,quit)

      use MainArrays

      implicit none

      INCLUDE '../includes/defaults.inc'

!     - local variables
      integer j,Fnlen,nunit,stat,segcode,gridcode
      logical PigOpenFileCD
      real xtest,ytest,ztest,xmin,xmax,ymin,ymax
      CHARACTER*256 fle
      character(80) Firstline
      logical Quit

! *** start
      nunit = 3

      if(.not.PigOpenFileCD(nunit,'Open Sample File', fle, &
          'XYZ Files (*.[xn][yo]*),*.xy*;NOD Files (*.nod),*.nod;All Files (*.*),*.*;')) then
        fnlen = len_trim(fle)
        call PigMessageOK('Error opening file '//fle(:fnlen),'OpenGrid')
        GridRName =  'NONE'
        quit = .true.
        return
      endif

      GridRName =  fle
      fnlen = len_trim( Fle )

      call PigPutMessage('Reading file '//fle(:fnlen))
      TotCoords = 0
      gridcode = 0
      quit = .false.

! *** try to determine format
      READ(nunit,'(a)', iostat=stat) Firstline

      if(firstline(1:4).eq."#NGH") then  !ngh grid file
        call PigMessageOK('Cannot sample from ngh file ','Sample')
        quit = .true.
      elseif(firstline(1:4).eq."#NOD") then  !nod point file
        rewind nunit
        call ReadNodeFile ( Quit )
      else
        read(firstline,*,iostat=stat) xtest,ytest,ztest,segcode  !xyc file
        if(stat.eq.0) then
          gridcode = 1
          dxray(1) = xtest
          dyray(1) = ytest
          depth(1) = ztest
          code(1) = segcode
          j=1
          do
            j = j+1
            read(nunit,*,iostat=stat) dxray(j),dyray(j),depth(j),code(j)
            if(stat.ne.0) then
              TotCoords = j-1
              TotBndys = 1
              PtsThisBnd(1) = TotCoords
              TotIntBndys = 0
              TotIntPts = 0
              itot = TotCoords
              xmin = minval(dxray(1:itot))
              xmax = maxval(dxray(1:itot))
              ymin = minval(dyray(1:itot))
              ymax = maxval(dyray(1:itot))
              call fullsize(xmin,ymin,xmax,ymax)
              exit
            endif
            !exist(j) = .true.
          enddo
        else
          read(firstline,*,iostat=stat) xtest,ytest,ztest  !xyz file, node pairs
          if(stat.eq.0) then
            dxray(1) = xtest
            dyray(1) = ytest
            depth(1) = ztest
            j=1
            do
              j = j+1
              read(nunit,*,iostat=stat) dxray(j),dyray(j),depth(j)
              if(stat.ne.0) then
                TotCoords = j-1
                TotBndys = 1
                PtsThisBnd(1) = TotCoords
                TotIntBndys = 0
                TotIntPts = 0
                itot = TotCoords
                xmin = minval(dxray(1:itot))
                xmax = maxval(dxray(1:itot))
                ymin = minval(dyray(1:itot))
                ymax = maxval(dyray(1:itot))
                call fullsize(xmin,ymin,xmax,ymax)
                exit
              endif
              !exist(j) = .true.
              code(j) = 1    
            enddo
          else
            call PigMessageOK('Unsupported file format ','Sample')
            quit = .true.
          endif
        endif          
      endif
      close(nunit)

      end subroutine

! ********************************************************************

      subroutine Join_Segments ()

      implicit none

!     - local variables

! *** start

      end subroutine

! ********************************************************************

      subroutine Set_Resolution (x,y,quit)

      implicit none

!     - passed variables
      real x,y
      logical quit

!     - local variables
      integer stat
      real ds
      character PigCursYesNo*1
      CHARACTER*80 ans, cstr, retstring

! *** start

      call PigDrawCoinSymbol(x, y)
      cstr= 'Enter spacing at this point:'
      call PigPrompt( cstr, RetString )
      read(RetString,*,iostat=stat) ds
      if(stat.ne.0) then
        call PigMessageOK('Error reading real number:','Sample')
      else
! *** store data point and spacing
      endif

      cstr= 'Add points to set spacing ?:'
      ans = PigCursYesNo (cstr)
      if (ANS(1:1) .eq. 'Y') then
        quit = .false.
        return
      endif

! *** interpolate and quit
      call Sample_Boundary (ds)

      quit = .true.

      end subroutine

! ********************************************************************

      subroutine Sample_Boundary (ds)

      use MainArrays

      implicit none

!     - local variables
      integer j, k, icount, isave
      real ds, ds2
      real x0, y0, dx, dy, dl2

! *** start
      ds2 = ds*ds
      x0 = dxray(1)
      y0 = dyray(1)
      icount = 0
      isave = 1
!   outer boundary
      do j= 1,PtsThisBnd(1)
        icount = icount + 1
        dx = x0 - dxray(icount)
        dy = y0 - dyray(icount)
        dl2 = dx*dx + dy*dy
        if(dl2.ge.ds2) then !save it
          isave = isave + 1
          x0 = dxray(icount)
          y0 = dyray(icount)
          dxray(isave) = x0
          dyray(isave) = y0
        endif
        PtsThisBnd(1) = isave
      enddo
!   loop over other boundaries
      do k = 2,TotBndys
        do j= 1,PtsThisBnd(k)
          icount = icount + 1
          dx = x0 - dxray(icount)
          dy = y0 - dyray(icount)
          dl2 = dx*dx + dy*dy
          if(dl2.ge.ds2) then !save it
            isave = isave + 1
            x0 = dxray(icount)
            y0 = dyray(icount)
            dxray(isave) = x0
            dyray(isave) = y0
          endif
          PtsThisBnd(k) = isave - PtsThisBnd(k-1)
        enddo
      enddo
      TotCoords = isave
      itot = isave
      
      end subroutine

! ********************************************************************
! *********************************************************************

      SUBROUTINE SELECT(M,X,Y,mind,NS,NPT,XS,YS,KIND)

!****STILL TO HAVE DISTANCE TEST INCLUDED*********************** N.B.
!       SUBROUTINE TO SELECT POINTS;   X(1)     ,Y(1)
!                                     X(1+NS)  ,Y(1+NS)
!                                     X(1+2*NS),Y(1+2*NS)
!                                      ...     , ...
!
!       FROM INPUT ARRAYS X,Y.  THE FINAL POINT SELECTED IS DISCARDED IF IT
!       LIES LESS THAN INT(NS/2) POINTS FROM THE FINAL ORIGINAL X,Y POINT,
!       except in case of contours (KIND=6) where last data point in each
!       is selected and the second last point is discarded if it is too 
!       close to the last point
!       -------------------------------------------------------------------

      INTEGER M,NS,NPT,KIND
      real X(*),Y(*),XS(*),YS(*),mind,distsq
      INTEGER II,MM,JM
!       SELECT FIRST POINT
      NPT=1
      XS(NPT)=X(1)
      YS(NPT)=Y(1)
!       NO. OF FURTHER POINTS TO SELECT
      MM=(M-1)/NS
!       SELECT THESE POINTS
      DO 101 II=1,MM
            JM=1+II*NS
            NPT=NPT+1
            XS(NPT)=X(JM)
            YS(NPT)=Y(JM)
101     CONTINUE
!       In case of everything but contours, 
!        1) discard last point selected if last point from data block
!        2) discard second last point if too close to last data point
      IF(KIND.ne.6) THEN
       IF(M.eq.MM*NS+1)THEN
         NPT=NPT-1
       ENDIF
!
      IF(M-(1+MM*NS).ne.0.and.M-(1+MM*NS).LT.NS/2) NPT=NPT-1
      ENDIF
!       In case of contours (kind = 6), add last point (for ns.gt.1)
      IF(KIND.eq.6) THEN
       IF(NS.ne.1)THEN
         NPT=NPT+1
         XS(NPT)=X(M)
         YS(NPT)=Y(M)
!          and delete second last point if too close to last point,
!          (only if more than total of 2 points selected)
         if(NPT.GT.2.and.(M-JM).LT.(NS+1)/2) then
           xs(npt-1)=xs(npt)
           ys(npt-1)=ys(npt)
           npt=npt-1
         endif
       ENDIF
      ENDIF
!       Discard last point of block if within minimum distance
!       mindist of first point from same block - useful for
!       islands and closed contours
      distsq = (XS(1)-XS(NPT))**2 + (YS(1)-YS(NPT))**2        
      IF(distsq.lt.mind**2) THEN
        NPT= NPT - 1
      ENDIF
      END 
! **********************************************************************

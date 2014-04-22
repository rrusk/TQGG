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

      SUBROUTINE Gridit2(maxnp,np,x,y,depth,ncode,maxngh,numngh,nbrs,maxne,ne,&
            nen,IECode,numbnd,NumIntBnd,numbndpts,Quit,AutoGenFlag)

! ********************************************************************
!  Purpose : To create an irregular triangular grid, given a set of
!            grid nodes, using the Sloan algotithm.
!********************************************************************

      IMPLICIT NONE

! *** PASSED VARIABLES
      integer maxnp,np,maxngh,numngh,maxne,ne,numbnd,numintbnd
      integer AutoGenFlag
      integer ncode(maxnp+3),nbrs(maxngh,maxnp)
      integer nen(4,maxne),IECode(maxne)
      integer numbndpts(numbnd)
      real :: x(maxnp+3),y(maxnp+3),depth(maxnp+3)
      logical Quit

! *** LOCAL VARIABLES

      INTEGER MaxCoords,start, end
      INTEGER NCOINPTS,npint
      integer nindx !,indx(maxnp)
      integer nce, ncb, ii, jj, jjadd
      integer elist(2,maxnp/2), list(maxnp)
      integer t(3,maxne),w(maxnp+3,2)
      character(1) ans

!------------------------------------------------------------------------
!  NOTES ON ARRAY DIMENSIONS
!
! *** arguments for front end Sloan routine CONTRI are :-

!        single integers  -  npts, n, nce, ncb
!        integer arrays   -  elist(2 , ), list( ), 
!                            t(3,2*n+1), v(3,2*n+1), w(npts+3,2)
!        real             -  x(npts+3), y(npts+3)

! -----------------------------------------------------------------------

!        At present in GRED assume  n = npts = ToTCoords 
!        Max value of n, npts, TotCoords is MREC (iden. to  MAXPTS) 
!
!        nce = actual total # of edges in bdys & internal constraints
!         - will be assumed for present to be not greater than MREC/2
!         - is second dimension of elist( , )
!
!        ncb = actual total # of edges in bdys & internal constraints
!              nb <= nce  , hence max size for present = MREC/2
!
!        Dimensions needed for Sloan arrays (see S/R CONTRI)
!
!        Actual elist(2,nce)  - so dimension as elist(2,MREC/2)
!                list(npts)   -       "      as list(MREC)
!                t(3,2*n+1)   -       "      as t(3,MAXTRI)
!                v(3,2*n+1)   -       "      as v(3,MAXTRI)
!                w(npts+3,2)  -       "      as w(MREC+3,2)
!
!                (MAXTRI = 2*MREC + 1
!
!        In GRIDIT, because boundary nodes come before interior nodes,
!        elist( , ) begins:
!
!             elist(1,1)        = 1                elist(2,1) = 2
!             elist(1,2)        = 2                elist(2,2) = 3
!             .....................                ..............
!             first entry for each boundary has elist(1, ) = first node
!             on that boundary and elist(2, ) = second node on that boundary
!
!             last entry for each boundary has elist(1, ) = last node
!             on that boundary and elist(2, ) = first node on that boundary
!             
!             e.g. if there is only 1 boundary,             
!
!      elist(1,TotCoords-1) = TotCoords - 1 ; elist(2,TotCoords-1) = TotCoords
!      elist(1,TotCoords)   = TotCoords     ; elist(2,TotCoords)   = 1
!
!        but this might not always be the case in GRED
!
!        In GRIDIT, list(1),...,list(TotCoords) always = 1 ,..., TotCoords 
!        but this might not always be the case in GRED 
!
!       Prepare edge list elist( , ) from input NODE information
       
        Quit = .false.
        MaxCoords = maxnp-3

        NCOINPTS = 0
        nindx = maxnp
        npint = 0
        do jj=1,NumBnd
          npint = npint + NumBndPts(jj)
        enddo
        npint = np - npint
        CALL COINCIDE(NCOINPTS,np,ncode,x,y)

        IF(NCOINPTS.gt.0) THEN
          call PigMessageYesNo('Coincident nodes: Remove them (Y/N)?', ans)
          if (ANS(1:1) .eq. 'Y') then
            call DeleteAnyNode2 (np,NumBnd,NumBndPts,x,y,depth,ncode )
          else
! *** remove marker codes
            DO ii = 1, np
              if(ncode(ii).lt.-10000) then
                  ncode(ii) = mod(ncode(ii),100000)
                  if(ncode(ii).lt.0) ncode(ii)=ncode(ii)+100000
              endif
            END DO
            Quit = .true.
            return
          endif
        ENDIF

        do jj = 1, np
          nCode(jj) = 0
        enddo

        start = 1
        ncb = 0
        nce = 0
        jjadd = 0
        do ii = 1,NumBnd
          if(NumBndPts(ii).le.0) then
            cycle
          endif
          end = (start-1) + NumBndPts(ii)
          do jj = start, end - 1
            elist(1,jj) = jj + jjadd
            elist(2,jj) = jj + jjadd + 1
            if(ii.le.NumBnd-NumIntBnd) ncb = ncb + 1
            nce = nce + 1
          enddo
! 45       continue
          if(ii.le.NumBnd-NumIntBnd) then
            ncb = ncb + 1
            elist(1,end) = end
            elist(2,end) = start
            nce = nce + 1
            start = end + 1
          else
            start = end
            jjadd = jjadd + 1
          endif
        enddo  !ii

        do 46 jj = 1, np
          list(jj) = jj
46      continue


        open(76,file='TriErrors.txt')
        call CONTRI(maxnp,np,nce,ncb,elist,x,y,ncode,list,w,&
                    nen,t,ne,AutoGenFlag)
        close(76)
        IF(AutoGenFlag.lt.0) THEN
          Quit = .true.
          return
        ENDIF
        
        IEcode = 1

! Convert from node and triangle data to NEIGH format and output

        nindx = maxnp
        CALL ALTER (nindx,np,ncode,maxngh,numngh,nbrs,ne,nen,&  !IECode,&
                     numbnd,NumIntBnd,numbndpts)

        numngh = maxngh !expand to full size for editing

        return
        END

! *********************************************************************

    SUBROUTINE ALTER(nindx,np,ncode,maxngh,numngh,nbrs,ne,nen,& !IECode,&
                     numbnd,NumIntBnd,numbndpts)

! ***********************************************************************
! This routine converts from triangle list (TRIANG 
! format) and node file (NODE format) to NEIGH format
! ******************************************************

    IMPLICIT NONE

! *** passed VARIABLES ***
      integer nindx
      integer np,ncode(np),maxngh,numngh,nbrs(maxngh,np)
      integer ne,nen(4,ne) !,IECode(ne)
      integer numbnd,NumIntBnd,NumBndPts(NumBnd)

! *** LOCAL VARIABLES ***

      integer nonbrs(nindx)
!   CUVX - current vertex being considered
      INTEGER CUVX
!   CUNBR - current neighbour being considered
    INTEGER CUNBR
    INTEGER II,JJ,KK,LL,MM
    LOGICAL pass1, NEWNBR
    character cstr*80

!   Starting neighbor list

!   Set count of nbrs to zero for all nodes

    DO 10 KK = 1, np
!      Exist(KK) = .TRUE.
        NONBRS(KK) = 0
!   and set all neighbour arrays to zero
          DO 15 JJ = 1, maxngh
          nbrs(JJ,KK) = 0
15        CONTINUE
10      CONTINUE


! Nodes in output NEIGH file are in same order as in input NODE file

! *** Check each triangle and check that each vertex is in each of 
! *** the other two vertices' neighbour lists

        pass1 = .true.
        DO 101 JJ = 1, ne
!            IECode(JJ) = 1 
! *** Check each vertex in current triangle
          DO 102 II = 1, 3
! *** Choose current vertex
            CUVX = nen(II,JJ)
! *** Take other two vertices in turn
            DO 103 LL = 1,3
              IF(LL.eq.II) GO TO 98
!              Choose current neighbour of chosen vertex 
              CUNBR = nen(LL,JJ)
!              Check if CUNBR is already in neighbour list of CUVX
              NEWNBR = .TRUE.
              IF(NONBRS(CUVX).eq.0) GO TO 99
              DO 104 MM = 1, NONBRS(CUVX)
                IF(CUNBR.eq.nbrs(MM,CUVX)) NEWNBR = .FALSE.
104           CONTINUE
99            CONTINUE
!              If CUNBR is new neighbour of CUVX, add to list
              IF(NEWNBR) THEN     
                if(nonbrs(cuvx).ge.maxngh) then
                  if(pass1) then
                    cstr =' Too many neighbor points - truncating:'
                      call PigMessageOK(cstr, 'Error')
!                   stop
                    pass1 = .false.
                  endif
                else
                  NONBRS(CUVX) = NONBRS(CUVX) + 1
                  nbrs(NONBRS(CUVX),CUVX) = CUNBR
                endif
              ENDIF
98            CONTINUE
103         CONTINUE
102       CONTINUE
101     CONTINUE

!       Find max number of neighbours
        numngh = 0
        DO 105 II = 1, np
!          if(NONBRS(II).le.0) ncode(II) = -9
          IF(NONBRS(II).gt.numngh) numngh = NONBRS(II)
105     CONTINUE

!       Set up computational codes : 1 everywhere on outer boundary
!                                    2 on island boundaries
!                                    90 at line constraints
!                                    0 at interior nodes
        CUVX = 0
        DO 108 JJ = 1, NumBnd
          DO 109 KK = 1, NumBndPts(JJ)
             CUVX = CUVX + 1
             IF (JJ.eq.1) THEN
                nCODE(CUVX) = 1
             ELSEIF (jj.le.NumBnd-NumIntBnd) then
                nCODE(CUVX) = 2
             ELSE
                nCODE(CUVX) = 90
             ENDIF
109       CONTINUE
108     CONTINUE

        do jj=cuvx+1,np
          ncode(jj) = 0
        enddo

    RETURN
    END

! *****************************************************************

      SUBROUTINE COINCIDE(NCOINPTS,totcoords,code,dxray,dyray)

!   Checks an input NODE file for any pairs of coincident points.
!       Outputs coordinates of coincident points to file GRIDIT.LOG
!       Returns NCOINPTS, number of coincident pairs. Uses index table
!   routine INDEXX from "Numerical Recipes" by Press, Flannery,
!   Teukolsky and Vetterling.
!
!   MAXPTS = max. permitted number of nodes 
!       NC = actual number of nodes
! ************************************************************

        IMPLICIT NONE

! *** passed variables
      INTEGER NCOINPTS
      integer totcoords, code (totcoords)
      real :: dxray(totcoords),dyray(totcoords)
      
! *** LOCAL VARIABLES ***
      INTEGER II,JJ
      real coinx,coiny,dx,dy,x1,x2,y1,y2
      real, parameter :: tol=1.e-5

!     NCOINPTS - number of locations where coincident nodes occur
!     PAIRS  - number of coincident pairs of points at current point
!     NUMPTS - number of coincident nodes at current point

      dx = abs(maxval(dxray(1:TotCoords)) - minval(dxray(1:TotCoords)))
      dy = abs(maxval(dyray(1:TotCoords)) - minval(dyray(1:TotCoords)))

! *** check boundaries first
      DO II = 1, TotCoords
        x1 = dxray(ii)
        y1 = dyray(ii)
        DO JJ = II+1, TotCoords
          x2 = dxray(jj)
          y2 = dyray(jj)
          IF (abs(x1-x2).lt.tol*dx) then
            if(abs(y1-y2).lt.tol*dy) then
              code(jj) = code(jj) - 100000
              NCOINPTS = NCOINPTS + 1
              coinx = dxray(jj)
              coiny = dyray(jj)
              call PigDrawCoinSymbol ( coinx, coiny )  !display coincident points
            endif
          endif
        enddo
      enddo

      RETURN 
      END

! ************************************************************************

      SUBROUTINE DeleteAnyNode2 (totcoords,NumBnd,PtsThisBnd,&
                                dxray,dyray,depth,ncode )

      IMPLICIT NONE

! PURPOSE: To update dxray(), dyray(), depth() during a successful call to
!          DelNode.
!   GIVEN: nodetype = type (2=boundary or 1=interior) of node to delete,
!          In Common DELETIT;
!               deletx = x coordinate of node to delete,
!               delety = y coordinate of node to delete,
!               delidx = index of node to delete.
! RETURNS: Updated Node data in Common NODES in NODESTOR.INC.
! EFFECTS: dxray(), dyray(), depth() are updated so that node specified
!          in Common DELETIT is removed. TotCoords, PtsThisBnd(), and
!          TotIntPts are updated as required.
!----------------------------------------------------------------------*

! - PASSED VARIABLES
      integer NumBnd
      integer totcoords,PtsThisBnd(NumBnd),ncode(totcoords)
      real :: dxray(totcoords),dyray(totcoords),depth(totcoords)

! - LOCAL VARIABLES
      integer  i,j, ptindx,numdel,numdelbnd, numbnd0

!--------------------START ROUTINE--------------------------------------*

!           - shuffle  dxray(), dyray(), depth() down 1 position

      ptindx = 0
      numdel = 0
      numbnd0 = 0
      DO i=1,NumBnd
        numdelbnd = 0
        do j=1,PtsThisBnd(i)
          ptindx = ptindx + 1
          if(ncode(ptindx).lt.-10000) then
            numdel = numdel + 1
            numdelbnd = numdelbnd + 1
          !endif
          elseif(ptindx-numdel.gt.0) then
            dxray(ptindx-numdel) = dxray(ptindx)
            dyray(ptindx-numdel) = dyray(ptindx)
            depth(ptindx-numdel) = depth(ptindx)
            ncode(ptindx-numdel) = ncode(ptindx)
          endif
        enddo
        !check for no points - if so eliminate boundary
        if((PtsThisBnd(i) - numdelbnd).le.0) then
          numbnd0 = numbnd0 + 1
        else
          PtsThisBnd(i-numbnd0) = PtsThisBnd(i) - numdelbnd
        endif
      ENDDO

      i = ptindx+1
      do j = i,totcoords
        ptindx = ptindx + 1
        if(ncode(ptindx).lt.-10000) then
          numdel = numdel + 1
        !endif
        elseif(ptindx-numdel.gt.0) then
          dxray(ptindx-numdel) = dxray(ptindx)
          dyray(ptindx-numdel) = dyray(ptindx)
          depth(ptindx-numdel) = depth(ptindx)
          ncode(ptindx-numdel) = ncode(ptindx)
        endif
      enddo

      totcoords = ptindx - numdel
      NumBnd = NumBnd - numbnd0

      RETURN
      END

! ********************************************************

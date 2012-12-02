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

!************************************************************************
        SUBROUTINE Element_lister(CHANGE,retrowanted, &
               itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
               x0off,y0off,scaleX,scaleY,igridtype)
!        SUBROUTINE Element_lister(CHANGE,retrowanted, &
!               itot,nbtot,dxray,dyray,depth,nl,exist,TotTr,ListTr,Tcode, &
!               x0off,y0off,scaleX,scaleY,igridtype)

           IMPLICIT NONE

!       WARNING - Boundary nodes retrieved in RETRONODES are stored here in
!                 local array RNODES( ). The nodes for each boundary are in a
!                 consecutive block, but the order of the nodes within each 
!                 each block is the reverse of the order usual in NODE format
!                 files, i.e. in rnodes( ), the outer boundary is in clockwise 
!                 order and islands are in counterclockwise order. Also, the outer 
!                 boundary is not necessarily the first block & rnodes( ) does not
!                 have the interior nodes in it. If the RETRONODE function in
!                 GRIDGEN is supposed to produce a properly ordered set of nodes,
!                 equivalent to reading in a NODE format file, then 3 further 
!                 steps need to be implemented:
!                  1) Put outer boundary first
!                  2) Reverse order of nodes in each boundary  
!                  3) Add interior nodes to (retro) node array


!       WARNING - do something about WRITE & PRINT statements if 
!                 using in interactive program 2) do something about 
!                 collinearity of connections
!    
!       Purpose: To form an element list for a grid in NEIGH format
!                - grid may consist of mixture of triangles and
!                - quadrilaterals. Some error detection capability.
!       Written : 7 June 1994 by Falconer Henry
!       Modified: 25 September 1998 to include RETRONODE function
!       Input variables
!       CHANGE = .true. if grid topology has changed, new list required
!              = .false. if element list does not need revision
!       itot   = number of nodes in grid
!       RETROWANTED = .true. if RETRONODE function required


! *** Passed variables ***

      LOGICAL CHANGE ! true indicates grid topology has been changed
      integer itot,nbtot,nl(nbtot,itot+1)
      integer TotTr,ListTr(4,2*itot),Tcode(2*itot)
      real dxray(itot), dyray(itot), depth(itot)
      integer igridtype
      real*8 x0off,y0off,scaleX,scaleY 
!      logical exist(itot)

! *** Local variables ***

      INTEGER NextVer                     ! Integer functions
      INTEGER IN, SNB(NBTOT+1)
      INTEGER numverts, ThisJ, NextCcwNbr, NextJ, NextVertex
      INTEGER I,J,K,M
      LOGICAL ELIVE

      INTEGER ELCOUNT
!      ELCOUNT is element counter
!      ListTr(1, ) to ListTr( ,4) are nodes of element ic ccw order
!      ListTr(5, )) indicates type of element
!         = 1  node with no neighbours
!         = 2  node with only 1 neighbour - element is line
!         = 3  triangular element
!          = 4  quadrilateral element
!         = 5  element with more than 4 nodes, assumed to be boundary

      INTEGER LISTERR(5)
!         LISTERR(1) = number of nodes found with no neighbours
!         LISTERR(2) = number of nodes found with only one neighbour
!         LISTERR(3) = number of triangles found
!         LISTERR(4) = number of quadrilaterals found
!         LISTERR(5) = number of boundaries found

      INTEGER NUMN(itot)   ! no. of neighbours of each node

!---  variables added 25/9/98 for RETRONODE operation ------------------------

      integer, parameter :: maxnnb=10000
      INTEGER RNODES(2*itot),NUMofBDYS
      integer, save :: NUMperBDY(MAXNNB)
      INTEGER NumIntBndys, Nfound

!     PASSED variable added 25/9/98 for RETRONODE operation
      LOGICAL retrowanted

!    Local variables added 25/9/98 for RETRONODE operation
      INTEGER firstnode, fnlen
      LOGICAL followboundary, PigOpenFile 

!     Local variables added 25/9/98 for output of retrieved node file
      CHARACTER*256 retroname
      INTEGER  sumofnodes, numouter, presum, rindex
      REAL minimumx

!---  end of extra variable section ------------------------------------------

! ----BEGIN------------------------------------------------------------

      if(change) then
      endif

!     number of boundaries so far detected
      NUMofBDYS = 0
!     number of boundary nodes found so far
      Nfound = 0
      NUMperBDY = 0

!     Count sort (ccw from east) the neighbours of each node 
      do I = 1, itot
        numn(I) = 0
        call SORTNBRS(I,NUMN(I),SNB,itot,nbtot,nl,dxray,dyray)
        do J = 1, NUMN(I)
          NL(J,I) = SNB(J)
        enddo
        if(numn(i).lt.nbtot) then
          do j=numn(i)+1,nbtot
            nl(j,i) = 0
          enddo
        endif
      enddo

!      Neighbour lists now sorted ccw, counted and packed left
!      (no need to set unused neighbours spaces at right to zero)

!     Set element counts to zero
      ListTr = 0
      elcount = 0
      LISTERR = 0

! ----------------------------------------------------------------------
!     BEGIN SEARCH FOR ELEMENTS
!     For each node in grid
      DO IN = 1, itot
!      check node is live
!        if(.not.EXIST(IN)) cycle !go to 200

!       first look after degenerate elements, nodes with 0 or 1 neighbours 
        if (numn(IN).lt.2) then

          if (numn(IN).eq.0) then
!           case of node with no neighbours
!           element represented as  IN,0,0,0,1  
            elcount = elcount + 1
            ListTr(1,elcount) = IN
            do k = 2,4
              ListTr(k,elcount) = 0
            enddo             
            TCode(elcount) = 1
            LISTERR(1) = LISTERR(1) + 1
          elseif(numn(IN).eq.1) then
!           case of node with one neighbour
!           element represented as  IN,nbr,0,0,2  
            elcount = elcount + 1
            ListTr(1,elcount) = IN
            ListTr(2,elcount) = NL(1,IN)
            do k = 3,4
              ListTr(k,elcount) = 0
            enddo             
            TCode(elcount) = 2
            LISTERR(2) = LISTERR(2) + 1
          endif
          cycle
        endif

!       - from here on, node IN assumed to have at least 2 neighbours
!      Begin element check around each node, starting from first neighbour
        j = 1
        do 60 while (j.le.numn(IN))
          ThisJ = j
          ELIVE = .true.      ! element still valid

!      begin new element with line from node IN to its Jth neighbour
          elcount = elcount + 1
          ListTr(1,elcount) = IN        ! first node of fresh element is node IN
          ListTr(2,elcount) = NL(j,IN)
!         kill element if lower-numbered node is encountered      
          if(ListTr(2,elcount).lt.IN) then
            elcount = elcount - 1
            j = j + 1
            go to 60
          endif

          numverts = 2

!        Find  next neighbour of IN in ccw direction for later check
!                                                 of polygon closure

          call CwwNbr(IN,ThisJ,NextCcwNbr,NextJ,numn(in),nbtot,itot,nl)

!         Find consecutive vertices of polygon in ccw direction until
!         polygon closes or terminates for some reason
!         - maybe make NextVer return an error indicator
!         nextvertex = NextVer(IN,NB(IN,j))
          nextvertex = NextVer(IN,NL(j,IN),numn(NL(j,IN)),nl,nbtot,itot)
          ListTr(3,elcount) = nextvertex

!         kill element if lower-numbered node is encountered      
          if(ListTr(3,elcount).le.IN) then
            elcount = elcount - 1
            j = j + 1
            go to 60
          endif
          if (nextvertex.eq.NextCcwNbr) then
!         element is triangle
! MUST INTRODUCE CHECK HERE TO DISREGARD ODDBALL CW CORNER TRIANGLE ??
! - maybe not necessary?
            ListTr(4,elcount) = 0
            TCode(elcount) = 3   ! triangle type
            LISTERR(3) = LISTERR(3) + 1
            j = j + 1
            go to 60
          else
!           ELEMENT IS POLYGON with more than 3 sides or is not closed
            nextvertex = NextVer(NL(j,IN),ListTr(3,elcount), &
                                numn(ListTr(3,elcount)),nl,nbtot,itot)
!           kill element if lower-numbered node is encountered      
            if(nextvertex.lt.IN) then
              elcount = elcount - 1
              j = j + 1
              go to 60
            endif
            ListTr(4,elcount) = nextvertex

            if (nextvertex.eq.NextCcwNbr) then
!           element is quadrilateral
              TCode(elcount) = 4
              LISTERR(4) = LISTERR(4) + 1
              j = j + 1
              go to 60
            else
              if(retrowanted) then
                followboundary = .true.
!          element has 5 or more sides - assume it is a boundary, not an element
                NUMofBDYS = NUMofBDYS + 1
!      store these 4 nodes top down in array RNODE( ) - note that outer boundary
!      will thus be in clockwise order and islands in ccw order. Dirns. will have 
!      to be reversed on output
                do m = 1,4
                  rnodes(Nfound+m) = ListTr(m,elcount)
                end do
                Nfound = Nfound + 4
                NUMperBDY(NUMofBDYS) = 4
                firstnode = ListTr(1,elcount)

!           look for 5th, 6th, ... nodes on this boundary
                do while(followboundary)
                  nextvertex = NextVer(rnodes(Nfound-1),rnodes(Nfound), &
     &                        numn(rnodes(Nfound)),nl,nbtot,itot)
!             kill element if lower-numbered node is encountered      
                  if(nextvertex.lt.IN) then
                    Nfound = Nfound - NUMperBDY(NUMofBDYS)
                    NUMofBDYS = NUMofBDYS - 1
                    elcount = elcount - 1
                    j = j + 1
                    go to 60
                  endif
                  if(nextvertex.ne.firstnode) then
!               boundary is not yet closed, add node to list of boundary nodes
                    Nfound = Nfound + 1
                    rnodes(Nfound) = nextvertex
                    NUMperBDY(NUMofBDYS) = NUMperBDY(NUMofBDYS) + 1

                  else
!             boundary now closed
                    followboundary = .false.               
                  endif
                enddo
              endif
              elcount = elcount - 1
              j = j + 1
              go to 60
            endif
          endif

60      enddo

      enddo  !itot loop

      TotTr = 0
      do j=1,elcount
        if(TCode(j).eq.3) then
          TotTr = TotTr + 1
          do k = 1,3
            ListTr(k,TotTr) = ListTr(k,j)
          enddo
          ListTr(4,TotTr) = 0
        TCode(TotTr) = 1
        elseif(TCode(j).eq.4) then
          TotTr = TotTr + 1
          do k = 1,4
            ListTr(k,TotTr) = ListTr(k,j)
          enddo
        TCode(TotTr) = 1
        endif
      enddo

!     output of RETRONODE operations - added 25/9/98
      if (retrowanted) then
!      If retrowanted was set true when element_lister was called,then at this
!      point, rnodes( ) holds the various boundaries, each in reverse of required
!      order.
!            First boundary has NUMperBDY(1) nodes, which are in
!              rnodes(1),...,rnodes(NUMperBDY(1))
!            Second boundary has NUMperBDY(2) nodes, which are in
!              rnodes(NUMperBDY(1)+1),....,rnodes(NUMperBDY(1)+(NUMperBDY(2))
!            and so on.

!      First step is to determine which of the NUMofBDYS boundaries is the outer.
!      Any of the extreme values must lie on outer boundary
!        - find minimum x-coordinate over all boundary nodes
        minimumx = 1.E+20
        do k = 1, Nfound
          if(minimumx.lt.dxray(rnodes(k))) minimumx = dxray(rnodes(k))
        end do

!      now find which boundary has the node with minimum x

        sumofnodes = 0
        do k = 1, NUMofBDYS
          do m = sumofnodes+1,sumofnodes+ NUMperBDY(k)
            if(dxray(rnodes(m)).le.minimumx) then
              numouter = k
!           the numouter-th boundary is the outer one
              presum = sumofnodes
!           presum is number of nodes in rnodes( ) prior to start of outer boundaary
!           outer boundary (in ccw order) consists of nodes
!           rnodes(presum+NUMperBNDY(numouter)) to rnodes(presum+1) 
              go to 444
            endif
          end do
          sumofnodes = sumofnodes + NUMperBDY(k)
        end do
444     continue

        if(PigOpenFile(23,'Recovered node file',retroname, &
              'Node File (*.nod),*.nod;All Files (*.*),*.*;') ) then
          fnlen = len_trim( retroname )
        ELSE
          close (23)
          return
        ENDIF
!        open(23,file='retronode.nod',status='unknown')

        write(23,'(a4)') '#NOD'
        write(23,'(4(1X,F13.5),1x,I4)') x0off,y0off,scaleX,scaleY,     &
     &                                     igridtype

        NumIntBndys = 0
        write(23,*) itot
        write(23,*) NUMofBDYS, NumIntBndys
!     output outer boundary
        write(23,*) NUMperBDY(numouter)
        do m = 1, NUMperBDY(numouter)
!          rindex = presum + NUMperBDY(numouter) + 1 - m
          rindex = sumofnodes + NUMperBDY(numouter) + 1 - m
          write(23,*) dxray(rnodes(rindex)),dyray(rnodes(rindex)),depth(rnodes(rindex))
        end do
!     write other boundaries 
        sumofnodes = 0
        do k = 1, NUMofBDYS
          if(k.eq.numouter) then
!        outer boundary - already output to file
            sumofnodes = sumofnodes + NUMperBDY(k)
          else
            write(23,*) NUMperBDY(k)

            do m = 1, NUMperBDY(k)
              rindex = sumofnodes + NUMperBDY(k) + 1 - m
              write(23,*) dxray(rnodes(rindex)),dyray(rnodes(rindex)), &
              depth(rnodes(rindex))
            end do
            sumofnodes = sumofnodes + NUMperBDY(k)

          endif     
        enddo
!     write interior nodes
!      first mark all Nfound nodes already found on boundaries  
!      counter variable NUMN(I) is free at this point     
        do k = 1, Nfound
          NUMN(rnodes(k)) = -NUMN(rnodes(k))-1         
        end do

        write(23,*) itot-Nfound
        do k = 1,itot
          if(NUMN(k).ge.0) then
            write(23,*) dxray(k),dyray(k),depth(k)
          else
!           restore NUMN() for boundary node (not strictly necessary)
            NUMN(k) = -(NUMN(k)+1)
          endif
        end do
        close (23)
      endif

      RETURN
      END
!************************************************************************
!************************************************************************
      SUBROUTINE CwwNbr(INODE,ThisJ,NextCcwNbr,NextJ,numin,nbm,nm,nl)

!     Purpose: Given position ThisJ in neighbour list of node INODE
!              routine returns the next neighbour of INODE counter-
!              clockwise, i.e. NextCcwNbr, and its position, NextJ, in
!              neighbour list of INODE, allowing for cyclic nature of 
!              list and assuming neighbours of all nodes already ccw

      IMPLICIT NONE

! *** Passed variables ***
      INTEGER INODE, ThisJ, numin, nbm, nm
      integer nl(nbm,nm+1)

! *** Local variables
      INTEGER NextJ, NextCcwNbr

! ----BEGIN------------------------------------------------------------

      if (ThisJ.lt.numin) then !numn(INODE))then
        NextJ = ThisJ + 1
      else
        NextJ = 1
      endif

      NextCcwNbr = NL(NextJ,INODE)

      RETURN
      END
!************************************************************************
      INTEGER FUNCTION NextVer(NODE, NBR, numnbr, nl, nbm, nm)

!     Purpose: Given some neighbour, with index NBR, of node NODE -
!              routine returns the next neighbour of NBR clockwise 
!              from NODE (generally not same as next neighbour of NODE 
!              ccw from NBR).
!              Assumes neighbours of all nodes already sorted ccw
! 
!     Use: When finding the vertices of a polygon in ccw direction,
!          next node is NextVer (thisnode,lastnode)

      IMPLICIT NONE

! *** Passed variables ***
      INTEGER NODE, NBR, numnbr, nbm,nm
      integer nl(nbm,nm+1)

! *** Local variables
      INTEGER K

! ----BEGIN------------------------------------------------------------

!     Find where NODE sits in neighbour list of NBR, and take preceding
!                                  neighbour in clockwise direction
      do k = 1, numnbr  !NUMN(NBR)
        if (NODE.eq.NL(k,NBR)) then
!         thus NODE is kth neighbour of NBR
          if (k.ne.1) then 
            NextVer = NL(k-1,NBR)
          else
            NextVer = NL(numnbr,nbr)  !NUMN(NBR),NBR)
          endif
        endif
      enddo

      RETURN
      END
!************************************************************************
!************************************************************************
      SUBROUTINE SORTNBRS(INODE,NBRS,SNB,nm,nbtot,nl,x,y)

!       Purpose: To sort neighbours of current node INODE into
!                counterclockwise order

      IMPLICIT NONE

! *** PASSED VARIABLES

      integer nm,nbtot,nl(nbtot,nm+1)
      integer INODE, NBRS, SNB(NBTOT+1)
      real x(nm),y(nm)
!       INODE - index of current node
!       NBRS  - number of neighbours of current node
!       SNB( ) - sorted neighbours & SNB(NBRS+1) = SNB(1)

! *** LOCAL VARIABLES

      REAL XDIFF,YDIFF,X1,Y1,X2,Y2
      integer I,J,N, TNb1, TNb2
      integer OCT,UNB(NBTOT),NUinOCT(8),NBinOCT(8,nbtot)
!       OCT - octant counter
!       UNB( ) - packed, unsorted neighbours  
!       NUinOCT(I) - No. of neighbours found in octant I
!       NBinOCT(I, ) - Indices of neighbours found in octant I
!       TNb1( ) - temporary first nbr in sort within octant
!       TNb2( ) - temporary second nbr in sort within octant

! ----BEGIN------------------------------------------------------------
!     Find total no. of nbrs of INODE and form packed unsorted list
      NBRS = 0
      unb = 0
      snb = 0
      DO I = 1, NBTOT
        IF(NL(I,INODE).GT.0) THEN
          NBRS = NBRS + 1
          UNB(NBRS) = NL(I,INODE)
        ENDIF
      enddo

!     Set counts of neighbours in octants to zero
!      DO I = 1,8
        NUinOCT = 0
        NBinOCT = 0
!      enddo

!     Sort nbrs into octants, update counts, store nbrs by octant
      DO I = 1, NBRS
        XDIFF = X(UNB(I)) - X(INODE)
        YDIFF = Y(UNB(I)) - Y(INODE)
        CALL FINDOCT(OCT,XDIFF,YDIFF) !INODE,UNB(I))
        NUinOCT(OCT) = NUinOCT(OCT) + 1
        NBinOCT(OCT,NUinOCT(OCT)) = UNB(I)
      enddo

!     If more than one nbr in octant, do ccw sort within octant

      N = 0

!         DO 30 OCT = 1,8
      OCT = 1
      DO 30 WHILE ((OCT .LE. 8) .AND. (N.LT.NBTOT)) 

        IF ( NUinOCT(OCT) .EQ. 1 ) THEN
          N = N + 1
          SNB(N) = NBinOCT(OCT,1)
        ELSEIF (NUinOCT(OCT) .gt. 1 ) THEN
          DO 20 I = 1, NUinOCT(OCT)          
            DO 20 J = 1, NUinOCT(OCT) - I     
              TNb1 = NBinOCT(OCT,J)
              TNb2 = NBinOCT(OCT,J+1)
              X1 = X(TNb1) - X(INODE)
              X2 = X(TNb2) - X(INODE)
              Y1 = Y(TNb1) - Y(INODE)
              Y2 = Y(TNb2) - Y(INODE)
!               Check cw v.ccw order by vector cross-product
              IF(X1*Y2-X2*Y1.LT.0)THEN 
                NBinOCT(OCT,J) = TNb2
                NBinOCT(OCT,J+1) = TNb1
              ENDIF
20        CONTINUE
          DO 25 I = 1, NUinOCT(OCT)
            N = N + 1
            IF (N.LE.NBTOT) THEN
              SNb(N) = NBinOCT(OCT,I)
            ENDIF
25        CONTINUE

        ENDIF

!         Following reset is necessary
        NUinOCT(OCT)=0
        OCT = OCT +1
30    CONTINUE

      SNb(N+1) = SNb(1)
!       
      RETURN
      END

! *********************************************************************
! *********************************************************************
      SUBROUTINE FINDOCT(OCT, X, Y ) !P1, P2)

!     Purpose: Finds which octant (45 degree sector) node P2 lies in
!               relative to node P1, by first sorting into quadrants,
!               then rotating axes 45 degrees and sorting into new
!               quadrants. Octants numbered counterclockwise from ENE

      IMPLICIT NONE

! *** PASSED VARIABLES ***
      integer   OCT !, P1, P2
      REAL    X, Y

! *** LOCAL VARIABLES ***   
      LOGICAL Up, Rite, UpRite, LeftUP

! ----BEGIN------------------------------------------------------------

      Up = Y .GE. 0
      Rite = X .GE. 0
      UpRite = Y+X .GE. 0
      LeftUp = Y-X .GE. 0

      IF ( Up ) THEN
        IF ( Rite ) THEN
          IF (.NOT. LeftUp ) THEN
            OCT = 1
          ELSE
            OCT = 2
          ENDIF
        ELSE 
          IF ( UpRite ) THEN
            OCT = 3
          ELSE
            OCT = 4
          ENDIF
        ENDIF
      ELSE
        IF (.NOT. Rite ) THEN
          IF ( LeftUp ) THEN
            OCT = 5
          ELSE
            OCT = 6
          ENDIF
        ELSE
          IF (.NOT. UpRite ) THEN
            OCT = 7
          ELSE
            OCT = 8
          ENDIF
        ENDIF
      ENDIF

      end

!*******************************************************************************
!*******************************************************************************

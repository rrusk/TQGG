!*********************************************************************  
 
      subroutine ReadnetCDFData(fname,Quit)

      use MainArrays
      USE UGrid_netCDFio
      
      implicit none
      
      INCLUDE '../includes/defaults.inc'
      
! *** passed varaibles
      character(len=*) :: fname
      logical quit

! *** local varaibles
      integer j,k,np,ne,ncn,iXYcoord,iZcoord,istat
      integer, allocatable :: nen(:,:)
      REAL*8    XMAX, YMAX, XMIN, YMIN
     
      call Read_GridSize_netCDF ( fname,np,ne,ncn,quit )
      if(quit) return
      write(*,*) ' np,ne,ncn=',np,ne,ncn
      itot = np
      TotTr = ne
      UTMzone = 'nul'
      if(ncn.eq.4) then
        call Read_Grid_netCDF ( np,ne,ncn,dxray,dyray,depth,code,Tcode,ListTr,&
                                iXYcoord,iZcoord,quit )
        if(quit) return
      elseif(ncn.eq.3) then
        ListTr = 0
        allocate(nen(3,TotTr),stat=istat)
        call Read_Grid_netCDF ( np,ne,ncn,dxray,dyray,depth,code,Tcode,nen,&
                                iXYcoord,iZcoord,quit )
        if(quit) return
        do j=1,TotTr
          do k=1,3
            ListTr(k,j) = nen(k,j)
          enddo
        enddo
        deallocate(nen)
      else
        Quit = .true.
      endif
      igridtype = iXYcoord
      izup = iZcoord

!      call Read_BoundarySize_netCDF (nbp,nbnd,nbnd1,err)
    
!      call Read_Boundary_netCDF (nbp,nbnd,bnode_index,bnode_id,bnodes,err)
    
      call Close_netCDF (quit)
      if(quit) return

! *** generate neighbor list

      CALL GenerateNL(itot,nbtot,nbtotr,NL,TotTr,ListTr)

      nbtotr = nbtot !expand to max !max(nbtotr,nbtot_max)
      DispNodes = .false.

      xmin = minval(dxray(1:itot))
      xmax = maxval(dxray(1:itot))
      ymin = minval(dyray(1:itot))
      ymax = maxval(dyray(1:itot))
      call fullsize(xmin,ymin,xmax,ymax)
     
      end subroutine

!*********************************************************************  
 
      subroutine WritenetCDFData(fname,Quit)
 
      use MainArrays
      USE UGrid_netCDFio
     
      implicit none
      
! *** passed varaibles
      character(len=*) :: fname
      logical quit

! *** local variables
      integer :: j,k,nbp
      integer :: np,ne,ncn,ixy,iz,ncount,dcount
      integer :: nindex(itot),nen(3,TotTr)
      integer :: NUMofBDYS, bnode_index(itot), bnode_id(itot), rnodes(2*itot)
      logical :: err
      
      quit = .false.
      err = .false.
      
      write(*,*) 'called WritenetCDFData(nunit,Quit)'//fname
!      call PigMessageOK( 'netCDF not enabled. Recomplie program.','ReadGrid' )
!      Quit = .true.
      
! *** set up index array for deleted nodes

      ncount = 0
      dcount = 0
      do j=1,itot
        if(code(j).lt.0) then
          nindex(j) = 0
          dcount = dcount + 1
        else
          ncount = ncount+1
          nindex(j) = ncount
          if(ncount.lt.j) then
            dxray(ncount) = dxray(j)
            dyray(ncount) = dyray(j)
            depth(ncount) = depth(j)
            code(ncount) = code(j)
          endif
        endif
      enddo

      itot = ncount
      np = itot
      ne = TotTr
      ncn = 3
      do j=1,TotTr
        do k=1,3
          ListTr(k,j) = nindex(ListTr(k,j))
          nen(k,j) = ListTr(k,j)
        enddo
        if(ListTr(4,j).gt.0) then
          ncn = 4
          ListTr(4,j) = nindex(ListTr(4,j))
        endif
      enddo
      ixy = igridtype
      if(scaleY.lt.0.) then
        iz = 0
      else
        iz = 1
      endif
      call Create_Grid_netCDF (np,ne,ncn,iXY,iZ,fname,err)
      if(err) then
        quit = .true.
        return
      endif

      if(ncn.eq.3) then
        call Write_Grid_netCDF (np,ne,ncn,dxray,dyray,depth,code,Tcode,nen,err)
      elseif(ncn.eq.4) then
        call Write_Grid_netCDF (np,ne,ncn,dxray,dyray,depth,code,Tcode,ListTr,err)
      endif
      if(err) then
        quit = .true.
        return
      endif
      
      call Boundary_list(np,nl,nbtot,dxray,dyray,code, &
                         nbp,NUMofBDYS,bnode_index, bnode_id,rnodes)

      call Write_Boundary_netCDF (nbp,NUMofBDYS,bnode_index, bnode_id,rnodes,err)

                 
      end subroutine

!************************************************************************
        
        SUBROUTINE Boundary_list(itot,nl,nbtot,dxray,dyray,ncode,&
                                 nbp,nbnd,bnode_index, bnode_id,rnodes)

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

    
!       Purpose: To form a boundary list for a grid.
!                - Some error detection capability.
!
!       Input variables
!       itot   = number of nodes in grid
!       nbtot   = number adjacent nodes


! *** Passed variables ***

      integer, intent(in) :: itot,nbtot,ncode(itot)
      integer, intent(inout) :: nl(nbtot,itot+1)
      INTEGER, intent(out) :: RNODES(2*itot),nbnd
      integer, intent(out) :: nbp, bnode_index(itot+1), bnode_id(itot)
      real*8, intent(in) :: dxray(itot), dyray(itot)


! *** Local variables ***

      INTEGER NextVer                     ! Integer functions
      integer ListTr(4)
      INTEGER IN, SNB(NBTOT+1)
      INTEGER numverts, ThisJ, NextCcwNbr, NextJ, NextVertex
      INTEGER I,J,K,M
!      LOGICAL ELIVE

!      ListTr(1, ) to ListTr( ,4) are nodes of element in ccw order

      INTEGER NUMN(itot)   ! no. of neighbours of each node

!---  variables added 25/9/98 for RETRONODE operation ------------------------

      integer, parameter :: maxnnb=10000
      integer, save :: NUMperBDY(MAXNNB)
      INTEGER Nfound,NUMofBDYS

!     PASSED variable added 25/9/98 for RETRONODE operation
      LOGICAL :: retrowanted=.true.

!    Local variables added 25/9/98 for RETRONODE operation
      INTEGER firstnode
      LOGICAL followboundary

!     Local variables added 25/9/98 for output of retrieved node file
      INTEGER  sumofnodes, numouter, presum
      REAL*8 minimumx

!---  end of extra variable section ------------------------------------------

! ----BEGIN------------------------------------------------------------

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
 !     LISTERR = 0

! ----------------------------------------------------------------------
!     BEGIN SEARCH FOR ELEMENTS
!     For each node in grid
      DO IN = 1, itot

!       first look after degenerate elements, nodes with 0 or 1 neighbours 
        if (numn(IN).lt.2) then
!           case of node with no or one neighbour.
          cycle
        endif

!       - from here on, node IN assumed to have at least 2 neighbours
!      Begin element check around each node, starting from first neighbour
        j = 1
!        do 60 while (j.le.numn(IN))
        do while (j.le.numn(IN))
          ThisJ = j
!          ELIVE = .true.      ! element still valid

!      begin new element with line from node IN to its Jth neighbour
          ListTr(1) = IN        ! first node of fresh element is node IN
          ListTr(2) = NL(j,IN)
!         kill element if lower-numbered node is encountered      
          if(ListTr(2).lt.IN) then
            j = j + 1
            cycle !go to 60
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
          ListTr(3) = nextvertex

!         kill element if lower-numbered node is encountered      
          if(ListTr(3).le.IN) then
            j = j + 1
            cycle !go to 60
          endif
          if (nextvertex.eq.NextCcwNbr) then
!         element is triangle
            ListTr(4) = 0
            j = j + 1
            cycle !go to 60
          else
!           ELEMENT IS POLYGON with more than 3 sides or is not closed
            nextvertex = NextVer(NL(j,IN),ListTr(3), &
                                numn(ListTr(3)),nl,nbtot,itot)
!           kill element if lower-numbered node is encountered      
            if(nextvertex.lt.IN) then
              j = j + 1
              cycle !go to 60
            endif
            ListTr(4) = nextvertex

            if (nextvertex.eq.NextCcwNbr) then
!           element is quadrilateral
              j = j + 1
              cycle !go to 60
            else
              if(retrowanted) then
                followboundary = .true.
!          element has 5 or more sides - assume it is a boundary, not an element
                NUMofBDYS = NUMofBDYS + 1
!      store these 4 nodes top down in array RNODE( ) - note that outer boundary
!      will thus be in clockwise order and islands in ccw order. Dirns. will have 
!      to be reversed on output
                do m = 1,4
                  rnodes(Nfound+m) = ListTr(m)
                end do
                Nfound = Nfound + 4
                NUMperBDY(NUMofBDYS) = 4
                firstnode = ListTr(1)

!           look for 5th, 6th, ... nodes on this boundary
                do while(followboundary)
                  nextvertex = NextVer(rnodes(Nfound-1),rnodes(Nfound), &
     &                        numn(rnodes(Nfound)),nl,nbtot,itot)
!             kill element if lower-numbered node is encountered      
                  if(nextvertex.lt.IN) then
                    Nfound = Nfound - NUMperBDY(NUMofBDYS)
                    NUMofBDYS = NUMofBDYS - 1
!                    elcount = elcount - 1
                    j = j + 1
                    cycle !go to 60
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
!              elcount = elcount - 1
              j = j + 1
              cycle !go to 60
            endif
          endif

!60      enddo
        enddo

      enddo  !itot loop

!      rnodes( ) holds the various boundaries, each in reverse of required order.
!     
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

        sumofnodes = 0
        nbnd = 0
        do k = 1, NUMofBDYS
          nbnd = nbnd + 1
          bnode_index(nbnd) = sumofnodes + 1
          bnode_id(nbnd) = ncode(rnodes(sumofnodes + 1))
          do m = sumofnodes+1,sumofnodes+ NUMperBDY(k)
            if(ncode(rnodes(m)).ne.bnode_id(nbnd)) then !new segment
              nbnd = nbnd + 1
              bnode_index(nbnd) = m
              bnode_id(nbnd) = ncode(rnodes(m))
            endif
          end do
          sumofnodes = sumofnodes + NUMperBDY(k)
        end do
        nbp = sumofnodes
        bnode_index(nbnd+1) = sumofnodes + 1
        

! *** test boundary
      open(41,file='btestout.dat',status='unknown')
      write(41,*) sumofnodes
      write(41,*) NUMofBDYS
      m = 0
      do k=1,NUMofBDYS
        write(41,*) NUMperBDY(k)
        do j=1,NUMperBDY(k)
          m = m + 1
          write(41,*) dxray(rnodes(m)),dyray(rnodes(m)),ncode(rnodes(m))
        enddo
      enddo
      m = 0
      write(41,*) m
      close(41)

      RETURN
      END

!************************************************************************

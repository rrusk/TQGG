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

! --------------------------------------------------------------------------*
!     Routines to read ngh files. 
! --------------------------------------------------------------------------*

      SUBROUTINE OpenGridFile( Quit )

! Purpose: Prompts user for the input grid file.
! Givens : None
! Returns: None
! Effects: User is repeatedly prompted for a grid file until a valid
!          filename is entered.  Grid file is opened, points are read,
!          and grid file is closed.

      implicit none

      LOGICAL Quit
!      integer NREC

      INCLUDE '../includes/defaults.inc'

      CHARACTER*256 fle
      LOGICAL notok
      integer  nunit, Fnlen
      logical PigOpenFileCD
!------------------BEGIN------------------

      notok = Quit
      Quit = .FALSE.
      nunit = 8

      if(.not.PigOpenFileCD(nunit,'Open Grid File', fle,&
     &     'Neighbour Files (*.ngh),*.ngh;All Files (*.*),*.*;')) then
        Quit = .TRUE.
!        fnlen = len_trim( Fle )
!        call PigMessageOK('Error opening file '//fle(:fnlen),'OpenGrid')
        GridRName =  'NONE'
        return
      endif
      GridRName =  fle
      fnlen = len_trim( Fle )

      call PigPutMessage('Reading file '//fle(:fnlen))
      call REDATA (notok)
      quit = notok
      close( nunit )
      if(.not.quit) call DoCheckEdges()

      END

! --------------------------------------------------------------------------*

      SUBROUTINE ReData (quit)
   
! Purpose : To read grid data into memory

      use MainArrays

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/cntcfg.inc'

!     - PASSED VARIABLES
      INTEGER NREC
      LOGICAL Quit

      REAL    XMAX, YMAX, XMIN, YMIN

!     - LOCAL VARIABLES
      INTEGER i , ii , j , n_offset, nbtot_max ! ,nn
!        - counters
      INTEGER   irec
!        - the number of records that is to be in the data file
      INTEGER numrec, numrecmax
        character*80 message
       LOGICAL NewFile
       character(80) Firstline

!------------------BEGIN-------------------------------

      NewFile = Quit

      QUIT = .FALSE.
      
!     - initialize 
      TotTr = 0
      if(NewFile) then
        do j=1,MAXTRI
          TCode(j) = 1
        enddo
        do i=1,mrec
          do j=1,nbtot
            NL(j,i) = 0
          enddo
        enddo

        nrec = 1
        nbtot_max = 0
        GridSIndex = 9999999
      else
        nrec = itot + 1
        nbtot_max = nbtotr
        GridSIndex = nrec
      endif

      call PigPutMessage( 'Reading Grid file.. [NEIGH] format' )

      READ(8,'(a)', err=9999, end=99999) Firstline

      if(firstline(1:4).eq."#NOD") then  !node file, new format
        call PigMessageOK( 'Node file.. Wrong format for grid.','ReadGrid' )
        Quit = .true.
        Return
      elseif(firstline(1:4).eq."#XYE") then  !xyz and element grid file, new format
        call ReadXYEData (Quit)
        return
      elseif(firstline(1:4).eq."#NGH") then  !neigh grid file, new format
        do
          READ(8,'(a)', err=9999, end=99999) Firstline
          if(firstline(1:1).ne."#") then    !comment lines
!           following line is internal read of firstline          
! - read offsets, scale factors, coordinate type
            READ( firstline, *, err = 9999, end=99999 ) x0off, y0off, scaleX, scaleY, igridtype
            exit
          endif
        enddo

! - read max number of nodes in this grid
        read(8,*,err=9999, end=99999) numrec

! - read max number of neighbours required for this grid
        READ( 8, *, err = 9999, end=99999 ) NBTOTR

      else  ! old format
! - read max number of nodes in this grid
        read(firstline,*,err=9999, end=99999) numrec
! - read max number of neighbours required for this grid
        READ( 8, *, err = 9999, end=99999 ) NBTOTR
! - read offsets, scale factors, coordinate type
        READ( 8, *, err = 9999, end=99999 ) x0off, y0off, scaleX, scaleY
        igridtype = 0
      endif  ! if new or old NGH format

      numrecmax = NUMREC+nrec-1
      if ( NUMRECmax .gt. MREC ) then
        call PigMessageOK('WARNING: Grid file has too many points-setting to maximum','ReadGrid')
        NUMRECmax = MREC
        NUMREC = MREC -nrec +1
      endif

      if ( NBTOTR .gt. NBTOT ) then
        call PigMessageOK('WARNING: Grid file has too many neighbors-setting to maximum','ReadGrid')
        NBTOTR = NBTOT
      endif
      write(message,'(a,i6,a,i3,a)') 'Reading Grid File with ',NUMREC,' nodes, ',NBTOTR,' neighbours'
      call PigPutMessage(message)

      i = nrec
      ii = 0
      n_offset = nrec - 1
!      if(nrec.gt.1) then
!        GridSIndex = nrec
!      endif
   
10    continue  
      READ(8,*,end=999,err=9999) IREC,DXRAY(i),DYRAY(i),CODE(i),DEPTH(i),( NL(j,i),j = 1, NBTOTR )

      do j=1,NBTOTR
        if(NL(j,i).gt.0) then
          NL(j,i) = NL(j,i) + n_offset
        endif
      enddo

      NREC = i
      !EXIST(i) = .TRUE.   

      do j=1,NBTOTR
        if( (NL(j,i) .lt. 0).OR.(NL(j,i) .gt. NUMRECmax)) then
!            set illegal number to zero - that should 
!            delete any lines to points out of grid
          write(message,'(a,i8,a,i8)')'Eliminated connection from node ',i,' to node ',NL(j,i)
          call PigPutMessage(message)
!            call PigUWait(2.0)
          NL(j,i) = 0
        endif
      end do

      i = i + 1   
      ii= ii+ 1   
!     
      if(ii.ge.NUMREC) goto 999
      goto 10   
   
999   continue
      if(ii.ne.NUMREC) then
          write(message,'(a,i6,a,i6,a)')'WARNING: Premature end of file. Only ',ii,' of expected ',NUMREC,' nodes read in'
          call PigMessageOK(message,'ReadGrid')
!          call PigUWait(3.0)
          if(NREC.eq.0) then
                quit = .true.
           endif
      endif

      ITOT = i - 1
      NREC = i
      nbtotr = nbtot !expand to max !max(nbtotr,nbtot_max)
      DispNodes = .false.

!      nn = itot

      if(int(ScaleY).eq.-999) then
        xlongmin = minval(dxray(1:itot))
        xlongmax = maxval(dxray(1:itot))
        xlongsum=ScaleX
      endif

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

      RETURN
   
9999  continue  
      call PigMessageOK('ERROR reading grid file: Most likely a format error','ReadGrid' )
      Quit = .TRUE.
      return
      
99999 continue  
      call PigMessageOK('ERROR reading grid file: Premature end of file in header.','ReadGrid' )
      Quit = .TRUE.
      return
      END

! --------------------------------------------------------------------------*

      SUBROUTINE ReadXYEData (quit)
   
! Purpose : To read grid data into memory

      use MainArrays

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/cntcfg.inc'

!     - PASSED VARIABLES
      LOGICAL Quit

      REAL    XMAX, YMAX, XMIN, YMIN

!     - LOCAL VARIABLES
      INTEGER i , j 
!        - counters
      INTEGER   istat
!        - the number of records that is to be in the data file
      INTEGER numrec, numele
      character(80) Firstline

!------------------BEGIN-------------------------------

      QUIT = .FALSE.
!      exist = .FALSE.
      
! - read max number of nodes and elements in this grid
      read(8,*,IOSTAT=istat) numrec, numele
      if(istat.lt.0) then
        call PigMessageOK('ERROR end of file at line 2','ReadGrid' )
        Quit = .TRUE.
        return
      elseif(istat.gt.0) then
        call PigMessageOK('ERROR reading XYE file at line 2','ReadGrid' )
        Quit = .TRUE.
        return
      endif

      do i=1,numrec
        read(8,*,IOSTAT=istat) dxray(i),dyray(i),depth(i)
        if(istat.lt.0) then
          call PigMessageOK('ERROR premature end of file','ReadGrid' )
          Quit = .TRUE.
          return
        elseif(istat.gt.0) then
          call PigMessageOK('ERROR reading XYE file: Most likely a format error','ReadGrid' )
          Quit = .TRUE.
          return
        endif
        !EXIST(i) = .TRUE.   
      enddo

      code = 0   
      itot = numrec

!  parse first line to find format      
      READ(8,'(a)',IOSTAT=istat ) Firstline
      if(istat.lt.0) then
        call PigMessageOK('ERROR premature end of file','ReadGrid' )
        Quit = .TRUE.
        return
      elseif(istat.gt.0) then
        call PigMessageOK('ERROR reading XYE file: Most likely a format error','ReadGrid' )
        Quit = .TRUE.
        return
      endif

      read(Firstline,*,IOSTAT=istat)  (ListTr(j,1),j=1,4),TCode(1)
      if(istat.eq.0) then
        do i=2,numele
          read(8,*,IOSTAT=istat) (ListTr(j,i),j=1,4),TCode(i)
          if(istat.lt.0) then
            call PigMessageOK('ERROR premature end of file','ReadGrid' )
            Quit = .TRUE.
            return
          elseif(istat.gt.0) then
            call PigMessageOK('ERROR reading XYE file: Most likely a format error','ReadGrid' )
            Quit = .TRUE.
            return
          endif
        enddo
      else
        read(Firstline,*,IOSTAT=istat)  (ListTr(j,1),j=1,3)
        if(istat.ne.0) then
          call PigMessageOK('ERROR reading XYE file: error in element list','ReadGrid' )
          Quit = .TRUE.
          return
        endif
        ListTr(4,1) = 0
        TCode(1) = 1
        do i=1,numele
          read(8,*,IOSTAT=istat) (ListTr(j,i),j=1,3)
          if(istat.lt.0) then
            call PigMessageOK('ERROR premature end of file','ReadGrid' )
            Quit = .TRUE.
            return
          elseif(istat.gt.0) then
            call PigMessageOK('ERROR reading XYE file: Most likely a format error','ReadGrid' )
            Quit = .TRUE.
            return
          endif
        enddo
      endif

      TotTr = numele

! *** generate neighbor list
      nindx = mrec
      CALL ALTER (nindx,itot,code,nbtot,nbtotr,NL,TotTr,ListTr,& !TCode,&
                     TotBndys,TotIntBndys,PtsThisBnd)

      nbtotr = nbtot !expand to max !max(nbtotr,nbtot_max)
      DispNodes = .false.

      if(int(ScaleY).eq.-999) then
        xlongmin = minval(dxray(1:itot))
        xlongmax = maxval(dxray(1:itot))
        xlongsum=ScaleX
      endif

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
      END

! --------------------------------------------------------------------------*

      SUBROUTINE DoCheckEdges()

!  Purpose:
!  1.  To check that all edges defined by and node's neighbour list are
!      also defined by the neighbouring node's neighbour list. This check
!      is necessary because the definition of each edge is defined in two
!      places: once in each nodes neighbour list. This means it is easy
!      for one of the lists to be modified and the other not modified.
!      For any edges defined only at one end, we add the other end's 
!      definition.
!  2.  To check that the neighbours in any node's neighbour list are
!      not listed more than once. Duplicates are deleted.
!  3.  To check that the neighbours exist. If they do not exist, they are
!      deleted from the neighbour list.
!  4.  To ensure that a node is not connected to itself. If it is, this
!      connection is deleted.
!  Method:
!      For each node in turn, check 2 is implemented first, and duplicates
!      removed without confirming with the user, but with information.
!      Then for each neighbour, the neighbours neighbour list is checked
!      to ensure that it contains this node. If not, it is added, subject
!      to user confirmation.

! dummy arguments
!      integer Nrec
! include files

      use MainArrays

! local variables
      integer i, j, k, nei, last
      character*80 cstr
! code
      do i=1,itot !Nrec-1
!          if(exist(i).or.code(i).ge.0)then
          if(code(i).ge.0)then
! test 3: remove non-existent neighbours
              do j=1,nbtot
                  if(NL(j,i).gt.0)then
!                      if(.not.exist(NL(j,i)).or.code(i).lt.0) then
                      if(code(i).lt.0) then
!                         pointing to a non-existent node
!                          NL(j,i) = 0
                          write(cstr,'(a,i7,a,i7)')'Removing connection from node ',i,' to non-existent (deleted) node',NL(j,i)
                          call PigPutMessage(cstr)
                          NL(j,i) = 0
                      else if(NL(j,i).gt.itot) then
!                         pointing to a node > last node
                          write(cstr,'(a,i7,a,i7)')'Removing connection from node ',i,' to non-existent ( >Nrec) node',NL(j,i)
                          call PigPutMessage(cstr)
                          NL(j,i) = 0
                      else if(NL(j,i).eq.i) then
! test 4: cannot be joined to self
                          write(cstr,'(a,i7,a)')'Removing connection from node ',i,' to itself!'
                          call PigPutMessage(cstr)
                          NL(j,i) = 0
                      endif
                  endif
              enddo
! test 2: compress neighbour list
!             step 1: replace duplicates with zeros
              do j=1,nbtot
                  if(NL(j,i).gt.0) then
                    do k=j+1,nbtot
                      if(NL(j,i).eq.NL(k,i)) then
!                         duplicate found
                          write(cstr,'(a,i6,a,i6,a,i6)')'Removing duplicated neighbour ',k,'(',NL(k,i),') from node',i
                          call PigPutMessage(cstr)
                          NL(k,i) = 0
                      endif
                    enddo
                  endif
              enddo
!             step 2: remove zero neighbours, move others to left in list
              last = nbtot
              do while  ((NL(last,i).eq.0).and.(last.gt.1))
                  last = last - 1
              enddo
              do j=1,last
                  if(NL(j,i).eq.0) then
                      do k=j,last
                          NL(k,i) = NL(k+1,i)
                      enddo
                  endif
              enddo
! test 1: check neighbour list of all neighbours includes this node
              do j=1,nbtot
                  nei = NL(j,i)
                  if(nei.ne.0) then
                      k=1
                      do while(    (NL(k,nei).ne.i).and.(k.lt.nbtot))
                          k = k+1
                      enddo
                      if(NL(k,nei).ne.i) then
!                         this node not found in neighbour's neighbour list
                          write (cstr,'(a,i7,a,i7)') 'Adding node ',i,' to neighbour list of node ',nei
                          call PigPutMessage(cstr)
                          k = 1
                          do while(    (NL(k,nei).ne.0).and.(k.lt.nbtot))
                              k = k + 1
                          enddo
                          if(k.le.nbtot)then
                              NL(k,nei) = i
                          else
                              call PigPutMessage('Too many neighbours already to add new one.')
                          endif
                      endif
                  endif
              enddo
          endif
      enddo

      end

! -------------------------------------------------------------------

      SUBROUTINE MERGE_GRID()

      use MainArrays

      IMPLICIT NONE

      INCLUDE '../includes/defaults.inc'

! *** Local variables ***
      character PigCursYesNo*1
      CHARACTER*80 ans, cstr
      INTEGER JJ, KK, LL
      REAL DIST, DISTMIN

      cstr= 'Merge coincident nodes ?:'
      ans = PigCursYesNo (cstr)
      if (ANS(1:1) .ne. 'Y') then
        return
      endif

      DO JJ = GridSIndex, itot !nrec   !1, NREC
!        IF(EXIST(JJ).and.(CODE(JJ).ge.0)) then
        IF(CODE(JJ).ge.0) then
! *** Find suitable minimum radius around node JJ; for present,
! *** use one fifth of distance to nearest existing neighbour.
          DISTMIN = 1.E+20
          DO 203 LL = 1, NBTOT
            if(NL(LL,JJ).ne.0) then
              DIST = ABS(DXRAY(JJ)-DXRAY(NL(LL,JJ))) + ABS(DYRAY(JJ)-DYRAY(NL(LL,JJ)))
              IF (DIST.le.DISTMIN) DISTMIN = DIST
            endif
203       CONTINUE
          DISTMIN = DISTMIN/5
          DO KK = 1, GridSIndex-1 !NREC
!            IF(EXIST(KK).and.(KK.NE.JJ).and.(CODE(KK).ge.0)) then
            IF((KK.NE.JJ).and.(CODE(KK).ge.0)) then
! *** Check if KK close enough to JJ to merge
              DIST = ABS(DXRAY(JJ)-DXRAY(KK)) + ABS(DYRAY(JJ)-DYRAY(KK))
              IF (DIST.le.DISTMIN) THEN
! *** Merge nodes KK and JJ, treating result as JJ
                call Transparent( JJ, KK)
! *** Set computational code of node JJ to required value
                if(code(jj).eq.6.or.code(kk).eq.6) then
                  code(kk) = 1
                elseif(code(jj).eq.91.and.code(kk).eq.91) then
                  CODE(kk) = 0
                endif
                code(jj) = -9
              ENDIF
            ENDIF
          enddo
        ENDIF
      enddo

      return
      END

!*----------------------------------------------------------------------*
!       Routines for reading and writing node data to file.                        *
!       ROUTINES: SaveNFinal, SaveNInterim, SaveNPoly, ShiftNodes,      *
!                 GetShift, OpenNodeFile, ReadNodeFile.                 *
!*----------------------------------------------------------------------*

      SUBROUTINE OpenNodeFile ( Quit )

!  PURPOSE:  To prompt user to enter a filename or "QUIT".
!    GIVEN:  NodeRName = 'NONE' if no current Node file.
!            User will input a filename.
!  RETURNS:  Quit - True if user choses to exit without opening file.
!  EFFECTS:  If specified file exists, ReadNodeFile is called to open
!            file on UNIT 3, else Quit will be true.
!*----------------------------------------------------------------------*

      implicit none      

      logical quit
!      integer nrec

!  - "INCLUDES"
      include '../includes/defaults.inc'

!  - LOCAL VARIABLES
      CHARACTER*256 FName
!      , ans
      CHARACTER*80 cstr
      integer Fnlen, nunit
      logical PigOpenFileCD

! ----------------START ROUTINE------------------------------------------

      quit = .true.
      nunit = 3

      if(PigOpenFileCD(nunit,'Open Node File', FName,&
             'Node Files (*.nod),*.nod;All Files (*.*),*.*;')) then
        quit = .false.
        fnlen = len_trim(Fname)
        call PigEraseMessage
        cstr = 'Reading File...[NODE] format.'
        call PigPutMessage ( cstr )
        call ReadNodeFile ( Quit )
        close(nunit)
        NodeRName = FName
        call PigEraseMessage
        DispNodes = .true.
      ENDIF

      END

!*-------------------------------------------------------------------------*

      SUBROUTINE ReadNodeFile ( Quit )

!  PURPOSE: To read NODE file data into memory arrays.
!    GIVEN: OpenNodeFile has specified an existing file, Fname, to open
!           and read.
!  RETURNS: dxray(),dyray() = arrays of coordinates of boundary and interior
!                             nodes with boundaries first,
!           depth() = array of depths at each node,
!           minx,miny = lower left NDC coordinate,
!           maxx,maxy = upper right NDC coordinate,
!  EFFECTS: Data from NODE file is in memory arrays in Common NODES
!           in NODESTOR.INC.
!  BUG: can read in and initialize arrays incorrectly if the external data
!       file is too large. It should truncate the read, setting all variables
!       appropriately.
!-----------------------------------------------------------------------*

      use MainArrays

! - "INCLUDES"
      INCLUDE '../includes/cntcfg.inc'

! -  Only boundaries are displayed when OUTLINEONLY is .TRUE.
      LOGICAL OUTLINEONLY
      COMMON /OUTLINE/ OUTLINEONLY

! - LOCAL VARIABLES
      INTEGER start, end, ii, jj, idiff, jjmax, bcode ! , nrec
      CHARACTER*80 cstr, firstline
      LOGICAL Quit
      REAL    XMAX, YMAX, XMIN, YMIN
      real xd, yd, dep
!       start,end - to mark indices of a set of boundary nodes or
!                   the set of interior nodes while reading
!       ii,jj - indices to increment from start to end
!       idiff - # nodes (if any) that have been excluded due to exceeding max
!               allowable nodes.

! -------------------START ROUTINE-----------------------------------

      x0off = 0.
      y0off = 0.
      scaleX = 1.
      scaleY = 1.
      start = 1
      end = 1
      outlineonly = .false.
      TotCoords = 0
      TotBndys = 0
      TotIntBndys = 0
      TotIntPts = 0
      Quit = .TRUE.

      READ(3,'(a)', err=990) Firstline

      if(firstline(1:4).eq."#NGH") then  !node file, new format
        call PigMessageOK( 'NGH file.. Wrong format for node file.','ReadNode' )
        Quit = .true.
        Return
      elseif(firstline(1:4).eq."#XYZ".or.firstline(1:4).eq."#XYE") then  !xyz and element grid file, new format
        call ReadXYZData (Quit)
        return
      elseif(firstline(1:4).eq."#NOD") then  !new node format
        do
          READ(3,'(a)', err=990) Firstline
          if(firstline(1:1).ne."#") then    !comment lines
            read(firstline,*,err=990)x0off,y0off,scaleX,scaleY,igridtype
            exit
          endif
        enddo
!       - read # of nodes total ( TotCoords )
        READ(3,*,err=9901) TotCoords
!       - read # of boundaries total ( TotBndys ) and number of internal lines
        READ(3,*,err=9902) TotBndys, TotIntBndys
      else
        rewind(3)
!       - read # of nodes total ( TotCoords )
        READ(3,*,err=9901) TotCoords
!       - read # of boundaries total ( TotBndys )
        READ(3,*,err=9902) TotBndys
      endif

      IF ( TotBndys .gt. Maxnnb ) THEN
        cstr = 'INPUT HAS TOO MANY BOUNDARIES'
        call PigMessageOK ( cstr,'ReadNode' )
        TotBndys = Maxnnb
      ENDIF
      write(cstr,'(a,i7,a,i4,a)') 'Reading ',TotCoords,' from ',TotBndys,' boundaries'
      call PigPutMessage(cstr)

!       - loop thru boundaries
      DO ii = 1, TotBndys
!         - read # nodes in each boundary ( ii )
        READ(3,*,END=70) PtsThisBnd(ii)
!         - end = end of nodes on boundary ( ii )
        end = ( start - 1 ) + PtsThisBnd(ii)
!         - loop thru node coordinates for boundary ( ii )
        write(cstr,'(a,i7,a,i4)') 'Reading ',PtsThisBnd(ii),' from boundary', ii
        call PigPutMessage(cstr)
        if(ii.eq.1) then
          bcode = 1
        else
          bcode = 2
        endif

        DO jj = start, end
          read(3,*,end=701) xd,yd,dep
          jjmax = jj
          if(jj.le.MaxPts) then
            dxray(jj) = xd
            dyray(jj) = yd
            Depth(jj) = dep
            !Exist(jj) = .true.
            code(jj) = bcode    
          endif
        END DO
        IF ( end .gt. MaxPts ) THEN
          cstr = 'MODEL HAS TOO MANY NODES - Reading start of file'
          call PigMessageOK ( cstr,'ReadNode' )
          PtsThisBnd(ii) = MaxPts - start + 1
          end = start - 1 + PtsThisBnd(ii)
          write(cstr,'(a,i7,a,i4)') 'PtsThisBnd reduced to ',PtsThisBnd(ii),' for boundary', ii
          call PigMessageOK ( cstr,'ReadNode' )

        ENDIF
!         - start = beginning of next set of boundary nodes
        start = end + 1
      END DO
!       - if no boundaries were read, start = 1
      IF ( TotBndys .eq. 0 )  start = 1
!       - read interior points, if there are any
      IF ( TotCoords .gt. end ) THEN
!            - read # of non-boundary points ( TotIntPts )

        READ(3,*,END=70) TotIntPts
!            - end = end of interior points
        end = start - 1 + TotIntPts
        IF ( end .gt. MaxPts ) THEN
          idiff = end - MaxPts
          call PigEraseMessage
          cstr = 'MODEL HAS TOO MANY POINTS'
          call PigMessageOK ( cstr,'ReadNode' )
          end = MaxPts
          TotIntPts = TotIntPts - idiff
        ENDIF
!            - loop thru interior nodes
        DO jj = start,end

!               - read interior nodes into arrays
          read(3,*,end=70) xd,yd,dep
          jjmax = jj
          if(jj.le.MaxPts) then
            dxray(jj) = xd
            dyray(jj) = yd
            Depth(jj) = dep
            !Exist(jj) = .true.
            Code(jj) = 0
          endif
        END DO
      ENDIF
!          - ( TotCoords .gt. end )
         goto 70

701      CONTINUE
        if (jjmax.ne.TotCoords) then
!             incorrect number of nodes read in/specified in file...
          cstr = 'NODE FILE HAS TOO FEW NODES - Read to end of file'
            call PigMessageOK ( cstr,'ReadNode' )

          PtsThisBnd(ii) = jjmax - start + 1
          write(cstr,'(a,i7,a,i4)') 'PtsThisBnd reduced to ',PtsThisBnd(ii),' for boundary', ii
            call PigMessageOK ( cstr,'ReadNode' )

          TotBndys = ii
          write(cstr,'(a,i7)') 'TotBndys reduced to ',TotBndys
            call PigMessageOK ( cstr,'ReadNode' )

          TotCoords = jjmax
          write(cstr,'(a,i7)') 'TotCoords reduced to ',TotCoords
            call PigMessageOK ( cstr,'ReadNode' )

        endif

! - 70 = end stmt for read stmts lines 191 & 206 (another internal loop exit)
 70      CONTINUE
      TotCoords = jjmax
      do ii=1,TotBndys
        write(cstr,'(a,i7,a,i4)') 'PtsThisBnd is ',PtsThisBnd(ii),' for boundary', ii
        call PigPutMessage(cstr)
      end do
      IF ( end .gt. MaxPts ) THEN
        cstr = 'TOO MANY DATA POINTS, DATAFILE TRUNCATED'
        call PigMessageOK ( cstr,'ReadNode' )
      ENDIF

!      ENDIF
      IF ( TotCoords .gt. MaxPts ) THEN
        cstr = 'INPUT HAS TOO MANY NODES'
        call PigMessageOK ( cstr,'ReadNode' )
        TotCoords = MaxPts
      ENDIF

!     - first condition is arbitrary lower bound here...
      if((TotCoords.lt.2000).or.(TotCoords.le.MaxPts/10 )) then
        outlineonly = .false.
      else
        outlineonly = .true.
      endif

      Quit = .false.
      itot = TotCoords
!      nrec = itot + 1
      nbtotr = 0

      ii = itot
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

990   continue
!     error reading file
      cstr =  'Error reading beginning of node file'
      call PigMessageOK ( cstr,'ReadNode' )
      Quit = .true.
      return

9901  continue
!     error reading TotCoords
      cstr =  'Error reading integer on line 1 of node file'
      call PigMessageOK ( cstr,'ReadNode' )
      Quit = .true.
      return

9902  continue
!     error reading TotBndys
      cstr = 'Error reading integer on line 2 of node file'
      call PigMessageOK ( cstr,'ReadNode' )
      Quit = .true.
      return

      END

!*----------------------------------------------------------------------*
!-----------------------------------------------------------------------*                                    *
!       This module contains routines for adding nodes from boundaries  *
!       and internal points.                                            *
!-----------------------------------------------------------------------*

      SUBROUTINE AddNodeFile( Quit)

! PURPOSE: To add together outer boundaries, internal boundaries,  
!          and internal points into a single file.
!   GIVEN: Initial data, if any;
! RETURNS: Added nodes in file
! EFFECTS: boundary and interior points from several node files are added 
!          together.
!----------------------------------------------------------------------*

      implicit none

! - PASSED VARIABLES
!        INTEGER nrec
        LOGICAL Quit

! - LOCAL VARIABLES
      integer readtype
      CHARACTER*256 nodfile
      CHARACTER*1 PigCursYesNo
      logical PigOpenFileCD
!------------------START ROUTINE---------------------------------------

!       - initialize defaults
      quit = .true.

!       - assign defaults
      nodfile = ' '

      IF (PigCursYesNo ('Replace OUTER boundary?').EQ.'Y') THEN
        if(.not.PigOpenFileCD(3,'Open NODE File', nodfile, &
     &          'Node Files (*.nod),*.nod;All Files (*.*),*.*;')) then
          close(unit=3, status='keep')
        ELSE
!             -  file specified
          Quit = .false.
          readtype = 1
          call ReadAddNodeFile ( readtype, nodfile, Quit )
        ENDIF
      endif


      IF (PigCursYesNo ('Add ISLAND boundary?').EQ.'Y') THEN
        if(.not.PigOpenFileCD(3,'Open NODE File', nodfile, &
     &          'Node Files (*.nod),*.nod;All Files (*.*),*.*;')) then
          close(unit=3, status='keep')
        ELSE
!             -  file specified
          Quit = .false.
          readtype = 2
          call ReadAddNodeFile ( readtype, nodfile, Quit )
        ENDIF
      endif


      IF (PigCursYesNo ('Add INTERIOR nodes?').EQ.'Y') THEN
        if(.not.PigOpenFileCD(3,'Open NODE File', nodfile, &
     &          'Node Files (*.nod),*.nod;All Files (*.*),*.*;')) then
          close(unit=3, status='keep')
        ELSE
!             -  file specified
          Quit = .false.
          readtype = 3
          call ReadAddNodeFile ( readtype, nodfile, Quit )
        ENDIF
      endif

      return
      end

!*----------------------------------------------------------------------*

      SUBROUTINE ReadAddNodeFile ( readtype, FName, Quit )

!  PURPOSE: To read NODE file data into memory arrays.
!    GIVEN: OpenNodeFile has specified an existing file, Fname, to open
!           and read.
!  RETURNS: dxray(),dyray() = arrays of coordinates of boundary and interior
!                             nodes with boundaries first,
!           depth() = array of depths at each node,
!           minx,miny = lower left NDC coordinate,
!           maxx,maxy = upper right NDC coordinate,
!           coinrng = range for coincident nodes.
!  EFFECTS: Data from NODE file is in memory arrays in Common NODES
!           in NODESTOR.INC.
!-----------------------------------------------------------------------*

      use MainArrays

      implicit none

! - "INCLUDES"
      include '../includes/defaults.inc'

! - COMMON BLOCKS

! -  Only boundaries are displayed when OUTLINEONLY is .TRUE.
      LOGICAL OUTLINEONLY
      COMMON /OUTLINE/ OUTLINEONLY

! - LOCAL VARIABLES
      INTEGER start, end, start1,start2
      integer i, readtype, ii, jj, bcode ! , nrec
      INTEGER TotCoords2, TotBndys2,  TotIntBndys2, PtsThisBnd2, TotIntpts2
      integer TotCoordsNew
      integer StartIntPts
      CHARACTER*256 FName, cstr*80, Firstline*80
      character*1 ans, PigCursYesNo
      LOGICAL Quit
      real x2, y2, dep
! -------------------START ROUTINE-----------------------------------

      start = 1
      end = 1
      outlineonly = .false.
 
      if(TotCoords.le.0) then
        dispnodes = .true.
        x0off = 0.
        y0off = 0.
        scaleX = 1.
        scaleY = 1.
      endif

      Quit = .TRUE.

      OPEN ( UNIT = 3, STATUS = 'OLD', FILE = FName )

      READ(3,'(a)', err=990) Firstline

      if(firstline(1:4).eq."#NOD") then  !new node format
        do
          READ(3,'(a)', err=990) Firstline
          if(firstline(1:1).ne."#") then    !comment lines
            read(firstline,*,err=990)x0off,y0off,scaleX,scaleY,igridtype
            exit
          endif
        enddo
!       - read # of nodes total ( TotCoords )
        READ(3,*,err=9901) TotCoords2
!       - read # of boundaries total ( TotBndys ) and number of internal lines
        READ(3,*,err=9902) TotBndys2, TotIntBndys2
      elseif(firstline(1:1).eq."#") then  !wrong format
        call PigMessageOK( 'Wrong format for node file.','ReadNode' )
        Quit = .true.
        Return
      else
        rewind(3)
!       - read # of nodes total ( TotCoords )
        READ(3,*,err=9901) TotCoords2
!       - read # of boundaries total ( TotBndys )
        READ(3,*,err=9902) TotBndys2
      endif

!       - loop thru boundaries
      DO ii = 1, TotBndys2
!       - read # nodes in each boundary ( ii )
        READ(3,*,END=701) PtsThisBnd2
!       - end = end of nodes on boundary ( ii )
        end = ( start - 1 ) + PtsThisBnd2
!       - loop thru node coordinates for boundary ( ii )
        if(readtype.eq.1) then !ii.eq.1.or.bndtest) then
          bcode = 1
        else
          bcode = 2
        endif

        IF ( TotCoords+PtsThisBnd2 .ge. MaxPts ) THEN
          cstr = 'TOO MANY NODES TO ADD - aborting'
          call PigMessageOK ( cstr,'addnode' )
          return
        ENDIF

        if(readtype.eq.1.or.readtype.eq.2) then  !(bndtest).or.(islon)) then
          TotCoordsNew = TotCoords
          DO jj = start, end
            read(3,*,end=701) x2,y2,dep
            call PigDrawModifySymbol(x2, y2)
            TotCoordsNew = TotCoordsNew + 1    
            if(jj.le.MaxPts) then
              dxray(TotCoordsNew) = x2
              dyray(TotCoordsNew) = y2
              Depth(TotCoordsNew) = dep
              !Exist(TotCoordsNew) = .true.
              code(TotCoordsNew) = bcode
            endif
          END DO

! - prompt
!         - Ask if they are to be kept
          if(readtype.eq.1.or.readtype.eq.2) then  !(bndtest).or.(islon)) then
            if(readtype.eq.1) then  !p1.and.bndtest) then
               cstr = 'Are new boundary nodes OK?:'
              ans = PigCursYesNo (cstr)
            elseif(readtype.eq.2) then  !p2.and.islon) then
               cstr = 'Are new island nodes OK?:'
              ans = PigCursYesNo (cstr)
            else
              ans(1:1) = 'Y'
            endif

            IF ( ans(1:1) .eq. 'Y' ) THEN
!           - save new nodes
!           - redraw nodes in boundary color
              start2 = TotCoords + 1
              DO jj = start2, TotCoordsNew
                x2 = dxray(jj)
                y2 = dyray(jj)
                call PigDrawBndSymbol(x2, y2)
              END DO
              if(readtype.eq.1) then  !bndtest) then
                TotCoords = PtsThisBnd2
                dxray(1:TotCoords) = dxray(start2:TotCoordsNew)
                dyray(1:TotCoords) = dyray(start2:TotCoordsNew)
                Depth(1:TotCoords) = Depth(start2:TotCoordsNew)
                !Exist(1:TotCoords) = .true.
                code(1:TotCoords)  = code(start2:TotCoordsNew)
                TotBndys = 1
                PtsThisBnd(TotBndys) =  PtsThisBnd2
                TotIntPts = 0
                exit
              elseif(readtype.eq.2) then  !bndtest) then
                start1 = TotCoords - TotIntPts + 1
                TotCoords = TotCoords - TotIntPts + PtsThisBnd2
                dxray(start1:TotCoords) = dxray(start2:TotCoordsNew)
                dyray(start1:TotCoords) = dyray(start2:TotCoordsNew)
                Depth(start1:TotCoords) = Depth(start2:TotCoordsNew)
                !Exist(start1:TotCoords) = .true.
                code(start1:TotCoords)  = code(start2:TotCoordsNew)
                TotBndys = TotBndys + 1
                PtsThisBnd(TotBndys) =  PtsThisBnd2
                TotIntPts = 0
              endif
            ELSE
!           - do not save new nodes, leave as is
!           - redraw nodes in background color
              start2 = TotCoords + 1
              DO i = start2, TotCoordsNew
                x2 = dxray(i)
                y2 = dyray(i)
                call PigEraseModifySymbol(x2, y2)
              END DO
            ENDIF
!           - ( ans = Y )
          endif
        else
          DO jj = start, end
            read(3,*,end=701)
          enddo
        endif
!         - start = beginning of next set of boundary nodes
        start = end + 1
      END DO

! - load existing internal nodes
      IF ( TotBndys2 .eq. 0 )  start = 1
      if(readtype.eq.3) then
!       - read interior points, if there are any
        IF ( TotCoords2 .gt. end ) THEN
          StartIntPts = TotCoords+1
          READ(3,*,END=701) TotIntPts2
          end = start - 1 + TotIntPts2
          IF ( end .gt. MaxPts ) THEN
            cstr = 'TOO MANY INTERNAL NODES TO ADD - aborting'
            call PigMessageOK ( cstr,'addnode' )
            close(3)
            return
          ENDIF

          do jj = start,end
!               - read interior nodes into arrays
            read(3,*,end=701) x2,y2,dep

            TotCoords = TotCoords + 1
            TotIntPts = TotIntPts + 1
            if(jj.le.MaxPts) then
              dxray(TotCoords) = x2
              dyray(TotCoords) = y2
              Depth(TotCoords) = dep
              !Exist(TotCoords) = .true.
              Code(TotCoords) = 0
              call PigDrawModifySymbol(x2, y2)
            endif
          END DO
! - prompt
!         - Ask if they are to be kept
          cstr = 'Are new nodes OK?:'
          ans = PigCursYesNo (cstr)
          call PigEraseMessage
          IF ( ans(1:1) .eq. 'Y' ) THEN
!           - save new nodes
!           - redraw nodes in interior color
            DO i = 1, StartIntPts, TotCoords
              x2 = dxray(i)
              y2 = dyray(i)
              call PigDrawIntSymbol(x2, y2)
            END DO
          ELSE
!           - do not save new nodes
!           - redraw nodes in background color
            DO i = StartIntPts, TotCoords
              x2 = dxray(i)
              y2 = dyray(i)
              call PigEraseModifySymbol(x2, y2)
            END DO
              TotCoords = StartIntPts - 1
              TotIntPts = TotIntPts - TotIntPts2
          ENDIF
!           - ( ans = Y )
          call PigEraseMessage
        endif
!          - ( TotCoords .gt. end )
      ENDIF
!            - (  inton  )

!     - first condition is arbitrary lower bound here...
      if(TotCoords.lt.2000) then
        outlineonly = .false.
      else
        outlineonly = .true.
      endif

      close(3)
      Quit = .false.
      itot = TotCoords
!      nrec = itot + 1
      nbtotr = 0
      return

701   CONTINUE
!             incorrect number of nodes read in/specified in file...
      cstr = 'Premature end of file'
      call PigMessageOK ( cstr, 'addnode' )
      return

990   continue
!     error reading file
      cstr =  'Error reading beginning of node file'
      call PigMessageOK ( cstr,'ReadNode' )
      Quit = .true.
      return
      
9901  continue
!     error reading TotCoords
      call PigMessageOK('Error reading integer on line 1 of node file','addnode')
      close(3)
      Quit = .true.
      return

9902  continue
!     error reading TotBndys
      call PigMessageOK('Error reading integer on line 1 of node file','addnode')
      close(3)
      Quit = .true.
      return

      END subroutine

!--------------------END ADDNODE ---------------------------------------*
! --------------------------------------------------------------------------*

      SUBROUTINE ReadXYZData (quit)
   
! Purpose : To read grid data into memory

      use MainArrays

      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/cntcfg.inc'

!     - PASSED VARIABLES
      LOGICAL Quit

      REAL    XMAX, YMAX, XMIN, YMIN

!     - LOCAL VARIABLES
      INTEGER i 
!        - counters
      INTEGER   istat
!        - the number of records that is to be in the data file
      INTEGER numrec

!------------------BEGIN-------------------------------

      QUIT = .FALSE.
!      exist = .FALSE.
      
! - read max number of nodes and elements in this grid
      read(3,*,IOSTAT=istat) numrec
      if(istat.lt.0) then
        call PigMessageOK('ERROR end of file at line 2','ReadXYZ' )
        Quit = .TRUE.
        return
      elseif(istat.gt.0) then
        call PigMessageOK('ERROR reading XYZ file at line 2','ReadXYZ' )
        Quit = .TRUE.
        return
      endif

      do i=1,numrec
        read(3,*,IOSTAT=istat) dxray(i),dyray(i),depth(i)
        if(istat.lt.0) then
          call PigMessageOK('ERROR premature end of file','ReadXYZ' )
          Quit = .TRUE.
          return
        elseif(istat.gt.0) then
          call PigMessageOK('ERROR reading XYZ file: Most likely a format error','ReadXYZ' )
          Quit = .TRUE.
          return
        endif
        !EXIST(i) = .TRUE.   
      enddo

      code = 0   
      itot = numrec
      TotTr = 0

      TotCoords = numrec
      TotBndys = 0
      TotIntBndys = 0
      PtsThisBnd = 0
      TotIntPts = TotCoords
      DispNodes = .true.

      if(int(ScaleY).eq.-999) then
        xlongmin = minval(dxray(1:itot))
        xlongmax = maxval(dxray(1:itot))
        xlongsum=ScaleX
      endif

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
      END

!-----------------------------------------------------------------------*

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

!*--------------------------------------------------------------------------*
!     This module contains the subroutines that write the grid information  *
!     to file based on the type of save the user has chosen. INTERIM save   *
!     which provides two versions with a generated file name or FINAL       *
!     that prompts the user for a final file name.                          *
!*--------------------------------------------------------------------------*

      SUBROUTINE SaveInterim( Quit )

! Purpose: Dispatch routine for interim file save.
! Givens : NREC - Number of points within the current grid
! Returns: None
! Effects: Default interim filename is toggled to the next interim
!          filename.

      use MainArrays

      implicit none

      INCLUDE '../includes/defaults.inc'

! Passed Parameters
      logical Quit

! Local variables.
      integer nunit,j,k,kk,nb
      integer nindex(0:itot)

      nunit = 9
      open( nunit, file=GridIName, status='UNKNOWN' )

! Toggle the Interim filename.
      if ( GridIName .eq. 'interim1.ngh' ) then
        LastInterim = 'interim1.ngh'
        call PigPutMessage('Saving interim file interim1.ngh')
        GridIName = 'interim2.ngh'
      else
        LastInterim = 'interim2.ngh'
        call PigPutMessage('Saving interim file interim2.ngh')
        GridIName = 'interim1.ngh'
      endif

! Write the file
      
!       sync arrays
!      do j=1,itot
!        if(code(j).lt.0) then
!          exist(j) = .false.
!        elseif(.not.exist(j)) then
!          code(j) = -9
!        endif
!      enddo      

      ! *** remove ghosts
      do j=1,itot
        if(code(j).ge.0) then
          kk=0
          do k=1,nbtotr
            nb = NL(k,j)
            if(nb.ne.0) then
              kk = kk + 1
              exit
            endif
          enddo
          if(kk.eq.0) then
            code(j) = -9
            cycle
          endif
        endif
      enddo

      call Write_ngh_file (nunit, nindex)
      close( nunit )
      call PigPutMessage('Done.')
      Quit = .false.
      
      END

!*--------------------------------------------------------------------------*

      SUBROUTINE SaveFinal( change, Quit )

! Purpose: Save final Grid and optionally the TRIANGLE and CRITERIA lists.
! Givens : data in MainArrays
! Returns: Quit - TRUE if user wishes to exit the editor
!          CHANGE - TRUE if a change in triangle criteria
! Effects: None

      use MainArrays

      implicit none

! Passed variables
      LOGICAL Quit, CHANGE

! Local variables
      integer nindex(0:itot)
      CHARACTER*256 ans
      integer nunit,j,k,kk,nb,istat,fnlen
      character PigCursYesNo*1, ans1*1
!      LOGICAL ListTri,ListCri
      logical PigOpenFileCD, ResOK

      if(igridtype.lt.0) then
        call PigMessageOK('Cannot save file with polar transform','WriteGrid')
        return
      endif
      
      !change = .true.
      
      ans = ' '
      nunit = 9
      ResOK = PigOpenFileCD(nunit,'Save Grid File', ans,                &
     &    'Neighbour File (*.[ng][gcr]*),*.ngh;All Files (*.*),*.*;')

      if(.not.resOK) then
        quit = .true.
        return
      endif
      
      ! *** remove ghosts
      do j=1,itot
        if(code(j).ge.0) then
          kk=0
          do k=1,nbtotr
            nb = NL(k,j)
            if(nb.ne.0) then
              kk = kk + 1
              exit
            endif
          enddo
          if(kk.eq.0) then
            code(j) = -9
            cycle
          endif
        endif
      enddo

      call PigPutMessage('Writing new grid file..')

      istat = index( ans, char(0) )
      if(istat.gt.0) then ! c string
        fnlen = istat -1
      else ! f string
        fnlen = len_trim( ans )
      endif

      write(*,*) fnlen-2,fnlen
      write(*,*) ans(fnlen-2:fnlen)
!      istat = index( ans, '.nc' )
      if(ans(fnlen-2:fnlen).eq.'.nc') then !netCDF file
        write(*,*) fnlen-2,fnlen
        write(*,*) ans(fnlen-2:fnlen)
        if(change) then
          call RemoveNotExist(itot,code,nbtot,nl)
          call Element_Lister(CHANGE, .FALSE. , &
             itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
             x0off,y0off,scaleX,scaleY,igridtype)
          change = .false.
        endif
        close(nunit,status='keep')
        call WritenetCDFData(ans,Quit)
      elseif(ans(fnlen-3:fnlen).eq.'.grd') then !grd file, nodes+elements
        write(*,*) fnlen-3,fnlen
        write(*,*) ans(fnlen-3:fnlen)
        if(change) then
          call RemoveNotExist(itot,code,nbtot,nl)
          call Element_Lister(CHANGE, .FALSE. , &
             itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
             x0off,y0off,scaleX,scaleY,igridtype)
          change = .false.
        endif
        call writeGRDfile (nunit, nindex)
        close( nunit )
        call PigPutMessage('Done')
      else ! ngh file
        write(*,*) fnlen-3,fnlen
        write(*,*) ans(fnlen-3:fnlen)
        call write_ngh_file (nunit, nindex)
        close( nunit )
        call PigPutMessage('Done')

!        if(.not.quit) then

          ans1 = PigCursYesNo('Save Triangle List File?')

          if(ans1.eq.'Y'.or.ans1(1:1).eq.'Y') then
            ans = ' '
            if(change) then
              call RemoveNotExist(itot,code,nbtot,nl)
              call Element_Lister(CHANGE, .FALSE. , &
                 itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                 x0off,y0off,scaleX,scaleY,igridtype)
              change = .false.
            endif
            nunit = 11
            ResOK = PigOpenFileCD(nunit,'Save Element File', ans,           &
     &        'Element File(*.el*), *.el*; All Files(*.*),*.*;' )
            if(ResOK) then
!              ListCri = .false.
!              ListTri = .true.
              call Tr_Dump(nunit,nindex)
              close(nunit)
            endif
          endif

          ans1 = PigCursYesNo('Save Open Boundary File?')

          if(ans1.eq.'Y'.or.ans1(1:1).eq.'Y') then
            ans = ' '
            nunit = 9
            ResOK = PigOpenFileCD(nunit,'Save Open Boundary File', ans,           &
     &        'Boundary File(*.bpt), *.bpt; All Files(*.*),*.*;' )
            if(ResOK) then
              call WRBndFILE(nunit,nindex)
              close(nunit)
            endif
          endif
!        endif
        quit = .false.
      endif


      END

!*--------------------------------------------------------------------------*

      SUBROUTINE WRBndFILE (LUN,nindex)

! PURPOSE : TO WRITE CURRENT open boundary DATA ONTO FILE
! GIVEN   : NREC - THE NUMBER OF RECORDS IN THE FILE
!           LUN - THE FORTRAN FILE UNIT NUMBER TO USE
!   *NOTE*  NREC has been found to be "NUM OF RECORDS + 1" .. Adjustment 
!           has been made.

      use MainArrays

      implicit none

! *** PASSED VARIABLES ***
      integer LUN
      integer nindex(0:itot)
  
! *** LOCAL VARIABLES ***
      integer I,ii
      real :: zero=0.

      write(lun,*) 'VARIABLES="n" "x" "y" "z" "code"'

! look for open boundary conditions
      DO I = 1,itot 
        ii = nindex(i)
        IF(ii.gt.0) then  
          if(code(ii).eq.5.or.code(ii).eq.6) then
            write (LUN,fmt=111,err=9999) ii,dxray(ii),dyray(ii),DEPTH(ii),CODE(ii)
          endif
        endif
      enddo  
   
! look for flow boundary conditions
      DO I = 1,itot 
        ii = nindex(i)
        IF(ii.gt.0) then  
          if(code(ii).eq.9) then
            write (LUN,fmt=111,err=9999) ii,dxray(ii),dyray(ii),DEPTH(ii),CODE(ii)
          endif
        endif
      enddo  

      write(lun,*) 'VARIABLES="n_ele" "code" "Q1" "Q2"'

! look for element discharge conditions
      ii = 99
      DO I = 1,TotTr 
        if(Tcode(i).eq.9) then
            write (LUN,'(2I8,2F5.1)',err=9999) i, ii, zero, zero
        endif
      enddo  
            
      endfile(LUN)
      return

9999  continue
      call PigMessageOK(' *** ERROR in writing Boundary condition file ****', ' ')

      return

111   FORMAT(I8,3(1X,1PE14.7),1X,I4)

      end   

!*--------------------------------------------------------------------------*

      subroutine writeGRDfile(iunit,nindex)

      use MainArrays
      
      implicit none
            
! *** passed variables
      integer iunit

! *** local variables
      integer j,count
      integer nindex(0:itot)
!      LOGICAL ListTri,ListCri

! *** set up index array for deleted nodes

      count = 0
      do j=1,itot
        if(code(j).lt.0) then
          nindex(j) = 0
        else
          count = count+1
          nindex(j) = count
        endif
      enddo

      write(*,*) ' Writing grd file....'

      write(iunit,'(a)') "#GRD"
      if(igridtype.eq.1) then
        write(iunit, 102 ) x0off, y0off, scaleX, scaleY, igridtype, UTMzone(1:3)
      else
        write(iunit, 101 ) x0off, y0off, scaleX, scaleY, igridtype
      endif
      write(iunit,*) count, TotTr
      
      do j=1,itot
        if(nindex(j).gt.0) then
          write(iunit,112) dxray(j),dyray(j),depth(j),code(j)
        endif
      enddo
     
!      ListCri = .false.
!      ListTri = .true.
      call Tr_Dump(iunit,nindex)

101   FORMAT(4(1X,1PE18.10),1X,I3)
102   FORMAT(4(1X,1PE18.10),1X,I3,1X,a3)
112   FORMAT(3(1X,1PE21.13),1X,I3)

      end   
      
 !*--------------------------------------------------------------------------*

      subroutine write_ngh_file(iunit,nindex)

      use MainArrays
      
      implicit none
            
! *** passed variables
      integer iunit

! *** local variables
      integer j,jj,k,kk,count,nb,newnbtot
      integer tmpnbrs(nbtot),nindex(0:itot)
      
      ! *** shrink nbrs array
      newnbtot = 0
      do j=1,itot
        if(code(j).ge.0) then
          kk = 0
          do k=1,nbtotr
            nb = NL(k,j)
            if(nb.ne.0) then
              kk = kk + 1
              NL(kk,j) = nb
            endif
          enddo
          if(kk.lt.nbtot) NL(kk+1:nbtot,j) = 0
          newnbtot = max(kk,newnbtot)
        endif
      enddo
      
! *** set up index array for deleted nodes

      count = 0
      do j=1,itot
        if(code(j).lt.0) then
          nindex(j) = 0
        else
          count = count+1
          nindex(j) = count
        endif
      enddo

      write(*,*) ' Writing ngh file....'

      write(iunit,'(a)') "#NGH"
      if(igridtype.eq.1) then
        write(iunit, 102 ) x0off, y0off, scaleX, scaleY, igridtype, UTMzone(1:3)
      else
        write(iunit, 101 ) x0off, y0off, scaleX, scaleY, igridtype
      endif
      write(iunit,*) count
      write(iunit,*) newnbtot
      
      jj = 0
      do j=1,itot
        if(nindex(j).gt.0) then
          tmpnbrs(1:nbtot)=NL(1:nbtot,j)
          kk = 0
          do k=1,nbtotr !add roll down
            nb = tmpnbrs(k)
            if(nb.gt.0) then
              kk = kk + 1
              tmpnbrs(kk) = nindex(nb)
            endif
          enddo
          tmpnbrs(kk+1:nbtot) = 0
          jj = jj + 1
          write(iunit,111) jj,dxray(j),dyray(j),code(j),depth(j),(tmpnbrs(k),k=1,newnbtot)
        endif
      enddo
     
101   FORMAT(4(1X,1PE18.10),1X,I3)
102   FORMAT(4(1X,1PE18.10),1X,I3,1X,a3)
111   FORMAT(I7,2(1X,1PE21.13),1X,I3,1X,1PE14.7,25(1x,I7))

      return
      end subroutine
      
!*----------------------------------------------------------------------*
!       Routines for reading and writing node data to file.                        *
!       ROUTINES: SaveNFinal, SaveNInterim, SaveNPoly, ShiftNodes,      *
!                 GetShift, OpenNodeFile                                *
!*----------------------------------------------------------------------*

      SUBROUTINE SaveNInterim ( success )

! PURPOSE: To write current data onto file, using file unit 9 as   
!           output file unit.  
!   GIVEN: Common NODES = data to write.
! RETURNS: success = .TRUE. if file saved , else .FALSE.
!          In Common IOVALUES ; IOvalue
!                               0 = OK.
!                               1 = user "quit" at filename.
!                               others = F77L3 specific.
! EFFECTS: Node data in dxray(), dyray(), depth() are witten to named file,
!          in NODE format. Default interim filename is toggled to the next
!          interim filename.
!*----------------------------------------------------------------------*

      implicit none 

! - PARAMETERS (constants)
      integer funit
      PARAMETER ( funit = 29 )

! - PASSED VARIABLES
      LOGICAL success

! - COMMON AREA 
!   - IOVALUES is for I/O status storage
      integer IOvalue
      COMMON /IOVALUES/ IOvalue

! - "INCLUDES"
      include '../includes/defaults.inc'

! - LOCAL VARIABLES
      CHARACTER*80 cstr
      LOGICAL Exists

!----------------START ROUTINE------------------------------------------

      Exists = .FALSE.
      INQUIRE( File = NodeIName, Exist = Exists )
      IF ( Exists ) THEN
        OPEN ( funit, file = NodeIName, status = 'UNKNOWN' )
        CLOSE ( unit = funit, status = 'DELETE' )
      ENDIF
!         - ( Exists )
      OPEN ( funit, file = NodeIName, status = 'NEW' )

!       - toggle the Interim filename.
      call PigEraseMessage
      IF ( NodeIName .eq. 'interim1.nod' ) THEN
        LastInterim = 'interim1.nod'
        cstr = 'Saving interim file interim1.nod'
        call PigPutMessage ( cstr )
        NodeIName = 'interim2.nod'
      ELSE
!         - interim file is Interim 2
        LastInterim = 'interim2.nod'
        cstr = 'Saving interim file interim2.nod'
        call PigPutMessage ( cstr )
        NodeIName = 'interim1.nod'
      ENDIF
!         - ( NodeIName = interim1.nod )

!       - write the file
      call write_node_file(funit)
      success = .true.

      end subroutine 

!*----------------------------------------------------------------------*

      SUBROUTINE SaveNFinal ( success )

! PURPOSE: To write current data onto file, using file unit 12 as   
!          output file unit. Any existing interim files saved will be deleted
!          if success = TRUE.
!   GIVEN: Common NODES = data to write.
! RETURNS: success = .TRUE. if file saved , else .FALSE.
!          In Common IOVALUES ; IOvalue
!                               0 = OK.
!                               1 = user "quit" at filename.
!                               others = F77L3 specific.
! EFFECTS: Node data in dxray(), dyray(), depth() are witten to named file,
!          in NODE format. User must choose a new file name, over-writing
!          an existing file is not allowed.
!*----------------------------------------------------------------------*

      use MainArrays, only : igridtype

      implicit none
 
      integer, parameter :: funit=29

! - PASSED VARIABLES
      LOGICAL success

! - COMMON AREA 
!   - IOVALUES is for I/O status storage
      integer IOvalue
      COMMON /IOVALUES/ IOvalue

! - LOCAL VARIABLES
      CHARACTER*256 Flename
      LOGICAL resOK
      logical PigOpenFileCD

!----------------START ROUTINE------------------------------------------


      if(igridtype.lt.0) then
        call PigMessageOK('Cannot save file with polar transform','WriteNode')
        return
      endif

      !      success = .FALSE.
      Flename = ' '

      ResOK = PigOpenFileCD(funit,'Save Node File', Flename,&
               'Node Files (*.nod),*.nod;All Files (*.*),*.*;')
      if(.not.resOK) return

!        fnlen = len_trim(Flename)

!        named = .TRUE.
!         - start writing...
        call PigPutMessage('Writing File...[NODE] format.')
      call write_node_file(funit)
      success = .true.

      end subroutine 

!*----------------------------------------------------------------------*

      subroutine write_node_file(funit)

      use MainArrays
      
      implicit none
            
! *** passed variables
      integer funit

! *** local variables
      integer i,j,k,tmptot
      integer endi, starti, IOvalue
      logical success
      character(128) cstring
      
!         - start writing...
      endi = 1
      starti = 1

      write(funit,'(a4)') '#NOD'
      if(igridtype.eq.1) then
        write(funit,102 ) x0off,y0off,scaleX,scaleY,igridtype,UTMzone(1:3)
      else
        write(funit,101) x0off,y0off,scaleX,scaleY,igridtype
      endif

!         - write the total number of nodes
      WRITE( funit, *, err = 99, iostat=IOvalue) TotCoords
!         - write total boundaries, check first for null boundaries
      tmptot = 0
      DO i = 1, TotBndys
        IF ( PtsThisBnd(i) .ne. 0 )  tmptot = tmptot + 1
      END DO
!           - ( i = 1, TotBndys )
      TotBndys = tmptot
      WRITE( funit,*, err=99, iostat=IOvalue) TotBndys,TotIntBndys
      DO i = 1, TotBndys
!           - write number of nodes this boundary
        IF ( PtsThisBnd(i) .ne. 0 ) THEN
!             - not a null boundary
          WRITE ( funit,*,err = 99,iostat=IOvalue) PtsThisBnd(i)
          endi = (starti - 1) + PtsThisBnd(i)
          DO j = starti, endi
!               - write nodes for this boundary
        WRITE ( funit, fmt = 910, err = 99, iostat = IOvalue ) dxray(j), dyray(j), Depth(j)
          END DO
!               - ( j = starti, endi )
          starti = endi + 1
        ENDIF
!             - ( PtsThisBnd(i) .ne. 0 )
      END DO
!           - ( i = 1, TotBndys )
!         - write number of interior nodes
      WRITE( funit,*, err = 99, iostat=IOvalue) TotIntPts
      endi = ( starti - 1 ) + TotIntPts
      DO k = starti, endi
!           - write interior nodes
        WRITE ( funit, fmt = 910, err = 99, iostat = IOvalue ) dxray(k), dyray(k), Depth(k)
      END DO
      CLOSE ( funit, status = 'KEEP', err = 99, iostat = IOvalue )
      IOvalue = 0
      success = .TRUE.
!        ENDIF
!         - ( Not Quitting )
    GOTO 999

!       - error trap
99      continue
    write(cstring,'(a,i3)') 'IOstat = ', MOD ( IOvalue, 256 )
    call PigMessageOK(cstring,'savenode')
    success = .False.
    return

999     CONTINUE
    call PigEraseMessage

101   FORMAT(4(1X,1PE18.10),1X,I3)
102   FORMAT(4(1X,1PE18.10),1X,I3,1X,a3)
910   format ( 2(1X, F21.13), 1X, 1PE14.7 )

      end subroutine
      
!---------------------------------------------------------------------------*

      SUBROUTINE TR_DUMP(nunit,nindex)

      use MainArrays
      
      implicit none

!     PASSED VARIABLES
      integer nunit, nindex(0:itot)

!     LOCAL VARIABLES
      character*1 PigCursYesNo, ans
!      integer izero

      integer I,J  !,K,L
 !     LOGICAL LOOP

      ans = PigCursYesNo('Include TRIANGLES ONLY?')

      call PigPutMessage('Writing triangle list')

201   FORMAT(4(1X,I7),1X,I3)
!      izero = 0
      nindex(0) = 0
      if(ans.eq.'Y') then
        DO I = 1, TotTr
          if(ListTr(4,I).eq.0) then
            WRITE (nunit,201) (nindex(ListTr(J,I)),J=1,4),TCode(I)
          endif
        enddo
      else
        DO I = 1, TotTr
          WRITE (nunit,201) (nindex(ListTr(J,I)),J=1,4),TCode(I)
        enddo
      endif
      call PigPutMessage('Done writing triangle list')

      END

!---------------------------------------------------------------------------*
!-------------------------END ----------------------------------------*

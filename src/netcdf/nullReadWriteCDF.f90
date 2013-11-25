!*********************************************************************  
 
      subroutine ReadnetCDFData(fname,Quit)
      
      implicit none
      
! *** passed varaibles
      character(len=*) :: fname
      logical quit
      
      write(*,*) 'called ReadnetCDFData(nunit,Quit)'//fname
      call PigMessageOK( 'netCDF not enabled. Recomplie program.','ReadGrid' )
      Quit = .true.
     
      end subroutine

!*********************************************************************  
 
      subroutine WritenetCDFData(fname,Quit)
      
      implicit none
      
! *** passed varaibles
      character(len=*) :: fname
      logical quit
      
      write(*,*) 'called WritenetCDFData(nunit,Quit)'//fname
      call PigMessageOK( 'netCDF not enabled. Recomplie program.','WriteGrid' )
      Quit = .true.
           
      end subroutine

!*********************************************************************  

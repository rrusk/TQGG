!*********************************************************************  
 
      subroutine ReadnetCDFData(fname,Quit)
      
      implicit none

      INCLUDE '../includes/defaults.inc'
            
! *** passed varaibles
      character(len=*) :: fname
      logical quit

      Quit = .TRUE.
      GridRName =  'NONE'
      fname = 'NONE'
      call PigMessageOK('Recompile with netCDF option','ReadnetCDF')

      end subroutine

!*********************************************************************  
 
      subroutine WritenetCDFData(fname,Quit)
      
      implicit none
      
! *** passed varaibles
      character(len=*) :: fname
      logical quit
      
      Quit = .TRUE.
      call PigMessageOK('Recompile with netCDF option','WritenetCDF')
      fname = 'NONE'
      end subroutine

!************************************************************************

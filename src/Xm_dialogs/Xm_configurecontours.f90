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

!-----------------------------------------------------------------------*
!                                                                       *
!       Configuration routines for depth contours                       *
!                                                                       *
!-----------------------------------------------------------------------*

      SUBROUTINE ConfigCntrs_init()

! Purpose: Configuration routine for plotting of contours with DrwLine &
!          DrwFill options.
! Givens : Initial or user set values in Common in CNTCFG.INC are already set.
! Returns: Values in Common in CNTCFG.INC may be altered by user.
! Effects: Values in Common in CNTCFG.INC will affect display of contours
!          and type of data displayed in DrwConFill & DrwCon.
!------------------------------------------------------------------------*

      use MainArrays
      
      implicit none

! - "INCLUDES"
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

! - LOCAL VARIABLES
!      integer :: i, j
      real rval
      logical success
      character*1 ans
      character*80 retstring

      logical FlagN
      logical FlagG
      logical FlagD
      logical FlagC
      common /MenuDrawFlags/ FlagN,FlagG,FlagC,FlagD

      !------------ BEGIN --------

      if(flagC) then
        call PigMessageYesNo ('DISABLE contours?',ans)
        if(ans(1:1).eq.'Y') then
          flagC = .false.
          return
        endif
      endif

      call PigMessageYesNo ('Use default contour configuration?',ans)
      if(ans(1:1).eq.'Y') then
        SMaxVal(1) = maxval(depth(1:itot))         
        SMinVal(1) = minval(depth(1:itot))
        NumCntValues(1) = 10
        FlagC = .true.
      else
        call PigPrompt( 'Enter upper limit:',retstring)
        call PigReadReal(retstring,rval,success)
        if(success) then
          SMaxVal(1) = rval                   
        else
          do while (.not.success)
            call PigPrompt( 'Error reading number- try again:',retstring)
            call PigReadReal(retstring,rval,success)
          enddo
          SMaxVal(1) = rval
        endif

        call PigPrompt( 'Enter lower limit:',retstring)
        call PigReadReal(retstring,rval,success)
        if(success) then
          SMinVal(1) = rval                   
        else
          do while (.not.success)
            call PigPrompt( 'Error reading number- try again:',retstring)
            call PigReadReal(retstring,rval,success)
          enddo
          SMinVal(1) = rval
        endif
            
        call PigPrompt( 'Enter number of levels:',retstring)
        call PigReadReal(retstring,rval,success)
        if(success) then
          NumCntValues(1) = nint(rval)                   
        else
          do while (.not.success)
            call PigPrompt( 'Error reading integer- try again:',retstring)
            call PigReadReal(retstring,rval,success)
          enddo
          NumCntValues(1) = nint(rval)
        endif
       
        FlagC = .true.      
      endif

      ScalePhase(1) = 'F'

      call DisplayRange()

      return       
      END

!-----------------------------------------------------------------------*

      SUBROUTINE DisplayRange()

! Purpose: Draws range values & colors for configuration panel (S/R ConfiCntrs)
!          as stored in CNTCFG.INC. Also handles all re-calculation of range
!          values.
!------------------------------------------------------------------------*

      implicit none

! - "INCLUDES"
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

! - LOCAL VARIABLES
      REAL rngunit
      INTEGER i !, row, colp, colb, clr, hit, lenp, lenb, endcnt
!     - storage for values that wont change in this routine
      INTEGER numv, dtyp
      REAL dmax, dmin

!------------ BEGIN --------

!     - values that wont change in this routine
      dtyp = DataType
      numv = NumCntValues(dtyp)
      dmax = SMaxVal(dtyp) 
      dmin = SMinVal(dtyp)

!       - re-calculate range values for current data type
!       - determine value of 1 unit as max-min divided by # of values
      if(numv.gt.1) then
        rngunit = (dmax - dmin) / (numv-1)
      else
        rngunit = (dmax - dmin)
      endif
!       -- init to zeroes
      DO i = 1, 16
         CntValues(dtyp,i) = 0.0
      ENDDO
!       - last value gets max
      CntValues(dtyp,numv) = dmax
!       - count down
      DO i = numv-1, 1, -1
!          - subtract a unit
         CntValues(dtyp,i) = CntValues(dtyp,i+1) - rngunit
      ENDDO

      END

!---------------------------------------------------------------------------*

      SUBROUTINE InitContVect

! Purpose: Initialize values stored in CNTCFG.INC.
! Givens : None
! Returns: Values in CNTCFG.INC.
! Effects: Attributes are set.
!---------------------------------------------------------------------------*

      implicit none
      include '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

! - LOCAL VARIABLES
      INTEGER i, j
        integer ClrTable(16)

!-------BEGIN------------
!       - set up color table by name
        ClrTable(1) = dkred
        ClrTable(2) = red
        ClrTable(3) = orange
        ClrTable(4) = yellow
        ClrTable(5) = green
        ClrTable(6) = dkgreen
        ClrTable(7) = cyan
        ClrTable(8) = dkcyan
        ClrTable(9) = blue
        ClrTable(10) = dkblue
        ClrTable(11) = black
        ClrTable(12) = dkgray
        ClrTable(13) = ltgray
        ClrTable(14) = white
        ClrTable(15) = violet
        ClrTable(16) = vioblue

!       - cntcfg values
!       -- default data type is depth
      DataType = 1
      DO i = 1, NumDataTypes
!          - each data type has 10 values as default
         NumCntValues(i) = 10
!          - labels turned off
         LabelsOn(i) = .FALSE.
         IF ( (i .eq. 3) .OR. (i .eq. 5) .OR. (i .eq. 7) ) THEN
!             - default is phase for Hphase, Uphase, Vphase
            ScalePhase(i) = 'P'
         ELSE
!             - default is scale for Hamp, Uamp, Vamp, Vrms
            ScalePhase(i) = 'S'
         ENDIF
!          - set a default color scale & init all values to zero, min & max
!          -- for values, when data read in, will determine actual defaults
         DO j = 1, MaxCntValues
           CntValues(i,j) = 0.0
           IF ( j .lt. 16 ) THEN
             CntColors(i,j) = ClrTable(j)
           ELSE
             CntColors(i,j) = ClrTable(16)
           ENDIF
         ENDDO
      ENDDO

!       - default Min & Max data values, these will change when data read in,
!       - scale defaults
      SMinVal(1) = 0.0
      SMaxVal(1) = 0.0

      RETURN
      END

!----------------------- END  -------------------------------*

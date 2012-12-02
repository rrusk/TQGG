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

!-----------------------------------------------------------------------*
!                       CONFIGV.FOR                                     *
!                                                                       *
!       Configuration routines for various operations. Routines:        *
!               ConfigVect (n/i), ConfigCntr, DisplayRange.             *
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

! - "INCLUDES"
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

! - COMMON BLOCKS
!     - variables to match cntcfg.inc Common area so values can be set
!     -- but aborted and cntcfg values will remain as they were
!     -- modified SEP92 - JDM
      INTEGER     LDataType
      CHARACTER*6 LTypeName
      CHARACTER*1 LScalePhase( NumDataTypes )
      LOGICAL     LLabelsOn( NumDataTypes )
      INTEGER     LNumCntValues( NumDataTypes )
      INTEGER     LCntColors( NumDataTypes, MaxCntValues )
      REAL        LCntValues( NumDataTypes, MaxCntValues )
      REAL        LSMinVal(NumDataTypes), LSMaxVal(NumDataTypes)
      REAL        LPMinVal(NumDataTypes), LPMaxVal(NumDataTypes)
!      COMMON /LCNTCFG/ LLabelsOn, LDataType, LCntColors,LNumCntValues, LCntValues,&
!                  LSMinVal, LSMaxVal,LPMinVal, LPMaxVal, LTypeName, LScalePhase
        COMMON /LCNTCFG/ LCntValues,LSMinVal, LSMaxVal, LPMinVal, LPMaxVal, &       
                LCntColors,LNumCntValues,LDataType, LLabelsOn, LTypeName, LScalePhase

! - LOCAL VARIABLES
      integer :: i, j
      logical FlagN
      logical FlagG
      logical FlagD
      logical FlagC
      common /MenuDrawFlags/ FlagN,FlagG,FlagC,FlagD

      !------------ BEGIN --------

!     - set local configuration values as stored in Common CNTCFG
      DO i = 1, NumDataTypes
        LLabelsOn(i) = LabelsOn(i)
        ScalePhase(i) = 'F'
        LScalePhase(i) = 'F'
        LNumCntValues(i) = NumCntValues(i)        
!       - scale
        LSMinVal(i) = SMinVal(i)
        LSMaxVal(i) = SMaxVal(i)
!       - phase
        LPMinVal(i) = PMinVal(i)
        LPMaxVal(i) = PMaxVal(i)
        DO j = 1, MaxCntValues
          LCntColors(i,j) = CntColors(i,j)
          LCntValues(i,j) = CntValues(i,j)
        ENDDO
      ENDDO
!     - Data Type & name (name not array since will hold only current type's
!     -- name & TypeName is unchangeable by user)
      LDataType = DataType
      LTypeName = TypeName(DataType)

! action here

       call DisplayRange()

!      IF ( .NOT. Cancelled ) THEN
        DataType = LDataType
        LabelsOn(LDataType) = LLabelsOn(LDataType)
        ScalePhase(LDataType) = LScalePhase(LDataType)
        NumCntValues(LDataType) = LNumCntValues(LDataType)
        SMinVal(LDataType) = LSMinVal(LDataType)
        SMaxVal(LDataType) = LSMaxVal(LDataType)
        PMinVal(LDataType) = LPMinVal(LDataType)
        PMaxVal(LDataType) = LPMaxVal(LDataType)
        DO i = 1, MaxCntValues
!         - color values for contours
          CntColors(LDataType,i) = LCntColors(LDataType,i)
!         - contour range values
          CntValues(LDataType,i) = LCntValues(LDataType,i)
        END DO
!      ENDIF

       FlagC = .not.FlagC

       return
       
       entry ConfigDepCntrs_ehandler ()


!     - loop getting user hits untill ACCEPT selected

!     - if not cancelled, set configured values
        DataType = LDataType
        LabelsOn(LDataType) = LLabelsOn(LDataType)
        ScalePhase(LDataType) = LScalePhase(LDataType)
        NumCntValues(LDataType) = LNumCntValues(LDataType)
        SMinVal(LDataType) = LSMinVal(LDataType)
        SMaxVal(LDataType) = LSMaxVal(LDataType)
        PMinVal(LDataType) = LPMinVal(LDataType)
        PMaxVal(LDataType) = LPMaxVal(LDataType)
        DO i = 1, MaxCntValues
!         - color values for contours
          CntColors(LDataType,i) = LCntColors(LDataType,i)
!         - contour range values
          CntValues(LDataType,i) = LCntValues(LDataType,i)
        END DO

      RETURN
      END

!-----------------------------------------------------------------------*
      SUBROUTINE DisplayRange()

! Purpose: Draws range values & colors for configuration panel (S/R ConfiCntrs)
!          as stored in CNTCFG.INC. Also handles all re-calculation of range
!          values.
! Givens : NumHits - number of hits active prior to first call to displayrange
!          Values in Common Lcntcfg
!          Locations and hitnumbers are hardcoded here and must match what
!          ConfigCntrs is expecting.
!          ReData in PLOTFILE sets up colors & range when new file read in.
! Returns: LCntValues() = recalculated range values in Common Lcntcfg, local
!                         configuration storage.
! Effects: Panel hit areas for Range values & colors are displayed. Blanking
!          of inactive hit areas is done. Range re-calculated as follows:
!            Max & Min data value for a given type is stored in cntcfg.inc
!             as SMinVal() & SMaxVal() or PMinVal() & PMaxVal()
!            Range is difference between min & max for the given data type
!            Depth range is divided by # of range values to display (unit);
!            Range # LNumCntValues = SMaxVal(LDataType) or PMaxVal(LDataType)
!                                    depending on LDataType;
!            Range # LNumCntValues-1 = Range # LNumCntValues - unit;
!            Range # LNumCntValues-2 = Range # LNumCntValues-1 - unit, etc..
!          Does Not change # of active hit areas, or # of units in a data
!          types range (LNumCntValues).
!------------------------------------------------------------------------*

! - dummy args.
!      INTEGER NumHits

! - "INCLUDES"
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

! - COMMON BLOCKS
!     - variables to match cntcfg.inc Common area so values can be set
!     -- but aborted and cntcfg values will remain as they were
      INTEGER     LDataType
      CHARACTER*6 LTypeName
      CHARACTER*1 LScalePhase( NumDataTypes )
      LOGICAL     LLabelsOn( NumDataTypes )
      INTEGER     LNumCntValues( NumDataTypes )
      INTEGER     LCntColors( NumDataTypes, MaxCntValues )
      REAL        LCntValues( NumDataTypes, MaxCntValues )
      REAL        LSMinVal(NumDataTypes), LSMaxVal(NumDataTypes)
      REAL        LPMinVal(NumDataTypes), LPMaxVal(NumDataTypes)
!    COMMON /LCNTCFG/ LLabelsOn, LDataType, LCntColors,LNumCntValues, LCntValues,&
!                  LSMinVal, LSMaxVal,LPMinVal, LPMaxVal, LTypeName, LScalePhase
      COMMON /LCNTCFG/ LCntValues,LSMinVal, LSMaxVal, LPMinVal, LPMaxVal, &       
              LCntColors,LNumCntValues,LDataType, LLabelsOn, LTypeName, LScalePhase
! - LOCAL VARIABLES
      REAL rngunit
      INTEGER i, row, colp, colb, clr, hit, lenp, lenb, endcnt
      CHARACTER*13 numtmp
!     - storage for values that wont change in this routine
      INTEGER numv, dtyp
      REAL dmax, dmin

!------------ BEGIN --------

!      call SetNumHits(NumHits)

113   FORMAT(F12.3)

!     - values that wont change in this routine
      dtyp = LDataType
      numv = LNumCntValues(dtyp)
!      IF ( LScalePhase(LdataType) .eq. 'S' ) THEN
      dmax = LSMaxVal(dtyp) 
      dmin = LSMinVal(dtyp)
!      ELSE
!      dmax = LPMaxVal(dtyp) 
!      dmin = LPMinVal(dtyp)
!      ENDIF
!       - re-calculate range values for current data type
!       - determine value of 1 unit as max-min divided by # of values
        if(numv.gt.1) then
        rngunit = (dmax - dmin) / (numv-1)
        else
        rngunit = (dmax - dmin)
        endif
!       -- init to zeroes
      DO i = 1, 16
         LCntValues(dtyp,i) = 0.0
      ENDDO
!       - last value gets max
      LCntValues(dtyp,numv) = dmax
!       - count down
      DO i = numv-1, 1, -1
!          - subtract a unit
         LCntValues(dtyp,i) = LCntValues(dtyp,i+1) - rngunit
      ENDDO

!       - blank any that are inactive
!       - constants
!       - length of hit box
      lenp = 4
!       - length of value display
      lenb = 12
!       - column hit boxes start in
      colp = 18
!       - column values start in
      colb = 2
!       - blank color
      clr = backgr
!       - counters
!       -- hit will vary from 39 to 11 by -2
!       -- row will vary from 21 to 7 by -1
      hit = 39
      row = 21
      endcnt = numv + 1
      DO i = 16, endcnt, -1
! agd no longer needed, replaced by next line...
!          call PanelHitBox( colp, row, hit, lenp, clr)
!        call BlankArea( colp, row, lenp )
!        call BlankArea( colb, row, lenb )
        hit = hit - 2
        row = row - 1
      ENDDO

!       - draw all those that are active
!       - constants
      colp = 2
      colb = 18
      lenp = 12
      lenb = 4
!       - counters
!       -- row will vary from 6 to 21 by 1
!       -- hit will vary from 9 to 39 by 2 ( or 1 twice per loop )
      row = 6
      hit = 8
      endcnt = numv
      DO i = 1, endcnt, 1
         WRITE ( numtmp, FMT = 113 ) LCntValues(dtyp,i)
!         call PanelHit( colp, row, hit, numtmp, lenp )
         hit = hit + 1
!         call PanelHitBox( colb, row, hit, lenb, 
!     +                       LCntColors(dtyp,i) )
         hit = hit + 1
         row = row + 1
      ENDDO
      END

!---------------------------------------------------------------------------*

      SUBROUTINE InitContVect

! Purpose: Initialize attributes stored in PLOTCFG.INC.
!          Also initialize values stored in CNTCFG.INC (MAY92).
!          Also initialize values stored in VECCFG.INC (JAN93).
! Givens : None
! Returns: Values in PLOTCFG.INC, CNTCFG.INC, & VECCFG.
! Effects: Attributes are set.
! Written: DEC91 - JDM.
! Modified: MAY92 - JDM, to set CNTCFG.INC values.
! Modified: JAN93 - JDM, to set VECCFG.INC values.
!---------------------------------------------------------------------------*


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
!       - plotcfg values
      NodeClr = red
      GridClr = cyan
      MeshClr = white
      TriangClr = cyan 
      ContClr = yellow
      BoundClr = green
      NodeType = 3
      VectClr = cyan
!        NodeNumON = .false.
!        ElemNumON = .false.

!       - cntcfg values
!       -- default data type is depth
      DataType = 1
      DO i = 1, NumDataTypes
!          - each data type has 8 values as default
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
!       - set the data types, these should not change
      TypeName(1) = 'Depth'
!       - default Min & Max data values, these will change when data read in,
!       - scale defaults
      SMinVal(1) = 0.0
      SMaxVal(1) = 0.0
!       -- default for phase types is 0 -> 360
      PMinVal(1) = 0.0
      PMaxVal(1) = 360.0

!       - cntcfg values

      RETURN
      END

!----------------------- END CONFIGV.FOR -------------------------------*

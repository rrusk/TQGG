!---------------------------------------------------------------
!                CNTCFG.INC
!     Contains values for configuring contours.
!	DataType = numerical representation of data type of contours:
!		   1 = Depth, 2 = Hamp, 3 = Hphase, 4 = Uamp,
!		   5 = Uphase, 6 = Vamp, 7 = Vphase, 8 = Vrms
!       TypeName() = array of text names describing data types:
!                  1) 'Depth', 2) 'Hamp', 3) 'Hphase', 4) 'Uamp',
!                  5) 'Uphase', 6) 'Vamp', 7) 'Vphase', 8) 'Vrms'
! LabelsOn() = array of label logicals, TRUE/FALSE
!	ScalePhase() = array of indicators for each Data Type:   
!		     'S'caler, or 'P'hase.		
!	CntColors(,) = 2 dimensional array of colors for each data type
!	CntValues(,) = 2 dimensional array of contour values for each data type
!	NumCntValues() = number of values in CntValues & CntColors
!                      that are currently valid, 1 -> 16 (2nd dimension)
!       SMaxVal() = Maximum data value for a given data type in scale mode
!       SMinVal() = Minimum data value for a given data type in scale mode
!       PMaxVal() = Maximum data value for a given data type in phase mode
!       PMinVal() = Minimum data value for a given data type in phase mode

!  6 Arrays of 8 elements (NumDataTypes) each, representing the 8 possible data
!  types and their individual configurations:
! INDEX   TypeName    ScalePhase   LabelsOn   NumCntValues  SMinVal  SMaxVal
!   1      'Depth'    'S'/'P'        T/F       1..16        <real>  <real>
!   .          .      'S'/'P'        T/F       1..16        <real>  <real>
!   .          .      'S'/'P'        T/F       1..16        <real>  <real>
!   8      'Vrms'     'S'/'P'        T/F       1..16        <real>  <real>

!       CntValues():                 CntColors():
!           (16 values)                 (16 values)
!          1,1 . . 1,16                1,1 . . 1,16
!  (8 types) .                 (8 types) .
!            .                           .
!          8,1 . . 8,16                8,1 . . 8,16

! Min & Max Values for data type 'x':
!         min value is Cntvalues(x,1)
!         max value is Cntvalues(x,NumCntvalues(x))
!---------------------------------------------------------------*

      INTEGER NumDataTypes
      PARAMETER ( NumDataTypes = 1 )

      INTEGER MaxCntValues
      PARAMETER ( MaxCntValues = 16 )

      INTEGER     DataType
      CHARACTER*6 TypeName( NumDataTypes )
      CHARACTER*1 ScalePhase( NumDataTypes )
      LOGICAL     LabelsOn( NumDataTypes )
      INTEGER     NumCntValues( NumDataTypes )
      INTEGER     CntColors( NumDataTypes, MaxCntValues )
      REAL        CntValues( NumDataTypes, MaxCntValues )
      REAL        SMaxVal(NumDataTypes), SMinVal(NumDataTypes)
      REAL        PMaxVal(NumDataTypes), PMinVal(NumDataTypes)

      COMMON /CNTCFG/ CntValues,SMaxVal,SMinVal,PMaxVal,PMinVal,                &
     &                CntColors, NumCntValues,DataType,LabelsOn,                &
     &                ScalePhase, TypeName      

!-------------------- END CNTCFG.INC ---------------------------*

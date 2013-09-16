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

C*----------------------------------------------------------------------*
C                       CONFIGV.FOR                                     *
C                                                                       *
C       Configuration routines for various operations. Routines:        *
C               ConfigVect (n/i), ConfigCntr, DisplayRange.             *
C                                                                       *
C*----------------------------------------------------------------------*
      SUBROUTINE ConfigCntrs_init()
C
C Purpose: Configuration routine for plotting of contours with DrwLine &
C          DrwFill options.
C Givens : Initial or user set values in Common in CNTCFG.INC are already set.
C Returns: Values in Common in CNTCFG.INC may be altered by user.
C Effects: Values in Common in CNTCFG.INC will affect display of contours
C          and type of data displayed in DrwConFill & DrwCon.
C------------------------------------------------------------------------*

      use MainArrays

C - "INCLUDES"
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

C - COMMON BLOCKS
C     - variables to match cntcfg.inc Common area so values can be set
C     -- but aborted and cntcfg values will remain as they were
C     -- modified SEP92 - JDM
	INTEGER     LDataType
	CHARACTER*6 LTypeName
	CHARACTER*1 LScalePhase( NumDataTypes )
	LOGICAL     LLabelsOn( NumDataTypes )
	INTEGER     LNumCntValues( NumDataTypes )
	INTEGER     LCntColors( NumDataTypes, MaxCntValues )
        REAL        LCntValues( NumDataTypes, MaxCntValues )
        REAL        LSMinVal(NumDataTypes), LSMaxVal(NumDataTypes)
        REAL        LPMinVal(NumDataTypes), LPMaxVal(NumDataTypes)
        COMMON /LCNTCFG/ LCntValues,LSMinVal, LSMaxVal,
     &                 LPMinVal, LPMaxVal,        
     +                 LCntColors,LNumCntValues,LDataType,
     +                 LLabelsOn, LTypeName, LScalePhase

C - LOCAL VARIABLES
      INTEGER hitnum
      integer :: i, j, anslngth
      REAL realval
      LOGICAL Accepted, Cancelled, success
      CHARACTER*13 numtmp
      CHARACTER*80 cstr, ans
      logical FlagN
      logical FlagG
      logical FlagD
      logical FlagC
      common /MenuDrawFlags/ FlagN,FlagG,FlagC,FlagD
c      integer numhits
C------------ BEGIN --------

113   FORMAT(F12.3)
        SMaxVal(1) = maxval(depth(1:itot))         
        SMinVal(1) = minval(depth(1:itot))
        NumCntValues(1) = 10
        FlagC = .true.

C     - set local configuration values as stored in Common CNTCFG
      DO i = 1, NumDataTypes
	  LLabelsOn(i) = LabelsOn(i)
	  ScalePhase(i) = 'F'
	  LScalePhase(i) = 'F'
	  LNumCntValues(i) = NumCntValues(i)        
C       - scale
	  LSMinVal(i) = SMinVal(i)
	  LSMaxVal(i) = SMaxVal(i)
C       - phase
	  LPMinVal(i) = PMinVal(i)
	  LPMaxVal(i) = PMaxVal(i)
	  DO j = 1, MaxCntValues
	    LCntColors(i,j) = CntColors(i,j)
	    LCntValues(i,j) = CntValues(i,j)
	  ENDDO
      ENDDO
C     - Data Type & name (name not array since will hold only current type's
C     -- name & TypeName is unchangeable by user)
      LDataType = DataType
      LTypeName = TypeName(DataType)

C     - Right window definition T9
      call InitRHPanel

C     - display all labels
      call PigSetTextColour( LabelColor )

      call PanelText( 2, 1, 'CONTOURS CONFIGURATION', 22 )

      call PanelText( 2, 3,  '     Draw:', 10 )
      call PanelText( 2, 4,  '     Type:', 10 )
      call PanelText( 2, 5,  '   Labels:', 10 )
      call PanelText( 2, 22,  'Colors:', 7 )

C     - create all selectable options
      call PigSetTextColour( HitColor )

C     - Create Data Type option (#1)
      FlagC = .true.
      IF ( FlagC) then
      	call PanelHit( 14, 3, 1, 'ON ', 3 )
	else
	  call PanelHit( 14, 3, 1, 'OFF', 3 )
	endif

C     - Create Scale/Phase option (#7)
      IF (LScalePhase(LDataType) .eq. 'L') THEN
	  call PanelHit( 14, 4, 7, 'Lines ', 6 )
      ELSE
	  LScalePhase(LDataType) = 'F'
	  call PanelHit( 14, 4, 7, 'Fill  ', 6 )
      ENDIF

C     - Create Labels option (#2)
      IF ( LLabelsOn(LDataType) ) THEN
	 call PanelHit( 14, 5, 2, 'ON ', 3 )
      ELSE
	 call PanelHit( 14, 5, 2, 'OFF', 3 )
      ENDIF

C     - Create CANCEL option (#3)
!      call PanelHit( 10, 23, 3, 'CLOSE', 6 )

C     - Create ACCEPT option (#4)
      call PanelHit( 10, 23, 4, 'CLOSE', 5 )

C     - Create Remove option (#5)
      call PanelHit( 11, 22, 5, 'Remove', 6 )

C     - Create Add option (#6)
      call PanelHit( 19, 22, 6, ' Add ', 5 )

C     - make active value & color areas selectable (#8 - #39)
C     -- #8 - #39 are in pairs (8/9, 10/11, 12/13..) each pair defining
C     -- a color & a range
c       call GetNumHits(NumHits)
c       call DisplayRange()
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
C         - color values for contours
	    CntColors(LDataType,i) = LCntColors(LDataType,i)
C         - contour range values
	    CntValues(LDataType,i) = LCntValues(LDataType,i)
	  END DO
!      ENDIF

       return
       
       entry ConfigDepCntrs_ehandler (hitnum)


C     - loop getting user hits untill ACCEPT selected
      Accepted = .FALSE.
      Cancelled = .FALSE.

C         - valid selection
	  IF ( hitnum .eq. 4 ) THEN
C           - Accept option
	    Accepted = .TRUE.
	    call ClearRHPanel
	  ELSEIF ( hitnum .eq. 1 ) THEN
C           - Draw
          FlagC = .not.FlagC
          IF ( FlagC) then
      	    call PanelHit( 14, 3, 1, 'ON ', 3 )
	    else
	      call PanelHit( 14, 3, 1, 'OFF', 3 )
	    endif
	  ELSEIF ( hitnum .eq. 2 ) THEN
C           - Labels
	    LLabelsOn(LDataType) = .NOT. LLabelsOn(LDataType)
	    IF ( LLabelsOn(LDataType) ) THEN
	      call PanelHit( 14, 5, 2, 'ON ', 3 )
	    ELSE
	      call PanelHit( 14, 5, 2, 'OFF', 3 )
	    ENDIF
	  ELSEIF ( hitnum .eq. 3 ) THEN
C           - CANCEL
	    Cancelled = .TRUE.
	  ELSEIF ( hitnum .eq. 5 ) THEN
C           - Remove color, if more than one
	    IF ( LNumCntValues(LDataType) .gt. 1 ) THEN
	       LNumCntValues(LDataType) = LNumCntValues(LDataType) - 1
C              - re-calculate range values & display
               call DisplayRange()
c               call SetNumHits( 7 + (2 * LNumCntValues(LDataType)) )
            ELSE
               call PigMessageOK(
     +            'One is Minimum # Colors To Display', ' ')
!              call PigUWait( 2.0 )
!              call PigEraseMessage
            ENDIF
          ELSEIF ( hitnum .eq. 6 ) THEN
C           - Add color
	    IF ( LNumCntValues(LDataType) .lt. 16 ) THEN
	       LNumCntValues(LDataType) = LNumCntValues(LDataType) + 1
C              - re-calculate range values & display
               call DisplayRange()
c               call SetNumHits( 7 + (2 * LNumCntValues(LDataType)) )
            ELSE
               call PigMessageOK( 'Maximum # Colors Already Used',' ')
!              call PigUWait( 2.0 )
!              call PigEraseMessage
            ENDIF
          ELSEIF ( hitnum .eq. 7 ) THEN
C           - Scale/Phase option
	    IF (LScalePhase(LDataType) .eq. 'L') THEN
	      LScalePhase(LDataType) = 'F'
	    ELSE
	      LScalePhase(LDataType) = 'L'
	    ENDIF
	    IF (LScalePhase(LDataType) .eq. 'L') THEN
	      call PanelHit( 14, 4, 7, 'Lines ', 6 )
	    ELSE
	      call PanelHit( 14, 4, 7, 'Fill  ', 6 )
	    ENDIF
	    call DisplayRange()

C         ********** 
C         - following (#8 -> #39) are range value & color options
C         -- 8 & 9 are range & color option #1..
C         -- 38 & 39 are range & color option #16
C 1
	  ELSEIF ( hitnum .eq. 8 ) THEN
C           - range value 1
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,1) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,1)
	      call PanelHit( 2, 6, 8, numtmp, 12 )
C             - value 1 always represents minimum value, set S or P -MinVal()
C             -- no other selections should set min val
!	      IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		LSMinVal(LDataType) = realval
!	      ELSE
!		LPMinVal(LDataType) = realval
!	      ENDIF
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 1 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 9 ) THEN
	    IF ( LCntColors(LDataType,1) .lt. NumColors ) THEN
	      LCntColors(LDataType,1) = LCntColors(LDataType,1) + 1
	    ELSE
	      LCntColors(LDataType,1) = 1
	    ENDIF
	    call PanelHitBox( 18, 6,  9, 4, LCntColors(LDataType,1) )
C 2
	  ELSEIF ( hitnum .eq. 10 ) THEN
C           - range value 2
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,2) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,2)
	      call PanelHit( 2, 7, 10, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 2 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 11 ) THEN
	    IF ( LCntColors(LDataType,2) .lt. NumColors ) THEN
	      LCntColors(LDataType,2) = LCntColors(LDataType,2) + 1
	    ELSE
	      LCntColors(LDataType,2) = 1
	    ENDIF
	    call PanelHitBox( 18, 7, 11, 4, LCntColors(LDataType,2) )
C 3
	  ELSEIF ( hitnum .eq. 12 ) THEN
C           - range value 3
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,3) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,3)
	      call PanelHit( 2, 8, 12, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 3 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 13 ) THEN
	    IF ( LCntColors(LDataType,3) .lt. NumColors ) THEN
	      LCntColors(LDataType,3) = LCntColors(LDataType,3) + 1
	    ELSE
	      LCntColors(LDataType,3) = 1
	    ENDIF
	    call PanelHitBox( 18, 8, 13, 4, LCntColors(LDataType,3) )
C 4
	  ELSEIF ( hitnum .eq. 14 ) THEN
C           - range value 4
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,4) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,4)
	      call PanelHit( 2, 9, 14, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 4 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 15 ) THEN
	    IF ( LCntColors(LDataType,4) .lt. NumColors ) THEN
	      LCntColors(LDataType,4) = LCntColors(LDataType,4) + 1
	    ELSE
	      LCntColors(LDataType,4) = 1
	    ENDIF
	    call PanelHitBox( 18, 9, 15, 4, LCntColors(LDataType,4) )
C 5
	  ELSEIF ( hitnum .eq. 16 ) THEN
C           - range value 5
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,5) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,5)
	      call PanelHit( 2, 10, 16, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 5 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 17 ) THEN
	    IF ( LCntColors(LDataType,5) .lt. NumColors ) THEN
	      LCntColors(LDataType,5) = LCntColors(LDataType,5) + 1
	    ELSE
	      LCntColors(LDataType,5) = 1
	    ENDIF
	    call PanelHitBox( 18, 10, 17, 4, LCntColors(LDataType,5) )
C 6
	  ELSEIF ( hitnum .eq. 18 ) THEN
C           - range value 6
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,6) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,6)
	      call PanelHit( 2, 11, 18, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 6 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 19 ) THEN
	    IF ( LCntColors(LDataType,6) .lt. NumColors ) THEN
	      LCntColors(LDataType,6) = LCntColors(LDataType,6) + 1
	    ELSE
	      LCntColors(LDataType,6) = 1
	    ENDIF
	    call PanelHitBox( 18, 11, 19, 4, LCntColors(LDataType,6) )
C 7
	  ELSEIF ( hitnum .eq. 20 ) THEN
C           - range value 7
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,7) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,7)
	      call PanelHit( 2, 12, 20, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 7 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 21 ) THEN
	    IF ( LCntColors(LDataType,7) .lt. NumColors ) THEN
	      LCntColors(LDataType,7) = LCntColors(LDataType,7) + 1
	    ELSE
	      LCntColors(LDataType,7) = 1
	    ENDIF
	    call PanelHitBox( 18, 12, 21, 4, LCntColors(LDataType,7) )
C 8
	  ELSEIF ( hitnum .eq. 22 ) THEN
C           - range value 8
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,8) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,8)
	      call PanelHit( 2, 13, 22, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 8 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 23 ) THEN
	    IF ( LCntColors(LDataType,8) .lt. NumColors ) THEN
	      LCntColors(LDataType,8) = LCntColors(LDataType,8) + 1
	    ELSE
	      LCntColors(LDataType,8) = 1
	    ENDIF
	    call PanelHitBox( 18, 13, 23, 4, LCntColors(LDataType,8) )
C 9
	  ELSEIF ( hitnum .eq. 24 ) THEN
C           - range value 9
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,9) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,9)
	      call PanelHit( 2, 14, 24, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 9 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 25 ) THEN
	    IF ( LCntColors(LDataType,9) .lt. NumColors ) THEN
	      LCntColors(LDataType,9) = LCntColors(LDataType,9) + 1
	    ELSE
	      LCntColors(LDataType,9) = 1
	    ENDIF
	    call PanelHitBox( 18, 14, 25, 4, LCntColors(LDataType,9) )
C 10
	  ELSEIF ( hitnum .eq. 26 ) THEN
C           - range value 10
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,10) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,10)
	      call PanelHit( 2, 15, 26, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 10 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 27 ) THEN
	    IF ( LCntColors(LDataType,10) .lt. NumColors ) THEN
	      LCntColors(LDataType,10) = LCntColors(LDataType,10) + 1
	    ELSE
	      LCntColors(LDataType,10) = 1
	    ENDIF
	    call PanelHitBox( 18, 15, 27, 4, LCntColors(LDataType,10))
C 11
	  ELSEIF ( hitnum .eq. 28 ) THEN
C           - range value 11
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,11) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,11)
	      call PanelHit( 2, 16, 28, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 11 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 29 ) THEN
	    IF ( LCntColors(LDataType,11) .lt. NumColors ) THEN
	      LCntColors(LDataType,11) = LCntColors(LDataType,11) + 1
	    ELSE
	      LCntColors(LDataType,11) = 1
	    ENDIF
	    call PanelHitBox( 18, 16, 29, 4, LCntColors(LDataType,11))
C 12
	  ELSEIF ( hitnum .eq. 30 ) THEN
C           - range value 12
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,12) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,12)
	      call PanelHit( 2, 17, 30, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 12 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 31 ) THEN
	    IF ( LCntColors(LDataType,12) .lt. NumColors ) THEN
	      LCntColors(LDataType,12) = LCntColors(LDataType,12) + 1
	    ELSE
	      LCntColors(LDataType,12) = 1
	    ENDIF
	    call PanelHitBox( 18, 17, 31, 4, LCntColors(LDataType,12))
C 13
	  ELSEIF ( hitnum .eq. 32 ) THEN
C           - range value 13
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,13) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,13)
	      call PanelHit( 2, 18, 32, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 13 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 33 ) THEN
	    IF ( LCntColors(LDataType,13) .lt. NumColors ) THEN
	      LCntColors(LDataType,13) = LCntColors(LDataType,13) + 1
	    ELSE
	      LCntColors(LDataType,13) = 1
	    ENDIF
	    call PanelHitBox( 18, 18, 33, 4, LCntColors(LDataType,13))
C 14
	  ELSEIF ( hitnum .eq. 34 ) THEN
C           - range value 14
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,14) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,14)
	      call PanelHit( 2, 19, 34, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 14 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 35 ) THEN
	    IF ( LCntColors(LDataType,14) .lt. NumColors ) THEN
	      LCntColors(LDataType,14) = LCntColors(LDataType,14) + 1
	    ELSE
	      LCntColors(LDataType,14) = 1
	    ENDIF
	    call PanelHitBox( 18, 19, 35, 4, LCntColors(LDataType,14))
C 15
	  ELSEIF ( hitnum .eq. 36 ) THEN
C           - range value 15
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,15) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,15)
	      call PanelHit( 2, 20, 36, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 15 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 37 ) THEN
	    IF ( LCntColors(LDataType,15) .lt. NumColors ) THEN
	      LCntColors(LDataType,15) = LCntColors(LDataType,15) + 1
	    ELSE
	      LCntColors(LDataType,15) = 1
	    ENDIF
	    call PanelHitBox( 18, 20, 37, 4, LCntColors(LDataType,15))
C 16
	  ELSEIF ( hitnum .eq. 38 ) THEN
C           - range value 16
	    success = .FALSE.
	    cstr = 'Enter new upper limit, <RTN> for no change:'
	    DO WHILE ( .NOT. success )
	      call PigPrompt( cstr, ans )
	      call PigReadReal ( ans, realval, success )
	    ENDDO
	    call PigEraseMessage
	    anslngth = LEN_TRIM( ans )
	    IF ( anslngth .gt. 0 ) THEN
	      LCntValues(LDataType,16) = realval
	      WRITE ( numtmp, FMT = 113 ) LCntValues(LDataType,16)
	      call PanelHit( 2, 21, 38, numtmp, 12 )
C             - set max if this value is upper value
	      IF ( LNumCntValues(LDataType) .eq. 16 ) THEN
!		IF ( LScalePhase(LDataType) .eq. 'S' ) THEN
		  LSMaxVal(LDataType) = realval
!		ELSE
!		  LPMaxVal(LDataType) = realval
!		ENDIF
	      ENDIF
	    ENDIF
	  ELSEIF ( hitnum .eq. 39 ) THEN
	    IF ( LCntColors(LDataType,16) .lt. NumColors ) THEN
	      LCntColors(LDataType,16) = LCntColors(LDataType,16) + 1
	    ELSE
	      LCntColors(LDataType,16) = 1
	    ENDIF
	    call PanelHitBox( 18, 21, 39, 4, LCntColors(LDataType,16))
	  ENDIF
C           - ( hitnum = 4 )
!      END DO
C       - ( NOT Accepted and NOT Cancelled )

C     - if not cancelled, set configured values
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
C         - color values for contours
	    CntColors(LDataType,i) = LCntColors(LDataType,i)
C         - contour range values
	    CntValues(LDataType,i) = LCntValues(LDataType,i)
	  END DO
!      ENDIF

      RETURN
      END

C*----------------------------------------------------------------------*
      SUBROUTINE DisplayRange()
C
C Purpose: Draws range values & colors for configuration panel (S/R ConfiCntrs)
C          as stored in CNTCFG.INC. Also handles all re-calculation of range
C          values.
C Givens : NumHits - number of hits active prior to first call to displayrange
C          Values in Common Lcntcfg
C          Locations and hitnumbers are hardcoded here and must match what
C          ConfigCntrs is expecting.
C          ReData in PLOTFILE sets up colors & range when new file read in.
C Returns: LCntValues() = recalculated range values in Common Lcntcfg, local
C                         configuration storage.
C Effects: Panel hit areas for Range values & colors are displayed. Blanking
C          of inactive hit areas is done. Range re-calculated as follows:
C            Max & Min data value for a given type is stored in cntcfg.inc
C             as SMinVal() & SMaxVal() or PMinVal() & PMaxVal()
C            Range is difference between min & max for the given data type
C            Depth range is divided by # of range values to display (unit);
C            Range # LNumCntValues = SMaxVal(LDataType) or PMaxVal(LDataType)
C                                    depending on LDataType;
C            Range # LNumCntValues-1 = Range # LNumCntValues - unit;
C            Range # LNumCntValues-2 = Range # LNumCntValues-1 - unit, etc..
C          Does Not change # of active hit areas, or # of units in a data
C          types range (LNumCntValues).
C------------------------------------------------------------------------*
c
c - dummy args.
c      INTEGER NumHits

C - "INCLUDES"
      INCLUDE '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'

C - COMMON BLOCKS
C     - variables to match cntcfg.inc Common area so values can be set
C     -- but aborted and cntcfg values will remain as they were
	INTEGER     LDataType
	CHARACTER*6 LTypeName
	CHARACTER*1 LScalePhase( NumDataTypes )
	LOGICAL     LLabelsOn( NumDataTypes )
	INTEGER     LNumCntValues( NumDataTypes )
	INTEGER     LCntColors( NumDataTypes, MaxCntValues )
        REAL        LCntValues( NumDataTypes, MaxCntValues )
        REAL        LSMinVal(NumDataTypes), LSMaxVal(NumDataTypes)
        REAL        LPMinVal(NumDataTypes), LPMaxVal(NumDataTypes)
        COMMON /LCNTCFG/ LCntValues,LSMinVal, LSMaxVal,
     &                 LPMinVal, LPMaxVal,        
     +                 LCntColors,LNumCntValues,LDataType,
     +                 LLabelsOn, LTypeName, LScalePhase
C - LOCAL VARIABLES
      REAL rngunit
      INTEGER i, row, colp, colb, clr, hit, lenp, lenb, endcnt
      CHARACTER*13 numtmp
C     - storage for values that wont change in this routine
      INTEGER numv, dtyp
      REAL dmax, dmin

C------------ BEGIN --------

c      call SetNumHits(NumHits)

113   FORMAT(F12.3)

C     - values that wont change in this routine
      dtyp = LDataType
      numv = LNumCntValues(dtyp)
!      IF ( LScalePhase(LdataType) .eq. 'S' ) THEN
	dmax = LSMaxVal(dtyp) 
	dmin = LSMinVal(dtyp)
!      ELSE
!	dmax = LPMaxVal(dtyp) 
!	dmin = LPMinVal(dtyp)
!      ENDIF
C       - re-calculate range values for current data type
C       - determine value of 1 unit as max-min divided by # of values
        if(numv.gt.1) then
	  rngunit = (dmax - dmin) / (numv-1)
        else
	  rngunit = (dmax - dmin)
        endif
C       -- init to zeroes
	DO i = 1, 16
	   LCntValues(dtyp,i) = 0.0
	ENDDO
C       - last value gets max
	LCntValues(dtyp,numv) = dmax
C       - count down
	DO i = numv-1, 1, -1
C          - subtract a unit
	   LCntValues(dtyp,i) = LCntValues(dtyp,i+1) - rngunit
	ENDDO

C       - blank any that are inactive
C       - constants
C       - length of hit box
	lenp = 4
C       - length of value display
	lenb = 12
C       - column hit boxes start in
	colp = 18
C       - column values start in
	colb = 2
C       - blank color
	clr = backgr
C       - counters
C       -- hit will vary from 39 to 11 by -2
C       -- row will vary from 21 to 7 by -1
	hit = 39
	row = 21
	endcnt = numv + 1
	DO i = 16, endcnt, -1
c agd no longer needed, replaced by next line...
c          call PanelHitBox( colp, row, hit, lenp, clr)
	  call BlankArea( colp, row, lenp )
	  call BlankArea( colb, row, lenb )
	  hit = hit - 2
	  row = row - 1
	ENDDO

C       - draw all those that are active
C       - constants
	colp = 2
	colb = 18
	lenp = 12
	lenb = 4
C       - counters
C       -- row will vary from 6 to 21 by 1
C       -- hit will vary from 9 to 39 by 2 ( or 1 twice per loop )
	row = 6
	hit = 8
	endcnt = numv
	DO i = 1, endcnt, 1
	   WRITE ( numtmp, FMT = 113 ) LCntValues(dtyp,i)
	   call PanelHit( colp, row, hit, numtmp, lenp )
	   hit = hit + 1
	   call PanelHitBox( colb, row, hit, lenb, 
     +                       LCntColors(dtyp,i) )
	   hit = hit + 1
	   row = row + 1
	ENDDO
	END

C*--------------------------------------------------------------------------*
      SUBROUTINE InitContVect
C
C Purpose: Initialize attributes stored in PLOTCFG.INC.
C          Also initialize values stored in CNTCFG.INC (MAY92).
C          Also initialize values stored in VECCFG.INC (JAN93).
C Givens : None
C Returns: Values in PLOTCFG.INC, CNTCFG.INC, & VECCFG.
C Effects: Attributes are set.
C Written: DEC91 - JDM.
C Modified: MAY92 - JDM, to set CNTCFG.INC values.
C Modified: JAN93 - JDM, to set VECCFG.INC values.
C*--------------------------------------------------------------------------*


      include '../includes/graf.def'
      INCLUDE '../includes/cntcfg.inc'
!	INCLUDE '../includes/veccfg.inc'
c	INCLUDE 'plotcfg.inc'

C - LOCAL VARIABLES
	INTEGER i, j
        integer ClrTable(16)
cgks        data ClrTable/10,2,13,5,3,11,6,14,4,12,0,8,9,1,7,15/

C-------BEGIN------------
c       - set up color table by name
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
C       - plotcfg values
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

C       - cntcfg values
C       -- default data type is depth
	DataType = 1
	DO i = 1, NumDataTypes
C          - each data type has 8 values as default
	   NumCntValues(i) = 10
C          - labels turned off
	   LabelsOn(i) = .FALSE.
	   IF ( (i .eq. 3) .OR. (i .eq. 5) .OR. (i .eq. 7) ) THEN
C             - default is phase for Hphase, Uphase, Vphase
	      ScalePhase(i) = 'P'
	   ELSE
C             - default is scale for Hamp, Uamp, Vamp, Vrms
	      ScalePhase(i) = 'S'
	   ENDIF
C          - set a default color scale & init all values to zero, min & max
C          -- for values, when data read in, will determine actual defaults
	   DO j = 1, MaxCntValues
	     CntValues(i,j) = 0.0
c	     IF ( j .lt. NumColors ) THEN
c	       CntColors(i,j) = foregr + j
	     IF ( j .lt. 16 ) THEN
	       CntColors(i,j) = ClrTable(j)
	     ELSE
	       CntColors(i,j) = ClrTable(16)
	     ENDIF
	   ENDDO
	ENDDO
C       - set the data types, these should not change
	TypeName(1) = 'Depth'
C       - default Min & Max data values, these will change when data read in,
C       - scale defaults
	SMinVal = 0.0
	SMaxVal = 0.0
C       -- default for phase types is 0 -> 360
	PMinVal(1) = 0.0
	PMaxVal(1) = 360.0

C       - cntcfg values

	RETURN
	END

C*---------------------- END CONFIGV.FOR -------------------------------*

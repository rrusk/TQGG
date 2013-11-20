
!***********************************************************************

!      Subroutine WPigOpenGraphicPkg(x1, x2, y1, y2)
!        real x1
!        real x2
!        real y1
!        real y2
!      End 

!      Subroutine WPigCloseGraphicPkg
!      end

!***********************************************************************

      Subroutine WPigPutMessage(Text)
        character*(*) Text
        integer  ilen
        
        ilen = LEN_TRIM(Text)
        write(*,*) Text(1:ilen)    
      end

      Subroutine WPigEraseMessage
      end

!      Subroutine WPigMessageOK(Text,title)
!        character*(*)   Text,title
!        integer(4)      ilen1  

!        ilen = LEN_TRIM(Text)
!        write(*,*) Text(1:ilen)//'OK?'
!        read(*,*)    
!      end

!      subroutine WPigCursYesNo (reply, messg)
!        CHARACTER(*) messg
!        character*1 reply
!        integer ilen

!        ilen = LEN_TRIM(messg)
!        write(*,*) messg(1:ilen)//'Yes/No(Y/N)?'
!        read(*,*)  reply
!        if(reply(1:1) .eq. 'y') reply = 'Y'
!      End

!      subroutine WPigCursYesNoCancel (reply, messg)
!        CHARACTER*(*) messg
!        character*1 reply
!        integer ilen

!        ilen = LEN_TRIM(messg)
!        write(*,*) messg(1:ilen)//'Yes/No/Cancel(Y/N/C)?'
!        read(*,*)  reply
!      end

!***********************************************************************

!    subroutine WPigSetWorldCoordinates0 (x1,x2,y1,y2)
!        real x1
!        real x2
!        real y1
!        real y2
!    End 

!    subroutine WPigGetWorldCoordinates0 (x1,x2,y1,y2)
!        real x1 
!        real x2 
!        real y1 
!        real y2 
!    End

    subroutine WPigSetWindowNum (WinNum)
        integer*2 WinNum,dummy
        dummy = WinNum
    End

!    subroutine WPigEraseMain ()
!    End

!***********************************************************************


!    subroutine WPigSetBackgrColour (BackgrColour)
!        integer BackgrColour
!    End

!    subroutine WPigSetForegrColour (ForegrColour)
!        integer ForegrColour
!    End

!***********************************************************************

!    subroutine WPigSetLineColour (LineColour)
!        integer LineColour
!    End

!    subroutine WPigSetLineWidth (LineWidth)
!        real LineWidth
!    End

!    subroutine WPigDrawPolyLine (n, x, y)
!        integer n
!        real x  (n)
!        real y  (n)
!    End

!***********************************************************************

!    subroutine WPigSetSymbolColour (SymbolColour)
!        integer SymbolColour
!    End

!    subroutine WPigSetSymbolNumber (SymbolNumber)
!        integer SymbolNumber
!    End

    subroutine WPigSetSymbolSize (SymbolSize)
        real SymbolSize,dummy
        dummy = SymbolSize
    End

!    subroutine WPigDrawSymbols  (np, x, y)
!        integer   np
!        real      x(np)
!        real      y(np)
!    End

!***********************************************************************

!    subroutine WPigSetFillColour (FillColour)
!        integer FillColour
!    End

!    subroutine WPigDrawFilledPolygon (n, x, y)
!        integer n
!        real x  (n)
!        real y  (n)
!    End

!***********************************************************************

!    subroutine WPigGetMouseAndButton(Window, Button, x, y)
!        integer     Window 
!        integer     Button 
!        real        x      
!        real        y      
!    End

!    subroutine WPigGetRubberMouseAndButton(Window, Button, x, y)
!        integer     Window 
!        integer     Button 
!        real        x      
!        real        y      
!    End

!      subroutine WPigRubberLine(Xnew, Ynew, Xinit, Yinit)
!        REAL  Xnew, Ynew, Xinit, Yinit
!      end

!      subroutine WPigDragRubberMouse (Win, xdif, ydif)
!        integer win
!        REAL Xdif, Ydif
!        End

!      subroutine WPigGetZoomArea2 (x1, x2, y1, y2)
!          real  x1 
!          real  x2 
!          real  y1 
!          real  y2 
!      End

!***********************************************************************

!      subroutine WPigDrawText ( PosX, PosY, Text)
!        real PosX
!        real PosY
!        character*(*) Text
!        integer ilen

!        ilen = LEN_TRIM(Text)
!        write(*,*) Text(1:ilen)
!      End

!      subroutine WPigGetString (PromptString,CharLen, RetString)
!          character*(*) PromptString
!          integer       CharLen
!          character*(*) RetString
!      End

!      subroutine WPigSetTextColour (TextColour)
!        integer TextColour
!      End

!      subroutine WPigSetTextAlignment (Horizontalp,  Verticalp)
!         integer*2 Horizontalp
!         integer*2 Verticalp
!      End

!      subroutine WPigGetTextExtent (string, TextWidth, TextHeight)
!         character*200   string       
!         real            TextWidth    
!         real            TextHeight   
!      End

!      subroutine WPigSetCharSpacing (NewSpacing)
!         real     NewSpacing
!      End

!***********************************************************************

      logical function WPigGetOpenFileName (prompt, fname, template)
        character*(*)  prompt
        character*(*)  fname
        character*(*)  template
        integer n1, n2, nlen
        logical WPigGetFileName

        fname=' '
        nlen = len_trim(prompt)
        n1 = index(template, '(') + 1
        n2 = index(template, ')') - 1
! error checks here
        if(WPigGetFileName (prompt(1:nlen)//char(0), fname, template(n1:n2)//char(0))) then
          WPigGetOpenFileName=.true.
        else
          WPigGetOpenFileName=.false.
        endif
        write(*,*) WPigGetOpenFileName, fname(1:1) 
        write(*,*) fname
      End

      logical function WPigOpenFileCD(nunit, prompt, fname, template)
        integer nunit, n1, n2, nlen
        character *(*) fname, prompt, template
        logical WPigGetFileName

        fname=' '
        nlen = len_trim(prompt)
        n1 = index(template, '(') + 1
        n2 = index(template, ')') - 1
! error checks here
        if(WPigGetFileName (prompt(1:nlen)//char(0), fname, template(n1:n2)//char(0))) then
          WPigOpenFileCD=.true.
          OPEN(UNIT=nunit,file=FNAME,status='unknown')
        else
          WPigOpenFileCD=.false.
        endif
        write(*,*) WPigOpenFileCD, fname(1:1) 
        write(*,*) fname
      end

      logical function WPigOpenFile(nunit, prompt, fname, template)
        integer nunit
        character *(*) fname, prompt, template
        integer n1, n2, nlen
        logical WPigGetFileName

        fname=' '
        nlen = len_trim(prompt)
        n1 = index(template, '(') + 1
        n2 = index(template, ')') - 1
! error checks here
        if(WPigGetFileName (prompt(1:nlen)//char(0), fname, template(n1:n2)//char(0))) then
          WPigOpenFile=.true.
          OPEN(UNIT=nunit,file=FNAME,status='unknown')
        else
          WPigOpenFile=.false.
        endif
        write(*,*) WPigOpenFile, fname(1:1) 
        write(*,*) fname
      end

!***********************************************************************

!      subroutine WPigUWait (delay_secs)
!          real delay_secs
!      End

!      subroutine WPigExit ()
!      End

!      subroutine WPigFatal (Message)
!         character*(*)   Message
!      End

!**********************************************************************

!      Subroutine MNU_MainMenuDisable
!    end

!**********************************************************************

!    Subroutine MNU_MainMenuEnable
!    end

!**********************************************************************

!      Subroutine MNU_NodeMenuDisable
!    end

!**********************************************************************

!    Subroutine MNU_NodeMenuEnable
!    end

!**********************************************************************

!      Subroutine MNU_GridMenuDisable
!    end

!**********************************************************************

!    Subroutine MNU_GridMenuEnable
!    end

!**********************************************************************


!    Subroutine SetMenuChkFlags( fn, fg, fc, fd )
!      logical fn, fg, fd, fc
!    end

!**********************************************************************


!    Subroutine SetTransChkFlags( f1,f2,f3,f4 )
!      logical f1,f2,f3,f4
!    end

!**********************************************************************


!    Subroutine SetLinChkFlags( fn )
!      logical fn
!    end

!**********************************************************************


!    Subroutine SetPolarChkFlags( fn )
!      logical fn
!    end

!**********************************************************************


!    Subroutine SetMercChkFlags( fn )
!      logical fn
!    end

!**********************************************************************


!    Subroutine SetTMChkFlags( fn )
!      logical fn
!    end

!**********************************************************************

    Subroutine gridgenhelp
    call WPigMessageOK ('See doc directory'//char(0),'help'//char(0)) 
    end

!**********************************************************************

    Subroutine limits ( np,npmax,nbmax,bndmax )
    implicit none

    character*40 str1,str2,str3,str4
    character*160 result
    integer lstr1,lstr2,lstr3,lstr4
    integer np,npmax,nbmax,bndmax    

    write(str1,'(a,i7)') 'np     = ',np
    write(str2,'(a,i7)') 'npmax  = ',npmax
    write(str3,'(a,i7)') 'nbmax  = ',nbmax
    write(str4,'(a,i7)') 'bndmax = ',bndmax
    lstr1 = len_trim(str1)
    lstr2 = len_trim(str2)
    lstr3 = len_trim(str3)
    lstr4 = len_trim(str4)
    result = str1(:lstr1)//char(10)//str2(:lstr2)//char(10)//&
        str3(:lstr3)//char(10)//str4(:lstr4)//char(10)//char(0)
    call PigMessageOK(result, 'LIMITS'//char(0))

    end

!**********************************************************************

    Subroutine about (pname, version)
    implicit none    
    character(*) pname, version
    write(*,*) 'Program ',pname,' ',version
    end

!**********************************************************************

    Subroutine resetmouseoption ()
    end

!---------------------------------------------------------------------------*

      FUNCTION length ( s )

! Purpose : Determines the length of the string up to but not including the
! first null character; for working with strings passed to C routines, gives
! same result as strlen on null-terminated strings.
! Givens  : string s
! Returns : number of characters in s before null character
! Effects : None

        implicit none
        character(len=*) :: s
        integer length

        do length = 0, len(s)-1, 1
            if (s(length+1:length+1) .eq. char(0)) return
        end do
      end function length

!--------------------------------------------------------------------------*

      SUBROUTINE InfoFiles

! Purpose : Displays the default filenames currently in use during the
!           current interactive session.
! Givens  : None
! Returns : None
! Effects : None

      INCLUDE '../includes/defaults.inc'

      character*2048 cstr ! must be large enough for concatenation of all output
      integer len1, len2, len3, len4, lenx
!-----------BEGIN------------------

      if(DispNodes) then
        len1 = len_trim(NodeRName)
        lenx = length (NodeRName)
      else
        len1 = len_trim(GridRName)
        lenx = length(GridRName)
      endif
      ! if string is null-terminated, only include characters up to
      ! the null-termination
      len1 = min(len1, lenx)

      len2 = len_trim(LastInterim)
      lenx = length(LastInterim)
      len2 = min(len2, lenx)

      len3 = len_trim(ContFName)
      lenx = length(ContFName)
      len3 = min(len3, lenx)

      len4 = len_trim(BoundFName)
      lenx = length(BoundFName)
      len4 = min(len4, lenx)

      if(DispNodes) then
        cstr = &
       'Node File: '// NodeRName(:len1)//char(10)//&
       'Last Interim Node File: '// LastInterim(:len2)//char(10)//&
       'Contours File: '// ContFName(:len3)//char(10)//&
       'Boundary File: '// BoundFName(:len4)//char(0)
      else
        cstr = &
       'Grid File: '// GridRName(:len1)//newline//&
       'Last Interim Grid File: '// LastInterim(:len2)//char(10)//&
       'Contours File: '// ContFName(:len3)//char(10)//&
       'Boundary File: '// BoundFName(:len4)//char(0)
      endif

      call PigMessageOK(cstr, 'FILES')

      END

!-----------------------------------------------------------------------*

!---------------------------------------------------------------------------*
!                           NODECONF.FOR                                    *
!     This module contains the controlling functions for the configuration  *
!     options. The setting of colors for boundaries, contours, and grid     *
!     lines and the display and toggling of configuration options in the    *
!     right hand display panel.                                             *
!       ROUTINES: ConfigNodes, ConfigBoundaries, ConfigContours, ConfigXhr. *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE ConfigNodes_Init

! PURPOSE: Toggles the node default attributes.
! GIVENS : None
! RETURNS: None
! EFFECTS: Defaults are set.
!--------------------------------------------------------------------------*

      call PigMessageOK('Unavailable','confignodes')

      return

      END

!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*
!                           CONFIG.FOR                                      *
!     This module contains the controlling functions for the configuration  *
!     option.  The setting of colours for boundaries, contours, and grid    *
!     lines and the display and toggling of configuration options in the    *
!     right hand display panel.                                             *
!---------------------------------------------------------------------------*
!---------------------------------------------------------------------------*

      SUBROUTINE ConfigLines_init
!      (hitnumdummy)

! Purpose: Toggles the grid default attributes.
! Givens : None
! Returns: None
! Effects: Defaults are set.

      implicit none

      call PigMessageOK('Unavailable','configgrid')

      END

!---------------------------------------------------------------------------*

      SUBROUTINE ConfigBoundaries_Init()

! Purpose: Toggles the attributes for the display of boundary information.
! Givens : None
! Returns: None
! Effects: None

      implicit none

      call PigMessageOK('Unavailable','configbnd')


      END

!---------------------------------------------------------------------------*

      SUBROUTINE ConfigContours_Init()

! Purpose: Toggles the attributes for the display of contour information.
! Givens : None
! Returns: None
! Effects: None

      implicit none


      END

!---------------------------------------------------------------------------*

      SUBROUTINE ConfigXhr(range)

! Purpose: Allow user to change the cursor sensitivity RANGE
! Givens : None
! Returns: None
! Effects: Range is updated only if a value was entered > 0.0 .

      implicit none


      INCLUDE '../includes/graf.def'

      REAL RANGE

      CHARACTER*80 ans
      CHARACTER*13 Oldr
      REAL temp
      LOGICAL Success

10    continue
      oldr = '             '
      write(oldr,'(F12.5)') RANGE

      call PigPrompt('RANGE (' //oldr// ') Enter new Range:',ans )

      call PigReadReal( ans, temp, Success )
      if ( .not. Success ) goto 10
      if ( temp .gt. 0.0 ) then
        RANGE = temp
      endif

      END


!---------------------------------------------------------------------------*
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

!---------------------END---------------------------------------------------*


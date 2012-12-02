
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

    Subroutine limits ( np,npmax,nbmax,bndmax )
    implicit none
    integer np,npmax,nbmax,bndmax    
    write(*,*) 'np = ',np
    write(*,*) 'npmax = ',npmax
    write(*,*) 'nbmax = ',nbmax
    write(*,*) 'bndmax = ',bndmax
    end

    Subroutine about (pname, version)
    implicit none    
    character(*) pname, version
    write(*,*) 'Program ',pname,' ',version
    end

    Subroutine resetmouseoption ()
    end

!**********************************************************************

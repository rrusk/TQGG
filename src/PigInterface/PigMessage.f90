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

  !***********************************************************************
!                      PIG Application Interface
!  ========================================================================= *
!  Purpose: The PIG.FOR and PIG1.FOR modules contains all necessary subroutines to
!           interface with a graphics system.  Many specific subroutines
!           will need to be changed as PIGS is ported from one graphical
!           environment to another.  All subroutines starting with an I, like
!           IPIGSetVP are internal calls and are not available for PUBLIC use.
! 
!  ------------------------------------------------------------------------- *

! ========================================================================= *

      Subroutine PigPutMessage(Text)

!  Purpose:  Draws a text message (Text) at set locations in the STATUSWIN
!            window, clears STATUSWIN first then draws
!  Givens :  character*(*)    Text   :  a variable length character string
!  Returns:  None
!  Effects:  Blanks the STATUSWIN, then prints Text (trailing blanks are
!            removed) in the current text colour in the STATUSWIN window

        character(*)   Text
        integer         len

        len = len_trim(Text)
        call WPigPutMessage (text(1:len)//char(0))

        end

! ========================================================================= *

      subroutine PigEraseMessage

!  Purpose:  Blanks out a defined viewport by calling IPigErase(WinNum)
!            with the fill colour set to the background colour. This gives
!            the effect of erasing an are on the screen. Called before text
!            is output to a viewport.
!  Givens :  None
!  Returns:  None
!  Effects:  Erases the entire status window, then draws the corresponding
!            border outline.

      call WPigEraseMessage

      end

! ========================================================================= *

      SUBROUTINE PigPrompt( PromptString, RetString )

!  Purpose: Prompt the user to input a string from the keyboard.
!  Givens:  character*(*) PromptString : String to use as the prompt.
!  Returns: character*80  RetString    : character return string.  Blank if an
!                                        error occurred.
!  Effects: User is prompted for appropriate input.

!      include 'ipig.def'

      CHARACTER*(*) PromptString
      CHARACTER*(*) RetString
      CHARACTER*(256)  tmpRetString
      integer nchar
      character*(256) tmpstr
      integer    LEN
!      integer PrevColour

!------------------BEGIN--------------------

      tmpstr = PromptString
      nchar = LEN_TRIM(PromptString)
      if(nchar+1.ge.LEN(tmpstr) ) then
        nchar = LEN(tmpstr) - 1
      endif
      tmpstr = PromptString(1:nchar)//char(0)
      nchar = LEN(RetString)
      if(nchar+1.ge.LEN(tmpRetString)) then
        nchar = LEN(tmpRetString) - 1
      endif
      tmpRetString = RetString(1:nchar)//char(0)
        call WPigGetString(tmpstr, nchar, tmpRetString)
      if(nchar.eq.-1) then  !cancel
        RetString(1:1)= char(0)
      elseif(nchar.eq.0) then
        RetString = ' '
      else
        RetString = tmpRetString(:nchar)
      endif

      END

! ========================================================================= *

      SUBROUTINE PigReadReal( S, RetVal, Success )

!  Purpose: Convert a string to a real number.
!  Givens : S - The string containing the real.
!  Returns: RetVal - The real number if the string was valid
!                    0.0 if the string was NULL.
!           Success - TRUE for a successful conversion.
!                   - FALSE otherwise.
!  Effects: None

! Passed variables.
      CHARACTER*(*)     S

!        - String to be converted.
      REAL              RetVal

!        - Result of the conversion
      LOGICAL           Success

! Local Variables.
      integer           status



      read(s,*,IOSTAT=status) retval
      if(status.eq.0) then
        Success = .TRUE.
      else
        Success = .FALSE.
      endif

      END

!---------------------------------------------------------------------------*
!                        NONPORT.FOR                                        *
!     The purpose of this module is to contain any nonportable routines     *
!---------------------------------------------------------------------------*

      SUBROUTINE PigMessageYesNo (Text,ans)

      character(*)   Text
      character*1 :: ans
      integer :: ilen
        
      ilen = LEN_TRIM(Text)
 !       write(*,'(a)') 'call  funct WPigCursYesNo, ans=',ans(1:1)
      call WPigCursYesNo(ans, Text(1:ilen)//char(0))
 !       write(*,'(a)') 'return from funct WPigCursYesNo, ans=',ans(1:1)
        
      return
      end

! **********************************************************

      CHARACTER*1 FUNCTION PigCursYesNo (messg)

!  Purpose: Provide ability to respond to yes/no prompt using cursor input.
!  Given  : messg = prompt message to respond to
!  Returns: PigCursYesNo = 1st letter of option selected

! - PASSED PARAMETERS
      CHARACTER*(*)  messg
!     CHARACTER*1    WPigCursYesNo
      character*1 reply
      character*(1024) Tmsg
      integer nchar

! --------- BEGIN--------

      Tmsg = messg
      nchar = min(1023,LEN_TRIM(messg))
      Tmsg(nchar+1:) = char(0)
      call WPigCursYesNo(reply, Tmsg)
      PigCursYesNo = reply
      call PigEraseMessage
      end

! **********************************************************

      CHARACTER*1 FUNCTION PigCursYesNoCancel (messg)

!  Purpose: Provide ability to respond to yes/no/cancel prompt using cursor input.
!  Given  : messg = prompt message to respond to
!  Returns: PigCursYesNoCancel = Y, N, or C
!  Effects: messg is displayed in dialog box.
!    Usage: To use this routine, pass the appropriate parameters, and assign
!           return values to "ans" & test "ans(1:1)", as in Prompt() calls.

! - PASSED PARAMETERS
      CHARACTER*(*) messg
      character*1 reply
      character*(1024) Tmsg
      integer nchar 

! --------- BEGIN--------

      Tmsg = messg
      nchar = min(1023,LEN_TRIM(messg))
      Tmsg(nchar+1:) = char(0)
      call WPigCursYesNoCancel(reply, Tmsg)
      PigCursYesNoCancel = reply
      call PigEraseMessage

      end
! ========================================================================= *

        Subroutine PigMessageOK(Text,title)
!  Purpose:  Creates a message window with (Text) and the caption (title).
!            Exit with the button OK.
!  Givens :  character*(*)    Text   :  a variable length character string
!            character*(*)    Title  :  a variable length character string
!  Returns:  None
!  Effects:  Prints Text (trailing blanks are
!            removed) in a message window.

      character*(*)   Text,title
      integer nchar

      nchar = LEN_TRIM(Text)
      call WPigMessageOK(Text(1:nchar)//char(0),title)

      end

! ========================================================================= *

      Subroutine PigStatusMessage(Text)

!  Purpose:  Creates a message window with (Text) and the caption (title).
!            Exit with the button OK.
!  Givens :  character*(*)    Text   :  a variable length character string
!            character*(*)    Title  :  a variable length character string
!  Returns:  None
!  Effects:  Prints Text (trailing blanks are
!            removed) in a message window.

      character*(*)   Text
      integer nchar

      nchar = LEN_TRIM(Text)
      call WPigStatusMessage(Text(1:nchar)//char(0))

      end

!---------------------------------------------------------------
!             file operations
!---------------------------------------------------------------
      
      logical function PigGetOpenFileName(prompt, name, template)

!  Purpose:  To select the name of an existing fil, so the caller can
!            then open the file.
!  Givens :  character*(*)      prompt - contains a message displayed with
!                               the request for a file name.
!            character*(*)      template - contains file name template
!                               including wild cards etc. 
!                               May require system dependent implementation.
!                               May be ignored completely.
!  Returns : logical            PigGetOpenFileName 
!                                       is .TRUE. if the user selected a 
!                                       valid filename, which is returned
!                                       in name. The file must exist.
!                                       is .FALSE. if the user cancelled
!                                       the operation.
!            character*(*)      name - contains a valid filename of an
!                               existing file.
!  Effects:  Gets the name of an existing file from the user.

      character *(*) name, prompt, template
      logical WPigGetOpenFileName

      PigGetOpenFileName = WPigGetOpenFileName(prompt,name,template)

      return
      end

!------------------------------------------------------------
      
      logical function PigOpenFileCD(nunit, prompt, fname, template)

!  Purpose:  To select the name of an existing or new file, open the file,
!            and change the default directory
!  Givens :  integer            nunit - logical unit number
!            character*(*)      prompt - contains a message displayed with
!                               the request for a file name.
!            character*(*)      template - contains file name template
!                               including wild cards etc. 
!                               May require system dependent implementation.
!                               May be ignored completely.
!  Returns : logical            PigOpenFileCD 
!                                       is .TRUE. if the user selected a 
!                                       valid filename, which is returned
!                                       in name. The file must exist.
!                                       is .FALSE. if the user cancelled
!                                       the operation.
!            character*(*)      name - contains a valid filename of an
!                               existing file.
!  Effects:  Opens a file 

      integer nunit
      character *(*) fname, prompt, template
      logical WPigOpenFileCD

      PigOpenFileCD = WPigOpenFileCD(nunit, prompt, fname, template)

      return
      end

!------------------------------------------------------------
            
      logical function PigOpenFile(nunit, prompt, fname, template)

!  Purpose:  To select the name of an existing or new file, open the file,
!  Givens :  integer            nunit - logical unit number
!            character*(*)      prompt - contains a message displayed with
!                               the request for a file name.
!            character*(*)      template - contains file name template
!                               including wild cards etc. 
!                               May require system dependent implementation.
!                               May be ignored completely.
!  Returns : logical            PigOpenFile 
!                                       is .TRUE. if the user selected a 
!                                       valid filename, which is returned
!                                       in name. The file must exist.
!                                       is .FALSE. if the user cancelled
!                                       the operation.
!            character*(*)      name - contains a valid filename of an
!                               existing file.
!  Effects:  Opens a file 

      integer nunit
      character *(*) fname, prompt, template
      logical WPigOpenFile

      PigOpenFile = WPigOpenFile(nunit, prompt, fname, template)

      return
      end
       
! ========================================================================= *

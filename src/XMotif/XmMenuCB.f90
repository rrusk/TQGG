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

!  ------------------------------------------------------------------------- *

      SUBROUTINE MenuCBsubs
        
      use MainArrays
        
      implicit none

!  PURPOSE:  This module contains subroutines to initialize and manipulate
!            the menu structures.

! *** GLOBAL VARIABLES
      include '../includes/graf.def'
      INCLUDE '../includes/defaults.inc'
      INCLUDE '../includes/edpolys.inc'

      LOGICAL TrHiOff
      COMMON /TH/ TrHiOff

      real, save :: RANGE = -999.

!       Only boundaries are displayed when OUTLINEONLY is .TRUE.
      LOGICAL OUTLINEONLY
      COMMON /OUTLINE/ OUTLINEONLY

      REAL    XMAX, YMAX, XMIN, YMIN

      real currstep
      logical firststep
      common /stepval/ currstep, firststep

      logical, save :: FlagPolar
      logical FlagN
      logical FlagG
      logical FlagD
      logical FlagC
      common /MenuDrawFlags/ FlagN,FlagG,FlagC,FlagD

      logical polylist(mrec)
      common /TG2Interface/ polylist

      integer, save :: Active_MW

! - PARAMETERS (constants)

      INTEGER  INACTIVE_MW, NODEINFO_MW, TRIINFO_MW, PLACEMARKERS_MW
      PARAMETER (&
     &        INACTIVE_MW=-1,&
     &        NODEINFO_MW=4, &
     &        TRIINFO_MW=9,& 
     &        PLACEMARKERS_MW=5)

      integer, parameter :: Sample_MW=20
      integer GridInsert_MW, GridCleave_MW,GridExchange_MW,GridDekite_MW
      parameter (GridInsert_MW=22,GridCleave_MW=21,GridExchange_MW=23,GridDekite_MW=24) 

      integer GridAddNode_MW,GridDelNode_MW,GridAddLine_MW,GridDelLine_MW
      integer GridMove_MW, GridMerge_MW
      parameter (GridAddNode_MW=25,GridDelNode_MW=26) 
      parameter (GridAddLine_MW=27,GridDelLine_MW=28,GridMove_MW=29,GridMerge_MW=30)

      integer NodeAddBnd_MW,NodeDelBnd_MW,NodeMoveBnd_MW,NodeRevBnd_MW
      integer NodeSplitBnd_MW,NodeJoinBnd_MW, NodeResample_MW
      integer NodeResBnd_MW, NodeStrBnd_MW, NodeDelIsl_MW
      integer NodeAddInt_MW,NodeDelInt_MW,NodeMoveInt_MW,NodeStrInt_MW
      parameter (NodeAddBnd_MW=31, NodeDelBnd_MW=32, NodeMoveBnd_MW=33, NodeRevBnd_MW=34)
      parameter (NodeSplitBnd_MW=39, NodeJoinBnd_MW=35, NodeResample_MW=40)
      parameter (NodeResBnd_MW=36, NodeStrBnd_MW=37, NodeDelIsl_MW=38)
      parameter (NodeAddInt_MW=41, NodeDelInt_MW=42, NodeMoveInt_MW=43, NodeStrInt_MW=45)

      integer PolyDef_MW
      parameter (PolyDef_MW=51)

      integer Zoomin_MW, Pan_MW
      parameter (Zoomin_MW=55,Pan_MW=56)

!       Local variables

      integer,save :: nrec,AutoGenFlag=0
      integer i,ierr
      integer ncode1,ncode2,iecode1,iecode2,ncn
      real zlimit,zlow,zscale
      LOGICAL, save :: Redrw, CHANGE, Ok, DrwFlag,Quit, retrowanted,success
      logical, save ::  newfile=.false.
      logical IN_BOX
      character cstr*80, ans*10, PigCursYesNo*1, deltype*1
      INTEGER PolyId, numvert
      real vertx1(maxvert+1),verty1(maxvert+1)

      integer Window, MouseButton
      integer, save :: Index
      real    MouseX, MouseY
      character*(80)  Message
      character*(20), save :: Program_name, Revision
      character(256),save :: cstrgrid
      integer, parameter :: BDOWN = 1, BUP = 2
      real, save :: xPan1, yPan1, xZoom1, yZoom1
      logical, save :: FirstPan=.false., LastPan=.false.
      logical, save :: FirstZoom=.false., LastZoom=.false.
      logical, save :: FirstPoint=.false., NextPoint=.false.

! ------------------------------------------------------------------------- *

      entry Initialiser()
        !Active_CW =INACTIVE_CW
        Active_MW =INACTIVE_MW

        newline = char(10)
        cstrgrid = 'Enter grid type:'//newline//&
                   ' 0 = latitude/longitude (degrees)'//newline//&
                   ' 1 = UTM coordinates (meters)'//newline//&
                   ' 2 = Cartesian coordinates (meters)'//newline//&
                   ' 3 = unspecified units'//newline//char(0)
        
!        AutoGenFlag = 0
!        newfile = .false.
!       - initialize EDPOLYS variables
        actvpoly = 0
        curpoly = 1
        numpolys = 0

!       - Set Marker type = .
        MarkType1 = 1

        GridRName = 'NONE'
        GridIName = 'interim1.ngh'
        LastInterim = 'NONE'
        BoundFName = 'NONE'
        ContFName = 'NONE'
        SoundFName = 'NONE'
        VCritName = 'NONE'
        TCritName = 'NONE'

        GridPColour = cyan
        GridSColour = red
        ModificationColour = yellow
        ContColour = violet
        BoundColour = green
        GridSIndex = 9999999
        DispCont = .false.
        DispBound = .false.
        GMerge = .false.
        BoundCCW = .false.
        CheckBN1 = .false.

        NodeRName = 'NONE        '
        NodeIName = 'interim1.nod'
        LastNInterim = 'NONE        '

        NodeIColor = cyan
        NodeBColor = green
        NodeSColor = orange
        ModIFColor = yellow
        NodeSIndex = 9999999
        NodeMType = 2
        MarkMType = 2
        MarkColor = white
        InfoColor = yellow

        BoundNColor = white
        ContNColor = yellow
        SoundColour = orange

        call ErasePermMarkers

        Quit = .FALSE.

        call PigSetWorldCoordinates(0.0,1.0,0.0,1.0)
        call fullsize(0.0,1.0,0.0,1.0)
        call InitContVect
        FlagPolar=.false.
        FlagN=.false.
        FlagG=.false.
        FlagD=.false.
        FlagC=.false.

        call MNU_GridAndNodeMenuDisable
        call MNU_PolyMenuDisable

        Program_Name = 'TQGridGen'
        Revision = '$Revision: 14.3 $'
        call About(Program_name, Revision )

        itot = 0
        totcoords = 0

        CHANGE  = .TRUE.
!        closeRHP = .true.

        call WPigStatusMessage ('Finished initializing'//char(0))
        return

! Callback routines 

!  File menu
        entry OpenGridFileCB() !open grid
          call MNU_MainMenuDisable
          if(itot.gt.0) then
            IF (PigCursYesNo ('SAVE existing file first?').EQ.'Y') THEN
              if(DispNodes) then
                call SaveNFinal(Quit)
              else
                call SaveFinal( change,Quit)
              endif
            endif
          endif
          Quit = .true.
          FlagG = .false.
          call OpenGridFile(Quit)
          call PigEraseMessage
          if(.not.Quit) then
            if(igridtype.eq.-9999) then !ask for grid type
              do
                call PigPrompt(cstrgrid, ans )
                READ( ans, *, iostat=ierr ) igridtype
                if(ierr.eq.0) exit
              enddo
            else  !verify grid type
              write(cstr,'(I2)') igridtype
              call PigMessageYesNo( 'Gridtype is '//cstr(1:2)//' OK?', ans)
              if(ans(1:1).eq.'N') then
                do
                  call PigPrompt(cstrgrid, ans )

                  READ( ans, *, iostat=ierr ) igridtype
                  if(ierr.eq.0) exit
                enddo
              endif
            endif
            IF (itot.gt.1000) then
              outlineonly = .TRUE.
            else
              outlineonly = .FALSE.
            endif
            call InitVertexMarkers
            if(TotTr.eq.0) then
              CHANGE  = .TRUE.
            else
              CHANGE  = .FALSE.
            endif
            FlagG = .true.
            FlagN = .false.
            call DrwFig(change)
          endif
          Active_MW = INACTIVE_MW
          call MNU_MainMenuEnable
          call MNU_NodeMenuDisable
          call MNU_GridMenuEnable
          if(numpolys.gt.0) then
            call MNU_PolyMenuEnable
          endif
          return
        entry AddGridFileCB() !add grid
          call MNU_MainMenuDisable
          Quit = .false.
          call OpenGridFile(Quit)
          call PigEraseMessage
          if(.not.Quit) then
            call Merge_Grid()
            IF (itot.gt.1000) then
              outlineonly = .TRUE.
            else
              outlineonly = .FALSE.
            endif
            call InitVertexMarkers
            CHANGE  = .TRUE.
            FlagG = .true.
            FlagN = .false.
            call DrwFig(change)
          endif
          Active_MW = INACTIVE_MW
          call MNU_MainMenuEnable
          call MNU_NodeMenuDisable
          call MNU_GridMenuEnable
          if(numpolys.gt.0) then
            call MNU_PolyMenuEnable
          endif
          return
        entry OpenNodeFileCB()
          call MNU_MainMenuDisable
          if(itot.gt.0) then
            IF (PigCursYesNo ('SAVE existing file first?').EQ.'Y') THEN
              if(DispNodes) then
                call SaveNFinal(Quit)
              else
                call SaveFinal( change,Quit)
              endif
            endif
          endif
          FlagN = .false.
          call OpenNodeFile( Quit)
          if(.not.Quit) then
            if(igridtype.eq.-9999) then !ask for grid type
              do
                call PigPrompt(cstrgrid, ans )
                READ( ans, *, iostat=ierr ) igridtype
                if(ierr.eq.0) exit
              enddo
            else  !verify grid type
              write(cstr,'(I2)') igridtype
              call PigMessageYesNo( 'Gridtype is '//cstr(1:2)//' OK?', ans)
              if(ans(1:1).eq.'N') then
                do
                  call PigPrompt(cstrgrid, ans )
                  READ( ans, *, iostat=ierr ) igridtype
                  if(ierr.eq.0) exit
                enddo
              endif
            endif
            IF (itot.gt.1000) then
              outlineonly = .TRUE.
            else
              outlineonly = .FALSE.
            endif
            firststep = .TRUE.
            CHANGE  = .FALSE.
            FlagN = .true.
            FlagG = .false.
            call DrwFig(change)
            tottr = 0
            newfile = .true.
          endif
          Active_MW = INACTIVE_MW
          call MNU_MainMenuEnable
          call MNU_GridMenuDisable
          call MNU_NodeMenuEnable
          if(numpolys.gt.0) then
            call MNU_PolyNodeMenuEnable
          endif
          return
        entry AddNodeFileCB()
          if(.not.FlagN) then
            call  PigMessageOK('Read a node file first', 'AddNode')
            return
          endif
          call MNU_MainMenuDisable
          FlagN = .true.
          call AddNodeFile( Quit)
          if(.not.Quit) then
            IF (itot.gt.1000) then
              outlineonly = .TRUE.
            else
              outlineonly = .FALSE.
            endif
            firststep = .TRUE.
            CHANGE  = .FALSE.
            FlagN = .true.
            FlagG = .false.
            call DrwFig(change)
            newfile = .true.
            tottr = 0
          endif
          Active_MW = INACTIVE_MW
          call MNU_MainMenuEnable
          call MNU_GridMenuDisable
          call MNU_NodeMenuEnable
          if(numpolys.gt.0) then
            call MNU_PolyNodeMenuEnable
          endif
          return
        entry XSectionCB()
          call MNU_MainMenuDisable
          if(itot.gt.0) then
            IF (PigCursYesNo ('SAVE existing file first?').EQ.'Y') THEN
              if(DispNodes) then
                call SaveNFinal(Quit)
              else
                call SaveFinal( change,Quit)
              endif
            endif
          endif
          call XSection( ncode1, Quit )
          if(.not.quit) then
            if(igridtype.eq.-9999) then !ask for grid type
              do
                call PigPrompt(cstrgrid, ans )
                READ( ans, *, iostat=ierr ) igridtype
                if(ierr.eq.0) exit
              enddo
            else  !verify grid type
              write(cstr,'(I2)') igridtype
              call PigMessageYesNo( 'Gridtype is '//cstr(1:2)//' OK?', ans)
              if(ans(1:1).eq.'N') then
                do
                  call PigPrompt(cstrgrid, ans )
                  READ( ans, *, iostat=ierr ) igridtype
                  if(ierr.eq.0) exit
                enddo
              endif
            endif
            IF (itot.gt.1000) then
              outlineonly = .TRUE.
            else
              outlineonly = .FALSE.
            endif
            if(ncode1.eq.0) then
              FlagG = .false.
              FlagN = .true.
              DispNodes = .true.
              call MNU_GridMenuDisable
              call MNU_NodeMenuEnable
              if(numpolys.gt.0) then
                call MNU_PolyNodeMenuEnable
              endif
            else
              FlagG = .true.
              FlagN = .false.
              DispNodes = .false.
              call MNU_NodeMenuDisable
              call MNU_GridMenuEnable
              if(numpolys.gt.0) then
                call MNU_PolyMenuEnable
              endif
            endif
            call InitVertexMarkers
            change = .true.
            call DrwFig(change)
          endif
          Active_MW = INACTIVE_MW
          call MNU_MainMenuEnable
          return
        entry SampleCB()
          call MNU_MainMenuDisable
          if(itot.gt.0) then
            IF (PigCursYesNo ('SAVE existing file first?').EQ.'Y') THEN
              if(DispNodes) then
                call SaveNFinal(Quit)
              else
                call SaveFinal( change,Quit)
              endif
            endif
          endif

          call Sample( quit )
          if(.not.quit) then
            if(igridtype.eq.-9999) then !ask for grid type
              do
                call PigPrompt(cstrgrid, ans )
                READ( ans, *, iostat=ierr ) igridtype
                if(ierr.eq.0) exit
              enddo
            else  !verify grid type
              write(cstr,'(I2)') igridtype
              call PigMessageYesNo( 'Gridtype is '//cstr(1:2)//' OK?', ans)
              if(ans(1:1).eq.'N') then
                do
                  call PigPrompt(cstrgrid, ans )
                  READ( ans, *, iostat=ierr ) igridtype
                  if(ierr.eq.0) exit
                enddo
              endif
            endif
            IF (itot.gt.1000) then
              outlineonly = .TRUE.
            else
              outlineonly = .FALSE.
            endif
            CHANGE  = .FALSE.
            FlagN = .true.
            FlagG = .false.
            call DrwFig(change)
            tottr = 0
            newfile = .true.
          endif
          Active_MW = INACTIVE_MW
          call MNU_MainMenuEnable
          call MNU_GridMenuDisable
          call MNU_NodeMenuEnable
          if(numpolys.gt.0) then
            call MNU_PolyNodeMenuEnable
          endif
          return
        entry SaveInterimCB()
          if(itot.gt.0) then
            if(DispNodes) then
              call SaveNInterim(Quit)
            else
              call SaveInterim(Quit)
            endif
          else
            call PigPutMessage('There are no nodes to save')
          endif
          return
        entry SaveFinalCB()
          call MNU_MainMenuDisable
          if(itot.gt.0) then
            if(DispNodes) then
              call SaveNFinal(Quit)
            else
              call SaveFinal( change,Quit)
            endif
          else
            call PigPutMessage('There are no nodes to save')
          endif
          call MNU_MainMenuEnable
          return
        entry PrintCB()
          return
        entry QuitCB()
          if(itot.gt.0) then
            IF (PigCursYesNo ('SAVE file before quitting?').EQ.'Y') THEN
              if(DispNodes) then
                call SaveNFinal(Quit)
              else
                call SaveFinal( change,Quit)
              endif
            endif
          endif
          cstr='Do you really want to quit?'
          IF (PigCursYesNo(cstr) .EQ. 'Y') THEN
            call wpigexit()
          endif
          return
!   View menu
        entry RedrawOnly()
          call DrwFig(CHANGE)
          return
        entry RedrawCB()
          outlineonly = .FALSE.
          call DrwFig(CHANGE)
          return
        entry OutlineCB()
          outlineonly = .TRUE.
          call DrwFig(CHANGE)
          return
        entry FullsizeCB
          if(itot.gt.0) then
            xmin = minval(dxray(1:itot))
            xmax = maxval(dxray(1:itot))
            ymin = minval(dyray(1:itot))
            ymax = maxval(dyray(1:itot))
            if(FlagPolar) then
              call PolarShift
            endif
            call fullsize(xmin,ymin,xmax,ymax)
            call DrwFig(CHANGE)
          endif
          return
        entry ZoomCB()
          Active_MW = Zoomin_MW
          call PigStatusMessage('Zoom ACTIVE: Drag area')
          FirstZoom=.true.
          LastZoom=.false.
          return
        entry ZoomOutCB()
          call DisplayOut (Redrw)
          if(FlagPolar) then
            call PolarShift
          endif
          IF (Redrw) call DrwFig(CHANGE)
          return        
        entry PanCB()
          Active_MW = Pan_MW
          call PigStatusMessage('Pan ACTIVE: Drag to new location')
          FirstPan=.true.
          LastPan=.false.
          return
        entry LastViewCB()
          call DisplayLast (Redrw)
          if(FlagPolar) then
            call PolarShift
          endif
          IF (Redrw) call DrwFig(CHANGE)
          return
         entry ScaleCB()
          Active_MW = INACTIVE_MW
          call ScaleOrShift (1)
          Redrw = .TRUE.
          IF (Redrw) call DrwFig(CHANGE)
          return
        entry ShiftCB()
          Active_MW = INACTIVE_MW
          call ScaleOrShift (2)
          Redrw = .TRUE.
          IF (Redrw) call DrwFig(CHANGE)
          return
        entry RotateCB()
          Active_MW = INACTIVE_MW
          call ScaleOrShift (3)
          Redrw = .TRUE.
          IF (Redrw) call DrwFig(CHANGE)
          return
          
       entry SPXCB()
! *** check grid type
          if(igridtype.eq.0) then
            call PolarTransform
            FlagPolar = .true.
            igridtype = -1
            call PigMessageOK('Transform to local plane', 'Polar Transform')
          elseif(igridtype.eq.-1) then
            call XYTransform
            FlagPolar = .false.
            igridtype = 0
            call PigMessageOK('Transform to longitude/latitude (degrees)', 'Polar Transform')
          else
            cstr = 'Cannot transform this grid type'//newline//&
                   'Not a latitue/longitude grid'//char(0)
            call PigMessageOK(cstr, 'Polar Transform')
          endif
          Redrw = .TRUE.
          IF (Redrw) call DrwFig(CHANGE)
          return
          
        entry TMXCB()
! *** check grid type
          if(igridtype.eq.0) then
            call TMTransform
            igridtype = 1
            call PigMessageOK('Transform to UTM (meters)', 'UTM Transform')
          elseif(igridtype.eq.1) then
            call InverseTMTransform
            igridtype = 0
            call PigMessageOK('Transform to longitude/latitude (degrees)', 'UTM Transform')
          else
            cstr = 'Cannot transform this grid type'//newline//&
                   'Not a latitue/longitude or UTM grid'//char(0)
            call PigMessageOK(cstr, 'UTM Transform')
          endif
          Redrw = .TRUE.
          IF (Redrw) call DrwFig(CHANGE)
          return

! Info menu
        entry NodeInfoCB()
          Active_MW = NODEINFO_MW
          call PigStatusMessage('Info ACTIVE: Pick a point')
          return
        entry EleInfoCB()
          Active_MW = TRIINFO_MW
! Update triangle list as needed.
          if(change) then
            call RemoveNotExist(itot,code,nbtot,nl)
            call Element_Lister(CHANGE, .FALSE. ,&
     &          itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode,&
     &          x0off,y0off,scaleX,scaleY,igridtype)
            change = .false.
          endif
          call PigStatusMessage('info ACTIVE: Pick an element')        
          return
        entry NodeCheckCB()
          call FlagsVertices()
          return
        entry EleCheckCB()
          call FlagsTriangles_Init()
          return
        entry BndCheckCB()
          call BoundaryCheck()
          return
        entry EraseCheckCB()
          TrHiOff=.true.
          call VMarkOff
          call ErasePermMarkers
          DrwFlag = .TRUE.
          Redrw = .TRUE.
          IF (Redrw) call DrwFig(CHANGE)
          return
        entry PMarkCB()
          Active_MW = PLACEMARKERS_MW
          call PigStatusMessage('PMarkers ACTIVE: Pick a point')        
         return
        entry PMDelLastCB()
          Active_MW = PLACEMARKERS_MW
          call RemoveLastMarker( Success )
          call PigStatusMessage('PMarkers ACTIVE: Pick a point')        
          return
        entry PMDelAllCB()
          Active_MW = INACTIVE_MW
          call ErasePermMarkers
          call PigEraseMessage
          return
        entry SetRangeCB()
          call ConfigXhr(range)
          return
        entry TooCloseCB()
          if(range.lt.0.) then
            call ConfigXhr(range)
          endif
          if(.not.dispnodes) then
            TotCoords = itot
          else
            itot = TotCoords
          endif
          call CoincidentNodes(TotCoords,totbndys,PtsThisBnd, &
                                dxray,dyray,depth,code,range,dispnodes,igridtype)
! *** redraw here
          itot = Totcoords
          call DrwFig(.false.)
          return
        entry FileInfoCB()
          call InfoFiles
          return
        entry LimitInfoCB()
          nrec = itot
          call Limits( nrec, mrec, nbtot, maxnnb )
          return

!    GridGen menu
        entry GenOneFrontCB()          
          if(DispNodes.and.itot.ge.3.and.TotIntPts.eq.0) then
              AutoGenFlag = 1  !.true.
          elseif(.not.DispNodes.and.AutoGenFlag.eq.1) then
              AutoGenFlag = 2  !.true.
          elseif(AutoGenFlag.ne.2) then
            Autogenflag = 0
          endif

          if(newfile) then
            call SetFrontGenOptions
            newfile = .false.
          endif

          if(AutoGenFlag.eq.1.or.AutoGenFlag.eq.2) then
            call Gridit2(mrec,itot,dxray,dyray,depth,code,nbtot,&
     &  nbtotr,NL,maxtri,TotTr,ListTr,TCode,TotBndys,TotIntBndys,&
     &  PtsThisBnd,Quit,AutoGenFlag)
            if(.not.Quit) then
              if(itot.gt.1000) then
                outlineonly = .true.
              else
                outlineonly = .false.
              endif
              change = .true.
              DispNodes = .false.
              FlagN = .false.
              FlagG = .true.
              call InitVertexMarkers
              call DrwFig(.FALSE.)
            endif
          else
            call PigPutMessage('Cannot generate front')
          endif
          return
        entry GenClusterCB()
          if(DispNodes.and.itot.ge.3.or.AutoGenFlag.gt.0) then
            IF (numpolys.eq.0.or.actvpoly.le.0) then
              call WholePoly(Ok)
            endif
            AutoGenFlag = 3  !.true.
            call Gridit2(mrec,itot,dxray,dyray,depth,code,nbtot,&
     &  nbtotr,NL,maxtri,TotTr,ListTr,TCode,TotBndys,TotIntBndys,&
     &  PtsThisBnd,Quit,AutoGenFlag)
            if(.not.Quit) then
              if(itot.gt.1000) then
                outlineonly = .true.
              else
                outlineonly = .false.
              endif
              change = .true.
              DispNodes = .false.
              FlagN = .false.
              FlagG = .true.
              call InitVertexMarkers
              call DrwFig( .FALSE.)
              call MNU_NodeMenuDisable
              call MNU_GridMenuEnable
              if(numpolys.gt.0) then
                call MNU_PolyMenuEnable
              endif
            endif
          else
            call PigPutMessage('Cannot generate without boundary nodes')
          endif
          return
        entry GenOptionsCB()
          call SetGenDisplayOptions
          return
        entry GenAllFrontsCB()           
          if(DispNodes.and.itot.ge.3.and.TotIntPts.eq.0) then
            AutoGenFlag = 4  !.true.
          else
            Autogenflag = 0
          endif

          if(newfile) then
            call SetFrontGenOptions
            newfile = .false.
          endif

          if(AutoGenFlag.eq.4) then
            call Gridit2(mrec,itot,dxray,dyray,depth,code,nbtot,&
     &  nbtotr,NL,maxtri,TotTr,ListTr,TCode,TotBndys,TotIntBndys,&
     &  PtsThisBnd,Quit,AutoGenFlag)
            if(.not.Quit) then
              if(itot.gt.1000) then
                outlineonly = .true.
              else
                outlineonly = .false.
              endif
              change = .true.
              DispNodes = .false.
              FlagN = .false.
              FlagG = .true.
              call InitVertexMarkers
              call DrwFig(.FALSE.)
              call MNU_NodeMenuDisable
              call MNU_GridMenuEnable
              if(numpolys.gt.0) then
                call MNU_PolyMenuEnable
              endif
            endif
          else
            call PigPutMessage('Cannot generate front')
          endif
          return
        entry FrontOptionsCB()
          call PigMessageOK('Under construction','options')
          return
        entry GenHexCB()
          CHANGE  = .TRUE.
          if(change) then
            call RemoveNotExist(itot,code,nbtot,nl)
            call Element_Lister(CHANGE, .FALSE. , &
                   itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                   x0off,y0off,scaleX,scaleY,igridtype)
            change = .false.
          endif
          call GenHexCells (mrec,itot,dxray,dyray,code,nbtot,nbtotr,NL,Maxtri,TotTr,ListTr)
          quit = .false.
          change = .true.
          call DrwFig(CHANGE)
          change = .true.
          return
        entry GenSquaresCB()
          CHANGE  = .TRUE.
          if(change) then
            call RemoveNotExist(itot,code,nbtot,nl)
            call Element_Lister(CHANGE, .FALSE. , &
                   itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                   x0off,y0off,scaleX,scaleY,igridtype)
            change = .false.
          endif
          call GenQuadCells (mrec,itot,dxray,dyray,code,nbtot,nbtotr,NL,MaxTri,TotTr,ListTr)
          quit = .false.
          change = .true.
          call DrwFig(CHANGE)
          change = .true.
          return
        entry GenMixedCB()
          CHANGE  = .TRUE.
          if(change) then
            call RemoveNotExist(itot,code,nbtot,nl)
            call Element_Lister(CHANGE, .FALSE. , &
                   itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                   x0off,y0off,scaleX,scaleY,igridtype)
            change = .false.
          endif
          call GenMixedCells (mrec,itot,dxray,dyray,code,nbtot,nbtotr,NL,MaxTri,TotTr,ListTr)
          quit = .false.
          change = .true.
          call DrwFig(CHANGE)
          change = .true.
          return
        entry TriangulateCB()
          if(DispNodes.and.itot.ge.3) then
            AutoGenFlag = 0  !.false.
            call Gridit2(mrec,itot,dxray,dyray,depth,code,nbtot,nbtotr,NL,maxtri,TotTr,&
                    ListTr,TCode,TotBndys,TotIntBndys,PtsThisBnd,Quit,AutoGenFlag)
            if(.not.Quit) then
              if(itot.gt.1000) then
                outlineonly = .true.
              else
                outlineonly = .false.
              endif
              change = .true.
              DispNodes = .false.
              FlagN = .false.
              FlagG = .true.
              call InitVertexMarkers
              call DrwFig( .FALSE.)
              call MNU_NodeMenuDisable
              call MNU_GridMenuEnable
              if(numpolys.gt.0) then
                call MNU_PolyMenuEnable
              endif
            endif
          else
            call PigPutMessage('Cannot triangulate without nodes')
          endif
          return

! Node edit menus
        entry DeleteNodeCB()
          Active_MW = NodeDelBnd_MW
          call PigStatusMessage('Pick an EXISTING point')
          return
        entry MoveNodeCB()
          Active_MW = NodeMoveBnd_MW
          call PigStatusMessage('Pick an EXISTING point')
          FirstPoint=.true.
          NextPoint=.false.
          return
        entry AddBndNodeCB()
          Active_MW = NodeAddBnd_MW
          call PigStatusMessage('Pick a NEW boundary point')
          return
        entry ReverseBndCB()
          Active_MW = NodeRevBnd_MW
          call PigStatusMessage('Pick an EXISTING boundary')
          return
        entry SplitBndCB()
          Active_MW = NodeSplitBnd_MW
          call PigStatusMessage('Pick an EXISTING boundary point')
          FirstPoint=.true.
          return
        entry JoinBndCB()
          Active_MW = NodeJoinBnd_MW
          call PigStatusMessage('Pick an EXISTING boundary point')
          FirstPoint=.true.
          NextPoint=.false.
          return
        entry ReSampleBndCB()
          Active_MW = NodeResample_MW
          call PigStatusMessage('Pick an EXISTING boundary point')
          FirstPoint=.true.
          NextPoint=.false.
          return
        entry ReselectBndCB()
          Active_MW = NodeResBnd_MW
          call PigStatusMessage('Pick an EXISTING boundary point')
          FirstPoint=.true.
          NextPoint=.false.
          return
        entry AddBndLineCB()
          Active_MW = NodeStrBnd_MW
          call PigStatusMessage('Pick an EXISTING boundary point')
          FirstPoint=.true.
          NextPoint=.false.
          return
        entry DeleteIslCB()
          Active_MW = NodeDelIsl_MW
          call PigStatusMessage('Select Any Node On The Island To Delete.')
          return
        entry AddIntNodeCB()
          Active_MW = NodeAddInt_MW
          call PigStatusMessage('Pick a NEW interior point')
          return
        entry AddIntLineCB()
          Active_MW = NodeStrInt_MW
          call PigStatusMessage('Pick first EXISTING interior point')
          FirstPoint=.true.
          NextPoint=.false.
          return

!  Grid edit menus
        entry AddGridEdgeCB()
          Active_MW = GridADDLINE_MW
          call PigStatusMessage('Pick a point')
          FirstPoint=.true.
          NextPoint=.false.
          CHANGE  = .TRUE.
          return
        entry AddGridNodeCB()
          Active_MW = GridAddNode_MW
          call PigStatusMessage('Pick a NEW point')
          CHANGE  = .TRUE.
          return
        entry DelGridEdgeCB()
          Active_MW = GridDelLine_MW
          call PigStatusMessage('Pick an EXISTING edge')
          CHANGE  = .TRUE.
          return
        entry DelGridNodeCB()
          Active_MW = GridDelnode_MW
          call PigStatusMessage('Pick an EXISTING point')
          CHANGE  = .TRUE.
          return
        entry MoveGridNodeCB()
          Active_MW = GridMove_MW
          call PigStatusMessage('Move ACTIVE: Pick first EXISTING point')
          FirstPoint=.true.
          NextPoint=.false.
          return
        entry MergeGridNodeCB()
          Active_MW = GridMerge_MW
          call PigStatusMessage('Merge ACTIVE: Pick first EXISTING point')
          FirstPoint=.true.
          NextPoint=.false.
          CHANGE  = .TRUE.
          return
        entry CleaveGridNodeCB()
          Active_MW = GridCleave_MW
          call PigStatusMessage('Cleave ACTIVE: Pick an EXISTING point')
          CHANGE  = .TRUE.
          return
        entry InsertGridEdgeCB()
          Active_MW = GridInsert_MW
          call PigStatusMessage('Pick an EXISTING edge')
          CHANGE  = .TRUE.
          return
        entry ExchangeGridEdgeCB()
          Active_MW = GridExchange_MW
          call PigStatusMessage('Pick an EXISTING edge')
          CHANGE  = .TRUE.
          return
        entry DekiteGridCB()
          Active_MW = GridDekite_MW
          call PigStatusMessage('Pick an EXISTING 4-point')
          CHANGE  = .TRUE.
          return
        entry ReshapeGridCB()
          call PigStatusMessage('Reshaping in display window only-please wait')
          if(change) then
            call RemoveNotExist(itot,code,nbtot,nl)
            call Element_Lister(CHANGE, .FALSE. ,&
     &          itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode,&
     &          x0off,y0off,scaleX,scaleY,igridtype)
            change = .false.
          endif

!            adjust position of nodes with code = 0 in dispaly window
          Active_MW = INACTIVE_MW
          do i=1,itot
            polylist(i) = IN_BOX(dxray(i),dyray(i))
          enddo
          call Reshape2(itot,nbtot,dxray,dyray,code,NL,polylist)
          Call DRWFIG(CHANGE)
          call PigEraseMessage
          return
        entry ConvertGrid2NodesCB()
          change=.true.
          retrowanted=.true.
          call RemoveNotExist(itot,code,nbtot,nl)
          call Element_Lister(change, retrowanted, &
     &         itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
               x0off,y0off,scaleX,scaleY,igridtype)
          return

!  polygon generation
        entry CreatePolyCB()
          Ok = .FALSE.
          Active_MW = PolyDef_MW
          call PigStatusMessage('Pick Vertices, pick first again to close polygon.')
          FirstPoint=.true.
          NextPoint=.false.
          return
        entry WholePolyCB()
          call WholePoly(Ok)
          if(.not.dispnodes) then
            call MNU_PolyMenuEnable
          else
            call MNU_PolyNodeMenuEnable
          endif               
          return
        entry CyclePolyCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          else
            call CyclePoly
          endif
          return
        entry DeletePolyCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          else
            call DeletePoly
            IF (actvpoly.gt.0.and..not.dispnodes) then
              call MNU_PolyMenuEnable
            elseif (actvpoly.gt.0.and.dispnodes) then
              call MNU_PolyNodeMenuEnable
            else
              call MNU_PolyMenuDisable
              call MNU_PolyNodeMenuDisable
            endif
          endif
          return
        entry WritePolyCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          else
            Ok = .FALSE.
            call SaveEPoly (Ok)
            IF (.NOT. Ok) THEN
              cstr = 'ERROR saving , polygon NOT SAVED.'
              call PigPutMessage(cstr)
            ELSE
              cstr = 'Polygon SAVED.'
              call PigPutMessage(cstr)
            ENDIF
          endif
          return
        entry ReadPolyCB()
          call ReadEPoly (Ok)
          IF (.NOT. Ok) THEN
            cstr = 'ERROR reading file.'
            call PigPutMessage(cstr)
          ENDIF
          IF (actvpoly.gt.0.and..not.dispnodes) then
            call MNU_PolyMenuEnable
          elseif (actvpoly.gt.0.and.dispnodes) then
            call MNU_PolyNodeMenuEnable
          else
            call MNU_PolyMenuDisable
            call MNU_PolyNodeMenuDisable
          endif
          return

!  node polygon operations
        entry PolyReSampleCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
      
            call PolyResampleNodes(polylist)
            itot = Totcoords
          endif
          return
        entry PolyDelBndCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
      
            deltype = 'B'
            call DelPolyNodes (deltype,polylist,TotCoords,Totbndys,&
                               TotIntpts,PtsThisBnd,dxray,dyray,depth,code)
            itot = Totcoords
          endif
          return
        entry PolyDelIntCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
      
            deltype = 'I'
            call DelPolyNodes (deltype,polylist,TotCoords,Totbndys,&
                               TotIntpts,PtsThisBnd,dxray,dyray,depth,code)
            itot = Totcoords
          endif
          return
        entry PolyDelAllCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
      
            deltype = 'A'
            call DelPolyNodes (deltype,polylist,TotCoords,Totbndys,&
                               TotIntpts,PtsThisBnd,dxray,dyray,depth,code)
            itot = Totcoords
          endif
          return

!  Grid polygon operations
        entry PolyNodeCodeCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
      
      !enter ncode1,ncode2
10          call PigPrompt('Enter existing node Code to change from:', ans )
            READ( ans, FMT = '(I4)', err = 10 ) ncode1

20          call PigPrompt('Enter node Code to change to:', ans )
            READ( ans, FMT = '(I4)', err = 20 ) ncode2
      
            call ChangeNodeCode (ncode1,ncode2,itot,code,polylist)
            call DrwFig(change)
          endif
          return
        entry PolyEleCodeCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
      
            if(change) then
              call RemoveNotExist(itot,code,nbtot,nl)
              call Element_Lister(CHANGE, .FALSE. , &
                   itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                   x0off,y0off,scaleX,scaleY,igridtype)
              change = .false.
            endif
12          call PigPrompt('Enter existing element Code to change from:', ans )
            READ( ans, FMT = '(I4)', err = 12 ) iecode1

22          call PigPrompt('Enter element Code to change to:', ans )
            READ( ans, FMT = '(I4)', err = 22 ) iecode2

            ncn = 4      
            call ChangeElementCode (iecode1,iecode2,itot,TotTr,ncn,ListTR,TCode,polylist)
            call DrwFig(change)
          endif
          return
        entry PolyDekiteCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
            
            call PigPutMessage( 'Working')
            call DeKite2(itot,nbtotr,nbtot,dxray,dyray,code,NL,polylist)
            call PigEraseMessage

            change = .true.
            call DrwFig(CHANGE)
            endif
            return
        entry PolyReshapeCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            if(change) then
              call RemoveNotExist(itot,code,nbtot,nl)
              call Element_Lister(CHANGE, .FALSE. , &
                   itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                   x0off,y0off,scaleX,scaleY,igridtype)
              change = .false.
            endif
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)
            
            call PigPutMessage( 'Working')
            call Reshape2(itot,nbtot,dxray,dyray,code,NL,polylist)
            call PigEraseMessage
            change = .true.
            call DrwFig(CHANGE)
          endif
          return
        entry PolyDelGridCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)

            call PigPutMessage( 'Working')
            call DeleteGridInPoly2 (itot,nbtot,code,NL,polylist)
            call PigEraseMessage
            
            change = .true.
            call DrwFig(CHANGE)
          endif
          return
        entry PolySplitGridCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)

            call PigPutMessage( 'Working')
            call SplitGridInPoly (itot,nbtot,code,NL,polylist)
            call PigEraseMessage
                  
            change = .true.
            call DrwFig(CHANGE)
          endif
          return
        entry PolyRefineGridCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            if(change) then
              call RemoveNotExist(itot,code,nbtot,nl)
              call Element_Lister(CHANGE, .FALSE. , &
                   itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                   x0off,y0off,scaleX,scaleY,igridtype)
              change = .false.
            endif
            change = .true.
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)

            call PigMessageYesNo( 'Refine with z>zlimit?', ans)
            if(ans(1:1).eq.'Y') then
            !enter zlimit
              call PigPrompt( 'Enter zlimit (cutoff>zlimit):',ans)
              read(ans,*,iostat=ierr) zlimit
              if(ierr.ne.0) then
                call PigPutMessage('Error reading zlimit')
                return
              endif
!     !       - now determine points that are inside currently active poly
              do i=1,itot
                if(depth(i).lt.zlimit) then
                  PolyList(i) = .FALSE.
                endif
              ENDDO
            endif

            call PigPutMessage( 'Working')
            ncn = 4
            call Refine_by2(mrec,itot,nbtot,maxtri,TotTr,dxray,dyray,depth,code,Polylist,NL,ListTr)
            call PigEraseMessage
            
            change = .true.
            call DrwFig(CHANGE)
            change = .true.
          endif
          return
        entry PolyCutGridCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)


            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)

            !enter zlimit
            call PigPrompt( 'Enter zlimit (cutoff>zlimit):',ans)
            read(ans,*,iostat=ierr) zlimit
            if(ierr.ne.0) then
              call PigPutMessage('Error reading zlimit')
              return
            endif

            call PigPrompt( 'Enter zlow limit (cutoff<zlow):',ans)
            read(ans,*,iostat=ierr) zlow
            if(ierr.ne.0) then
              call PigPutMessage('Error reading zlow')
              return
            endif
            
            call PigPutMessage( 'Working')
            call CutGridInPoly (itot,nbtot,code,NL,depth,zlimit,zlow,polylist)
            change = .true.
            call RemoveNotExist(itot,code,nbtot,nl)
            call Element_Lister(CHANGE, .FALSE. , &
                     itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                     x0off,y0off,scaleX,scaleY,igridtype)
            call RemoveDanglingLinks (itot,nbtot,nbtotr,NL,TotTr,ListTr)
!     !      call RemoveDisconnected

            call PigEraseMessage
                  
            change = .true.
            call DrwFig(CHANGE)
          endif
          return
        entry PolySetDepthCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)

            do
              call PigPrompt('Enter scale factor for depths : ',ans)
              read(ans,*,iostat=ierr) zscale
              if(ierr.eq.0) exit
            enddo

            do
              call PigPrompt('Enter amount to add to depths : ',ans)
              read(ans,*,iostat=ierr) zlimit
              if(ierr.eq.0) exit
            enddo

            call SetDepth2 (itot,zscale,zlimit,depth,polylist)
            
            change = .true.
            call DrwFig( CHANGE)
          endif
          return
        entry PolyReDepthCB()
          IF (numpolys.eq.0) then
            call PigPutMessage('Please define a polygon first.')
          elseIF (actvpoly.le.0) then
            call PigPutMessage('Please activate a polygon first.')
          ELSE
            PolyId = actvpoly
            numvert = vertcnt(actvpoly)
            vertx1 = 0.
            verty1 = 0.
            vertx1(1:numvert) = vertx(actvpoly,1:numvert)
            verty1(1:numvert) = verty(actvpoly,1:numvert)

            call ListInPoly2(numvert,vertx1,verty1,mrec,itot,dxray,dyray,polylist)

            call PigPutMessage( 'Working')
            call ReDepth2 (itot,dxray,dyray,depth,polylist)
            call PigEraseMessage
            
            change = .true.
            call DrwFig(CHANGE)
          endif
          return

!  configure menus
        entry ConfigNodeCB()
          !Active_CW = INACTIVE_CW
          Active_MW = INACTIVE_MW
          call ConfigNodes_Init
          !Active_CW = CONFIGNODES_CW
          return
        entry ConfigGridCB()
          !Active_CW = INACTIVE_CW
          Active_MW = INACTIVE_MW
          call ConfigLines_Init
          !Active_CW = CONFIG_CW
          return
          return
        entry ConfigContourCB()
          Active_MW = INACTIVE_MW
          call ConfigCntrs_init
          return
        entry ConfigDataCB()
          !Active_CW = INACTIVE_CW
          Active_MW = INACTIVE_MW
          call ConfigBoundaries_Init
          !Active_CW = CONFIG_CW
          return

!  about
        entry AboutCB()
          call About(Program_name, Revision)
          return

!  help
        entry HelpCB()
          call GridGenHelp
          return

!*******************************        
!*
!*  MOUSE EVENT HANDLER
!*
!*******************************        
      entry MouseEHandler(Window, MouseButton, MouseX, MouseY)

      nrec = itot + 1
      if( (MouseButton.eq.BDOWN).and.(Active_MW.ne.INACTIVE_MW)) then
        if(Active_MW.eq.NODEINFO_MW) then
!     - see if the point exists
          call CHKPT( MouseX, MouseY, INDEX, ierr )
          if ( ierr .eq. 1 ) then
            call PigMessageOK('ERROR - Invalid point.','NodeInfo')
          else
            call PutNodeMarker(index)
            call WPigNodeInfo(index)  !hook for dialog
          endif
          call PigStatusMessage('Info ACTIVE: Pick a point')        
        elseif(Active_MW.eq.TRIINFO_MW) then
          call LocateElement( MouseX, MouseY, INDEX, ierr )
          if ( ierr .eq. 1 ) then
            call PigMessageOK('ERROR - Invalid element.','ElementInfo')
          else
            call PutTriMarker(index)
            call WPigElementInfo(index)  !hook for dialog
          endif
          call PigStatusMessage('Info ACTIVE: Pick an element')        
        elseif(Active_MW.eq.Zoomin_MW) then
          if(FirstZoom) then
            FirstZoom=.false.
            LastZoom=.true.
            xZoom1 = MouseX
            yZoom1 = MouseY
            call PigDrawModifySymbol( MouseX, MouseY )
          endif
        elseif(Active_MW.eq.Pan_MW) then
          if(FirstPan) then
            FirstPan=.false.
            LastPan=.true.
            xPan1 = MouseX
            yPan1 = MouseY
            call PigDrawModifySymbol( MouseX, MouseY )
          endif
        elseif(Active_MW.eq.PolyDef_MW) then
          call DefPoly2 (FirstPoint,NextPoint,MouseX, MouseY,Ok)
          if(.not.FirstPoint.and..not.NextPoint) then
            call PigStatusMessage('Done')
            Active_MW = INACTIVE_MW
            IF (actvpoly.gt.0.and..not.dispnodes) then
              call MNU_PolyMenuEnable
              call MNU_PolyNodeMenuDisable
            elseIF (actvpoly.gt.0.and.dispnodes) then
              call MNU_PolyNodeMenuEnable
              call MNU_PolyMenuDisable
            else
              call MNU_PolyNodeMenuDisable
              call MNU_PolyMenuDisable
            endif
          endif            
        elseif(Active_MW.eq.NodeAddBnd_MW) then
          call AddBdNode ( MouseX, MouseY )
          call PigStatusMessage('AddBndNode ACTIVE: Pick a NEW boundary point')
        elseif(Active_MW.eq.NodeDelBnd_MW) then
          call DelNode ( MouseX, MouseY )
          call PigStatusMessage('DelBndNode ACTIVE: Pick a boundary point')
        elseif(Active_MW.eq.NodeMoveBnd_MW) then
          call MovTypNode2 ( MouseX, MouseY, FirstPoint, NextPoint  )  !, 2)
          call PigStatusMessage('MoveBndNode ACTIVE: Pick a boundary point')
        elseif(Active_MW.eq.NodeRevBnd_MW) then
          call ReverseBoundary (MouseX, MouseY)
          call PigStatusMessage('ReverseBnd ACTIVE: Pick a boundary')        
        elseif(Active_MW.eq.NodeSplitBnd_MW) then
          call SplitBoundaries (MouseX, MouseY, FirstPoint )
          if(FirstPoint) then
            call PigStatusMessage('SplitBnd ACTIVE: Pick boundary point')
          endif        
        elseif(Active_MW.eq.NodeJoinBnd_MW) then
          call JoinBoundaries (MouseX, MouseY, FirstPoint, NextPoint )
          if(FirstPoint) then
            call PigStatusMessage('JoinBnd ACTIVE: Pick first boundary point')
          elseif(NextPoint) then            
            call PigStatusMessage('JoinBnd ACTIVE: Pick second boundary point')
          endif        
        elseif(Active_MW.eq.NodeReSample_MW) then
          call ReSampleBndNodes(MouseX,MouseY,FirstPoint,NextPoint)       
          if(.not.FirstPoint.and..not.NextPoint) then
            Active_MW =INACTIVE_MW
            call PigStatusMessage('Done')
          else            
            call PigStatusMessage('ResampleBndNodes ACTIVE: Pick a node')
          endif
        elseif(Active_MW.eq.NodeResBnd_MW) then
          call ReSelBndNodes(MouseX,MouseY,FirstPoint,NextPoint)       
          if(.not.FirstPoint.and..not.NextPoint) then
            Active_MW =INACTIVE_MW
            call PigStatusMessage('Done')
          else            
            call PigStatusMessage('ReselectBndNodes ACTIVE: Pick a node')
          endif
        elseif(Active_MW.eq.NodeStrBnd_MW) then
          call StraightBnd (nrec, MouseX, MouseY, FirstPoint, NextPoint)
          if(.not.FirstPoint.and..not.NextPoint) then
            Active_MW =INACTIVE_MW
            call PigStatusMessage('Done')
          else            
            call PigStatusMessage('InsertBndLine ACTIVE: Pick a boundary node')
          endif        
        elseif(Active_MW.eq.NodeDelIsl_MW) then
          call DelIsland (MouseX, MouseY)        
          call PigStatusMessage('DeleteIsland ACTIVE: Pick an island')        
        elseif(Active_MW.eq.NodeAddInt_MW) then
          call AddInNode (MouseX, MouseY)
          call PigStatusMessage('AddIntNode ACTIVE: Pick a NEW node')        
        elseif(Active_MW.eq.NodeDelInt_MW) then
          call DelNode ( MouseX, MouseY )
          call PigStatusMessage('DelIntNode ACTIVE: Pick an interior node')        
        elseif(Active_MW.eq.NodeMoveInt_MW) then
          call MovTypNode2 ( MouseX, MouseY, FirstPoint, NextPoint  )  !, 1)
          call PigStatusMessage('MoveIntNode ACTIVE: Pick an interior node')        
        elseif(Active_MW.eq.NodeStrInt_MW) then
          call StraightInt (nrec, MouseX, MouseY, FirstPoint, NextPoint)
          if(.not.FirstPoint.and..not.NextPoint) then
            Active_MW =INACTIVE_MW
            call PigStatusMessage('Done')
          else            
            call PigStatusMessage('InsertIntLine ACTIVE: Pick an interior node')
          endif        
        elseif(Active_MW.eq.GridAddLine_MW) then
          call GridAddLine( MouseX, MouseY, FirstPoint, NextPoint, change )
          call PigStatusMessage('AddLine ACTIVE: Pick a node')        
        elseif(Active_MW.eq.GridAddNode_MW) then
          if(change) then
            call RemoveNotExist(itot,code,nbtot,nl)
            call Element_Lister(CHANGE, .FALSE. , &
                   itot,nbtot,dxray,dyray,depth,nl,TotTr,ListTr,Tcode, &
                   x0off,y0off,scaleX,scaleY,igridtype)
            change = .false.
          endif
          call AddPro(MouseX, MouseY, CHANGE, NREC)
          call PigStatusMessage('AddNode ACTIVE: Pick a NEW node')        
        elseif(Active_MW.eq.GridDelLine_MW) then
          call DelSeg(MouseX, MouseY, CHANGE, IERR)
          call PigStatusMessage('DeleteLine ACTIVE: Pick an edge')        
        elseif(Active_MW.eq.GridDelNode_MW) then
          call DelPro(MouseX, MouseY,CHANGE)
          call PigStatusMessage('DelNode ACTIVE: Pick a node')        
          change = .true.
        elseif(Active_MW.eq.GridMove_MW) then
          call LnChg3( MouseX, MouseY, FirstPoint, NextPoint )
          call PigStatusMessage('MoveNode ACTIVE: Pick a node')        
          change = .true.
        elseif(Active_MW.eq.GridMerge_MW) then
          call MergeNode( MouseX, MouseY, FirstPoint, NextPoint )
          call PigStatusMessage('MergeNode ACTIVE: Pick a node')        
          change = .true.
        elseif(Active_MW.eq.GridCleave_MW) then
          call Cleave(NREC, MouseX, MouseY, IERR)
          call PigStatusMessage('CleaveNode ACTIVE: Pick a node')        
          change = .true.
        elseif(Active_MW.eq.GridInsert_MW) then
          call Insert(NREC, MouseX, MouseY, IERR)
          call PigStatusMessage('Insert ACTIVE: Pick an edge')        
          change = .true.
       elseif(Active_MW.eq.GridExchange_MW) then
          call Xchange(MouseX, MouseY, IERR)
          call PigStatusMessage('Exchange ACTIVE: Pick an edge')        
          change = .true.
        elseif(Active_MW.eq.GridDekite_MW) then
          call Dekite(MouseX, MouseY)
          call PigStatusMessage('Dekite ACTIVE: Pick a node')        
          change = .true.
        elseif(Active_MW.eq.PLACEMARKERS_MW) then
          call PutPermMarker ( MouseX, MouseY, Success )
          call PigStatusMessage('PMarkers ACTIVE: Pick a point')        
        endif

      elseif( (MouseButton.eq.BUP).and.(Active_MW.ne.INACTIVE_MW)) then
        if(Active_MW.eq.Zoomin_MW) then
          if(LastZoom) then
            call DisplayIn3 (xZoom1, yZoom1, mousex, mousey, Redrw)
            FirstZoom=.true.
            LastZoom=.false.
            if(FlagPolar) then
              call PolarShift
            endif
            IF (Redrw) call DrwFig(CHANGE)
            call PigStatusMessage('Zoom ACTIVE: Drag area')
            endif
        elseif(Active_MW.eq.Pan_MW) then
          if(LastPan) then
            xPan1 = MouseX - xPan1
            yPan1 = MouseY - yPan1
            call DisplayPan3 ( xPan1, yPan1, Redrw)
            FirstPan=.true.
            LastPan=.false.
            if(FlagPolar) then
              call PolarShift
            endif
            IF (Redrw) call DrwFig(CHANGE)
            call PigStatusMessage('Pan ACTIVE: Drag to new location')
          endif
        endif
 
      else
        write(Message,'(a,i3,i4,2f8.3)') 'MOUSE_EVENT:',Window,MouseButton, MouseX, MouseY
        call PigPutMessage(Message)
      endif

      return
      end

! ========================================================================= *

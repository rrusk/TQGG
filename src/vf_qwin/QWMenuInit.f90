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

!**********************************************************************

!     setup initial menu settings

      Logical Function INITIALSETTINGS()

      USE DFLIB

      include       'ipig.def'

      EXTERNAL OpenGridFileCB
      EXTERNAL AddGridFileCB
      EXTERNAL OpenNodeFileCB
      EXTERNAL AddNodeFileCB
      EXTERNAL XSectionCB
      EXTERNAL SampleCB
      EXTERNAL SaveInterimCB
      EXTERNAL SaveFinalCB
      EXTERNAL QuitCB
      EXTERNAL RedrawCB
      EXTERNAL OutlineCB
      EXTERNAL FullsizeCB
      EXTERNAL ZoomCB
      EXTERNAL ZoomOutCB
      EXTERNAL PanCB
      EXTERNAL LastViewCB
      EXTERNAL ScaleCB
      EXTERNAL ShiftCB
      EXTERNAL RotateCB
      EXTERNAL SPXCB
      EXTERNAL MercXCB
      EXTERNAL TMXCB
      EXTERNAL NodeInfoCB
      EXTERNAL EleInfoCB
      EXTERNAL NodeCheckCB
      EXTERNAL EleCheckCB
      EXTERNAL EraseCheckCB
      EXTERNAL PMarkCB
      EXTERNAL PMDelLastCB
      EXTERNAL PMDelAllCB
      EXTERNAL SetRangeCB
      EXTERNAL TooCloseCB
      EXTERNAL FileInfoCB
      EXTERNAL LimitInfoCB
      EXTERNAL GenOneFrontCB
      EXTERNAL GenClusterCB
      EXTERNAL GenOptionsCB
      EXTERNAL GenAllFrontsCB
      EXTERNAL FrontOptionsCB
      EXTERNAL GenHexCB
      EXTERNAL GenSquaresCB
      EXTERNAL GenMixedCB
      EXTERNAL TriangulateCB
      EXTERNAL DeleteNodeCB
      EXTERNAL MoveNodeCB
      EXTERNAL AddBndNodeCB
      EXTERNAL ReverseBndCB
      EXTERNAL JoinBndCB
      EXTERNAL ReselectBndCB
      EXTERNAL AddBndLineCB
      EXTERNAL DeleteIslCB
      EXTERNAL AddIntNodeCB
      EXTERNAL AddIntLineCB
      EXTERNAL PolyDelBndCB
      EXTERNAL PolyDelIntCB
      EXTERNAL PolyDelAllCB
      EXTERNAL AddGridEdgeCB
      EXTERNAL DelGridEdgeCB
      EXTERNAL AddGridNodeCB
      EXTERNAL DelGridNodeCB
      EXTERNAL MoveGridNodeCB
      EXTERNAL MergeGridNodeCB
      EXTERNAL CleaveGridNodeCB
      EXTERNAL InsertGridEdgeCB
      EXTERNAL ExchangeGridEdgeCB 
      EXTERNAL DekiteGridCB
      EXTERNAL ReshapeGridCB
      EXTERNAL ConvertGrid2NodesCB
      EXTERNAL CreatePolyCB
      EXTERNAL WholePolyCB
      EXTERNAL CyclePolyCB
      EXTERNAL DeletePolyCB
      EXTERNAL ReadPolyCB
      EXTERNAL WritePolyCB
      EXTERNAL PolyNodeCodeCB
      EXTERNAL PolyEleCodeCB
      EXTERNAL PolyDekiteCB
      EXTERNAL PolyReshapeCB
      EXTERNAL PolyDelGridCB
      EXTERNAL PolySplitGridCB
      EXTERNAL PolyRefineGridCB
      EXTERNAL PolyCutGridCB
      EXTERNAL PolySetDepthCB
      EXTERNAL PolyReDepthCB
      EXTERNAL PlotNodeCB
      EXTERNAL ConfigNodeCB
      EXTERNAL PlotGridCB
      EXTERNAL ConfigGridCB
      EXTERNAL PlotContourCB
      EXTERNAL ConfigContourCB
      EXTERNAL PlotDataCB
      EXTERNAL ConfigDataCB
      EXTERNAL HelpCB
      EXTERNAL AboutCB
      type (qwinfo) winfo
      LOGICAL result
!  start initialization
      
      winfo.TYPE = QWIN$MAX
      result = SETWSIZEQQ(QWIN$FRAMEWINDOW, winfo)

      result = APPENDMENUQQ(1,$MENUENABLED,'File'C,NUL )
      result = APPENDMENUQQ(1,$MENUENABLED,'OpenGrid'C,OpenGridFileCB )
      result = APPENDMENUQQ(1,$MENUENABLED,'AddGrid'C,AddGridFileCB )
      result = APPENDMENUQQ(1,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(1,$MENUENABLED,'OpenNode'C,OpenNodeFileCB )
      result = APPENDMENUQQ(1,$MENUENABLED,'AddNode'C,AddNodeFileCB )
      result = APPENDMENUQQ(1,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(1,$MENUENABLED,'CrossSection'C,XSectionCB )
      result = APPENDMENUQQ(1,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(1,$MENUENABLED,'Sample'C,SampleCB )
      result = APPENDMENUQQ(1,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(1,$MENUENABLED,'InterimSave'C,SaveInterimCB )
      result = APPENDMENUQQ(1,$MENUENABLED,'SaveAs'C,SaveFinalCB )
      result = APPENDMENUQQ(1,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(1,$MENUENABLED,'Print'C,WINPRINT )
      result = APPENDMENUQQ(1,$MENUENABLED,'Exit'C,QuitCB )

      result = APPENDMENUQQ(2,$MENUENABLED,'View'C,NUL )
      result = APPENDMENUQQ(2,$MENUENABLED,'Redraw'C,RedrawCB )
      result = APPENDMENUQQ(2,$MENUENABLED,'Outline'C,OutlineCB )
      result = APPENDMENUQQ(2,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(2,$MENUENABLED,'Fullsize'C,FullsizeCB )
      result = APPENDMENUQQ(2,$MENUENABLED,'Zoom'C,ZoomCB )
      result = APPENDMENUQQ(2,$MENUENABLED,'ZoomOut'C,ZoomOutCB )
      result = APPENDMENUQQ(2,$MENUENABLED,'Pan'C,PanCB )
      result = APPENDMENUQQ(2,$MENUENABLED,'LastView'C,LastViewCB )
      result = APPENDMENUQQ(2,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(2,$MENUENABLED,'Scale'C,ScaleCB )
      result = APPENDMENUQQ(2,$MENUENABLED,'Shift'C,ShiftCB )
      result = APPENDMENUQQ(2,$MENUENABLED,'Rotate'C,RotateCB )
      result = APPENDMENUQQ(2,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(2,$MENUENABLED,'SP Polar Trans'C,SPXCB)
      result = APPENDMENUQQ(2,$MENUENABLED,'Mercator Trans'C,MercXCB)
      result = APPENDMENUQQ(2,$MENUENABLED,'TM Transform'C,TMXCB )

      result = APPENDMENUQQ(3,$MENUENABLED,'Info'C,NUL )
      result = APPENDMENUQQ(3,$MENUENABLED,'NodeInfo'C,NodeInfoCB )
      result = APPENDMENUQQ(3,$MENUENABLED,'ElementInfo'C,EleInfoCB )
      result = APPENDMENUQQ(3,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(3,$MENUENABLED,'NodeCheck'C,NodeCheckCB )
      result = APPENDMENUQQ(3,$MENUENABLED,'ElementCheck'C,EleCheckCB )
      result = APPENDMENUQQ(3,$MENUENABLED,'EraseChecks'C,EraseCheckCB )
      result = APPENDMENUQQ(3,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(3,$MENUENABLED,'PMarkers'C,PMarkCB )
      result = APPENDMENUQQ(3,$MENUENABLED,'EraseLast'C,PMDelLastCB )
      result = APPENDMENUQQ(3,$MENUENABLED,'EraseAll'C,PMDelAllCB )
      result = APPENDMENUQQ(3,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(3,$MENUENABLED,'SetRange'C,SetRangeCB )
      result = APPENDMENUQQ(3,$MENUENABLED,'TooClose'C,TooCloseCB )
      result = APPENDMENUQQ(3,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(3,$MENUENABLED,'Files'C,FileInfoCB )
      result = APPENDMENUQQ(3,$MENUENABLED,'Limits'C,LimitInfoCB )

      result = APPENDMENUQQ(4,$MENUENABLED,'GenGrids'C,NUL )
      result = APPENDMENUQQ(4,$MENUENABLED,'GenOneFront'C,GenOneFrontCB )
      result = APPENDMENUQQ(4,$MENUENABLED,'GenClusterPts'C,GenClusterCB )
      result = APPENDMENUQQ(4,$MENUENABLED,'Options'C,GenOptionsCB )
      result = APPENDMENUQQ(4,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(4,$MENUENABLED,'GenAllFronts'C,GenAllFrontsCB )
      result = APPENDMENUQQ(4,$MENUENABLED,'Options'C,FrontOptionsCB )
      result = APPENDMENUQQ(4,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(4,$MENUENABLED,'OverlayHex'C,GenHexCB )
      result = APPENDMENUQQ(4,$MENUENABLED,'OverlaySquares'C,GenSquaresCB )
      result = APPENDMENUQQ(4,$MENUENABLED,'OverlayMixed'C,GenMixedCB )
      result = APPENDMENUQQ(4,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(4,$MENUENABLED,'TriangulateNodes'C,TriangulateCB)

      result = APPENDMENUQQ(5,$MENUENABLED,'EditNode'C,NUL )
      result = APPENDMENUQQ(5,$MENUENABLED,'DeleteNode'C,DeleteNodeCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'MoveNode'C,MoveNodeCB )
      result = APPENDMENUQQ(5,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(5,$MENUENABLED,'AddBndNode'C,AddBndNodeCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'ReverseBnd'C,ReverseBndCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'JoinBnd'C,JoinBndCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'ReSelectBnd'C,ReselectBndCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'BndLine'C,AddBndLineCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'DeleteIsland'C,DeleteIslCB )
      result = APPENDMENUQQ(5,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(5,$MENUENABLED,'AddIntNode'C,AddIntNodeCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'IntLine'C,AddIntLineCB )
      result = APPENDMENUQQ(5,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(5,$MENUENABLED,'PolyDeleteBnd'C,PolyDelBndCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'PolyDeleteInt'C,PolyDelIntCB )
      result = APPENDMENUQQ(5,$MENUENABLED,'PolyDeleteAll'C,PolyDelAllCB )

      result = APPENDMENUQQ(6,$MENUENABLED,'EditGrid'C,NUL )
      result = APPENDMENUQQ(6,$MENUENABLED,'AddLine'C,AddGridEdgeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'AddNode'C,AddGridNodeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'DeleteLine'C,DelGridEdgeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'DeleteNode'C,DelGridNodeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'Move'C,MoveGridNodeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'Merge'C,MergeGridNodeCB )
      result = APPENDMENUQQ(6,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(6,$MENUENABLED,'CleaveNode'C,CleaveGridNodeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'Insert'C,InsertGridEdgeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'Exchange'C,ExchangeGridEdgeCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'DeKite'C,DekiteGridCB )
      result = APPENDMENUQQ(6,$MENUENABLED,'ReShape'C,ReshapeGridCB )
      result = APPENDMENUQQ(6,$MENUSEPARATOR,' 'C,NUL )
      result = APPENDMENUQQ(6,$MENUENABLED,'GridToNodes'C,ConvertGrid2NodesCB )

      result = APPENDMENUQQ(7,$MENUENABLED,'DefineGroup'C,NUL )
      result = APPENDMENUQQ(7,$MENUENABLED,'Create'C,CreatePolyCB )
      result = APPENDMENUQQ(7,$MENUENABLED,'Whole'C,WholePolyCB )
      result = APPENDMENUQQ(7,$MENUENABLED,'Cycle'C,CyclePolyCB )
      result = APPENDMENUQQ(7,$MENUENABLED,'Delete'C,DeletePolyCB )
      result = APPENDMENUQQ(7,$MENUENABLED,'Read'C,ReadPolyCB )
      result = APPENDMENUQQ(7,$MENUENABLED,'Write'C,WritePolyCB )

      result = APPENDMENUQQ(8,$MENUENABLED,'EditGroup'C,NUL )
      result = APPENDMENUQQ(8,$MENUENABLED,'NodeCode'C,PolyNodeCodeCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'ElementCode'C,PolyEleCodeCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'DeKite'C,PolyDekiteCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'ReShape'C,PolyReshapeCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'DeleteGrid'C,PolyDelGridCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'SplitGrid'C,PolySplitGridCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'RefineGrid'C,PolyRefineGridCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'CutoffGrid'C,PolyCutGridCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'ScaleDepths'C,PolySetDepthCB )
      result = APPENDMENUQQ(8,$MENUENABLED,'ReDepth'C,PolyReDepthCB )

      result = APPENDMENUQQ(9,$MENUENABLED,'Config'C,NUL )
      !result = APPENDMENUQQ(9,$MENUENABLED,'DrawNode'C,PlotNodeCB )
      result = APPENDMENUQQ(9,$MENUENABLED,'ConfigNode'C,ConfigNodeCB )
      !result = APPENDMENUQQ(9,$MENUSEPARATOR,' 'C,NUL )
      !result = APPENDMENUQQ(9,$MENUENABLED,'DrawGrid'C,PlotGridCB )
      result = APPENDMENUQQ(9,$MENUENABLED,'ConfigGrid'C,ConfigGridCB )
      !result = APPENDMENUQQ(9,$MENUSEPARATOR,' 'C,NUL )
      !result = APPENDMENUQQ(9,$MENUENABLED,'DrawContour'C,PlotContourCB )
      result = APPENDMENUQQ(9,$MENUENABLED,'ConfigContour'C,ConfigContourCB)
      !result = APPENDMENUQQ(9,$MENUSEPARATOR,' 'C,NUL )
      !result = APPENDMENUQQ(9,$MENUENABLED,'DrawData'C,PlotDataCB )
      result = APPENDMENUQQ(9,$MENUENABLED,'ConfigData'C,ConfigDataCB )

      result = APPENDMENUQQ(10,$MENUENABLED,'Help'C,NUL )
      result = APPENDMENUQQ(10,$MENUENABLED,'TQGG Help'C,HelpCB )
      result = APPENDMENUQQ(10,$MENUENABLED,'About TQGG'C,WINABOUT )

      INITIALSETTINGS = .true.
      End function INITIALSETTINGS

!**********************************************************************

      Subroutine MNU_MainMenuDisable

	USE DFLIB

	logical result

	result = modifymenuflagsqq(1,0,$MENUGRAYED)
	result = modifymenuflagsqq(2,0,$MENUGRAYED)
	result = modifymenuflagsqq(3,0,$MENUGRAYED)
	result = modifymenuflagsqq(4,0,$MENUGRAYED)
	result = modifymenuflagsqq(5,0,$MENUGRAYED)
	result = modifymenuflagsqq(6,0,$MENUGRAYED)
	result = modifymenuflagsqq(7,0,$MENUGRAYED)
	result = modifymenuflagsqq(8,0,$MENUGRAYED)
	result = modifymenuflagsqq(9,0,$MENUGRAYED)
	result = modifymenuflagsqq(10,0,$MENUGRAYED)
!	result = modifymenuflagsqq(11,0,$MENUGRAYED)

	return
	end

!**********************************************************************

	Subroutine MNU_MainMenuEnable

	USE DFLIB

	logical result

	result = modifymenuflagsqq(1,0,$MENUENABLED)
	result = modifymenuflagsqq(2,0,$MENUENABLED)
	result = modifymenuflagsqq(3,0,$MENUENABLED)
	result = modifymenuflagsqq(4,0,$MENUENABLED)
	result = modifymenuflagsqq(5,0,$MENUENABLED)
	result = modifymenuflagsqq(6,0,$MENUENABLED)
	result = modifymenuflagsqq(7,0,$MENUENABLED)
	result = modifymenuflagsqq(8,0,$MENUENABLED)
	result = modifymenuflagsqq(9,0,$MENUENABLED)
	result = modifymenuflagsqq(10,0,$MENUENABLED)
!	result = modifymenuflagsqq(11,0,$MENUENABLED)

	return
	end

!**********************************************************************

      Subroutine MNU_PolyMenuEnable

	USE DFLIB

	logical result

	result = modifymenuflagsqq(8,0,$MENUENABLED)

	return
	end

!**********************************************************************

      Subroutine MNU_PolyMenuDisable

	USE DFLIB

	logical result

	result = modifymenuflagsqq(8,0,$MENUGRAYED)

	return
	end

!**********************************************************************

      Subroutine MNU_NodeMenuDisable

	USE DFLIB

	logical result

!	result = modifymenuflagsqq(4,1,$MENUGRAYED)
!	result = modifymenuflagsqq(4,3,$MENUGRAYED)
!	result = modifymenuflagsqq(4,4,$MENUGRAYED)
!	result = modifymenuflagsqq(4,6,$MENUGRAYED)
!	result = modifymenuflagsqq(4,7,$MENUGRAYED)
	result = modifymenuflagsqq(4,12,$MENUGRAYED)
	result = modifymenuflagsqq(5,0,$MENUGRAYED)

	return
	end

!**********************************************************************

	Subroutine MNU_NodeMenuEnable

	USE DFLIB

	logical result

!	result = modifymenuflagsqq(4,1,$MENUENABLED)
!	result = modifymenuflagsqq(4,3,$MENUENABLED)
!	result = modifymenuflagsqq(4,4,$MENUENABLED)
!	result = modifymenuflagsqq(4,6,$MENUENABLED)
!	result = modifymenuflagsqq(4,7,$MENUENABLED)
	result = modifymenuflagsqq(4,12,$MENUENABLED)
	result = modifymenuflagsqq(5,0,$MENUENABLED)

	return
	end

!**********************************************************************

      Subroutine MNU_GridMenuDisable

	USE DFLIB

	logical result

	result = modifymenuflagsqq(3,2,$MENUGRAYED)
    result = modifymenuflagsqq(3,5,$MENUGRAYED)
	result = modifymenuflagsqq(4,8,$MENUGRAYED)
	result = modifymenuflagsqq(4,9,$MENUGRAYED)
	result = modifymenuflagsqq(4,10,$MENUGRAYED)
	result = modifymenuflagsqq(6,0,$MENUGRAYED)
	result = modifymenuflagsqq(8,0,$MENUGRAYED)

	return
	end

!**********************************************************************

	Subroutine MNU_GridMenuEnable

	USE DFLIB

	logical result

	result = modifymenuflagsqq(3,2,$MENUENABLED)
    result = modifymenuflagsqq(3,5,$MENUENABLED)
	result = modifymenuflagsqq(4,8,$MENUENABLED)
	result = modifymenuflagsqq(4,9,$MENUENABLED)
	result = modifymenuflagsqq(4,10,$MENUENABLED)
	result = modifymenuflagsqq(6,0,$MENUENABLED)
	result = modifymenuflagsqq(8,0,$MENUENABLED)

	return
	end

!**********************************************************************


	Subroutine SetMenuChkFlags0( fn, fg, fc, fd )

	USE DFLIB

	logical result
      logical fn, fg, fd, fc

	if(fn) then
	  result = modifymenuflagsqq(9,1,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(9,1,$MENUENABLED.OR.$MENUUNCHECKED)
	endif

	if(fg) then
	  result = modifymenuflagsqq(9,4,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(9,4,$MENUENABLED.OR.$MENUUNCHECKED)
	endif

	if(fc) then
	  result = modifymenuflagsqq(9,7,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(9,7,$MENUENABLED.OR.$MENUUNCHECKED)
	endif

	if(fd) then
	  result = modifymenuflagsqq(9,10,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(9,10,$MENUENABLED.OR.$MENUUNCHECKED)
	endif

	return
	end

!**********************************************************************


	Subroutine SetTransChkFlags( f1,f2,f3,f4 )

	USE DFLIB

      logical f1,f2,f3,f4

      call SetLinChkFlags(f1)
      call SetPolarChkFlags(f2)
      call SetMercChkFlags(f3)
      call SetTMChkFlags(f4)

	return
	end

!**********************************************************************


	Subroutine SetLinChkFlags( fn )

	USE DFLIB

	logical result
      logical fn

	if(fn) then
	  result = modifymenuflagsqq(2,9,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(2,9,$MENUENABLED.OR.$MENUUNCHECKED)
	endif


	return
	end

!**********************************************************************


	Subroutine SetPolarChkFlags( fn )

	USE DFLIB

	logical result
      logical fn

	if(fn) then
	  result = modifymenuflagsqq(2,13,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(2,13,$MENUENABLED.OR.$MENUUNCHECKED)
	endif


	return
	end

!**********************************************************************


	Subroutine SetMercChkFlags( fn )

	USE DFLIB

	logical result
      logical fn

	if(fn) then
	  result = modifymenuflagsqq(2,14,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(2,14,$MENUENABLED.OR.$MENUUNCHECKED)
	endif


	return
	end

!**********************************************************************


	Subroutine SetTMChkFlags( fn )

	USE DFLIB

	logical result
      logical fn

	if(fn) then
	  result = modifymenuflagsqq(2,15,$MENUENABLED.OR.$MENUCHECKED)
      else
	  result = modifymenuflagsqq(2,15,$MENUENABLED.OR.$MENUUNCHECKED)
	endif


	return
	end

!**********************************************************************

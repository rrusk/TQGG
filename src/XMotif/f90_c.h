/*
 * f90_c.h
 *
 *  Created on: Jan 31, 2013
 *      Author: rrusk
 */

#ifndef F90_C_H_
#define F90_C_H_

#define  Initialiser  initialiser_
#define  OpenGridFileCB  opengridfilecb_
#define  AddGridFileCB  addgridfilecb_
#define  OpenNodeFileCB  opennodefilecb_
#define  AddNodeFileCB  addnodefilecb_
#define  SampleCB  samplecb_
#define  XSectionCB  xsectioncb_
#define  SaveInterimCB  saveinterimcb_
#define  SaveFinalCB  savefinalcb_
#define  PrintCB  printcb_
#define  QuitCB  quitcb_
#define  RedrawCB  redrawcb_
#define  OutlineCB  outlinecb_
#define  FullsizeCB  fullsizecb_
#define  ZoomCB  zoomcb_
#define  ZoomOutCB  zoomoutcb_
#define  PanCB  pancb_
#define  LastViewCB  lastviewcb_
#define  ScaleCB  scalecb_
#define  ShiftCB  shiftcb_
#define  RotateCB  rotatecb_
#define  SPXCB  spxcb_
#define  MercXCB  mercxcb_
#define  TMXCB  tmxcb_
#define  NodeInfoCB  nodeinfocb_
#define  EleInfoCB  eleinfocb_
#define  NodeCheckCB  nodecheckcb_
#define  EleCheckCB  elecheckcb_
#define  BndCheckCB  bndcheckcb_
#define  EraseCheckCB  erasecheckcb_
#define  PMarkCB  pmarkcb_
#define  PMDelLastCB  pmdellastcb_
#define  PMDelAllCB  pmdelallcb_
#define  SetRangeCB  setrangecb_
#define  TooCloseCB  tooclosecb_
#define  FileInfoCB  fileinfocb_
#define  LimitInfoCB  limitinfocb_
#define  GenOneFrontCB  genonefrontcb_
#define  GenClusterCB  genclustercb_
#define  GenOptionsCB  genoptionscb_
#define  GenAllFrontsCB  genallfrontscb_
#define  FrontOptionsCB  frontoptionscb_
#define  GenHexCB  genhexcb_
#define  GenSquaresCB  gensquarescb_
#define  GenMixedCB  genmixedcb_
#define  TriangulateCB  triangulatecb_
#define  DeleteNodeCB  deletenodecb_
#define  MoveNodeCB  movenodecb_
#define  AddBndNodeCB  addbndnodecb_
#define  ReverseBndCB  reversebndcb_
#define  JoinBndCB  joinbndcb_
#define  SplitBndCB  splitbndcb_
#define  ReSampleBndCB  resamplebndcb_
#define  ReselectBndCB  reselectbndcb_
#define  AddBndLineCB  addbndlinecb_
#define  DeleteIslCB  deleteislcb_
#define  AddIntNodeCB  addintnodecb_
#define  AddIntLineCB  addintlinecb_
#define  AddGridEdgeCB addgridedgecb_
#define  DelGridEdgeCB delgridedgecb_
#define  AddGridNodeCB addgridnodecb_
#define  DelGridNodeCB delgridnodecb_
#define  MoveGridNodeCB movegridnodecb_
#define  MergeGridNodeCB mergegridnodecb_
#define  CleaveGridNodeCB cleavegridnodecb_
#define  InsertGridEdgeCB insertgridedgecb_
#define  ExchangeGridEdgeCB exchangegridedgecb_
#define  DekiteGridCB dekitegridcb_
#define  ReshapeGridCB reshapegridcb_
#define  ConvertGrid2NodesCB convertgrid2nodescb_
#define  CreatePolyCB  createpolycb_
#define  WholePolyCB  wholepolycb_
#define  CyclePolyCB  cyclepolycb_
#define  DeletePolyCB  deletepolycb_
#define  ReadPolyCB  readpolycb_
#define  WritePolyCB  writepolycb_
#define  PolyReSampleCB  polyresamplecb_
#define  PolyDelBndCB  polydelbndcb_
#define  PolyDelIntCB  polydelintcb_
#define  PolyDelAllCB  polydelallcb_
#define  PolyNodeCodeCB  polynodecodecb_
#define  PolyEleCodeCB  polyelecodecb_
#define  PolyDekiteCB  polydekitecb_
#define  PolyReshapeCB  polyreshapecb_
#define  PolyDelGridCB  polydelgridcb_
#define  PolySplitGridCB  polysplitgridcb_
#define  PolyRefineGridCB  polyrefinegridcb_
#define  PolyCutGridCB  polycutgridcb_
#define  PolySetDepthCB  polysetdepthcb_
#define  PolyReDepthCB  polyredepthcb_
#define  PlotNodeCB  plotnodecb_
#define  ConfigNodeCB  confignodecb_
#define  PlotGridCB  plotgridcb_
#define  ConfigGridCB  configgridcb_
#define  PlotContourCB  plotcontourcb_
#define  ConfigContourCB  configcontourcb_
#define  PlotDataCB  plotdatacb_
#define  ConfigDataCB  configdatacb_
#define  AboutCB  aboutcb_
#define  HelpCB  helpcb_
//#define  MenuCB  menucb_
//#define  MouseEHandler  mouseehandler_
#define  WPigMessageOK  wpigmessageok_
#define  WPigStatusMessage  wpigstatusmessage_
#define  WPigCursYesNo  wpigcursyesno_
#define  WPigCursYesNoCancel  wpigcursyesnocancel_
#define  WPigGetFileName  wpiggetfilename_
#define  WPigGetString  wpiggetstring_
#define  WPigSetLineColour  wpigsetlinecolour_
#define  WPigSetFillColour  wpigsetfillcolour_
#define  WPigSetSymbolColour  wpigsetsymbolcolour_
#define  WPigSetSymbolNumber  wpigsetsymbolnumber_
#define  WPigDrawPolyLine  wpigdrawpolyline_
#define  WPigDrawSymbols  wpigdrawsymbols_
#define  WPigDrawFilledPolygon  wpigdrawfilledpolygon_
#define  WPigSetWorldCoordinates  wpigsetworldcoordinates_
#define  WPigGetWorldCoordinates  wpiggetworldcoordinates_
#define  WPigEraseMain  wpigerasemain_
#define  WPigExit  wpigexit_
#define  MNU_MainMenuEnable mnu_mainmenuenable_
#define  MNU_MainMenuDisable mnu_mainmenudisable_
#define  MNU_GridAndNodeMenuDisable mnu_gridandnodemenudisable_
#define  MNU_GridMenuEnable mnu_gridmenuenable_
#define  MNU_GridMenuDisable mnu_gridmenudisable_
#define  MNU_NodeMenuEnable mnu_nodemenuenable_
#define  MNU_NodeMenuDisable mnu_nodemenudisable_
#define  MNU_PolyMenuEnable mnu_polymenuenable_
#define  MNU_PolyMenuDisable mnu_polymenudisable_
#define  MNU_PolyNodeMenuEnable mnu_polynodemenuenable_
#define  MNU_PolyNodeMenuDisable mnu_polynodemenudisable_
#define  WPigNodeInfo wpignodeinfo_
#define  GetNodeInfo getnodeinfo_
#define  WPigElementInfo wpigelementinfo_
#define  GetElementInfo getelementinfo_
#define  WPigNodeCheck wpignodecheck_
#define  WPigElementCheck wpigelementcheck_
#define  ElementCheck elementcheck_
#define  ReDrawOnly redrawonly_
#define  SetUserValue setuservalue_

#endif /* F90_C_H_ */

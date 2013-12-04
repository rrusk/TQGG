/*
 * XmMain.h
 *
 *  Created on: Feb 2, 2013
 *      Author: rrusk
 */

#ifndef XMMAIN_H_
#define XMMAIN_H_

#include <Xm/RowColumn.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/CascadeBG.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/FileSB.h>
#include <Xm/Form.h>
#include <Xm/TextF.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Label.h>       /* Label */
#include <Xm/LabelG.h>
#include <stdlib.h>

#include "f90_c.h"			/* binds gfortran subroutines to c */

#define  YES 1
#define  NO 2
#define  CANCEL 3

//extern double WCentx;
//extern double WCenty;
//extern double WtoVScale;

Widget      toplevel;
Widget      TopMenuW;
Widget      GridEditW;
Widget      NodeEditW;
Widget      GroupEditW;
Widget      GridGenW, GridGen1W;
Widget      message_w;
Widget      messageok_dialog;
Widget      messageyesno_dialog;
Widget      messageyesnocancel_dialog;
Widget      files_dialog;
Widget      prompt_dialog;
Widget      MainCanvas;

XGCValues    gcv;
GC           gc;
Dimension    width, height;
int          use_pixmap;  // allows user to toggle pixmap on/off

typedef struct _menu_item
{
	char              *label;          /* the label for the item */
	WidgetClass       *class;          /* pushbutton, label, separator... */
	char               mnemonic;       /* mnemonic; NULL if none */
	char              *accelerator;    /* accelerator; NULL if none */
	char              *accel_text;     /* to be converted to compound string */
	void             (*callback)();    /* routine to call; NULL if none */
	XtPointer          callback_data;  /* client_data for callback() */
	struct _menu_item *subitems;       /* pullright menu items, if not NULL */
} MenuItem;

typedef struct dummy_struct_name{
	int     reason;
	char    *answer;
	char    *name;
} XPCallback_data;

XtAppContext app;
//   static  int     XSymbolColour, XSymbolNumber;
//   static  int     XFillColour, XForegrColour, XBackgrColour,XTextColour, XLineColour;

//    static  int answer2;
//    static  double WorldX1, WorldX2, WorldY1, WorldY2, WCentx, WCenty,WtoVScale;
//    static  double X1, X2, Y1, Y2;

void exit( int exit_code );
void fileok_cb(Widget, XtPointer, XtPointer);
void filecancel_cb(Widget, XtPointer, XtPointer);
void promptok_cb(Widget, XtPointer, XtPointer);
void promptcancel_cb(Widget, XtPointer, XtPointer);
//void set_color(Widget, XtPointer, XtPointer) ;
void CreateDialogs( Widget );
void CreateDrawTools( Widget );
//void redraw();
void setUsePixmap();

//int   WPigGetOpenFileName(char *prompt, char *name, char *tmpl, const int dummy_len1, const int dummy_len2, const int dummy_len3);

void  Initialiser(void);
void  OpenGridFileCB(void);
void  AddGridFileCB(void);
void  OpenNodeFileCB(void);
void  AddNodeFileCB(void);
void  SampleCB(void);
void  XSectionCB(void);
void  SaveInterimCB(void);
void  SaveFinalCB(void);
void  PrintCB(void);
void  QuitCB(void);
void  RedrawCB(void);
void  OutlineCB(void);
void  FullsizeCB(void);
void  ZoomCB(void);
void  ZoomOutCB(void);
void  PanCB(void);
void  LastViewCB(void);
void  ScaleCB(void);
void  ShiftCB(void);
void  RotateCB(void);
void  SPXCB(void);
void  MercXCB(void);
void  TMXCB(void);
void  NodeInfoCB(void);
void  EleInfoCB(void);
void  NodeCheckCB(void);
void  EleCheckCB(void);
void  EraseCheckCB(void);
void  PMarkCB(void);
void  PMDelLastCB(void);
void  PMDelAllCB(void);
void  SetRangeCB(void);
void  TooCloseCB(void);
void  FileInfoCB(void);
void  LimitInfoCB(void);
void  GenOneFrontCB(void);
void  GenClusterCB(void);
void  GenOptionsCB(void);
void  GenAllFrontsCB(void);
void  FrontOptionsCB(void);
void  GenHexCB(void);
void  GenSquaresCB(void);
void  GenMixedCB(void);
void  TriangulateCB(void);
void  DeleteNodeCB(void);
void  MoveNodeCB(void);
void  AddBndNodeCB(void);
void  ReverseBndCB(void);
void  JoinBndCB(void);
void  ReselectBndCB(void);
void  AddBndLineCB(void);
void  DeleteIslCB(void);
void  AddIntNodeCB(void);
void  AddIntLineCB(void);
void  AddGridEdgeCB(void);
void  DelGridEdgeCB(void);
void  AddGridNodeCB(void);
void  DelGridNodeCB(void);
void  MoveGridNodeCB(void);
void  MergeGridNodeCB(void);
void  CleaveGridNodeCB(void);
void  InsertGridEdgeCB(void);
void  ExchangeGridEdgeCB(void);
void  DekiteGridCB(void);
void  ReshapeGridCB(void);
void  ConvertGrid2NodesCB(void);
void  CreatePolyCB(void);
void  WholePolyCB(void);
void  CyclePolyCB(void);
void  DeletePolyCB(void);
void  ReadPolyCB(void);
void  WritePolyCB(void);
void  PolyDelBndCB(void);
void  PolyDelIntCB(void);
void  PolyDelAllCB(void);
void  PolyNodeCodeCB(void);
void  PolyEleCodeCB(void);
void  PolyDekiteCB(void);
void  PolyReshapeCB(void);
void  PolyDelGridCB(void);
void  PolySplitGridCB(void);
void  PolyRefineGridCB(void);
void  PolyCutGridCB(void);
void  PolySetDepthCB(void);
void  PolyReDepthCB(void);
//    void  PlotNodeCB(void);
void  ConfigNodeCB(void);
//    void  PlotGridCB(void);
void  ConfigGridCB(void);
//    void  PlotContourCB(void);
void  ConfigContourCB(void);
//    void  PlotDataCB(void);
void  ConfigDataCB(void);
void  AboutCB(void);
void  HelpCB(void);
//    void  MenuCB( int *);
//    void  MouseEHandler(int *, int *, float *, float *);
void  drawing_area_callback(Widget, XtPointer, XtPointer);

#endif /* XMMAIN_H_ */

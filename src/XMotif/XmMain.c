/*
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
*/

#include <stdio.h>

#include "XmMain.h"
#include "pigdefs.h"

static XPCallback_data mycall_data;
static int answer;
/*----------------------------------------------------------------------------*/

void WPigMessageOK( char *question, char *reply, const int dummy_len1, const int dummy_len2 ) 
{
    XmString    yesno_question_string, title_string;
    Arg             args[5];
    Cardinal        n;

    yesno_question_string = XmStringGenerate(question, NULL, XmCHARSET_TEXT, "TAGOK");
    title_string = XmStringGenerate(reply, NULL, XmCHARSET_TEXT, "TAGOK");
    n =  0;
    XtSetArg(args[n], XmNmessageString, yesno_question_string); n++;
    XtSetValues(messageok_dialog, args, n);
    XmStringFree(yesno_question_string);

    XtVaSetValues(messageok_dialog, XmNdialogTitle, title_string, NULL);
    XmStringFree(title_string);

    XtManageChild(messageok_dialog);

    answer = 0;
    while (answer == 0)
        XtAppProcessEvent (app, XtIMAll);
/*    scanf ("%d",&n);

    XtUnmanageChild(messageok_dialog);*/
}

/*----------------------------------------------------------------------------*/

void WPigCursYesNo( char *reply, char *question, const int dummy_len1, const int dummy_len2) 
{
    XmString    yesno_question_string;
    Arg             args[5];
    Cardinal        n;   

    /*printf ("void WPigCursYesNo(%s)\n", question);*/ 
    yesno_question_string = XmStringCreateLocalized(question);
    n =  0;
    XtSetArg(args[n], XmNmessageString, yesno_question_string); n++;
    XtSetValues(messageyesno_dialog, args, n);
    XmStringFree(yesno_question_string);

    XtManageChild(messageyesno_dialog);
    
    answer = 0;
    while (answer == 0)
        XtAppProcessEvent (app, XtIMAll);

    reply[0] = 'N';
    if(answer == 1) reply[0] = 'Y';
    /*printf ("WPigMessageYesNo returning \"%c\"\n", reply[0]);*/
}

/*----------------------------------------------------------------------------*/

void WPigCursYesNoCancel( char *reply, char *question, const int dummy_len1, const int dummy_len2) 
{
    XmString    yesno_question_string;
    Arg             args[5];
    Cardinal        n;   

    /*printf ("void WPigCursYesNo(%s)\n", question);*/ 
    yesno_question_string = XmStringCreateLocalized(question);
    n =  0;
    XtSetArg(args[n], XmNmessageString, yesno_question_string); n++;
    XtSetValues(messageyesnocancel_dialog, args, n);
    XmStringFree(yesno_question_string);

    XtManageChild(messageyesnocancel_dialog);
    
    answer = 0;
    while (answer == 0)
        XtAppProcessEvent (app, XtIMAll);

    reply[0] = 'N';
    if(answer == 1) reply[0] = 'Y';
    if(answer == 3) reply[0] = 'C';
    /*printf ("WPigMessageYesNo returning \"%c\"\n", reply[0]);*/
}

/*----------------------------------------------------------------------------*/

int WPigGetFileName(char *prompt, char *name, char *tmpl )
{
    XmString    prompt_string;
    XmString    template_string;
    Arg         args[5];
    Cardinal    n;
    //char *filename;

    int anslen= strlen(name);    
    
    /*printf("prompt WPigGetFileName(%s)\n", prompt);
    printf("template WPigGetFileName(%s)\n", tmpl);*/

    prompt_string = XmStringCreateSimple(prompt);

    template_string = XmStringCreateSimple(tmpl);

    n =  0;
    XtSetArg(args[n], XmNselectionLabelString, prompt_string); n++;
    XtSetArg(args[n], XmNpattern, template_string); n++;
    XtSetValues(files_dialog, args, n);
    XmStringFree(prompt_string);
    XmStringFree(template_string);

    XtManageChild(files_dialog);

    mycall_data.reason = XmCR_NO_MATCH;
    while (mycall_data.reason == XmCR_NO_MATCH)
        XtAppProcessEvent (app, XtIMAll);

    XtUnmanageChild(files_dialog);

    name[0] = '\0';

    if (mycall_data.reason == XmCR_OK){
        name = (void *)strncpy(name,mycall_data.name,(anslen-1));
                //name = mycall_data.name;
    }
//    printf ("return from int iWPigGetOpenFileName(%s)\n", name);

    if(strlen(name)==0) return FALSE;
    /* otherwise return true */
    return TRUE;
}

/*----------------------------------------------------------------------------*/

void WPigGetString(char *prompt, int *anslen, char *retstring )
{
    XmString    prompt_string;
    //XmString    template_string;
    Arg         args[5];
    Cardinal    n;
       
    /*printf ("int WPigGetString(%s)\n", prompt);*/
    /*printf ("int WPigGetString(%d)\n", *anslen);*/

//    prompt_string = XmStringCreateSimple(prompt);
    prompt_string = XmStringCreateLocalized(prompt);

    n =  0;
    XtSetArg(args[n], XmNselectionLabelString, prompt_string); n++;
//    XtSetArg (args[n], XmNautoUnmanage, False); n++;
    prompt_dialog = XmCreatePromptDialog (toplevel, "prompt", args, n);
    XmStringFree (prompt_string);
    XtSetSensitive (XtNameToWidget (prompt_dialog, "Help"), False);
    XtAddCallback (prompt_dialog, XmNcancelCallback, promptcancel_cb, &mycall_data);
    XtAddCallback (prompt_dialog, XmNokCallback, promptok_cb, &mycall_data);
//    XtSetValues(prompt_dialog, args, n);
//    XmStringFree(prompt_string);

    XtManageChild(prompt_dialog);

    mycall_data.reason = XmCR_NO_MATCH;
    while (mycall_data.reason == XmCR_NO_MATCH)
        XtAppProcessEvent (app, XtIMAll);

    XtDestroyWidget (XtParent(prompt_dialog));

//    XtUnmanageChild(prompt_dialog);

    switch (mycall_data.reason) {
    case XmCR_OK:
        (void)strncpy(retstring, mycall_data.answer,*anslen);
        *anslen = strlen(retstring);
        break;
    case XmCR_CANCEL:
        retstring[0] = '\0';
        *anslen = -1;
        break;
    default:
        retstring[0] = '\0';
        *anslen = 0;
        break;
    }
//    printf ("int WPigGetString return (%s)\n", retstring);
}  

/*----------------------------------------------------------------------------*/

void WPigStatusMessage(char *message)
{
/* Set the text of the message window to message,  */
    /* XmString xmessage = XmStringCreateLocalized(message); */
    XmString xmessage = XmStringCreateSimple(message);
    XtVaSetValues(message_w,XmNlabelString, xmessage, NULL);
    XmStringFree(xmessage); 
    /* try XmUpdateDisplay */
    XmUpdateDisplay(message_w);
}
/*----------------------------------------------------------------------------*/

void  WPigExit(void)
{
    exit(0);
}

/* build_menu.c -- Demonstrate the BuildMenu() routine and
** how it can be used to build pulldown -and- pullright menus.
** Menus are defined by declaring an array of MenuItem structures.
*/
/* Pulldown menus are built from cascade buttons, so this function
** also includes pullright menus. Create the menu, the cascade button
** that owns the menu, and then the submenu items.
*/
/*----------------------------------------------------------------------------*/
Widget BuildMenu (Widget parent, int menu_type, char *menu_title, char menu_mnemonic, 
                  Boolean tear_off, MenuItem *items)
{
	Widget   menu, cascade, widget;
	int      i;
	XmString str;
	Arg      args[8];
	int      n;

	if (menu_type == XmMENU_PULLDOWN)	
		menu = XmCreatePulldownMenu (parent, "_pulldown", NULL, 0);
	else {
		n = 0;
		XtSetArg (args[n], XmNpopupEnabled, XmPOPUP_AUTOMATIC_RECURSIVE); n++;
		menu = XmCreatePopupMenu (parent, "_popup", args, n);
	}
	
	if (tear_off) 
		XtVaSetValues (menu, XmNtearOffModel, XmTEAR_OFF_ENABLED, NULL);
		
	if (menu_type == XmMENU_PULLDOWN) {
		str = XmStringCreateLocalized (menu_title);
		n = 0;
		XtSetArg (args[n], XmNsubMenuId, menu); n++;
		XtSetArg (args[n], XmNlabelString, str); n++;
		XtSetArg (args[n], XmNmnemonic, menu_mnemonic); n++;
		cascade = XmCreateCascadeButtonGadget (parent, menu_title, args, n);
		XtManageChild (cascade);
		XmStringFree (str);
	}

	/* Now add the menu items */
	for (i = 0; items[i].label != NULL; i++) {
		/* If subitems exist, create the pull-right menu by calling this
		** function recursively. Since the function returns a cascade
		** button, the widget returned is used.. 
		*/
		
		if (items[i].subitems)
			widget = BuildMenu (menu, XmMENU_PULLDOWN, items[i].label, 
                                                items[i].mnemonic, tear_off, items[i].subitems);
		else
			widget = XtVaCreateManagedWidget (items[i].label, *items[i].class, menu, NULL);

		/* Whether the item is a real item or a cascade button with a
		** menu, it can still have a mnemonic.
		*/
		if (items[i].mnemonic)
			XtVaSetValues (widget, XmNmnemonic, items[i].mnemonic, NULL);
			
		/* any item can have an accelerator, except cascade menus. But,
		** we don't worry about that; we know better in our declarations.
		*/
		if (items[i].accelerator) {
			str = XmStringCreateLocalized (items[i].accel_text);
			
			XtVaSetValues (widget, XmNaccelerator, items[i].accelerator, XmNacceleratorText, 
				       str, NULL);
			XmStringFree (str);
		}
		
		if (items[i].callback) {
			String resource;
			
			if (XmIsToggleButton(widget) || XmIsToggleButtonGadget(widget))
				resource = XmNvalueChangedCallback;
			else
				resource = XmNactivateCallback;
				
			XtAddCallback(widget, resource, items[i].callback,(XtPointer) items[i].callback_data);
		}
	}

	return (menu_type == XmMENU_PULLDOWN) ? cascade : menu ;
}

/* callback functions for menu items declared later... */
/*void menu_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
   int menuid = (int) client_data;

    printf ("Calling menuid %d\n", menuid);
    MenuCB ( &menuid );
}*/

void yesnoCB(Widget widget, XtPointer client_data, XtPointer call_data)
{
    int *answer = (int *) client_data;
    XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *) call_data;

    if (cbs->reason == XmCR_OK)
        *answer = YES;
    else if (cbs->reason == XmCR_CANCEL)
        *answer = NO;
    else if (cbs->reason == XmCR_HELP)
        *answer = CANCEL;
   
    /*printf ("yesnoCB %d\n", *answer);*/
}

MenuItem file_menu[] = {
	{ "OpenGrid", &xmPushButtonGadgetClass, 'O', NULL, NULL, OpenGridFileCB, (XtPointer) 101, (MenuItem *) NULL },
	{ "AddGrid", &xmPushButtonGadgetClass, 'A', NULL, NULL, AddGridFileCB, (XtPointer) 102, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "OpenNode", &xmPushButtonGadgetClass, 'N', NULL, NULL, OpenNodeFileCB, (XtPointer) 111, (MenuItem *) NULL },
    { "AddNode", &xmPushButtonGadgetClass, 'd', NULL, NULL, AddNodeFileCB, (XtPointer) 112, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
	{ "Sample",&xmPushButtonGadgetClass, 'S', NULL, NULL, SampleCB, (XtPointer) 121, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "XSection",&xmPushButtonGadgetClass, 'X', NULL, NULL, XSectionCB, (XtPointer) 131, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "InterimSave", &xmPushButtonGadgetClass, 'm', NULL, NULL, SaveInterimCB, (XtPointer) 141, (MenuItem *) NULL },
    { "SaveAs", &xmPushButtonGadgetClass, 'v', NULL, NULL, SaveFinalCB, (XtPointer) 142, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "Print", &xmPushButtonGadgetClass, 'P', NULL, NULL, PrintCB, (XtPointer) 151, (MenuItem *) NULL },
    { "Quit", &xmPushButtonGadgetClass, 'Q', NULL, NULL, QuitCB, (XtPointer) 161, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem view_menu[] = {
    { "Redraw", &xmPushButtonGadgetClass, 'R', NULL, NULL, RedrawCB, (XtPointer) 201, (MenuItem *) NULL },
    { "Outline", &xmPushButtonGadgetClass, 'O', NULL, NULL, OutlineCB, (XtPointer) 202, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "FullSize", &xmPushButtonGadgetClass, 'F', NULL, NULL, FullsizeCB, (XtPointer) 211, (MenuItem *) NULL },
    { "Zoom", &xmPushButtonGadgetClass, 'Z', NULL, NULL, ZoomCB, (XtPointer) 212, (MenuItem *) NULL },
    { "ZoomOut", &xmPushButtonGadgetClass, 'z', NULL, NULL, ZoomOutCB, (XtPointer) 215, (MenuItem *) NULL },
    { "Pan", &xmPushButtonGadgetClass, 'P', NULL, NULL, PanCB, (XtPointer) 213, (MenuItem *) NULL },
    { "LastView",&xmPushButtonGadgetClass, 'V', NULL, NULL, LastViewCB, (XtPointer) 214, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "Scale", &xmPushButtonGadgetClass, 'l', NULL, NULL, ScaleCB, (XtPointer) 671, (MenuItem *) NULL },
    { "Shift", &xmPushButtonGadgetClass, 'f', NULL, NULL, ShiftCB, (XtPointer) 672, (MenuItem *) NULL },
    { "Rotate", &xmPushButtonGadgetClass, 'R', NULL, NULL, RotateCB, (XtPointer) 673, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "PolarTransform", &xmPushButtonGadgetClass, 'P', NULL, NULL, SPXCB, (XtPointer) 233, (MenuItem *) NULL },
    { "UTMTransform", &xmPushButtonGadgetClass, 'T', NULL, NULL, TMXCB, (XtPointer) 237, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem info_menu[] = {
    { "NodeInfo", &xmPushButtonGadgetClass, 'N', NULL, NULL, NodeInfoCB, (XtPointer) 301, (MenuItem *) NULL },
    { "ElementInfo", &xmPushButtonGadgetClass, 'E', NULL, NULL, EleInfoCB, (XtPointer) 302, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "NodeCheck", &xmPushButtonGadgetClass, 'o', NULL, NULL, NodeCheckCB, (XtPointer) 311, (MenuItem *) NULL },
    { "ElementCheck", &xmPushButtonGadgetClass, 'C', NULL, NULL, EleCheckCB, (XtPointer) 312, (MenuItem *) NULL },
    { "BoundaryCheck", &xmPushButtonGadgetClass, 'C', NULL, NULL, BndCheckCB, (XtPointer) 314, (MenuItem *) NULL },
    { "EraseChecks", &xmPushButtonGadgetClass, 'r', NULL, NULL, EraseCheckCB, (XtPointer) 313, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "PMarkers",&xmPushButtonGadgetClass, 'P', NULL, NULL, PMarkCB, (XtPointer) 321, (MenuItem *) NULL },
    { "EraseLast",&xmPushButtonGadgetClass, 'L', NULL, NULL, PMDelLastCB, (XtPointer) 322, (MenuItem *) NULL },
    { "EraseAll",&xmPushButtonGadgetClass, 'A', NULL, NULL, PMDelAllCB, (XtPointer) 323, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "ConfigContr",&xmPushButtonGadgetClass, 'S', NULL, NULL, ConfigContourCB, (XtPointer) 922, (MenuItem *) NULL },
    { "ConfigData", &xmPushButtonGadgetClass, 'N', NULL, NULL, ConfigDataCB, (XtPointer) 932, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "SetRange",&xmPushButtonGadgetClass, 'u', NULL, NULL, SetRangeCB, (XtPointer) 341, (MenuItem *) NULL },
    { "TooClose",&xmPushButtonGadgetClass, 'R', NULL, NULL, TooCloseCB, (XtPointer) 342, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "Files", &xmPushButtonGadgetClass, 'F', NULL, NULL, FileInfoCB, (XtPointer) 351, (MenuItem *) NULL },
    { "Limits", &xmPushButtonGadgetClass, 'm', NULL, NULL, LimitInfoCB, (XtPointer) 352, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem gridgen_menu[] = {
    { "OneFront", &xmPushButtonWidgetClass, 'F', NULL, NULL, GenOneFrontCB, (XtPointer) 401, (MenuItem *) NULL },
    { "Clusters", &xmPushButtonGadgetClass, 'C', NULL, NULL, GenClusterCB, (XtPointer) 402, (MenuItem *) NULL },
    { "Options",&xmPushButtonGadgetClass, 'O', NULL, NULL, GenOptionsCB, (XtPointer) 403, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "AllFronts", &xmPushButtonGadgetClass, 'A', NULL, NULL, GenAllFrontsCB, (XtPointer) 411, (MenuItem *) NULL },
    { "Options", &xmPushButtonGadgetClass, 'p', NULL, NULL, FrontOptionsCB, (XtPointer) 412, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "OverlayHex", &xmPushButtonGadgetClass, 'H', NULL, NULL, GenHexCB, (XtPointer) 421, (MenuItem *) NULL },
    { "OverlaySquares",&xmPushButtonGadgetClass, 'S', NULL, NULL, GenSquaresCB, (XtPointer) 423, (MenuItem *) NULL },
    { "OverlayMixed", &xmPushButtonGadgetClass, 'M', NULL, NULL, GenMixedCB, (XtPointer) 425, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "Triangulate", &xmPushButtonGadgetClass, 'T', NULL, NULL, TriangulateCB, (XtPointer) 431, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};


MenuItem nodeedit_menu[] = {
    { "DeleteNode", &xmPushButtonGadgetClass, 'D', NULL, NULL, DeleteNodeCB, (XtPointer) 502, (MenuItem *) NULL },
    { "MoveNode", &xmPushButtonGadgetClass, 'M', NULL, NULL, MoveNodeCB, (XtPointer) 503, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "AddBndNode", &xmPushButtonGadgetClass, 'A', NULL, NULL, AddBndNodeCB, (XtPointer) 501, (MenuItem *) NULL },
    { "Reverse", &xmPushButtonGadgetClass, 'R', NULL, NULL, ReverseBndCB, (XtPointer) 511, (MenuItem *) NULL },
    { "Join", &xmPushButtonGadgetClass, 'J', NULL, NULL, JoinBndCB, (XtPointer) 514, (MenuItem *) NULL },
    { "Split", &xmPushButtonGadgetClass, 'J', NULL, NULL, SplitBndCB, (XtPointer) 515, (MenuItem *) NULL },
    { "ReSample", &xmPushButtonGadgetClass, 's', NULL, NULL, ReSampleBndCB, (XtPointer) 512, (MenuItem *) NULL },
    { "Reselect", &xmPushButtonGadgetClass, 's', NULL, NULL, ReselectBndCB, (XtPointer) 512, (MenuItem *) NULL },
    { "AddBndLine",&xmPushButtonGadgetClass, 'L', NULL, NULL, AddBndLineCB, (XtPointer) 521, (MenuItem *) NULL },
    { "DeleteIsland",&xmPushButtonGadgetClass, 'I', NULL, NULL, DeleteIslCB, (XtPointer) 522, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "AddIntNode",&xmPushButtonGadgetClass, 'd', NULL, NULL, AddIntNodeCB, (XtPointer) 551, (MenuItem *) NULL },
    { "AddIntLine", &xmPushButtonGadgetClass, 'n', NULL, NULL, AddIntLineCB, (XtPointer) 557, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem gridedit_menu[] = {
    { "AddLine", &xmPushButtonGadgetClass, 'A', NULL, NULL, AddGridEdgeCB, (XtPointer) 601, (MenuItem *) NULL },
    { "DeleteLine", &xmPushButtonGadgetClass, 'D', NULL, NULL, DelGridEdgeCB, (XtPointer) 602, (MenuItem *) NULL },
    { "AddNode", &xmPushButtonGadgetClass, 'd', NULL, NULL, AddGridNodeCB, (XtPointer) 611, (MenuItem *) NULL },
    { "DeleteNode", &xmPushButtonGadgetClass, 'e', NULL, NULL, DelGridNodeCB, (XtPointer) 612, (MenuItem *) NULL },
    { "Move", &xmPushButtonGadgetClass, 'M', NULL, NULL, MoveGridNodeCB, (XtPointer) 613, (MenuItem *) NULL },
    { "Merge",&xmPushButtonGadgetClass, 'G', NULL, NULL, MergeGridNodeCB, (XtPointer) 621, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "CleaveNode",&xmPushButtonGadgetClass, 'C', NULL, NULL, CleaveGridNodeCB, (XtPointer) 631, (MenuItem *) NULL },
    { "Insert", &xmPushButtonGadgetClass, 'I', NULL, NULL, InsertGridEdgeCB, (XtPointer) 633, (MenuItem *) NULL },
    { "Exchange", &xmPushButtonGadgetClass, 'E', NULL, NULL, ExchangeGridEdgeCB, (XtPointer) 635, (MenuItem *) NULL },
    { "Dekite", &xmPushButtonGadgetClass, 'k', NULL, NULL, DekiteGridCB, (XtPointer) 637, (MenuItem *) NULL },
    { "Reshape",&xmPushButtonGadgetClass, 's', NULL, NULL, ReshapeGridCB, (XtPointer) 639, (MenuItem *) NULL },
    { "sep",&xmSeparatorGadgetClass, ' ', NULL, NULL, 0, (XtPointer) 0, (MenuItem *) NULL },
    { "GridToNodes",&xmPushButtonGadgetClass, 'T', NULL, NULL, ConvertGrid2NodesCB, (XtPointer) 651, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem definegroup_menu[] = {
    { "Create", &xmPushButtonGadgetClass, 'C', NULL, NULL, CreatePolyCB, (XtPointer) 701, (MenuItem *) NULL },
    { "Whole", &xmPushButtonGadgetClass, 'W', NULL, NULL, WholePolyCB, (XtPointer) 702, (MenuItem *) NULL },
    { "Cycle", &xmPushButtonGadgetClass, 'y', NULL, NULL, CyclePolyCB, (XtPointer) 711, (MenuItem *) NULL },
    { "Delete", &xmPushButtonGadgetClass, 'D', NULL, NULL, DeletePolyCB, (XtPointer) 713, (MenuItem *) NULL },
    { "Read",&xmPushButtonGadgetClass, 'R', NULL, NULL, ReadPolyCB, (XtPointer) 721, (MenuItem *) NULL },
    { "Write",&xmPushButtonGadgetClass, 'W', NULL, NULL, WritePolyCB, (XtPointer) 722, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem groupnode_menu[] = {
    { "ReSample", &xmPushButtonGadgetClass, 'B', NULL, NULL, PolyReSampleCB, (XtPointer) 905, (MenuItem *) NULL },
    { "DeleteBnd", &xmPushButtonGadgetClass, 'B', NULL, NULL, PolyDelBndCB, (XtPointer) 901, (MenuItem *) NULL },
    { "DeleteInt", &xmPushButtonGadgetClass, 'I', NULL, NULL, PolyDelIntCB, (XtPointer) 902, (MenuItem *) NULL },
    { "DeleteAll", &xmPushButtonGadgetClass, 'A', NULL, NULL, PolyDelAllCB, (XtPointer) 903, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem groupedit_menu[] = {
    { "NodeCode",&xmPushButtonGadgetClass, 'N', NULL, NULL, PolyNodeCodeCB, (XtPointer) 821, (MenuItem *) NULL },
    { "ElementCode", &xmPushButtonGadgetClass, 'E', NULL, NULL, PolyEleCodeCB, (XtPointer) 822, (MenuItem *) NULL },
    { "Dekite", &xmPushButtonGadgetClass, 'k', NULL, NULL, PolyDekiteCB, (XtPointer) 831, (MenuItem *) NULL },
    { "Reshape", &xmPushButtonGadgetClass, 'k', NULL, NULL, PolyReshapeCB, (XtPointer) 832, (MenuItem *) NULL },
    { "DeleteGrid", &xmPushButtonGadgetClass, 'l', NULL, NULL, PolyDelGridCB, (XtPointer) 833, (MenuItem *) NULL },
    { "SplitGrid",&xmPushButtonGadgetClass, 'S', NULL, NULL, PolySplitGridCB, (XtPointer) 835, (MenuItem *) NULL },
    { "RefineGrid", &xmPushButtonGadgetClass, 'R', NULL, NULL, PolyRefineGridCB, (XtPointer) 837, (MenuItem *) NULL },
    { "CutOffGrid", &xmPushButtonGadgetClass, 'u', NULL, NULL, PolyCutGridCB, (XtPointer) 839, (MenuItem *) NULL },
    { "SetDepth", &xmPushButtonGadgetClass, 'a', NULL, NULL, PolySetDepthCB, (XtPointer) 851, (MenuItem *) NULL },
    { "ReDepth", &xmPushButtonGadgetClass, 'p', NULL, NULL, PolyReDepthCB, (XtPointer) 853, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};

MenuItem help_menu[] = {
    { "TQGG Help", &xmPushButtonGadgetClass, 'R', NULL, NULL, HelpCB, (XtPointer) 990, (MenuItem *) NULL },
    { "About", &xmPushButtonGadgetClass, 'O', NULL, NULL, AboutCB, (XtPointer) 992, (MenuItem *) NULL },
    { NULL, NULL, ' ', NULL, NULL, NULL, NULL, NULL }
};


int main (int argc, char *argv[])
{
	Widget       main_w, menubar, help_w, tmp; /* drawing_a */
    /*Widget       messageok_dialog;*/
/*	XtAppContext app;*/
	XmString     xms, yes, no, cancel;
    Arg          args[5];
	int          n;

	// App defaults to using pixmap
    if (argc == 2 && strncmp(argv[1],"-nopm", 5) == 0) {
    	use_pixmap = 0;  // but user can request no pixmap on command-line
    } else {
    	use_pixmap = 1;
    }
    setUsePixmap(use_pixmap);
    
	XtSetLanguageProc (NULL, NULL, NULL);
	toplevel = XtVaOpenApplication (&app, "Demos", NULL, 0, &argc, argv, NULL,
					sessionShellWidgetClass, XmNwidth, 734, XmNheight, 750, NULL);

	/* Create a MainWindow widget that contains a DrawingArea in
	** its work window.
	*/
/*	n = 0;
	XtSetArg (args[n], XmNscrollingPolicy, XmAUTOMATIC); n++;
	main_w = XmCreateMainWindow (toplevel, "main_w", args, n);*/
    main_w = XmCreateMainWindow (toplevel, "main_w", NULL, 0);
    
    menubar = XmCreateMenuBar (main_w, "menubar", NULL, 0);
	BuildMenu (menubar, XmMENU_PULLDOWN, "File", 'F', False, file_menu);
    BuildMenu (menubar, XmMENU_PULLDOWN, "View", 'V', False, view_menu);
	BuildMenu (menubar, XmMENU_PULLDOWN, "Info", 'I', False, info_menu);
    BuildMenu (menubar, XmMENU_PULLDOWN, "GridGen", 'G', False, gridgen_menu);
    BuildMenu (menubar, XmMENU_PULLDOWN, "NodeEdit", 'N', False, nodeedit_menu);
    BuildMenu (menubar, XmMENU_PULLDOWN, "GridEdit", 'r', False, gridedit_menu);
    BuildMenu (menubar, XmMENU_PULLDOWN, "Polygons", 'P', False, definegroup_menu);
    BuildMenu (menubar, XmMENU_PULLDOWN, "NodeInPoly", 'C', False, groupnode_menu);
    BuildMenu (menubar, XmMENU_PULLDOWN, "GridInPoly", 'E', False, groupedit_menu);
    help_w = BuildMenu (menubar, XmMENU_PULLDOWN, "Help", 'H', False, help_menu);
    XtVaSetValues (menubar, XmNmenuHelpWidget, help_w, NULL);
    XtManageChild (menubar);
    
    /* build dialogs */

    //CreateDialogs (toplevel);
    
    n = 0;
    xms = XmStringCreateLocalized ("initializing ok");
    XtSetArg (args[n], XmNmessageString, xms); n++;
    messageok_dialog = XmCreateMessageDialog (menubar, "INFO", args, n);
    XmStringFree (xms);    
    answer = 0;
    XtAddCallback (messageok_dialog, XmNokCallback, yesnoCB, (XtPointer) &answer);
                /* Remove cancel,Help  button from ok question dialog. */
    tmp = XmMessageBoxGetChild(messageok_dialog,XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(tmp);
    tmp = XmMessageBoxGetChild(messageok_dialog,XmDIALOG_CANCEL_BUTTON);
    XtUnmanageChild(tmp);

/*    XtManageChild (messageok_dialog);*/


    n = 0;
    xms = XmStringCreateLocalized ("initializing yes/no");
    yes = XmStringCreateLocalized ("Yes");
    no = XmStringCreateLocalized ("No");
    XtSetArg (args[n], XmNmessageString, xms); n++;
    messageyesno_dialog = XmCreateMessageDialog (menubar, "OK", args, n);
    XtVaSetValues (messageyesno_dialog, XmNmessageString, xms, 
        XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
        XmNokLabelString, yes,
        XmNcancelLabelString, no, NULL);    
    XmStringFree (xms);    
    XmStringFree(yes);
    XmStringFree(no);
    answer = 0;
    XtAddCallback (messageyesno_dialog, XmNokCallback, yesnoCB, (XtPointer) &answer);
    XtAddCallback (messageyesno_dialog, XmNcancelCallback, yesnoCB, (XtPointer) &answer);
                /* Remove cancel,Help  button from yesno_question dialog. */
    tmp = XmMessageBoxGetChild(messageyesno_dialog, XmDIALOG_HELP_BUTTON);
    XtUnmanageChild(tmp);

/*    XtManageChild (messageyesno_dialog);*/

    n = 0;
    xms = XmStringCreateLocalized ("initializing yes/no/cancel");
    yes = XmStringCreateLocalized ("Yes");
    no = XmStringCreateLocalized ("No");
    cancel = XmStringCreateLocalized ("Cancel");
    XtSetArg (args[n], XmNmessageString, xms); n++;
    messageyesnocancel_dialog = XmCreateMessageDialog (menubar, "OK", args, n);
    XtVaSetValues (messageyesnocancel_dialog, XmNmessageString, xms, XmNokLabelString, yes,
        XmNcancelLabelString, no, XmNhelpLabelString, cancel, NULL);    
    XmStringFree (xms);    
    XmStringFree(yes);
    XmStringFree(no);
    XmStringFree(cancel);
    answer = 0;
    XtAddCallback (messageyesnocancel_dialog, XmNokCallback, yesnoCB, (XtPointer) &answer);
    XtAddCallback (messageyesnocancel_dialog, XmNcancelCallback, yesnoCB, (XtPointer) &answer);
    XtAddCallback (messageyesnocancel_dialog, XmNhelpCallback, yesnoCB, (XtPointer) &answer);


/*    XtManageChild (messageyesnocancel_dialog);*/
      
    /* Create a simple Motif 2.1 FileSelectionDialog */
    XtSetArg (args[0], XmNpathMode, XmPATH_MODE_RELATIVE);
    files_dialog = XmCreateFileSelectionDialog (toplevel, "filesb", args, 1);
    XtSetSensitive (XtNameToWidget (files_dialog, "Help"), False);
    XtAddCallback (files_dialog, XmNcancelCallback, filecancel_cb, &mycall_data);
    XtAddCallback (files_dialog, XmNokCallback, fileok_cb, &mycall_data);
/*    XtManageChild (files_dialog);*/

    /* Create a simple Motif 2.1 PromptDialog */
    
//    n = 0;
//    xms = XmStringCreateLocalized ("initializing prompt");
//    XtSetArg (args[n], XmNselectionLabelString, xms); n++;
//    XtSetArg (args[n], XmNautoUnmanage, False); n++;
//    prompt_dialog = XmCreatePromptDialog (toplevel, "prompt", args, n);
//    XmStringFree (xms);
//    XtSetSensitive (XtNameToWidget (prompt_dialog, "Help"), False);
//    XtAddCallback (prompt_dialog, XmNcancelCallback, promptcancel_cb, &mycall_data);
//    XtAddCallback (prompt_dialog, XmNokCallback, promptok_cb, &mycall_data);
    
    /*XtManageChild (prompt_dialog);*/
   
    /* Create a DrawingArea -- no actual drawing will be done. */
	MainCanvas = XmCreateDrawingArea (main_w, "MainCanvas", NULL, 0);
    XtAddCallback (MainCanvas, XmNinputCallback, drawing_area_callback, &mycall_data);
    XtVaGetValues (MainCanvas, XmNwidth, &width, XmNheight, &height, NULL);
 //   printf ("height,width= (%d,%d)\n", height, width);
    if (use_pixmap) {
    	XtAddCallback (MainCanvas, XmNexposeCallback, drawing_area_callback, &mycall_data);
    }

    gcv.foreground = BlackPixelOfScreen (XtScreen (MainCanvas));
    gc = XCreateGC (XtDisplay (MainCanvas), RootWindowOfScreen (XtScreen (MainCanvas)), GCForeground, &gcv);
    XtVaSetValues (MainCanvas, XmNuserData, gc, NULL);

    CreateDrawTools (MainCanvas);

    n = 0;
    xms = XmStringCreateLocalized ("initializing");
    XtSetArg (args[n], XmNlabelString, xms); n++;
    message_w = XmCreateLabel( main_w,"message", args, n);
    
    XtVaSetValues(main_w,
        XmNmessageWindow, message_w,
        XmNcommandWindowLocation, XmCOMMAND_BELOW_WORKSPACE,
        NULL);

    XtManageChild (message_w);
   

    XtManageChild (MainCanvas);


    XtManageChild (main_w);
	XtRealizeWidget (toplevel);
    
    GridGenW = XtNameToWidget(menubar,"GridGen");
    GridGen1W = XtNameToWidget(menubar,"OneFront");
    /* XtSetSensitive(GridGen1W, False);*/
    GridEditW = XtNameToWidget(menubar,"GridEdit");
    NodeEditW = XtNameToWidget(menubar,"NodeEdit");
    GroupNodeW = XtNameToWidget(menubar,"NodeInPoly");
    GroupEditW = XtNameToWidget(menubar,"GridInPoly");
    TopMenuW = menubar;

    /* initialize menuloop */
    /*int menuid0 = 0;
    MenuCB ( &menuid0 );*/
    Initialiser();
       
    XtAppMainLoop (app);
    return 0;
}

/* callback routine when the user selects OK in the FileSelection
** Dialog. Just print the file name selected.
**/
void fileok_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
    char *filename;
/*    int *answer2 = (int *) client_data;*/
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    XPCallback_data*   ptr2 = (XPCallback_data *) client_data;

    filename = (char *) XmStringUnparse (cbs->value, XmFONTLIST_DEFAULT_TAG, 
                          XmCHARSET_TEXT, XmCHARSET_TEXT, NULL, 0, XmOUTPUT_ALL);

    if (!filename)
        return;
        
    /* must have been an internal error */
    
    if (!*filename) {
        /* nothing typed? */
        printf("No file selected.");

        XtFree (filename);
        
        /* even "" is an allocated byte */
        return;
    }
    mycall_data.reason = XmCR_OK;
    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &ptr2->name);


/*    *answer2 = YES;*/
//    printf ("Filename given: \"%s\"\n", filename);
    XtFree (filename);
}

void filecancel_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
/*    int *answer2 = (int *) client_data;
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    XPCallback_data*   ptr2 = (XPCallback_data *) client_data;*/
 
    mycall_data.reason = XmCR_CANCEL;
   

}

void promptok_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
    XmFileSelectionBoxCallbackStruct *cbs = (XmFileSelectionBoxCallbackStruct *) call_data;
    XPCallback_data*   ptr2 = (XPCallback_data *) client_data;

    mycall_data.reason = XmCR_OK;
    XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &ptr2->answer);

} 
  
void promptcancel_cb (Widget widget, XtPointer client_data, XtPointer call_data)
{
 
    mycall_data.reason = XmCR_CANCEL;
  
}

/* Callback routine for DrawingArea's input callbacks and the
** PushButton's activate callback.  Determine which it is by
** testing the cbs->reason field.
*/

void MNU_MainMenuDisable ()
{
    XtSetSensitive(TopMenuW, False);
}

/* This function includes a kludge to redraw the MainCanvas - need a better way
 * to ensure that pixmap is copied to MainCanvas even when no expose event occurs.
 */
void MNU_MainMenuEnable ()
{
    //if (use_pixmap) redraw(); // included here in case there is no expose event causing canvas redraw
    XtSetSensitive(TopMenuW, True);
}

void MNU_GridAndNodeMenuDisable ()
{
    XtSetSensitive(GridEditW, False);
    XtSetSensitive(GroupEditW, False);
    XtSetSensitive(GridGenW, False);
    XtSetSensitive(NodeEditW, False);
    XtSetSensitive(GroupNodeW, False);
}


void MNU_GridMenuDisable ()
{
    XtSetSensitive(GridEditW, False);
    XtSetSensitive(GroupEditW, False);
    /*XtSetSensitive(XtNameToWidget(GridGenW,"OverlayHex" ), False);*/
}

void MNU_GridMenuEnable ()
{
    XtSetSensitive(GridEditW, True);
    XtSetSensitive(GridGenW, True);
}


void MNU_NodeMenuDisable ()
{
    XtSetSensitive(NodeEditW, False);
    XtSetSensitive(GroupNodeW, False);
}

void MNU_NodeMenuEnable ()
{
    XtSetSensitive(NodeEditW, True);
    XtSetSensitive(GridGenW, True);
}

void MNU_PolyMenuDisable ()
{
    XtSetSensitive(GroupEditW, False);
}

void MNU_PolyMenuEnable ()
{
    XtSetSensitive(GroupEditW, True);
}

void MNU_PolyNodeMenuDisable ()
{
    XtSetSensitive(GroupNodeW, False);
}

void MNU_PolyNodeMenuEnable ()
{
    XtSetSensitive(GroupNodeW, True);
}


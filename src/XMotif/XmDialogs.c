 
/*
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
*/

#include "XmMain.h"

// nodeinfo, element info, node check, element check dialog

/*----------------------------------------------------------------------------*/

// The following code implements the info/elementcheck menu item

int ntest;
int mode;

// callback used by WPigElementCheck when switching between full color and marker
void toggledModeCB (Widget widget, XtPointer client_data, XtPointer call_data)
{
	long which = (long) client_data;
	XmToggleButtonCallbackStruct *state = (XmToggleButtonCallbackStruct *) call_data;

	if (state->set == XmSET) {
		if (which == 0) {
			mode = 1;
		} else {
			mode = 0;
		}
	} else {
		mode = 0;
	}
}

// calback used by WPigElementCheck when switching between tests
void toggledTestCB (Widget widget, XtPointer client_data, XtPointer call_data)
{
	long which = (long) client_data;
	XmToggleButtonCallbackStruct *state = (XmToggleButtonCallbackStruct *) call_data;

	if (state->set == XmSET) {
		ntest = which + 1;
	 } else {
		ntest = 0;
	 }
}

// callback attached to CLOSE button of WPigElementCheck
void destroyCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	XtDestroyWidget (XtParent(w));
}

// callback attached to RUN CHECK button of WPigElementCheck
void checkCB(Widget w, XtPointer client_data, XtPointer call_data)
{
	ElementCheck(&ntest, &mode);
}

// Contains logic implementing info/ElementCheck dialog
void  WPigElementCheck(void)
{
	ntest = 1;
	mode = 1;
	Widget dialog, rc, label, label2, label3, radio_box, radio_box2, separator, separator2;
	Arg args0[6], args[6], args2[6], args3[6];
	XmString t, title_string, check, close, title, title2, title3, fullcolor, colormarker,
			eql, dep, a2d, ccw, g90, c0d, ext;

	int n = 0;
	t = XmStringCreateLocalized("the title");
	check = XmStringCreateLocalized("RUN CHECK");
	close = XmStringCreateLocalized("CLOSE");
	XtSetArg (args0[n], XmNselectionLabelString, t); n++;
	XtSetArg (args0[n], XmNautoUnmanage, False); n++;
	XtSetArg (args0[n], XmNuserData, 0); n++;
	XtSetArg (args0[n], XmNcancelLabelString, close); n++;
	XtSetArg (args0[n], XmNokLabelString, check); n++;
	// Dialog could attached to prompt_dialog or another Widget rather than toplevel.
	// Might also be able to customize XmCreateInformationDialog or another built-in menu
	// rather than XmCreatePromptDialog.
	dialog = XmCreatePromptDialog (toplevel, "ElementCheck", args0, n);
	XtAddCallback(dialog, XmNcancelCallback, destroyCB, NULL);
	XtAddCallback(dialog, XmNokCallback, checkCB, NULL);
	// Hide unwanted buttons
	XtUnmanageChild(XtNameToWidget(dialog, "Selection"));
	XtUnmanageChild(XtNameToWidget(dialog, "Text"));
	//XtUnmanageChild(XtNameToWidget(dialog, "OK"));
	XtUnmanageChild(XtNameToWidget(dialog, "Help"));
	XmStringFree(t);
	XmStringFree(check);
	XmStringFree(close);

	// set title of popup dialog box
    title_string = XmStringGenerate("ElementCheck", NULL, XmCHARSET_TEXT, "TAGOK");
    XtVaSetValues(dialog, XmNdialogTitle, title_string, NULL);
    XmStringFree(title_string);

	XtSetLanguageProc (NULL, NULL, NULL);

	fullcolor  = XmStringCreateLocalized ("Full Color");
	colormarker  = XmStringCreateLocalized ("Color Marker");
	eql  = XmStringCreateLocalized ("EQL");
	dep  = XmStringCreateLocalized ("DEP");
	a2d  = XmStringCreateLocalized ("A2D");
	ccw  = XmStringCreateLocalized ("CCW");
	g90  = XmStringCreateLocalized ("G90");
	c0d  = XmStringCreateLocalized ("Code");
    ext  = XmStringCreateLocalized ("Ext");

	// RowColumn manages labels, seperators and both radio boxes
	rc = XmCreateRowColumn(dialog, "rowcol", NULL, 0);

	title = XmStringCreateLocalized("Color Triangles by Criteria");
	n = 0;
	XtSetArg( args[n], XmNlabelString, title); n++;
	label = XmCreateLabel(rc, "label", args, n);
	XmStringFree (title);

	separator = XmCreateSeparatorGadget (rc, "sep", NULL, 0);

	title2 = XmStringCreateLocalized("Select Coloring Mode");
	n = 0;
	XtSetArg( args2[n], XmNlabelString, title2); n++;
	label2 = XmCreateLabel(rc, "label2", args2, n);
	XmStringFree (title2);

	radio_box = XmVaCreateSimpleRadioBox (rc,
		"radio_box",
		0,  // the inital choice
		toggledModeCB, // the callback routine
		XmVaRADIOBUTTON, fullcolor,  NULL, NULL, NULL,
		XmVaRADIOBUTTON, colormarker,  NULL, NULL, NULL,
		NULL);

	separator2 = XmCreateSeparatorGadget (rc, "sep2", NULL, 0);

	title3 = XmStringCreateLocalized("Select Criteria");
	n = 0;
	XtSetArg( args3[n], XmNlabelString, title3); n++;
	label3 = XmCreateLabel(rc, "label3", args3, n);
	XmStringFree (title3);

	radio_box2 = XmVaCreateSimpleRadioBox (rc,
		"radio_box2",
		0,  // the inital choice
		toggledTestCB, // the callback routine
		XmVaRADIOBUTTON, eql, NULL, NULL, NULL,
		XmVaRADIOBUTTON, dep,  NULL, NULL, NULL,
		XmVaRADIOBUTTON, a2d,  NULL, NULL, NULL,
		XmVaRADIOBUTTON, ccw, NULL, NULL, NULL,
		XmVaRADIOBUTTON, g90, NULL, NULL, NULL,
		XmVaRADIOBUTTON, c0d, NULL, NULL, NULL,
        XmVaRADIOBUTTON, ext, NULL, NULL, NULL,
		NULL);

	XmStringFree (fullcolor);
	XmStringFree (colormarker);
	XmStringFree (eql);
	XmStringFree (dep);
	XmStringFree (a2d);
	XmStringFree (ccw);
	XmStringFree (g90);
	XmStringFree (c0d);
    XmStringFree (ext);

	XtManageChild (label);
	XtManageChild (separator);
	XtManageChild (label2);
	XtManageChild (radio_box);
	XtManageChild (separator2);
	XtManageChild (label3);
	XtManageChild (radio_box2);
	XtManageChild (rc);
	XtManageChild (dialog);

}

/*----------------------------------------------------------------------------*/

// The following code implements the info/node check menu item

//int *theCriteria, *userNcount, *userNcount1, *userNcount2, *maxCrit;
int *theCriteria, *maxCrit;

unsigned long toggles_set = (unsigned long) 0 ; /* has the bits of which toggles are set */

char *strings[] = { "C0", "C1", "C2", "C3", "C4", "C5", "C6", "NC0",
	"DLT", "DGT", "DBTW", "NBGT", "NBLT", "NBE", "EXT", "C=?"};

/* callback for all ToggleButtons of WPigNodeCheck.*/
void toggledNodeCheckCB (Widget widget, XtPointer client_data, XtPointer call_data)
{
	int bit = (int) client_data;
	XmToggleButtonCallbackStruct *toggle_data = (XmToggleButtonCallbackStruct *) call_data;

    long which = (long) client_data;

	if (toggle_data->set == XmSET){ /* if the toggle button is set, flip its bit */
		toggles_set |= (1 << bit);
        ntest = which + 1;
        SetUserValue(&ntest);
    }else /* if the toggle is "off", turn off the bit. */
		toggles_set &= ~(1 << bit);
}

/* used by check_bits callback for WPigNodeCheck */
void setupOutput() {
	int i;
	for (i = 0; i < *maxCrit; i++) { //XtNumber (strings); i++) {
		if (toggles_set & (1<<i)) {
			theCriteria[i] = 1;
		} else {
			theCriteria[i] = 0;
		}
	}
}

/* callback for "Run Check" (aka OK) button used by WPigNodeCheck */
void check_bits (Widget widget, XtPointer client_data, XtPointer call_data)
{
/*	int parm1 = 0;
	int change = 1;*/

    setupOutput();
    ReDrawOnly();
/*	DrwFig(&parm1, &change);*/
}

//void WPigNodeCheck(int *ans, int *user_ncount, int *user_ncount1, int *user_ncount2,
void WPigNodeCheck(int *ans, int TheCriteria[], int *maxcrit)
{
	Widget       rowcol, toggle_box, w, t, check, close;
	int          i;
	Arg          args[4];

	theCriteria = TheCriteria;
//	userNcount = user_ncount;
//	userNcount1 = user_ncount1;
//	userNcount2 = user_ncount2;
	maxCrit = maxcrit;

	// hard-coded here so that this skeleton code replicates
	// functionality of older version
//    *userNcount = 8;
//    *userNcount1 = 4;
//    *userNcount2 = 4;

	XtSetLanguageProc (NULL, NULL, NULL);

	int n = 0;
	t = XmStringCreateLocalized("Numerical Input");
	check = XmStringCreateLocalized("RUN CHECK");
	close = XmStringCreateLocalized("CLOSE");
	XtSetArg (args[n], XmNselectionLabelString, t); n++;
	XtSetArg (args[n], XmNautoUnmanage, False); n++;
	XtSetArg (args[n], XmNuserData, 0); n++;
	XtSetArg (args[n], XmNcancelLabelString, close); n++;
	XtSetArg (args[n], XmNokLabelString, check); n++;
	rowcol = XmCreatePromptDialog (toplevel, "NodeCheck", args, n);
	XtAddCallback(rowcol, XmNcancelCallback, destroyCB, NULL);
	XtAddCallback(rowcol, XmNokCallback, check_bits, NULL);

	// hides Numeric Input box
	XtUnmanageChild(XtNameToWidget(rowcol, "Selection"));
	XtUnmanageChild(XtNameToWidget(rowcol, "Text"));

	i = 0;
	XtSetArg (args[i], XmNpacking, XmPACK_COLUMN); i++;
	XtSetArg (args[i], XmNnumColumns, 3); i++;
	toggle_box = XmCreateRowColumn (rowcol, "togglebox", args, i);

	/* simply loop thru the strings creating a widget for each one */
	for (i = 0; i < XtNumber (strings); i++) {
			w = XmCreateToggleButtonGadget (toggle_box, strings[i], NULL, 0);
			XtAddCallback (w, XmNvalueChangedCallback, toggledNodeCheckCB, (XtPointer) i);
			XtManageChild (w);
	}

	w = XmCreateSeparatorGadget (rowcol, "sep", NULL, 0);
	XtManageChild (w);

	XtManageChild (rowcol);
	XtManageChild (toggle_box);
}

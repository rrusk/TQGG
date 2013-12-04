 
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

#include "math.h"

#include "XmMain.h"

extern void getnodeinfo_(int *, double *, double *, double *, int *, int *, int (*)[] );
extern void setnodeinfo_(int *, int *, double *);
extern void getelementinfo_(int *, double *, double *, double *, int *, int (*)[] );
extern void setelementinfo_(int *, int *);
extern void locateelement_(double *, double *, int *, int *);
extern void puttrimarker_(int *);
extern void putpermmarker_( double *, double *, int *);
extern void elementcheck_(int *num, int *mode);
extern void redrawonly_(void);
extern void setuservalue_(int *num, int *mode);


// nodeinfo, element info, node check, element check dialog

/*----------------------------------------------------------------------------*/

// Implements the Info/NodeInfo and ElementInfo menu items

    Widget nodeinfo_dialog;
    Widget node_index_widget;
    Widget node_x_widget;
    Widget node_y_widget;
    Widget node_z_widget;
    Widget node_code_widget;
    Widget elementinfo_dialog;
    Widget element_index_widget;
    Widget element_x_widget;
    Widget element_y_widget;
    Widget element_z_widget;
    Widget element_code_widget;
    
    int NodeInfoDialogExists=0;
    int ElementInfoDialogExists=0;
    int Lastnode = 0;
    int Lastelement = 0;

char *int_to_str(int integer, int max_chars) {
	/* Modified from
	 * http://stackoverflow.com/questions/8257714/how-to-convert-an-int-to-string-in-c
	 * to accomodate the value 0.  Does not handle negatives. */

//	int num_chars = 0;
//
//	int order = integer > 0 ? (int)(ceil(log10(integer))) : 1;
//	int num_chars = order + 1
//	printf("%f\n", log10(integer));
//	printf("%f\n", ceil(log10(integer)));
//	int num_chars = (int)((ceil(log10(integer)) + 1));

//	printf("Calculated number of characters: %d\n", num_chars);
	char *str = (char *)malloc(max_chars * sizeof(char));
//	puts("Allocated string");
	sprintf(str, "%d", integer);
//	puts("Did sprintf");

	return str;
}

char *real_to_str(double num, int max_chars) {

    char *str = (char *)malloc(max_chars * sizeof(char));
//    puts("Allocated real string");
    sprintf(str, "%f", num);
//    puts("Did f sprintf");

    return str;
}

Widget create_labeled_textfield(Widget *parent, char *label_string) {
	Widget form_widget;
	Widget label_widget;
	Widget text_widget;
	Arg args[8];
	int n = 0;

	XtSetArg(args[n], XmNfractionBase, 10); n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	form_widget = XmCreateForm(*parent, "form", args, n);

	n = 0;
	XtSetArg(args[n], XmNtopAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNrightPosition, 3); n++;
	XtSetArg(args[n], XmNalignment, XmALIGNMENT_END); n++;
	XtSetArg(args[n], XmNnavigationType, XmNONE); n++;
	label_widget = XmCreateLabelGadget(form_widget, label_string, args, n);
	XtManageChild(label_widget);

	n = 0;
	XtSetArg(args[n], XmNtraversalOn, True); n++;
	XtSetArg(args[n], XmNleftAttachment, XmATTACH_POSITION); n++;
	XtSetArg(args[n], XmNleftPosition, 4); n++;
	XtSetArg(args[n], XmNrightAttachment, XmATTACH_FORM); n++;
	XtSetArg(args[n], XmNnavigationType, XmTAB_GROUP); n++;
	text_widget = XmCreateTextField(form_widget, "text", args, n);
	XtManageChild(text_widget);

	XtManageChild(form_widget);

	return text_widget;
}

void UpdateNodeInfoDialog(int *index, double *x, double *y, double *z, int *code) {
    // Accept parameters for index, x, y, z, etc. and update them
    
    char *index_str;
    char *x_str;
    char *y_str;
    char *z_str;
    char *code_str;
    int max_chars;
    
//    printf("C code received:");
//    printf("index: %d\n", *index);
//    printf("x: %f\n", *x);
//    printf("y: %f\n", *y);
//    printf("z: %f\n", *z);
//    printf("code: %d\n", *code);
    
    // set strings here
    max_chars = 15;

    index_str = int_to_str(*index, max_chars);
//    puts("Created index string");
//    puts(index_str);
    XmTextFieldSetString(node_index_widget, index_str);
    XtFree(index_str);

    x_str = real_to_str(*x, max_chars);
    XmTextFieldSetString(node_x_widget, x_str);
    XtFree(x_str);

    y_str = real_to_str(*y, max_chars);
    XmTextFieldSetString(node_y_widget, y_str);
    XtFree(y_str);

    z_str = real_to_str(*z, max_chars);
    XmTextFieldSetString(node_z_widget, z_str);
    XtFree(z_str);

    code_str = int_to_str(*code, max_chars);
    XmTextFieldSetString(node_code_widget, code_str);
    XtFree(code_str);
    
    XtManageChild(nodeinfo_dialog);
}


void update_callback(Widget widget, XtPointer client_data, XtPointer call_data) {
//	puts("Update was pressed.");
    
    char *index_str;
//    char *x_str;
//    char *y_str;
    char *z_str;
    char *code_str;
    int index, code;
    double xn;
    double yn;
    double zn;
    int numngh;
    int nv[100];

    index_str = XmTextFieldGetString (node_index_widget);
    index = atoi(index_str);
    XtFree(index_str);
//    printf("index: %d\n", index);
    if(index == Lastnode) {
//      puts(" index=Lastnode");
      code_str = XmTextFieldGetString (node_code_widget);
      code = atoi(code_str);
      XtFree(code_str);
      z_str = XmTextFieldGetString (node_z_widget);
      zn = atof(z_str);
      XtFree(z_str);
//      printf("zn: %f\n", zn);
//      printf("ncode: %d\n", code);
      setnodeinfo_(&index, &code, &zn);
    }
    else{
      GetNodeInfo( &index,&xn,&yn,&zn,&code,&numngh,&nv );
//      printf("update callback:");
//      printf("index: %d\n", index);
//      printf("xn: %f\n", xn);
//      printf("yn: %f\n", yn);
//      printf("zn: %f\n", zn);
//      printf("ncode: %d\n", code);
//      printf("numngh: %d\n", numngh);
      UpdateNodeInfoDialog(&index,&xn,&yn,&zn,&code);
      Lastnode = index;
    }
}

void close_callback(Widget widget, XtPointer client_data, XtPointer call_data) {
	XtDestroyWidget(XtParent(widget));
    NodeInfoDialogExists=0;
}
  
void CreateNodeInfoDialog(int *index, double *x, double *y, double *z, int *code) {
	// Accept parameters for index, x, y, z, etc. and display them

//	printf("C code received:");
//	printf("index: %d\n", *index);
//	printf("x: %f\n", *x);
//	printf("y: %f\n", *y);
//	printf("z: %f\n", *z);
//	printf("code: %d\n", *code);

	Widget dialog;
	Widget row_column;
//	NodeInfoWidget* node_info_widget;

	XmString title;
	XmString update_label;
	XmString close_label;

    char *index_str;
    char *x_str;
    char *y_str;
    char *z_str;
    char *code_str;
    int max_chars=15;

	Arg args[6];

	int n = 0;

	title = XmStringCreateLocalized("the title");
	update_label = XmStringCreateLocalized("Update");
	close_label = XmStringCreateLocalized("Close");
	XtSetArg (args[n], XmNselectionLabelString, title); n++;
	XtSetArg (args[n], XmNautoUnmanage, False); n++;
	XtSetArg (args[n], XmNuserData, 0); n++;
	XtSetArg (args[n], XmNcancelLabelString, close_label); n++;
	XtSetArg (args[n], XmNokLabelString, update_label); n++;

	// more appropriate dialog (without these unneeded extra pieces)?
	dialog = XmCreatePromptDialog (toplevel, "NodeInfo", args, n);
	XtAddCallback(dialog, XmNokCallback, update_callback, NULL);
	XtAddCallback(dialog, XmNcancelCallback, close_callback, NULL);

	// Hide unwanted buttons
	XtUnmanageChild(XtNameToWidget(dialog, "Selection"));
	XtUnmanageChild(XtNameToWidget(dialog, "Text"));
	XtUnmanageChild(XtNameToWidget(dialog, "Help"));

	XmStringFree(title);
	XmStringFree(update_label);
	XmStringFree(close_label);

	row_column = XmCreateRowColumn(dialog, "MainRowCol", NULL, 0);

    node_index_widget = create_labeled_textfield(&row_column, "Index:");
    node_x_widget = create_labeled_textfield(&row_column, "X:");
    node_y_widget = create_labeled_textfield(&row_column, "Y:");
    node_z_widget = create_labeled_textfield(&row_column, "Z:");
    node_code_widget = create_labeled_textfield(&row_column, "Code:");

    index_str = int_to_str(*index, max_chars);
//    puts("Created index string");
    XmTextFieldSetString(node_index_widget, index_str);
    free(index_str);

    x_str = real_to_str(*x, max_chars);
    XmTextFieldSetString(node_x_widget, x_str);
    free(x_str);

    y_str = real_to_str(*y, max_chars);
    XmTextFieldSetString(node_y_widget, y_str);
    free(y_str);

    z_str = real_to_str(*z, max_chars);
    XmTextFieldSetString(node_z_widget, z_str);
    free(z_str);

    code_str = int_to_str(*code, max_chars);
    XmTextFieldSetString(node_code_widget, code_str);
    free(code_str);

	XtManageChild(row_column);
	XtManageChild(dialog);
    nodeinfo_dialog = dialog;
}

void WPigNodeInfo(int *index) {
    
//    int NodeInfoDialogExists;
    double xn;
    double yn;
    double zn;
    int index2;
    int ncode;
    int numngh;
    int nv[100];

//    printf("C code received:");
//    printf("index: %d\n", *index);

//    NodeInfoDialogExists=0;
    // if dialog doesn't exist, create it. Otherwise skip this.
    Lastnode = *index;
    if(NodeInfoDialogExists==0) {
    // get parameters for index, x, y, z, etc. and display them
        index2 = *index;
        GetNodeInfo( &index2,&xn,&yn,&zn,&ncode,&numngh,&nv );
//        printf("C code received2:");
//        printf("index: %d\n", *index);
//        printf("xn: %f\n", xn);
//        printf("yn: %f\n", yn);
//        printf("zn: %f\n", zn);
//        printf("ncode: %d\n", ncode);
//        printf("numngh: %d\n", numngh);
        CreateNodeInfoDialog(&index2,&xn,&yn,&zn,&ncode);
        NodeInfoDialogExists=1;
    }
    else{
      if(*index>0){
    // get parameters for index, x, y, z, etc. and update display
        index2 = *index;
        GetNodeInfo( &index2,&xn,&yn,&zn,&ncode,&numngh,&nv );
//        printf("C code received2:");
//        printf("index: %d\n", *index);
//        printf("xn: %f\n", xn);
//        printf("yn: %f\n", yn);
//        printf("zn: %f\n", zn);
//        printf("ncode: %d\n", ncode);
//        printf("numngh: %d\n", numngh);
        UpdateNodeInfoDialog(&index2,&xn,&yn,&zn,&ncode);
      }
    }
}

void UpdateElementInfoDialog(int *index, double *x, double *y, double *z, int *code) {
    
    char *index_str;
    char *x_str;
    char *y_str;
    char *z_str;
    char *code_str;
    int max_chars;
    
//    printf("C code received:");
//    printf("index: %d\n", *index);
//    printf("xc: %f\n", *x);
//    printf("yc: %f\n", *y);
//    printf("zc: %f\n", *z);
//    printf("ecode: %d\n", *code);
    // set strings here
    max_chars = 15;

    index_str = int_to_str(*index, max_chars);
//    puts("Created index string");
//    puts(index_str);
    XmTextFieldSetString(element_index_widget, index_str);
    free(index_str);

    x_str = real_to_str(*x, max_chars);
    XmTextFieldSetString(element_x_widget, x_str);
    free(x_str);

    y_str = real_to_str(*y, max_chars);
    XmTextFieldSetString(element_y_widget, y_str);
    free(y_str);

    z_str = real_to_str(*z, max_chars);
    XmTextFieldSetString(element_z_widget, z_str);
    free(z_str);

    code_str = int_to_str(*code, max_chars);
    XmTextFieldSetString(element_code_widget, code_str);
    free(code_str);
    
    XtManageChild(elementinfo_dialog);
}

void update_ele_callback(Widget widget, XtPointer client_data, XtPointer call_data) {
//    puts("Update was pressed.");
    
    char *index_str;
//    char *x_str;
//    char *y_str;
//    char *z_str;
    char *code_str;
    int index, code;
    double xn;
    double yn;
    double zn;
//    int numngh;
    int nv[4];

    index_str = XmTextFieldGetString (element_index_widget);
    index = atoi(index_str);
    XtFree(index_str);
    puttrimarker_(&index);
//    printf("index: %d\n", index);
    if(index == Lastelement) {
//      puts(" index=Lastnode");
      code_str = XmTextFieldGetString (element_code_widget);
      code = atoi(code_str);
      XtFree(code_str);
//      printf("ecode: %d\n", code);
      setelementinfo_(&index, &code);
    }
    else{
      GetElementInfo( &index,&xn,&yn,&zn,&code,&nv );
//      printf("update callback:");
//      printf("index: %d\n", index);
//      printf("xn: %f\n", xn);
//      printf("yn: %f\n", yn);
//      printf("zn: %f\n", zn);
//      printf("ncode: %d\n", code);
      UpdateElementInfoDialog(&index,&xn,&yn,&zn,&code);
      Lastelement = index;
    }
}


void locate_ele_callback(Widget widget, XtPointer client_data, XtPointer call_data) {

//    char *index_str;
    char *x_str;
    char *y_str;
//    char *z_str;
//    char *code_str;
    int index, code, ierr, nv[4];
    double xn;
    double yn;
    double zn;

    x_str = XmTextFieldGetString (element_x_widget);
    xn = atof(x_str);
    XtFree(x_str);
    y_str = XmTextFieldGetString (element_y_widget);
    yn = atof(y_str);
    XtFree(y_str);
    putpermmarker_( &xn, &yn, &ierr );
    locateelement_(&xn, &yn, &index, &ierr);

    if(ierr==0){
      printf("locate:");
      printf("index: %d\n", index);
      printf("x: %f\n", xn);
      printf("y: %f\n", yn);
      puttrimarker_(&index);
      GetElementInfo( &index,&xn,&yn,&zn,&code,&nv );
      UpdateElementInfoDialog(&index,&xn,&yn,&zn,&code);
      Lastelement = index;

    }
}
    
void close_ele_callback(Widget widget, XtPointer client_data, XtPointer call_data) {
    XtDestroyWidget(XtParent(widget));
    ElementInfoDialogExists=0;
}
    
void CreateElementInfoDialog(int *index, double *x, double *y, double *z, int *code) {
    // Accept parameters for index, x, y, z, etc. and display them

//    printf("C code received:");
//    printf("index: %d\n", *index);
//    printf("x: %f\n", *x);
//    printf("y: %f\n", *y);
//    printf("z: %f\n", *z);
//    printf("code: %d\n", *code);

    Widget dialog;
    Widget row_column;

    XmString title;
    XmString update_label;
    XmString close_label;
    XmString locate_label;

    char *index_str;
    char *x_str;
    char *y_str;
    char *z_str;
    char *code_str;
    int max_chars=15;

    Arg args[6];

    int n = 0;

    title = XmStringCreateLocalized("the title");
    update_label = XmStringCreateLocalized("Update");
    close_label = XmStringCreateLocalized("Close");
    locate_label = XmStringCreateLocalized("Locate");
    XtSetArg (args[n], XmNselectionLabelString, title); n++;
    XtSetArg (args[n], XmNautoUnmanage, False); n++;
    XtSetArg (args[n], XmNuserData, 0); n++;
    XtSetArg (args[n], XmNcancelLabelString, close_label); n++;
    XtSetArg (args[n], XmNapplyLabelString, locate_label); n++;
    XtSetArg (args[n], XmNokLabelString, update_label); n++;

    // more appropriate dialog (without these unneeded extra pieces)?
    dialog = XmCreatePromptDialog (toplevel, "ElementInfo", args, n);
    XtAddCallback(dialog, XmNokCallback, update_ele_callback, NULL);
    XtAddCallback(dialog, XmNcancelCallback, close_ele_callback, NULL);
    XtAddCallback(dialog, XmNapplyCallback, locate_ele_callback, NULL);
    XtManageChild(XtNameToWidget(dialog, "Apply"));

    // Hide unwanted buttons
    XtUnmanageChild(XtNameToWidget(dialog, "Selection"));
    XtUnmanageChild(XtNameToWidget(dialog, "Text"));
    XtUnmanageChild(XtNameToWidget(dialog, "Help"));

    XmStringFree(title);
    XmStringFree(update_label);
    XmStringFree(close_label);
    XmStringFree(locate_label);

    row_column = XmCreateRowColumn(dialog, "MainRowCol", NULL, 0);

    element_index_widget = create_labeled_textfield(&row_column, "Index:");
    element_x_widget = create_labeled_textfield(&row_column, "X:");
    element_y_widget = create_labeled_textfield(&row_column, "Y:");
    element_z_widget = create_labeled_textfield(&row_column, "Z:");
    element_code_widget = create_labeled_textfield(&row_column, "Code:");

    index_str = int_to_str(*index, max_chars);
//    puts("Created index string");
    XmTextFieldSetString(element_index_widget, index_str);
    free(index_str);

    x_str = real_to_str(*x, max_chars);
    XmTextFieldSetString(element_x_widget, x_str);
    free(x_str);

    y_str = real_to_str(*y, max_chars);
    XmTextFieldSetString(element_y_widget, y_str);
    free(y_str);

    z_str = real_to_str(*z, max_chars);
    XmTextFieldSetString(element_z_widget, z_str);
    free(z_str);

    code_str = int_to_str(*code, max_chars);
    XmTextFieldSetString(element_code_widget, code_str);
    free(code_str);

    XtManageChild(row_column);
    XtManageChild(dialog);
    elementinfo_dialog = dialog;

}


void WPigElementInfo(int *index) {
    
//    int ElementInfoDialogExists;
    int index2;
    double xc;
    double yc;
    double zc;
    int ecode;
    int elist[4];
  
//    ElementInfoDialogExists=0;
    // if dialog doesn't exist, create it. Otherwise skip this.
    Lastelement = *index;
    if(ElementInfoDialogExists==0) {
    // get parameters for index, x, y, z, etc. and display them
        index2 = *index;
        GetElementInfo( &index2,&xc,&yc,&zc,&ecode,&elist );
//        printf("C code received2:");
//        printf("index: %d\n", *index);
//        printf("xc: %f\n", xc);
//        printf("yc: %f\n", yc);
//        printf("zc: %f\n", zc);
//        printf("ecode: %d\n", ecode);
        CreateElementInfoDialog(&index2,&xc,&yc,&zc,&ecode);
        ElementInfoDialogExists=1;
    }
    else{
      if(*index>0){
    // get parameters for index, x, y, z, etc. and update display
        index2 = *index;
        GetElementInfo( &index2,&xc,&yc,&zc,&ecode,&elist );
//        printf("C code received2:");
//        printf("index: %d\n", *index);
//        printf("xc: %f\n", xc);
//        printf("yc: %f\n", yc);
//        printf("zc: %f\n", zc);
//        printf("ecode: %d\n", ecode);
        UpdateElementInfoDialog(&index2,&xc,&yc,&zc,&ecode);
      }
    }
}

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

char *strings[] = { "C0", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9",
	 "NC0", "C=?","DLT", "DGT", "DBTW", "NBLT", "NBGT", "NBE", "EXT"};

/* callback for all ToggleButtons of WPigNodeCheck.*/
void toggledNodeCheckCB (Widget widget, XtPointer client_data, XtPointer call_data)
{
	int bit = (int) client_data;
	XmToggleButtonCallbackStruct *toggle_data = (XmToggleButtonCallbackStruct *) call_data;

    long which = (long) client_data;

	if (toggle_data->set == XmSET){ /* if the toggle button is set, flip its bit */
		toggles_set |= (1 << bit);
        ntest = which + 1;
        mode = 1;
        SetUserValue(&ntest,&mode);
    }else {/* if the toggle is "off", turn off the bit. */
		toggles_set &= ~(1 << bit);
        ntest = which + 1;
        mode = 0;
        SetUserValue(&ntest,&mode);}
}

/* used by check_bits callback for WPigNodeCheck */
//void setupOutput() {
//	int i;
//	for (i = 0; i < *maxCrit; i++) { //XtNumber (strings); i++) {
//		if (toggles_set & (1<<i)) {
//			theCriteria[i] = 1;
//		} else {
//			theCriteria[i] = 0;
//		}
//	}
//}

/* callback for "Run Check" (aka OK) button used by WPigNodeCheck */
void check_bits (Widget widget, XtPointer client_data, XtPointer call_data)
{
/*	int parm1 = 0;
	int change = 1;*/

//    setupOutput();
    ReDrawOnly();
/*	DrwFig(&parm1, &change);*/
}

//void WPigNodeCheck(int *ans, int *user_ncount, int *user_ncount1, int *user_ncount2,
//void WPigNodeCheck(int *ans, int TheCriteria[], int *maxcrit)
void WPigNodeCheck(void)
{
	Widget       rowcol, toggle_box, w;
    XmString     tstr, check, close;
	int          i;
	Arg          args[4];

//	theCriteria = TheCriteria;
//	userNcount = user_ncount;
//	userNcount1 = user_ncount1;
//	userNcount2 = user_ncount2;
//	maxCrit = maxcrit;

	// hard-coded here so that this skeleton code replicates
	// functionality of older version
//    *userNcount = 8;
//    *userNcount1 = 4;
//    *userNcount2 = 4;

	XtSetLanguageProc (NULL, NULL, NULL);

	int n = 0;
	tstr = XmStringCreateLocalized("Numerical Input");
	check = XmStringCreateLocalized("RUN CHECK");
	close = XmStringCreateLocalized("CLOSE");
	XtSetArg (args[n], XmNselectionLabelString, tstr); n++;
	XtSetArg (args[n], XmNautoUnmanage, False); n++;
	XtSetArg (args[n], XmNuserData, 0); n++;
	XtSetArg (args[n], XmNcancelLabelString, close); n++;
	XtSetArg (args[n], XmNokLabelString, check); n++;
	rowcol = XmCreatePromptDialog (toplevel, "NodeCheck", args, n);
	XtAddCallback(rowcol, XmNcancelCallback, destroyCB, NULL);
	XtAddCallback(rowcol, XmNokCallback, check_bits, NULL);
    XmStringFree(tstr);
    XmStringFree(check);
    XmStringFree(close);

    // hides Numeric Input box
	XtUnmanageChild(XtNameToWidget(rowcol, "Selection"));
	XtUnmanageChild(XtNameToWidget(rowcol, "Text"));

	i = 0;
	XtSetArg (args[i], XmNpacking, XmPACK_COLUMN); i++;
	XtSetArg (args[i], XmNnumColumns, 4); i++;
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

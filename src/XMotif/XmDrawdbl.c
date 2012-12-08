 
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

#include <Xm/DrawingA.h>
#include <stdlib.h>
#include "pigdefs.h"

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
#define  MouseEHandler  mouseehandler_

Widget  MainCanvas;
Pixmap	pixmap;	/* used to redraw the drawing area */

XGCValues    gcv;
GC           gc;
Dimension    width, height;

static  int     XSymbolColour, XSymbolNumber;
static  int     XFillColour, XLineColour; //XForegrColour, XBackgrColour,XTextColour, XLineColour;
static  double WorldX1, WorldX2, WorldY1, WorldY2, WCentx, WCenty,WtoVScale; 
static  double X1, X2, Y1, Y2; 
    void  MouseEHandler(int *, int *, double *, double *);


String colors[] = {"Black", "Navy", "LimeGreen", "Turquoise", "Magenta", "Purple", "Orange",
           "Grey", "Brown", "Blue", "Green", "Cyan", "Red", "Violet", "Yellow",
           "White", "Pink", "Wheat"};

/*----------------------------------------------------------------------------*/

void set_color (Widget widget, XtPointer client_data, XtPointer call_data)
{
    String   color = (String) client_data;
    Display *dpy = XtDisplay (widget);
    Colormap cmap = DefaultColormapOfScreen (XtScreen (widget));
    XColor   col, unused;
    
    if (!XAllocNamedColor (dpy, cmap, color, &col, &unused)) {
        printf ( "Can't alloc %s", color);
        return;
    }
    
    XSetForeground (dpy, gc, col.pixel);
}

/*----------------------------------------------------------------------------*/

/* drawing setup- colors, lines, etc*/
void CreateDrawTools ( Widget parent )
{
    MainCanvas = parent;

    gcv.foreground = BlackPixelOfScreen (XtScreen (MainCanvas));
    gc = XCreateGC (XtDisplay (MainCanvas), RootWindowOfScreen (XtScreen (MainCanvas)), GCForeground, &gcv);
    XtVaSetValues (MainCanvas, XmNuserData, gc, NULL);
}

/*----------------------------------------------------------------------------*/

/* Create pixmap to store contents of drawing area */
void initPixmap() {
	/* create a pixmap the same size as the drawing area. */
	pixmap = XCreatePixmap (XtDisplay(MainCanvas), RootWindowOfScreen (XtScreen (MainCanvas)),
			width, height, DefaultDepthOfScreen (XtScreen(MainCanvas)));
	/* clear pixmap with white */
	XFillRectangle (XtDisplay (MainCanvas), pixmap, gc, 0, 0, width, height);
}

/*----------------------------------------------------------------------------*/

/* Copy pixmap to drawing area */
void redraw()
{
	XCopyArea (XtDisplay(MainCanvas), pixmap, XtWindow(MainCanvas), gc,
			0, 0, width, height, 0, 0);
}

/*----------------------------------------------------------------------------*/

/* Callback routine for DrawingArea's input callbacks and the
** PushButton's activate callback.  Determine which it is by
** testing the cbs->reason field.
*/
/*----------------------------------------------------------------------------*/

void drawing_area_callback (Widget widget, XtPointer client_data, XtPointer call_data)
{
    static Position x, y;
    double Mousex, Mousey;
    int  MAINWIN = 1;
    int  BDOWN = 1;
    int  BUP = 2;
    XmDrawingAreaCallbackStruct *cbs = (XmDrawingAreaCallbackStruct *) call_data;
    XEvent *event = cbs->event;
    //printf ("drawing cb: (%s)\n", "top");
    
    if (cbs->reason == XmCR_INPUT) {
        /* activated by DrawingArea input event -- draw lines.
        ** Button Down events anchor the initial point and Button
        ** Up draws from the anchor point to the button-up point.
        */
        if (event->xany.type == ButtonPress) {
            /* anchor initial point (i.e., save its value) */
            x = event->xbutton.x;
            y = event->xbutton.y;
            //printf ("drawing cb: point 1 (%d,%d)\n", x,y);
            Mousex = (WCentx - (0.5*width-x)/WtoVScale);
            Mousey = (WCenty + (0.5*height-y)/WtoVScale);
            MouseEHandler( &MAINWIN, &BDOWN, &Mousex, &Mousey);
       }
        else if (event->xany.type == ButtonRelease) {
            /* draw full line; get GC and use in XDrawLine() */
            GC gc;

            XtVaGetValues (widget, XmNuserData, &gc, NULL);
            XDrawLine (event->xany.display, cbs->window, gc, x, y, 
                                   event->xbutton.x, event->xbutton.y);
            /* draw into the pixmap as well for redrawing later */
            XDrawLine (event->xany.display, pixmap, gc, x, y,
            		event->xbutton.x, event->xbutton.y);
            x = event->xbutton.x;
            y = event->xbutton.y;
            //printf ("drawing cb: point 2 (%d,%d)\n", x,y);
            Mousex = (WCentx - (0.5*width-x)/WtoVScale);
            Mousey = (WCenty + (0.5*height-y)/WtoVScale);
            MouseEHandler( &MAINWIN, &BUP, &Mousex, &Mousey);
       }
    }
    if (cbs->reason == XmCR_EXPOSE) {
    	GC gc;
    	/* get the DrawingArea widget */
    	XtVaGetValues (widget, XmNuserData, &gc, NULL);
    	XCopyArea (event->xany.display, pixmap, event->xany.window,
    			gc, 0, 0, width, height, 0, 0);
    }
    if (cbs->reason == XmCR_ACTIVATE) {
        /* activated by pushbutton -- clear parent's window */
        XClearWindow (event->xany.display, XtWindow (XtParent (widget)));
    }
}

/*----------------------------------------------------------------------------*/

void  WPigEraseMain(void)
{

    XtVaGetValues( MainCanvas, 
        XtNheight, &height, 
        XtNwidth, &width, 
        NULL);
    /*printf ("erasemain height,width= (%d,%d)\n", height, width);*/

    XSetForeground (XtDisplay (MainCanvas), gc,BlackPixelOfScreen (XtScreen (MainCanvas)));

    XFillRectangle (XtDisplay(MainCanvas), XtWindow(MainCanvas), gc, 0, 0, width, height );
    XFillRectangle (XtDisplay(MainCanvas), pixmap, gc, 0, 0, width, height );

    XSetForeground (XtDisplay (MainCanvas), gc,WhitePixelOfScreen (XtScreen (MainCanvas)));

}  

/*----------------------------------------------------------------------------*/

void  WPigSetWorldCoordinates(const double *xlow, const double *xhigh, const double *ylow, const double *yhigh)
{
    XtVaGetValues( MainCanvas, 
        XtNheight, &height, 
        XtNwidth, &width, 
        NULL);
    printf ("set val height,width= (%d,%d)\n", height, width);

    WorldX1 = (double) *xlow;
    WorldX2 = (double) *xhigh;
    WorldY1 = (double) *ylow;
    WorldY2 = (double) *yhigh;

    WCentx = width/(WorldX2 - WorldX1);
    WCenty = height/(WorldY2 - WorldY1);
    WtoVScale = 0.9*WCentx;
    if(WCenty < WCentx) {
      WtoVScale = 0.9*WCenty;
    }
    WCentx = 0.5*(WorldX2 + WorldX1);
    WCenty = 0.5*(WorldY2 + WorldY1);

    // Initializing pixmap here because dimensions of the pixmap are known here!!!
    initPixmap();
}

/*----------------------------------------------------------------------------*/

void  WPigGetWorldCoordinates( double *xlow, double *xhigh, double *ylow, double *yhigh)
{

    X1 = (WCentx - 0.5*width/WtoVScale);
    X2 = (WCentx + 0.5*width/WtoVScale);
    Y1 = (WCenty - 0.5*height/WtoVScale);
    Y2 = (WCenty + 0.5*height/WtoVScale);
    *xlow  =  X1;
    *xhigh =  X2;
    *ylow  =  Y1;
    *yhigh =  Y2;

    return;
}

/*----------------------------------------------------------------------------*/

void  WPigSetLineColour (const int *NewColour)
{
    XLineColour = (int) *NewColour;
    /*printf ("linecolour= (%d)\n", XLineColour);*/
    set_color (MainCanvas, colors[XLineColour], NULL); /* set the gc's color according to name */
    /*XSetForeground (XtDisplay (MainCanvas), gc,BlackPixelOfScreen (XtScreen (MainCanvas)));*/
}

/*----------------------------------------------------------------------------*/

void  WPigSetSymbolColour (const int *NewColour)
{
    XSymbolColour = (int) *NewColour;
    /*printf ("symbolcolour= (%d)\n", XSymbolColour);*/
    set_color (MainCanvas, colors[XSymbolColour], NULL); /* set the gc's color according to name */
    /*XSetForeground (XtDisplay (MainCanvas), gc,BlackPixelOfScreen (XtScreen (MainCanvas)));*/
}

/*----------------------------------------------------------------------------*/

void  WPigSetSymbolNumber (const int *NewNumber)
{
    XSymbolNumber = (int) labs(*NewNumber);
}

/*----------------------------------------------------------------------------*/

void  WPigSetFillColour (const int *NewColour)
{
    XFillColour = (int) *NewColour;
    /*printf ("fillcolour= (%d)\n", XFillColour);*/
    set_color (MainCanvas, colors[XFillColour], NULL);
    /*XSetForeground (XtDisplay (MainCanvas), gc,WhitePixelOfScreen (XtScreen (MainCanvas)));*/
}

/*----------------------------------------------------------------------------*/

void  WPigDrawPolyLine(const int *NPoints, const double *xarray, const double *yarray)
{
/*printf("void  WPigDrawPolyLine(Long *NPoints, float *xarray, float *yarray)(NPoints = %li)\n",
        (long) *NPoints);*/

/*XSetForeground (XtDisplay (MainCanvas), gc,BlackPixelOfScreen (XtScreen (MainCanvas)));*/

#define MAXFASTPOINTS 5
    if(*NPoints <= MAXFASTPOINTS) {
        int NumPoints;
        static  XPoint  points[MAXFASTPOINTS];

        for (NumPoints = 0; NumPoints < (int) *NPoints; NumPoints++) {

            points[NumPoints].x = 0.5*width + (xarray[NumPoints]-WCentx)*WtoVScale;
            points[NumPoints].y = 0.5*height - (yarray[NumPoints]-WCenty)*WtoVScale;
        }
        //XDrawLines(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
        //    points, (int)*NPoints, CoordModeOrigin);
        XDrawLines(XtDisplay(MainCanvas), pixmap, gc, points, (int)*NPoints,
        		CoordModeOrigin);
#undef MAXFASTPOINTS
    } else {
/* #define MAXFASTPOINTS 8192 */
#define MAXFASTPOINTS 512
/* #define MAXFASTPOINTS (*NPoints+1) */
        int NumPoints, n;
        int j;
        static  XPoint  points[MAXFASTPOINTS];
//        short   x, y;
 //       double  xd, yd;

        for (j= 0; j < *NPoints; j += MAXFASTPOINTS - 1) {
            n = ((*NPoints - j) < MAXFASTPOINTS) ? (*NPoints - j) : MAXFASTPOINTS;

            for (NumPoints = 0; NumPoints < n; NumPoints++) {

            points[NumPoints].x = 0.5*width + (xarray[j+NumPoints]-WCentx)*WtoVScale;
            points[NumPoints].y = 0.5*height - (yarray[j+NumPoints]-WCenty)*WtoVScale;

            }

            //XDrawLines(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
            //    points, n, CoordModeOrigin);
            XDrawLines(XtDisplay(MainCanvas), pixmap, gc,
                points, n, CoordModeOrigin);
    
        }
        /* pause(); */
#undef MAXFASTPOINTS
    }
/*    XSetForeground(XtDisplay(MainCanvas), gc, CmsColourDefs[XForegrColour]);*/
}

/*----------------------------------------------------------------------------*/

void  WPigDrawSymbols (const int *NPoints, const double *xarray, const double *yarray)
{
    short  x, y, xysize;
    int NumPoints;

    xysize = 0.003*(height+width);
    for (NumPoints = 0; NumPoints < (int) *NPoints; NumPoints++) {

      x = 0.5*width + (xarray[NumPoints]-WCentx)*WtoVScale;
      y = 0.5*height - (yarray[NumPoints]-WCenty)*WtoVScale;
    
      switch(XSymbolNumber){

        case WPCROSS:
        {
            /* load up 'segments' with 'Cross' data for XDrawSegments */
            int     Num = 2;
            XSegment    segments[2];


            segments[0].x1 = (short) x - xysize;
            segments[0].y1 = (short) y;
            segments[0].x2 = (short) x + xysize;
            segments[0].y2 = (short) y;
            segments[1].x1 = (short) x;
            segments[1].y1 = (short) y - xysize;
            segments[1].x2 = (short) x;
            segments[1].y2 = (short) y + xysize;
            XDrawSegments(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc, segments, Num);
            XDrawSegments(XtDisplay(MainCanvas), pixmap, gc, segments, Num);
            break;
        }
        case WPSPLAT:
        {
            /* load up 'segments' with 'Splat' data for XDrawSegments */
            int     Num = 4;
            XSegment    segments[4];

            segments[0].x1 = (short) x;
            segments[0].y1 = (short) y - xysize;
            segments[0].x2 = (short) x;
            segments[0].y2 = (short) y + xysize ;
            segments[1].x1 = (short) x + xysize;
            segments[1].y1 = (short) y + xysize;
            segments[1].x2 = (short) x - xysize;
            segments[1].y2 = (short) y - xysize;
            segments[2].x1 = (short) x - xysize;
            segments[2].y1 = (short) y + xysize;
            segments[2].x2 = (short) x + xysize;
            segments[2].y2 = (short) y - xysize;
            segments[3].x1 = (short) x - xysize;
            segments[3].y1 = (short) y;
            segments[3].x2 = (short) x + xysize;
            segments[3].y2 = (short) y;
            XDrawSegments(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
                    segments, Num);
            XDrawSegments(XtDisplay(MainCanvas), pixmap, gc,
                    segments, Num);
            break;
        }
        case WPSQUARE:
        {
            /* load up 'points' with 'Square' data for XDrawLines */
            int     Num = 5;
            XPoint      points[20];

            points[0].x = (short) x -  xysize;
            points[0].y = (short) y -  xysize;
            points[1].x = (short) x -  xysize;
            points[1].y = (short) y +  xysize;
            points[2].x = (short) x +  xysize;
            points[2].y = (short) y +  xysize;
            points[3].x = (short) x +  xysize;
            points[3].y = (short) y -  xysize;
            points[4].x = (short) x -  xysize;
            points[4].y = (short) y -  xysize;
            XDrawLines(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
                    points, Num, CoordModeOrigin);
            XDrawLines(XtDisplay(MainCanvas), pixmap, gc,
                    points, Num, CoordModeOrigin);

            break;
        }
        case WPEX:
        {
            /* load up 'segments' with 'Ex' data for XDrawSegments */
            int     Num = 2;
            XSegment    segments[2];

            segments[0].x1 = (short) x -  xysize;
            segments[0].y1 = (short) y -  xysize;
            segments[0].x2 = (short) x +  xysize;
            segments[0].y2 = (short) y +  xysize;
            segments[1].x1 = (short) x +  xysize;
            segments[1].y1 = (short) y -  xysize;
            segments[1].x2 = (short) x -  xysize;
            segments[1].y2 = (short) y +  xysize;
            XDrawSegments(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
                    segments, Num);
            XDrawSegments(XtDisplay(MainCanvas), pixmap, gc,
                    segments, Num);

            break;
        }
        case WPDIAMOND:
        {
            /* load up 'points' with 'Diamond' data for XDrawLines */
            int     Num = 5;
            XPoint      points[20];

            points[0].x = (short) (x -  xysize);
            points[0].y = (short) (y);
            points[1].x = (short) (x);
            points[1].y = (short) (y -  xysize);
            points[2].x = (short) (x +  xysize);
            points[2].y = (short) (y);
            points[3].x = (short) (x);
            points[3].y = (short) (y +  xysize);
            points[4].x = (short) (x -  xysize);
            points[4].y = (short) (y);
            XDrawLines(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
                    points, Num, CoordModeOrigin);
            XDrawLines(XtDisplay(MainCanvas), pixmap, gc,
                    points, Num, CoordModeOrigin);
            break;
        }
        case WPPOINT:
        {
            /* load up 'points' with 'Point' data for XDrawLines */
            int     Num = 5;
            XPoint      points[20];

            points[0].x = (short) x +  xysize;
            points[0].y = (short) y +  xysize;
            points[1].x = (short) x +  xysize;
            points[1].y = (short) y -  xysize;
            points[2].x = (short) x -  xysize;
            points[2].y = (short) y -  xysize;
            points[3].x = (short) x -  xysize;
            points[3].y = (short) y +  xysize;
            points[4].x = (short) x +  xysize;
            points[4].y = (short) y +  xysize;
            XFillPolygon(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
                    points, Num, Complex, CoordModeOrigin);
            XFillPolygon(XtDisplay(MainCanvas), pixmap, gc,
                    points, Num, Complex, CoordModeOrigin);
            break;
        }
        case WPCIRCLE:
        {
            /* already have necessary info to draw a circle */
            XDrawArc(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
                    (int) x -  xysize,
                    (int) y -  xysize,
                    (int) 2 *  xysize,
                    (int) 2 *  xysize,
                    0, 360 * 64);
            XDrawArc(XtDisplay(MainCanvas), pixmap, gc,
                    (int) x -  xysize,
                    (int) y -  xysize,
                    (int) 2 *  xysize,
                    (int) 2 *  xysize,
                    0, 360 * 64);
            break;
        }
      }
    }
    return;
}
/*----------------------------------------------------------------------------*/

void  WPigDrawFilledPolygon(const int *NPoints, const double *xarray, const double *yarray)
{
    int NumPoints;
    XPoint  points[20];

    for (NumPoints = 0; NumPoints < (int) *NPoints; NumPoints++) {

        points[NumPoints].x =  0.5*width + (xarray[NumPoints]-WCentx)*WtoVScale;
        points[NumPoints].y =  0.5*height - (yarray[NumPoints]-WCenty)*WtoVScale;

    }
       /* XSetForeground(XtDisplay(MainCanvas), gc, CmsColourDefs[XFillColour]);*/

        XFillPolygon(XtDisplay(MainCanvas), XtWindow(MainCanvas), gc,
            points, *NPoints, Complex, CoordModeOrigin);
        XFillPolygon(XtDisplay(MainCanvas), pixmap, gc,
             points, *NPoints, Complex, CoordModeOrigin);

      /*  XSetForeground(XtDisplay(MainCanvas), gc, CmsColourDefs[XForegrColour]);*/

}


/*----------------------------------------------------------------------------*/


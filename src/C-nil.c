
/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/nil.c - M_DRAW driver for null output device"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, May 2011"

/* 
 ===============================================================================
 near-null driver. Updates current position, sets drawing surface size
 ===============================================================================
*/

#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>
#include "draw.h"

#define MIN(x, y)  ((x) < (y) ? (x) : (y))

#define FALSE 0
#define TRUE  1

/* total drawing area size in x direction */
#define NIL_XSIZE 8500  
/* total drawing area size in y direction */
#define NIL_YSIZE 11000 

static int      NIL_lastx = -1, NIL_lasty = -1;/* last (x, y) drawn */
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */
static int NIL_mapcolor(int i, int r, int g, int b) {
   return(0);
}
/******************************************************************************/
/* NIL_RESIZE  for initialization or a page clear, see if device is resized   */
static int NIL_RESIZE(void) {
   int prefx, prefy, prefxs, prefys;
   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

   if (prefxs != -1 ) {
      vdevice.sizeSy = prefys;
      vdevice.sizeSx = prefxs;
      vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
   } else{
      vdevice.sizeSy = NIL_YSIZE;  /* size in resolution rasters */
      vdevice.sizeSx = NIL_XSIZE;  /* size in resolution rasters */
   }
}
/******************************************************************************/
/* NIL_init set up the environment. Returns 1 on success. */
static int NIL_init(void) {
   NIL_RESIZE();

   vdevice.depth = 8; /* set number of colors available */
   vdevice.sizeX = vdevice.sizeY = MIN(NIL_XSIZE,NIL_YSIZE); /* current viewport to use */

   return (1);
}
/******************************************************************************/
/* NIL_exit do a flush and close the output file if necessary.  */
static int NIL_exit(void) {
   return (0);
}
/******************************************************************************/
/* NIL_draw draw to an x, y point.  */
static int NIL_draw(int x, int y) {
   NIL_lastx=x;
   NIL_lasty=y;
   return (0);
}
/******************************************************************************/
/* NIL_clear flush the current page without resetting the graphics state */
static int NIL_clear(void) {
   NIL_RESIZE();
   return(0);
}
/******************************************************************************/
/* NIL_color change the color of the pen */
static int NIL_color(int col) {
   return(0);
}
/******************************************************************************/
/* value sets raster line width */
static int NIL_setlw(int width) {
   return(0);
}
/******************************************************************************/
/* NIL_font load in small or large - could be improved.  */
static int NIL_font(char *font) {

   if (strcmp(font, "small") == 0) {
      vdevice.hwidth = 12.0/72.0/.03937;
      vdevice.hheight = vdevice.hwidth * 2.2;
   } else if (strcmp(font, "large") == 0) {
      vdevice.hheight = 55.0/72.0/.03937;
      vdevice.hwidth = vdevice.hheight / 4.5;
   } else
      return(0);
   return(1);
}
/******************************************************************************/
/* NIL_string output a string.  */
static int NIL_string(char *s) {
   NIL_lastx = NIL_lasty = -1;
   return(0);
}
/******************************************************************************/
/* NIL_char output a character */
static int NIL_char(char c) {
	 NIL_lastx = NIL_lasty = -1;
   return(0);
}
/******************************************************************************/
/* fill a polygon */
static int NIL_fill(int n, int x[], int y[]) {
   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];
   NIL_lastx = NIL_lasty = -1;           /* fill destroys current path */
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry nildev = {
"nil",          /*  name of device */
"large",        /*  name of large font */
"small",        /*  name of small font */
noop,           /*  Set  drawing in back buffer */
NIL_char,       /*  Draw a hardware character */
noop,           /*  Check if a key was hit */
NIL_clear,      /*  Clear the screen to current color */
NIL_color,      /*  Set current color */
NIL_draw,       /*  Draw a line */
NIL_exit,       /*  Exit graphics */
NIL_fill,       /*  Fill a polygon */
NIL_font,       /*  Set hardware font */
noop,           /*  Set drawing in front buffer */
noop,           /*  Wait for and get the next key hit */
NIL_init,       /*  Initialize the device */
noop2,          /*  Get mouse/cross hair position */
NIL_mapcolor,   /*  Set color indices */
NIL_setlw,      /*  Set line width */
NIL_string,     /*  Draw a hardware string */
noop,           /*  Swap front and back buffers */
noop            /*  Syncronize the display */
};
/******************************************************************************/
/* copy the nil device into vdevice.dev.  */
int _NIL_draw_devcpy(void) {
   vdevice.dev = nildev;
   return(0);
}
/******************************************************************************/

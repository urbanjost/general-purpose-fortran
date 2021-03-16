/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/canvas.c - M_DRAW driver for HTML 5.0 canvas element"
#ident "@(#)M_DRAW:author:John S. Urban"
#ident "@(#)M_DRAW:version:1.0, Mar 2007"
/*
 ===============================================================================
X.lineCap='round';
X.lineJoin='round';

X.fillStyle="#000000";
X.strokeStyle='black';

X.lineWidth=8;


 Usage Notes:

  o  assuming 10 units to a raster raster ; so a call for size 4000,4000 would create
     a 400x400 pixel display surface. Default is 425x550 pixels.

  o  currently, only Apple, Mozilla/Firefox and Opera support a CANVAS -- this is a
     preliminary specification.

  o  Use negative color values to specify line thickness in raster units in
     this version of draw

  o  Note line weight is always specified as scaled to local coordinate system size.
     This is good, as it allows thickness to simply scale with a rescaled plot.

  Last Time:

  o  Changed scale(1) call so that changing the width and height values
     causes the frame to stretch to fit the new area

  o  A line of zero length does not always print as a point even
     when line terminators on; drawing a circle when a zero length line
     is encountered.

  o  making a function that takes an array and draws a polyline to make
     files less verbose ( and collect line segments into polylines).

     and made the canvas name a parameter to the drawCanvasNNN() function
     so that it is easy to use the same plot more than once. That is, 
     change
            drawCanvas1("canvas_id10");
              to
            drawCanvas1("canvas_id1");
            drawCanvas1x("canvas_id10");

              and
            <canvas id="canvas_id1" width="125" height="250"></canvas>
              to
            <canvas id="canvas_id1" width="125" height="250"></canvas>
            <canvas id="canvas_id10" width="250" height="500"></canvas>

     and the same drawing will show up twice at two sizes.

  o  Got fancy with adding resize buttons and such. Works with opera(1) and
     firefox(1) browsers, but might have to back it out or dress it up
     relatively soon.

  Next Time:

  o  Should be able to make an animated movie like an mpeg

  o  extend functions so they call VML or SVG instead of CANVAS, or print
     out values or convert to a PostScript or Adobe PDF file by writing to 
     document

  o  should default plot size be width of display?

  o  Consider making JavaScript versions of the Hershey fonts so functions
     can be made that print text so text strings are editable.
 ===============================================================================
     multiply X.lineWidth*FACTOR to keep line thickness constant
     then change lines that say "X.lineWidth=" to "X.lineWidth=FACTOR*"
 ===============================================================================
*/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#ifndef MINGW
#include <sys/utsname.h>
#include <pwd.h>
#endif

#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include "draw.h"

extern FILE     *_draw_outfile();
extern FILE     *draw_fp;

/* How to convert degrees to radians */
#ifndef PI
#define PI      3.14159265358979323846264338327950288419716939937510
#endif

#define d2r(x)     ((x) * PI / 180.0)
#define r2d(x)     ((x) * 180.0 / PI)

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))

#define FLIPY(y)        ((int)((vdevice.sizeSy)-(y)))

#define FALSE 0
#define TRUE  1

/* Assume 1000 units per inch, and default size of 4.25inx5.5in*/
/* total drawing area size in x direction */
#define CANVASYSIZE 5500
/* total drawing area size in y direction */
#define CANVASXSIZE 4250
/* scale factor for going from world coordinates to device coordinates. Make bigger to increase accuracy of vector data */
#define CANVASTORAS 10

static int      points=0;
static int      canvas_first_time = 1, drawn = 0, LAST_X = -1, LAST_Y = -1;/* last (x, y) drawn */

static int CANVAS_MOVED=0;

#define CMAPSIZE 256
struct rgb_color {
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
};
static struct rgb_color canvas_carr[CMAPSIZE];

/******************************************************************************/
static int      PolyLineOpen = FALSE; /* PolyLine not open */
static int      ObjectOpen = FALSE; /* Object not open */
static int      curcol = 0; /* Current pen color (black) */
static int      curwid = 1; /* Current pen color width */
static int      curpat  = 0; /* Current fill pattern*/
static int      pgroup=1; /* groupid reserved for the entire page */
/******************************************************************************/
static void CANVAS_header() {

   time_t tod;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;

   time(&tod);
   fprintf(draw_fp, "<!DOCTYPE html>\n");
   fprintf(draw_fp, "<html>\n");
   fprintf(draw_fp, "<head>\n");
   fprintf(draw_fp, "<title>\n");
   /*
   fprintf(draw_fp, "  Creator: M_DRAW HTML CANVAS driver 1.0 2007-12-25\n");
   */
   fprintf(draw_fp, "  Creator: M_DRAW HTML CANVAS driver 1.2 2008-01-28\n");
   fprintf(draw_fp, "  Author: John S. Urban\n");
   fprintf(draw_fp, "  CreationDate: %s\n",ctime(&tod));
   fprintf(draw_fp, "</title>\n");

   fprintf(draw_fp, "<!--\n");

#ifndef MINGW
   un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
   uname(un);

   if ((username = getlogin()) == NULL ){
      pw = getpwuid(getuid());
      username = pw->pw_name;
   }
   fprintf(draw_fp, "  For: %s on OS=%.*s\n       NETWORK_NAME=%.*s\n       RELEASE=%.*s\n       VERSION=%.*s\n       MACHINE=%.*s\n",
       username,
       (int)sizeof(un->sysname),  un->sysname,
       (int)sizeof(un->nodename), un->nodename,
       (int)sizeof(un->release),  un->release,
       (int)sizeof(un->version),  un->version,
       (int)sizeof(un->machine),  un->machine);

   fprintf(draw_fp, "-->\n");
#endif

}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */
static int CANVAS_mapcolor(int i, int r, int g, int b) {
   if (i >= CMAPSIZE || i < 0 ){
      return(-1);
   }
   canvas_carr[i].red = (unsigned short)(r);
   canvas_carr[i].green = (unsigned short)(g);
   canvas_carr[i].blue = (unsigned short)(b);
   return(0);
}
/******************************************************************************/
/* CANVAS_init set up the environment. Returns 1 on success. */
static int CANVAS_init(void) {
   int prefx, prefy, prefxs, prefys;
   int i;
   void CANVAS_header();
   draw_fp = _draw_outfile();

   if (!canvas_first_time) return(1);

   CANVAS_header();

   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

   if (prefxs != -1 ) {
      vdevice.sizeSy = prefys;
      vdevice.sizeSx = prefxs;
      vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
   } else{
      vdevice.sizeSy = CANVASYSIZE;  /* size in resolution rasters */
      vdevice.sizeSx = CANVASXSIZE;  /* size in resolution rasters */
      vdevice.sizeX = vdevice.sizeY = MIN(CANVASXSIZE,CANVASYSIZE); /* current viewport to use */
   }



   fprintf(draw_fp, "<!-- canvas \" width=\"%fpx\" height=\"%fpx\" viewBox=\"0 0 %d %d\"-->\n",
   (float)vdevice.sizeSx/CANVASTORAS,(float)vdevice.sizeSy/CANVASTORAS,
   vdevice.sizeSx,vdevice.sizeSy);


   fprintf(draw_fp, "<script type=\"application/x-javascript\">\n");
   fprintf(draw_fp, "//---------------------------------------------------------------------------------------------------------------\n");
   fprintf(draw_fp, "function d(X,A){ // draw polyline\n");
   fprintf(draw_fp, "X.beginPath();\n");
   fprintf(draw_fp, "   X.moveTo(A[0],A[1]);\n");
   fprintf(draw_fp, "   for(i=2; i<A.length;i=i+2){\n");
   fprintf(draw_fp, "      X.lineTo(A[i],A[i+1]);\n");
   fprintf(draw_fp, "   }\n");
   fprintf(draw_fp, "   X.stroke();\n");
   fprintf(draw_fp, "   // draw (most) points\n");
   fprintf(draw_fp, "   radius=Math.max(1,X.lineWidth/2);\n");
   fprintf(draw_fp, "   if(A.length == 4){\n");
   fprintf(draw_fp, "    if( A[0] == A[2] & A[1] == A[3]){\n");
   fprintf(draw_fp, "         X.beginPath(); // draw circles for dots with radius set to 1/2 line thickness\n");
   fprintf(draw_fp, "         X.arc(A[0],A[1],radius,0,Math.PI*2,true);\n");
   fprintf(draw_fp, "         X.fill();\n");
   fprintf(draw_fp, "         // X.beginPath(); // square dots, which are faster in some browsers\n");
   fprintf(draw_fp, "         // X.moveTo(A[0]-radius,A[1]);\n");
   fprintf(draw_fp, "         // X.lineTo(A[0]+radius,A[1]);\n");
   fprintf(draw_fp, "         // X.stroke();\n");
   fprintf(draw_fp, "      }\n");
   fprintf(draw_fp, "    }\n");
   fprintf(draw_fp, "}\n");
   fprintf(draw_fp, "//---------------------------------------------------------------------------------------------------------------\n");
   fprintf(draw_fp, "function f(X,A){ // fill polygon\n");
   fprintf(draw_fp, "X.beginPath();\n");
   fprintf(draw_fp, "   X.moveTo(A[0],A[1]);\n");
   fprintf(draw_fp, "   for(i=2; i<A.length;i=i+2){\n");
   fprintf(draw_fp, "      X.lineTo(A[i],A[i+1]);\n");
   fprintf(draw_fp, "   }\n");
   fprintf(draw_fp, "   X.fill();\n");
   fprintf(draw_fp, "}\n");
   fprintf(draw_fp, "//---------------------------------------------------------------------------------------------------------------\n");
   fprintf(draw_fp, "function resizecanvas(canvasname,factor){\n");
   fprintf(draw_fp, "   var canvas = document.getElementById(canvasname);\n");
   fprintf(draw_fp, "   if(canvas.getContext){\n");
   fprintf(draw_fp, "       if(factor < 0 ){\n");
   fprintf(draw_fp, "          // reset to original size\n");
   fprintf(draw_fp, "          resizecanvas(canvasname,-factor/canvas.width);\n");
   fprintf(draw_fp, "       }else{\n");
   fprintf(draw_fp, "          // alert(  \" canvasname=\" + canvasname  +\n");
   fprintf(draw_fp, "          //        \" canvas.width=\" + canvas.width  +\n");
   fprintf(draw_fp, "          //        \" canvas.height=\" + canvas.height +\n");
   fprintf(draw_fp, "          //        \" factor=\" + factor\n");
   fprintf(draw_fp, "          //      );\n");
   fprintf(draw_fp, "          xwidth=canvas.width*factor;\n");
   fprintf(draw_fp, "          yheight=canvas.height*factor;\n");
   fprintf(draw_fp, "          var X = canvas.getContext(\"2d\");\n");
   fprintf(draw_fp, "          // In opera(1) both setAttribute('width'... and canvas.width=... are needed for some reason\n");
   fprintf(draw_fp, "          // In firefox(1) more sensibly either one works\n");
   fprintf(draw_fp, "          canvas.setAttribute('width',   \"\" + xwidth); // clears and resize the canvas\n");
   fprintf(draw_fp, "          canvas.setAttribute('height', \"\" + yheight); // clears and resize the canvas\n");
   fprintf(draw_fp, "          canvas.width = xwidth;                       // clears and resize the canvas\n");
   fprintf(draw_fp, "          canvas.height = yheight;                     // clears and resize the canvas\n");
   fprintf(draw_fp, "       }\n");
   fprintf(draw_fp, "   }\n");
   fprintf(draw_fp, "}\n");
   fprintf(draw_fp, "//---------------------------------------------------------------------------------------------------------------\n");
   fprintf(draw_fp, "\n");
   fprintf(draw_fp, "</script>\n");

   fprintf(draw_fp, "<script language=\"JavaScript\" src=\"resizecanvas.js\" type=\"text/javascript\">");
   fprintf(draw_fp, "</script>\n");

   fprintf(draw_fp, "<script type=\"application/x-javascript\">\n");

   fprintf(draw_fp, "function drawCanvas%d(canvasname){\n",pgroup);
 
   fprintf(draw_fp, "var canvas = document.getElementById(canvasname); // page %d\n", pgroup);
   fprintf(draw_fp, "if(canvas.getContext){\n");

   fprintf(draw_fp, "var X = canvas.getContext(\"2d\");\n");
   fprintf(draw_fp, "//<!-- id=\"Page%d\" -->\n", pgroup);
   fprintf(draw_fp, "X.save();\n");

   /*
   fprintf(draw_fp, "X.scale(%f,%f);\n", 1.0/CANVASTORAS, 1.0/CANVASTORAS );
   The plot is being made to stretch to the canvas size so it is easy to change the plot size
   */

   fprintf(draw_fp, "X.scale(canvas.width/%d,canvas.height/%d);\n", vdevice.sizeSx, vdevice.sizeSy );

   
   fprintf(draw_fp, "X.lineWidth=%d;\n",MAX(1,vdevice.sizeX*curwid/10000));
   fprintf(draw_fp, "X.fillStyle='black';\n");
   fprintf(draw_fp, "X.strokeStyle='black';\n");
   fprintf(draw_fp, "X.lineCap='round';\n");
   fprintf(draw_fp, "X.lineJoin='round';\n");

   vdevice.depth = 8;
   for (i = 0; i < CMAPSIZE; i++) /* set up the basic colors */
   {
      canvas_carr[i].red=255;
      canvas_carr[i].green=255;
      canvas_carr[i].blue=255;
   }

   CANVAS_mapcolor(0, 255, 255, 255);
   CANVAS_mapcolor(1, 255, 0, 0);
   CANVAS_mapcolor(2, 0, 255, 0);
   CANVAS_mapcolor(3, 255, 255, 0);
   CANVAS_mapcolor(4, 0, 0, 255);
   CANVAS_mapcolor(5, 255, 0, 255);
   CANVAS_mapcolor(6, 0, 255, 255);
   CANVAS_mapcolor(7, 0, 0, 0);

   CANVAS_mapcolor( 8, 155, 0, 0);
   CANVAS_mapcolor( 9, 0, 155, 0);
   CANVAS_mapcolor(10, 155, 255, 255);
   CANVAS_mapcolor(11, 155, 155, 0);
   CANVAS_mapcolor(12, 0, 0, 155);
   CANVAS_mapcolor(13, 155, 0, 155);
   CANVAS_mapcolor(14, 0, 155, 155);
   CANVAS_mapcolor(15, 100, 100, 100);
   PolyLineOpen = FALSE; /* Polyline not open */
   ObjectOpen = FALSE; /* Object not open */
   curcol=0;
   curwid=1;
   curpat=0;
   drawn = 0;
   return (1);
   /*      Set other line drawing parameters */
   /*      Move                              */
   /*      Set a default font height         */
}
/******************************************************************************/
static int closeline(void){
/* No "point" object in CANVAS; what do zero-length lines do?
   So keep track of whether drew any vector since last move in CANVAS_MOVED
*/
   /*int half_box;*/
   if(PolyLineOpen){
      if(CANVAS_MOVED == 0 ){ /* ASSUME NULL LINES ARE DOTS OR POINTS */
        /* circle of radius = line width */
        /*
          fprintf(draw_fp, "\nX.arc(%d,%d,%d,0,Math.PI*2.0,true)\n", LAST_X, FLIPY(LAST_Y),
             MAX(1,vdevice.sizeX*curwid/10000)); 
        */
         fprintf(draw_fp, "];d(X,A);\n"); /* end curve */
      }else{
         fprintf(draw_fp, "];d(X,A);\n"); /* end curve */
      }
      PolyLineOpen = FALSE; /* Polyline not open */
      points = 0;
   }
   return (0);
}
/******************************************************************************/
static int closeObject(void){
   if(ObjectOpen){
      fprintf(draw_fp, "// end object\n");
      ObjectOpen = FALSE; /* flag object not open */
      points = 0;
   }
   return (0);
}
/******************************************************************************/
static int openline(void){
   if(!PolyLineOpen){
      PolyLineOpen = TRUE; /* flag Polyline open */
   }
   return (0);
}
/******************************************************************************/
static int openObject(void){
   if(!ObjectOpen){
      fprintf(draw_fp, "// start object\n");
      ObjectOpen = TRUE; /* Object open */
   }
   return (0);
}
/******************************************************************************/
/* CANVAS_exit do a flush and close the output file if necessary.  */
static int CANVAS_exit(void) {
   int ipages;
   int ixtemp;
   int iytemp;

   closeline(); /* close Polyline line if it is open */
   closeObject(); /* close object if it is open */

   fprintf(draw_fp, "X.restore();\n");
   fprintf(draw_fp, "}else{\n"); /* */
   fprintf(draw_fp, "alert('Your browser needs HTML CANVAS support to view this page');\n}\n"); /* */
   fprintf(draw_fp, "} // end of page\n"); /* Page Clear, End of Page Group */

   fprintf(draw_fp, "function drawCanvases(SCALE){\n"); /* */
   for(ipages=0;ipages<pgroup;ipages++){
      fprintf(draw_fp, "resizecanvas(\"canvas_id%d\",SCALE);drawCanvas%d(\"canvas_id%d\");\n",ipages+1,ipages+1,ipages+1); /* */
   }
   fprintf(draw_fp, "}\n"); /* */

   fprintf(draw_fp, "</script>\n"); /* */
   fprintf(draw_fp, "</head>\n"); /* */
   fprintf(draw_fp, "<body onload=\"drawCanvases(1.0);\">\n"); /* */
   fprintf(draw_fp, "\n"); /* */

   fprintf(draw_fp, "<!-- SIMPLE CANVAS CALLS\n"); /* */
   for(ipages=0;ipages<pgroup;ipages++){
      fprintf(draw_fp, "  <canvas id=\"canvas_id%d\" width=\"%d\" height=\"%d\"></canvas>\n",
         ipages+1, 
         (int)vdevice.sizeSx/CANVASTORAS,
         (int)vdevice.sizeSy/CANVASTORAS); /* */
   }
   fprintf(draw_fp, "-->\n"); /* */

   fprintf(draw_fp, "<!-- CANVAS CALLS TO resizecanvas()\n"); /* */
   fprintf(draw_fp, "-->\n"); /* */

   fprintf(draw_fp, " <form>\n");

   ixtemp=(int)vdevice.sizeSx/CANVASTORAS;
   iytemp=(int)vdevice.sizeSy/CANVASTORAS;

   fprintf(draw_fp, "  <input type=\"button\" value=\"fit width\"  onclick=\"drawCanvases(-%d);drawCanvases(window.innerWidth/%d)\" />\n",ixtemp,ixtemp);
   fprintf(draw_fp, "  <input type=\"button\" value=\"fit height\" onclick=\"drawCanvases(-%d);drawCanvases(window.innerHeight/%d)\" />\n",ixtemp,iytemp);
   fprintf(draw_fp, "  <input type=\"button\" value=\"stamps\"     onclick=\"drawCanvases(-100)\" />\n");
   fprintf(draw_fp, "  <input type=\"button\" value=\"smaller\"    onclick=\"drawCanvases(0.80)\" />\n");
   fprintf(draw_fp, "  <input type=\"button\" value=\"bigger\"     onclick=\"drawCanvases(1.20)\" />\n");
   fprintf(draw_fp, " </form>\n");

   for(ipages=0;ipages<pgroup;ipages++){
      fprintf(draw_fp, "<canvas id=\"canvas_id%d\"\n",ipages+1);
      fprintf(draw_fp, "   onclick=\"resizecanvas(this.id,1.1);drawCanvas%d(this.id);\"\n",ipages+1);
      fprintf(draw_fp, "   onkeypress=\"resizecanvas(this.id,%d/this.width);drawCanvas%d(this.id);\"\n",
         (int)vdevice.sizeSx/CANVASTORAS,
         ipages+1
      );
      fprintf(draw_fp, "   ondblclick=\"resizecanvas(this.id,%d/this.width);drawCanvas%d(this.id);\"\n",
         (int)vdevice.sizeSx/CANVASTORAS,
         ipages+1
      );
      fprintf(draw_fp, "   width=\"%d\"\n",  (int)vdevice.sizeSx/CANVASTORAS);
      fprintf(draw_fp, "   height=\"%d\"\n", (int)vdevice.sizeSy/CANVASTORAS);
      fprintf(draw_fp, ">The CANVAS element is not displayed by this browser</canvas>\n"); /* */
   }

   fprintf(draw_fp, "\n"); /* */
   fprintf(draw_fp, "</body>\n"); /* */
   fprintf(draw_fp, "</html>\n"); /* */
   fprintf(draw_fp, "<!--- End of Document  -->\n");
   drawn = 0;
   points = 0;

   if (draw_fp != stdout && draw_fp != stderr ){
                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
   }
   return (0);
}
/******************************************************************************/
/* CANVAS_draw draw to an x, y point.  */
/* Note: (0, 0) is defined as the top left of the window in CANVAS.  */
static int CANVAS_draw(int x, int y) {
   static char linefeed[2] = {' ','\n'};

   if (LAST_X != vdevice.cpVx || LAST_Y != vdevice.cpVy ){
      closeline(); /* close line if required */
      openObject(); /* start Object if required */
      openline(); /* start line */
      fprintf(draw_fp, "A=[%d,%d", vdevice.cpVx, FLIPY(vdevice.cpVy));
      LAST_X=vdevice.cpVx;
      LAST_Y=vdevice.cpVy;
      CANVAS_MOVED=0;
      points = 1;
   }
   openline(); /* start line if required */

   if(points == 0){
      fprintf(draw_fp, "A=[%d,%d", x ,FLIPY(y));

      CANVAS_MOVED=0;
   }else{
      if(LAST_X!=x || LAST_Y!=y)CANVAS_MOVED=CANVAS_MOVED+1;

      fprintf(draw_fp, "%c,%d,%d", linefeed[(points % 8/7)], x ,FLIPY(y));
   }

   points++;
   LAST_X = x;
   LAST_Y = y;
   drawn = 1;
   return (0);
}
/******************************************************************************/
/* CANVAS_clear flush the current page without resetting the graphics state */
static int CANVAS_clear(void) {
   closeline(); /* close line if required */
   closeObject(); /* close Object if required */
   if (drawn)
   {
     fprintf(draw_fp, "X.restore();\n");
     fprintf(draw_fp, "}else{\n"); /* */
     fprintf(draw_fp, "alert('Your browser needs HTML CANVAS support to view this page');\n}\n"); /* */
     fprintf(draw_fp, "}// <!-- End Page%d -->\n",pgroup); /* Page Clear, End of Page Group */
     pgroup++; /* increment page id */
     fprintf(draw_fp, "function drawCanvas%d(canvasname){\n",pgroup);

     fprintf(draw_fp, "var canvas = document.getElementById(canvasname); // page %d\n", pgroup);
     fprintf(draw_fp, "if(canvas.getContext){\n");

     fprintf(draw_fp, "var X = canvas.getContext(\"2d\");\n");
     fprintf(draw_fp, "X.save();\n");

     /*
     fprintf(draw_fp, "X.scale(%f,%f);\n", 1.0/CANVASTORAS, 1.0/CANVASTORAS );
     The plot is being made to stretch to the canvas size so it is easy to change the plot size
     */
     fprintf(draw_fp, "X.scale(canvas.width/%d,canvas.height/%d);\n", vdevice.sizeSx, vdevice.sizeSy );

     fprintf(draw_fp, "X.lineCap='round';\n");
     fprintf(draw_fp, "X.lineJoin='round';\n");
     fprintf(draw_fp, "X.lineWidth=%d;\n",MAX(1,vdevice.sizeX*curwid/10000));
     fprintf(draw_fp, "X.fillStyle=\"#%2.2x%2.2x%2.2x\";\n",
        canvas_carr[curcol].red, 
        canvas_carr[curcol].green, 
        canvas_carr[curcol].blue);  /* fill color */
     fprintf(draw_fp, "X.strokeStyle=\"#%2.2x%2.2x%2.2x\";\n",
        canvas_carr[curcol].red, 
        canvas_carr[curcol].green, 
        canvas_carr[curcol].blue);  /* fill color */
   }
   drawn = 0;
   points = 0;
   CANVAS_MOVED=0;
   return(0);
}
/******************************************************************************/
/*
 *      value sets raster line width
 */
static int CANVAS_setlw(int width) {
   closeline(); /* close line if required */
   closeObject(); /* close Object if required */
   if ( width >= 0 ) {
      curwid = width;
   }
   fprintf(draw_fp, "X.lineWidth=%d;\n",MAX(1,vdevice.sizeX*curwid/10000));
   return(0);
}
/******************************************************************************/
/* CANVAS_color change the color of the pen
 *      kludged so negative value sets raster line width
 *      if exceed allowable number of colors maybe pick a line style
 *      or something like a gradient fill style for fun
 */
static int CANVAS_color(int col) {
   closeline(); /* close line if required */
   closeObject(); /* close Object if required */
   if ( col < 0 )
   {
      CANVAS_setlw(abs(col));
   } else {
      curpat = col/CMAPSIZE;
      curcol = col % CMAPSIZE;
   }
   fprintf(draw_fp, "X.fillStyle=\"#%2.2x%2.2x%2.2x\";\n",
      canvas_carr[curcol].red, 
      canvas_carr[curcol].green, 
      canvas_carr[curcol].blue);  /* fill color */
   fprintf(draw_fp, "X.strokeStyle=\"#%2.2x%2.2x%2.2x\";\n",
      canvas_carr[curcol].red, 
      canvas_carr[curcol].green, 
      canvas_carr[curcol].blue);  /* fill color */
   return(0);
}
/******************************************************************************/
/* HTML CANVAS does not support hardware fonts directly */
static int CANVAS_font(char *fontname) {
    draw_font("futura.m");
    return(0);
}
/******************************************************************************/
/* CANVAS_string output a string.  */
static int CANVAS_string(char *s) {
   return(0);
}
/******************************************************************************/
static int CANVAS_char(char c){ /* CANVAS_char output a character */
   char  s[2];
   s[0] = c;
   s[1]='\0';
   CANVAS_string(s);
   return(0);
}
/******************************************************************************/
static int CANVAS_fill(int n, int x[], int y[]) { /* fill a polygon */
   int     i;
   static char linefeed[2] = {' ','\n'};
   closeline(); /* close line if required */
   closeObject(); /* close line if required */

   fprintf(draw_fp, "// Polygon\n");
   fprintf(draw_fp, "X.fillStyle=\"#%2.2x%2.2x%2.2x\";\n",
      canvas_carr[curcol].red, 
      canvas_carr[curcol].green, 
      canvas_carr[curcol].blue);  /* fill color */
   fprintf(draw_fp, "X.strokeStyle=\"#%2.2x%2.2x%2.2x\";\n",
      canvas_carr[curcol].red, 
      canvas_carr[curcol].green, 
      canvas_carr[curcol].blue);  /* fill color */

   fprintf(draw_fp, "X.lineWidth=%d;\n", MAX(1,vdevice.sizeX*curwid/10000)); /* edge width */

   fprintf(draw_fp, "A=[%d,%d ", x[0],FLIPY(y[0]));

   for (i = 1; i < n; i++)
   {
      fprintf(draw_fp, ",%d,%d%c", x[i], FLIPY(y[i]),linefeed[(i % 8/7)]);
   }

   /* close path */
   fprintf(draw_fp, " ,%d,%d", x[0], FLIPY(y[0]));
   fprintf(draw_fp, "];f(X,A)\n");



   vdevice.cpVx = x[n - 1]; /* update current position */
   vdevice.cpVy = y[n - 1];

   LAST_X = vdevice.cpVx;
   LAST_Y = vdevice.cpVy;
   drawn = 1;
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry canvasdev = {
   "canvas",        /* name of device */
   "large",         /* name of large font */
   "small",         /* name of small font */
   noop,            /* Set drawing in back buffer */
   CANVAS_char,     /* Draw a hardware character */
   noop,            /* Check if a key was hit */
   CANVAS_clear,    /* Clear the screen to current color */
   CANVAS_color,    /* Set current color */
   CANVAS_draw,     /* Draw a line */
   CANVAS_exit,     /* Exit graphics */
   CANVAS_fill,     /* Fill a polygon */
   CANVAS_font,     /* Set hardware font */
   noop,            /* Set drawing in front buffer */
   noop,            /* Wait for and get the next key hit */
   CANVAS_init,     /* Initialize the device */
   noop2,           /* Get mouse/cross hair position */
   CANVAS_mapcolor, /* Set color indices */
   CANVAS_setlw,    /* Set line width */
   CANVAS_string,   /* Draw a hardware string */
   noop,            /* Swap front and back buffers */
   noop             /* Syncronize the display */
};
/******************************************************************************/
/* copy the canvas device into vdevice.dev.  */
int _CANVAS_draw_devcpy(void) {
   vdevice.dev = canvasdev;
   return(0);
}
/******************************************************************************/

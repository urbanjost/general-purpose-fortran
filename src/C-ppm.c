/*******************************************************************************/
/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/ppm.c - M_DRAW driver for ppm (Poskanzer pixmap) files"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 3.0, Jul 2019"
/*
@(#) Version 1.0, John S. Urban, Feb 1997
@(#) Version 2.0, John S. Urban, Nov 2005
@(#) Version 3.0, John S. Urban, Jul 2019
     The file(1) command assumes no comments in header. Moved comments so
     proper size is reported. The display(1) command reads a P6 file improperly
     with comments after max color intensity so very specifically need to do
       magic number\nsize\comment(s)\max color\pixel data
     even though Poskanzer pixmap order is supposed to be very flexible.

This driver makes  pbmplus-readable color (P3 and P6  format)  pixmap   files.

I usually use the popular netplus/pbmplus(1) packages to    convert  the    P3
and   P6    files    to    other formats to use in HTML documents or to import
into many other products.

The xv(1) package can display, rescale and convert P3 and P6 files too.

Unfortunately,  ppm  only  supports  single-frame  files.  For  P3  files  you
need   to   use  voutput(3[cf]}   to   keep   changing the output file, or use
csplit(1) on the P3 files to split them apart.

A very different but much more productive tack is taken  with  the  binary  P6
format.   Writing  to device p6 writes to the  standard  input  of  a  command
called   'p6to'   that  MUST BE IN YOUR SEARCH PATH.  and the environment
variable P6TO must be set to something. So something like:

#!/bin/sh
# this is the p6to script used by the M_DRAW p6 driver
exec cat >$$.p6

would do, but this is intended primarily for you to use with scripts that call
pbmplus commands; giving M_DRAW the appearance of being able to write in dozens
of popular bitmap formats. This way, you can write  GIF  or  PCX  or  JPEG  or
whatever kind of bitmap you can generate with P6 input files.

In fact, I only have the pbmplus package handy on a  SunOS machine, but I  use
draw  the  most on a Cray. No problem. I   use   a remote   shell   to get to
the command from another machine and I  generate   GIF   images    this    way
without    even noticing   all   the   forks   and   network connections being
made -- really!

One of the easiest ways is to make a script with a case statement in  it  that
triggers  from an environment variable.  The details vary with the scripting
language (Bourne, ksh, csh, perl, tcl/tk) but the  basic  idea  is  the  same.
Here's a basic Bourne shell script:

#!/bin/sh
# this is the p6to script used by the M_DRAW p6 driver
case "$P6TO" in
GIF)remsh sirius ppmtogif >$$.gif # gif file
# remsh starts a remote shell. Use rsh or ssh on some machines
# sirius seems like a good fake name for a remote Sun. But siriusly folks ...
;;
TIFF)remsh sirius pnmtotiff >$$.tiff # tiff file
;;
XBM)remsh sirius 'ppmtopgm|pgmtopbm|pbmtoxbm' > $$.xbm # greyscale X11 bitmap
;;
XV) cat > $$.p6; xv $$.p6 & # read into the xv bitmap viewing utility
;;
*)
cat > $$.p6
# if you need more control of the filename, consider doing a putenv in your
# program of a variable name that is then used to build the file name.
;;
esac
exit

The popen(); remote shell and IO redirect all work so nicely together you just
figure that anyone not using Unix just never heard of it.

Please pass any upgrades or comments back to me if you get a chance.

*-----------------------------------------------------------------*
| Author: John S. Urban                                           |
*-----------------------------------*-----------------------------*
| Westinghouse Electric Corporation | osjsu@westinghouse.com      | NO
| Cray Research                     | urban@cray.com              | NO
| Silicon Graphics, Incorporated    | urban@sgi.com               | NO
| Digital Equipment Corporation     | John.Urban@digital.com      | NO
| Compaq                            | John.Urban@compaq.com       | NO
| Hewlett-Packard                   | John.Urban@hp.com           | NO
| Betchtel                          | urbanjs@bettis.gov          |
*-----------------------------------*-----------------------------*
================================================================================
   USAGE NOTES ON THE PPM DRIVER:

See the PBM driver

I have used this driver on UNICOS(Cray), ULTRIX, Tru64 Unix, HP-UX, Linux,
SunOS, Solaris, SGI IRIX,IBM AIX, and NeXT so it should  be  reasonably portable.

If TRUECOLOR is not defined only the color number (from 0 to 255) is stored
in  the pixmap. This color number is then used to generate RGB values when the
page is WRITTEN. This reduces the amount of storage needed, but means that  if
you  draw with pen N and then change the color of pen N and draw with it again
that everything in the printed image that used pen N  will  all  be  the  same
color  (the last one defined for pen N before printing). To get a "true color"
behavior would require saving RGB values for each point,  which  would  triple
the storage requirements but would otherwise be easy to do.

Line thickness is supported with filled rectangular  polygons  when  the  line
thickness  is  greater  than  1.  Square  ends  are  used  that go only to the
endpoints unless line thickness is greater than  5,  in  which  case  complete
circles  are  added  to the endpoints.  If very short polylines are drawn with
the circles on the ends slight errors can occur.

If you have a pre-ANSI C compiler  you  will  have  to  remove  the  PROTOTYPE
statements and change a few procedure headers back to the old K&R style.
================================================================================

References: 1) Fundamentals of Interactive Computer Graphics, Foley & Van Dam, Addison Wesley Publishing Company, 1982
            2) ppm - portable bitmap file format, 27 September 1991, Copyright (C) 1989, 1991 by Jef Poskanzer.

 1996, 1997, 2004, 2005  Author: John S. Urban

 This  software  is  public  domain  and  may be  used  for  any  purpose
 commercial or otherwise.  It is offered  without any guarantee as to its
 suitability  for any purpose or as to the sanity of its  writers.  We do
 ask that the  source is passed on to anyone  that  requests  a copy, and
 that people who get copies don't go round claiming they wrote it.

================================================================================
*/

/******************************************************************************/
#define TRUECOLOR

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#ifndef MINGW
#include <pwd.h>
#include <sys/utsname.h>
#endif
#include <math.h>
#include "draw.h"

extern FILE     *_draw_outfile();
extern  FILE     *draw_fp;
static FILE     *fpP6;

#define byte unsigned char

#define MAX(x,y)  ((x) > (y) ? (x) : (y))
#define MIN(x,y)  ((x) < (y) ? (x) : (y))
#define ABS(x)    ((x) < 0 ? -(x) : (x))
#ifndef PI
#define PI      3.14159265358979323846264338327950288419716939937510
#endif

static int X_SIZE, Y_SIZE, LAST; /* size of graphics array */


#define P3  2
#define P6  3
#define CHAR  4
static int GLOBAL_driver = 0;
static int GLOBAL_font = 0;
static int GLOBAL_color = 0;

#define UNDRAWN 0
#define DRAWN   1
static int GLOBAL_drawn = UNDRAWN; /* flag whether page is blank or not */

static int GLOBAL_rasters = 1; /* line thickness */

static int GLOBAL_lastx, GLOBAL_lasty;     /* position of last draw */

typedef struct {
   int     r, g, b;
} ColorTable;
/* The  macro  SET_PIXEL will set a given pixel in the graphics arrays */
#ifndef TRUECOLOR
#define    SET_PIXEL(x,y) ( *(graphics_rgb + (x) * Y_SIZE + (y) ) = (char)(GLOBAL_color) )
/* NB: to fit in a byte CMAPSIZE HAS to be 255 MAX */
#define    CMAPSIZE        256
#define    CMAPDEPTH       8
byte *graphics_rgb; /* the graphics data */
#else
/*
        WITHOUT BOUNDS CHECK FOR CLARITY
        Checking bounds of line endpoints would probably be more efficient

#define    SET_BYTE_R(x,y,intensity) ( *(graphics_r + (x) * Y_SIZE + (y) ) = (intensity) )
#define    SET_BYTE_G(x,y,intensity) ( *(graphics_g + (x) * Y_SIZE + (y) ) = (intensity) )
#define    SET_BYTE_B(x,y,intensity) ( *(graphics_b + (x) * Y_SIZE + (y) ) = (intensity) )
*/
#define    SET_BYTE_R(x,y,intensity) ( *(graphics_r + MAX(MIN((x) * Y_SIZE + (y),LAST),0) ) = (intensity) )
#define    SET_BYTE_G(x,y,intensity) ( *(graphics_g + MAX(MIN((x) * Y_SIZE + (y),LAST),0) ) = (intensity) )
#define    SET_BYTE_B(x,y,intensity) ( *(graphics_b + MAX(MIN((x) * Y_SIZE + (y),LAST),0) ) = (intensity) )


#define SET_BIT_ZERO(x,y) (( *(graphics + MAX(MIN(( (x) / 8 ) * Y_SIZE + (y) ,LAST),0))) |= (0x80 >> ( (x) % 8 ) ) )
#define SET_BIT_ONE(x,y)  (( *(graphics + MAX(MIN(( (x) / 8 ) * Y_SIZE + (y) ,LAST),0))) &= (0x80 >> ( (x) % 8 ) ) )


#define    cur_r (coltab[GLOBAL_color].r)
#define    cur_g (coltab[GLOBAL_color].g)
#define    cur_b (coltab[GLOBAL_color].b)
#define    SET_PIXEL(x,y) (SET_BYTE_R((x),(y),(cur_r)),SET_BYTE_G((x),(y),(cur_g)),SET_BYTE_B((x),(y),(cur_b)))
#define    CMAPSIZE        8192
#define    CMAPDEPTH       13
byte *graphics_r; /* the big graphics data */
byte *graphics_g; /* the big graphics data */
byte *graphics_b; /* the big graphics data */
#endif
/******************************************************************************/
static ColorTable coltab[CMAPSIZE];
/******************************************************************************/
static int PPM_MEMSET(void) { /* set graphics array to all zero */
   int i;

   /*--- IF YOU HAVE IT, MEMSET IS PROBABLY FASTER
#ifndef TRUECOLOR
        memset(graphics_rgb, (char)GLOBAL_color, sizeof(byte) * Y_SIZE * X_SIZE);
#else
        memset(graphics_r, (char)coltab[GLOBAL_color].r, sizeof(byte) * Y_SIZE * X_SIZE);
        memset(graphics_g, (char)coltab[GLOBAL_color].g, sizeof(byte) * Y_SIZE * X_SIZE);
        memset(graphics_b, (char)coltab[GLOBAL_color].b, sizeof(byte) * Y_SIZE * X_SIZE);
#endif
        ---*/

   for ( i=0; i< (X_SIZE * Y_SIZE); i++) {
#ifndef TRUECOLOR
      *(graphics_rgb + i) = (char)GLOBAL_color;
#else
      *(graphics_r + i) = (char)coltab[GLOBAL_color].r;
      *(graphics_g + i) = (char)coltab[GLOBAL_color].g;
      *(graphics_b + i) = (char)coltab[GLOBAL_color].b;
#endif
   }
   return(0);
}
/*******************************************************************************/
static int PPM_setlw(int w){ /* Set the line width */
   GLOBAL_rasters = w*vdevice.sizeX/10000.0;
   GLOBAL_rasters = MAX(1,GLOBAL_rasters);
   return(0);
}
/******************************************************************************/
static int PPM_color(int col){ /* change the current color */
   if(col < 0){
      PPM_setlw(abs(col));
   } else{
      GLOBAL_color = ABS(col % CMAPSIZE) ;
   }
   return(0);
}
/******************************************************************************/
static int PPM_mapcolor(int indx, int r, int g, int b){  /* set values in pseudo color map.  */
   if (indx < CMAPSIZE && indx >= 0) {
      coltab[indx].r = ABS(r % CMAPSIZE) ;
      coltab[indx].g = ABS(g % CMAPSIZE) ;
      coltab[indx].b = ABS(b % CMAPSIZE) ;
   }
   return(0);
}
/******************************************************************************/
static int PPM_RESIZE(void) {
   int prefx, prefy, prefxs, prefys;
   int i;

   /* ---DETERMINE SIZE OF GRAPHICS PIXMAP */
   /* see if a size was user-specified using the prefsize procedure */
   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
   if (prefxs != -1 ) {
      if (prefys <= 0 ){
         fprintf(stderr,"*PPM_init* y size of %d set to 400\n",prefys);
         prefys = 400;
      }else{
         vdevice.sizeSy = prefys;
      }
      if (prefxs <= 0 ){
         fprintf(stderr,"*PPM_init* y size of %d set to 600\n",prefys);
         prefxs = 600;
      }else{
         vdevice.sizeSx = prefxs;
      }
   }
   else{
      /* nice default value */
      prefx = 0;
      prefy = 0;
      vdevice.sizeSy = 400;
      vdevice.sizeSx = 600;
   }
   X_SIZE=vdevice.sizeSx;
   Y_SIZE=vdevice.sizeSy;
   LAST=X_SIZE*Y_SIZE-1;
#ifndef TRUECOLOR
   graphics_rgb = (byte *) malloc( X_SIZE * Y_SIZE * sizeof(byte) ); /* the graphics array */
#else
   graphics_r = (byte *) malloc( X_SIZE * Y_SIZE * sizeof(byte) ); /* the graphics array */
   graphics_g = (byte *) malloc( X_SIZE * Y_SIZE * sizeof(byte) ); /* the graphics array */
   graphics_b = (byte *) malloc( X_SIZE * Y_SIZE * sizeof(byte) ); /* the graphics array */
#endif
}
/******************************************************************************/
static int PPM_init(void) {
   int i;
   int PPM_MEMSET(void); /* set graphics array to all zero */

   PPM_RESIZE();
   vdevice.sizeX = vdevice.sizeY = MIN(vdevice.sizeSy,vdevice.sizeSx);
   PPM_MEMSET(); /* set the graphics array to 0 */

   vdevice.depth = CMAPDEPTH;

   draw_fp = _draw_outfile();

   /* Cause scaling to be 0 to maxX maxY: prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy */

   GLOBAL_lastx = -1111111;
   GLOBAL_lasty = -1111111;

   GLOBAL_drawn = UNDRAWN;

   PPM_mapcolor(0, 255, 255, 255);
   PPM_mapcolor(1, 255, 0, 0);
   PPM_mapcolor(2, 0, 255, 0);
   PPM_mapcolor(3, 255, 255, 0);
   PPM_mapcolor(4, 0, 0, 255);
   PPM_mapcolor(5, 255, 0, 255);
   PPM_mapcolor(6, 0, 255, 255);
   PPM_mapcolor(7, 0, 0, 0);

   for(i=8; i<CMAPSIZE; i++){
      PPM_mapcolor(i, 255, 255, 255);
   }

   return(1);
}
/******************************************************************************/
static int PPM_DRAW_LINE(int x,int y) { /* draws a line across a graphics array */
   int runcount;
   int dx,dy;
   int xinc,yinc;
   int xplot,yplot;

   SET_PIXEL(GLOBAL_lastx,GLOBAL_lasty); /* move to initial spot */

   runcount=0;

   dx = abs(GLOBAL_lastx-x);

   xinc=0;
   if (x > GLOBAL_lastx)  xinc=  1;
   if (x == GLOBAL_lastx) xinc=  0;
   if (x < GLOBAL_lastx)  xinc= -1;

   dy = abs(GLOBAL_lasty-y);

   yinc=0;
   if (y > GLOBAL_lasty)  yinc=  1;
   if (y == GLOBAL_lasty) yinc=  0;
   if (y < GLOBAL_lasty)  yinc= -1;

   xplot = GLOBAL_lastx;
   yplot = GLOBAL_lasty;

   if (dx>dy) {
      /* iterate x */
      while (xplot != x) {
         xplot += xinc;
         runcount += dy;
         if (runcount >= (dx-runcount)) {
            yplot += yinc;
            runcount -= dx;
         }
         SET_PIXEL(xplot,yplot);
      }
   } else {
      /* iterate y */
      while (yplot != y) {
         yplot += yinc;
         runcount += dx;
         if (runcount >= (dy-runcount)) {
            xplot += xinc;
            runcount -= dy;
         }
         SET_PIXEL(xplot,yplot);
      }
   }

   GLOBAL_lastx = xplot;
   GLOBAL_lasty = yplot;

   return(0);
}
/*******************************************************************************/
static int PPM_YINTERCEPT(int yscan, int x1, int y1, int x2, int y2,int  *xintercept, int *yprev) {
/*
Determine if scan line intercepts the line segment. If it does, return the x intercept.
*/
   int deltay, yprevious;
   float   t;
   yprevious = *yprev; /* the value we need to use in this pass */
   *yprev = y1;        /* store the value for the next call to (probably) use */
   deltay = y2 - y1;
   if ( deltay == 0 ){
      /* horizontal lines do not contribute to scan line intercepts */
      *yprev=yprevious;
      return(0);
   }
   t = (float)(yscan - y1) / deltay;
   if (t > 0.0 && t <= 1.0) {
      /* scan line and line segment intersect but not at leading vertex */
      *xintercept = x1 + t*(x2 - x1) + 0.5;
      return (1);
   } else if ( t == 0.0 ){
      /* scan line and line segment intersect at leading vertex */
      *xintercept = x1 + t*(x2 - x1) + 0.5;
      if(yprevious <= y1 && y2 <= y1 ){
         /* local maximum */
         return (1);
      } else if(yprevious >= y1 && y2 >= y1 ){
         /* local minimum */
         return (1);
      } else{
         /* ignore duplicate at vertex that is not a local maximum or minimum */
         return (0);
      }
   }
   /* scan line and line segment did not intersect */
   return (0);
}
/*******************************************************************************/
static void PPM_SOLID_FILL(int n, int x[], int y[]) { /* fill polygon of n points drawn by polyline <x,y>.  */
   int i, j, sorted, yhorizontal, xint, tmp, xmin, xmax, ymax, ymin, xi[MAXVERTS], yprev;

   if ( n > MAXVERTS) {
      fprintf(stderr,"*PPM_SOLID_FILL* more than %d vertices in a polygon\n",MAXVERTS);
      return;
   }

   /* find clip range */
   ymin = ymax = y[0];
   xmin = xmax = x[0];
   for (i = 0; i < n; i++) {
      ymax = MAX(ymax, y[i]);
      ymin = MIN(ymin, y[i]);
      xmax = MAX(xmax, x[i]);
      xmin = MIN(xmin, x[i]);
   }
   /* ensure scan lines are generated that do not cause out-of-bound problems in the y direction */
   ymin=MAX(ymin,0);
   ymax=MIN(ymax,Y_SIZE);

   /* For each y value, get a list of X intersections... */
   yhorizontal = ymax ;
   while (yhorizontal >= ymin) {
      j = 0;
      yprev = y[n-1];
      for (i = 0; i < n-1; i++)
         if (PPM_YINTERCEPT(yhorizontal, x[i], y[i], x[i+1], y[i+1], &xint, &yprev)){
            xi[j] = xint;
            j++;
         }
      /* Last one. */
      if (PPM_YINTERCEPT(yhorizontal, x[n-1], y[n-1], x[0], y[0], &xint, &yprev)){
         xi[j] = xint;
         j++;
      }

      /* odd pairs means something went wrong in figuring out whether to count vertices or not */
      if( 2 * (j/2) != j){
         fprintf(stderr,"*PPM_SOLID_FILL* Internal error: odd number of intersection points (%d) \n",j);
      }

      /* Sort the X intersections... */
      sorted = 0;
      while (!sorted) {
         sorted = 1;
         for (i = 0; i < j-1; i++)
            if (xi[i] > xi[i+1]) {
               tmp = xi[i];
               xi[i] = xi[i+1];
               xi[i+1] = tmp;
               sorted = 0;
            }
      }

      /* Draw the horizontal lines */
      /* should make sure within X clipping range */
      for (i = 0; i < j-1; i += 2) {
         GLOBAL_lastx=MAX(0,MIN(xi[i],X_SIZE));
         GLOBAL_lasty=yhorizontal;
         PPM_DRAW_LINE(MAX(0,MIN(xi[i+1],X_SIZE)), yhorizontal);
      }
      yhorizontal -= 1;
   }
}
/******************************************************************************/
static int PPM_ENDCAP_CIRCLE(int x, int y){ /* Draw a circle on thick line segment end point */
   /* there are more efficient ways to do this */
   /* circle precision */
#define nsegs 15
   /* static int nsegs= 15; */
   float cx, cy, dx, dy, angle, cosine, sine ;
   /* array to place circle points on */
   int cxras[nsegs], cyras[nsegs];
   int i;

   angle = 2.0 * PI / nsegs;
   cosine = cos((double)angle);
   sine = sin((double)angle);

   /* first point on circle */
   cxras[0] = cx =  x + GLOBAL_rasters/2.0;
   cyras[0] = cy = y;
   for (i = 1; i < nsegs; i++) {
      dx = cx - x;
      dy = cy - y;
      cxras[i] = ( cx = x + dx * cosine - dy * sine) ;
      cyras[i] = ( cy = y + dx * sine   + dy * cosine) ;
   }
   PPM_SOLID_FILL(nsegs,cxras,cyras);
   return(0);
}
/******************************************************************************/
static int PPM_fill(int n, int x[], int y[]) { /* "fill" a polygon */
   int     i;

   /* update current position if needed */
   GLOBAL_lastx=x[0];
   GLOBAL_lasty=y[0];

   for (i = 1; i < n; i++){
      PPM_DRAW_LINE(x[i],y[i]); /* draw outline across graphics array */
   }
   if ( x[n-1] != x[0] || y[n-1] != y[0] ) /* close the polygon if it is not closed */
      PPM_DRAW_LINE(x[0],y[0]);

   PPM_SOLID_FILL(n, x, y);

   /* update current position */
   GLOBAL_lastx = vdevice.cpVx = x[n - 1];
   GLOBAL_lasty = vdevice.cpVy = y[n - 1];

   GLOBAL_drawn = DRAWN;
   return(0);
}
/******************************************************************************/
static int PPM_draw(int x, int y) { /* print the commands to draw a line from the current graphics position to (x, y).  */
   int     holdx, holdy;
   int xwide[4], ywide[4];
   float cosa, sina;
   double angle;

   if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
      GLOBAL_lastx=vdevice.cpVx;
      GLOBAL_lasty=vdevice.cpVy;
   }

   if ( GLOBAL_rasters <= 1){
      PPM_DRAW_LINE(x,y);
   } else{
      /* thick lines are made from filled polygon(s) */
      /* add a circle to ends of really thick lines */
      if( GLOBAL_rasters >= 6){
         holdx=GLOBAL_lastx;
         holdy=GLOBAL_lasty;
         PPM_ENDCAP_CIRCLE(GLOBAL_lastx,GLOBAL_lasty);
         PPM_ENDCAP_CIRCLE(x,y);
         GLOBAL_lastx=holdx;
         GLOBAL_lasty=holdy;
      }
      angle=atan2((double)(y-GLOBAL_lasty),(double)(x-GLOBAL_lastx)) + PI/2.0;
      cosa=(GLOBAL_rasters/2.0)*cos(angle);
      sina=(GLOBAL_rasters/2.0)*sin(angle);
      xwide[0]=x+cosa;
      xwide[1]=GLOBAL_lastx+cosa;
      xwide[2]=GLOBAL_lastx-cosa;
      xwide[3]=x-cosa;

      ywide[0]=y+sina;
      ywide[1]=GLOBAL_lasty+sina;
      ywide[2]=GLOBAL_lasty-sina;
      ywide[3]=y-sina;

      PPM_SOLID_FILL(4,xwide,ywide);
   }
   GLOBAL_drawn = DRAWN;
   return(0);
}
/*******************************************************************************/
static int P3_print_graphics(void) { /* print_graphics -- print the graphics bit array as a ppm P3 file*/
   int x; /* current x BYTE */
   int y; /* current y location */
   int index ;
#ifndef TRUECOLOR
   int pix;
#endif
   time_t tod;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;

   (void) fprintf(draw_fp,"P3\n"); /* magic number of a clear text PPM file */
   (void) fprintf(draw_fp,"%d %d\n",X_SIZE,Y_SIZE); /* size of bitmap */
#ifndef TRUECOLOR
   (void) fprintf(draw_fp,"# PRINTTIMECOLOR 256 entry colortable version\n");
#else
   (void) fprintf(draw_fp,"# TRUECOLOR 8192 entry colortable version\n");
#endif
   (void) fprintf(draw_fp,"# CREATOR: M_DRAW ppm driver; version 3.0 2019/07/10\n"); /* ppm P3 file can contain comment lines*/
   (void) fprintf(draw_fp,"# AUTHOR:  John S. Urban\n");

        time(&tod);
        fprintf(draw_fp,"# CreationDate: %s",ctime(&tod));

#ifndef MINGW
        un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
        uname(un);

        if ((username = getlogin()) == NULL ){
        pw = getpwuid(getuid());
        username = pw->pw_name;
        }
        fprintf(draw_fp,"# For: %s on OS=%.*s NETWORK_NAME=%.*s RELEASE=%.*s VERSION=%.*s MACHINE=%.*s\n",username,
                (int)sizeof(un->sysname),  un->sysname,
                (int)sizeof(un->nodename), un->nodename,
                (int)sizeof(un->release),  un->release,
                (int)sizeof(un->version),  un->version,
                (int)sizeof(un->machine),  un->machine);
#endif

   (void) fprintf(draw_fp,"# csplit multiframe file FILE.p3: csplit -f P3 -k FILE.p3 '%%^P3%%' '/^P3/' '{999}'\n");

   (void) fprintf(draw_fp,"255\n"); /* maximum value of a color intensity*/
   /* notice going from bottom to top because putting out in a right handed coordinate system, was assuming left-handed */
   for (y = (Y_SIZE-1); y >= 0; y--) { /* Loop for each byte in the array */
      for ( x = 0; x < X_SIZE ; x++){
         index = Y_SIZE * x + y;
#ifndef TRUECOLOR
         pix = (int)*(graphics_rgb + index);
         /* The manual says a P3 ppm file should not be wider than 70 characters */
         (void) fprintf(draw_fp,"%d %d %d\n",coltab[pix].r, coltab[pix].g, coltab[pix].b);
#else
         /* The manual says a P3 ppm file should not be wider than 70 characters */
         (void) fprintf(draw_fp,"%d " ,graphics_r[index]);
         (void) fprintf(draw_fp,"%d " ,graphics_g[index]);
         (void) fprintf(draw_fp,"%d\n",graphics_b[index]);
#endif
      }
   } /* end of writing a column */
   (void) fprintf(draw_fp,"\n");
   GLOBAL_drawn = UNDRAWN;
   return(0);
}
/*******************************************************************************/
static void P6_print_graphics(void) { /* print_graphics -- print the graphics bit array as a ppm P6 file*/
   int x; /* current x BYTE */
   int y; /* current y location */
   int index;
#ifndef TRUECOLOR
   int pix;
#endif
   char *varname;
   time_t tod;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;
   varname=getenv("P6TO");
   if(varname==(char *)NULL) { /* P6TO is undefined so use standard I/O */
      fpP6 = draw_fp;
   } else{                     /* P6TO is defined so try to use p6to filter */
      fpP6 = popen("p6to", "w");
      if (!fpP6) {
         fprintf(stderr, "*P6_print_graphics* Couldn't open pipe to p6to command.\n");
         fpP6 = draw_fp; /* drop back to using normal file */
         /* exit(1); */
      }
   }


   (void) fprintf(fpP6,"P6\n"); /* magic number of a ppm file */
   (void) fprintf(fpP6,"%d %d\n",X_SIZE,Y_SIZE); /* size of bitmap */
   (void) fprintf(fpP6,"# CREATOR: M_DRAW ppm driver; version 1.0 1997/02/02\n"); /*ppm P6 file can contain comment lines*/
   (void) fprintf(fpP6,"# AUTHOR:  John S. Urban\n");
        time(&tod);
        fprintf(fpP6,"# CreationDate: %s",ctime(&tod));

#ifndef MINGW
        un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
        uname(un);

        if ((username = getlogin()) == NULL ){
        pw = getpwuid(getuid());
        username = pw->pw_name;
        }
        fprintf(fpP6,"# For: %s on OS=%.*s NETWORK_NAME=%.*s RELEASE=%.*s VERSION=%.*s MACHINE=%.*s\n",username,
                (int)sizeof(un->sysname),  un->sysname,
                (int)sizeof(un->nodename), un->nodename,
                (int)sizeof(un->release),  un->release,
                (int)sizeof(un->version),  un->version,
                (int)sizeof(un->machine),  un->machine);
#endif


   (void) fprintf(fpP6,"255\n"); /* maximum value of a color intensity*/
   /* notice going from bottom to top because putting out in a right handed coordinate system, was assuming left-handed */
   for (y = (Y_SIZE-1); y >= 0; y--) {
      for ( x = 0; x < X_SIZE ; x++){
         index = Y_SIZE * x + y;
#ifndef TRUECOLOR
         pix = *(graphics_rgb + index);
         putc((char)coltab[pix].r,fpP6);
         putc((char)coltab[pix].g,fpP6);
         putc((char)coltab[pix].b,fpP6);
#else
         putc((char)graphics_r[index],fpP6);
         putc((char)graphics_g[index],fpP6);
         putc((char)graphics_b[index],fpP6);
#endif
      }
   }

   if(varname!=(char *)NULL) {  /* if tried to use pipe close it */
      fflush(fpP6);
      pclose(fpP6);
   }

   GLOBAL_drawn = UNDRAWN;
}
/******************************************************************************/
static void CHAR_print_graphics(void) /* print_graphics -- print the graphics bit array as an xterm ASCII file */
{
   int x; /* current x BYTE */
   int y; /* current y location */
   int index ;
#ifndef TRUECOLOR
   int pix;
#endif
   int icolor;

   /* notice going from bottom to top because putting out in a right handed coordinate system, was assuming left-handed */
   (void) fprintf(draw_fp,"\033[8;%d;%dt\n",Y_SIZE,X_SIZE);  /* set xterm size to array size if possible */
   for (y = (Y_SIZE-1); y >= 0; y--) { /* Loop for each byte in the array */
      for ( x = 0; x < X_SIZE ; x++){
         index = Y_SIZE * x + y;
#ifndef TRUECOLOR
         pix = (int)*(graphics_rgb + index);
         icolor=(coltab[pix].r*1+coltab[pix].g*2+coltab[pix].b*4)/255 % 8;
#else
         icolor=(graphics_r[index]*1+graphics_g[index]*2+graphics_b[index]*4)/255 % 8;
#endif
         (void) fprintf(draw_fp,"\033[4%dm ",icolor);
      }
   (void) fprintf(draw_fp,"\n");
   } /* end of writing a column */
   (void) fprintf(draw_fp,"\033[37;40m\n");
   GLOBAL_drawn = UNDRAWN;
}
/******************************************************************************/
static int PPM_PRINT(void) {
/* exit from draw printing the command to flush the buffer.  */
   if( GLOBAL_drawn ){
      switch(GLOBAL_driver) {
      case P3:
         P3_print_graphics();
         break;
      case P6:
         P6_print_graphics();
         break;
      case CHAR:
         CHAR_print_graphics();
         break;
      default:
         fprintf(stderr, "ppm driver: UNKNOWN DRIVER NAME\n");
         P3_print_graphics();
      }
   }
   fflush(draw_fp); /* flush the output file */
   return(0);
}
/******************************************************************************/
static int PPM_exit(void) { /* exit from draw printing the command to flush the buffer.  */
   PPM_PRINT();
   if (draw_fp != stdout && draw_fp != stderr ){
                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
   }

/* release the graphics data */
#ifndef TRUECOLOR
   free(graphics_rgb);
#else
   free(graphics_r);
   free(graphics_g);
   free(graphics_b);
#endif

   return(0);
}
/******************************************************************************/
static int PPM_clear(void) { /* flush current page and clear graphics array */

   int PPM_MEMSET(); /* set graphics array to all zero */

   PPM_PRINT();
/* release the graphics data */
#ifndef TRUECOLOR
   free(graphics_rgb);
#else
   free(graphics_r);
   free(graphics_g);
   free(graphics_b);
#endif
   PPM_RESIZE();
   PPM_MEMSET(); /* set the graphics array to 0 */
   return(0);
}
/******************************************************************************/
static int PPM_font(char *fontname) { /* load in large or small */
        if (strcmp(fontname, "small") == 0) {
                vdevice.hwidth = 8.00; /* Size in plotter resolution units */
                vdevice.hheight = 13.0;
                GLOBAL_font = 0;
        } else if (strcmp(fontname, "large") == 0) {
                vdevice.hwidth = 15.00;
                vdevice.hheight = 39.00;
                GLOBAL_font = 1;
                /* draw_font("futura.m"); */ /* use software till add second font later */
        } else
                return(0);

        return(1);
}
/******************************************************************************/
/* The X11-Window system public domain 8x13 fixed font. */
#define NUMCHARS 95
#define FROW 13
#define FCOL 8
static unsigned char font8x13[NUMCHARS][FROW] = {
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
{0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x10, 0x00, 0x00},
{0x24, 0x24, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
{0x00, 0x24, 0x24, 0x7e, 0x24, 0x7e, 0x24, 0x24, 0x00, 0x00, 0x00},
{0x00, 0x10, 0x3c, 0x50, 0x38, 0x14, 0x78, 0x10, 0x00, 0x00, 0x00},
{0x22, 0x52, 0x24, 0x08, 0x08, 0x10, 0x24, 0x2a, 0x44, 0x00, 0x00},
{0x00, 0x00, 0x30, 0x48, 0x48, 0x30, 0x4a, 0x44, 0x3a, 0x00, 0x00},
{0x38, 0x30, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
{0x04, 0x08, 0x08, 0x10, 0x10, 0x10, 0x08, 0x08, 0x04, 0x00, 0x00},
{0x20, 0x10, 0x10, 0x08, 0x08, 0x08, 0x10, 0x10, 0x20, 0x00, 0x00},
{0x00, 0x00, 0x24, 0x18, 0x7e, 0x18, 0x24, 0x00, 0x00, 0x00, 0x00},
{0x00, 0x00, 0x10, 0x10, 0x7c, 0x10, 0x10, 0x00, 0x00, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x38, 0x30, 0x40, 0x00},
{0x00, 0x00, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x38, 0x10, 0x00},
{0x02, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x80, 0x00, 0x00},
{0x18, 0x24, 0x42, 0x42, 0x42, 0x42, 0x42, 0x24, 0x18, 0x00, 0x00},
{0x10, 0x30, 0x50, 0x10, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00},
{0x3c, 0x42, 0x42, 0x02, 0x04, 0x18, 0x20, 0x40, 0x7e, 0x00, 0x00},
{0x7e, 0x02, 0x04, 0x08, 0x1c, 0x02, 0x02, 0x42, 0x3c, 0x00, 0x00},
{0x04, 0x0c, 0x14, 0x24, 0x44, 0x44, 0x7e, 0x04, 0x04, 0x00, 0x00},
{0x7e, 0x40, 0x40, 0x5c, 0x62, 0x02, 0x02, 0x42, 0x3c, 0x00, 0x00},
{0x1c, 0x20, 0x40, 0x40, 0x5c, 0x62, 0x42, 0x42, 0x3c, 0x00, 0x00},
{0x7e, 0x02, 0x04, 0x08, 0x08, 0x10, 0x10, 0x20, 0x20, 0x00, 0x00},
{0x3c, 0x42, 0x42, 0x42, 0x3c, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00},
{0x3c, 0x42, 0x42, 0x46, 0x3a, 0x02, 0x02, 0x04, 0x38, 0x00, 0x00},
{0x00, 0x00, 0x10, 0x38, 0x10, 0x00, 0x00, 0x10, 0x38, 0x10, 0x00},
{0x00, 0x00, 0x10, 0x38, 0x10, 0x00, 0x00, 0x38, 0x30, 0x40, 0x00},
{0x02, 0x04, 0x08, 0x10, 0x20, 0x10, 0x08, 0x04, 0x02, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x00, 0x00},
{0x40, 0x20, 0x10, 0x08, 0x04, 0x08, 0x10, 0x20, 0x40, 0x00, 0x00},
{0x3c, 0x42, 0x42, 0x02, 0x04, 0x08, 0x08, 0x00, 0x08, 0x00, 0x00},
{0x3c, 0x42, 0x42, 0x4e, 0x52, 0x56, 0x4a, 0x40, 0x3c, 0x00, 0x00},
{0x18, 0x24, 0x42, 0x42, 0x42, 0x7e, 0x42, 0x42, 0x42, 0x00, 0x00},
{0xfc, 0x42, 0x42, 0x42, 0x7c, 0x42, 0x42, 0x42, 0xfc, 0x00, 0x00},
{0x3c, 0x42, 0x40, 0x40, 0x40, 0x40, 0x40, 0x42, 0x3c, 0x00, 0x00},
{0xfc, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0xfc, 0x00, 0x00},
{0x7e, 0x40, 0x40, 0x40, 0x78, 0x40, 0x40, 0x40, 0x7e, 0x00, 0x00},
{0x7e, 0x40, 0x40, 0x40, 0x78, 0x40, 0x40, 0x40, 0x40, 0x00, 0x00},
{0x3c, 0x42, 0x40, 0x40, 0x40, 0x4e, 0x42, 0x46, 0x3a, 0x00, 0x00},
{0x42, 0x42, 0x42, 0x42, 0x7e, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00},
{0x7c, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00},
{0x1e, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x44, 0x38, 0x00, 0x00},
{0x42, 0x44, 0x48, 0x50, 0x60, 0x50, 0x48, 0x44, 0x42, 0x00, 0x00},
{0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x7e, 0x00, 0x00},
{0x82, 0x82, 0xc6, 0xaa, 0x92, 0x92, 0x82, 0x82, 0x82, 0x00, 0x00},
{0x42, 0x42, 0x62, 0x52, 0x4a, 0x46, 0x42, 0x42, 0x42, 0x00, 0x00},
{0x3c, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00},
{0x7c, 0x42, 0x42, 0x42, 0x7c, 0x40, 0x40, 0x40, 0x40, 0x00, 0x00},
{0x3c, 0x42, 0x42, 0x42, 0x42, 0x42, 0x52, 0x4a, 0x3c, 0x02, 0x00},
{0x7c, 0x42, 0x42, 0x42, 0x7c, 0x50, 0x48, 0x44, 0x42, 0x00, 0x00},
{0x3c, 0x42, 0x40, 0x40, 0x3c, 0x02, 0x02, 0x42, 0x3c, 0x00, 0x00},
{0xfe, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x00},
{0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00},
{0x82, 0x82, 0x44, 0x44, 0x44, 0x28, 0x28, 0x28, 0x10, 0x00, 0x00},
{0x82, 0x82, 0x82, 0x82, 0x92, 0x92, 0x92, 0xaa, 0x44, 0x00, 0x00},
{0x82, 0x82, 0x44, 0x28, 0x10, 0x28, 0x44, 0x82, 0x82, 0x00, 0x00},
{0x82, 0x82, 0x44, 0x28, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x00},
{0x7e, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x40, 0x7e, 0x00, 0x00},
{0x3c, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x3c, 0x00, 0x00},
{0x80, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x02, 0x00, 0x00},
{0x78, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x78, 0x00, 0x00},
{0x10, 0x28, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00},
{0x38, 0x18, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x3c, 0x02, 0x3e, 0x42, 0x46, 0x3a, 0x00, 0x00},
{0x40, 0x40, 0x40, 0x5c, 0x62, 0x42, 0x42, 0x62, 0x5c, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x40, 0x40, 0x42, 0x3c, 0x00, 0x00},
{0x02, 0x02, 0x02, 0x3a, 0x46, 0x42, 0x42, 0x46, 0x3a, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x7e, 0x40, 0x40, 0x3c, 0x00, 0x00},
{0x1c, 0x22, 0x20, 0x20, 0x7c, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x3a, 0x44, 0x44, 0x38, 0x40, 0x3c, 0x42, 0x3c},
{0x40, 0x40, 0x40, 0x5c, 0x62, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00},
{0x00, 0x10, 0x00, 0x30, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00},
{0x00, 0x04, 0x00, 0x0c, 0x04, 0x04, 0x04, 0x04, 0x44, 0x44, 0x38},
{0x40, 0x40, 0x40, 0x44, 0x48, 0x70, 0x48, 0x44, 0x42, 0x00, 0x00},
{0x30, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00},
{0x00, 0x00, 0x00, 0xec, 0x92, 0x92, 0x92, 0x92, 0x82, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x5c, 0x62, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x5c, 0x62, 0x42, 0x62, 0x5c, 0x40, 0x40, 0x40},
{0x00, 0x00, 0x00, 0x3a, 0x46, 0x42, 0x46, 0x3a, 0x02, 0x02, 0x02},
{0x00, 0x00, 0x00, 0x5c, 0x22, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x30, 0x0c, 0x42, 0x3c, 0x00, 0x00},
{0x00, 0x20, 0x20, 0x7c, 0x20, 0x20, 0x20, 0x22, 0x1c, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x44, 0x44, 0x44, 0x44, 0x44, 0x3a, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x44, 0x44, 0x44, 0x28, 0x28, 0x10, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x82, 0x82, 0x92, 0x92, 0xaa, 0x44, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x42, 0x24, 0x18, 0x18, 0x24, 0x42, 0x00, 0x00},
{0x00, 0x00, 0x00, 0x42, 0x42, 0x42, 0x46, 0x3a, 0x02, 0x42, 0x3c},
{0x00, 0x00, 0x00, 0x7e, 0x04, 0x08, 0x10, 0x20, 0x7e, 0x00, 0x00},
{0x0e, 0x10, 0x10, 0x08, 0x30, 0x08, 0x10, 0x10, 0x0e, 0x00, 0x00},
{0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x00},
{0x70, 0x08, 0x08, 0x10, 0x0c, 0x10, 0x08, 0x08, 0x70, 0x00, 0x00},
{0x24, 0x54, 0x48, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00}
};
/* 13x25 font, bottom row first, left pixel in lsb */
static short int font13x25[NUMCHARS][25] = {
  /* */  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000},
  /*!*/  {000000,000000,000000,000000,000000,0x00e0,0x00e0,0x00e0,000000,000000,000000,0x0040,0x0040,0x0040,0x0040,0x0040,0x00e0,0x00e0,0x00e0,0x00e0,0x00e0,0x00e0,0x00e0,0x00e0,0x0040},
  /*"*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,0x0208,0x0208,0x0208,0x0208,0x0208,0x0208,0x0208},
  /*#*/  {000000,000000,000000,000000,000000,000000,0x0208,0x0208,0x0208,0x0208,0x0208,0x0208,0x1fff,0x0208,0x0208,0x0208,0x0208,0x0208,0x1fff,0x0208,0x0208,0x0208,0x0208,0x0208,0x0208},
  /*$*/  {000000,000000,000000,000000,000000,000000,0x0040,0x0040,0x0040,0x03f8,0x0444,0x0842,0x0840,0x0840,0x0440,0x03f8,0x0044,0x0042,0x0042,0x0842,0x0444,0x03f8,0x0040,0x0040,0x0040},
  /*%*/  {000000,000000,000000,000000,000000,000000,0x0c00,0x1200,0x1201,0x0c01,0x0002,0x0004,0x0008,0x0010,0x0020,0x0040,0x0080,0x0100,0x0200,0x0400,0x0800,0x1006,0x1009,0x0009,0x0006},
  /*&*/  {000000,000000,000000,000000,000000,000000,0x1078,0x1084,0x0902,0x0601,0x0601,0x0901,0x1081,0x0042,0x0024,0x0018,0x0018,0x0024,0x0042,0x0042,0x0042,0x0042,0x0024,0x0018,000000},
  /*'*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,0x0001,0x0002,0x0004,0x0008,0x0010,0x0030,0x0078,0x0078,0x0078,0x0030,000000},
  /*(*/  {000000,000000,000000,000000,000000,000000,0x0080,0x0040,0x0020,0x0020,0x0010,0x0008,0x0008,0x0004,0x0004,0x0004,0x0004,0x0004,0x0008,0x0008,0x0010,0x0020,0x0020,0x0040,0x0080},
  /*)*/  {000000,000000,000000,000000,000000,000000,0x0020,0x0040,0x0080,0x0080,0x0100,0x0200,0x0200,0x0400,0x0400,0x0400,0x0400,0x0400,0x0200,0x0200,0x0100,0x0080,0x0080,0x0040,0x0020},
  /***/  {000000,000000,000000,000000,000000,000000,0x0040,0x0040,0x0040,0x1041,0x0842,0x0444,0x0248,0x0150,0x00e0,0x1fff,0x00e0,0x0150,0x0248,0x0444,0x0842,0x1041,0x0040,0x0040,0x0040},
  /*+*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x1fff,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,000000,000000,000000},
  /*,*/  {000000,000000,0x0001,0x0002,0x0004,0x0008,0x0010,0x0030,0x0078,0x0078,0x0078,0x0030,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000},
  /*-*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,0x1fff,000000,000000,000000,000000,000000,000000,000000,000000,000000},
  /*.*/  {000000,000000,000000,000000,000000,000000,000000,0x0038,0x007c,0x007c,0x007c,0x0038,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000},
  /*-/-*/{000000,000000,000000,000000,000000,000000,000000,000000,0x0001,0x0001,0x0002,0x0004,0x0008,0x0010,0x0020,0x0040,0x0080,0x0100,0x0200,0x0400,0x0800,0x1000,0x1000,000000,000000},
  /*0*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1003,0x1005,0x1009,0x1011,0x1021,0x1041,0x1081,0x1101,0x1201,0x1401,0x1801,0x1001,0x0802,0x0404,0x03f8},
  /*1*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0048,0x0070,0x0060,0x0040},
  /*2*/  {000000,000000,000000,000000,000000,000000,0x1fff,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0002,0x03fc,0x0400,0x0800,0x1000,0x1000,0x1000,0x1000,0x1001,0x0802,0x0404,0x03f8},
  /*3*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1000,0x1000,0x1000,0x0800,0x0400,0x03e0,0x0400,0x0800,0x1000,0x1000,0x1000,0x1001,0x0802,0x0404,0x03f8},
  /*4*/  {000000,000000,000000,000000,000000,000000,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x1fff,0x0201,0x0201,0x0202,0x0204,0x0208,0x0210,0x0220,0x0240,0x0280,0x0300,0x0200},
  /*5*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1000,0x1000,0x1000,0x1000,0x1000,0x0800,0x0400,0x03ff,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x1fff},
  /*6*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1001,0x1001,0x1001,0x0801,0x0401,0x03ff,0x0001,0x0001,0x0001,0x0001,0x0002,0x0004,0x0808,0x0410,0x03e0},
  /*7*/  {000000,000000,000000,000000,000000,000000,0x0001,0x0001,0x0001,0x0002,0x0002,0x0004,0x0008,0x0010,0x0020,0x0040,0x0080,0x0100,0x0200,0x0400,0x0800,0x0800,0x1000,0x1000,0x1fff},
  /*8*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8,0x0404,0x0802,0x1001,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8},
  /*9*/  {000000,000000,000000,000000,000000,000000,0x00f8,0x0104,0x0202,0x0400,0x0800,0x1000,0x1000,0x1000,0x1000,0x1ff8,0x1004,0x1002,0x1001,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8},
  /*:*/  {000000,000000,000000,000000,000000,000000,000000,000000,0x0030,0x0078,0x0078,0x0030,000000,000000,000000,000000,000000,000000,0x0030,0x0078,0x0078,0x0030,000000,000000,000000},
  /*;*/  {000000,000000,0x0001,0x0002,0x0004,0x0008,0x0010,0x0030,0x0078,0x0078,0x0078,0x0030,000000,000000,000000,000000,000000,000000,0x0030,0x0078,0x0078,0x0030,000000,000000,000000},
  /*<*/  {000000,000000,000000,000000,000000,000000,0x0200,0x0100,0x0080,0x0040,0x0020,0x0010,0x0008,0x0004,0x0002,0x0001,0x0002,0x0004,0x0008,0x0010,0x0020,0x0040,0x0080,0x0100,0x0200},
  /*=*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,0x1fff,000000,000000,000000,000000,000000,0x1fff,000000,000000,000000,000000,000000,000000},
  /*>*/  {000000,000000,000000,000000,000000,000000,0x0008,0x0010,0x0020,0x0040,0x0080,0x0100,0x0200,0x0400,0x0800,0x1000,0x0800,0x0400,0x0200,0x0100,0x0080,0x0040,0x0020,0x0010,0x0008},
  /*?*/  {000000,000000,000000,000000,000000,0x0040,0x00e0,0x0040,000000,000000,000000,0x0040,0x0040,0x0080,0x0100,0x0200,0x0400,0x0800,0x1000,0x1000,0x1001,0x1001,0x0802,0x0404,0x03f8},
  /*@*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0002,0x0001,0x0001,0x0ee1,0x1111,0x1111,0x1111,0x1111,0x1111,0x12e1,0x1001,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8},
  /*A*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1fff,0x1001,0x1001,0x1001,0x1001,0x0802,0x0802,0x0404,0x0208,0x0110,0x00a0,0x00a0,0x0040},
  /*B*/  {000000,000000,000000,000000,000000,000000,0x03ff,0x0408,0x0808,0x1008,0x1008,0x1008,0x1008,0x0808,0x0408,0x03f8,0x0408,0x0808,0x1008,0x1008,0x1008,0x1008,0x0808,0x0408,0x03ff},
  /*C*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x1001,0x0802,0x0404,0x03f8},
  /*D*/  {000000,000000,000000,000000,000000,000000,0x03ff,0x0408,0x0808,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x1008,0x0808,0x0408,0x03ff},
  /*E*/  {000000,000000,000000,000000,000000,000000,0x1fff,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x007f,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x1fff},
  /*F*/  {000000,000000,000000,000000,000000,000000,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x007f,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x1fff},
  /*G*/  {000000,000000,000000,000000,000000,000000,0x0ff8,0x1004,0x1002,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1f01,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0002,0x1004,0x0ff8},
  /*H*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1fff,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001},
  /*I*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x03f8},
  /*J*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x1e00},
  /*K*/  {000000,000000,000000,000000,000000,000000,0x1001,0x0801,0x0401,0x0201,0x0101,0x0081,0x0041,0x0021,0x0011,0x000f,0x0009,0x0011,0x0021,0x0041,0x0081,0x0101,0x0201,0x0401,0x0801},
  /*L*/  {000000,000000,000000,000000,000000,000000,0x1fff,0x1001,0x1001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001},
  /*M*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1041,0x1041,0x10a1,0x10a1,0x1111,0x1209,0x1209,0x1405,0x1803,0x1001},
  /*N*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1001,0x1001,0x1801,0x1401,0x1201,0x1201,0x1101,0x1081,0x1041,0x1041,0x1021,0x1011,0x1009,0x1009,0x1005,0x1003,0x1001},
  /*O*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8},
  /*P*/  {000000,000000,000000,000000,000000,000000,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x03ff,0x0401,0x0801,0x1001,0x1001,0x1001,0x1001,0x0801,0x0401,0x03ff},
  /*Q*/  {000000,000000,000000,000000,0x0c00,0x0200,0x03f8,0x0494,0x0862,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8},
  /*R*/  {000000,000000,000000,000000,000000,000000,0x1001,0x0801,0x0401,0x0201,0x0101,0x0081,0x0041,0x0021,0x0011,0x03ff,0x0401,0x0801,0x1001,0x1001,0x1001,0x1001,0x0801,0x0401,0x03ff},
  /*S*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1000,0x1000,0x1000,0x0800,0x0400,0x03f8,0x0004,0x0002,0x0001,0x0001,0x0001,0x1001,0x0802,0x0404,0x03f8},
  /*T*/  {000000,000000,000000,000000,000000,000000,0x00e0,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x1041,0x1fff},
  /*U*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001},
  /*V*/  {000000,000000,000000,000000,000000,000000,0x0040,0x0040,0x00a0,0x00a0,0x0110,0x0110,0x0208,0x0208,0x0404,0x0404,0x0802,0x0802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001},
  /*W*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1803,0x1405,0x1405,0x1209,0x1209,0x1111,0x1111,0x10a1,0x1041,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001},
  /*X*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1001,0x0802,0x0802,0x0404,0x0208,0x0110,0x00a0,0x0040,0x00a0,0x0110,0x0208,0x0404,0x0802,0x0802,0x1001,0x1001,0x1001},
  /*Y*/  {000000,000000,000000,000000,000000,000000,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x00a0,0x0110,0x0208,0x0404,0x0802,0x0802,0x1001,0x1001,0x1001,0x1001},
  /*Z*/  {000000,000000,000000,000000,000000,000000,0x1fff,0x0001,0x0001,0x0002,0x0004,0x0004,0x0008,0x0010,0x0020,0x0040,0x0080,0x0100,0x0200,0x0400,0x0400,0x0800,0x1000,0x1000,0x1fff},
  /*[*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x0008,0x03f8},
  /*\*/  {000000,000000,000000,000000,000000,000000,000000,000000,0x1000,0x1000,0x0800,0x0400,0x0200,0x0100,0x0080,0x0040,0x0020,0x0010,0x0008,0x0004,0x0002,0x0001,0x0001,000000,000000},
  /*]*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x0200,0x03f8},
  /*^*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,0x1001,0x0802,0x0404,0x0208,0x0110,0x00a0,0x0040},
  /*_*/  {000000,000000,000000,000000,000000,000000,0x1fff,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000},
  /*`*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,0x0400,0x0200,0x0100,0x0080,0x0040,0x0060,0x00f0,0x00f0,0x00f0,0x0060,000000},
  /*a*/  {000000,000000,000000,000000,000000,000000,0x17f8,0x0804,0x0802,0x0802,0x0802,0x0804,0x0ff8,0x0800,0x0800,0x0800,0x0800,0x0404,0x03f8,000000,000000,000000,000000,000000,000000},
  /*b*/  {000000,000000,000000,000000,000000,000000,0x03f9,0x0405,0x0803,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x0803,0x0405,0x03f9,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001},
  /*c*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x0001,0x0001,0x0001,0x0001,0x0001,0x1001,0x0802,0x0404,0x03f8,000000,000000,000000,000000,000000,000000},
  /*d*/  {000000,000000,000000,000000,000000,000000,0x13f8,0x1404,0x1802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1802,0x1404,0x13f8,0x1000,0x1000,0x1000,0x1000,0x1000,0x1000},
  /*e*/  {000000,000000,000000,000000,000000,000000,0x0ff8,0x0004,0x0002,0x0001,0x0001,0x0001,0x1fff,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8,000000,000000,000000,000000,000000,000000},
  /*f*/  {000000,000000,000000,000000,000000,000000,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x03f8,0x0040,0x0040,0x0040,0x0040,0x0040,0x1040,0x0880,0x0500,0x0200},
  /*g*/  {0x03f8,0x0404,0x0802,0x1001,0x1000,0x1000,0x13f8,0x1404,0x1802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1802,0x1404,0x13f8,000000,000000,000000,000000,000000,000000},
  /*h*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x0803,0x0405,0x03f9,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001},
  /*i*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0070,000000,000000,000000,0x00e0,0x00e0,0x00e0,000000},
  /*j*/  {0x00f0,0x0108,0x0204,0x0402,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0400,0x0700,000000,000000,000000,0x0700,0x0700,0x0700,000000},
  /*k*/  {000000,000000,000000,000000,000000,000000,0x0804,0x0404,0x0204,0x0104,0x0084,0x0044,0x0024,0x0014,0x002c,0x0044,0x0084,0x0104,0x0204,0x0004,0x0004,0x0004,0x0004,0x0004,0x0004},
  /*l*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0070},
  /*m*/  {000000,000000,000000,000000,000000,000000,0x1041,0x1041,0x1041,0x1041,0x1041,0x1041,0x1041,0x1041,0x1041,0x1041,0x08a3,0x0515,0x0209,000000,000000,000000,000000,000000,000000},
  /*n*/  {000000,000000,000000,000000,000000,000000,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x0803,0x0405,0x03f9,000000,000000,000000,000000,000000,000000},
  /*o*/  {000000,000000,000000,000000,000000,000000,0x03f8,0x0404,0x0802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x0802,0x0404,0x03f8,000000,000000,000000,000000,000000,000000},
  /*p*/  {0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x03f9,0x0405,0x0803,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x0803,0x0405,0x03f9,000000,000000,000000,000000,000000,000000},
  /*q*/  {0x1000,0x1000,0x1000,0x1000,0x1000,0x1000,0x13f8,0x1404,0x1802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1802,0x1404,0x13f8,000000,000000,000000,000000,000000,000000},
  /*r*/  {000000,000000,000000,000000,000000,000000,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x0001,0x1001,0x0803,0x0405,0x03f9,000000,000000,000000,000000,000000,000000},
  /*s*/  {000000,000000,000000,000000,000000,000000,0x03fc,0x0402,0x0800,0x0800,0x0800,0x0400,0x03f8,0x0004,0x0002,0x0002,0x0002,0x0804,0x07f8,000000,000000,000000,000000,000000,000000},
  /*t*/  {000000,000000,000000,000000,000000,000000,0x0200,0x0500,0x0880,0x1040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x07fc,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040},
  /*u*/  {000000,000000,000000,000000,000000,000000,0x13f8,0x1404,0x1802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,000000,000000,000000,000000,000000,000000},
  /*v*/  {000000,000000,000000,000000,000000,000000,0x0040,0x00a0,0x0110,0x0208,0x0404,0x0802,0x0802,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,000000,000000,000000,000000,000000,000000},
  /*w*/  {000000,000000,000000,000000,000000,000000,0x0208,0x0514,0x08a2,0x08a2,0x1041,0x1041,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,000000,000000,000000,000000,000000,000000},
  /*x*/  {000000,000000,000000,000000,000000,000000,0x1001,0x0802,0x0404,0x0208,0x0110,0x00a0,0x0040,0x00a0,0x0110,0x0208,0x0404,0x0802,0x1001,000000,000000,000000,000000,000000,000000},
  /*y*/  {0x03f8,0x0404,0x0802,0x1001,0x1000,0x1000,0x1000,0x1000,0x1ff8,0x1004,0x1002,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,0x1001,000000,000000,000000,000000,000000,000000},
  /*z*/  {000000,000000,000000,000000,000000,000000,0x1fff,0x0002,0x0004,0x0008,0x0010,0x0020,0x0040,0x0080,0x0100,0x0200,0x0400,0x0800,0x1fff,000000,000000,000000,000000,000000,000000},
  /*{*/  {000000,000000,000000,000000,000000,000000,0x0600,0x0100,0x0080,0x0040,0x0040,0x0040,0x0040,0x0040,0x0020,0x0010,0x0020,0x0040,0x0040,0x0040,0x0040,0x0040,0x0080,0x0100,0x0600},
  /*|*/  {000000,000000,000000,000000,000000,000000,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,000000,000000,000000,000000,000000,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040,0x0040},
  /*}*/  {000000,000000,000000,000000,000000,000000,0x000c,0x0010,0x0020,0x0040,0x0040,0x0040,0x0040,0x0040,0x0080,0x0100,0x0080,0x0040,0x0040,0x0040,0x0040,0x0040,0x0020,0x0010,0x000c},
  /*~*/  {000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,000000,0x0600,0x0900,0x1080,0x1041,0x0021,0x0012,0x000c}
};
/******************************************************************************/
static void PPM_char0(char c) { /* PPM_char output a small character */

  int             i,j,k;
  char            wrd;
/*
HARDWARE first draft
check to make sure do not exceed arrays bounds
make sure in proper location
hardcodes to just use font 8x13 (and now 13x25), should go to any font bitmap
rotate font
*/
        k=(int)c-32; /* get which row to use out of the bitpatterns */
        if(k < 0 || k > 95 ){
           return;
        }
        /* fprintf(stderr,"PPM_char %c %d\n",c,k); */
        for (i=0;i<11;i++){
           wrd=font8x13[k][i];
           /* fprintf(stderr,"x y= %d %d\n",vdevice.cpVx,vdevice.cpVy); */
           for ( j=0; j<8; j++ ){
              if ((wrd & (0x80 >> j)) != 0){
                  SET_PIXEL((int)(j+vdevice.cpVx),(int)(vdevice.cpVy-i+13));
                  /* fprintf(stderr,"#"); */
              }else{
                  /* fprintf(stderr,"."); */
              }
           }
           /* fprintf(stderr,"0x%2.2x\n",wrd); */
        }
        /* fprintf(stderr,"===============\n"); */
        vdevice.cpVx+= 8;
        GLOBAL_lastx=vdevice.cpVx;
        GLOBAL_lasty=vdevice.cpVy;
        GLOBAL_drawn = DRAWN;
}
/******************************************************************************/
static void PPM_char1(char c) {
  int             i,j,k;
  short int       wrd;
/* #define FROW 25 */
/* #define FCOL 9 */
/* #define NUMCHARS 95 */
        k=(int)c-32; /* get which row to use out of the bitpatterns */
        if(k < 0 || k > 95 ){
           return;
        }
        for (i=0;i<25;i++){
           wrd=font13x25[k][i];
           for (j=0;j<13;j++){
              if (((wrd  >> j) & 1) != 0){
                  SET_PIXEL((int)(j+vdevice.cpVx),(int)(vdevice.cpVy+i));
                  /* fprintf(stdout,"#"); */
              }else{
                  /* fprintf(stdout,"."); */
              }
           }
           /* fprintf(stdout," 0x%4.4X\n",wrd); */
        }
           /* fprintf(stdout,"%d ----------\n",k); */
        vdevice.cpVx+= 15;
        GLOBAL_lastx=vdevice.cpVx;
        GLOBAL_lasty=vdevice.cpVy;
        GLOBAL_drawn = DRAWN;
}
/******************************************************************************/
static int PPM_char(char c) { /* output a hardware character */
   if (GLOBAL_font == 0){
       PPM_char0(c);
   }else{
       PPM_char1(c);
   }
   return(0);
}
/******************************************************************************/
static int PPM_string(char *s) { /* output a string.  */
        int i;

        if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
                GLOBAL_lastx=vdevice.cpVx;
                GLOBAL_lasty=vdevice.cpVy;
        }

        for(i=0; s[i]!='\0'; i++){
          PPM_char(s[i]);
        }
        return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry PPMdev = {
   "ppm",       /* name of device */
   "large",     /* name of large font */
   "small",     /* name of small font */
   noop,        /* Set drawing in back buffer */
   PPM_char,    /* Draw a hardware character */
   noop,        /* Check if a key was hit */
   PPM_clear,   /* Clear the screen to current color */
   PPM_color,   /* Set current color */
   PPM_draw,    /* Draw a line */
   PPM_exit,    /* Exit graphics */
   PPM_fill,    /* Fill a polygon */
   PPM_font,    /* PPM_font Set hardware font */
   noop,        /* Set drawing in front buffer */
   noop,        /* Wait for and get the next key hit */
   PPM_init,    /* Initialize the device */
   noop2,       /* Get mouse/cross hair position */
   PPM_mapcolor,/* Set color indices */
   PPM_setlw,   /* Set line width */
   PPM_string,  /* Draw a hardware string */
   noop,        /* Swap front and back buffers */
   noop         /* Syncronize the display */
};
/******************************************************************************/
int _PPM_draw_devcpy(void) {
   vdevice.dev = PPMdev;
   vdevice.dev.Vinit = PPM_init;
   vdevice.dev.devname = "ppm";
   GLOBAL_driver = P3;
   return(0);
}
/******************************************************************************/
int _P3_draw_devcpy(void) {
   vdevice.dev = PPMdev;
   vdevice.dev.Vinit = PPM_init;
   vdevice.dev.devname = "p3";
   GLOBAL_driver = P3;
   return(0);
}
/******************************************************************************/
int _P6_draw_devcpy(void) {
   vdevice.dev = PPMdev;
   vdevice.dev.Vinit = PPM_init;
   vdevice.dev.devname = "p6";
   GLOBAL_driver = P6;
   return(0);
}
/******************************************************************************/
int _CHAR_draw_devcpy(void) {
   vdevice.dev = PPMdev;
   vdevice.dev.Vinit = PPM_init;
   vdevice.dev.devname = "char";
   GLOBAL_driver = CHAR;
   return(0);
}
/*******************************************************************************/

/* this code is licensed as public domain */

#ident "@(#)M_DRAW:source - driver/usemap.c - M_DRAW driver for helping to create HTML image maps"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Jun 2010"
/*******************************************************************************/
/*

This driver writes out the edges of any polygon in a format that can be used
with an HTML image map; if the same sizes are used a plot generated with the
ppm driver; you will have clickable regions in your pixmap when converted to
a GIF image.

If the polygons overlap you need to reverse the order of the polygon
definitions in the output file. The numeric field in the <AREA> titles 
should help.

*/
/******************************************************************************/
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

extern FILE *_draw_outfile();
extern FILE *draw_fp;

#define byte unsigned char

#define MAX(x,y)  ((x) > (y) ? (x) : (y))
#define MIN(x,y)  ((x) < (y) ? (x) : (y))
#define ABS(x)    ((x) < 0 ? -(x) : (x))

#define FLIPY(y) ((int)((vdevice.sizeSy)-(y)))

static int X_SIZE, Y_SIZE, LAST; /* size of graphics array */

static int GLOBAL_font = 0;
static int GLOBAL_color = 0;
static int GLOBAL_lastx, GLOBAL_lasty;     /* position of last draw */
static int POLYGON_COUNT=0;
static int PAGE_COUNT=1;

typedef struct {
   int     r, g, b;
} ColorTable;

#define    CMAPSIZE        8192
#define    CMAPDEPTH       13

static ColorTable coltab[CMAPSIZE];
/******************************************************************************/
static int USEMAP_setlw(int w){ /* Set the line width */
   return(0);
}
/******************************************************************************/
static int USEMAP_color(int col){ /* change the current color */
   if(col < 0){
      /* set linewidth */
   } else{
      GLOBAL_color = ABS(col % CMAPSIZE) ;
   }
   return(0);
}
/******************************************************************************/
static int USEMAP_mapcolor(int indx, int r, int g, int b){  /* set values in pseudo color map.  */
   if (indx < CMAPSIZE && indx >= 0) {
      coltab[indx].r = ABS(r % CMAPSIZE) ;
      coltab[indx].g = ABS(g % CMAPSIZE) ;
      coltab[indx].b = ABS(b % CMAPSIZE) ;
   }
   return(0);
}
/******************************************************************************/
static int USEMAP_init(void) {
   int prefx, prefy, prefxs, prefys;
   int i;
   /* see if a size was user-specified using the prefsize procedure */
   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
   if (prefxs != -1 ) {
      if (prefys <= 0 ){
         fprintf(stderr,"*USEMAP_init* y size of %d set to 400\n",prefys);
         prefys = 400;
      }else{
         vdevice.sizeSy = prefys;
      }
      if (prefxs <= 0 ){
         fprintf(stderr,"*USEMAP_init* y size of %d set to 600\n",prefys);
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
   vdevice.sizeX = vdevice.sizeY = MIN(vdevice.sizeSy,vdevice.sizeSx);
   X_SIZE=vdevice.sizeSx;
   Y_SIZE=vdevice.sizeSy;
   LAST=X_SIZE*Y_SIZE-1;

   vdevice.depth = CMAPDEPTH;

   draw_fp = _draw_outfile();

   /* Cause scaling to be 0 to maxX maxY: prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy */

   GLOBAL_lastx = -1111111;
   GLOBAL_lasty = -1111111;

   USEMAP_mapcolor(0, 255, 255, 255);
   USEMAP_mapcolor(1, 255, 0, 0);
   USEMAP_mapcolor(2, 0, 255, 0);
   USEMAP_mapcolor(3, 255, 255, 0);
   USEMAP_mapcolor(4, 0, 0, 255);
   USEMAP_mapcolor(5, 255, 0, 255);
   USEMAP_mapcolor(6, 0, 255, 255);
   USEMAP_mapcolor(7, 0, 0, 0);

   for(i=8; i<CMAPSIZE; i++){
      USEMAP_mapcolor(i, 255, 255, 255);
   }

   fprintf(draw_fp,"<html>\n");
   fprintf(draw_fp,"<!-- =======================================================================-->\n");
   fprintf(draw_fp,"</--\n");
   fprintf(draw_fp,"   This file lists the polygons in your plot as HTML image maps.\n");
   fprintf(draw_fp,"   if the same plot is generated with the ppm driver\n");
   fprintf(draw_fp,"   and that is converted to a GIF file you will have n");
   fprintf(draw_fp,"   an HTML document with clickable regions in it.\n");
   fprintf(draw_fp,"\n");
   fprintf(draw_fp,"   If the polygons overlap you need to reverse the order\n");
   fprintf(draw_fp,"   of polygon definitions in the output file.\n");
   fprintf(draw_fp,"-->\n");
   fprintf(draw_fp,"<!-- =======================================================================-->\n");
   fprintf(draw_fp,"<head>\n");
      fprintf(draw_fp,"  <meta http-equiv=\"Content-Language\" content=\"en\" />\n");
      fprintf(draw_fp,"  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=us-ascii\" />\n");
      fprintf(draw_fp,"  <meta name=\"description\" content=\"polygons mapped to HTML images \"/>\n");
      fprintf(draw_fp,"  <meta name=\"author\" content=\"John S. Urban\">\n");
      fprintf(draw_fp,"  <meta name=\"date\" content=\"2010-06-11\">\n");
      fprintf(draw_fp,"  <meta name=\"keywords\" content=\"M_DRAW, USEMAP, IMG, Image Maps, HTML\">\n");
      fprintf(draw_fp,"  <meta name=\"generator\" content=\"M_DRAW graphics library\" />\n");
      fprintf(draw_fp,"  <title>Image Map</title>\n");
      fprintf(draw_fp,"  <style type=\"text/css\">\n");
      fprintf(draw_fp,"  </style>\n");
      fprintf(draw_fp,"  <script type=\"text/javascript\">\n");
      fprintf(draw_fp,"//||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||\n");
      fprintf(draw_fp,"// do something when different regions are clicked; could be any ECMAScript function, or simply use URLs instead )\n");
      fprintf(draw_fp,"    function dosomething(elementid,string){\n");
      fprintf(draw_fp,"       alert( string );\n");
      fprintf(draw_fp,"    }\n");
      fprintf(draw_fp,"//||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||\n");
      fprintf(draw_fp,"  </script>\n");
   fprintf(draw_fp,"</head>\n");
   fprintf(draw_fp,"<!-- =======================================================================-->\n");
   fprintf(draw_fp,"<body>\n");

   return(1);
}
/******************************************************************************/
static int USEMAP_fill(int n, int x[], int y[]) { /* "fill" a polygon */
   int     i;
   int icount = 0;

   if(POLYGON_COUNT == 0){

      fprintf(draw_fp,"<div><img id=\"IMAGE%d\" ",PAGE_COUNT);
         fprintf(draw_fp," src=\"IMAGE%d.gif\" ",PAGE_COUNT);
         fprintf(draw_fp," alt=\"Page %d\" ",PAGE_COUNT);
         fprintf(draw_fp," usemap=\"#PAGE%d\" ",PAGE_COUNT);
         fprintf(draw_fp," height=\"%d\" width=\"%d\" />\n",vdevice.sizeSx,vdevice.sizeSy);

      fprintf(draw_fp,"<map id=\"PAGE%d\" name=\"PAGE%d\">\n",PAGE_COUNT,PAGE_COUNT);
   }
   POLYGON_COUNT++;

   /* update current position if needed */
   GLOBAL_lastx=x[0];
   GLOBAL_lasty=y[0];

   fprintf(draw_fp,"<area title=\"POLYGON: %d color:%d %d %d\" alt=\"POLYGON%d region\" shape=\"poly\" coords=\"",
      POLYGON_COUNT,
      coltab[GLOBAL_color].r, coltab[GLOBAL_color].g, coltab[GLOBAL_color].b,
      POLYGON_COUNT);

   for (i = 1; i < n; i++) {
      icount++;
      if(i > 1){ fprintf(draw_fp,","); }                     /* separate point values with a comma */
      /* if(icount > 10){ fprintf(draw_fp,"\n   "); icount=0;}*/  /* add line breaks once in a while */
      fprintf(draw_fp,"%d,%d",x[i],FLIPY(y[i]));             /* draw outline across graphics array */
   }
   if ( x[n-1] != x[0] || y[n-1] != y[0] ){
   /* close the polygon if it is not closed */
      fprintf(draw_fp,",%d,%d",x[0],FLIPY(y[0])); /* draw outline across graphics array */
   }
   fprintf(draw_fp,"\" onClick=\"dosomething('PAGE%d','clicked POLYGON%d');\">\n",PAGE_COUNT,POLYGON_COUNT);

   /* update current position */
   GLOBAL_lastx = vdevice.cpVx = x[n - 1];
   GLOBAL_lasty = vdevice.cpVy = y[n - 1];

   return(0);
}
/******************************************************************************/
static int USEMAP_draw(int x, int y) { /* print the commands to draw a line from the current graphics position to (x, y).  */

   if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
      GLOBAL_lastx=vdevice.cpVx;
      GLOBAL_lasty=vdevice.cpVy;
   }
   return(0);
}
/*******************************************************************************/
static int USEMAP_exit(void) { /* exit from draw printing the command to flush the buffer.  */
   if(POLYGON_COUNT != 0){
      fprintf(draw_fp,"<area title=\"BACKGROUND\" alt=\"background\" shape=\"rect\" coords=\"0,0,%d,%d\" onclick=\"dosomething('PAGE%d','you clicked BACKGROUND');\" />\n",
         vdevice.sizeSy,vdevice.sizeSx,PAGE_COUNT);
      fprintf(draw_fp,"</map>\n</div>\n");
   }
   fprintf(draw_fp,"</body>\n");
   fprintf(draw_fp,"</html>\n");
   if (draw_fp != stdout && draw_fp != stderr ){
	        fflush(draw_fp);
	        if(vdevice.writestoprocess == 2){
		   pclose(draw_fp);
		}else{
                   fclose(draw_fp);
		}
   }
   return(0);
}
/******************************************************************************/
static int USEMAP_clear(void) { /* flush current page and clear graphics array */
   if(POLYGON_COUNT != 0){
      fprintf(draw_fp,"<area title=\"BACKGROUND\" alt=\"background\" shape=\"rect\" coords=\"0,0,%d,%d\" onclick=\"dosomething('PAGE%d','you clicked BACKGROUND');\" />\n",
         vdevice.sizeSy,vdevice.sizeSx,PAGE_COUNT);
      fprintf(draw_fp,"</map>\n</div>\n");
   }
   PAGE_COUNT++;
   POLYGON_COUNT=0;
   return(0);
}
/******************************************************************************/
static int USEMAP_font(char *fontname) { /* load in large or small */
        if (strcmp(fontname, "small") == 0) {
                vdevice.hwidth = 8.00; /* Size in plotter resolution units */
                vdevice.hheight = 13.0;
                GLOBAL_font = 0;
        } else if (strcmp(fontname, "large") == 0) {
                vdevice.hwidth = 15.00;
                vdevice.hheight = 39.00;
                GLOBAL_font = 1;
		draw_font("futura.m"); /* use software till add second font later */
        } else
                return(0);

        return(1);
}
/******************************************************************************/
static int USEMAP_char(char c) { /* output a hardware character */
  if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
     GLOBAL_lastx=vdevice.cpVx;
     GLOBAL_lasty=vdevice.cpVy;
  }
     GLOBAL_lastx=vdevice.cpVx+vdevice.hwidth;
   return(0);
}
/******************************************************************************/
static int USEMAP_string(char *s) { /* output a string.  */

        char c;

        if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
                GLOBAL_lastx=vdevice.cpVx;
                GLOBAL_lasty=vdevice.cpVy;
        }

        while ((c = *s++)){
           GLOBAL_lastx=vdevice.cpVx+vdevice.hwidth;
           switch(c) {
           case '(':
              break;
           case ')':
              break;
           case '\\':
              break;
           default:
              break;
           }
        }
        return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry USEMAPdev = {
   "usemap",       /* name of device */
   "large",        /* name of large font */
   "small",        /* name of small font */
   noop,           /* Set drawing in back buffer */
   USEMAP_char,    /* Draw a hardware character */
   noop,           /* Check if a key was hit */
   USEMAP_clear,   /* Clear the screen to current color */
   USEMAP_color,   /* Set current color */
   USEMAP_draw,    /* Draw a line */
   USEMAP_exit,    /* Exit graphics */
   USEMAP_fill,    /* Fill a polygon */
   USEMAP_font,    /* USEMAP_font Set hardware font */
   noop,           /* Set drawing in front buffer */
   noop,           /* Wait for and get the next key hit */
   USEMAP_init,    /* Initialize the device */
   noop2,          /* Get mouse/cross hair position */
   USEMAP_mapcolor,/* Set color indices */
   USEMAP_setlw,   /* Set line width */
   USEMAP_string,  /* Draw a hardware string */
   noop,           /* Swap front and back buffers */
   noop            /* Syncronize the display */
};
/******************************************************************************/
int _USEMAP_draw_devcpy(void) {
   vdevice.dev = USEMAPdev;
   vdevice.dev.Vinit = USEMAP_init;
   vdevice.dev.devname = "usemap";
   return(0);
}
/******************************************************************************/

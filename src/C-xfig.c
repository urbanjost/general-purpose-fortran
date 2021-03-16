/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/xfig.c - M_DRAW driver for xfig(1) files"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0"

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "draw.h"

#define XSPACE_SIZE      10200
#define YSPACE_SIZE      13200

#define MAXCOLOR        256
static int MAXCOLOR_used;

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))

#define FLIPY(y)        ((vdevice.sizeSy*page)-(y))


extern FILE     *_draw_outfile();

static int      page = 1;
static int      drawn = 0; /* has this page been drawn on yet? */
static int      curcol = 0;  /* black */
static int      xfiglstx = -1, xfiglsty = -1; /* last (x, y) drawn */
static int      xfigfont=14;

extern float hardwidth[128];

struct rgb_color{
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
};

static struct rgb_color carray[MAXCOLOR];

extern  FILE     *draw_fp;

/* How to convert degrees to radians */
#ifndef PI
#define PI      3.14159265358979323846264338327950288419716939937510
#endif
#define d2r(x)     ((x) * PI / 180.0)
#define r2d(x)     ((x) * 180.0 / PI)

static int xfig_string(char *s);

static int      curwid = 1; /* Current pen width */
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value */
static int xfig_mapcolor(int i, int r, int g, int b) {
        if(i >= MAXCOLOR ){
           return(-1);
        }
        carray[i].red = (unsigned short)(r);
        carray[i].green = (unsigned short)(g);
        carray[i].blue = (unsigned short)(b);
        MAXCOLOR_used = MAX(MAXCOLOR_used,i+1);
        /*
           object_code=0
           color_field=32 to 543 (512 total)
        */
        if(!drawn){
          fprintf(draw_fp,"0 %d #%2.2x%2.2x%2.2x\n",i+32,r,g,b);
        }else{
          fprintf(draw_fp,"#0 %d #%2.2x%2.2x%2.2x\n", i+32,r,g,b);
          fprintf(stderr,"xfig only supports mapcolor before any drawing\n");
        }
        return(0);
}
/******************************************************************************/
/*
 * xfig_init
 *
 *      Set up the environment. Returns 1 on success.
 */
static int xfig_init(void) {
        int i; draw_fp = _draw_outfile();
        int prefx, prefy, prefxs, prefys;

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        } else{
           vdevice.sizeSy = YSPACE_SIZE;  /* size in resolution rasters */
           vdevice.sizeSx = XSPACE_SIZE;  /* size in resolution rasters */
           vdevice.sizeX = vdevice.sizeY = MIN(XSPACE_SIZE,YSPACE_SIZE); /* current viewport to use */
        }

        fprintf(draw_fp,"#FIG 3.2\n");
        fprintf(draw_fp,"Portrait\n");
        fprintf(draw_fp,"Center\n");
        fprintf(draw_fp,"Inches\n");
        fprintf(draw_fp,"Letter\n");
        fprintf(draw_fp,"100\n");
        fprintf(draw_fp,"Multiple\n");
        fprintf(draw_fp,"-2\n");
        fprintf(draw_fp,"#xfig file from M_DRAW\n");
        fprintf(draw_fp,"1200 2\n");
        fprintf(draw_fp,"# PAGESIZE 0 0 %d %d\n",vdevice.sizeX,vdevice.sizeY);

        vdevice.depth = 8;

        for (i=0; i < MAXCOLOR; i++){
           carray[i].red=0;
           carray[i].green=0;
           carray[i].blue=0;
        }

        drawn = 0; /* has this page been drawn on yet? */

        /* initial color table */
        xfig_mapcolor(0, 255, 255, 255);
        xfig_mapcolor(1, 255,   0,   0);
        xfig_mapcolor(2,   0, 255,   0);
        xfig_mapcolor(3, 255, 255,   0);
        xfig_mapcolor(4,   0,   0, 255);
        xfig_mapcolor(5, 255,   0, 255);
        xfig_mapcolor(6,   0, 255, 255);
        xfig_mapcolor(7,   0,   0,   0);
        xfig_mapcolor(8, 155,   0,   0);
        xfig_mapcolor(9,   0, 155,   0);
        xfig_mapcolor(10, 155, 255, 255);
        xfig_mapcolor(11, 155, 155,   0);
        xfig_mapcolor(12,   0,   0, 155);
        xfig_mapcolor(13, 155,   0, 155);
        xfig_mapcolor(14,   0, 155, 155);
        xfig_mapcolor(15, 100, 100, 100);

        MAXCOLOR_used = 16;
        page = 1;
        curcol = 0;  /* black */
        xfiglstx = -1, xfiglsty = -1; /* last (x, y) drawn */
        return(1);
}
/******************************************************************************/
static int xfig_exit(void) { /* Flush remaining data and close output file if necessary.  */
   if(drawn){
      fprintf(draw_fp,"-6\n"); /* END PAGE */
   }
   fprintf(draw_fp,"# EXIT\n");  /* end page */
   fflush(draw_fp);
   if (draw_fp != stdout && draw_fp != stderr ) {
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
#define L_OBJECT_TYPE 2
#define L_SUB_TYPE    1
int L_LINE_STYLE;  /* -1 default, 0 solid, 1 dashed, 2 dotted, 3 dash-dot, 4 dash dot dot, 5 dash dot dot dot */

int     L_THICKNESS;
int     L_PEN_COLOR;
int     L_FILL_COLOR;

#define L_DEPTH 0
#define L_PEN_STYLE  -1
#define L_AREA_FILL -1
#define L_STYLE_VAL 0.000
#define L_JOIN_STYLE 1

#define L_CAP_STYLE 1
#define L_RADIUS -1
#define L_FORWARD_ARROW 0
#define L_BACKWARD_ARROW 0

#define L_NPOINTS 2

static int xfig_draw(int x, int y) {               /* draw to an x, y point.  */

   if(!drawn){ /* START PAGE */
      fprintf(draw_fp,"6 0 %d %d %d\n",FLIPY(0),vdevice.sizeX,FLIPY(vdevice.sizeY));
      drawn = 1;
   }
   /*
   fprintf(draw_fp,"# VECTOR\n");
   */

   L_THICKNESS=curwid*vdevice.sizeX*2/1200/80/3;
   L_THICKNESS=MAX(1,L_THICKNESS);
   L_PEN_COLOR=(curcol%MAXCOLOR_used)+32;
   L_FILL_COLOR=(curcol%MAXCOLOR_used)+32;
   L_LINE_STYLE=curcol/MAXCOLOR_used;

   fprintf(draw_fp,"%d %d %d %d %d %d %d %d %d %f %d %d %d %d %d %d\n",
      L_OBJECT_TYPE,L_SUB_TYPE, L_LINE_STYLE,
      L_THICKNESS,  L_PEN_COLOR,L_FILL_COLOR,
      L_DEPTH,      L_PEN_STYLE,L_AREA_FILL,    L_STYLE_VAL,     L_JOIN_STYLE,
      L_CAP_STYLE,  L_RADIUS,   L_FORWARD_ARROW,L_BACKWARD_ARROW,
      L_NPOINTS);

   if (xfiglstx != vdevice.cpVx || xfiglsty != vdevice.cpVy) {
   }

   fprintf(draw_fp," %d %d", vdevice.cpVx, FLIPY(vdevice.cpVy) );  /* move */
   fprintf(draw_fp," %d %d\n", x, FLIPY(y) ); /* draw */

   xfiglstx = x;
   xfiglsty = y;
   return(0);
}
/******************************************************************************/
/* load in small or large - could be improved. Radically KLUDGED; made SoftText extern  */
static int xfig_font(char *fontname) {
   int i;
   float rat;
   /* assuming vdevice.hwidth is the desired width of the reference character,
    * this is a list of percentages of the other character widths.
    */
   static int helvetica_w[128] = {
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,
      27,33,43,55,55,88,72,28,33,33,39,58,27,33,28,28,55,55,55,55,55,55,55,55,55,55,
      34,34,58,58,58,61,97,
      73,73,73,73,66,61,78,72,28,55,73,61,83,72,78,66,78,72,67,61,72,66,94,66,66,61,
      33,28,33,58,55,28,
      55,61,55,61,55,33,61,61,28,28,55,28,88,61,61,61,61,39,55,33,61,55,78,55,55,50,
      39,28,39,58,0
   };

   vdevice.attr->a.softtext = SOFTHARDWARE;
   /* textsize will be obeyed after the font is set
    * maybe should read current software size and convert virtual
    * to device instead of resetting each time */

      if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
         vdevice.hwidth=11.0*300.0/72.0;
         vdevice.hheight=11.0*300.0/72.0;
      }

   if (strcmp(fontname, "small") == 0) {
      xfigfont=14; /* font type */
      rat=0.55; /* Kludge Factor */

      for (i = 0; i < 128; i++){
         hardwidth[i]=1.00 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
         }
   } else if (strcmp(fontname, "large") == 0) {
      xfigfont=18; /* font type */
      rat=1.26; /* Kludge Factor */
      rat=0.90; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=((float)helvetica_w[i])/100.0 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
      }
   } else{
      return(0);
   }

   return(1);
}
/******************************************************************************/
static int xfig_clear(void) {                  /* xfig_clear - Erase the plot */
   if (drawn) {

      fprintf(draw_fp,"-6\n"); /* END PAGE */
      fprintf(draw_fp,"# END PAGE %d\n",page);
      page=page+1;
      fprintf(draw_fp,"# START PAGE %d\n",page);
      /* BGCOLOR */
      /*
      fprintf(draw_fp,"~ %d %d %d\n",
         (carray[curcol].red+1)*256-1,
         (carray[curcol].green+1)*256-1,
         (carray[curcol].blue+1)*256-1);
       */
   }
   drawn = 0;
   return(0);
}
/******************************************************************************/
static int xfig_setlw(int rasters) {     /* Change the thickness of the lines */
   curwid=ABS(rasters);
   return(0);
}
/******************************************************************************/
static int xfig_color(int col) {             /* Change the color of the lines */
   if (col > MAXCOLOR){
      return(0);
   }else if(col < 0 ){
      xfig_setlw(ABS(col));
      return(0);
   }
   curcol = col%MAXCOLOR;
   return(0);
}
/******************************************************************************/
static int xfig_char(char c) {                           /* Output a character */
  char  s[2];
  s[0] = c; s[1]='\0';
  xfig_string(s);
  return(0);
}
/******************************************************************************/
/* output a character string using current character size and rotation angle. */
static int xfig_string(char *s) {

   if(!drawn){ /* START PAGE */
      fprintf(draw_fp,"6 0 %d %d %d\n",FLIPY(0),vdevice.sizeX,FLIPY(vdevice.sizeY));
      drawn = 1;
   }

   if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
      fprintf(stderr,"*xfig_string* ERROR: ZERO SIZE CHARACTERS\n");
      vdevice.hwidth=11.0*300.0/72.0;
      vdevice.hheight=11.0*300.0/72.0;
   }

   fprintf(draw_fp,"4 ");                             /* OBJECT   text */
   fprintf(draw_fp,"0 ");                             /* SUB_TYPE left justified */
   fprintf(draw_fp," %d ",(curcol%MAXCOLOR_used)+32); /* COLOR  */
   fprintf(draw_fp," %d ",(curcol%MAXCOLOR_used)+32); /* DEPTH  */
   fprintf(draw_fp," -1 ");                           /* PEN_STYLE  */
   fprintf(draw_fp," %d ",xfigfont                 ); /* FONT  */
   fprintf(draw_fp," %f ",vdevice.hwidth/1200.0*72.); /* FONT SIZE IN POINTS */
   fprintf(draw_fp," %f ",                            /* ANGLE IN RADIANS */
      atan2((double)vdevice.attr->a.textsin,(double)vdevice.attr->a.textcos));
   if( xfigfont == 0){
      fprintf(draw_fp," %d ",4                        ); /* FONT FLAGS  */
      fprintf(draw_fp," %f ",vdevice.hheight          ); /* HEIGHT      */
      fprintf(draw_fp," %f ",vdevice.hwidth*strlen(s) ); /* LENGTH      */
   }else{
      fprintf(draw_fp," %d ",4                        ); /* FONT FLAGS  */
      fprintf(draw_fp," %f ",vdevice.hheight          ); /* HEIGHT      */
      fprintf(draw_fp," %f ",draw_strlength(s)       ); /* LENGTH      */
   }
   fprintf(draw_fp," %d ",vdevice.cpVx      );         /* X             */
   fprintf(draw_fp," %d ",FLIPY(vdevice.cpVy));        /* Y             */
   fprintf(draw_fp,"%s\\001\n",s);                     /* STRING        */

   xfiglstx = xfiglsty = -1;
   return(0);
}
/******************************************************************************/

#define OBJECT_TYPE 2
#define SUB_TYPE 3
#define LINE_STYLE 0

#define THICKNESS 0
#define PEN_COLOR -1
int     FILL_COLOR;

#define DEPTH 0
#define PEN_STYLE  -1
int AREA_FILL;
#define STYLE_VAL 0.000
#define JOIN_STYLE 1

#define CAP_STYLE 1
#define RADIUS 0
#define FORWARD_ARROW 0
#define BACKWARD_ARROW 0

int     NPOINTS;

static int xfig_fill(int n, int x[], int y[]) {  /* fill and draw the polygon */
   int     i;

   if(!drawn){ /* START PAGE */
      fprintf(draw_fp,"6 0 %d %d %d\n",FLIPY(0),vdevice.sizeX,FLIPY(vdevice.sizeY));
      drawn = 1;
   }

   fprintf(draw_fp,"# POLYGON\n");

   /*
   THICKNESS=curwid*vdevice.sizeX*2/1200/80/3;
   THICKNESS=MAX(1,L_THICKNESS);
   PEN_COLOR=(curcol%MAXCOLOR_used)+32;
   */
   AREA_FILL=curcol/MAXCOLOR_used;

   if(AREA_FILL == 0){
      AREA_FILL=20;
   }else{
      AREA_FILL=40+MIN(AREA_FILL,22);
   }

   FILL_COLOR=(curcol%MAXCOLOR_used)+32;
   NPOINTS=n+1;

   fprintf(draw_fp,"%d %d %d %d %d %d %d %d %d %f %d %d %d %d %d %d\n",
      OBJECT_TYPE,SUB_TYPE,LINE_STYLE,
      THICKNESS,PEN_COLOR,FILL_COLOR,
      DEPTH,PEN_STYLE,AREA_FILL,STYLE_VAL,JOIN_STYLE,
      CAP_STYLE,RADIUS,FORWARD_ARROW,BACKWARD_ARROW,
      NPOINTS);
   for (i = 0; i < n; i++)
   {
      fprintf(draw_fp," %d %d\n", x[i], FLIPY(y[i]));
   }
   fprintf(draw_fp," %d %d\n# END POLYGON\n", x[0], FLIPY(y[0]));
   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];
   xfiglstx = xfiglsty = -1;
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry xfigdev = {
        "xfig",         /* name of device */
        "large",        /* name of large font */
        "small",        /* name of small font */
        noop,           /* set drawing into back buffer */
        xfig_char,      /* draw a hardware character */
        noop,           /* check if a key was hit */
        xfig_clear,     /* clear screen to current color */
        xfig_color,     /* set current color */
        xfig_draw,      /* draw a line */
        xfig_exit,      /* exit graphics */
        xfig_fill,      /* fill a polygon */
        xfig_font,      /* set hardware font */
        noop,           /* set drawing into front buffer */
        noop,           /* wait for and get the next key hit */
        xfig_init,      /* initialize the device */
        noop2,          /* get mouse/crosshair position */
        xfig_mapcolor,  /* set color indices */
        xfig_setlw,     /* Set line width */
        xfig_string,    /* draw a hardware string */
        noop,           /* swap front and back buffers */
        noop            /* syncronize the display */
};
/******************************************************************************/
int _XFIG_draw_devcpy(void) {      /* copy the xfig plot device into vdevice.dev.  */
        vdevice.dev = xfigdev;
        return(0);
}
/******************************************************************************/

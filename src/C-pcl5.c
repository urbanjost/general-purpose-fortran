/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/pcl5.c - M_DRAW driver for PCL5 and HPGL2"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Dec 1996"
/*

HPGL2/2  Driver for draw John S. Urban. Last updated: Wed Dec 11, 1996

This driver generates HPGL2 and PCL5 commands.

It obeys the  prefsize()  procedure  (resolution  is  assumed to be 1000
rasters per inch as far as addressability is concerned).

Driver names are hpgl2port, hpgl2land, ppcl5port, pcl5land.

I have used PCL5 on a variety of HP  LaserJets,  LexMark  printers,  and
HPGL2 on 36"  CALCOMP  plotters  with an HPGL2  mode with good  success.
WordPerfect  and other PC packages do a reasonable  job of importing the
HPGL2 files.

*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "draw.h"
#define MIN(x,y)        ((x) < (y) ? (x) : (y))


extern FILE     *_draw_outfile();

static int drawn = 0;

static int      plotlstx, plotlsty;     /* position of last draw */

extern  FILE     *draw_fp;

#define CMAPSIZE 256

#define P_RESET         0
#define P_MOVE          1
#define P_DRAW          2
#define P_TXTSIZE       3
#define P_BEGTXT        4
#define P_ENDTXT        5
#define P_PEN           6
#define P_WIDE          7
#define P_CLEAR         8
#define P_EXIT          9
#define P_FILL         10
#define P_ENDFILL      11
#define P_BREAKFILL    12

/* basic commands for HPGL2 */
static char     *HPGL2[] = {
        "DF;\n",
        "PU%d,%d;\n",
        "PD%d,%d;\n",
        "SI%.4f,%.4f;\n",
        "LB",
        "\003\n",
        "SP%d;\n",
        "PW%.4f;\n",
        "PG;\n",
        "\033.)\n",
        "PM0",
        "PM2;FP0;EP", /* exit polygon mode fill polygon using odd-even algorithm; fill polygon; edge-fill polygon */
        "PM1"
};

/* basic commands for pcl5 */
static char     *pcl5[] = {
        "DF;\n",
        "PU%d,%d;\n",
        "PD%d,%d;\n",
        "SI%.4f,%.4f;\n",
        "LB",
        "\003\n",
        "SP%d;\n",
        "PW%.4f;\n",
        "                                                                                ", /* "\033%%1A\033E\n\033&l2A\033&l1O\033%%1B\n", */
        "\033%%1A\033E\n",
        "PM0\n",
        "PM2;FP0;EP\n", /* exit polygon mode fill polygon using odd-even algorithm; fill polygon; edge-fill polygon */
        "PM1\n"
};

static char     **plotcmds;

static int HPGL2_mapcolor(int i,int  r,int  g,int  b);
/******************************************************************************/
/*
CORRECT THESE NOTES:
      prefxs and prefys are preferred plotting area size in rasters
      prefy is the BOTTOM margin on this - make big note in manual
      NON-INTUITIVE for LANDSCAPE: prefx is the LEFT margin

         A4  : 10200 x  7320
         A3  : 14720 x 10560
         A2  : 18734 x 13440
         A1  : 29774 x 21360
     17 x 11 : 15370 x 10259
     8.5 x 11:  7650 x  9540
     11 x 8.5:  9540 x  7650
*/
/******************************************************************************/
/* Performs the common parts of HPGL2 initialization.  */
static int HPGL2_common_init(int minx,int  maxx,int  miny,int  maxy){
        vdevice.depth = 8;

        draw_fp = _draw_outfile();

        /* * The next line is for serial lines if you need to set modes */
        fprintf(draw_fp, "\033.(;\033.I81;;17;\033.N;19:IN;");

        /* * Cause scaling to be 0 to maxX maxY.  */
        fprintf(draw_fp, "IP%d,%d,%d,%d;", minx, miny, maxx, maxy);

        fprintf(draw_fp, "SC0,%d,0,%d;", vdevice.sizeSx, vdevice.sizeSy);
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */
        fprintf(draw_fp,"NP256;\n"); /* Establish the number of pens in the HPGL/2 palette*/
        fprintf(draw_fp,"CR0,255,0,255,0,255;\n"); /* set color range for relative color data in the HPGL/2 palette*/

        HPGL2_mapcolor( 0,255,255,255); /* black */
        HPGL2_mapcolor( 1,255,0  ,0  ); /* red */
        HPGL2_mapcolor( 2,0  ,255,0  ); /* green */
        HPGL2_mapcolor( 3,255,255,0  ); /* yellow */
        HPGL2_mapcolor( 4,0  ,0  ,255); /* blue */
        HPGL2_mapcolor( 5,255,0  ,255); /* magenta */
        HPGL2_mapcolor( 6,0  ,255,255); /* cyan */
        HPGL2_mapcolor( 7,0  ,0  ,0  ); /* white */
        HPGL2_mapcolor( 8,155,0  ,0  ); /* */
        HPGL2_mapcolor( 9,0  ,155,0  ); /* */
        HPGL2_mapcolor(10,155,255,255); /* */
        HPGL2_mapcolor(11,155,155,0  ); /* */
        HPGL2_mapcolor(12,0  ,0  ,155); /* */
        HPGL2_mapcolor(13,155,0  ,155); /* */
        HPGL2_mapcolor(14,0  ,155,155); /* */
        HPGL2_mapcolor(15,100,100,100); /* */

        plotcmds = HPGL2;
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(UNUSED);
}
/******************************************************************************/
/* Performs the common parts of PCL5 initialization.  */
static int PCL5_common_init(int minx, int maxx, int miny, int maxy) {
        vdevice.depth = 8;

        draw_fp = _draw_outfile();

        /* (exit PCL5 mode) Enter HPGL2 mode, cause scaling to be 0 to maxX maxY */
        pcl5[P_CLEAR]=malloc(100);
        sprintf(pcl5[P_CLEAR], "\033%%%%1A\033E\n\033&l2A\033&l1O\033%%%%1B\nIP%d,%d,%d,%d;SC0,%d,0,%d;",
        minx, miny, maxx, maxy, vdevice.sizeSx, vdevice.sizeSy);

        fprintf(draw_fp,"%s",pcl5[P_CLEAR]);
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */
        fprintf(draw_fp,"NP256;\n"); /* Establish the number of pens in the HPGL/2 palette*/
        fprintf(draw_fp,"CR0,255,0,255,0,255;\n"); /* set color range for relative color data in the HPGL/2 palette*/

        plotcmds = pcl5;
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(UNUSED);
}
/******************************************************************************/
/* Performs the common parts of HPGL2 initialization in portrait mode */
static int HPGL2_common_initp(int minx, int maxx, int miny, int maxy) {
        vdevice.depth = 8;

        draw_fp = _draw_outfile();

        /* * The next line is for serial lines if you need to set modes */
        fprintf(draw_fp, "\033.(;\033.I81;;17;\033.N;19:IN;");

        /* * Cause scaling to be 0 to maxX maxY.  */
        fprintf(draw_fp, "IP%d,%d,%d,%d;", minx, miny, maxx, maxy);

        fprintf(draw_fp, "SC0,%d,0,%d;", vdevice.sizeSx, vdevice.sizeSy);
        fprintf(draw_fp, "RO90;");  /* rotate 90 degrees */
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */
        fprintf(draw_fp,"NP256;\n"); /* Establish the number of pens in the HPGL/2 palette*/
        fprintf(draw_fp,"CR0,255,0,255,0,255;\n"); /* set color range for relative color data in the HPGL/2 palette*/

        plotcmds = HPGL2;
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(UNUSED);
}
/******************************************************************************/
/* Performs the common parts of PCL5 initialization in portrait mode */

static int PCL5_common_initp(int minx,int maxx,int miny,int maxy) {
        vdevice.depth = 8;

        draw_fp = _draw_outfile();

        /* (exit PCL5 mode) Enter HPGL2 mode, cause scaling to be 0 to maxX maxY , rotate 90 degrees */
        pcl5[P_CLEAR]=malloc(100);
        sprintf(pcl5[P_CLEAR], "\033%%%%1A\033E\n\033&l2A\033&l1O\033%%%%1B\nIP%d,%d,%d,%d;SC0,%d,0,%d;RO90;",
        minx, miny, maxx, maxy, vdevice.sizeSx, vdevice.sizeSy);

        fprintf(draw_fp,"%s",pcl5[P_CLEAR]);
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */
        fprintf(draw_fp,"NP256;\n"); /* Establish the number of pens in the HPGL/2 palette*/
        fprintf(draw_fp,"CR0,255,0,255,0,255;\n"); /* set color range for relative color data in the HPGL/2 palette*/

        plotcmds = pcl5;
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(UNUSED);
}
/******************************************************************************/
static int HPGL2_PORT_init(void) {
        /* 8.5 x 11 portrait paper Letter size - JSU */
        int prefx, prefy, prefxs, prefys;

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        }else{
           prefx = 0;
           prefy = 0;
           vdevice.sizeSx = 7650;
           vdevice.sizeSy = 9540;
           vdevice.sizeX = vdevice.sizeY = 7650;
        }
        return(HPGL2_common_initp(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
static int PCL5_PORT_init(void) {
        /* 8.5 x 11 portrait paper Letter size - JSU */
        int prefx, prefy, prefxs, prefys;

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        }else{
           prefx = 0;
           prefy = 0;
           vdevice.sizeSx = 7650;
           vdevice.sizeSy = 9540;
           vdevice.sizeX = vdevice.sizeY = 7650;
        }
        return(PCL5_common_initp(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
static int HPGL2_LAND_init(void) {
        /* 8.5 x 11 landscape paper Letter size - JSU */
        int prefx, prefy, prefxs, prefys;

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        }else{
           prefx = 0;
           prefy = 0;
           vdevice.sizeSy = 7650;
           vdevice.sizeSx = 9540;
           vdevice.sizeX = vdevice.sizeY = 7650;
        }

        return(HPGL2_common_init(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
static int PCL5_LAND_init(void) {
        /* 8.5 x 11 landscape paper Letter size - JSU */
        int prefx, prefy, prefxs, prefys;

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        }else{
           prefx = 0;
           prefy = 0;
           vdevice.sizeSy = 7650;
           vdevice.sizeSx = 9540;
           vdevice.sizeX = vdevice.sizeY = 7650;
        }

        return(PCL5_common_init(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
/*
 * HPGL2_draw
 *
 *      print the commands to draw a line from the current graphics position
 * to (x, y).
 */
static int HPGL2_draw(int x, int y) {
        if (plotlstx != vdevice.cpVx || plotlsty != vdevice.cpVy)
                fprintf(draw_fp, plotcmds[P_MOVE], vdevice.cpVx, vdevice.cpVy);

        fprintf(draw_fp, plotcmds[P_DRAW], x, y);
        plotlstx = x;
        plotlsty = y;
        drawn = 1;
        return(UNUSED);
}
/******************************************************************************/
/* HPGL2_exit
 * exit from draw printing the command to put away the pen and flush the buffer.
 */
static int HPGL2_exit(void) {
        fprintf(draw_fp, plotcmds[P_PEN], 0);
        fprintf(draw_fp, "%s",plotcmds[P_EXIT]);
        fflush(draw_fp);

        if (draw_fp != stdout && draw_fp != stderr ){
                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
        }
        drawn = 1;
        return(UNUSED);
}
/******************************************************************************/
/* HPGL2_clear flush current page without resetting graphics state of printer
 */
static int HPGL2_clear(void) {
        if (drawn){
                fprintf(draw_fp, "%s",plotcmds[P_CLEAR]);
        }
        drawn = 0;
        return(UNUSED);
}
/******************************************************************************/
static int HPGL2_setlw(int i) {
   static float line_width = 0.2;
      if ( i <= -1 ) {
         line_width=0.0; /* hpgl will draw thinnest line possible */
      } else {
         line_width=(float)vdevice.sizeX/10000.0*abs(i); /* figure number of rasters */
         /*
            Assuming pen width is in mm and 300dpi resolution
            1 "raster" =1/300 inch
            1inch=25.4mm
            := approximately one raster at 300dpi is .085 mm
         */
         line_width=line_width*0.0846666/4.0;
      }
      fprintf(draw_fp,plotcmds[P_WIDE] , line_width);
   return(UNUSED);
}
/******************************************************************************/
/*
 * HPGL2_color
 *
 *      change the current pen number.
 *
 *      set the current drawing color index if ind >= 0.
 *      set the line width in raster units if ind  <  0.
 *      use of -1 value implies use thinnest line
 */
static int HPGL2_color(int i) {
   static float line_width = 0.2;
   if ( i < 0 )
   {
      HPGL2_setlw(abs(i));
   } else {
      fprintf(draw_fp, plotcmds[P_PEN], i);
      /*
         The pen thickness might return to the default each time
         a new pen is selected in HPGL2; but draw expects
         these properties to be separate. Newer versions of HPGL2 documentation
         indicate that PW stays in effect until a PU or another PW command
         is encountered; but an older manual states that the pen thickness
         returns to the default value when a new pen is selected. A third
         possibility is that each pen retains its last thickness so I think
         it is better to force it each time a pen number is selected.

         Could reduce redundant calls by checking if same as last call
      */
      fprintf(draw_fp,plotcmds[P_WIDE] , line_width);
   }
        return(UNUSED);
}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */
static int HPGL2_mapcolor(int i,int  r,int  g,int  b) {
        /* fprintf(stderr, "mapcolor = %d %d %d %d\n", i,r,g,b); */

        if (i >= CMAPSIZE || i < 0 ){
                return(-1);
        }

        if (i >= CMAPSIZE){
                return(UNUSED);
        }
        fprintf(draw_fp, "PC%d,%d,%d,%d\n", i, r, g, b);
        return(UNUSED);
}
/******************************************************************************/
/*
 * HPGL2_font
 *
 *      load in large or small
 */
static int HPGL2_font(char *font) {
        if (strcmp(font, "small") == 0) {
                vdevice.hwidth = 97.01; /* Size in plotter resolution units */
                vdevice.hheight = vdevice.hwidth * 2.0;
                fprintf(draw_fp, plotcmds[P_TXTSIZE], 0.16, 0.32);
        } else if (strcmp(font, "large") == 0) {
                vdevice.hwidth = 145.5;
                vdevice.hheight = vdevice.hwidth * 2.0;
                fprintf(draw_fp, plotcmds[P_TXTSIZE], 0.24, 0.48);
        } else
                return(0);

        return(1);
}
/******************************************************************************/
/*
 * HPGL2_char
 *
 *      draw a character.
 */
static int HPGL2_char(char c) {

        if (plotlstx != vdevice.cpVx || plotlsty != vdevice.cpVy)
                fprintf(draw_fp, plotcmds[P_MOVE], vdevice.cpVx, vdevice.cpVy);

        fprintf(draw_fp, "%s",plotcmds[P_BEGTXT]);

        fprintf(draw_fp, "%c", c);

        fprintf(draw_fp, "%s",plotcmds[P_ENDTXT]);

        plotlstx = plotlsty = -1111111;
        drawn = 1;
        return(UNUSED);
}
/******************************************************************************/
/* output a string.  */
static int HPGL2_string(char *s) {

        if (plotlstx != vdevice.cpVx || plotlsty != vdevice.cpVy)
                fprintf(draw_fp, plotcmds[P_MOVE], vdevice.cpVx, vdevice.cpVy);

        fprintf(draw_fp, "%s",plotcmds[P_BEGTXT]);

        fputs(s, draw_fp);

        fprintf(draw_fp, "%s",plotcmds[P_ENDTXT]);

        plotlstx = plotlsty = -1111111;
        drawn = 1;
        return(UNUSED);
}
/******************************************************************************/
/* "fill" a polygon */
static int HPGL2_fill(int n, int x[], int y[]) {
        int     i;
        int xclose, yclose, move_draw;
        xclose=x[0];
        yclose=y[0];

        if (plotlstx != x[0] || plotlsty != y[0])
                fprintf(draw_fp, plotcmds[P_MOVE], x[0], y[0]);      /* update current position if needed */

        fprintf(draw_fp, "%s",plotcmds[P_FILL]);                          /* enter polygon mode */

        move_draw=P_DRAW;
        for (i = 1; i < n; i++){
                /* If not last point in list and hit first point close this as a subpolygon */
                if (x[i] == xclose && y[i] == yclose && (i != (n-1)) )
                {
                   fprintf(draw_fp, plotcmds[P_DRAW], x[i], y[i]);   /* define polygon vertices */
                   fprintf(draw_fp, "%s",plotcmds[P_BREAKFILL]);          /* end subpolygon */
                   move_draw=P_MOVE;                            /* move to first point in next subpolygon */
                }else{
                   fprintf(draw_fp, plotcmds[move_draw],x[i],y[i]);  /* define polygon vertices */
                   move_draw=P_DRAW;                            /* if moving because starting a new subpolygon return to drawing */
                }
        }

        fprintf(draw_fp, "%s",plotcmds[P_ENDFILL]);                       /* end polygon and fill it */

        fprintf(draw_fp, plotcmds[P_MOVE], x[n-1], y[n-1]);          /* current position should be last one moved to, not x0,y0 */

        plotlstx = vdevice.cpVx = x[n - 1];
        plotlsty = vdevice.cpVy = y[n - 1];
        drawn = 1;
        return(UNUSED);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry HPGL2dev = {
        "HPGL2",        /* char *devname -- name of device */
        "large",        /* char *large -- name of large font */
        "small",        /* char *small -- name of small font */
        noop,           /* int (*Vbackb)() -- Set drawing in back buffer */
        HPGL2_char,     /* int (*Vchar)() -- Draw a hardware character */
        noop,           /* int (*Vcheckkey)() -- Check if a key was hit */
        HPGL2_clear,    /* int (*Vclear)() -- Clear screen to current color */
        HPGL2_color,    /* int (*Vcolor)() -- Set current color */
        HPGL2_draw,     /* int (*Vdraw)() -- Draw a line */
        HPGL2_exit,     /* int (*Vexit)() -- Exit graphics */
        HPGL2_fill,     /* int (*Vfill)() -- Fill a polygon */
        HPGL2_font,     /* int (*Vfont)() -- Set hardware font */
        noop,           /* int (*Vfrontb)() -- Set drawing in front buffer */
        noop,           /* int (*Vgetkey)() -- Wait for and get next key hit */
        HPGL2_LAND_init,/* int (*Vinit)() -- Initialise device */
        noop2,          /* int (*Vlocator)() -- Get mouse/cross hair position */
        HPGL2_mapcolor, /* int (*Vmapcolor)() -- Set color indices */
        HPGL2_setlw,    /* Set line width */
        HPGL2_string,   /* int (*Vstring)() -- Draw a hardware string */
        noop,           /* int (*Vswapb)() -- Swap front and back buffers */
        noop            /* int (*Vsync)() -- Syncronise display */
};
/******************************************************************************/
int _PCL5_LAND_draw_devcpy(void){ /* copy the PCL5 landscape device into vdevice.dev.  */
        vdevice.dev = HPGL2dev;
        vdevice.dev.Vinit = PCL5_LAND_init;
/* if you don't have structure assignment ... this would set Vinit
        char    *dev, *tdev, *edev;

        dev = (char *)&HPGL2dev;
        tdev = (char *)&vdevice.dev;
        edev = dev + sizeof(Device);

        while (dev != edev)
                *tdev++ = *dev++;
*/
        return(UNUSED);
}
/******************************************************************************/
int _PCL5_PORT_draw_devcpy(void){/* copy the PCL5 portrait device into vdevice.dev.  */
        vdevice.dev = HPGL2dev;
        vdevice.dev.Vinit = PCL5_PORT_init;
        return(UNUSED);
}
/******************************************************************************/
int _HPGL2_PORT_draw_devcpy(void){/* copy the HPGL2 portrait device into vdevice.dev.  */
        vdevice.dev = HPGL2dev;
        vdevice.dev.Vinit = HPGL2_PORT_init;
        return(UNUSED);
}
/******************************************************************************/
int _HPGL2_LAND_draw_devcpy(void){/* copy the HPGL2 landscape  device into vdevice.dev.  */
        vdevice.dev = HPGL2dev;
        vdevice.dev.Vinit = HPGL2_LAND_init;
        return(UNUSED);
}
/******************************************************************************/

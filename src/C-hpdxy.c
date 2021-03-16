/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/hpdxy.c - M_DRAW driver for DXY and HPGL"

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "draw.h"
#define MIN(x,y)        ((x) < (y) ? (x) : (y))

extern FILE     *_draw_outfile();
static int drawn = 0;
static int      plotlstx, plotlsty;     /* position of last draw */
extern  FILE     *draw_fp;
/*
   For importing into Microsoft Word 9x -- HPGL will not be supported
   in Windows 97 or 2000.

   Microsoft Word does not map the pens to the expected de-facto
   color mappings by default
*/

static int remap[8]={0,1,2,3,4,5,6,7};

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

/*
 *  basic commands for hpgl
 *  NOTE: Old HP plotters might require a , delimiter and no spaces
 */
static char     *hpgl[] = {
        "DF;\n",
        "PU %d %d;\n",
        "PD %d %d;\n",
        "SI %.4f %.4f;\n",
        "LB",
        "\003\n",
        "SP %d;\n",
        "PW %.4f;\n",
        "PG;\n",
        "\033.)\n",
        "PM0",
        "PM2;FP0;EP", /* exit polygon mode fill polygon using odd-even algorithm; fill polygon; edge-fill polygon */
        "PM1"
};

/* basic commands for pcl */
static char     *pcl[] = {
        "DF;\n",
        "PU %d %d;\n",
        "PD %d %d;\n",
        "SI %.4f %.4f;\n",
        "LB",
        "\003\n",
        "SP %d;\n",
        "PW %.4f;\n",
        "\033%%1A\033E\n\033&l2A\033&l1O\033%%1B\n",
        "\033%%1A\033E\n",
        "PM0\n",
        "PM2;FP0;EP\n", /* exit polygon mode fill polygon using odd-even algorithm; fill polygon; edge-fill polygon */
        "PM1\n"
};
/* basic commands for dxy */
static char     *dxy[] = {
        "",
        "M %d,%d\n",
        "D %d,%d\n",
        "S %d\n",
        "P",
        "\n",
        "J %d\n",
        "PG;\n",
        "\033.)\n",
        " ",
        " ",
        " "
};

static char     **plotcmds;

static char CLEAR_DEVICE[100];
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
/* Performs the common parts of HPGL initialization.  */
static int HPGL_common_init(int minx, int maxx, int miny, int maxy) {
        vdevice.depth = 3; /* eight colors */

        draw_fp = _draw_outfile();

        /* * The next line is for serial lines if you need to set modes */
        fprintf(draw_fp, "\033.(;\033.I81;;17;\033.N;19:IN;");

        /* * Cause scaling to be 0 to maxX maxY.  */
        fprintf(draw_fp, "IP %d,%d,%d,%d;", minx, miny, maxx, maxy);

        fprintf(draw_fp, "SC 0,%d,0,%d;", vdevice.sizeSx, vdevice.sizeSy);
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */

        plotcmds = hpgl;
        strncpy(CLEAR_DEVICE,plotcmds[P_CLEAR],100); /* this has parameters sometimes so it is treated differently */
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(1);
}
/******************************************************************************/
/* Performs the common parts of HPGL initialization in portrait mode */
static int HPGL_common_initp(int minx, int maxx, int miny, int maxy) {
        vdevice.depth = 3; /* eight colors */

        draw_fp = _draw_outfile();

        /* * The next line is for serial lines if you need to set modes */
        fprintf(draw_fp, "\033.(;\033.I81;;17;\033.N;19:IN;");

        /* * Cause scaling to be 0 to maxX maxY.  */
        fprintf(draw_fp, "IP %d,%d,%d,%d;", minx, miny, maxx, maxy);

        fprintf(draw_fp, "SC 0,%d,0,%d;", vdevice.sizeSx, vdevice.sizeSy);
        fprintf(draw_fp, "RO 90;");  /* rotate 90 degrees */
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */

        plotcmds = hpgl;
        strncpy(CLEAR_DEVICE,plotcmds[P_CLEAR],100); /* this has parameters sometimes so it is treated differently */
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(1);
}
/******************************************************************************/
/* Performs the common parts of PCL initialization.  */
static int PCL_common_init(int minx, int maxx, int miny, int maxy) {
        vdevice.depth = 3;

        draw_fp = _draw_outfile();

        /* (exit PCL mode) Enter HPGL2 mode, cause scaling to be 0 to maxX maxY */
        sprintf(CLEAR_DEVICE, "\033%%%%1A\033E\n\033&l2A\033&l1O\033%%%%1B\nIP %d,%d,%d,%d;SC 0,%d,0,%d;",
        minx, miny, maxx, maxy, vdevice.sizeSx, vdevice.sizeSy);

        fprintf(draw_fp,"%s",CLEAR_DEVICE);
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */

        plotcmds = pcl;
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(1);
}
/******************************************************************************/
/* Performs the common parts of PCL initialization in portrait mode */
static int PCL_common_initp(int minx, int maxx, int miny, int maxy) {
        vdevice.depth = 3;

        draw_fp = _draw_outfile();

        /* (exit PCL mode) Enter HPGL2 mode, cause scaling to be 0 to maxX maxY , rotate 90 degrees */
        sprintf(CLEAR_DEVICE, "\033%%%%1A\033E\n\033&l2A\033&l1O\033%%%%1B\nIP %d,%d,%d,%d;SC 0,%d,0,%d;RO 90;",
        minx, miny, maxx, maxy, vdevice.sizeSx, vdevice.sizeSy);

        fprintf(draw_fp,"%s",CLEAR_DEVICE);
        fprintf(draw_fp,"TR0;\n"); /* transparency mode off, white areas are opaque */

        plotcmds = pcl;
        plotlstx = -1111111;
        plotlsty = -1111111;

        drawn = 0;

        return(1);
}
/******************************************************************************/
static int HPGL_PORT_init(void) {
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
        return(HPGL_common_initp(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
static int PCL_PORT_init(void) {
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
        return(PCL_common_initp(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
static int HPGL_LAND_init(void) {
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

        return(HPGL_common_init(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
static int PCL_LAND_init(void) {
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

        return(PCL_common_init(prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy));
}
/******************************************************************************/
/*
 * DXY_init
 *
 *      set up dxy plotter. Returns 1 on success.
 */
static int DXY_init(void) {
        draw_fp = _draw_outfile();

        vdevice.sizeX = vdevice.sizeY = 1920;

        vdevice.sizeSx = 2668;
        vdevice.sizeSy = 1920;

        plotcmds = dxy;
        strncpy(CLEAR_DEVICE,plotcmds[P_CLEAR],100); /* this has parameters sometimes so it is treated differently */
        plotlstx = -1;
        plotlsty = -1;

        drawn = 0;

        fprintf(draw_fp, "%s",plotcmds[P_RESET]);

        return(1);
}
/******************************************************************************/
/*
 * PLOT_draw
 *
 *      print the commands to draw a line from the current graphics position
 * to (x, y).
 */
static int PLOT_draw(int x, int y) {
        if (plotlstx != vdevice.cpVx || plotlsty != vdevice.cpVy)
                fprintf(draw_fp, plotcmds[P_MOVE], vdevice.cpVx, vdevice.cpVy);

        fprintf(draw_fp, plotcmds[P_DRAW], x, y);
        plotlstx = x;
        plotlsty = y;
        drawn = 1;
        return(UNUSED);
}
/******************************************************************************/
/* PLOT_exit
 * exit from draw printing the command to put away the pen and flush the buffer.
 */
static int PLOT_exit(void) {
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
/* PLOT_clear flush current page without resetting graphics state of printer
 */
static int PLOT_clear(void) {
        if (drawn){
                /*
                  CLEAR_DEVICE is plotcmds[P_CLEAR]) expanded
                  to a fixed string for this device format
                */
                 fprintf(draw_fp,"%s", CLEAR_DEVICE);
        }
        drawn = 0;
        return(UNUSED);
}
/******************************************************************************/
static int PLOT_setlw(int i) {
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
 * PLOT_color
 *
 *      change the current pen number.
 *
 *      set the current drawing color index if ind >= 0.
 *      set the line width in raster units if ind  <  0.
 *      use of -1 value implies use thinnest line
 */
static int PLOT_color(int i) {
   static float line_width = 0.2;
   if ( i < 0 ) {
         PLOT_setlw(abs(i));
   } else {
      if(i<8){
         fprintf(draw_fp, plotcmds[P_PEN], remap[i]);
      }else{
         fprintf(draw_fp, plotcmds[P_PEN], i);
      }
      /*
         The pen  thickness  might return to the default each time a new
         pen is selected in hpgl; but draw expects these  properties to
         be  separate.  Newer  versions of HPGL  documentation  indicate
         that PW stays in effect  until a PU or  another  PW  command is
         encountered;  but an older manual states that the pen thickness
         returns  to the  default  value when a new pen is  selected.  A
         third  possibility is that each pen retains its last  thickness
         so I think it is better to force it each time a pen  number  is
         selected.

         Could reduce redundant calls by checking if same as last call
      */
      fprintf(draw_fp,plotcmds[P_WIDE] , line_width);
   }
   return(UNUSED);
}
/******************************************************************************/
/* load in large or small */
static int HPGL_font(char *font) {
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
/* load in large or small */
static int DXY_font(char *font) {
        if (strcmp(font, "small") == 0) {
                vdevice.hwidth = 24.25;
                vdevice.hheight = vdevice.hwidth * 2.0;
                fprintf(draw_fp, plotcmds[P_TXTSIZE], 3);
        } else if (strcmp(font, "large") == 0) {
                vdevice.hwidth = 36.375;
                vdevice.hheight = vdevice.hwidth * 2.0;
                fprintf(draw_fp, plotcmds[P_TXTSIZE], 5);
        } else
                return(0);

        return(1);
}
/******************************************************************************/
/* draw a character */
static int PLOT_char(char c) {

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
static int PLOT_string(char *s) {

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
static int PLOT_fill(int n, int x[], int y[]) {
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
static int noop4(int a, int b, int c, int d) { return(-1); }
/******************************************************************************/
static DevEntry hpgldev = {
        "hpgl",         /* char *devname -- name of device */
        "large",        /* char *large -- name of large font */
        "small",        /* char *small -- name of small font */
        noop,           /* int (*Vbackb)() -- Set drawing in back buffer */
        PLOT_char,      /* int (*Vchar)() -- Draw a hardware character */
        noop,           /* int (*Vcheckkey)() -- Check if a key was hit */
        PLOT_clear,     /* int (*Vclear)() -- Clear screen to current color */
        PLOT_color,     /* int (*Vcolor)() -- Set current color */
        PLOT_draw,      /* int (*Vdraw)() -- Draw a line */
        PLOT_exit,      /* int (*Vexit)() -- Exit graphics */
        PLOT_fill,      /* int (*Vfill)() -- Fill a polygon */
        HPGL_font,      /* int (*Vfont)() -- Set hardware font */
        noop,           /* int (*Vfrontb)() -- Set drawing in front buffer */
        noop,           /* int (*Vgetkey)() -- Wait for and get next key hit */
        HPGL_LAND_init, /* int (*Vinit)() -- Initialise device */
        noop2,          /* int (*Vlocator)() -- Get mouse/cross hair position */
        noop4,          /* int (*Vmapcolor)() -- Set color indices */
        PLOT_setlw,     /* int (*Vsetlw)() -- Set line width */
        PLOT_string,    /* int (*Vstring)() -- Draw a hardware string */
        noop,           /* int (*Vswapb)() -- Swap front and back buffers */
        noop            /* int (*Vsync)() -- Syncronise display */
};
/******************************************************************************/
static DevEntry dxydev = {
        "dxy",          /* char *devname -- name of device */
        "large",        /* char *large -- name of large font */
        "small",        /* char *small -- name of small font */
        noop,           /* int (*Vbackb)() -- Set drawing in back buffer */
        PLOT_char,      /* int (*Vchar)() -- Draw a hardware character */
        noop,           /* int (*Vcheckkey)() -- Check if a key was hit */
        PLOT_clear,     /* int (*Vclear)() -- Clear screen to current color */
        PLOT_color,     /* int (*Vcolor)() -- Set current color */
        PLOT_draw,      /* int (*Vdraw)() -- Draw a line */
        PLOT_exit,      /* int (*Vexit)() -- Exit graphics */
        PLOT_fill,      /* int (*Vfill)() -- Fill a polygon */
        HPGL_font,      /* int (*Vfont)() -- Set hardware font */
        noop,           /* int (*Vfrontb)() -- Set drawing in front buffer */
        noop,           /* int (*Vgetkey)() -- Wait for and get next key hit */
        DXY_init,       /* int (*Vinit)() -- Initialise device */
        noop2,          /* int (*Vlocator)() -- Get mouse/cross hair position */
        noop4,          /* int (*Vmapcolor)() -- Set color indices */
        PLOT_setlw,     /* int (*Vsetlw)() -- Set line width */
        PLOT_string,    /* int (*Vstring)() -- Draw a hardware string */
        noop,           /* int (*Vswapb)() -- Swap front and back buffers */
        noop            /* int (*Vsync)() -- Syncronise display */
};
/******************************************************************************/
static int HPGL_setpens(void){
   int i;
   char    *env_variable;
   if ((env_variable = (char *)getenv("VPENS")) != NULL){
      sscanf(env_variable, "%d,%d,%d,%d,%d,%d,%d,%d",
          &remap[0], &remap[1], &remap[2], &remap[3],
          &remap[4], &remap[5], &remap[6], &remap[7]);
   } else{
      for(i=0; i<8; i++){
         remap[i]=i;
      }
   }
   return(UNUSED);
}
/******************************************************************************/
/* copy the HPGL device into vdevice.dev.  */
int _HPGL_A2_draw_devcpy(void) {
/* if you don't have structure assignment ...
        char    *dev, *tdev, *edev;

        dev = (char *)&hpgldev;
        tdev = (char *)&vdevice.dev;
        edev = dev + sizeof(Device);

        while (dev != edev)
                *tdev++ = *dev++;
*/
        vdevice.dev = hpgldev;
        return(UNUSED);
}
/******************************************************************************/
int _PCL_LAND_draw_devcpy(void) {
        vdevice.dev = hpgldev;
        vdevice.dev.Vinit = PCL_LAND_init;
        return(UNUSED);
}
/******************************************************************************/
int _PCL_PORT_draw_devcpy(void) {
        vdevice.dev = hpgldev;
        vdevice.dev.Vinit = PCL_PORT_init;
        return(UNUSED);
}
/******************************************************************************/
int _HPGL_PORT_draw_devcpy(void) {
        vdevice.dev = hpgldev;
        vdevice.dev.Vinit = HPGL_PORT_init;
        /*strcpy(vdevice.dev.devname,"hpglport");*/
        /*vdevice.dev.devname={'h','p','g','l','p','o','r','t','\0'};*/
        vdevice.dev.devname="hpglport";
        HPGL_setpens();
        return(UNUSED);
}
/******************************************************************************/
int _HPGL_LAND_draw_devcpy(void) {
        vdevice.dev = hpgldev;
        vdevice.dev.Vinit = HPGL_LAND_init;
        /*strcpy(vdevice.dev.devname,"hpglland");*/
        /*vdevice.dev.devname={'h','p','g','l','l','a','n','d','\0'};*/
        vdevice.dev.devname="hpglland";
        HPGL_setpens();
        return(UNUSED);
}
/******************************************************************************/
/* copy the DXY device into vdevice.dev.  */
int _DXY_draw_devcpy(void) {
/* if you don't have structure assignment ...
        char    *dev, *tdev, *edev;

        dev = (char *)&dxydev;
        tdev = (char *)&vdevice.dev;
        edev = dev + sizeof(DevEntry);

        while (dev != edev)
                *tdev++ = *dev++;
*/
        vdevice.dev = dxydev;
        return(UNUSED);
}
/******************************************************************************/

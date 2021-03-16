/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/fti.c - M_DRAW driver for Silicon Graphics icon geometries ((.fti files)"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Aug 1997"

/*
fti driver for draw; Version 1.0, John S. Urban, Aug 1997
Please pass any upgrades or comments back to me if you get a chance.
*-----------------------------------*-----------------------------*
| Cray Research                     | urban@cray.com              |
| Silicon Graphics, Incorporated    | urban@sgi.com               |
*-----------------------------------*-----------------------------*
Next time in:
   o gather lines into polylines
   o option to have if between pages or put each page to a new file
   o maybe clear screen to current color should fill in a polygon background
   o maybe initialize with a standard template
*/
/******************************************************************************/
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "draw.h"
extern FILE     *_draw_outfile();
extern  FILE     *draw_fp;
#define ABS(x)    ((x) < 0 ? -(x) : (x))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))

static int GLOBAL_color = 0;
static int GLOBAL_lastx = -1111111;
static int GLOBAL_lasty = -1111111;
/******************************************************************************/
static int FTI_color(int col){ /* change the current color */
/* Empirically found that color is from 0 to 15 inclusive else a negative value from -16 to -255 */
        int icol;
        GLOBAL_color = ABS(col % 256) ;
        if(GLOBAL_color > 15 ){
                icol=-GLOBAL_color;
        } else{
                icol=GLOBAL_color;
        }
        switch(icol) {
        case 0:
                fprintf(draw_fp,"color(iconcolor);\n");
                break;
        case 7:
                fprintf(draw_fp,"color(outlinecolor);\n");
                break;
        default:
                fprintf(draw_fp,"color(%d);\n",icol); /* Set current pen color index to n. */
        }
        return(0);
}
/******************************************************************************/
static int FTI_init(void) {
        vdevice.sizeSy = 1000;
        vdevice.sizeSx = 1000;
        vdevice.sizeX = vdevice.sizeY = MIN(vdevice.sizeSy,vdevice.sizeSx);
        vdevice.depth = 1;
        draw_fp = _draw_outfile();
        GLOBAL_lastx = -1111111;
        GLOBAL_lasty = -1111111;
        (void) fprintf(draw_fp,"#FTI\n");                                                /* magic number of a clear text FTI file */
        (void) fprintf(draw_fp,"# CREATOR: M_DRAW FTI driver; version 1.0 1997/08/02\n"); /* FTI file can contain comment lines*/
        (void) fprintf(draw_fp,"# Version 1: 19970802, Author: John S. Urban\n");
               fprintf(draw_fp,"color(outlinecolor);\n");
        return(1);
}
/******************************************************************************/
static int FTI_fill(int n, int x[], int y[]){ /* "fill" a polygon */
        int     i;
        fprintf(draw_fp,"bgnpolygon();\n"); /* begin polygon */
        for (i = 0; i < n; i++){
                fprintf(draw_fp,"   vertex(%f,%f);\n",x[i]/10.0,y[i]/10.0); /* draw outline across graphics array */
        }
        fprintf(draw_fp,"endpolygon();\n"); /* end polygon */
        /* update current position */
        GLOBAL_lastx = vdevice.cpVx = x[n - 1];
        GLOBAL_lasty = vdevice.cpVy = y[n - 1];
        return(UNUSED);
}
/******************************************************************************/
static int FTI_draw(int x,int  y){ /* print the commands to draw a line from the current graphics position to (x, y).  */
        if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
             GLOBAL_lastx=vdevice.cpVx;
             GLOBAL_lasty=vdevice.cpVy;
             fprintf(draw_fp,"move(%f,%f);\n",GLOBAL_lastx/10.0,GLOBAL_lasty/10.0);
        }
        fprintf(draw_fp,"draw(%f,%f);\n",x/10.0,y/10.0);
        return(UNUSED);
}
/*******************************************************************************/
static int FTI_exit(void){ /* exit from draw printing the command to flush the buffer.  */
        fflush(draw_fp); /* flush the output file */
        if (draw_fp != stdout && draw_fp != stderr ){
                (void) fprintf(draw_fp,"# End of FTI File\n");
                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
        }
        return(UNUSED);
}
/*******************************************************************************/
static int FTI_clear(void){ /* flush current page and clear graphics array */
                (void) fprintf(draw_fp,"# End of FTI Page\n");
        return(UNUSED);
}
/******************************************************************************/
static int FTI_font(char *font){ /* load in large or small */
        fprintf(stderr, "error: NO HARDWARE FONT\n");
        if (strcmp(font, "small") == 0) {
                vdevice.hwidth = 97.01; /* Size in plotter resolution units */
                vdevice.hheight = vdevice.hwidth * 2.0;
        } else if (strcmp(font, "large") == 0) {
                vdevice.hwidth = 145.5;
                vdevice.hheight = vdevice.hwidth * 2.0;
        } else
                return(0);
        return(1);
}
/******************************************************************************/
static int FTI_string(char *s){ /* output a string.  */
        if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
                GLOBAL_lastx=vdevice.cpVx;
                GLOBAL_lasty=vdevice.cpVy;
        }
        fprintf(draw_fp,"# %s\n",s);
        GLOBAL_lastx = GLOBAL_lasty = -1111111; /* undefine current position because used hardware text ?*/
        return(UNUSED);
}
/******************************************************************************/
static int FTI_char(char c){ /* output a character */
  char  s[2];
  s[0] = c; s[1]='\0';
  FTI_string(s);
  return(UNUSED);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop1(int x) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
static int noop4(int a, int b, int c, int d) { return(-1); }
/******************************************************************************/
static DevEntry FTIdev = {
                "FTI",       /* name of device */
                "large",     /* name of large font */
                "small",     /* name of small font */
                noop,        /* Set drawing in back buffer */
                FTI_char,    /* Draw a hardware character */
                noop,        /* Check if a key was hit */
                FTI_clear,   /* Clear the screen to current color */
                FTI_color,   /* Set current color */
                FTI_draw,    /* Draw a line */
                FTI_exit,    /* Exit graphics */
                FTI_fill,    /* Fill a polygon */
                FTI_font,    /* Set hardware font */
                noop,        /* Set drawing in front buffer */
                noop,        /* Wait for and get the next key hit */
                FTI_init,    /* Initialize the device */
                noop2,       /* Get mouse/cross hair position */
                noop4,       /* Set color indices */
                noop1,       /* Set line thickness */
                FTI_string,  /* Draw a hardware string */
                noop,        /* Swap front and back buffers */
                noop         /* Syncronize the display */
};
/******************************************************************************/
int _FTI_draw_devcpy(void) {
        vdevice.dev = FTIdev;
        vdevice.dev.Vinit = FTI_init;
        return(UNUSED);
}
/******************************************************************************/

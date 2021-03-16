/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/cgmt.c - M_DRAW driver for CGM (Computer Graphics Metafile) clear text"
#ident "@(#)M_DRAW:author : John S. Urban"
#ident "@(#)M_DRAW:version : 1.3, Jan 1997"
/*
================================================================================
Computer Graphics Metafile clear text driver  for  M_DRAW  1.3
    Based  on information contained in ANSI X3.122 - 1986

Thanks to Kurt Miller, Bettis Atomic Power Lab, without whom this driver
would not have been written.

John S. Urban, SGI
http://reality.sgi.com/urban/index.html

Created:      Fri, 17 Jan 1997
Last Revised: Sat,  1 Nov 1997
Last Revised: Thu,  4 Dec 1997 -- backfitted to M_DRAW 1.1 for USH
================================================================================
Many PC programs state they read CGM files.  My experience has been that
most  such  programs  only read a small subset of  the CGM standard.  If
you find such a program cannot  read   the    CGM   generated   by  this
driver   I   have   found   two  popular   programs  that can be used to
convert  the  CGM  clear  text   file generated  by this driver  to   an
appropriate  binary  format.  In  addition, they can be used to view and
convert practically any CGM file:

 o gplot(1)  from  the  Pittsburgh Supercomputer Center
 o ralcgm(1) from Rutherford Appleton Laboratory.
================================================================================
NEXT TIME IN:
o Full hardware text support
================================================================================
NOTE:
The current version  of  the  CGM  ANSI/ISO  standard  (commonly  called
CGM:1992) is:

  Information Processing Systems--Computer Graphics Metafile for the
  Storage and Transfer of Picture Description Information,
  ANSI/ISO 8632-1992.

This standard superseded the earlier CGM:1986  (ANSI  X3.122-1986)  ANSI
standard.    The  CGM  standard  is  contained  in  four  ISO  standards
documents:

  ISO/IEC 8632-1:1992 Part 1: Functional Specification
  ISO/IEC 8632-3:1992 Part 2: Character Encoding
  ISO/IEC 8632-3:1992 Part 3: Binary Encoding
  ISO/IEC 8632-4:1992 Part 4: Clear Text Encoding

These documents may be obtained from the following organizations:

  International Standards Organization (ISO)
  1 rue de Varembe
  Case Postal 56
  CH-1211 Geneva 20 Switzerland
  Voice: +41 22 749 01 11
  Fax:   +41 22 733 34 30

  American National Standards Institute (ANSI)
  Sales Department
  1430 Broadway
  New York, NY 10018
  Voice: 212.642.4900

  Canadian Standards Association (CSA)
  Sales Group
  178 Rexdale Blvd.
  Rexdale, Ontario, M9W 1R3
  Voice: 416.747.4044
================================================================================
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "draw.h"

#define MAX(x, y)       ((x) > (y) ? (x) : (y))

#define CGMT_X_SIZE 32767
#define CGMT_Y_SIZE 32767

#define TRUE  1
#define FALSE 0

extern FILE     *draw_fp;
extern FILE     *_draw_outfile();

static int      first_time = TRUE,
                page_count = 1,
                drawn = FALSE,  /* have  we drawn anything on this page yet */
                curcol = 0,     /* draw black */
                curwidth = 2,     /* current line width in rasters */
                CMAPSIZE_used = 0,
                plotlstx = -1,  /* x position of last draw */
                plotlsty = -1;  /* y position of last draw */

#define CMAPSIZE 256
struct rgb_color{
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
};
static struct rgb_color carray[CMAPSIZE];
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value */
static int CGMT_mapcolor(int i, int r, int g, int b) {
        if(i >= CMAPSIZE ){
           return(-1);
        }
        carray[i].red = (unsigned short)(r);
        carray[i].green = (unsigned short)(g);
        carray[i].blue = (unsigned short)(b);
        fprintf(draw_fp,"COLRTABLE %d %d %d %d;\n",i+1,r,g,b);
        CMAPSIZE_used = MAX(CMAPSIZE_used,i+1);
        return(0);
}
/******************************************************************************/
static int CGMT_init(void){ /* set up graphic environment. Returns 1 on success.  */
        int CGMT_mapcolor(int i, int r, int g, int b);
        int i;

        draw_fp = _draw_outfile();

        if (!first_time)
                return(1);

        fputs("BEGMF 'THIS CGM CREATED BY M_DRAW';\n",draw_fp);
        fputs("% Computer Graphics Metatfile %\n",draw_fp);
        fputs("% clear text CGM driver for M_DRAW 1.3 %\n",draw_fp);
        fputs("% Based on ANSI X3.122-1986 %\n",draw_fp);
        fputs("   MFVERSION 1;\n",draw_fp);
        fputs("   MFDESC 'M_DRAW CLEAR TEXT CGM';\n",draw_fp);
        fputs("   MFELEMLIST 'DRAWINGPLUS';\n",draw_fp);
        fputs("   INTEGERPREC -32768 32767;\n",draw_fp);
        fputs("   COLRPREC 255;\n",draw_fp);
        fputs("   COLRINDEXPREC 255;\n",draw_fp);
                /* reserve pen 0 for use by background */
        fputs("   MAXCOLRINDEX 256;\n",draw_fp);
        fputs("   COLRVALUEEXT 0 0 0 255 255 255;\n",draw_fp);

        fputs("BEGMFDEFAULTS;\n",draw_fp);
        fputs("   VDCINTEGERPREC 0 32767;\n",draw_fp);
        fputs("ENDMFDEFAULTS;\n",draw_fp);

        fputs("BEGPIC 'PICTURE1';\n",draw_fp);
        fputs("   COLRMODE INDEXED;\n",draw_fp);
        fputs("   BACKCOLR 255 255 255;\n",draw_fp);
        fputs("   VDCEXT 0 0 32767 32767;\n",draw_fp);
        fputs("   MARKERSIZEMODE ABSTRACT;\n",draw_fp);
        fputs("   EDGEWIDTHMODE ABSTRACT;\n",draw_fp);
        fputs("   SCALEMODE ABSTRACT 0;\n",draw_fp);
        fputs("   LINEWIDTHMODE ABSTRACT;\n",draw_fp);
        fputs("BEGPICBODY;\n",draw_fp);
        fputs("  INTSTYLE SOLID;\n",draw_fp);
        fputs("  LINETYPE 1;\n",draw_fp);
        fputs("  LINEWIDTH 2;\n",draw_fp);
        fputs("  TEXTCOLR 8;\n",draw_fp);
        fputs("  LINECOLR 8;\n",draw_fp);
        fputs("  EDGECOLR 8;\n",draw_fp);
        fputs("  FILLCOLR 8;\n",draw_fp);

        vdevice.sizeSx  = CGMT_X_SIZE;
        vdevice.sizeSy  = CGMT_Y_SIZE;
        vdevice.sizeX = vdevice.sizeY =CGMT_Y_SIZE;

        vdevice.depth = 8;

        for (i=0; i < CMAPSIZE; i++){
           carray[i].red=0;
           carray[i].green=0;
           carray[i].blue=0;
        }

        CGMT_mapcolor(0, 255, 255, 255);
        CGMT_mapcolor(1, 255,   0,   0);
        CGMT_mapcolor(2,   0, 255,   0);
        CGMT_mapcolor(3, 255, 255,   0);
        CGMT_mapcolor(4,   0,   0, 255);
        CGMT_mapcolor(5, 255,   0, 255);
        CGMT_mapcolor(6,   0, 255, 255);
        CGMT_mapcolor(7,   0,   0,   0);
        CGMT_mapcolor(8, 155,   0,   0);
        CGMT_mapcolor(9,   0, 155,   0);
        CGMT_mapcolor(10, 155, 255, 255);
        CGMT_mapcolor(11, 155, 155,   0);
        CGMT_mapcolor(12,   0,   0, 155);
        CGMT_mapcolor(13, 155,   0, 155);
        CGMT_mapcolor(14,   0, 155, 155);
        CGMT_mapcolor(15, 100, 100, 100);

        /* print initial color table */
        CMAPSIZE_used = 16;
        fputs ("COLRTABLE 1\n",draw_fp);
        for( i = 0; i <CMAPSIZE_used; i++) {
           fprintf(draw_fp,"   %d %d %d\n",carray[i].red,
                                       carray[i].green,
                                       carray[i].blue);
        }
        fputs(";\n",draw_fp);

        plotlstx = -1111111;
        plotlsty = -1111111;

        return(1);
}
/******************************************************************************/
/* close the CGMT output file */
static int CGMT_exit(void) {
        fputs("ENDPIC;\n",draw_fp);
        fputs("ENDMF;\n",draw_fp);
        fflush(draw_fp);
        if (draw_fp != stdout && draw_fp != stderr ){
                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
        }
        plotlstx = -1;
        plotlsty = -1;
        drawn = FALSE;
        first_time = TRUE;

        return(UNUSED);

}
/******************************************************************************/
/* draw to an x, y point.  */
static int CGMT_draw(int x, int y) {
        if (plotlstx != vdevice.cpVx || plotlsty != vdevice.cpVy ){
                plotlstx=vdevice.cpVx;
                plotlsty=vdevice.cpVy;
        }

        fprintf(draw_fp, "LINE %d %d %d %d;\n", plotlstx, plotlsty, x, y);

        drawn = TRUE;
        plotlstx = x;
        plotlsty = y;
        return(UNUSED);
}
/******************************************************************************/
static int CGMT_setlw(int rasters){ /* line width in rasters */
        curwidth=abs(rasters);
        curwidth=MAX(1,curwidth);
        if ( drawn != 0 ){
                fprintf(draw_fp, "LINEWIDTH %d;\n",curwidth*vdevice.sizeX/10000);
        }
        return(UNUSED);
}
/******************************************************************************/
/* flush the current page without resetting the graphics state */
static int CGMT_clear(void) {
        int i;
        if (drawn){

                fputs("ENDPIC;\n",draw_fp);

                page_count++;
                fprintf(draw_fp,"BEGPIC 'PICTURE%d';\n",page_count);
                fputs("   COLRMODE INDEXED;\n",draw_fp);
                fprintf(draw_fp,"   BACKCOLR %d %d %d;\n",carray[curcol].red,
                                                  carray[curcol].green,
                                                  carray[curcol].blue);
                fputs("   VDCEXT 0 0 32767 32767;\n",draw_fp);
                fputs("   LINEWIDTHMODE ABSTRACT;\n",draw_fp);
                fputs("   MARKERSIZEMODE ABSTRACT;\n",draw_fp);
                fputs("   EDGEWIDTHMODE ABSTRACT;\n",draw_fp);
                fputs("   SCALEMODE ABSTRACT 0;\n",draw_fp);
                fputs("BEGPICBODY;\n",draw_fp);
                CGMT_setlw(curwidth); /* page-independent declaration */
                /* print current color table */
                fputs("COLRTABLE 1\n",draw_fp);
                for ( i = 0; i <CMAPSIZE_used; i++){
                   fprintf(draw_fp,"   %d %d %d\n",carray[i].red,
                                              carray[i].green,
                                              carray[i].blue);
                }
                fputs(";\n",draw_fp);
        }
        drawn = FALSE;
        plotlstx = -1;
        plotlsty = -1;

        return(UNUSED);
}
/******************************************************************************/
static int CGMT_color(int col) {
        /* select one of the standard colors from the default pallet */
        if(col >= CMAPSIZE ){
                return(0);
        } else if ( col < 0 ){
                CGMT_setlw(abs(col));
        } else{
                fprintf(draw_fp,"LINECOLR %d;\n",col+1);
                fprintf(draw_fp,"TEXTCOLR %d;\n",col+1);
                fprintf(draw_fp,"EDGECOLR %d;\n",col+1);
                fprintf(draw_fp,"FILLCOLR %d;\n",col+1);
        }
        return(UNUSED);
}
/******************************************************************************/
/* load in small or large */
static int CGMT_font(char *fontname) {
        if (strcmp(fontname, "small") == 0) {
                vdevice.hwidth = 22.0;
                vdevice.hheight = vdevice.hwidth = 1.833;
        } else if (strcmp(fontname, "large") == 0) {
                vdevice.hwidth = 35.0;
                vdevice.hheight = vdevice.hwidth = 1.833;
        } else{
                return(0);
        }

        plotlstx = plotlsty = -1;

        return(1);
}
/******************************************************************************/
static int CGMT_char(char c) { /* CGMT_char output a character */
        fprintf(draw_fp, "TEXT %d %d FINAL '%c';\n",plotlstx, plotlsty, c);
        plotlstx = vdevice.cpVx += (int)vdevice.hwidth;
        drawn = TRUE;
        return(UNUSED);
}
/******************************************************************************/
static int CGMT_string(char *s){ /* output a character string */
        fprintf(draw_fp, "TEXT %d %d FINAL '%s';\n",plotlstx, plotlsty, s);
        plotlstx = vdevice.cpVx += (int)vdevice.hwidth * (int)strlen(s) ;
        drawn = TRUE;
        return(UNUSED);
}
/******************************************************************************/
/* fill a polygon */
static int CGMT_fill(int n,int x[],int y[]) {
        int     i;

        /* only put a linefeed into prints of polygon points every 5 sets of points */
        static char linefeed[2] = {' ','\n'};

        fprintf(draw_fp, "INTSTYLE SOLID;\n");
        fprintf(draw_fp, "POLYGON\n");

        for (i = 0; i < n; i++)
        {
                fprintf(draw_fp, "%d %d%c", x[i], y[i], linefeed[(i % 6)/5]);
        }

        fprintf(draw_fp, ";\n");

        vdevice.cpVx = x[n - 1];
        vdevice.cpVy = y[n - 1];

        drawn = TRUE;

        return(UNUSED);
}
/******************************************************************************/
static int CGMT_sync(void){
        fflush(draw_fp);
        return(UNUSED);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry CGMTdev = {
                "cgmt",        /* name of device */
                "large",       /* name of large font */
                "small",       /* name of small font */
                noop,          /* Set drawing in back buffer */
                CGMT_char,     /* Draw a hardware character */
                noop,          /* Check if a key was hit */
                CGMT_clear,    /* Clear the screen to current color */
                CGMT_color,    /* Set current color */
                CGMT_draw,     /* Draw a line */
                CGMT_exit,     /* Exit graphics */
                CGMT_fill,     /* Fill a polygon */
                CGMT_font,     /* Set hardware font */
                noop,          /* Set drawing in front buffer */
                noop,          /* Wait for and get the next key hit */
                CGMT_init,     /* Initialize the device */
                noop2,         /* Get mouse/cross hair position */
                CGMT_mapcolor, /* Set color indices */
                CGMT_setlw,    /* Set line width */
                CGMT_string,   /* Draw a hardware string */
                noop,          /* Swap front and back buffers */
                CGMT_sync      /* Syncronize the display */
};
/******************************************************************************/
/* copy the device into vdevice.dev.  */
int _CGMT_draw_devcpy(void) {
        vdevice.dev = CGMTdev;
        return(UNUSED);
}

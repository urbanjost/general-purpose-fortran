/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/pbm.c - M_DRAW driver for PBM/X11 bitmaps"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 3.0, Mar 1995,2004"
/*
 PBM/X11 bitmap driver for draw; Version 1.0, John S. Urban, Jan 1995
 PBM/X11 bitmap driver for draw; Version 2.0, John S. Urban, Dec 1996
 PBM/X11 bitmap driver for draw; Version 3.0, John S. Urban, Dec 2004

   This driver makes  pbmplus-readable monochrome (P1 and P4 format)
    bitmaps, an X11 bitmap (xbm) file, and a BM file (eye-readable
   bitmap readable by atobm(1x) for use with bitmap(1x).  The popular
   xv(1), pbmplus(1) and ImageMagick utilities can read these formats.

   The X11 bitmaps are commonly used with xsetroot(1); as icons
   for X11 applications; by the MIT X11 utility bitmap(1x); and are
   usable by Mosaic and other popular WWW browsers as inline
   images. X11 bitmaps can also be used as include files to define
   pixmaps in C X11 code.  Some example uses with basic MIT X11
   utility programs follow:

      xterm -xrm 'XTerm*iconPixmap: file.xbm' # use X11 file as xterm icon
      xsetroot -bitmap file.xbm # set background display of X11 bitmap
      bitmap file.xbm # modify the bitmap with a pixel-oriented editor

   Some recent window managers need coerced into obeying xsetroot.

   I usually use the netplus/pbmplus packages, ImageMagick or
   xv(1) to convert the P1 and P4 files to other formats or to set
   background and foreground color.

   Unfortunately, these formats only support single-frame files.
   You need to use voutput(3[cf]) to keep changing the output file,
   or use csplit(1) on the P1 and xbm files to split them apart.
   For example:

      csplit -k -n 3 OUTPUT.xbm '/\}/+1' '{99999}'

   My original reason for creating this driver was so I could
   generate on-the-fly XY plots for Mosaic interfaces based on
   form-supplied and varying date. It works very well for that
   purpose.

   PS.:

   If you have a newer  version  of the Internet code xv(1)  try
   the emboss algorithm  on a bitmap.  Might not be all that
   functional,  but adds nice visual impact to otherwise plain
   monochrome graphics.

   *-----------------------------------------------------------------*
   | Author: John S. Urban                                           |
   *-----------------------------------*-----------------------------*
   | Westinghouse Electric Corporation | urban.j.s@mts400.pgh.wec.com| OBSOLETE
   *-----------------------------------*-----------------------------*
   | Cray Research, Incorporated       | urban@cray.com              | OBSOLETE
   *-----------------------------------*-----------------------------*
   | Silicon Graphics, Incorporated    | urban@sgi.com               | OBSOLETE
   *-----------------------------------*-----------------------------*
   | Digital Equipment Corp.           | John.Urban@digital.com      | OBSOLETE
   *-----------------------------------*-----------------------------*
   | Compaq                            | John.Urban@compaq.com       | OBSOLETE
   *-----------------------------------*-----------------------------*
   | Hewlett-Packard                   | John.Urban@hp.com           | OBSOLETE
   *-----------------------------------*-----------------------------*
   | Bettis Atomic Power Laboratories  | urbanjs@bettis.gov          |
   *-----------------------------------*-----------------------------*
   Please pass any upgrades back to me.

   Sun Jan 21 02:10:02 EST 1996: Version 1.0
   Sat Dec  7 18:03:48 EST 1996: Version 2.0
      o  Added linethickness entry to driver works with newer M_DRAW version.
      o  obey prefsize(), allocate and release memory for any size.
      o  Solid fill of polygons.
      o  Obey line thickness.
   Sat Dec  7 18:03:48 EST 2004: Version 3.0
      o  Added bitmapped fonts instead of defaulting to futura fonts.
      o  deallocated bitmap array when exiting and other cleanup.

================================================================================
   USAGE NOTES ON THE PBM DRIVER:

   I have used this driver on UNICOS(Cray), Next NextStep, HP-UX, SunOS,
   SGI IRIX and IRIX64, Solaris, Linux, Digital ULTRIX, Compaq Tru64, IBM IAX.

   Line thickness is supported with filled rectangular polygons
   when the line thickness is greater than 1. Square ends are used
   that go only to the endpoints unless line thickness is greater
   than 5, in which case complete circles are added to the endpoints.

   Anything drawn in the background color will be white; all other
   colors will draw as black.  This permits the use of white fill
   and white lines to "erase" regions that have been colored (set
   to black).

   Complex polygons are supported by assuming a move follows any
   return to first point of current polygon.
================================================================================
   Unusual Uses:

   By making relatively simple translators for the P4 output you can
      o print to a Anacomp COM unit that can print  raster images
      o create Xerox IMG graphics for high-speed printers
      o Interface FIDAP to make bitmaps using the user driver interface
================================================================================
    NEXT TIME IN:

   I might expand on this driver, but believe you will find it valuable as-is.
   It would be useful to add
    o different joints for polylines,
    o better support of hardware fonts (possibly by reading in a user pixmap)
    o support dither patterns to simulate color
    o support multi-frame output without requiring user calls to change the output file,
      depending on ending given to voutput (precludes using stdout) or based on a UNIX environment variable
    o probably overindex graphics array when clipping is off
       o  Support for MPEG movies?
       o  make it so a "page" goes to next chunk, allowing rows x columns
    o  straight to JPEG for WWW browser
    o  check machine-independent for low endian, high endian, and word size
    o  allow preloading a P1 or P4 file into the "background" array
    o option to reverse black and white
    o  add an emboss  feature like recent  versions of xv use.  Does pbm
       stuff have a similar algorithm?  If make one, have a raise/lower
       switch (reverse video would work for grayscale output). Probably
       a convolution of a grayscale would work.
    o   fill of any color except background fills in the polygon with user-specified dither patterns

   Might be nice to allow other line terminators similar to X11 or
   PostScript, and make them selectable. Might be nice to have an
   "escape" routine in M_DRAW that allows for non-standard options
   like that to be included in drivers. Would make line thickness
   much less susceptible to output device variations if line
   thickness were specified as a thickness in world coordinates;
   but this means the raster thickness would need calculated every
   time the window/viewport configuration changed or with every
   line draw. The relative width of a raster varies greatly with
   various output devices.

   Consider an escape option to not outline polygons.

   Consider allowing a move in a polygon definition.
================================================================================

 References: 1) Chapter 10, Practical C Programming, Steve Oualline, O'Reilly & Associates, Inc. 1991
             2) Fundamentals of Interactive Computer Graphics, Foley & Van Dam, Addison Wesley Publishing Company, 1982
             3) pbm - portable bitmap file format, 27 September 1991, Copyright (C) 1989, 1991 by Jef Poskanzer.

 From Reference 1:
    In bitmapped  graphics, each pixel on the screen is represented by a
    single bit in memory.  There is no data type for an array of bits in
    C.  The closest we can come is an array of bytes.

    This macro  turns on the bit  (pixel)  located at (x,y).  We need to
    compute two  values:  the  coordinate  of the byte and the number of
    the bit  within  the byte.  Our bit  address  is  (x,y).  Bytes  are
    groups of eight bits so our byte address is (x/8,y).

    The bit  within  the byte is not so  simple.  We want to  generate a
    mask  consisting of the single bit we want to set.  For the leftmost
    bit, this should be 10000000#2 or 80#16.  This occurs when (x%8)==0.
    The next bit is  01000000#2 or (0x80>>1)  and occurs when  (x%8)==1.
    So  in  order  to  generate  our  bitmask,  we  use  the  expression
    (0x80>>(x%8)).

    Now that we have the byte  location and the bit mask, all we have to
    do is set the bit.  The  following  macro  will set a given bit in a
    bitmapped  graphics array named  graphics:
*/
#define MAX(x,y)  ((x) > (y) ? (x) : (y))
#define MIN(x,y)  ((x) < (y) ? (x) : (y))
#define ABS(x)    ((x) < 0 ? -(x) : (x))

#define  SET_BIT_ARRAY(x,y) graphics[ (x)/8] [y] |= (0x80 >> ((x)%8))
/*
#define SET_BIT_ZERO(x,y) (( *(graphics + ( (x) / 8 ) * Y_SIZE + (y) )) |= (0x80 >> ( (x) % 8 ) ) )
#define SET_BIT_ONE(x,y)  (( *(graphics + ( (x) / 8 ) * Y_SIZE + (y) )) &= (0x80 >> ( (x) % 8 ) ) )
*/
#define SET_BIT_ZERO(x,y) (( *(graphics + MAX(MIN(( (x) / 8 ) * Y_SIZE + (y) ,LAST),0))) |= (0x80 >> ( (x) % 8 ) ) )
#define SET_BIT_ONE(x,y)  (( *(graphics + MAX(MIN(( (x) / 8 ) * Y_SIZE + (y) ,LAST),0))) &= (0x80 >> ( (x) % 8 ) ) )
/* if BITVALUE is 0 then set bits to 1, else set bits to 0 */
#define SET_BIT(x,y) (BITVALUE ? SET_BIT_ZERO(x,y) : SET_BIT_ONE(x,y))

/* get bit value at given coordinate */
#define GET_BIT_ARRAY(x,y) ((graphics[(x) / 8][(y)]) & (0x80 >> ((x) % 8)))
#define GET_BIT(x,y)      ( *(graphics + ( (x) / 8 ) *Y_SIZE + (y)) & (0x80 >> (x % 8)))

/******************************************************************************/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#ifndef MINGW
#include <sys/utsname.h>
#include <pwd.h>
#endif
#include <math.h>
#include <math.h>
#include "draw.h"

extern FILE     *_draw_outfile();
extern  FILE     *draw_fp;

#define byte unsigned char

#ifndef PI
#define PI      3.14159265358979323846264338327950288419716939937510
#endif


static int X_SIZE, Y_SIZE, LAST; /* size of graphics array */

byte *graphics; /* the graphics data */

#define XBM   1
#define P1    2
#define P4    3
#define BM    4
#define CHARS 5
static int GLOBAL_driver = 0;
static int GLOBAL_font = 0;

static int GLOBAL_color = 0, GLOBAL_background = 0;
static int BITVALUE=0;

#define UNDRAWN 0
#define DRAWN   1
static int GLOBAL_drawn = UNDRAWN; /* flag whether page is blank or not */

static int GLOBAL_rasters = 1; /* line thickness */

static int GLOBAL_lastx, GLOBAL_lasty;     /* position of last draw */
/******************************************************************************/
/* PROTOTYPES */
static void PBM_SOLID_FILL(int n, int x[], int y[]); /* fill polygon of n points drawn by polyline <x,y>.  */
/******************************************************************************/
static void PBM_MEMSET(void){/* set graphics array to all zero */
        int i;

        /*--- IF YOU HAVE IT, MEMSET IS PROBABLY FASTER
        memset(graphics, 0, sizeof(byte) * Y_SIZE * ((X_SIZE+7)/8));
        ---*/

        for ( i=0; i<((X_SIZE+7) /8) * Y_SIZE; i++) {
           *(graphics + i) = '\0';
        }
}
/******************************************************************************/
static int PBM_init(void) {
        int prefx, prefy, prefxs, prefys;

        /* ---DETERMINE SIZE OF GRAPHICS PIXMAP */
        /* see if a size was user-specified using the prefsize procedure */
        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
                if (prefys <= 0 ){
                        fprintf(stderr,"*PBM_init* y size of %d set to 400\n",prefys);
                        prefys = 400;
                }
                else{
                        vdevice.sizeSy = prefys;
                }
                if (prefxs <= 0 ){
                        fprintf(stderr,"*PBM_init* y size of %d set to 600\n",prefys);
                        prefxs = 600;
                }
                else{
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
        LAST = ((X_SIZE + 7 ) / 8)*Y_SIZE-1;
        graphics = (byte*)draw_vallocate(((X_SIZE + 7 ) / 8)*Y_SIZE,"PBM_init"); /* allocate the graphics data */

        PBM_MEMSET(); /* set the graphics array to 0 */

        vdevice.depth = 1;

        draw_fp = _draw_outfile();

        /* Cause scaling to be 0 to maxX maxY: prefx, vdevice.sizeSx+prefx, prefy, vdevice.sizeSy+prefy */

        GLOBAL_lastx = -1111111;
        GLOBAL_lasty = -1111111;

        GLOBAL_drawn = UNDRAWN;

        return(1);
}
/*******************************************************************************/
static void PBM_DRAW_LINE(int x,int y){ /* draws a line across a graphics array */
        int runcount;
        int dx,dy;
        int xinc,yinc;
        int xplot,yplot;

        SET_BIT(GLOBAL_lastx,GLOBAL_lasty); /* move to initial spot */

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
                        SET_BIT(xplot,yplot);
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
                        SET_BIT(xplot,yplot);
                }
        }
        GLOBAL_lastx = xplot;
        GLOBAL_lasty = yplot;
}
/******************************************************************************/
static void PBM_ENDCAP_CIRCLE(int x, int y){/* Draw a circle on thick line segment end point */
        /* there are more efficient ways to do this */
        /* circle precision */
#define nsegs 15
        /*==================================================================================
        On SunOS pittpa 5.3 Generic_101318-75 sun4m sparc, Sun May 18 16:32:09 EDT 1997
        Got error message when used nsegs to dimension cxras and xyras below,
        changes nsegs to be defined.
        ../drivers/ppm.c", line 334: integral constant expression expected
        static int nsegs= 15;
        const nsegs= 15;
        ==================================================================================*/
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
        PBM_SOLID_FILL(nsegs,cxras,cyras);
}
/******************************************************************************/
static int PBM_fill(int n, int x[], int y[]){/* "fill" a polygon */
        int     i;

        /* update current position if needed */
        GLOBAL_lastx=x[0];
        GLOBAL_lasty=y[0];

        for (i = 1; i < n; i++){
                PBM_DRAW_LINE(x[i],y[i]); /* draw outline across graphics array */
        }
        if ( x[n-1] != x[0] || y[n-1] != y[0] ) /* close the polygon if it is not closed */
                PBM_DRAW_LINE(x[0],y[0]);

        PBM_SOLID_FILL(n, x, y);

        /* update current position */
        GLOBAL_lastx = vdevice.cpVx = x[n - 1];
        GLOBAL_lasty = vdevice.cpVy = y[n - 1];

        GLOBAL_drawn = DRAWN;
        return(0);
}
/******************************************************************************/
static int PBM_draw(int x, int y){/* print the commands to draw a line from the current graphics position to (x, y).  */
        int     holdx, holdy;
        int xwide[4], ywide[4];
        float cosa, sina;
        double angle;


        if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
             GLOBAL_lastx=vdevice.cpVx;
             GLOBAL_lasty=vdevice.cpVy;
        }

        if ( GLOBAL_rasters <= 1){
           PBM_DRAW_LINE(x,y);
        }else{
           /* thick lines are made from filled polygon(s) */
           /* add a circle to ends of really thick lines */
           if( GLOBAL_rasters >= 6){
              holdx=GLOBAL_lastx;
              holdy=GLOBAL_lasty;
              PBM_ENDCAP_CIRCLE(GLOBAL_lastx,GLOBAL_lasty);
              PBM_ENDCAP_CIRCLE(x,y);
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

           PBM_SOLID_FILL(4,xwide,ywide);
        }
        GLOBAL_drawn = DRAWN;
        return(0);
}
/*******************************************************************************/
static void P1_print_graphics(void){ /* print_graphics -- print the graphics bit array as a pbm P1 file*/
        int x; /* current x BYTE */
        int y; /* current y location */
        int bit; /* bit we are testing in the current byte */
        int column, index;
        int write_column = 1;

        time_t tod;
#ifndef MINGW
        struct utsname unstr, *un;
#endif
        char *username;
        struct passwd *pw;

        (void) fprintf(draw_fp,"P1\n"); /* magic number of a pbm file */
        (void) fprintf(draw_fp,"# CREATOR: M_DRAW pbm driver; version 2.0 1996/12/07\n"); /*pbm P1 file can contain comment lines*/
        (void) fprintf(draw_fp,"# V1: 1995, John S. Urban\n");
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

        (void) fprintf(draw_fp,"%d %d\n",X_SIZE,Y_SIZE); /* size of bitmap */

        /* notice going from bottom to top because putting out in a right handed coordinate system, was assuming left-handed */
        for (y = (Y_SIZE-1); y >= 0; y--) {
                /* Loop for each byte in the array */
                column=1; /* count column because pbm P1 files do not have to be multiples of 8 bits wide */
                for ( x = 0; x <(X_SIZE + 7) / 8; x++){
                        /* Handle each bit */
                        for (bit = 0x80; bit > 0;bit = (bit >>1)) {
                                /* file is not multiple of 8 bits wide (but storage array is) and came to last significant column */
                                if (column > X_SIZE)
                                        break;
                                /* output a character representing the bit */
                                index = Y_SIZE * x + y;
                                if (( *(graphics + index) & bit) != 0)
                                        (void) fprintf(draw_fp,"1");
                                else
                                        (void) fprintf(draw_fp,"0");
                                /* increment the dataset column counter */
                                column++;

                                /* The manual says a P1 pbm file should not be wider than 70 characters */
                                write_column++;
                                if ( write_column > 70 )
                                {
                                        (void) fprintf(draw_fp,"\n");
                                        write_column = 1;
                                }
                        }
                } /* end of writing a row */
        } /* end of writing a column */
        (void) fprintf(draw_fp,"\n");
        GLOBAL_drawn = UNDRAWN;
}
/*******************************************************************************/
static void P4_print_graphics(void){ /* print_graphics -- print the graphics bit array as a pbm P4 file*/
        int x; /* current x BYTE */
        int y; /* current y location */
        int index;
        time_t tod;
#ifndef MINGW
        struct utsname unstr, *un;
#endif
        char *username;
        struct passwd *pw;

        (void) fprintf(draw_fp,"P4\n"); /* magic number of a pbm file */
        (void) fprintf(draw_fp,"# CREATOR: M_DRAW pbm driver; version 2.0 1996/12/07\n"); /*pbm P4 file can contain comment lines*/
        (void) fprintf(draw_fp,"# V1: 1995, John S. Urban\n");
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

        (void) fprintf(draw_fp,"%d %d\n",X_SIZE,Y_SIZE); /* size of bitmap */

/* notice going from bottom to top because putting out in a right handed coordinate system, was assuming left-handed */
        for (y = (Y_SIZE-1); y >= 0; y--) {
                for ( x = 0; x <(X_SIZE + 7) / 8; x++){
                        index = Y_SIZE * x + y;
                        putc((char)*(graphics + index),draw_fp);
                }
        }
        GLOBAL_drawn = UNDRAWN;
}
/*******************************************************************************/
static void BM_print_graphics1(void){ /* print_graphics -- print the graphics bit array as a BM file*/
        int x; /* current x BYTE */
        int y; /* current y location */
        int bit; /* bit we are testing in the current byte */
        int column, index;

        /* notice going from bottom to top because putting out in a right handed coordinate system, was assuming left-handed */
        for (y = (Y_SIZE-1); y >= 0; y--) {
                /* Loop for each byte in the array */
                column=1; /* count column because pbm P1 files do not have to be multiples of 8 bits wide */
                for ( x = 0; x <(X_SIZE + 7) / 8; x++){
                        /* Handle each bit */
                        for (bit = 0x80; bit > 0;bit = (bit >>1)) {
                                /* file is not multiple of 8 bits wide (but storage array is) and came to last significant column */
                                if (column > X_SIZE)
                                        break;
                                /* output a character representing the bit */
                                index = Y_SIZE * x + y;
                                if ((*(graphics+index) & bit) != 0)
                                        (void) fprintf(draw_fp,"-");
                                else
                                        (void) fprintf(draw_fp,"#");
                                /* increment the dataset column counter */
                                column++;
                        }
                } /* end of writing a row */
                (void) fprintf(draw_fp,"\n");
        } /* end of writing a column */
        GLOBAL_drawn = UNDRAWN;
}
/*******************************************************************************/
static void BM_print_graphics(void) { /* print_graphics -- print the graphics bit array as a BM file*/
        int x, y; /* current location */
        /* notice going from bottom to top because putting out in a right handed coordinate system, was assuming left-handed */
        for (y = (Y_SIZE-1); y >= 0; y--) {
           for ( x = 0; x < X_SIZE ; x++) GET_BIT(x,y) ?  putc('#',draw_fp) : putc('-',draw_fp);
           (void) putc('\n',draw_fp); /* end of writing a row */
        } /* end of writing columns */
        GLOBAL_drawn = UNDRAWN;
}
/*******************************************************************************/
static void XBM_print_graphics(void){/* print graphics bitmap as an X11 bitmap file */
        /* graphics is a byte array whose width is a multiple of 8 */
        int   x; int   y;
        int   print_column;
        int   nbytes;
        int   index;
        /* ENDIAN char* hexchar = "0123456789abcdef"; */
        char* hexchar = "084c2a6e195d3b7f";
        byte  item;

        /* should probably use filename to build strings */
        fprintf(draw_fp,"#define M_DRAW_width %d\n",X_SIZE);
        fprintf(draw_fp,"#define M_DRAW_height %d\n",Y_SIZE);
        fprintf(draw_fp,"static char M_DRAW_bits[] = {\n ");

        /* number of bytes to write */
        nbytes = Y_SIZE * ((X_SIZE + 7)/8);

        print_column=1;

        x=0;
        y=Y_SIZE-1;

        for (y = (Y_SIZE-1); y >= 0; y--) {
                for ( x = 0; x <(X_SIZE + 7) / 8; x++,nbytes--){
                        index = Y_SIZE * x + y;
                        putc('0',draw_fp);
                        putc('x',draw_fp);
                        item = *(graphics + index);
                        /*
                           cute trick that I probably will not remember for
                           writing hex representation of the bitmap in order
                           desired by X11 bitmap file
                        */
                        putc(hexchar[item & 15],draw_fp);
                        putc(hexchar[item >> 4],draw_fp);
                        print_column += 5;
                        if (print_column>75){
                                putc(',',draw_fp);
                                putc('\n',draw_fp);
                                putc(' ',draw_fp);
                                print_column=1;
                        }else{
                                putc(',',draw_fp);
                        }
                        if(nbytes == 2)
                                break; /* special rules for printing last byte */
                }
        }
        index = Y_SIZE * x + y;
        fprintf(draw_fp,"0x%02x\n};\n",(byte) *(graphics+index)); /* last byte should not be followed by a comma */
        GLOBAL_drawn = UNDRAWN;
}
/******************************************************************************/
static void PBM_PRINT(void){/* exit from draw printing the command to flush the buffer.  */

        if( GLOBAL_drawn ){
                switch(GLOBAL_driver) {
                case P1:
                        P1_print_graphics();
                        break;
                case P4:
                        P4_print_graphics();
                        break;
                case XBM:
                        XBM_print_graphics();
                        break;
                case BM:
                        BM_print_graphics();
                        break;
                default:
                        fprintf(stderr, "pbm driver: UNKNOWN DRIVER NAME\n");
                        P1_print_graphics();
                }
        }
        fflush(draw_fp); /* flush the output file */
}
/******************************************************************************/
static int PBM_exit(void){/* exit from draw printing the command to flush the buffer.  */
        PBM_PRINT();
        free(graphics); /* release the graphics data */
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
/*******************************************************************************/
static int PBM_setlw(int w){/* Set the line width */
        GLOBAL_rasters=w*vdevice.sizeX/10000;
        GLOBAL_rasters = MAX(1,GLOBAL_rasters);
        return(0);
}
/******************************************************************************/
static int PBM_clear(void){/* flush current page and clear graphics array */

        PBM_PRINT();
        PBM_MEMSET();
        GLOBAL_background = GLOBAL_color;
        return(0);

}
/******************************************************************************/
static int PBM_font(char *fontname){ /* load in large or small */
        if (strcmp(fontname, "small") == 0) {
                fprintf(stderr,"set font small\n");
                vdevice.hwidth = 8.00; /* Size in plotter resolution units */
                vdevice.hheight = 13.0;
                GLOBAL_font = 0;
        } else if (strcmp(fontname, "large") == 0) {
                vdevice.hwidth = 15.00;
                vdevice.hheight = 39.00;
                GLOBAL_font = 1;
                /*font("futura.m");*/ /* use software till add second font later */
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
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
{0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x10, 0x00, 0x00 },
{0x24, 0x24, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
{0x00, 0x24, 0x24, 0x7e, 0x24, 0x7e, 0x24, 0x24, 0x00, 0x00, 0x00 },
{0x00, 0x10, 0x3c, 0x50, 0x38, 0x14, 0x78, 0x10, 0x00, 0x00, 0x00 },
{0x22, 0x52, 0x24, 0x08, 0x08, 0x10, 0x24, 0x2a, 0x44, 0x00, 0x00 },
{0x00, 0x00, 0x30, 0x48, 0x48, 0x30, 0x4a, 0x44, 0x3a, 0x00, 0x00 },
{0x38, 0x30, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
{0x04, 0x08, 0x08, 0x10, 0x10, 0x10, 0x08, 0x08, 0x04, 0x00, 0x00 },
{0x20, 0x10, 0x10, 0x08, 0x08, 0x08, 0x10, 0x10, 0x20, 0x00, 0x00 },
{0x00, 0x00, 0x24, 0x18, 0x7e, 0x18, 0x24, 0x00, 0x00, 0x00, 0x00 },
{0x00, 0x00, 0x10, 0x10, 0x7c, 0x10, 0x10, 0x00, 0x00, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x38, 0x30, 0x40, 0x00 },
{0x00, 0x00, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x38, 0x10, 0x00 },
{0x02, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x80, 0x00, 0x00 },
{0x18, 0x24, 0x42, 0x42, 0x42, 0x42, 0x42, 0x24, 0x18, 0x00, 0x00 },
{0x10, 0x30, 0x50, 0x10, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00 },
{0x3c, 0x42, 0x42, 0x02, 0x04, 0x18, 0x20, 0x40, 0x7e, 0x00, 0x00 },
{0x7e, 0x02, 0x04, 0x08, 0x1c, 0x02, 0x02, 0x42, 0x3c, 0x00, 0x00 },
{0x04, 0x0c, 0x14, 0x24, 0x44, 0x44, 0x7e, 0x04, 0x04, 0x00, 0x00 },
{0x7e, 0x40, 0x40, 0x5c, 0x62, 0x02, 0x02, 0x42, 0x3c, 0x00, 0x00 },
{0x1c, 0x20, 0x40, 0x40, 0x5c, 0x62, 0x42, 0x42, 0x3c, 0x00, 0x00 },
{0x7e, 0x02, 0x04, 0x08, 0x08, 0x10, 0x10, 0x20, 0x20, 0x00, 0x00 },
{0x3c, 0x42, 0x42, 0x42, 0x3c, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00 },
{0x3c, 0x42, 0x42, 0x46, 0x3a, 0x02, 0x02, 0x04, 0x38, 0x00, 0x00 },
{0x00, 0x00, 0x10, 0x38, 0x10, 0x00, 0x00, 0x10, 0x38, 0x10, 0x00 },
{0x00, 0x00, 0x10, 0x38, 0x10, 0x00, 0x00, 0x38, 0x30, 0x40, 0x00 },
{0x02, 0x04, 0x08, 0x10, 0x20, 0x10, 0x08, 0x04, 0x02, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x7e, 0x00, 0x00, 0x00, 0x00 },
{0x40, 0x20, 0x10, 0x08, 0x04, 0x08, 0x10, 0x20, 0x40, 0x00, 0x00 },
{0x3c, 0x42, 0x42, 0x02, 0x04, 0x08, 0x08, 0x00, 0x08, 0x00, 0x00 },
{0x3c, 0x42, 0x42, 0x4e, 0x52, 0x56, 0x4a, 0x40, 0x3c, 0x00, 0x00 },
{0x18, 0x24, 0x42, 0x42, 0x42, 0x7e, 0x42, 0x42, 0x42, 0x00, 0x00 },
{0xfc, 0x42, 0x42, 0x42, 0x7c, 0x42, 0x42, 0x42, 0xfc, 0x00, 0x00 },
{0x3c, 0x42, 0x40, 0x40, 0x40, 0x40, 0x40, 0x42, 0x3c, 0x00, 0x00 },
{0xfc, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0xfc, 0x00, 0x00 },
{0x7e, 0x40, 0x40, 0x40, 0x78, 0x40, 0x40, 0x40, 0x7e, 0x00, 0x00 },
{0x7e, 0x40, 0x40, 0x40, 0x78, 0x40, 0x40, 0x40, 0x40, 0x00, 0x00 },
{0x3c, 0x42, 0x40, 0x40, 0x40, 0x4e, 0x42, 0x46, 0x3a, 0x00, 0x00 },
{0x42, 0x42, 0x42, 0x42, 0x7e, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00 },
{0x7c, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00 },
{0x1e, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x44, 0x38, 0x00, 0x00 },
{0x42, 0x44, 0x48, 0x50, 0x60, 0x50, 0x48, 0x44, 0x42, 0x00, 0x00 },
{0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x40, 0x7e, 0x00, 0x00 },
{0x82, 0x82, 0xc6, 0xaa, 0x92, 0x92, 0x82, 0x82, 0x82, 0x00, 0x00 },
{0x42, 0x42, 0x62, 0x52, 0x4a, 0x46, 0x42, 0x42, 0x42, 0x00, 0x00 },
{0x3c, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00 },
{0x7c, 0x42, 0x42, 0x42, 0x7c, 0x40, 0x40, 0x40, 0x40, 0x00, 0x00 },
{0x3c, 0x42, 0x42, 0x42, 0x42, 0x42, 0x52, 0x4a, 0x3c, 0x02, 0x00 },
{0x7c, 0x42, 0x42, 0x42, 0x7c, 0x50, 0x48, 0x44, 0x42, 0x00, 0x00 },
{0x3c, 0x42, 0x40, 0x40, 0x3c, 0x02, 0x02, 0x42, 0x3c, 0x00, 0x00 },
{0xfe, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x00 },
{0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00 },
{0x82, 0x82, 0x44, 0x44, 0x44, 0x28, 0x28, 0x28, 0x10, 0x00, 0x00 },
{0x82, 0x82, 0x82, 0x82, 0x92, 0x92, 0x92, 0xaa, 0x44, 0x00, 0x00 },
{0x82, 0x82, 0x44, 0x28, 0x10, 0x28, 0x44, 0x82, 0x82, 0x00, 0x00 },
{0x82, 0x82, 0x44, 0x28, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x00 },
{0x7e, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x40, 0x7e, 0x00, 0x00 },
{0x3c, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x3c, 0x00, 0x00 },
{0x80, 0x80, 0x40, 0x20, 0x10, 0x08, 0x04, 0x02, 0x02, 0x00, 0x00 },
{0x78, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x78, 0x00, 0x00 },
{0x10, 0x28, 0x44, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00 },
{0x38, 0x18, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x3c, 0x02, 0x3e, 0x42, 0x46, 0x3a, 0x00, 0x00 },
{0x40, 0x40, 0x40, 0x5c, 0x62, 0x42, 0x42, 0x62, 0x5c, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x40, 0x40, 0x42, 0x3c, 0x00, 0x00 },
{0x02, 0x02, 0x02, 0x3a, 0x46, 0x42, 0x42, 0x46, 0x3a, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x7e, 0x40, 0x40, 0x3c, 0x00, 0x00 },
{0x1c, 0x22, 0x20, 0x20, 0x7c, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x3a, 0x44, 0x44, 0x38, 0x40, 0x3c, 0x42, 0x3c },
{0x40, 0x40, 0x40, 0x5c, 0x62, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00 },
{0x00, 0x10, 0x00, 0x30, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00 },
{0x00, 0x04, 0x00, 0x0c, 0x04, 0x04, 0x04, 0x04, 0x44, 0x44, 0x38 },
{0x40, 0x40, 0x40, 0x44, 0x48, 0x70, 0x48, 0x44, 0x42, 0x00, 0x00 },
{0x30, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x7c, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0xec, 0x92, 0x92, 0x92, 0x92, 0x82, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x5c, 0x62, 0x42, 0x42, 0x42, 0x42, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x42, 0x42, 0x42, 0x3c, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x5c, 0x62, 0x42, 0x62, 0x5c, 0x40, 0x40, 0x40 },
{0x00, 0x00, 0x00, 0x3a, 0x46, 0x42, 0x46, 0x3a, 0x02, 0x02, 0x02 },
{0x00, 0x00, 0x00, 0x5c, 0x22, 0x20, 0x20, 0x20, 0x20, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x3c, 0x42, 0x30, 0x0c, 0x42, 0x3c, 0x00, 0x00 },
{0x00, 0x20, 0x20, 0x7c, 0x20, 0x20, 0x20, 0x22, 0x1c, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x44, 0x44, 0x44, 0x44, 0x44, 0x3a, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x44, 0x44, 0x44, 0x28, 0x28, 0x10, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x82, 0x82, 0x92, 0x92, 0xaa, 0x44, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x42, 0x24, 0x18, 0x18, 0x24, 0x42, 0x00, 0x00 },
{0x00, 0x00, 0x00, 0x42, 0x42, 0x42, 0x46, 0x3a, 0x02, 0x42, 0x3c },
{0x00, 0x00, 0x00, 0x7e, 0x04, 0x08, 0x10, 0x20, 0x7e, 0x00, 0x00 },
{0x0e, 0x10, 0x10, 0x08, 0x30, 0x08, 0x10, 0x10, 0x0e, 0x00, 0x00 },
{0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x00, 0x00 },
{0x70, 0x08, 0x08, 0x10, 0x0c, 0x10, 0x08, 0x08, 0x70, 0x00, 0x00 },
{0x24, 0x54, 0x48, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 }
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
static void PBM_char0(char c) { /* PBM_char output a small character */

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
        /*fprintf(stderr,"PBM_char %c %d\n",c,k);*/
        for (i=0;i<11;i++){
           wrd=font8x13[k][i];
           /*fprintf(stderr,"x y= %d %d\n",vdevice.cpVx,vdevice.cpVy);*/
           for ( j=0; j<8; j++ ){
              if ((wrd & (0x80 >> j)) != 0){
                  SET_BIT((int)(j+vdevice.cpVx),(int)(vdevice.cpVy-i+13));
                  /*fprintf(stderr,"#"); */
              }else{
                  /*fprintf(stderr,"."); */
              }
           }
           /*fprintf(stderr,"0x%2.2x\n",wrd); */
        }
        /*fprintf(stderr,"===============\n"); */
        vdevice.cpVx+= 8;
        GLOBAL_lastx=vdevice.cpVx;
        GLOBAL_lasty=vdevice.cpVy;
        GLOBAL_drawn = DRAWN;
}
/******************************************************************************/
static void PBM_char1(char c) {
  int             i,j,k;
  short int       wrd;
/* FROW 25 FCOL 9 NUMCHARS 95 */
        k=(int)c-32; /* get which row to use out of the bitpatterns */
        if(k < 0 || k > 95 ){
           return;
        }
        for (i=0;i<25;i++){
           wrd=font13x25[k][i];
           for (j=0;j<13;j++){
              if (((wrd  >> j) & 1) != 0){
                  SET_BIT((int)(j+vdevice.cpVx),(int)(vdevice.cpVy+i));
                  /*fprintf(stdout,"#"); */
              }else{
                  /*fprintf(stdout,"."); */
              }
           }
           /*fprintf(stdout," 0x%4.4X\n",wrd);*/
        }
           /*fprintf(stdout,"%d ----------\n",k); */
        vdevice.cpVx+= 15;
        GLOBAL_lastx=vdevice.cpVx;
        GLOBAL_lasty=vdevice.cpVy;
        GLOBAL_drawn = DRAWN;
}
/******************************************************************************/
static int PBM_char(char c) { /* PBM_char output a hardware character */
   if (GLOBAL_font == 0){
       PBM_char0(c);
   }else{
       PBM_char1(c);
   }
   return(0);
}
/******************************************************************************/
static int PBM_string(char *s){/* output a string.  */
        int i;

        if (GLOBAL_lastx != vdevice.cpVx || GLOBAL_lasty != vdevice.cpVy){
                GLOBAL_lastx=vdevice.cpVx;
                GLOBAL_lasty=vdevice.cpVy;
        }

        fprintf(stderr,"PBM_string %s\n",s);
        for(i=0; s[i]!='\0'; i++){
          PBM_char(s[i]);
        }
        return(0);
}
/*******************************************************************************/
static int PBM_color(int col){/* change the current color */
        if ( col < 0 ){
                PBM_setlw(ABS(col));
        }else{
                GLOBAL_color = ABS(col) % 8;
                 /* if new color is not the background color, set bits; else clear bits */
                if (GLOBAL_color != GLOBAL_background ){
                 BITVALUE = 1;
                }else{
                 BITVALUE = 0;
                }
        }
        /* if BITVALUE is 0 then set bits to 1, else set bits to 0 */
       return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
static int noop4(int a, int b, int c, int d) { return(-1); }
/******************************************************************************/
static DevEntry PBMdev = {
                "PBM",       /* name of device */
                "large",     /* name of large font */
                "small",     /* name of small font */
                noop,        /* Set drawing in back buffer */
                PBM_char,    /* Draw a hardware character */
                noop,        /* Check if a key was hit */
                PBM_clear,   /* Clear the screen to current color */
                PBM_color,   /* Set current color */
                PBM_draw,    /* Draw a line */
                PBM_exit,    /* Exit graphics */
                PBM_fill,    /* Fill a polygon */
                PBM_font,    /* Set hardware font */
                noop,        /* Set drawing in front buffer */
                noop,        /* Wait for and get the next key hit */
                PBM_init,    /* Initialize the device */
                noop2,       /* Get mouse/cross hair position */
                noop4,       /* Set color indices */
                PBM_setlw,   /* Set line width */
                PBM_string,  /* Draw a hardware string */
                noop,        /* Swap front and back buffers */
                noop         /* Syncronize the display */
};
/******************************************************************************/
int _PBM_draw_devcpy(void) {
        vdevice.dev = PBMdev;
        vdevice.dev.Vinit = PBM_init;
        vdevice.dev.devname = "pbm";
        GLOBAL_driver = P1;
        return(0);
}
/******************************************************************************/
int _BM_draw_devcpy(void){
        vdevice.dev = PBMdev;
        vdevice.dev.Vinit = PBM_init;
        vdevice.dev.devname = "bm";
        GLOBAL_driver = BM;
        return(0);
}
/******************************************************************************/
int _P1_draw_devcpy(void){
        vdevice.dev = PBMdev;
        vdevice.dev.Vinit = PBM_init;
        vdevice.dev.devname="p1";
        GLOBAL_driver = P1;
        return(0);
}
/******************************************************************************/
int _XBM_draw_devcpy(void) {
        vdevice.dev = PBMdev;
        vdevice.dev.Vinit = PBM_init;
        vdevice.dev.devname="xbm";
        GLOBAL_driver = XBM;
        return(0);
}
/******************************************************************************/
int _P4_draw_devcpy(void) {
        vdevice.dev = PBMdev;
        vdevice.dev.Vinit = PBM_init;
        vdevice.dev.devname="p4";
        GLOBAL_driver = P4;
        return(0);
}
/*******************************************************************************/
static int PBM_YINTERCEPT(int yscan, int x1, int y1, int x2, int y2, int *xintercept,int *yprev){
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
static void PBM_SOLID_FILL(int n, int x[], int y[]){/* fill polygon of n points drawn by polyline <x,y>.  */
        int i, j, sorted, yhorizontal, xint, tmp, xmin, xmax, ymax, ymin, xi[MAXVERTS], yprev;

        if ( n > MAXVERTS) {
           fprintf(stderr,"*PBM_SOLID_FILL* more than %d vertices in a polygon\n",MAXVERTS);
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
                        if (PBM_YINTERCEPT(yhorizontal, x[i], y[i], x[i+1], y[i+1], &xint, &yprev))
                                        xi[j++] = xint;
                /* Last one. */
                if (PBM_YINTERCEPT(yhorizontal, x[n-1], y[n-1], x[0], y[0], &xint, &yprev))
                                xi[j++] = xint;

                /* odd pairs means something went wrong in figuring out whether to count vertices or not */
                if( 2 * (j/2) != j){
                   fprintf(stderr,"*PBM_SOLID_FILL* Internal error: odd number of intersection points (%d) \n",j);
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
                        PBM_DRAW_LINE(MAX(0,MIN(xi[i+1],X_SIZE)), yhorizontal);
                }
                yhorizontal -= 1;
        }
}
/*******************************************************************************/
/******************************************************************************
 pbm(5)                                                               pbm(5)
                              27 September 1991

 NAME
      pbm - portable bitmap file format

 DESCRIPTION
      The portable bitmap format is a lowest common denominator monochrome
      file format.  It was originally designed to make it reasonable to mail
      bitmaps between different types of machines using the typical stupid
      network mailers we have today.  Now it serves as the common language
      of a large family of bitmap conversion filters.  The definition is as
      follows:

      - A "magic number" for identifying the file type.  A pbm file's magic
        number is the two characters "P1".

      - Whitespace (blanks, TABs, CRs, LFs).

      - A width, formatted as ASCII characters in decimal.

      - Whitespace.

      - A height, again in ASCII decimal.

      - Whitespace.

      - Width * height bits, each either '1' or '0', starting at the top-
        left corner of the bitmap, proceeding in normal English reading
        order.

      - The character '1' means black, '0' means white.

      - Whitespace in the bits section is ignored.

      - Characters from a "#" to the next end-of-line are ignored
        (comments).

      - No line should be longer than 70 characters.

      Here is an example of a small bitmap in this format:
      P1
      # feep.pbm
      24 7
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 1 1 1 0
      0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0
      0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 0 0 0 1 1 1 1 0
      0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0
      0 1 0 0 0 0 0 1 1 1 1 0 0 1 1 1 1 0 0 1 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

      Programs that read this format should be as lenient as possible,
      accepting anything that looks remotely like a bitmap.

      <-- Suggestions, for example:
        o  allow any width of lines
        o  look at characters and make first one off and next on one
           if characters are not 0 and 1
      JSU-->

      There is also a variant on the format, available by setting the
      RAWBITS option at compile time.  This variant is different in the
      following ways:

      - The "magic number" is "P4" instead of "P1".

      - The bits are stored eight per byte, high bit first low bit last.

      - No whitespace is allowed in the bits section, and only a single
        character of whitespace (typically a newline) is allowed after the
        height.

      - The files are eight times smaller and many times faster to read and
        write.

 SEE ALSO
      atktopbm(1), brushtopbm(1), cmuwmtopbm(1), g3topbm(1), gemtopbm(1),
      icontopbm(1), macptopbm(1), mgrtopbm(1), pi3topbm(1), xbmtopbm(1),
      ybmtopbm(1), pbmto10x(1), pnmtoascii(1), pbmtoatk(1), pbmtobbnbg(1),
      pbmtocmuwm(1), pbmtoepson(1), pbmtog3(1), pbmtogem(1), pbmtogo(1),
      pbmtoicon(1), pbmtolj(1), pbmtomacp(1), pbmtomgr(1), pbmtopi3(1),
      pbmtoplot(1), pbmtoptx(1), pbmtox10bm(1), pbmtoxbm(1), pbmtoybm(1),
      pbmtozinc(1), pbmlife(1), pbmmake(1), pbmmask(1), pbmreduce(1),
      pbmtext(1), pbmupc(1), pnm(5), pgm(5), ppm(5)

 AUTHOR
      Copyright (C) 1989, 1991 by Jef Poskanzer.
 ******************************************************************************/

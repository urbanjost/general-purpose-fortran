/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/tek.c - M_DRAW driver for Tektronix 401x or equivalent (like X11 xterm)"
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
/*--------------------------*/
#ifdef BSD
#include <sgtty.h>
#elif Linux
#include <termio.h>
#elif CYGWIN
#include <termio.h>
#elif Darwin
#include <termios.h>
#elif HPUX
#include <sys/termio.h>
#else
#include <termio.h>
#endif
/*--------------------------*/
#include "draw.h"

#define         MASK            037
#define         BELL            '\007'
#define         FF              '\014'
#define         CAN             '\030'
#define         SUB             '\032'
#define         ESC             '\033'
#define         GS              '\035'
#define         US              '\037'
#define         ETX             '\003'

#define TEK_X_SIZE              1023
#define TEK_Y_SIZE              767

#include <signal.h>

static int      LoYold = -1, HiYold = -1, HiXold = -1;
static int      tlstx, tlsty;
static int      click;                  /* to emulate a mouse click */

extern FILE *draw_fp;
extern FILE *_draw_outfile();

static int xterm=-1; /* tek or xterm */

/* 1024 might need to be changed for xterm; maybe 197 OK; should use ENVIRONMENT variable VTEKPAUSE */
static int PAUSE;

static int TEKMODE=-1;
/******************************************************************************/
static void XTERM_4010(){
   int i;
   if (draw_fp != stdout && draw_fp != stderr)return;
/* already assumed in TEK mode */
   if (TEKMODE == 1) return;
   fflush(draw_fp);
   putc(ESC,draw_fp);
   putc('[',draw_fp);
   putc('?',draw_fp);
   putc('3',draw_fp);
   putc('8',draw_fp);
   putc('h',draw_fp);
   fflush(draw_fp);
   for (i=1;i<PAUSE;i++){
      putc(0,draw_fp);
   }
   fflush(draw_fp);
   TEKMODE=1;
   return;
}
/******************************************************************************/
static void XTERM_vt102(){
   int i;
   if (draw_fp != stdout && draw_fp != stderr)return;
   if (TEKMODE == 0) return; /* already in VT102 mode */
   fflush(draw_fp);
   putc(ESC,draw_fp);
   putc(ETX,draw_fp);
   /* putc('T',draw_fp); */
   fflush(draw_fp);
   for (i=1;i<PAUSE;i++){
      putc(0,draw_fp);
   }
   fflush(draw_fp);
   TEKMODE=0;
   return;
}
/******************************************************************************/
/* TEK_init set up the graphics mode.  */
static int TEK_init(void) {
        /* actually only need to set modes in xhair and pause routines */
        char            *varname;

        vdevice.depth = 1;
        draw_fp = _draw_outfile();
        putc(GS,draw_fp);                    /* enter graphics mode */

        vdevice.sizeX = vdevice.sizeY = TEK_Y_SIZE;
        vdevice.sizeSx = TEK_X_SIZE;
        vdevice.sizeSy = TEK_Y_SIZE;

        tlstx = tlsty = -1;
        TEKMODE=-2; /* -1 ... nothing yet, do next thing ; vt102 mode=0 tek4010 mode=1 */

        varname=getenv("VTEKPAUSE");
        if(varname==(char *)NULL)
        {
           varname="2560";
        }
        sscanf(varname,"%d",&PAUSE);
        fprintf(stderr,"*TEK_init* PAUSE=%d\n",PAUSE);

        return(1);
}
/******************************************************************************/
/* cleans up before going back to normal mode */
static int TEK_exit(void) {
        if(xterm==0)XTERM_4010();
        putc(US,draw_fp);
        putc(CAN,draw_fp);
        TEKMODE=-3; /* do xterm settings for sure */
        if(xterm==0)XTERM_vt102();
        fflush(draw_fp);
        if (draw_fp != stdout && draw_fp != stderr){
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
        }
        TEKMODE=-3; /* do xterm settings for sure */
        return(UNUSED);
}
/******************************************************************************/
/* out_bytes output 2 optimized bytes to the terminal */
static void out_bytes(int x,int y) {
        int HiY, LoY, HiX, LoX;

        if (x < 0)                      /* a bit of last minute checking */
                x = 0;
        if (y < 0)
                y = 0;
        if (x > 1023)
                x = 1023;
        if (y > 1023)
                y = 1023;

        HiY = y / 32  +  32;
        LoY = y % 32  +  96;
        HiX = x / 32  +  32;
        LoX = x % 32  +  64;

        /*
         * optimize the output stream. Ref: Tektronix Manual.
         */

        if (HiYold != HiY) {
                fprintf(draw_fp,"%c", HiY);
                HiYold = HiY;
        }

        if ((LoYold != LoY) || (HiXold != HiX)) {
                fprintf(draw_fp,"%c", LoY);
                LoYold = LoY;
                if (HiXold != HiX) {
                        fprintf(draw_fp,"%c", HiX);
                        HiXold = HiX;
                }
        }

        fprintf(draw_fp,"%c", LoX);
}
/******************************************************************************/
/* TEK_draw draw from the current graphics position to the new one (x, y) */
static int TEK_draw(int x, int y) {
        if(xterm==0 )XTERM_4010();
        if (tlstx != vdevice.cpVx || tlsty != vdevice.cpVy) {
                putc(GS,draw_fp);
                LoYold = HiYold = HiXold = -1;  /* Force output of all bytes */
                out_bytes(vdevice.cpVx, vdevice.cpVy);
        }

        out_bytes(x, y);
        tlstx = x;
        tlsty = y;

        if(xterm==0 )XTERM_vt102();
        if (draw_fp == stdout || draw_fp == stderr){
           fflush(draw_fp);
        }
        return(UNUSED);
}
/******************************************************************************/
/* TEK_getkey return the next key typed.  */
static int TEK_getkey(void) {
#ifdef BSD
        struct sgttyb   oldtty, newtty;
        char            c;

        TEKMODE=-4; /* do xterm settings for sure */
        if(xterm==0 )XTERM_4010();

        ioctl(0, TIOCGETP, &oldtty);

        newtty = oldtty;
        newtty.sg_flags = RAW;

        ioctl(0, TIOCSETP, &newtty);

        read(0, &c, 1);

        ioctl(0, TIOCSETP, &oldtty);
#else
        struct termio   oldtty, newtty;
        char            c;

        if(xterm==0 )XTERM_4010();

        ioctl(0, TCGETA, &oldtty);

        newtty = oldtty;
        newtty.c_iflag = BRKINT | IXON | ISTRIP;
        newtty.c_lflag = 0;
        newtty.c_cc[VEOF] = 1;

        ioctl(0, TCSETA, &newtty);

        read(0, &c, 1);

        ioctl(0, TCSETA, &oldtty);
#endif

        if(xterm==0 )XTERM_vt102();
        return(c);
}
/******************************************************************************/
/*
 * TEK_locator
 *
 *      get the position of the crosshairs. This gets a bit sticky since
 * we have no mouse, and the crosshairs do not behave like a mouse - even a rat!
 * In this case the keys 1 to 9 are used, with each one returning a power of
 * two.
 */
static int TEK_locator(int *x, int *y) {
        char            buf[5];
        int             i;
#ifdef BSD
        struct sgttyb   oldtty, newtty;
#else
        struct termio   oldtty, newtty;
#endif

        if (click) {                    /* for compatibility with other devs */
                click = 0;
                return(0);
        }

        click = 1;
        TEKMODE=-5; /* do xterm settings for sure */
        if(xterm==0)XTERM_4010();

#ifdef BSD
        ioctl(0, TIOCGETP, &oldtty);

        newtty = oldtty;
        newtty.sg_flags = RAW;

        ioctl(0, TIOCSETP, &newtty);
#else
        ioctl(0, TCGETA, &oldtty);

        newtty = oldtty;
        newtty.c_iflag = BRKINT | IXON | ISTRIP;
        newtty.c_lflag = 0;
        newtty.c_cc[VEOF] = 1;

        ioctl(0, TCSETA, &newtty);
#endif

        fputs("\037\033\032", draw_fp);
        if (draw_fp == stdout || draw_fp == stderr){
           fflush(draw_fp);
        }

        /* Tek 4010/4014 return 8 bytes upon cross-hair read:
         *
         *
         *              0       character pressed
         *
         *              1,2     encoded x position
         *              3,4     encoded y position
         *              5,6     CR,LF           - ignored
         *              7       EOF             - ignored
         */

        /* first we read in the five meaningful bytes */

        for (i = 0; i < 5; i++)
                buf[i] = getchar();

        /* just in case we get the newline chars */

#ifdef BSD
        ioctl(0, TIOCFLUSH, (char *)NULL);
#else
        ioctl(0, TCFLSH, (char *)NULL);
#endif

        *x = ((buf[1] & MASK) << 5) | (buf[2] & MASK);
        *y = ((buf[3] & MASK) << 5) | (buf[4] & MASK);

#ifdef BSD
        ioctl(0, TIOCSETP, &newtty);
#else
        ioctl(0, TCSETA, &oldtty);
#endif

        tlstx = tlsty = -1;

        if(xterm==0)XTERM_vt102();
        return(1 << ((int)buf[0] - '1'));
}
/******************************************************************************/
/*
 *      clear the screen.
 *
 *      NOTE - You may need to actually output a certain number of
 *      NUL chars here to get the (at least) 1 second delay.
 *      This may occur if running through a communication package
 *      or with something like ethernet. If this is the case then
 *      throw away the sleep(2) and bung in a loop.
 *      Here's a sample ...
 *
 *      for (i = 0; i < 960; i++) putc(0,draw_fp);
 *
 *      (for 9600 baud rate)
 *
 */
static int TEK_clear(void) {
        if(xterm==0)XTERM_4010();
        putc(US,draw_fp);
        putc(ESC,draw_fp);
        putc(FF,draw_fp);
        if (draw_fp == stdout || draw_fp == stderr){
           fflush(draw_fp);
        }

        tlstx = tlsty = -1;

        if(xterm==0){
           TEKMODE=-6; /* do xterm settings for sure */
           XTERM_vt102();
        }else{
           if (draw_fp == stdout || draw_fp == stderr){
              fflush(draw_fp);
              sleep(1); /* for Tektronix slow erase */
           }
        }
        return(UNUSED);
}
/******************************************************************************/
/* set for large or small mode.  */
static int TEK_font(char *font) {
        if(xterm==0)XTERM_4010();
        if (strcmp(font, "small") == 0) {
                fprintf(draw_fp,"\033:");
                vdevice.hwidth = 8.0;
                vdevice.hheight = 15.0;
        } else if (strcmp(font, "large") == 0) {
                fprintf(draw_fp,"\0338");
                vdevice.hwidth = 14.0;
                vdevice.hheight = 17.0;
        } else {
                if(xterm==0)XTERM_vt102();
                return(0);
        }

        tlstx = tlsty = -1;

        if(xterm==0)XTERM_vt102();
        return(1);
}
/******************************************************************************/
/* outputs one char */
static int TEK_char(char c) {
        if(xterm==0)XTERM_4010();
        if (tlstx != vdevice.cpVx || tlsty != vdevice.cpVy) {
                putc(GS,draw_fp);
                LoYold = HiYold = HiXold = -1;  /* Force output of all bytes */
                out_bytes(vdevice.cpVx, vdevice.cpVy);
        }

        putc(US,draw_fp);
        putc(c,draw_fp);

        tlstx = tlsty = -1;

        if(xterm==0)XTERM_vt102();
        if (draw_fp == stdout || draw_fp == stderr){
           fflush(draw_fp);
        }
        return(UNUSED);
}
/******************************************************************************/
/* outputs a string */
static int TEK_string(char *s) {
        if(xterm==0)XTERM_4010();
        if (tlstx != vdevice.cpVx || tlsty != vdevice.cpVy) {   /* move to start */
                putc(GS,draw_fp);
                LoYold = HiYold = HiXold = -1;  /* Force output of all bytes */
                out_bytes(vdevice.cpVx, vdevice.cpVy);
        }

        putc(US,draw_fp);
        fputs(s, draw_fp);

        tlstx = tlsty = -1;

        if(xterm==0)XTERM_vt102();
        if (draw_fp == stdout || draw_fp == stderr){
           fflush(draw_fp);
        }
        return(UNUSED);
}
/******************************************************************************/
/* "fill" a polygon */
static int TEK_fill(int n, int x[], int y[]) {
        int     i;

        if(xterm==0)XTERM_4010();
        if (tlstx != x[0] || tlsty != y[0]) {
                putc(GS,draw_fp);
                LoYold = HiYold = HiXold = -1;  /* Force output of all bytes */
                out_bytes(x[0], y[0]);
        }

        for (i = 1; i < n; i++)
                out_bytes(x[i], y[i]);

        out_bytes(x[0], y[0]);

        if (draw_fp == stdout || draw_fp == stderr){
           fflush(draw_fp);
        }

        tlstx = vdevice.cpVx = x[n - 1];
        tlsty = vdevice.cpVy = y[n - 1];
        if(xterm==0)XTERM_vt102();
        return(UNUSED);
}
/******************************************************************************/
/* flush the tektronix device (and force return to vt102 mode on xterm) */
static int TEK_sync(void) {

        if (draw_fp == stdout || draw_fp == stderr){
           fflush(draw_fp);
        }
        TEKMODE=-1; /* do xterm settings for sure */
        if(xterm==0)XTERM_vt102();
        return(UNUSED);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop1(int x) { return(-1); }
static int noop4(int a, int b, int c, int d) { return(-1); }
/******************************************************************************/
static DevEntry tekdev = {
                "tek",         /* name of device */
                "large",       /* name of large font */
                "small",       /* name of small font */
                noop,          /* Set drawing in back buffer */
                TEK_char,      /* Draw a hardware character */
                noop,          /* Check if a key was hit */
                TEK_clear,     /* Clear the screen to current color */
                noop1,         /* Set current color */
                TEK_draw,      /* Draw a line */
                TEK_exit,      /* Exit graphics */
                TEK_fill,      /* Fill a polygon */
                TEK_font,      /* Set hardware font */
                noop,          /* Set drawing in front buffer */
                TEK_getkey,    /* Wait for and get the next key hit */
                TEK_init,      /* Initialize the device */
                TEK_locator,   /* Get mouse/cross hair position */
                noop4,         /* Set color indices */
                noop1,         /* Set line width */
                TEK_string,    /* Draw a hardware string */
                noop,          /* Swap front and back buffers */
                TEK_sync       /* Syncronize the display */
};
/******************************************************************************/
/* copy the tektronix device into vdevice.dev.  */
int _TEK_draw_devcpy(void) {

        vdevice.dev = tekdev;
        vdevice.dev.Vinit = TEK_init;
        xterm=-1;
        return(UNUSED);
}
/******************************************************************************/
/* copy the xterm(1) version of the tektronix device into vdevice.dev.  */
int _XTEK_draw_devcpy(void) {

        vdevice.dev = tekdev;
        vdevice.dev.Vinit = TEK_init;
        xterm=0;
        return(UNUSED);
}
/******************************************************************************/


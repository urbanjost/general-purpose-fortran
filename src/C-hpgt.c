/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/hpgt.c - M_DRAW driver for HP Graphics Terminals"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Mar 1999"
/*
#define BSD 1
 */
#include <stdio.h>
#include <string.h>

/*
#include <stropts.h>
*/
#include <sys/ioctl.h>

#include <unistd.h>

#ifdef BSD
#include <sgtty.h>
#elif HPUX
#include <sys/termio.h>
#else
#include <termio.h>
#endif
#include "draw.h"

#define HPGT_X_SIZE             640
#define HPGT_Y_SIZE             400

#include <signal.h>

static int      click, tlstx, tlsty;
extern  FILE     *draw_fp;
extern FILE     *_draw_outfile();

/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* set for large or small mode.  */
static int HPGT_font(char *font) {
        int     size;
        vdevice.hwidth = 5.0;
        vdevice.hheight = 7.0;
        size = 2;

        if (strcmp(font, "5x7") == 0)
                size = 1;
        else if ((strcmp(font, "10x14") == 0) || (strcmp(font, "small") == 0))
                size = 2;
        else if ((strcmp(font, "15x21") == 0) || (strcmp(font, "large") == 0))
                size = 3;
        else if (strcmp(font, "20x28") == 0)
                size = 4;
        else if (strcmp(font, "25x35") == 0)
                size = 5;
        else if (strcmp(font, "30x42") == 0)
                size = 6;
        else if (strcmp(font, "35x49") == 0)
                size = 7;
        else if (strcmp(font, "40x56") == 0)
                size = 8;

        vdevice.hwidth *= size;
        vdevice.hheight *= size;
        fprintf(draw_fp, "\033*m%dm", size);
        tlstx = tlsty = -1;
        fflush(draw_fp);

        return(1);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* set up the graphics mode.  */
static int HPGT_init(void) {
        /*
         * Find out if we are mono or colour.
         * and what our resolution currently is.
         */
/*
        char    buf[128];
*/
        int     i, b1, b2, b3, llx=0, lly=0, urx=0, ury=0;

        click = 0;
        draw_fp = _draw_outfile();

/*
        fprintf(draw_fp, "\033*s5^");
        fgets(buf, 128, stdin);
        sscanf(buf, "%d,%d,%d,%d\n", &llx, &lly, &urx, &ury);
        vdevice.depth = 1;
        fprintf(draw_fp, "\033*s6^");
        fgets(buf, 128, stdin);
        sscanf(buf, "%d,%d,%d", &b1, &b2, &b3);

        if (b3 == 0)
                vdevice.depth = 1;
        else
                vdevice.depth = 3;
        */

        vdevice.depth = 3;

        fprintf(draw_fp, "\033*dC");
        fprintf(draw_fp, "\033*dF");
        /*
         * Solid area fills
         */
        fprintf(draw_fp, "\033*m1g");
        /*
         * Clear graphics memory
         */
        fprintf(draw_fp, "\033*dA");

        vdevice.sizeSx = urx - llx;
        vdevice.sizeSy = ury - lly;
        vdevice.sizeSx = HPGT_X_SIZE;
        vdevice.sizeSy = HPGT_Y_SIZE;
        vdevice.sizeX = vdevice.sizeY = vdevice.sizeSy;
        tlstx = tlsty = -1;
        HPGT_font("5x7");
        fflush(draw_fp);

        return(1);
}


/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* cleans up before going back to normal mode */
static int HPGT_exit(void) {
        /*
         * Graphics display off
         * Alpha display on.
         */
        fprintf(draw_fp, "\033*dD\033*dE");
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* draw from the current graphics position to the new one (x, y) */
static int HPGT_draw(int x, int y) {
        fprintf(draw_fp, "\033*pa%d,%d", vdevice.cpVx, vdevice.cpVy);
        fprintf(draw_fp, " %d,%dZ", x, y);
        tlstx = x;
        tlsty = y;
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* return the next key typed.  */
static int HPGT_getkey(void) {
#ifdef BSD
        struct sgttyb   oldtty, newtty;
        char            c;

        fflush(draw_fp);
        ioctl(0, TIOCGETP, &oldtty);

        newtty = oldtty;
        newtty.sg_flags = RAW;

        ioctl(0, TIOCSETP, &newtty);

        read(0, &c, 1);

        ioctl(0, TIOCSETP, &oldtty);
#else
        struct termio   oldtty, newtty;
        char            c;

        fflush(draw_fp);
        ioctl(0, TCGETA, &oldtty);

        newtty = oldtty;
        newtty.c_iflag = BRKINT | IXON | ISTRIP;
        newtty.c_lflag = 0;
        newtty.c_cc[VEOF] = 1;

        ioctl(0, TCSETA, &newtty);

        read(0, &c, 1);

        ioctl(0, TCSETA, &oldtty);
#endif
        return(c);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */

/*
 * get the position of the crosshairs. This gets a bit sticky since
 * we have no mouse, and the crosshairs do not behave like a mouse - even a rat!
 * In this case the keys 1 to 9 are used, with each one returning a power of
 * two.
 */
static int HPGT_locator(int *x, int *y) {
        int             c, i;
        char            buf[50];

        if (click) {                    /* for compatibility with other devs */
                click = 0;
                return(0);
        }

        click = 1;
        c = -1;

        if (draw_fp == stdout) {
                fprintf(draw_fp, "\033*s4^");
                fgets(buf, 50, stdin);
                sscanf(buf, "%d,%d,%d", x, y, &c);
        }
        return(1 << (c - '1'));
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* clear the screen.  */
static int HPGT_clear(void) {
        /*
         * A rectangular fill absolute
        if (vdevice.depth == 3)
                fprintf(draw_fp, "\033*m0,0,%d,%de", vdevice.sizeSx, vdevice.sizeSy);
        else
         */
                /*
                 * Clear graphics memory
                 */
                fprintf(draw_fp, "\033*dA");

        tlstx = tlsty = -1;

        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
static int HPGT_color(int i) {
        /* Select 1 of the standard colours from the system pallete */
        if (vdevice.depth == 1)
                return(0);

        i %= 8;
        fprintf(draw_fp, "\033*m%dx", i);
        /*... and the graphics text */
        fprintf(draw_fp, "\033*n%dx", i);
        fprintf(draw_fp, "\033*e%dx", i);
        fflush(draw_fp);
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* outputs one char */
static int HPGT_char(char c) {
        /*
         * Output a label.
         */
        if (tlstx != vdevice.cpVx || tlsty != vdevice.cpVy)
                fprintf(draw_fp, "\033*pa%d,%dZ", vdevice.cpVx, vdevice.cpVy);

        fprintf(draw_fp, "\033*l%c\015", c);
        tlstx = vdevice.cpVx += (int)vdevice.hwidth;
        fflush(draw_fp);
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* outputs a string */
static int HPGT_string(char *s) {
        if (tlstx != vdevice.cpVx || tlsty != vdevice.cpVy)
                fprintf(draw_fp, "\033*pa%d,%dZ", vdevice.cpVx, vdevice.cpVy);

        fprintf(draw_fp, "\033*l%s\015", s);
        fflush(draw_fp);
        tlstx = vdevice.cpVx += (int)strlen(s) * (int)vdevice.hwidth;
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* "fill" a polygon */
static int HPGT_fill(int n, int x[], int y[]) {
        int     i;

        fprintf(draw_fp, "\033*pas");
        for (i = 0; i < n; i++)
                fprintf(draw_fp, "%d,%d ", x[i], y[i]);

        fprintf(draw_fp, "T");
        fflush(draw_fp);
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* Flush/sync the graphics */
static int HPGT_sync(void) {
        fflush(draw_fp);
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop1(int x) { return(-1); }
static int noop4(int a, int b, int c, int d) { return(-1); }
/*
 * the device entry
 */
static DevEntry hpgtdev = {
                "hpgt",        /* name of device */
                "large",       /* name of large font */
                "small",       /* name of small font */
                noop,          /* Set drawing in back buffer */
                HPGT_char,     /* Draw a hardware character */
                noop,          /* HPGT_checkkey,  Check if a key was hit */
                HPGT_clear,    /* Clear the screen to current color */
                HPGT_color,    /* Set current color */
                HPGT_draw,     /* Draw a line */
                HPGT_exit,     /* Exit graphics */
                HPGT_fill,     /* Fill a polygon */
                HPGT_font,     /* Set hardware font */
                noop,          /* Set drawing in front buffer */
                HPGT_getkey,   /* Wait for and get the next key hit */
                HPGT_init,     /* Initialize the device */
                HPGT_locator,  /* Get mouse/cross hair position */
                noop4,         /* HPGT_mapcolor, Set color indices */
                noop1,         /* Set line width */
                HPGT_string,   /* Draw a hardware string */
                noop,          /* Swap front and back buffers */
                HPGT_sync      /* Syncronize the display */
};
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */
/* copy the tektronix device into vdevice.dev.  */
int _HPGT_draw_devcpy(void) {
        vdevice.dev = hpgtdev;
        return(0);
}
/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */

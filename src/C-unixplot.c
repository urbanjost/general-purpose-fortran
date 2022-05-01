/* this code is licensed as public domain */

#ident "@(#)M_DRAW:driver/unixplot.c - M_DRAW driver for UNIX 'plot' format files"

#include <stdio.h>
#include <string.h>
#include "draw.h"

#define SPACE_SIZE      1024
#define POINT(x, y)     (0x10000 * (y) + (x))
#define MAXCOLOR        7

extern FILE     *_draw_outfile();

static int      uplot_first_time = 1, drawn = 0,
#ifdef MAP_TO_LINESTYLES
                curcol = 0,                     /* black */
#endif
                uplotlstx = -1, uplotlsty = -1; /* last (x, y) drawn */

#undef MAP_TO_LINESTYLES        /* This works, but doesn't look great on text */

/*
 * Line style map for our standard colours
 */
static char     *colormap[MAXCOLOR + 1] = {
        "solid",
        "dotted",
        "shortdashed",
        "longdashed",
        "dotdashed",
        "solid",
        "solid",
        "solid"
};

extern  FILE     *draw_fp;

/*
 * putpt
 *
 *      Put a point out to the file.  Two two-byte values, little-endian.
 *      NOTE:  This assumes 8-bit chars and 16-bit shorts.
 */
static void putpt(int x, int y, FILE *draw_fp) {
        short sx, sy;

        sx = (short) x;
        sy = (short) y;

        putc((sx & 0xff), draw_fp);
        putc(((sx >> 8) & 0xff), draw_fp);
        putc((sy & 0xff), draw_fp);
        putc(((sy >> 8) & 0xff), draw_fp);
}

/*
 * uplot_init
 *
 *      Set up the unixplot environment. Returns 1 on success.
 */
static int uplot_init(void) {
        draw_fp = _draw_outfile();

        if (!uplot_first_time)
                return(1);

        putc('s', draw_fp);
        putpt(0, 0, draw_fp);
        putpt(SPACE_SIZE, SPACE_SIZE, draw_fp);

        vdevice.sizeSx = vdevice.sizeSy = SPACE_SIZE;
        vdevice.sizeX = vdevice.sizeY = SPACE_SIZE;

        vdevice.depth = 1;

        return(1);
}


/*
 * uplot_exit
 *
 *      Flush remaining data and close the output file if necessary.
 */
static int uplot_exit(void) {
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

/*
 * uplot_draw
 *
 *      draw to an x, y point.
 */
static int uplot_draw(int x, int y) {
        if (uplotlstx != vdevice.cpVx || uplotlsty != vdevice.cpVy)
        {
                putc('m', draw_fp);
                putpt(vdevice.cpVx, vdevice.cpVy, draw_fp);
        }

        putc('n', draw_fp);
        putpt(x, y, draw_fp);
        uplotlstx = x;
        uplotlsty = y;
        drawn = 1;
        return(0);
}

/*
 * uplot_font
 *
 *      There's not much we can do for this.  We can't even know
 *      the width or height!
 */
static int uplot_font(char *font) {
        if (strcmp(font, "large") == 0 || strcmp(font, "small") == 0)
        {
                vdevice.hwidth = 1.0;
                vdevice.hheight = 1.0;
                return(1);
        }
        else
        {
                return(0);
        }
}

/*
 * uplot_clear
 *
 *      Erase the plot
 */
static int uplot_clear(void) {
        if (drawn) {
                putc('e', draw_fp);
        }
        drawn = 0;
        return(0);
}

/*
 * uplot_color
 *
 *      Change the linestyle of the lines
 */
static int uplot_color(int col) {
        if (col > MAXCOLOR)
                return(0);


#ifdef MAP_TO_LINESTYLES
        curcol = col;
        fprintf(draw_fp, "f%s\n", colormap[curcol]);
#endif
        return(0);
}

/*
 * uplot_char
 *
 *      Output a character
 */
static int uplot_char(char c) {
        if (uplotlstx != vdevice.cpVx || uplotlsty != vdevice.cpVy)
        {
                putc('m', draw_fp);
                putpt(vdevice.cpVx, vdevice.cpVy, draw_fp);
        }

        fprintf(draw_fp, "t%c\n", c);

        drawn = 1;
        uplotlstx = uplotlsty = -1;
        return(0);
}

/*
 * uplot_string
 *
 *      output a string one char at a time.
 */
static int uplot_string(char *s) {
        if (uplotlstx != vdevice.cpVx || uplotlsty != vdevice.cpVy)
        {
                putc('m', draw_fp);
                putpt(vdevice.cpVx, vdevice.cpVy, draw_fp);
        }

        fprintf(draw_fp, "t%s\n", s);

        drawn = 1;
        uplotlstx = uplotlsty = -1;
        return(0);
}

/*
 * uplot_fill
 *
 *      Should do a fill, but we can't so just draw the polygon
 */
static int uplot_fill(int n, int x[], int y[]) {
        int     i;

        putc('m', draw_fp);
        putpt(x[0], y[0], draw_fp);

        for (i = 1; i < n; i++)
        {
                putc('n', draw_fp);
                putpt(x[i], y[i], draw_fp);
        }
        putc('n', draw_fp);
        putpt(x[0], y[0], draw_fp);

        vdevice.cpVx = x[n - 1];
        vdevice.cpVy = y[n - 1];

        uplotlstx = uplotlsty = -1;
        return(0);
}
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop1(int x) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
static int noop4(int a, int b, int c, int d) { return(-1); }

static DevEntry uplotdev = {
        "unixplot",     /* name of device */
        "large",        /* name of large font */
        "small",        /* name of small font */
        noop,           /* set drawing into back buffer */
        uplot_char,     /* draw a hardware character */
        noop,           /* check if a key was hit */
        uplot_clear,    /* clear screen to current color */
        uplot_color,    /* set current color */
        uplot_draw,     /* draw a line */
        uplot_exit,     /* exit graphics */
        uplot_fill,     /* fill a polygon */
        uplot_font,     /* set hardware font */
        noop,           /* set drawing into front buffer */
        noop,           /* wait for and get the next key hit */
        uplot_init,     /* initialize the device */
        noop2,          /* get mouse/crosshair position */
        noop4,          /* set color indices */
        noop1,           /* Set line width */
        uplot_string,   /* draw a hardware string */
        noop,           /* swap front and back buffers */
        noop            /* syncronize the display */
};

/* copy the unixplot device into vdevice.dev.  */
int _UNIXPLOT_draw_devcpy(void) {
        vdevice.dev = uplotdev;
        return(0);
}

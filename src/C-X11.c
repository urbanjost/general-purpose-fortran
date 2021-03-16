/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/X11.c - M_DRAW driver for X11 Windows"

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "draw.h"

static  char      *me = "M_draw";
#define LARGEFONT       "fixed"
#define SMALLFONT       "fixed"


#define MIN(x,y)        ((x) < (y) ? (x) : (y))
#define CMAPSIZE        256
#define EV_MASK         KeyPressMask|ButtonReleaseMask|ExposureMask|ButtonPressMask

static  Window          winder;
static  Display         *display;
static  int             screen;
static  unsigned long   carray[CMAPSIZE];
static  Colormap        colormap;

static  Drawable        theDrawable;
static  GC              theGC;
static  Pixmap          bbuf;           /* Back buffer pixmap */
static  int             back_used = 0;  /* Have we backbuffered ? */

static  XFontStruct     *font_id = (XFontStruct *)NULL;
XEvent                  event;

static  unsigned long   colour;
static  unsigned int    h, w;
static  char            *smallf, *largef;
static  char            *wintitle;

#define X11DEV 0
#define x11DEV 1
static int GLOBAL_device = X11DEV;
/******************************************************************************/
/* PROTOTYPES */
static int X11_mapcolor(int i, int r, int g, int b);
static int X11_backbuf(void);
static int X11_color(int ind);
static int X11_clear(void);
static int X11_swapbuf(void);
static int X11_frontbuf(void);
static int X11_sync(void);
static int X11_checkkey(void);
/******************************************************************************/
static void X11_NEW(void){
   int idum;
   idum=X11_checkkey();
   XFlush(display);
   X11_backbuf();
   X11_clear();
   X11_swapbuf();
   X11_frontbuf();
   XSetBackground(display,theGC,carray[7]);
   XFlush(display);
}
/******************************************************************************/
static int X11_RESIZE(void) { /* Adjust to new window size if it has changed */

   XWindowAttributes retWindowAttributes;
   int x, y, w, h;
   Window rootw, childw;
   unsigned int mask;

   /* get window size -- using XGetWindowAttributes() */
   /* Fill attribute structure with information about window in case size has changed */
   if (XGetWindowAttributes(display, winder, &retWindowAttributes) == 0) {
      fprintf(stderr, "X11_clear: failed to get window attributes.\n");
      exit(-1);
   }

   if(retWindowAttributes.width != vdevice.sizeSx || retWindowAttributes.height != vdevice.sizeSy ){

      x = retWindowAttributes.x;
      y = retWindowAttributes.y;
      w = retWindowAttributes.width;
      h = retWindowAttributes.height;

      XQueryPointer(display, winder, &rootw, &childw, &x, &y, &x, &y, &mask);
      XTranslateCoordinates(display, winder, retWindowAttributes.root, 0, 0, &x, &y, &rootw);
      vdevice.sizeSx = w;
      vdevice.sizeSy = h;
   
      if (back_used) {
         back_used = 0;
         X11_backbuf();
      }
      XFlush(display);
   }

   return(UNUSED);
}
/******************************************************************************/
static int X11_init(void) { /* X11_init -- initialises X11 display.  */
   extern int X11_driver_handler();
   int             i;
   int             x, y, prefx, prefy, prefxs, prefys;
   unsigned int    bw, depth, mask;
   Window          rootw, childw;
   char            *av[2], *geom;
   char            name[MAXVERTS];
   char            *varname;

   XSetWindowAttributes    theWindowAttributes;
   XWindowAttributes       retWindowAttributes;
   XSizeHints              theSizeHints;
   unsigned long           theWindowMask;
   XWMHints                theWMHints;

   av[0] = me;
   av[1] = (char *)NULL;

   varname=getenv("PLTDEVICE");
   if(varname==(char *)NULL) {
      varname="UNDEFINED";
   }

   if (!strcmp(varname,"NOCLOSE")) {
      fprintf(stderr,"%s: returning to X11 window\n", me);
   }else{
      if ((display = XOpenDisplay((char *)NULL)) == (Display *)NULL) {
         fprintf(stderr,"%s: X11_init: can't connect to X server\n", me);
         exit(1);
      }
      winder = DefaultRootWindow(display);
      screen = DefaultScreen(display);
      colormap = DefaultColormap(display, screen);

      XSetIOErrorHandler(X11_driver_handler);
   }

/* Set our standard colors...  */

   depth = vdevice.depth = DefaultDepth(display, screen);
   if (vdevice.depth == 1) {

/*
    Black and white - anything that's not black is white for X11,
    anything that's not white is black for x11.
*/
      if (GLOBAL_device == X11DEV ){
         carray[0] = BlackPixel(display, screen);
         for (i = 1; i < CMAPSIZE; i++)
            carray[i] = WhitePixel(display, screen);
      } else{
         carray[0] = WhitePixel(display, screen);
         for (i = 1; i < CMAPSIZE; i++)
            carray[i] = BlackPixel(display, screen);
      }
   } else {
      /*
       * Color, try to get our colors close to what's in the
       * default colormap.
       */
      if(GLOBAL_device == X11DEV){
         X11_mapcolor(0, 0, 0, 0);
         X11_mapcolor(7, 255, 255, 255);
      } else{
         X11_mapcolor(7, 0, 0, 0);
         X11_mapcolor(0, 255, 255, 255);
      }
      X11_mapcolor(1, 255, 0, 0);
      X11_mapcolor(2, 0, 255, 0);
      X11_mapcolor(3, 255, 255, 0);
      X11_mapcolor(4, 0, 0, 255);
      X11_mapcolor(5, 255, 0, 255);
      X11_mapcolor(6, 0, 255, 255);
   }

   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

/* NEED TO USE XGRABPOINTER here??? */
   XQueryPointer(display, winder, &rootw, &childw, &x, &y, &x, &y, &mask);

   if (childw == None)
      childw = rootw;

   XGetGeometry(display, childw, &rootw, &x, &y, &w, &h, &bw, &depth);

   vdevice.depth = depth;

/* theWindowAttributes.backing_store = WhenMapped; */

   theWindowAttributes.backing_store = Always;
   theWindowAttributes.save_under = True;
   theWindowAttributes.border_pixel = carray[1];
   theWindowAttributes.background_pixel = carray[7];


/* See if there is something in the .Xdefaults file regarding VOGL/M_DRAW.  */

   if ((smallf = XGetDefault(display, me, "smallfont")) == (char *)NULL)
      smallf = SMALLFONT;

   if ((largef = XGetDefault(display, me, "largefont")) == (char *)NULL)
      largef = LARGEFONT;

   if ((wintitle = XGetDefault(display, me, "title")) == (char *)NULL)
      wintitle = (char *)NULL;

   geom = XGetDefault(display, me, "Geometry");

   if (geom != (char *)NULL) {
      theSizeHints.flags = PSize | PMinSize ;
      theSizeHints.flags = 0 ;
      mask = XParseGeometry(geom, &x, &y, &w, &h);

      if (mask & XValue)
         theSizeHints.flags |= USPosition;

      if (mask & YValue)
         theSizeHints.flags |= USPosition;

      if (mask & WidthValue)
         theSizeHints.flags |= USSize;

      if (mask & HeightValue)
         theSizeHints.flags |= USSize;

      if (mask & XNegative)
         x = DisplayWidth(display, screen) - 2*bw - w + x;

      if (mask & YNegative)
         y = DisplayHeight(display, screen) - 2*bw - h + y;

   } else
      theSizeHints.flags = PPosition | PSize;

   if (prefx > -1) {
      x = prefx;
      y = prefy;
   }

   if (prefxs > -1) {
      w = prefxs;
      h = prefys;
   }

   if (bw == 0){
      bw = 4;
   }

   x -= bw;
   y -= bw;

   if (x <= 0)
      x = 0;

   if (y <= 0)
      y = 0;

   w -= 4 * bw;
   h -= 4 * bw;

   if (strcmp(varname,"NOCLOSE")){
      theWindowMask = CWBorderPixel|CWBackingStore;

      winder = XCreateWindow(display,
          winder,
          x, y,
          w, h,
          bw,
          (int)depth,
          InputOutput,
          CopyFromParent,
          theWindowMask,
          &theWindowAttributes
          );

      theSizeHints.x = x;
      theSizeHints.y = y;
      theSizeHints.width = w;
      theSizeHints.height = h;
      theSizeHints.min_width = w;
      theSizeHints.max_width = w;
      theSizeHints.min_height = h;
      theSizeHints.max_height = h;

      if (wintitle == (char *)NULL || strlen(wintitle) <= 0) {
         sprintf(name, "%s %d (win id 0x%x)", me, getpid(), (int)winder);
      } else {
         strcpy(name,wintitle);
      }

      XSetStandardProperties(display,
          winder,
          name,
          name,
          None,
          av,
          1,
          &theSizeHints
          );

      theWMHints.initial_state = NormalState;
      theWMHints.input = True;
      theWMHints.flags = StateHint | InputHint;
      XSetWMHints(display, winder, &theWMHints);

      theDrawable = (Drawable)winder;

/* Create Graphics Context and Drawable */
      theGC = XDefaultGC(display, screen);
      theDrawable = (Drawable)winder;
   }
   XSelectInput(display, winder, EV_MASK);
   XMapRaised(display, winder);
   XFlush(display);

/* Wait for Exposure event. */


        do {
                XNextEvent(display, &event);
        } while (event.type != Expose && event.type != MapNotify);

/* Set the input Focus to us. */
        if (prefx == -1 && prefxs == -1)
                XSetInputFocus(display, winder, RevertToParent, CurrentTime);

   X11_RESIZE();
   vdevice.sizeX = vdevice.sizeY = MIN(h, w);

   if (strcmp(varname,"NOCLOSE")){
      if (back_used) {
         back_used = 0;
         X11_backbuf();
      }
   } else{
      X11_NEW();
   }
   return(1);
}
/******************************************************************************/
/* X11_exit   cleans up before returning the window to normal.  */
static int X11_exit(void) {
   char            *varname;
   varname=getenv("PLTDEVICE");
   if(varname==NULL) {
      varname="UNDEFINED";
   }
   if (!strcmp(varname,"NOCLOSE")) {
      fprintf(stderr,"%s planning on returning to X11 window\n", me);
      return(1);
   }
   XSync(display, 0);
   if (back_used) {
      XFreePixmap(display, bbuf);
      bbuf = 0xffffffff;
      back_used = 0;
   }
   if (font_id != (XFontStruct *)NULL) {
      XFreeFont(display, font_id);
      font_id = (XFontStruct *)NULL;
   }
#ifdef NEWCMAP
   if (colormap != DefaultColormap(display, screen)){
      XFreeColormap(display, colormap);
      colormap = 0;
   }
#endif
/* -------------------
!!        XUnmapWindow(display, winder);
!!        XSetCloseDownMode(display,RetainTemporary);
!!        XSetCloseDownMode(display,DestroyAll);
----------------- */

   XDestroyWindow(display, winder);

/* -------------------
!!        XFreeGC(display, theGC);
!!        CloseDisplay(display);
----------------- */

   XSync(display,0);
   XFlush(display);
   XCloseDisplay(display);

   display = (Display *)NULL;

   winder = 0;
   return(1);
}
/******************************************************************************/
/*
 * X11_draw
 *
 *      draws a line from the current graphics position to (x, y).
 *
 * Note: (0, 0) is defined as the top left of the window in X (easy
 * to forget).
 */
static int X11_draw(int x, int y) {
   XDrawLine(display,
       theDrawable,
       theGC,
       vdevice.cpVx, vdevice.sizeSy - vdevice.cpVy,
       x, vdevice.sizeSy - y
       );
   if (vdevice.sync)
      XSync(display, 0);
   return(UNUSED);
}
/******************************************************************************/
/*
 * X11_getkey
 *
 *      grab a character from the keyboard - blocks until one is there.
 */
static int X11_getkey(void) {
   char    c;

   do {
      XNextEvent(display, &event);
      if (event.type == KeyPress) {
         if (XLookupString((XKeyEvent *)&event, &c, 1, NULL, NULL) > 0)
            return((int)c);
         else
            return(0);
      }
   } while (event.type != KeyPress);
   return(UNUSED);
}
/******************************************************************************/
/*
 * X11_checkkey
 *
 *      Check if there has been a keyboard key pressed.
 *      and return it if there is.
 */
static int X11_checkkey(void) {
   char    c;

   if (!XCheckWindowEvent(display, winder, KeyPressMask, &event))
      return(0);

   if (event.type == KeyPress)
      if (XLookupString((XKeyEvent *)&event, &c, 1, NULL, NULL) > 0)
         return((int)c);

   return(0);
}
/******************************************************************************/
/*
 * X11_locator
 *
 * return the window location of the cursor, plus which mouse button,
 * if any, has been pressed.
 */
static int X11_locator(int *wx, int *wy) {
   Window  rootw, childw;
   int     x, y ;
   unsigned int mask;

   XQueryPointer(display, winder, &rootw, &childw, &x, &y, wx, wy, &mask);

   *wy = (int)vdevice.sizeSy - *wy;

   return(mask >> 8);
}
/******************************************************************************/
static int X11_clear(void) { /* Clear the screen (or current buffer )to current colour */
   X11_RESIZE();
   XSync(display, 0);
   XSetBackground(display, theGC, colour);
   XFillRectangle(display,
      theDrawable,
      theGC,
      0,
      0,
      (unsigned int)vdevice.sizeSx + 1,
      (unsigned int)vdevice.sizeSy + 1
      );
   XFlush(display); /* always flush on clear even if vdevice.sync is not on (0) */
   return(UNUSED);
}
/******************************************************************************/
/* set the current drawing color index if ind >= 0.;
   set the line width in raster units if ind  <  0.
 */
static int X11_color(int ind) {
   static unsigned int line_width=0; /* 0 would be fast line of width 1 */
   int line_style = LineSolid;       /* If LineOnOffDash or LineDoubleDash, must set dashes */
   int cap_style = CapRound;         /* else CapNotLast, CapButt, or CapProjecting */
   int join_style = JoinRound;       /* else JoinMiter or JoinBevel */
   if ( ind < 0 )
   {
      if ( ind == -1 )
      {
         ind = 0;
      }
      line_width=abs(ind);
      XSetLineAttributes(display,theGC,line_width, line_style, cap_style, join_style);
   } else {
      colour = carray[ind];
      XSetForeground(display, theGC, colour);
   }
   return(UNUSED);
}
/******************************************************************************/
/* set the line width in raster units if ind  <  0.  input units in 1/10000 of X size */
static int X11_setlw(int ind) {
   static unsigned int line_width=0; /* 0 would be fast line of width 1 */
   int line_style = LineSolid;       /* If LineOnOffDash or LineDoubleDash, must set dashes */
   int cap_style = CapRound;         /* else CapNotLast, CapButt, or CapProjecting */
   int join_style = JoinRound;       /* else JoinMiter or JoinBevel */
   ind=(float)vdevice.sizeX/10000.0*ind;
   if ( ind <= -1 )
   {
      ind = 0;
   }
   line_width=abs(ind);
   XSetLineAttributes(display,theGC,line_width, line_style, cap_style, join_style);
   return(UNUSED);
}
/******************************************************************************/
/* change index i in the color map to the appropriate r, g, b, value.  */
static int X11_mapcolor(int i, int r, int g, int b) {
   int     stat;
   XColor  tmp;

   if (i >= CMAPSIZE)
      return(-1);


   /*
         * For Black and White.
         * If the index is 0 and r,g,b != 0 then we are remapping black.
         * If the index != 0 and r,g,b == 0 then we make it black.
         */
   if (vdevice.depth == 1) {
      if ( GLOBAL_device == X11DEV){
         if (i == 0 && (r != 0 || g != 0 || b != 0))
            carray[i] = WhitePixel(display, screen);
         else if (i != 0 && r == 0 && g == 0 && b == 0)
            carray[i] = BlackPixel(display, screen);
      } else{
         if (i == 0 && (r != 255 || g != 255 || b != 255))
            carray[i] = BlackPixel(display, screen);
         else if (i != 0 && r == 255 && g == 255 && b == 255)
            carray[i] = WhitePixel(display, screen);
      }
   } else {
      tmp.red = (unsigned short)(r / 255.0 * 65535);
      tmp.green = (unsigned short)(g / 255.0 * 65535);
      tmp.blue = (unsigned short)(b / 255.0 * 65535);
      tmp.flags = 0;
      tmp.pixel = (unsigned long)i;

      if ((stat = XAllocColor(display, colormap, &tmp)) == 0) {
         fprintf(stderr, "XAllocColor failed (status = %d)\n", stat);
         /* exit(1); */
      } else{
         carray[i] = tmp.pixel;
      }
   }

   if (vdevice.sync){
      XFlush(display);
   }
   return(0);
}
/******************************************************************************/
/* Set up a hardware font. Return 1 on success 0 otherwise.  */
static int X11_font(char *fontfile) {
   XGCValues       xgcvals;

   if (font_id != (XFontStruct *)NULL)
      XFreeFont(display, font_id);

   if (strcmp(fontfile, "small") == 0) {
      if ((font_id = XLoadQueryFont(display, smallf)) == (XFontStruct *)NULL) {
         fprintf(stderr, "%s X11.c couldn't open small font '%s'\n", me, smallf);
         fprintf(stderr, "You'll have to redefine it....\n");
         return(0);
      } else
         fontfile = smallf;

   } else if (strcmp(fontfile, "large") == 0) {
      if ((font_id = XLoadQueryFont(display, largef)) == (XFontStruct *)NULL) {
         fprintf(stderr, "%s X11.c couldn't open large font '%s'\n", me, largef);
         fprintf(stderr, "You'll have to redefine it....\n");
         return(0);
      }
      fontfile = largef;
   } else {
      if ((font_id = XLoadQueryFont(display, fontfile)) == (XFontStruct *)NULL) {
         fprintf(stderr, "%s X11.c couldn't open fontfile '%s'\n", me, fontfile);
         return(0);
      }
   }

   vdevice.hheight = font_id->max_bounds.ascent + font_id->max_bounds.descent;
   vdevice.hwidth = font_id->max_bounds.width;

   xgcvals.font = XLoadFont(display, fontfile);
   XChangeGC(display, theGC, GCFont, &xgcvals);

   return(1);
}
/******************************************************************************/
/* outputs one char - is more complicated for other devices */
static int X11_char(char c) {
   /* char    *s = "  "; */
   char  s[2];
   s[0] = c; 
   s[1]='\0';
   XDrawString(display, theDrawable, theGC, vdevice.cpVx, (int)(vdevice.sizeSy - vdevice.cpVy), s, 1);

   if (vdevice.sync)
      XFlush(display);
   return(UNUSED);
}
/******************************************************************************/
static int X11_string(char s[]) { /* Display a string at the current drawing position.  */
   XDrawString(display, theDrawable, theGC, vdevice.cpVx, (int)(vdevice.sizeSy - vdevice.cpVy), s, strlen(s));
   if (vdevice.sync)
      XFlush(display);
   return(UNUSED);
}
/******************************************************************************/
static int X11_fill(int n, int x[], int y[]) { /* fill a polygon */
   char    buf[BUFSIZ];
   XPoint  plist[MAXVERTS];
   int     i;

   if (n > MAXVERTS) {
      sprintf(buf, "%s: more than %d points in a polygon", me,MAXVERTS);
      draw_verror(buf);
   }

   for (i = 0; i < n; i++) {
      plist[i].x = x[i];
      plist[i].y = vdevice.sizeSy - y[i];
   }

   XFillPolygon(display, theDrawable, theGC, plist, n, Nonconvex, CoordModeOrigin);

   vdevice.cpVx = x[n-1];
   vdevice.cpVy = y[n-1];

   if (vdevice.sync)
      XFlush(display);
   return(UNUSED);
}
/******************************************************************************/
/* Set up double buffering by allocating the back buffer and setting drawing into it.  */
static int X11_backbuf(void) {
        if (!back_used)
                bbuf = XCreatePixmap(display,
                    (Drawable)winder,
                    (unsigned int)vdevice.sizeSx + 1,
                    (unsigned int)vdevice.sizeSy + 1,
                    (unsigned int)vdevice.depth
                    );

        theDrawable = (Drawable)bbuf;

        back_used = 1;

        return(1);
}
/******************************************************************************/
/* Swap the back and from buffers. (Really, just copy the back buffer to the screen).  */
static int X11_swapbuf(void) {
   XCopyArea(display,
       theDrawable,
       winder,
       theGC,
       0, 0,
       (unsigned int)vdevice.sizeSx + 1,
       (unsigned int)vdevice.sizeSy + 1,
       0, 0
       );

   XSync(display, 0);      /* Not XFlush */
   return(UNUSED);
}
/******************************************************************************/
static int X11_frontbuf(void) { /* Make sure we draw to the screen.  */
   theDrawable = (Drawable)winder;
   return(UNUSED);
}
/******************************************************************************/
static int X11_sync(void) { /* Syncronise the display with what we think has been sent to it...  */
   XSync(display, 0);
   return(UNUSED);
}
/******************************************************************************/
/*
 * the device entry
 */
static DevEntry X11dev = {
   "X11",
   "large",
   "small",
   X11_backbuf,
   X11_char,
   X11_checkkey,
   X11_clear,
   X11_color,
   X11_draw,
   X11_exit,
   X11_fill,
   X11_font,
   X11_frontbuf,
   X11_getkey,
   X11_init,
   X11_locator,
   X11_mapcolor,
   X11_setlw,   /* Set line width */
   X11_string,
   X11_swapbuf,
   X11_sync
};
/******************************************************************************/
/*
 * the device entry
 */
static DevEntry x11dev = {
   "x11",
   "large",
   "small",
   X11_backbuf,
   X11_char,
   X11_checkkey,
   X11_clear,
   X11_color,
   X11_draw,
   X11_exit,
   X11_fill,
   X11_font,
   X11_frontbuf,
   X11_getkey,
   X11_init,
   X11_locator,
   X11_mapcolor,
   X11_setlw,   /* Set line width */
   X11_string,
   X11_swapbuf,
   X11_sync
};
/******************************************************************************/
int _X11_draw_devcpy(void) { /* copy the X11 device into vdevice.dev.  */
   vdevice.dev = X11dev;
   GLOBAL_device = X11DEV;
   return(UNUSED);
}
/******************************************************************************/
int _x11_draw_devcpy(void) { /* copy the x11 device into vdevice.dev.  */
   vdevice.dev = x11dev;
   GLOBAL_device = x11DEV;
   return(UNUSED);
}
/******************************************************************************/
int X11_driver_handler(Display *display,XErrorEvent *myerr) { /* X11_driver_handler */
   char msg[80];
   XGetErrorText(display,myerr->error_code,msg,80);
   fprintf(stderr,"*draw* STOPPED code %s\n",msg);
   exit(1);

   return(UNUSED);
}
/******************************************************************************/

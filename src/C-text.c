/* this code is licensed as public domain */
/*
Determining whether to clip or not is estimated in drawstr assuming
hardware text is fixed-space unless clipping is off
*/
/*

KLUDGED TO SUPPORT HARWARE TEXT THAT IS SCALABLE AND ROTABLE. CLEAN UP

Hardware text that can act like software text in the xy device plane

Very rough first draft has been made:
To get right size, must call textsize after any  viewport/window  change
or font call.

Text should be scalable, and normalized character widths must be known.
It is assumed this text can be rotated.

Could support fixed  spacing of  proportional  font easily by doing what
code to handle clipping does (put out one character at a time if supposed
to be doing  fixed-space text and a proportional  hardware font is being
used)

strlength:  strlength  must be  changed  to take  into  account  current
viewing environment properly if want to even minimally support xyz.

determining  whether to clip or not is  estimated  in  drawstr  assuming
hardware text is fixed-space unless clipping is off
*/
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>

#include <math.h>

#ifdef NOSTRRCHR
#define strrchr rindex
#endif
#include <string.h>

#include "draw.h"

#define ABS(x)  ((x) < 0 ? -(x) : (x))
#define MAX(a, b)       ((a) < (b) ? (b) : (a))
#define XCOORD(x)       ((x) - 'R')
#define YCOORD(y)       ('R' - (y))
#define SKEW(x, y)      (x  + vdevice.attr->a.skew * y)

static  float   SCSIZEX = 1.0, SCSIZEY = 1.0;

static  int     Loaded = 0;
static  int     nchars;

float hardwidth[128];

static  char    errmsg1[120] = "font: unable to open ";

static  struct  {
        char    *p;     /* All the vectors in the font */
        char    **ind;  /* Pointers to where the chars start in p */
        int     as;     /* Max ascender of a character in this font */
        int     dec;    /* Max descender of a character in this font */
        int     mw;     /* Max width of a character in this font */
} ftab;

float   draw_strlength(char *s);
static  void    draw_actual_move(void);
static  void    draw_drawhardchar(char c);
float   draw_getfontwidth(void);
float   draw_getfontheight(void);
#define PROCEDURE

/*
 * Hershey text justification
 */
#define V_XCENTERED     1
#define V_YCENTERED     2
#define V_LEFT          4       /* The default */
#define V_RIGHT         8
#define V_TOP           16
#define V_BOTTOM        32      /* The default */
/*
 * Text weights
 */
#define V_NORMAL          0       /* The default */
#define V_BOLD            1

/*
 * Line thickness
 */
#define V_THIN            0
#define V_THICK           1

/******************************************************************************/
#ident "@(#)M_DRAW:font - loads in a font."
PROCEDURE void draw_font(char *name) {
        Token   *tok;
   /*--------------------------------------------------------------------*/
        if (!vdevice.initialized)
                draw_verror("font: draw not initialized");
   /*--------------------------------------------------------------------*/
        if (vdevice.inobject) {
                tok = draw_newtokens(2 + strlen(name) / sizeof(Token));
                tok[0].i = OBJ_FONT;
                strcpy((char *)&tok[1], name);
                return;
        }
   /*--------------------------------------------------------------------*/
   /* JSU? MAKE SURE WHEN PUSHING AND POPPING THAT A RELOAD IS NOT NEEDED */
   /*--------------------------------------------------------------------*/
        /*
         * check we aren't loading the same font twice in a row
         * assuming pathname leaves are unique
         */
   /*--------------------------------------------------------------------*/
        if (*name == '/') {  /* assume Unix pathnames */
                if (strcmp(strrchr(name, '/') + 1, vdevice.attr->a.font) == 0){
                        return;
                }
        } else if (strrchr(name, '\\') != NULL){   /*assume PC filenames  */
               if (strcmp(strrchr(name, '\\') + 1, vdevice.attr->a.font) == 0){   /*assume PC filenames  */
                        return;
                }
        } else if (strcmp(name, vdevice.attr->a.font) == 0){
                return;
        }
   /*--------------------------------------------------------------------*/
        vdevice.attr->a.softtext = REALHARDWARE;
   /*--------------------------------------------------------------------*/
        if (draw_hershfont(name)) {
                vdevice.attr->a.softtext = HERSHEY;
                return;
        } else if (strcmp(name, "large") == 0) {
                if (!(*vdevice.dev.Vfont)(vdevice.dev.large))
                        draw_verror("font: unable to open large font");
        } else if (strcmp(name, "small") == 0) {
                if (!(*vdevice.dev.Vfont)(vdevice.dev.small))
                        draw_verror("font: unable to open small font");
        } else if (!(*vdevice.dev.Vfont)(name)) {
                strcat(errmsg1, "fontfile ");
                strcat(errmsg1, name);
                /* if not found fall back to large font */
                fprintf(stderr, "%s\n", errmsg1);
                strcpy(name,"large");
                draw_font(name);
        }
   /*--------------------------------------------------------------------*/
      if (vdevice.attr->a.softtext == SOFTHARDWARE) {  /* Soft Hardware set by driver call */
         draw_textsize(vdevice.attr->a.fontwidth, vdevice.attr->a.fontheight);
      }
   /*--------------------------------------------------------------------*/
       if (*name == '/') {
                strcpy(vdevice.attr->a.font, strrchr(name, '/') + 1);
        } else if (strrchr(name,'\\') != NULL ) {
                strcpy(vdevice.attr->a.font, strrchr(name, '\\') + 1);
        }else{
                strcpy(vdevice.attr->a.font, name);
        }
   /*--------------------------------------------------------------------*/
}
/******************************************************************************/
#ident "@(#)M_DRAW:numchars - return the number of characters in the currently loaded hershey font."
/*
 * (The 128 is the number of chars in a hardware font)
 */
PROCEDURE int draw_numchars(void) {
        if (vdevice.attr->a.softtext == HERSHEY ){
                return((int)nchars);
        }else{ /* Hardware and soft hardware */
                return(128);
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:hershfont - Load in a Hershey font"
/* 
 * First try the font library, if that fails try the current directory, otherwise return 0.
 */
PROCEDURE int draw_hershfont(char *fontname) {
        FILE    *hfp;
        int     i;
        int     nvects, n;
        char    path[120], *flib;

        if ((flib = getenv("M_DRAW_FONTPATH")) == (char *)NULL) {
                strcpy(path, FONTLIB);
#ifdef MINGW
                strcat(path, "\\");
#else
                strcat(path, "/");
#endif
                strcat(path, fontname);
        } else {
                strcpy(path, flib);
#ifdef MINGW
                strcat(path, "\\");
#else
                strcat(path, "/");
#endif
                strcat(path, fontname);
        }

        /*
        fprintf(stderr,"FONT PATH=%s\n",path);
        fprintf(stderr,"FONTNAME=%s\n",fontname);
        */

#ifdef MINGW
        if ((hfp = fopen(path, "r+b")) == (FILE *)NULL)
                if ((hfp = fopen(fontname, "r+b")) == (FILE *)NULL)
#else
        if ((hfp = fopen(path, "r")) == (FILE *)NULL)
                if ((hfp = fopen(fontname, "r")) == (FILE *)NULL)
#endif
                        return (0);

        if (fread(&nchars, sizeof(nchars), 1, hfp) != 1)
                return (0);

        if (fread(&nvects, sizeof(nvects), 1, hfp) != 1)
                return(0);

        if (fread(&n, sizeof(n), 1,  hfp) != 1)
                return(0);

        ftab.as = (int)n;

        if (fread(&n, sizeof(n), 1, hfp) != 1)
                return(0);

        ftab.dec = (int)n;

        if (fread(&n, sizeof(n), 1, hfp) != 1)
                return(0);

        ftab.mw = (int)n + vdevice.attr->a.skew * ftab.as;

        /*
         *  Allocate space for it all....
         */
        if (Loaded) {
                if (ftab.ind[0]){
                        draw_vfree(ftab.ind[0],"from hershfont 1");
                }
                if (ftab.ind){
                        draw_vfree(ftab.ind,"from hershfont 2");
                }
                Loaded = 0;
        }

        ftab.ind = (char **)draw_vallocate(sizeof(char *)*(nchars + 1),"from hershfont 3");

        ftab.p = (char *)draw_vallocate((unsigned)(2 * nvects),"from hershfont 4");

        /*
         *  As we read in each character, figure out what ind should be
         */

        for (i = 0; i < nchars; i++) {
                if (fread(&n , sizeof(n), 1, hfp) != 1)
                        return(0);

                if (fread(ftab.p, 1, (unsigned)n, hfp) != (unsigned)n)
                        return(0);

                ftab.ind[i] = ftab.p;
                ftab.p += n;
        }

        ftab.ind[nchars] = ftab.p;      /* To Terminate the last one */

        fclose(hfp);
        vdevice.attr->a.softtext = Loaded = 1;

        if (*fontname == '/'){
                strcpy(vdevice.attr->a.font, strrchr(fontname, '/') + 1);
        }else if ( strrchr(fontname,'\\') != NULL){
                strcpy(vdevice.attr->a.font, strrchr(fontname, '\\') + 1);
        }else{
                strcpy(vdevice.attr->a.font, fontname);
        }
        return(1);
}
/******************************************************************************/
#ident "@(#)M_DRAW:getcharsize - get the width and height of a single character"
/*
 * At the moment, for
 * the Hershey characters, the height returned is always that of the
 * difference between the maximum descender and ascender.
 */
PROCEDURE void draw_getcharsize(char c, float *width, float *height){
      int     i;
      int     ii;
      float a, b;
      float fudge=.0123;
   /*--------------------------------------------------------------------*/
      if (!vdevice.initialized)
         draw_verror("getcharsize: draw not initialized");
   /*--------------------------------------------------------------------*/
      if (vdevice.attr->a.softtext== HERSHEY) {
         if (!Loaded){
            draw_verror("getcharsize: no software font loaded");
        }
         *height = (float)(ftab.as - ftab.dec) * SCSIZEY;

         if (vdevice.attr->a.fixedwidth){
            *width = ftab.mw * SCSIZEX;
         } else{
            if ((ii = c - 32) < 0)
               ii = 0;
            if (ii >= nchars)
               ii = nchars - 1;
            *width = (ftab.ind[ii][1] - ftab.ind[ii][0]) * SCSIZEX;
         }
   /*--------------------------------------------------------------------*/
      } else if ( vdevice.attr->a.softtext == SOFTHARDWARE ) {
         /* assume hardware fonts have 128 characters */
         if ( (i = (int)c) >= 0 && i <= 128)
         {
            *width= hardwidth[i]*draw_getfontwidth();
            *height=draw_getfontheight();
            if(*height == 0 || *width == 0){
               fprintf(stderr,"*getcharsize* warning: zero dimension detected %f %f\n",*width,*height);
            }
         } else{
            fprintf(stderr,"WHAT? %d %f %d\n",i,hardwidth[i],nchars);
            *height=0.0;
            *width=0.0;
         }
   /*--------------------------------------------------------------------*/
      } else if ( vdevice.attr->a.softtext == REALHARDWARE ) {
         draw_VtoWxy(vdevice.hwidth+fudge, vdevice.hheight+fudge, width, height);
         draw_VtoWxy(0.0+fudge,0.0+fudge, &a, &b);
         *height -= b;
         *width -= a;
/*
   SOMETHING WRONG WITH VtoWxy, maybe only viewport larger than default?
   Added .01 to avoid 0
   Look at this later. Leaving print statements here for now
   ---------------------------------
        fprintf(stderr,"*getcharsize (2) VtoWxy %f %f %f %f\n",vdevice.hwidth,
           vdevice.hheight,*width,*height);
        fprintf(stderr,"*getcharsize (3) origin %f %f %c\n",a,b,c);
        fprintf(stderr,"*getcharsize (4) height width  %f %f\n",*height,*width);
*/
     }
}
/******************************************************************************/
static struct {
        float   x, y;
} btab[] = {
        {0, 0},
        {0.5, 0.5},
        {0, 1.0},
        {1.0, 1.0},
        {1.0, 0}
};
/******************************************************************************/
#ident "@(#)M_DRAW:drawchar - Display a character from the currently loaded font."
PROCEDURE void draw_drawchar(int c) {
        char    *p, *e;
        Token   *pr;
        int     Move, b, i, x, y, xt, yt, sync;
        float   xp, yp, tmp, xsave, ysave;
        float   tcos, tsin;
   /*--------------------------------------------------------------------*/
        if (vdevice.inobject) {
                pr = draw_newtokens(2);
                pr[0].i = OBJ_DRAWCHAR;
                pr[1].i = c;
                return;
        }
   /*--------------------------------------------------------------------*/
        if (vdevice.attr->a.softtext ==  SOFTHARDWARE ) {
                if (!vdevice.cpVvalid)
                        draw_actual_move();
                draw_drawhardchar(c);
                if(vdevice.attr->a.fixedwidth){
                  draw_rmove(draw_getfontwidth(), 0.0, 0.0);
                }else{
                  draw_rmove((float)(hardwidth[(int)c]*draw_getfontwidth()), 0.0, 0.0);
                }
                return;
        }
   /*--------------------------------------------------------------------*/
      if ( vdevice.attr->a.softtext != HERSHEY ) {    /* Not Hershey */
         if (!vdevice.cpVvalid){
            draw_actual_move();
	 }
         draw_drawhardchar(c);
         draw_rmove(draw_getfontwidth(), 0.0, 0.0);
         return;
      }
   /*--------------------------------------------------------------------*/
        if (!Loaded)
                draw_verror("drawchar: no font loaded");
   /*--------------------------------------------------------------------*/
        if ((sync = vdevice.sync))
                vdevice.sync = 0;
   /*--------------------------------------------------------------------*/
        tcos = vdevice.attr->a.textcos;
        tsin = vdevice.attr->a.textsin;

        if ((i = c - 32) < 0)
                i = 0;
        if (i >= nchars)
                i = nchars - 1;

        xsave = vdevice.cpW[V_X];
        ysave = vdevice.cpW[V_Y];

        Move = 1; xt = (vdevice.attr->a.fixedwidth ? -ftab.mw / 2 : XCOORD(ftab.ind[i][0]));
        yt = ftab.dec;

        /* Justify in the x direction */
        if (vdevice.attr->a.justify & V_XCENTERED) {
                xt = 0;
        } else if (vdevice.attr->a.justify & V_RIGHT) {
                xt = (vdevice.attr->a.fixedwidth ? ftab.mw / 2 : -XCOORD(ftab.ind[i][0]));
        }

        /* Justify in the y direction */
        if (vdevice.attr->a.justify & V_YCENTERED) {
                yt = 0;
        } else if (vdevice.attr->a.justify & V_TOP) {
                yt = -ftab.dec;
        }

        e = ftab.ind[i+1];
        b = 0;
        do {
                Move = 1;
                p = ftab.ind[i] + 2;
                while(p < e) {
                        x = XCOORD((int)(*p++));
                        y = YCOORD((int)(*p++));
                        x = SKEW(x, y);
                        if (x != -50) {                 /* means move */
                                xp = (btab[b].x + (float)(x - xt))*SCSIZEX;
                                yp = (btab[b].y + (float)(y - yt))*SCSIZEY;
                                tmp = xp;
                                xp = tcos*tmp - tsin*yp + xsave;
                                yp = tsin*tmp + tcos*yp + ysave;
                                if (Move) {
                                        Move = 0;
                                        draw_move(xp, yp, vdevice.cpW[V_Z]);
                                } else {
                                        draw_draw(xp, yp, vdevice.cpW[V_Z]);
                                }
                        } else
                                Move = 1;
                }
        } while (++b < vdevice.attr->a.bold);
        /*
         * Move to right hand of character.
         */

        tmp = vdevice.attr->a.fixedwidth ? (float)ftab.mw : (float)(ftab.ind[i][1] - ftab.ind[i][0]);
        tmp *= SCSIZEX;
        xsave += tcos*tmp;
        ysave += tsin*tmp;
        draw_move(xsave, ysave, vdevice.cpW[V_Z]);

        if (sync) {
                vdevice.sync = 1;
                (*vdevice.dev.Vsync)();
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:drawhardchar - Displays a hardware character."
/*
 *      NOTE: Only does gross clipping to the viewport.
 *            Current world position becomes undefined (ie you have
 *            to do an explicit move after calling hardware text)
 */
PROCEDURE static void draw_drawhardchar(char c) {
   /*--------------------------------------------------------------------*/
        if (!vdevice.clipoff) {
                if (vdevice.cpVx - (int)vdevice.hwidth > vdevice.maxVx)
                        return;
        /*--------------------------------------------------------------------*/
                if (vdevice.cpVx < vdevice.minVx)
                        return;
        /*--------------------------------------------------------------------*/
                if (vdevice.cpVy - (int)vdevice.hheight > vdevice.maxVy)
                        return;
        /*--------------------------------------------------------------------*/
                if (vdevice.cpVy < vdevice.minVy)
                        return;
        }
   /*--------------------------------------------------------------------*/
        /*fprintf(stderr,"*drawhardchar* c=%c\n",c);*/
        (*vdevice.dev.Vchar)(c);
}
/******************************************************************************/
#ident "@(#)M_DRAW:textsize - set software character scaling values"
/*
 * Note: Only changes software char size. Should be called
 * after a font has been loaded.
 *
 * CANNOT HAVE HARDWARE TEXT IN AN OBJECT? WHY?   JSU
 */
PROCEDURE void draw_textsize(float width, float height) {
        float   a;
        Token   *tok;
      float p1W[4];
      float p2W[4];
      float result1[4];
      float result2[4];
      /*--------------------------------------------------------------------*/
        if (!vdevice.initialized)
                draw_verror("textsize: draw not initialized");
      /*--------------------------------------------------------------------*/
      if (!Loaded)
         draw_verror("textsize: no font loaded");
      /*--------------------------------------------------------------------*/
      if (vdevice.inobject) {
         tok = draw_newtokens(3);
         tok[0].i = OBJ_TEXTSIZE;
         tok[1].f = width;
         tok[2].f = height;
         return;
      }
      /*--------------------------------------------------------------------*/
      vdevice.attr->a.fontwidth=width;   /* save for pop and push */
      vdevice.attr->a.fontheight=height; /* save for pop and push */
      /*--------------------------------------------------------------------*/
      if (vdevice.attr->a.softtext == SOFTHARDWARE) {  /* Soft Hardware */

         /* software text size will be stored for use by hardware text size.
          * Software text size request will be converted to hardware units
          * and stored for use by hardware text size. This will mean funny
          * things will happen if window/viewport changes occur, as these
          * fonts will stay the same physical size like a real hardware
          * character unless a new textsize call is made.
          */

         /*fprintf(stderr,"*textsize 2 * vdevice.hwidth=%f vdevice.hheight=%f\n",vdevice.hwidth,vdevice.hheight);*/

         /* simplified planar conversion of virtual units to device units */
              p1W[V_X]=width;
              p1W[V_Y]=height;
              p1W[V_Z]=0.0;
              p1W[V_W]=1.0;

              p2W[V_X]=0.0;
              p2W[V_Y]=0.0;
              p2W[V_Z]=0.0;
              p2W[V_W]=1.0;
              draw_multvector(result1,p1W,vdevice.transmat->m);
              draw_multvector(result2,p2W,vdevice.transmat->m);

          vdevice.hwidth=ABS(draw_WtoVx(result1)-draw_WtoVx(result2));
          vdevice.hheight=ABS(draw_WtoVy(result1)-draw_WtoVy(result2));
         /* fprintf(stderr,"*textsize 3 * vdevice.hwidth=%f vdevice.hheight=%f\n",vdevice.hwidth,vdevice.hheight); */


         if(vdevice.hwidth == 0 ){
           fprintf(stderr,"*textsize* warning: zero character width\n");
         }
         if(vdevice.hheight == 0 ){
            fprintf(stderr,"*textsize* warning: zero character height\n");
         }
         return;
      /*--------------------------------------------------------------------*/
      } else if ( vdevice.attr->a.softtext == REALHARDWARE ) {   /* Hardware set by driver when load font */
         return;
      }
      /*--------------------------------------------------------------------*/

        a = (float)MAX(ftab.mw, (ftab.as - ftab.dec));
        vdevice.attr->a.fontwidth = width;
        vdevice.attr->a.fontheight = height;
        SCSIZEX = width / a;
        SCSIZEY = height / a;
}
/******************************************************************************/
#ident "@(#)M_DRAW:getfontwidth - Return the maximum width of the current font."
PROCEDURE float draw_getfontwidth(void){
      float a, b, c, d;
      float fudge=0.00123;
      if (!vdevice.initialized)
         draw_verror("getfontwidth: draw not initialized");
      /*--------------------------------------------------------------------*/
      if (vdevice.attr->a.softtext == HERSHEY ) {
         if (!Loaded)
            draw_verror("getfontwidth: No font loaded");
         return((float)(SCSIZEX * MAX(ftab.mw, (ftab.as - ftab.dec))));
      /*--------------------------------------------------------------------*/
      } else if(vdevice.attr->a.softtext == SOFTHARDWARE ){
              return(vdevice.attr->a.fontwidth);   /* save for pop and push */
      /*--------------------------------------------------------------------*/
      } else {  /* HARDWARE */
         draw_VtoWxy(vdevice.hwidth+fudge, vdevice.hheight+fudge, &c, &d);
         draw_VtoWxy(0.0+fudge, 0.0+fudge, &a, &b);
         c -= a;
         if(c == 0){
            fprintf(stderr,"*getfontwidth* warning: zero dimension detected %f\n",c);
         }
         return(c);
      }
      /*--------------------------------------------------------------------*/
}
/******************************************************************************/
#ident "@(#)M_DRAW:getfontheight - Return the maximum Height of the current font"
PROCEDURE float draw_getfontheight(void) {
        float   a, b, c, d;
        float fudge=0.0123;
   /*--------------------------------------------------------------------*/
        if (!vdevice.initialized){
                draw_verror("getfontheight: draw not initialized");
         }
   /*--------------------------------------------------------------------*/
        if (vdevice.attr->a.softtext == HERSHEY) {
             if (!Loaded){
                draw_verror("getfontheight: No font loaded");
             }
             d=(float)(SCSIZEY * MAX(ftab.mw, (ftab.as - ftab.dec)));
            if(d == 0){
            fprintf(stderr,"*getfontheight* 1: zero dimension detected %f\n",d);
            }
            return(d);
   /*--------------------------------------------------------------------*/
      } else if(vdevice.attr->a.softtext == SOFTHARDWARE ){
              return(vdevice.attr->a.fontheight);   /* save for pop and push */
   /*--------------------------------------------------------------------*/
        } else {
          draw_VtoWxy(vdevice.hwidth+fudge, vdevice.hheight+fudge, &c, &d);
          draw_VtoWxy(0.0+fudge, 0.0+fudge, &a, &b);
          d -= b;
          if(d == 0){
            fprintf(stderr,"*getfontheight* 2: zero dimension detected %f\n",d);
          }
          return(d);
        }
   /*--------------------------------------------------------------------*/
}
/******************************************************************************/
#ident "@(#)M_DRAW:getfontdec - Return the maximum descender of the current font"
PROCEDURE float draw_getfontdec() {
      float a, b, c, d;
      float fudge=0.0123;
      if (!vdevice.initialized)
         draw_verror("getfontdec: draw not initialized");
      if (vdevice.attr->a.softtext == HERSHEY ) {
         if (!Loaded){
            draw_verror("getfontdec: No font loaded");
         }
         return((float)(SCSIZEY * ftab.dec));
      } else {  /* GUESS */
         draw_VtoWxy(vdevice.hwidth+fudge, vdevice.hheight+fudge, &c, &d);
         draw_VtoWxy(0.0+fudge, 0.0+fudge, &a, &b);
         d -= b;
         return(-d*.28);
      }
}
/******************************************************************************/
#ident "@(#)M_DRAW:getfontasc - Return the maximum ascender of the current font"
PROCEDURE float draw_getfontasc() {
      float a, b, c, d;
      float fudge=0.0123;
      if (!vdevice.initialized)
         draw_verror("getfontasc: draw not initialized");
      if (vdevice.attr->a.softtext == HERSHEY ) {
         if (!Loaded){
            draw_verror("getfontasc: No font loaded");
         }
         return((float)(SCSIZEY * ftab.as));
      } else {  /* GUESS */
         draw_VtoWxy(vdevice.hwidth+fudge, vdevice.hheight+fudge, &c, &d);
         draw_VtoWxy(0.0+fudge, 0.0+fudge, &a, &b);
         d -= b;
         return(-d*.72);
      }
}
/******************************************************************************/
#ident "@(#)M_DRAW:getfontsize - get the current character size in user coords."
/*
 * Hardware text may or may not be really that accurate,
 * depending on what type of font you are using on the device.
 * For software Hershey fonts, the character width is that of
 * a the widest character and the height the height of the tallest.
 *
 */
PROCEDURE void draw_getfontsize(float *cw, float *ch) {
        *cw = draw_getfontwidth();
        *ch = draw_getfontheight();
}
/******************************************************************************/
#ident "@(#)M_DRAW:drawhstr - Display the text string using the currently loaded Hershey font"
PROCEDURE static void draw_drawhstr(char *string) {
        char    c;
        int     i, sync, oldClipoff, NeedClip, oldJustify;
        float   p[4], q[4];

        if (!vdevice.initialized)
                draw_verror("drawhstr: not initialized");

        /*
         * For the duration of hershey strings, turn off
         * "vdevice.attr->a.justify" as we have already compensated
         * for it in drawstr()
         */
        oldJustify = vdevice.attr->a.justify;
        vdevice.attr->a.justify = V_LEFT;

        /*
         * Determine if we can get away with "clipoff"
         */
        oldClipoff = vdevice.clipoff;
        if (!oldClipoff) {  /* Only do this if we have to ... ie. if clipping is on */
                q[0] = vdevice.cpW[V_X];
                q[1] = vdevice.cpW[V_Y];
                q[2] = vdevice.cpW[V_Z];
                q[3] = 1.0;
                draw_multvector(p, q, vdevice.transmat->m);
                NeedClip = 0;
                for (i = 0; i < 3; i++)
                        NeedClip = ((p[3] + p[i] < 0.0) ||
                                    (p[3] - p[i] < 0.0)) || NeedClip;
                if (!NeedClip) {        /* The other end, only if we have to */
                        q[0] += draw_strlength(string);
                        q[1] += draw_getfontheight();
                        draw_multvector(p, q, vdevice.transmat->m);
                        NeedClip = 0;
                        for (i = 0; i < 3; i++)
                                NeedClip = ((p[3] + p[i] < 0.0) ||
                                            (p[3] - p[i] < 0.0)) || NeedClip;
                }
                if (!NeedClip)
                        vdevice.clipoff = 1; /* ie. Don't clip */

        }

        /*
         * Now display each character
         *
         */
        if ((sync = vdevice.sync))
                vdevice.sync = 0;

        while ((c = *string++))
                draw_drawchar(c);

        if (sync) {
                vdevice.sync = 1;
                (*vdevice.dev.Vsync)();
        }

        /*
         * Restore ClipOff
         */
        vdevice.clipoff = oldClipoff;
        vdevice.attr->a.justify = oldJustify;
}
/******************************************************************************/
#ident "@(#)M_DRAW:drawstr - Draw a string from the current pen position."
PROCEDURE void draw_drawstr(char *string) {
        float   sl, width, height, cx, cy;
        float   tcos, tsin;
        float   x1, y1, x2, y2;
        float   tangle;
        float   tangle2;
        char    *str = string, c;
        Token   *tok;
   /*--------------------------------------------------------------------*/
        if(!vdevice.initialized)
                draw_verror("drawstr: draw not initialized");
   /*--------------------------------------------------------------------*/
        if (vdevice.inobject) {
                tok = draw_newtokens(2 + strlen(str) / sizeof(Token));
                tok[0].i = OBJ_DRAWSTR;
                strcpy((char *)&tok[1], str);
                return;
        }
   /*--------------------------------------------------------------------*/
#ifdef SUN_CC
        /* Note that SUN's unbundled ANSI C compiler bitches about this
        sl = (float)strlen(string);
        ... so we change it to */
        sl = (float)(size_t)strlen(string);
#else
        sl = (float)strlen(string);
#endif

        tcos = vdevice.attr->a.textcos;
        tsin = vdevice.attr->a.textsin;
         cx = vdevice.cpW[V_X] ; /* current position in world coordinates */
         cy = vdevice.cpW[V_Y] ;
   /*--------------------------------------------------------------------*/
        height = draw_getfontheight();
        width = draw_strlength(string);
   /*--------------------------------------------------------------------*/
        /* Justify in the x direction */
        if (vdevice.attr->a.justify & V_XCENTERED) {
                width /= 2.0;
        } else if (vdevice.attr->a.justify & V_RIGHT) {
                ;       /* NO change */
        } else {        /* V_LEFT as default */
                width = 0.0;
        }
   /*--------------------------------------------------------------------*/
        /* Justify in the y direction */
        if (vdevice.attr->a.justify & V_YCENTERED) {
                height /= 2.0;
        } else if (vdevice.attr->a.justify & V_TOP) {
                ;       /* NO change */
        } else {        /* V_BOTTOM as default */
                height = 0.0;
        }
   /*--------------------------------------------------------------------*/
        cx = vdevice.cpW[V_X] + height * tsin - width * tcos;
        cy = vdevice.cpW[V_Y] - height * tcos - width * tsin;
        draw_move(cx, cy, vdevice.cpW[V_Z]);
   /*--------------------------------------------------------------------*/
        if (vdevice.attr->a.softtext == HERSHEY) {
                /*  As we are using software text then call the routine
                    to display it in the current font */
                draw_drawhstr(string);
   /*--------------------------------------------------------------------*/
         /* CLEAN THIS UP*/
      } else if (vdevice.attr->a.softtext == SOFTHARDWARE) {
              /* VERY MESSY WAY TO FIND OUT WHAT WAY TEXT IS POINTED BY A ROTATE() */
           draw_move2(cx , cy);
           draw_actual_move();
                 /* fprintf(stderr,"*drawstr* a center=%f %f ",vdevice.cpVx,vdevice.cpVy); */
                 x1=vdevice.cpVx;
                 y1=vdevice.cpVy;
           draw_move2(cx + draw_strlength(string), cy);
           draw_actual_move();
                 /* fprintf(stderr,"*drawstr* b center=%f %f ",vdevice.cpVx,vdevice.cpVy); */
                 x2=vdevice.cpVx;
                 y2=vdevice.cpVy;
                 tangle=atan2((double)(y2-y1),(double)(x2-x1));
                 tangle2=atan2((double)(vdevice.attr->a.textsin),(double)(vdevice.attr->a.textcos));
                 /* fprintf(stderr,"*drawstr* c tangle=%f %f\n",tangle,tangle/D2R); */
                 vdevice.attr->a.textcos=cos(tangle+tangle2);
                 vdevice.attr->a.textsin=sin(tangle+tangle2);
           draw_move2(cx , cy);
           draw_actual_move();

           /* if a rotated coordinate system need to angle text */

           draw_move2(cx,cy);
           draw_actual_move(); /* Really move there */
           /* fprintf(stderr,"*drawstr* SOFTHARDWARE\n"); */
           if (vdevice.clipoff){
              if (vdevice.attr->a.fixedwidth){ /* force fixed spacing */
                while ((c = *str++)) {
                 /*fprintf(stderr,"*drawstr* A c=%c\n",c);*/
                 draw_drawhardchar(c);
                 vdevice.cpVx += vdevice.hwidth;
                }
              }else{
                 /*fprintf(stderr,"*drawstr* B string=%s\n",string);*/
                 (*vdevice.dev.Vstring)(string); /* draw with no clipping */
              }
           }else { /* Check if string is within viewport because clipping is on */
            if (vdevice.cpVx > vdevice.minVx &&
                vdevice.cpVx + (int)(sl * (vdevice.hwidth - 1)) < vdevice.maxVx &&
                vdevice.cpVy - (int)vdevice.hheight < vdevice.maxVy &&
                vdevice.cpVy > vdevice.minVy){
                if(vdevice.attr->a.fixedwidth){ /* force fixed spacing */
                   while ((c = *str++)) {
                      draw_drawhardchar(c);
                      /*fprintf(stderr,"*drawstr* C c=%c\n",c);*/
                      vdevice.cpVx += vdevice.hwidth;
                   }
                }else{  /* BUG: TRUSTING DEVICE TO DO CLIPPING */
                     /*fprintf(stderr,"*drawstr* D string=%s\n",string);*/
                     (*vdevice.dev.Vstring)(string); /* draw with no clipping */
                }
           }else{
              if (vdevice.attr->a.fixedwidth){ /* force fixed spacing */
                while ((c = *str++)) {
                 /*fprintf(stderr,"*drawstr* E c=%c\n",c);*/
                 draw_drawhardchar(c);
                 vdevice.cpVx += vdevice.hwidth;
                }
              }else{  /* BUG: TRUSTING DEVICE TO DO CLIPPING */
                  /*fprintf(stderr,"*drawstr* E string=%s\n",string);*/
                  (*vdevice.dev.Vstring)(string); /* draw with no clipping */
              }
           }
           draw_move(cx + draw_strlength(string), cy, vdevice.cpW[V_Z]);
           draw_actual_move(); /* Really move there */
           vdevice.attr->a.textcos=tcos; /* restore in case changed */
           vdevice.attr->a.textsin=tsin; /* restore in case changed */
           return;
        }
   /*--------------------------------------------------------------------*/
        } else {   /* REALHARDWARE */
                draw_actual_move();  /* Really move there */

                /*   If not clipping then simply display text and return  */

                if (vdevice.clipoff)
                        (*vdevice.dev.Vstring)(string);
                else { /* Check if string is within viewport */
                        if (vdevice.cpVx > vdevice.minVx &&
                            vdevice.cpVx + (int)(sl * (vdevice.hwidth - 1)) < vdevice.maxVx &&
                            vdevice.cpVy - (int)vdevice.hheight < vdevice.maxVy &&
                            vdevice.cpVy > vdevice.minVy)
                                (*vdevice.dev.Vstring)(string);
                        else
                                while ((c = *str++)) {
                                        draw_drawhardchar(c);
                                        vdevice.cpVx += vdevice.hwidth;
                                }
                }

                draw_move(cx + draw_getfontwidth() * sl, cy, vdevice.cpW[V_Z]);

        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:istrlength - Find out the length of a string in raw 'Hershey coordinates'."
PROCEDURE static        int draw_istrlength(char *s) {
        char    c;
        int     i, j, len = 0;


        if (vdevice.attr->a.fixedwidth) {
                return((int)(strlen(s) * ftab.mw));
        }else{
                while ((c = *s++)) {
                        if ((i = (int)c - 32) < 0 || i >= nchars){
                                i = nchars - 1;
                        }
                        j = ftab.ind[i][1] - ftab.ind[i][0];
                        len += j;
                }
                len += (int)(0.5 * vdevice.attr->a.skew * ftab.as);
                return(len);
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:strlength - Find out the length (in world coords) of a string."
PROCEDURE float draw_strlength(char *s){ /* Hershey SoftHardware Hardware */
      char c;
      int i;
      float len = 0.0;
   /*--------------------------------------------------------------------*/
      if (!vdevice.initialized){
         draw_verror("strlength: draw not initialized");
      }
   /*--------------------------------------------------------------------*/
      if (vdevice.attr->a.softtext == HERSHEY){
         return((float)(draw_istrlength(s) * SCSIZEX));
   /*--------------------------------------------------------------------*/
      } else if ( vdevice.attr->a.softtext == SOFTHARDWARE ) {
         if (vdevice.attr->a.fixedwidth){
            return((float)(strlen(s) * draw_getfontwidth()));
         }else{
            while ((c = *s++)) {
            /* assume hardware fonts have 128 characters */
               if ( (i = (int)c) >= 0 && i <= 128)
               {
             /*fprintf(stderr,"%c",c);*/
                len += hardwidth[i];
               } else{
                fprintf(stderr,"*strlength* unexpected character at position %d\n",i);
               }
            }
            /*
            fprintf(stderr,"\n");
            fprintf(stderr,"*strlength* len=%f\n",len);
            fprintf(stderr,"*strlength* fontwidth=%f\n",draw_getfontwidth());
            fprintf(stderr,"*strlength* xx=%f\n",len*draw_getfontwidth());
            */
            return((float)(len*draw_getfontwidth()));
         }
   /*--------------------------------------------------------------------*/
      } else {   /* HARDWARE */
         return((float)(strlen(s) * draw_getfontwidth()));
      }
}
/******************************************************************************/
#ident "@(#)M_DRAW:boxtext - Stretch and draw text so it fits in a 'box' - note only works with Hershey text"
PROCEDURE void draw_boxtext(float x, float y, float l, float h, char *s) {
   float oscsizex, oscsizey;
   Token *tok;

   if(!vdevice.initialized)
      draw_verror("boxtext: draw not initialized");

   if(vdevice.attr->a.softtext != HERSHEY)
      draw_verror("boxtext: need a hershey vector font loaded");

   if(vdevice.inobject) {
      tok = draw_newtokens(6 + strlen(s) / sizeof(Token));

      tok[0].i = OBJ_BOXTEXT;
      tok[1].f = x;
      tok[2].f = y;
      tok[3].f = l;
      tok[4].f = h;
      strcpy((char *) &tok[5], s);

      return;
   }

   oscsizex = SCSIZEX;
   oscsizey = SCSIZEY;
   if(strlen(s) != 0) {
      /*
       set width so string length is the same a "l"
       */
      SCSIZEX = l /(float) draw_istrlength(s);

      /*
       * set character height so it is the same as "h"
       */
      SCSIZEY = h /(float) (ftab.as - ftab.dec);
      draw_move(x, y, vdevice.cpW[V_Z]);

      /* assumes justification is bottom left */
      draw_pushattributes();
      draw_centertext(0);
      draw_drawstr(s);
      draw_popattributes();
   }

   SCSIZEX = oscsizex;
   SCSIZEY = oscsizey;
}
/******************************************************************************/
#ident "@(#)M_DRAW:boxfit - Set up the scales etc for text so that a string of 'nchars' characters of the maximum width in the font fits in a box."
PROCEDURE void draw_boxfit(float l, float h, int nchars) {
        if(!vdevice.initialized) {
                draw_verror("boxfit: draw not initialized");
        }

        if(vdevice.attr->a.softtext != HERSHEY) {
                draw_verror("boxfit: cannot rescale hardware font");
        }

        SCSIZEX = l /(float)(nchars * ftab.mw);
        SCSIZEY = h /(float)(ftab.as - ftab.dec);
}
/******************************************************************************/
#ident "@(#)M_DRAW:centertext - Turns centering of text on or off. Turns off all other justifying."
/*
 *     (Just like in old M_DRAW).
 */
PROCEDURE void draw_centertext(int onoff) {
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(2);
         tok[0].i = OBJ_CENTERTEXT;
         tok[1].i = onoff;
         return;
      }
   /*--------------------------------------------------------------------*/
        if (onoff)
                vdevice.attr->a.justify = V_XCENTERED | V_YCENTERED;
        else
                vdevice.attr->a.justify = V_LEFT | V_BOTTOM;
}
/******************************************************************************/
#ident "@(#)M_DRAW:textjustify - Directly turns on/off justification"
/*PROCEDURE void draw_textjustify(unsigned val) {*/
PROCEDURE void draw_textjustify(char val) {
   /*--------------------------------------------------------------------*/
      Token *tok;
      /*
      fprintf(stdout,"TEXTJUSTIFY %d\n",val);
      */
      if (vdevice.inobject) {
         tok = draw_newtokens(2);
         tok[0].i = OBJ_TEXTJUSTIFY;
         tok[1].i = val;
         return;
      }
   /*--------------------------------------------------------------------*/
        vdevice.attr->a.justify = val;
}
/******************************************************************************/
#ident "@(#)M_DRAW:xcentertext - Directly turns on xcentering"
PROCEDURE void draw_xcentertext(void) {
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(1);
         tok[0].i = OBJ_XCENTERTEXT;
         return;
      }
   /*--------------------------------------------------------------------*/
        vdevice.attr->a.justify |= V_XCENTERED;
        vdevice.attr->a.justify &= ~(V_LEFT | V_RIGHT);
}
/******************************************************************************/
#ident "@(#)M_DRAW:ycentertext - Directly turns on ycentering"
PROCEDURE void draw_ycentertext(void) {
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(1);
         tok[0].i = OBJ_YCENTERTEXT;
         return;
      }
   /*--------------------------------------------------------------------*/
        vdevice.attr->a.justify |= V_YCENTERED;
        vdevice.attr->a.justify &= ~(V_TOP | V_BOTTOM);
}
/******************************************************************************/
#ident "@(#)M_DRAW:leftjustify - Turns on leftjustification"
PROCEDURE void draw_leftjustify(void) {
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(1);
         tok[0].i = OBJ_LEFTJUSTIFY;
         return;
      }
   /*--------------------------------------------------------------------*/
        /*
         * If left justification is on, then V_XCENTER must be off
         * and V_RIGHT must be off
         */
        vdevice.attr->a.justify |= V_LEFT;
        vdevice.attr->a.justify &= ~(V_RIGHT | V_XCENTERED);
}
/******************************************************************************/
#ident "@(#)M_DRAW:rightjustify - Turns on rightjustification"
PROCEDURE void draw_rightjustify(void) {
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(1);
         tok[0].i = OBJ_RIGHTJUSTIFY;
         return;
      }
   /*--------------------------------------------------------------------*/
        /*
         * If right justification is on, then V_XCENTER must be off
         * and V_LEFT must be off
         */
        vdevice.attr->a.justify |= V_RIGHT;
        vdevice.attr->a.justify &= ~(V_LEFT | V_XCENTERED);
}
/******************************************************************************/
#ident "@(#)M_DRAW:topjustify - Turns on topjustification"
PROCEDURE void draw_topjustify(void) { 
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(1);
         tok[0].i = OBJ_TOPJUSTIFY;
         return;
      }
   /*--------------------------------------------------------------------*/
        /*
         * If top justification is on, then V_YCENTER must be off
         * and V_BOTTOM must be off
         */
        vdevice.attr->a.justify |= V_TOP;
        vdevice.attr->a.justify &= ~(V_BOTTOM | V_YCENTERED);
}
/******************************************************************************/
#ident "@(#)M_DRAW:bottomjustify - Turns on bottomjustification"
PROCEDURE void draw_bottomjustify(void) {
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(1);
         tok[0].i = OBJ_BOTTOMJUSTIFY;
         return;
      }
   /*--------------------------------------------------------------------*/
        /*
         * If bottom justification is on, then V_YCENTER must be off
         * and V_TOP must be off
         */
        vdevice.attr->a.justify |= V_BOTTOM;
        vdevice.attr->a.justify &= ~(V_TOP | V_YCENTERED);
}
/******************************************************************************/
#ident "@(#)M_DRAW:fixedwidth - Turns fixedwidth text on or off"
PROCEDURE void draw_fixedwidth(int onoff){
   /*--------------------------------------------------------------------*/
      Token *tok;
      if (vdevice.inobject) {
         tok = draw_newtokens(2);
         tok[0].i = OBJ_FIXEDWIDTH;
         tok[1].i = onoff;
         return;
      }
   /*--------------------------------------------------------------------*/
      vdevice.attr->a.fixedwidth = onoff;
}
/******************************************************************************/
#ident "@(#)M_DRAW:textang - set software character angle in degrees"
/*
 * strings will be written along a line 'ang' degrees from the
 * horizontal screen direction
 *
 * Note: only changes software character angle
 *
 */
PROCEDURE void draw_textang(float ang) {
        Token   *tok;

        if (!vdevice.initialized)
                draw_verror("textang: draw not initialized");

   /*--------------------------------------------------------------------*/
        if (vdevice.inobject) {
                tok = draw_newtokens(3);

                tok[0].i = OBJ_TEXTANG;
                tok[1].f = cos((double)(ang * D2R));
                tok[2].f = sin((double)(ang * D2R));

                return;
        }
   /*--------------------------------------------------------------------*/

        vdevice.attr->a.textcos = cos((double)(ang * D2R));
        vdevice.attr->a.textsin = sin((double)(ang * D2R));
}
/******************************************************************************/
#ident "@(#)M_DRAW:textslant - Defines the obliqueness of the fonts."
PROCEDURE void draw_textslant(float val) {
   /*--------------------------------------------------------------------*/
        Token   *tok;
        if (vdevice.inobject) {
                tok = draw_newtokens(2);

                tok[0].i = OBJ_TEXTSLANT;
                tok[1].f = val;
                return;
        }
   /*--------------------------------------------------------------------*/
        vdevice.attr->a.skew = val;
}
/******************************************************************************/
#ident "@(#)M_DRAW:textweight - Defines the weight of the fonts."
PROCEDURE void draw_textweight(int val) {
        vdevice.attr->a.bold = val ? 5 : 0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:actual_move - implements a move"
/*
 * Actually do a move (multiplying by the current transform and updating the
 * actual screen coords) instead of just setting the current spot in world
 * coords.
 */
PROCEDURE static void draw_actual_move(void) {
        Vector  v2;

        draw_multvector(v2, vdevice.cpW, vdevice.transmat->m);
        vdevice.cpVvalid = 0;

        vdevice.cpVx = draw_WtoVx(v2);
        vdevice.cpVy = draw_WtoVy(v2);

        draw_copyvector(vdevice.cpWtrans, v2);
}
/******************************************************************************/

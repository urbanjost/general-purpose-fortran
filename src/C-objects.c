/* this code is licensed as public domain */
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#ifdef MSWPC
#include <fcntl.h>
#endif

#ifndef MSWPC
#ifdef SYS5
#include <fcntl.h>
#else
#include <sys/file.h>
#endif
#endif

/*
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
*/

#include "draw.h"


extern void             draw_polyobj(int n, Token dp[]);

typedef struct o {
        int             obno;
        TokList         *tlist;
        struct o        *next;
} Object;

static Object           *object_table[MAXENTS];

static int              obno = -1, omax = 0;

/******************************************************************************/
#ident "@(#)M_DRAW:makeobj - start a new object."
void draw_makeobj(int n){
        Object  *o;

        if (!vdevice.initialized)
                draw_verror("makeobj: draw not initialized");

        for (o = object_table[n % MAXENTS]; o != (Object *)NULL; o = o->next)
                if (o->obno == n) {
                        draw_delobj(n);
                        break;
                }

        obno = n;
        vdevice.tokens = (TokList *)NULL;

        vdevice.inobject = 1;

        if (omax <= n)
                omax = n + 1;
}
/******************************************************************************/
#ident "@(#)M_DRAW:closeobj - close an object"
void draw_closeobj(void){
        Object  *o;

        if (!vdevice.inobject)
                draw_verror("closeobj: not in an object");

        vdevice.inobject = 0;

        o = (Object *)draw_vallocate(sizeof(Object),"from closeobj");
        o->obno = obno;
        o->tlist = vdevice.tokens;
        o->next = object_table[obno % MAXENTS];

        object_table[obno % MAXENTS] = o;

        obno = -1;
}
/******************************************************************************/
#ident "@(#)M_DRAW:delobj - deletes an object, freeing its memory"
void draw_delobj(int n){
        Object  *o, *lo;
        TokList *tl, *ntl;

        for (lo = o = object_table[n % MAXENTS];
             o != (Object *)NULL;
             lo = o, o = o->next)
                if (o->obno == n)
                        break;

        if (o != (Object *)NULL) {
                for (tl = o->tlist; tl != (TokList *)NULL; tl = ntl) {
                        ntl = tl->next;
                        if (tl->toks){
                                draw_vfree(tl->toks,"from delobj 1");
                        }
                        draw_vfree(tl,"from delobj 2");
                }
                if (lo == object_table[n % MAXENTS]){
                        object_table[n % MAXENTS] = (Object *)NULL;
                }else{
                        lo->next = o->next;
                }
                draw_vfree(o,"from delobj 3");
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:genobj - generates a unique object identifier"
int draw_genobj(void){
        return(omax++);
}
/******************************************************************************/
#ident "@(#)M_DRAW:getopenobj - returns the object currently being edited, -1 if none."
int draw_getopenobj(void){
        return(obno);
}

/******************************************************************************/
#ident "@(#)M_DRAW:loadobj - reads in object file file and makes it the object referred to by n."
void draw_loadobj(int n, char *file){
        int     objfd;
        TokList *tl;
        Object  *o;

        if ((objfd = open(file, O_RDONLY)) == EOF) {
                fprintf(stderr, "loadobject: unable to open object file %s", file);
                return;
        }

        o = (Object *)draw_vallocate(sizeof(Object),"from loadobj 1");
        o->obno = n;
        o->next = object_table[n % MAXENTS];

        object_table[n % MAXENTS] = o;

        o->tlist = tl = (TokList *)draw_vallocate(sizeof(TokList),"from loadobj 2");
        read(objfd, &tl->count, sizeof(tl->count));

        tl->toks = (Token *)draw_vallocate(tl->count * sizeof(Token),"from loadobj 3");
        tl->next = (TokList *)NULL;

        read(objfd, tl->toks, tl->count * sizeof(Token));

        close(objfd);
}
/******************************************************************************/
#ident "@(#)M_DRAW:saveobj - saves the object n into file file."
void draw_saveobj(int n, char *file){
        int     objfd, count;
        Object  *o;
        TokList *tl;

        if ((objfd = open(file, O_CREAT | O_TRUNC | O_WRONLY, 0664)) == EOF) {
                fprintf(stderr, "saveobject: unable to open object file %s", file);
		return;
        }

        for (o = object_table[n % MAXENTS]; o != (Object *)NULL; o = o->next)
                if (o->obno == n)
                        break;

        if (o == (Object *)NULL)
                return;

        count = 0;
        for (tl = o->tlist; tl != (TokList *)NULL; tl = tl->next)
                count += tl->count;

        write(objfd, &count, sizeof(count));

        for (tl = o->tlist; tl != (TokList *)NULL; tl = tl->next)
                write(objfd, tl->toks, tl->count * sizeof(Token));

        close(objfd);
}

/******************************************************************************/
#ident "@(#)M_DRAW:doarc - draw an arc or circle."
static void draw_doarc(float x, float y, float xoff, float yoff, float cosine, float sine, int nsegs){
        float   cx, cy, dx, dy;
        int     i;

        cx = x + xoff;
        cy = y + yoff;
        draw_move2(cx, cy);

        for (i = 0; i < nsegs; i++)  {
                dx = cx - x;
                dy = cy - y;
                cx = x + dx * cosine - dy * sine;
                cy = y + dx * sine + dy * cosine;
                draw_draw2(cx, cy);
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:callobj - draws an object"
void draw_callobj(int n){
   Object    *o;
   TokList   *tl;
   Matrix    prod, tmpmat;
   Tensor    S;
   int      sync, i, j, inpoly;
   float    cx, cy, cz, *m;
   register Token *t, *et, *pt;

   if (!vdevice.initialized)
      draw_verror("callobj: draw not initialized");

   if (vdevice.inobject) {
      t = draw_newtokens(2);

      t[0].i = OBJ_CALLOBJ;
      t[1].i = n;

      return;
   }

   for (o = object_table[n % MAXENTS]; o != (Object *)NULL; o = o->next)
      if (o->obno == n)
         break;

   if (o == (Object *)NULL)
      return;

   if ((sync = vdevice.sync))
      vdevice.sync = 0;

   for (tl = o->tlist; tl != (TokList *)NULL; tl = tl->next) {
      t = tl->toks;
      et = &tl->toks[tl->count];
      while (t != et) {
         switch (t->i) {
         case OBJ_ARC:
            draw_doarc(t[1].f, t[2].f, t[3].f, t[4].f, t[5].f, t[6].f, t[7].i);
            t += 8;
            break;
         case OBJ_BACKBUFFER:
            draw_backbuffer();
            t++;
            break;
         case OBJ_BACKFACING:
            draw_backface(t[1].i);
            t += 2;
            break;
         case OBJ_BOXTEXT:
            draw_boxtext(t[1].f, t[2].f, t[3].f, t[4].f, (char *)&t[5]);
            t += 6 + (int)strlen((char *)&t[5]) / sizeof(Token);
            break;
         case OBJ_CALLOBJ:
            draw_callobj(t[1].i);
            t += 2;
            break;
         case OBJ_TEXTSLANT:
            draw_textslant(t[1].f);
            t += 2;
            break;
         case OBJ_CENTERTEXT:
            draw_centertext(t[1].i);
            t += 2;
            break;
         case OBJ_TEXTJUSTIFY:
            draw_textjustify(t[1].i);
            t += 2;
            break;
         case OBJ_RIGHTJUSTIFY:
            draw_rightjustify();
            t += 1;
            break;
         case OBJ_LEFTJUSTIFY:
            draw_leftjustify();
            t += 1;
            break;
         case OBJ_XCENTERTEXT:
            draw_xcentertext();
            t += 1;
            break;
         case OBJ_YCENTERTEXT:
            draw_ycentertext();
            t += 1;
            break;
         case OBJ_TOPJUSTIFY:
            draw_topjustify();
            t += 1;
            break;
         case OBJ_BOTTOMJUSTIFY:
            draw_bottomjustify();
            t += 1;
            break;
         case OBJ_CIRCLE:
            inpoly = vdevice.inpolygon;
            if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !inpoly)
             draw_makepoly();
             draw_doarc(t[1].f, t[2].f, t[3].f, 0.0, t[4].f, t[5].f, t[6].i);
            if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !inpoly)
             draw_closepoly();
            else
             draw_draw2(t[1].f + t[3].f, t[2].f);
            t += 7;
            break;
         case OBJ_CLEAR:
            (*vdevice.dev.Vclear)();
            t++;
            break;
         case OBJ_COLOR:
            draw_color(t[1].i);
            t += 2;
            break;
         case OBJ_LINEWIDTH:
            draw_linewidth(t[1].i);
            t += 2;
            break;
         case OBJ_DRAW:
            draw_draw(t[1].f, t[2].f, t[3].f);
            t += 4;
            break;
         case OBJ_DASHCODE:
            draw_dashcode(t[1].f);
            t += 2;
            break;
         case OBJ_DRAWCHAR:
            draw_drawchar(t[1].i);
            t += 2;
            break;
         case OBJ_DRAWSTR:
            draw_drawstr((char *)&t[1]);
            t += 2 + (int)strlen((char *)&t[1]) / sizeof(Token);
            break;
         case OBJ_LINESTYLE:
            draw_linestyle((char *)&t[1]);
            t += 2 + (int)strlen((char *)&t[1]) / sizeof(Token);
            break;
         case OBJ_FIXEDWIDTH:
            draw_fixedwidth(t[1].i);
            t += 2;
            break;
         case OBJ_FONT:
            draw_font((char *)&t[1]);
            t += 2 + (int)strlen((char *)&t[1]) / sizeof(Token);
            break;
         case OBJ_HATCHANG:
            vdevice.attr->a.hatchcos = t[1].f;
            vdevice.attr->a.hatchsin = t[2].f;
            t += 3;
            break;
         case OBJ_HATCHPITCH:
            vdevice.attr->a.hatchpitch = t[1].f;
            t += 2;
            break;
         case OBJ_LOADMATRIX:
            m = (float *)vdevice.transmat->m;
            for (i = 0; i < 16; i++)
             *m++ = (++t)->f;

            /* may have changed mapping from world to device coords */
            vdevice.cpVvalid = 0; 
            t++;

            break;
         case OBJ_MAPCOLOR:
            draw_mapcolor(t[1].i, t[2].i, t[3].i, t[4].i);
            t += 5;
            break;
         case OBJ_MOVE:
            draw_move(t[1].f, t[2].f, t[3].f);
            t += 4;
            break;
         case OBJ_MULTMATRIX:
            m = (float *)tmpmat;
            for (i = 0; i < 16; i++)
             *m++ = (++t)->f;

            draw_mult4x4(prod, tmpmat, vdevice.transmat->m);
            draw_loadmatrix(prod);
            t++;
            break;
         case OBJ_POLY:
            draw_polyobj(t[1].i, &t[2]);
            t += 2 + 3 * t[1].i;
            break;
         case OBJ_POLYFILL:
            draw_polyfill(t[1].i);
            t += 2;
            break;
         case OBJ_POLYHATCH:
            draw_polyhatch(t[1].i);
            t += 2;
            break;
         case OBJ_POPATTRIBUTES:
            draw_popattributes();
            t++;
            break;
         case OBJ_POPMATRIX:
            draw_popmatrix();
            t++;
            break;
         case OBJ_POPVIEWPORT:
            draw_popviewport();
            t++;
            break;
         case OBJ_PUSHATTRIBUTES:
            draw_pushattributes();
            t++;
            break;
         case OBJ_PUSHMATRIX:
            draw_pushmatrix();
            t++;
            break;
         case OBJ_PUSHVIEWPORT:
            draw_pushviewport();
            t++;
            break;
         case OBJ_RCURVE:
            i = (++t)->i;
            cx = (++t)->f;
            cy = (++t)->f;
            cz = (++t)->f;
            m = (float *)tmpmat;
            for (j = 0; j < 16; j++)
             *m++ = (++t)->f;
            draw_mult4x4(prod, tmpmat, vdevice.transmat->m);
            draw_drcurve(i, prod);
            vdevice.cpW[V_X] = cx;
            vdevice.cpW[V_Y] = cy;
            vdevice.cpW[V_Z] = cz;
            t++;
            break;
         case OBJ_RPATCH:
            pt = t + 10;
            cx = (++t)->f;
            cy = (++t)->f;
            cz = (++t)->f;
            for (i = 0; i < 4; i++)
             for (j = 0; j < 4; j++) {
              S[0][i][j] = (pt++)->f;
              S[1][i][j] = (pt++)->f;
              S[2][i][j] = (pt++)->f;
              S[3][i][j] = (pt++)->f;
             }

            draw_transformtensor(S, vdevice.transmat->m);
            draw_drpatch(S, t[1].i, t[2].i, t[3].i, t[4].i, t[5].i, t[6].i);

            vdevice.cpW[V_X] = cx;
            vdevice.cpW[V_Y] = cy;
            vdevice.cpW[V_Z] = cz;
            t = pt;
            break;
         case OBJ_SECTOR:
            inpoly = vdevice.inpolygon;

            if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !inpoly)
             draw_makepoly();

            draw_doarc(t[1].f, t[2].f, t[3].f, t[4].f, t[5].f, t[6].f, t[7].i);
            draw_draw(t[1].f, t[2].f, 0.0);

            if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !inpoly)
             draw_closepoly();
            else
             draw_draw2(t[1].f + t[3].f, t[2].f + t[4].f);
            t += 8;
            break;
         case OBJ_TEXTANG:
            vdevice.attr->a.textcos = t[1].f;
            vdevice.attr->a.textsin = t[2].f;
            t += 3;
            break;
         case OBJ_TEXTSIZE:
            draw_textsize(t[1].f, t[2].f);
            t += 3;
            break;
         case OBJ_VIEWPORT:
            draw_viewport(t[1].f, t[2].f, t[3].f, t[4].f);
            t += 5;
            break;
         case OBJ_TRANSLATE:
            draw_translate(t[1].f, t[2].f, t[3].f);
            t += 4;
            break;
         case OBJ_SCALE:
            /*
             * Do the operations directly on the top matrix of
             * the stack to speed things up.
             */

            vdevice.transmat->m[0][0] *= t[1].f;
            vdevice.transmat->m[0][1] *= t[1].f;
            vdevice.transmat->m[0][2] *= t[1].f;
            vdevice.transmat->m[0][3] *= t[1].f;

            vdevice.transmat->m[1][0] *= t[2].f;
            vdevice.transmat->m[1][1] *= t[2].f;
            vdevice.transmat->m[1][2] *= t[2].f;
            vdevice.transmat->m[1][3] *= t[2].f;

            vdevice.transmat->m[2][0] *= t[3].f;
            vdevice.transmat->m[2][1] *= t[3].f;
            vdevice.transmat->m[2][2] *= t[3].f;
            vdevice.transmat->m[2][3] *= t[3].f;

            t += 4;
            break;
         case OBJ_ROTATE:
            draw_rotate(t[1].f, (char)t[2].i);
            t += 3;
            break;
         case OBJ_VFLUSH: /* JSU: ADDED THIS Sun Jun  4 22:47:05 EDT 1995 */
            t++;
            break;
         case OBJ_RECT: /* JSU: ADDED THIS Sun May  8  2004 */
            draw_rect(t[1].f, t[2].f, t[3].f, t[4].f);
            t += 5;
            break;
         default:
            fprintf(stderr,"UNEXPECTED OBJECT %d\n",t->i);
            draw_verror("draw: internal error in callobj");
            exit(1);
         }
      }
   }

   if (sync) {
      vdevice.sync = 1;
      (*vdevice.dev.Vsync)();
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:isobj - returns 1 if there is an object n, 0 otherwise."
int draw_isobj(int n){
        Object  *o;

        for (o = object_table[n % MAXENTS]; o != (Object *)NULL; o = o->next)
                if (o->obno == n)
                        break;

        return(o != (Object *)NULL);
}
/******************************************************************************/

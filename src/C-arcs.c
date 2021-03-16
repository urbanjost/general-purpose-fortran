/* this code is licensed as public domain */
#include "draw.h"
#include <math.h>
#include <stdio.h>

static int      nsegs = 32;
/******************************************************************************/
#ident "@(#)M_DRAW:arcprecision - sets the number of segments in an arc or circle. - obsolete function."
void draw_arcprecision(int noseg) {
        nsegs = noseg;
}
/******************************************************************************/
#ident "@(#)M_DRAW:circleprecision - sets the number of segments in an arc or circle."
void draw_circleprecision(int noseg) {
        nsegs = noseg;
}
/******************************************************************************/
#ident "@(#)M_DRAW:arc - draw an arc at given location."
#ident "@(#)M_DRAW ... Precision of arc (# line segments) is calculated from the value given to circleprecision."
void draw_arc (float x, float y, float radius, float startang, float endang) {
   Token *t;
   float cx, cy, dx, dy;
   float deltang, cosine, sine, angle;
   int sync, i, numsegs;

   if (!vdevice.initialized)
      draw_verror ("arc: draw not initialized");

   if ((sync = vdevice.sync))
      vdevice.sync = V_FALSE;

   angle = startang * D2R;
   numsegs = fabs (endang - startang) / 360.0 * nsegs + 0.5;
   /*JSU*/ deltang = (endang - startang) * D2R / numsegs;
   cosine = cos ((double) deltang);
   sine = sin ((double) deltang);

   if (vdevice.inobject) {
      t = draw_newtokens (8);
      t[0].i = OBJ_ARC;
      t[1].f = x;
      t[2].f = y;
      t[3].f = radius * cos ((double) angle);
      t[4].f = radius * sin ((double) angle);
      t[5].f = cosine;
      t[6].f = sine;
      t[7].i = numsegs;
      return;
   }

   /* calculates initial point on arc */

   cx = x + radius * cos ((double) angle);
   cy = y + radius * sin ((double) angle);
   draw_move2 (cx, cy);

   for (i = 0; i < numsegs; i++) {
      dx = cx - x;
      dy = cy - y;
      cx = x + dx * cosine - dy * sine;
      cy = y + dx * sine + dy * cosine;
      draw_draw2 (cx, cy);
   }

   if (sync) {
      vdevice.sync = V_TRUE;
      (*vdevice.dev.Vsync) ();
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:sector - draw a sector at given location."
#ident "@(#)M_DRAW ... The number of line segments in the arc of the segment is the same as in arc."
void draw_sector (float x, float y, float radius, float startang, float endang) {
   Token *t;
   float cx, cy, dx, dy;
   float deltang, cosine, sine, angle;
   int sync, i, numsegs, inpoly;

   if (!vdevice.initialized)
      draw_verror ("segment: draw not initialized");

   angle = startang * D2R;
   numsegs = fabs (endang - startang) / 360.0 * nsegs + 0.5;
   deltang = (endang - startang) * D2R / numsegs;
   cosine = cos ((double) deltang);
   sine = sin ((double) deltang);

   if (vdevice.inobject) {
      t = draw_newtokens (8);
      t[0].i = OBJ_SECTOR;
      t[1].f = x;
      t[2].f = y;
      t[3].f = radius * cos ((double) angle);
      t[4].f = radius * sin ((double) angle);
      t[5].f = cosine;
      t[6].f = sine;
      t[7].i = numsegs;
      return;
   }

   if ((sync = vdevice.sync))
      vdevice.sync = V_FALSE;

   inpoly = vdevice.inpolygon;

   if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !inpoly){
      draw_makepoly ();		/* want it filled */
   }

   draw_move2 (x, y);
   /* calculates initial point on arc */

   cx = x + radius * cos ((double) angle);
   cy = y + radius * sin ((double) angle);

   draw_draw2 (cx, cy);

   for (i = 0; i < numsegs; i++) {
      dx = cx - x;
      dy = cy - y;
      cx = x + dx * cosine - dy * sine;
      cy = y + dx * sine + dy * cosine;
      draw_draw2 (cx, cy);
   }
   draw_draw2 (x, y);

   if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !inpoly) {
      draw_closepoly ();
   }
   if (sync) {
      vdevice.sync = V_TRUE;
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:circle - * Draw a circle of given radius at given world coordinates."
#ident "@(#)M_DRAW ... The number of segments in the circle is the same as that of an arc."
void draw_circle (float x, float y, float radius) {
   Token *t;
   float cx, cy, dx, dy;
   float angle, cosine, sine;
   int sync, i, inpoly;
   int need_to_close = 0;

   if (!vdevice.initialized)
      draw_verror ("circle: draw not initialized");

   angle = 2.0 * PI / nsegs;
   cosine = cos ((double) angle);
   sine = sin ((double) angle);

   if (vdevice.inobject) {
      t = draw_newtokens (7);
      t[0].i = OBJ_CIRCLE;
      t[1].f = x;
      t[2].f = y;
      t[3].f = radius;
      t[4].f = cosine;
      t[5].f = sine;
      t[6].i = nsegs;
      return;
   }

   if ((sync = vdevice.sync)) {
      vdevice.sync = V_FALSE;
   }

   cx = x + radius;
   cy = y;

   inpoly = vdevice.inpolygon;

   if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !inpoly) {
      draw_makepoly ();		/* want it filled */
      need_to_close = 1;
   } else {
      need_to_close = 0;
   }

   draw_move2 (cx, cy);
   for (i = 0; i < nsegs - 1; i++) {
      dx = cx - x;
      dy = cy - y;
      cx = x + dx * cosine - dy * sine;
      cy = y + dx * sine + dy * cosine;
      draw_draw2 (cx, cy);
   }
   draw_draw2 (x + radius, y);

   if (need_to_close) {
      draw_closepoly ();
   }
   if (sync) {
      vdevice.sync = V_TRUE;
      (*vdevice.dev.Vsync) ();
   }
}
/******************************************************************************/

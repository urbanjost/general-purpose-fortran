/* this code is licensed as public domain */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "draw.h"

static int do_dash = 1;

/* a == b or b > a */
#define NEAR(a, b)      (((a) - (b)) / (a) < 0.0001)
#define checkdash(l)    ((*(++l->dashp) == '\0' ? *(l->dashp = l->style) : *l->dashp) != '0')

/******************************************************************************/
#ident "@(#)M_DRAW:dashcode - Set the current dash length"
void draw_dashcode (float d) {
   Token *tok;
   if (d < 0.0) {
      d = -d;
   }

   if (d == 0.0) {
      d = 0.1;
   }
/*--------------------------------------------------------------------*/
   if (vdevice.inobject) {
      tok = draw_newtokens (2);
      tok[0].i = OBJ_DASHCODE;
      tok[1].f = d;
      return;
   }
/*--------------------------------------------------------------------*/
   vdevice.attr->a.dash = d;
}
/******************************************************************************/
#ident "@(#)M_DRAW:linestyle - Set the current linestyle as a character string of 1's and 0's"
void draw_linestyle (char *l) {
   Attribute *line;
   char *c;
   Token *tok;
/*--------------------------------------------------------------------*/
   if (vdevice.inobject) {
      tok = draw_newtokens (2 + strlen (l) / sizeof (Token));
      tok[0].i = OBJ_LINESTYLE;
      strcpy ((char *) &tok[1], l);
      return;
   }
/*--------------------------------------------------------------------*/
   line = &vdevice.attr->a;

   if (!l || !*l) {
      line->style = (char *) NULL;
   } else {
      /*
       * If it's all non-0 (or ' ') then it's a solid line
       */
      for (c = l; c != NULL; c++) {
         if (*c == '0' || *c == ' ') {
            if (line->style != (char *) NULL) {
               draw_vfree (line->style, "from linestyle");
            }
            line->style =
               (char *) draw_vallocate (strlen (l) + 1, "from linestyle");
            strcpy (line->style, l);
            line->dashp = line->style;
            line->adist = 0.0;
            do_dash = *line->dashp != '0';
            return;
         }
      }
      line->style = (char *) NULL;
   }

   return;
}

/******************************************************************************/
#ident "@(#)M_DRAW:dashline - Draw dashed lines.Assumes p0 - p1 are valid (possibly clipped endpoints)"
void draw_dashline (register Vector p0, register Vector p1) {
   int vx, vy, sync;
   float dx, dy, dz, dw, dist, ldist, tdist;
   Vector pd;
   Attribute *line;

   line = &vdevice.attr->a;
   if (line->dash == 0.0) {
      vx = draw_WtoVx (p1);
      vy = draw_WtoVy (p1);

      (*vdevice.dev.Vdraw) (vx, vy);
      vdevice.cpVx = vx;
      vdevice.cpVy = vy;
      return;
   }

   /*
    * The distance for this line segment
    */
   dx = p1[V_X] - p0[V_X];
   dy = p1[V_Y] - p0[V_Y];
   dz = p1[V_Z] - p0[V_Z];
   dw = p1[V_W] - p0[V_W];
   ldist = sqrt (dx * dx + dy * dy + dz * dz + dw * dw);

   /*
    * If this distance is less than it takes to
    * complete the current dash then just do it.
    */
   if (ldist <= (line->dash - line->adist)) {

      if (NEAR (line->dash, line->adist)) {
         line->adist = 0.0;
         do_dash = checkdash (line);
      }

      line->adist += ldist;

      vx = draw_WtoVx (p1);
      vy = draw_WtoVy (p1);

      if (do_dash)
         (*vdevice.dev.Vdraw) (vx, vy);

      vdevice.cpVx = vx;
      vdevice.cpVy = vy;

      return;

   } else {
      if ((sync = vdevice.sync))        /* We'll sync at the end */
         vdevice.sync = 0;

      /*
       * If this distance will take us over the end of a
       * dash then break it up.
       */

      /*
       * Handle the initial case where we start in the middle
       * of a dash.
       */

      tdist = 0.0;
      draw_copyvector (pd, p0);

      if (line->adist > 0.0) {

         tdist = (line->dash - line->adist);

         if (NEAR (line->dash, line->adist)) {
            line->adist = 0.0;
            do_dash = checkdash (line);
         }

         line->adist += tdist;

         dist = tdist / ldist;
         pd[V_X] += dx * dist;
         pd[V_Y] += dy * dist;
         pd[V_Z] += dz * dist;
         pd[V_W] += dw * dist;
         vx = draw_WtoVx (pd);
         vy = draw_WtoVy (pd);


         if (do_dash)
            (*vdevice.dev.Vdraw) (vx, vy);

         vdevice.cpVx = vx;
         vdevice.cpVy = vy;
      }

      dx *= line->dash / ldist;
      dy *= line->dash / ldist;
      dz *= line->dash / ldist;
      dw *= line->dash / ldist;
      dist = line->dash;

      while (tdist <= ldist - dist) {

         if (NEAR (line->dash, line->adist)) {
            line->adist = 0.0;
            do_dash = checkdash (line);
         }

         pd[V_X] += dx;
         pd[V_Y] += dy;
         pd[V_Z] += dz;
         pd[V_W] += dw;

         vx = draw_WtoVx (pd);
         vy = draw_WtoVy (pd);

         line->adist += dist;
         tdist += dist;

         if (do_dash)
            (*vdevice.dev.Vdraw) (vx, vy);

         vdevice.cpVx = vx;
         vdevice.cpVy = vy;
      }


      /*
       * Check the last little bit....
       */
      if (NEAR (line->dash, line->adist)) {
         line->adist = 0.0;
         do_dash = checkdash (line);
      }

      dx = p1[V_X] - pd[V_X];
      dy = p1[V_Y] - pd[V_Y];
      dz = p1[V_Z] - pd[V_Z];
      dw = p1[V_W] - pd[V_W];
      dist = sqrt (dx * dx + dy * dy + dz * dz + dw * dw);

      line->adist += dist;

      vx = draw_WtoVx (p1);
      vy = draw_WtoVy (p1);

      if (do_dash)
         (*vdevice.dev.Vdraw) (vx, vy);

      vdevice.cpVx = vx;
      vdevice.cpVy = vy;
   }

   if (sync) {
      vdevice.sync = 1;
      (*vdevice.dev.Vsync) ();
   }

}

/******************************************************************************/
#ident "@(#)M_DRAW:draw - draw a line form the logical graphics position to the world coordinates x, y, z."
void draw_draw (float x, float y, float z) {
   Token *tok;
   int vx, vy;
   Vector res;

/*--------------------------------------------------------------------*/
   if (!vdevice.initialized)
      draw_verror ("draw: draw not initialized");
/*--------------------------------------------------------------------*/
   if (vdevice.inpolygon) {
      (*vdevice.pdraw) (x, y, z);

      vdevice.cpW[V_X] = x;
      vdevice.cpW[V_Y] = y;
      vdevice.cpW[V_Z] = z;

      vdevice.cpVvalid = 0;

      return;
   }
/*--------------------------------------------------------------------*/
   if (vdevice.inobject) {
      tok = draw_newtokens (4);

      tok[0].i = OBJ_DRAW;
      tok[1].f = x;
      tok[2].f = y;
      tok[3].f = z;

      vdevice.cpW[V_X] = x;
      vdevice.cpW[V_Y] = y;
      vdevice.cpW[V_Z] = z;

      vdevice.cpVvalid = 0;

      return;
   }
/*--------------------------------------------------------------------*/
   if (!vdevice.cpVvalid) {
      draw_multvector (vdevice.cpWtrans, vdevice.cpW, vdevice.transmat->m);
   }
/*--------------------------------------------------------------------*/

   vdevice.cpW[V_X] = x;
   vdevice.cpW[V_Y] = y;
   vdevice.cpW[V_Z] = z;
   draw_multvector (res, vdevice.cpW, vdevice.transmat->m);

/*--------------------------------------------------------------------*/
   if (vdevice.clipoff) {
      vx = draw_WtoVx (res);    /* just draw it */
      vy = draw_WtoVy (res);

      if (vdevice.attr->a.style != (char *) NULL) {
         draw_dashline (vdevice.cpWtrans, res);
         vdevice.cpVvalid = 0;
         return;
      }

      (*vdevice.dev.Vdraw) (vx, vy);

      vdevice.cpVx = vx;
      vdevice.cpVy = vy;

      vdevice.cpVvalid = 0;
   } else {
      if (vdevice.cpVvalid)
         draw_quickclip (vdevice.cpWtrans, res);
      else
         draw_clip (vdevice.cpWtrans, res);
   }
/*--------------------------------------------------------------------*/

   vdevice.cpWtrans[V_X] = res[V_X];
   vdevice.cpWtrans[V_Y] = res[V_Y];
   vdevice.cpWtrans[V_Z] = res[V_Z];
   vdevice.cpWtrans[V_W] = res[V_W];
}

/******************************************************************************/
#ident "@(#)M_DRAW:draw2 - draw a line from the logical graphics position  to the world coordinates x, y."

void draw_draw2 (float x, float y) {
   if (!vdevice.initialized) {
      draw_verror ("draw2: draw not initialized");
   }
   draw_draw (x, y, 0.0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:rdraw - 3D relative draw from the logical graphics position by dx, dy, dz."

void draw_rdraw (float dx, float dy, float dz) {
   if (!vdevice.initialized) {
      draw_verror ("rdraw: draw not initialized");
   }

   draw_draw ((vdevice.cpW[V_X] + dx), (vdevice.cpW[V_Y] + dy),
              (vdevice.cpW[V_Z] + dz));
}
/******************************************************************************/
#ident "@(#)M_DRAW:rdraw2 - 2D relative draw from the logical graphics position by dx, dy."
void draw_rdraw2 (float dx, float dy) {

   if (!vdevice.initialized) {
      draw_verror ("rdraw2: draw not initialized");
   }

   draw_draw ((vdevice.cpW[V_X] + dx), (vdevice.cpW[V_Y] + dy), 0.0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:sdraw2 - Draw directly in proportion to screen coordinates."
void draw_sdraw2 (float xs, float ys) {
   int nx, ny;

   if (!vdevice.initialized){
      draw_verror ("sdraw2: draw not initialized");
   }

   nx = (xs / 2 + 0.5) * vdevice.sizeX;
   ny = (0.5 + ys / 2) * vdevice.sizeY;

   (*vdevice.dev.Vdraw) (nx, ny);

   vdevice.cpVx = nx;
   vdevice.cpVy = ny;
   vdevice.cpVvalid = 0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:rsdraw2 - Relative draw as a fraction of screen size."
void draw_rsdraw2 (float dxs, float dys) {
   int ndx, ndy;

   if (!vdevice.initialized){
      draw_verror ("rsdraw2: draw not initialized");
   }

   ndx = dxs * vdevice.sizeSx / 2;
   ndy = dys * vdevice.sizeSy / 2;

   (*vdevice.dev.Vdraw) (vdevice.cpVx + ndx, vdevice.cpVy + ndy);

   vdevice.cpVx += ndx;
   vdevice.cpVy += ndy;
}
/******************************************************************************/

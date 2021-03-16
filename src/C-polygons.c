/* this code is licensed as public domain */
#include <stdio.h>
#include "draw.h"
#include <math.h>

#define MAX(x, y) ((x) > (y) ? (x) : (y))
#define MIN(x, y) ((x) < (y) ? (x) : (y))
#define ABS(x)  ((x) < 0 ? -(x) : (x))

static   float F[6][4], S[6][4], I[4], p[MAXVERTS][4], newp[MAXVERTS][3];
static   int nout, first[6], np;
static   int ip1[MAXVERTS], ip2[MAXVERTS];

/*
 *  Orientation of backfacing polygons(in screen coords)
 */
static   int clockwise = 1;

/******************************************************************************/
static void draw_polyoutline(int n, int ipx[], int ipy[]);
static void draw_polyclip(register int n);
static void draw_shclose(int side);
static void draw_shclip(float p[4], int side);
static int draw_intersect(int side, register Vector I, register Vector p);
static int draw_visible(int side);
static int draw_yintersect(float y,float x1,float y1,float x2,float y2,float *xint);
static int draw_dopoly(int n);
static int draw_checkbacki(void);
/******************************************************************************/
#ident "@(#)M_DRAW:polyfill - set the polygon fill flag. This will always turn off hatching."
void draw_polyfill(int onoff) {
   Token *tok;
   if (vdevice.inobject) {
      tok = draw_newtokens(2);
      tok[0].i = OBJ_POLYFILL;
      tok[1].i = onoff;
      return;
   }
   vdevice.attr->a.fill = onoff;
   vdevice.attr->a.hatch = 0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:polyhatch - set the polygon hatch flag. This will always turn off fill."
void draw_polyhatch(int onoff) {
   Token *tok;
   if (vdevice.inobject) {
      tok = draw_newtokens(2);
      tok[0].i = OBJ_POLYHATCH;
      tok[1].i = onoff;
      return;
   }
   vdevice.attr->a.hatch = onoff;
   vdevice.attr->a.fill = 0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:hatchang - set the hatch angle"
void draw_hatchang(float a){
   Token *tok;
   if (!vdevice.initialized)
      draw_verror("hatchang - draw not initialized");
   if (vdevice.inobject) {
      tok = draw_newtokens(3);
      tok[0].i = OBJ_HATCHANG;
      tok[1].f = cos((double)(a*D2R));
      tok[2].f = sin((double)(a*D2R));
      return;
   }
   vdevice.attr->a.hatchcos = cos((double)(a*D2R));
   vdevice.attr->a.hatchsin = sin((double)(a*D2R));
}
/******************************************************************************/
#ident "@(#)M_DRAW:hatchpitch - set the hatch pitch"
void draw_hatchpitch(float a) {
   Token *tok;
   if (!vdevice.initialized)
      draw_verror("hatchpitch - draw not initialized");
   if (vdevice.inobject) {
      tok = draw_newtokens(2);
      tok[0].i = OBJ_HATCHPITCH;
      tok[1].f = a;
      return;
   }
   vdevice.attr->a.hatchpitch = a;
}
/*****************************************************************************/
#ident "@(#)M_DRAW:hatch - hatch the polygon defined by the n points in the array p."
static void draw_hatch( int n, float p[][3]) {
   float yl, xint, y1, y2, x1, x2, tmp, ymax = -1.0e30, ymin = 1.0e30;
   float xi[256];
   float z, s, c, pitch;
   int   sync, i, j, sorted;

   c = vdevice.attr->a.hatchcos;
   s = vdevice.attr->a.hatchsin;
   pitch = vdevice.attr->a.hatchpitch;

   if ((sync = vdevice.sync)){
      vdevice.sync = 0;
   }
   /* Rotate by the x-hatch angle...  */
   for (i = 0; i < n; i++) {
      tmp = p[i][0];
      p[i][0] = p[i][0] * c - p[i][1] * s;
      p[i][1] = tmp * s + p[i][1] * c;
      ymax = MAX(ymax, p[i][1]);
      ymin = MIN(ymin, p[i][1]);
   }
   /* For each y value, get a list of X intersections...  */
   yl = ymax - pitch;
   while (yl > ymin) {
      j = 0;
      for (i = 0; i < n-1; i++){
         if (draw_yintersect(yl, p[i][0], p[i][1], p[i+1][0], p[i+1][1], &xint)){
            xi[j++] = xint;
	 }
      }
      /* Last one.  */
      if (draw_yintersect(yl, p[n-1][0], p[n-1][1], p[0][0], p[0][1], &xint)){
         xi[j++] = xint;
      }
      /* Sort the X intersections...  */
      sorted = 0;
      while (!sorted) {
         sorted = 1;
         for (i = 0; i < j-1; i++){
            if (xi[i] > xi[i+1]) {
               tmp = xi[i];
               xi[i] = xi[i+1];
               xi[i+1] = tmp;
               sorted = 0;
            }
         }
      }

      /* Draw the lines (Rotated back)...  */
      z = p[0][2];
      for (i = 0; i < j-1; i += 2) {
         y1 = yl * c - xi[i] * s;
         y2 = yl * c - xi[i+1] * s;
         x1 = xi[i] * c + yl * s;
         x2 = xi[i+1] * c + yl * s;
         draw_move(x1, y1, z);
         draw_draw(x2, y2, z);
      }
      yl -= pitch;
   }
   for (i = 0; i < n; i++) {
      tmp = p[i][0];
      p[i][0] = tmp*c + p[i][1]*s;
      p[i][1] = p[i][1]*c - tmp*s;
   }
   draw_move(p[0][0], p[0][1], p[0][2]);
   for (i = 1; i < n; i++){
      draw_draw(p[i][0], p[i][1], p[i][2]);
   }

   draw_draw(p[0][0], p[0][1], p[0][2]);

   if (sync) {
      vdevice.sync = 1;
      (*vdevice.dev.Vsync)();
   }
}
/******************************************************************************/
static int draw_yintersect(float y, float x1, float y1, float x2, float y2, float *xint) {
   float t, a;

   a = y2 - y1;
   if (ABS(a) >= 0.00001) {
      t = (y - y1) / a;
      if (t >= 0.0 && t <= 1.0) {
         *xint = x1 + t*(x2 - x1);
         return (1);
      }
   }
   return (0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:backfacedir - Set which direction backfacing polygons are defined to be."
#ident "@(#)M_DRAW:... * 1 = clockwise (in screen coords) 0 = anticlockwise."
void draw_backfacedir(int cdir) {
   clockwise = cdir;
}
/******************************************************************************/
#ident "@(#)M_DRAW:backface - Turns on culling of backfacing polygons. A polygon is"
#ident "@(#)M_DRAW:... backfacing if its orientation in *screen* coords is clockwise."
void draw_backface(int onoff) {
   vdevice.attr->a.backface = onoff;
}
/******************************************************************************/
#ident "@(#)M_DRAW:dopoly - do a transformed polygon with n edges using fill or hatch"
static int draw_dopoly(int n) {
   int   i;

   if (n > MAXVERTS) {
      fprintf(stderr,"dopoly: can't hatch or fill a polygon with more than %d vertices",MAXVERTS);
      return(0);
   }

   if (!vdevice.clipoff) {
      draw_polyclip(n);
   } else {
      nout = n;
      for (i = 0; i < n; i++) {
         ip1[i] = draw_WtoVx(p[i]);
         ip2[i] = draw_WtoVy(p[i]);
      }
   }

   if (vdevice.attr->a.backface && draw_checkbacki()){
      return(0);
   }

   if (vdevice.attr->a.fill) {
      if (nout > 2) {
         (*vdevice.dev.Vfill)(nout, ip1, ip2);
      }
   } else {
      vdevice.cpVx = ip1[0];
      vdevice.cpVy = ip2[0];
      vdevice.cpVvalid = 0;
      draw_polyoutline(nout, ip1, ip2);
   }
   return(1);
}
/******************************************************************************/
#ident "@(#)M_DRAW:polyoutline - draws a polygon outline from already transformed points."
static void draw_polyoutline(int n, int ipx[], int ipy[]) {
   int   i;

   if (n > 2) {
      for (i = 1; i < n; i++) {
         (*vdevice.dev.Vdraw)(ipx[i], ipy[i]);

         vdevice.cpVx = ipx[i];
         vdevice.cpVy = ipy[i];
      }
      (*vdevice.dev.Vdraw)(ipx[0], ipy[0]);

      vdevice.cpVx = ipx[0];
      vdevice.cpVy = ipy[0];
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:polyobj - construct a polygon from a object token list"
void draw_polyobj(int n, Token dp[]) {
   int   b, i, j;
   float vect[4], result[4];

   for (i = 0, j = 0; i < n; i++, j += 3) {
      vect[V_X] = dp[j + V_X].f;
      vect[V_Y] = dp[j + V_Y].f;
      vect[V_Z] = dp[j + V_Z].f;
      vect[V_W] = 1;
      draw_multvector(result, vect, vdevice.transmat->m);
      p[i][V_X] = result[V_X];
      p[i][V_Y] = result[V_Y];
      p[i][V_Z] = result[V_Z];
      p[i][V_W] = result[V_W];
   }

   /* Already un-synced in the callobj routine... */

   b = draw_dopoly(n);
   if (b && vdevice.attr->a.hatch){
      draw_hatch(n, (float (*)[3])dp);
   }
   vdevice.cpW[V_X] = dp[V_X].f;
   vdevice.cpW[V_Y] = dp[V_Y].f;
   vdevice.cpW[V_Z] = dp[V_Z].f;
}
/******************************************************************************/
#ident "@(#)M_DRAW:poly2 - construct polygon from a (x, y) array of points provided by user"
void draw_poly2(int n, float dp[][2]) {
   int   i;
   float np[MAXVERTS][3];

   if (!vdevice.initialized)
      draw_verror("poly2: draw not initialized");

   for (i = 0; i < n; i++) {
      np[i][V_X] = dp[i][V_X];
      np[i][V_Y] = dp[i][V_Y];
      np[i][V_Z] = 0.0;
   }
   draw_poly(n, np);
}
/******************************************************************************/
#ident "@(#)M_DRAW:poly - construct a polygon from an array of points provided by the user."
void draw_poly(int n, float dp[][3]) {
   int   sync, b, i, j;
   Vector   vect, result;
   Token *tok;

   if (!vdevice.initialized){
      draw_verror("poly: draw not initialized");
   }

   if (vdevice.inobject) {
      tok = draw_newtokens(2 + 3 * n);
      tok[0].i = OBJ_POLY;
      tok[1].i = n;
      for (i = 0, j = 2; i < n; i++, j += 3) {
         tok[j + V_X].f = dp[i][V_X];
         tok[j + V_Y].f = dp[i][V_Y];
         tok[j + V_Z].f = dp[i][V_Z];
      }
      return;
   }


   for (i = 0; i < n; i++) {
      vect[V_X] = dp[i][V_X];
      vect[V_Y] = dp[i][V_Y];
      vect[V_Z] = dp[i][V_Z];
      vect[V_W] = 1;
      draw_multvector(result, vect, vdevice.transmat->m);
      p[i][V_X] = result[V_X];
      p[i][V_Y] = result[V_Y];
      p[i][V_Z] = result[V_Z];
      p[i][V_W] = result[V_W];
   }

   if ((sync = vdevice.sync)){
      vdevice.sync = 0;
   }

   b = draw_dopoly(n);
   if (b && vdevice.attr->a.hatch){
      draw_hatch(n, dp);
   }

   if (sync) {
      vdevice.sync = 1;
      (*vdevice.dev.Vsync)();
   }

   vdevice.cpW[V_X] = dp[0][V_X];
   vdevice.cpW[V_Y] = dp[0][V_Y];
   vdevice.cpW[V_Z] = dp[0][V_Z];
}
/******************************************************************************/
#ident "@(#)M_DRAW:pmove - set the start position of a polygon"
void draw_pmove(float x, float y, float z) {
   np = 0;
   p[np][V_X] = x;
   p[np][V_Y] = y;
   p[np][V_Z] = z;
   p[np][V_W] = 1.0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:pdraw - add another vertex to the polygon array"
void draw_pdraw(float x, float y, float z) {
   char  buf[100];

   np++;

   if (np >= MAXVERTS) {
      sprintf(buf,
         "pdraw: can't draw a polygon with more than %d vertices",
         MAXVERTS);
      draw_verror(buf);
   }

   p[np][V_X] = x;
   p[np][V_Y] = y;
   p[np][V_Z] = z;
   p[np][V_W] = 1.0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:makepoly - set up a polygon which will be constructed by a series of move draws."
void draw_makepoly(void) {
   vdevice.inpolygon = 1;
   vdevice.pmove = draw_pmove;
   vdevice.pdraw = draw_pdraw;
   np = 0;
   p[np][V_X] = vdevice.cpW[V_X];
   p[np][V_Y] = vdevice.cpW[V_Y];
   p[np][V_Z] = vdevice.cpW[V_Z];
   p[np][V_W] = 1.0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:closepoly - draw the polygon started by the above."
void draw_closepoly(void) {
   float lstx, lsty, lstz;
   Vector   result ;
   int   sync, b, i, j;
   Token *tok;

   if (!vdevice.initialized){
      draw_verror("closepoly: draw not initialized");
   }

   vdevice.inpolygon = 0;

   if (vdevice.inobject) {
      tok = draw_newtokens(2 + 3 * (np + 1));
      tok[0].i = OBJ_POLY;
      tok[1].i = np + 1;
      for (i = 0, j = 2; i <= np; i++, j += 3) {
         tok[j + V_X].f = p[i][V_X];
         tok[j + V_Y].f = p[i][V_Y];
         tok[j + V_Z].f = p[i][V_Z];
      }

      return;
   }

   lstx = p[np][V_X];
   lsty = p[np][V_Y];
   lstz = p[np][V_Z];

   np++;

   if (vdevice.attr->a.hatch){
      for (i = 0; i < np; i++) {
         newp[i][0] = p[i][0];
         newp[i][1] = p[i][1];
         newp[i][2] = p[i][2];
      }
   }

   for (i = 0; i < np; i++) {
      draw_multvector(result, p[i], vdevice.transmat->m);
      p[i][V_X] = result[V_X];
      p[i][V_Y] = result[V_Y];
      p[i][V_Z] = result[V_Z];
      p[i][V_W] = result[V_W];
   }

   if ((sync = vdevice.sync)){
      vdevice.sync = 0;
   }
   b = draw_dopoly(np);
   if (b && vdevice.attr->a.hatch){
      draw_hatch(np, newp);
   }

   if (sync) {
      vdevice.sync = 1;
      (*vdevice.dev.Vsync)();
   }
   vdevice.cpW[V_X] = lstx;
   vdevice.cpW[V_Y] = lsty;
   vdevice.cpW[V_Z] = lstz;
}
/******************************************************************************/
#ident "@(#)M_DRAW:checkbacki - Checks if a transformed polygon is backfacing or not."
static int draw_checkbacki(void) {

#ifdef MSWPC /* Only has 16 bit ints */
#define  BACKFACE(z) (clockwise ? ((z) <= 0L) : ((z) > 0L))
   long  z;
#else
#define  BACKFACE(z) (clockwise ? ((z) <= 0) : ((z) > 0))
   int   z;
#endif

   int   x1, x2, y1, y2;

   /* 
   if the polygon has points very close together integer math will
   make this get the wrong result - JSU
   */ 

   x1 = ip1[1] - ip1[0];
   x2 = ip1[2] - ip1[1];
   y1 = ip2[1] - ip2[0];
   y2 = ip2[2] - ip2[1];

#ifdef MSWPC
   z = (long)x1 * (long)y2 - (long)y1 * (long)x2;
#else
   z = x1 * y2 - y1 * x2;
#endif

   return(BACKFACE(z));
}
/******************************************************************************/
#ident "@(#)M_DRAW:The following routines are an implementation of the Sutherland - Hodgman"
#ident "@(#)M_DRAW:polygon clipper, as described in 'Reentrant Polygon Clipping'"
#ident "@(#)M_DRAW:Communications of the ACM Jan 1974, Vol 17 No. 1."
/******************************************************************************/
#ident "@(#)M_DRAW:polyclip"
static void draw_polyclip(register int n) {
   int   i;

   nout = 0;
   for (i = 0; i < 6; i++)
      first[i] = 1;

   for (i = 0; i < n; i++)
      draw_shclip(p[i], 0);

   draw_shclose(0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:shclip"
static void draw_shclip(float p[4], int side) {
   float P[4];

   if (side == 6) {
      ip1[nout] = draw_WtoVx(p);
      ip2[nout++] = draw_WtoVy(p);
   } else {
      draw_copyvector(P, p);
      if (first[side]) {
         first[side] = 0;
         draw_copyvector(F[side], P);
      } else if (draw_intersect(side, I, P)) {
         draw_shclip(I, side + 1);
      }
      draw_copyvector(S[side], P);
      if (draw_visible(side))
         draw_shclip(S[side], side + 1);
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:shclose"
static void draw_shclose(int side) {
   if (side < 6) {
      if (draw_intersect(side, I, F[side]))
         draw_shclip(I, side + 1);

      draw_shclose(side + 1);

      first[side] = 1;
   }
}

/******************************************************************************/
#ident "@(#)M_DRAW:intersect"
static int draw_intersect(int side, register Vector I, register Vector p) {
   register float wc1=0.0, wc2=0.0, a;

   switch (side) {
   case 0:   /* x - left */
      wc1 = p[3] + p[0];
      wc2 = S[side][3] + S[side][0];
      break;
   case 1:   /* x - right */
      wc1 = p[3] - p[0];
      wc2 = S[side][3] - S[side][0];
      break;
   case 2:   /* y - bottom */
      wc1 = p[3] + p[1];
      wc2 = S[side][3] + S[side][1];
      break;
   case 3:   /* y - top */
      wc1 = p[3] - p[1];
      wc2 = S[side][3] - S[side][1];
      break;
   case 4:   /* z - near */
      wc1 = p[3] + p[2];
      wc2 = S[side][3] + S[side][2];
      break;
   case 5:   /* z - far */
      wc1 = p[3] - p[2];
      wc2 = S[side][3] - S[side][2];
      break;
   default:
      draw_verror("intersect: ridiculous side value");
   }

   if (wc1 * wc2 < 0.0) { /* Both are opposite in sign - crosses */
      a = wc1 / (wc1 - wc2);
      if (a < 0.0 || a > 1.0) {
         return(0);
      } else {
         I[0] = p[0] + a * (S[side][0] - p[0]);
         I[1] = p[1] + a * (S[side][1] - p[1]);
         I[2] = p[2] + a * (S[side][2] - p[2]);
         I[3] = p[3] + a * (S[side][3] - p[3]);
         return(1);
      }
   }
   return(0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:visible"
static int draw_visible(int side) {
   float wc=0.0;

   switch (side) {
   case 0:   /* x - left */
      wc = S[side][3] + S[side][0];
      break;
   case 1:   /* x - right */
      wc = S[side][3] - S[side][0];
      break;
   case 2:   /* y - bottom */
      wc = S[side][3] + S[side][1];
      break;
   case 3:   /* y - top */
      wc = S[side][3] - S[side][1];
      break;
   case 4:   /* z - near */
      wc = S[side][3] + S[side][2];
      break;
   case 5:   /* z - far */
      wc = S[side][3] - S[side][2];
      break;
   default:
      draw_verror("visible: ridiculous side value");
   }

   return(wc >= 0.0);
}
/******************************************************************************/

/* this code is licensed as public domain */
#include "draw.h"
#include <math.h>

/*
 * NOTE: the words hither and yon are used in this file instead of near and far
 * as they are keywords on some PC compilers (groan). Change them back at your 
 * own peril.
 */

#define  SQ(a) ((a)*(a))
#define COT(a) ((float)(cos((double)(a)) / sin((double)(a))))

/******************************************************************************/
#ident "@(#)M_DRAW:polarview - Specify the viewer's position in polar coordinates"
/*
 * by giving the distance from the viewpoint to the world origin,
 *
 * the azimuthal angle in the x-y plane, measured from the y-axis,
 * the incidence angle in the y-z plane, measured from the z-axis,
 * and the twist angle about the line of sight.
 *
 */
void draw_polarview(float dist, float azim, float inc, float twist) {
   if (!vdevice.initialized)
      draw_verror("polarview: draw not initialized");

   draw_translate(0.0, 0.0, -dist);
   draw_rotate(-twist, 'z');
   draw_rotate(-inc, 'x');
   draw_rotate(-azim, 'z');
}
/******************************************************************************/
#ident "@(#)M_DRAW:up - set the up vector"
void draw_up(float x, float y, float z) {
   vdevice.upset = 1;

   vdevice.upvector[V_X] = x;
   vdevice.upvector[V_Y] = y;
   vdevice.upvector[V_Z] = z;
}
/******************************************************************************/
#ident "@(#)M_DRAW:normallookat - do the standard lookat transformation."
static void draw_normallookat(float vx,float vy,float vz,float px,float py,float pz){
   float l2, l3, sintheta, sinphi, costheta, cosphi;
   Matrix   tmp;

   l2 = sqrt((double)(SQ((px - vx)) + SQ((pz - vz))));
   l3 = sqrt((double)(SQ((px - vx)) + SQ((py - vy)) + SQ((pz - vz))));

   if (l3 != 0.0) {
      sinphi = (vy - py) / l3;
      cosphi = l2 / l3;

      /*
       * Rotate about X by phi
       */
      draw_identmatrix(tmp);
      tmp[1][1] = tmp[2][2] = cosphi;
      tmp[1][2] = sinphi;
      tmp[2][1] = -sinphi;
      draw_multmatrix(tmp);
   }

   if (l2 != 0.0) {
      sintheta = (px - vx) / l2;
      costheta = (vz - pz) / l2;

      /*
       * Rotate about Y by theta
       */
      draw_identmatrix(tmp);
      tmp[0][0] = tmp[2][2] = costheta;
      tmp[0][2] = -sintheta;
      tmp[2][0] = sintheta;
      draw_multmatrix(tmp);
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:lookatwithup - do the standard lookat transformation using an up vector as well."
static void draw_lookatwithup(float vx,float vy,float vz,float px,float py,float pz){
   Vector   t, u, s;
   Matrix   tmp;
   double   dy, dz, lt, lu;

   t[V_X] = vx - px;
   t[V_Y] = vy - py;
   t[V_Z] = vz - pz;

   u[V_X] = vdevice.upvector[V_X];
   u[V_Y] = vdevice.upvector[V_Y];
   u[V_Z] = vdevice.upvector[V_Z];

   lt = sqrt(t[V_X] * t[V_X] + t[V_Y] * t[V_Y] + t[V_Z] * t[V_Z]);

   if (lt == 0.0)
      draw_verror("draw: can't have eyepoint and reference point same in lookat\n");

   lu = sqrt(u[V_X] * u[V_X] + u[V_Y] * u[V_Y] + u[V_Z] * u[V_Z]);

   if (lu == 0.0)
      draw_verror("draw: invalid up vector in lookat.\n");

   /*
    * normalise t and u
    */
   t[V_X] /= lt;
   t[V_Y] /= lt;
   t[V_Z] /= lt;

   u[V_X] /= lu;
   u[V_Y] /= lu;
   u[V_Z] /= lu;

   dz = t[V_X] * u[V_X] + t[V_Y] * u[V_Y] + t[V_Z] * u[V_Z];

   if (fabs(dz) >= 1.0)
      draw_verror("draw: up vector and direction of view are the same.\n");

   dy = sqrt(1.0 - dz * dz);

   /*
    * calculate view up
    */
   u[V_X] = (u[V_X] - dz * t[V_X]) / dy;
   u[V_Y] = (u[V_Y] - dz * t[V_Y]) / dy;
   u[V_Z] = (u[V_Z] - dz * t[V_Z]) / dy;

   /*
    * calculate side vector (cross product of u and t)
    */
   s[V_X] = u[V_Y] * t[V_Z] - u[V_Z] * t[V_Y];
   s[V_Y] = u[V_Z] * t[V_X] - u[V_X] * t[V_Z];
   s[V_Z] = u[V_X] * t[V_Y] - u[V_Y] * t[V_X];

   draw_identmatrix(tmp);

   tmp[0][0] = s[V_X];
   tmp[1][0] = s[V_Y];
   tmp[2][0] = s[V_Z];

   tmp[0][1] = u[V_X];
   tmp[1][1] = u[V_Y];
   tmp[2][1] = u[V_Z];

   tmp[0][2] = t[V_X];
   tmp[1][2] = t[V_Y];
   tmp[2][2] = t[V_Z];

   draw_multmatrix(tmp);
}
/******************************************************************************/
#ident "@(#)M_DRAW:lookat - Specify the viewer's position by giving a viewpoint and a reference point in world coordinates."
/*
 * A twist about the line of sight may also be given. 
 */
void draw_lookat(float vx,float vy,float vz,float px,float py,float pz,float twist){
   if (!vdevice.initialized)
      draw_verror("lookat: draw not initialized");

   draw_rotate(-twist, 'z');

   if (vdevice.upset)
      draw_lookatwithup(vx, vy, vz, px, py, pz);
   else
      draw_normallookat(vx, vy, vz, px, py, pz);

   draw_translate(-vx, -vy, -vz);
}
/******************************************************************************/
#ident "@(#)M_DRAW:perspective - Specify a perspective viewing pyramid in world coordinates"
/*
 * by giving a field of view, aspect ratio, and the locations of the 
 * near(hither) and far(yon) clipping planes in the z direction.
 */
void draw_perspective(float fov,float  aspect,float  hither,float  yon) {
   Matrix    mat;

   if (!vdevice.initialized)
      draw_verror("perspective: draw not initialized");

   if (aspect == 0.0)
      draw_verror("perspective: can't have zero aspect ratio!");

   if ((yon - hither) == 0.0)
      draw_verror("perspective: near clipping plane same as far one.");

   if (fov == 0.0 || fov == 180.0)
      draw_verror("perspective: bad field of view passed.");

   draw_identmatrix(mat);

   mat[0][0] = COT((D2R * fov / 2.0)) / aspect;
   mat[1][1] = COT((D2R * fov / 2.0));

   mat[2][2] = -(yon + hither) / (yon - hither);
   mat[2][3] = -1;
   mat[3][2] = -2.0 * yon * hither / (yon - hither);
   mat[3][3] = 0;

   draw_loadmatrix(mat);
}
/******************************************************************************/
#ident "@(#)M_DRAW:window - Specify a perspective viewing pyramid in world coordinates"
/*
 * by giving a rectangle at the near clipping plane and the location
 * of the far clipping plane.
 *
 */
void draw_window(float left,float right,float bottom,float top,float hither,float yon){
   Matrix    mat;

   if (!vdevice.initialized)
      draw_verror("window: draw not initialized");

   if ((right - left) == 0.0)
      draw_verror("window: left clipping plane same as right one.");

   if ((top - bottom) == 0.0)
      draw_verror("window: bottom clipping plane same as top one.");

   if ((yon - hither) == 0.0)
      draw_verror("window: near clipping plane same as far one.");

   draw_identmatrix(mat);

   mat[0][0] = 2.0 * hither / (right - left);
   mat[1][1] = 2.0 * hither / (top - bottom);
   mat[2][0] = (right + left) / (right - left);
   mat[2][1] = (top + bottom) / (top - bottom);
   mat[2][2] = -(yon + hither) / (yon - hither);
   mat[2][3] = -1.0;
   mat[3][2] = -2.0 * yon * hither / (yon - hither);
   mat[3][3] = 0.0;

   draw_loadmatrix(mat);
}
/******************************************************************************/
#ident "@(#)M_DRAW:ortho - Define a three dimensional viewing box"
/*
 * by giving the left,
 * right, bottom and top clipping plane locations and the distances
 * along the line of sight to the near and far clipping planes.
 *
 */
void draw_ortho(float left,float right,float bottom,float top,float hither,float yon){
   Matrix    mat;

   if (!vdevice.initialized)
      draw_verror("ortho: draw not initialized");

   if ((right - left) == 0.0)
      draw_verror("ortho: left clipping plane same as right one.");

   if ((top - bottom) == 0.0)
      draw_verror("ortho: bottom clipping plane same as top one.");

   if ((yon - hither) == 0.0)
      draw_verror("ortho: near clipping plane same as far one.");

   draw_identmatrix(mat);

   mat[0][0] = 2.0 / (right - left);
   mat[1][1] = 2.0 / (top - bottom);
   mat[2][2] = -2.0 / (yon - hither);
   mat[3][0] = -(right + left) / (right - left);
   mat[3][1] = -(top + bottom) / (top - bottom);
   mat[3][2] = -(yon + hither) / (yon - hither);

   draw_loadmatrix(mat);
}
/******************************************************************************/
#ident "@(#)M_DRAW:ortho2 - Specify a two dimensional viewing rectangle. "
void draw_ortho2(float left,float  right,float  bottom,float  top){
   Matrix   mat;

   if (!vdevice.initialized)
      draw_verror("ortho2: draw not initialized");

   draw_identmatrix(mat);

   if ((right - left) == 0.0)
      draw_verror("ortho2: left clipping plane same as right one.");

   if ((top - bottom) == 0.0)
      draw_verror("ortho2: bottom clipping plane same as top one.");

   mat[0][0] = 2.0 / (right - left);
   mat[1][1] = 2.0 / (top - bottom);
   mat[2][2] = -1.0;
   mat[3][0] = -(right + left) / (right - left);
   mat[3][1] = -(top + bottom) / (top - bottom);

   draw_loadmatrix(mat);
}
/******************************************************************************/

/* this code is licensed as public domain */
#include <stdio.h>
#include "draw.h"

static   Vstack *vsfree = (Vstack *)NULL;
/******************************************************************************/
#ident "@(#)M_DRAW:pushviewport - pushes the current viewport on the viewport stack"
void draw_pushviewport(void) {
   Vstack   *nvport;
   Token *tok;

   if (!vdevice.initialized)
      draw_verror("pushviewport: draw not initialized");

   if (vdevice.inobject) {
      tok = draw_newtokens(1);
      tok->i = OBJ_PUSHVIEWPORT;
      return;
   }

   if (vsfree != (Vstack *)NULL) {
      nvport = vdevice.viewport;
      vdevice.viewport = vsfree;
      vsfree = vsfree->back;
      vdevice.viewport->back = nvport;
      vdevice.viewport->v.left = nvport->v.left;
      vdevice.viewport->v.right = nvport->v.right;
      vdevice.viewport->v.bottom = nvport->v.bottom;
      vdevice.viewport->v.top = nvport->v.top;
   } else {
      nvport = (Vstack *)draw_vallocate(sizeof(Vstack),"from pushviewport");
      nvport->back = vdevice.viewport;
      nvport->v.left = vdevice.viewport->v.left;
      nvport->v.right = vdevice.viewport->v.right;
      nvport->v.bottom = vdevice.viewport->v.bottom;
      nvport->v.top = vdevice.viewport->v.top;
      vdevice.viewport = nvport;
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:popviewport - pops the top viewport off the viewport stack."
void draw_popviewport(void) {
   Token *tok;
   Vstack   *nvport;

   if (!vdevice.initialized)
      draw_verror("popviewport: draw not initialized");

   if (vdevice.inobject) {
      tok = draw_newtokens(1);

      tok->i = OBJ_POPVIEWPORT;

      return;
   }

   if (vdevice.viewport->back == (Vstack *)NULL)
      draw_verror("popviewport: viewport stack underflow");
   else {
      nvport = vdevice.viewport;
      vdevice.viewport = vdevice.viewport->back;
      nvport->back = vsfree;
      vsfree = nvport;
   }

   vdevice.maxVx = vdevice.viewport->v.right * vdevice.sizeX;
   vdevice.maxVy = vdevice.viewport->v.top * vdevice.sizeY;
   vdevice.minVx = vdevice.viewport->v.left * vdevice.sizeX;
   vdevice.minVy = vdevice.viewport->v.bottom * vdevice.sizeY;

   draw_CalcW2Vcoeffs();
}
/******************************************************************************/
#ident "@(#)M_DRAW:viewport - Define a Viewport in Normalized Device Coordinates"
/*
 * The viewport defines that fraction of the screen that the window will
 * be mapped onto.  The screen dimension is -1.0 -> 1.0 for both X & Y.
 */
void draw_viewport(float xlow, float xhigh, float ylow, float yhigh){
   Token *tok;
   char  buf[45];

   if (!vdevice.initialized)
      draw_verror("viewport: draw not initialized");

   /* A few preliminary checks .... */

   if (xlow >= xhigh) {
      sprintf(buf,"viewport: xleft(%5.2f) >= xright(%5.2f)", xlow, xhigh);
      draw_verror(buf);
   }
   if (ylow >= yhigh) {
      sprintf(buf,"viewport: ybottom(%5.2f) >= ytop(%5.2f)", ylow, yhigh);
      draw_verror(buf);
   }

   if (vdevice.inobject) {
      tok = draw_newtokens(5);

      tok[0].i = OBJ_VIEWPORT;
      tok[1].f = xlow;
      tok[2].f = xhigh;
      tok[3].f = ylow;
      tok[4].f = yhigh;

      return;
   }

   /*
    * convert to 0.0 to 1.0
    */
   xlow = xlow / 2 + 0.5;
   xhigh = xhigh / 2 + 0.5;
   ylow = ylow / 2 + 0.5;
   yhigh = yhigh / 2 + 0.5;

   /*
    * Make sure the viewport stack knows about us.....
    */
   vdevice.viewport->v.left = xlow;
   vdevice.viewport->v.right = xhigh;
   vdevice.viewport->v.bottom = ylow;
   vdevice.viewport->v.top = yhigh;

   vdevice.maxVx = xhigh * vdevice.sizeX;
   vdevice.maxVy = yhigh * vdevice.sizeY;
   vdevice.minVx = xlow * vdevice.sizeX;
   vdevice.minVy = ylow * vdevice.sizeY;

   draw_CalcW2Vcoeffs();
}
/******************************************************************************/
#ident "@(#)M_DRAW:getviewport - Returns the left, right, bottom and top limits of the current viewport."
void draw_getviewport(float *left, float *right, float *bottom, float *top){
   *left = (vdevice.viewport->v.left - 0.5) * 2;
   *right = (vdevice.viewport->v.right - 0.5) * 2;
   *bottom = (vdevice.viewport->v.bottom - 0.5) * 2;
   *top = (vdevice.viewport->v.top - 0.5) * 2;
}
/******************************************************************************/

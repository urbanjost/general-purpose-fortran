/* this code is licensed as public domain */
#include "draw.h"
/******************************************************************************/
#ident "@(#)M_DRAW:rect - draw a rectangle given two opposite corners"
void draw_rect(float x1, float y1, float x2, float y2) {
   int started = 0;
   int sync;
   Token *tok;


   if (!vdevice.initialized)
      draw_verror("rect: draw not initialized");

   if(vdevice.inobject){
      tok = draw_newtokens(5);
      tok[0].i = OBJ_RECT;
      tok[1].f = x1;
      tok[2].f = y1;
      tok[3].f = x2;
      tok[4].f = y2;
      return;
   }

   /* if hatch is on or fill is on and not in a polygon start one */
   if ((vdevice.attr->a.fill || vdevice.attr->a.hatch) && !vdevice.inpolygon){
      draw_makepoly();  /* want it filled */
      started = 1;
   } else{
      started = 0;
   }
   if (( sync =vdevice.sync))
	   vdevice.sync = 0;

   draw_move2(x1, y1);
   draw_draw2(x2, y1);
   draw_draw2(x2, y2);
   draw_draw2(x1, y2);
   draw_draw2(x1, y1);

   /* 
    * if fill on or hatch on and in a polygon started in this call close polygon
    */
   if (started){
      draw_closepoly();
   }

   if(sync){
	   vdevice.sync = 1;
	   (*vdevice.dev.Vsync)();
   }
}
/******************************************************************************/
#ident "@(#)M_DRAW:srect - draw a rectangle given two opposite corners in screen coords."
void draw_srect(float x1, float y1, float x2, float y2) {
	int	sync;

	if (!vdevice.initialized)
		draw_verror("rect: draw not initialized");

	if ((sync = vdevice.sync))
		vdevice.sync = 0;

	draw_smove2(x1, y1);
	draw_sdraw2(x2, y1);
	draw_sdraw2(x2, y2);
	draw_sdraw2(x1, y2);

	draw_sdraw2(x1, y1);

	if (sync) {
		vdevice.sync = 1;
		(*vdevice.dev.Vsync)();
	}
}
/******************************************************************************/

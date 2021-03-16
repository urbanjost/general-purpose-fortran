/* this code is licensed as public domain */
#include "draw.h"
/******************************************************************************/
#ident "@(#)M_DRAW:point - plot a point in x, y, z."
void draw_point(float x, float y, float z){

	if(!vdevice.initialized)
		draw_verror("point: draw not initialized");

	draw_move(x, y, z);  
	draw_draw(x, y, z);	
}
/******************************************************************************/
#ident "@(#)M_DRAW:point2 - plot a point in x, y."
void draw_point2(float x, float y){
	draw_move(x, y, 0.0);
	draw_draw(x, y, 0.0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:spoint - plot a point in screen coords."
void draw_spoint2(float xs, float ys){
	if (!vdevice.initialized)
		draw_verror("spoint2: draw not initialized");

	(*vdevice.dev.Vdraw)(vdevice.cpVx = 
	   (xs + 0.5) * (vdevice.maxVx - vdevice.minVx),
	   vdevice.cpVy = (ys + 0.5) * (vdevice.maxVy - vdevice.minVy));
}
/******************************************************************************/

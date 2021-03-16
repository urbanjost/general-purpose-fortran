/* this code is licensed as public domain */
#include "draw.h"
#include <stdlib.h>
#include <stdio.h>

/******************************************************************************/
#ident "@(#)M_DRAW:getgp - return the current (x, y, z) graphics position"
void draw_getgp(float *x, float *y, float *z){
	*x = vdevice.cpW[V_X];
	*y = vdevice.cpW[V_Y];
	*z = vdevice.cpW[V_Z];
	/*
	if (vdevice.inobject) {
	        call move(x,y,z);
		fprintf(stderr,"warning: getgp called in object \n");
	}
	*/
}
/******************************************************************************/
#ident "@(#)M_DRAW:getgp2 - return the current (x, y) graphics position"
void draw_getgp2(float *x, float *y){
	*x = vdevice.cpW[V_X];
	*y = vdevice.cpW[V_Y];
	/*
	if (vdevice.inobject) {
	        call move2(x,y);
		fprintf(stderr,"warning: getgp2 called in object \n");
	}
	*/
}
/******************************************************************************/
#ident "@(#)M_DRAW:getgpt - return the current transformed graphics position."
void    draw_getgpt(float *x, float *y, float *z, float *w) {
	draw_multvector(vdevice.cpWtrans, vdevice.cpW, vdevice.transmat->m);

        *x = vdevice.cpWtrans[V_X];
        *y = vdevice.cpWtrans[V_Y];
        *z = vdevice.cpWtrans[V_Z];
        *w = vdevice.cpWtrans[V_W];
}
/******************************************************************************/
#ident "@(#)M_DRAW:sgetgp2 - return the current (x, y) graphics position in screen coordinates"
void draw_sgetgp2(float *x, float *y){
	float	sx, sy;

	sx = vdevice.maxVx - vdevice.minVx;
	sy = vdevice.maxVy - vdevice.minVy;

	draw_multvector(vdevice.cpWtrans, vdevice.cpW, vdevice.transmat->m);
	*x = 2.0 * draw_WtoVx(vdevice.cpWtrans) / sx - 1.0;
	*y = 2.0 * draw_WtoVy(vdevice.cpWtrans) / sy - 1.0;
	/*
	if (vdevice.inobject) {
		fprintf(stderr,"warning: sgetgp2 called in object \n");
	}
	*/
}
/******************************************************************************/

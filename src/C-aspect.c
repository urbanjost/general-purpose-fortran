/* this code is licensed as public domain */
#include "draw.h"
#include <stdio.h>
#include <stdlib.h>

#define	MINF(x, y)	(float)((x) < (y) ? (x) : (y))
#define	MIN(x, y)	((x) < (y) ? (x) : (y))
/******************************************************************************/
#ident "@(#)M_DRAW:getaspect - Gets the aspect ratio of the display/window.  IE. y / x"
float draw_getaspect(void) {
        /*
	if (vdevice.inobject) {
		fprintf(stderr,"*warning: getaspect called in object definition\n");
	}
	*/
	return((float)vdevice.sizeSy / (float)vdevice.sizeSx);
}
/******************************************************************************/
#ident "@(#)M_DRAW:getdisplaysize - Returns the raw size of the display window in pixel units as floating point values."
void draw_getdisplaysize(float *x, float *y) {
        /*
	if (vdevice.inobject) {
		fprintf(stderr,"*warning: getdisplaysize called in object definition\n");
	}
	*/
	*x = (float)vdevice.sizeSx;
	*y = (float)vdevice.sizeSy;
}
/******************************************************************************/
#ident "@(#)M_DRAW:getfactors - returns x,y scaling factors for use with the viewport call"
#ident "@(#)M_DRAW ... so the viewport can be set to the whole display/window."
void draw_getfactors(float *x, float *y) {
        /*
	if (vdevice.inobject) {
		fprintf(stderr,"*warning: getfactors called in object definition\n");
	}
	*/
	*x = (float)vdevice.sizeSx / (float)vdevice.sizeX;
	*y = (float)vdevice.sizeSy / (float)vdevice.sizeY;
}
/******************************************************************************/
#ident "@(#)M_DRAW:expandviewport - set viewport to whole device"
/*
 *    M_DRAW will normally use the largest square it can fit onto the
 * actual display device. This call says to use the whole device... however
 * you must then take into account any distortion that will occur due to
 * the non square mapping.
 */
void draw_expandviewport(void){
      vdevice.attr->a.exvp = 1;
      vdevice.sizeX = vdevice.sizeSx;
      vdevice.sizeY = vdevice.sizeSy;

      draw_CalcW2Vcoeffs();
}
/******************************************************************************/
#ident "@(#)M_DRAW:unexpandviewport - opposite of the above (expandviewport)"
void draw_unexpandviewport(void){
      vdevice.attr->a.exvp = 0;
      vdevice.sizeY = vdevice.sizeX = MIN(vdevice.sizeSx, vdevice.sizeSy);

      draw_CalcW2Vcoeffs();
}
/******************************************************************************/

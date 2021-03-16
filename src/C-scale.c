/* this code is licensed as public domain */
#include "draw.h"

#ident "@(#)M_DRAW:scale - Set up a scale matrix and premultiply it and the top matrix on the stack."
void draw_scale(float x, float y, float z){
	Token	*tok;

	if (!vdevice.initialized)
		draw_verror("scale: draw not initialized");

	if (vdevice.inobject) {
		tok = draw_newtokens(4);

		tok[0].i = OBJ_SCALE;
		tok[1].f = x;
		tok[2].f = y;
		tok[3].f = z;

		return;
	}

	/*
	 * Do the operations directly on the top matrix of
	 * the stack to speed things up.
	 */

	vdevice.transmat->m[0][0] *= x;
	vdevice.transmat->m[0][1] *= x;
	vdevice.transmat->m[0][2] *= x;
	vdevice.transmat->m[0][3] *= x;

	vdevice.transmat->m[1][0] *= y;
	vdevice.transmat->m[1][1] *= y;
	vdevice.transmat->m[1][2] *= y;
	vdevice.transmat->m[1][3] *= y;

	vdevice.transmat->m[2][0] *= z;
	vdevice.transmat->m[2][1] *= z;
	vdevice.transmat->m[2][2] *= z;
	vdevice.transmat->m[2][3] *= z;
}

/* this code is licensed as public domain */
#include "draw.h"
/******************************************************************************/
#ident "@(#)M_DRAW:move - Move the logical graphics position to the world coordinates x, y, z."
void draw_move(float x, float y, float z) {
        Token   *p;

        if (!vdevice.initialized)
                draw_verror("move: draw not initialized");


        vdevice.cpW[V_X] = x;
        vdevice.cpW[V_Y] = y;
        vdevice.cpW[V_Z] = z;

        vdevice.cpVvalid = 0;

        if (vdevice.inpolygon) {
                (*vdevice.pmove)(x, y, z);
                return;
        }

        if (vdevice.inobject) {
                p = draw_newtokens(4);

                p[0].i = OBJ_MOVE;
                p[1].f = x;
                p[2].f = y;
                p[3].f = z;

                return;
        }

        if (vdevice.clipoff) {          /* update device coords as well */
                draw_multvector(vdevice.cpWtrans, vdevice.cpW, vdevice.transmat->m);
                vdevice.cpVx = draw_WtoVx(vdevice.cpWtrans);
                vdevice.cpVy = draw_WtoVy(vdevice.cpWtrans);
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:move2 - Move the logical graphics position to the world coords x, y, 0.0"
/*
 * (I.e. a 2D move is defined as a 3D move with the Z-coord set to zero)
 */
void draw_move2(float x, float y) {
        if (!vdevice.initialized)
                draw_verror("move2: draw not initialized");

        draw_move(x, y, 0.0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:rmove - move the logical graphics position from the current world coordinates by dx, dy, dz"
void draw_rmove(float dx, float dy, float dz) {
   if (!vdevice.initialized)
      draw_verror("rmove: draw not initialized");

   draw_move((vdevice.cpW[V_X] + dx),
        (vdevice.cpW[V_Y] + dy),
        (vdevice.cpW[V_Z] + dz));
}
/******************************************************************************/
#ident "@(#)M_DRAW:rmove2 - Move Relative in 2D."
void draw_rmove2(float dx, float dy) {
        if (!vdevice.initialized)
                draw_verror("rmove2: draw not initialized");

        draw_move((vdevice.cpW[V_X] + dx), (vdevice.cpW[V_Y] + dy), 0.0);
}
/******************************************************************************/
#ident "@(#)M_DRAW:smove2 - Move directly as a fraction of the screen size."
void draw_smove2(float xs, float ys) {
        if (!vdevice.initialized)
                draw_verror("smove2: draw not initialized");

        vdevice.cpVx = (xs / 2 + 0.5) * (vdevice.sizeX);
        vdevice.cpVy = (0.5 + ys / 2) * (vdevice.sizeY);
}
/******************************************************************************/
#ident "@(#)M_DRAW:rsmove2 - Relative move as a fraction of the screen size."
void draw_rsmove2(float dxs, float dys) {
        if (!vdevice.initialized)
                draw_verror("rsmove2: draw not initialized");

        vdevice.cpVx += dxs / 2 * (vdevice.sizeX);
        vdevice.cpVy += dys / 2 * (vdevice.sizeY);
}
/******************************************************************************/

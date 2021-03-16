/* this code is licensed as public domain */
#include "draw.h"
#include <stdio.h>

static	int	px = -1, py = -1, pxs = -1, pys = -1;
/******************************************************************************/
#ident "@(#)M_DRAW:prefposition - Specify a preferred position for a window that is under control of a window manager."
/*
 *	Position is the location of the upper left corner.
 *	Should be called before vinit.
 */
void draw_prefposition(int x, int y){
	if (x < -1 ){
		fprintf(stderr,"prefposition: bad x value %d\n",x);
                px=py=-1;
        }
	if (y < -1 ){
		fprintf(stderr,"prefposition: bad y value %d\n",y);
                px=py=-1;
        }
	px = x;
	py = y;
}
/******************************************************************************/
#ident "@(#)M_DRAW:prefsize - Specify the preferred size for a window under control of a window manager."
/*
 * Should be called before vinit.
 */
void draw_prefsize(int x, int y){
	if (x < -1 || x == 0){
		fprintf(stderr,"prefsize: bad x value %d\n",x);
                px=py=-1;
        }
	if (y < -1 || y == 0){
		fprintf(stderr,"prefsize: bad y value %d\n",y);
                px=py=-1;
        }
	pxs = x;
	pys = y;
}
/******************************************************************************/
#ident "@(#)M_DRAW:getprefposandsize - Returns the preferred position and size of a window under control of a window manager."
/*
 * (-1 for unset parameters)
 */
void draw_getprefposandsize(int *x, int *y, int *xs, int *ys){
	*x = px;
	*y = py;
	*xs = pxs;
	*ys = pys;
}
/******************************************************************************/

/* this code is licensed as public domain */
#include <stdlib.h>
#include <stdio.h>
#include "draw.h"
/******************************************************************************/
#ident "@(#)M_DRAW:verror - print an error on the graphics device, and then exit."
/*
 * Only called for fatal errors. We assume that stderr is always there.
 */
void draw_verror(char *str){
#ifdef MSWIN
	mswin_verror(str);
#else
#ifdef OS2
        PM_verror(str);
#else
	fprintf(stderr, "%s\n", str);
#endif
#endif
	if (vdevice.initialized){
		draw_vexit();
	}
        abort();
}
/******************************************************************************/

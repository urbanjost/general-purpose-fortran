/* this code is licensed as public domain */
#include <stdlib.h>
#include <stdio.h>
#include "draw.h"
/******************************************************************************/
#ident "@(#)M_DRAW:backbuffer - swap drawing to backbuffer - returns -1 if no backbuffer is available."
int draw_backbuffer(void) {
        vdevice.attr->a.inbackbuffer = 1;
        vdevice.sync = 0;
        return((*vdevice.dev.Vbackb)());
}
/******************************************************************************/
#ident "@(#)M_DRAW:frontbuffer - start drawing in the front buffer again. This will always work!"
void draw_frontbuffer(void) {
        (*vdevice.dev.Vfrontb)();
        vdevice.attr->a.inbackbuffer = 0;
        vdevice.sync = 1;
}
/******************************************************************************/
#ident "@(#)M_DRAW:swapbuffers - swap the back and front buffers - returns -1 if no backbuffer is available."
int draw_swapbuffers(void) {
        if (vdevice.attr->a.inbackbuffer != 1){
           fprintf(stderr,"swapbuffers: double buffering not initialized.\n");
           return(-1);
        }
        return((*vdevice.dev.Vswapb)());
}
/******************************************************************************/

/* this code is licensed as public domain */
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "draw.h"
/******************************************************************************/
#ident "@(#)M_DRAW:vallocate - malloc with error checking"
/*
 * Though declared as "void", this should return a pointer
 */
void *draw_vallocate(size_t num_bytes,char* from){
        void *temp;
        char buf[60];

	/*
	fprintf(stderr,"VALLOCATE %d %s\n",num_bytes,from);
	*/

        temp = malloc( num_bytes );
        if( temp == NULL ) {
                perror( "malloc: " );
                sprintf(buf,"*vallocate*: request for %ld bytes returned NULL", (long int)num_bytes);
                draw_verror(buf);
                exit( -1 );
        }
        return temp;
}
/******************************************************************************/
#ident "@(#)M_DRAW:realloc with error checking"
void draw_vfree1(void *APRT,char* from ){ 
        char buf[80];
	int *newpointer=0;
	/* MINGW exits if newpointer == 0 */
        if( ((newpointer=realloc(APRT,(size_t)0)) == NULL) && (*newpointer != 0)) {
                perror( "realloc: " );
                sprintf(buf,"*vfree*: request for freeing memory via realloc failed from %s",from);
                draw_verror(buf);
                exit( -1 );
        }
	free(newpointer);
}
/******************************************************************************/
#ident "@(#)M_DRAW:vfree - free with error checking"
void draw_vfree(void *APRT,char* from ){ 
        char buf[80];
        free(APRT);
}
/******************************************************************************/

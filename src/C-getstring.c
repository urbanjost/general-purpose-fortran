/* this code is licensed as public domain */
#include "draw.h"
#include <stdio.h>

#define	EOT	4		/* Our EOF char */
#define	BS	8		/* A backspace char */
#define	DEL	127		/* Another backspace char (or erase) */
#define	CR	13		/* End of line */
/******************************************************************************/
#ident "@(#)M_DRAW:getstring - Get a character string from the keyboard in graphics mode."
/*
 *	Echoes in the current color/font/transformation.
 *	returns s as parameter and the number of chars read as
 *	function value (-1 on EOF).
 *
 *	Draws in the current color, erases in the color bcol.
 *
 *	NB. no overflow checking is done on the string s.
 */
int draw_getstring (int bcol, char *s) {
   int i, nc, pos, col;
   float cw, ch;

   s[0] = '\0';
   pos = -1;
   nc = draw_numchars () + 32;

   col = vdevice.attr->a.color;

   while ((i = draw_getkey ()) != CR) {
      /*
      fprintf(stderr,"CHAR=%d\n",i);
      */
      switch (i) {
      case -1:
	 s[++pos] = '\0';
	 return (pos);
      case EOT:
	 s[++pos] = '\0';
	 return (pos);

      case BS:
      case DEL:
	 if (pos >= 0) {
	    /*
	     * Over draw prev char in color bcol
	     */
	    draw_getcharsize (s[pos], &cw, &ch);
	    draw_rmove (-cw, 0.0, 0.0);
	    draw_color (bcol);
	    draw_drawchar (s[pos]);
	    draw_rmove (-cw, 0.0, 0.0);
	    draw_color (col);
	    s[pos--] = '\0';
	 }
	 break;

      default:
	 /*
	  * A new char... draw it in color col.
	  */
	 if (i > nc || i < 32){
	    continue;
	 }

	 s[++pos] = (char) i;
	 draw_drawchar (s[pos]);
      }
   }

   s[++pos] = '\0';

   return (pos);
}

/******************************************************************************/

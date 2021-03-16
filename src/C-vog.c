/* this code is licensed as public domain */

#ident "@(#)M_DRAW:driver/vog.c - M_DRAW driver for writing M_DRAW calls as text commands for the ush(1) shell"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Mar 2004"
/* 
 ===============================================================================
 Write back out as low-level draw calls
 Use negative color values to specify line thickness in raster units.
 ===============================================================================
*/

#include <stdio.h>
#include <string.h>
#include <time.h>
#ifndef MINGW
#include <pwd.h>
#include <sys/utsname.h>
#endif

#include <unistd.h>
#include <sys/types.h>
#include "draw.h"

extern FILE     *_draw_outfile();

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))

/* total drawing area size in x direction */
#define VOGXSIZE 8500  
/* total drawing area size in y direction */
#define VOGYSIZE 11000 

static int      vog_first_time = 1;
static int      VOGlastx = -1, VOGlasty = -1;/* last (x, y) drawn */

extern  FILE     *draw_fp;
/******************************************************************************/
static int      pgroup=1; /* groupid reserved for the entire page */
/******************************************************************************/
static int VOG_header(void) {

   time_t tod;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;
   time(&tod);
   fprintf(draw_fp,"#M_DRAW VOG Driver. Data Creation Date: %s",ctime(&tod));

#ifndef MINGW
   un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
   uname(un);

   if ((username = getlogin()) == NULL ){
      pw = getpwuid(getuid());
      username = pw->pw_name;
   }
   fprintf(draw_fp,"#  For: %s on OS=%.*s\n#       NETWORK_NAME=%.*s\n#       RELEASE=%.*s\n#       VERSION=%.*s\n#       MACHINE=%.*s\n",
       username,
       (int)sizeof(un->sysname),  un->sysname,
       (int)sizeof(un->nodename), un->nodename,
       (int)sizeof(un->release),  un->release,
       (int)sizeof(un->version),  un->version,
       (int)sizeof(un->machine),  un->machine);
#endif

   return(0);
}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */
static int VOG_mapcolor(int i, int r, int g, int b) {
   fprintf(draw_fp,"mapcolor %d %d %d %d\n",i,r,g,b);
   return(0);
}
/******************************************************************************/
/* VOG_init set up the environment. Returns 1 on success. */
static int VOG_init(void) {
   int prefx, prefy, prefxs, prefys;
   int VOG_header();
   draw_fp = _draw_outfile();
if (!vog_first_time) return(1); VOG_header();

   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

   if (prefxs != -1 ) {
      vdevice.sizeSy = prefys;
      vdevice.sizeSx = prefxs;
      vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
   } else{
      vdevice.sizeSy = VOGYSIZE;  /* size in resolution rasters */
      vdevice.sizeSx = VOGXSIZE;  /* size in resolution rasters */
      vdevice.sizeX = vdevice.sizeY = MIN(VOGXSIZE,VOGYSIZE); /* current viewport to use */
   }


   fprintf(draw_fp,"#vinit vog\n");
   fprintf(draw_fp,"page 0 %d 0 %d\n",vdevice.sizeSx,vdevice.sizeSy);

   return (1);
}
/******************************************************************************/
/* VOG_exit do a flush and close the output file if necessary.  */
static int VOG_exit(void) {
   fprintf(draw_fp, "vflush\n"); 
   fprintf(draw_fp, "page 0\n"); 
   fprintf(draw_fp,"#vexit\n"); /* End of Document */

   if (draw_fp != stdout && draw_fp != stderr ){
                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
   }
   return (0);
}
/******************************************************************************/
/* VOG_draw draw to an x, y point.  */
static int VOG_draw(int x, int y) {
   if (VOGlastx != vdevice.cpVx || VOGlasty != vdevice.cpVy ){
      fprintf(draw_fp, "move2 %d %d;", vdevice.cpVx, vdevice.cpVy);
   }
   fprintf(draw_fp, " draw2 %d %d\n", x ,y);
   VOGlastx=x;
   VOGlasty=y;
   return (0);
}
/******************************************************************************/
/* VOG_clear flush the current page without resetting the graphics state */
static int VOG_clear(void) {
     pgroup++; /* increment page id */
     fprintf(draw_fp,"clear\n#Page %d\n", pgroup);
   return(0);
}
/******************************************************************************/
/* VOG_color change the color of the pen
 *      kludged so negative value sets raster line width
 */
static int VOG_color(int col) {
   if ( col < 0 ) {
    fprintf(draw_fp,"linewidth %d\n",ABS(col));
   } else {
     fprintf(draw_fp,"color %d\n",col);
   }
   return(0);
}
/******************************************************************************/
/* 
 *      value sets raster line width
 */
static int VOG_setlw(int width) {
   if ( width > 0 ) {
    /*fprintf(draw_fp,"linewidth %d\n",width*vdevice.sizeX/10000); */
    fprintf(draw_fp,"linewidth %d\n",width);
   }
   return(0);
}
/******************************************************************************/
/* VOG_font load in small or large - could be improved.  */
static int VOG_font(char *font) {
   

   if (strcmp(font, "small") == 0) {
      vdevice.hwidth = 12.0/72.0/.03937;
      vdevice.hheight = vdevice.hwidth * 2.2;
   } else if (strcmp(font, "large") == 0) {
      vdevice.hheight = 55.0/72.0/.03937;
      vdevice.hwidth = vdevice.hheight / 4.5;
   } else
      return(0);

   fprintf(draw_fp, "textsize %f %f;",vdevice.hwidth,vdevice.hheight);
   fprintf(draw_fp, "font %s \n",font);
   return(1);
}
/******************************************************************************/
/* VOG_string output a string.  */
static int VOG_string(char *s) {

    if (VOGlastx != vdevice.cpVx || VOGlasty != vdevice.cpVy){
       fprintf(draw_fp, "move2 %d %d;", vdevice.cpVx, vdevice.cpVy);
    }
   fprintf(draw_fp, "textsize %f %f;",vdevice.hwidth,vdevice.hheight);
   fprintf(draw_fp, "drawstr %s \n",s);
   VOGlastx = VOGlasty = -1;

   return(0);
}
/******************************************************************************/
/* VOG_char output a character */
static int VOG_char(char c) {
         if (VOGlastx != vdevice.cpVx || VOGlasty != vdevice.cpVy){
            fprintf(draw_fp, "move2 %d %d;", vdevice.cpVx, vdevice.cpVy);
         }
         fprintf(draw_fp, "drawchar %c",c);
         VOGlastx = VOGlasty = -1;
   return(0);
}
/******************************************************************************/
/* fill a polygon */
static int VOG_fill(int n, int x[], int y[]) {
   int     i;
   static char linefeed[2] = {';','\n'};
   fprintf(draw_fp, "polyfill 1;move2 %d %d;makepoly;\n", x[0],y[0]); /* start of polygon */
   for (i = 1; i < n; i++)
   {
      fprintf(draw_fp, "draw2 %d %d%c", x[i], y[i],linefeed[(i % 8/7)]);
   }
   fprintf(draw_fp, "closepoly;polyfill 0\n"); /* end of polygon */
   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];
   VOGlastx = VOGlasty = -1;           /* fill destroys current path */
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry vogdev = {
   "vog",        /* name of device */
   "large",      /* name of large font */
   "small",      /* name of small font */
   noop,         /* Set drawing in back buffer */
   VOG_char,     /* Draw a hardware character */
   noop,         /* Check if a key was hit */
   VOG_clear,    /* Clear the screen to current color */
   VOG_color,    /* Set current color */
   VOG_draw,     /* Draw a line */
   VOG_exit,     /* Exit graphics */
   VOG_fill,     /* Fill a polygon */
   VOG_font,     /* Set hardware font */
   noop,         /* Set drawing in front buffer */
   noop,         /* Wait for and get the next key hit */
   VOG_init,     /* Initialize the device */
   noop2,        /* Get mouse/cross hair position */
   VOG_mapcolor, /* Set color indices */
   VOG_setlw,    /* Set line width */
   VOG_string,   /* Draw a hardware string */
   noop,         /* Swap front and back buffers */
   noop          /* Syncronize the display */
};
/******************************************************************************/
/* copy the vog device into vdevice.dev.  */
int _VOG_draw_devcpy(void) {
   vdevice.dev = vogdev;
   return(0);
}
/******************************************************************************/

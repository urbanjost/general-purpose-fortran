/* this code is licensed as public domain */

#ident "@(#)M_DRAW:driver/vml.c - M_DRAW driver for VML (Microsoft Vector Markup Language)"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Mar 1999"
/* 
 ===============================================================================
 Low level interface to VML
 Based on http://www.w3.org/TR/NOTE-VML
          http://www.microsoft.com/standards/vml/default.asp

 Microsoft says this format will be supported in Internet Explorer 5.0 
 and Microsoft Office 2000 products including Word.

 Quite a bit of the VML specification needs clarification. Testing against
 IE5.

 Recollecting  connected line segments into polylines because VML is too
 verbose to do otherwise.

 Use negative color values to specify line thickness in raster units.
 ===============================================================================
 NEED TO DO YET:

 This driver  subgroups  according to pen attribute  change inside of an
 entire  page group.  That means the entire page comes in as one grouped
 object  into your  document,  but that if you  ungroup it you will find
 most things arranged into reasonable  subobjects.  If you wish to force
 the  beginning  of a new VML  subobject  just  change  the pen color or
 thickness.  All polygons are  considered  objects, by the way.  Even if
 this does not make sense when you read it it is very important  because
 otherwise  your  drawings will crawl away like a bunch of ants when you
 try to manipulate the drawing or objects in it.

 Need  decent  support of hardware  fonts  (size,  angle,  font ...).

 Should  support  center, left, right and top, bottom middle  justify
 flags for hardware text too.

 Can line weight be specified as scaled to local coordinate system size?
 This would allow thickness to scale with a rescaled plot.

 See if can reduce file size. VML documentation is not very clear.

 A line of zero length does not print as a point; need to make into a point
or make sure zero-length vectors are changed to have length
 ===============================================================================
https://msdn.microsoft.com/en-us/library/hh801223(v=vs.85).aspx

VML is no longer supported

Impact 

•Applies to Internet Explorer 10 and later.
•Affects IE10 Standards mode and later, including interoperable quirks mode.

 Vector Markup Language (VML) is obsolete in Internet Explorer 10
 (quirks and 10 document modes).

 For Internet Explorer 10, VML will not work (is obsolete) in the
 following document modes:

• Interoperable quirks mode 
 •IE10 standards mode

   The common uses of VML now have standards-based alternatives implemented
   in Internet Explorer 10 and current versions of other browsers. This
   legacy Windows Internet Explorer features remains available in Internet
   Explorer 10 in document modes 5, 7, 8, and 9 though their performance is
   inferior to their hardware-accelerated, standards-based replacements. We
   encourage web developers to move their sites to standards-based
   technologies rather than relying on legacy document modes.
   
   SVG, implemented in Windows Internet Explorer 9, provides the
   functionality needed to replace VML in websites and applications that use
   it. The VML to SVG Migration Guide, published on the Microsoft Download
   Center, provides guidance for moving from VML to SVG.
   
   Additionally, the Raphaël JavaScript Library is a lightweight and
   easy-to-use graphics API that uses SVG when available and VML when
   not. Raphaël is a good choice for building vector graphics solutions
   that work in Windows Internet Explorer 8 and other newer browsers.

 ===============================================================================
*/
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#ifndef MINGW
#include <pwd.h>
#include <sys/utsname.h>
#endif

#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include "draw.h"

extern FILE     *_draw_outfile();

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))
#define FLIPY(y)        ((vdevice.sizeSy)-(y))

#define FALSE 0
#define TRUE  1

#define VMLXSIZE 450 /* total drawing area size in x direction */
#define VMLYSIZE 650 /* total drawing area size in y direction */
/* scale factor for going from world coordinates to device coordinates. Make bigger to increase accuracy of vector data */
#define VMLTORAS 20 

static int      points=0;
static int      vml_first_time = 1, drawn = 0, pslstx = -1, pslsty = -1;/* last (x, y) drawn */
extern  FILE     *draw_fp;

static int OLDX;
static int OLDY;
static int VML_MOVED=0;

#define CMAPSIZE 256
struct rgb_color {
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
};
static struct rgb_color vml_carr[CMAPSIZE];

/******************************************************************************/
static int      lineopen = FALSE; /* PolyLine not open */
static int      shapeopen = FALSE; /* shape not open */
static int      curcol = 0; /* Current pen color (black) */
static int      curwid = 1; /* Current pen width */
static int      curpat  = 0; /* Current fill pattern*/
static int      pgroup=1; /* groupid reserved for the entire page */
extern float hardwidth[128]; /* array to store hardware character widths */
static char fontstyle[256];
/******************************************************************************/
static int VML_header() {

   time_t tod;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;
   fprintf(draw_fp,"<html xmlns:v=\"urn:schemas-microsoft-com:vml\"\n");
   fprintf(draw_fp,"      xmlns:o=\"urn:schemas-microsoft-com:office:office\">\n");
   fprintf(draw_fp,"<!--\n");
   time(&tod);
   fprintf(draw_fp,"  Data Creation Date: %s",ctime(&tod));
/*----------------------------------------------------------------------------*/
#ifndef MINGW
   un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
   uname(un);

   if ((username = getlogin()) == NULL ){
      pw = getpwuid(getuid());
      username = pw->pw_name;
   }
   fprintf(draw_fp,"  For: %s on OS=%.*s\n       NETWORK_NAME=%.*s\n       RELEASE=%.*s\n       VERSION=%.*s\n       MACHINE=%.*s\n",
       username,
       (int)sizeof(un->sysname),  un->sysname,
       (int)sizeof(un->nodename), un->nodename,
       (int)sizeof(un->release),  un->release,
       (int)sizeof(un->version),  un->version,
       (int)sizeof(un->machine),  un->machine);
#endif
/*----------------------------------------------------------------------------*/
   fprintf(draw_fp,"-->\n");

   fprintf(draw_fp,"<head>\n");

      fprintf(draw_fp,"<style>\n");
      fprintf(draw_fp," v\\:* { behavior:url(#default#VML); }\n");
      fprintf(draw_fp," o\\:* { behavior:url(#default#VML); }\n");
      fprintf(draw_fp,"</style>\n");

      fprintf(draw_fp,"<title>VML graphics</title>\n");

      fprintf(draw_fp,"<meta name=\"Author\" content=\"John S. Urban\">\n");
      fprintf(draw_fp,"<meta name=\"Description\" content=\"@(#)M_DRAW VML Driver Version 1.0.1, March  1999\">\n");

   fprintf(draw_fp,"</head>\n");

   fprintf(draw_fp,"<body topmargin=\"10\" leftmargin=\"10\" bgcolor=\"#FFFFFF\" link=\"#000066\" vlink=\"#666666\" text=\"#000000\">\n");
   fprintf(draw_fp,"<font face=\"VERDANA,ARIAL,HELVETICA\" size=\"2\">\n");
   fprintf(draw_fp,"<p>&nbsp;</p>\n");

   return(0);
}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */
static int VML_mapcolor(int i, int r, int g, int b) {
   if (i >= CMAPSIZE || i < 0 ){
      return(-1);
   }
   vml_carr[i].red = (unsigned short)(r);
   vml_carr[i].green = (unsigned short)(g);
   vml_carr[i].blue = (unsigned short)(b);
   return(0);
}
/******************************************************************************/
/* VML_init set up the environment. Returns 1 on success. */
static int VML_init(void) {
   int prefx, prefy, prefxs, prefys;
   int i;
   int VML_header();
   draw_fp = _draw_outfile();

   if (!vml_first_time) return(1);

   VML_header();

   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

   if (prefxs != -1 ) {
      vdevice.sizeSy = prefys*VMLTORAS;
      vdevice.sizeSx = prefxs*VMLTORAS;
      vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs )*VMLTORAS;
   } else{
      vdevice.sizeSy = VMLYSIZE*VMLTORAS;  /* size in resolution rasters */
      vdevice.sizeSx = VMLXSIZE*VMLTORAS;  /* size in resolution rasters */
      vdevice.sizeX = vdevice.sizeY = MIN(VMLXSIZE,VMLYSIZE)*VMLTORAS; /* current viewport to use */
   }

   fprintf(draw_fp,"<a name=\"Page%d\"></a>\n", pgroup);
   fprintf(draw_fp,"<v:group id=\"Page%d\" style='mso-position-horizontal-relative:char;", pgroup);
   fprintf(draw_fp," mso-position-vertical-relative:line;");
   fprintf(draw_fp," left:0pt;top:0pt;width:%dpt;height:%dpt'\n",vdevice.sizeSx/VMLTORAS,vdevice.sizeSy/VMLTORAS);

   fprintf(draw_fp," url=\"#Page%d\"", pgroup);
   fprintf(draw_fp," coordsize=\"%d,%d\" >\n",vdevice.sizeSx,vdevice.sizeSy);

   vdevice.depth = 8;
   for (i = 0; i < CMAPSIZE; i++) /* set up the basic colors */
   {
      vml_carr[i].red=255;
      vml_carr[i].green=255;
      vml_carr[i].blue=255;
   }

   VML_mapcolor(0, 255, 255, 255);
   VML_mapcolor(1, 255, 0, 0);
   VML_mapcolor(2, 0, 255, 0);
   VML_mapcolor(3, 255, 255, 0);
   VML_mapcolor(4, 0, 0, 255);
   VML_mapcolor(5, 255, 0, 255);
   VML_mapcolor(6, 0, 255, 255);
   VML_mapcolor(7, 0, 0, 0);

   VML_mapcolor( 8, 155, 0, 0);
   VML_mapcolor( 9, 0, 155, 0);
   VML_mapcolor(10, 155, 255, 255);
   VML_mapcolor(11, 155, 155, 0);
   VML_mapcolor(12, 0, 0, 155);
   VML_mapcolor(13, 155, 0, 155);
   VML_mapcolor(14, 0, 155, 155);
   VML_mapcolor(15, 100, 100, 100);
   lineopen = FALSE; /* Polyline not open */
   shapeopen = FALSE; /* Polyline not open */
   curcol=0;
   curwid=1;
   curpat=0;
   drawn = 0;
   return (1);
   /*      Set other line drawing parameters */
   /*      Move                              */
   /*      Set a default font height         */
}
/******************************************************************************/
static int closeline(void){
   if(lineopen){
      if(VML_MOVED == 0 ){
         /* NULL LINES ARE NOT POINTS, BUT NO-OPS TO 2002+ WINDOWS */
         /* fprintf(draw_fp," <!-- NULL LINE-->"); */
         /* fprintf(draw_fp," %d %d ",OLDX+1,OLDY+1); */

         /* draw small square to make a point */
         /* TOO BIG, SHOWS UP IN SOFTWARE TEXT TOO (MAYBE JUST MAKE SMALLER?
         fprintf(draw_fp," t %d %d r %d %d %d %d %d %d %d %d",-VMLTORAS/2,-VMLTORAS/2,
                 0,VMLTORAS,
                 VMLTORAS,0,
                 0,-VMLTORAS,
                 -VMLTORAS,0);
         */

         fprintf(draw_fp," t -1 -1 r 1 1 ");
      }
      fprintf(draw_fp, " e"); /* end curve */
      lineopen = FALSE; /* Polyline not open */
      points = 0;
   }
   return (0);
}
/******************************************************************************/
static int closeshape(void){
   if(shapeopen){
      fprintf(draw_fp, "\"><v:stroke joinstyle=\"round\" endcap=\"round\"/>\n"); /* end curve */
      fprintf(draw_fp, "</v:shape>\n"); /* end curve */
      shapeopen = FALSE; /* Polyline not open */
      points = 0;
   }
   return (0);
}
/******************************************************************************/
static int openline(void){
   if(!lineopen){
      fprintf(draw_fp,"\nm");
      lineopen = TRUE; /* Polyline open */
   }
   return (0);
}
/******************************************************************************/
static int openshape(void){
   if(!shapeopen){
      fprintf(draw_fp,
          "<v:shape ");
      fprintf(draw_fp,
          " style='position:absolute;top:0;left:0;width:%d;height:%d'\n", vdevice.sizeSx,vdevice.sizeSy);
      fprintf(draw_fp,"filled=\"false\" stroke=\"true\"");
      fprintf(draw_fp,
          " strokeweight=\"%dpt\" strokecolor=\"rgb(%d,%d,%d)\"\npath=\"nf",
          curwid*vdevice.sizeX/10000/VMLTORAS,
          vml_carr[curcol].red, vml_carr[curcol].green, vml_carr[curcol].blue);

      shapeopen = TRUE; /* shape open */
   }
   return (0);
}
/******************************************************************************/
/* VML_exit do a flush and close the output file if necessary.  */
static int VML_exit(void) {
   closeline(); /* close Polyline line if it open */
   closeshape(); /* close shape line if it open */
   fprintf(draw_fp, "</v:group>\n"); /* Page Clear, End of Page Group */
   fprintf(draw_fp,"</font>\n</body>\n</html>\n"); /* End of Document */
   drawn = 0;
   points = 0;

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
/* VML_draw draw to an x, y point.  */
/* Note: (0, 0) is defined as the top left of the window in VML.  */
static int VML_draw(int x, int y) {
   static char linefeed[2] = {' ','\n'};

   if (pslstx != vdevice.cpVx || pslsty != vdevice.cpVy ){
      closeline(); /* close line if required */
      openshape(); /* start shape if required */
      openline(); /* start line */
      fprintf(draw_fp, " %d %d l", vdevice.cpVx, FLIPY(vdevice.cpVy));

      OLDX=vdevice.cpVx;
      OLDY=FLIPY(vdevice.cpVy);
      VML_MOVED=0;

      points = 1;
   }
   openshape(); /* start shape if required */
   openline(); /* start line if required */
   if(points == 0){
      fprintf(draw_fp, " %d %d l", x ,FLIPY(y));

      OLDX=x;
      OLDY=FLIPY(y);
      VML_MOVED=0;

   }else{

      if(OLDX!=x || OLDY!=FLIPY(y))VML_MOVED++;
      OLDX=x;
      OLDY=FLIPY(y);

      fprintf(draw_fp, "%c%d %d", linefeed[(points % 8/7)], x ,FLIPY(y));
   }
   points++;
   pslstx = x;
   pslsty = y;
   drawn = 1;
   return (0);
}
/******************************************************************************/
/* VML_clear flush the current page without resetting the graphics state */
static int VML_clear(void) {
   closeline(); /* close line if required */
   closeshape(); /* close shape if required */
   if (drawn)
   {

     pgroup++; /* increment page id */
     fprintf(draw_fp, "</v:group>\n"); /* Page Clear, End of Page Group */
     fprintf(draw_fp,"<a name=\"Page%d\"></a>\n", pgroup);
     fprintf(draw_fp,"<br/>\n");
     fprintf(draw_fp,"<v:group id=\"Page%d\" style='mso-position-horizontal-relative:char;", pgroup);
     fprintf(draw_fp,"mso-position-vertical-relative:line;");
     fprintf(draw_fp,"left:0pt;top:0pt;width:%dpt;height:%dpt'\n",vdevice.sizeSx/VMLTORAS,vdevice.sizeSy/VMLTORAS);
     fprintf(draw_fp," url=\"#Page%d\"", pgroup);
     fprintf(draw_fp," coordsize=\"%d,%d\" >\n",vdevice.sizeSx,vdevice.sizeSy);
   }
   drawn = 0;
   points = 0;
   return(0);
}
/******************************************************************************/
/* VML_color change the color of the pen
 *      kludged so negative value sets raster line width
 *      if exceed allowable number of colors maybe pick a line style
 *      or something like a gradient fill style for fun
 */
static int VML_color(int col) {
   closeline(); /* close line if required */
   closeshape(); /* close shape if required */
   if ( col < 0 )
   {
      curwid = abs(col);
   } else {
      curpat = col/CMAPSIZE;
      curcol = col % CMAPSIZE;
   }
   return(0);
}
/******************************************************************************/
/* 
 *      value sets raster linewidth
 */
static int VML_setlw(int width) {
   closeline(); /* close line if required */
   closeshape(); /* close shape if required */
   if ( width >= 0 ) {
      curwid = width;
   }
   return(0);
}
/******************************************************************************/
/* load in small or large - could be improved. Radically KLUDGED; made SoftText extern  */
static int VML_font(char *fontname) {
   int i;
   float rat;
   /* assuming vdevice.hwidth is the desired width of the reference character,
    * this is a list of percentages of the other character widths.
    */
   static int helvetica_w[128] = {
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,
      27,33,43,55,55,88,72,28,33,33,39,58,27,33,28,28,55,55,55,55,55,55,55,55,55,55,
      34,34,58,58,58,61,97,
      73,73,73,73,66,61,78,72,28,55,73,61,83,72,78,66,78,72,67,61,72,66,94,66,66,61,
      33,28,33,58,55,28,
      55,61,55,61,55,33,61,61,28,28,55,28,88,61,61,61,61,39,55,33,61,55,78,55,55,50,
      39,28,39,58,0
   };

   vdevice.attr->a.softtext = SOFTHARDWARE;
   /* textsize will be obeyed after the font is set
    * maybe should read current software size and convert virtual
    * to device instead of resetting each time */

      if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
         vdevice.hwidth=11.0*VMLTORAS;
         vdevice.hheight=11.0*VMLTORAS;
      }

   if (strcmp(fontname, "small") == 0) {
      rat=0.55; /* Kludge Factor */

      for (i = 0; i < 128; i++){
         hardwidth[i]=1.00 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
         }
      strcpy(fontstyle,"Font-weight:bold; font-family:'Times New Roman'");
      strcpy(fontstyle,"font-family:'Times New Roman'");
   } else if (strcmp(fontname, "large") == 0) {
      rat=1.00; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=((float)helvetica_w[i])/100.0*rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr," font table  %f %c \n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
      }
      strcpy(fontstyle,"font-weight:bold; font-family:'Arial Black', Arial, sans-serif");
      strcpy(fontstyle,"font-family:'Arial Black', Arial, sans-serif");
   } else{
      strcpy(fontstyle,"font-weight:bold; font-family:'Arial Black', Arial, sans-serif");
      strcpy(fontstyle,"font-family:'Arial Black', Arial, sans-serif");
      return(0);
   }

   return(1);
}
/******************************************************************************/
/* output a character string using current character size and rotation angle. */
static int VML_string(char *s) {
   char    c;
   int     i;
   float   slen;
   float   sheight;
   int uneven;
   int x;
   int y;
   int ijust;

   closeline(); /* close line if required */
   closeshape(); /* close shape if required */

   if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
      fprintf(stderr,"*VML_string* ERROR: ZERO SIZE CHARACTERS\n");
      vdevice.hwidth=11.0;
      vdevice.hheight=11.0;
   }
   slen=draw_strlength(s)*vdevice.hwidth*VMLTORAS;
   sheight=vdevice.hheight;

   /* There is a bug where if the line is horizontal, the text prints with zero character height */
        uneven=0;
        if( FLIPY(vdevice.cpVy) == FLIPY(vdevice.cpVy+slen*vdevice.attr->a.textsin) ){
                uneven=1;
        }else{
                uneven=0;
        }
        /* VML aligns thru center of text */
        ijust=(int)(0.50*sheight);

        /* The line the text will be placed along */
        x=(int)(vdevice.cpVx+slen*vdevice.attr->a.textcos);
        y=(int)(uneven+vdevice.cpVy+slen*vdevice.attr->a.textsin);

fprintf(draw_fp,"<v:line from=\"%d,%d\" to=\"%d,%d\"\n", vdevice.cpVx, FLIPY(vdevice.cpVy+ijust), x, FLIPY(y+ijust));

      /* the coordinate reference */
      fprintf(draw_fp,
          " style='position:absolute;top:0;left:0;width:%d;height:%d'\n", vdevice.sizeSx,vdevice.sizeSy);

      fprintf(draw_fp,"filled=\"false\" stroke=\"true\"");

      /* 
      fprintf(draw_fp," strokeweight=\"%dpt\" ", curwid*vdevice.sizeX/10000/VMLTORAS,
       */

      /* character outline stroke color */
      fprintf(draw_fp," strokecolor=\"rgb(%d,%d,%d)\">\n",
          vml_carr[curcol].red, vml_carr[curcol].green, vml_carr[curcol].blue);

      /* character fill color */
fprintf(draw_fp,"<v:fill on=\"t\" color=\"rgb(%d,%d,%d)\"/>\n",
          vml_carr[curcol].red, vml_carr[curcol].green, vml_carr[curcol].blue);

fprintf(draw_fp,"<v:path textpathok=\"t\"></v:path>\n");

          /* string */
fprintf(draw_fp,"<v:textpath  on=\"t\" fitpath =\"t\" string=\"");
   for(i=0; (c=s[i]) != '\0' ;i++)
   {
      switch(c) {
      case '"':
         fprintf(draw_fp, "&quot;");
         break;
      case '<':
         fprintf(draw_fp, "&lt;");
         break;
      case '>':
         fprintf(draw_fp, "&gt;");
         break;
      default:
         fprintf(draw_fp, "%c",c);
      }

   }
fprintf(draw_fp,"\" style=\"%s;",fontstyle);

fprintf(draw_fp,"v-text-align:stretch-justify;fontsize:%d\"/>\n",(int)sheight);
fprintf(draw_fp,"</v:line>\n");

   drawn = 1;
   pslstx = x;
   pslsty = y;
   closeshape(); /* close shape if required */
   return(0);
}
/******************************************************************************/
/* VML_char output a character */
static int VML_char(char c){
   char  s[2];
   s[0] = c; 
   s[1]='\0';
   VML_string(s);
   return(0);
}
/******************************************************************************/
/* fill a polygon */
static int VML_fill(int n, int x[], int y[]) {
   int     i;
   static char linefeed[2] = {' ','\n'};
   closeline(); /* close line if required */
   closeshape(); /* close line if required */
   fprintf(draw_fp, "<v:shape style='position:absolute;top:0;left:0;width:%d;height:%d'\n",
        vdevice.sizeSx,vdevice.sizeSy);
   fprintf(draw_fp, " stroke=\"false\" filled=\"true\" fillcolor=\"rgb(%d,%d,%d)\" coordsize=\"%d,%d\"\npath=\"m %d %d l\n",
       vml_carr[curcol].red, vml_carr[curcol].green, vml_carr[curcol].blue, 
       vdevice.sizeSx,vdevice.sizeSy,
       (int)x[0],(int)FLIPY(y[0]));

   for (i = 1; i < n; i++) {
      fprintf(draw_fp, " %d %d%c", x[i], FLIPY(y[i]),linefeed[(i % 8/7)]);
   }

   fprintf(draw_fp, " x e\"/></v:shape>\n"); /* end of polygon */

   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];

   pslstx = pslsty = -1;           /* fill destroys current path */
   drawn = 1;
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry vmldev = {
   "vml",     /* name of device */
   "large",      /* name of large font */
   "small",      /* name of small font */
   noop,         /* Set drawing in back buffer */
   VML_char,     /* Draw a hardware character */
   noop,         /* Check if a key was hit */
   VML_clear,    /* Clear the screen to current color */
   VML_color,    /* Set current color */
   VML_draw,     /* Draw a line */
   VML_exit,     /* Exit graphics */
   VML_fill,     /* Fill a polygon */
   VML_font,     /* Set hardware font */
   noop,         /* Set drawing in front buffer */
   noop,         /* Wait for and get the next key hit */
   VML_init,     /* Initialize the device */
   noop2,        /* Get mouse/cross hair position */
   VML_mapcolor, /* Set color indices */
   VML_setlw,    /* Set line width */
   VML_string,   /* Draw a hardware string */
   noop,         /* Swap front and back buffers */
   noop          /* Syncronize the display */
};
/******************************************************************************/
/*  copy the vml device into vdevice.dev.  */
int _VML_draw_devcpy(void) {
   vdevice.dev = vmldev;
   return(0);
}
/******************************************************************************/

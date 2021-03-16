/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/mif.c - M_DRAW driver for FrameMaker Maker Interchange files"
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.0, Sep 1993"

/*

 If enhancements,  problems, or other news about this driver comes up, I
 would like to hear about it.  I CANNOT  guarantee a  response,  but for
 today, anyway:
                   osjsu@eureka.pgh.wec.com
                   urbanj@h01.pgh.wec.com
 ===============================================================================
Good news bears repeating.

 This  software  is  public  domain  and  may be  used  for  any  purpose
 commercial or otherwise.  It is offered  without any guarantee as to its
 suitability  for any purpose or as to the sanity of its  writers.  We do
 ask that the  source is passed on to anyone  that  requests  a copy, and
 that people who get copies don't go round claiming they wrote it.

 ===============================================================================
 Kludges:

 Had to use a kludge  so  FrameMaker  would  not  ignore  single-element
 groups so everything could easily be collected into the page.
 ===
 Recollecting  connected line segments into polylines because MIF is too
 verbose to do otherwise.
 ===
 MIF only  supports  colors 0 thru 7; and  patterns 0 thru 15.  Combined
 them so that  start  using  patterns  for  colors  based on  pattern  =
 (color/8)
 ===
 Breaking  polylines  into  groups  of  1500  or  less  points  so  that
 FrameMaker  does not  generate  a  polyline  in  PostScript  that  some
 printers will not have enough memory for.
 ===
 Added approximately a 12.5 mm (1/2 inch) border all around drawing, and
 assume a portrait 8.5x11 inch drawing surface.
 ===============================================================================
 Some of the nice things:

 This driver  subgroups  according to pen attribute  change inside of an
 entire page group.  Translated,  that means the entire page comes in as
 one  grouped  object  into your  FrameMaker  document,  but that if you
 ungroup  it  you  will  find  most  things   arranged  into  reasonable
 subobjects.  If you wish to force the beginning of a new MIF  subobject
 just change the pen color or  thickness.  All  polygons are  considered
 objects, by the way.  Even if this does not make sense when you read it
 it is very important  because  otherwise  your drawings will crawl away
 like a bunch of ants when you try to manipulate  the drawing or objects
 in it.

 Use negative color values to specify line thickness in raster units.
 ===============================================================================
 Thinks to think about next time I'm in here:

  o what to reset when do vinit?
  o what happens if more than 1 mif file appended together?
  o what  kind of  polygons  can MIF use?  Trial  and  Error on  convex,
    concave, complex ...
  o Add user control of border and size like in other drivers
  o Decent  support of fonts  (size,  angle, font ...).  Should  support
    center, left, right and top, bottom middle justify flags too.
  o Reduce file size by judicious reduction of number of digits and such
  o Rumor has it that FrameMaker 4.0 can support rgb color specification
    and hardware dash codes that work and editable vertical text.
 ===============================================================================
 Version 19950408: Added comment section to help identify origin of MIF file.
                   Added .2 precision qualifier to %f of points to reduce file
                   size (.01mm is assumed to be a reasonable accuracy).
                   Added Macro define statements to reduce file size.
 ===============================================================================
 Version 2.0 Mon Dec  9 23:29:37 CST 1996
 Added MIF_setlw now that draw supports line thickness ( previously I used a
  kludge where negative color index actually set line thickness);
 ===============================================================================
*/

#include <stdlib.h>
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

#define FALSE 0
#define TRUE  1
#define MIFXSIZE0 215.0  /* total area size in x direction */
#define MIFYSIZE0 279.0  /* total area size in y direction */
#define MIFXSIZE 190.0  /* total drawing area size in x direction, keep this a whole number */
#define MIFYSIZE 254.0  /* total drawing area size in y direction, keep this a whole number */
#define MIFTORAS 20.0   /* make this bigger to make drawing have a higher resolution */
/*
  FrameMaker  writes a PostScript file with as many points in a polyline
  as were in the  FrameMaker  polyline.  This  causes  problems  on most
  printers where there is a limit on the number of points in a polyline.
  The  PostScript  manual  from Adobe  suggests no more than 1500 points
  should be in a  polyline.  The driver  breaks  any very long  polyline
  into segments MAXPOINTS points long to avoid the FrameMaker bug.
*/
#define MAXPOINTS 1000   /* reduced from 1500 to 1000 to accommodate bug in Xerox 4700 printer */

static int      points=0;
static int      mif_first_time = 1, drawn = 0, pslstx = -1, pslsty = -1;/* last (x, y) drawn */
extern  FILE     *draw_fp;

static int MIFVERSION=0;
#define CMAPSIZE 256
struct rgb_color{
      unsigned short int red;
      unsigned short int green;
      unsigned short int blue;
};
static struct rgb_color MIF_carr[CMAPSIZE];

/* translation table between basic standard draw colors and mif standard colors */
static float cmap[8] = {
   1, /* White     */
   2, /* Red       */
   3, /* Green     */
   7, /* Yellow    */
   4, /* Blue      */
   6, /* Magenta   */
   5, /* Cyan      */
   0  /* Black     */
};
/******************************************************************************/
static int      polyopen = FALSE; /* PolyLine not open */
static int      curcol = 0; /* Current pen color (black) */
static int      curcol4 = 0; /* Current pen color (black) */
static float    curwid = 1.000; /* Current pen width */
static int      groupid  = 1; /* Current group id */
static int      curpat  = 0; /* Current fill pattern*/
static float    borderx =(MIFXSIZE0-MIFXSIZE)/2.0; /* all-around border width in millimeters */
static float    bordery =(MIFYSIZE0-MIFYSIZE)/2.0; /* all-around border width in millimeters */
static int      pgroup; /* groupid reserved for the entire page */
static int      members=0; /* how many entities are in this group */
static float    kludgex,kludgey;
/******************************************************************************/
static int MIF_header(void) {

   time_t tod;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;
        fprintf(draw_fp,"<Comment -\n");
        fprintf(draw_fp,"  Creator: M_DRAW MIF driver\n");
        fprintf(draw_fp,"  Title: Very Ordinary Graphics Learning Environment\n");
        fprintf(draw_fp,"  Author: John S. Urban\n");

        time(&tod);
        fprintf(draw_fp,"  CreationDate: %s",ctime(&tod));

#ifndef MINGW
        un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
        uname(un);

        if ((username = getlogin()) == NULL ){
        pw = getpwuid(getuid());
        username = pw->pw_name;
        }
        fprintf(draw_fp,"  For: %s on OS=%.*s NETWORK_NAME=%.*s RELEASE=%.*s VERSION=%.*s MACHINE=%.*s\n",username,
                (int)sizeof(un->sysname),  un->sysname,
                (int)sizeof(un->nodename), un->nodename,
                (int)sizeof(un->release),  un->release,
                (int)sizeof(un->version),  un->version,
                (int)sizeof(un->machine),  un->machine);
#endif

        fprintf(draw_fp,"> # End of <Comment> Statement\n");
        return(0);
}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value */
static int MIF_mapcolor(int i, int r, int g, int b){
    if(i >= CMAPSIZE || i < 0){
       return(-1);
    }
    MIF_carr[i].red=(unsigned short )r;
    MIF_carr[i].green=(unsigned short )g;
    MIF_carr[i].blue=(unsigned short )b;
    return(0);
}
/******************************************************************************/
/*  M_DRAW color is RGB 0-255; need CYMB, 0-100  */
static void printcolor(int thecolor){
/*
 * NOTE: ObColor is a MIF 4.0 feature, not MIF 3.0, and the color declarations
 *       should be in a <ColorCatalog Color ....> top-level statement and it
 *       is not clear what repeating a colortag should do and this is verbose
 *       but it works just fine in any version of FrameMaker I have had access
 *       to.  If FrameMaker would just let you do <ObColor #CCCMMMYYYBBB >
 *       I'd be happy.
*/
   int C, M, Y, B;

   if(MIFVERSION != 4)return;

   C=(255-MIF_carr[thecolor].red)*100/255;
   M=(255-MIF_carr[thecolor].green)*100/255;
   Y=(255-MIF_carr[thecolor].blue)*100/255;
   B=MIN(C,M);
   B=MIN(B,Y);
   C=C-B;
   M=M-B;
   Y=Y-B;
   fprintf(draw_fp,"<Color ");
   fprintf(draw_fp,"<ColorTag `%3.3d%3.3d%3.3d%3.3d'>",C,M,Y,B);
   fprintf(draw_fp,"<ColorCyan %d>",C);
   fprintf(draw_fp,"<ColorMagenta %d>",M);
   fprintf(draw_fp,"<ColorYellow %d>",Y);
   fprintf(draw_fp,"<ColorBlack %d>",B);
   fprintf(draw_fp,">");
   fprintf(draw_fp,"<ObColor `%3.3d%3.3d%3.3d%3.3d'>",C,M,Y,B);
}
/******************************************************************************/
/* MIF_init set up the environment. Returns 1 on success. */
static int MIF_init(void) {
   int i;
   int prefx, prefy, prefxs, prefys;
   int MIF_header(void);
   draw_fp = _draw_outfile();

   if (!mif_first_time)
      return(1);

   fprintf(draw_fp,"<MIFFile %d.0> # Generated by M_DRAW driver (John S. Urban, Sept. 1993)\n",MIFVERSION);
   fprintf(draw_fp,"<Document\n");
   fprintf(draw_fp,"<Units Umm>\n");
   fprintf(draw_fp,"<DPageSize %f %f >\n",MIFXSIZE0,MIFYSIZE0);
   fprintf(draw_fp,"># end of Document\n");

   MIF_header();
   fprintf(draw_fp,"# MACRO section to reduce file size \n");
   fprintf(draw_fp,"define(P,Point)\n");
   fprintf(draw_fp,"define(S,Separation)\n");
   fprintf(draw_fp,"define(SN,Smoothed No)\n");
   fprintf(draw_fp,"define(PW,PenWidth)\n");
   fprintf(draw_fp,"define(PL,PolyLine)\n");
   fprintf(draw_fp,"define(F,Fill)\n");
   fprintf(draw_fp,"define(GID,GroupID)\n");
   fprintf(draw_fp,"define(G,Group)\n");
   fprintf(draw_fp,"# END MACRO section to reduce file size \n");

   fprintf(draw_fp,"<Page \n");
   fprintf(draw_fp,"<PageSize %f %f >\n",MIFXSIZE0,MIFYSIZE0);

   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

   if (prefxs != -1 ) {
      vdevice.sizeSy = prefys;
      vdevice.sizeSx = prefxs;
      vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
      borderx = prefx;
      bordery = prefy;
   }else{
      vdevice.sizeSy = MIFYSIZE*MIFTORAS;  /* size in resolution rasters */
      vdevice.sizeSx = MIFXSIZE*MIFTORAS;  /* size in resolution rasters */
      vdevice.sizeX = vdevice.sizeY = MIFXSIZE*MIFTORAS; /* current viewport to use */
      borderx =(MIFXSIZE0-MIFXSIZE)/2.0; /* all-around border width in millimeters */
      bordery =(MIFYSIZE0-MIFYSIZE)/2.0; /* all-around border width in millimeters */
   }

   fprintf(draw_fp,"# ADDRESSABILITY IN RASTERS %d %d\n",vdevice.sizeSx,vdevice.sizeSy);

   for (i = 0; i < CMAPSIZE; i++){ /* set up the basic colors */
      MIF_carr[i].red=255;
      MIF_carr[i].green=255;
      MIF_carr[i].blue=255;
   }

   MIF_mapcolor(0, 255, 255, 255);
   MIF_mapcolor(1, 255, 0, 0);
   MIF_mapcolor(2, 0, 255, 0);
   MIF_mapcolor(3, 255, 255, 0);
   MIF_mapcolor(4, 0, 0, 255);
   MIF_mapcolor(5, 255, 0, 255);
   MIF_mapcolor(6, 0, 255, 255);
   MIF_mapcolor(7, 0, 0, 0);

   MIF_mapcolor( 8, 155, 0, 0);
   MIF_mapcolor( 9, 0, 155, 0);
   MIF_mapcolor(10, 155, 255, 255);
   MIF_mapcolor(11, 155, 155, 0);
   MIF_mapcolor(12, 0, 0, 155);
   MIF_mapcolor(13, 155, 0, 155);
   MIF_mapcolor(14, 0, 155, 155);
   MIF_mapcolor(15, 100, 100, 100);

   polyopen = FALSE; /* Polyline not open */
   curcol=0;
   curcol4=0;
   curwid=1.0;
   curpat=0;
   pgroup=groupid; /* page id */
   groupid++; /* start a new group for objects on this page*/
   drawn = 0;
   return (1);
   /*      Set other line drawing parameters */
   /*      Move                              */
   /*      Set a default font height         */
}
/******************************************************************************/
static int closeline(void){
   if(polyopen){
      fprintf(draw_fp, "> #end curve PL\n");
      polyopen = FALSE; /* Polyline not open */
      points = 0;
   }
   return (0);
}
/******************************************************************************/
static int openline(void){
   if(!polyopen){
      fprintf(draw_fp, "<PL\n");
      fprintf(draw_fp, " <GID %d >\n",groupid); /* PolyLine */
      members++;
      fprintf(draw_fp, " <SN>\n");
      fprintf(draw_fp, " <PW %f>\n",curwid*vdevice.sizeX/10000.0/MIFTORAS);
      fprintf(draw_fp, " <S %d >\n",curcol);
      printcolor(curcol4);
      fprintf(draw_fp, " <F %d >\n",15); /* assume lines should not be filled */
      polyopen = TRUE; /* Polyline open */
   }
   return (0);
}
/******************************************************************************/
static int groupem(void){
   /* Framemaker ignores requests to put a single entity into a group so make
      a polyline that move to the first point of the polyline/polygon/text so
      there will be two members; else act sanely */
   if(members == 1)
   {
      fprintf(draw_fp, "<PL\n");
      fprintf(draw_fp, " <GID %d >\n",groupid); /* PolyLine */
      fprintf(draw_fp, " <P %.2f %.2f >\n", kludgex, kludgey);
      fprintf(draw_fp, "> #end curve PL\n");
      fprintf(draw_fp, " <G <ID %d> <GID %d>>\n",groupid,pgroup); /*Driver exit */
      members=0;
      groupid++; /* start a new group if pen attributes were changed/called */
   } else if(members != 0){
      fprintf(draw_fp, " <G <ID %d> <GID %d>>\n",groupid,pgroup); /*Driver exit */
      members=0;
      groupid++; /* start a new group if pen attributes were changed/called */
   }
   return (0);
}
/******************************************************************************/
/* MIF_exit do a flush and close the output file if necessary.  */
static int MIF_exit(void) {
   closeline(); /* close Polyline line if it open */
   /*
           Define the group for all the objects to make the chart easier to manipulate
           after it is imported into FrameMaker
        */
   groupem();
   fprintf(draw_fp, " <G <ID %d> >\n",pgroup); /*Page Clear*/
   fprintf(draw_fp, "> #end of Page\n");
   fprintf(draw_fp, "# End of MIFFile\n");
   pgroup=groupid; /* page id */
   groupid++; /* increment group id */
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
/* MIF_draw draw to an x, y point.  */
/* Note: (0, 0) is defined as the top left of the window in MIF (easy to forget).  */
static int MIF_draw(int x, int y){
   float realx, realy;
   float realx0, realy0;
   realx = (x)/MIFTORAS+borderx;
   realy = (vdevice.sizeSy -y)/MIFTORAS+bordery;
   realx0 = (vdevice.cpVx)/MIFTORAS+borderx;
   realy0 = (vdevice.sizeSy - vdevice.cpVy)/MIFTORAS+bordery;

   if (pslstx != vdevice.cpVx || pslsty != vdevice.cpVy || points >= MAXPOINTS ){
      closeline(); /* close line if required */
      openline(); /* start line */
      fprintf(draw_fp, " <P %.2f %.2f >\n", realx0, realy0);
      kludgex=realx0;
      kludgey=realy0;
      points = 1;
   }
   openline(); /* start line if required */
   fprintf(draw_fp, " <P %.2f %.2f >\n", realx, realy);
   points++;
   pslstx = x;
   pslsty = y;
   drawn = 1;
   return (0);
}
/******************************************************************************/
/* MIF_clear flush the current page without resetting the graphics state */
static int MIF_clear(void) {
   closeline(); /* close line if required */
   if (drawn)
   {
      groupem();
      fprintf(draw_fp, " <G <ID %d> >\n",pgroup); /*Page Clear*/
      fprintf(draw_fp,"> #end of Page\n");
      fprintf(draw_fp,"<Page\n");
      fprintf(draw_fp,"<PageSize  %f %f >\n",MIFXSIZE0,MIFYSIZE0);
      pgroup=groupid; /* page id */
      groupid++; /* increment group id */
   }
   drawn = 0;
   points = 0;
   return(0);
}
/******************************************************************************/
/* MIF_color change the color of the pen
 *      kludged so negative value sets raster line width
 */
static int MIF_color(int col) {
   closeline(); /* close line if required */
   if ( col < 0 )
   {
      curwid = abs(col);
   } else {
      curcol4= col % CMAPSIZE;      /* MIF 4.0 allows user-defined colors */
      col=col % MIN(256,CMAPSIZE);  /* following assumes result is 0 thru 255 */
      curcol = cmap[col % 8];       /* MIF 3.0 only has colors 0 thru 7 */
      if(MIFVERSION == 4 ){
         curpat = col/CMAPSIZE;     /* MIF 4.0 does not need patterns; it has colors*/
      }else{
         curpat = col/16;           /* MIF 3.0 has patterns 0 thru 15 */
      }
   }
   groupem();
   return(0);
}
/******************************************************************************/
static int MIF_setlw(int width){  /* Set line width */
   closeline(); /* close line if required */
   curwid = abs(width);
   groupem();
   return(0);
}
/******************************************************************************/
/* MIF_font load in small or large - could be improved.  */
static int MIF_font(char *font) {
   if (strcmp(font, "small") == 0) {
      vdevice.hwidth = 12.0/72.0/.03937*MIFTORAS;
      vdevice.hheight = vdevice.hwidth * 2.2;
   } else if (strcmp(font, "large") == 0) {
      vdevice.hheight = 55.0/72.0/.03937*MIFTORAS;
      vdevice.hwidth = vdevice.hheight / 4.5;
   } else
      return(0);

   return(1);
}
/******************************************************************************/
/* MIF_string output a string.  */
static int MIF_string(char *s) {
   float realy0, realx0;
   char    c;
   int     i;

   realy0 = (vdevice.sizeSy - vdevice.cpVy)/MIFTORAS+bordery;
   realx0 = (vdevice.cpVx)/MIFTORAS+bordery;
   closeline(); /* close line if required */
   fprintf(draw_fp,"<TextLine \n");
   fprintf(draw_fp,  " <GID %d >\n",groupid); /* TextLine */
   members++;
   fprintf(draw_fp, " <F %d >\n",curpat);
   fprintf(draw_fp, " <TLOrigin    %f  %f > \n",realx0, realy0);
   kludgex=realx0;
   kludgey=realy0;
   fprintf(draw_fp,"  <Font \n");
   fprintf(draw_fp,"   <FFamily `Courier'> \n");
   fprintf(draw_fp,"   <FSize %f > \n",vdevice.hheight/MIFTORAS);
   fprintf(draw_fp,"   <FBold Yes> \n");
   fprintf(draw_fp,"   <FUnderline  No> \n");
   fprintf(draw_fp,"   <FSeparation %d > \n",curcol);
   fprintf(draw_fp,"  > # end Font \n");

   fprintf(draw_fp, "<String `");

   for(i=0; (c=s[i]) != '\0' ;i++)
   {
      switch(c) {
      case '\\':
         fprintf(draw_fp, "\\\\");
                  break;
               case '`':
                  fprintf(draw_fp, "\\Q");
                  break;
               case '>':
                  fprintf(draw_fp, "\\>");
                  break;
               case '\'':
                  fprintf(draw_fp, "\\q");
                  break;
               default:
                  fprintf(draw_fp, "%c",c);
      }

   }
   fprintf(draw_fp,"'> \n");
   fprintf(draw_fp,"> # end TextLine [%s] \n",s);
   drawn = 1;
   pslstx = pslsty = -1;
   return(0);
}
/******************************************************************************/
/* MIF_char output a character */
static int MIF_char(char c) {
  char  s[2];
  s[0] = c; s[1]='\0';
  MIF_string(s);
  return(0);
}
/******************************************************************************/
/* fill a polygon */
static int MIF_fill(int n, int x[], int y[]) {
   int     i;
   float realx, realy ;

   closeline(); /* close line if required */
   fprintf(draw_fp, "<Polygon <SN>\n");
   fprintf(draw_fp, " <GID %d >\n",groupid); /* Polygon */
   members++;
   fprintf(draw_fp, " <PW %f>\n",vdevice.sizeX*curwid/10000.0/MIFTORAS);
   fprintf(draw_fp, " <S %d >\n",curcol);
   printcolor(curcol4);
   fprintf(draw_fp, " <F %d >\n",curpat);

   kludgex = (x[0])/MIFTORAS+borderx;
   kludgey = (vdevice.sizeSy -y[0])/MIFTORAS+bordery;

   for (i = 0; i < n; i++)
   {
      realx = (x[i])/MIFTORAS+borderx;
      realy = (vdevice.sizeSy -y[i])/MIFTORAS+bordery;
      fprintf(draw_fp, " <P %.2f %.2f >\n", realx, realy);
   }

   fprintf(draw_fp, "> #end of Polygon \n");


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
static DevEntry mifdev = {
                "mif",       /* name of device */
                "large",     /* name of large font */
                "small",     /* name of small font */
                noop,        /* Set drawing in back buffer */
                MIF_char,    /* Draw a hardware character */
                noop,        /* Check if a key was hit */
                MIF_clear,   /* Clear the screen to current color */
                MIF_color,   /* Set current color */
                MIF_draw,    /* Draw a line */
                MIF_exit,    /* Exit graphics */
                MIF_fill,    /* Fill a polygon */
                MIF_font,    /* Set hardware font */
                noop,        /* Set drawing in front buffer */
                noop,        /* Wait for and get the next key hit */
                MIF_init,    /* Initialize the device */
                noop2,       /* Get mouse/cross hair position */
                MIF_mapcolor,/* Set color indices */
                MIF_setlw,   /* Set line width */
                MIF_string,  /* Draw a hardware string */
                noop,        /* Swap front and back buffers */
                noop         /* Syncronize the display */
};
/******************************************************************************/
/* copy the mif device into vdevice.dev.  */
int _MIF_draw_devcpy(void) {
   /*      - if you don't have structure assignment ...
        char    *dev, *tdev, *edev;
        dev = (char *)&mifdev;
        tdev = (char *)&vdevice.dev;
        edev = dev + sizeof(Device);

        while (dev != edev)
                *tdev++ = *dev++;
*/
   vdevice.dev = mifdev;
   MIFVERSION=3;
   vdevice.depth = 3;
   return(0);
}
/******************************************************************************/
/* copy the mif device into vdevice.dev.  */
int _MIF4_draw_devcpy(void) {
   /*      - if you don't have structure assignment ...
        char    *dev, *tdev, *edev;
        dev = (char *)&mifdev;
        tdev = (char *)&vdevice.dev;
        edev = dev + sizeof(Device);

        while (dev != edev)
                *tdev++ = *dev++;
*/
   vdevice.dev = mifdev;
   MIFVERSION=4;
   vdevice.depth = 8;
   return(0);
}
/******************************************************************************/

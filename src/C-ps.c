/* this code is licensed as public domain */
#ident "@(#)M_DRAW:driver/ps.c - M_DRAW driver for PostScript files"
/*
   depth=1 monochrome
   depth=4 grayscale, 8 shades
   depth=9 color, 256 colors

   The background color is assumed to be whatever curcol was set to
   when the last clear was done.

   In  monochrome  mode,  everything  is drawn in black  except the
   current background color, which is drawn as white.

   In  grayscale  mode,  everything  is mapped to 8 shades of gray.
   Lines are  grayscaled  too.  Another  grayscale  where lines are
   solid but polygons are grayscaled would probably be useful too.
   SHOULD USE COLOR CONVERSION TO CHANGE PENS TO MONCHROME VALUES.

   In color, mapcolor is functional and all colors are left alone.

   A Clear does NOT fill in the  drawing  surface  with the current
   color; it probably should.

   Note that this driver will not print a blank page
   ================================================================
   In the middle of:

   Really need to clean up how hardware text should be autosized
   to the current  software  text size, and should pick up the
   current font text angle too.

   small  and  large  could  pick out a normal  and  bold  font, or
   something like that.

   Should put in the encapsulated  PostScript  structuring comments
   too.
   ================================================================
   Next time in:

   It would be very  useful to have an  option to lay out  multiple
   pages per "page"; and to allow the overall size to be controlled
   via an initialization call or via an environment  variable (at
   least in unix); allowing border specification would be nice too.
   A clear  would go to the next  logical  page  until a real  page
   advance was needed.

   Should  gather  polylines up into longer  polylines  (do not put
   more than 1500 points into a single polyline, however)

   should initial curcol,curbak be 0 or 7?

   See if putting more draws on a line makes anything print faster

   I am loosing my rgb setting when I go in and out of a driver
   (like when use hcopy command or sz command to print or resize
   window in USH). Any way to keep the table cleanly?

   vgetdev call always returns PostScript instead of other names.
   Is that good or bad?
*/

#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#ifndef MINGW
#include <pwd.h>
#include <sys/utsname.h>
#endif
#include <unistd.h>
#include <sys/types.h>
#include "draw.h"
#include <math.h>
#include <string.h>
#define M_DRAW 1

/* How to convert degrees to radians */
#ifndef PI
#define PI      3.14159265358979323846264338327950288419716939937510
#endif
#define d2r(x)     ((x) * PI / 180.0)
#define r2d(x)     ((x) * 180.0 / PI)

#define FALSE 0
#define TRUE  1
/*
 * Maximum number of points to put into a polyline, reduced from 1500 to 1000
 * because of Xerox 4700 bug
 */
#define MAXPOINTS 1000

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))

#define CMAPSIZE 8192
struct rgb_color {
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
   };

static struct rgb_color ps_carr[CMAPSIZE];

extern float hardwidth[128];

static int      ps_first_time = 1, drawn = 0,
                curcol = 0,                     /* draw black */
                curback = 0,
                curwidth = 2,
                curwidthwritten = -999,
                polyopen = FALSE, /* PolyLine not open */
                points=0,
                pslstx = -1, pslsty = -1;       /* last (x, y) drawn */
static int      Page_Count = 0 ;
static char     hardfont[30] = { 'C', 'o', 'u', 'r', 'i', 'e', 'r', '\0' };

/* gray scale map for our standard colours */
static float    graymap[8] = {
                        1.00,   /* DRAW WHITE (Same as "background") */
                        0.30,
                        0.59,
                        0.89,
                        0.11,
                        0.41,
                        0.70,
                        0.0     /* DRAW BLACK (So you "see" the line) */
};

extern FILE     *_draw_outfile();
extern  FILE     *draw_fp;

/******************************************************************************/
static int PS_string(char *s);
static int PS_mapcolor(int i, int r, int g,int b);
/******************************************************************************/
static int PS_header(int xmin,int ymin,int xmax,int ymax,int xoff,int yoff) {

   time_t tod;
   float a;
/*----------------------------------------------------------------------------*/
#ifndef MINGW
   struct utsname unstr, *un;
#endif
/*----------------------------------------------------------------------------*/
   char *username;
   struct passwd *pw;
        fputs("%!PS-Adobe-1.0\n", draw_fp);
        fprintf(draw_fp,"%%%%DocumentFonts: Courier Helvetica-Bold\n");
        fprintf(draw_fp,"%%%%Creator: M_DRAW PostScript driver 3.0\n");
        fprintf(draw_fp,"%%%%Title: M_DRAW\n");
        a=72.0/300.0;
        fprintf(draw_fp,"%%%%BoundingBox: %d %d %d %d\n",
          (int)((xmin+xoff)*a), (int)((ymin+yoff)*a),
          (int)((xmax+xoff)*a), (int)((ymax+yoff)*a));
        fprintf(draw_fp,"%%%%Pages: (atend)\n") ;

        time(&tod);
        fprintf(draw_fp,"%%%%CreationDate: %s",ctime(&tod));

/*----------------------------------------------------------------------------*/
#ifndef MINGW
        un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
        uname(un);

        if ((username = getlogin()) == NULL ){
        pw = getpwuid(getuid());
        username = pw->pw_name;
        }
        fprintf(draw_fp,"%%%%For: %s on OS=%.*s NETWORK_NAME=%.*s RELEASE=%.*s VERSION=%.*s MACHINE=%.*s\n",username,
                (int)sizeof(un->sysname),  un->sysname,
                (int)sizeof(un->nodename), un->nodename,
                (int)sizeof(un->release),  un->release,
                (int)sizeof(un->version),  un->version,
                (int)sizeof(un->machine),  un->machine);

#endif
/*----------------------------------------------------------------------------*/

        return(0);
}
/******************************************************************************/
/* Initialization that is common to both layouts */
static int PS_common_init(void) {
        int i;

        fprintf(draw_fp, "2 setlinewidth\n1 setlinejoin\n1 setlinecap\n");   /* Set other line drawing parameters    */
        fprintf(draw_fp, "/sf /Courier findfont def\n");                     /* Speed up symbol font handling        */
        fprintf(draw_fp, "/m /moveto load def\n");                           /* Move                                 */
        fprintf(draw_fp, "/B /moveto load def\n");                           /* Beginning of PolyLine                */
        fprintf(draw_fp, "/E /stroke load def\n");                           /* End of PolyLine                      */
        fprintf(draw_fp, "/d { lineto } def\n");                             /* Draw                                 */
        fprintf(draw_fp, "/p /lineto load def\n");                           /* Polygon Draw                         */
        fprintf(draw_fp, "/h { sf exch scalefont setfont } def\n");          /* Set character height                 */
        fprintf(draw_fp, "/s /show load def\n");                             /* Show character string                */
        fprintf(draw_fp, "/g /setgray load def\n");                          /* Set gray scale                       */
        fprintf(draw_fp, "/RJ { stringwidth neg exch neg exch rmoveto } bind def\n");                        /* Right Justify string */
        fprintf(draw_fp, "/CJ { stringwidth 2 div neg exch 2 div neg exch rmoveto } bind def\n");            /* Center string */
        fprintf(draw_fp, "45 h\n");                                          /* Set a default font height            */
        fprintf(draw_fp, "%%%%EndProlog\n");                                                 /* Set a default font height            */

        drawn = 0;

        for (i = 0; i < CMAPSIZE; i++)                                  /* set up the basic colors */
        {
                ps_carr[i].red=255;
                ps_carr[i].green=255;
                ps_carr[i].blue=255;
        }

        PS_mapcolor(0, 255, 255, 255);
        PS_mapcolor(1, 255, 0, 0);
        PS_mapcolor(2, 0, 255, 0);
        PS_mapcolor(3, 255, 255, 0);
        PS_mapcolor(4, 0, 0, 255);
        PS_mapcolor(5, 255, 0, 255);
        PS_mapcolor(6, 0, 255, 255);
        PS_mapcolor(7, 0, 0, 0);

        PS_mapcolor( 8, 155, 0, 0);
        PS_mapcolor( 9, 0, 155, 0);
        PS_mapcolor(10, 155, 255, 255);
        PS_mapcolor(11, 155, 155, 0);
        PS_mapcolor(12, 0, 0, 155);
        PS_mapcolor(13, 155, 0, 155);
        PS_mapcolor(14, 0, 155, 155);
        PS_mapcolor(15, 100, 100, 100);

        vdevice.hwidth= 11.0*300.0/72.0;
        vdevice.hheight=11.0*300.0/72.0;

        polyopen = FALSE; /* Polyline not open */
        Page_Count = 0 ;

        return(1);
}
/******************************************************************************/
static int PS_init(void) /* PS_init set up the PostScript (Landscape) environment. Returns 1 on success.  */
{
        int prefx, prefy, prefxs, prefys;
        draw_fp = _draw_outfile();
        if (!ps_first_time)
                return(1);
/*
   Assume:
   printer is 300 dpi; landscape print with plotting area height of 2250 starting 2450 up from bottom of page
   that is 3000 long with a 150 offset on right edge

      prefxs and prefys are preferred plotting area size in rasters (assume 300 per inch)
      prefy is the BOTTOM margin on this - make big note in manual
   NON-INTUITIVE for LANDSCAPE:
      prefx is the LEFT margin
*/

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        }else{
           prefx = 150;
           prefy = 150;
           vdevice.sizeSy = 2250;
           vdevice.sizeSx = 3000;
           vdevice.sizeX = vdevice.sizeY = 2250;
        }
        PS_header(0,0,vdevice.sizeSy,vdevice.sizeSx,prefy,prefx);
        fprintf(draw_fp,"%%%%Orientation: Landscape\n");
        fprintf(draw_fp,"%%%%EndComments\n");
        fprintf(draw_fp,"%%%%BeginProlog\n");

        fprintf(draw_fp,"save\n");
        fprintf(draw_fp, "72 300 div dup scale 90 rotate %d %d translate\n",prefx,0-vdevice.sizeSy-prefy);

        PS_common_init();
        return (1);
}
/******************************************************************************/
#ifndef M_DRAW
/*
 * Set the line style...
 */
static int PS_setls(int lss) {
        unsigned ls = lss;
        int     i, d, a, b, offset;

        if (ls == 0xffff) {
                fprintf(draw_fp, "[] 0 setdash\n");
                return(0);
        }

        fputc('[', draw_fp);

        for (i = 0; i < 16; i++)        /* Over 16 bits */
                if ((ls & (1 << i)))
                        break;

        offset = i;

#define ON      1
#define OFF     0

        a = b = OFF;
        if (ls & (1 << 0))
                a = b = ON;

        d = 0;
        for (i = 0; i < 16; i++) {      /* Over 16 bits */
                if (ls & (1 << i))
                        a = ON;
                else
                        a = OFF;

                if (a != b) {
                        b = a;
                        fprintf(draw_fp, "%d ", d * 2 + 1);
                        d = 0;
                }

                d++;
        }

        fprintf(draw_fp, "] %d setdash\n", offset);

        return(0);
}
#endif
/******************************************************************************/
static void closeline(void){
   if(polyopen){
      if ( points % 10 ) fprintf(draw_fp,"\n");
      fprintf(draw_fp, "E %%PTS=%d\n",points);
      polyopen = FALSE; /* Polyline not open */
      points = 0;
   }
}
/******************************************************************************/
static int PS_setlw(int w){ /* Set the line width */
        static unsigned int line_width;
        curwidth = w;
        if ( curwidthwritten != w ) {
           line_width=abs(w)-1;
           line_width=line_width*vdevice.sizeX/10000.0;
           line_width=MAX(1,line_width);
           closeline(); /* close line if required */
           if (drawn != 0 ) {
              fprintf(draw_fp, "%hd setlinewidth\n",line_width);
              curwidthwritten = w;
           }
        }
        /*fprintf(draw_fp, "%% %hd linewidth %hd w %hd curwidth %hd curwidthwritten \n",line_width,w,curwidth,curwidthwritten);*/
        return(0);
}
/******************************************************************************/
/* change the grey value of the ink; kludged so negative value sets raster line width */
static int PS_color(int col) {

        closeline(); /* close line if required */

        if ( col < 0 ) {
                PS_setlw(abs(col));
                return(0);
        } else if (drawn == 0 ) {
                curcol = col;
                return(0);
        } else if (vdevice.depth == 1 ){ /* Monochrome World : Everything is black and white */
                if (col == curback )  /* you should see everything except the background color */
                {
                  fprintf(draw_fp," 1 g\n"); /* draw same as background (WHITE) */
                } else {
                  fprintf(draw_fp," 0 g\n"); /* Everything is black if not background */
                }
        } else if (vdevice.depth == 3 ) {
                fprintf(draw_fp, "%3.2f g\n", graymap[col % 8]);
        } else {
                if (col >= CMAPSIZE)
                        return(0);
                fprintf(draw_fp, "%f %f %f setrgbcolor\n",
                   ps_carr[col].red/255.0,
                   ps_carr[col].green/255.0,
                   ps_carr[col].blue/255.0);
        }
        curcol = col ;
        return(0);
}
/******************************************************************************/
static void iffirst(int ii) {
        if ( drawn == 0 ){
           ++Page_Count;
           fprintf(draw_fp, "%%%%Page: %d %d\n",Page_Count,Page_Count);
           fprintf(draw_fp, "save %% %d\n",ii);
           drawn = 1;          /* must be set to prevent recursion before PS_ routines are called */
           PS_color(curcol);   /* page-independent declaration of current color for purposes of structuring */
           PS_setlw(curwidth); /* page-independent declaration of current width for purposes of structuring */
           if(ii != 3 )PS_string("\0");    /* page-independent declaration of current font  for purposes of structuring */
        }
}
/******************************************************************************/
static void openline(void){
   if(!polyopen){
      PS_setlw(curwidth); /* If current color has not been written, update it */
      fprintf(draw_fp, "%d %d B\n", vdevice.cpVx, vdevice.cpVy);
      polyopen = TRUE; /* Polyline open */
   }
}
/******************************************************************************/
/* set up the PostScript (Portrait) environment. Returns 1 on success.  */
static int PSP_init(void) {
        int prefx, prefy, prefxs, prefys;
        draw_fp = _draw_outfile();

        if (!ps_first_time)
                return(1);

        draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);
        if (prefxs != -1 ) {
           vdevice.sizeSy = prefys;
           vdevice.sizeSx = prefxs;
           vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
        }else{
           prefx = 150;
           prefy = 150;
           vdevice.sizeSy = 3000;
           vdevice.sizeSx = 2250;
           vdevice.sizeX = vdevice.sizeY = 2250;
        }

        PS_header(0,0,vdevice.sizeSx,vdevice.sizeSy,prefx,prefy);
        fprintf(draw_fp,"%%%%Orientation: Portrait\n");
        fprintf(draw_fp,"%%%%EndComments\n");
        fprintf(draw_fp,"%%%%BeginProlog\n");
        fprintf(draw_fp,"save\n");
        fprintf(draw_fp,"72 300 div dup scale %d %d translate\n",prefx,prefy);

        PS_common_init();
        return (1);
}
/******************************************************************************/
/* do a showpage and close the output file if necessary.  */
static int PS_exit(void) {
        closeline(); /* close Polyline line if it open */

        if (drawn){
          fprintf(draw_fp, "showpage\n");
          fprintf(draw_fp, "restore\n");
          fflush(draw_fp);
        }else{
           drawn = 0;
        }

        fprintf(draw_fp,"%%%%Trailer\n");
        fprintf(draw_fp,"%%%%Pages: %d\n",Page_Count) ;
        fprintf(draw_fp, "restore\n");
        fprintf(draw_fp,"%%%%EOF\n") ;
        if (draw_fp != stdout && draw_fp != stderr ){

                fflush(draw_fp);
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
        }

        return(0);
}
/******************************************************************************/
/* draw to an x, y point.  */
static int PS_draw(int x, int y) {
        static char ten[10]={' ',' ',' ',' ',' ',' ',' ',' ',' ','\n' }; /* cheap trick */
        iffirst(1);
        if (pslstx != vdevice.cpVx || pslsty != vdevice.cpVy || points >= MAXPOINTS){
                closeline(); /* close line if required */
                openline(); /* start line */
                points = 1;
        }

        openline();
        fprintf(draw_fp, "%d %d d%c", x, y,ten[points%10]);
        points++;
        pslstx = x;
        pslsty = y;

        return(0);
}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */

static int PS_mapcolor(int i, int r, int g,int b) {
        /* fprintf(stderr, "mapcolor = %d %d %d %d\n", i,r,g,b); */

        if (i >= CMAPSIZE || i < 0 ){
                return(-1);
        }

        ps_carr[i].red = (unsigned short)(r);
        ps_carr[i].green = (unsigned short)(g);
        ps_carr[i].blue = (unsigned short)(b);
        return(0);
}
/******************************************************************************/
/* load in small or large - could be improved. Radically KLUDGED; made SoftText extern  */
static int PS_font(char *fontname) {
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

   /* select Courier for small and Helvetica-Bold for big */

   vdevice.attr->a.softtext = SOFTHARDWARE;
   /* textsize will be obeyed after the font is set
    * maybe should read current software size and convert virtual
    * to device instead of resetting each time */

      if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
         vdevice.hwidth=11.0*300.0/72.0;
         vdevice.hheight=11.0*300.0/72.0;
      }

   /*
   fprintf(stderr,"*PS_font* vdevice.hwidth=%f",vdevice.hwidth);
   fprintf(stderr," vdevice.hheight=%f\n",vdevice.hheight);
   */
   if (strcmp(fontname, "small") == 0) {
      strncpy(hardfont,"Courier",30);
         rat=0.60; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=1.00 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr,"font table %f %c\n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
         }
   } else if (strcmp(fontname, "large") == 0) {
      strncpy(hardfont,"Helvetica-Bold",30);
      rat=1.26; /* Kludge Factor */
      rat=1.00; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=((float)helvetica_w[i])/100.0 * rat; /* ratio of character width to vdevice.hwidth*/
         /*fprintf(stderr,"font table %f %c\n",hardwidth[i],i);*/ /* VERIFY FONT TABLE*/
      }
   } else{
      return(0);
   }

   return(1);
}
/******************************************************************************/
/* flush the current page without resetting the graphics state of the laser printer.  */
static int PS_clear(void) {
   if (drawn){
      closeline(); /* close line if required */
      fprintf(draw_fp, "showpage restore\n");
   }
   points = 0;
   drawn = 0;
   curback = curcol;

   return(0);
}
/******************************************************************************/
/* PS_char output a character */
static int PS_char(char c) {
  char  s[2];
  s[0] = c; s[1]='\0';
  PS_string(s);
  return(0);
}
/******************************************************************************/
static int STRIPSPECIAL(char *s,char *stmp) {
        int ipos=0;
        char c;
        while ((c = *s++)){
                switch(c) {
                case '(':
                        stmp[ipos++]='\\';
                        break;
                case ')':
                        stmp[ipos++]='\\';
                        break;
                case '\\':
                        stmp[ipos++]='\\';
                        break;
                default:
                        break;
                }
                stmp[ipos++]=c;
        }
                stmp[ipos]='\0';
        return(0);
}
/******************************************************************************/
/*
   <x,y> is current position (assumed defined)
   rot angle of path of text in degrees
   string to print
   horizontal justification 0=left justified; 1 right; 2 center
*/
static int DISPSTR(int x, int y, float rot, char *s, int just, float szw, float szh) {
    char tmpstr[256];

    /*fprintf(draw_fp,"/%s findfont [ %f 0 0 %f 0 0] makefont setfont %% scale the font\n",hardfont,szw,szh*0.78);*/
    fprintf(draw_fp,"/%s findfont [ %f 0 0 %f 0 0] makefont setfont %% scale the font\n",hardfont,szw,szh);

    if (szw <= 0.0 || szh <=0.0 ) {
        /*fprintf(stderr,"*DISPSTR* zero size for string %s=%f %f\n",s,szw,szh);*/
        /*JSU  return(0);*/
    }

    if ( s == NULL || strlen(s) == 0 ){
        /*fprintf(stderr,"*DISPSTR* print null string\n",s);*/
        return(0);
    }

    fprintf(draw_fp, "gsave\n");
    fprintf(draw_fp, "%d %d translate\n", x, (int)(y+0.22*szh));  /*JSU NOW */  /*JSU NOW */
    fprintf(draw_fp, "%f rotate\n", rot);
    fprintf(draw_fp, "0 0  m %% Transformed text location\n");
    STRIPSPECIAL(s, tmpstr);
    switch (just) {
    case 0:
        fprintf(draw_fp, "(%s) s\n", tmpstr);
        break;
    case 1:
        fprintf(draw_fp, "(%s) RJ s\n", tmpstr);
        break;
    case 2:
        fprintf(draw_fp, "(%s) CS s\n", tmpstr);
        break;
    }
    fprintf(draw_fp, "grestore\n");
    return(0);
}
/******************************************************************************/
/* output a character making sure that a '\' is sent first when appropriate. */
/* output a string :CAUTION: supports current hardware text angle*/
static int PS_string(char *s) {
        float rot;
        /*fprintf(stderr,"PS_string='%s'\n",s);*/
        closeline(); /* close line if required */
        /* STRLEN OF NULL BOOM ON SUNOS if ( s != NULL || strlen(s) != 0 ){ */
        if ( s != NULL ){
           iffirst(3);
        }
        rot=r2d(atan2((double)vdevice.attr->a.textsin,(double)vdevice.attr->a.textcos));
        /*
        fprintf(stderr,"*PS_string* vdevice.cpVx=%f",vdevice.cpVx);
        fprintf(stderr," vdevice.cpVy=%f",vdevice.cpVy);
        fprintf(stderr," rot=%f",rot);
        fprintf(stderr," string=%s",s);
        fprintf(stderr," vdevice.hwidth=%f",vdevice.hwidth);
        fprintf(stderr," vdevice.hheight=%f\n",vdevice.hheight);
        */
        if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
           fprintf(stderr,"*PS_string* ERROR: ZERO SIZE CHARACTERS\n");
           vdevice.hwidth=11.0*300.0/72.0;
           vdevice.hheight=11.0*300.0/72.0;
        }
        DISPSTR(vdevice.cpVx,vdevice.cpVy,rot,s,0,vdevice.hwidth,vdevice.hheight);
        pslstx = pslsty = -1;
        return(0);
}
/******************************************************************************/
/* fill a polygon */
static int PS_fill(int n, int x[], int y[]) {
   int     i;

   /* only put a linefeed into prints of polygon points every 8 sets of points */
   /* SunOS would not allow this form (said too many initializers): static char linefeed[2] = " \n"; */
   static char linefeed[2] = {' ','\n'};

   closeline(); /* close line if required */
   iffirst(4);

   fprintf(draw_fp, " newpath %d %d m %% Fill a polygon\n", x[0], y[0]);

   for (i = 1; i < n; i++) {
      fprintf(draw_fp, " %d %d p%c", x[i], y[i],linefeed[(i % 8)/7]);
   }
   fprintf(draw_fp, "closepath\n");

   if (vdevice.depth == 1 ) {
      if (curcol == curback ) {
         fprintf(draw_fp, "1 g\n");  /* solid white fill */
      }else{
         fprintf(draw_fp, "0 g\n");  /* solid black fill */
      }
   } else if (vdevice.depth == 3 ) {
      fprintf(draw_fp, "%3.2f g\n", graymap[curcol % 8]);
   } else if (vdevice.depth > 1 && curcol < CMAPSIZE) {
      fprintf(draw_fp,"%f %f %f setrgbcolor\n",
                   ps_carr[curcol].red/255.0,
                   ps_carr[curcol].green/255.0,
                   ps_carr[curcol].blue/255.0);
   }
   fprintf(draw_fp, "eofill\n");
   vdevice.cpVx = x[n - 1];
   vdevice.cpVy = y[n - 1];
   pslstx = pslsty = -1;      /* fill destroys current path */
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry psdev = {
                "postscript",   /* name of device */
                "large",        /* name of large font */
                "small",        /* name of small font */
                noop,           /* Set drawing in back buffer */
                PS_char,        /* Draw a hardware character */
                noop,           /* Check if a key was hit */
                PS_clear,       /* Clear the screen to current color */
                PS_color,       /* Set current color */
                PS_draw,        /* Draw a line */
                PS_exit,        /* Exit graphics */
                PS_fill,        /* Fill a polygon */
                PS_font,        /* Set hardware font */
                noop,           /* Set drawing in front buffer */
                noop,           /* Wait for and get the next key hit */
                PS_init,        /* Initialize the device */
                noop2,          /* Get mouse/cross hair position */
                PS_mapcolor,    /* Set color indices */
#ifndef M_DRAW
                PS_setls,       /* Set line style */
#endif
                PS_setlw,       /* Set line width */
                PS_string,      /* Draw a hardware string */
                noop,           /* Swap front and back buffers */
                noop            /* Syncronize the display */
};
/******************************************************************************/
/* copy the PostScript device into vdevice.dev.  */
int _PS_draw_devcpy(void) {
/*      - if you don't have structure assignment ...
        char    *dev, *tdev, *edev;
        dev = (char *)&psdev;
        tdev = (char *)&vdevice.dev;
        edev = dev + sizeof(Device);

        while (dev != edev)
                *tdev++ = *dev++;
*/
        vdevice.dev = psdev;
        vdevice.dev.Vinit = PS_init;
        vdevice.depth = 1;
        return(0);
}
/******************************************************************************/
/* copy the PostScript portrait device into vdevice.dev.  */
int _PSP_draw_devcpy(void) {
        vdevice.dev = psdev;
        vdevice.dev.Vinit = PSP_init;
        vdevice.depth = 1;
        return(0);
}
/******************************************************************************/
int _PSC_draw_devcpy(void) {
        vdevice.dev = psdev;
        vdevice.dev.Vinit = PS_init;
        vdevice.depth = 13;
        return(0);
}
/******************************************************************************/
int _PPSC_draw_devcpy(void) {
        vdevice.dev = psdev;
        vdevice.dev.Vinit = PSP_init;
        vdevice.depth = 13;
        return(0);
}
/******************************************************************************/
int _PSG_draw_devcpy(void){ /* grayscale PostScript, 8 shades */
        vdevice.dev = psdev;
        vdevice.dev.Vinit = PS_init;
        vdevice.depth = 3;
        return(0);
}
/******************************************************************************/
int _PPSG_draw_devcpy(void){ /* Portrait grayscale PostScript, 8 shades */
        vdevice.dev = psdev;
        vdevice.dev.Vinit = PSP_init;
        vdevice.depth = 3;
        return(0);
}
/******************************************************************************/

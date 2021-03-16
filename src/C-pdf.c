/* this code is licensed as public domain */
#ident "@(#)M_DRAW:source - driver/pdf.c - M_DRAW driver for Adobe Illustrator PDF "
#ident "@(#)M_DRAW:author - John S. Urban"
#ident "@(#)M_DRAW:version - 1.5, Mar 2005"

/* 
   depth=9 color, 256 colors

   Low level interface to clear-text Adobe PDF file
   described at http://www.adobe.com
 ===============================================================================
 So that document can be written in one pass, and assuming one stream is
 one page, file structure is basically the following list of objects

 #   2 Info
 |
 |              #--# 4 stream
 |              |  #-- 5 Length of stream
 |     #--------#   6 Page with Parent 3
 |     |
 |     |        #--# 7 stream
 |     |        |  #-- 8 Length of stream
 |     #--------#  9 Page with Parent 3
 |     |
 |     |        #--#10 stream
 |     |        |  #-- 11 Length of stream
 |     #--------#  12 Page with Parent 3
 |     |
 |     |        #--#   (n-1)*3+4  stream
 |     |        |  #-- (n-1)*3+5  Length of stream
 |     #--------#   n (n-1)*3+6 Page with Parent 3
 |     |
 |  #--# 3 Pages (with Kids list )
 |  |
 |  |
 #--#  1 Root (Catalog)
 ===============================================================================
 Tested with the following PDF viewers:
  o xpdf
  o The Adobe PDF viewer, called from CygWin via "cygstart FILENAME.pdf"
  o Ghostscript-based tools such as gs and ghostview or gv

The Adobe viewer gives no diagnostic messages that I know of, xpdf gives
some and repairs files, gs gives good diagnostic messages.

  xpdf -cmd # shows a PDF file as it is being read, probably handy for debugging
  xpdf -remote  # lets you make scripts to show particular pages or view files
 ===============================================================================
 JUST STARTING -- Need to consider
 - map "raster" units to PDF "physical" units so get expected page size
 - how to scale and rotate and translate hardware font

 Usage Notes:
o assuming 72 points to an inch, about 720 rasters to the inch
o Recollecting  connected line segments into polylines
o So far, am assuming clear called before drawing first page
o Use negative color values to specify line thickness in raster units in
  this version of draw
o Note that initially made a dummy reference table at end, which xpdf and Adobe seem to be ok with
o clear text, even though not encouraged by Adobe documentation
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

/* How to convert degrees to radians */
#ifndef PI
#define PI      3.14159265358979323846264338327950288419716939937510
#endif

#define d2r(x)     ((x) * PI / 180.0)
#define r2d(x)     ((x) * 180.0 / PI)

#define MAX(x, y)       ((x) > (y) ? (x) : (y))
#define MIN(x, y)       ((x) < (y) ? (x) : (y))
#define ABS(x)          ((x) < 0 ? -(x) : (x))

#define FALSE 0
#define TRUE  1

extern float hardwidth[128];
static char hardfont[30]={'/','F','1','\0'};

/* total drawing area size in x direction in pts*/
#define PDF_YSIZE 792
/* total drawing area size in y direction in pts*/
#define PDF_XSIZE 612
#define RAS_PER_PT 10

static int      points_printed=0;
static int      PDF_first_time = 1;
static int      drawn = 0;
static int      LAST_X = -1, LAST_Y = -1;/* last (x, y) drawn */
extern FILE     *draw_fp;
static long     stream_start=0;

static int PDF_MOVED=0;

#define CMAPSIZE 256
struct rgb_color {
   unsigned short int red;
   unsigned short int green;
   unsigned short int blue;
};
struct rgb_color PDF_carr[CMAPSIZE];

/******************************************************************************/
static int      PolyLineOpen = FALSE; /* PolyLine not open */
static int      ObjectOpen = FALSE; /* Object not open */
static int      curcol = 0; /* Current pen color (black) */
static int      curwid = 1; /* Current pen color width */
static int      curpat  = 0; /* Current fill pattern*/
static int      pgroup=1; /* groupid reserved for the entire page */
static int      pplus=4; /* starting object ID delta for page contents */
/******************************************************************************/
static float page_dx=0;
static float page_dy=0;
static float page_angle=0;
static float page_xscale=1.0/RAS_PER_PT;
static float page_yscale=1.0/RAS_PER_PT;
/*
Assuming physical units are 72 points per inch, and want to use integers
for coordinates, then making 10 rasters per point gives a resolution of
720 rasters per inch.
*/
static int num_xrefs=0;
static long *xrefs=NULL;
/******************************************************************************/
static int PDFprint(char *s) { /* print string to FILE draw_fp preceding ()\ by \ */
        char c;
        while ((c = *s++)){
                switch(c) {
                case '(':
                case ')':
                case '\\':
                    putc('\\',draw_fp);
                        break;
                default:
                        break;
                }
                    putc(c,draw_fp);
        }
        return(0);
}
/******************************************************************************/
/*
Simple store of character position returned by ftell before each object is
started for printing the reference catalog at the end. If run out of room
in array, allocate a new array and copy old one to it.

Make sure to release this storage when driver is closed.
*/
static void store_start_obj(int id){
   if(id >= num_xrefs) {
      long *new_xrefs;
      int delta, new_num_xrefs;
      delta = num_xrefs / 5;
      if(delta < 1000){
         delta += 1000;
      }
      new_num_xrefs = num_xrefs + delta;
      new_xrefs = (long *)malloc(new_num_xrefs * sizeof(*new_xrefs));
      if(new_xrefs == NULL) {
         fprintf(stderr,
            "*store_start_obj* Unable to allocate object table entry %d.",id);
         exit(1);
      }
      memcpy(new_xrefs, xrefs, num_xrefs * sizeof(*xrefs));
      if(xrefs != NULL) {
         free(xrefs);
      }
      xrefs = new_xrefs;
      num_xrefs = new_num_xrefs;
   }
   xrefs[id] = ftell(draw_fp);
   /*
   fprintf(stderr,"id=%ld,pos=%ld\n",id,xrefs[id]);
   */
   fprintf(draw_fp,"%d 0 obj\n",id);
}
/******************************************************************************/
static int PDF_header() {

   /* extern struct tm *localtime(); */
   struct tm *thetime;
   time_t timebuf;
#ifndef MINGW
   struct utsname unstr, *un;
#endif
   char *username;
   struct passwd *pw;

   fprintf(draw_fp,"%%PDF-1.4\n");
   /*
      Note: If a PDF file contains binary data, as most do , it is
      recommended that the header line be immediately followed by a
      comment line containing at least four binary characters--that is,
      characters whose codes are 128 or greater. This will ensure proper behavior of file
      transfer applications that inspect data near the beginning of a
      file to determine whether to treat the file's contents as text or as binary.
   */
   fprintf(draw_fp,"%%%c%c%c%c\n",128,129,130,131);
   fprintf(draw_fp,"%% PDF: Adobe Portable Document Format\n");


   fprintf(draw_fp,"%% Info Section\n");
   store_start_obj(2);  /* Info is assumed to be object 2 */
   fprintf(draw_fp,"<</Producer(M_DRAW PDF driver 1.0 2005-04-01, John S. Urban)\n");
   fprintf(draw_fp,"  /Title(M_DRAW PDF plot)\n");
   fprintf(draw_fp,"  /Subject(graphic)\n");

/* int tm_min       Minutes after the hour [0-59] */
/* int tm_hour      Hours since midnight [0-23] */
/* int tm_mday      Day of the month [1-31] */
/* int tm_mon       Months since January [0-11] */
/* int tm_year      Years since 1900 */
/* int tm_wday      Days since Sunday [0-6] */
/* int tm_yday      Days since January 1 [0-365] */
/* int tm_isdst     Daylight Saving Time flag */
/* long tm_gmtoff   Seconds east of Greenwich. (Negative indicates west.) */
/* char *tm_zone    Timezone string, for example, GMT */
   timebuf = time(&timebuf);
   thetime = localtime(&timebuf);
   fprintf(draw_fp,"  /CreationDate(D:%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d)\n",
      thetime->tm_year+1900,
      thetime->tm_mon+1,
      thetime->tm_mday,
      thetime->tm_hour,
      thetime->tm_min,
      thetime->tm_sec);
   fprintf(draw_fp,"  /ModDate(D:%4.4d%2.2d%2.2d%2.2d%2.2d%2.2d)\n",
      thetime->tm_year+1900,
      thetime->tm_mon+1,
      thetime->tm_mday,
      thetime->tm_hour,
      thetime->tm_min,
      thetime->tm_sec);

#ifndef MINGW
   un = &unstr; /* initialize the pointer to an address with enough room to store the returned value in */
   uname(un);
   if ((username = getlogin()) == NULL ){
      pw = getpwuid(getuid());
      username = pw->pw_name;
   }
   fprintf(draw_fp,"  /Author(%s)\n",username);
   fprintf(draw_fp,"  /Creator(PID=%ld)\n",(long)getpid());

   fprintf(draw_fp,"  /Keywords(M_DRAW graphics library on OS=");
       PDFprint(un->sysname);
   fprintf(draw_fp,"\n  NETWORK_NAME=");
       PDFprint(un->nodename);
   fprintf(draw_fp,"\n  RELEASE=");
       PDFprint(un->release);
   fprintf(draw_fp,"\n  VERSION=");
       PDFprint(un->version);
   fprintf(draw_fp,"\n  MACHINE=");
       PDFprint(un->machine);
#endif
   fprintf(draw_fp,")>>endobj\n");
   return(0);
}
/******************************************************************************/
/* change index i in the color map to the appropriate rgb value. */
static int PDF_mapcolor(int i, int r, int g, int b) {
   if (i >= CMAPSIZE || i < 0 ){
      return(-1);
   }
   PDF_carr[i].red = (unsigned short)(r);
   PDF_carr[i].green = (unsigned short)(g);
   PDF_carr[i].blue = (unsigned short)(b);
   return(0);
}
/******************************************************************************/
/* PDF_init set up the environment. Returns 1 on success. */
static int PDF_init(void) {
   int prefx, prefy, prefxs, prefys;
   int i;
   int PDF_header();

/******************************************************************************/
/* reinitialize, especially for calls after switching drivers */
   points_printed = 0;
   drawn = 0;
   LAST_X = -1, LAST_Y = -1;/* last (x, y) drawn */
   stream_start=0;
   PolyLineOpen = FALSE; /* PolyLine not open */
   ObjectOpen = FALSE; /* Object not open */
   curcol = 0; /* Current pen color (black) */
   curwid = 1; /* Current pen color width */
   curpat  = 0; /* Current fill pattern*/
   pgroup=1; /* groupid reserved for the entire page */
   pplus=4; /* starting object ID delta for page contents */
   page_dx=0;
   page_dy=0;
   page_angle=0;
   page_xscale=1.0/RAS_PER_PT;
   page_yscale=1.0/RAS_PER_PT;
   num_xrefs=0;
   xrefs=NULL;
   draw_fp = _draw_outfile();

   if (!PDF_first_time) return(1);

   PDF_header();

   draw_getprefposandsize(&prefx, &prefy, &prefxs, &prefys);

   if (prefxs != -1 ) {
      vdevice.sizeSy = prefys;
      vdevice.sizeSx = prefxs;
      vdevice.sizeX = vdevice.sizeY = MIN(prefys, prefxs );
   } else{
      vdevice.sizeSy = PDF_YSIZE*RAS_PER_PT;  /* size in resolution rasters */
      vdevice.sizeSx = PDF_XSIZE*RAS_PER_PT;  /* size in resolution rasters */
      vdevice.sizeX = vdevice.sizeY = MIN(PDF_XSIZE*RAS_PER_PT,PDF_YSIZE*RAS_PER_PT); /* current viewport to use */
   }
   fprintf(draw_fp,"%% width=%fin height=%fin \n",
      (float)vdevice.sizeSx/72.0/RAS_PER_PT,
      (float)vdevice.sizeSy/72.0/RAS_PER_PT);
   fprintf(draw_fp,"%% width=%frasters height=%frasters \n",
      (float)vdevice.sizeSx,
      (float)vdevice.sizeSy);

   fprintf(draw_fp,"%% create contents for page=%d\n", pgroup);
   /*
   An indirect reference to object FOLLOWING this stream
   */
   store_start_obj((pgroup-1)*3+pplus);
   fprintf(draw_fp,"<< /Length %d 0 R>>%% length follows stream\n",(pgroup-1)*3+pplus+1);

   fprintf(draw_fp,"stream\n");
   stream_start=(long)ftell(draw_fp);
   fprintf(draw_fp,"q\n");  /* save graphics state onto stack */

/*
� Translations are specified as   [1 0 0 1 tx ty ], where tx and ty are the distances
           to translate the origin of the coordinate system in the horizontal and vertical
           dimensions, respectively.
� Scaling is obtained by     [ sx 0 0 sy 0 0 ]. This scales the coordinates so that 1
           unit in the horizontal and vertical dimensions of the new coordinate system is
           the same size as sx and sy units, respectively, in the previous coordinate system.
� Rotations are produced by [ cos  sin  -sin  cos  0 0 ], which has the effect
           of rotating the coordinate system axes by an angle  counterclockwise.
� Skew is specified by [1 tan  tan  1 0 0 ], which skews the x axis by an angle
            and the y axis by an angle .
*/



   fprintf(draw_fp,"1 0 0 1 %f %f cm %% Translate\n",page_dx,page_dy);
   fprintf(draw_fp,"%f %f %f %f 0 0 cm %% Rotate\n",
   cos(page_angle),
   sin(page_angle),
   -sin(page_angle),
   cos(page_angle));
   fprintf(draw_fp,"%f 0 0 %f 0 0 cm %% Scale\n",page_xscale,page_yscale);
   fprintf(draw_fp,"1 j\n"); /* line cap circular */
   fprintf(draw_fp,"1 J\n"); /* line join circular */
   fprintf(draw_fp,"%d w\n",(int)(curwid*vdevice.sizeX/10000)); /* line width */

   vdevice.depth = 8;
   for (i = 0; i < CMAPSIZE; i++) /* set up the basic colors */
   {
      PDF_carr[i].red=255;
      PDF_carr[i].green=255;
      PDF_carr[i].blue=255;
   }

   PDF_mapcolor(0, 255, 255, 255);
   PDF_mapcolor(1, 255, 0, 0);
   PDF_mapcolor(2, 0, 255, 0);
   PDF_mapcolor(3, 255, 255, 0);
   PDF_mapcolor(4, 0, 0, 255);
   PDF_mapcolor(5, 255, 0, 255);
   PDF_mapcolor(6, 0, 255, 255);
   PDF_mapcolor(7, 0, 0, 0);

   PDF_mapcolor( 8, 155, 0, 0);
   PDF_mapcolor( 9, 0, 155, 0);
   PDF_mapcolor(10, 155, 255, 255);
   PDF_mapcolor(11, 155, 155, 0);
   PDF_mapcolor(12, 0, 0, 155);
   PDF_mapcolor(13, 155, 0, 155);
   PDF_mapcolor(14, 0, 155, 155);
   PDF_mapcolor(15, 100, 100, 100);
   PolyLineOpen = FALSE; /* Polyline not open */
   ObjectOpen = FALSE; /* Object not open */
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
/* No "point" object in PDF; null lines do not show up at all with default line terminators.  */

   /*int half_box;*/
   if(PolyLineOpen){
      if(PDF_MOVED == 0 ){ /* ASSUME NULL LINES ARE DOTS OR POINTS */

      /*
          Does not seem needed for PDF using Adobe, xpdf gv viewers
          fprintf(draw_fp,"\nS %% circle cx=\"%d\" cy=\"%d\" r=\"%d\"\n",LAST_X,LAST_Y,vdevice.sizeX*curwid/1000/2);
       */
         fprintf(draw_fp, " S\n");  /* end and stroke curve*/

      } else{
         fprintf(draw_fp, " S\n");  /* end and stroke curve*/
      }
      PolyLineOpen = FALSE; /* Polyline not open */
   }
   points_printed = 0;
   LAST_X=-1, LAST_Y=-1;
   return (0);
}
/******************************************************************************/
static int closeObject(void){
   if(ObjectOpen){
      ObjectOpen = FALSE; /* object not open */
      points_printed = 0;
   }
   return (0);
}
/******************************************************************************/
static int openline(void){
   if(!ObjectOpen){
      ObjectOpen = TRUE; /* Object open */
   }
   if(!PolyLineOpen){
      PolyLineOpen = TRUE; /* Polyline open */
   }
   return (0);
}
/******************************************************************************/
static int openObject(void){
   if(!ObjectOpen){
      ObjectOpen = TRUE; /* Object open */
   }
   return (0);
}
/******************************************************************************/
/* PDF_exit do a flush and close the output file if necessary.  */
static int PDF_exit(void) {
   int i;
   int STARTXREF;
   long POS;
   closeline(); /* close Polyline line if it is open */
   closeObject(); /* close object if it is open */
   fprintf(draw_fp,"Q");
   POS=ftell(draw_fp);
   fprintf(draw_fp,"\nendstream\n");
   fprintf(draw_fp,"endobj\n");

   store_start_obj((pgroup-1)*3+pplus+1);
   fprintf(draw_fp,"%ld endobj",(long)POS-stream_start);
   fprintf(draw_fp,"%% size of stream, start=%ld end=%ld\n",stream_start,(long)POS);

   store_start_obj((pgroup-1)*3+pplus+2);
   fprintf(draw_fp,"<</Type/Page/MediaBox[ 0 0 %d %d ]/Parent 3 0 R\n",vdevice.sizeSx/RAS_PER_PT,vdevice.sizeSy/RAS_PER_PT);
   fprintf(draw_fp,"/Resources <</ProcSet[/PDF/Text]/Font<<\n");
   /* one for each basefont */
   fprintf(draw_fp,"   /F1<</Type/Font/Subtype/Type1/BaseFont/Courier/Encoding/WinAnsiEncoding\n");
   fprintf(draw_fp," /FirstChar 32/LastChar 251/Widths[\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600 600\n");
   fprintf(draw_fp," 600 600 600 600 600 600 600 600 600 600 600 600]>>\n");
   fprintf(draw_fp,"   /F2<</Type/Font/Subtype/Type1/BaseFont/Helvetica/Encoding/WinAnsiEncoding\n");
   fprintf(draw_fp," /FirstChar 32/LastChar 251/Widths[\n");
   fprintf(draw_fp," 278 333 474 556 556 889 722 278 333 333 389 584 278 333 278 278\n");
   fprintf(draw_fp," 556 556 556 556 556 556 556 556 556 556 333 333 584 584 584 611\n");
   fprintf(draw_fp," 975 722 722 722 722 667 611 778 722 278 556 722 611 833 722 778\n");
   fprintf(draw_fp," 667 778 722 667 611 722 667 944 667 667 611 333 278 333 584 556\n");
   fprintf(draw_fp," 278 556 611 556 611 556 333 611 611 278 278 556 278 889 611 611\n");
   fprintf(draw_fp," 611 611 389 556 333 611 556 778 556 556 500 389 280 389 584 278\n");
   fprintf(draw_fp," 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278\n");
   fprintf(draw_fp," 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278\n");
   fprintf(draw_fp," 278 333 556 556 167 556 556 556 556 238 500 556 333 333 611 611\n");
   fprintf(draw_fp," 278 556 556 556 278 278 556 350 278 500 500 556 1000 1000 278 611\n");
   fprintf(draw_fp," 278 333 333 333 333 333 333 333 333 278 333 333 278 333 333 333\n");
   fprintf(draw_fp," 1000 278 278 278 278 278 278 278 278 278 278 278 278 278 278 278\n");
   fprintf(draw_fp," 278 1000 278 370 278 278 278 278 611 778 1000 365 278 278 278 278\n");
   fprintf(draw_fp," 278 889 278 278 278 278 278 278 278 611 944 611]>>\n");
   fprintf(draw_fp,">> >>/Contents %d 0 R>> endobj\n", (pgroup-1)*3+pplus);

   store_start_obj(3);
   fprintf(draw_fp," << /Type /Pages /Count %d\n",pgroup);
   fprintf(draw_fp,"/Kids [\n");
   for(i=1;i<=pgroup;i++){
      fprintf(draw_fp,"%d 0 R\n",(i-1)*3+6);
   }
   fprintf(draw_fp,"]\n");

   fprintf(draw_fp,">>endobj\n");

   store_start_obj(1);
   fprintf(draw_fp," << /Type /Catalog /Pages 3 0 R>> endobj\n");

   STARTXREF=ftell(draw_fp);
   fprintf(draw_fp,"xref\n");                              /* Index for PDF */
   fprintf(draw_fp,"0 %d\n",(pgroup-1)*3+pplus+3);               /* size of index */
   fprintf(draw_fp,"0000000000 65535 f \n");   /* */
   for(i=1;i<(pgroup)*3+pplus;i++){
      fprintf(draw_fp, "%10.10ld 00000 n \n",xrefs[i]); /* */
   }
   /* note Root and Info are not relative, assume obj1 is root, obj2 is info */
   fprintf(draw_fp,"trailer << /Size %d /Root 1 0 R /Info 2 0 R>>\n",
      (pgroup)*3+pplus);  /* */
   fprintf(draw_fp,"startxref\n"); /* */
   fprintf(draw_fp,"%4.4d\n",STARTXREF);   /* */
   fprintf(draw_fp,"%%%%EOF\n");     /* */

   drawn = 0;
   points_printed = 0;

   fflush(draw_fp);
   if (draw_fp != stdout && draw_fp != stderr ){
                if(vdevice.writestoprocess == 2){
                   pclose(draw_fp);
                }else{
                   fclose(draw_fp);
                }
   }
   if(num_xrefs != 0){
      if(xrefs != NULL) {
         free(xrefs);
      }
      num_xrefs=0;
   }
   return (0);
}
/******************************************************************************/
/* draw to an x, y point.  */
/* Note: (0, 0) is defined as the top left of the window in PDF.  */
static int PDF_draw(int x, int y) {
   static char linefeed[2] = {' ','\n'};

   if (LAST_X != vdevice.cpVx || LAST_Y != vdevice.cpVy ){
      closeline(); /* close line if required */
      openObject(); /* start Object if required */
      openline(); /* start line */
      fprintf(draw_fp, "%d %d m", vdevice.cpVx, vdevice.cpVy);
      LAST_X=vdevice.cpVx;
      LAST_Y=vdevice.cpVy;
      PDF_MOVED=0;
      points_printed = 1;
   }
   openline(); /* start line if required */

   if(points_printed == 0){
      fprintf(draw_fp, "%d %d m", x ,y);

      PDF_MOVED=0;
   }else{
      if(LAST_X != x || LAST_Y != y)PDF_MOVED=PDF_MOVED+1;

      fprintf(draw_fp, "%c%d %d l", linefeed[(points_printed % 8/7)], x ,y);
   }

   points_printed++;
   LAST_X = x;
   LAST_Y = y;
   drawn = 1;
   return (0);
}
/******************************************************************************/
/* PDF_clear flush the current page without resetting the graphics state */
static int PDF_clear(void) {
   /* fpos_t POS; */
   long POS;
   closeline(); /* close line if required */
   if (drawn)
   {

     fprintf(draw_fp,"Q");  /* restore graphics state from stack */
     POS=ftell(draw_fp);
     fprintf(draw_fp,"\nendstream\n");
     fprintf(draw_fp,"endobj\n");

     store_start_obj((pgroup-1)*3+pplus+1);
     fprintf(draw_fp,"%ld endobj",(long)POS-stream_start);
     fprintf(draw_fp,"%% size of stream, start=%ld end=%ld\n",stream_start,(long)POS);

     store_start_obj((pgroup-1)*3+pplus+2);
     fprintf(draw_fp,"<</Type/Page/MediaBox [ 0 0 %d %d ] /Parent 3 0 R /Resources <</ProcSet[/PDF]>>\n",
        vdevice.sizeSx/RAS_PER_PT, vdevice.sizeSy/RAS_PER_PT);
     fprintf(draw_fp,"/Contents %d 0 R>> endobj\n", (pgroup-1)*3+pplus);
     fprintf(draw_fp,"%% End Page %d\n",pgroup); /* Page Clear, End of Page Group */

     pgroup++; /* increment page id */
     fprintf(draw_fp,"%% id=Page%d\n", pgroup);

     store_start_obj((pgroup-1)*3+pplus);
     fprintf(draw_fp,"<< /Length %d 0 R>> ",(pgroup-1)*3+pplus+1);
     fprintf(draw_fp,"stream\n");
     stream_start=(long)ftell(draw_fp);
     fprintf(draw_fp,"q\n");  /* save graphics state onto stack */
     fprintf(draw_fp,"1 0 0 1 %f %f cm %% Translate\n",page_dx,page_dy);
     fprintf(draw_fp,"%f %f %f %f 0 0 cm %% Rotate\n",
        cos(page_angle),
        sin(page_angle),
        -sin(page_angle),
        cos(page_angle));
     fprintf(draw_fp,"%f 0 0 %f 0 0 cm %% Scale\n",page_xscale,page_yscale);
     fprintf(draw_fp,"1 j\n"); /* line cap circular */
     fprintf(draw_fp,"1 J\n"); /* line join circular */
     fprintf(draw_fp,"%d w\n",(int)(curwid*vdevice.sizeX/10000)); /* line width */
   }
   closeObject(); /* close Object if required */
   drawn = 0;
   points_printed = 0;
   PDF_MOVED=0;
   return(0);
}
/******************************************************************************/
/*
 *      value sets line width
 */
static int PDF_setlw(int width) {
   closeline(); /* close line if required */
   closeObject(); /* close Object if required */
   curwid = abs(width);
   fprintf(draw_fp,"%d w\n",(int)(curwid*vdevice.sizeX/10000)); /* line width */
   return(0);
}
/******************************************************************************/
/* PDF_color change the color of the pen
 *      kludged so negative value sets raster line width
 *      if exceed allowable number of colors maybe pick a line style
 *      or something like a gradient fill style for fun
 */
static int PDF_color(int col) {
   closeline(); /* close line if required */
   closeObject(); /* close Object if required */
   if ( col < 0 )
   {
      PDF_setlw(abs(col));
   } else {
      curpat = col/CMAPSIZE;
      curcol = col % CMAPSIZE;
      fprintf(draw_fp,"%f %f %f RG ",
         PDF_carr[curcol].red/255.0,
         PDF_carr[curcol].green/255.0,
         PDF_carr[curcol].blue/255.0);
      fprintf(draw_fp,"%f %f %f rg\n",
         PDF_carr[curcol].red/255.0,
         PDF_carr[curcol].green/255.0,
         PDF_carr[curcol].blue/255.0);
   }
   return(0);
}
/******************************************************************************/
/* load in small or large - could be improved. Radically KLUDGED; made SoftText extern  */
static int PDF_font(char *fontname) {
   int i;
   float rat;
   /* assuming vdevice.hwidth is the desired width of the reference character,
    * this is a list of percentages of the other character widths.
      | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|
      | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |
      | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|
      | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |
      | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  ' |
      | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |
      | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |
      | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |
      | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |
      | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |
      | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |
      | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |
      | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |
      |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |
      |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |
      |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|

    */
   static int helvetica_w[128] = {
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
      278,333,474,556,556,889,722,278,333,333,389,584,278,333,278,278,
      556,556,556,556,556,556,556,556,556,556,333,333,584,584,584,611,
      975,722,722,722,722,667,611,778,722,278,556,722,611,833,722,778,
      667,778,722,667,611,722,667,944,667,667,611,333,278,333,584,556,
      278,556,611,556,611,556,333,611,611,278,278,556,278,889,611,611,
      611,611,389,556,333,611,556,778,556,556,500,389,280,389,584,278
   };

   /* select Courier for small and Helvetica for big */

   vdevice.attr->a.softtext = SOFTHARDWARE;
   /* textsize will be obeyed after the font is set
    * maybe should read current software size and convert virtual
    * to device instead of resetting each time */

      if(vdevice.hwidth == 0 || vdevice.hheight == 0 ){
         vdevice.hwidth=PDF_XSIZE*RAS_PER_PT;  /* total drawing area size in x direction */
         vdevice.hheight=PDF_YSIZE*RAS_PER_PT; /* total drawing area size in y direction */
      }

   /* fprintf(stderr,"*PDF_font* vdevice.hwidth=%f",vdevice.hwidth); */
   /* fprintf(stderr," vdevice.hheight=%f\n",vdevice.hheight); */

   if (strcmp(fontname, "small") == 0) {
      strncpy(hardfont,"/F1",30);
         rat=0.072; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=600.0/72.0 * rat; /* ratio of character width to vdevice.hwidth*/
         }
   } else if (strcmp(fontname, "large") == 0) {
      strncpy(hardfont,"/F2",30);
      rat=0.072; /* Kludge Factor */
      for (i = 0; i < 128; i++){
         hardwidth[i]=((float)helvetica_w[i])/72.0 * rat; /* ratio of character width to vdevice.hwidth*/
      }
   } else{
      return(0);
   }

   return(1);
}
/******************************************************************************/
/* PDF_string output a string.  */
static int PDF_string(char *s) {
   char    c;
   int     i;
   float   slength;
   float rot; /* rotation angle of path of text in degrees */
   float fudge;

   closeline(); /* close line if required */

   rot=r2d(atan2((double)vdevice.attr->a.textsin,(double)vdevice.attr->a.textcos));
   rot=atan2((double)vdevice.attr->a.textsin,(double)vdevice.attr->a.textcos);

   fprintf(draw_fp,"BT %s", hardfont);                  /* Font family /F1 or /F2 */
   fprintf(draw_fp," %d Tf ", (int)vdevice.hheight);                  /* Font size */
   /* M_DRAW currently does not use baseline, it uses bottom of font box. Should change */
   fudge=vdevice.cpVy+0.22*vdevice.hheight;
   if(rot != 0){
      fprintf(draw_fp,"%f %f %f %f ",cos(rot),sin(rot),-sin(rot),cos(rot)); /* Rotate */
      fprintf(draw_fp,"%d %d Tm(",(int)vdevice.cpVx,(int)fudge);  /* Translate x, y position for start of string */
   }else{
      fprintf(draw_fp,"%d %d Td(",
         (int)vdevice.cpVx,                 /* x position for start of string */
         (int)fudge);                /* y position for start of string */
   }

   for(i=0; (c=s[i]) != '\0' ;i++) {
      switch(c) {   /* Do I need to expand strings like </text> or ; to &amp; or something? */
      case '(' : fprintf(draw_fp, "\\(");   break;
      case ')' : fprintf(draw_fp, "\\)");   break;
      case '\\': fprintf(draw_fp, "\\\\"); break;
      default  : fprintf(draw_fp, "%c",c);
      }
   }
   fprintf(draw_fp,")Tj ");
   /*
   if(rot != 0){
      fprintf(draw_fp,"1 0 0 1 0 0 Tm\n");
   }
   */
   fprintf(draw_fp,"ET\n");
   drawn = 1;
   /* This position needs made accurate */
   slength=strlen(s);
   LAST_X=vdevice.cpVx+slength*vdevice.attr->a.textcos;
   LAST_Y=vdevice.cpVy+slength*vdevice.attr->a.textsin;
   return(0);

}
/******************************************************************************/
static int PDF_char(char c){ /* PDF_char output a character */
   char  s[2];
   s[0] = c;
   s[1]='\0';
   PDF_string(s);
   return(0);
}
/******************************************************************************/
static int PDF_fill(int n, int x[], int y[]) { /* fill a polygon */
/* Maybe should leave edge off stroke:none */
   int     i;
   static char linefeed[2] = {' ','\n'};
   closeline(); /* close line if required */
   closeObject(); /* close line if required */

   fprintf(draw_fp,"%d %d m\n", x[0],y[0]);

   for (i = 1; i < n; i++)
   {
      fprintf(draw_fp, "%d %d l%c", x[i], y[i],linefeed[(i % 8/7)]);
   }

   /* close path */
   fprintf(draw_fp, "%d %d l%c", x[0], y[0],linefeed[(i % 8/7)]);
   fprintf(draw_fp, "f*\n");

   vdevice.cpVx = x[n - 1]; /* update current position */
   vdevice.cpVy = y[n - 1];

   LAST_X = vdevice.cpVx;
   LAST_Y = vdevice.cpVy;
   drawn = 1;
   return(0);
}
/******************************************************************************/
/* no operations - do nothing but return -1 */
static int noop(void) { return(-1); }
static int noop2(int *x, int *y) { return(-1); }
/******************************************************************************/
static DevEntry pdfdev = {
   "pdf",        /* name of device */
   "large",      /* name of large font */
   "small",      /* name of small font */
   noop,         /* Set drawing in back buffer */
   PDF_char,     /* Draw a hardware character */
   noop,         /* Check if a key was hit */
   PDF_clear,    /* Clear the screen to current color */
   PDF_color,    /* Set current color */
   PDF_draw,     /* Draw a line */
   PDF_exit,     /* Exit graphics */
   PDF_fill,     /* Fill a polygon */
   PDF_font,     /* Set hardware font */
   noop,         /* Set drawing in front buffer */
   noop,         /* Wait for and get the next key hit */
   PDF_init,     /* Initialize the device */
   noop2,        /* Get mouse/cross hair position */
   PDF_mapcolor, /* Set color indices */
   PDF_setlw,    /* Set line width */
   PDF_string,   /* Draw a hardware string */
   noop,         /* Swap front and back buffers */
   noop          /* Syncronize the display */
};
/******************************************************************************/
/* copy the pdf device into vdevice.dev.  */
int _PDF_draw_devcpy(void) {
   vdevice.dev = pdfdev;
   return(0);
}
/******************************************************************************/

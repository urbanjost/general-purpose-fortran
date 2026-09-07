#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "draw.h"
static float atx;
static float aty;

static float littlebar=8.0;        /* these numbers are arbitrary,*/
                                   /* as long as the ratio*/
static float bigbar=20.0;          /* stays between 2:1 and 3:1 */

static float ysize=75.0;
/*------------------------------------------------------------------------------*/
void SetBarcodeSize(float narrow,float wide, float tall){
   littlebar=narrow;
   bigbar=wide;
   ysize=tall;
}
/*------------------------------------------------------------------------------*/
void SetBarcodeStart(float xstart,float ystart){
   atx=xstart;
   aty=ystart;
}
/*------------------------------------------------------------------------------*/
/*
   @(#) M_drawplus::barcode(3c): draw 3-of-9 bar code
   given an ASCII string, do what's necessary. tack on delimiter '*'s
*/
void PrintBarcode(char *string)
{
        int x;
        void PutBars(char c);

        PutBars('*');   /* starting delimiter */
        for(x=0;string[x];x++){
                PutBars(string[x]);     /* do each char */
        }
        PutBars('*');   /* ending delimiter */
}
/*------------------------------------------------------------------------------*/
/*
   low-level M_DRAW graphics commands to draw filled rectangles
*/
void DoRectangle(float atx,float xsize,float aty,float ysize){
        draw_pushattributes();
           draw_polyfill(1); /* true*/
            draw_rect(atx,aty,atx+xsize,aty+ysize);
           draw_polyfill(0); /* false*/
           /*fprintf(stdout,"%f %f %f %f\n",atx,aty,atx+xsize,aty+ysize);*/
        draw_popattributes();
}
/*------------------------------------------------------------------------------*/
/*
   given the thick/thin map of 9 0's and 1's, order up some rectangles
   of the appropriate sizes and positions
*/
void BarChar(char *mapstring){
        int x;

        float barsize;

        for(x=0;x < 9;x++){
                if(mapstring[x]=='0'){
                        /* thin one */
                        barsize=littlebar;
                } else {
                        /* thick one */
                        barsize=bigbar;
                }
                if(x%2){
                        /* gap - do nothing */
                } else {
                        /* black bar - draw it */
                        DoRectangle(atx,barsize,aty,ysize);
                }
                atx += barsize; /* move over that much */
        }
        atx += littlebar;       /* gap between each bar code character */
}
/*------------------------------------------------------------------------------*/
/*
   determine the actual thick/thin pattern for the submitted character
*/
void PutBars(char c){
        /*fprintf(stdout,"=================== %c\n",c);*/
        switch(c){
                case '0': BarChar("000110100");break;
                case '1': BarChar("100100001");break;
                case '2': BarChar("001100001");break;
                case '3': BarChar("101100000");break;
                case '4': BarChar("000110001");break;
                case '5': BarChar("100110000");break;
                case '6': BarChar("001110000");break;
                case '7': BarChar("000100101");break;
                case '8': BarChar("100100100");break;
                case '9': BarChar("001100100");break;
                case '-': BarChar("010000101");break;
                case '.': BarChar("110000100");break;
                case ' ': BarChar("011000100");break;
                case '+': BarChar("010001010");break;
                case '%': BarChar("000101010");break;
                case '$': BarChar("010101000");break;
                case '/': BarChar("010100010");break;
                case 'A':
                case 'a': BarChar("100001001");break;
                case 'B':
                case 'b': BarChar("001001001");break;
                case 'C':
                case 'c': BarChar("101001000");break;
                case 'D':
                case 'd': BarChar("000011001");break;
                case 'E':
                case 'e': BarChar("100011000");break;
                case 'F':
                case 'f': BarChar("001011000");break;
                case 'G':
                case 'g': BarChar("000001101");break;
                case 'H':
                case 'h': BarChar("100001100");break;
                case 'I':
                case 'i': BarChar("001001100");break;
                case 'J':
                case 'j': BarChar("000011100");break;
                case 'K':
                case 'k': BarChar("100000011");break;
                case 'L':
                case 'l': BarChar("001000011");break;
                case 'M':
                case 'm': BarChar("101000010");break;
                case 'N':
                case 'n': BarChar("000010011");break;
                case 'O':
                case 'o': BarChar("100010010");break;
                case 'P':
                case 'p': BarChar("001010010");break;
                case 'Q':
                case 'q': BarChar("000000111");break;
                case 'R':
                case 'r': BarChar("100000110");break;
                case 'S':
                case 's': BarChar("001000110");break;
                case 'T':
                case 't': BarChar("000010110");break;
                case 'U':
                case 'u': BarChar("110000001");break;
                case 'V':
                case 'v': BarChar("011000001");break;
                case 'W':
                case 'w': BarChar("111000000");break;
                case 'X':
                case 'x': BarChar("010010001");break;
                case 'Y':
                case 'y': BarChar("110010000");break;
                case 'Z':
                case 'z': BarChar("011010000");break;

                /* an asterisk and everything else! */
                default:  BarChar("010010100");break;
        }
}
/*------------------------------------------------------------------------------*/
void barcode(
      float xcorner,
      float ycorner,
      float xsmall,
      float xlarge,
      float ysize,
      char *string
   ){
   SetBarcodeSize(xsmall,xlarge,ysize);
   SetBarcodeStart(xcorner,ycorner);
   PrintBarcode(string);
}
/*------------------------------------------------------------------------------*/

void barcode_sun_f(
      float *xcorner,
      float *ycorner,
      float *xsmall,
      float *xlarge,
      float *ysize,
      char *s,
      int len
   ){
        char            buf[BUFSIZ];
        register char   *p;

        strncpy(buf, s, len);
        buf[len] = 0;

        for (p = &buf[len - 1]; *p == ' '; p--)
                ;

        *++p = 0;

        barcode(*xcorner,*ycorner,*xsmall,*xlarge,*ysize,buf);
}
/*------------------------------------------------------------------------------*/

#define MAX(x,y)  ((x) > (y) ?  (x) : (y))
#define MIN(x,y)  ((x) < (y) ?  (x) : (y))
#define ABS(x)    ((x) <   0 ? -(x) : (x))
#ifndef PI
#define PI 3.14159265358979323846264338327950288419716939937510
#endif

/*============================================================================*/
#ifdef TESTPRGC
void main()
{
   char device[10];

   int wide=640, tall=640, rows, xoff, yoff, box_sz;

   long int i20, i30, ncols, nrows, ilines;

   float  bottom, left, sun_radius, planet_radius, planet_offset;

   extern void draw_spirograph();
   extern void draw_vexit();

   draw_prefsize(wide,tall);

   fprintf(stderr,"Enter output device: ");
   gets(device);
   draw_vinit(device);
   draw_ortho2(0.0, (float)wide, 0.0, (float)tall);

   /*linewidth(3);*/ /* really slows down pbm driver because all lines are polygons */
   /*linewidth(1); */
   draw_polyfill(1);
   draw_vsetflush(0);
   draw_color(BLACK);
   draw_clear();
   draw_color(WHITE);

   rows=1;
   box_sz=MIN(wide,tall)/rows;       /* size of biggest box to use and get specified number of rows */
   nrows = (int)tall/box_sz;         /* number of rows of objects to draw */
   ncols = (int)wide/box_sz;         /* number of columns of objects to draw */
   xoff = (wide - ncols * box_sz)/2; /* initial x offset to begin row at to center drawings */
   yoff = (tall - nrows * box_sz)/2; /* initial x offset to begin column at to center drawings */

   sun_radius = 148;
   planet_radius = 1;

   for ( ilines = 1; ilines <= 300; ilines++ ){
      for( i20 = 1; i20 <= ncols; i20++ ){
         left = (i20-1)*box_sz+xoff;
         for( i30 = 1; i30 <= nrows; i30++ ){
            bottom = (i30-1)*box_sz+yoff;
            draw_color(BLACK);
            draw_rect(left,bottom,left+box_sz,bottom+box_sz);
            draw_color(WHITE);
            planet_offset= sun_radius;
            spirograph(left + box_sz/2.0,bottom + box_sz/2.0,sun_radius,planet_radius,planet_offset,box_sz/2.0,ilines);
            draw_vflush();
            draw_getkey();
            ilines++;
         }
      }
      draw_vflush();
      draw_getkey();
   }
   draw_vflush();
   draw_getkey();
   draw_vexit();
}
#endif
/*============================================================================*/
void spirograph(xcenter, ycenter, sun_radius, planet_radius, planet_offset, radius, ilines)
float xcenter, ycenter, sun_radius, planet_radius, planet_offset, radius;
long int ilines;
{
   long int i;
   float a, b, c, con1, con2, factor, rlines, u ;

   float   polyxy[5000][2];

   fprintf(stderr,"CENTER = %f %f\n",xcenter,ycenter);
   fprintf(stderr,"CONFIGURATION RADIUS = %f %f %f\n",sun_radius,planet_radius,planet_offset);
   fprintf(stderr,"FIT RADIUS = %f \n",radius);
   fprintf(stderr,"LINES = %ld\n",ilines);
   c = sun_radius;
   b = planet_offset;
   a = planet_radius;
   rlines = ilines;
   factor = radius/(c - a + b);
   c = factor*c;
   a = factor*a;
   b = factor*b;
   ilines = rlines;
   con1 = PI*2.*(c/a)/rlines;
   u = 0;
   con2 = (1 - a/c)*u;
   polyxy[0][0] = (c - a)*cos( a*u/c ) + b*cos( con2 ) + xcenter;
   polyxy[0][1] = (c - a)*sin( a*u/c ) - b*sin( con2 ) + ycenter;

   for ( i = 1; i <= ilines; i++ )
   {
      u = con1*i;
      con2 = (1 - a/c)*u;
      if( con2 >=  16777216. )
      {
         con2 = fmod( con2, PI );
      }
      polyxy[i][0] = (c - a)*cos( a*u/c ) + b*cos( con2 ) + xcenter;
      polyxy[i][1] = (c - a)*sin( a*u/c ) - b*sin( con2 ) + ycenter;
   }
   if (ilines >= 0){
        draw_poly2(ilines,polyxy);
   }
   return;
}

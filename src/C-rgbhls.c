/*============================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>

void rgbhls(float r,float g,float b,float *h,float *l,float *s)
{
   /*
   calculates a hue, lightness, saturation value for a color given in
   red, green, and blue components.
   
   given  : r, g, b each as a value of 0 to 100.
   desired: h as a value of 0 to 360 degrees, l and s 
            each as a value of 0 to 100.
   
   this particular algorithm was taken from Foley and Van Dam.
   */
   float clrmax,clrmin,clrdel,clrsum,rr,gg,bb;
   r /= 100.;
   g /= 100.;
   b /= 100.;

   clrmax = r;
   if(g > clrmax) clrmax = g;
   if(b > clrmax) clrmax = b ;

   clrmin = r;
   if(g < clrmin) clrmin = g;
   if(b < clrmin) clrmin = b ;

   clrdel=clrmax-clrmin;
   clrsum=clrmax+clrmin;
   *l = clrsum/2.;

   if(clrdel != 0.) {
      rr=(clrmax-r)/clrdel;
      gg=(clrmax-g)/clrdel;
      bb=(clrmax-b)/clrdel;

      if(*l <= 0.5){
         *s=clrdel/clrsum;
      } else{
         *s=clrdel/(2.-clrsum);
      }

      if     (r == clrmax) {
         *h=bb-gg;
      } else if(g == clrmax) {
         *h=2.+rr-bb;
      } else if(b == clrmax) {
         *h=4.+gg-rr;
      }

      *h *= 60.;
      if(*h < 0.) *h += 360.;

   } else{
      *s=0.;
      *h=0.;
   }
   *l *= 100.;
   *s *= 100.;
}

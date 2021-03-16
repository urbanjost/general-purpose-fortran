/*============================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>

void hvsrgb(float h, float v, float s, float *r, float *g, float *b)
{
   /*
           this procedure calculates the red, green, blue equivalent
           for a color given in hue, saturation, & value components.
           given  : h as value of 0 to 360 degrees.
                    s and v each as a value of 0 to 100.
           desired: r, g, and b as a value of 0 to 100.
           this particular algorithm was taken from foley and van dam.
           */
   int ifloor;
   float    f,p,q,t;
   v /= 100.0;
   s /= 100.0;
   if(s == 0.0) {
      *r=v;
      *g=v;
      *b=v;
   }

   if(h == 360.0) h=0.0;
   h /= 60.0;
   ifloor=h;
   f=h-ifloor;
   p=v*(1.0-s);
   q=v*(1.0-(s*f));
   t=v*(1.0-(s*(1-f)));
   if(ifloor == 0) {
      *r=v;
      *g=t;
      *b=p;
   } else if(ifloor == 1) {
      *r=q;
      *g=v;
      *b=p;
   } else if(ifloor == 2) {
      *r=p;
      *g=v;
      *b=t;
   } else if(ifloor == 3) {
      *r=p;
      *g=q;
      *b=v;
   } else if(ifloor == 4) {
      *r=t;
      *g=p;
      *b=v;
   } else if(ifloor == 5) {
      *r=v;
      *g=p;
      *b=q;
   }
   *r *= 100.0;
   *g *= 100.0;
   *b *= 100.0;
}

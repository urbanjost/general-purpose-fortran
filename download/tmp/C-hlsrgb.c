/*============================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>

void hlsrgb(float hue,
            float lightness,
            float saturation,
            float *red,
            float *green,
            float *blue)
{
/*
   this procedure calculates the red, green, & blue components for a
   color given in hue, lightness, & saturation values.
             given  : hue as a value of 0 to 360 degrees.
                      lightness and saturation each as a value of 0 to 100.
             desired: red, green, and blue each as a value of 0 to 100.

  this algorithm based on Foley and Van Dam.
*/
   float rgbval(float color1,float color2,float h);
   float clr1, clr2;

   lightness /= 100.0;
   saturation /= 100.0;

   if(saturation == 0.0){
      *red = lightness;
      *green = lightness;
      *blue = lightness;
   }

   if(lightness <= 0.5){
      clr2=lightness*(1.0+saturation);
   } else{
      clr2=lightness+saturation-lightness*saturation;
   }

   clr1=2.0*lightness-clr2;

   *red=rgbval(clr1,clr2,hue+120.0);
   *green=rgbval(clr1,clr2,hue);
   *blue=rgbval(clr1,clr2,hue-120.0);

   *red *= 100.0;
   *green *= 100.0;
   *blue *= 100.0;

   return;
}

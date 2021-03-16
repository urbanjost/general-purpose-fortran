/*============================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>

void rgbhvs(float red,float green,float blue,float *hue,float *value,float *saturation) {
/*
   calculate a hue, saturation, value equivalent for a color given in
   red, green, & blue components.
           
      given  : red, green, blue each as a value of 0 to 100.
      desired: hue as a value of 0 to 360 degrees,
                saturation and value (0 to 100)
           
   Algorithm based on Fundamentals of Interactive Computer Graphics,
   Foley and Van Dam.
*/
   float color_max, color_min, color_delta, rr, gg, bb;
   red /= 100.0;
   green /= 100.0;
   blue /= 100.0;

   color_max = red;
   if(green > color_max) color_max = green;
   if(blue  > color_max) color_max = blue ;

   color_min = red;
   if(green < color_min) color_min = green;
   if(blue  < color_min) color_min = blue ;

   color_delta=color_max-color_min;

   *value=color_max;
   if(color_max != 0.0){
      *saturation=color_delta/color_max;
   }else{
      *saturation=0.0;
   }

   if(*saturation != 0.0){
      rr=(color_max-red)/color_delta;
      gg=(color_max-green)/color_delta;
      bb=(color_max-blue)/color_delta;
      if(red == color_max){
         *hue=bb-gg;
      } else if(green == color_max){
         *hue=2.0+rr-bb;
      } else if(blue == color_max){
         *hue=4.0+gg-rr;
      } else{
        fprintf(stderr,"*rgbhvs* error: internal - no color is maximum!\n");
      }

      *hue *= 60.0;
      if(*hue < 0.0) *hue += 360.0;
   }
   *value *= 100.0;
   *saturation *= 100.0;
   return;
}

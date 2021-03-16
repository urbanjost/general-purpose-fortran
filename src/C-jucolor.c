/*
   This is heavily based on chapter 17 of  "Fundamentals  of Interactive
   Computer   Graphics";   J.  D.  Foley   and  A.  Van   Dam;  and  was
   modeled on FORTRAN  routines  created by Kevin
   Kendall to help ease  program  conversions  for a family of  in-house
   codes.
                                John S. Urban
                        19941026

   Color is a complex  topic; but it is often  much more  convenient  to
   specify colors using  something other than the RGB color model.  Some
   of the most common models are:
      RGB - color TV monitors
      YIQ - Broadcast TV color system
      CMY - Cyan, Magenta, Yellow : pigment-based printing devices
      HLS - Hue, Lightness, Saturation
      HSV - Hue, Saturation, Value

   As many sources indicate, color conversions  are a tricky  business;
   but these  simplified  conversions  work  quite  well for most  basic
   needs.

   All calls are  expected  to be made  thru jucolor.  If you wish to call the
   support routines directly to get performance gains it is up to you to
   ensure that the input data is properly conditioned.

*/
/*============================================================================*/
#include <stdio.h>
#include <string.h>
void jucolor(modei,clr1i,clr2i,clr3i,modeo,clr1o,clr2o,clr3o,status)
   char *modei ,*modeo;
   float clr1i,clr2i,clr3i;
   float *clr1o,*clr2o,*clr3o;
   int *status;
{
char ident[] = "@(#)jucolor(3c):this procedure converts a color's components from one color model to another";
   /*
   
   this procedure converts a color's components from one color model to
   another.  it supports the following color models:
   .    hls  -  hue, lightness, saturation
   .    hvs  -  hue, value,     saturation
   .    rgb  -  red, green,     blue
   .    yiq  -   ?    ?          ?
   
   modei,  clr1i,  clr2i,  clr3i & modeo are input values to this procedure.
   status, clr1o,  clr2o & clr3o are output values from this procedure.
   
   modei and modeo are character variables, the others are floats.
   
   modei specifies the color model that applies to the input color
         components  clr1i, clr2i, & clr3i.

   modeo specifies the color model desired  the output color
         components  clr1o, clr2o, & clr3o.
   
   valid values  modei and modeo as well as the corresponding
   meanings  clr1_, clr2_, and clr3_  are as shown below:
   .    mode      clr1       clr2         clr3
   .    'hls'     hue        lightness    saturation
   .    'hsl'     hue        saturation   lightness
   .    'hvs'     hue        value        saturation
   .    'hsv'     hue        saturation   value
   .    'rgb'     red        green        blue
   .    'yiq'      ?          ?            ?
   
   lightness, value, saturation, red, green, blue, & y range from 0 to 100,
   hue ranges from 0 to 360 degrees,
   i   ranges from -60 to 60,
   q   ranges from -52 to 52
   
   as a minimum, this procedure equates the output color values to the
   input color values.
   
   status is returned to signal the following conditions:
   .    -1   modei = modeo, so no substantial conversion was done,
   .     1   one of the input color values was outside the allowable range,
   .     2   modei was invalid
   .     3   modeo was invalid
   */
   char modeiu[3],modeou[3];
   float c1   ,c2   ,c3;
   float r    ,g    ,b;

/*
   void hlsrgb(float hue, 
               float lightness,
               float saturation,
               float *red,
               float *green,
               float *blue);
   void hvsrgb(float h,
               float v,
               float s,
               float *r,
               float *g,
               float *b);
   void rgbhls(float r,
               float g,
               float b,
               float *h,
               float *l,
               float *s);
*/
   void hlsrgb( );
   void hvsrgb( );
   void rgbhls( );
   void rgbhvs( );

   /* reset the status flag. */
   *status=0;
   /* as a minimum, set the output colors equal to the input colors. */
   *clr1o=clr1i;
   *clr2o=clr2i;
   *clr3o=clr3i;
   /* ensure that the input character strings are uppercase */
   strncpy(modeiu,modei,3);
   strncpy(modeou,modeo,3);
   /* check  a trivial instance. */
   if(strncmp(modeiu,modeou,3) == 0)
   {
      *status = -1;
      return;
   }
   /* check  a transpose of terms, another trivial instance. */
   if(modeiu[0] == 'h')
   {
      if( (strncmp(modeiu,"hls",3) == 0)  && (strncmp(modeou,"hsl",3) == 0)
          || (strncmp(modeiu,"hsl",3) == 0)  && (strncmp(modeou,"hls",3) == 0)
          || (strncmp(modeiu,"hvs",3) == 0)  && (strncmp(modeou,"hsv",3) == 0)
          || (strncmp(modeiu,"hsv",3) == 0)  && (strncmp(modeou,"hvs",3) == 0) )
      {
         *clr2o=clr3i;
         *clr3o=clr2i;
         *status = -1;
         return;
      }
   }
   /* assign new variables so that the input arguments can't possibly
   be changed by subsequent procedures. */
   c1=clr1i;
   c2=clr2i;
   c3=clr3i;
   /* check  valid range of values. */
   if(modeiu[0]  ==  'h')
   {
      if(c1  <    0. || c1  >  360.) *status = 1;
   } else{
      if(c1  <    0. || c1  >  100.) *status = 1;
   }
   if(modeiu[0]  ==  'y')
   {
      /* don't believe exhaustive test of value ranges  yiq.  eg:
         yiq=(100.,60.,52.) converted to rgb produces values >  100 !?
       */
      if(c2  <  -60. || c2  >   60.) *status = 1;
      if(c3  <  -52. || c3  >   52.) *status = 1;
   } else{
      if(c2  <    0. || c2  >  100.) *status = 1;
      if(c3  <    0. || c3  >  100.) *status = 1;
   }
   if(*status != 0) return;
   /* first, convert input values to rgb values. */
   if     (strncmp(modeiu,"hls",3) == 0)
   {
      hlsrgb(c1,c2,c3,&r,&g,&b);
   } else if(strncmp(modeiu,"hsl",3) == 0)
   {
      hlsrgb(c1,c3,c2,&r,&g,&b);
   } else if(strncmp(modeiu,"hvs",3) == 0)
   {
      hvsrgb(c1,c2,c3,&r,&g,&b);
   } else if(strncmp(modeiu,"hsv",3) == 0)
   {
      hvsrgb(c1,c3,c2,&r,&g,&b);
   } else if(strncmp(modeiu,"rgb",3) == 0)
   {
      r=c1;
      g=c2;
      b=c3;
   } else if(strncmp(modeiu,  "yiq",3) == 0)
   {
      r= 1.*c1 + 0.94826224*c2 + 0.62401264*c3;
      g= 1.*c1 - 0.27606635*c2 - 0.63981043*c3;
      b= 1.*c1 - 1.1054502 *c2 + 1.7298578 *c3;
      /*
      !
      !---if outside the valid range of values, truncate to allow
      !   reasonable roundoff and then retest.  this should pass values
      !   essentially 0 or 100, but fail others.
      !   the above mula for rgb from yiq can give answers slightly
      !   less than 0 and slightly greater than 100.  the truncation
      !   should fix this. (maybe there is a better way to do this?)
      !   the retest should then catch the instances such as
      !   yiq=(100.,60.,52) as mentioned earlier.
      !
      */
      if(r < 0. || r > 100.) r=(int)(r*10000.)/10000.;
      if(g < 0. || g > 100.) g=(int)(g*10000.)/10000.;
      if(b < 0. || b > 100.) b=(int)(b*10000.)/10000.;
      if( r < 0. ||  r > 100. ||
          g < 0. ||  g > 100. ||
          b < 0. ||  b > 100.)
      {
         *status=1;
         return;
      }
   } else{
      *status=2;
      return;
   }
   /* then convert to the desired output values */
   if     (strncmp(modeou,"hls",3) == 0)
   {
      rgbhls(r,g,b,clr1o,clr2o,clr3o);
   } else if(strncmp(modeou,"hsl",3) == 0)
   {
      rgbhls(r,g,b,clr1o,clr3o,clr2o);
   } else if(strncmp(modeou,"hvs",3) == 0)
   {
      rgbhvs(r,g,b,clr1o,clr2o,clr3o);
   } else if(strncmp(modeou,"hsv",3) == 0)
   {
      rgbhvs(r,g,b,clr1o,clr3o,clr2o);
   } else if(strncmp(modeou,"rgb",3) == 0)
   {
      *clr1o=r;
      *clr2o=g;
      *clr3o=b;
   } else if(strncmp(modeou,"yiq",3) == 0)
   {
      *clr1o=0.30*r + 0.59*g + 0.11*b;
      *clr2o=0.60*r - 0.28*g - 0.32*b;
      *clr3o=0.21*r - 0.52*g + 0.31*b;
   } else{
      *status=3;
      return;
   }
   /*
   Eliminate any roundoff that exceeds the limits.   This  assumes  we  may
   occasionally  get  some  values  slightly  past  the limits  that should
   really be equal to the limit.  I've seen  this  happen  beyond  the  7th
   decimal place on the vax with the upper limits; I really haven't seen it
   on the lower bounds.  Of course if there was a real programming  problem
   with some of the other color conversion routines such that they returned
   incorrect output values well beyond the limits, this roundoff  procedure
   may  mask  such a problem since it assumes anything beyond the limits is
   only a slight roundoff problem.
   */
   if   (*clr1o  <    0.) {
      *clr1o =   0.;
   }
   if(modeou[0]  ==  'h') {
      if(*clr1o  >=  360.){
         *clr1o = 360.;
      }
   } else{
      if(*clr1o  >=  100.){
         *clr1o = 100.;
      }
   }
   if(modeou[0]  ==  'y')
   {
      if(*clr2o  <  -60.) *clr2o = -60.;
      if(*clr2o  >   60.) *clr2o =  60.;
      if(*clr3o  <  -52.) *clr3o = -52.;
      if(*clr3o  >   52.) *clr3o =  52.;
   } else{
      if(*clr2o  <    0.) *clr2o =   0.;
      if(*clr2o  >  100.) *clr2o = 100.;
      if(*clr3o  <    0.) *clr3o =   0.;
      if(*clr3o  >  100.) *clr3o = 100.;
   }
}

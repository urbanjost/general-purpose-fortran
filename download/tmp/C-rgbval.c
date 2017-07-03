/*============================================================================*/
float rgbval(float color1,float color2,float h)
/*
@(#) Ensure a color value is in the appropriate range and quadrant
*/
{
   float rgb;
   while ( h > 360.0 ) {
      h = h - 360.0;
   }
   while(h < 0.0) {
      h = h + 360.0;
   }
   if(h < 60.0) {
      rgb = color1+(color2-color1)*h/60.0;
   } else if(h < 180.0) {
      rgb=color2;
   } else if(h < 240.0) {
      rgb=color1+(color2-color1)*(240.0-h)/60.0;
   } else{
      rgb=color1;
   }
   return(rgb);
}
/*============================================================================*/

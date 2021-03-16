/*============================================================================
@(#) For converting RGB colors to a reasonable grayscale
   Monochrome devices that support intensity can have intensity calculated
   from the specified Red, Green, Blue intensities as 0.30*R + 0.59*G +
   0.11*B, as in US. color television systems, NTSC encoding.  Note that
   most devices do not have an infinite range of monochrome intensities
   available.

  ============================================================================*/
float rgbmono(
    float red_component,
    float green_component,
    float blue_component){

   float gray_scale;
   gray_scale = 0.30*red_component + 0.59*green_component + 0.11*blue_component;
   return(gray_scale);
}
/*============================================================================*/

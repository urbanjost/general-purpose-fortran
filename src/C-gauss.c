#include <stdlib.h>
extern double gauss (double min, double max, unsigned int tightness);
#define MAX_RANDOM_NUM 0x7fffffff
/*
 ******************************************************************************
 * NAME: gauss  -  gaussian random value generator
 *
 * SYNOPSIS:
 *
 *    double gauss
 *    (
 *       min        -  bottom of range
 *       max        -  top of range
 *       tightness  -  specifies how tight around the average (min _ max)/2
 *                     the values are to be
 *                     must be >= 1
 *                     for value of 1, the random values are evenly distributed
 *    )
 *
 * RETURN VALUE:
 *    double - random value in the specified range following the gaussian
 *             distribution of the specified tightness
 *
 * DESCRIPTION:
 *    Generates a random value between the given min and max.  The tightness
 * parameter specifies how focused the generated values are around the
 * middle of the specified range [min..max].  For the tightness value of 1,
 * the random values will be uniformly distributed (no focus).  For the
 * increasing values of tightness, the random values will increasingly crowd
 * around the middle of the range (min+max)/2.
 *
 ****************************************************************************** 
 */
double gauss (double min, double max, unsigned int tightness) {
   int i;
   double val = 0.0;
   double range = max - min;

   for (i = 0; i < tightness; i++) {
      val += (random () * range) / MAX_RANDOM_NUM;
   }

   val /= tightness;

   return (val + min);
}

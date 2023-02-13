#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <sys/fcntl.h>
#define DEBUG
#undef DEBUG

/*
 *  Some useful stuff....
 */
#define ABS(x)  ((int)(x) < 0 ? (int)(-x) : (int)(x))
#define MAX(x,y) ((int)(x) < (int)(y) ? (int)(y) : (int)(x))
#define MIN(x,y) ((int)(x) < (int)(y) ? (int)(x) : (int)(y))

/*
 * convert hershey byte to coordinate
 */
#define  COORD(x) ((int)(x) - (int)'R')

/*
 *  Max Number of Hershey characters.
 */
#define  MAX_CHARS 7300

#define  MAX_POINTS 500         /* max points in a char */
#define  MAX_BUF  (2 * MAX_POINTS)      /* max buffer size */

/*
 * hershey table tape
 */
typedef struct
{
   int len;
   char *ch;
} HTAB;

/*
 *  These next few tables define which of the hershey characters
 *  actually make up the various fonts.
 */

#define  MAX_ENTS 200

typedef struct
{
   char *name;
   int ent[MAX_ENTS];
} FTAB;

FTAB fonts[] = {
   { "times.g",{
    2199, 0, 2214, 0, 2217, 0, 733, 0, 2274, 0, 2271, 0, 2272, 0,
    2216, 0, 2721, 0, 2722, 0, 728, 0, 725, 0, 1211, 0, 724, 0,
    1210, 0, 720, 0, 2200, 2209, 1212, 1213, 2241, 0, 2238, 0,
    2242, 0, 2215, 0, 2273, 0, 2027, 2028, 2048, 0, 2030, 0,
    2031, 0, 2047, 0, 2029, 0, 2033, 0, 2035, 0, 229, 0, 2036, 2039,
    2041, 2042, 2034, 0, 2043, 2046, 718, 0, 2050, 0, 2040, 0,
    2049, 0, 2032, 0, 1405, 0, 804, 0, 1406, 0, 2247, 0, 3929, 0,
    2249, 0, 2127, 2128, 2148, 0, 2130, 2131, 2147, 0, 2129, 0,
    2133, 0, 2135, 0, 727, 0, 2136, 2139, 2141, 2142, 2134, 0,
    2143, 2146, 2237, 0, 2150, 0, 2140, 0, 2149, 0, 2132, 0,
    1407, 0, 723, 0, 1408, 0, 2246, 0, 3934, 0, 0, 0,
    }},
   { "times.i",{
    2199, 0, 2764, 0, 2778, 0, 733, 0, 2769, 0, 2271, 0, 2272, 0,
    2777, 0, 2771, 0, 2772, 0, 728, 0, 725, 0, 2761, 0, 724, 0,
    2760, 0, 720, 0, 2750, 2759, 2762, 2763, 2241, 0, 2238, 0, 2242, 0,
    2765, 0, 2273, 0, 2051, 2076, 1405, 0, 804, 0, 1406, 0, 2247, 0,
    3929, 0, 2249, 0, 2151, 2176, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "times.r",{
    2199, 0, 2214, 0, 2728, 0, 733, 0, 2274, 0, 2271, 0, 2272, 0,
    2216, 0, 2221, 0, 2222, 0, 728, 0, 725, 0, 1211, 0, 724, 0,
    1210, 0, 720, 0, 2200, 2209, 1212, 1213, 2241, 0, 2238, 0, 2242, 0,
    2215, 0, 2273, 0, 2001, 2026, 1405, 0, 804, 0, 1406, 0, 2247, 0,
    3929, 0, 2249, 0, 2101, 2126, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "times.ib",{
    2199, 0, 3264, 0, 2778, 0, 733, 0, 3269, 0, 2271, 0, 3268, 0,
    2777, 0, 3271, 3272, 3273, 0, 2725, 0, 3261, 0, 724, 0, 3260, 0,
    2720, 0, 3250, 3259, 3262, 3263, 2241, 0, 2726, 0, 2242, 0, 3265, 0,
    2273, 0, 3051, 3076, 1405, 0, 804, 0, 1406, 0, 2247, 0, 3929, 0,
    2249, 0, 3151, 3176, 1407, 0, 723, 0, 1408, 0, 2246, 0, 3934, 0, 0, 0,
    }},
   { "times.rb",{
    2199, 0, 3214, 0, 3228, 0, 733, 0, 3219, 0, 2271, 0, 3218, 0,
    3227, 0, 3221, 0, 3222, 0, 2723, 0, 2725, 0, 3211, 0, 724, 0,
    3210, 0, 2720, 0, 3200, 3209, 3212, 3213, 2241, 0, 2726, 0, 2242, 0,
    3215, 0, 2273, 0, 3001, 3026, 1405, 0, 804, 0, 1406, 0, 2247, 0,
    3929, 0, 2249, 0, 3101, 3126, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "futura.l",{
    2199, 0, 714, 0, 717, 0, 733, 0, 719, 0, 2271, 0, 734, 0,
    2251, 0, 721, 0, 722, 0, 728, 0, 725, 0, 1211, 0, 724, 0,
    1210, 0, 720, 0, 700, 709, 1212, 1213, 2241, 0, 2238, 0, 2242, 0,
    715, 0, 2273, 0, 501, 526, 1405, 0, 804, 0, 1406, 0, 832, 0,
    3927, 0, 1252, 0, 601, 626, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "futura.m",{
    2199, 0, 2714, 0, 2728, 0, 733, 0, 2719, 0, 2271, 0, 2272, 0,
    2727, 0, 2221, 2222, 2723, 0, 2725, 0, 2711, 0, 724, 0, 2710, 0,
    2720, 0, 2700, 2709, 2712, 2713, 2241, 0, 2726, 0, 2242, 0, 2715, 0,
    2273, 0, 2501, 2526, 1405, 0, 804, 0, 1406, 0, 2247, 0, 3929, 0,
    2249, 0, 2601, 2626, 2227, 0, 723, 0, 2228, 0, 2246, 0, 3934, 0, 0, 0,
    }},
   { "gothic.eng",{
    2199, 0, 3714, 0, 3728, 0, 733, 0, 3719, 0, 2271, 0, 3718, 0,
    3227, 0, 3221, 0, 3222, 0, 2723, 0, 2725, 0, 3711, 0, 724, 0,
    3710, 0, 2720, 0, 3700, 3709, 3712, 3713, 2241, 0, 2726, 0, 2242, 0,
    3715, 0, 2273, 0, 3501, 3526, 1405, 0, 804, 0, 1406, 0, 2247, 0,
    3931, 0, 2249, 0, 3601, 3626, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "gothic.ger",{
    2199, 0, 3714, 0, 3728, 0, 733, 0, 3719, 0, 2271, 0, 3718, 0,
    3227, 0, 3221, 0, 3222, 0, 2723, 0, 2725, 0, 3711, 0, 724, 0,
    3710, 0, 2720, 0, 3700, 3709, 3712, 3713, 2241, 0, 2726, 0, 2242, 0,
    3715, 0, 2273, 0, 3301, 3326, 1405, 0, 804, 0, 1406, 0, 2247, 0,
    3932, 0, 2249, 0, 3401, 3426, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "gothic.ita",{
    2199, 0, 3714, 0, 3728, 0, 733, 0, 3719, 0, 2271, 0, 3718, 0,
    3227, 0, 3221, 0, 3222, 0, 2723, 0, 2725, 0, 3711, 0, 724, 0,
    3710, 0, 2720, 0, 3700, 3709, 3712, 3713, 2241, 0, 2726, 0, 2242, 0,
    3715, 0, 2273, 0, 3801, 3826, 1405, 0, 804, 0, 1406, 0, 2247, 0,
    3933, 0, 2249, 0, 3901, 3926, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "math.low",{
    2199, 0, 2233, 2235, 229, 0, 2271, 0, 2243, 2244, 2221, 2222,
    728, 0, 725, 0, 1211, 0, 724, 0, 1210, 0, 720, 0, 700, 709,
    2401, 2402, 2241, 0, 2238, 0, 2242, 0, 2239, 0, 2240, 0,
    601, 626, 1405, 0, 804, 0, 1406, 0, 2245, 0, 2270, 0, 718, 719,
    2255, 0, 2267, 0, 2256, 2266, 2267, 0, 2268, 0, 2412, 0, 2403, 2406,
    2279, 0, 2077, 0, 2237, 0, 2230, 0, 738, 0, 1407, 0, 739, 0,
    1408, 0, 740, 0, 2246, 0, 0, 0,
    }},
   { "math.upp",{
    2199, 0, 2233, 2235, 229, 0, 2271, 0, 2243, 2244, 2221, 2222, 728, 0,
    725, 0, 711, 0, 724, 0, 710, 0, 720, 0, 700, 709, 2401, 2402,
    2241, 0, 2238, 0, 2242, 0, 2239, 0, 2240, 0, 501, 526, 1405, 0,
    804, 0, 1406, 0, 2245, 0, 2270, 0, 718, 0, 728, 0, 2255, 0,
    2267, 0, 2256, 2266, 2267, 0, 2268, 0, 2412, 0, 2403, 2406, 2279, 0,
    2077, 0, 2237, 0, 2230, 0, 738, 0, 1406, 0, 739, 0, 1407, 0,
    740, 0, 2246, 0, 0, 0,
    }},
   { "cyrillic",{
    2199, 0, 2214, 0, 2217, 0, 733, 0, 2828, 0, 2923, 0, 2928, 0,
    2216, 0, 2721, 0, 2722, 0, 728, 0, 725, 0, 1211, 0, 724, 0,
    1210, 0, 720, 0, 2200, 2209, 1212, 1213, 2241, 0, 2238, 0,
    2242, 0, 2215, 0, 2273, 0, 2801, 2802, 2830, 0, 2805, 0,
    2809, 0, 2821, 0, 2804, 0, 2807, 0, 2809, 0, 2824, 0,
    2811, 2816, 2825, 0, 2817, 2819, 2831, 0, 2803, 0, 2826, 0,
    2822, 0, 2820, 0, 2808, 0, 2806, 0, 804, 0, 2827, 0, 2832, 0,
    2829, 0, 2823, 0, 2901, 2902, 2830, 0, 2905, 0, 2910, 0,
    2921, 0, 2904, 0, 2907, 0, 2909, 0, 2924, 0, 2911, 2916,
    2925, 0, 2917, 2919, 2931, 0, 2903, 0, 2926, 0, 2922, 0,
    2920, 0, 2908, 0, 2906, 0, 2927, 0, 2932, 0, 2929, 0, 2246, 0,
    0, 0,
    }},
   { "astrology",{
    2199, 0, 2312, 0, 2728, 0, 2281, 2285, 2221, 0, 2222, 0, 2286, 2287,
    1211, 0, 2288, 0, 1210, 0, 2289, 0, 3250, 3259, 2290, 2295, 2301, 0,
    3001, 3026, 2302, 0, 804, 0, 2303, 2306, 3151, 3176, 2308, 2311,
    2246, 0, 0, 0,
    }},
   { "meteorology",{
    2199, 0, 750, 753, 2271, 0, 754, 757, 728, 0, 758, 0, 1211, 0,
    724, 0, 1210, 0, 720, 0, 700, 709, 759, 763, 2215, 0, 2273, 0,
    501, 526, 1405, 0, 804, 0, 1406, 0, 519, 0, 766, 0, 765, 0,
    601, 626, 1407, 0, 767, 0, 1408, 0, 768, 0, 2246, 0, 0, 0,
    }},
   { "music",{
    2199, 0, 2214, 0, 2318, 2329, 2378, 0, 2331, 0, 2200, 2209, 712, 0,
    2330, 2331, 2381, 0, 2332, 0, 2382, 0, 2377, 0, 2051, 2076, 1405, 0,
    804, 0, 1406, 0, 2248, 0, 724, 0, 2249, 0, 2151, 2176, 1407, 0,
    723, 0, 1408, 0, 2246, 0, 3934, 0, 0, 0,
    }},
   { "greek",{
    2199, 0, 714, 0, 717, 0, 733, 0, 719, 0, 2271, 0, 734, 0,
    2251, 0, 721, 0, 722, 0, 728, 0, 725, 0, 1211, 0, 724, 0,
    1210, 0, 720, 0, 700, 709, 1212, 1213, 2241, 0, 2238, 0,
    2242, 0, 715, 0, 2273, 0, 527, 528, 548, 0, 530, 531, 547, 0,
    529, 0, 533, 0, 535, 0, 229, 0, 536, 539, 541, 542, 534, 0,
    543, 546, 718, 0, 550, 0, 540, 0, 549, 0, 532, 0, 1405, 0,
    804, 0, 1406, 0, 832, 0, 3927, 0, 1252, 0, 627, 628, 648, 0,
    630, 631, 647, 0, 629, 0, 633, 0, 635, 0, 727, 0, 636, 639,
    641, 642, 634, 0, 643, 646, 2237, 0, 650, 0, 640, 0, 649, 0,
    632, 0, 1407, 0, 723, 0, 1408, 0, 2246, 0, 3934, 0, 0, 0,
    }},
   { "cursive",{
    2199, 0, 714, 0, 717, 0, 733, 0, 719, 0, 2271, 0, 734, 0,
    216, 0, 721, 0, 722, 0, 728, 0, 725, 0, 1211, 0, 724, 0,
    1210, 0, 720, 0, 700, 709, 1212, 1213, 2241, 0, 2238, 0, 2242, 0,
    715, 0, 2273, 0, 551, 576, 1405, 0, 804, 0, 1406, 0, 832, 0,
    3928, 0, 1252, 0, 651, 676, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "script",{
    2199, 0, 2214, 0, 2728, 0, 733, 0, 2274, 0, 2271, 0, 2272, 0,
    2216, 0, 2221, 0, 2222, 0, 728, 0, 725, 0, 1211, 0, 724, 0,
    1210, 0, 720, 0, 2750, 2759, 1212, 1213, 2241, 0, 2238, 0, 2242, 0,
    2215, 0, 2273, 0, 2551, 2576, 1405, 0, 804, 0, 1406, 0, 2247, 0,
    3930, 0, 2249, 0, 2651, 2676, 1407, 0, 723, 0, 1408, 0, 2246, 0,
    3934, 0, 0, 0,
    }},
   { "markers",{
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    2199, 0, 840, 844, 866, 0, 845, 847, 850, 855, 834, 0,
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    840, 844, 866, 0, 845, 847, 850, 855, 834, 0,
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0, 2199, 0,
    0, 0,
    }},
   { "symbolic",{
    2199, 0, 800, 0, 797, 834, 840, 847, 850, 855, 857, 0, 864, 0,
    860, 863, 804, 0, 865, 874, 735, 0, 745, 0, 2276, 0, 2277, 2278,
    740, 744, 2407, 2411, 2187, 0, 2190, 2196, 856, 0, 2246, 0, 0, 0,
    }}
};


/*
 * Read the raw hersh font data and write the binary font files
 * as specified above or as specified in an index file.
 */

/* extern char *malloc(); */

HTAB hersh[MAX_CHARS];

/*
 * getcharacter
 *
 * read in a character line from a hershey font file
 */
int
getcharacter (FILE * fp, int *no, int *pairs, char *buf)
{
   int i;
   char *p, tmp[10];

   /*
    * read in the numbers
    */
   for (i = 0; i < 5; i++) {
      if ((tmp[i] = fgetc (fp)) == '\n')        /* take care of the odd blank line */
         tmp[i] = fgetc (fp);
      if (feof (fp))
         return (0);
   }
   tmp[5] = 0;
   sscanf (tmp, "%d", no);

   for (i = 0; i < 3; i++) {
      tmp[i] = fgetc (fp);
      if (feof (fp))
         return (0);
   }
   tmp[3] = 0;
   sscanf (tmp, "%d", pairs);


   /*
    * read in the pairs
    */
   for (p = buf, i = 0; i < 2 * *pairs; i++, p++)
      if ((*p = fgetc (fp)) == '\n')
         *p = fgetc (fp);

   *p = 0;

   fgetc (fp);                  /* skip newline at end */

   return (1);
}

/*
 * main driver - if argc > 2 we are creating a file from an
 * index, otherwise use the table in h2v.h
 */
int
main (argc, argv)
     int argc;
     char **argv;
{
   void readdata (), readindex (), writefont ();

   FILE *fp;
   FTAB table;
   int i;

   if (argc != 2 && argc != 4) {
      fprintf (stderr, "Usage: h2v datafile [indexfile fontfile]\n");
      exit (1);
   }
   if ((fp = fopen (argv[1], "r")) == NULL) {
      fprintf (stderr, "h2v: can't open hersh data file %s\n", argv[1]);
      exit (1);
   }

   readdata (fp);

   if (argc == 4) {
      readindex (argv[2], argv[3], &table);
      writefont (&table);
   } else
      for (i = 0; i < (int) (sizeof (fonts) / sizeof (FTAB)); i++)
         writefont (&fonts[i]);

   exit (0);
}

/*
 *  readdata
 *
 *  Reads the raw hersh data
 */
void
readdata (fp)
     FILE *fp;
{
   int charno, pairs;
   char buf[MAX_BUF];

   while (getcharacter (fp, &charno, &pairs, buf)) {
      hersh[charno - 1].ch = malloc (2 * pairs + 1);
      strcpy (hersh[charno - 1].ch, buf);
      hersh[charno - 1].len = strlen (hersh[charno - 1].ch);
   }

   fclose (fp);
}

/*
 *  readindex
 *
 *  Read an index file into index tab.
 */
void
readindex (name, fname, tab)
     char *name, *fname;
     FTAB *tab;
{

   FILE *fp;
   int i;

   if ((fp = fopen (name, "r")) == NULL) {
      fprintf (stderr, "h2v: can't open index file\n");
      exit (1);
   }

   tab->name = fname;

   i = 0;
   while (fscanf (fp, "%d %d", &tab->ent[i], &tab->ent[i + 1]) == 2)
      if ((i += 2) >= MAX_ENTS - 2) {
         fprintf (stderr, "h2v: indexfile too big - increase MAX_ENTS\n");
         exit (1);
      }

   tab->ent[i] = 0;

   fclose (fp);
}

/*
 * writefont
 *
 * output a font to file name based on font table tab
 */
void
writefont (tab)
     FTAB *tab;
{
   int i, nchars, asdecw[3];
   int start, end, nvects, fd;
#ifdef DEBUG
   int ii;
#endif
   int x;
   char *p;
   HTAB *curch;

   fprintf (stderr, "Font name: %s\n", tab->name);

#ifdef PC
   if ((fd =
        open (tab->name, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY,
              0644)) < 0) {
#else
   if ((fd = open (tab->name, O_WRONLY | O_CREAT | O_TRUNC, 0644)) < 0) {
#endif
      fprintf (stderr, "Can't open output file: %s\n", tab->name);
      exit (1);
   }

   asdecw[0] = asdecw[2] = -100;
   asdecw[1] = 100;
   nvects = nchars = 0;

   lseek (fd, (long) (5 * sizeof (int)), 0);    /* Leave room for stuff at top */

   for (i = 0; (start = tab->ent[i]) != 0; i += 2) {
      end = tab->ent[i + 1];
#ifdef DEBUG
      fprintf (stderr, "Char: %d to %d\n", start, end);
#endif
      do {
         curch = &hersh[start - 1];
         nchars++;
         if (curch->ch == (char *) NULL) {
            fprintf (stderr, "h2v: character %d not available\n", start);
            exit (1);
         }
         asdecw[2] = MAX (asdecw[2], curch->ch[1] - curch->ch[0]);

#ifdef DEBUG
         fprintf (stderr, "Char: %d length %d\n", start, curch->len);
#endif
         for (p = &curch->ch[2]; *p; p++) {
            x = *p++;
            if (x != ' ') {
               asdecw[0] = MAX (asdecw[0], COORD (*p));
               asdecw[1] = MIN (asdecw[1], COORD (*p));
            }
         }

         nvects += curch->len / 2;
         if (write (fd, &curch->len, sizeof (int)) != sizeof (int)) {
            fprintf (stderr, "h2v: ERROR writing character length to file\n");
            exit (1);
         }
         if (write (fd, curch->ch, (unsigned) curch->len) != curch->len) {
            fprintf (stderr, "h2v: ERROR writing character data to file\n");
            exit (1);
         }
#ifdef DEBUG
         fprintf (stderr, "Char: %d :", curch->len);
         for (ii = 0; ii < curch->len; ii++) {
            fprintf (stderr, " %d", curch->ch + ii);
         }
         fprintf (stderr, "\n");
#endif

         start++;
      } while (start <= end);
   }

#ifdef DEBUG
   fprintf (stderr, "Font name: %s ", tab->name);
   fprintf (stderr, "nchars: %d, nvects: %d ", nchars, nvects);
   fprintf (stderr, "ascender: %d, descender: %d maxwidth: %d\n",
            asdecw[0], asdecw[1], asdecw[2]);
#endif

   lseek (fd, 0L, 0);
   if (write (fd, &nchars, sizeof (nchars)) != sizeof (nchars)) {
      fprintf (stderr, "Error writing to file\n");
      exit (1);
   }

   if (write (fd, &nvects, sizeof (nvects)) != sizeof (nvects)) {
      fprintf (stderr, "Error writing to file\n");
      exit (1);
   }
   if (write (fd, asdecw, sizeof (asdecw)) != sizeof (asdecw)) {
      fprintf (stderr, "Error writing to file\n");
      exit (1);
   }

   close (fd);
}

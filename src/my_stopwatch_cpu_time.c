/*
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <pwd.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <utime.h>
*/
#include <stdio.h>
#include <stdlib.h>
#include <sys/times.h>
#include <sys/param.h>

void my_stopwatch_cpu_time( float *c, float *u, float *s ){
   clock_t t;
   struct tms mytime;

   t = times (&mytime);                              /* call "times" */
   *u = ((float) mytime.tms_utime) / ((float) HZ);   /* user time in 1/HZ seconds is in tms_utime */
   *s = ((float) mytime.tms_stime) / ((float) HZ);   /* HZ is in sys/param.h */
   *c = *u + *s;
   return;
}

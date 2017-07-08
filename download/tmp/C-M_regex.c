#include <sys/types.h>
#include <regex.h>
#include <string.h>
#include <stdlib.h>
/* ------------------------------------------------------------------------------------------------------------------------------ */
void C_regalloc(regex_t **preg_return) {
  *preg_return = malloc(sizeof(**preg_return));
}
/* ------------------------------------------------------------------------------------------------------------------------------ */
/* pattern must be NUL terminated. */
void C_regcomp(regex_t *preg, const char *pattern,
               const char *flags, int *status_return) {
  int i, cflags=0;
  for (i=0;flags[i];i++) {
    switch (flags[i]) {
      case 'i': cflags |= REG_ICASE; break;
      case 'm': cflags |= REG_NEWLINE; break;
      case 'x': cflags |= REG_EXTENDED; break;
      case 'n': cflags |= REG_NOSUB; break;
      case ' ': break;
      default: *status_return=-2; return;
    }
  }
  *status_return = regcomp(preg,pattern,cflags);
}
/* ------------------------------------------------------------------------------------------------------------------------------ */
void C_regexec(const regex_t *preg, const char *string, int nmatch,
               int matches[nmatch][2], const char *flags,
               int *status_return) {
  int i, eflags=0;
  int j=0;
  const char * p = string;
  regmatch_t *pmatch;
  int start;
  int finish;
  for (i=0;flags[i];i++) {
    switch (flags[i]) {
      case 'b': eflags |= REG_NOTBOL; break;
      case 'e': eflags |= REG_NOTEOL; break;
      case ' ': break;
      default: *status_return=-2; return;
    }
  }
/* ------------------------------------------------------------------------------------------------------------------------------ */
   // Added by Trurl The Constructor
   // Elkin Arroyo
   pmatch = malloc(sizeof(regmatch_t)*nmatch);
   while(1 && j<=nmatch) {
      int no_match;
      *status_return = regexec(preg,p,nmatch,pmatch,eflags);
      if (status_return[0]) {
          break;
      }
      if (pmatch[0].rm_so == -1) {
          break;
      }
      start  = pmatch[0].rm_so + (p - string);
      finish = pmatch[0].rm_eo + (p - string);
      matches[j][0]=start;
      matches[j][1]=finish;
      p +=  pmatch[0].rm_eo;
      j=j+1;
  }
  free(pmatch);
}
/* ------------------------------------------------------------------------------------------------------------------------------ */

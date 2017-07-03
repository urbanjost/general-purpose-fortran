#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <readline/readline.h>
#include <readline/history.h>
/*
  @(#) Fortran-callable C routine that calls readline(3c)
       The Fortran routine can use the f2003 ISO_C_BINDING module
        to do this in a portable manner.
  assumes you have the GNU readline library libreadline.a available
*/
/* -------------------------------------------------------------------------- */
void show_history_list() {
  HISTORY_STATE *state =  history_get_history_state();
  int i;
  printf("History list now:\n");
  for (i=0; i < state->length; i++) {
    printf("%d: '%s'%s\n", i, state->entries[i]->line, (i == state->offset? "*":""));
  }
}
/* -------------------------------------------------------------------------- */
void FCreadline(int len, char *myline, char prompt[]){
/*
@(#)FCreadline(3c): return line from readline(3c) to Fortran. John S. Urban, 20100323

   Simple procedure that uses readline in "normal" (i.e. non-callback) mode.

   len    -- number of characters in argument "myline"
   myline -- Fortran CHARACTER variable to receive the line read by readline(3c)
   prompt -- prompt string to precede read

*/
   /* readline(3c) will return the read line to this pointer */
   char *line;
   /* counter for padding returned line with spaces */
   int i;

   using_history();
   /* use readline(3c) to read a line of input in edit mode */
   line=readline(prompt);
   add_history(line);

   /* if the "h" command is on a line by itself show history */
   if(strcmp(line,"h")==0){
      show_history_list();
   }

   /* copy line returned by readline(3c) to MYLINE up to length of MYLINE */
   strncpy(myline,line,(size_t)len);

   /* starting with null, pad with spaces to end */
   for(i=strlen(line);i<len;i++){
     myline[i]=' ';
   }

   /* free memory used to return line from readline(3c) */
   free(line);
}

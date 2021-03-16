/***********************************************************************************************************************************
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
***********************************************************************************************************************************/
#include <stdio.h>
#include <stdlib.h>
/*
   Created:
   Last Modified: Sun Dec 29 21:15:13 EST 1996
   Author: John S. Urban
   Calls: getchar, fprintf

   Paranoid function to read a line of text from a text file or terminal
     o  assumes buffer is big enough to hold buffsize characters including the terminator
     o  assumes getchar does not produce an error
     o  returns EOF if encountered , else number of characters read including ignored characters but not counting the new line 
     o  If EOF is encountered on a line not terminated by newline, EOF is not returned (assume next call will)
   
*/
int read_line(char buffer[],int buffsize) {
   char ident[] = "@(#)read_line(3c):Paranoid function to read a line of text from a text file or terminal";
   static long int calls = 1; /* count number of times called for debug message */
   int stored_into = 0;
   int  integer;

   if(buffsize <= 1){
      fprintf(stderr,"*read_line* error: buffer length <= 1\n");
      /* this might be seen as an EOF on some systems */
      stored_into = -1;
   }
   else{
      while( (integer = getchar()) != '\n'){
         /* read characters until an end-of-line is encountered */
         if (integer == EOF){
            /* end-of-file was encountered */
            buffer[stored_into] = '\0';
            if(stored_into == 0){
               return(EOF);
            }
            else{
               return(stored_into);
            }
         }
         else if(stored_into < buffsize -1 ){
            /* everything looks OK, store the character and continue */
            buffer[stored_into] = (char)integer;
         }
         else if(stored_into == buffsize - 1){
            /* buffer was not described as being big enough to hold any more */
            fprintf(stderr,"*read_line* W-A-R-N-I-N-G: buffer length of %d exceeded, call %ld\n",buffsize,
                calls);
            fprintf(stderr,"*read_line* IGNORING: %c",(char)integer);
         }
         else{
            /* buffer already overflowed, print additional ignored characters */
            fprintf(stderr,"%c",(char)integer);
         }
         ++stored_into;
      }

      if ( stored_into > buffsize -1 ){
         /* buffer size was exceeded, terminate line of ignored characters and make sure terminator fits in buffer*/
         fprintf(stderr,"\n");
         buffer[buffsize -1] = '\0';
      }
      else{
         /* add line terminator */
         buffer[stored_into] = '\0';
      }

   }
   calls++;
   return(stored_into);
}
/***********************************************************************************************************************************
||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
***********************************************************************************************************************************/

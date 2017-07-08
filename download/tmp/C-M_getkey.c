/*
   hopefully one of these will work on this system

      #define BSD
      #define V13
      #define V13B
      #define LINUX
*/
#define LINUX
#ifdef LINUX
/* @(#) Driver for reading a character from keyboard in raw I/O mode */
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termio.h>
#include <signal.h>
/* return the next key typed in hot (raw I/O) mode.  */
char Fgetkey (void) {
   struct termio oldtty, newtty;
   char c;
   ioctl (0, TCGETA, &oldtty);
   newtty = oldtty;
   newtty.c_iflag = BRKINT | IXON | ISTRIP;
   newtty.c_lflag = 0;
   newtty.c_cc[VEOF] = 1;
   ioctl (0, TCSETA, &newtty);
   read (0, &c, 1);
   ioctl (0, TCSETA, &oldtty);
   return (c);
}
#endif
/**********************************************************************************************************************************/
#ifdef V13
/* @(#) Driver for reading a character from keyboard in raw I/O mode */
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <termios.h> */
#include <string.h>
#include <sys/ioctl.h>
#include <termio.h>
#include <signal.h>
#include <sys/types.h> */
/* return the next key typed in hot (raw I/O) mode.  */
char Fgetkey (void) {
   static struct termios oldtty, newtty;
   static tcflag_t save_flags;
   char c;
   tcgetattr (0, &oldtty);
   newtty = oldtty;
   newtty.c_iflag = BRKINT | IXON | ISTRIP;
   newtty.c_lflag = 0;
   newtty.c_cc[VEOF] = 1;
   tcsetattr (0, TCSANOW, &newtty);
   read (0, &c, 1);
   tcsetattr (0, TCSANOW, &oldtty);
   return (c);
}
#endif
/**********************************************************************************************************************************/
#ifdef V13B
/* @(#) Driver for reading a character from keyboard in raw I/O mode */
#include <stdio.h>
#include <termios.h>
char Fgetkey (void){
   static struct termios termattr, saveattr;
   static tcflag_t save_flags;

   tcgetattr (0, &termattr);
   saveattr = termattr;
   termattr.c_lflag &= ~(ICANON);
   termattr.c_lflag &= ~(ECHO);
   termattr.c_cc[VMIN] = 1;
   termattr.c_cc[VTIME] = 0;
   tcsetattr (0, TCSADRAIN, &termattr);
   *nextch = getchar ();
   tcsetattr (0, TCSADRAIN, &saveattr);
   return ((char)nextch);
}
#endif
/**********************************************************************************************************************************/
#ifdef BSD
/* @(#) Driver for reading a character from keyboard in raw I/O mode */
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sgtty.h>
#include <sys/termio.h>
#include <signal.h>

/* return the next key typed in hot (raw I/O) mode.  */
char Fgetkey (void){
   struct sgttyb oldtty, newtty;
   char c;

   ioctl (0, TIOCGETP, &oldtty);

   newtty = oldtty;
   newtty.sg_flags = RAW;

   ioctl (0, TIOCSETP, &newtty);

   read (0, &c, 1);

   ioctl (0, TIOCSETP, &oldtty);
   /* fprintf(stderr,"C:c=%c\n",c); */
   /* fflush(stdout); */
   return (c);
}
#endif
#ifdef TESTPRG
#include <stdlib.h>
#include <stdio.h>
/* read keys in hot (raw I/O) mode until letter q is hit */
main () {
   char keyvalue;
   keyvalue = 0;
   fprintf (stdout, "press keys ('q' to quit)\n");
   fflush (stdout);
   fflush (stdin);
   while (keyvalue != 'q') {
      keyvalue = Fgetkey ();
      if(keyvalue != '\0'){
         fprintf (stdout, "C:KEY=%c %d\n", keyvalue, keyvalue);
      }else{
         usleep(5);
         fflush (stdout);
      }
   }
}
#endif

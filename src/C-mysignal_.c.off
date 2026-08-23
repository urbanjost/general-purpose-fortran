/*----------------------------------------------------------------------------*/
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/* #include <termios.h> */
/* #include <ctype.h>   */
/* #include <syslog.h>  */

#include <signal.h>
#include <setjmp.h>

void mymain_(int *istatus);

int	fromatty = 0;

int     istatus;
void	my_intr();

jmp_buf	toplevel;
/*----------------------------------------------------------------------------*/
void mysignal_(void) {
	/*fprintf(stderr,"STARTING MYSIGNAL\n");*/
	if ((istatus=setjmp(toplevel)) == 0){   /* place to return to from interrupt*/
	/*	fprintf(stderr,"Initializing Signal Handling\n");*/
	} else{                  

	/*	tcflush(stdout,TCIOFLUSH);*/
	/*	tcflush(stdin, TCIOFLUSH);*/
	/*	tcflush(stderr, TCIOFLUSH);*/

		fprintf(stderr,"Interrupted. return code=%d \n",istatus);

	/*	fflush(stdout);*/
	/*	fflush(stdin);*/
	/*	fflush(stderr);*/

#ifndef MINGW
	        sleep(1);
#endif
	}
	signal(SIGINT, my_intr);
	(void)mymain_(&istatus);
	/* fprintf(stderr,"ENDING MYSIGNAL\n");*/
}
/*----------------------------------------------------------------------------*/
void my_intr(int arg){
	if (fromatty++ > 30)  /* after 30 interrupts really quit, or if not a TTY session, or whatnot*/
	    exit(0);
	longjmp(toplevel, 1);
	fprintf(stderr,"never got here \n");
}
/*----------------------------------------------------------------------------*/

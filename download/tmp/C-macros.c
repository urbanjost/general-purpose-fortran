/* Make ncurses(3f) macros available to Fortran */
#include <ncurses.h>
#include <stdio.h>
/*----------------------------------------------------------------------------*/
void macro_getyx(WINDOW *win, int *y, int *x){
   getyx(win,*y,*x);
}
/*----------------------------------------------------------------------------*/
void macro_getparyx(WINDOW *win, int *y, int *x){
   getparyx(win,*y,*x);
}
/*----------------------------------------------------------------------------*/
void macro_getbegyx(WINDOW *win, int *y, int *x){
   getbegyx(win,*y,*x);
}
/*----------------------------------------------------------------------------*/
void macro_getmaxyx(WINDOW *win, int *y, int *x){
   getmaxyx(win,*y,*x);
   /*
   fprintf(stderr,"MAXX=%d\n",*x);
   fprintf(stderr,"MAXY=%d\n",*y);
   */
}
/*----------------------------------------------------------------------------*/
void macro_getcolor(int *CURSES_COLORS, int *CURSES_COLOR_PAIRS){
   *CURSES_COLORS=COLORS;
   *CURSES_COLOR_PAIRS=COLOR_PAIRS;
   /*
   fprintf(stderr,"COLORS=%d\n",COLOR_PAIRS);
   fprintf(stderr,"COLOR_PAIRS=%d\n",COLORS);
   */
   /*
   FILE *fp;
   fp=fopen("c.20","w");
   fprintf(fp,"COLORS=%ld\n",COLORS);
   fprintf(fp,"COLOR_PAIRS=%ld\n",COLOR_PAIRS);
   */
}
/*----------------------------------------------------------------------------*/
int macro_PAIR_NUMBER( attr_t attrs){
return(PAIR_NUMBER(attrs));
}
/*----------------------------------------------------------------------------*/
WINDOW *returnstd(void){
return(stdscr);
}
/*----------------------------------------------------------------------------*/
WINDOW *returncur(void){
return(curscr);
}
/*----------------------------------------------------------------------------*/
WINDOW *macro_getwin(const char *filename){
   FILE *filep;
   WINDOW *newwin;
   filep=fopen(filename,"r");
   newwin=getwin(filep);
   fclose(filep);
   return(newwin);
}
/*----------------------------------------------------------------------------*/
int macro_putwin(WINDOW *win, const char *filename){
   FILE *filep;
   int ierr;
   filep=fopen(filename,"w");
   ierr=putwin(win,filep);
   fclose(filep);
   return(ierr);
}
/*----------------------------------------------------------------------------*/
int macro_debug(WINDOW *win, const char *filename){
   FILE *fp2;
   int ierr;
   fprintf(fp2,"got here A\n");
   fp2=fopen("c.20","w");
   fprintf(fp2,"filename=%s\n",filename);
   fprintf(fp2,"win=%ld\n",win);
   fprintf(fp2,"got here B\n");
   fflush(fp2);
   fclose(fp2);
   return(ierr);
}
/*----------------------------------------------------------------------------*/
/*><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/* At least using the ISO_C_BINDING standard there seems to be a problem passing a variable argument list so ... */
/* printw group with one integer argument */
/*----------------------------------------------------------------------------*/
int i_printw(const char *fmt, int i)                              { return(printw(fmt,i)); }
int i_wprintw(WINDOW *win, const char *fmt, int i)                { return(wprintw(win,fmt,i)); }
int i_mvprintw(int y, int x, const char *fmt, int i)              { return(mvprintw(y,x,fmt,i)); }
int i_mvwprintw(WINDOW *win, int y, int x, const char *fmt, int i){ return(mvwprintw(win,y,x,fmt,i)); }
/*----------------------------------------------------------------------------*/
int l_printw(const char *fmt, long i)                              { return(printw(fmt,i)); }
int l_wprintw(WINDOW *win, const char *fmt, long i)                { return(wprintw(win,fmt,i)); }
int l_mvprintw(int y, int x, const char *fmt, long i)              { return(mvprintw(y,x,fmt,i)); }
int l_mvwprintw(WINDOW *win, int y, int x, const char *fmt, long i){ return(mvwprintw(win,y,x,fmt,i)); }
/*----------------------------------------------------------------------------*/
int i2_printw(const char *fmt, short i)                              { return(printw(fmt,i)); }
int i2_wprintw(WINDOW *win, const char *fmt, short i)                { return(wprintw(win,fmt,i)); }
int i2_mvprintw(int y, int x, const char *fmt, short i)              { return(mvprintw(y,x,fmt,i)); }
int i2_mvwprintw(WINDOW *win, int y, int x, const char *fmt, short i){ return(mvwprintw(win,y,x,fmt,i)); }
/*----------------------------------------------------------------------------*/
/* printw group with no arguments after format */
int n_printw(const char *fmt)                                     { return(printw(fmt)); }
int n_wprintw(WINDOW *win, const char *fmt)                       { return(wprintw(win,fmt)); }
int n_mvprintw(int y, int x, const char *fmt)                     { return(mvprintw(y,x,fmt)); }
int n_mvwprintw(WINDOW *win, int y, int x, const char *fmt)       { return(mvwprintw(win,y,x,fmt)); }
/*----------------------------------------------------------------------------*/
/* printw group with one real argument */
int r_printw(const char *fmt, float r)                              { return(printw(fmt,r)); }
int r_wprintw(WINDOW *win, const char *fmt, float r)                { return(wprintw(win,fmt,r)); }
int r_mvprintw(int y, int x, const char *fmt, float r)              { return(mvprintw(y,x,fmt,r)); }
int r_mvwprintw(WINDOW *win, int y, int x, const char *fmt, float r){ return(mvwprintw(win,y,x,fmt,r)); }
/*----------------------------------------------------------------------------*/
/* printw group with one string argument */
int s_printw(const char *fmt, char *string)                              { return(printw(fmt,string)); }
int s_wprintw(WINDOW *win, const char *fmt, char *string)                { return(wprintw(win,fmt,string)); }
int s_mvprintw(int y, int x, const char *fmt, char *string)              { return(mvprintw(y,x,fmt,string)); }
int s_mvwprintw(WINDOW *win, int y, int x, const char *fmt, char *string){ return(mvwprintw(win,y,x,fmt,string)); }
/*----------------------------------------------------------------------------*/
/* printw group with two integer arguments */
int ii_printw(const char *fmt, int i, int j)                              { return(printw(fmt,i,j)); }
int ii_wprintw(WINDOW *win, const char *fmt, int i, int j)                { return(wprintw(win,fmt,i,j)); }
int ii_mvprintw(int y, int x, const char *fmt, int i, int j)              { return(mvprintw(y,x,fmt,i,j));
/* \n and other escaped characters do not appear to work */
/* fprintf(stderr,"Y=%d X=%d I=%d J=%d FORMAT=%s\n",y,x,i,j,fmt); */
}
int ii_mvwprintw(WINDOW *win, int y, int x, const char *fmt, int i, int j){ return(mvwprintw(win,y,x,fmt,i,j)); }
/*----------------------------------------------------------------------------*/
/*><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
int i_scanw(char *fmt, int *i)                              { return(scanw(fmt,i)); }
int i_wscanw(WINDOW *win, char *fmt, int *i)                { return(wscanw(win,fmt,i)); }
int i_mvscanw(int y, int x, char *fmt, int *i)              { return(mvscanw(y,x,fmt,i)); }
int i_mvwscanw(WINDOW *win, int y, int x, char *fmt, int *i){ return(mvwscanw(win,y,x,fmt,i)); }
/*----------------------------------------------------------------------------*/
int l_scanw(char *fmt, long *i)                              { return(scanw(fmt,i)); }
int l_wscanw(WINDOW *win, char *fmt, long *i)                { return(wscanw(win,fmt,i)); }
int l_mvscanw(int y, int x, char *fmt, long *i)              { return(mvscanw(y,x,fmt,i)); }
int l_mvwscanw(WINDOW *win, int y, int x, char *fmt, long *i){ return(mvwscanw(win,y,x,fmt,i)); }
/*----------------------------------------------------------------------------*/
int i2_scanw(char *fmt, short *i)                              { return(scanw(fmt,i)); }
int i2_wscanw(WINDOW *win, char *fmt, short *i)                { return(wscanw(win,fmt,i)); }
int i2_mvscanw(int y, int x, char *fmt, short *i)              { return(mvscanw(y,x,fmt,i)); }
int i2_mvwscanw(WINDOW *win, int y, int x, char *fmt, short *i){ return(mvwscanw(win,y,x,fmt,i)); }
/*----------------------------------------------------------------------------*/
/* scanw group with one real argument */
int r_scanw(char *fmt, float *r)                              { return(scanw(fmt,r)); }
int r_wscanw(WINDOW *win, char *fmt, float *r)                { return(wscanw(win,fmt,r)); }
int r_mvscanw(int y, int x, char *fmt, float *r)              { return(mvscanw(y,x,fmt,r)); }
int r_mvwscanw(WINDOW *win, int y, int x, char *fmt, float *r){ return(mvwscanw(win,y,x,fmt,r)); }
/*----------------------------------------------------------------------------*/
/* scanw group with one string argument */
int s_scanw(char *fmt, char *string)                              { return(scanw(fmt,string)); }
int s_wscanw(WINDOW *win, char *fmt, char *string)                { return(wscanw(win,fmt,string)); }
int s_mvscanw(int y, int x, char *fmt, char *string)              { return(mvscanw(y,x,fmt,string)); }
int s_mvwscanw(WINDOW *win, int y, int x, char *fmt, char *string){ return(mvwscanw(win,y,x,fmt,string)); }
/*----------------------------------------------------------------------------*/
/* scanw group with two integer arguments */
int ii_scanw(char *fmt, int *i, int *j)                              { return(scanw(fmt,i,j)); }
int ii_wscanw(WINDOW *win, char *fmt, int *i, int *j)                { return(wscanw(win,fmt,i,j)); }
int ii_mvscanw(int y, int x, char *fmt, int *i, int *j)              { return(mvscanw(y,x,fmt,i,j));
/* \n and other escaped characters do not appear to work */
/* fprintf(stderr,"Y=%d X=%d I=%d J=%d FORMAT=%s\n",y,x,*i,*j,fmt); */
}
int ii_mvwscanw(WINDOW *win, int y, int x, char *fmt, int *i, int *j){ return(mvwscanw(win,y,x,fmt,i,j)); }
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><*/
/*----------------------------------------------------------------------------*/

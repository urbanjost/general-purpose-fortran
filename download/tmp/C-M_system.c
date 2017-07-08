#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <dirent.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <strings.h>
#include <pwd.h>

#define MIN(x,y)  ((x) < (y) ? (x) : (y))

 extern char **environ;

 extern int FHOST_NAME_MAX;
 int FHOST_NAME_MAX=HOST_NAME_MAX;

 extern mode_t FS_IRGRP;
 extern mode_t FS_IROTH;
 extern mode_t FS_IRUSR;
 extern mode_t FS_IRWXG;
 extern mode_t FS_IRWXO;
 extern mode_t FS_IRWXU;
 extern mode_t FS_IWGRP;
 extern mode_t FS_IWOTH;
 extern mode_t FS_IWUSR;
 extern mode_t FS_IXGRP;
 extern mode_t FS_IXOTH;
 extern mode_t FS_IXUSR;
 extern mode_t FDEFFILEMODE;
 extern mode_t FACCESSPERMS;

  mode_t FS_IRGRP=S_IRGRP;
  mode_t FS_IROTH=S_IROTH;
  mode_t FS_IRUSR=S_IRUSR;
  mode_t FS_IRWXG=S_IRWXG;
  mode_t FS_IRWXO=S_IRWXO;
  mode_t FS_IRWXU=S_IRWXU;
  mode_t FS_IWGRP=S_IWGRP;
  mode_t FS_IWOTH=S_IWOTH;
  mode_t FS_IWUSR=S_IWUSR;
  mode_t FS_IXGRP=S_IXGRP;
  mode_t FS_IXOTH=S_IXOTH;
  mode_t FS_IXUSR=S_IXUSR;
  mode_t FDEFFILEMODE=DEFFILEMODE;
  mode_t FACCESSPERMS=ACCESSPERMS;

char **ep;
extern long int longest_env_variable;

/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   wrapper around readdir(3c) for a call from Fortran
*/
void my_readdir(DIR *dirp, char *filename, int *err){

   struct dirent *dp;
   int length;

   err = 0;
   length = 0;

   if ((dp = readdir (dirp)) != NULL) {
      length=(int)strlen(dp->d_name);
      strncpy(filename,dp->d_name,length);
   }else{
      err=&errno; /*When the end of the directory is encountered, a null pointer is returned and errno is not changed.*/
   }
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   flush stdin and stderr and all files open in C
*/
void my_flush(void){
   fflush(NULL);
   /* For good measure */
   fflush(stdin);
   fflush(stdout);
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   wrapper to step through environment table
*/

void my_initenv(){
/*
   Set pointer into environment table to beginning of table,
   but find longest current variable length so can make buffer
   big enough by scanning current table. There is probably a
   C variable that defines this length; but hopefully this
   entire method of reading the environment table will be
   superceeded if I can figure out what is wrong with the
   version that returns an arbitrary string length directly.
   See:
      xargs --show-limits
*/
long int newlength;
   ep=environ;
   longest_env_variable=4096;
   while ((*ep)){
      newlength=(long int)strlen(*ep);
      if(newlength > longest_env_variable){
         longest_env_variable=newlength;
      }
      *ep++;
   }
   ep=environ;
}

void my_readenv(char *variable){
size_t length_in;
size_t length_out;
size_t length_copy;
   if ( *ep == NULL ){
      strncpy(variable,"",1);
      /*
      fprintf(stdout,"%s [%s]\n","REWIND TABLE",variable);
      */
      my_initenv();                                  /* reset pointer to start of table */
   }else{
      length_in=strlen(variable);
      length_out=strlen(*ep);
      length_copy=MIN(length_in,length_out);

      strncpy(variable,*ep,length_copy);
      *ep=*ep++;
   }
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
  return last error number for functions that explicitly state they set it
  use a function as this might be a macro
*/
int my_errno() {
return(errno);
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
void system_unbuffer() {
/*
   This routine turns off buffering of standard input so that Kevin
   Serafini can control both input and output so that he can control
   USH from the xush(1) X11 windows GUI program using forked pipes
*/
        char ident[] = "@(#)system_unbuffer(3c): FORTRAN-callable call to turn off buffering of stdin";

        if( setvbuf( stdin, NULL, _IOLBF, 0 ) != 0 ) {
                perror( "setvbuf" );
                exit(5);
        }
        if( setvbuf( stdout, NULL, _IOLBF, 0 ) != 0 ) {
                perror( "setvbuf" );
                exit(5);
        }
        fprintf(stderr, "IN THE BUFF!\n");
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   my_uname -- return system information from uname(3c) to Fortran subroutine
*/
void my_uname (char *which, char *string, int *stringlen) {
   struct utsname name;
   int i;
   int j;
   if (uname (&name) == -1) {
      fprintf (stderr, "*my_uname* cannot get system name\n");
      strncpy (string, "UNKNOWN", *stringlen);
   } else {
      switch (*which) {
      case 's': strncpy (string, name.sysname, *stringlen);
         break;
      case 'n': strncpy (string, name.nodename, *stringlen);
         break;
      case 'r': strncpy (string, name.release, *stringlen);
         break;
      case 'v': strncpy (string, name.version, *stringlen);
         break;
      case 'm': strncpy (string, name.machine, *stringlen);
         break;
      case 'T':
         fprintf (stderr, "*my_uname* sysname:  %s\n", name.sysname);
         fprintf (stderr, "*my_uname* nodename: %s\n", name.nodename);
         fprintf (stderr, "*my_uname* release:  %s\n", name.release);
         fprintf (stderr, "*my_uname* version:  %s\n", name.version);
         fprintf (stderr, "*my_uname* machine:  %s\n", name.machine);
         strncpy (string, "", *stringlen);
         break;
      default:
         fprintf (stderr, "*my_uname* error: unknown switch %c \n",
                  *which);
         fprintf (stderr, "*my_uname* my_uname:%s:%c:%d\n", string, *which,
                  *stringlen);
         strncpy (string, "UNKNOWN", *stringlen);
      }
   }
   /*
      remove null string terminator and fill string with blanks for Fortran
      */
   for (j = strlen (string); j < *stringlen; j++) {
      string[j] = ' ';
   }
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <unistd.h>
/* ===============================================================================================================================*/
/*
 *  Decides whether a given file name is a directory.
 *  return 1 if file exists and is a directory
 */
int check_dir (const char *path) {
   struct stat sb;
   return stat (path, &sb) == 0 && S_ISDIR (sb.st_mode);
}
/* ===============================================================================================================================*/
/* Check for regular file. */
int check_reg (const char *path) {
   struct stat sb;
   return stat (path, &sb) == 0 && S_ISREG (sb.st_mode);
}
/* ===============================================================================================================================*/
/*
 * Decides whether a given file name is a symbolic link.
 * return 1 if file exists and is a symlink, 0 otherwise.
 */
int c_islink(const char *fname) {
  struct stat statbuf;

  if (lstat(fname, &statbuf)) {
    return 0;
  } else {
    return S_ISLNK(statbuf.st_mode);
  }
}
/* ===============================================================================================================================*/
/*
 * Checks whether a given file exists.
 * return 1 if file exists, 0 otherwise.
 */
int c_file_exists(const char *fname) {
  struct stat statbuf;
  return !stat(fname, &statbuf);
}
/* ===============================================================================================================================*/
/*
int main () {
   printf ("input.txt is a regular file?  %s\n",
           check_reg ("input.txt") ? "yes" : "no");
   printf ("docs is a directory?  %s\n", check_dir ("docs") ? "yes" : "no");
   printf ("/input.txt is a regular file?  %s\n",
           check_reg ("/input.txt") ? "yes" : "no");
   printf ("/docs is a directory?  %s\n", check_dir ("/docs") ? "yes" : "no");
   return 0;
}
*/

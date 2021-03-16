#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <pwd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <utime.h>

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
       long int longest_env_variable=0L;
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   wrapper around access(3c) for a call from Fortran
*/
int my_access(const char *pathname, int which) {
   int n;
   /*fprintf(stdout," which values = %d %d %d %d %d\n",F_OK,R_OK,W_OK,X_OK,which);*/
   n = access (pathname, which);
   return (n);
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/* does a recursive mkdir(3c) for a POSIX pathname */
void my_mkdir (char *dir, int mode, int *ier) {
   char *p = NULL;
   char buf[4096];
   size_t len;

   snprintf(buf, sizeof(buf), "%s", dir);
   len = strlen (buf);
   if (buf[len - 1] == '/') {
      buf[len - 1] = 0;
   }
   for (p = buf + 1; *p; p++) {
      if(*p == '/') {
         *p = 0;
         mkdir (buf, mode);
         *p = '/';
      }
   }
   *ier=mkdir(buf, mode);
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   wrapper around utime(3c) for a call from Fortran
*/
int my_utime(const char *file, int times[2]) {
   struct utimbuf ut;
   /* time_t ut[2]; */
   int n;

   ut.actime  = (time_t)times[0];
   ut.modtime = (time_t)times[1];
   n = utime (file, &ut);
   /*
   ut[0] = times[0];
   ut[1] = times[l];
   n = utime (file, ut);
   */
   return (n);
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   wrapper around chown(3c) for a call from Fortran
*/
int my_chown(char *filename, long long int uid, long long int gid){
   return chown(filename, (uid_t)uid, (gid_t)gid);
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   wrapper around readdir(3c) for a call from Fortran
*/
void my_readdir(DIR *dirp, char *filename, int *ierr){

   struct dirent *dp;
   int length;

   *ierr = 0;
   length = 0;

   if ((dp = readdir (dirp)) != NULL) {
      length=(int)strlen(dp->d_name)+1;
      strncpy(filename,dp->d_name,length);
   }else{
      *ierr = -1; /*When the end of the directory is encountered, a null pointer is returned and errno is not changed.*/
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
char* my_realpath (char *symlinkpath) {
   return(realpath (symlinkpath, NULL));
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/*
   wrapper to step through environment table
*/
/*--------------------------------------------------------------------------------------------------------------------------------*/
void my_initenv(){
/*
   Set pointer into environment table to beginning of table,
   but find longest current variable length so can make buffer
   big enough by scanning current table. There is probably a
   C variable that defines this length; but hopefully this
   entire method of reading the environment table will be
   superseded if I can figure out what is wrong with the
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
/*--------------------------------------------------------------------------------------------------------------------------------*/
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

      strncpy(variable,*ep,length_copy+1);
      *ep++;
   }
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
int my_getgrgid(long long int id, char *groupname){
struct group *grp;

   errno = 0;

   if ( (grp = getgrgid((gid_t)id) ) != NULL) {
      strcpy(groupname,grp->gr_name);
   } else {
      strncpy(groupname,"",1);
      perror("getgrgid");
   }
   return errno;
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
int my_getpwuid(long long int id, char *username){
struct passwd *pwd;

   errno = 0;

   if ( (pwd = getpwuid((uid_t)id) ) != NULL) {
      strcpy(username,pwd->pw_name);
   } else {
      strncpy(username,"",1);
      perror("getpwuid");
   }
   return errno;
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
/*
   Use unix routine times(3c) to measure user execution time between
   calls in seconds.
*/

void my_cpu_time( float *c, float *u, float *s ){
#include <sys/times.h>
#include <sys/param.h>
   clock_t t;
   struct tms mytime;

   t = times (&mytime);                              /* call "times" */
   *u = ((float) mytime.tms_utime) / ((float) HZ);   /* user time in 1/HZ seconds is in tms_utime */
   *s = ((float) mytime.tms_stime) / ((float) HZ);   /* HZ is in sys/param.h */
   *c = *u + *s;
   return;
}
/*--------------------------------------------------------------------------------------------------------------------------------*/
/* ===============================================================================================================================*/
/*
 *  Decides whether a given file name is a directory.
 *  return 1 if file exists and is a directory
 */
int my_isdir (const char *path) {
   struct stat sb;
   return stat(path, &sb) == 0 && S_ISDIR (sb.st_mode);
}
/* ===============================================================================================================================*/
/* Check for regular file. */
int my_isreg (const char *path) {
   struct stat sb;
   return stat(path, &sb) == 0 && S_ISREG (sb.st_mode);
}
/* ===============================================================================================================================*/
/* determine if filename is a block device */
int my_isblk(const char *path){
   struct stat sb;
   return stat(path, &sb) == 0 && S_ISBLK (sb.st_mode);
}
/* ===============================================================================================================================*/
/* determine if filename is a character device */
int my_ischr(const char *path){
   struct stat sb;
   return stat(path, &sb) == 0 && S_ISCHR (sb.st_mode);
}
/* ===============================================================================================================================*/
/* determine if filename is a fifo - named pipe */
int my_isfifo(const char *path){
   struct stat sb;
   return stat(path, &sb) == 0 && S_ISFIFO (sb.st_mode);
}
/* ===============================================================================================================================*/
/* determine if filename is a socket */
int my_issock(const char *path){
   struct stat sb;
   return stat(path, &sb) == 0 && S_ISSOCK (sb.st_mode);
}
/* ===============================================================================================================================*/
/*
 * Decides whether a given file name is a symbolic link.
 * return 1 if file exists and is a symlink, 0 otherwise.
 */
int my_islnk(const char *fname) {
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
int my_file_exists(const char *fname) {
  struct stat statbuf;
  return !stat(fname, &statbuf);
}
/* ===============================================================================================================================*/
#include <grp.h>
#include <time.h>
#include <locale.h>
#include <langinfo.h>
#include <stdint.h>

void my_stat(char *file,long int *values, int *ierr, int debug){
struct stat *buf = malloc(sizeof (struct stat));                          /* allocates memory for stat structure. */
struct passwd *pwd;
struct group *grp;
struct tm *tm;
char datestring[256];
static char local_buff[17] = { 0 };
int i;

   errno = 0;                                                              /* always set errno to zero first.      */
   if (stat (file, buf) != 0) {
   perror (file);                                                          /* if stat does not work, print a diagnostic. */
      *ierr=1;
      return;
   }

   *ierr=0;
   values[0]=buf->st_dev;                      /* st_dev    device file currently resides on. */
   values[1]=buf->st_ino;                      /* st_ino    The inode for the file */
   values[2]=buf->st_mode;                     /* st_mode   The current permissions on the file. */
   values[3]=buf->st_nlink;                    /* st_nlink  number of links to this file. */
   values[4]=buf->st_uid ;                     /* st_uid    The User ID for the file. */
   values[5]=buf->st_gid ;                     /* st_gid    The Group ID for the file. */
   values[6]=buf->st_rdev;                     /* st_rdev   ID of device containing directory entry for file (0 if not available) */
   values[7]=buf->st_size;                     /* st_size   file size in bytes */
   values[8]=buf->st_atime+0.5;                /* st_atime  most recent time file was accessed. */
   values[9]=buf->st_mtime+0.5;                /* st_mtime  most recent time file contents modified. */
   values[10]=buf->st_ctime+0.5;               /* st_ctime  most recent time file permissions changed. */
   values[11]=buf->st_blksize;                 /*           Preferred I/O block size (-1 if not available) */
   values[12]=buf->st_blocks;                  /*           Number of blocks allocated (-1 if not available) */

   if(debug==0){
   printf("Information for %s ",file);
   printf("(The file %s a symbolic link)\n", (S_ISLNK(buf->st_mode)) ? "is" : "is not");
   printf("---------------------------\n");
   printf("File Size ........................ %jd bytes\n",(intmax_t)buf->st_size);  /* st_size      file size in bytes */
   printf("Number of Links .................. %ld\n",buf->st_nlink);        /* st_nlink     number of links to this file. */
   printf("File inode ....................... %ld\n",buf->st_ino);          /* st_ino       The inode for the file */

   i=0;
/*
  This varies, but at least one ls(1) command uses this convention:

  The file type is one of the following characters:
       -    regular file
       b    block special file
       c    character special file
       C    high performance (   contiguous data   ) file
       d    directory
       D    door (Solaris 2.5 and up)
       l    symbolic link
       M    off-line (   migrated   ) file (Cray DMF)
       n    network special file (HP-UX)
       p    FIFO (named pipe)
       P    port (Solaris 10 and up)
       s    socket
       ?    some other file type
  The file mode bits listed are similar to symbolic mode specifications (*note Symbolic Modes::). But ls(1) combines
  multiple bits into the third character of each set of permissions as follows:
       s    If the set-user-ID or set-group-ID bit and the corresponding executable bit are both set.
       S    If the set-user-ID or set-group-ID bit is set but the corresponding executable bit is not set.
       t    If the restricted deletion flag or sticky bit, and the other-executable bit, are both set. The restricted deletion
        flag is another name for the sticky bit. *Note Mode Structure::.
       T    If the restricted deletion flag or sticky bit is set but the other-executable bit is not set.
       x    If the executable bit is set and none of the above apply.
       -    Otherwise.
  Following the file mode bits is a single character that specifies
  whether an alternate access method such as an access control list
  applies to the file. When the character following the file mode
  bits is a space, there is no alternate access method. When it is a
  printing character, then there is such a method.

  GNU ls(1) uses a . character to indicate a file with a security context, but no other alternate access method.

  A file with any other combination of alternate access methods is marked with a    +    character.

*/
   if( S_ISDIR(buf->st_mode) ){                               /* st_mode      The current permissions on the file. */
       local_buff[i]='d';
   }else if( S_ISCHR(buf->st_mode) ){
       local_buff[i]='c';
   }else if( S_ISLNK(buf->st_mode) ){
       local_buff[i]='l';
   }else if( S_ISBLK(buf->st_mode) ){
       local_buff[i]='b';
   }else if( S_ISFIFO(buf->st_mode) ){
       local_buff[i]='p';
   }else if( S_ISSOCK(buf->st_mode) ){
       local_buff[i]='s';
   }else if( S_ISREG(buf->st_mode) ){
       local_buff[i]='-';
   }else{
       local_buff[i]='?';
   }
   i++;
   local_buff[i]=(buf->st_mode & S_IRUSR) ? 'r' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IWUSR) ? 'w' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IXUSR) ? 'x' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IRGRP) ? 'r' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IWGRP) ? 'w' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IXGRP) ? 'x' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IROTH) ? 'r' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IWOTH) ? 'w' : '-'; i++;
   local_buff[i]=(buf->st_mode & S_IXOTH) ? 'x' : '-'; i++;
   local_buff[i]='\0';
   printf("File Permissions ................. %s\n",local_buff);
   printf ("Owner ID ........................ %-8d\n", buf->st_uid);

                                                                          /* Print out owner name if found using getpwuid(). */
   if ((pwd = getpwuid (buf->st_uid)) != NULL){                           /* st_uid          The User ID for the file. */
      printf ("Owner ........................... %-8s\n", pwd->pw_name);
   }
   printf ("Owner ID ........................ %-8d\n", buf->st_uid);
                                                                          /* Print out group name if found using getgrgid(). */
   if ((grp = getgrgid (buf->st_gid)) != NULL){                           /* st_gid          The Group ID for the file. */
      printf ("Group name ...................... %-8s\n", grp->gr_name);
   }
   printf ("Group ID ........................ %-8d\n", buf->st_gid);

   tm = localtime (&buf->st_mtime);                                       /* st_mtime  most recent time file contents modified. */
   strftime (datestring, sizeof (datestring), nl_langinfo (D_T_FMT), tm); /* Get localized date string. */
   printf ("file contents last modified ..... %s\n", datestring );

   tm = localtime (&buf->st_atime);                                       /* st_atime  most recent time file was accessed. */
   strftime (datestring, sizeof (datestring), nl_langinfo (D_T_FMT), tm); /* Get localized date string. */
   printf ("file contents last accessed ..... %s\n", datestring );

   tm = localtime (&buf->st_ctime);                                       /* st_ctime  most recent time file permissions changed. */
   strftime (datestring, sizeof (datestring), nl_langinfo (D_T_FMT), tm); /* Get localized date string. */
   printf ("file permissions last changed ... %s\n", datestring );

   printf ("device .......................... %ld\n", buf->st_dev);       /* st_dev   device file currently resides on. */
   }
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
/* ===============================================================================================================================*/
const char *my_get_perm(long int imode){
static char perms_buff[15];
char ftype = '?';
mode_t mode;

   mode=(mode_t) imode;
   if (S_ISREG(mode))  ftype = '-';
   if (S_ISLNK(mode))  ftype = 'l';
   if (S_ISDIR(mode))  ftype = 'd';
   if (S_ISBLK(mode))  ftype = 'b';
   if (S_ISCHR(mode))  ftype = 'c';
   if (S_ISFIFO(mode)) ftype = 'p';
#ifdef S_ISDOOR
   if (S_ISDOOR(mode)) ftype = 'D'; /* Solaris 2.6, etc. */
#endif

   sprintf(perms_buff, "%c%c%c%c%c%c%c%c%c%c %c%c%c",
      ftype,
      mode &  S_IRUSR ? 'r' : '-',
      mode &  S_IWUSR ? 'w' : '-',
      mode &  S_IXUSR ? 'x' : '-',

      mode &  S_IRGRP ? 'r' : '-',
      mode &  S_IWGRP ? 'w' : '-',
      mode &  S_IXGRP ? 'x' : '-',

      mode &  S_IROTH ? 'r' : '-',
      mode &  S_IWOTH ? 'w' : '-',
      mode &  S_IXOTH ? 'x' : '-',

      mode &  S_ISUID ? 'U' : '-',
      mode &  S_ISGID ? 'G' : '-',
      mode &  S_ISVTX ? 'S' : '-');

   return perms_buff;
}
/* ===============================================================================================================================*/
/*
   To get stream I/O out of stdin and stdout, make a getc and putc callable from Fortran
*/
char getkeyC (void) {
   /* @(#) Driver for reading a character from stdin */
   char c;
   read (0, &c, 1);
   return (c);
}

int putkeyC (char c) {
   /* @(#) Driver for writing a character to stdout */
   write (1, &c, 1);
   return (c);
}
/* ===============================================================================================================================*/

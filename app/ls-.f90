!----------------------------------------------------------------------------------------------------------------------------------
program demo_system_readdir
use M_kracken, only  : kracken, rget, lget, sgets, sget
use M_system, only : system_opendir,system_readdir, system_closedir, system_stat, system_isdir
use iso_c_binding, only : c_ptr
implicit none

! ident_1="@(#) ls-(1f) list files in a directory"

character(len=:),allocatable :: directories(:)
character(len=:),allocatable :: directory
type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
logical                      :: listall
logical                      :: ls_l
logical                      :: ls_csv
integer                      :: i
integer                      :: ierr
character(len=*),parameter   :: default_dfmt='year-month-dayThour:minute:second'
character(len=:),allocatable :: dfmt
!----------------------------------------------------------------------------------------------------------------------------------
   call kracken('ls','. -help F -version F -a .F. -fmt -l .F. -csv .F.')
   call help_usage(lget('ls_help'))
   call help_version(lget('ls_version'))
   ls_l=lget('ls_l')
   ls_csv=lget('ls_csv')
   listall=lget('ls_a')
   dfmt=sget('ls_fmt')
   if(dfmt.eq.'')dfmt=default_dfmt
   directories=sgets('ls_oo')
   if(size(directories).eq.0)then
      directories=['.']
   endif

   do i=1,size(directories)
      if(system_isdir(trim(directories(i))))then
         directory=directories(i)
         if(directory.eq.'')then                             ! pathname is a directory
            directory='.'
         endif
         call system_opendir(directory,dir,ierr)             ! open directory stream to read from
         if(ierr.ne.0)stop 1
         do                                                  ! read directory
            call system_readdir(dir,filename,ierr)
            if(filename.eq.' ')exit
            if(filename(1:1).eq.'.'.and..not.listall)cycle   ! do not list files starting with "." unless -a switch is present
            if(ls_l)then                                     ! if long listing requested write file details
               filename=trim(directory)//'/'//trim(filename)
               call printit()
            elseif(ls_csv)then                               ! if -csv option print file details as CSV
               filename=trim(directory)//'/'//trim(filename)
               call print_csv()
            else                                             ! just print filenames
               write(*,'(a)')filename
            endif
         enddo
         call system_closedir(dir,ierr)                      ! close directory stream
         if(ierr.ne.0)stop 3
      else                                                   ! pathname not a directory just a file
         filename=trim(directories(i))
         if(filename.eq.'')filename='.'
         if(ls_l)then                                        ! if -l parameter present
            call printit()
         elseif(ls_csv)then                                  ! if -csv switch present
            call print_csv()
         else
            write(*,'(a)')filename
         endif
      endif
   enddo
contains
!----------------------------------------------------------------------------------------------------------------------------------
subroutine printit
use M_system, only : system_getpwuid, system_getgrgid, system_perm
use M_time, only :   fmtdate, u2d
implicit none
integer                      :: ierr
integer(kind=8)              :: values(13)
integer(kind=8) :: &
   Device_ID,           Inode_number,          File_mode,                  Number_of_links,  Owner_uid,         &
   Owner_gid,           Directory_device,      File_size,                  Last_access,      Last_modification, &
   Last_status_change,  Preferred_block_size,  Number_of_blocks_allocated
EQUIVALENCE                                      &
   ( VALUES(1)  , Device_ID                  ) , &
   ( VALUES(2)  , Inode_number               ) , &
   ( VALUES(3)  , File_mode                  ) , &
   ( VALUES(4)  , Number_of_links            ) , &
   ( VALUES(5)  , Owner_uid                  ) , &
   ( VALUES(6)  , Owner_gid                  ) , &
   ( VALUES(7)  , Directory_device           ) , &
   ( VALUES(8)  , File_size                  ) , &
   ( VALUES(9)  , Last_access                ) , &
   ( VALUES(10) , Last_modification          ) , &
   ( VALUES(11) , Last_status_change         ) , &
   ( VALUES(12) , Preferred_block_size       ) , &
   ( VALUES(13) , Number_of_blocks_allocated )

   !write(*, FMT="('Inode number:',                T30, I0)",advance='no') values(2)
   !write(*, FMT="(' No. of blocks allocated:',     I0)",advance='no') values(13)

   call system_stat(filename,values,ierr)
   if(ierr.eq.0)then
      write(*, FMT="(o6.0,t7,1x,a)",advance='no') File_mode,system_perm(File_mode)
      write(*, FMT="(1x,I0,t4)",advance='no')     Number_of_links
      write(*, FMT="(1x,A,t10)",advance='no')     system_getpwuid(Owner_uid)
      write(*, FMT="(1x,A,t10)",advance='no')     system_getgrgid(Owner_gid)
      write(*, FMT="(1x,bn,I0,t10)",advance='no') File_size
      write(*, FMT="(1x,A)",advance='no')         fmtdate(u2d(int(max(Last_access,Last_modification,Last_status_change))),dfmt)
      write(*, FMT="(1x,a)")filename
   endif

end subroutine printit
!----------------------------------------------------------------------------------------------------------------------------------
subroutine print_csv
use M_system, only : system_getpwuid, system_getgrgid, system_perm
use M_time, only :   fmtdate, u2d
implicit none
integer                      :: ierr
integer(kind=8)              :: values(13)
logical,save                 :: called=.false.
integer(kind=8) :: &
   Device_ID,           Inode_number,          File_mode,                  Number_of_links,  Owner_uid,         &
   Owner_gid,           Directory_device,      File_size,                  Last_access,      Last_modification, &
   Last_status_change,  Preferred_block_size,  Number_of_blocks_allocated
EQUIVALENCE                                      &
   ( VALUES(1)  , Device_ID                  ) , &
   ( VALUES(2)  , Inode_number               ) , &
   ( VALUES(3)  , File_mode                  ) , &
   ( VALUES(4)  , Number_of_links            ) , &
   ( VALUES(5)  , Owner_uid                  ) , &
   ( VALUES(6)  , Owner_gid                  ) , &
   ( VALUES(7)  , Directory_device           ) , &
   ( VALUES(8)  , File_size                  ) , &
   ( VALUES(9)  , Last_access                ) , &
   ( VALUES(10) , Last_modification          ) , &
   ( VALUES(11) , Last_status_change         ) , &
   ( VALUES(12) , Preferred_block_size       ) , &
   ( VALUES(13) , Number_of_blocks_allocated )

   call system_stat(filename,values,ierr)
   if(ierr.eq.0)then
      if(.not.called)then
         called=.true.
         write(*,'(*(a))')                &
         '"Inode_number",',               &
         '"Number_of_blocks_allocated",', &
         '"File_mode",',                  &
         '"Number_of_links",',            &
         '"Owner",',                      &
         '"Groupname",',                  &
         '"File_size",',                  &
         '"Last_access",',                &
         '"Last_modification",',          &
         '"Last_status_change",',         &
         '"Pathname"'
      endif

      write(*,FMT=101) Inode_number, Number_of_blocks_allocated, system_perm(File_mode), Number_of_links,                 &
              system_getpwuid(Owner_uid),system_getpwuid(Owner_gid), File_size,                                           &
              fmtdate(u2d(Last_access),dfmt), fmtdate(u2d(Last_modification),dfmt),fmtdate(u2d(Last_status_change),dfmt), &
            !!Last_access, Last_modification, Last_status_change,                                                         &
              filename
   endif
   !! 101 format(i0,",",i0,",",a,",",i0,",",a,",",a,",",i0,",",i0,",",i0,",",i0,",",a)
   101 format(i0,",",i0,",",a,",",i0,",",a,",",a,",",i0,",",a,",",a,",",a,",",a)

end subroutine print_csv
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                                                                            ',&
'       ls-(1f) - [FUNIX:FILESYSTEM] list files in a directory                                                                   ',&
'       (LICENSE:PD)                                                                                                             ',&
'SYNOPSIS                                                                                                                        ',&
'       ls- [directory|--version|--help] [ -a] [ -l|-csv]                                                                        ',&
'DESCRIPTION                                                                                                                     ',&
'       Given a directory name list files in the directory                                                                       ',&
'OPTIONS                                                                                                                         ',&
'       pathname    name of directory or pathname to display contents of.                                                        ',&
'                   Defaults to current directory.                                                                               ',&
'       -a          show hidden files (files beginning with ".").                                                                ',&
'       -l          long listing                                                                                                 ',&
'       -fmt        alternate format for date and time. Calls fmtdate(3f).                                                       ',&
'       -csv        generate output as a CSV file. Filenames should not have                                                     ',&
'                   ,"'' characters in them. Very useful for use with sqlite3(1)                                                 ',&
'                   and making a file that can be read into most spreadsheets,                                                   ',&
'       --help      display command help and exit                                                                                ',&
'       --version   output version information and exit                                                                          ',&
'EXAMPLES                                                                                                                        ',&
' Sample command lines ...                                                                                                       ',&
'                                                                                                                                ',&
'        ls-                                                                                                                     ',&
'        ls- . /tmp -l                                                                                                           ',&
'                                                                                                                                ',&
'        # add Unix Epoch date                                                                                                   ',&
'        ls- -l -fmt year-month-day hour:minute:second epoch                                                                     ',&
'                                                                                                                                ',&
'        # use the phase of the moon for the date                                                                                ',&
'        # ls- -l -fmt %p %P                                                                                                     ',&
'                                                                                                                                ',&
'EXTENDED SQLITE EXAMPLE                                                                                                         ',&
'                                                                                                                                ',&
'  The CSV output can often just be read by spreadsheets. Typically the                                                          ',&
'  file suffix ".csv" is required. Assuming you have bash(1), sqlite3(1)                                                         ',&
'  and column(1) on your platform this is an example script that shows                                                           ',&
'  how SQL statements can be used to generate many kinds of file reports                                                         ',&
'  (number of bytes owned by users, number of files, sorting, ... It                                                             ',&
'  assumes you or somone who will assist you is familiar with SQL and                                                            ',&
'  sqlite3(1):                                                                                                                   ',&
'                                                                                                                                ',&
'   #!/bin/bash                                                                                                                  ',&
'   #@(#) list files accessed today in current directory                                                                         ',&
'   export SCRATCH=/tmp/$(uuidgen).csv          # create scratch file name                                                       ',&
'   trap "/bin/rm -f $SCRATCH" EXIT             # ensure scratch file is removed                                                 ',&
'   ls- -csv -- . |tail -n +2>$SCRATCH          # generate CSV file                                                              ',&
'   (                                                                                                                            ',&
'   # read CSV file into an SQLite file and generate a report as HTML table                                                      ',&
'   sqlite3 \                                                                                                                    ',&
'    -cmd ''CREATE TABLE directory("Inode_number" INT,                                                                           ',&
'      "Number_of_blocks_allocated" INT,                                                                                         ',&
'      "File_mode" TEXT,                                                                                                         ',&
'      "Number_of_links" INT,                                                                                                    ',&
'      "Owner" TEXT,                                                                                                             ',&
'      "Groupname" TEXT,                                                                                                         ',&
'      "File_size" INT,                                                                                                          ',&
'      "Last_access" DATE,                                                                                                       ',&
'      "Last_modification" DATE,                                                                                                 ',&
'      "Last_status_change" DATE,                                                                                                ',&
'      "Pathname" TEXT );'' \                                                                                                    ',&
'      -cmd ''.mode csv'' \                                                                                                      ',&
'      -cmd ".import $SCRATCH directory" <<\end_of_file                                                                          ',&
'   -- .schema                                                                                                                   ',&
'   .mode column                                                                                                                 ',&
'   .header on                                                                                                                   ',&
'   SELECT Pathname, File_mode, Owner, Groupname, File_size, strftime(''%Y-%m-%d %H:%M:%S'', Last_access) as "Last_Access"       ',&
'      FROM directory                                                                                                            ',&
'      WHERE DATE(''now'', ''start of day'') < Last_access                                                                       ',&
'      ORDER BY Pathname ASC;                                                                                                    ',&
'   end_of_file                                                                                                                  ',&
'   )| column -t -s ''|''                                                                                                        ',&
'   exit                                                                                                                         ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        ls-(1f)>',&
'@(#)DESCRIPTION:    list files in a directory>',&
'@(#)VERSION:        1.0, 20161120>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2023-02-12 12:23:02 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demo_system_readdir
!----------------------------------------------------------------------------------------------------------------------------------

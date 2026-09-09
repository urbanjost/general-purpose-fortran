program change_exe

! ident_1="@(#) changen(1f) rename files by changing old fixed string to new string"

use M_CLI2, only : set_args, sgets, lget, sget, names=>unnamed
use M_strings, only : substitute
implicit none
character(len=:),allocatable       :: old, new
character(len=:),allocatable       :: newname
character(len=:),allocatable       :: cmd
character(len=:),allocatable       :: action
character(len=:),allocatable       :: help_text(:)
character(len=:),allocatable       :: version_text(:)
character(len=:),allocatable       :: default
integer                            :: i, ierr
logical                            :: dryrun, verbose
integer                            :: cstat
integer                            :: big
character(len=256)                 :: sstat

   call setup()
   call set_args( ' -dryrun F -cmd "mv -i" ',help_text,version_text) ! parse command line
   dryrun=lget('dryrun')
   verbose=lget('verbose')
   cmd=trim(sget('cmd'))
   if(verbose)then
      write(*,*)'cmd:    ',cmd
      write(*,*)'verbose:',verbose
      write(*,*)'dryrun: ',dryrun
   endif
   if(size(names).eq.2)then
      default='*'//trim(names(1))
      big=max(len(default),len(names))
      names=[character(len=big) :: names,default]
   endif

   select case(size(names))
   case(3:)
      old=trim(names(1))
      new=trim(names(2))
      do i=3,size(names,dim=1)
         newname=names(i)//repeat(' ',256)
         call substitute(newname,old,new,ierr)                       ! if ierr>0 it is the number of changes made to string
         if(ierr.gt.0)then
            action=cmd//' '//trim(names(i))//' '//newname
            if(dryrun.or.verbose)then
               write(*,'(a)') trim(action)
            endif
            if(.not.dryrun)then
               call execute_command_line(action,cmdstat=cstat,cmdmsg=sstat)
            endif
         endif
      enddo
   case default
      write(*,*)'<ERROR>*matchn* incorrect number of parameters'
   endselect
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   changen(1f) - [FILE EDIT] replace old fixed string with new fixed',&
'   string in names of files                                         ',&
'   (LICENSE:PD)                                                     ',&
'                                                                    ',&
'SYNOPSIS                                                            ',&
' combinations:                                                      ',&
'                                                                    ',&
'   changen [ -dryrun][ -cmd COMMAND] old new  FILENAMES             ',&
'    or                                                              ',&
'   changen  --version| --help                                       ',&
'                                                                    ',&
'DESCRIPTION                                                         ',&
'   Given an old and new string and a list of filenames replace all  ',&
'   occurrences of the original string with the new string in the    ',&
'   filenames.                                                       ',&
'                                                                    ',&
'OPTIONS                                                             ',&
'   old,new       change occurrences of old string to new string in  ',&
'                 filenames. Trailing spaces are ignored.            ',&
'   FILENAMES     names of files to rename. default is "*old".       ',&
'   -dryrun       write the commands to stdout instead of executing them',&
'   -verbose      echo the commands to be executed                      ',&
'   -cmd COMMAND  change command from "mv -i" to specified command name ',&
'   --help        display this help and exit                            ',&
'   --version     output version information and exit                   ',&
'                                                                       ',&
'EXAMPLE                                                                ',&
'  Sample commands:                                                     ',&
'                                                                       ',&
'   # change all files with .f90 in their name to names ending in .F90  ',&
'   changen .f90 .F90 *.f90                                             ',&
'   # copy all files with .f90 in their name to names ending in .F90    ',&
'   changen .f90 .F90 *.f90 -cmd cp                                     ',&
'AUTHOR                                                                 ',&
'   John S. Urban                                                       ',&
'LICENSE                                                                ',&
'   Public Domain                                                       ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        changen(1f)>                                         ',&
'@(#)DESCRIPTION:    rename files by changing old fixed string to new string>',&
'@(#)VERSION:        1.0, 2017-06-29>                                        ',&
'@(#)AUTHOR:         John S. Urban>                                          ',&
'']
end subroutine setup
end program change_exe

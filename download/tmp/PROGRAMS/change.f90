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
'NAME                                                                            ',&
'   change(1f) - replace old fixed string with new fixed string in filenames     ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   change c/old/new/ FILENAMES [-dryrun][-cmd COMMAND]| --version| --help       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a change directive and a list of filenames replace all occurrences of  ',&
'   the original string with the new string.                                     ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   c/old/new/    change occurrences of old string to new string in filenames    ',&
'   FILENAMES     names of files to rename                                       ',&
'   -dryrun       write the commands to stdout instead of executing them         ',&
'   -cmd COMMAND  change command from "mv" to specified command name             ',&
'   --help        display this help and exit                                     ',&
'   --version     output version information and exit                            ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'   # change all files with .f90 in their name to .F90                           ',&
'   change c/.f90/.F90/  *.f90                                                   ',&
'   change c@/usr/bin/@/bin/@  /usr/bin/*                                        ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    change(1f) - replace old fixed string with new fixed string in filenames
!!
!!##SYNOPSIS
!!
!!    change c/old/new/ FILENAMES [-dryrun][-cmd COMMAND]| --version| --help
!!
!!##DESCRIPTION
!!    Given a change directive and a list of filenames replace all occurrences of
!!    the original string with the new string.
!!
!!##OPTIONS
!!    c/old/new/    change occurrences of old string to new string in filenames
!!    FILENAMES     names of files to rename
!!    -dryrun       write the commands to stdout instead of executing them
!!    -cmd COMMAND  change command from "mv" to specified command name
!!    --help        display this help and exit
!!    --version     output version information and exit
!!
!!##EXAMPLE
!!
!!    # change all files with .f90 in their name to .F90
!!    change c/.f90/.F90/  *.f90
!!    change c@/usr/bin/@/bin/@  /usr/bin/*
!===================================================================================================================================
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        change(1f)>',&
'@(#)DESCRIPTION:    rename files by changing old fixed string to new string>',&
'@(#)VERSION:        1.0, 2017-06-29>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Thu, Jun 29th, 2017 2:09:27 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_change
use M_kracken, only : kracken, sgets, lget, IPvalue, sget
use M_strings, only : change
implicit none
character(len=*),parameter :: ident="@(#) given string of form days-hh:mm:ss convert to seconds'"
character(len=IPvalue),allocatable :: names(:)
character(len=IPvalue)             :: newname
character(len=:),allocatable       :: cmd
character(len=:),allocatable       :: action
integer                            :: i, ierr
logical                            :: dryrun, verbose

   call kracken('change', &
   & ' -oo -help .F. -version .F. -dryrun .F. -cmd mv -verbose .F.') ! parse command line
   call help_usage(lget('change_help'))                                     ! display help information and stop if true
   call help_version(lget('change_version'))                                ! display version information and stop if true
   dryrun=lget('change_dryrun')
   verbose=lget('change_verbose')
   cmd=sget('change_cmd')

   names=sgets('change_oo')
   select case(size(names))
   case(1)
   case(2:)
      do i=2,size(names,dim=1)
         newname=names(i)
         call change(newname,names(1),ierr)                                 ! if ierr>0 it is the number of changes made to string
         if(ierr.gt.0)then
            action=cmd//' '//trim(names(i))//' '//newname
            if(dryrun.or.verbose)then
               write(*,*) trim(action)
            endif
            if(.not.dryrun)then
               call execute_command_line(action)
            endif
         endif
      enddo
   endselect
end program demo_change

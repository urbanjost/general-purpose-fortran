!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program rename
use M_strings, only : replace
use M_kracken, only : kracken, lget, sget, sgets
use M_kracken, only : IPvalue ! length of keyword value
use M_kracken, only : kracken_comment
use M_system,  only : system_rename, system_perror
implicit none
logical                            :: verbose
logical                            :: dryrun
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: old, new
character(len=:),allocatable       :: oldname
character(len=:),allocatable       :: newname
integer                            :: i
integer                            :: ierr
!-----------------------------------------------------------------------------------------------------------------------------------
   kracken_comment=char(0)
   ! define command arguments,default values and crack command line
   call kracken('rename','-help .false. -version .false. -verbose .false. -dryrun .false.')
!-----------------------------------------------------------------------------------------------------------------------------------
   call help_usage(lget('rename_help'))                             ! if -help option is present, display help text and exit
   call help_version(lget('rename_version'))                        ! if -version option is present, display version text and exit
   filenames=sgets('rename_oo')
   if(size(filenames).gt.2)then
      old=trim(filenames(1))
      new=trim(filenames(2))
   else
      write(*,*)'*rename-* error -- must supply old and new string and filenames'
      stop
   endif
   verbose=lget('rename_verbose')
   dryrun=lget('rename_dryrun')
   if(dryrun)verbose=.true.
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=3,size(filenames)
      oldname=trim(filenames(i))
      newname=trim(replace(oldname,old,new))
      if(oldname.eq.newname)cycle
      if(verbose)then
         write(*,*)'*rename-* move '//oldname//' to '//newname
      endif
      if(.not.dryrun)then
         ierr=system_rename(oldname,newname)
         if(ierr.ne.0)then
            call system_perror('*rename* error renaming'//oldname)
         endif
      endif
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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
'   rename-(1f) - [FUNIX:FILESYSTEM] rename files by replacing first occurrence of a string in a filename with new string        ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   rename- old new file... [ -verbose][ -dryrun]|[ -help|-version]                                                              ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   rename-(1f) will rename the specified files by replacing the                                                                 ',&
'   first occurrence of expression in their name by replacement.                                                                 ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'   Given the files foo1, ..., foo9, foo10, ..., foo278, the commands                                                            ',&
'                                                                                                                                ',&
'      rename- foo foo0 foo?                                                                                                     ',&
'      rename- foo foo0 foo??                                                                                                    ',&
'                                                                                                                                ',&
'   will turn them into foo001, ..., foo009, foo010, ..., foo278. And                                                            ',&
'                                                                                                                                ',&
'      rename- .htm .html *.htm                                                                                                  ',&
'                                                                                                                                ',&
'   will fix the extension of *.htm files.                                                                                       ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'    old        represents a string to change in filenames                                                                       ',&
'    new        the replacement string for EXPRESSION.                                                                           ',&
'    -verbose   Print information about what the program does.                                                                   ',&
'    --help     Display a help message and exit.                                                                                 ',&
'    --version  Display version information and exit.                                                                            ',&
'    --dryrun   Does all file operations except for moving the                                                                   ',&
'               changed file back to the original. Implies --version.                                                            ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    rename-(1f) - [FUNIX:FILESYSTEM] rename files by replacing first occurrence of a string in a filename with new string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    rename- old new file... [ -verbose][ -dryrun]|[ -help|-version]
!!
!!##DESCRIPTION
!!    rename-(1f) will rename the specified files by replacing the
!!    first occurrence of expression in their name by replacement.
!!
!!##EXAMPLES
!!
!!    Given the files foo1, ..., foo9, foo10, ..., foo278, the commands
!!
!!       rename- foo foo0 foo?
!!       rename- foo foo0 foo??
!!
!!    will turn them into foo001, ..., foo009, foo010, ..., foo278. And
!!
!!       rename- .htm .html *.htm
!!
!!    will fix the extension of *.htm files.
!!
!!##OPTIONS
!!     old        represents a string to change in filenames
!!     new        the replacement string for EXPRESSION.
!!     -verbose   Print information about what the program does.
!!     --help     Display a help message and exit.
!!     --version  Display version information and exit.
!!     --dryrun   Does all file operations except for moving the
!!                changed file back to the original. Implies --version.
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
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
'@(#)PROGRAM:        rename-(1)>',&
'@(#)DESCRIPTION:    replace strings in filenames>',&
'@(#)VERSION:        1.0, 20181020>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2022-01-09 23:08:33 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program rename
!-----------------------------------------------------------------------------------------------------------------------------------

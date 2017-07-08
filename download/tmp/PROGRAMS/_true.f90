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
'       _true(1f) - [FUNIX]do nothing, successfully                              ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       _true [ignored command line arguments]                                   ',&
'       _true OPTION                                                             ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       Exit with a status code indicating failure.                              ',&
'                                                                                ',&
'       --verbose  display an ASCII graphic of a shining sun                     ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'       _true && echo SUCCESSFUL || echo DID NOT WORK                            ',&
'SEE ALSO                                                                        ',&
'       _true(1f)                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _true(1f) - [FUNIX]do nothing, successfully
!!
!!##SYNOPSIS
!!
!!        _true [ignored command line arguments]
!!        _true OPTION
!!
!!##DESCRIPTION
!!        Exit with a status code indicating failure.
!!
!!        --verbose  display an ASCII graphic of a shining sun
!!        --help     display this help and exit
!!        --version  output version information and exit
!!
!!##EXAMPLE
!!
!!        _true && echo SUCCESSFUL || echo DID NOT WORK
!!##SEE ALSO
!!        _true(1f)
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
'@(#)PROGRAM         _true(1f)>',&
'@(#)DESCRIPTION:    do nothing, successfully>',&
'@(#)VERSION:        1.0, 20170125>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:16:07 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program true
use M_kracken, only : kracken, lget
use M_messages, only : junsun
implicit none
character(len=*),parameter::ident="@(#)_true(1f): do nothing, successfully"
call kracken('true','-help .F. -version .F. -verbose .F.')
call help_usage(lget('true_help'))      ! if -help option is present, display help text and exit
call help_version(lget('true_version')) ! if -version option is present, display version text and exit
if(lget('true_verbose'))then
   call junsun('s')
endif
stop
end program true

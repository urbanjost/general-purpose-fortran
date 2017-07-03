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
'   sec2days - [TIME] Convert seconds to string of form dd-hh:mm:ss              ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   sec2days nnnn | --version| --help                                            ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given an integer string representing seconds convert it to a string          ',&
'   of the form                                                                  ',&
'                                                                                ',&
'      dd-hh:mm:ss                                                               ',&
'                                                                                ',&
'   Where dd is days, hh hours, mm minutes and ss seconds.                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   nnnn        number of seconds to convert to string of form dd-hh:mm:ss       ',&
'   --help      display this help and exit                                       ',&
'   --version   output version information and exit                              ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'   sec2days 129860                                                              ',&
'                                                                                ',&
'   1-12:04:20                                                                   ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    sec2days - [TIME] Convert seconds to string of form dd-hh:mm:ss
!!
!!##SYNOPSIS
!!
!!    sec2days nnnn | --version| --help
!!
!!##DESCRIPTION
!!    Given an integer string representing seconds convert it to a string
!!    of the form
!!
!!       dd-hh:mm:ss
!!
!!    Where dd is days, hh hours, mm minutes and ss seconds.
!!
!!##OPTIONS
!!    nnnn        number of seconds to convert to string of form dd-hh:mm:ss
!!    --help      display this help and exit
!!    --version   output version information and exit
!!
!!##EXAMPLE
!!
!!    sec2days 129860
!!
!!    1-12:04:20
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
'@(#)PROGRAM:        sec2days(1f)>',&
'@(#)DESCRIPTION:    convert seconds to string of form dd-hh:mm:ss>',&
'@(#)VERSION:        1.0, 2016-06-17>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:05:56 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_sec2days
use M_kracken, only: kracken, dget, lget
use M_time, only :   sec2days
implicit none
character(len=*),parameter :: ident="@(#) given string of form days-hh:mm:ss convert to seconds'"
doubleprecision  :: dvalue
   call kracken('sec2days',' -oo -help .F. -version .F.')                    ! parse command line
   call help_usage(lget('sec2days_help'))                                    ! display help information and stop if true
   call help_version(lget('sec2days_version'))                               ! display version information and stop if true

   dvalue=dget('sec2days_oo') ! on error, dget writes messages so do not use it directly in the write statement with old compilers
   write(*,'(a)')sec2days(dvalue)

end program demo_sec2days

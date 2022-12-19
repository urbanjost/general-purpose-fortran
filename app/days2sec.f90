program demo_days2sec
use M_kracken95, only: kracken, sget, lget, rget
use M_time,    only: days2sec
use M_strings, only: v2s
implicit none
character(len=*),parameter   :: ident="@(#)days2sec(1): given string of form dd-hh:mm:ss convert to seconds"
character(len=:),allocatable :: printline
   call kracken('days2sec',' -oo -help .F. -version .F. -denominator 1')     ! parse command line
   call help_usage(lget('days2sec_help'))                                    ! display help information and stop if true
   call help_version(lget('days2sec_version'))                               ! display version information and stop if true
   ! get value from command line with SGET, convert to seconds with DAYS2SEC, and make into a nicer value string with V2S
   printline=v2s(days2sec(sget('days2sec_oo'))/rget('days2sec_denominator')) ! not in write statement so error message will print
   write(*,'(a)')printline
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
'   days2sec(1f) - [TIME] Convert [[-]dd-][[hh:]mm:]ss to seconds                                                                ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   days2sec dd-hh:mm:ss | --version| --help                                                                                     ',&
'   days2sec NNdNNhNNmNNs                                                                                                        ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Given a duration in the form dd-hh:mm:ss where dd is days, hh hours,                                                         ',&
'   mm minutes and ss seconds convert it to seconds. Many utilities (ps(1),                                                      ',&
'   for example) show times in this format to make it more intelligible;                                                         ',&
'   but it generally easier to perform math on values represented in                                                             ',&
'   seconds.                                                                                                                     ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   dd-hh:mm:ss  Given a string representing a duration of time in the                                                           ',&
'                following forms:                                                                                                ',&
'                                                                                                                                ',&
'                  dd-hh:mm:ss                                                                                                   ',&
'                     hh:mm:ss                                                                                                   ',&
'                        mm:ss                                                                                                   ',&
'                           ss                                                                                                   ',&
'                                                                                                                                ',&
'                convert it to seconds.                                                                                          ',&
'                                                                                                                                ',&
'                The numeric values may represent floating point numbers.                                                        ',&
'                                                                                                                                ',&
'                Spaces are ignored.                                                                                             ',&
'                                                                                                                                ',&
'    NNdNNhNNmNNs  Simple numeric values may also be used with unit suffixes;                                                    ',&
'                  where s,m,h, or d represents seconds, minutes, hours                                                          ',&
'                  or days and w represents weeks. Allowed aliases for w,d,h,m, and s units are                                  ',&
'                                                                                                                                ',&
'                   w -  weeks,week,wk,wks                                                                                       ',&
'                   d -  days,day                                                                                                ',&
'                   m -  minutes,minute,min                                                                                      ',&
'                   h -  hours,hour,hrs,hr                                                                                       ',&
'                   s -  seconds,second,sec,secs                                                                                 ',&
'                                                                                                                                ',&
'                  The numeric values may represent floating point numbers.                                                      ',&
'                                                                                                                                ',&
'                  Spaces, commas  and case are ignored.                                                                         ',&
'                                                                                                                                ',&
'   --denominator  divide the result by this value. Default is one(1).                                                           ',&
'   --help         display this help and exit                                                                                    ',&
'   --version      output version information and exit                                                                           ',&
'                                                                                                                                ',&
'EXAMPLE                                                                                                                         ',&
'  Usage                                                                                                                         ',&
'                                                                                                                                ',&
'    days2sec 1-12:04:20                                                                                                         ',&
'    129860                                                                                                                      ',&
'    days2sec 1.5 days                                                                                                           ',&
'    129600                                                                                                                      ',&
'    days2sec 1.5 days 4hrs 30minutes                                                                                            ',&
'    145800                                                                                                                      ',&
'    days2sec 10s 10S 10s # DUPLICATES WITH UNITS ARE ALLOWED                                                                    ',&
'    30                                                                                                                          ',&
'    days2sec 1 1 1  # SPACES ARE IGNORED                                                                                        ',&
'    111                                                                                                                         ',&
'SEE ALSO                                                                                                                        ',&
'    sec2days(1)                                                                                                                 ',&
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
!!    days2sec(1f) - [TIME] Convert [[-]dd-][[hh:]mm:]ss to seconds
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    days2sec dd-hh:mm:ss | --version| --help
!!    days2sec NNdNNhNNmNNs
!!
!!##DESCRIPTION
!!    Given a duration in the form dd-hh:mm:ss where dd is days, hh hours,
!!    mm minutes and ss seconds convert it to seconds. Many utilities (ps(1),
!!    for example) show times in this format to make it more intelligible;
!!    but it generally easier to perform math on values represented in
!!    seconds.
!!
!!##OPTIONS
!!    dd-hh:mm:ss  Given a string representing a duration of time in the
!!                 following forms:
!!
!!                   dd-hh:mm:ss
!!                      hh:mm:ss
!!                         mm:ss
!!                            ss
!!
!!                 convert it to seconds.
!!
!!                 The numeric values may represent floating point numbers.
!!
!!                 Spaces are ignored.
!!
!!     NNdNNhNNmNNs  Simple numeric values may also be used with unit suffixes;
!!                   where s,m,h, or d represents seconds, minutes, hours
!!                   or days and w represents weeks. Allowed aliases for w,d,h,m, and s units are
!!
!!                    w -  weeks,week,wk,wks
!!                    d -  days,day
!!                    m -  minutes,minute,min
!!                    h -  hours,hour,hrs,hr
!!                    s -  seconds,second,sec,secs
!!
!!                   The numeric values may represent floating point numbers.
!!
!!                   Spaces, commas  and case are ignored.
!!
!!    --denominator  divide the result by this value. Default is one(1).
!!    --help         display this help and exit
!!    --version      output version information and exit
!!
!!##EXAMPLE
!!
!!   Usage
!!
!!     days2sec 1-12:04:20
!!     129860
!!     days2sec 1.5 days
!!     129600
!!     days2sec 1.5 days 4hrs 30minutes
!!     145800
!!     days2sec 10s 10S 10s # DUPLICATES WITH UNITS ARE ALLOWED
!!     30
!!     days2sec 1 1 1  # SPACES ARE IGNORED
!!     111
!!##SEE ALSO
!!     sec2days(1)
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
'@(#)PROGRAM:        days2sec(1f)>',&
'@(#)DESCRIPTION:    convert dd-hh:mm:ss string to seconds>',&
'@(#)VERSION:        1.0, 2016-06-17>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2022-12-18 00:51:11 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demo_days2sec

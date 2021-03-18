program demo_sec2days
use M_kracken95, only : kracken, lget, sget, IPvalue
use M_time,    only : sec2days, realtime
use M_strings, only : substitute
implicit none
character(len=*),parameter     :: ident="@(#)sec2days(1f): convert seconds to string of form dd-hh:mm:ss"
character(len=:),allocatable   :: strlocal
character(len=:),allocatable   :: radix
character(len=IPvalue)         :: line
   call kracken('sec2days',' -oo -crop .F -radix . -help .F. -version .F.') ! parse command line
   call help_usage(lget('sec2days_help'))                                   ! display help information and stop if true
   call help_version(lget('sec2days_version'))                              ! display version information and stop if true
   radix=trim(sget('sec2days_radix'))
   line=sget('sec2days_oo')
   if(radix.ne.'.')then
      call substitute(line,'.',' ')
      call substitute(line,radix,'.')
   endif
   strlocal=sec2days(trim(line),lget('sec2days_crop')) ! get command line option and convert to dd-hh:mm:ss string
   write(*,'(a)')strlocal
contains
subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   sec2days(1f) - [TIME] Convert seconds to string of form dd-hh:mm:ss          ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   sec2days nnnn[.xxx] [ -crop]| --version| --help                              ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a numeric string representing seconds convert it to a string           ',&
'   of the form                                                                  ',&
'                                                                                ',&
'      dd-hh:mm:ss                                                               ',&
'                                                                                ',&
'   where dd is days, hh hours, mm minutes and ss seconds.                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   nnnn[.xxx]  number of seconds to convert to string of form dd-hh:mm:ss.      ',&
'               nnnn may be interspersed with unit codes d,h,m,s. Spaces,        ',&
'               commas and case are ignored. Allowed aliases for the unit        ',&
'               codes are                                                        ',&
'                 d  days and day                                                ',&
'                 h  hours,hour,hrs, and hr                                      ',&
'                 m  minutes,minute and min                                      ',&
'                 s  seconds,second and sec                                      ',&
'                                                                                ',&
'   -crop       trim leading zero values from output                             ',&
'   -radix      character used as decimal separator                              ',&
'   --help      display this help and exit                                       ',&
'   --version   output version information and exit                              ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
' usage                                                                          ',&
'                                                                                ',&
'   sec2days 129860                                                              ',&
'   1-12:04:20                                                                   ',&
'   sec2days 1d2h3m4s                                                            ',&
'   1-02:03:04                                                                   ',&
'   sec2days 1.0 days 2 hours 3 minutes 4 seconds                                ',&
'   1-02:03:04                                                                   ',&
'   sec2days 1.5d                                                                ',&
'   1-12:00:00                                                                   ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    sec2days(1f) - [TIME] Convert seconds to string of form dd-hh:mm:ss
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    sec2days nnnn[.xxx] [ -crop]| --version| --help
!!
!!##DESCRIPTION
!!    Given a numeric string representing seconds convert it to a string
!!    of the form
!!
!!       dd-hh:mm:ss
!!
!!    where dd is days, hh hours, mm minutes and ss seconds.
!!
!!##OPTIONS
!!    nnnn[.xxx]  number of seconds to convert to string of form dd-hh:mm:ss.
!!                nnnn may be interspersed with unit codes d,h,m,s. Spaces,
!!                commas and case are ignored. Allowed aliases for the unit
!!                codes are
!!                  d  days and day
!!                  h  hours,hour,hrs, and hr
!!                  m  minutes,minute and min
!!                  s  seconds,second and sec
!!
!!    -crop       trim leading zero values from output
!!    -radix      character used as decimal separator
!!    --help      display this help and exit
!!    --version   output version information and exit
!!
!!##EXAMPLE
!!
!!  usage
!!
!!    sec2days 129860
!!    1-12:04:20
!!    sec2days 1d2h3m4s
!!    1-02:03:04
!!    sec2days 1.0 days 2 hours 3 minutes 4 seconds
!!    1-02:03:04
!!    sec2days 1.5d
!!    1-12:00:00
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
! @(#)help_version(3f): prints version information
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        sec2days(1f)>',&
'@(#)DESCRIPTION:    convert seconds to string of form dd-hh:mm:ss>',&
'@(#)VERSION:        1.0, 2016-06-17>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Mon, Mar 15th, 2021 12:50:04 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
end program demo_sec2days

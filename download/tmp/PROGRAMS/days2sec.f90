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
'   days2sec - [TIME] Convert [[-]dd-][[hh:]mm:]ss to seconds                    ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   days2sec dd-hh:mm:ss | --version| --help                                     ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a duration in the form dd-hh:mm:ss where dd is days, hh hours,         ',&
'   mm minutes and ss seconds convert it to seconds. Many utilities (ps(1),      ',&
'   for example) show times in this format to make it more intelligible;         ',&
'   but it generally easier to perform math on values represented in             ',&
'   seconds.                                                                     ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   dd-hh:mm:ss  Given a string representing a duration of time in the           ',&
'                following forms:                                                ',&
'                                                                                ',&
'                  dd-hh:mm:ss                                                   ',&
'                     hh:mm:ss                                                   ',&
'                        mm:ss                                                   ',&
'                           ss                                                   ',&
'                convert it to seconds. If a value is not specified on           ',&
'                the command line, values are read one per line from stdin.      ',&
'   --help       display this help and exit                                      ',&
'   --version    output version information and exit                             ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'    days2sec 1-12:04:20                                                         ',&
'                                                                                ',&
'    129860                                                                      ',&
'SEE ALSO                                                                        ',&
'    sec2days(1)                                                                 ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    days2sec - [TIME] Convert [[-]dd-][[hh:]mm:]ss to seconds
!!
!!##SYNOPSIS
!!
!!    days2sec dd-hh:mm:ss | --version| --help
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
!!                 convert it to seconds. If a value is not specified on
!!                 the command line, values are read one per line from stdin.
!!    --help       display this help and exit
!!    --version    output version information and exit
!!
!!##EXAMPLE
!!
!!     days2sec 1-12:04:20
!!
!!     129860
!!##SEE ALSO
!!     sec2days(1)
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
'@(#)PROGRAM:        days2sec(1f)>',&
'@(#)DESCRIPTION:    convert dd-hh:mm:ss string to seconds>',&
'@(#)VERSION:        1.0, 2016-06-17>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:01:48 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_days2sec
use M_kracken, only: kracken, sget, lget
use M_time, only :   days2sec
implicit none
character(len=*),parameter :: ident="@(#) given string of form days-hh:mm:ss convert to seconds'"
doubleprecision            :: dvalue
integer                    :: ios
character(len=80)          :: line
   call kracken('days2sec',' -oo -help .F. -version .F.')                    ! parse command line
   call help_usage(lget('days2sec_help'))                                    ! display help information and stop if true
   call help_version(lget('days2sec_version'))                               ! display version information and stop if true
   if(sget('days2sec_oo').ne.' ')then
      dvalue=days2sec(sget('days2sec_oo')) ! call this way instead of in write statement so error message will print
      write(*,'(i0)')int(dvalue)
   else
      INFINITE: do
         read(*,'(a)',iostat=ios)line
         if(ios.ne.0)exit INFINITE
         dvalue=days2sec(line)
         write(*,'(i0)')int(dvalue)
      enddo INFINITE
   endif
end program demo_days2sec

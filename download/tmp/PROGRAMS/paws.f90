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
'   paws - [TIME] pause until specified time or for specified duration           ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   paws dd-hh:mm:ss[.xxx][-msg message][-repeat TIMES[-fmt ]]|...               ',&
'   [-uet|-jed|-dat|[-date|-until]]                                              ',&
'   paws --version|--help                                                        ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh hours,     ',&
'   mm minutes and ss.xxx seconds convert it to seconds. Then, pause for that    ',&
'   many seconds. Alternatively, pause until specified date has passed.          ',&
'   If no duration is specified wait until a carriage return is entered.         ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   dd-hh:mm:ss  Given a string representing a duration of time in the           ',&
'                following forms:                                                ',&
'                                                                                ',&
'                  dd-hh:mm:ss[.xx]                                              ',&
'                     hh:mm:ss[.xx]                                              ',&
'                        mm:ss[.xx]                                              ',&
'                           ss[.xx]                                              ',&
'                                                                                ',&
'                convert it to seconds and pause for that amount of time.        ',&
'   -date|-until wait until the specified date has passed (before starting       ',&
'                optional pause). See guessdate(3f) for syntax allowed for       ',&
'                the date.                                                       ',&
'   -uet         wait until the specified Unix Epoch Time has passed             ',&
'   -jed         wait until the specified Julian Ephemeris Date has passed       ',&
'   -dat         wait until the specified date vector has passed                 ',&
'                (year month day timezone hour minutes seconds milliseconds)     ',&
'   -repeat NNN  The duration is repeated NNN times with the date displaced      ',&
'                at the end of each pause.                                       ',&
'   --msg        message to display before pausing                               ',&
'   --help       display this help and exit                                      ',&
'   --version    output version information and exit                             ',&
'                                                                                ',&
'   For more information on the format of the dates, see the now(1) command.     ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  Typical usage:                                                                ',&
'                                                                                ',&
'   paws 2:00:00              # pause for two hours                              ',&
'   paws 3600                 # pause one hour                                   ',&
'   paws 0.10                 # pause one tenth of a second                      ',&
'   paws 1 -repeat 60         # pause sixty seconds, display the date each second',&
'   paws -until 23:59:59      # pause until midnight                             ',&
'   paws 15:00 -date 23:59:59 # pause till midnight then an additional 15 minutes',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    paws - [TIME] pause until specified time or for specified duration
!!
!!##SYNOPSIS
!!
!!    paws dd-hh:mm:ss[.xxx][-msg message][-repeat TIMES[-fmt ]]|...
!!    [-uet|-jed|-dat|[-date|-until]]
!!    paws --version|--help
!!
!!##DESCRIPTION
!!    Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh hours,
!!    mm minutes and ss.xxx seconds convert it to seconds. Then, pause for that
!!    many seconds. Alternatively, pause until specified date has passed.
!!    If no duration is specified wait until a carriage return is entered.
!!
!!##OPTIONS
!!    dd-hh:mm:ss  Given a string representing a duration of time in the
!!                 following forms:
!!
!!                   dd-hh:mm:ss[.xx]
!!                      hh:mm:ss[.xx]
!!                         mm:ss[.xx]
!!                            ss[.xx]
!!
!!                 convert it to seconds and pause for that amount of time.
!!    -date|-until wait until the specified date has passed (before starting
!!                 optional pause). See guessdate(3f) for syntax allowed for
!!                 the date.
!!    -uet         wait until the specified Unix Epoch Time has passed
!!    -jed         wait until the specified Julian Ephemeris Date has passed
!!    -dat         wait until the specified date vector has passed
!!                 (year month day timezone hour minutes seconds milliseconds)
!!    -repeat NNN  The duration is repeated NNN times with the date displaced
!!                 at the end of each pause.
!!    --msg        message to display before pausing
!!    --help       display this help and exit
!!    --version    output version information and exit
!!
!!    For more information on the format of the dates, see the now(1) command.
!!
!!##EXAMPLE
!!
!!   Typical usage:
!!
!!    paws 2:00:00              # pause for two hours
!!    paws 3600                 # pause one hour
!!    paws 0.10                 # pause one tenth of a second
!!    paws 1 -repeat 60         # pause sixty seconds, display the date each second
!!    paws -until 23:59:59      # pause until midnight
!!    paws 15:00 -date 23:59:59 # pause till midnight then an additional 15 minutes
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
'@(#)PROGRAM:        paws(1f)>',&
'@(#)DESCRIPTION:    pause until specified time or for specified duration>',&
'@(#)VERSION:        1.0, 20160731>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:04:50 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_sleep
use M_kracken, only: kracken, sget, lget, dget, igets, iget
use M_time, only :   days2sec, realtime, u2d, d2u, j2d, system_sleep, guessdate, now, fmtdate
implicit none
character(len=*),parameter :: ident="@(#) given string of form days-hh:mm:ss convert to seconds'"
real(kind=realtime)          :: dvalue
integer                      :: dat(8)
integer                      :: i
integer                      :: itimes
integer                      :: ios
logical                      :: until
character(len=:),allocatable :: time_format
character(len=1)             :: cpaws

   call kracken('paws',' -oo -msg -uet -jed -dat -date -until -repeat 0 -fmt -help .F. -version .F.') ! parse command line
   call help_usage(lget('paws_help'))                                      ! display help information and stop if true
   call help_version(lget('paws_version'))                                 ! display version information and stop if true
   until=.false.

   if(sget('paws_msg').ne.' ')then
      write(*,'(a)')trim(sget('paws_msg'))
   endif

   if(sget('paws_uet').ne.' ')then
      dvalue=max( 0.0d0, dget('paws_uet')-d2u() )
      call system_sleep(nint(dvalue))
      until=.true.
   endif

   if(sget('paws_jed').ne.' ')then
      dvalue=max( 0.0d0, d2u(j2d(dget('paws_jed')))-d2u() )
      call system_sleep(nint(dvalue))
      until=.true.
   endif

   if(sget('paws_dat').ne.' ')then
      dat=igets('paws_dat')
      dvalue=max(0.0d0, d2u(dat)-d2u())
      call system_sleep(nint(dvalue))
      until=.true.
   endif

   if(sget('paws_date').ne.' ')then
      call guessdate(sget('paws_date'),dat)
      write(*,*)'pausing till '//fmtdate(dat)
      dvalue=max(0.0d0, d2u(dat)-d2u())
      call system_sleep(nint(dvalue))
      until=.true.
   endif
   if(sget('paws_until').ne.' ')then
      call guessdate(sget('paws_until'),dat)
      write(*,*)'pausing till '//fmtdate(dat)
      dvalue=max(0.0d0, d2u(dat)-d2u())
      call system_sleep(nint(dvalue))
      until=.true.
   endif

   if(sget('paws_oo').ne.' ')then
      dvalue=max( 0.0d0, days2sec(sget('paws_oo')) )
      itimes=iget('paws_repeat')
      if(itimes.eq.0)then
         call system_sleep(dvalue)
      else
         time_format=sget('paws_fmt')
         do i=itimes,0,-1
            write(*,'(i0.3,1x,a)')i,now(time_format)
            call system_sleep(dvalue)
         enddo
      endif
   else                                                      ! no pause time specified. Prompt and read from stdin
      if(.not.until)then
         write(*,'("continue...")',advance="no")
         read(*,'(a)',iostat=ios) cpaws
      endif
   endif

end program demo_system_sleep

program demo_system_sleep
use M_kracken, only: kracken, sgets, lget, iget, sget
use M_time, only :   days2sec, realtime, system_sleep
use M_strings, only: substitute, compact
implicit none
real(kind=realtime)           :: delay_value
character(len=80),allocatable :: delays(:)
integer                       :: i
integer                       :: j
integer                       :: n
real                          :: chance
logical                       :: countdown, countup
logical                       :: verbose
integer                       :: interval
character(len=:),allocatable  :: cmd
   call kracken('sleep',' -oo 0.0 -cmd " " -help .F. -version .F. -countdown .F. -countup .F. -interval 1 -verbose .F.') ! parse command line
   call help_usage(lget('sleep_help'))                        ! display help information and stop if true
   call help_version(lget('sleep_version'))                   ! display version information and stop if true
   countdown=lget('sleep_countdown')
   countup=lget('sleep_countup')
   interval=iget('sleep_count')
   verbose=lget('sleep_verbose')
   cmd=sget('sleep_cmd')
!===================================================================================================================================
   ! determine value of cyclical pause duration
   delays=sgets('sleep_oo')
   if(verbose) write(*,*)'DELAYS(SEC)=',(trim(delays(i)),' ',i=1,size(delays))
   if(interval.lt.1)interval=1
   if(verbose) write(*,*)'INTERVAL=',interval
   do i=1,size(delays)
      if(index(delays(i),'r').ne.0)then                                  ! random number seconds between zero and value specified
         call substitute(delays(i),'r','s')                              ! change 'r' suffix to 's'
         call random_number(chance)
         delay_value=max( 0.0d0, days2sec(delays(i)) )                   ! get value converted to seconds
         delay_value=delay_value*chance                                  ! randomize the number
         if(verbose)write(*,*)'RANDOM DELAY(SEC)=',delay_value
      else                                                               ! simply convert to seconds
         delay_value=max( 0.0d0, days2sec(delays(i)) )
         if(verbose)write(*,*)'DELAY(SEC)=',delay_value
      endif
      if(countdown.and.delay_value.gt.1)then
         write(*,*)'COUNTDOWN=',countdown
         do j=int(delay_value),1,-interval
            call system_sleep(interval)
            write(*,'(i0,a)',advance='no')j,merge(new_line('A'),' ',mod(j,10).eq.0)
            if(cmd.ne.'')write(*,*)
            if(cmd.ne.'')call execute_command_line(cmd)
         enddo
      elseif(countup.and.delay_value.gt.1)then
         write(*,*)'COUNTUP=',countup
         do j=1,int(delay_value),interval
            call system_sleep(interval)
            write(*,'(i0,a)',advance='no')j,merge(new_line('A'),' ',mod(j,10).eq.0)
            if(cmd.ne.'')write(*,*)
            if(cmd.ne.'')call execute_command_line(cmd)
         enddo
      else
         if(verbose)write(*,*)'SLEEP'
         call system_sleep(delay_value)
         if(cmd.ne.'')write(*,*)
         if(cmd.ne.'')call execute_command_line(cmd)
      endif
   enddo
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
'NAME                                                                            ',&
'   sleep-(1f) - [FUNIX:TIME] pause for specified duration                       ',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'   sleep- [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d|r]] [ -countdown|-countup -interval]|--help|--version',&
'DESCRIPTION                                                                     ',&
'   Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh            ',&
'   hours, mm minutes and ss.xxx seconds pause for the specified amount          ',&
'   of time.                                                                     ',&
'                                                                                ',&
'   Alternatively, the time may be specified by a number immediately             ',&
'   followed by a unit letter; where s is seconds, m is minutes, h is            ',&
'   hours and d is days.                                                         ',&
'                                                                                ',&
'   The suffix r is the same as the suffix s accept a random time between        ',&
'   zero and the specified number of seconds is used. This is useful for         ',&
'   spreading out cron(1) tasks in a HPC cluster. Only the seconds are           ',&
'   randomized so the r suffix is generally used by itself with a single         ',&
'   value.                                                                       ',&
'                                                                                ',&
'   Spaces before a suffix are significant. A suffix without a prefix is         ',&
'   treated as if the prefix zero (0) was present.                               ',&
'                                                                                ',&
'   Given multiple arguments, pause for the time specified by the sum of         ',&
'   the values.                                                                  ',&
'OPTIONS                                                                         ',&
'   dd-hh:mm:ss  Given a string representing a duration of time in the           ',&
'                following forms:                                                ',&
'                                                                                ',&
'                  dd-hh:mm:ss[.xx]                                              ',&
'                     hh:mm:ss[.xx]                                              ',&
'                        mm:ss[.xx]                                              ',&
'                           ss[.xx]                                              ',&
'      or                                                                        ',&
'   xx[.yy]SUFFIX  where Suffix may be s for seconds, m for minutes, h for hours,',&
'                  or d for days; or r for a random number of seconds up to the  ',&
'                  value.                                                        ',&
'   -countdown     sleep in one-second intervals and count down to               ',&
'                  end of sleep command                                          ',&
'   -countup       sleep in one-second intervals and count up till end           ',&
'                  of command                                                    ',&
'   -interval COUNT   how many seconds between counts when -countdown and -countup',&
'                  are specified. Values other than one may trim the total sleep ',&
'                  time by up to COUNT seconds.                                  ',&
'     cmd          system command to repeat at the end of each interval          ',&
'   --verbose      output verbose messages                                       ',&
'   --help         display this help and exit                                    ',&
'   --version      output version information and exit                           ',&
'EXAMPLE                                                                         ',&
'  usage:                                                                        ',&
'                                                                                ',&
'   sleep- 0.10     # pause one tenth of a second                                ',&
'   sleep- 3m 10s   # pause three minutes and 10 seconds                         ',&
'   sleep- 1:00:00  # pause for one hour                                         ',&
'   sleep 50r       # sleep a random number of seconds up to 50                  ',&
'   sleep- 1d 86400 # pause two days                                             ',&
'   sleep- 1-3:30   # pause one day, three hours and thirty minutes              ',&
'SEE ALSO                                                                        ',&
'   sleep(1), usleep(1), watch(1), xargs(1), yes(1), seq(1), repeat(1csh)        ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    sleep-(1f) - [FUNIX:TIME] pause for specified duration
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    sleep- [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d|r]] [ -countdown|-countup -interval]|--help|--version
!!##DESCRIPTION
!!    Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh
!!    hours, mm minutes and ss.xxx seconds pause for the specified amount
!!    of time.
!!
!!    Alternatively, the time may be specified by a number immediately
!!    followed by a unit letter; where s is seconds, m is minutes, h is
!!    hours and d is days.
!!
!!    The suffix r is the same as the suffix s accept a random time between
!!    zero and the specified number of seconds is used. This is useful for
!!    spreading out cron(1) tasks in a HPC cluster. Only the seconds are
!!    randomized so the r suffix is generally used by itself with a single
!!    value.
!!
!!    Spaces before a suffix are significant. A suffix without a prefix is
!!    treated as if the prefix zero (0) was present.
!!
!!    Given multiple arguments, pause for the time specified by the sum of
!!    the values.
!!##OPTIONS
!!    dd-hh:mm:ss  Given a string representing a duration of time in the
!!                 following forms:
!!
!!                   dd-hh:mm:ss[.xx]
!!                      hh:mm:ss[.xx]
!!                         mm:ss[.xx]
!!                            ss[.xx]
!!       or
!!    xx[.yy]SUFFIX  where Suffix may be s for seconds, m for minutes, h for hours,
!!                   or d for days; or r for a random number of seconds up to the
!!                   value.
!!    -countdown     sleep in one-second intervals and count down to
!!                   end of sleep command
!!    -countup       sleep in one-second intervals and count up till end
!!                   of command
!!    -interval COUNT   how many seconds between counts when -countdown and -countup
!!                   are specified. Values other than one may trim the total sleep
!!                   time by up to COUNT seconds.
!!      cmd          system command to repeat at the end of each interval
!!    --verbose      output verbose messages
!!    --help         display this help and exit
!!    --version      output version information and exit
!!##EXAMPLE
!!
!!   usage:
!!
!!    sleep- 0.10     # pause one tenth of a second
!!    sleep- 3m 10s   # pause three minutes and 10 seconds
!!    sleep- 1:00:00  # pause for one hour
!!    sleep 50r       # sleep a random number of seconds up to 50
!!    sleep- 1d 86400 # pause two days
!!    sleep- 1-3:30   # pause one day, three hours and thirty minutes
!!##SEE ALSO
!!    sleep(1), usleep(1), watch(1), xargs(1), yes(1), seq(1), repeat(1csh)
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
'@(#)PROGRAM:        sleep-(1f)>',&
'@(#)DESCRIPTION:    given string of form days-hh:mm:ss pause for specified amount of time>',&
'@(#)VERSION:        1.0, 20170822>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)COMPILED:       2024-11-24 04:44:17 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demo_system_sleep

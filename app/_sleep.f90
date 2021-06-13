subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   _sleep(1f) - [FUNIX:TIME] pause for specified duration                       ',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'   _sleep [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d]] [ -countdown|-countup -count]|--help|--version',&
'DESCRIPTION                                                                     ',&
'   Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh            ',&
'   hours, mm minutes and ss.xxx seconds pause for the specified amount          ',&
'   of time.                                                                     ',&
'                                                                                ',&
'   Alternatively, the time may be specified by a number immediately             ',&
'   followed by a unit letter; where s is seconds, m is minutes, h is            ',&
'   hours and d is days.                                                         ',&
'                                                                                ',&
'   If the suffix r is used, a random time between zero and the specified        ',&
'   number of seconds is used. This is useful for spreading out cron(1)          ',&
'   tasks in a HPC cluster.                                                      ',&
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
'                  or d for days.                                                ',&
'   -countdown     sleep in one-second intervals and count down to               ',&
'                  end of sleep command                                          ',&
'   -countup       sleep in one-second intervals and count up till end           ',&
'                  of command                                                    ',&
'   -count COUNT   how many seconds between counts when -countdown and -countup  ',&
'                  are specified. Values other than one may trim the total sleep ',&
'                  time by up to COUNT seconds.                                  ',&
'   --help         display this help and exit                                    ',&
'   --version      output version information and exit                           ',&
'EXAMPLE                                                                         ',&
'  usage:                                                                        ',&
'                                                                                ',&
'   _sleep 0.10     # pause one tenth of a second                                ',&
'   _sleep 3m 10s   # pause three minutes and 10 seconds                         ',&
'   _sleep 1:00:00  # pause for one hour                                         ',&
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
!!    _sleep(1f) - [FUNIX:TIME] pause for specified duration
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    _sleep [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d]] [ -countdown|-countup -count]|--help|--version
!!##DESCRIPTION
!!    Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh
!!    hours, mm minutes and ss.xxx seconds pause for the specified amount
!!    of time.
!!
!!    Alternatively, the time may be specified by a number immediately
!!    followed by a unit letter; where s is seconds, m is minutes, h is
!!    hours and d is days.
!!
!!    If the suffix r is used, a random time between zero and the specified
!!    number of seconds is used. This is useful for spreading out cron(1)
!!    tasks in a HPC cluster.
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
!!                   or d for days.
!!    -countdown     sleep in one-second intervals and count down to
!!                   end of sleep command
!!    -countup       sleep in one-second intervals and count up till end
!!                   of command
!!    -count COUNT   how many seconds between counts when -countdown and -countup
!!                   are specified. Values other than one may trim the total sleep
!!                   time by up to COUNT seconds.
!!    --help         display this help and exit
!!    --version      output version information and exit
!!##EXAMPLE
!!
!!   usage:
!!
!!    _sleep 0.10     # pause one tenth of a second
!!    _sleep 3m 10s   # pause three minutes and 10 seconds
!!    _sleep 1:00:00  # pause for one hour
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
'@(#)PROGRAM:        _sleep(1f)>',&
'@(#)DESCRIPTION:    given string of form days-hh:mm:ss pause for specified amount of time>',&
'@(#)VERSION:        1.0, 20170822>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)COMPILED:       Mon, May 24th, 2021 9:29:10 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_sleep
use M_kracken, only: kracken, sgets, lget, iget
use M_time, only :   days2sec, realtime, system_sleep
use M_strings, only: substitute
implicit none
real(kind=realtime)           :: delay_value
character(len=80),allocatable :: delays(:)
integer                       :: i
integer                       :: j
integer, allocatable          :: seed(:)
integer                       :: n
real                          :: chance
integer                       :: values(1:8)
logical                       :: countdown, countup
logical                       :: verbose
integer                       :: count
   call kracken('sleep',' -oo 0.0 -help .F. -version .F. -countdown .F. -countup .F. -count 1 -verbose .F.') ! parse command line
   call help_usage(lget('sleep_help'))                        ! display help information and stop if true
   call help_version(lget('sleep_version'))                   ! display version information and stop if true
   countdown=lget('sleep_countdown')
   countup=lget('sleep_countup')
   count=iget('sleep_count')
   verbose=lget('sleep_verbose')
!===================================================================================================================================
   ! determine value of cyclical pause duration
   delays=sgets('sleep_oo')
   if(verbose)write(*,*)'DELAYS(SEC)=',delays
   if(verbose)write(*,*)'COUNTDOWN=',countdown
   if(verbose)write(*,*)'COUNTUP=',countup
   if(verbose)write(*,*)'COUNT=',count
   if(count.lt.1)count=1
   do i=1,size(delays)
      if(index(delays(i),'r').ne.0)then                                  ! random number seconds between zero and value specified
         call substitute(delays(i),'r','s')                              ! change 'r' suffix to 's'

         call date_and_time(values=values)                               ! jump through hoops to get a random number from 0 to 1
         call random_seed(size=n)
         allocate(seed(1:n))
         seed(:) = values(8)
         call random_seed(put=seed)
         call random_number(chance)

         delay_value=max( 0.0d0, days2sec(delays(i)) )                   ! get value converted to seconds
         delay_value=delay_value*chance                                  ! randomize the number
      else                                                               ! simply convert to seconds
         delay_value=max( 0.0d0, days2sec(delays(i)) )
      endif
      if(verbose)write(*,*)'DELAY(SEC)=',delay_value
      if(countdown.and.delay_value.gt.1)then
         if(verbose)write(*,*)'COUNTDOWN'
         do j=int(delay_value),1,-count
            call system_sleep(count)
            write(*,'(i0,a)',advance='no')j,merge(new_line('A'),' ',mod(j,10).eq.0)
         enddo
      elseif(countup.and.delay_value.gt.1)then
         if(verbose)write(*,*)'COUNTUP'
         do j=1,int(delay_value),count
            call system_sleep(count)
            write(*,'(i0,a)',advance='no')j,merge(new_line('A'),' ',mod(j,10).eq.0)
         enddo
      else
         if(verbose)write(*,*)'SLEEP'
         call system_sleep(delay_value)
      endif
   enddo
!===================================================================================================================================
end program demo_system_sleep

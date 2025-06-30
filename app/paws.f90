program demo_system_sleep
use M_kracken, only: kracken, sget, lget, dget, igets, iget
use M_time,    only: days2sec, realtime, u2d, d2u, j2d, system_sleep, guessdate, now, fmtdate
implicit none
character(len=*),parameter :: ident="@(#)paws(1):pause until specified time or for specified duration"
real(kind=realtime)          :: until_value
real(kind=realtime)          :: delay_value
integer                      :: dat(8)
integer                      :: i
integer                      :: itimes
integer                      :: ios
logical                      :: until
logical                      :: once
character(len=:),allocatable :: time_format
character(len=:),allocatable :: msg
character(len=:),allocatable :: cmd
character(len=1)             :: cpaws
integer                      :: cstat
character(len=256)           :: sstat

                                                             ! parse command line
   call kracken('paws',' -oo -debug .f. -cmd -msg -uet -jd -dat -date -until -repeat 0 -fmt -help .F. -version .F.')
   call help_usage(lget('paws_help'))                        ! display help information and stop if true
   call help_version(lget('paws_version'))                   ! display version information and stop if true
   until=.false.
   until_value=0.0
!===================================================================================================================================
                                                             ! delay until specified date has passed using various date descriptions
   if(sget('paws_uet').ne.' ')then
      until_value=max( 0.0d0, dget('paws_uet')-d2u() )
      if(lget('paws_debug'))then
         write(*,*)'UET ...........',dget('paws_uet')
         write(*,*)'d2u ...........',d2u()
         write(*,*)'until_value ...',until_value
         call system_sleep(nint(until_value))
         until=.true.
      endif
   endif

   if(sget('paws_jd').ne.' ')then
      until_value=max( 0.0d0, d2u(j2d(dget('paws_jd')))-d2u() )
      call system_sleep(nint(until_value))
      until=.true.
   endif

   if(sget('paws_dat').ne.' ')then
      dat=igets('paws_dat')
      until_value=max(0.0d0, d2u(dat)-d2u())
      call system_sleep(nint(until_value))
      until=.true.
   endif

   if(sget('paws_date').ne.' ')then
      call guessdate(sget('paws_date'),dat)
      write(*,*)'pausing till '//fmtdate(dat)
      until_value=max(0.0d0, d2u(dat)-d2u())
      call system_sleep(nint(until_value))
      until=.true.
   endif

   if(sget('paws_until').ne.' ')then
      call guessdate(sget('paws_until'),dat)
      write(*,*)'pausing till '//fmtdate(dat)
      until_value=max(0.0d0, d2u(dat)-d2u())
      call system_sleep(nint(until_value))
      until=.true.
   endif
!===================================================================================================================================
   ! determine value of cyclical pause duration
   if(sget('paws_oo').ne.' ')then
      delay_value=max( 0.0d0, days2sec(sget('paws_oo')) )
   else
      delay_value=0.0
   endif
!===================================================================================================================================
   itimes=iget('paws_repeat')
   if(itimes.le.0)then
      once=.true.
      itimes=1
   else
      once=.false.
   endif

   time_format=sget('paws_fmt')
   msg=trim(sget('paws_msg'))
   cmd=trim(sget('paws_cmd'))
!===================================================================================================================================
   if(lget('paws_debug'))then
      write(*,*)'cmd ...........; ',trim(cmd)
      write(*,*)'msg ...........; ',trim(msg)
      write(*,*)'time_format ...; ',trim(time_format )
      write(*,*)'delay_value ...;',delay_value
      write(*,*)'until .........;',until
      write(*,*)'until_value ...;',until_value
      write(*,*)'once ..........;',once
      write(*,*)'itimes ........;',itimes
   endif
!===================================================================================================================================
   do i=itimes,1,-1

      if(msg.ne.' ')then
         write(*,'(a)')msg
      endif

      if(.not.once)then
         write(*,'(i0.3,1x,a)')i,now(time_format)
      endif

      if(cmd.ne.' ')then
         call execute_command_line(cmd,cmdstat=cstat,cmdmsg=sstat)
      endif

      if(.not.until.and.delay_value.le.0)then      ! no pause time specified. Prompt and read from stdin
         write(*,'("continue...")',advance="no")
         read(*,'(a)',iostat=ios) cpaws
      else
         call system_sleep(delay_value)
      endif

   enddo

   if(.not.once)then
      write(*,'(i0.3,1x,a)')i,now(time_format)
   endif
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
'   paws(1f) - [TIME] pause until specified time or for specified duration       ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   paws [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d]][ -msg message][ -cmd command][ -repeat TIMES[ -fmt ]]|...',&
'   [ -uet|-jd|-dat|[ -date|-until]]                                             ',&
'   paws --version|--help                                                        ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh hours,     ',&
'   mm minutes and ss.xxx seconds convert it to seconds. Then, pause for that    ',&
'   many seconds. Alternatively, pause until specified date has passed.          ',&
'   If no duration is specified wait until a carriage return is entered.         ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   dd-hh:mm:ss   Given a string representing a duration of time in the          ',&
'                 following forms:                                               ',&
'                                                                                ',&
'                   dd-hh:mm:ss[.xx]                                             ',&
'                      hh:mm:ss[.xx]                                             ',&
'                         mm:ss[.xx]                                             ',&
'                            ss[.xx]                                             ',&
'                      or                                                        ',&
'                   xx[.yy]SUFFIX                                                ',&
'                                                                                ',&
'                 convert it to seconds and pause for that amount of time.       ',&
'                 Suffix may be s for seconds, m for minutes, h for hours,       ',&
'                 or d for days.                                                 ',&
'   -date|-until  wait until the specified date has passed (before starting      ',&
'                 optional pause). See guessdate(3f) for syntax allowed for      ',&
'                 the date.                                                      ',&
'   -uet          wait until the specified Unix Epoch Time has passed            ',&
'   -jd           wait until the specified Julian Date has passed                ',&
'   -dat          wait until the specified date vector has passed                ',&
'                 (year month day timezone hour minutes seconds milliseconds)    ',&
'   -repeat NNN   The duration is repeated NNN times with the date displayed     ',&
'                 at the end of each pause.                                      ',&
'   --msg         message to display before pausing                              ',&
'   --cmd         command to execute after a pause                               ',&
'   --fmt         date format (see fmtdate(3f) for details)                      ',&
'   --help        display this help and exit                                     ',&
'   --version     output version information and exit                            ',&
'                                                                                ',&
'   For more information on the format of the dates, see the now(1) command.     ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
' Typical usage:                                                                 ',&
'                                                                                ',&
'  paws 2:00:00              # pause for two hours                               ',&
'  paws 3600                 # pause one hour                                    ',&
'  paws 0.10                 # pause one tenth of a second                       ',&
'  paws 1 -repeat 60         # pause sixty seconds, displaying date each second  ',&
'  paws -until 23:59:59      # pause until midnight                              ',&
'  paws 15:00 -date 23:59:59 # wait till midnight then an additional 15 minutes  ',&
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
!!    paws(1f) - [TIME] pause until specified time or for specified duration
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    paws [dd-hh:mm:ss[.xxx]|xxx.yyy[s|m|h|d]][ -msg message][ -cmd command][ -repeat TIMES[ -fmt ]]|...
!!    [ -uet|-jd|-dat|[ -date|-until]]
!!    paws --version|--help
!!
!!##DESCRIPTION
!!    Given a duration in the form dd-hh:mm:ss.xxx where dd is days, hh hours,
!!    mm minutes and ss.xxx seconds convert it to seconds. Then, pause for that
!!    many seconds. Alternatively, pause until specified date has passed.
!!    If no duration is specified wait until a carriage return is entered.
!!
!!##OPTIONS
!!    dd-hh:mm:ss   Given a string representing a duration of time in the
!!                  following forms:
!!
!!                    dd-hh:mm:ss[.xx]
!!                       hh:mm:ss[.xx]
!!                          mm:ss[.xx]
!!                             ss[.xx]
!!                       or
!!                    xx[.yy]SUFFIX
!!
!!                  convert it to seconds and pause for that amount of time.
!!                  Suffix may be s for seconds, m for minutes, h for hours,
!!                  or d for days.
!!    -date|-until  wait until the specified date has passed (before starting
!!                  optional pause). See guessdate(3f) for syntax allowed for
!!                  the date.
!!    -uet          wait until the specified Unix Epoch Time has passed
!!    -jd           wait until the specified Julian Date has passed
!!    -dat          wait until the specified date vector has passed
!!                  (year month day timezone hour minutes seconds milliseconds)
!!    -repeat NNN   The duration is repeated NNN times with the date displayed
!!                  at the end of each pause.
!!    --msg         message to display before pausing
!!    --cmd         command to execute after a pause
!!    --fmt         date format (see fmtdate(3f) for details)
!!    --help        display this help and exit
!!    --version     output version information and exit
!!
!!    For more information on the format of the dates, see the now(1) command.
!!
!!##EXAMPLES
!!
!!  Typical usage:
!!
!!   paws 2:00:00              # pause for two hours
!!   paws 3600                 # pause one hour
!!   paws 0.10                 # pause one tenth of a second
!!   paws 1 -repeat 60         # pause sixty seconds, displaying date each second
!!   paws -until 23:59:59      # pause until midnight
!!   paws 15:00 -date 23:59:59 # wait till midnight then an additional 15 minutes
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
'@(#)PROGRAM:        paws(1f)>',&
'@(#)DESCRIPTION:    pause until specified time or for specified duration>',&
'@(#)VERSION:        1.0, 20160731>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2025-06-29 08:21:19 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version

end program demo_system_sleep

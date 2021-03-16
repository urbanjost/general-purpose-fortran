!>
!!##NAME
!!    _time(1f) - [FUNIX] display time used by a command
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!     time 'commands'
!!##DESCRIPTION
!!     Given a command time it and report all clock and system
!!     time used.
!!
!!     Demonstrates the use of the M_stopwatch(3fm) module.
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!     _time 'hostname;sleep 3;pwd;date'
!!
!!   Typical output:
!!
!!    buzz
!!    /home/urbanjs/V600/LIBRARY/libGPF/EXE/FUNIX
!!    Sun Nov  4 22:11:09 EST 2018
!!    COMMAND:hostname;sleep 3;pwd;date
!!      times:
!!            cpu=    0.03 user=    0.00  sys=    0.03 wall=    3.55
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
program demo_print_watch
! example program starts a watch W1, stops it, and prints the results
use,intrinsic :: iso_fortran_env, only : &
   ERROR_UNIT, INPUT_UNIT, OUTPUT_UNIT ! access computing environment
use M_stopwatch, only : watchtype
use M_stopwatch, only : option_stopwatch, create_watch, start_watch
use M_stopwatch, only : stop_watch, print_watch, destroy_watch
implicit none

! ident_1="@(#)_time(1f): time command"

type(watchtype)              :: w1
character(len=4096)          :: arg
integer                      :: errcode
integer                      :: i
integer                      :: ilen
character(len=:),allocatable :: cmd
integer                      :: cmdstat
character(len=256)           :: cmdmsg

   cmd=''
   do i=1,command_argument_count()
      call get_command_argument(i,arg,length=ilen)
      cmd=cmd//arg(:ilen)//' '
   enddo

   if(cmd.eq.'')stop

   call option_stopwatch(                                            &
      default_clock=[character(len=4) :: 'cpu','wall','user','sys'], &
      io_unit_print=ERROR_UNIT,                                      &
      io_unit_error=ERROR_UNIT)

   call create_watch(watch=w1, name='times')
   call start_watch(watch=w1)

   call execute_command_line(cmd,cmdstat=cmdstat,cmdmsg=cmdmsg) ! do something that takes some time
   if(cmdstat.ne.0)then
      write(ERROR_UNIT,'(a)')trim(cmdmsg)
   endif
   call stop_watch(watch=w1)
   call print_watch(watch=w1, title='COMMAND:'//cmd, err=errcode)
   call destroy_watch(w1)

end program demo_print_watch

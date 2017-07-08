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
'    _kill(1f) - [FUNIX] send signals to processes                               ',&
'SYNTAX                                                                          ',&
'    _kill PIDs [-s signal_number] [--help|--version]                            ',&
'DESCRIPTION                                                                     ',&
'    Calls system_kill(3f), which calls kill(3c) to send signals                 ',&
'    to processes.                                                               ',&
'OPTIONS                                                                         ',&
'    PIDs       PID numbers to send signal to                                    ',&
'    -s         signal number to send to the processes                           ',&
'    --help     display this help and exit                                       ',&
'    --version  output version information and exit                              ',&
'EXAMPLE                                                                         ',&
'   Sample execution:                                                            ',&
'                                                                                ',&
'    > $ _kill 60476 234234 OTHER -s 9                                           ',&
'    > *a2d* - cannot produce number from string [OTHER]                         ',&
'    > *a2d* - [Bad value during integer read]                                   ',&
'    > *kill*: SIGNAL=9 PID=60476 successfully sent                              ',&
'    > *kill*: process not found                                                 ',&
'    > *kill*: PID value of 0 is not supported                                   ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     _kill(1f) - [FUNIX] send signals to processes
!!##SYNTAX
!!     _kill PIDs [-s signal_number] [--help|--version]
!!##DESCRIPTION
!!     Calls system_kill(3f), which calls kill(3c) to send signals
!!     to processes.
!!##OPTIONS
!!     PIDs       PID numbers to send signal to
!!     -s         signal number to send to the processes
!!     --help     display this help and exit
!!     --version  output version information and exit
!!##EXAMPLE
!!
!!    Sample execution:
!!
!!     > $ _kill 60476 234234 OTHER -s 9
!!     > *a2d* - cannot produce number from string [OTHER]
!!     > *a2d* - [Bad value during integer read]
!!     > *kill*: SIGNAL=9 PID=60476 successfully sent
!!     > *kill*: process not found
!!     > *kill*: PID value of 0 is not supported
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
'@(#)PROGRAM:        _kill(1f)>',&
'@(#)DESCRIPTION:    send signals to processes>',&
'@(#)VERSION:        1.0, 2017-05-23>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:11:09 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_kill
use M_kracken, only   : kracken, lget, igets, iget
   use M_system, only : system_kill, system_perror
   implicit none
   integer             :: i,signal
   integer,allocatable :: pids(:)

   call kracken('kill','  -s 15 -help .F. -version .F.')                                   ! define command line arguments
   call help_usage(lget('kill_help'))                                                      ! display help if -help present
   call help_version(lget('kill_version'))                                                 ! display version if -version present
   signal=iget('kill_s')                                                                   ! get signal number to use
   pids=igets('kill_oo')                                                                   ! get list of PIDs

   do i=1,size(pids)                                                                       ! step through PIDs
      if(pids(i).eq.0)then
         write(*,"('*kill*: PID value of ',i0,' not supported')")pids(i)
      else
         if(system_kill(pids(i),signal).ne.0)then                                          ! send signal SIGNAL to pid PID
            call system_perror('*kill*')                                                   ! write message if an error was detected
         else
            write(*,'("*kill*: SIGNAL=",i0," PID=",i0," successfully sent")')signal,pids(i)
         endif
      endif
   enddo

end program demo_system_kill

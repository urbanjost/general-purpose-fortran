     program demo_system_kill
     use M_system, only : system_kill
     use M_system, only : system_perror
     implicit none
     integer           :: i,pid,ios,ierr,signal=9
     character(len=80) :: argument

        do i=1,command_argument_count()
           ! get arguments from command line
           call get_command_argument(i, argument)
           ! convert arguments to integers assuming they are PID numbers
           read(argument,'(i80)',iostat=ios) pid
           if(ios.ne.0)then
              write(*,*)'bad PID=',trim(argument)
           else
              write(*,*)'kill SIGNAL=',signal,' PID=',pid
           ! send signal SIGNAL to pid PID
              ierr=system_kill(pid,signal)
           ! write message if an error was detected
              if(ierr.ne.0)then
                 call system_perror('*demo_system_kill*')
              endif
           endif
        enddo
  end program demo_system_kill

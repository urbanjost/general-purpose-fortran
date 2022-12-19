      program demo_get_command_argument
      implicit none
      character(len=255)           :: progname
      integer                      :: count, i, argument_length, istat
      character(len=:),allocatable :: arg

       ! command name assuming it is less than 255 characters in length
        call get_command_argument (0, progname, status=istat)
        if (istat == 0) then
           print *, "The program's name is " // trim (progname)
        else
           print *, "Could not get the program's name " // trim (progname)
        endif

       ! get number of arguments
        count = command_argument_count()
        write(*,*)'The number of arguments is ',count

        !
        ! allocate string array big enough to hold command line
        ! argument strings and related information
        !
        do i=1,count
           call get_command_argument(number=i,length=argument_length)
           if(allocated(arg))deallocate(arg)
           allocate(character(len=argument_length) :: arg)
           call get_command_argument(i, arg,status=istat)
           ! show the results
           write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
           & i,istat,argument_length,arg
        enddo

      end program demo_get_command_argument

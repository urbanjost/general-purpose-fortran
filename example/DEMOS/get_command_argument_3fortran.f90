      program demo_get_command_argument
      implicit none
      integer                      :: count, i, istat
      character(len=:),allocatable :: arg

       ! command name
        arg=get_arg(0,istat)
        if (istat == 0) then
           print *, "The program's name is " // trim (arg)
        else
           print *, "Could not get the program's name " // trim (arg)
        endif

       ! get number of arguments
        count = command_argument_count()
        write(*,*)'The number of arguments is ',count

       ! show argument values
        do i=1,count
           arg=get_arg(i,istat)
           ! show the results
           write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
           & i,istat,len(arg),arg
        enddo

      contains

      function get_arg(n,status) result(arg)
      integer,intent(in)           :: n
      integer,intent(out),optional :: status
      integer                      :: argument_length, istat
      character(len=:),allocatable :: arg
        !
        ! allocate string big enough to hold command line argument
        !
         call get_command_argument( number=n, length=argument_length )
         if(allocated(arg))deallocate( arg )
         allocate(character(len=argument_length) :: arg )
         call get_command_argument(n, arg, status=istat )
         if(present(status)) status=istat
      end function get_arg

      end program demo_get_command_argument

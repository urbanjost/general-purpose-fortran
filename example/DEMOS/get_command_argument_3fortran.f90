          program demo_get_command_argument
          implicit none
          character(len=255)           :: progname
          integer                      :: stat
          integer                      :: count,i, longest, argument_length
          integer,allocatable          :: istat(:), ilen(:)
          character(len=:),allocatable :: arguments(:)
            ! get number of arguments
            count = command_argument_count()
            write(*,*)'The number of arguments is ',count
            ! simple usage
            call get_command_argument (0, progname, status=stat)
            if (stat == 0) then
               print *, "The program's name is " // trim (progname)
            endif
            ! showing how to make an array to hold any argument list
            ! find longest argument
            longest=0
            do i=0,count
               call get_command_argument(number=i,length=argument_length)
               longest=max(longest,argument_length)
            enddo
            ! allocate string array big enough to hold command line argument strings
            ! and related information
            allocate(character(len=longest) :: arguments(0:count))
            allocate(istat(0:count))
            allocate(ilen(0:count))
            ! read the arguments into the array
            do i=0,count
              call get_command_argument(i, arguments(i),status=istat(i),length=ilen(i))
            enddo
            ! show the results
            write (*,'(i3.3,1x,i0.5,1x,i0.5,1x,"[",a,"]")') &
            & (i,istat(i),ilen(i),arguments(i)(:ilen(i)),i=0,count)
          end program demo_get_command_argument

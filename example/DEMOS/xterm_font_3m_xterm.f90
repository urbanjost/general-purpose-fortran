          program demo_xterm_font
          use M_xterm, only : xterm_font
          implicit none
          character(len=256) :: string
          character(len=1)   :: paws
          integer            :: i,count,ios
          ! get number of arguments on command line
          count = command_argument_count()
          ! if at least one name on command line select fonts,
          ! pausing between names until last font name
          !is reached
          do i=1,count
             call get_command_argument(number=i,value=string)
             write(*,'(2a)',advance='no')'font=',trim(string)
             call xterm_font(string)
             if(i.eq.count)exit
             read(*,'(a)',iostat=ios)paws
          enddo
          end program demo_xterm_font

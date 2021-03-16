          program demo_M_getopt_long
            use M_getopt_long
            implicit none
            character(len=1) :: c
            integer :: i
            integer :: digit_optind = 0
            type(getopt_type), pointer :: opts

            integer :: this_option_optind
            integer :: option_index
            character(len=1), parameter :: NIL = char(0)

            type(getopt_option_type) :: long_options(6) = (/ &
                getopt_option_type("add",     1, NULL(), NIL), &
                getopt_option_type("append",  0, NULL(), NIL), &
                getopt_option_type("delete",  1, NULL(), NIL), &
                getopt_option_type("verbose", 0, NULL(), NIL), &
                getopt_option_type("create",  1, NULL(), 'c'), &
                getopt_option_type("file",    1, NULL(), NIL) /)
            character(len=*), parameter :: optstring = "abc:d:012"

            call getopt_new(opts,optstring,long_options)

            do
              this_option_optind = merge(opts%index,1,opts%index>0)
              option_index = 0
              c = getopt(opts,option_index)
              write(*,*)'retval=',c
              select case(c)
              case (GETOPT_STATUS_END)
                exit
              case (GETOPT_STATUS_NIL)
                write(*,'(2A)',advance='no') "option ", trim(long_options(option_index)%name)
                if (associated(opts%optarg)) &
                  write(*,'(2A)',advance='no') "with arg ", opts%optarg
                write(*,*) !newline

              case ('0','1','2')
                if (digit_optind /= 0 .and. digit_optind /= this_option_optind) &
                  write(*,*) "digits occur in two different argv-elements."
                digit_optind = this_option_optind
                write(*,*)"option ",c

              case ('a','b')
                write(*,*)"option ",c

              case ('c','d')
                write(*,*)"option ",c," with value '",opts%optarg,'"'

              case default
                write(*,*) "?? getopt returned character code ",ichar(c)," ??"

              end select

            end do

            if (opts%index <= opts%argc) then
              write(*,'(A)',advance='no') "non-option ARGV-elements: "
              do i=opts%index,opts%argc
                write(*,'(A,1X)',advance='no') getopt_argv(opts,i)
              end do
              write(*,*) ! newline
            end if

            stop
          end program demo_M_getopt_long

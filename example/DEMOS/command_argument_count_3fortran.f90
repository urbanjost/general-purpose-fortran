          program demo_command_argument_count
          implicit none
          integer :: count
              count = command_argument_count()
              print *, count
          end program demo_command_argument_count

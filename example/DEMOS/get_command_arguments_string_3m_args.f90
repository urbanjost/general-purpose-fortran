          program demo_get_command_arguments_string
          use M_journal, only : journal
          use M_args, only : get_command_arguments_string
          implicit none
          integer :: ier
          character(len=:),allocatable :: cmd
          call get_command_arguments_string(cmd,ier)
          write(*,*)'CMD=',trim(cmd)
          write(*,*)'LEN(CMD)=',len(cmd)
          write(*,*)'IER=',ier
          end program demo_get_command_arguments_string

          program demo_get_command_arguments_stack
          use M_args,    only : get_command_arguments_stack
          implicit none
          character(len=:),allocatable :: myargs(:)
          integer                      :: i
          myargs=get_command_arguments_stack()
          write(*,'(i0,t10,a)')(i,myargs(i),i=1,size(myargs))
          write(*,*)'longest argument is ',len(myargs)
          write(*,*)'number of arguments is ',size(myargs)
          end program demo_get_command_arguments_stack

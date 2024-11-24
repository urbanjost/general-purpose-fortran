      program demo_get_arg
      use M_args, only : get_arg
      implicit none
      integer                      :: i
         do i=1,command_argument_count()
            write(*,*)i,get_arg(i)
         enddo
      end program demo_get_arg

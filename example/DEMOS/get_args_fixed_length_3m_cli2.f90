      program demo_get_args_fixed_length
      use M_CLI2,  only : set_args, get_args_fixed_length
      implicit none

       ! Define args
      character(len=80)   :: title
       ! Parse command line
      call set_args(' --title "my title" ')
       ! Assign values to variables
      call get_args_fixed_length('title',title)
       ! Use values
      write(*,*)'title=',title

      end program demo_get_args_fixed_length

      program demo_get_args_fixed_length
      use M_CLI2,  only : set_args, get_args_fixed_length
      implicit none
      ! DEFINE ARGS
      character(len=80)   :: title
      call set_args(' &
         & -title "my title" &
         & ')
      ! ASSIGN VALUES TO ELEMENTS
         call get_args_fixed_length('title',title)
      ! USE VALUES
         write(*,*)'title=',title
      end program demo_get_args_fixed_length

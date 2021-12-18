      program demo_get_args_fixed_size
      use M_CLI2,  only : set_args, get_args_fixed_size
      implicit none
      integer,parameter   :: dp=kind(0.0d0)
      ! DEFINE ARGS
      real                :: x(2)
      real(kind=dp)       :: y(2)
      integer             :: p(3)
      character(len=80)   :: title(1)
      logical             :: l(4), lbig(4)
      complex             :: cmp(2)
      ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
      !   o only quote strings
      !   o set all logical values to F or T.
      call set_args(' &
         & -x 10.0,20.0 &
         & -y 11.0,22.0 &
         & -p -1,-2,-3 &
         & -title "my title" &
         & -l F,T,F,T -L T,F,T,F  &
         & --cmp 111,222.0,333.0e0,4444 &
         & ')
      ! ASSIGN VALUES TO ELEMENTS
         call get_args_fixed_size('x',x)
         call get_args_fixed_size('y',y)
         call get_args_fixed_size('p',p)
         call get_args_fixed_size('title',title)
         call get_args_fixed_size('l',l)
         call get_args_fixed_size('L',lbig)
         call get_args_fixed_size('cmp',cmp)
      ! USE VALUES
         write(*,*)'x=',x
         write(*,*)'p=',p
         write(*,*)'title=',title
         write(*,*)'l=',l
         write(*,*)'L=',lbig
         write(*,*)'cmp=',cmp
      end program demo_get_args_fixed_size

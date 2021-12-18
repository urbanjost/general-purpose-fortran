      program demo_get_args
      use M_CLI2,  only : filenames=>unnamed, set_args, get_args
      implicit none
      integer                      :: i
      ! DEFINE ARGS
      real                         :: x, y, z
      real,allocatable             :: p(:)
      character(len=:),allocatable :: title
      logical                      :: l, lbig
      ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
      !   o only quote strings and use double-quotes
      !   o set all logical values to F or T.
      call set_args(' &
         &-x 1 -y 2 -z 3 &
         &-p -1,-2,-3 &
         &--title "my title" &
         & -l F -L F  &
         & --label " " &
         & ')
      ! ASSIGN VALUES TO ELEMENTS
      ! SCALARS
      call get_args('x',x,'y',y,'z',z)
      call get_args('l',l)
      call get_args('L',lbig)
      ! ALLOCATABLE STRING
      call get_args('title',title)
      ! NON-ALLOCATABLE ARRAYS
      call get_args('p',p)
      ! USE VALUES
      write(*,'(1x,g0,"=",g0)')'x',x, 'y',y, 'z',z
      write(*,*)'p=',p
      write(*,*)'title=',title
      write(*,*)'l=',l
      write(*,*)'L=',lbig
      if(size(filenames).gt.0)then
         write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
      endif
      end program demo_get_args

      program demo_set_args
      use M_CLI2,  only : filenames=>unnamed, set_args, get_args
      use M_CLI2,  only : get_args_fixed_size
      implicit none
      integer                      :: i
      ! DEFINE ARGS
      real                         :: x, y, z
      real                         :: p(3)
      character(len=:),allocatable :: title
      logical                      :: l, lbig
      integer,allocatable          :: ints(:)
      !
      !  DEFINE COMMAND (TO SET INITIAL VALUES AND ALLOWED KEYWORDS)
      !  AND READ COMMAND LINE
      call set_args(' &
         ! reals
         & -x 1 -y 2.3 -z 3.4e2 &
         ! integer array
         & -p -1,-2,-3 &
         ! always double-quote strings
         & --title "my title" &
         ! set all logical values to F or T.
         & -l F -L F &
         ! set allocatable size to zero if you like by using a delimiter
         & -ints , &
         ! string should be a single character at a minimum
         & --label " " &
         & ')
      ! ASSIGN VALUES TO ELEMENTS
      !     SCALARS
      call get_args('x',x)
      call get_args('y',y)
      call get_args('z',z)
      call get_args('l',l)
      call get_args('L',lbig)
      call get_args('ints',ints)      ! ALLOCATABLE ARRAY
      call get_args('title',title)    ! ALLOCATABLE STRING
      call get_args_fixed_size('p',p) ! NON-ALLOCATABLE ARRAY
      ! USE VALUES
      write(*,*)'x=',x
      write(*,*)'y=',y
      write(*,*)'z=',z
      write(*,*)'p=',p
      write(*,*)'title=',title
      write(*,*)'ints=',ints
      write(*,*)'l=',l
      write(*,*)'L=',lbig
      ! UNNAMED VALUES
      if(size(filenames).gt.0)then
         write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
      endif
      end program demo_set_args

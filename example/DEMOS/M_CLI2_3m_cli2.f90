      program demo_M_CLI2
      use M_CLI2,  only : set_args, get_args
      use M_CLI2,  only : filenames=>unnamed
      use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
      implicit none
      integer                      :: i
      integer,parameter            :: dp=kind(0.0d0)
      !
      ! DEFINE ARGS
      real                         :: x, y, z
      real(kind=dp),allocatable    :: point(:)
      logical                      :: l, lbig
      logical,allocatable          :: logicals(:)
      character(len=:),allocatable :: title    ! VARIABLE LENGTH
      character(len=40)            :: label    ! FIXED LENGTH
      real                         :: p(3)     ! FIXED SIZE
      logical                      :: logi(3)  ! FIXED SIZE
      !
      ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
      !   o set a value for all keywords.
      !   o double-quote strings
      !   o set all logical values to F or T.
      !   o value delimiter is comma, colon, or space
      call set_args('                         &
              & -x 1 -y 2 -z 3                &
              & -p -1 -2 -3                   &
              & --point 11.11, 22.22, 33.33e0 &
              & --title "my title" -l F -L F  &
              & --logicals  F F F F F         &
              & -logi F T F                   &
              & --label " " &
              ! note space between quotes is required
              & ')
      ! ASSIGN VALUES TO ELEMENTS
      call get_args('x',x)         ! SCALARS
      call get_args('y',y)
      call get_args('z',z)
      call get_args('l',l)
      call get_args('L',lbig)
      call get_args('title',title) ! ALLOCATABLE STRING
      call get_args('point',point) ! ALLOCATABLE ARRAYS
      call get_args('logicals',logicals)
      !
      ! for NON-ALLOCATABLE VARIABLES

      ! for non-allocatable string
      call get_args_fixed_length('label',label)

      ! for non-allocatable arrays
      call get_args_fixed_size('p',p)
      call get_args_fixed_size('logi',logi)
      !
      ! USE VALUES
      write(*,*)'x=',x, 'y=',y, 'z=',z, x+y+z
      write(*,*)'p=',p
      write(*,*)'point=',point
      write(*,*)'title=',title
      write(*,*)'label=',label
      write(*,*)'l=',l
      write(*,*)'L=',lbig
      write(*,*)'logicals=',logicals
      write(*,*)'logi=',logi
      !
      ! unnamed strings
      !
      if(size(filenames) > 0)then
         write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
      endif
      !
      end program demo_M_CLI2

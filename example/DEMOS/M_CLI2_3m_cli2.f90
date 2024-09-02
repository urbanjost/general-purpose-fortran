      program demo_M_CLI2
      use M_CLI2,  only : set_args, get_args
      use M_CLI2,  only : filenames=>unnamed
      use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
      implicit none
      integer,parameter            :: dp=kind(0.0d0)
      integer                      :: i
       !
       ! Define ARGS
      real                         :: x, y, z
      logical                      :: l, lbig
      character(len=40)            :: label    ! FIXED LENGTH
      real(kind=dp),allocatable    :: point(:)
      logical,allocatable          :: logicals(:)
      character(len=:),allocatable :: title    ! VARIABLE LENGTH
      real                         :: p(3)     ! FIXED SIZE
      logical                      :: logi(3)  ! FIXED SIZE
       !
       ! DEFINE AND PARSE (TO SET INITIAL VALUES) COMMAND LINE
       !   o set a value for all keywords.
       !   o double-quote strings, strings must be at least one space
       !     because adjacent double-quotes designate a double-quote
       !     in the value.
       !   o set all logical values to F
       !   o numeric values support an "e" or "E" exponent
       !   o for lists delimit with a comma, colon, or space
      call set_args('                         &
              & -x 1 -y 2 -z 3                &
              & -p -1 -2 -3                   &
              & --point 11.11, 22.22, 33.33e0 &
              & --title "my title" -l F -L F  &
              & --logicals  F F F F F         &
              & --logi F T F                  &
              & --label " " &
              ! note space between quotes is required
              & ')
       ! Assign values to elements using G_ARGS(3f).
       ! non-allocatable scalars can be done up to twenty per call
      call get_args('x',x, 'y',y, 'z',z, 'l',l, 'L',lbig)
       ! As a convenience multiple pairs of keywords and variables may be
       ! specified if and only if all the values are scalars and the CHARACTER
       ! variables are fixed-length or pre-allocated.
       !
       ! After SET_ARGS(3f) has parsed the command line
       ! GET_ARGS(3f) retrieves the value of keywords accept for
       ! two special cases. For fixed-length CHARACTER variables
       ! see GET_ARGS_FIXED_LENGTH(3f). For fixed-size arrays see
       ! GET_ARGS_FIXED_SIZE(3f).
       !
       ! allocatables should be done one at a time
      call get_args('title',title) ! allocatable string
      call get_args('point',point) ! allocatable arrays
      call get_args('logicals',logicals)
       !
       ! less commonly ...

       ! for fixed-length strings
      call get_args_fixed_length('label',label)

       ! for non-allocatable arrays
      call get_args_fixed_size('p',p)
      call get_args_fixed_size('logi',logi)
       !
       ! all done parsing, use values
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

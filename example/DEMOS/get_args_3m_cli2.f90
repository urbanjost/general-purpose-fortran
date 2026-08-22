      program demo_get_args
      use M_CLI2,  only : filenames=>unnamed, set_args, get_args
      use M_CLI2,  only : dget, iget, lget, rget, sget, cget
      use M_CLI2,  only : dgets, igets, lgets, rgets, sgets, cgets
      implicit none
      integer                      :: i
       ! Define ARGS
      real                         :: x, y, z
      real,allocatable             :: p(:)
      character(len=:),allocatable :: title
      logical                      :: l, lbig
       ! Define and parse (to set initial values) command line
       !   o only quote strings and use double-quotes
       !   o set all logical values to F or T.
      call set_args('         &
         & -x 1 -y 2 -z 3     &
         & -p -1,-2,-3        &
         & --title "my title" &
         & -l F -L F          &
         & --label " "        &
         & ')
       ! Assign values to elements
       ! Scalars
      call get_args( 'x',x, 'y',y, 'z',z, 'l',l, 'L',lbig )
       ! Allocatable string
      call get_args('title',title)
       ! Allocatable arrays
      call get_args('p',p)
       ! Use values
      write(*,'(1x,g0,"=",g0)')'x',x, 'y',y, 'z',z
      write(*,*)'p=',p
      write(*,*)'title=',title
      write(*,*)'l=',l
      write(*,*)'L=',lbig
      if(size(filenames) > 0)then
         write(*,'(i6.6,3a)')(i,'[',filenames(i),']',i=1,size(filenames))
      endif
      ! or the equivalent using functions instead of get_args(3)
      write(*,'(1x,g0,"=",g0)')'x',rget('x'), 'y',rget('y'), 'z',rget('z')
      write(*,*)'p=',rgets('p')
      write(*,*)'title=',sget('title')
      write(*,*)'l=',lget('l')
      write(*,*)'L=',lget('L')
      end program demo_get_args

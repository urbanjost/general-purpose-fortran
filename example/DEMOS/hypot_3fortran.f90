      program demo_hypot
      use, intrinsic :: iso_fortran_env, only : &
       & real_kinds, real32, real64, real128
      implicit none
      real(kind=real32) :: x, y
      real(kind=real32),allocatable :: xs(:), ys(:)
      integer :: i
      character(len=*),parameter :: f='(a,/,SP,*(3x,g0,1x,g0:,/))'

         x = 1.e0_real32
         y = 0.5e0_real32

         write(*,*)
         write(*,'(*(g0))')'point <',x,',',y,'> is ',hypot(x,y)
         write(*,'(*(g0))')'units away from the origin'
         write(*,*)

         ! elemental
         xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]
         ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]

         write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
         write(*,f)"have distances from the origin of ",hypot(xs,ys)
         write(*,f)"the closest is",minval(hypot(xs,ys))

      end program demo_hypot

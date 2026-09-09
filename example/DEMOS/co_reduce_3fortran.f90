      program demo_co_reduce
      implicit none
      integer :: val

         val = this_image()
         call co_reduce(val, myprod, 1)
         if (this_image() == 1) then
            write(*,*) "Product value", val  ! prints num_images() factorial
         endif

      contains

      pure function myprod(a, b)
         integer, value :: a, b
         integer :: myprod
         myprod = a * b
      end function myprod

      end program demo_co_reduce

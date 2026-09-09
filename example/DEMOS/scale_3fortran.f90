      program demo_scale
      implicit none
      real :: x
      complex :: c
      integer :: i
         x = 1.0
         print *, (scale(x,i),i=1,5)
         x = 3.0
         print *, (scale(x,i),i=1,5)
         print *, (scale(log(1.0),i),i=1,5)
         ! on modern machines radix(x) is almost certainly 2
         x = 178.1387e-4
         i = 5
         print *, x, i, scale(x, i), x*radix(x)**i
         ! x*radix(x)**i is the same except roundoff errors are not restricted
         i = 2
         print *, x, i, scale(x, i), x*radix(x)**i
         ! relatively easy to do complex values as well
         c=(3.0,4.0)
         print *, c, i, scale_complex(c, i)!, c*radix(c)**i
      contains
      function scale_complex(x, n)
      ! example supporting complex value for default kinds
      complex, intent(in) :: x
      integer, intent(in) :: n
      complex :: scale_complex
         scale_complex=cmplx(scale(x%re, n), scale(x%im, n), kind=kind(x%im))
      end function scale_complex
      end program demo_scale

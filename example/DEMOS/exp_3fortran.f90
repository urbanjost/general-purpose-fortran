      program demo_exp
      implicit none
      integer,parameter :: dp=kind(0.0d0)
      real              :: x, re, im
      complex           :: cx
      real              :: r_array(3), r_array_result(3)
      complex           :: c_array(2), c_array_result(2)
      integer           :: i

         x = 1.0
         write(*,*)"Euler's constant is approximately",exp(x)

         !! complex values
         ! given
         re=3.0
         im=4.0
         cx=cmplx(re,im)

         ! complex results from complex arguments are Related to Euler's formula
         write(*,*)'given the complex value ',cx
         write(*,*)'exp(x) is',exp(cx)
         write(*,*)'is the same as',exp(re)*cmplx(cos(im),sin(im),kind=kind(cx))

         ! exp(3) is the inverse function of log(3) so
         ! the real component of the input must be less than or equal to
         write(*,*)'maximum real component',log(huge(0.0))
         ! or for double precision
         write(*,*)'maximum doubleprecision component',log(huge(0.0d0))

         ! but since the imaginary component is passed to the cos(3) and sin(3)
         ! functions the imaginary component can be any real value

         ! Real array example
         r_array = [0.0, 1.0, -1.0]
         r_array_result = exp(r_array)
         do i = 1, size(r_array)
           write(*, '(A, I0, A, F15.10)') "exp(r_array(", i, ")) = ", r_array_result(i)
         enddo

         ! Complex array example
         c_array = [cmplx(0.0, 0.0, kind=dp), cmplx(1.0, 1.0, kind=dp)]
         c_array_result = exp(c_array)
         do i = 1, size(c_array)
           write(*, '(A, I0, A, F15.10, A, F15.10, A)') "exp(c_array(", i, ")) = (", &
           real(c_array_result(i)), ", ", aimag(c_array_result(i)), ")"
         enddo
      end program demo_exp

      program demo_exp
      implicit none
      real :: x, re, im
      complex :: cx

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

      end program demo_exp

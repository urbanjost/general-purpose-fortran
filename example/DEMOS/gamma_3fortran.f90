      program demo_gamma
      use, intrinsic :: iso_fortran_env, only : wp=>real64, int64
      implicit none
      real :: x, xa(4)
      integer :: i, j

         ! basic usage
         x = gamma(1.0)
         write(*,*)'gamma(1.0)=',x

         ! elemental
         xa=gamma([1.0,2.0,3.0,4.0])
         write(*,*)xa
         write(*,*)

         ! gamma() is related to the factorial function
         do i = 1, 171
            ! check value is not too big for default integer type
            if (factorial(i)  <=  huge(0)) then
               write(*,*) i, nint(factorial(i)), 'integer'
            elseif (factorial(i)  <=  huge(0_int64)) then
               write(*,*) i, nint(factorial(i),kind=int64),'integer(kind=int64)'
            else
               write(*,*) i, factorial(i) , 'user factorial function'
               write(*,*) i, product([(real(j, kind=wp), j=1, i)]), 'product'
               write(*,*) i, gamma(real(i + 1, kind=wp)), 'gamma directly'
            endif
         enddo

      contains
      function factorial(i) result(f)
      !  GAMMA(X) computes Gamma of X. For positive whole number values of N the
      !  Gamma function can be used to calculate factorials, as (N-1)! ==
      !  GAMMA(REAL(N)). That is
      !
      !      n! == gamma(real(n+1))
      !
      integer, intent(in) :: i
      real(kind=wp) :: f
         if (i  <=  0) then
            write(*,'(*(g0))') '<ERROR> gamma(3) function value ', i, ' <= 0'
            stop '<STOP> bad value in gamma function'
         endif
         f = anint(gamma(real(i + 1,kind=wp)))
      end function factorial

      end program demo_gamma

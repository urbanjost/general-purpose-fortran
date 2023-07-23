      program demo_gamma
      use, intrinsic :: iso_fortran_env, only : wp=>real64
      implicit none
      real :: x, xa(4)
      integer :: i

         x = gamma(1.0)
         write(*,*)'gamma(1.0)=',x

         ! elemental
         xa=gamma([1.0,2.0,3.0,4.0])
         write(*,*)xa
         write(*,*)

         ! gamma(3) is related to the factorial function
         do i=1,20
            ! check value is not too big for default integer type
            if(factorial(i).gt.huge(0))then
               write(*,*)i,factorial(i)
            else
               write(*,*)i,factorial(i),int(factorial(i))
            endif
         enddo
         ! more factorials
         FAC: block
         integer,parameter :: n(*)=[0,1,5,11,170]
         integer :: j
            do j=1,size(n)
               write(*,'(*(g0,1x))')'factorial of', n(j),' is ', &
                & product([(real(i,kind=wp),i=1,n(j))]),  &
                & gamma(real(n(j)+1,kind=wp))
            enddo
         endblock FAC

         contains
         function factorial(i) result(f)
         integer,parameter :: dp=kind(0d0)
         integer,intent(in) :: i
         real :: f
            if(i.le.0)then
               write(*,'(*(g0))')'<ERROR> gamma(3) function value ',i,' <= 0'
               stop      '<STOP> bad value in gamma function'
            endif
            f=gamma(real(i+1))
         end function factorial
      end program demo_gamma

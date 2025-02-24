program primes
!(LICENSE:MIT)
   use M_factor, only: i_is_prime
   implicit none
   integer  :: i
   integer  :: icount=0
   integer,parameter :: n= huge(0)
   do i=2, n
      if(i_is_prime(i))then
         icount=icount+1
         write(*,*)icount,i
      endif
   enddo
end program primes

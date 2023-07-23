!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program test_suite_M_factor
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_framework__verify, only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start
use :: M_framework__verify, only : unit_check_level
use M_framework__verify, only : unit_check_stop
use :: M_factor
implicit none
interface; subroutine test_greatest_common_divisor(); end ; end interface
interface; subroutine test_i_is_prime(); end ; end interface
interface; subroutine test_least_common_multiple(); end ; end interface
interface; subroutine test_prime_factors(); end ; end interface
unit_check_level=0
call unit_check_start('M_factor')
   call test_greatest_common_divisor
   call test_i_is_prime
   call test_least_common_multiple
   call test_prime_factors
   !x!call test_gcd
   !x!call test_gcd_2
   call unit_check_stop()
end program test_suite_M_factor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_least_common_multiple
use M_framework__verify, only : unit_check, unit_check_good, unit_check_done, unit_check_start, unit_check_level
use :: M_factor
implicit none
   call unit_check_start('least_common_multiple')
   ! SCALAR:
   call writeit(10,24,120,'scalar:')
   call writeit(15,30,30,'scalar:')
   call writeit(-15,-30,30,'scalar:')
   call writeit(15,-30,30,'scalar:')
   call writeit(-15,30,30,'scalar:')
   ! VECTOR:
   call writeit_v([10,24],120,'vector:')
   call writeit_v([15,30],30,'vector:')
   call writeit_v([-15,-30],30,'vector:')
   call writeit_v([5,-15,-40],120,'vector:')
   call writeit_v([2,3,4,5],60,'vector:')
   ! Special cases:
   call writeit_v([15,0],0,'special cases:')
   call writeit_v([-15,0],0,'special cases:')
   call writeit_v([0],0,'special cases:')
   call writeit_v([-10],10,'special cases:')
   call writeit_v([22],22,'special cases:')
   call writeit_v([0,0],0,'special cases:')
   call writeit_v([0,0,0,0,0],0,'special cases:')
   call writeit_v([0,0,0,-1,0],0,'special cases:')
   call writeit_v([0,0,0,33,0,3,11],0,'special cases:')

   call unit_check_done('least_common_multiple')
contains

subroutine writeit(ii,jj,answer,message)
integer,intent(in)          :: ii,jj
integer,intent(in)          :: answer
character(len=*),intent(in) :: message
character(len=1024)         :: line
   write(line,'(a,"For least_common_multiple(",I0,",",I0,") the value is ",I0," which is ",L1)')&
   & trim(message),ii,jj,least_common_multiple(ii,jj),least_common_multiple(ii,jj).eq.answer
   call unit_check('least_common_multiple',least_common_multiple(ii,jj).eq.answer,msg=line)
end subroutine writeit

subroutine writeit_v(array,answer,message)
integer,intent(in)          :: array(:)
integer,intent(in)          :: answer
character(len=*),intent(in) :: message
character(len=1024)         :: line
integer                     :: i
  write(line,'(a,*(i0,1x))')"For LCM([",[(array(i),i=1,size(array))]
  call unit_check('least_common_multiple',least_common_multiple(array).eq.answer,message,line, ']) the value is',answer)
end subroutine writeit_v

end subroutine test_least_common_multiple
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_greatest_common_divisor
use M_framework__verify, only : unit_check, unit_check_good, unit_check_done, unit_check_start
use :: M_factor
implicit none
integer, allocatable :: matrix(:,:)
   call unit_check_start('greatest_common_divisor')
   ! SCALAR:
   call writeit(26,130,26)
   call writeit(91,390,13)
   call writeit(-91,390,13)
   call writeit(91,-390,13)
   call writeit(-41,-43,1)
   call writeit(-20,-10,10)
   call writeit(20,10,10)
   ! VECTOR:
   call writeit_v([26,130,91,390],13)
   call writeit_v([5,7,11,13,17,19,23,29,31,37,41,43,47],1)
   call writeit_v([-20,-10,0],10)
   call writeit_v([20,10,0],10)
   call writeit_v([26,130],26)
   call writeit_v([91,390],13)
   call writeit_v([-91,390],13)
   call writeit_v([91,-390],13)
   call writeit_v([-41,-43],1)
   call writeit_v([-20,-10],10)
   call writeit_v([20,10],10)
   ! MATRIX:
   matrix=reshape([ 11,22,33,44,55,66],[2,3])
   call write_matrix(matrix,11)
   matrix=reshape([5,7,11,13,17,19,23,29,31,37,41,43,47],[13,1])
   call write_matrix(matrix,1)
   matrix=reshape([40,80,120,160],[2,2])
   call write_matrix(matrix,40)

   call unit_check_done('greatest_common_divisor')

   contains

   subroutine writeit(ii,jj,answer)
   integer,intent(in) :: ii,jj
   integer,intent(in) :: answer
   character(len=1024) :: line
      write(line,'("greatest_common_divisor([",i0,",",i0,"]) produces ",i0)') ii,jj,greatest_common_divisor(ii,jj)
      call unit_check('greatest_common_divisor',greatest_common_divisor(ii,jj).eq.answer,msg=line)
   end subroutine writeit

   subroutine writeit_v(vector,answer)
   integer,intent(in) :: vector(:)
   integer,intent(in) :: answer
   character(len=1024) :: line1
   character(len=1024) :: line2
      write(line1,'("GCD([",*(i0:,","))')vector
      write(line2,'("]) produces ",i0)') greatest_common_divisor(vector)
      call unit_check('greatest_common_divisor',greatest_common_divisor(vector).eq.answer,line1,line2)
   end subroutine writeit_v

   subroutine write_matrix(matrix,answer)
   integer,intent(in) :: matrix(:,:)
   integer,intent(in) :: answer
   character(len=1024) :: line1
   character(len=1024) :: line2
   character(len=1024) :: line3
      write(line1,'(a,i0,1x,i0)')'MATRIX SHAPE:',size(matrix,dim=1),size(matrix,dim=2)
      write(line2,'("GCD([",*(i0:,","))')matrix
      write(line3,'("]) produces ",i0)') greatest_common_divisor(matrix)
      call unit_check('greatest_common_divisor',greatest_common_divisor(matrix).eq.answer,line1,line2,line3)
   end subroutine write_matrix

end subroutine test_greatest_common_divisor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_i_is_prime
use M_framework__verify, only : unit_check, unit_check_good, unit_check_done, unit_check_start, unit_check_level
use :: M_factor
implicit none
integer   :: i
integer   :: icount=0
integer   :: isum=0
!*!integer  :: n=2**31-1
integer   :: n=10000
integer,parameter :: nn(*)=[      9973, 1127326861, 1127326883, 1127326897, 1127326901, 1127326927, 1127326957, &
                            1127326961, 1127326969, 1127326973, 1127327029]
   call unit_check_start('i_is_prime')
   do i=2,n
      if(i_is_prime(i))then
         icount=icount+1
         isum=isum+i
         if(unit_check_level.gt.0)then
            write(*,*)'prime ',icount,' is ',i, isum
         endif
      endif
   enddo
   if(unit_check_level.gt.0)then
      write(*,*)'number of primes from 2 to ',n,' is ',icount
      write(*,*)'sum of primes from 2 to ',n,' is ',isum
   endif
   call unit_check('i_is_prime',icount.eq.1229,msg='should be 1229')
   call unit_check('i_is_prime',isum.eq.5736396,msg='should be 5736396')

   CHECKTHEM: block
   do i=1,size(nn)
      call unit_check('i_is_prime',i_is_prime(nn(i)),'testing',nn(i),'which should be prime')
   enddo
   endblock CHECKTHEM

   call unit_check('i_is_prime', all(.not.i_is_prime([4,6,8,9,10,12,14,15,16,18])),msg='check some values that are not prime')

   call unit_check_done('i_is_prime')
end subroutine test_i_is_prime
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_prime_factors
use M_framework__verify, only : unit_check, unit_check_good, unit_check_done, unit_check_start, unit_check_level
use :: M_factor
implicit none
integer,allocatable :: primes(:)
integer,allocatable :: exponents(:)
   integer  :: iexp(10), iprm(10), nprm, number
   logical  :: verbose=.true.

   if(unit_check_level.gt.0)then
      verbose=.true.
   else
      verbose=.false.
   endif
   call unit_check_start('prime_factors')
   number=2030303
   primes=[11,379,487]
   exponents=[1,1,1]
   call prime_factors(number, nprm, iprm, iexp,verbose)
   call unit_check('prime_factors',all(iprm(:nprm).eq.primes).and.all(iexp(:nprm).eq.exponents),'factors of ',number)

   number=2147483646
   primes=[2,3,7,11,31,151,331]
   exponents=[1,2,1, 1, 1,  1,  1]
   call prime_factors(number, nprm, iprm, iexp,verbose)
   call unit_check('prime_factors',all(iprm(:nprm).eq.primes).and.all(iexp(:nprm).eq.exponents),'factors of ',number)

   number=2147483647
   primes=[2147483647]
   exponents=[1]
   call prime_factors(number, nprm, iprm, iexp,verbose)
   call unit_check('prime_factors',all(iprm(:nprm).eq.primes).and.all(iexp(:nprm).eq.exponents),'factors of ',number)

   call unit_check_done('prime_factors')

end subroutine test_prime_factors
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

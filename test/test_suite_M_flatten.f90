program runtest
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64
use,intrinsic :: iso_fortran_env, only : stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
use M_framework, only : unit_test, unit_test_end, unit_test_mode
use M_framework, only : unit_test_start, unit_test_msg, unit_test_level
use M_framework, only : unit_test_stop
use M_framework, only : str

use M_flatten,   only : flatten
implicit none
logical, parameter :: T=.true., F=.false.

call unit_test_mode(  &
   keep_going=T,      &
   flags=[0],         &
   luns=[stdout],     &
   command='',        &
   brief=F,           &
   match='',          &
   interactive=F,     &
   CMDLINE=T,         &
   debug=F)

unit_test_level=0


call unit_test_msg('M_flatten','This section contains unit tests for procedures in the M_flatten(3f) module.')

call test_flatten()
call unit_test_stop('M_flatten tests completed')

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_flatten()

integer,allocatable             :: expected(:)
integer                         :: a
integer                         :: b0, b1(-1:1), b2(2,2), b3(2,2,1), b4(2,2,2)

integer(kind=int64),allocatable :: expected_64(:)
integer(kind=int64)             :: a_64
integer(kind=int64)             :: b0_64, b1_64(-1:1), b2_64(2,2), b3_64(2,2,1), b4_64(2,2,2)

real(kind=real64),allocatable       :: expected_64r(:)
real(kind=real64)                   :: a_64r
real(kind=real64)                   :: b0_64r, b1_64r(-1:1), b2_64r(2,2), b3_64r(2,2,1), b4_64r(2,2,2)

real(kind=real32),allocatable       :: expected_32r(:)
real(kind=real32)                   :: a_32r
real(kind=real32)                   :: b0_32r, b1_32r(-1:1), b2_32r(2,2), b3_32r(2,2,1), b4_32r(2,2,2)

   call unit_test_start('flatten        ','scalar test')

   a=0
   call wanted ( a, b0 )
   call unit_test('flatten',b0==1_int32 ,msg="scalar int32")
   expected=[2_int32,3_int32,4_int32]
   call wanted ( a, b1 )
   call unit_test('flatten',all(b1==expected) ,"rank one int32",str(b1))
   expected=[5_int32,6_int32,7_int32,8_int32]
   call wanted ( a, b2 )
   call unit_test('flatten',all(pack(b2,T)==expected) ,"rank two int32",str(pack(b2,T)))
   expected=[9_int32,10_int32,11_int32,12_int32]
   call wanted ( a, b3 )
   call unit_test('flatten',all(pack(b3,T)==expected) ,"rank three int32",str(pack(b3,T)))

   a_64=0
   call wanted_64 ( a_64, b0_64 )
   call unit_test('flatten',b0_64==1_int64 ,msg="scalar int64")
   expected_64=[2_int64,3_int64,4_int64]
   call wanted_64 ( a_64, b1_64 )
   call unit_test('flatten',all(b1_64==expected_64) ,"rank one int64",str(b1_64))
   expected_64=[5_int64,6_int64,7_int64,8_int64]
   call wanted_64 ( a_64, b2_64 )
   call unit_test('flatten',all(pack(b2_64,T)==expected_64) ,"rank two int64",str(pack(b2_64,T)))
   expected_64=[9_int64,10_int64,11_int64,12_int64]
   call wanted_64 ( a_64, b3_64 )
   call unit_test('flatten',all(pack(b3_64,T)==expected_64) ,"rank three int64",str(pack(b3_64,T)))

   a_64r=0.0_real64
   call wanted_64r ( a_64r, b0_64r )
   call unit_test('flatten',b0_64r==1.0_real64 ,msg="scalar real64")
   expected_64r=[2.0_real64,3.0_real64,4.0_real64]
   call wanted_64r ( a_64r, b1_64r )
   call unit_test('flatten',all(b1_64r==expected_64r) ,"rank one real64",str(b1_64r))
   expected_64r=[5.0_real64,6.0_real64,7.0_real64,8.0_real64]
   call wanted_64r ( a_64r, b2_64r )
   call unit_test('flatten',all(pack(b2_64r,T)==expected_64r) ,"rank two real64",str(pack(b2_64r,T)))
   expected_64r=[9.0_real64,10.0_real64,11.0_real64,12.0_real64]
   call wanted_64r ( a_64r, b3_64r )
   call unit_test('flatten',all(pack(b3_64r,T)==expected_64r) ,"rank three real64",str(pack(b3_64r,T)))

   a_32r=0.0_real32
   call wanted_32r ( a_32r, b0_32r )
   call unit_test('flatten',b0_32r==1.0_real32 ,msg="scalar real32")
   expected_32r=[2.0_real32,3.0_real32,4.0_real32]
   call wanted_32r ( a_32r, b1_32r )
   call unit_test('flatten',all(b1_32r==expected_32r) ,"rank one real32",str(b1_32r))
   expected_32r=[5.0_real32,6.0_real32,7.0_real32,8.0_real32]
   call wanted_32r ( a_32r, b2_32r )
   call unit_test('flatten',all(pack(b2_32r,T)==expected_32r) ,"rank two real32",str(pack(b2_32r,T)))
   expected_32r=[9.0_real32,10.0_real32,11.0_real32,12.0_real32]
   call wanted_32r ( a_32r, b3_32r )
   call unit_test('flatten',all(pack(b3_32r,T)==expected_32r) ,"rank three real32",str(pack(b3_32r,T)))

   a=0
   expected=[1,2,3,4,5,6,7,8]
   call wanted ( a, b4 )
   call unit_test('flatten',all(pack(b4,T)==expected) ,"rank 2x2x2",str(pack(b4,T)))
   a=0
   b4=-99
   expected=[-99,1,-99,2,-99,3,-99,4]
   call wanted ( a, b4(2,:,:) )
   call unit_test('flatten',all(pack(b4,T)==expected) ,"rank 2x2x2 (2,:,:)",str(pack(b4,T)))
   a=0
   b4=-99
   expected=[-99,-99,1,2,-99,-99,3,4]
   call wanted ( a, b4(:,2,:) )
   call unit_test('flatten',all(pack(b4,T)==expected) ,"rank 2x2x2 (:,2,:)",str(pack(b4,T)))

   ! Alternatively, to avoid using pointers directly
   ! write the called routine to expect a flattened
   ! array and call the argument with flatten().

   a=0
   call wanted1 ( a, flatten(b0) )
   call unit_test('flatten',b0==1 ,msg="scalar")
   expected=[2,3,4]
   call wanted1 ( a, flatten(b1) )
   call unit_test('flatten',all(b1==expected) ,"rank one",str(b1))
   expected=[5,6,7,8]
   call wanted1 ( a, flatten(b2) )
   call unit_test('flatten',all(pack(b2,T)==expected) ,"rank two",str(pack(b2,T)))
   expected=[9,10,11,12]
   call wanted1 ( a, flatten(b3) )
   call unit_test('flatten',all(pack(b3,T)==expected) ,"rank three",str(pack(b3,T)))

   a=0
   expected=[1,2,3,4,5,6,7,8]
   call wanted1 ( a, flatten(b4) )
   call unit_test('flatten',all(pack(b4,T)==expected) ,"rank 2x2x2",str(pack(b4,T)))

   ! a call like call wanted1 ( a, flatten(b4(2,:,:)) ) would create and change a temporary, not the desired values
   ! so have to copy values to change into an array that can be passed directory and then disperse the returned
   ! values into the selected positions
   a=0
   b4=-99
   expected=[-99,1,-99,2,-99,3,-99,4]
   b2=b4(2,:,:)
   call wanted1 ( a, flatten(b2) )
   b4(2,:,:)=b2
   call unit_test('flatten',all(pack(b4,T)==expected) ,"rank 2x2x2 (2,:,:)",str(pack(b4,T)))
   a=0
   b4=-99
   expected=[-99,-99,1,2,-99,-99,3,4]
   b2=b4(:,2,:)
   call wanted1 ( a, flatten(b2) )
   b4(:,2,:)=b2
   call unit_test('flatten',all(pack(b4,T)==expected) ,"rank 2x2x2 (:,2,:)",str(pack(b4,T)))

   call unit_test_end('flatten')

end subroutine test_flatten

subroutine wanted1( a, b)
integer, intent(inout) :: a
integer, intent(out)   :: b(:)
integer                :: i
   do i=1,size(b)
      a = a + 1
      b(i) = a
   enddo
end subroutine wanted1

subroutine wanted( a, b)
! This technique is known as pointer rank remapping (introduced in Fortran 2003 and expanded in Fortran 2008).
! requires the multi-dimensional target array is simply contiguous.
integer, intent(inout)                  :: a
integer,target, contiguous, intent(out) :: b(..)
integer                                 :: i
integer,pointer                         :: p_b(:)
   p_b=>flatten(b)
   do i=1,size(b)
      a = a + 1
      p_b(i) = a
   enddo
end subroutine wanted

subroutine wanted_32r( a, b)
! This technique is known as pointer rank remapping (introduced in Fortran 2003 and expanded in Fortran 2008).
! requires the multi-dimensional target array is simply contiguous.
real(kind=real32),intent(inout)                   :: a
real(kind=real32),target, contiguous, intent(out) :: b(..)
integer                                           :: i
real(kind=real32),pointer                         :: p_b(:)
   p_b=>flatten(b)
   do i=1,size(b)
      a = a + 1.0_real32
      p_b(i) = a
   enddo
end subroutine wanted_32r

subroutine wanted_64r( a, b)
! This technique is known as pointer rank remapping (introduced in Fortran 2003 and expanded in Fortran 2008).
! requires the multi-dimensional target array is simply contiguous.
real(kind=real64),intent(inout)                   :: a
real(kind=real64),target, contiguous, intent(out) :: b(..)
integer                                           :: i
real(kind=real64),pointer                         :: p_b(:)
   p_b=>flatten(b)
   do i=1,size(b)
      a = a + 1.0_real64
      p_b(i) = a
   enddo
end subroutine wanted_64r

subroutine wanted_64( a, b)
! This technique is known as pointer rank remapping (introduced in Fortran 2003 and expanded in Fortran 2008).
! requires the multi-dimensional target array is simply contiguous.
integer(kind=int64),intent(inout)                   :: a
integer(kind=int64),target, contiguous, intent(out) :: b(..)
integer                                             :: i
integer(kind=int64),pointer                         :: p_b(:)
   p_b=>flatten(b)
   do i=1,size(b)
      a = a + 1
      p_b(i) = a
   enddo
end subroutine wanted_64
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT












!-----------------------------------------------------------------------------------------------------------------------------------


!-----------------------------------------------------------------------------------------------------------------------------------
program M_test_suite_M_anything
use, intrinsic :: ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use, intrinsic :: ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
use M_anything, only : anyinteger_to_string, anyscalar_to_int64
use M_anything, only : anyscalar_to_real, anyscalar_to_double
use M_anything, only : anything_to_bytes, bytes_to_anything
use M_anything, only : empty, assignment(=)
use M_framework__msg
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify, only : unit_test_stop
use M_framework__verify, only : unit_test_level
implicit none
   unit_test_level=0
!! setup
   call test_anyscalar_to_int64()
   call test_anyinteger_to_string()
   call test_anyscalar_to_real()
   call test_anyscalar_to_double()
   call test_anything_to_bytes()

   call test_empty()
!!teardown
   call unit_test_stop()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_int64()

call unit_test_start('anyscalar_to_int64',msg='')
call unit_test('anyscalar_to_int64',anyscalar_to_int64(huge(0_int8)) .eq.127_int64, huge(0_int8))
call unit_test('anyscalar_to_int64',anyscalar_to_int64(huge(0_int16)).eq.32767_int64, huge(0_int16))
call unit_test('anyscalar_to_int64',anyscalar_to_int64(huge(0_int32)).eq.2147483647_int64, huge(0_int32))
call unit_test('anyscalar_to_int64',anyscalar_to_int64(huge(0_int64)).eq.9223372036854775807_int64, huge(0_int64))
call unit_test_done('anyscalar_to_int64',msg='')
end subroutine test_anyscalar_to_int64
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyinteger_to_string()

call unit_test_start('anyinteger_to_string',msg='')
call unit_test('anyinteger_to_string',anyinteger_to_string(huge(0_int8)) .eq.'127', huge(0_int8))
call unit_test('anyinteger_to_string',anyinteger_to_string(huge(0_int16)).eq.'32767', huge(0_int16))
call unit_test('anyinteger_to_string',anyinteger_to_string(huge(0_int32)).eq.'2147483647', huge(0_int32))
call unit_test('anyinteger_to_string',anyinteger_to_string(huge(0_int64)).eq.'9223372036854775807', huge(0_int64))
call unit_test_done('anyinteger_to_string',msg='')
end subroutine test_anyinteger_to_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_double()

real :: infinity
!!character(len=*),parameter :: line='infinity'
character(len=8)           :: line='infinity'
read(line,*)infinity
call unit_test_start('anyscalar_to_double',msg='')
call unit_test('anyscalar_to_double',anyscalar_to_double(huge(0_int8))     .eq. huge(0_int8),     huge(0_int8))
call unit_test('anyscalar_to_double',anyscalar_to_double(huge(0_int16))    .eq. huge(0_int16),    huge(0_int16))
call unit_test('anyscalar_to_double',anyscalar_to_double(huge(0_int32))    .eq. huge(0_int32),    huge(0_int32))
call unit_test('anyscalar_to_double',anyscalar_to_double(huge(0_int64))    .eq. huge(0_int64),    huge(0_int64))
call unit_test('anyscalar_to_double',anyscalar_to_double(huge(0.0_real32)) .eq. huge(0.0_real32), huge(0.0_real32))
call unit_test('anyscalar_to_double',anyscalar_to_double(huge(0.0_real64)) .eq. huge(0.0_real64), huge(0.0_real64))

call unit_test_done('anyscalar_to_double',msg='')
end subroutine test_anyscalar_to_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_real()

real :: infinity
!!character(len=*),parameter :: line='infinity'
character(len=8)           :: line='infinity'
read(line,*)infinity
call unit_test_start('anyscalar_to_real',msg='')
call unit_test('anyscalar_to_real',anyscalar_to_real(huge(0_int8))     .eq. real(huge(0_int8)),     huge(0_int8))
call unit_test('anyscalar_to_real',anyscalar_to_real(huge(0_int16))    .eq. real(huge(0_int16)),    huge(0_int16))
call unit_test('anyscalar_to_real',anyscalar_to_real(huge(0_int32))    .eq. real(huge(0_int32)),    huge(0_int32))
call unit_test('anyscalar_to_real',anyscalar_to_real(huge(0_int64))    .eq. real(huge(0_int64)),    huge(0_int64))
call unit_test('anyscalar_to_real',anyscalar_to_real(huge(0.0_real32)) .eq. real(huge(0.0_real32)), huge(0.0_real32))

call unit_test('anyscalar_to_real',anyscalar_to_real(huge(0.0_real64)) .eq. infinity,               huge(0.0_real64))
call unit_test('anyscalar_to_real',anyscalar_to_real(1234.0_real64)   .eq. 1234.0_real64,   1234.0_real64)
call unit_test_done('anyscalar_to_real',msg='')
end subroutine test_anyscalar_to_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anything_to_bytes()
integer :: i, j
call unit_test_start('anything_to_bytes',msg='')
call unit_test('anything_to_bytes',any(anything_to_bytes(huge(0_int8))      .eq. transfer(huge(0_int8),'A')) )
call unit_test('anything_to_bytes',any(anything_to_bytes(huge(0_int16))     .eq. transfer(huge(0_int16),'A')) )
call unit_test('anything_to_bytes',any(anything_to_bytes(huge(0_int32))     .eq. transfer(huge(0_int32),'A')) )
call unit_test('anything_to_bytes',any(anything_to_bytes(huge(0_int64))     .eq. transfer(huge(0_int64),'A')) )
call unit_test('anything_to_bytes',any(anything_to_bytes(huge(0.0_real32))  .eq. transfer(huge(0.0_real32),'A')) )
call unit_test('anything_to_bytes',any(anything_to_bytes(huge(0.0_real64))  .eq. transfer(huge(0.0_real64),'A')) )
call unit_test('anything_to_bytes',any(anything_to_bytes('this is a string') .eq. transfer('this is a string','A')) )

call unit_test('anything_to_bytes',any(&
        & anything_to_bytes(['aaaaaaaaaa','bbbbbbbbbb']) .eq. transfer(['aaaaaaaaaa','bbbbbbbbbb'],'A')),'check against transfer')

call unit_test('anything_to_bytes',size(anything_to_bytes('this is a string')) .eq. len('this is a string'),'check byte count')

i=size( anything_to_bytes(['aaaaaaaaaa','bbbbbbbbbb']))
j=len('aaaaaaaaaabbbbbbbbbb')
call unit_test('anything_to_bytes',i.eq.j,'expected',i,'got',j,'check array byte count')

call unit_test_done('anything_to_bytes',msg='')
end subroutine test_anything_to_bytes
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_empty 
!!use M_anything, only : empty, assignment(=) 
implicit none 
doubleprecision,allocatable  :: d(:)
integer,allocatable          :: i(:)
real,allocatable             :: r(:)
character(len=:),allocatable :: c(:)
integer, allocatable         :: ints(:) 
character(:), allocatable    :: strs(:) 
integer                      :: answer

   call unit_test_start('empty') !  register an entry for specified name in database with status of zero (0)

   d=empty
   r=empty
   i=empty
   c=empty

   call unit_test('empty', size(d).eq.0, 'checking double')
   call unit_test('empty', size(r).eq.0, 'checking real')
   call unit_test('empty', size(i).eq.0, 'checking integer')
   call unit_test('empty', size(c).eq.0, 'checking character')

   ints = empty 
   answer=0
   call check_ints(answer,ints) 

   ints = [1,2,3] 
   answer=3
   call check_ints(answer,ints) 
   call unit_test('empty',all(ints.eq.[1,2,3]),msg='normal allocation')

   ints = empty 
   answer=0
   call check_ints(answer,ints) 

   strs = empty 
   answer=0
   call check_strs(answer,strs) 

   strs = [ "apple", "orang", "banan" ] 
   answer=3
   call check_strs(answer,strs) 
   call unit_test('empty',all(strs.eq.["apple","orang","banan"]),msg='normal allocation') 

   strs = empty 
   answer=0
   call check_strs(answer,strs) 

   call unit_test_done('empty')

end subroutine test_empty 
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine check_ints(answer,ints) 
integer,intent(in),allocatable :: ints(:) 
integer,intent(in) :: answer
   !  if mask test fails, change database status for specified entry to -1 and stop program, else continue
   if(allocated(ints))then
      call unit_test('empty',size(ints).eq.answer,'size is',answer)
   endif
end subroutine check_ints
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine check_strs(answer,strs) 
integer,intent(in)           :: answer
character(len=:),allocatable,intent(in)  :: strs(:) 
integer k 
   if ( allocated(strs) ) then 
       if(unit_test_level.gt.0)then
          print *, "strs: val = ", ( strs( k ) // " ", k=1,size(strs) ) 
          print *, "      len_elem = ", len(strs( 1 )) 
       endif
      call unit_test('empty',size(strs).eq.answer,'size is',answer)
   endif 
end subroutine check_strs
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program M_test_suite_M_anything

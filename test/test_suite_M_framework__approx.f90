program runtest
use, intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
use, intrinsic :: iso_fortran_env,  only : real32, real64, real128
use, intrinsic :: iso_fortran_env,  only : ERROR_UNIT,OUTPUT_UNIT
use M_framework__msg
use M_framework__verify
use M_framework__journal
use M_framework__approx
implicit none
character(len=*),parameter :: g='(*(g0,1x))'
   call unit_test_mode(level=0,luns=[OUTPUT_UNIT])
   call test_accdig()         ! compare two real numbers only up to a specified number of digits
   call test_almost()         ! function compares two numbers only up to a specified number of digits
   call test_in_margin()      ! check if two reals are approximately equal using a relative margin
   call test_round()          ! round val to specified number of significant digits
   call test_significant()    ! round val to specified number of significant digits
   call unit_test_stop()
   contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_in_margin()

   call unit_test_start('in_margin',msg='')
   !!call unit_test('in_margin', 0.eq.0, 'checking', 100)
     call unit_test('in_margin', .not.in_margin(4.00000,3.99999,0.000000001),'compare',4.00000,3.99999,0.000000001)
     call unit_test('in_margin', .not.in_margin(4.00000,3.99999,0.00000001) ,'compare',4.00000,3.99999,0.00000001)
     call unit_test('in_margin', .not.in_margin(4.00000,3.99999,0.0000001)  ,'compare',4.00000,3.99999,0.0000001)
     call unit_test('in_margin', .not.in_margin(4.00000,3.99999,0.000001)   ,'compare',4.00000,3.99999,0.000001)
     call unit_test('in_margin', in_margin(4.00000,3.99999,0.00001)         ,'compare',4.00000,3.99999,0.00001)
     call unit_test('in_margin', in_margin(4.00000,3.99999,0.0001)          ,'compare',4.00000,3.99999,0.0001)
     call unit_test('in_margin', in_margin(4.00000,3.99999,0.001)           ,'compare',4.00000,3.99999,0.001)
     call unit_test('in_margin', in_margin(4.00000,3.99999,0.01)            ,'compare',4.00000,3.99999,0.01)

     call unit_test('in_margin',.not.all(in_margin([4.0,40.0,400.0,4000.0,40000.0],&
                                                  & [3.9,39.9,399.9,3999.9,39999.9],0.000001)),'should all be false')
     call unit_test('in_margin',     all(in_margin([4.0,40.0,400.0,4000.0,40000.0],&
                                                  & [3.9,39.9,399.9,3999.9,39999.9],0.1)),'should all be true')

   call unit_test_end('in_margin',msg='')
end subroutine test_in_margin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_significant()
doubleprecision,allocatable :: answers(:)
doubleprecision,allocatable :: expected(:)

   call unit_test_start('significant',msg='')

   answers=significant(1.23456789012345d0,[1,2,3,4,5,6,7,8,9],'RU')
   expected=[2.0d0, 1.3d0, 1.24d0, 1.235d0, 1.2346d0, 1.23457d0, 1.234568d0, 1.2345679d0, 1.2345679d0]
   call unit_test('significant',all( answers == expected),'RU, round up')

   answers=significant(1.23456789012345d0,[1,2,3,4,5,6,7,8,9],'RD')
   expected=[1.0d0, 1.2d0, 1.23d0, 1.234d0, 1.2345d0, 1.23456d0, 1.234567d0, 1.2345678d0, 1.23456789d0]
   call unit_test('significant',all( answers == expected),'RD, round down ')

   answers=significant(1.23456789012345d0,[1,2,3,4,5,6,7,8,9],'RZ')
   expected=[1.0d0, 1.2d0, 1.23d0, 1.234d0, 1.2345d0, 1.23456d0, 1.234567d0, 1.2345678d0, 1.23456789d0]
   call unit_test('significant',all( answers == expected),'RZ, round towards zero')

   answers=significant(1.23456789012345d0,[1,2,3,4,5,6,7,8,9],'RN')
   expected=[1.0d0, 1.2d0, 1.23d0, 1.235d0, 1.2346d0, 1.23457d0, 1.234568d0, 1.2345679d0, 1.23456789d0]
   call unit_test('significant',all( answers == expected),'RN, round towards nearest representable number')

   answers=significant(1.23456789012345d0,[1,2,3,4,5,6,7,8,9],'RC')
   expected=[1.0d0, 1.2d0, 1.23d0, 1.235d0, 1.2346d0, 1.23457d0, 1.234568d0, 1.2345679d0, 1.23456789d0]
   call unit_test('significant',all( answers == expected),'RC, compatible rounding rounds to closest representable number')

   answers=significant(1.23456789012345d0,[1,2,3,4,5,6,7,8,9],'RP')
   expected=[1.0d0, 1.2d0, 1.23d0, 1.235d0, 1.2346d0, 1.23457d0, 1.234568d0, 1.2345679d0, 1.23456789d0]
   call unit_test('significant',all( answers == expected),'RP, processor-dependent rounding ')

   call unit_test_end('significant',msg='')
end subroutine test_significant
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_round()

   call unit_test_start('round',msg='')
   !!call unit_test('round', 0.eq.0, 'checking', 100)
   call unit_test_end('round',msg='')
end subroutine test_round
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_accdig()
integer,parameter :: sz=16
doubleprecision   :: a, b, aarr(sz), barr(sz)
integer           :: i, ind, indarr(sz)
real              :: acurcy, acurcyarr(sz)

   call unit_test_start('accdig',msg='')
   do i=1,sz
      a=1.0d0
      b=a+1.0d0/(10.0d0**i)
      call accdig(a,b,8.0,acurcy,ind)
      if(unit_test_level>0) write(*,g)i,a,b,acurcy,ind
      aarr(i)=a
      barr(i)=b
   enddo
   call accdig(aarr,barr,8.0,acurcyarr,indarr)
   if(unit_test_level>0) write(*,g)(aarr(i),barr(i),acurcyarr(i),indarr(i),new_line('a'),i=1,sz)
   call unit_test('accdig', all(indarr.eq.[1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0]), 'expected 7 bad, got ',count(indarr.eq.1)+0)
   call unit_test_end('accdig',msg='')
end subroutine test_accdig
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_almost()
real    :: x, y, r
integer :: i
logical,parameter :: expected(*)=[.true., .true., .false., .false., .false., .false., .false., .false.]

   call unit_test_start('almost',msg='')
   x=1.2345678
   y=1.2300000
   do i=1,8
       r=real(i)
       call unit_test('almost',almost(x,y,r,verbose=.false.).eqv.expected(i),'for',x,y,r,'expected',expected(i))
   enddo
   call unit_test_end('almost',msg='')
end subroutine test_almost
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!   call test_accdig()         ! compare two real numbers only up to a specified number of digits
!   call test_round()          ! round val to specified number of significant digits
!   call test_significant()    ! round val to specified number of significant digits

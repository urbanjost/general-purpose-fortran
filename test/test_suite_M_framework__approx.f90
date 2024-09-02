program runtest
use M_framework__msg
use M_framework__verify
use M_framework__journal
use M_framework__approx
   call test_accdig()         ! compare two real numbers only up to a specified number of digits
   call test_almost()         ! function compares two numbers only up to a specified number of digits
   call test_dp_accdig()      ! compare two double numbers only up to a specified number of digits
   call test_in_margin()      ! check if two reals are approximately equal using a relative margin
   call test_round()          ! round val to specified number of significant digits
   call test_significant()    ! round val to specified number of significant digits
   call unit_check_stop()
   contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_in_margin()

   call unit_check_start('in_margin',msg='')
   !!call unit_check('in_margin', 0.eq.0, 'checking', 100)
     call unit_check('in_margin', .not.in_margin(4.00000,3.99999,0.000000001),'compare',4.00000,3.99999,0.000000001)
     call unit_check('in_margin', .not.in_margin(4.00000,3.99999,0.00000001) ,'compare',4.00000,3.99999,0.00000001)
     call unit_check('in_margin', .not.in_margin(4.00000,3.99999,0.0000001)  ,'compare',4.00000,3.99999,0.0000001)
     call unit_check('in_margin', .not.in_margin(4.00000,3.99999,0.000001)   ,'compare',4.00000,3.99999,0.000001)
     call unit_check('in_margin', in_margin(4.00000,3.99999,0.00001)         ,'compare',4.00000,3.99999,0.00001)
     call unit_check('in_margin', in_margin(4.00000,3.99999,0.0001)          ,'compare',4.00000,3.99999,0.0001)
     call unit_check('in_margin', in_margin(4.00000,3.99999,0.001)           ,'compare',4.00000,3.99999,0.001)
     call unit_check('in_margin', in_margin(4.00000,3.99999,0.01)            ,'compare',4.00000,3.99999,0.01)

     call unit_check('in_margin',.not.all(in_margin([4.0,40.0,400.0,4000.0,40000.0],&
                                                  & [3.9,39.9,399.9,3999.9,39999.9],0.000001)),'should all be false')
     call unit_check('in_margin',     all(in_margin([4.0,40.0,400.0,4000.0,40000.0],&
                                                  & [3.9,39.9,399.9,3999.9,39999.9],0.1)),'should all be true')

   call unit_check_done('in_margin',msg='')
end subroutine test_in_margin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_significant()

   call unit_check_start('significant',msg='')
   !!call unit_check('significant', 0.eq.0, 'checking', 100)
   call unit_check_done('significant',msg='')
end subroutine test_significant
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_round()

   call unit_check_start('round',msg='')
   !!call unit_check('round', 0.eq.0, 'checking', 100)
   call unit_check_done('round',msg='')
end subroutine test_round
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_accdig()

   call unit_check_start('accdig',msg='')
   !!call unit_check('accdig', 0.eq.0, 'checking', 100)
   call unit_check_done('accdig',msg='')
end subroutine test_accdig
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_almost()
real    :: x, y, r
integer :: i
logical,parameter :: expected(*)=[.true., .true., .false., .false., .false., .false., .false., .false.]

   call unit_check_start('almost',msg='')
   x=1.2345678
   y=1.2300000
   do i=1,8
       r=real(i)
       call unit_check('almost',almost(x,y,r,verbose=.false.).eqv.expected(i))
   enddo
   call unit_check_done('almost',msg='')
end subroutine test_almost
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dp_accdig()

   call unit_check_start('dp_accdig',msg='')
   !!call unit_check('dp_accdig', 0.eq.0, 'checking', 100)
   call unit_check_done('dp_accdig',msg='')
end subroutine test_dp_accdig
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!   call test_accdig()         ! compare two real numbers only up to a specified number of digits
!   call test_dp_accdig()      ! compare two double numbers only up to a specified number of digits
!   call test_round()          ! round val to specified number of significant digits
!   call test_significant()    ! round val to specified number of significant digits

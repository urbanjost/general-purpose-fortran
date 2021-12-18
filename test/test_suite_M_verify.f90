program runtest
use M_msg
use M_verify
use M_journal
   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   call test_accdig()         ! compare two real numbers only up to a specified number of digits
   call test_almost()         ! function compares two numbers only up to a specified number of digits
   call test_dp_accdig()      ! compare two double numbers only up to a specified number of digits
   call test_in_margin()      ! check if two reals are approximately equal using a relative margin
   call test_round()          ! round val to specified number of significant digits
   call unit_check_stop()
   contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_in_margin()

   call unit_check_start('in_margin',msg='')
   !!call unit_check('in_margin', 0.eq.0, 'checking', 100)
   call unit_check_done('in_margin',msg='')
end subroutine test_in_margin
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

   call unit_check_start('almost',msg='')
   !!call unit_check('almost', 0.eq.0, 'checking', 100)
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

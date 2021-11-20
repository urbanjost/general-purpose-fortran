program program_test_suite_M_display
use M_display
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_display'
!   call test_suite_M_display()
   write(*,*)'COMPLETED M_display'
end program program_test_suite_M_display

program program_test_suite_M_steam67
use M_steam67
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_steam67'
   call test_suite_M_steam67()
   write(*,*)'COMPLETED M_steam67'
end program program_test_suite_M_steam67

program program_test_suite_M_hybrid
use M_hybrid
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_hybrid'
   call test_suite_M_hybrid()
   write(*,*)'COMPLETED M_hybrid'
end program program_test_suite_M_hybrid

program program_test_suite_M_BRE
use M_BRE
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_BRE'
   call test_suite_M_BRE()
   write(*,*)'COMPLETED M_BRE'
end program program_test_suite_M_BRE

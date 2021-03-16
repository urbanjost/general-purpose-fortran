program program_test_suite_M_fixedform
use M_fixedform
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_fixedform'
   call test_suite_M_fixedform()
   write(*,*)'COMPLETED M_fixedform'
end program program_test_suite_M_fixedform

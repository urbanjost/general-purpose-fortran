program program_test_suite_M_units
use M_units
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_units'
   call test_suite_M_units()
   write(*,*)'COMPLETED M_units'
end program program_test_suite_M_units

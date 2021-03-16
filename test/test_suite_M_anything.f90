program program_test_suite_M_anything
use M_anything
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_anything'
   call test_suite_M_anything()
   write(*,*)'COMPLETED M_anything'
end program program_test_suite_M_anything

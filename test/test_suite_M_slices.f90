program program_test_suite_M_slices
use M_slices
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_slices'
   call test_suite_M_slices()
   write(*,*)'COMPLETED M_slices'
end program program_test_suite_M_slices

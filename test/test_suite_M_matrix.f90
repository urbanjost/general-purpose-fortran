program program_test_suite_M_matrix
use M_matrix
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_matrix'
   call test_suite_M_matrix()
   write(*,*)'COMPLETED M_matrix'
end program program_test_suite_M_matrix

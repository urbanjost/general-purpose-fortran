program program_test_suite_M_getopt_long
use M_getopt_long
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_getopt_long'
   !call test_suite_M_getopt_long()
   write(*,*)'COMPLETED M_getopt_long'
end program program_test_suite_M_getopt_long

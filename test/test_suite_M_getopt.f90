program program_test_suite_M_getopt
use M_getopt
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_getopt'
   call test_suite_M_getopt()
   write(*,*)'COMPLETED M_getopt'
end program program_test_suite_M_getopt

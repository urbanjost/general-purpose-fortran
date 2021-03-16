program program_test_suite_M_getkey
use M_getkey
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_getkey'
   call test_suite_M_getkey()
   write(*,*)'COMPLETED M_getkey'
end program program_test_suite_M_getkey

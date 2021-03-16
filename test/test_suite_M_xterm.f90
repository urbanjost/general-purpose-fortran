program program_test_suite_M_xterm
use M_xterm
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_xterm'
   call test_suite_M_xterm()
   write(*,*)'COMPLETED M_xterm'
end program program_test_suite_M_xterm

program program_test_suite_M_hashkeys
use M_hashkeys
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_hashkeys'
   call test_suite_M_hashkeys()
   write(*,*)'COMPLETED M_hashkeys'
end program program_test_suite_M_hashkeys

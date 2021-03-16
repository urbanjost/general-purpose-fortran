program program_test_suite_M_big_integer
use M_big_integer
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_big_integer'
   call test_suite_M_big_integer()
   write(*,*)'COMPLETED M_big_integer'
end program program_test_suite_M_big_integer

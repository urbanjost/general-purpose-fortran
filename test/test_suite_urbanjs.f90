program program_test_suite_urbanjs
use urbanjs
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED urbanjs'
   call test_suite_urbanjs()
   write(*,*)'COMPLETED urbanjs'
end program program_test_suite_urbanjs

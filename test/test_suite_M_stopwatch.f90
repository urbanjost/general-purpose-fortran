program program_test_suite_M_stopwatch
use M_stopwatch
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_stopwatch'
   call test_suite_M_stopwatch()
   write(*,*)'COMPLETED M_stopwatch'
end program program_test_suite_M_stopwatch

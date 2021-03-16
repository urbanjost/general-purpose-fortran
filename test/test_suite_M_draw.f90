program program_test_suite_M_draw
use M_draw
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_draw'
   call test_suite_M_draw()
   write(*,*)'COMPLETED M_draw'
end program program_test_suite_M_draw

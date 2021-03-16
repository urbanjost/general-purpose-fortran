program program_test_suite_M_drawplus
use M_drawplus
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_drawplus'
   call test_suite_M_drawplus()
   write(*,*)'COMPLETED M_drawplus'
end program program_test_suite_M_drawplus

program program_test_suite_M_f2kcli
use M_f2kcli
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_f2kcli'
   call test_suite_M_f2kcli()
   write(*,*)'COMPLETED M_f2kcli'
end program program_test_suite_M_f2kcli

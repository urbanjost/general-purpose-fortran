program program_test_suite_M_kracken
use M_kracken
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_kracken'
   call test_suite_M_kracken()
   write(*,*)'COMPLETED M_kracken'
end program program_test_suite_M_kracken

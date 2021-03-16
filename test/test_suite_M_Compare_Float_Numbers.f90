program program_test_suite_M_Compare_Float_Numbers
use M_Compare_Float_Numbers
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_Compare_Float_Numbers'
   call test_suite_M_Compare_Float_Numbers()
   write(*,*)'COMPLETED M_Compare_Float_Numbers'
end program program_test_suite_M_Compare_Float_Numbers

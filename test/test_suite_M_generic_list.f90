program program_test_suite_M_generic_list
use M_generic_list
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_generic_list'
   call test_suite_M_generic_list()
   write(*,*)'COMPLETED M_generic_list'
end program program_test_suite_M_generic_list

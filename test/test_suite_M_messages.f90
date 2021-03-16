program program_test_suite_M_messages
use M_messages
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_messages'
   call test_suite_M_messages()
   write(*,*)'COMPLETED M_messages'
end program program_test_suite_M_messages

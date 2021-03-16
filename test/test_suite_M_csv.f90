program program_test_suite_M_csv
use M_csv
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_csv'
   call test_suite_M_csv()
   write(*,*)'COMPLETED M_csv'
end program program_test_suite_M_csv

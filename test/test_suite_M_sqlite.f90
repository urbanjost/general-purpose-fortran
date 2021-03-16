program program_test_suite_M_sqlite
use M_sqlite
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_sqlite'
   call test_suite_M_sqlite()
   write(*,*)'COMPLETED M_sqlite'
end program program_test_suite_M_sqlite

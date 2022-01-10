program runtest
use M_sha3, only : test_suite_M_sha3
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED test_suite_M_sha3'
   call test_suite_M_sha3()
   write(*,*)'COMPLETEDtest_suite_ M_sha3'
end program runtest

program runtest
use M_hashkeys, only : test_suite_M_hashkeys
use M_hashkeys, only : test_suite_sha256
use M_verify
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED test_suite_M_hashkeys'
   call test_suite_M_hashkeys()
   call test_suite_sha256()
   write(*,*)'COMPLETED test_suite_ M_hashkeys'
   call unit_check_stop()
end program runtest

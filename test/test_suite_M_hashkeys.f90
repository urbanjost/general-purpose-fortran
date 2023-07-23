program runtest
use M_hashkeys, only : test_suite_M_hashkeys
use M_hashkeys, only : test_suite_sha256
use M_framework__verify, only : unit_check_stop
   write(*,*)'STARTED test_suite_M_hashkeys'
   call test_suite_M_hashkeys()
   call test_suite_sha256()
   write(*,*)'COMPLETED test_suite_M_hashkeys'
   call unit_check_stop()
end program runtest

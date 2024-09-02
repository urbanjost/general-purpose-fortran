program runtest
use M_hashkeys__sha3, only : test_suite_M_hashkeys__sha3
use M_framework__verify, only : unit_check_stop
   write(*,*)'STARTED test_suite_M_hashkeys__sha3'
   call test_suite_M_hashkeys__sha3()
   write(*,*)'COMPLETED test_suite_M_hashkeys__sha3'
   call unit_check_stop()
end program runtest

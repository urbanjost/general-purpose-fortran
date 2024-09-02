module M_test_suite_M_graph
use M_framework__msg
use M_graph
private
public test_suite_m_graph
contains
subroutine test_suite_m_graph()
! this should contains tests for all public procedures in the module
   call test_generate_graph()
end subroutine test_suite_m_graph

subroutine test_generate_graph()
use M_framework__verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad, unit_check_done

end subroutine test_generate_graph

end module M_test_suite_M_graph

program runtest
use M_framework__msg
use M_framework__verify, only : unit_check_stop
use M_test_suite_M_graph
implicit none
   call test_suite_M_graph()
   call unit_check_stop()
end program runtest

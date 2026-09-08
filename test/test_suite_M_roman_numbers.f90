program runtest
use, intrinsic :: iso_fortran_env, only: &
& stdin => input_unit,   &
& stdout => output_unit, &
& stderr => error_unit
use M_framework, only : unit_test_start, unit_test, unit_test_msg
use M_framework, only : unit_test_end, unit_test_stop, unit_test_mode
use M_framework, only : unit_test_level, unit_test_flags
!use M_xxxx
implicit none
logical, parameter :: T=.true., F=.false.
logical            :: matched
! optional call to change default modes
   call unit_test_mode(       &
       keep_going=T,           &
       flags=[0],              &
       luns=[stderr],          &
       command='',             &
       brief=F,                &
       match='',               &
       interactive=F,          &
       CMDLINE=T,              &
       debug=F)

   unit_test_level=0

   call test_suite_M_roman_numbers()
   call unit_test_stop()

contains

subroutine test_suite_M_roman_numbers()
   call unit_test_start("M_roman_numbers",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("M_roman_numbers", 0 .eq. 0, "checking",100)
   call unit_test_end("M_roman_numbers",msg="")
end subroutine test_suite_M_roman_numbers

end program runtest

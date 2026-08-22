program runtest
use, intrinsic :: iso_fortran_env, only: &
& stdin => input_unit,   &
& stdout => output_unit, &
& stderr => error_unit
use M_framework, only : unit_test_start, unit_test, unit_test_msg
use M_framework, only : unit_test_end, unit_test_stop, unit_test_mode
use M_framework, only : unit_test_level ! unit_test_flags
use M_framework, only: str
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

   call test_suite_str   ()
   call test_suite_stderr()
   call test_suite_wrt   ()
   call test_suite_fmt   ()
   call test_suite_set   ()
   call test_suite_pdec  ()
   call test_suite_assert()
   call unit_test_stop('M_framework_msg')

contains

subroutine test_suite_str   ()
logical,allocatable :: tests(:)

  call unit_test_start('str','test building message strings',matched=matched)
   if(.not.matched)return

  tests=[logical :: ]

  call add('INTEGER',str(10),'10','10')
  call add('LOGICAL',str(.false.),'F','F')
  call add('LOGICAL',str(.true.),'T','T')
  call add('REAL',str(100.0),'100.000000','100.0000')
  call add('COMPLEX',str((11.0,22.0)),'(11.0000000,22.0000000)','(11.00000,22.00000)')
  call add('COMPOUND',str(10,100.0,"string",(11.0,22.0),.false.), &
       & '10 100.000000 string (11.0000000,22.0000000) F',&
       & '10 100.0000 string (11.00000,22.00000) F')
  call unit_test_msg('str','tally is ',str(tests)//'') ! //'' for gfortran-11 bug
  call unit_test_end("str   ",msg="")

end subroutine test_suite_str   

subroutine add(message,question,answer,answer2)
character(len=*),intent(in)   :: message
character(len=*),intent(in)   :: question
character(len=*),intent(in)   :: answer
character(len=*),intent(in)   :: answer2
logical                       :: passed
  passed=question .eq. answer
  if(passed)then
     call unit_test('str',passed,'testing',message,'expected',answer,'got',question)
  else
     passed=question .eq. answer2
     call unit_test('str',passed,'testing',message,'expected',answer2,'got',question)
  endif
end subroutine add

subroutine test_suite_stderr()
   call unit_test_start("stderr",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("stderr", 0 .eq. 0, "checking",100)
   call unit_test_end("stderr",msg="")
end subroutine test_suite_stderr

subroutine test_suite_wrt   ()
   call unit_test_start("wrt",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("wrt   ", 0 .eq. 0, "checking",100)
   call unit_test_end("wrt",msg="")
end subroutine test_suite_wrt   

subroutine test_suite_fmt   ()
   call unit_test_start("fmt",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("fmt   ", 0 .eq. 0, "checking",100)
   call unit_test_end("fmt",msg="")
end subroutine test_suite_fmt   

subroutine test_suite_set   ()
   call unit_test_start("set",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("set   ", 0 .eq. 0, "checking",100)
   call unit_test_end("set",msg="")
end subroutine test_suite_set   

subroutine test_suite_pdec  ()
   call unit_test_start("pdec",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("pdec  ", 0 .eq. 0, "checking",100)
   call unit_test_end("pdec",msg="")
end subroutine test_suite_pdec  

subroutine test_suite_assert()
   call unit_test_start("assert",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("assert", 0 .eq. 0, "checking",100)
   call unit_test_end("assert",msg="")
end subroutine test_suite_assert

end program runtest


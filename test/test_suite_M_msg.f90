program main
use M_msg, only: str

implicit none
logical             :: allpassed=.true.
logical,allocatable :: tests(:)

  tests=[logical :: ]

  call add('INTEGER',str(10),'10')
  call add('LOGICAL',str(.false.),'F')
  call add('LOGICAL',str(.true.),'T')
  call add('REAL',str(100.0),'100.000000')
  call add('COMPLEX',str((11.0,22.0)),'(11.0000000,22.0000000)')
  call add('COMPOUND',str(10,100.0,"string",(11.0,22.0),.false.),'10 100.000000 string (11.0000000,22.0000000) F')

  write(*,'(*(g0,1x))')tests
  if (allpassed)then
     write(*,'(*(g0,1x))')"*M_msg::str* Passed",size(tests),"tests"
     stop 0
  else
     write(*,'(*(g0,1x))')"*M_msg::str* Failed",count(.not.tests),"Passed",count(tests)
     stop 1
  endif

contains
subroutine add(message,question,answer)
character(len=*),intent(in)   :: message
character(len=*),intent(in)   :: question
character(len=*),intent(in)   :: answer
logical                       :: passed
  passed=question .eq. answer
  write(*,'(*(g0,1x))')passed,'expected ', answer, 'got',question
  tests=[tests,passed]
  allpassed=allpassed.and.passed
end subroutine add
end program main

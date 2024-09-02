program main
use M_framework__msg, only: str

implicit none
logical             :: allpassed=.true.
logical,allocatable :: tests(:)

  tests=[logical :: ]

  call add('INTEGER',str(10),'10','10')
  call add('LOGICAL',str(.false.),'F','F')
  call add('LOGICAL',str(.true.),'T','T')
  call add('REAL',str(100.0),'100.000000','100.0000')
  call add('COMPLEX',str((11.0,22.0)),'(11.0000000,22.0000000)','(11.00000,22.00000)')
  call add('COMPOUND',str(10,100.0,"string",(11.0,22.0),.false.), &
       & '10 100.000000 string (11.0000000,22.0000000) F',&
       & '10 100.0000 string (11.00000,22.00000) F')
  write(*,'(*(g0,1x))')tests
  if (allpassed)then
     write(*,'(*(g0,1x))')"*M_framework__msg::str* Passed",size(tests),"tests"
     stop 0
  else
     write(*,'(*(g0,1x))')"*M_framework__msg::str* Failed",count(.not.tests),"Passed",count(tests)
     stop 1
  endif

contains
subroutine add(message,question,answer,answer2)
character(len=*),intent(in)   :: message
character(len=*),intent(in)   :: question
character(len=*),intent(in)   :: answer
character(len=*),intent(in)   :: answer2
logical                       :: passed
  passed=question .eq. answer
  if(.not.passed)then
     passed=question .eq. answer2
  endif
  write(*,'(*(g0,1x))')passed,'expected ', answer, 'got',question
  tests=[tests,passed]
  allpassed=allpassed.and.passed
end subroutine add
end program main

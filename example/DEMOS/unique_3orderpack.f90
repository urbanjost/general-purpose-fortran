     program demo_unique
     ! remove duplicates with remaining elements remaining in initial order
     use M_orderpack, only : unique
     implicit none
     character(len=*),parameter :: list= '(*(g0:,", "))'
     integer :: nuni

     int : block
     integer,allocatable :: INOUTVALS(:)
      INOUTVALS=[44,33,33,33,22,11,33,44,55,33]
      print list,'ORIGINAL:',INOUTVALS
      call unique(INOUTVALS,nuni)
      INOUTVALS=INOUTVALS(:nuni)
      print list,'UNIQUE:',INOUTVALS
     endblock int

     end program demo_unique

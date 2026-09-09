     program demo_ev1ran
     use m_datapac, only : ev1ran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=4000
     real :: x(n)
     integer :: iseed
        call label('ev1ran')
        iseed=12345
        call ev1ran(n,iseed,x)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_ev1ran

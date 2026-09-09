     program demo_expran
     use m_datapac, only : expran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=300
     real :: x(n)
     integer :: iseed
        call label('expran')
        iseed=12345
        call expran(n,iseed,x)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_expran

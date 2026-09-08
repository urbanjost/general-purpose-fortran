     program demo_lgnran
     use m_datapac, only : lgnran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=500
     real :: x(n)
     integer :: iseed
        call label('lgnran')
        iseed=12345
        call lgnran(N,Iseed,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_lgnran

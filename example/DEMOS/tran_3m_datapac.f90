     program demo_tran
     use m_datapac, only : tran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=400
     real :: x(n)
     integer :: iseed
     integer :: nu
        call label('tran')
        nu=3
        iseed=12345
        call tran(N,Nu,Iseed,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_tran

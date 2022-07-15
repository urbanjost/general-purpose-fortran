     program demo_logran
     use m_datapac, only : logran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=4000
     integer           :: iseed
     real              :: x(n)
        call label('logran')
        iseed=12345
        call logran(N,Iseed,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_logran

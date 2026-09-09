     program demo_georan
     use m_datapac, only : georan, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=4000
     real :: x(n)
     integer :: iseed
     real :: P
        call label('georan')
        P=0.2
        iseed=12345
        call georan(N,P,Iseed,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_georan

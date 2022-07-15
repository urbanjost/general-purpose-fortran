     program demo_poiran
     use m_datapac, only : poiran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=500
     real :: x(n)
     integer :: iseed
     real :: alamba
        call label('poiran')
        alamba=2.0
        iseed=12345
        call poiran(N,Alamba,Iseed,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_poiran

     program demo_ev2ran
     use m_datapac, only : ev2ran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=8000
     real :: x(n)
     integer :: iseed
     real :: gamma
        call label('ev2ran')
        gamma=3.4
        iseed=12345
        call ev2ran(N,Gamma,Iseed,X)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_ev2ran

     program demo_lamran
     use m_datapac, only : lamran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=400
     real :: x(n)
     integer :: iseed
     real :: gamma
        call label('lamran')
        gamma=3.4
        iseed=12345
        call lamran(n,gamma,iseed,x)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_lamran

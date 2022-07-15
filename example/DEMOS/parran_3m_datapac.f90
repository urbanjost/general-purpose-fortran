     program demo_parran
     use m_datapac, only : parran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=4000
     real :: x(n)
     integer :: iseed
     real :: gamma
        call label('parran')
        gamma=3.4
        iseed=12345
        call parran(n,gamma,iseed,x)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_parran

     program demo_gamran
     use m_datapac, only : gamran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=4000
     real :: x(n)
     integer :: iseed
     real :: gamma
        call label('gamran')
        gamma=3.4
        iseed=12345
        call gamran(n,gamma,iseed,x)
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_gamran

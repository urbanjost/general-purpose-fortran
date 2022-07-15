     program demo_binran
     use M_datapac, only : binran
     implicit none
     real :: x(40), P
     integer :: N, Npar, Iseed
        Iseed=0
        P=0.88
        N=size(x)
        Npar=11111
        call BINRAN(N,P,Npar,Iseed,X)
        write(*,*)X
     end program demo_binran

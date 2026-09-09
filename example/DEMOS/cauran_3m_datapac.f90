     program demo_cauran
     use m_datapac, only : cauran, plott, label, plotxt, sort
     implicit none
     integer,parameter :: n=100
     real              :: x(n)
     integer           :: iseed
        call label('cauran')
        iseed=12345
        call cauran(n,iseed,x)
        write(*,*)x
        call plotxt(x,n)
        call sort(x,n,x) ! sort to show distribution
        call plotxt(x,n)
     end program demo_cauran

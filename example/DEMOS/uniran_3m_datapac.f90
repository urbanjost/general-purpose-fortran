     program demo_uniran
     use M_datapac, only : uniran, plotxt, sort, label
     implicit none
     integer,parameter :: n=400
     real :: x(n)
     integer :: iseed
        call label('uniran')
        iseed=1234
        call UNIRAN(n,Iseed,X)
        call plotxt(x,n) ! plot random values
        call sort(x,n,x) ! sort values
        call plotxt(x,n) ! should display the f(x)=1 nature
                         ! of the distribution
     end program demo_uniran

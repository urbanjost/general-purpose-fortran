     program demo_hist
     use M_datapac, only : hist
     implicit none
     real,allocatable :: x(:)
     integer :: i
     integer :: n
        x=[(real(i),i=1,100)]
        n=size(x)
        call hist(x,n)
     end program demo_hist

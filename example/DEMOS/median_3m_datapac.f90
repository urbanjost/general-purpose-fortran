     program demo_median
     use M_datapac, only : median, label
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     real,allocatable :: x(:)
     real :: xmed
     integer :: iwrite , n

        call label('median')
        x=[ -10.0, 10.0, 0.0, 1.0, 2.0 ]
        n=size(x)
        call median(x, n, 1, xmed)
        write(*,g)' median of',x,'is',xmed

        x=[ 10.0, 20.0, 3.0, 40.0 ]
        n=size(x)
        call median(x, n, 1, xmed)
        write(*,g)' median of',x,'is',xmed

    end program demo_median

     program demo_copy
     use M_datapac, only : copy
     implicit none
     character(len=*),parameter :: g='(*(g0.3,1x))'
     real,allocatable :: from(:), to(:)
        from=[1.0,2.0,3.0,4.0,5.0]
        to=[-1.0,-1.0,-1.0,-1.0,-1.0,-1.0]
        call copy(from,3,to)
        write(*,g)to
     end program demo_copy

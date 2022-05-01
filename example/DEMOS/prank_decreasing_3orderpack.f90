     program demo_prank_decreasing
     ! create index to lowest N values in input array in decreasing order
     use M_orderpack, only : prank_decreasing
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: INVALS(:)
     integer,allocatable :: irngt(:)
     integer :: nord
     INVALS=[10,5,7,1,4,5,6,8,9,10,1]
     nord=5
     allocate(irngt(nord))
        write(*,g)'ORIGINAL:',INVALS
        call prank_decreasing(INVALS,irngt,nord)
        write(*,g)'NUMBER OF INDICES TO RETURN:',nord
        write(*,g)'RETURNED INDICES:',irngt
        write(*,g)nord,'MAXIMUM VALUES:',INVALS(irngt(:nord))
     end program demo_prank_decreasing

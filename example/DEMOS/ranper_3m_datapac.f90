     program demo_ranper
     use M_datapac, only : ranper
     implicit none
     integer,parameter :: n=10
     integer           :: istart
     real              :: x(n)
     integer           :: i
        do i=1,3
           istart=i
           call  RANPER(N,Istart,X)
           write(*,*)istart
           write(*,'(*(g0.2,1x))')x
        enddo
     end program demo_ranper

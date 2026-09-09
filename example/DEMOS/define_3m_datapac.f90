     program demo_define
     use M_datapac, only : define
     implicit none
     real :: x(4)
        call define(x,size(x),3.33333)
        write(*,'(*(g0.4,1x))')x
     end program demo_define

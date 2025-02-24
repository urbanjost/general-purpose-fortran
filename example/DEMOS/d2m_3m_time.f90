      program demo_d2m
      use M_time, only : d2m, realtime
      implicit none
      integer :: dat(8)
         call date_and_time(values=dat)
         write(*,'(" Today is:",*(i0:,":"))')dat
         write(*,*)'Modified Julian Date is ',d2m(dat)
      end program demo_d2m

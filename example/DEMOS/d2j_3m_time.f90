           program demo_d2j
           use M_time, only : d2j
           implicit none
           integer :: dat(8)
              call date_and_time(values=dat)
              write(*,'(" Today is:",*(i0:,":"))')dat
              write(*,*)'Julian Date is ',d2j(dat)
           end program demo_d2j

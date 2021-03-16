           program demo_d2u
           use M_time, only : d2u
           implicit none
           integer           :: dat(8)
              call date_and_time(values=dat)
              write(*,'(" Today is:",*(i0:,":"))')dat
              write(*,*)'Unix Epoch time is ',d2u(dat)
           end program demo_d2u

           program demo_box_month
           use M_time, only : box_month
           implicit none
           integer           :: dat(8)
           character(len=21) :: calendar(8)
              call date_and_time(values=dat)
              call box_month(dat,calendar)
              write(*,'(a)')calendar
           end program demo_box_month

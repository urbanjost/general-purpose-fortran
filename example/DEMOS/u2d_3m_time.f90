           program demo_u2d
           use M_time, only : u2d, d2u, fmtdate, realtime
           implicit none
           real(kind=realtime) :: today
           integer :: dat(8)
              ! get the date using intrinsic
              call date_and_time(values=dat)
              ! convert today to Julian Date
              today=d2u(dat)
              write(*,*)'Today=',fmtdate(u2d(today))
              ! subtract day
              write(*,*)'Yesterday=',fmtdate(u2d(today-86400.0d0))
              ! add day
              write(*,*)'Tomorrow=',fmtdate(u2d(today+86400.0d0))
           end program demo_u2d

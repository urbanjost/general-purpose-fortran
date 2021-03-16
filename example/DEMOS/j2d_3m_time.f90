           program demo_j2d
           use M_time, only : j2d, d2j, fmtdate, realtime
           implicit none
           real(kind=realtime) :: today
           integer :: dat(8)
              call date_and_time(values=dat) ! get the date using intrinsic
              today=d2j(dat)                  ! convert today to Julian Date
              write(*,*)'Today=',fmtdate(j2d(today))
              ! math is easy with Julian Days and Julian Dates
              write(*,*)'Yesterday=',fmtdate(j2d(today-1.0d0))
              write(*,*)'Tomorrow=',fmtdate(j2d(today+1.0d0))
           end program demo_j2d

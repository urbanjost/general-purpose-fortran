      program demo_m2d
      use M_time, only : m2d, d2m, fmtdate, realtime
      implicit none
      integer,parameter   :: dp=kind(0.0d0)
      real(kind=realtime) :: today
      integer             :: dat(8)
         call date_and_time(values=dat) ! get the date using intrinsic
         today=d2m(dat)                  ! convert today to Julian Date
         write(*,*)'Today=',fmtdate(m2d(today))
         ! math is easy with Julian Days and Julian Dates
         write(*,*)'Yesterday=',fmtdate(m2d(today-1.0_dp))
         write(*,*)'Tomorrow=',fmtdate(m2d(today+1.0_dp))
      end program demo_m2d

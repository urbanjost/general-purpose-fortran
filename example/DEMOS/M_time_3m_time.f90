       program demo_M_time
       use M_time, only: j2d, d2j, u2d, d2u, fmtdate, realtime
       integer :: dat(8)
       real(kind=realtime) :: julian, unixtime
       character(len=*),parameter :: iso_fmt='%Y-%M-%DT%h:%m:%s.%x%z'
       character(len=:),allocatable :: friendly

          friendly='%W, %L %d, %Y %H:%m:%s %N' ! a nice friendly format

          call date_and_time(values=dat)  ! current time is placed in array

          write(*,*)'Today'
          write(*,*)'ISO       ',fmtdate(dat,iso_fmt)
          write(*,*)'Friendly  ',fmtdate(dat,friendly)
          write(*,*)'ISO week  ',fmtdate(dat,'%I')

          julian=d2j(dat)
          unixtime=d2u(dat)

          write(*,*)'Yesterday' ! subtract a day from scalar time and print
          write(*,*)'          ',fmtdate(u2d(unixtime-86400),iso_fmt)
          write(*,*)'          ',fmtdate(j2d(julian-1.0),friendly)
          write(*,*)'          ',fmtdate(j2d(julian-1.0),'%I')

          write(*,*)'Tomorrow'  ! add a day to scalar time and print
          write(*,*)'          ',fmtdate(u2d(unixtime+86400),iso_fmt)
          write(*,*)'          ',fmtdate(j2d(julian+1.0),friendly)
          write(*,*)'          ',fmtdate(j2d(julian+1.0),'%I')

          write(*,*)'Next Week'  ! add a week to scalar time and print
          write(*,*)'          ',fmtdate(u2d(unixtime+7*86400),iso_fmt)
          write(*,*)'          ',fmtdate(j2d(julian+7.0),friendly)
          write(*,*)'          ',fmtdate(j2d(julian+7.0),'%I')

       end program demo_M_time

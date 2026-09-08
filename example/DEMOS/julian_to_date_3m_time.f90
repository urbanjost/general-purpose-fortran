       program demo_julian_to_date
       use M_time, only : julian_to_date, fmtdate, realtime
       implicit none
       integer,parameter   :: dp=kind(0.0d0)
       real(kind=realtime) :: juliandate
       integer             :: dat(8)
       integer             :: ierr
          ! set sample Julian Date
          juliandate=2457589.129_dp
          ! create DAT array for this date
          call julian_to_date(juliandate,dat,ierr)
          write(*,*)'Sample Date=',fmtdate(dat)
          ! go back one day
          call julian_to_date(juliandate-1.0_dp,dat,ierr)
          write(*,*)'Day Before =',fmtdate(dat)
          ! go forward one day
          call julian_to_date(juliandate+1.0_dp,dat,ierr)
          write(*,*)'Day After  =',fmtdate(dat)
       end program demo_julian_to_date

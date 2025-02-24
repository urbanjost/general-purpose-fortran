       program demo_modified_julian_to_date
       use M_time, only : modified_julian_to_date, fmtdate, realtime
       implicit none
       integer,parameter   :: dp=kind(0.0d0)
       real(kind=realtime) :: modified_juliandate, tomorrow, yesterday
       integer             :: dat(8)
       integer             :: ierr
          ! set sample Modified Julian Date
          modified_juliandate=60700.503682349771_dp
          ! create DAT array for this date
          call modified_julian_to_date(modified_juliandate,dat,ierr)
          write(*,*)'Sample Date=',fmtdate(dat)
          !
          ! go back one day
          yesterday= modified_juliandate-1.0
          call modified_julian_to_date(yesterday,dat,ierr)
          write(*,*)'Day Before =',fmtdate(dat)
          !
          ! go forward one day
          tomorrow= modified_juliandate+1
          call modified_julian_to_date(tomorrow,dat,ierr)
          write(*,*)'Day After  =',fmtdate(dat)
          !
       end program demo_modified_julian_to_date

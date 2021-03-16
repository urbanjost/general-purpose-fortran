            program demo_julian_to_date
            use M_time, only : julian_to_date, fmtdate, realtime
            implicit none
            real(kind=realtime)     :: juliandate
            integer                 :: dat(8)
            integer                 :: ierr
               ! set sample Julian Date
               juliandate=2457589.129d0
               ! create DAT array for this date
               call julian_to_date(juliandate,dat,ierr)
               write(*,*)'Sample Date=',fmtdate(dat)
               ! go back one day
               call julian_to_date(juliandate-1.0d0,dat,ierr)
               write(*,*)'Day Before =',fmtdate(dat)
               ! go forward one day
               call julian_to_date(juliandate+1.0d0,dat,ierr)
               write(*,*)'Day After  =',fmtdate(dat)
            end program demo_julian_to_date

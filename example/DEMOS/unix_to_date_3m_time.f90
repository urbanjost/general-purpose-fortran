            program demo_unix_to_date
            use M_time, only : unix_to_date, u2d, fmtdate, realtime
            implicit none
            real(kind=realtime)           :: unixtime
            ! seconds in a day
            real(kind=realtime),parameter :: DAY=86400.0d0
            integer                       :: dat(8)
            integer                       :: ierr
               ! sample Unix Epoch time
               unixtime=1468939038.4639933d0
               ! create DAT array for today
               call unix_to_date(unixtime,dat,ierr)
               write(*,*)'Sample Date=',fmtdate(dat)
               ! go back one day
               call unix_to_date(unixtime-DAY,dat,ierr)
               ! subtract day and print
               write(*,*)'Day Before =',fmtdate(dat)
               ! go forward one day
               call unix_to_date(unixtime+DAY,dat,ierr)
               ! add day print
               write(*,*)'Day After  =',fmtdate(dat)
            end program demo_unix_to_date

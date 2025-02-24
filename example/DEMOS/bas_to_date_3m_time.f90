       program demo_bas_to_date
       use M_time, only : bas_to_date, fmtdate, realtime, BAStime
       implicit none
       integer,parameter          :: dp=kind(0.0d0)
       type(BAStime)              :: bas, tomorrow, yesterday
       integer                    :: dat(8)
       integer                    :: ierr
       character(len=*),parameter :: g='(*(g0,1x))'
          write(*,g)'bas_to_date:'
          ! set sample Baseday and Seconds date
          bas=BAStime( 60700, 0.213682349771_dp)
          ! create DAT array for this date
          call bas_to_date(bas,dat,ierr)
          write(*,g)'Sample Date=',fmtdate(dat)
          !
          write(*,g)'add and subtract days from base_day:'
          ! go back one day
          yesterday= BAStime(bas%base_day-1,bas%secs)
          call bas_to_date(yesterday,dat,ierr)
          write(*,g)'Day Before =',fmtdate(dat)
          !
          ! go forward one day
          tomorrow= BAStime(bas%base_day+1,bas%secs)
          call bas_to_date(tomorrow,dat,ierr)
          write(*,g)'Day After  =',fmtdate(dat)

          write(*,g)'add and subtract seconds from BAS:'
          ! go back one day
          yesterday=bas-86400
          call bas_to_date(yesterday,dat,ierr)
          write(*,g)'Day Before =',fmtdate(dat)
          !
          ! go forward one day
          yesterday=bas+86400
          call bas_to_date(tomorrow,dat,ierr)
          write(*,g)'Day After  =',fmtdate(dat)
          !
       end program demo_bas_to_date

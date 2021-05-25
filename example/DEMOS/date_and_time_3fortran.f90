           program demo_time_and_date
           implicit none
           character(len=8)     :: date
           character(len=10)    :: time
           character(len=5)     :: zone
           integer,dimension(8) :: values
               call date_and_time(date,time,zone,values)
               ! using keyword arguments
               call date_and_time(DATE=date,TIME=time,ZONE=zone)
               call date_and_time(VALUES=values)
               print '(*(g0))','DATE="',date,'" TIME="',time,'" ZONE="',zone,'"'
               write(*,'(i5,a)') &
                & values(1),' - The year', &
                & values(2),' - The month', &
                & values(3),' - The day of the month', &
                & values(4),' - Time difference with UTC in minutes', &
                & values(5),' - The hour of the day', &
                & values(6),' - The minutes of the hour', &
                & values(7),' - The seconds of the minute', &
                & values(8),' - The milliseconds of the second'
           end program demo_time_and_date

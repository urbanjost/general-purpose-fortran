      program demo_date_and_time
         implicit none
         character(len=8)     :: date
         character(len=10)    :: time
         character(len=5)     :: zone
         integer, dimension(8) :: values

         call date_and_time(date, time, zone, values)

         ! using keyword arguments
         call date_and_time(DATE=date, TIME=time, ZONE=zone)
         print '(*(g0))','DATE="',date,'" TIME="',time,'" ZONE="',zone,'"'

         call date_and_time(VALUES=values)
         write (*, '(i5,a)') &
          & values(1), ' - The year', &
          & values(2), ' - The month', &
          & values(3), ' - The day of the month', &
          & values(4), ' - Time difference with UTC in minutes', &
          & values(5), ' - The hour of the day', &
          & values(6), ' - The minutes of the hour', &
          & values(7), ' - The seconds of the minute', &
          & values(8), ' - The milliseconds of the second'

         write (*, '(a)') iso_8601()
      contains
         function iso_8601()
         ! return date using ISO-8601 format at a resolution of seconds
         character(len=8)  :: dt
         character(len=10) :: tm
         character(len=5)  :: zone
         character(len=25) :: iso_8601
         call date_and_time(dt, tm, zone)
            ISO_8601 = dt(1:4)//'-'//dt(5:6)//'-'//dt(7:8) &
                     & //'T'//                             &
                     & tm(1:2)//':'//tm(3:4)//':'//tm(5:6) &
                     & //zone(1:3)//':'//zone(4:5)
         end function iso_8601
      end program demo_date_and_time

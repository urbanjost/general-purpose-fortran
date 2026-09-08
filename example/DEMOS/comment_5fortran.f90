      program demo_comment
      integer :: values(8)
      character(len=:),allocatable :: string
      character(len=1),parameter   :: dash='-',colon=':',dot='.'
      real :: x=3.0, y=4.0
         ! comments may appear on a continued line
         ! blank lines are comment lines
         call date_and_time(values=values)
         associate( &

          ! DATE
          YR=>values(1),      & ! The year
          MO=>values(2),      & ! The month
          DY=>values(3),      & ! The day of the month

          ! TIME
          UTC=>values(4),     & ! Time difference with UTC in minutes
          HR=>values(5),      & ! The hour of the day
          MIN=>values(6),     & ! The minutes of the hour
          SEC=>values(7),     & ! The seconds of the minute
          MILLI=>values(8) )    ! The milliseconds of the second

          write(*,'(*(g0))')YR,dash,MO,dash,DY,'T', &
          & HR,colon,MIN,colon,SEC,dot,MILLI
         end associate

         string='no comment allowed &
            &on the end of a continued string &
            ! keep going ...
            & but comment lines are allowed between ' ! but can go on the end

         ! the next exclamation is part of a literal string, and so has
         ! nothing to do with comments
         print *, 'Hello World! X=',x,'Y=',y

      end program demo_comment

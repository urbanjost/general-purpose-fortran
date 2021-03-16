           program demo_sec2days
           use M_time, only : sec2days
           implicit none
              write(*,*)sec2days(129860)
              write(*,*)sec2days(80000.0d0)
              write(*,*)sec2days(80000.0,crop=.true.)
              write(*,*)sec2days('1 day 2.0hr 100 min 300.0seconds')
           end program demo_sec2days

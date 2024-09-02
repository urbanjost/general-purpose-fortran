      program demo_now
      use M_time, only : now
      implicit none
         write(*,*)now("The current date is &
            &year/month/day hour:minute:second timezone")
         write(*,*)now("The current date is &
            &WEEKDAY at HOUR GOOD, MONTH DAY, year")
         write(*,*)now("The current date is &
            &%w, %l %d, %Y %H:%m:%s %N")
         write(*,*)now("iso")
      end program demo_now

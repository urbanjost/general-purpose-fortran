      program demo_now
      use M_time, only : now, locale
      implicit none
        ! MACROS
         write(*,*)now("The current date is &
            &%w, %l %d, %Y %H:%m:%s %N")
        ! If macros are not found substitute values for KEYWORDS
         write(*,*)now("The current date is &
            &year/month/day hour:minute:second timezone")
         write(*,*)now("The current date is &
            &longweekday at HOUR GOOD, longmonth shortday, year")
        ! including some HIGH-LEVEL KEYWORDS
         write(*,*)now("iso")
        ! and if no keywords are found, ABBREVIATED MACROS
         write(*,*)now("Y-M-D h:m:s")
        ! and basic INTERNATIONALIZATION is available
         call locale('french')
         write(*,*)now("%W, %L %D, %Y %h:%m:%s ")
         call locale('slovak')
         write(*,*)now("%W, %L %D, %Y %h:%m:%s ")
         call locale('spanish')
         write(*,*)now("%W, %L %D, %Y %h:%m:%s ")
      end program demo_now

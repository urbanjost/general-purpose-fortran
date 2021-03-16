           program demo_d2w
           use M_time, only : d2w
           implicit none
           integer           :: dat(8)     ! input date array
           integer           :: iso_year, iso_week, iso_weekday
           character(len=10) :: iso_name
              call date_and_time(values=dat)
              call d2w(dat,iso_year,iso_week,iso_weekday,iso_name)
              write(*,'("ISO-8601 Week:   ",a)')iso_name
              write(*,'(a,i0)')'ISO-8601 year    ',iso_year
              write(*,'(a,i0)')'ISO-8601 week    ',iso_week
              write(*,'(a,i0)')'ISO-8601 weekday ',iso_weekday
           end program demo_d2w

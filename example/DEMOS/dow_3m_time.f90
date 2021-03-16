           program demo_dow
           use M_time, only : dow
           implicit none
           integer          :: dat(8)     ! input date array
           integer          :: weekday
           character(len=9) :: day
           integer          :: ierr
             call date_and_time(values=dat)
             call dow(dat, weekday, day, ierr)
             write(*,'(a,i0)')'weekday=',weekday
             write(*,'(a,a)')'day=',trim(day)
             write(*,'(a,i0)')'ierr=',ierr
           end program demo_dow

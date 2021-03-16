           program demo_easter
           use M_time, only : easter, fmtdate
           implicit none
           integer :: year
           integer :: dat(8) ! year,month,day,tz,hour,minute,second,millisecond
             call date_and_time(values=dat)  ! get current year
             year=dat(1)
             call easter(year, dat)
             write(*,*)fmtdate(dat,&
             "Easter day: the %d day of %L in the year of our Lord %Y")
           end program demo_easter

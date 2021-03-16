            program demo_date_to_unix
            use M_time, only : date_to_unix, realtime
            implicit none
            integer             :: dat(8)
            real(kind=realtime) :: unixtime
            integer             :: ierr
               call date_and_time(values=dat)
               write(*,'(" Today is:",*(i0:,":"))')dat
               call date_to_unix(dat,unixtime,ierr)
               write(*,*)'Unix Epoch time is ',unixtime
               write(*,*)'ierr is ',ierr
            end program demo_date_to_unix

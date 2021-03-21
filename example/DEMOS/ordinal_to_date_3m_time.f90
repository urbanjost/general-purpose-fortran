           program demo_ordinal_to_date
           use M_time, only : ordinal_to_date
           implicit none
           INTEGER            :: yyyy, ddd, mm, dd, yy
           integer            :: dat(8)
           integer            :: ios
             INFINITE: do
                write(*,'(a)',advance='no')&
                & 'Enter year YYYY and ordinal day of year DD '
                read(*,*,iostat=ios)yyyy,ddd
                if(ios.ne.0)exit INFINITE
                ! recover month and day from year and day number.
                call ordinal_to_date(yyyy, ddd, dat)
                yy=dat(1)
                mm=dat(2)
                dd=dat(3)
                write(*,'(*(g0))')'For Year ',yyyy,' and Ordinal day ',ddd,  &
                &         ' Month is ',mm,' and Day of Month is ',dd, &
                &         ' and Year is ',yy
              enddo INFINITE
           end program demo_ordinal_to_date

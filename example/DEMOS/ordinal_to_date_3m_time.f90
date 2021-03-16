           program demo_ordinal_to_date
           use M_time, only : ordinal_to_date
           implicit none
           INTEGER            :: yyyy, ddd, mm, dd
           integer            :: dat(8)
           integer            :: ios
             INFINITE: do
                write(*,'(a)',advance='no')&
                & 'Enter year YYYY and ordinal day of year DD '
                read(*,*,iostat=ios)yyyy,ddd
                if(ios.ne.0)exit INFINITE
                ! recover month and day from year and day number.
                call ordinal_to_date(yyyy, ddd, dat)
                mm=dat(2)
                dd=dat(3)
                write(*,*)'For Year ',yyyy,' and Ordinal day ',ddd, &
                &         ' Month is ',mm,' and Day of Month is',dd
              enddo INFINITE
           end program demo_ordinal_to_date

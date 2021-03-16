           program demo_scan
           implicit none
             write(*,*) scan("fortran", "ao")          ! 2, found 'o'
             write(*,*) scan("fortran", "ao", .true.)  ! 6, found 'a'
             write(*,*) scan("fortran", "c++")         ! 0, found none
           end program demo_scan

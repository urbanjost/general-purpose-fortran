          program demo_index
          implicit none
                                            !1234567890123456789012345678901234567890
          character(len=*),parameter :: str='Search this string for this expression'
             write(*,*)index(str,'this').eq.8,              &
                       index(str,'this',back=.true.).eq.24, &
                       ! INDEX is case-sensitive
                       index(str,'This').eq.0
          end program demo_index

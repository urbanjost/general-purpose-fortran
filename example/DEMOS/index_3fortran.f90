          program demo_index
          implicit none
          character(len=*),parameter :: str=&
          'Search this string for this expression'
          !1234567890123456789012345678901234567890
          write(*,*)&
             index(str,'this').eq.8,              &
             ! return value is counted from the left end even if BACK=.TRUE.
             index(str,'this',back=.true.).eq.24, &
             ! INDEX is case-sensitive
             index(str,'This').eq.0
          end program demo_index

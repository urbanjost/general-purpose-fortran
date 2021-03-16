          program demo_adjustc
          use M_strings, only : adjustc
          !  using length of the input string
             write(*,'(a)')       '================================'
             write(*,'(a)')adjustc('centered string                 ')
             write(*,'(a)')adjustc('                 centered string')
             write(*,'(a)')adjustc('  centered string               ')
          !  using explicit output string length
             write(*,'(a)')repeat('=',50)
             write(*,'(a)')adjustc('this is a centered string',50)
             write(*,'(a)')repeat('=',50)
          end program demo_adjustc

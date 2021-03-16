           program demo_upper_quoted
           use M_strings, only: upper_quoted
           implicit none
           character(len=:),allocatable  :: s
           s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with ""&
              & Quote" everything else'
              write(*,*) 'mixed-case input string is ....',s
              write(*,*) 'upper-case output string is ...',upper_quoted(s)
              write(*,'(1x,a,*(a:,"+"))') 'upper_quoted(3f) is elemental ==>', &
              & upper_quoted(["abc","def","ghi"])
           end program demo_upper_quoted

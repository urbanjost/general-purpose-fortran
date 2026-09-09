      program demo_lower_quoted
      use M_strings, only: lower_quoted
      implicit none
      character(len=:),allocatable  :: s
      s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with ""&
         & Quote" everything else'
         write(*,*) 'mixed-case input string is ....',s
         write(*,*) 'lower-case output string is ...',lower_quoted(s)
         write(*,'(1x,a,*(a:,"+"))') 'lower_quoted(3f) is elemental ==>', &
         & lower_quoted(["abc","def","ghi"])
      end program demo_lower_quoted

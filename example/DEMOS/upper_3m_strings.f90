           program demo_upper
           use M_strings, only: upper
           implicit none
           character(len=:),allocatable  :: s
              s=' ABCDEFG abcdefg '
              write(*,*) 'mixed-case input string is ....',s
              write(*,*) 'upper-case output string is ...',upper(s)
              write(*,*) 'make first character uppercase  ... ',&
              & upper('this is a sentence.',1,1)
              write(*,'(1x,a,*(a:,"+"))') 'UPPER(3f) is elemental ==>',&
              & upper(["abc","def","ghi"])
           end program demo_upper

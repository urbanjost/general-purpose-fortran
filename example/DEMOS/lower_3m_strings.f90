             program demo_lower
             use M_strings, only: lower
             implicit none
             character(len=:),allocatable  :: s
                s=' ABCDEFG abcdefg '
                write(*,*) 'mixed-case input string is ....',s
                write(*,*) 'lower-case output string is ...',lower(s)
             end program demo_lower

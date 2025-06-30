        program demo_toupper
        use M_strings__chars, only: toupper
        implicit none
        character(len=1),allocatable :: s(:)
           s=transfer(' ABCDEFG abcdefg ','A',size=17)
           write(*,*) 'mixed-case input string is ....',s
           write(*,*) 'upper-case output string is ...',toupper(s)
        end program demo_toupper

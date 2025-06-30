        program demo_tolower
        use M_strings__chars, only: tolower
        implicit none
        character(len=1),allocatable  :: s(:)
           s=transfer(' ABCDEFG abcdefg ','A',size=17)
           write(*,*) 'mixed-case input string is ....',s
           write(*,*) 'lower-case output string is ...',tolower(s)
        end program demo_tolower

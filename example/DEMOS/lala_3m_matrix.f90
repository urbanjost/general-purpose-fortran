        program demo_LALA
        use M_matrix, only : lala

           write(*,'(a)')'optionally initialize scratch area size'
           call LALA(20000)

           write(*,'(a)')'do some commands'
           call LALA([character(len=80) :: &
           & 'semi;                         ',&
           & 'a=magic(4),b=-a               ',&
           & 'a+b;a;b                       ',&
           & "display('That is all Folks!') "])

           write(*,'(a)')'do a single command'
           call LALA('who')

           write(*,'(a)')'enter interactive mode'
           call LALA()

           write(*,'(a)')'ending program'
        end program demo_LALA

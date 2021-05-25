             program demo_LAFF
             use M_matrix, only : laff

                write(*,'(a)')'optionally initialize scratch area size'
                call LAFF(20000)

                write(*,'(a)')'do some commands'
                call LAFF([character(len=80) :: &
                & 'semi;                         ',&
                & 'a=magic(4),b=-a               ',&
                & 'a+b;a;b                       ',&
                & 'display("That is all Folks!") '])

                write(*,'(a)')'do a single command'
                call LAFF('who')

                write(*,'(a)')'enter interactive mode'
                call LAFF()

                write(*,'(a)')'ending program'
       end program demo_LAFF

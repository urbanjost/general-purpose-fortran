            program demo_ifin_laff
            use M_matrix, only : ifin_laff
            implicit none
               write(*,*)'eps ',ifin_laff('eps')
               write(*,*)'unknown ',ifin_laff('unknown')
            end program demo_ifin_laff

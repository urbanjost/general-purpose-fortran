       program demo_ifin_lala
       use M_matrix, only : ifin_lala
       implicit none
          write(*,*)'eps ',ifin_lala('eps')
          write(*,*)'unknown ',ifin_lala('unknown')
       end program demo_ifin_lala

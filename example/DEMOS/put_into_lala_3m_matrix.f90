          program demo_put_into_lala
          use M_matrix, only : lala, get_from_lala, put_into_lala
          implicit none
          integer :: ierr

             ! store some data from the program into lala(3f)
             call put_into_lala('A',[1,2,3,4,5,6,7,8,9],ierr)
             call put_into_lala('B',[1.1,2.2,3.3],ierr)
             call put_into_lala('C',"This is my title",ierr)

             ! call lala(3f) and display the values
             call lala([character(len=80) :: &
             & 'who,A,B', &
             & 'display(C);', &
             & '', &
             & ''])

              end program demo_put_into_lala

          program demo_put_into_laff
          use M_matrix, only : laff, get_from_laff, put_into_laff
          implicit none
          integer :: ierr

             ! store some data from the program into laff(3f)
             call put_into_laff('A',[1,2,3,4,5,6,7,8,9],ierr)
             call put_into_laff('B',[1.1,2.2,3.3],ierr)
             call put_into_laff('C',"This is my title",ierr)

             ! call laff(3f) and display the values
             call laff([character(len=80) :: &
             & 'who,A,B', &
             & 'display(C);', &
             & '', &
             & ''])

              end program demo_put_into_laff

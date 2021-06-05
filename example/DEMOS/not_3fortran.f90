           program demo_not
           implicit none
           integer :: i
              i=13741
              write(*,'(b32.32,1x,i0)')i,i
              write(*,'(b32.32,1x,i0)')not(i),not(i)
           end program demo_not

          program demo_magic_square
          use M_math, only : magic_square
          implicit none
          integer           :: arr(15,15)
          integer           :: i, j, k

             do k=1,15
                write(*,*)'K=',k
                call magic_square(arr(:k,:k))
                do i=1,k
                   write(*,'(i2,":",*(i5):)')i,(int(arr(i,j)),j=1,k),sum(arr(k,:k))
                enddo
             enddo
       end program demo_magic_square

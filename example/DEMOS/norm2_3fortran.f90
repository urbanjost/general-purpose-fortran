        program demo_norm2
        implicit none

        real :: x(3,3) = reshape([ &
           1, 2, 3, &
           4, 5, 6, &
           7, 8, 9  &
           ],shape(x),order=[2,1])

          write(*,*) 'x='
          write(*,'(4x,3f4.0)')transpose(x)

          write(*,*) 'norm2(x)=',norm2(x)

          write(*,*) 'x**2='
          write(*,'(4x,3f4.0)')transpose(x**2)
          write(*,*)'sqrt(sum(x**2))=',sqrt(sum(x**2))

        end program demo_norm2

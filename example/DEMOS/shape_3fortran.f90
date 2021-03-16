          program demo_shape
          implicit none
            integer, dimension(-1:1, -1:2) :: a
            write(*,*) shape(a)             ! [ 3, 4 ]
            write(*,*) size(shape(42))      ! [ ]
          end program demo_shape

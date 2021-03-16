          program demo_ior
          implicit none
          integer :: i, j, k
             i=53       ! i=00110101 binary (lowest order byte)
             j=45       ! j=00101101 binary (lowest order byte)
             k=ior(i,j) ! k=00111101 binary (lowest order byte) , k=61 decimal
             write(*,'(i8,1x,b8.8)')i,i,j,j,k,k
          end program demo_ior

          program demo_pointers
             use SomeModule, DoSomething => A
             implicit none

             !Declare variables
             integer, parameter :: m = 3, n = 3
             integer, pointer :: p(:)=>null(), q(:,:)=>null()
             integer, allocatable, target :: A(:,:)
             integer :: istat = 0, i, j
             character(80) :: fmt

          !  Write format string for matrices
          !  (/ A / A, " = [", 3( "[",3(i2, 1x), "]" / 5x), "]" )
             write (fmt, '("(/ A / A, "" = ["", ", i0, "( ""["",", i0, "(i2, 1x), ""]"" / 5x), ""]"" )")') m, n

             allocate(A(m, n), q(m, n), stat = istat)
             if (istat /= 0) stop 'Error during allocation of A and q'

          !  Matrix A is:
          !  A = [[ 1  4  7 ]
          !       [ 2  5  8 ]
          !       [ 3  6  9 ]
          !       ]
             A = reshape([(i, i = 1, size(A))], shape(A))
             q = A

             write(*, fmt) "Matrix A is:", "A", ((A(i, j), j = 1, size(A, 2)), i = 1, size(A, 1))

          !  p will be associated with the first column of A
             p => A(:, 1)

          !  This operation on p has a direct effect on matrix A
             p = p ** 2

          !  This will end the association between p and the first column of A
             nullify(p)

          !  Matrix A becomes:
          !  A = [[ 1  4  7 ]
          !       [ 4  5  8 ]
          !       [ 9  6  9 ]
          !       ]
             write(*, fmt) "Matrix A becomes:", "A", ((A(i, j), j = 1, size(A, 2)), i = 1, size(A, 1))

          !  Perform some array operation
             q = q + A

          !  Matrix q becomes:
          !  q = [[ 2  8 14 ]
          !       [ 6 10 16 ]
          !       [12 12 18 ]
          !       ]
             write(*, fmt) "Matrix q becomes:", "q", ((q(i, j), j = 1, size(A, 2)), i = 1, size(A, 1))

          !  Use p as an ordinary array
             allocate (p(1:m*n), stat = istat)
             if (istat /= 0) stop 'Error during allocation of p'

          !  Perform some array operation
             p = reshape(DoSomething(A + A ** 2), shape(p))

          !  Array operation:
          !      p(1) = 3
          !      p(2) = 21
          !      p(3) = 91
          !      p(4) = 21
          !      p(5) = 31
          !      p(6) = 43
          !      p(7) = 57
          !      p(8) = 73
          !      p(9) = 91
             write(*, '("Array operation:" / (4x,"p(",i0,") = ",i0))') (i, p(i), i = 1, size(p))

             deallocate(A, p, q, stat = istat)
             if (istat /= 0) stop 'Error during deallocation'

          end program demo_pointers

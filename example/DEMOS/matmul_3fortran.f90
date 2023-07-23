      program demo_matmul
      implicit none
      integer :: a(2,3), b(3,2), c(2), d(3), e(2,2), f(3), g(2), v1(4),v2(4)
         a = reshape([1, 2, 3, 4, 5, 6], [2, 3])
         b = reshape([10, 20, 30, 40, 50, 60], [3, 2])
         c = [1, 2]
         d = [1, 2, 3]
         e = matmul(a, b)
         f = matmul(c,a)
         g = matmul(a,d)

         call print_matrix_int('A is ',a)
         call print_matrix_int('B is ',b)
         call print_vector_int('C is ',c)
         call print_vector_int('D is ',d)
         call print_matrix_int('E is matmul(A,B)',e)
         call print_vector_int('F is matmul(C,A)',f)
         call print_vector_int('G is matmul(A,D)',g)

         ! look at argument shapes when one is a vector
         write(*,'(" > shape")')
         ! at least one argument must be of rank two
         ! so for two vectors at least one must be reshaped
         v1=[11,22,33,44]
         v2=[10,20,30,40]

         ! these return a vector C(1:1)
         ! treat A(1:n) as A(1:1,1:n)
         call print_vector_int('Cd is a vector (not a scalar)',&
         & matmul(reshape(v1,[1,size(v1)]),v2))
         ! or treat B(1:m) as B(1:m,1:1)
         call print_vector_int('cD is a vector too',&
         & matmul(v1,reshape(v2,[size(v2),1])))

         ! or treat A(1:n) as A(1:1,1:n) and B(1:m) as B(1:m,1:1)
         ! but note this returns a matrix C(1:1,1:1) not a vector!
         call print_matrix_int('CD is a matrix',matmul(&
         & reshape(v1,[1,size(v1)]), &
         & reshape(v2,[size(v2),1])))

      contains

      ! CONVENIENCE ROUTINES TO PRINT IN ROW-COLUMN ORDER
      subroutine print_vector_int(title,arr)
      character(len=*),intent(in)  :: title
      integer,intent(in)           :: arr(:)
         call print_matrix_int(title,reshape(arr,[1,shape(arr)]))
      end subroutine print_vector_int

      subroutine print_matrix_int(title,arr)
      !@(#) print small 2d integer arrays in row-column format
      character(len=*),parameter :: all='(" > ",*(g0,1x))' ! a handy format
      character(len=*),intent(in)  :: title
      integer,intent(in)           :: arr(:,:)
      integer                      :: i
      character(len=:),allocatable :: biggest

         print all
         print all, trim(title)
         biggest='           '  ! make buffer to write integer into
         ! find how many characters to use for integers
         write(biggest,'(i0)')ceiling(log10(max(1.0,real(maxval(abs(arr))))))+2
         ! use this format to write a row
         biggest='(" > [",*(i'//trim(biggest)//':,","))'
         ! print one row of array at a time
         do i=1,size(arr,dim=1)
            write(*,fmt=biggest,advance='no')arr(i,:)
            write(*,'(" ]")')
         enddo

      end subroutine print_matrix_int

      end program demo_matmul

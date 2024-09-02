      program demo_product
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
      character(len=1),parameter :: nl=new_line('a')

      NO_DIM: block
      !    If DIM is not specified, the result is the product of all the
      !    selected array elements.
      integer :: i,n, p1, p2
      integer,allocatable :: array(:)
         ! all elements are selected by default
         do n=1,10
            print all, 'factorial of ',n,' is ', product([(real(i),i=1,n)])
         enddo

         ! using a mask
         array=[10,12,13,15,20,25,30]
         p1=product(array, mask=mod(array, 2)==1) ! only odd elements
         p2=product(array, mask=mod(array, 2)/=1) ! only even elements
         print all, nl,'product of all elements',product(array) ! all elements
         print all, ' odd * even =',nl,p1,'*',p2,'=',p1*p2

         ! NOTE: If ARRAY is a zero-sized array, the result is equal to one
         print all
         print all, 'zero-sized array=>',product([integer :: ])
         ! NOTE: If nothing in the mask is true, this also results in a null
         !       array
         print all, 'all elements have a false mask=>', &
                  & product(array,mask=.false.)

      endblock NO_DIM

      WITH_DIM: block
      integer :: rect(2,3)
      integer :: box(2,3,4)

      !  lets fill a few arrays
         rect = reshape([ &
           1, 2, 3,       &
           4, 5, 6        &
         ],shape(rect),order=[2,1])
         call print_matrix_int('rect',rect)

      !  Find the product of each column in RECT.
         print all, 'product of columns=',product(rect, dim = 1)

      ! Find the product of each row in RECT.
         print all, 'product of rows=',product(rect, dim = 2)

      ! now lets try a box
         box(:,:,1)=rect
         box(:,:,2)=rect*(+10)
         box(:,:,3)=rect*(-10)
         box(:,:,4)=rect*2
         ! lets look at the values
         call print_matrix_int('box 1',box(:,:,1))
         call print_matrix_int('box 2',box(:,:,2))
         call print_matrix_int('box 3',box(:,:,3))
         call print_matrix_int('box 4',box(:,:,4))

         ! remember without dim= even a box produces a scalar
         print all, 'no dim gives a scalar',product(real(box))

         ! only one plane has negative values, so note all the "1" values
         ! for vectors with no elements
         call print_matrix_int('negative values', &
         & product(box,mask=box < 0,dim=1))

      !   If DIM is specified and ARRAY has rank greater than one, the
      !   result is a new array in which dimension DIM has been eliminated.

         ! pick a dimension to multiply though
         call print_matrix_int('dim=1',product(box,dim=1))

         call print_matrix_int('dim=2',product(box,dim=2))

         call print_matrix_int('dim=3',product(box,dim=3))

      endblock WITH_DIM

      contains

      subroutine print_matrix_int(title,arr)
      implicit none

      !@(#) print small 2d integer arrays in row-column format

      character(len=*),intent(in)  :: title
      integer,intent(in)           :: arr(:,:)
      integer                      :: i
      character(len=:),allocatable :: biggest

         print all
         print all, trim(title),':(',shape(arr),')'  ! print title
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

      end program demo_product

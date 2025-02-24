      program demo_transpose
      implicit none
      integer,allocatable :: array(:,:)
      integer,parameter   :: values(3,5)= reshape([&
          1,  2,  3,  4,  5,    &
         10, 20, 30, 40, 50,    &
         11, 22, 33, 44, -1055  &
       ],shape(values),order=[2,1])

         array=values
         call print_matrix_int('array:',array)
         array=transpose(array)
         call print_matrix_int('array transposed:',array)
         array=transpose(array)
         call print_matrix_int('transposed transpose:',array)

      contains

      subroutine print_matrix_int(title,arr)
      ! print small 2d integer arrays in row-column format
      implicit none
      character(len=*),intent(in)  :: title
      integer,intent(in)           :: arr(:,:)
      integer                      :: i
      character(len=:),allocatable :: biggest
         write(*,'(a," shape(",i0,",",i0,")")')trim(title),shape(arr)  ! print title
         biggest='           '  ! make buffer to write integer into
         ! find how many characters to use for integers
         write(biggest,'(i0)')ceiling(log10(max(1.0,real(maxval(abs(arr))))))+2
         ! use this format to write a row
         biggest='("   [",*(i'//trim(biggest)//':,","))'
         ! print one row of array at a time
         do i=1,size(arr,dim=1)
            write(*,fmt=biggest,advance='no')arr(i,:)
            write(*,'(" ]")')
         enddo
      end subroutine print_matrix_int

      end program demo_transpose

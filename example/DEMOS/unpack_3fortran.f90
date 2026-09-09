      program demo_unpack
      implicit none
      logical,parameter :: T=.true., F=.false.
      integer,parameter :: rows=3, cols=3
      integer           :: i
      logical           :: mask(rows,cols) = reshape([ &
         T, F, F, &
         F, T, F, &
         F, F, T  &
      ],[3,3])
      integer :: field(rows,cols) = reshape([ &
         1, 2, 3, &
         4, 5, 6, &
         7, 8, 9  &
      ],[3,3])
      integer :: result(rows,cols)

        ! mask and field must conform or field must be a scalar
         write(*,*) 'if the logical mask is'
         do i=1,size(mask,dim=1)
            write(*,*)mask(i,:)
         enddo
         write(*,*) 'and field is a scalar (in this case, 0)'
         write(*,*) 'the result is the shape of the mask'
         write(*,*) 'with all values set to the scalar value'
         write(*,*) 'except the true elements of the mask are'
         write(*,*) 'filled in row-column order with values'
         write(*,*) 'from the vector of values [11,22,33]'
         result = unpack( [11,22,33], mask, field=0 )
         call print_matrix_int('result=', result)

         write(*,*) 'if field is an array it must conform'
         write(*,*) 'to the shape of the mask'
         call print_matrix_int('field=',field)
         write(*,*) 'and the combination results in'
         result = unpack( [11,22,33], mask, field )
         call print_matrix_int('result=', result)

      contains

      subroutine print_matrix_int(title,arr)
      ! @(#) convenience routine:
      !      prints small integer arrays in row-column format
      implicit none
      character(len=*),intent(in)  :: title
      integer,intent(in)           :: arr(:,:)
      integer                      :: i
      character(len=:),allocatable :: biggest

         write(*,*)trim(title)
         ! make buffer to write integer into
         biggest='           '
         ! find how many characters to use for integers
         write(biggest,'(i0)')ceiling(log10(max(1.0,real(maxval(abs(arr))))))+2
         ! use this format to write a row
         biggest='("  [",*(i'//trim(biggest)//':,","))'
         ! print one row of array at a time
         do i=1,size(arr,dim=1)
            write(*,fmt=biggest,advance='no')arr(i,:)
            write(*,'(" ]")')
         enddo
      end subroutine print_matrix_int

      end program demo_unpack

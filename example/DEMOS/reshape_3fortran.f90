      program demo_reshape
      implicit none
      ! notice the use of "shape(box)" on the RHS
      integer :: box(3,4)=reshape([1,2,3,4,5,6,7,8,9,10,11,12],shape(box))
      integer,allocatable :: v(:,:)
      integer :: rc(2)
         ! basics0
          ! what is the current shape of the array?
          call printi('shape of box is ',box)
          ! change the shape
          call printi('reshaped ',reshape(box,[2,6]))
          call printi('reshaped ',reshape(box,[4,3]))

         ! fill in row column order using order
          v=reshape([1,2,3,4,10,20,30,40,100,200,300,400],[1,12])
          call printi('here is some data to shape',v)
          call printi('normally fills columns first ',reshape([v],[3,4]))
          call printi('fill rows first', reshape([v],[3,4],order=[2,1]))

          ! if we take the data and put in back in filling
          ! rows first instead of columns, and flipping the
          ! height and width of the box we not only fill in
          ! a vector using row-column order we actually
          ! transpose it.
          rc(2:1:-1)=shape(box)
          ! copy the data in changing column number fastest
          v=reshape(box,rc,order=[2,1])
          call printi('reshaped and reordered',v)
          ! of course we could have just done a transpose
          call printi('transposed',transpose(box))

         ! making the result bigger than source using pad
          v=reshape(box,rc*2,pad=[-1,-2,-3],order=[2,1])
          call printi('bigger and padded and reordered',v)
      contains

      subroutine printi(title,arr)
      implicit none

      !@(#) print small 2d integer arrays in row-column format

      character(len=*),parameter :: all='(*(g0,1x))' ! a handy format
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

      end subroutine printi

      end program demo_reshape

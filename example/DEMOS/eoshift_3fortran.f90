      program demo_eoshift
      implicit none
      integer, dimension(3,3) :: a
      integer :: i

         write(*,*)'original'
         a = reshape( [ 1, 2, 3, 4, 5, 6, 7, 8, 9 ], [ 3, 3 ])
         call printi(a)

         write(*,*)'shift each row differently'
         a = eoshift(a, SHIFT=[1, 2, -2], BOUNDARY=-5, DIM=2)
         call printi(a)

         write(*,*)'shift each column differently'
         a = eoshift(a, SHIFT=[1, 2, -2], BOUNDARY=-5, DIM=1)
         call printi(a)

         write(*,*)'original'
         call printi(reshape([(i,i=1,12)],[3,4]))
         write(*,'(*(g0))')'shift=+2,dim=1'
         call printi(eoshift(reshape([(i,i=1,12)],[3,4]),+2,dim=1))
         write(*,'(*(g0))')'shift=+2,dim=2'
         call printi(eoshift(reshape([(i,i=1,12)],[3,4]),+2,dim=2))
         write(*,'(*(g0))')'shift=-2,dim=1'
         call printi(eoshift(reshape([(i,i=1,12)],[3,4]),-2,dim=1))
         write(*,'(*(g0))')'shift=-2,dim=2'
         call printi(eoshift(reshape([(i,i=1,12)],[3,4]),-2,dim=2))
      contains
      subroutine printi(arr)
      !@(#) print small 2d integer arrays in row-column format
      integer,intent(in) :: arr(:,:)
      integer            :: i
      character(len=40)  :: biggest
         write(biggest,'(*(g0))')'(1x,*(i',                   &
         & ceiling(log10(max(1.0,real(maxval(abs(arr))))))+2, &
         & ':,","))'
         do i=1,size(arr,dim=1)
            write(*,fmt=biggest)arr(i,:)
         enddo
      end subroutine printi

      end program demo_eoshift

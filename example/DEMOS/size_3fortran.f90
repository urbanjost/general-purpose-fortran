      program demo_size
      implicit none
      integer :: arr(0:2,-5:5)
         write(*,*)'SIZE of simple two-dimensional array'
         write(*,*)'SIZE(arr)       :total count of elements:',size(arr)
         write(*,*)'SIZE(arr,DIM=1) :number of rows         :',size(arr,dim=1)
         write(*,*)'SIZE(arr,DIM=2) :number of columns      :',size(arr,dim=2)

         ! pass the same array to a procedure that passes the value two
         ! different ways
         call interfaced(arr,arr)
      contains

      subroutine interfaced(arr1,arr2)
      ! notice the difference in the array specification
      ! for arr1 and arr2.
      integer,intent(in) :: arr1(:,:)
      integer,intent(in) :: arr2(2,*)
         !
         write(*,*)'interfaced assumed-shape array'
         write(*,*)'SIZE(arr1)        :',size(arr1)
         write(*,*)'SIZE(arr1,DIM=1)  :',size(arr1,dim=1)
         write(*,*)'SIZE(arr1,DIM=2)  :',size(arr1,dim=2)

      !  write(*,*)'SIZE(arr2)        :',size(arr2)
         write(*,*)'SIZE(arr2,DIM=1)  :',size(arr2,dim=1)
      !
      ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
      !  write(*,*)'SIZE(arr2,DIM=2)  :',size(arr2,dim=2)

      end subroutine interfaced

      end program demo_size

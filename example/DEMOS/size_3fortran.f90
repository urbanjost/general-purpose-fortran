           program demo_size
           implicit none
           integer :: i, j
           integer :: arr(0:2,-5:5)=reshape([(((i-1)*11+j,i=1,3),j=1,11)],[3,11])
              write(*,*) 'SIZE of simple one-dimensional array=', &
              & size([ 11, 22, 33 ])    ! 3

              write(*,*)'body'
              write(*,*)'SHAPE(arr)       :',shape(arr)
              write(*,*)'SIZE(arr)        :',size(arr)
              write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
              write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
              write(*,*)'note lower bound is not "1"'
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'UBOUND(arr)      :',ubound(arr)
              write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
              write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
              write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
              write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)

              call interfaced(arr,arr)
              call nointerface(arr)
           contains

           subroutine interfaced(arr,arr2)
           integer,intent(in)  :: arr(:,:)
           integer,intent(in)  :: arr2(2,*)
              !
              write(*,*)'interfaced assumed-shape arr2ay'
              !
              ! ‘source’ argument of ‘shape’ intrinsic at (1) must not be
              ! an assumed size array
              !!write(*,*)'SHAPE(arr2)       :',shape(arr2)
              ! The upper bound in the last dimension must appear in the reference
              ! to the assumed size array ‘arr2’ at (1)
              !!write(*,*)'SIZE(arr2)        :',size(arr2)
              write(*,*)'SIZE(arr2,DIM=1)  :',size(arr2,dim=1)
              ! ‘dim’ argument of ‘size’ intrinsic at (1) is not
              !a valid dimension index
              !!write(*,*)'SIZE(arr2,DIM=2)  :',size(arr2,dim=2)
              write(*,*)'note lower bound is "1"'
              write(*,*)'LBOUND(arr2)      :',lbound(arr2)
              write(*,*)'LBOUND(arr2)      :',lbound(arr2)
              ! The upper bound in the last dimension must appear in the
              ! reference to the assumed size array ‘arr2’ at (1)
              !!write(*,*)'UBOUND(arr2)      :',ubound(arr2)
              write(*,*)'LBOUND(arr2,DIM=1):',lbound(arr2,dim=1)
              write(*,*)'UBOUND(arr2,DIM=1):',ubound(arr2,dim=1)
              write(*,*)'LBOUND(arr2,DIM=2):',lbound(arr2,dim=2)
              ! ‘dim’ argument of ‘ubound’ intrinsic at (1) is not
              ! a valid dimension index
              !!write(*,*)'UBOUND(arr2,DIM=2):',ubound(arr2,dim=2)
              !
              write(*,*)'interfaced'
              !
              write(*,*)'SHAPE(arr)       :',shape(arr)
              write(*,*)'SIZE(arr)        :',size(arr)
              write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
              write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
              write(*,*)'note lower bound is "1"'
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'LBOUND(arr)      :',lbound(arr)
              write(*,*)'UBOUND(arr)      :',ubound(arr)
              write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
              write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
              write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
              write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
              !
           end subroutine interfaced
           !!
           ! NOTE: If NOINTERFACE(3f) had an assumed-shape argument with :
           !       for dimensions it could only be properly called with
           !       an explicit interface
           !!
           subroutine nointerface(arr)
           integer,intent(in) :: arr(3,*)
              write(*,*)'nointerface'
            ! SHAPE(3f) CANNOT BE USED ON AN ASSUMED SIZE ARRAY
            !!write(*,*)'SHAPE(arr)       :',shape(arr)
            !!write(*,*)'SIZE(arr)        :',size(arr)
              write(*,*)'SIZE(arr,DIM=1)  :',size(arr,dim=1)
            ! CANNOT DETERMINE SIZE OF ASSUMED SIZE ARRAY LAST DIMENSION
            !!write(*,*)'SIZE(arr,DIM=2)  :',size(arr,dim=2)
              write(*,*)'note lower bound is "1"'
              write(*,*)'LBOUND(arr)      :',lbound(arr)
            !!write(*,*)'UBOUND(arr)      :',ubound(arr)
              write(*,*)'LBOUND(arr,DIM=1):',lbound(arr,dim=1)
              write(*,*)'UBOUND(arr,DIM=1):',ubound(arr,dim=1)
              write(*,*)'LBOUND(arr,DIM=2):',lbound(arr,dim=2)
            !!write(*,*)'UBOUND(arr,DIM=2):',ubound(arr,dim=2)
           end subroutine nointerface
           !!
           end program demo_size

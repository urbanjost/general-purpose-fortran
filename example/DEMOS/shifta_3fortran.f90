      program demo_shifta
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer(kind=int32) :: ival
      integer             :: shift
      integer(kind=int32) :: oval
      integer(kind=int32),allocatable :: ivals(:)
      integer             :: i
      integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])

        ! basic usage
        write(*,*)shifta(100,3)

        ! loop through some interesting values
         shift=5

         ivals=[ -1, -0, +0, +1, &
         & int(b"01010101010101010101010101010101"), &
         & int(b"10101010101010101010101010101010"), &
         & int(b"00000000000000000000000000011111") ]

         ! does your platform distinguish between +0 and -0?
         ! note the original leftmost bit is used to fill in the vacated bits

         write(*,'(/,"SHIFT =  ",i0)') shift
         do i=1,size(ivals)
            ival=ivals(i)
            write(*,'(  "I =      ",b32.32," == ",i0)') ival,ival
            oval=shifta(ival,shift)
            write(*,'(  "RESULT = ",b32.32," == ",i0)') oval,oval
         enddo
         ! elemental
         write(*,*)"characteristics of the result are the same as input"
         write(*,'(*(g0,1x))') &
           & "kind=",kind(shifta(arr,3)), "shape=",shape(shifta(arr,3)), &
           & "size=",size(shifta(arr,3)) !, "rank=",rank(shifta(arr,3))

      end program demo_shifta

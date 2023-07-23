      program demo_shiftl
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer             :: shift
      integer(kind=int32) :: oval
      integer(kind=int32) :: ival
      integer(kind=int32),allocatable :: ivals(:)
      integer             :: i

        print *, ' basic usage'
        ival=100
        write(*,*)ival, shiftl(ival,3)

       ! elemental (input values may be conformant arrays)
        print *, ' elemental'

       ! loop through some ivalues
         shift=9
         ivals=[ &
         & int(b"01010101010101010101010101010101"), &
         & int(b"10101010101010101010101010101010"), &
         & int(b"11111111111111111111111111111111") ]

         write(*,'(/,"SHIFT =  ",i0)') shift
         do i=1,size(ivals)
            ! print initial value as binary and decimal
            write(*,'(  "I =      ",b32.32," == ",i0)') ivals(i),ivals(i)
            ! print shifted value as binary and decimal
            oval=shiftl(ivals(i),shift)
            write(*,'(  "RESULT = ",b32.32," == ",i0)') oval,oval
         enddo

        ! more about elemental
         ELEM : block
         integer(kind=int8)  :: arr(2,2)=reshape([2,4,8,16],[2,2])
         write(*,*)"characteristics of the result are the same as input"
         write(*,'(*(g0,1x))') &
           & "kind=",kind(shiftl(arr,3)), "shape=",shape(shiftl(arr,3)), &
           & "size=",size(shiftl(arr,3)) !, "rank=",rank(shiftl(arr,3))
         endblock ELEM

      end program demo_shiftl

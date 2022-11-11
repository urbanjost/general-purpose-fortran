        program demo_ishftc
        use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
        implicit none
        integer             :: i
        character(len=*),parameter :: g='(b32.32,1x,i0)'

           write(*,*) ishftc(3, 1),' <== typically should have the value 6'

          ! shift a value by various amounts
           do i= -bit_size(0), bit_size(0), 8
              write(*,g) ishftc(huge(0),i), i
           enddo

        end program demo_ishftc

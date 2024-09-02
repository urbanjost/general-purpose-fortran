      program demo_ishftc
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer             :: i
      character(len=*),parameter :: g='(b32.32,1x,i0)'
        ! basics
         write(*,*) ishftc(3, 1),' <== typically should have the value 6'

         print *, 'lets start with this:'
         write(*,'(b32.32)')huge(0)
         print *, 'shift the value by various amounts, negative and positive'
         do i= -bit_size(0), bit_size(0), 8
            write(*,g) ishftc(huge(0),i), i
         enddo
        print *,'elemental'
        i=huge(0)
        write(*,*)ishftc(i,[2,3,4,5])
        write(*,*)ishftc([2**1,2**3,-2**7],3)
        print *,'note the arrays have to conform when elemental'
        write(*,*)ishftc([2**1,2**3,-2**7],[5,20,0])

      end program demo_ishftc

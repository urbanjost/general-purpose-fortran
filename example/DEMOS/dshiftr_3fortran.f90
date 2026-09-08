      program demo_dshiftr
      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      implicit none
      integer(kind=int32) :: i, j
      integer             :: shift

        ! basic usage
         write(*,*) dshiftr (1, 2**30, 2)

        ! print some calls as binary to better visualize the results
         i=-1
         j=0
         shift=5

         ! print values
          write(*,'(*(g0))')'I=',i,' J=',j,' SHIFT=',shift
          write(*,'(b32.32)') i,j, dshiftr (i, j, shift)

        ! visualizing a "combined right shift" ...
         i=int(b"00000000000000000000000000011111")
         j=int(b"11111111111111111111111111100000")
         ! appended together ( i//j )
         ! 0000000000000000000000000001111111111111111111111111111111100000
         ! shifted right SHIFT values dropping off shifted values
         !      00000000000000000000000000011111111111111111111111111111111
         ! keep enough rightmost bits to fill the kind
         !                                 11111111111111111111111111111111
         ! so the result should be all 1s bits ...

          write(*,'(*(g0))')'I=',i,' J=',j,' SHIFT=',shift
          write(*,'(b32.32)') i,j, dshiftr (i, j, shift)

      end program demo_dshiftr

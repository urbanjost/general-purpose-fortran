      program demo_nint
      implicit none
      integer,parameter   :: dp=kind(0.0d0)
      real,allocatable    :: in(:)
      integer,allocatable :: out(:)
      integer             :: i
      real                :: x4
      real(kind=dp)       :: x8

        ! basic use
         x4 = 1.234E0
         x8 = 4.721_dp
         print *, nint(x4), nint(-x4)
         print *, nint(x8), nint(-x8)

        ! elemental
         in = [ -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, -0.4, &
              &  0.0,   &
              & +0.04, +0.5, +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ]
         out = nint(in)
         do i=1,size(in)
            write(*,*)in(i),out(i)
         enddo

        ! dusty corners
         ISSUES: block
         use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
         integer :: icheck
            ! make sure input is in range for the type returned
            write(*,*)'Range limits for typical KINDS:'
            write(*,'(1x,g0,1x,g0)')  &
            & int8,huge(0_int8),   &
            & int16,huge(0_int16), &
            & int32,huge(0_int32), &
            & int64,huge(0_int64)

            ! the standard does not require this to be an error ...
            x8=12345.67e15 ! too big of a number
            icheck=selected_int_kind(ceiling(log10(x8)))
            write(*,*)'Any KIND big enough? ICHECK=',icheck
            print *, 'These are all wrong answers for ',x8
            print *, nint(x8,kind=int8)
            print *, nint(x8,kind=int16)
            print *, nint(x8,kind=int32)
            print *, nint(x8,kind=int64)
         endblock ISSUES

      end program demo_nint

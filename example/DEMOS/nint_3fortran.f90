           program demo_nint
           implicit none
           integer,parameter :: dp=kind(0.0d0)
           real              :: x4 = 1.234E0
           real(kind=dp)     :: x8 = 4.721_dp

           ! basic use
              print *, nint(x4), nint(x8),nint(-x8)

           ! issues
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

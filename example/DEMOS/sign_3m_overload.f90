     program demo_sign
     use, intrinsic :: iso_fortran_env, only : &
     & integer_kinds, int8, int16, int32, int64
     use, intrinsic :: iso_fortran_env, only : &
     & real32, real64, real128
     ! overload can take a single argument
     use M_overload, only: sign, lt, le, eq, ne, gt, ge
     implicit none
     ! basics
     write(*,*) sign(1234), sign(-1234), sign(huge(0.0)), sign(-huge(0.0))
     ! any type of integer or real
     write(*,*) merge('sign works','sign fails', sign(10_int8).eq.1         &
     & .and. sign(-10_int8).eq.-1        )
     write(*,*) merge('sign works','sign fails', sign(10_int16).eq.1        &
     & .and. sign(-10_int16).eq.-1       )
     write(*,*) merge('sign works','sign fails', sign(10_int32).eq.1        &
     & .and. sign(-10_int32).eq.-1       )
     write(*,*) merge('sign works','sign fails', sign(10_int64).eq.1        &
     & .and. sign(-10_int64).eq.-1       )
     write(*,*) merge('sign works','sign fails', sign(10.0_real32).eq.1.0   &
     & .and. sign(-10.0_real32).eq.-1.0  )
     write(*,*) merge('sign works','sign fails', sign(10.0_real64).eq.1.0d0 &
     & .and. sign(-10.0_real64).eq.-1.0d0 )
     !write(*,*) merge('sign works','sign fails', sign(10.0_real128).eq.1.0  &
     !& .and. sign(-10.0_real128).eq.-1.0 )
     !
     !write (*, *) sign(10 < 20)
     !
     !if (sum(sign([1 > 2, 3 == 4, 10 < 5, 100 > 50])) > 2) then
     !   write (*, *) 'two or more are not true'
     !endif
     !
     end program demo_sign

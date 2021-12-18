    program demo_M_overload

     use, intrinsic :: iso_fortran_env, only : &
             & integer_kinds, int8, int16, int32, int64
     use, intrinsic :: iso_fortran_env, only : &
             & real32, real64, real128
     use M_compare_float_numbers, only : operator(.EqualTo.)

     ! allow strings to be converted to integers
     use M_overload, only : int
     ! allow strings to be converted to floating point
     use M_overload, only : real,dble
     ! use == like .eqv.
     use M_overload, only : operator(==)
     ! use /= like .neqv.
     use M_overload, only : operator(/=)
     use M_overload, only : operator(//)
     ! take a single argument
     use M_overload, only : sign
     ! allow strings of different length on merge
     use M_overload, only : merge
     implicit none
     character(len=:),allocatable :: cmd
     character(len=*), parameter :: gen='(*("[",g0,"]":,","))'

       write(*,gen)merge('a','bbbbb',1.eq.1)
       write(*,gen)merge('a','bbbbb',1.eq.2)
       write(*,gen)merge(['a','b'],['bbbbb','ccccc'],1.eq.2)

       if(int('1234')               .eq.1234) &
        & write(*,*)'int("STRING") works '
       if(real('1234.56789')        .EqualTo.1234.56789) &
        & write(*,*)'real("STRING") works '
       if(dble('1234.5678901234567').EqualTo.1234.5678901234567d0) &
        & write(*,*)'dble("STRING") works '

        if (.true. == .true. ) &
        & write(*,*)'== works like .eqv. for LOGICAL values'
       if (.true. /= .false. ) &
        & write(*,*)'/= works like .neqv. for LOGICAL values'

        write(*,*)' The value is '//10//' which is less than '//20.2

       write(*,*) merge('sign works','sign fails',&
              & sign(10_int8).eq.1 &
        & .and. sign(-10_int8).eq.-1 )
       write(*,*) merge('sign works','sign fails',&
              & sign(10_int16).eq.1 &
        & .and. sign(-10_int16).eq.-1 )
       write(*,*) merge('sign works','sign fails',&
              & sign(10_int32).eq.1 &
        & .and. sign(-10_int32).eq.-1 )
       write(*,*) merge('sign works','sign fails',&
              & sign(10_int64).eq.1 &
        & .and. sign(-10_int64).eq.-1 )
       write(*,*) merge('sign works','sign fails',&
              & sign(10.0_real32).eq.1.0 &
        & .and. sign(-10.0_real32).eq.-1.0 )
       write(*,*) merge('sign works','sign fails',&
              & sign(10.0_real64).eq.1.0 &
        & .and. sign(-10.0_real64).eq.-1.0 )
       write(*,*) merge('sign works','sign fails',&
        & sign(10.0_real128).eq.1.0&
        & .and. sign(-10.0_real128).eq.-1.0 )
  contains

    end program demo_M_overload

    program demo_M_overload

     use, intrinsic :: iso_fortran_env, only : &
             & integer_kinds, int8, int16, int32, int64
     use, intrinsic :: iso_fortran_env, only : &
             & real32, real64, real128

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
     ! convert logical expressions to integer
     use M_overload, only : oz, zo, lt, le, eq, ne, gt, ge
     implicit none
     character(len=:),allocatable :: cmd
     character(len=*), parameter :: gen='(*("[",g0,"]":,","))'

       ! merge() with different string lengths expanded to longest
       write(*,gen)merge('a','bbbbb',1.eq.1)
       write(*,gen)merge('a','bbbbb',1.eq.2)
       write(*,gen)merge(['a','b'],['bbbbb','ccccc'],1.eq.2)

       ! int() can take strings representing a number as input'
       if(int('1234') .eq.1234) write(*,*)'int("STRING") works '
       ! as can real() and dble()
       if(abs(real('1234.56789') - 1234.56789).lt.2*epsilon(0.0)) &
        & write(*,*)'real("STRING") works '
       if(abs(dble('1234.5678901234567')- 1234.5678901234567d0).lt.epsilon(0.0d0)) &
        & write(*,*)'dble("STRING") works '

       ! and logical values can be treated numerically
       write(*,*) merge('int works for .FALSE.','int fails for .FALSE.',int(.FALSE.).ne.0)
       write(*,*) merge('int works for .TRUE.','int fails for .TRUE.',int(.TRUE.).eq.0)
       write(*,*) sum(int([.true.,.false.,.true.]))

       ! and == and /= work for logical expressions
       if (.true. == .true. ) &
       & write(*,*)'== works like .eqv. for LOGICAL values'
       if (.true. /= .false. ) &
       & write(*,*)'/= works like .neqv. for LOGICAL values'

       ! // will allow any intrinsic type and convert it to a string
       write(*,*)' The value is '//10//' which is less than '//20.2
       block
       character(len=:),allocatable :: myfmt
       integer :: i
          i=24
          ! build a format with a variable numeric value
          myfmt='("[",I'//i//',"]")'
          write(*,fmt=myfmt)20
       endblock

       ! logical values as numeric values
       write(*,*) sum([int(.false.),int(.false.)])
       write(*,*) int([.false.,.true.,.false.])
       write(*,*) sum(int([.false.,.true.,.false.]))

       ! and sign() assumes the second argument is 1
       write(*,*) merge('sign works','sign fails',&
        & sign(10_int8).eq.1 &
        & .and. sign(-10_int8).eq.-1 )

     contains

     end program demo_M_overload

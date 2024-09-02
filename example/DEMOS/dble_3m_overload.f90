     program demo_dble

     use, intrinsic :: iso_fortran_env, only : &
             & integer_kinds, int8, int16, int32, int64
     use, intrinsic :: iso_fortran_env, only : &
             & real32, real64, real128

     use M_overload, only : int, real, dble
     use M_overload, only : merge
     implicit none
     character(len=*), parameter :: gen='(*("[",g0,"]":,","))'
        ! basics
        ! note returns int64
        write(*,gen) int('1234')*2
        write(*,gen) real('1234.56789')*2
        write(*,gen) dble('1234.5678901234567')*2
        ! tests
        if(int('1234') .eq. 1234 ) &
                & write(*,*)'int("STRING") works '
        if(abs(real('1234.56789') - 1234.56789) .lt. 2*epsilon(0.0)) &
                & write(*,*)'real("STRING") works '
        if(abs(dble('1234.5678901234567') - 1234.5678901234567d0) .lt. &
        & epsilon(0.0d0)) &
                & write(*,*)'dble("STRING") works '
        ! logical arguments work as well
        ! so let us settle this once and for all
        write(*,*)'.true. is',int(.true.)
        write(*,*)'.false. is',int(.false.)
        write(*,*)'LOGICAL ARRAY   ', dble([.false., .true., .false., .true.])
        write(*,*) merge('int works for .FALSE.','int fails for .FALSE.', &
                & int(.FALSE.).ne.0)
        write(*,*) merge('int works for .TRUE.','int fails for .TRUE.', &
                & int(.TRUE.).eq.0)
        ! and also note the argument can be metamorphic
        ! call a function with a metamorphic argument so values can be
        ! any values that represent a numeric value ...
        write(*,*)'METAMORPHIC     ', promote(1,1.0,1.0d0)
        write(*,*)'METAMORPHIC     ', promote('3',(2.0,0.0),.true.)
        write(*,*)'METAMORPHIC     ', promote('3','3','3')
        write(*,*)'METAMORPHIC     ', promote(.true.,.false.,.true.)
        write(*,*)'METAMORPHIC     ', promote((3.0,4.0),0.0,0)

     contains
     function promote(value1,value2,value3)
        class(*),intent(in) :: value1
        class(*),intent(in) :: value2
        class(*),intent(in) :: value3
        doubleprecision,allocatable :: promote
        promote=sum([dble(value1),dble(value2),dble(value3)])
     end function promote

     end program demo_dble

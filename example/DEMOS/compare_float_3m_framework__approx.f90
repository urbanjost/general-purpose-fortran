     program demo_compare_float
     use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
     use,intrinsic :: iso_fortran_env, only : real32, real64, real128
     use,intrinsic :: iso_fortran_env, only : error_unit,output_unit
     use M_framework__approx,          only : compare_float
     use M_framework__approx,          only : &
     & operator(.equalto.), operator(.greaterthan.), operator(.lessthan.)
     implicit none
     integer,parameter       :: wp=int32
     integer                 :: i
     character(len=80),save  :: line='10*0.1'
     real(kind=wp)           :: a(10), x, y, ulp
        write(*,*)'is 10*0.1 == 1.0?'
        ! sum up 0.1 ten times hopefully in a manner compiler does not
        ! optimize it and in the process make it equal
        a=0.1_wp
        read(line,*)a
        x=sum(a)
        y=1.0_wp
        write(*, *)merge('    EQUAL ','NOT EQUAL!',x .eq. y)
        write(*,'(*(g0,1x,z0,1x))')x,x,y,y ! show decimal and hexadecimal value
        write(*, *)'regular',x .eq. y, x .gt. y, x .lt. y ! standard operators
        ! For the default ULP=1.0, the relational operators can be used
        write(*, *)'compare',x .equalto. y, x .greaterthan. y, x .lessthan. y
        do i=0,10
           ulp=real(i,kind=wp)/2.0
           write(*,*) i, compare_float( x, y, ulp=ulp ) ,'ULP=',ulp
        enddo
     end program demo_compare_float

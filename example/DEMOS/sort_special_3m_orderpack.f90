     program demo_sort_special
     ! sort an array using insertion sort
     use,intrinsic :: iso_fortran_env, only : int32, real32, real64
     use M_orderpack, only : sort_special
     implicit none
     ! an insertion sort is very efficient for very small arrays
     ! but generally slower than methods like quick-sort and merge-sort.
     integer,parameter :: isz=2000
     real(kind=real64) :: dd(isz), hi, low
        ! make an array of random values
        call random_seed()
        call random_number(dd)
        dd=dd*1000000.0-500000.0
        low= minval(dd)
        hi = maxval(dd)
        ! sort the data
        call sort_special(dd)
        ! cursory checks
        if(any(dd(1:isz-1) .gt. dd(2:isz)))stop 'ERROR: array not sorted'
        write(*,*)'check min:',dd(1).eq.low
        write(*,*)'check max:',dd(isz).eq.hi
        write(*,*)'PASSED: random array is now sorted'
     end program demo_sort_special

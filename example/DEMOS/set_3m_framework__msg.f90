     program demo_set
     use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
     use,intrinsic :: iso_fortran_env, only : real32, real64, real128
     use M_framework__msg, only : set
     implicit none
     real(kind=real32)    :: a; namelist /all/a
     real(kind=real64)    :: b; namelist /all/b
     real(kind=real128)   :: c; namelist /all/c
     integer(kind=int8)   :: i; namelist /all/i
     integer(kind=int16)  :: j; namelist /all/j
     integer(kind=int32)  :: k; namelist /all/k
     integer(kind=int64)  :: l; namelist /all/l
        call set([1,2,3,4,5,6,7],a,b,c,i,j,k,l)
        write(*,nml=all)
        call set(10,a)
        call set(100,l)
        write(*,nml=all)
     end program demo_set

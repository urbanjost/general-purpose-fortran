     program demo_stderr
     use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
     use,intrinsic :: iso_fortran_env, only : real32, real64, real128
     use,intrinsic :: iso_fortran_env, only : real=> real32, integer=> int32
     use M_framework__msg, only: stderr
     implicit none

     call stderr('A simple message')
     call stderr('error: RVALUE=',3.0/4.0)
     call stderr('error: IVALUE=',123456789)
     call stderr('error: LVALUE=',.true.)

     SEVERAL: block
     integer :: least=10, most=999, ival=-10
     call stderr('error: value',ival, &
             & 'should be between',least,'and',most)
     endblock SEVERAL

     call stderr('real32  :',huge(0.0_real32),0.0_real32, &
             & 12345.6789_real32,tiny(0.0_real32))
     call stderr('real64  :',huge(0.0_real64),0.0_real64, &
             & 12345.6789_real64,tiny(0.0_real64))
     !#ifdef __NVCOMPILER
     !#else
     call stderr('real128 :',huge(0.0_real128),0.0_real128, &
             & 12345.6789_real128,tiny(0.0_real128))
     !#endif
     call stderr('complex :',cmplx(huge(0.0_real),tiny(0.0_real)))

     call stderr('error: program will now stop')
     stop 1

     end program demo_stderr

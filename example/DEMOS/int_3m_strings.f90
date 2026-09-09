       program demo_int
       use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
       use M_strings, only: int
       implicit none
       character(len=*),parameter :: g='(*(g0,1x))'
          write(*,g)int('100'),int('20.4')
          write(*,g)'intrinsic int(3f) still works',int(20,int32)
          write(*,g)'elemental',&
          & int([character(len=23) :: '10','20.3','20.5','20.6'])
       end program demo_int

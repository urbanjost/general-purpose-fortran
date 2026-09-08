       program demo_nint
       use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
       use M_strings, only: nint
       implicit none
       character(len=*),parameter :: g='(*(g0,1x))'
          write(*,g)nint('100'),nint('20.4')
          write(*,g)'intrinsic nint(3f) still works',nint(20.4)
          write(*,g)'elemental',&
          & nint([character(len=23) :: '10','20.3','20.5','20.6'])
       end program demo_nint

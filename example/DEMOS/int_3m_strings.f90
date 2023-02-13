       program demo_int
       use M_strings, only: int
       implicit none
       write(*,*)int('100'),int('20.4')
       write(*,*)'int still works',int(20.4)
       write(*,*)'elemental',&
       & int([character(len=23) :: '10','20.3','20.5','20.6'])
       end program demo_int

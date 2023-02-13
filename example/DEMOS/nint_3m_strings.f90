       program demo_nint
       use M_strings, only: nint
       implicit none
       write(*,*)nint('100'),nint('20.4')
       write(*,*)'nint still works',nint(20.4)
       write(*,*)'elemental',&
       & nint([character(len=23) :: '10','20.3','20.5','20.6'])
       end program demo_nint

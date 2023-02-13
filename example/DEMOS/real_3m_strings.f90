       program demo_real
       use M_strings, only: real
       implicit none
       write(*,*)real('100'),real('20.4')
       write(*,*)'real still works',real(20)
       write(*,*)'elemental',&
       & real([character(len=23) :: '10','20.3','20.5','20.6'])
       end program demo_real

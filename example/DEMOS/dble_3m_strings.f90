       program demo_dble
       use M_strings, only: dble
       implicit none
       write(*,*)dble('100'),dble('20.4')
       write(*,*)'dble still works',dble(20),dble(20.4)
       write(*,*)'elemental',&
       & dble([character(len=23) :: '10','20.3','20.5','20.6'])
       end program demo_dble

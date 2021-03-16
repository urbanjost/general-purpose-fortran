           program demo_snum0
           use m_calculator, only: rnum0, snum0
           character(len=80)  :: ic,jc,kc

           rdum=rnum0('A=83/2') ! set a variable in the calculator
           kc=snum0('$MYTITLE="This is my title variable"')

           ic=snum0('$STR("VALUE IS [",A,"]")')
           jc=snum0('$MYTITLE')

           write(*,*)'IC=',trim(ic)
           write(*,*)'JC=',trim(jc)
           write(*,*)'KC=',trim(kc)

           end program demo_snum0

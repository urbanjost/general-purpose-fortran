          program demo_getvalue
          use M_calculator, only : rnum0
          use M_calculator, only: getvalue
          value1=rnum0('A=100/2') ! store something into calculator
          write(*,*)value1,getvalue('A')
          end program demo_getvalue

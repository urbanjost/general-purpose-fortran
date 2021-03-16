          program demo_rgetvalue
          use M_calculator, only : rnum0
          use M_calculator, only: rgetvalue
          value1=rnum0('A=100/2') ! store something into calculator
          write(*,*)value1,rgetvalue('A')
          end program demo_rgetvalue

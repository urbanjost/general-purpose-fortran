          program demo_stuffa
          use M_calculator, only : stuffa
          use M_calculator, only : snum0
          implicit none
             call stuffa('$A','')
             call stuffa('$mystring','this is the value of the string')
             write(*,*)snum0('$mystring')
             call stuffa('$mystring','this is the new value of the string')
             write(*,*)snum0('$mystring')
          end program demo_stuffa

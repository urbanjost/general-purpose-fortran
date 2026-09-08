     program demo_getvalue
     use M_calculator, only : rnum0
     use M_calculator, only: getvalue
     implicit none
     real :: value1
        value1=rnum0('A=100/2') ! store something into calculator
        write(*,*)value1,getvalue('A')
     end program demo_getvalue

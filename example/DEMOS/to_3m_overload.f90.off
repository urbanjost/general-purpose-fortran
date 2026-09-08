     program demo_to
     use M_overload, only: to, operator(.to.)
     implicit none
     character(len=*),parameter :: gen='(*(g0,1x))'
        print gen, [11.to.16]
        print gen, 2.5 * [1.to.4]
        print gen, 2.5 * to(1,4)+10
     end program demo_to

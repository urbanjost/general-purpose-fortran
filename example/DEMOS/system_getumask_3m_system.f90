     program demo_getumask
     use M_system, only : system_getumask, system_setumask
     integer :: i
     write(*,101)(system_getumask(),i=1,4)
     101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
     end program demo_getumask

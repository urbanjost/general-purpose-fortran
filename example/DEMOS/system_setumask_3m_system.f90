     program demo_setumask
     use M_system, only : system_getumask, system_setumask
     integer :: newmask
     integer :: i
     integer :: old_umask
     write(*,101)(system_getumask(),i=1,4)
     101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
     newmask=63
     old_umask=system_setumask(newmask)
     write(*,*)'NEW'
     write(*,101)(system_getumask(),i=1,4)
     end program demo_setumask

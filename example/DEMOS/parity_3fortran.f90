      program demo_parity
      implicit none
      logical, parameter :: T=.true., F=.false.
      logical :: x(3,4)
        ! basics
         print *, parity([T,F])
         print *, parity([T,F,F])
         print *, parity([T,F,F,T])
         print *, parity([T,F,F,T,T])
         x(1,:)=[T,T,T,T]
         x(2,:)=[T,T,T,T]
         x(3,:)=[T,T,T,T]
         print *, parity(x)
         print *, parity(x,dim=1)
         print *, parity(x,dim=2)
      end program demo_parity

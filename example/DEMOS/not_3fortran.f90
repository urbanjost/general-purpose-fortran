      program demo_not
      implicit none
      integer :: i
        ! basics
         i=-13741
         print *,'the input value',i,'represented in bits is'
         write(*,'(1x,b32.32,1x,i0)') i, i
         i=not(i)
         print *,'on output it is',i
         write(*,'(1x,b32.32,1x,i0)') i, i
         print *, " on a two's complement machine flip the bits and add 1"
         print *, " to get the value with the sign changed, for example."
         print *, 1234, not(1234)+1
         print *, -1234, not(-1234)+1
         print *, " of course 'x=-x' works just fine and more generally."
      end program demo_not

        program demo_not
        implicit none
        integer :: i
          ! basics
           i=13741
           print *,'the input value',i,'represented in bits is'
           write(*,'(1x,b32.32,1x,i0)') i, i
           i=not(i)
           print *,'on output it is',i
           write(*,'(1x,b32.32,1x,i0)') i, i
        end program demo_not

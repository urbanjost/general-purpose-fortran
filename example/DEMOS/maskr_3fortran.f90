        program demo_maskr
        implicit none
        integer :: i
          ! basics
           i=maskr(1)
           write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-1)
           i=maskr(5)
           write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-5)
           i=maskr(11)
           write(*,'(i0,1x,b0,1x,b0/)') i,i, shiftl(7,bit_size(0)-11)
          ! elemental
           write(*,'(*(i11,1x,b0.32,1x,/))') maskr([(i,i,i=0,bit_size(0),4)])
        end program demo_maskr

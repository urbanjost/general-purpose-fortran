      program demo_maskl
      implicit none
      integer :: i
        ! basics
         i=3
         write(*,'(i0,1x,b0)') i, maskl(i)

        ! elemental
         write(*,'(*(i11,1x,b0.32,1x,/))') maskl([(i,i,i=0,bit_size(0),4)])
      end program demo_maskl

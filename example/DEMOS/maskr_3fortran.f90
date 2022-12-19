      program demo_maskr
      implicit none
      integer :: i

        ! basics
         print *,'basics'
         write(*,'(i0,t5,b32.32)') 1, maskr(1)
         write(*,'(i0,t5,b32.32)') 5,  maskr(5)
         write(*,'(i0,t5,b32.32)') 11, maskr(11)
         print *,"should be equivalent on two's-complement processors"
         write(*,'(i0,t5,b32.32)') 1,  shiftr(-1,bit_size(0)-1)
         write(*,'(i0,t5,b32.32)') 5,  shiftr(-1,bit_size(0)-5)
         write(*,'(i0,t5,b32.32)') 11, shiftr(-1,bit_size(0)-11)

        ! elemental
         print *,'elemental '
         print *,'(array argument accepted like called with each element)'
         write(*,'(*(i11,1x,b0.32,1x,/))') maskr([(i,i,i=0,bit_size(0),4)])

      end program demo_maskr

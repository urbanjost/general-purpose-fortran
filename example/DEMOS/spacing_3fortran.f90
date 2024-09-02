      program demo_spacing
      implicit none
      integer, parameter :: sgl = selected_real_kind(p=6, r=37)
      integer, parameter :: dbl = selected_real_kind(p=13, r=200)

         write(*,*) spacing(1.0_sgl)
         write(*,*) nearest(1.0_sgl,+1.0),nearest(1.0_sgl,+1.0)-1.0

         write(*,*) spacing(1.0_dbl)
      end program demo_spacing

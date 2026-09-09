      program demo_mo2v
      use M_time, only : mo2v
      implicit none
         write(*,*)mo2v("April")
         write(*,*)mo2v('Apr')
         write(*,*)mo2v('sexember')
         write(*,*)mo2v('unknown')  ! returns -1
      end program demo_mo2v

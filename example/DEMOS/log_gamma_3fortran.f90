      program demo_log_gamma
      implicit none
      real :: x = 1.0
         write(*,*)x,log_gamma(x) ! returns 0.0
         write(*,*)x,log_gamma(3.0) ! returns 0.693 (approximately)
      end program demo_log_gamma

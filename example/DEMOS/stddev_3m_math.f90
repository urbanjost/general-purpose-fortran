      program demo_stddev
      use M_math, only : stddev
      implicit none
      integer :: i
      real,parameter :: vals(*)=[(i*1.0,i=0,100)]
         !*!write(*,*)vals
         write(*,*)size(vals)
         write(*,*)sum(vals)/size(vals)
         write(*,*)stddev(vals,size(vals),sum(vals)/size(vals))
      end program demo_stddev

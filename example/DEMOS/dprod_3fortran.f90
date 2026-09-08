      program demo_dprod
      implicit none
      integer,parameter :: dp=kind(0.0d0)
      real :: x = 5.2
      real :: y = 2.3
      doubleprecision :: xx
      real(kind=dp)   :: dd

         print *,'algebraically 5.2 x 2.3 is exactly 11.96'
         print *,'as floating point values results may differ slightly:'
         ! basic usage
         dd = dprod(x,y)
         print *, 'compare dprod(xy)=',dd, &
         & 'to x*y=',x*y, &
         & 'to dble(x)*dble(y)=',dble(x)*dble(y)

         print *,'test if an expected result is produced'
         xx=-6.0d0
         write(*,*)DPROD(-3.0, 2.0),xx
         write(*,*)merge('PASSED','FAILED',DPROD(-3.0, 2.0) == xx)

         print *,'elemental'
         print *, dprod( [2.3,3.4,4.5], 10.0 )
         print *, dprod( [2.3,3.4,4.5], [9.8,7.6,5.4] )

      end program demo_dprod

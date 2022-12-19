      program demo_cos
      implicit none
      character(len=*),parameter :: g2='(a,t20,g0)'
      doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
         write(*,g2)'COS(0.0)=',cos(0.0)
         write(*,g2)'COS(PI)=',cos(PI)
         write(*,g2)'COS(PI/2.0d0)=',cos(PI/2.0d0),'EPSILON=',epsilon(PI)
         write(*,g2)'COS(2*PI)=',cos(2*PI)
         write(*,g2)'COS(-2*PI)=',cos(-2*PI)
         write(*,g2)'COS(-2000*PI)=',cos(-2000*PI)
         write(*,g2)'COS(3000*PI)=',cos(3000*PI)
      end program demo_cos

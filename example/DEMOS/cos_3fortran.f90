          program demo_cos
          implicit none
          doubleprecision,parameter :: PI=atan(1.0d0)*4.0d0
          write(*,*)'COS(0.0)=',cos(0.0)
          write(*,*)'COS(PI)=',cos(PI)
          write(*,*)'COS(PI/2.0d0)=',cos(PI/2.0d0),' EPSILON=',epsilon(PI)
          write(*,*)'COS(2*PI)=',cos(2*PI)
          write(*,*)'COS(-2*PI)=',cos(-2*PI)
          write(*,*)'COS(-2000*PI)=',cos(-2000*PI)
          write(*,*)'COS(3000*PI)=',cos(3000*PI)
          end program demo_cos

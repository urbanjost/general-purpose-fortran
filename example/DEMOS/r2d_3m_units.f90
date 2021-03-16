          program demo_r2d
          use M_units, only :  r2d
          use M_units, only : pi8=>pi
          implicit none
          real :: pi=real(pi8)
             write(*,*)'With REAL array input    ', r2d([ 0.0, PI/4.0, PI/2.0, 3.0*PI/2.0, PI ])
             write(*,*)'With INTEGER array input ', r2d([0,1,2,3,4])
             write(*,*)'With DOUBLEPRECISION     ', r2d(0.0d0),r2d(PI/4.0d0),r2d(PI/2.0d0),r2d(3.0d0*PI/2.0d0),r2d(PI)
          end program demo_r2d

          program demo_disp_get
          real :: xx(2,3), yy(2,3)
             xx(1,:)=[ 1.0, 6.0, 5.0  ]
             xx(2,:)=[ 2.4, 4.0, 6.0  ]
             yy(1,:)=[ 0.0, 3.5, 2.0  ]
             yy(2,:)=[ 7.0, 4.0, 8.22 ]
             call disp_xy(xx,yy)
          contains

          subroutine disp_xy(x,y)
          use M_display
          real x(:,:), y(:,:)
          type(disp_settings) ds
             ds = disp_get()
             call disp_set(digmax=4, sep=',')
             call disp('x=',x)
             write(*,*)
             call disp('y=',y)
             call disp_set(ds)
          end subroutine disp_xy

          end program demo_disp_get

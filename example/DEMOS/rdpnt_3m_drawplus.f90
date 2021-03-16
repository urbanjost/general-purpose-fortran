          program demo_rdpnt
          use M_drawplus, only : rdpnt
          use M_draw
          implicit none
          real    :: x1, y1, x2, y2
          integer :: key
          call vinit(' ')
          x1=0.0
          y1=0.0
           write(*,*)'click at the same point twice to end the loop'
          do
             call rdpnt(x1,y1,x2,y2,key)
             if(key.le.0)exit
             ! if the mouse is clicked twice without moving exit the loop
             if(x1.eq.x2 .and. y1.eq.y2)exit
             write(*,*)'points are ',x1,y1,' and ',x2,y2,'; and key is ',key
             call color(D_RED)
             call circle(x1,y1,0.03)
             call color(D_GREEN)
             call circle(x2,y2,0.05)
             x1=x2
             y1=y2
          enddo
          call vexit()
          end program demo_rdpnt

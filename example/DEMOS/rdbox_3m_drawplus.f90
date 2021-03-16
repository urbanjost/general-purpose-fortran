            program demo_rdbox
            use M_drawplus, only : rdbox
            use M_draw
            implicit none
            real    :: x1, y1, x2, y2
            integer :: key
            call vinit(' ')
            call color(D_GREEN)
            do
               call rdbox(x1,y1,x2,y2,key)
               if(key.le.0)exit
               ! if the mouse is clicked twice without moving exit the loop
               if(x1.eq.x2 .and. y1.eq.y2)exit
               write(*,*)'corners are ',x1,y1,' and ',x2,y2,'; and key is ',key
               call move2(x1,y1)
               call draw2(x2,y2)
               call move2(x1,y2)
               call draw2(x2,y1)
            enddo
            call vexit()
            end program demo_rdbox

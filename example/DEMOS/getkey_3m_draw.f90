            program demo_getkey
            use :: M_draw
            !! set up drawing environment
            call prefsize(600,600)
            call voutput('+')
            call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
            call ortho2(-300.0,300.0,-300.0,300.0)
            call textsize(500.0,500.0)
            call linewidth(130)
            call centertext(.true.)
            call color(D_BLACK)
            call clear()
            write(*,*)'press any character to see it displayed in the default font'
            write(*,*)'Enter "q" to quit'
            do
              ichar=getkey()
              if(ichar.lt.0)then
                 write(*,*)'this device does not support getkey'
                 exit
              elseif(ichar.ne.0)then
                 call color(D_BLACK)
                 call clear()
                 call color(D_BLUE)
                 call move2(0.0,0.0)
                 call drawstr(char(ichar))
              endif
              if(char(ichar).eq.'q')then
                 write(*,*)'press any key to exit'
                 ichar=getkey()
                 exit
              endif
            enddo
            call vexit()
            end program demo_getkey

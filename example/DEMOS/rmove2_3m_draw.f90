            program demo_rmove2
            use M_draw, only: prefsize, vinit, ortho2, clear, getkey
            use M_draw, only: move2, rmove2, rdraw2, vexit
            use M_draw, only: linewidth
            call prefsize(500,500)
            call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
            call ortho2(-110.0,110.0,-110.0,110.0)
            call move2(-100.0,-100.0)
            call linewidth(70)
            do i=1,20
               call rmove2(10.0, 0.0)
               call rdraw2( 0.0,10.0)
            enddo
            ipaws=getkey()
            call vexit()
            end program demo_rmove2

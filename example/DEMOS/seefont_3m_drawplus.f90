          program demo_seefont
          use M_draw
          use M_drawplus, only : seefont
          implicit none
          character(len=128) :: fontname
          integer            :: iwidth
             call prefsize(1000,800)
             call vinit(' ')
             call linewidth(20)
             call seefont(' ')
             call vexit()
          end program demo_seefont

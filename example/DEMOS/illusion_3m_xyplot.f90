          program demo_illusion
          use M_draw,      only : vinit, color, clear, backbuffer, getkey, vexit, page
          use M_xyplot,    only : illusion
          call vinit(' ')
          call page(0.0,4800.0,0.0,4800.0)
          call color(6)
          call clear()
          call color(5)
          idum=backbuffer()
          call illusion('TOP','BOTTOM','LEFT','RIGHT')
          idum=getkey()
          call vexit()
          end program demo_illusion

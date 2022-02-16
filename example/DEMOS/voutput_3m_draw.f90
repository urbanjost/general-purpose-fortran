     program demo_voutput
     use M_draw
     implicit none
     ! want a 400x400 raster output
     call prefsize(400,400)
     ! convert PPM to a GIF file using ppmtogif(1)
     call voutput('|ppmtogif >voutput.3m_draw.gif')
     ! start graphics using PPM device
     call vinit('p6')
     ! draw a filled circle
     call color(D_RED)
     call polyfill(.true.)
     call circle(0.0,0.0,1.0)
     !
     call vexit()
     end program demo_voutput

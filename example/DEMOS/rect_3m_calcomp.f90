     program demo_rect
     use M_calcomp, only : plots, plot, newpen, rect
     use M_calcomp, only : END,MOVE
     implicit none
     real  :: xmax,ymax
     real  :: xstart, ystart ! lower left corner before rotation
     real  :: height, wdth
     real  :: angle

        xmax=8.5
        ymax=7.0
        xstart=2.5
        ystart=1.0
        height=3.0
        wdth=5.0

        call plots(0.0,xmax,0.0,ymax)
        ! (make a small dot at xstart,ystart>
        call rect(xstart,ystart,0.04,0.04,45.0,MOVE)
        ! rectangle
        call newpen(1)
        angle=0.0
        call rect(xstart,ystart,height,wdth,angle,MOVE)
        ! rotated rectangle
        angle=45.0
        call newpen(2)
        call rect(xstart,ystart,height,wdth,angle,MOVE)
        ! end graphics
        call plot(0.0,0.0,END)
     end program demo_rect

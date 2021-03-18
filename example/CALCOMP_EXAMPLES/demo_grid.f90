          program demo_grid
          use M_calcomp, only : plots, plot, newpen, grid
          use M_calcomp, only : END
          implicit none
          real              :: xmax=8.5,ymax=11.0
          real              :: step
             call plots(0.0,xmax,0.0,ymax)  ! make a 8 1/2 x 11 inch page
             call newpen(1)                 ! red
             step=0.25                      ! make 1/4 inch grid
             call grid(0.0,0.0,step,step,int(xmax/step)+1,int(ymax/step)+1)
             call newpen(2)                 ! green
             step=0.50                      ! make 1/2 inch grid
             call grid(0.0,0.0,step,step,int(xmax/step)+1,int(ymax/step)+1)
             call plot(0.0,0.0,END)         ! end graphics
          end program demo_grid

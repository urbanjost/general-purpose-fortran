          program demo_poly
          use M_calcomp, only : plots, poly, plot
          implicit none
          real              :: xstart, ystart
          real              :: side_length
          real              :: number_of_sides
          real              :: angle
          integer           :: i
             call plots(0.0,10.0,0.0,10.0)
             call plot(0.001,0.001,-3) ! move origin a bit so lines on edge OK
             call poly(0.0,0.0,10.0,4.0,0.0) ! 10 inch square
             side_length=2.35
             xstart=(10.0-side_length)/2.0
             ystart=0.5
             angle=0.0
             do i = 3,12
                number_of_sides=real(i)
                call poly(xstart,ystart,side_length,number_of_sides,angle)
             enddo
             call plot(0.0,0.0,999)
          end program demo_poly

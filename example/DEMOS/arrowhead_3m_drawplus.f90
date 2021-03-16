          program demo_arrowhead
          use :: M_draw
          use :: M_drawplus, only : arrowhead
          implicit none
          real :: xpoint,ypoint, xstart,ystart
          real :: PI, ang, size
          integer :: i, idraw, idum
          call prefsize(600,600)
          call vinit('')
          call ortho2(-10.0,10.0,-10.0,10.0)
          call linewidth(100)
          call color(D_WHITE)
          call clear()
          xstart=0.0
          ystart=0.0
          size=1.00
          idraw=0

          call color(D_RED)
          xpoint=9.0
          ypoint=0.0
          call arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)

          call color(D_GREEN)
          xpoint=9.0
          ypoint=9.0
          call arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)

          call color(D_BLACK)
          idraw=-1
          PI=4.0*atan(1.0)
          ang=PI/2.0
          do i=1,20+1
             xpoint=9.0*cos(ang)
             ypoint=9.0*sin(ang)
             call arrowhead(xpoint,ypoint,xstart,ystart,size,idraw)
             ang=ang+PI/20.0
          enddo
          idum=getkey()
          call vexit()
          end program demo_arrowhead

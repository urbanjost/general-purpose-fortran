     program demo_polyarea_shoelace
     use M_math, only : polyarea_shoelace
     implicit none
     !                          A  B      C    D      E    F
     real,allocatable :: x(:)
     real,allocatable :: y(:)

     x=[ 0.0, 10.0,  0.0, 10.0,  0.0,  0.0]   !*! hourglass crosses itself. unexpected value
     y=[10.0, 10.0,  0.0,  0.0, 10.0, 10.0]
     write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)

     x=[ 0.0, 10.0,  0.0,  0.0, 10.0, 0.0,  0.0] !*! crosses itself. maybe not what you expect
     y=[10.0, 10.0,  0.0, 10.0,  0.0, 0.0, 10.0]
     write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)

     x=[ 0.0,  0.0, 10.0, 10.0,  0.0 ]     ! square clockwise

     y=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]
     write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)

     x=[ 0.0, 0.0, 10.0,  10.0,  0.0 ]     ! square counterclockwise
     y=[10.0, 0.0,  0.0,  10.0, 10.0 ]
     write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)

     end program demo_polyarea_shoelace

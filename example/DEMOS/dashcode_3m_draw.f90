         program demo_dashcode
         use M_draw
         implicit none
         integer        :: icolor
         integer        :: ikey
         real,parameter :: b=0.5
         real           :: x, y
         real           :: dcode
            call prefsize(1000,200)
            call vinit(' ')
            call color(D_BLACK)
            call clear()
            call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)
            call textsize(2.0, 2.4)
            call font("futura.m")
            call ycentertext()
            icolor=7; dcode=0.010/5.0; X=-23.0; Y=3.5  ; call line()
            icolor=1; dcode=0.025/5.0; X=-23.0; Y=-0.0 ; call line()
            icolor=2; dcode=0.030/5.0; X=-23.0; Y=-3.5 ; call line()
            icolor=4; dcode=0.050/5.0; X=0.0;   Y=3.5  ; call line()
            icolor=5; dcode=0.075/5.0; X=0.0;   Y=-0.0 ; call line()
            icolor=6; dcode=0.100/5.0; X=0.0;   Y=-3.5 ; call line()
            call linewidth(200)
            call color(D_BLUE)
            call move2(-25.0, -5.0)
            call draw2(-25.0,  5.0)
            call draw2(25.0,   5.0)
            call draw2(25.0,  -5.0)
            call draw2(-25.0, -5.0)
            ikey=getkey()
            call vexit()
         contains
         subroutine line
         character(len=6) :: string
            call linestyle('11100100')
            call dashcode(dcode)
            call color(icolor)
            call linewidth(70)
            call move2(X,Y)
            call rdraw2(10.0, 0.0)
            call rmove2(3.0, 0.0)
            call linestyle('')
            call linewidth(180)
            call color(D_WHITE)
            write(string,'(f6.3)')dcode
            call drawstr (string)
         end subroutine line
         end program demo_dashcode

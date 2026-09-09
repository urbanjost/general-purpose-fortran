     program demo_polyhatch
     use M_draw
     use M_draw, only: D_BLACK, D_WHITE
     use M_draw, only: D_RED, D_GREEN, D_BLUE
     use M_draw, only: D_YELLOW, D_MAGENTA, D_CYAN
     implicit none
     integer :: key
     real :: N = 11.0
        call prefsize(600*10/6, 200*10/6)
        call vinit(' ')
        call page(-15.0, 15.0, -5.0, 5.0)
        call linewidth(100)
        call color(D_BLACK)
        call clear()
        call color(D_RED)
        call spirograph(-10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 0)
        call polyhatch(.true.) ! turn on polygon hatching
        call hatchang(45.0)
        call hatchpitch(0.3)
        call color(D_GREEN)
        call spirograph(10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 2)
        call vflush()
        key = getkey()
        call vexit()
     contains
     subroutine spirograph(xc,yc,sun,planet0,offset0,rad,ilines,ang,angs,ifill)
     real, parameter :: PI=3.14159265358979323846264338327950288419716939937510
     ! center of curve
     real, intent(in)    :: xc, yc
     ! radii of sun, planet, and planet offset
     real, intent(in)    :: sun, planet0, offset0
     ! radius to fit the shape to (no fit if radius is 0)
     real, intent(in)    :: rad
     ! number of points to sample along curve
     integer, intent(in) :: ilines
     ! angle to rotate the shape by, to orientate it.
     real, intent(in)    :: ang
     ! angle to start sampling points at; ccw is +; 0 is East
     real, intent(in)    :: angs
     ! 1 make a filled polygon, 2 make a hatched polygon
     integer, intent(in) :: ifill
     real                :: ang1, con1, con2, factor, offset, planet
     real                :: r, sunr, u, xpoin, xpoin1, ypoin, ypoin1
     integer             :: i10
        sunr = sun
        offset = offset0
        planet = planet0
        if (ilines  ==  0) return
        if (planet  ==  0.0) return
        if (sunr  ==  0.0) return
        if (rad  /=  0 .and. sunr - planet + offset  /=  0) then
           factor = rad/(sunr - planet + offset)
           sunr = factor*sunr
           planet = factor*planet
           offset = factor*offset
        end if
        u = 0.0 + ang
        con1 = PI*2.*(sunr/planet)/real(ilines)
        con2 = (1.0 - planet/sunr)*u
        xpoin1 = (sunr - planet)*cos(planet*u/sunr) + offset*cos(con2)
        ypoin1 = (sunr - planet)*sin(planet*u/sunr) - offset*sin(con2)
        ang1 = atan2(ypoin1, xpoin1) + angs
        r = sqrt(xpoin1**2 + ypoin1**2)
        xpoin1 = r*cos(ang1) + xc
        ypoin1 = r*sin(ang1) + yc
        select case (ifill)
        case (0)
        case (1)
           call polyfill(.true.)
           call makepoly()
        case (2)
           call polyhatch(.true.)
           call makepoly()
        case (3:)
           call makepoly()
        case default
        end select
        call move2(xpoin1, ypoin1)
        do i10 = 1, ilines
           u = con1*i10 + ang
           con2 = (1.0 - planet/sunr)*u
           if (con2  >=  2**24) con2 = amod(con2, PI)
           xpoin = (sunr - planet)*cos(planet*u/sunr) + offset*cos(con2)
           ypoin = (sunr - planet)*sin(planet*u/sunr) - offset*sin(con2)
           ang1 = atan2(ypoin, xpoin) + angs
           r = sqrt(xpoin**2 + ypoin**2)
           xpoin = r*cos(ang1) + xc
           ypoin = r*sin(ang1) + yc
           call draw2(xpoin, ypoin)
        end do
        if (ifill  >  0) then
           call closepoly()
           call polyfill(.false.)
        end if
     end subroutine spirograph
     end program demo_polyhatch

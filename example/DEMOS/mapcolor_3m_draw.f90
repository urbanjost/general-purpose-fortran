     program demo_mapcolor
     !   good program to exercise color tables, and look at differences
     !   when actual output device has a color table that is dynamic,
     !   or only has a small color table (a frame in this program takes
     !   at least SLICES*RINGS colors to produce accurately).
     use M_draw
     implicit none
     real                 :: lightstep
     integer              :: ii, iframe
     integer, parameter    :: SLICES = 30
     integer, parameter    :: RINGS = 8
     real                 :: LIGHTNESS
     integer, parameter    :: BOX = 500
     integer              :: ipaws
     integer              :: istart, iend
     character(len=20)    :: device
        call prefsize(BOX, BOX)
        call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
        call polyfill(.true.)
        call color(D_BLACK)
        call clear()
        call color(D_WHITE)
        call page(-110.0/2.0+5, 85.0/2.0+5, -110.0/2.0, 110.0/2.0)
        LIGHTNESS = 100.0
        lightstep = -5

        call vgetdev(device)
        select case (device)
        case ('p6', 'p3', 'ppm') ! just do one wheel
           istart = 10
           iend = 10
           LIGHTNESS = 50.0
        case default           ! do many lightnesses
           istart = 1
           iend = 19
        end select

        do ii = istart, iend
           iframe = ii
           call color(D_BLACK)
           call clear()
           call color(D_WHITE)
           call wheel()
           LIGHTNESS = LIGHTNESS + LIGHTSTEP
           ipaws = getkey()
        end do
        call vexit()
     contains
     !
     subroutine wheel() ! draw an entire wheel
     character(len=40) :: inline
     real              :: hue_val
     integer           :: ii
        call textang(0.0)
        call color(D_WHITE)
        call textsize(5.0, 6.0)
        call font('times.r')
        call move2(0.0, 103.0/2.0)
        call centertext(.true.)
        call linewidth(30)
        call drawstr('COLOR WHEEL')
        call linewidth(0)
        call textsize(2.5, 2.5)
        call font('futura.l')
        call move2(0.0, 90.0/2.0)
        write (inline, '("lightness=",f6.2)') LIGHTNESS
        call linewidth(30)
        call drawstr(inline)
        call linewidth(0)
        call textsize(1.5, 1.5)
        hue_val = 0
        do ii = SLICES, 1, -1
           call slice(hue_val)
        end do
        call centertext(.false.)
     end subroutine wheel
     !
     subroutine slice(hue_val) ! draw a slice
     integer           :: buffer
     real              :: hue_val, ang_inc
     character(len=40) :: inline
     real              :: step
     real              :: X1, X2, X3, X4
     real              :: Y1, Y2, Y3, Y4
     !
     integer           :: maxcolors, current_color
     integer           :: ir, ig, ib
     real              :: r, g, b
     real              :: saturation
     !
     integer           :: status
     integer           :: icount
     real              :: angle1, angle2
     real              :: radius1, radius2, radius3, radius4
     !
     integer, save      :: color_count = 0
        !
        buffer = 8
        ANG_INC = 360.0/SLICES
        angle1 = hue_val - ANG_INC/2
        angle2 = angle1 + ANG_INC
        saturation = 100
        radius1 = 32
        radius3 = radius1 + 4
        radius4 = radius1 + 7
        ! draw tic from wheel to start of angle label
        call color(D_WHITE)
        call linewidth(40)
        call move2(radius1*cosd(hue_val), radius1*sind(hue_val))
        call draw2(radius3*cosd(hue_val), radius3*sind(hue_val))
        ! draw degree label at tic
        call textang(hue_val)
        call move2(radius4*cosd(hue_val), radius4*sind(hue_val))
        write (inline, '(i0)') nint(hue_val)
        call linewidth(20)
        call drawstr(inline)
        call linewidth(0)
        step = radius1/real(RINGS)
        radius2 = radius1 - step
        ! draw a chunk in a slice
        MAXCOLORS = (256) - buffer
        do icount = RINGS + 1, 2, -1
           ! add buffer to leave base colors alone
           CURRENT_COLOR = MOD(color_count, MAXCOLORS) + buffer
           color_count = color_count + 1
           ! fancy mapcolor
           call hlsrgb(hue_val, LIGHTNESS, saturation, r, g, b, status)
           ir = int(r*255.0/100.0 + 0.50)
           ig = int(g*255.0/100.0 + 0.50)
           ib = int(b*255.0/100.0 + 0.50)
           call mapcolor(CURRENT_COLOR, ir, ig, ib)
           call color(CURRENT_COLOR)
           !
           X1 = cosd(angle1)*radius2
           Y1 = sind(angle1)*radius2
           X2 = cosd(angle1)*radius1
           Y2 = sind(angle1)*radius1
           !
           X3 = cosd(angle2)*radius2
           Y3 = sind(angle2)*radius2
           X4 = cosd(angle2)*radius1
           Y4 = sind(angle2)*radius1
           !
           call makepoly()
           call move2(X1, Y1)
           call draw2(X2, Y2)
           call draw2(X4, Y4)
           call draw2(X3, Y3)
           call closepoly()
           !
           saturation = saturation - 100.0/RINGS
           radius1 = radius2
           radius2 = radius1 - step
        end do
        hue_val = hue_val + ANG_INC
     end subroutine slice
     !
     subroutine hlsrgb(H, L, S, R, G, B, status)
     ! convert HLS(hue,lightness,saturation) values to RGB components
     !     given  : hue as a value of 0 to 360 degrees.
     !     .        lightness and saturation each as a value of 0 to 100.
     !     desired: r, g, and b each as a value of 0 to 100.
     !
     real, intent(in)   :: H, L, S
     real, intent(out)  :: R, G, B
     integer           :: status
     real              :: hue, lightness, saturation
     real              :: clr1, clr2
        ! passively report on bad input values
        if (h < 0.0 .or. h > 360.0) status = 1
        if (l < 0.0 .or. l > 100.0) status = 1
        if (s < 0.0 .or. s > 100.0) status = 1
        hue = H
        lightness = L/100.0
        saturation = S/100.0
        if (saturation == 0.0) then
           R = lightness
           G = lightness
           B = lightness
        end if
        if (lightness <= 0.50) then
           clr2 = lightness*(1.0 + saturation)
        else
           clr2 = lightness + saturation - lightness*saturation
        end if
        clr1 = 2.0*lightness - clr2
        R = rgbval(clr1, clr2, hue + 120.0)*100.0
        G = rgbval(clr1, clr2, hue)*100.0
        B = rgbval(clr1, clr2, hue - 120.0)*100.0
     end subroutine hlsrgb
     real function rgbval(clr1,clr2,h)
     ! rgbval(3fp): ensure a value is in the appropriate range and quadrant
     real    :: clr1,clr2
     real    :: h
     real    :: h2
        h2=h
        do
           if(h2 > 360.0 ) then
              h2=h2-360.0
              cycle
           endif
           exit
        enddo
        do
           if( h2  <  0.0 ) then
              h2=h2+360.0
              cycle
           endif
           exit
        enddo
        if(h2 < 60.0 ) then
           rgbval=clr1+(clr2-clr1)*h2/60.0
        else if(h2 < 180.0) then
           rgbval=clr2
        else if(h2 < 240.0) then
           rgbval=clr1+(clr2-clr1)*(240.0-h2)/60.0
        else
           rgbval=clr1
        endif
     end function rgbval
     end program demo_mapcolor

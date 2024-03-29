     program demo_makepoly
     use :: M_draw
     implicit none
     integer,parameter :: wide=640, tall=640
     integer :: rows, xoff, yoff, box_sz
     integer :: i20, i30, ncols, nrows, ilines
     real    :: bottom, left, sun_radius, planet_radius, planet_offset
     integer :: ipaws
        call prefsize(wide,tall)
        call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
        call ortho2(0.0, real(wide), 0.0, real(tall) )
        ! really slows down pbm driver because all lines are polygons
        ! call linewidth(3)
        call color(D_WHITE)
        call clear()
        call color(D_BLACK)
        rows=1
        ! size of biggest box to use and get specified number of rows
        box_sz=MIN(wide,tall)/rows
        ! number of rows of objects to draw
        nrows = tall/box_sz
        ! number of columns of objects to draw
        ncols = wide/box_sz
        ! initial x offset to begin row at to center drawings
        xoff = (wide - ncols * box_sz)/2
        ! initial x offset to begin column at to center drawings
        yoff = (tall - nrows * box_sz)/2
        sun_radius = 148
        planet_radius = 1
        do ilines = 1, 300
           do i20 = 1, ncols
              left = (i20-1)*box_sz+xoff
              do i30 = 1, nrows
                 bottom = (i30-1)*box_sz+yoff
                 call color(D_BLACK)
              call makepoly()
                 call rect(left,bottom,left+box_sz,bottom+box_sz)
              call closepoly()
                 planet_offset= sun_radius
                    call color(mod(ilines,15)+1)
                    call hypoc(left + box_sz/2.0, bottom + box_sz/2.0, &
                 & sun_radius, planet_radius, planet_offset, &
                 & box_sz/2.0, ilines,  &
                 & 0.0, 0.0, 1)
              enddo
           enddo
           ipaws=getkey()
        enddo
        call vexit()
     contains
     !
     !  Make shapes using hypocycloidal curves.
     !
     subroutine hypoc(xc,yc,sun,planet0,offset0,radius,ilines,ang,angs,ifill)
     use M_draw
     implicit none
     real,parameter  :: PI= 3.14159265358979323846264338327950288419716939937510
     real,intent(in) :: xc, yc      ! center of curve
     ! radii of sun, planet, and planet offset
     real,intent(in) :: sun,planet0,offset0
     real,intent(in)    :: radius
     integer,intent(in) :: ilines
     ! radius to fit the shape to (no fit if radius is 0)
     real,intent(in)    :: ang
     ! number of points to sample along curve
     real,intent(in)    :: angs
     ! angle to rotate the shape by, to orientate it.
     integer,intent(in) :: ifill
     ! angle to start sampling points at; ccw is +; 0 is East
     integer            :: i10
     ! 1 make a filled polygon, 2 make a hatched polygon
     real               :: ang1, con1, con2, factor
     real               :: offset, planet, r, sunr, u
     real               :: xpoin, xpoin1, ypoin, ypoin1
        sunr=sun
        offset=offset0
        planet=planet0
        if(ilines.eq.0.0) return
        if(planet.eq.0.0) return
        if(sunr.eq.0.0)   return
        if(radius.ne.0.and.sunr-planet+offset.ne.0)then
           factor=radius/(sunr-planet+offset)
           sunr=factor*sunr
           planet=factor*planet
           offset=factor*offset
        endif
        u=0.0+ang
        con1=PI*2.*(sunr/planet)/real(ilines)
        con2=(1.0-planet/sunr)*u
        xpoin1=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
        ypoin1=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
        ang1=atan2(ypoin1,xpoin1)+angs
        r=sqrt(xpoin1**2+ypoin1**2)
        xpoin1=r*cos(ang1)+xc
        ypoin1=r*sin(ang1)+yc
        select case(ifill)
        case(:0)
        case(1:)
           call makepoly()
        end select
        call move2(xpoin1,ypoin1)
        do i10=1,ilines
           u=con1*i10+ang
           con2=(1.0-planet/sunr)*u
           if(con2.ge.2**24) con2=amod(con2,PI)
           xpoin=(sunr-planet)*cos(planet*u/sunr)+offset*cos(con2)
           ypoin=(sunr-planet)*sin(planet*u/sunr)-offset*sin(con2)
           ang1=atan2(ypoin,xpoin)+angs
           r=sqrt(xpoin**2+ypoin**2)
           xpoin=r*cos(ang1)+xc
           ypoin=r*sin(ang1)+yc
           call draw2(xpoin,ypoin)
        enddo
        call draw2(xpoin1,ypoin1)
        if(ifill.gt.0)then
          call closepoly()
        endif
     end subroutine hypoc
     end program demo_makepoly

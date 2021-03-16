          program demo_makepoly
          use :: M_pixel
          use :: M_writegif, only : writegif
          use :: M_writegif_animated, only : write_animated_gif
          implicit none
          integer,parameter :: wide=640, tall=640
          integer :: rows, xoff, yoff, box_sz
          integer :: i20, i30, ncols, nrows, ilines
          real    :: bottom, left, sun_radius, planet_radius, planet_offset
          character(len=40) :: filename
          integer :: movie(300,0:wide-1,0:tall-1)
             call prefsize(wide,tall)
             call vinit()
             call ortho2(0.0, real(wide), 0.0, real(tall) )
             ! call linewidth(3) ! really slows down pbm driver because all lines are polygons
             call color(7)
             call clear()
             call color(0)
             rows=1
             box_sz=MIN(wide,tall)/rows       ! size of biggest box to use and get specified number of rows
             nrows = tall/box_sz              ! number of rows of objects to draw
             ncols = wide/box_sz              ! number of columns of objects to draw
             xoff = (wide - ncols * box_sz)/2 ! initial x offset to begin row at to center drawings
             yoff = (tall - nrows * box_sz)/2 ! initial x offset to begin column at to center drawings
             sun_radius = 148
             planet_radius = 1
             do ilines = 1, 300
                do i20 = 1, ncols
                   left = (i20-1)*box_sz+xoff
                   do i30 = 1, nrows
                      bottom = (i30-1)*box_sz+yoff
                      call color(0)
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
                movie(ilines,:,:)=P_pixel
                write(filename,'("hypoc.",i0,".gif")')ilines
                !!call writegif(filename,P_pixel,P_colormap)
             enddo
             call write_animated_gif('makepoly.3m_pixel.gif',movie,P_colormap,delay=70)
             call vexit()
          contains
          !
          !  Make shapes using hypocycloidal curves.
          !
          subroutine hypoc(xcenter,ycenter,sunr0,planet0,offset0,radius,ilines,ang,angs,ifill)
          use M_pixel
          implicit none
          real,parameter     :: PI= 3.14159265358979323846264338327950288419716939937510
          real,intent(in)    :: xcenter, ycenter      ! center of curve
          real,intent(in)    :: sunr0,planet0,offset0 ! radii of sun, planet, and planet offset
          real,intent(in)    :: radius                ! radius to fit the shape to (no fit if radius is 0)
          integer,intent(in) :: ilines                ! number of points to sample along curve
          real,intent(in)    :: ang                   ! angle to rotate the shape by, to orientate it.
          real,intent(in)    :: angs                  ! angle to start sampling points at; ccw is +; 0 is East
          integer,intent(in) :: ifill                 ! 1 make a filled polygon, 2 make a hatched polygon
          integer            :: i10
          real               :: ang1, con1, con2, factor
          real               :: offset, planet, r, sunr, u
          real               :: xpoin, xpoin1, ypoin, ypoin1
             sunr=sunr0
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
             xpoin1=r*cos(ang1)+xcenter
             ypoin1=r*sin(ang1)+ycenter
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
                xpoin=r*cos(ang1)+xcenter
                ypoin=r*sin(ang1)+ycenter
                call draw2(xpoin,ypoin)
             enddo
             call draw2(xpoin1,ypoin1)
             if(ifill.gt.0)then
               call closepoly()
             endif
          end subroutine hypoc
          end program demo_makepoly

             program demo_locator !     track a cube with the locator
             use M_draw
             parameter(TRANS=20.0, SC=0.1)
             integer, parameter :: FACE=1, FILLED=2, OUTLINE=3
             character(len=10)  :: device
             character(len=1)   :: but
             logical            :: back, fill, hatch
             integer            :: ios
                write(*,*)'x,y,z = translate'
                write(*,*)'s     = apply scale'
                write(*,*)'+,-   = change scale/translate direction'
                write(*,*)'f,h   = fill, hatch'
                write(*,*)'b     = toggle backface'
                write(*,*)'q     = quit'

                print*, 'Enter output device:'
                read(*, '(a)',iostat=ios) device
                if(ios.ne.0) device=' '

                call prefposition(50, 50)
                call prefsize(500, 500)

                call vinit(device)

                call window(-800.0, 800.0, -800.0, 800.0, -800.0, 800.0)
                call lookat(0.0, 0.0, 1500.0, 0.0, 0.0, 0.0, 0.0)

                tdir = TRANS
                scal = SC

                !
                ! Start with a very ordinary filled cube like in the original demo...
                !
                call polyhatch(.false.)
                call hatchang(45.0)
                call hatchpitch(40.0)
                call polyfill(.true.)

                fill = .true.
                hatch = .false.
                back = .true.

                call makeobj(FACE)
                call makepoly()
                call rect(-200.0, -200.0, 200.0, 200.0)
                call closepoly()
                call closeobj()

                call makecube(FILLED)

                nplanes = getdepth()
                if (nplanes .eq. 1) call makecube(OUTLINE)

                call backface(back)
                !
                ! Setup drawing into the backbuffer....
                !
                if (backbuffer().lt.0) then
                   call vexit()
                   write(*,*)'Device can''t support doublebuffering'
                   stop
                endif

                INFINITE: do
                   idum = slocator(x, y)
                   call pushmatrix()
                   call rotate(100.0 * x, 'y')
                   call rotate(100.0 * y, 'x')
                   call color(D_BLACK)
                   call clear()
                   call callobj(FILLED)
                   if (nplanes .eq. 1 .and. (fill .or. hatch)) call callobj(OUTLINE)
                   call popmatrix()
                   call swapbuffers()

                   but = char(checkkey())
                   select case(but)
                    case('x')
                      call translate(tdir, 0.0, 0.0)
                    case('y')
                      call translate(0.0, tdir, 0.0)
                    case('z')
                      call translate(0.0, 0.0, tdir)
                    case('s')
                      call scale(scal, scal, scal)
                    case('f')
                      fill = .not. fill
                      hatch = .false.
                      call polyfill(fill)
                    case('h')
                      hatch = .not. hatch
                      fill = .false.
                      call polyhatch(hatch)
                    case('b')
                      back = .not. back
                      call backface(back)
                    case('-')
                      tdir = -tdir
                      if (scal .lt. 1.0) then
                         scal = 1.0 + SC
                      else
                         scal = 1.0 - SC
                      endif
                    case('+')
                      tdir = TRANS
                    case('q',char(27))
                      call vexit()
                      stop
                   end select
                enddo INFINITE
             contains

             subroutine makecube(obj)
             integer obj

                call makeobj(obj)
                if (obj .eq. OUTLINE) then
                   call pushattributes()
                   call color(D_BLACK)
                   call polyfill(.false.)
                   call polyhatch(.false.)
                endif

                call pushmatrix()
                call translate(0.0, 0.0, 200.0)
                if (obj .eq. FILLED) call color(D_RED)
                call callobj(FACE)
                call popmatrix()

                call pushmatrix()
                call translate(200.0, 0.0, 0.0)
                call rotate(90.0, 'y')
                if (obj .eq. FILLED) call color(D_GREEN)
                call callobj(FACE)
                call popmatrix()

                call pushmatrix()
                call translate(0.0, 0.0, -200.0)
                call rotate(180.0, 'y')
                if (obj .eq. FILLED) call color(D_BLUE)
                call callobj(FACE)
                call popmatrix()

                call pushmatrix()
                call translate(-200.0, 0.0, 0.0)
                call rotate(-90.0, 'y')
                if (obj .eq. FILLED) call color(D_CYAN)
                call callobj(FACE)
                call popmatrix()

                call pushmatrix()
                call translate(0.0, 200.0, 0.0)
                call rotate(-90.0, 'x')
                if (obj .eq. FILLED) call color(D_MAGENTA)
                call callobj(FACE)
                call popmatrix()

                call pushmatrix()
                call translate(0.0, -200.0, 0.0)
                call rotate(90.0, 'x')
                if (obj .eq. FILLED) call color(D_YELLOW)
                call callobj(FACE)
                call popmatrix()

                if (obj .eq. OUTLINE) call popattributes()

                call closeobj()

              end subroutine makecube

              end program demo_locator

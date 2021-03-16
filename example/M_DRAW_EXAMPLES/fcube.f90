!@(#)  demonstrate double buffering and what happens when you hit clipping plane
!(LICENSE:PD)
!
!      Specifying an extra argument turns on the filling.
!
        program cube
        use M_draw
        character(len=30) :: device
        character(len=1)  :: c
        real    r, t, dr, dt
        integer nplanes
        logical fill, back, backdir

        integer BLACK, GREEN, RED, BLUE, WHITE
        parameter (BLACK = 0, GREEN = 2, RED = 1, BLUE = 4, WHITE = 7)

        print*,'Enter output device:'
        read(*,'(a)') device

        dr = 10.0
        dt = 0.2
        print*,'Enter delta in degrees (10 is typical):'
        read(*,*) dr

        call prefsize(300, 300)

        call vinit(device)


        nplanes = getdepth()

        fill = .true.
        back = .true.
        backdir = .true.

        call polyfill(fill)
        call backface(back)
        call backfacedir(backdir)

        call color(BLACK)
        call clear

        call window(-1.5, 1.5, -1.5, 1.5, 9.0, -5.0)
        call lookat(0.0, 0.0, 12.0, 0.0, 0.0, 0.0, 0.0)

!
! Setup drawing into the backbuffer....
!
        if (backbuffer().lt.0) then
                call vexit
                write(*,*)'Device can''t support doublebuffering'
                stop
        endif

        t = 0.0

        r = 0.0

 10     continue
            if (r.ge.360) r = 0.0
            call color(BLACK)
            call clear

            call pushmatrix

            call translate(0.0, 0.0, t)
            call rotate(r, 'y')
            call rotate(r, 'z')
            call rotate(r, 'x')
            call color(WHITE)

            call drawcube(nplanes)

            if (nplanes .eq. 1 .and. fill) then
                call polyfill(.false.)
                call color(0)
                call drawcube(nplanes)
                call polyfill(fill)
            endif

            call popmatrix

            t = t + dt
            if (t.gt.3.0 .or. t.lt.-18.0) dt = -dt

            call swapbuffers

            c = char(checkkey())
            if (c .eq. 'f') then
                fill = .not. fill
                call polyfill(fill)
            else if (c .eq. 'b') then
                back = .not. back
                call backface(back)
            else if (c .eq. 'd') then
                backdir = .not. backdir
                call backfacedir(backdir)
            else if (c .ne. char(0)) then
                call vexit
                stop
            endif

            r = r + dr
        goto 10

        end

!
! this routine draws the cube, using colours if available
!
        subroutine drawcube(nplanes)
        use M_draw
        integer nplanes

        integer BLACK, RED, GREEN, YELLOW, BLUE
        integer MAGENTA, CYAN, WHITE
        parameter (BLACK = 0, RED = 1, GREEN = 2)
        parameter (YELLOW = 3, BLUE = 4, MAGENTA = 5)
        parameter (CYAN = 6, WHITE = 7)

        real carray(3, 8)
        data carray/                                                    &
     &     -1.0,  -1.0,   1.0,                                          &
     &      1.0,  -1.0,   1.0,                                          &
     &      1.0,   1.0,   1.0,                                          &
     &     -1.0,   1.0,   1.0,                                          &
     &     -1.0,  -1.0,  -1.0,                                          &
     &      1.0,  -1.0,  -1.0,                                          &
     &      1.0,   1.0,  -1.0,                                          &
     &     -1.0,   1.0,  -1.0/

        if (nplanes.gt.1) call color(RED)

        call makepoly
                call move(carray(1,1), carray(2,1), carray(3,1))
                call draw(carray(1,2), carray(2,2), carray(3,2))
                call draw(carray(1,3), carray(2,3), carray(3,3))
                call draw(carray(1,4), carray(2,4), carray(3,4))
                call draw(carray(1,1), carray(2,1), carray(3,1))
        call closepoly

        if (nplanes.gt.1) call color(GREEN)

        call makepoly
                call move(carray(1,6), carray(2,6), carray(3,6))
                call draw(carray(1,5), carray(2,5), carray(3,5))
                call draw(carray(1,8), carray(2,8), carray(3,8))
                call draw(carray(1,7), carray(2,7), carray(3,7))
                call draw(carray(1,6), carray(2,6), carray(3,6))
        call closepoly

        if (nplanes.gt.1) call color(YELLOW)

        call makepoly
                call move(carray(1,2), carray(2,2), carray(3,2))
                call draw(carray(1,6), carray(2,6), carray(3,6))
                call draw(carray(1,7), carray(2,7), carray(3,7))
                call draw(carray(1,3), carray(2,3), carray(3,3))
                call draw(carray(1,2), carray(2,2), carray(3,2))
        call closepoly

        if (nplanes.gt.1) call color(BLUE)

        call makepoly
                call move(carray(1,1), carray(2,1), carray(3,1))
                call draw(carray(1,4), carray(2,4), carray(3,4))
                call draw(carray(1,8), carray(2,8), carray(3,8))
                call draw(carray(1,5), carray(2,5), carray(3,5))
                call draw(carray(1,1), carray(2,1), carray(3,1))
        call closepoly

        if (nplanes.gt.1) call color(MAGENTA)

        call makepoly
                call move(carray(1,3), carray(2,3), carray(3,3))
                call draw(carray(1,7), carray(2,7), carray(3,7))
                call draw(carray(1,8), carray(2,8), carray(3,8))
                call draw(carray(1,4), carray(2,4), carray(3,4))
                call draw(carray(1,3), carray(2,3), carray(3,3))
        call closepoly

        if (nplanes.gt.1) call color(CYAN)

        call makepoly
                call move(carray(1,1), carray(2,1), carray(3,1))
                call draw(carray(1,5), carray(2,5), carray(3,5))
                call draw(carray(1,6), carray(2,6), carray(3,6))
                call draw(carray(1,2), carray(2,2), carray(3,2))
                call draw(carray(1,1), carray(2,1), carray(3,1))
        call closepoly

        end

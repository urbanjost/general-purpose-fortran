!@(#)  Demonstrate a rotating translating tetrahedron, and doublebuffering
!
program ftetra
!(LICENSE:PD)
use M_draw

        integer BLACK, GREEN, RED, BLUE
        parameter (BLACK = 0, GREEN = 2, RED = 1, BLUE = 4)

        integer TETRAHEDRON
        parameter (TETRAHEDRON = 1)

        real R, tx, tz, rotval, drotval, zeye
        integer i
        logical back, backdir, fill
        character(len=50) :: device
        character(len=1)  :: c

        call prefsize(300, 300)

        print*,'Enter output device:'
        read(*,'(a)') device

        back = .true.
        backdir = .true.
        fill = .true.

        call vinit(device)
!
! Make the tetrahedral object
!
        call maketheobject

        rotval = 0.0
        drotval = 5.0
        zeye = 5.0

        R = 1.6

        tx = 0.0
        tz = R

!all polyfill(fill)
        call backface(back)
        call backfacedir(backdir)
        call clipping(.false.)

!
! set up a perspective projection with a field of view of
! 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
! and the far clipping plane at 1000.0.
!
        call perspective(40.0, 1.0, 0.001, 15.0)
        call lookat(0.0, 0.0, zeye, 0.0, 0.0, 0.0, 0.0)

!
! Setup drawing into the backbuffer....
!
        if (backbuffer().lt.0) then
                call vexit
                write(*,*)'Device can''t support doublebuffering'
                stop
        endif

!
! here we loop back here ad-naseum until someone hits a non interpreted key
!
 10     continue

          rotval = 0.0

          do 20 i = 0, int(359.0 / drotval)

            call color(BLACK)
            call clear

!
! Rotate the whole scene...(this accumulates - hence
! drotval)
!
            call rotate(drotval * 0.1, 'x')
            call rotate(drotval * 0.1, 'z')

            call color(RED)
                call pushmatrix
                call polyfill(.false.)
                call rotate(90.0, 'x')
                call circle(0.0, 0.0, R)
                call polyfill(fill)
            call popmatrix

            call color(BLUE)
            call move(0.0, 0.0, 0.0)
            call draw(tx, 0.0, tz)
                        
!
! Remember! The order of the transformations is
! the reverse of what is specified here in between
! the pushmatrix and the popmatrix. These ones don't
! accumulate because of the push and pop.
!

            call pushmatrix
                call translate(tx, 0.0, tz)
                call rotate(rotval, 'x')
                call rotate(rotval, 'y')
                call rotate(rotval, 'z')
                call scale(0.4, 0.4, 0.4)
                call callobj(TETRAHEDRON)
            call popmatrix

            tz = R * cos(rotval * 3.1415926535 / 180)
            tx = R * sin(rotval * 3.1415926535 / 180)

            call swapbuffers

            c = char(checkkey())
            if (c .eq. 'f') then
                fill = .not. fill
                call polyfill(fill)
            else if(c .eq. 'b') then
                back = .not. back
                call backface(back)
            else if (c .eq. 'd') then
                backdir = .not. backdir
                call backfacedir(backdir)
            else if (c .ne. char(0)) then
                call vexit
                stop
            endif

            rotval = rotval + drotval

 20       continue

        goto 10
                
end program ftetra
!-----------------------------------------------------------------------------------------------------------------------------------
! maketheobject
!
!       generate a tetrahedron object as a series of move draws
!
        subroutine maketheobject

      use M_draw

        integer RED, GREEN, YELLOW, CYAN, MAGENTA
        parameter (RED = 1, GREEN = 2, YELLOW = 3, CYAN = 5, MAGENTA = 6)

        integer TETRAHEDRON, NSIDES, NFACES, NPNTS
        parameter (TETRAHEDRON = 1, NSIDES = 3, NFACES = 4, NPNTS = 4)

        integer colface(NFACES)

        real pnts(3, NPNTS)

        integer faces(NSIDES, NFACES)

        integer i, j
        real x, y, z


        data pnts/               &
     &  -0.5, 0.866, -0.667,     &
     &  -0.5, -0.866, -0.667,    &
     &   1.0, 0.0, -0.667,       &
     &   0.0, 0.0, 1.334/


        data colface/GREEN, YELLOW, CYAN, MAGENTA/

        data faces/   &
     &  3, 2, 1,      &
     &  1, 2, 4,      &
     &  2, 3, 4,      &
     &  3, 1, 4/

        call makeobj(TETRAHEDRON)

        do 20 i = 1, NFACES
                call makepoly
                call color(colface(i))
                x = pnts(1, faces(1, i))
                y = pnts(2, faces(1, i))
                z = pnts(3, faces(1, i))
                call move(x, y, z)
                do 10 j = 2, NSIDES
                    x = pnts(1, faces(j,i))
                    y = pnts(2, faces(j,i))
                    z = pnts(3, faces(j,i))
                    call draw(x, y, z)
 10             continue
                call closepoly
 20     continue

        call closeobj

        end subroutine maketheobject

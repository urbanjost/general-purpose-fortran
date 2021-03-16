program fworld
!@(#) perspective, objects, color, software text
!
! most of the things in this program have been done before but it has
! a certain novelty value.
!
!(LICENSE:PD)

   use M_draw

   integer BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE
   parameter(BLACK = 0)
   parameter(RED = 1)
   parameter(GREEN = 2)
   parameter(YELLOW = 3)
   parameter(BLUE = 4)
   parameter(MAGENTA = 5)
   parameter(CYAN = 6)
   parameter(WHITE = 7)

   integer SPHERE
   real RADIUS, PI
   parameter (RADIUS = 10.0, PI = 3.1415926535, SPHERE = 1)

   character(len=20) :: dev

   print*,'Enter device:'
   read(*,'(a)') dev
   call vinit(dev)

   call vsetflush(.false.)

   call clipping(.false.)

   call font('futura.m')

   call perspective(80.0, 1.0, 0.001, 50.0)
   call lookat(13.0, 13.0, 8.0, 0.0, 0.0, 0.0, 0.0)

   call color(BLACK)
   call clear

   call makesphere

!
!       draw the main one in cyan
!
   call color(CYAN)

   call callobj(SPHERE)

!
!       draw a smaller one outside the main one in white
!
   call color(WHITE)

   call pushmatrix
   call translate(0.0, (-1.4) * RADIUS, 1.4 * RADIUS)
   call scale(0.3, 0.3, 0.3)
   call callobj(SPHERE)
   call popmatrix

!
!       scale the text
!
   call boxfit(2.0 * PI * RADIUS, 0.25 * RADIUS, 31)

!
!       now write the text in rings around the main sphere
!

   call color(GREEN)
   call showroundtext('Around the world in eighty days ')

   call color(BLUE)
!
!       note: that software text is rotated here as
!       anything else would be whether you use textang
!       or rotate depends on what you are trying to do.
!       Experience is the best teacher here.
!
   call rotate(90.0, 'x')
   call showroundtext('Around the world in eighty days ')

   call color(RED)
   call rotate(90.0, 'z')
   call showroundtext('Around the world in eighty days ')

   call vflush()
   idum=getkey()

   call vexit

end program fworld
!
! showroundtext
!
!       draw string str wrapped around a circle in 3d
!
subroutine showroundtext(str)

   use M_draw

   character(len=*) :: str

   real i, inc, RADIUS
   parameter (RADIUS = 10.0)
   integer j

   inc = 360.0 / float(len_trim(str))

   j = 1
   i=0
   do i10 = 1, len_trim(str)
      call pushmatrix
!
!                       find the spot on the edge of the sphere
!                       by making it (0, 0, 0) in world coordinates
!
      call rotate(i, 'y')
      call translate(0.0, 0.0, RADIUS)

      call move(0.0, 0.0, 0.0)

      call drawchar(str(j:j))
      j = j + 1
      call popmatrix
      i=i+inc
   enddo

end subroutine showroundtext

!
! makesphere
!
!       create the sphere object
!
subroutine makesphere

   use M_draw

   integer SPHERE
   parameter (SPHERE = 1)
   parameter(PI = 3.1415926535)
   parameter(RADIUS = 10.0)
   real i

   call makeobj(SPHERE)

   do i10 = 0, 180, 20
      call pushmatrix
      i=real(i10)
      call rotate(i, 'y')
      call circle(0.0, 0.0, RADIUS)
      call popmatrix
   enddo

   call pushmatrix
   call rotate(90.0, 'x')
   do i20 = -90, 90, 20
      a=real(i20)
      r = RADIUS * cos(a*PI/180.0)
      z = RADIUS * sin(a*PI/180.0)
      call pushmatrix
      call translate(0.0, 0.0, -z)
      call circle(0.0, 0.0, r)
      call popmatrix
   enddo
   call popmatrix

   call closeobj

end subroutine makesphere

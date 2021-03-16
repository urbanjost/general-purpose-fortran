program dlines
!
!@(#)  A program showing basic line styles.
!
!(LICENSE:PD)

   use M_draw

   character(len=40) :: device

   print*,'Enter output device: '
   read(*, '(a)')device

   call vinit(device)
   call vsetflush(.true.)
   call up(0.0, 1.0, 0.0)
   call perspective(90.0, 1.0, 0.3, 3.0)
   call translate(0.0, 0.0, -1.3)

   call drawscene
   call rotate(-30.0, 'y')
   call rotate(-30.0, 'x')
   call drawscene

   call vexit
end program dlines

subroutine drawscene

   use M_draw

   integer BLACK, RED, GREEN, BLUE, YELLOW, MAGENTA, WHITE
   integer CYAN
   parameter(BLACK = 0)
   parameter(RED = 1)
   parameter(GREEN = 2)
   parameter(YELLOW = 3)
   parameter(BLUE = 4)
   parameter(MAGENTA = 5)
   parameter(CYAN = 6)
   parameter(WHITE = 7)

   call color(BLACK)
   call clear

   call color(GREEN)
   call dashcode(0.03)

   call linestyle(' ')
   call xcentertext
   call move2(-0.45, 0.9)
   call drawstr('Linestyle: "10"')
   call move2(-0.45, 0.7)
   call drawstr('Linestyle: "110"')
   call move2(-0.45, 0.5)
   call drawstr('Linestyle: "111010"')
   call move2(-0.45, 0.3)
   call drawstr('Linestyle: "0001"')

   call linestyle('10')
   call move2(-0.9, 0.9)
   call draw2( 0.0, 0.9)
   call circle(0.6, 0.6, 0.4)

   call drawbox(0.9)
   call drawsine(0.9)

   call color(RED)
   call linestyle('110')
   call move2(-0.9, 0.7)
   call draw2( 0.0, 0.7)
   call circle(0.6, 0.6, 0.3)
   call drawbox(0.7)
   call drawsine(0.7)

   call color(CYAN)
   call linestyle('111010')
   call move2(-0.9, 0.5)
   call draw2( 0.0, 0.5)
   call circle(0.6, 0.6, 0.2)
   call drawbox(0.5)
   call drawsine(0.5)

   call color(YELLOW)
   call linestyle('0001')
   call move2(-0.9, 0.3)
   call draw2( 0.0, 0.3)
   call circle(0.6, 0.6, 0.1)
   call drawbox(0.3)
   call drawsine(0.3)

   idum=getkey()

end subroutine drawscene

subroutine drawbox(s)

   use M_draw

   real    s

   call pushmatrix

   call rotate(30.0, 'x')
   call rotate(60.0, 'y')
   call translate(-0.7, -1.2, 0.0)
   call scale(s, s, s)

   call move(0.0, 0.0, 0.0)

   call draw(1.0, 0.0, 0.0)
   call draw(1.0, 1.0, 0.0)
   call draw(0.0, 1.0, 0.0)
   call draw(0.0, 0.0, 0.0)

   call draw(0.0, 0.0, -1.0)
   call draw(1.0, 0.0, -1.0)
   call draw(1.0, 1.0, -1.0)
   call draw(0.0, 1.0, -1.0)
   call draw(0.0, 0.0, -1.0)

   call move(0.0, 1.0, -1.0)
   call draw(0.0, 1.0, 0.0)

   call move(1.0, 1.0, 0.0)
   call draw(1.0, 1.0, -1.0)

   call move(1.0, 0.0, 0.0)
   call draw(1.0, 0.0, -1.0)

   call popmatrix

end subroutine drawbox

subroutine drawsine(s)

   use M_draw

   real    s, RAD, AMP
   parameter(RAD = 0.5, AMP = 0.04)

   real    a, x, y, z

   call pushmatrix

   call translate(RAD + 0.2, -0.5, 0.0)
   call scale(s, s, s)

   call move(RAD, 0.0, 0.0)
   a=0.0
   do i10 = 0,2*314,2
      x = RAD * cos(a)
      z = RAD * sin(a)
      y = AMP * sin(a * 6.0)

      call draw(x, y, z)
      a=a+0.02
   enddo
   call popmatrix
end subroutine drawsine

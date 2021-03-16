program demo_fixedwidth
!(LICENSE:PD)
use M_draw
implicit none

character(len=50) :: device
character(len=80) :: fname
integer           :: ios
integer           :: idum

   print*,'Enter output device:'
   read(*,'(a)',iostat=ios) device
   if(ios.ne.0)device=' '

   print*,'Enter a font name:'
   read(*,'(a)',iostat=ios) fname
   if(ios.ne.0)fname='futura.l'

   call vinit(device)

   call color(D_BLACK)
   call clear

   call font(fname)

   call ortho2(0.0, 1.0, 0.0, 1.0)

   call drawgrid()

!
! show some scaled text on the grid (In the bottom part)
!
   call boxtext(0.1, 0.4, 0.8, 0.1, '{This is Some text] | $')

   idum=getkey()

   call color(D_BLACK)
   call clear

   call drawgrid

!
! centertext causes text to be centered around the current graphics
! position this is especially useful if you want your text to come
! out centered on a line, or a character to be centered on a point
! in a graph. A non-zero (.true.) argument turns centertext on.
!
! show a string centered on the center line
!
   call centertext(.true.)

   call boxtext(0.5, 0.5, 0.8, 0.1, '{This is Some Centered text] | $')
!
! turn centertext off. We use an argument with the value zero (.false.).
!
   call centertext(.false.)

   idum=getkey()

   call color(D_BLACK)
   call clear()

!
! rotate the grid so that it is the same angle as the text after
! textang for text ang.
!
   call pushmatrix()
   call translate(0.5, 0.5, 0.0)
   call rotate(90.0, 'z')
   call translate(-0.5, -0.5, 0.0)

   call drawgrid()
   call popmatrix()

!
! turn on centered text again
!
   call centertext(.true.)

!
! set the angle to 90.
!
   call textang(90.0)

!
! draw the string
!
   call boxtext(0.5, 0.5, 0.8, 0.1, '{This is Some Rotated Centered text] | $')
!
! turn off center text
!
   call centertext(.false.)

!
! set text angle back to 90
!
   call textang(0.0)

   idum=getkey()

   call color(D_BLACK)
   call clear

   call drawgrid

!
! as all the software fonts are proportionally spaced we use
! the fixedwidth call to make each character take the same amount
! of horizontal space. As with centertext this is done by passing
! fixedwidth a non-zero (.true.) argument.
!
   call fixedwidth(.true.)

   call boxtext(0.1, 0.5, 0.8, 0.1, '{This is Some Fixedwidth text] | $')

   idum=getkey()

   call color(D_BLACK)
   call clear

   call drawgrid

!
! now try centered and fixewidth at the same time
!
   call centertext(.true.)

   call move2(0.5, 0.5)
   call drawstr('{This is Some Cent.Fixedwidth text] | $')

   call centertext(.false.)

   idum=getkey()
   call color(D_BLACK)
   call clear

   call drawgrid

!
! scale the text so tha a character is the size of a box in
! the grid.
!
   call boxfit(0.8, 0.1, 8)

!
! draw the two strings fixedwidth (it is still turned on)
!
   call move2(0.1, 0.4)
   call drawstr('ABCDefgh')

   call move2(0.1, 0.5)
   call drawstr('IJKLmnop')

   idum=getkey()

   call vexit
!@(#) draw a grid in the middle of the screen
contains
!
subroutine drawgrid
implicit none
real           :: x
integer        :: i

   call color(D_GREEN)
   call rect(0.1, 0.4, 0.9, 0.6)

   x = 0.2

   do i = 1, 8
      call move2(x, 0.4)
      call draw2(x, 0.6)
      x = x + 0.1
   enddo

   call move2(0.1, 0.5)
   call draw2(0.9, 0.5)

   call color(D_RED)

end subroutine drawgrid

end program demo_fixedwidth

          program demo_example_text_justification   !      demonstrate still more features of text
          use M_draw
          character(len=20) :: dev
          integer           :: ios

          write(*,'(a)',advance='no')'Enter device: '
          read(*, '(a)',iostat=ios) dev
          if(ios.ne.0)dev=' '
          call vinit(dev)
          call textsize(0.03, 0.04)
          call ortho2(0.0, 1.0, 0.0, 1.0)
          call color(D_RED)
          call clear()

          call drawstuff()
          !       Now do it all with the text rotated ....
          call textang(45.0); call drawstuff()
          call textang(160.0); call drawstuff()
          call textang(270.0); call drawstuff()
          !       Now with a single character
          call textjustify(achar(0))

          call drawstuff2(0.0)
          call drawstuff2(90.0)
          call drawstuff2(160.0)
          call drawstuff2(270.0)

          do
             idum=getkey()
             select case(idum)
             case(:-1)                  ; exit
             case(ichar('q'),ichar('Q')); exit
             end select
          enddo
          call vexit()
          contains

          subroutine drawstuff
          use M_draw

          call color(D_BLACK)
          !call polyfill(1)
          call polyfill(.true.)
          !               So rect clears a bit for us

          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call color(D_GREEN)
          call move2(0.5, 0.5)
          call leftjustify()
          call drawstr('This is Left Justified text')

          idum=getkey()

          call color(D_BLACK)
          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call color(D_YELLOW)
          call move2(0.5, 0.5)
          call centertext(.true.)
          call drawstr('This is Centered text')
          call centertext(.false.)

          idum=getkey()

          call color(D_BLACK)
          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call color(D_MAGENTA)
          call move2(0.5, 0.5)
          call rightjustify()
          call drawstr('This is Right Justified text')
          call textjustify(achar(0))

          idum=getkey()
          end subroutine drawstuff

          subroutine drawstuff2(ang)
          use M_draw

          call color(D_BLACK)
          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call textang(ang)
          call color(D_GREEN)
          call move2(0.5, 0.5)
          call leftjustify()
          call drawchar('B')

          call textang(0.0)
          call textjustify(achar(0))
          call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be leftjustified')
          call pushattributes()
          idum=getkey()

          call color(D_BLACK)
          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call textang(ang)
          call color(D_YELLOW)
          call move2(0.5, 0.5)
          call centertext(.true.)
          call drawchar('B')
          call centertext(.false.)
          call textang(0.0)
          call textjustify(achar(0))
          call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be centered')
          call pushattributes()

          idum=getkey()

          call color(D_BLACK)
          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call textang(ang)
          call color(D_MAGENTA)
          call move2(0.5, 0.5)
          call rightjustify()
          call drawchar('B')
          call textang(0.0)
          call textjustify(achar(0))
          call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be rightjustified')
          call pushattributes()

          idum=getkey()

          call color(D_BLACK)
          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call textang(ang)
          call color(D_MAGENTA)
          call move2(0.5, 0.5)
          call topjustify()
          call drawchar('B')
          call textang(0.0)
          call textjustify(achar(0))
          call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be topjustified')
          call pushattributes()

          idum=getkey()

          call color(D_BLACK)
          call rect(0.1, 0.1, 0.9, 0.9)
          call color(D_WHITE)
          call move2(0.1, 0.5)
          call draw2(0.9, 0.5)
          call move2(0.5, 0.1)
          call draw2(0.5, 0.9)

          call textang(ang)
          call color(D_MAGENTA)
          call move2(0.5, 0.5)
          call topjustify()
          call rightjustify()
          call drawchar('B')
          call textang(0.0)
          call textjustify(achar(0))
          call boxtext(0.1, 0.1, 0.4, 0.02, 'The ''B'' should be right/topjustified')

          idum=getkey()

          end subroutine drawstuff2

          end program demo_example_text_justification

       program demo_poly2
       use M_draw
       implicit none
       integer :: i,j
       integer :: ipaws, icolor
       real    :: xx,yy
          call prefsize(512,512)
          call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
          call ortho2(0.0,256.0,0.0,256.0)
          call linewidth(1)
          call polyfill(.true.)
          ! step thru a series of rectangular cells
          icolor=0
          xx=0.0
          do i=1,16
             yy=0.0
             do j=1,16
                yy=yy+16.0
                icolor=icolor+1
                call setcolor(icolor,xx,yy)
             enddo
             xx=xx+16.0
          enddo
          ipaws=getkey()
          call vexit()
       contains

       subroutine setcolor(iset,xx,yy)
       integer,intent(in) :: iset
       real,intent(in)    :: xx,yy
       real    :: points(2,100)
       integer :: red, green, blue
       if(iset.gt.255)return
       ! determine coordinates of next square
       points(1:2,1)=[xx,      yy      ]
       points(1:2,2)=[xx,      yy+16.0 ]
       points(1:2,3)=[xx+16.0, yy+16.0 ]
       points(1:2,4)=[xx+16.0, yy      ]
       points(1:2,5)=[xx,      yy      ]
       ! get some RGB values to try
       red=irand(0,255)
       green=irand(0,255)
       blue=irand(0,255)
       ! set a color number to the new RGB values
       call mapcolor(icolor, red, green, blue)
       ! set to the new color
       call color(icolor)
       ! fill the rectangle in that color
       call poly2(5,points)
       end subroutine setcolor

       function irand(first,last) result(rand_int)
       use, intrinsic :: iso_fortran_env, only : dp=>real64
       integer,intent(in) :: first
       integer,intent(in) :: last
       integer              :: rand_int
       real(kind=dp)        :: rand_val
          call random_number(rand_val)
          rand_int = first + FLOOR((last+1-first)*rand_val)
       end function irand

       end program demo_poly2

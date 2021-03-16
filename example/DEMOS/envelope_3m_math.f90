          program demo_envelope
          use M_draw
          use M_math,     only : envelope        ! Find vertices (in clockwise order) of a polygon enclosing the points
          use M_math,     only : locpt           ! find if a point is inside a polygonal path
          use M_math,     only : polyarea        ! compute the area bounded by a closed polygonal curve
          implicit none
          integer,parameter :: n=6
          !  3--------------4
          !   \           /
          !     \       /
          !       \   /
          !         X 2,5
          !       /  \
          !     /      \
          !   /          \
          !  1--------------6
          real,parameter    :: x(n)=[-5.0, 0.0,-5.0, 5.0, 0.0, 5.0]
          real,parameter    :: y(n)=[-5.0, 0.0, 5.0, 5.0, 0.0,-5.0]
          real              :: xy(2,n)
          integer           :: vertex(n)
          integer           :: nvert
          integer           :: i
          integer           :: idum
             xy(1,:)=x
             xy(2,:)=y
             call vinit(' ')
             call page(-10.0,10.0,-10.0,10.0)
             call color(D_BLACK) ! set current color to black
             call clear()        ! clear to current color
             call polyfill(.true.)
             call color(D_BLUE)  ! we want to draw polygon in this color
             call poly2(n,xy)    ! draw filled polygon using points given
             idum=getkey()       ! pause for some input
             call color(D_CYAN)
             call polyhatch(.true.)
             call envelope(x, y, n, vertex, nvert)   ! calculate envelope
             call poly2(nvert,xy(:,vertex(1:nvert))) ! draw hatched envelope
             idum=getkey()       ! pause for some input
             call polyhatch(.false.)
             call linewidth(50)
             call color(D_WHITE)
             call poly2(n,xy)    ! draw line along original points
             idum=getkey()       ! pause for some input
             call random_seed()
             do i=1,70
                call pickrandom()
             enddo
             idum=getkey()       ! pause for some input
             call vexit()        ! wrap up and exit graphics mode
             write(*,*)'polyarea=',polyarea(x,y)
             write(*,*)'polyarea=',polyarea( xy(1,vertex(1:nvert)), xy(2,vertex(1:nvert)))
          contains
          subroutine pickrandom()
          ! randomly pick a point in the plot area and color it according to whether it is inside
          ! the original polygon
          real :: pointx, pointy
          integer :: l, m
             call random_number(pointx)
             call random_number(pointy)
             pointx=pointx*20.0-10.0
             pointy=pointy*20.0-10.0
             call locpt(pointx,pointy,x,y,n,l,m)
             select case(l)
              case(-1)
                call color(D_RED)
              case(0)
                call color(D_YELLOW)
              case(1)
                call color(D_GREEN)
              case default
                write(*,*)'*pickrandom* internal error: L value unknown'
                call color(D_WHITE)
             end select
             call circle(pointx,pointy,0.2)
          end subroutine pickrandom
          end program demo_envelope

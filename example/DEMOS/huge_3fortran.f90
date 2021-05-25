           program demo_huge_tiny
           ! or, "why I have my own NINT function"
           implicit none
           character(len=*),parameter :: f='(i2,1x,2(i11,1x),f14.0:,1x,l1,1x,a)'
           integer :: i,j,k,biggest
           real :: v, w
             ! basic
             print *, huge(0), huge(0.0), huge(0.0d0)
             print *, tiny(0.0), tiny(0.0d0)

             ! advanced
             biggest=huge(0)
             ! be careful when using integers in computation
             do i=1,14
                j=6**i   ! Danger, Danger
                w=6**i   ! Danger, Danger
                v=6.0**i
                k=v      ! Danger, Danger
                if(v.gt.biggest)then
                   write(*,f) i, j, k, v, v.eq.w, 'wrong j and k and w'
                else
                   write(*,f) i, j, k, v, v.eq.w
                endif
             enddo
           end program demo_huge_tiny

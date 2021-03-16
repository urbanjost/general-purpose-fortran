           program demo_scale1
           use M_math, only : scale1
           implicit none
           real :: start, end
           real :: xminp, xmaxp, dist
           integer :: intervals
           intervals=5
           write(*,*)'Enter start and end values'
           do
             read(*,*,end=999)start,end
             call scale1(start,end,intervals,xminp,xmaxp,dist)
             write(*,'(a,g0,a,g0,a,i0,a,g0)') &
                     & 'nice range is ',xminp,' to ',xmaxp,' by ', &
                     & nint((xmaxp-xminp)/dist),' intervals of ',dist
           enddo
           999 continue
           end program demo_scale1

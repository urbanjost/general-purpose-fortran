           program demo_scale3
           use M_math, only : scale3
           implicit none
           real :: start, end
           real :: xminp, xmaxp, dist
           integer :: intervals
           integer :: ios
           ! real :: treme(2)
           intervals=5
           write(*,*)'Enter start and end values'
           do
             read(*,*,iostat=ios)start,end
             if(ios.ne.0)exit
             call scale3(start,end,intervals,xminp,xmaxp,dist)
             ! treme(1)=log10(xminp)
             ! treme(2)=log10(xmaxp)
             ! treme(1)=floor(treme(1))
             ! treme(2)=ceiling(treme(2))
             ! if(treme(2).eq.treme(1))treme(2)=treme(2)+1
             write(*,'(a,g0,a,g0,a,i0,a,g0)') &
                     & 'nice range is 10**',log10(xminp),' to 10**',log10(xmaxp),' by ', &
                     & nint((log10(xmaxp)-log10(xminp))/dist),' intervals of ',dist
           enddo
           end program demo_scale3

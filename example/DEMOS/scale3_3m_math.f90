      program demo_scale3
      use M_math, only : scale3
      implicit none
      real :: start, end
      real :: xminp, xmaxp, dist
      integer :: intervals
      integer :: iostat
      intervals=5
      write(*,'(a)',advance='no')'Enter start and end values:'
      do
        read(*,*,iostat=iostat)start,end
        if(iostat.ne.0)exit
        call scale3(start,end,intervals,xminp,xmaxp,dist)
        write(*,'(*(g0))')                                &
        & 'nice log range is 10**', log10(xminp),         &
        & ' to 10**',log10(xmaxp),                        &
        & ' by ', nint((log10(xmaxp)-log10(xminp))/dist), &
        & ' intervals of 10**',dist
      enddo
      end program demo_scale3

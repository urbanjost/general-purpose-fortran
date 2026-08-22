      program demo_nearest
      implicit none
      character(len=*),parameter :: g='(*(g0,1x))'
      character(len=*),parameter :: ref='(a,1x,*(g20.15,1x))'
      character(len=*),parameter :: lim='(a,1x,*(g20.15,1x))'
      real                       :: x, y

         write (*,g) 'The basics ...'

         x = nearest(42.0, 1.0)
         y = nearest(42.0, -1.0)
         write (*,'(a,g20.15,a,g20.15,a,g20.15)')'for 42 +',x,'-', y,'delta',x-y

         write (*,g) 'For reference ...'

         write (*,ref) 'TINY    ',tiny(0.0)
         write (*,ref) 'HUGE    ',huge(0.0)
         write (*,ref) 'EPSILON ',epsilon(0.0)
         write (*,ref) 'SPACING ',spacing(tiny(0.0)),spacing(huge(0.0))

         write (*,g) 'Tesing the limits ...'

         write (*,lim) 'For TINY()', &
          nearest(tiny(0.0),1.0),    &
          nearest(tiny(0.0),-1.0),   &
          nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)

         write (*,lim) 'For HUGE()', &
          nearest(huge(0.0),1.0),    &
          nearest(huge(0.0),-1.0),   &
          nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)

      end program demo_nearest

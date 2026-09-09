      program demo_exponent
      implicit none
      real    :: x
      integer :: i
         print *, 'basic usage'
         print *, exponent([2.0,32.0,256.0,0.25])
         print *, exponent([1.0,10.0,100.0])
         print '(g0,1x,a,g0,1x,b32.32)', 500.0, 'exponent(500.0)=', &
         exponent(500.0), 500.0
         print '(g0,1x,a,g0,1x,b32.32)', 512.0, 'exponent(512.0)=', &
         exponent(512.0), 512.0
         print '(g0,1x,a,g0,1x,b32.32)', 550.0, 'exponent(550.0)=', &
         exponent(550.0), 525.0
         print *,'==>',log([500.0,512.0,550.0])/log(2.0)
         x=9.31
         i = exponent(x)
         print *, i ,  x

         print *, 'elemental'
         print *, exponent([10.0,100.0,1000.0,-10000.0])

         ! beware of overflow, it may occur silently
         !print *, 2**[10.0,100.0,1000.0,-10000.0]

         print *, 'exponent range'
         print *, minexponent(0.0),    maxexponent(0.0)
         print *, exponent(tiny(0.0)), exponent(huge(0.0))
         call dusty_corners()
      contains
      subroutine dusty_corners()
      use, intrinsic :: ieee_arithmetic
      real :: my_inf, my_neg_inf
      real :: my_qnan, my_snan

         print *
         print *, 'exponent(0.0)=', exponent(0.0)
         print *
         ! Generate positive infinity
         my_inf = ieee_value(my_inf, ieee_positive_inf)
         !print "(A,b32.32)" ,'in binary format      = ',my_inf
         print *, 'ieee_value(my_inf, ieee_positive_inf) =', my_inf
         print *
         ! Generate negative infinity
         my_neg_inf = ieee_value(my_neg_inf, ieee_negative_inf)
         print *,'ieee_value(my_inf, ieee_neg_inf)', my_neg_inf
         print *
         print *,'exponent([my_inf,my_neg_inf]) =',exponent([my_inf,my_neg_inf])

         if (ieee_support_nan(x)) then

            print *
            my_qnan = ieee_value(my_qnan, ieee_quiet_nan)
            print *, 'ieee_value(my_qnan, ieee_quiet_nan) =', my_qnan
            my_snan = ieee_value(my_snan, ieee_signaling_nan)
            print *, 'ieee_value(my_snan, ieee_signaling_nan) =', my_snan
            print *, 'exponent([my_qnan,my_snan]) =',exponent([my_qnan,my_snan])
            print *
            print *, 'Not sure ...'
            print *, 'exponent(tiny(0.0)/2)=', exponent(tiny(0.0)/2)

         endif
      end subroutine dusty_corners

      end program demo_exponent

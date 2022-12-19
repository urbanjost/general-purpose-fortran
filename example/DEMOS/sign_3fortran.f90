      program demo_sign
      implicit none
        ! basics
         print *,  sign( -12,  1 )
         print *,  sign( -12,  0 )
         print *,  sign( -12, -1 )
         print *,  sign(  12,  1 )
         print *,  sign(  12,  0 )
         print *,  sign(  12, -1 )

         if(sign(1.0,-0.0)== -1.0)then
            print *, 'this processor distinguishes +0 from -0'
         else
            print *, 'this processor does not distinguish +0 from -0'
         endif

         print *,  'elemental', sign( -12.0, [1.0, 0.0, -1.0] )

      end program demo_sign

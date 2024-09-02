      program demo_shape
      implicit none
      character(len=*),parameter :: all='(*(g0,1x))'
      integer, dimension(-1:1, -1:2) :: a
         print all, 'shape of array=',shape(a)
         print all, 'shape of constant=',shape(42)
         print all, 'size of shape of constant=',size(shape(42))
         print all, 'ubound of array=',ubound(a)
         print all, 'lbound of array=',lbound(a)
      end program demo_shape

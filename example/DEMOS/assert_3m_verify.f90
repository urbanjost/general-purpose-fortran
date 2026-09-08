     program demo_assert
     use M_verify, only : assert
     implicit none
     real :: a, toobig=1024
     a=2000
     call assert('myroutine', 101, a > toobig, 'The value is too large', a, ' > ', toobig)
     end program demo_assert

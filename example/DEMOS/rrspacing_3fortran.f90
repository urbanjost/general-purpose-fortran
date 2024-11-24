      program demo_rrspacing
      implicit none
      integer, parameter :: sgl = selected_real_kind(p=6, r=37)
      integer, parameter :: dbl = selected_real_kind(p=13, r=200)
      character(len=*),parameter :: gen='(*(g0))', nl=new_line('A')
      real(kind=sgl) :: x
         x=-3.0_sgl
         print gen, &
         'rrspacing(',x,'_sgl)=', rrspacing(x),                       nl, &
         'rrspacing(x)=abs(fraction(x))*float(radix(x))**digits(x)',  nl, &
         'so this should be the same as rrspacing():',                nl, &
         abs( fraction(x) ) * float( radix(x) )**digits(x),           nl, &
         'RRSPACING (-3.0) has the value 0.75x2**24 for reals',       nl, &
         'on current typical platforms. For reference:',              nl, &
         '   0.75*2**24=', 0.75*2**24,                                nl, &
         'sign should not matter, so',rrspacing(x)==rrspacing(-x),    nl, &
         'note the kind of the value is significant',                 nl, &
         rrspacing(-3.0_dbl),                                         nl, &
         'for common platforms rrspacing(487923.3d0)=>',              nl, &
         '   8.382458680573952E+015',                                 nl, &
         rrspacing(487923.3d0),                                       nl, &
         ' '
      end program demo_rrspacing

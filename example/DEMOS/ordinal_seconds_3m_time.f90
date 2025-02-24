      program demo_ordinal_seconds
      use M_time, only : ordinal_seconds
      implicit none
      character(len=*),parameter :: gen='(*(g0))'
      integer          :: i, istart, iend
      real,volatile    :: x
      istart = ordinal_seconds()
      x = 0.0
      do i = 1, 1000000000
         x = x+sqrt(real(i))
      enddo
      print gen, 'x=',x
      iend = ordinal_seconds()
      print gen, 'that took ',iend-istart,' seconds'
      print gen, iend,'-',istart,'=',iend-istart
      end program demo_ordinal_seconds

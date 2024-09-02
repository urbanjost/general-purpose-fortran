program demo_M_framework__tictoc
   use M_tictoc
   implicit none

   integer, parameter :: dp = selected_real_kind(15)
   character(len=*),parameter :: gen='(*(g0,1x))'

   type(timer)     :: clock
   integer         :: i, j
   real(kind=dp)   :: x(10000)
   namelist /vals/ clock

   clock=timer()
   call clock%tic()
   do j = 1, 100000
      x(:) = log(23.)*[(i, i=1, 10000)]
   end do
   call clock%toc()

   write (*, gen) 'CPU TIME = ', clock%cputime()

   write (*, gen) 'Now measuring inside the loop...'
   call clock%tic()
   do j = 1, 100000
      x(:) = log(23.)*[(i, i=1, 10000)]

      if (j == 5000) then
         call clock%toc()
         write(*, gen) 'CPU TIME NOW (without pausing) = ', clock%cputime()
      endif
   end do

   call clock%toc()
   call clock%print()
   call clock%print('<TIMING>')

end program demo_M_framework__tictoc

program demo_M_framework__timing
   use M_framework__timing
   implicit none

   integer, parameter :: dp = selected_real_kind(15)
   character(len=*),parameter :: gen='(*(g0,1x))'

   type(timer)     :: clock
   integer         :: i, j
   real(kind=dp)   :: x(10000)
   namelist /vals/ clock

   clock=timer()
   write (*, gen)'Before calls'
   write (*, nml=vals)

   write (*, gen)'Initialize:TIC'
   call clock%tic()
   write (*, nml=vals)
   do j = 1, 100000
      x(:) = log(23.)*[(i, i=1, 10000)]
   end do
   write (*, gen)':TOC:'
   call clock%toc()

   write (*, gen) 'CPU TIME = ', clock%cputime()
   write (*, nml=vals)

   write (*, gen) 'Now measuring inside the loop...'
   write (*, nml=vals)

   call clock%tic()
   do j = 1, 100000
      x(:) = log(23.)*[(i, i=1, 10000)]

      if (j == 5000) then
         call clock%toc()
         write(*, gen) 'CPU TIME NOW (without pausing) = ', clock%cputime()
         write (*, nml=vals)
      endif
   end do

   call clock%toc()

   call clock%print()

   write(*,*)clock
   write (*, nml=vals)

end program demo_M_framework__timing

      program demo_system_sleep
      use M_system, only : system_sleep
      use M_time, only : now
      implicit none
      integer :: i
         !
         write(*,'(a)')"Time before integer call is: ",now()
         call system_sleep(4)
         write(*,'(a)')"Time after  integer call is: ",now()
         !
         write(*,'(a)')"Time before real call is: ",now()
         call system_sleep(4.0)
         write(*,'(a)')"Time after  real call is: ",now()
         !
         write(*,'(a)')"Time before loop is: ",now()
         do i=1,1000
            call system_sleep(4.0/1000.0)
         enddo
         write(*,'(a)')"Time after loop  is: ",now()
         !
      end program demo_system_sleep

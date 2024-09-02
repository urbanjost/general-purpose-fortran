      program demo_system_sleep
      use M_system, only : system_sleep, epoch_to_calendar
      implicit none
      integer :: i
         !
         write(*,'(2a)')"Time before integer call is: ",epoch_to_calendar()
         call system_sleep(4)
         write(*,'(2a)')"Time after  integer call is: ",epoch_to_calendar()
         write(*,*)
         write(*,'(2a)')"Time before real call is: ",epoch_to_calendar()
         call system_sleep(4.0)
         write(*,'(2a)')"Time after  real call is: ",epoch_to_calendar()
         write(*,*)
         write(*,'(2a)')"Time before loop is: ",epoch_to_calendar()
         do i=1,1000
            call system_sleep(4.0/1000.0)
         enddo
         write(*,'(2a)')"Time after loop  is: ",epoch_to_calendar()
      end program demo_system_sleep

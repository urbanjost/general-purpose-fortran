      program demo_system_signal
      use M_system, only : system_signal
      implicit none
      logical :: loop=.true.
      integer, parameter :: SIGINT=2,SIGQUIT=3
      call system_signal(SIGINT,exitloop)
      call system_signal(SIGQUIT,quit)
      write(*,*)'Starting infinite loop. Press Ctrl+C to exit.'
      do while(loop)
      enddo
      write(*,*)'Reporting from outside the infinite loop.'
      write(*,*)'Starting another loop. Do Ctrl+\ anytime to quit.'
      loop=.true.
      call system_signal(2)
      write(*,*)&
       & 'Just installed do-nothing handler for SIGINT. Try Ctrl+C to test.'
      do while(loop)
      enddo
      write(*,*)'You should never see this line when running this demo.'

      contains

      subroutine exitloop(signum)
        integer :: signum
        write(*,*)'Caught SIGINT. Exiting infinite loop.'
        loop=.false.
      end subroutine exitloop

      subroutine quit(signum)
        integer :: signum
        STOP 'Caught SIGQUIT. Stopping demo.'
      end subroutine quit
      end program demo_system_signal

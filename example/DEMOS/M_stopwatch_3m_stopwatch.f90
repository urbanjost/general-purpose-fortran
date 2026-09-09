       ! program demo_M_stopwatch and modules
       ! The following times are measured:
       !
       !  o each phase each time through the loop
       !  o total time for each phase
       !  o the total time
       !
       !  There will be printed output that should not be included in the
       !  measured time.
       !
       module globals
       use M_stopwatch
       implicit none
       private
       public :: watchgroup
       public :: setup_watches

       ! the group with all the watches; global var
       type (watchgroup), public :: ALL_GROUPS

       type (watchtype), public, dimension(5) :: w
       ! The watches are: w(1) time for phase 1 this time through the loop
       !                  w(2) time for phase 2 this time through the loop
       !                  w(3) total time for phase 1
       !                  w(4) total time for phase 2
       !                  w(5) total time
       ! The watch groups are: GROUPS_FOR_ONE phase 1 times w(1) and w(3)
       !                       GROUPS_FOR_TWO phase 2 times w(2) and w(4)
       !                       ALL_GROUPS all of them (declared in module globals)
       type (watchgroup), public :: GROUPS_FOR_ONE, GROUPS_FOR_TWO
       contains
       subroutine setup_watches

          ! Measure only cpu and wall time
          call option_stopwatch(default_clock=(/"cpu ","wall"/))
          call create_watch(w,name=(/ "phase 1      ", &      ! create the watches
                                      "phase 2      ", &
                                      "total phase 1", &
                                      "total phase 2", &
                                      "Total        " /) )
          call create_watchgroup(w(1),GROUPS_FOR_ONE)         ! create the groups
          call join_watchgroup(w(3),GROUPS_FOR_ONE)
          call create_watchgroup(w(2:4:2),GROUPS_FOR_TWO)     ! a shorter way
          call create_watchgroup(w,ALL_GROUPS)
          call start_watch(w(5))                              ! start the total time

       end subroutine setup_watches

       end module globals

       module workers
       implicit none
       ! The routines being measured
       public :: subone
       contains

       subroutine subone(n,c) ! just to give us something to time.
       use M_stopwatch
       use globals
       integer, intent(in) :: n
       real, intent(out)   :: c
       integer :: i
       real :: a=2.0,b
       b=real(n)
       do i=1,n
          c=a*b
       end do

       call pause_watch(ALL_GROUPS)
       write(unit=*,fmt=*) "Performed ",n," multiplications"
       call end_pause_watch(ALL_GROUPS)

       end subroutine subone

       end module workers

       program demo_M_stopwatch
       use M_stopwatch
       use globals
       use workers
       implicit none

       integer :: i, nmult                ! loop counter, number of multiplies to do
       logical :: cpu_is_there            ! flag for cpu clock
       real    :: zz

       call setup_watches()

       nmult = 200000
       do i=1,3
          write(*,'(a)')repeat('=',70)
          write(*,*)'LOOP',i,':'
          ! reset the watches that measure the time for this loop
          call reset_watch(w(1:2))
          ! start the phase 1 watches, do phase 1, and stop the phase 1 watches
          call start_watch(GROUPS_FOR_ONE)
          nmult = 5*nmult
          call subone(nmult,zz)
          call stop_watch(GROUPS_FOR_ONE)

          call start_watch(GROUPS_FOR_TWO)       ! same for phase 2
          nmult = 2*nmult
          call subone(nmult,zz)
          call stop_watch(GROUPS_FOR_TWO)

       ! pause the cpu clock of the total time watch while printing the current times,
       ! if the cpu clock is available on this implementation, but leave the wall
       ! clock running.  The call to inquiry_stopwatch should be outside the loop, but
       ! this should make a clearer illustration.

          call inquiry_stopwatch(cpu_avail=cpu_is_there)
          if (cpu_is_there) then
             call pause_watch(w(5),"cpu")
          end if

          write(*,'(a)')repeat('-',70)
          call print_watch(w(1:2),title="Times for this loop")
          write(*,'(a)')repeat('-',70)
          call print_watch(w(3:4),title="Total times so far")

          if (cpu_is_there) then
             call end_pause_watch(w(5),"cpu")
          end if

       end do

       write(*,'(a)')repeat('=',70)
       ! print the total times
       call print_watch([w(3),w(4),w(5)],title="Final total times")

       write(unit=*,fmt=*)&
       &"Note: the difference between the sum of the first two wall clocks"
       write(unit=*,fmt=*)&
       &"      and the Total wall clock is due to not pausing the wall clock"
       write(unit=*,fmt=*)&
       &"      on the Total watch while printing."

       call destroy_watch(w)                ! destroy the watches
       end program demo_M_stopwatch

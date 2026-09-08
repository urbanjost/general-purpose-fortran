      program demo_atan2pi
      real :: z
      complex :: c
      real, parameter :: h2d = 180.0
       !
       ! basic usage
        ! atan2pi (1.5574077, 1.0) has the value 1.0 (approximately).
        z=atan2pi(1.5574077, 1.0)
        write(*,*) 'half-revolutions=',z,'degrees=',h2d*z
       !
       ! elemental arrays
        write(*,*)'elemental',atan2pi( [10.0, 20.0], [30.0,40.0] )
       !
       ! elemental arrays and scalars
        write(*,*)'elemental',atan2pi( [10.0, 20.0], 50.0 )
       !
       ! break complex values into real and imaginary components
       ! (note TAN2() can take a complex type value )
        c=(0.0,1.0)
        write(*,*)'complex',c,atan2pi( x=c%re, y=c%im )
       !
       ! extended sample converting cartesian coordinates to polar
        COMPLEX_VALS: block
        real                :: ang
        complex,allocatable :: vals(:)
        integer             :: i
       !
        vals=[ &
          ( 1.0, 0.0 ), & ! 0
          ( 1.0, 1.0 ), & ! 45
          ( 0.0, 1.0 ), & ! 90
          (-1.0, 1.0 ), & ! 135
          (-1.0, 0.0 ), & ! 180
          (-1.0,-1.0 ), & ! 225
          ( 0.0,-1.0 )]   ! 270
          write(*,'(a)')repeat('1234567890',8)
        do i=1,size(vals)
           ang=atan2pi(vals(i)%im,vals(i)%re)
           write(*,101)vals(i),ang,h2d*ang
        enddo
        101 format(             &
        & 'X= ',f5.2,           &
        & ' Y= ',f5.2,          &
        & ' HALF-REVOLUTIONS= ',f7.3,        &
        & T50,' DEGREES= ',g0.4)
       endblock COMPLEX_VALS
      !
      end program demo_atan2pi

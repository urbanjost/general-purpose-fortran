      program demo_atan2d
      implicit none
      integer,parameter  :: wp=kind(0.0)
      real(wp),parameter :: d2r=acos(-1.0_wp)/180.0_wp
      real :: z
      complex :: c
       !
       ! basic usage
        ! atan2d (1.5574077, 1.0) has the value 1.0 radian (approximately).
        z=atan2d(1.5574077, 1.0)
        write(*,*) 'degrees=',z,'radians=',d2r*z
       !
       ! elemental arrays
        write(*,*)'elemental',atan2d( [10.0, 20.0], [30.0,40.0] )
       !
       ! elemental arrays and scalars
        write(*,*)'elemental',atan2d( [10.0, 20.0], 50.0 )
       !
       ! multi-dimensional returns multi-dimensional
       write(*,*) atan2(reshape([1.0,1.0,1.0,1.0],[2,2]),&
       & reshape([1.0,1.0,1.0,1.0],[2,2]) )
       !
       ! break complex values into real and imaginary components
        c=(0.0,1.0)
        write(*,*)'complex value treated as components', &
        & c,atan2d( x=c%re, y=c%im )
       !
       ! extended sample
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
        do i=1,size(vals)
           ang=atan2d(vals(i)%im, vals(i)%re)
           write(*,101)vals(i),ang,d2r*ang
        enddo
        101 format(             &
        & 'X= ',f5.2,           &
        & ' Y= ',f5.2,          &
        & ' ANGLE= ',g0,        &
        & T38,'RADIANS= ',g0.4)
       endblock COMPLEX_VALS
      !
      end program demo_atan2d

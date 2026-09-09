!*==qa5.f90 processed by SPAG 8.01RF 18:20 12 Dec 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
program qa5
! (LICENSE:Public Domain)
use M_calcomp
implicit none
real              :: xar(10), yar(10)
real              :: r(19), angle(19)
character(len=50) :: ibcd
real              :: a, angl, x, theta, xa, ya, xb, yb, xc, yc, xd, yd, bang, beta, xx, dr
integer           :: i, k, kin, inteq
!
!
   kin = 50
   call make_c_qa5()
   open (unit=kin,file='qa5.dat')

   call plots(0.0,8.5,0.0,11.0)
   call plot(0.0,-11.0,3)
!     CALL PLOT(0.0,-10.5,-3)
   call nframe()
! DRAW FRAME
   call plot(7.0,0.0,2)
   call plot(7.0,9.0,2)
   call plot(0.0,9.0,2)
   call plot(0.0,0.0,2)
! COMMENTS ARE INSERTED
   read (kin,99001) ibcd
   call symbol(0.7,8.5,0.14,ibcd,inteq,0.0,40)
   read (kin,99001) ibcd
   call symbol(0.7,4.25,0.14,ibcd,inteq,0.0,23)
   read (kin,99001) ibcd
   call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,23)
! TWO PAIRS OF AXES ARE DRAWN
   read (kin,99001) ibcd
   call axis(1.0,4.75,ibcd,-1,5.0,0.0,0.0,1.0)
   call axis(1.0,4.75,ibcd,1,3.0,90.0,0.0,1.0)
   call axis(1.0,0.75,ibcd,-1,5.0,0.0,0.0,1.0)
   call axis(1.0,0.75,ibcd,1,3.0,90.0,0.0,1.0)
! CURVX IS DRAWN
   call plot(1.0,4.75,-3)
   call curvx(0.1,5.0,2.40,0.0,0.75,2.0,-0.525,3.0,0.075,4.0)
   call plot(-1.0,-4.75,-3)
! CURVY IS DRAWN
   call plot(1.0,0.75,-3)
   call curvy(0.1,3.0,9.0,1.26,-6.0,2.52,1.0,3.78,0.0,0.0)
   call plot(-1.0,-0.75,-3)
! EQUATIONS ARE DRAWN
   read (kin,99001) ibcd
   call symbol(3.0,7.75,0.09,ibcd,inteq,0.0,35)
   read (kin,99001) ibcd
   call symbol(3.0,3.90,0.09,ibcd,inteq,0.0,27)
!     CALL PLOT(11.0,0.0,-3)
   call nframe()
!     DRAW FRAME
   call plot(7.0,0.0,2)
   call plot(7.0,9.0,2)
   call plot(0.0,9.0,2)
   call plot(0.0,0.0,2)
!     READ AXIS TITLES
!     DRAW AXIS
   read (kin,99001) ibcd
   call axis(0.75,0.75,ibcd,-12,5.0,0.0,5.0,1.0)
   read (kin,99001) ibcd
   call axis(0.75,0.75,ibcd,9,7.0,90.0,0.0,100.0)
!     DRAW COMMENTS
   read (kin,99001) ibcd
   call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,34)
   call plot(5.0,7.8,3)
   call plot(5.1,7.8,2)
   read (kin,99001) ibcd
   call symbol(5.2,7.80,0.09,ibcd,inteq,0.0,6)
   inteq = 1
   call symbol(5.0,7.60,0.10,ibcd,inteq,0.0,-1)
   inteq = 999
   read (kin,99001) ibcd
   call symbol(5.2,7.60,0.09,ibcd,inteq,0.0,5)
! SMOOTHING
   call smoot(0.75,3.75,0)
   call smoot(1.75,2.5,-2)
   call smoot(2.25,5.75,-2)
   call smoot(2.75,7.0,-2)
   call smoot(3.25,7.25,-2)
   call smoot(4.25,6.75,-2)
   call smoot(4.75,3.75,-2)
   call smoot(5.75,4.75,-24)
! FLINE IS USED
   read (kin,99002) (xar(i),yar(i),i=1,8)
99002 format (2f4.2)
   xar(9) = 0.0
   xar(10) = 1.0
   yar(9) = 0.0
   yar(10) = 1.0
   call plot(0.75,3.25,3)
   call fline(xar,yar,-8,1,1,1)
!     CALL PLOT(11.0,0.0,-3)
   call nframe()
! DRAW FRAME
   call plot(7.0,0.0,2)
   call plot(7.0,9.0,2)
   call plot(0.0,9.0,2)
   call plot(0.0,0.0,2)
! DRAW COMMENTS
   read (kin,99001) ibcd
   call symbol(0.7,8.25,0.14,ibcd,inteq,0.0,42)
   read (kin,99001) ibcd
   call symbol(0.7,3.80,0.14,ibcd,inteq,0.0,22)
! AXIS IS DRAWN
   read (kin,99001) ibcd
   call axis(1.0,4.75,ibcd,-8,5.0,0.0,0.0,25.0)
   read (kin,99003) (xar(i),yar(i),i=1,6)
99003 format (f4.2,f4.0)
   call scalg(yar,3.0,6,1)
   read (kin,99001) ibcd
   call lgaxs(1.0,4.75,ibcd,11,3.0,90.0,yar(7),yar(8))
   call scale(xar,5.0,6,1)
   call plot(1.0,4.75,-3)
   call lglin(xar,yar,6,1,0,1,1)
   call plot(-1.0,-4.75,-3)
! POLAR SUBROUTINE IS USED
   x = 0.0
   do k = 1, 19
      theta = x*0.0174533
      r(k) = 2.0*(1.0-cos(theta))
      angle(k) = theta
      x = x + 10.0
   enddo
   call plot(5.0,0.75,-3)
   dr = 1.0
   call polar(r,angle,19,1,0,1,0.0,dr)
   angl = 30.0
   a = 1.0
   do i = 1, 5
      theta = angl*0.0174533
      xa = cos(theta)
      ya = sin(theta)
      call plot(xa,ya,3)
      xb = 1.1*xa
      yb = 1.1*ya
      call plot(xb,yb,2)
      xc = xb + 0.05*xa
      yc = yb + 0.05*ya
      if ( i>3 ) a = 1.5
      beta = 1.570797 - theta
      xd = xc - 0.105*a*cos(beta)
      yd = yc + 0.105*a*sin(beta)
      bang = 270.0 + angl
      call number(xd,yd,0.105,angl,bang,-1)
      angl = angl + 30.0
   enddo
   xx = 0.0
   do i = 1 , 19
      angle(i) = xx*0.0174533
      r(i) = 1.0
      xx = xx + 10.0
   enddo
   dr = 1
   call polar(r,angle,19,1,0,1,0.0,dr)
   call plot(-5.0,-0.75,-3)
! AXIS IS DRAWN
   read (kin,99001) ibcd
   call axis(1.0,0.75,ibcd,-1,4.0,0.0,4.0,-1.0)
   call axis(5.0,0.75,ibcd,-1,1.0,0.0,0.0,1.0)
   read (kin,99001) ibcd
   call symbol(3.75,3.5,0.09,ibcd,inteq,0.0,23)
   call plot(11.0,0.0,999)
   close (unit=kin, status='delete')
   stop
99001 format (a40,a2)
contains
subroutine make_c_qa5()
integer , parameter :: io = 40
   open (unit=io,file='qa5.dat')
   write (io,'(a)') 'SAMPLE OF SCIENTIFIC SUBROUTINES PACKAGE'
   write (io,'(a)') 'USING CURVY SUBROUTINE'
   write (io,'(a)') 'USING CURVX SUBROUTINE'
   write (io,'(a)') ''
   write (io,'(a)') 'Y=0.075X**4-0.525X**3+0.75X**2+2.40'
   write (io,'(a)') 'X=Y**3.78-6Y**2.52+9Y**1.26'
   write (io,'(a)') 'SERVICE TIME'
   write (io,'(a)') 'FREQUENCY'
   write (io,'(a)') 'USING FLINE AND SMOOT SUBROUTINES'
   write (io,'(a)') 'SMOOT'
   write (io,'(a)') 'FLINE'
   write (io,'(a)') ' 075 325'
   write (io,'(a)') ' 175 200'
   write (io,'(a)') ' 225 525'
   write (io,'(a)') ' 275 650'
   write (io,'(a)') ' 325 675'
   write (io,'(a)') ' 425 625'
   write (io,'(a)') ' 475 325'
   write (io,'(a)') ' 575 425'
   write (io,'(a)') 'USING SCALG, LGAXS, AND LGLIN SUBROUTINES'
   write (io,'(a)') 'USING POLAR SUBROUTINE'
   write (io,'(a)') 'ALTITUDE'
   write (io,'(a)') ' 100 250'
   write (io,'(a)') ' 200 110'
   write (io,'(a)') ' 300 500'
   write (io,'(a)') ' 400 900'
   write (io,'(a)') ' 500 200'
   write (io,'(a)') ' 600 140'
   write (io,'(a)') 'TEMPERATURE'
   write (io,'(a)') ''
   write (io,'(a)') 'RADIUS=2*(1-COS(ANGLE))'
   close (unit=io)
end subroutine make_c_qa5
end program qa5

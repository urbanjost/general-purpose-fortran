!*==qa4.f90 processed by SPAG 8.01RF 18:28 12 Dec 2024
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
program qa4
! (LICENSE:Public Domain)
use m_calcomp
implicit none
! 07/30/69
real              :: d, dx, dy
real              :: x(104), y(104)
real              :: xl, yl, rma, rmi, a, tho, thf, di, f, w, rf, ro, sl, sn, h
integer           :: i, j, kin, ipn, klas, nrec, j1, j2, nx, ny, inc, nc, ipen
integer           :: npts
character(len=40) :: msg
integer           :: spag_nextblock_1
equivalence (x(1),xl), (y(1),yl)
   spag_nextblock_1 = 1
   spag_dispatchloop_1: do
!-----------------------------------------------------------------------
   select case (spag_nextblock_1)
!-----------------------------------------------------------------------
   case (1)
!
      call make_c_qa4()
      ! create datafile
      kin = 50
      f = 1.0
      ipn = 2
      call plots(0.0,8.5,0.0,11.0)
      open (unit=kin,file='qa4.dat')
      spag_nextblock_1 = 2
!-----------------------------------------------------------------------
   case (2)
      spag_loop_1_1: do
         read (kin,99001) klas, nrec, msg
99001       format (2(1x,i2,7x),a40)
         if ( klas<0 ) then
         elseif ( klas==0 ) then
            exit spag_loop_1_1
!-----------------------------------------------------------------------
         elseif ( klas==1 ) then
            do i = 1, nrec
               read (kin,99002) xl, yl, tho, thf, ro, rf, di
               call circl(xl,yl,tho,thf,ro,rf,di)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==2 ) then
            do i = 1, nrec
               read (kin,99003) x(1), y(1), npts, inc
99003             format (2(1x,f9.3),1x,i3,7x,i1)
               j1 = inc + 1
               j2 = inc*npts + 1 - inc
               do j = j1, j2, inc
                  read (kin,99002) x(j), y(j)
               enddo
               j = j2 + inc
               x(j) = 0.
               y(j) = 0.
               j = j + inc
               x(j) = 1.
               y(j) = 1.
               call dashl(x,y,npts,inc)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==3 ) then
            do i = 1, nrec
               read (kin,99002) xl, yl, d
               call dashp(xl,yl,d)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==4 ) then
            do i = 1, nrec
               read (kin,99004) xl, yl, rma, rmi, a, tho, thf, ipen
99004             format (7(1x,f9.3),1x,i1)
               call elips(xl,yl,rma,rmi,a,tho,thf,ipen)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==5 ) then
            do i = 1, nrec
               read (kin,99002) x(1), y(1), x(2), y(2), x(3), y(3)
               call fit(x(1),y(1),x(2),y(2),x(3),y(3))
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==6 ) then
            do i = 1, nrec
               read (kin,99005) xl, yl, dx, dy, nx, ny
99005             format (4(1x,f9.3),2(1x,i2,7x))
               call grid(xl,yl,dx,dy,nx,ny)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==7 ) then
            do i = 1, nrec
               read (kin,99002) xl, yl, sl, sn, a
               call poly(xl,yl,sl,sn,a)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==8 ) then
            do i = 1, nrec
               read (kin,99008) xl, yl, h, w, a, ipen
99008             format (5(1x,f9.3),1x,i2)
               call rect(xl,yl,h,w,a,ipen)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==9 ) then
            spag_loop_2_2: do i = 1, nrec
               read (kin,99006) xl, yl, h, msg, inc
99006             format (3(1x,f9.3),a40,1x,i3)
               read (kin,99007) a, nc
99007             format (1x,f9.3,1x,i2)
               if ( inc<0 ) exit spag_loop_2_2
               if ( inc==0 ) then
               endif
               call symbol(xl,yl,h,msg,inc,a,nc)
            enddo spag_loop_2_2
!-----------------------------------------------------------------------
         elseif ( klas==10 ) then
            do i = 1, nrec
               read (kin,99002) xl, yl
               call plot(xl,yl,-3)
            enddo
!-----------------------------------------------------------------------
         elseif ( klas==11 ) then
            do i = 1, nrec
               read (kin,99002) f
               call factor(f)
            enddo
         elseif ( klas==12 .or. klas==15 ) then
!-----------------------------------------------------------------------
         elseif ( klas==13 ) then
            call factor(1.)
            call plot(20.,0.,999)
            close (unit=kin, status='delete')
            stop
!-----------------------------------------------------------------------
         else
            exit spag_loop_1_1
         endif
!-----------------------------------------------------------------------
      enddo spag_loop_1_1
!-----------------------------------------------------------------------
      do i = 1, nrec
         read (kin,99002) x(1), y(1), x(2), y(2), x(3), y(3), x(4), y(4)
         do j = 1, 4
            if ( x(j)==0 ) then
               if ( y(j)==0 ) then
                  ipn = 3
                  cycle
               endif
            endif
            call plot(x(j),y(j),ipn)
            ipn = 2
         enddo
      enddo
      spag_nextblock_1 = 2
      cycle spag_dispatchloop_1
!-----------------------------------------------------------------------
   end select
!-----------------------------------------------------------------------
   enddo spag_dispatchloop_1
99002 format (7(1x,f9.3),f7.1)
!-----------------------------------------------------------------------
contains

subroutine make_c_qa4()
integer, parameter :: io = 40
   open (unit=io,file='qa4.dat')
   write (io,'(a)') '  8         1                RECT'
   write (io,'(a)') ' 1.        1.        9.         7.       0.         3'
   write (io,'(a)') '  9         7                SYMBOL'
   write (io,'(a)') ' 1.5       9.5       .14      SAMPLE OF GENERAL SUBROUTINES PACKAGE    999'
   write (io,'(a)') ' 0.        37'
   write (io,'(a)') ' 2.25      9.        .105     CIRCL                                    999'
   write (io,'(a)') ' 0.         6'
   write (io,'(a)') ' 5.75      9.        .105     ELIPS                                    999'
   write (io,'(a)') ' 0.         5'
   write (io,'(a)') ' 2.25      6.5       .105     FIT, DASHP                               999'
   write (io,'(a)') ' 0.        11'
   write (io,'(a)') ' 5.75      6.5       .105     POLY                                     999'
   write (io,'(a)') ' 0.         4'
   write (io,'(a)') ' 3.75      4.25      .105     GRID, DASHL                              999'
   write (io,'(a)') ' 0.        12'
   write (io,'(a)') ' 2.        1.1       .07      THE BORDER IS DRAWN WITH RECT            999'
   write (io,'(a)') ' 0.        29'
   write (io,'(a)') '  1         3                CIRCL'
   write (io,'(a)') ' 3.25      8.        0.        720.      .75       .25       0.'
   write (io,'(a)') ' 3.25      8.        0.        360.      .75       .25       1.'
   write (io,'(a)') ' 3.35      8.        0.        360.      .85       .85       0.'
   write (io,'(a)') '  4         6                ELIPS'
   write (io,'(a)') ' 6.5       8.        .5        .7        0.        0.        360.      3'
   write (io,'(a)') ' 6.6       8.        .6        .6        0.        0.        360.      3'
   write (io,'(a)') ' 6.7       8.        .7        .5        0.        0.        360.      3'
   write (io,'(a)') ' 6.8       8.        .8        .4        0.        0.        360.      3'
   write (io,'(a)') ' 6.9       8.        .9        .3        0.        0.        360.      3'
   write (io,'(a)') ' 7.        8.        1.        .2        0.        0.        360.      3'
   write (io,'(a)') '            3                DATA'
   write (io,'(a)') ' 0.        0.        1.5       5.        1.5       5.5       2.375     6.'
   write (io,'(a)') ' 3.5       6.125     2.625     5.5       1.5       5.5       0.        0.'
   write (io,'(a)') ' 1.5       5.        2.625     5.        3.5       5.625     0.        0.'
   write (io,'(a)') '  3         1                  DASHP'
   write (io,'(a)') ' 2.375     5.625     .1'
   write (io,'(a)') '            1                  DATA'
   write (io,'(a)') ' 1.5       5.        1.5       5.        1.5       5.        0.       0'
   write (io,'(a)') '  3         2                  DASHP'
   write (io,'(a)') ' 2.375     5.625     .1'
   write (io,'(a)') ' 2.375     6.125     .1'
   write (io,'(a)') '  5         2                FIT'
   write (io,'(a)') ' 2.625     5.        2.5       5.25      2.625     5.5'
   write (io,'(a)') ' 3.5       5.625     3.375     5.875     3.5       6.125'
   write (io,'(a)') '  7        10                  POLY'
   write (io,'(a)') ' 5.75      5.        .35       3.        0.'
   write (io,'(a)') ' 5.75      5.        .35       4.        0.'
   write (io,'(a)') ' 5.75      5.        .35       5.        0.'
   write (io,'(a)') ' 5.75      5.        .35       6.        0.'
   write (io,'(a)') ' 5.75      5.        .35       7.        0.'
   write (io,'(a)') ' 5.75      5.        .35       8.        0.'
   write (io,'(a)') ' 5.75      5.        .35       9.        0.'
   write (io,'(a)') ' 5.75      5.        .35       10.       0.'
   write (io,'(a)') ' 5.75      5.        .35       11.       0.'
   write (io,'(a)') ' 5.75      5.        .35       12.       0.'
   write (io,'(a)') '  6         2                GRID'
   write (io,'(a)') ' 1.5       2.        .25       .25       24         8'
   write (io,'(a)') ' 1.51      1.99      1.5       1.         4         2'
   write (io,'(a)') '  2         1                DASHL'
   write (io,'(a)') ' 1.75      2.25       11       1'
   write (io,'(a)') ' 2.5       3.75'
   write (io,'(a)') ' 2.75      3.25'
   write (io,'(a)') ' 3.        3.5'
   write (io,'(a)') ' 3.5       2.75'
   write (io,'(a)') ' 4.        2.5'
   write (io,'(a)') ' 4.25      3.25'
   write (io,'(a)') ' 5.25      2.75'
   write (io,'(a)') ' 5.5       3.75'
   write (io,'(a)') ' 6.5       2.5'
   write (io,'(a)') ' 7.25      3.5'
   write (io,'(a)') ' 13                           END'
   close (unit=io)
end subroutine make_c_qa4

end program qa4

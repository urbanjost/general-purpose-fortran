program qa4
! (LICENSE:Public Domain)
   use M_calcomp
   implicit none
   ! 07/30/69
   real :: x, y, d, dx, dy
   dimension x(104), y(104)
   real :: xl, yl, rma, rmi, a, tho, thf, di, f, w, rf, ro, sl, sn, h
   integer :: i, j, kin, ipn, klas, nrec, j1, j2, nx, ny, inc, nc, ipen
   integer :: npts
   character*40 msg
   equivalence(x(1), xl), (y(1), yl)
9001 format(2(1x, i2, 7x), a40)
9007 format(7(1x, f9.3), f7.1)
9009 format(2(1x, f9.3), 1x, i3, 7x, i1)
9012 format(7(1x, f9.3), 1x, i1)
9014 format(4(1x, f9.3), 2(1x, i2, 7x))
9016 format(3(1x, f9.3), a40, 1x, i3)
9017 format(1x, f9.3, 1x, i2)
9021 format(5(1x, f9.3), 1x, i2)
!
   call make_c_qa4()   ! create datafile
   kin = 50
   f = 1.0
   ipn = 2
   call plots(0.0, 8.5, 0.0, 11.0)
!-----------------------------------------------------------------------
   open (unit=kin, file='qa4.dat')
1  continue
   read (kin, 9001) klas, nrec, msg
   if (klas) 1, 100, 40
40 goto(200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1, 1400, 1500, 1), klas
!-----------------------------------------------------------------------
100 continue
   do i = 1, nrec
      read (kin, 9007) x(1), y(1), x(2), y(2), x(3), y(3), x(4), y(4)
      do j = 1, 4
         if (x(j)) 140, 120, 140
120      if (y(j)) 140, 130, 140
130      ipn = 3
         cycle
140      call plot(x(j), y(j), ipn)
         ipn = 2
      enddo
   enddo
   goto 1
!-----------------------------------------------------------------------
200 continue
   do i = 1, nrec
      read (kin, 9007) xl, yl, tho, thf, ro, rf, di
      call circl(xl, yl, tho, thf, ro, rf, di)
   enddo
   goto 1
!-----------------------------------------------------------------------
300 continue
   do i = 1, nrec
      read (kin, 9009) x(1), y(1), npts, inc
      j1 = inc + 1
      j2 = inc*npts + 1 - inc
      do j = j1, j2, inc
         read (kin, 9007) x(j), y(j)
      enddo
      j = j2 + inc
      x(j) = 0.
      y(j) = 0.
      j = j + inc
      x(j) = 1.
      y(j) = 1.
      call dashl(x, y, npts, inc)
   enddo
   goto 1
!-----------------------------------------------------------------------
400 continue
   do i = 1, nrec
      read (kin, 9007) xl, yl, d
      call dashp(xl, yl, d)
   enddo
   goto 1
!-----------------------------------------------------------------------
500 continue
   do i = 1, nrec
      read (kin, 9012) xl, yl, rma, rmi, a, tho, thf, ipen
      call elips(xl, yl, rma, rmi, a, tho, thf, ipen)
   enddo
   goto 1
!-----------------------------------------------------------------------
600 continue
   do i = 1, nrec
      read (kin, 9007) x(1), y(1), x(2), y(2), x(3), y(3)
      call fit(x(1), y(1), x(2), y(2), x(3), y(3))
   enddo
   goto 1
!-----------------------------------------------------------------------
700 continue
   do i = 1, nrec
      read (kin, 9014) xl, yl, dx, dy, nx, ny
      call grid(xl, yl, dx, dy, nx, ny)
   enddo
   goto 1
!-----------------------------------------------------------------------
800 continue
   do i = 1, nrec
      read (kin, 9007) xl, yl, sl, sn, a
      call poly(xl, yl, sl, sn, a)
   end do
   goto 1
!-----------------------------------------------------------------------
900 continue
   do i = 1, nrec
      read (kin, 9021) xl, yl, h, w, a, ipen
      call rect(xl, yl, h, w, a, ipen)
   end do
   goto 1
!-----------------------------------------------------------------------
1000 continue
   do i = 1, nrec
      read (kin, 9016) xl, yl, h, msg, inc
      read (kin, 9017) a, nc
      if (inc) 1, 1030, 1040
1040  continue
1030  continue
      call symbol(xl, yl, h, msg, inc, a, nc)
   enddo
   goto 1
!-----------------------------------------------------------------------
1100 continue
   do i = 1, nrec
      read (kin, 9007) xl, yl
      call plot(xl, yl, -3)
   enddo
   goto 1
!-----------------------------------------------------------------------
1200 continue
   do i = 1, nrec
      read (kin, 9007) f
      call factor(f)
   enddo
   goto 1
!-----------------------------------------------------------------------
1400 continue
   call factor(1.)
   call plot(20., 0., 999)
   stop
!-----------------------------------------------------------------------
1500 continue
   do i = 1, nrec
   end do
   goto 1
!-----------------------------------------------------------------------
contains

   subroutine make_c_qa4()
      integer, parameter :: io = 40
      open (unit=io, file='qa4.dat')
      write (io, '(a)') '  8         1                RECT'
      write (io, '(a)') ' 1.        1.        9.         7.       0.         3'
      write (io, '(a)') '  9         7                SYMBOL'
      write (io, '(a)') ' 1.5       9.5       .14      SAMPLE OF GENERAL SUBROUTINES PACKAGE    999'
      write (io, '(a)') ' 0.        37'
      write (io, '(a)') ' 2.25      9.        .105     CIRCL                                    999'
      write (io, '(a)') ' 0.         6'
      write (io, '(a)') ' 5.75      9.        .105     ELIPS                                    999'
      write (io, '(a)') ' 0.         5'
      write (io, '(a)') ' 2.25      6.5       .105     FIT, DASHP                               999'
      write (io, '(a)') ' 0.        11'
      write (io, '(a)') ' 5.75      6.5       .105     POLY                                     999'
      write (io, '(a)') ' 0.         4'
      write (io, '(a)') ' 3.75      4.25      .105     GRID, DASHL                              999'
      write (io, '(a)') ' 0.        12'
      write (io, '(a)') ' 2.        1.1       .07      THE BORDER IS DRAWN WITH RECT            999'
      write (io, '(a)') ' 0.        29'
      write (io, '(a)') '  1         3                CIRCL'
      write (io, '(a)') ' 3.25      8.        0.        720.      .75       .25       0.'
      write (io, '(a)') ' 3.25      8.        0.        360.      .75       .25       1.'
      write (io, '(a)') ' 3.35      8.        0.        360.      .85       .85       0.'
      write (io, '(a)') '  4         6                ELIPS'
      write (io, '(a)') ' 6.5       8.        .5        .7        0.        0.        360.      3'
      write (io, '(a)') ' 6.6       8.        .6        .6        0.        0.        360.      3'
      write (io, '(a)') ' 6.7       8.        .7        .5        0.        0.        360.      3'
      write (io, '(a)') ' 6.8       8.        .8        .4        0.        0.        360.      3'
      write (io, '(a)') ' 6.9       8.        .9        .3        0.        0.        360.      3'
      write (io, '(a)') ' 7.        8.        1.        .2        0.        0.        360.      3'
      write (io, '(a)') '            3                DATA'
      write (io, '(a)') ' 0.        0.        1.5       5.        1.5       5.5       2.375     6.'
      write (io, '(a)') ' 3.5       6.125     2.625     5.5       1.5       5.5       0.        0.'
      write (io, '(a)') ' 1.5       5.        2.625     5.        3.5       5.625     0.        0.'
      write (io, '(a)') '  3         1                  DASHP'
      write (io, '(a)') ' 2.375     5.625     .1'
      write (io, '(a)') '            1                  DATA'
      write (io, '(a)') ' 1.5       5.        1.5       5.        1.5       5.        0.       0'
      write (io, '(a)') '  3         2                  DASHP'
      write (io, '(a)') ' 2.375     5.625     .1'
      write (io, '(a)') ' 2.375     6.125     .1'
      write (io, '(a)') '  5         2                FIT'
      write (io, '(a)') ' 2.625     5.        2.5       5.25      2.625     5.5'
      write (io, '(a)') ' 3.5       5.625     3.375     5.875     3.5       6.125'
      write (io, '(a)') '  7        10                  POLY'
      write (io, '(a)') ' 5.75      5.        .35       3.        0.'
      write (io, '(a)') ' 5.75      5.        .35       4.        0.'
      write (io, '(a)') ' 5.75      5.        .35       5.        0.'
      write (io, '(a)') ' 5.75      5.        .35       6.        0.'
      write (io, '(a)') ' 5.75      5.        .35       7.        0.'
      write (io, '(a)') ' 5.75      5.        .35       8.        0.'
      write (io, '(a)') ' 5.75      5.        .35       9.        0.'
      write (io, '(a)') ' 5.75      5.        .35       10.       0.'
      write (io, '(a)') ' 5.75      5.        .35       11.       0.'
      write (io, '(a)') ' 5.75      5.        .35       12.       0.'
      write (io, '(a)') '  6         2                GRID'
      write (io, '(a)') ' 1.5       2.        .25       .25       24         8'
      write (io, '(a)') ' 1.51      1.99      1.5       1.         4         2'
      write (io, '(a)') '  2         1                DASHL'
      write (io, '(a)') ' 1.75      2.25       11       1'
      write (io, '(a)') ' 2.5       3.75'
      write (io, '(a)') ' 2.75      3.25'
      write (io, '(a)') ' 3.        3.5'
      write (io, '(a)') ' 3.5       2.75'
      write (io, '(a)') ' 4.        2.5'
      write (io, '(a)') ' 4.25      3.25'
      write (io, '(a)') ' 5.25      2.75'
      write (io, '(a)') ' 5.5       3.75'
      write (io, '(a)') ' 6.5       2.5'
      write (io, '(a)') ' 7.25      3.5'
      write (io, '(a)') ' 13                           END'
      close (unit=io)
   end subroutine make_c_qa4

end program qa4

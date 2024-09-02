program qa3
! (LICENSE:Public Domain)
   use M_calcomp
   implicit none
   character*28 ichr1
   character*26 ichr2
   character*4 ichr3, ichr4
   character*19 ichr5, ichr7
   character*21 ichr6
   character*17 ichr8
   character*10 lbcd1, lbcd2
   real :: angle, height, xx, yy, deltax, age, rad, value, x, y
   real :: xarray, yarray
   dimension xarray(62), yarray(62)
   integer :: i10, i, j, inteq
   ichr1 = 'PLOTTED ON A CALCOMP PLOTTER'
   ichr2 = 'USING  Y = X -0.7*X +0.1*X'
   ichr3 = 'ANG='
   ichr4 = ', H='
   ichr5 = 'ANGULAR LETTER TEST'
   ichr6 = 'CAR MODEL AGE (YEARS)'
   ichr7 = 'CAR VALUE (DOLLARS)'
   ichr8 = 'AVERAGE CAR VALUE'
   lbcd1 = 'X-ABSCISSA'
   lbcd2 = 'Y-ORDINATE'
   call plots(0.0, 8.5, 0.0, 11.0)
! PLOT THREE GRAPHS ILLUSTRATING SCALE, AXIS, AND LINE
   deltax = 0.04
   do 110 i = 1, 3
! AXIS DRAWS LABELS AS MUCH AS 0.4 INCHES TO THE NEGATIVE OF AXIS CENTER;
! EITHER USE AN ORIGIN OFFSET OF AT LEAST THIS VALUE OR DO NOT USE AN
! ORIGIN VALUE OF LESS THAN 0.4 OR CLIPPING WILL OCCUR
      call plot(0.4, 0.4, -3)
      deltax = 2.0*deltax
      xarray(1) = deltax
      do 105 j = 1, 60
         yarray(j) = xarray(j)**2 - 0.7*xarray(j)**3 + 0.1*xarray(j)**4
105      xarray(j + 1) = xarray(j) + deltax
         call scale(xarray(1), 6.5, 60, 1)
         call scale(yarray(1), 10.0, 60, 1)
         call axis(0.0, 0.0, lbcd1, -10, 6.5, 0.0, xarray(61), xarray(62))
         call axis(0.0, 0.0, lbcd2, 10, 10.0, 90.0, yarray(61), yarray(62))
         call newpen(i)
         call line(xarray(1), yarray(1), 60, 1, 2*(i - 2), i)
         call newpen(1)
         call symbol(1.3, 10., .14, ichr1, inteq, 0.0, 28)
         call symbol(1.3, 9.7, .14, ichr2, inteq, 0.0, 26)
         call number(2.98, 9.8, .1, 2.0, 0., -1)
         call number(3.96, 9.8, .1, 3.0, 0., -1)
         call number(4.94, 9.8, .1, 4.0, 0., -1)
!110     CALL PLOT(10.0,0.0,-3)
110      call nframe()
! PLOT ANGULAR LETTER TEST
         call plot(4.5, 5.5, -3)
         angle = 0.0
         height = 0.105
         do 120 i = 1, 8
            call newpen(i)
            rad = 0.0174533*angle
            xx = 0.5*cos(rad)
            yy = 0.5*sin(rad)
            call symbol(xx, yy, height, ichr3, inteq, angle, 4)
            call number(999.0, 999.0, height, angle, angle, -1)
            call symbol(999.0, 999.0, height, ichr4, inteq, angle, 4)
            call number(999.0, 999.0, height, height, angle, 3)
            height = height + 0.035
            angle = angle + 45.0
120         continue
            call newpen(1)
            call symbol(-1.4, 4.0, 0.14, ichr5, inteq, 0.0, 19)
            call plot(4.5, 5.0, 3)
            call plot(-4.5, 5.0, 2)
            call plot(-4.5, -5.5, 2)
            call plot(4.5, -5.5, 2)
            call plot(4.5, 5.0, 2)
!     CALL PLOT( 6.5,-5.5,-3)
            call nframe()
!     CALL TO SYMBOL USES -0.5Y, -0.8-.14  X
!     (-.14 FOR CHARACTER HEIGHT)
            call plot(0.95, 0.5, -3)
! PLOT CAR VALUE CHART WITHOUT USING SCALE,AXIS,OR LINE
            x = 1.0
! PLOT X-AXIS
            do 130 i = 1, 7
               call plot(x - 1.0, 0.0, 3)
               call plot(x, 0.0, 2)
               call plot(x, -0.1, 2)
               call number(x - .02, -0.25, 0.1, x, 0.0, -1)
130            x = x + 1.0
               call symbol(2.0, -0.5, 0.14, ichr6, inteq, 0.0, 21)
! PLOT Y-AXIS
               value = 1000.0
               do 140 i = 1, 6
                  y = 0.0015*value
                  call plot(0.0, y - 1.5, 3)
                  call plot(0.0, y - .75, 2)
                  call plot(-.1, y - .75, 2)
                  call plot(0.0, y - .75, 2)
                  call plot(0.0, y, 2)
                  call plot(-.1, y, 2)
                  call number(-0.7, y, 0.14, value, 0.0, -1)
140               value = value + 1000.0
                  call symbol(-0.8, 3.1, 0.14, ichr7, inteq, 90.0, 19)
! PLOT CURVES
                  call newpen(2)
                  do 150 i = 2000, 6000, 500
                     value = i
                     age = 0.0
                     call plot(age, 0.0015*value, 3)
                     do 150 j = 1, 84
                        value = value*0.972
                        age = age + 0.08333
150                     call plot(age, 0.0015*value, 2)
                        call newpen(3)
                        call symbol(3.0, 6.0, 0.21, ichr8, inteq, 0.0, 17)
                        call plot(0.0, 0.0, 999)
                        stop
                        end program qa3

program qa7
! (LICENSE:Public Domain)
   use M_calcomp
   implicit none
   real :: xarray, yarray
   dimension xarray(20), yarray(20)
   integer :: i, k
   real :: x, y
   data xarray/0.0, 2.5, 5.0, 10.0, 16.0, 20.0, 23.0, 28.0, 35.0, 40.0, 44.0, 51.0, 60.0, 64.0, 69.0, 75.0, 82.0, 88.0, 00.0, 00.0/
  data yarray/0.0,-20.0,-48.0,-70.0,-98.0,-110.0,-125.0,-142.0, -130.0,-115.0,-80.0,-35.0,-10.0,5.0,22.0,35.0,42.0,58.0,00.0, 000.0/
!    CHECK RELATIVE FRAME SIZING
!    USING 1 AND 2 MAKE SURE MAXIMUM VALUE BECOMES UPPER LEFT CORNER
!    OF PLOT AND USING 3 MAKE SURE LARGE ORIGIN OFFSETS ARE WORKING
   call plots(0.0, 8.5, 0.0, 11.0)
   do i = 1, 3
      call plot(0.5, 0.5, -3)
      call scale(xarray, 5.0, 18, 1)
      call scale(yarray, 6.0, 18, 1)
      if (i .le. 2) then
         if (i .eq. 1) then
            x = 6.0
            y = 7.0
         else
            x = 8.0
            y = 9.5
         endif
         call plot(x, 0.0, 2)
         call plot(x, y, 2)
         call plot(0.0, y, 2)
         call plot(0.0, 0.0, 2)
      else
         call plot(22.5, 13.5, -3)
      endif
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      call axis(0.0, 0.0, 'TIME IN MILLISECONDS', -20, 5.0, 0.0, xarray(19), xarray(20))
      call axis(0.0, 0.0, 'VOLTAGE', 7, 6.0, 90.0, yarray(19), yarray(20))
      call line(xarray, yarray, 18, 1, 2, 4)
      call symbol(0.5, 5.6, 0.21, 'PERFORMANCE TST1', 999, 0.0, 16)
      call symbol(0.5, 5.2, 0.14, 'REF. NO. 1623-46', 999, 0.0, 16)
      call nframe()
   end do
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! CHECK OUT INTENSITY SETTINGS WITH HARDWARE CHARACTERS
   call mset("HARD")
   call mset("XLAR")
   y = .25
   call plot(0., 0., -3)
   call plot(10., 0., 2)
   call plot(10., 10., 2)
   call plot(0., 10., 2)
   call plot(0., 0., 2)
   do k = 1, 100, 13
      call setpar('INTE', k)
      call plot(.5, y, 3)
      call plot(5., y, 2)
      call symbol(5.1, y, 7.0, 'AMR', 999, 0., 3)
      y = y + .5
   end do
   call nframe()
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     TEST HARDWARE TEXT SIZES
!
   call mset("HARD")
   call mset("MEDI")
   call plot(0.0, 0.0, -3)
   call plot(28., 0.0, 2)
   call plot(28., 20., 2)
   call plot(0., 20., 2)
   call plot(0., 0., 2)
!
   y = 0.
   do i = 19, 30
      call mpset('TSIZ', float(i))
      call symbol(1., y, .2, 'TEST @@@@@\\\\\ END ', 999, 0., 20)
      y = y + .31
   enddo
   call mset("SOFT")
   call symbol(1., 10., .50, '.50 INCH CHARACTERS ', -1, 0.0, 20)
   call symbol(1., 11., .50, '.50 INCH CHARACTERS ', 999, 0.0, 20)
   call nframe()
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!        A COMPILED COPY OF THE FR80 DUMPER IS AVAILABLE; JUST MAKE
!        PLTOUT BY USING THE NODISP OPTION ON THE COM PROCEDURES AND
!        THEN RUN THE FOLLOWING JCL:
!
!        REWIND,DN=PLTOUT.
!        COPYF,I=PLTOUT,O=FT01.
!        REWIND,DN=FT01.
!        ATTACH,DN=DUMP,PDN=CR80DMP,ID=OSCRI.
!        DUMP.
!     TEST THE USE OF HARDWARE CHARACTERS ON THE COM UNITS
!
   call mset("HARD")
   call mset("MEDI")
   call plot(0.0, 0.0, -3)
   call plot(28., 0.0, 2)
   call plot(28., 20., 2)
   call plot(0., 20., 2)
   call plot(0., 0., 2)
!
   y = 0.0
   do i = 1, 64
      call mpset('TSIZ', float(i))
      call symbol(1., y, .2, 'TEST @@@@@\\\\\ END ', 999, 0., 20)
      y = y + .31
   enddo
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   call plot(0.0, 0.0, 999)
end program qa7

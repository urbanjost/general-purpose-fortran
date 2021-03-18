program qa6
! (LICENSE:Public Domain)
   use m_calcomp
   dimension totx(10),toty(10),am(10,10),cv(10)
   !
   kin=50
   call make_c_qa6()
   open(unit=kin,file='qa6.dat')
   !
   read (kin,'(3I6)')nx,ny,ncv
   write (6,900)nx,ny,ncv
   read (kin,810)(totx(i),i=1,nx)
   write (6,910)(totx(i),i=1,nx)
   read (kin,810)(toty(i),i=1,ny)
   write (6,920)(toty(i),i=1,ny)
   write (6,'("0AM")')
   !
   do j=1,ny
      read (kin,810)(am(i,j),i=1,nx)
      write (6,940)(am(i,j),i=1,nx)
   enddo
   !
   read (kin,810)(cv(i),i=1,ncv)
   write (6,950)(cv(i),i=1,ncv)
   ! plotting
   call plots(0.0,8.5,0.0,11.0)
   call plot(0.,1.,-3)
   call cntour(am,nx,ny,totx,toty,3.0,cv,ncv,.true.,10)
   call plot(0.0,0.0,999)
810 format(8F6.1)
900 format('1','NX =',I2,'NY =',I2,'NCV =',I2)
910 format('0','TOTX',/,(' ',5X,10F10.1))
920 format('0','TOTY',/(' ',5X,10F10.1))
940 format('0',5X,10F10.1)
950 format('0','CV',/,(' ',10F10.1))
   stop
contains
   subroutine make_c_qa6()
      ! create test input file
      integer,parameter :: io=40
      open(unit=io,file='qa6.dat')
      write(io,'(a)')'     6     7     8'
      write(io,'(a)')'0.    3.    6.    8.    10.   12.'
      write(io,'(a)')'0.    2.    3.    5.    7.    8.    10.'
      write(io,'(a)')'0.    0.    0.    0.    0.    0.'
      write(io,'(a)')'20.   25.   29.   27.   23.   20.'
      write(io,'(a)')'30.   39.   48.   42.   36.   30.'
      write(io,'(a)')'50.   62.   94.   69.   57.   50.'
      write(io,'(a)')'70.   79.   89.   82.   76.   70.'
      write(io,'(a)')'80.   84.   88.   86.   83.   80.'
      write(io,'(a)')'100.  100.  100.  100.  100.  100.'
      write(io,'(a)')'90.   80.   70.   60.   50.   40.   30.   20.'
      close(unit=io)
   end subroutine make_c_qa6
end program qa6

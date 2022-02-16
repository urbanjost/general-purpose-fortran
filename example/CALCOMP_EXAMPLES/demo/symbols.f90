program qa2
! (LICENSE:Public Domain)
use M_calcomp, only : plots, plot, symbol, number, nframe
use M_calcomp, only : MOVE, DRAW
!
!     THIS ROUTINE PRODUCES A SYMBOL TABLE, WHICH SHOWS THE CHARACTERS
!     AVAILABLE IN THE SYMBOL ROUTINE.
!
   character*38 ichr1,ichr2
   character*60 ichr3
   character*28 ichr4
   character*1 ibcd
   dimension znum(4)
   data znum /10293.84756,193.75,-204.86,-12345.6789/
   ichr1='CHARACTERS AVAILABLE IN SYMBOL ROUTINE'
   ichr2='  FOR CALCOMP'
   ichr3='INTEGER FOR USE IN SYMBOL CALL SHOWN TO LEFT OF EACH SYMBOL'
   ichr4='EXAMPLE OF NUMBER SUBROUTINE'
   call plots(0.0, 7.0, 0.0, 9.0)
   !!call grid(0.0, 0.0, 0.5, 0.5, 18, 23)

   call plot(0.8,0.8,1001)

   call plot(0.0,11.0,2)
   call plot (8.5,11.0,2)
   call plot (8.5,0.0,2)
   call plot (0.0,0.0,2)
   call symbol(0.4,10.50,.2,ichr1,inteq,0.0,38)
   call symbol(0.4,10.25,.2,ichr2,inteq,0.0,38)
   call plot(8.1,10.0,3)
   call plot(0.4,10.0,2)
   call plot(0.4, 0.5,2)
   call plot(8.1, 0.5,2)
   z=0.0
   m=0
   xs=0.85
   ys=0.25
   x=0.4
   y=9.5
   do ia=1,6
      do ib=1,15
         call number(x+.10,y+.18,.14,z,0.0,-1)
         call symbol(x+xs,y+ys ,.4 ,ibcd,m,0.0,-1)
         z=z+1.0
         m=m+1
         y=y-0.6
      enddo
      if (ia.eq.6) call number (x+.10,y+.18,.14,z,0.0,-1)
      if (ia.eq.6) call symbol (x+xs,y+ys,.4,ibcd,m,0.0,-1)
      x=x+1.283
      call plot(x,0.5,3)
      call plot(x,10.,2)
      y=9.5
      xs=.65
      ys=.05
   enddo
   call symbol(0.6,.25,.12,ichr3,inteq,0.0,60)
!     call plot(10.0,0.0,-3)
   call nframe()
! THE FOLLOWING TESTS THE NUMBER SUBROUTINE FOR PRECISION
   call symbol(0.5,2.5,.20,ichr4,inteq,90.0,28)
   y=10.0
   do ia=1,4
      do ib=1,11
         call number(1.0,y,.14,znum(ia),0.0,ib-6)
         y=y-0.2
      enddo   
      y=y-0.3
   enddo   
!     call plot ( 5.0,0.0,-3)
   call nframe()
   call plot(0.0,0.0,999)
   stop
end program qa2

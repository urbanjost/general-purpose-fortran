          program demo_M_calcomp
          use M_calcomp
          ! 07/30/69
          real              :: x(104), y(104)
          character(len=40) :: msg
          integer,parameter :: kin = 50
          equivalence(x(1),xl),(y(1),yl)
          9007 format(7(1X,F9.3),F7.1)
             call make_c_qa4()   ! create datafile
             f = 1.0
             ipn = 2
             call plots(0.0,10.0,0.0,10.0)
          !-----------------------------------------------------------------------
             open(unit=kin,file='qa4.dat',action="read")
             NEXTREAD: do
                read(kin,9001) nrec, msg
                9001 format(1X,I2,7X,A40)
                write(*,*)'NREC=',nrec,'MSG=',trim(msg)
                select case(adjustl(msg))
          !-----------------------------------------------------------------------
                 case('DATA')
                   do i = 1,nrec
                      read(kin,9007) x(1),y(1),x(2),y(2),x(3),y(3),x(4),y(4)
                      do j = 1,4
                         if(x(j).eq.0)then
                            if(y(j).eq.0)then
                               ipn = 3
                               cycle
                            endif
                         endif
                         call plot(x(j),y(j),ipn)
                         ipn = 2
                      enddo
                   enddo
          !-----------------------------------------------------------------------
                 case('CIRCL')
                   do i = 1,nrec
                      read(kin,9007) xl,yl,tho,thf,ro,rf,di
                      call circl(xl,yl,tho,thf,ro,rf,di)
                   enddo
          !-----------------------------------------------------------------------
                 case('DASHL')
                   do i = 1,nrec
                      read(kin,9009) x(1),y(1),npts,inc
                      9009 format(2(1X,F9.3),1X,I3,7X,I1)
                      j1 = inc+1
                      j2 = inc*npts+1-inc
                      do j = j1,j2,inc
                         read(kin,9007) x(j),y(j)
                      enddo
                      j = j2+inc
                      x(j) = 0.
                      y(j) = 0.
                      j = j+inc
                      x(j) = 1.
                      y(j) = 1.
                      call dashl(x,y,npts,inc)
                   enddo
          !-----------------------------------------------------------------------
                 case('DASHP')
                   do i = 1,nrec
                      read(kin,9007) xl,yl,d
                      call dashp(xl,yl,d)
                   enddo
          !-----------------------------------------------------------------------
                 case('ELIPS')
                   do i = 1,nrec
                      read(kin,9012) xl,yl,rma,rmi,a,th0,thf,ipen
                      9012 format(7(1X,F9.3),1X,I1)
                      call elips(xl,yl,rma,rmi,a,tho,thf,ipen)
                   enddo
          !-----------------------------------------------------------------------
                 case('FIT')
                   do i = 1,nrec
                      read(kin,9007) x(1),y(1),x(2),y(2),x(3),y(3)
                      call fit(x(1),y(1),x(2),y(2),x(3),y(3))
                   enddo
          !-----------------------------------------------------------------------
                 case('GRID')
                   do i = 1,nrec
                      read(kin,9014) xl,yl,dx,dy,nx,ny
                      9014 format(4(1X,F9.3),2(1X,I2,7X))
                      call grid(xl,yl,dx,dy,nx,ny)
                   enddo
          !-----------------------------------------------------------------------
                 case('POLY')
                   do i = 1,nrec
                      read(kin,9007) xl,yl,sl,sn,a
                      call poly(xl,yl,sl,sn,a)
                   enddo
          !-----------------------------------------------------------------------
                 case('RECT')
                   do i = 1,nrec
                      read(kin,9021) xl,yl,h,w,a,ipen
                      9021 format(5(1X,F9.3),1X,I2)
                      call rect(xl,yl,h,w,a,ipen)
                   enddo
          !-----------------------------------------------------------------------
                 case('SYMBOL')
                   do i = 1,nrec
                      read(kin,9016) xl,yl,h,msg,inc
                      9016 format(3(1X,F9.3), A40,1X,I3)
                      read(kin,9017) a,nc
                      9017 format(1X,F9.3,1X,I2)
                      if(inc.lt.0)cycle NEXTREAD
                      call symbol(xl,yl,h,msg,inc,a,nc)
                   enddo
          !-----------------------------------------------------------------------
                 case('1100')
                   do i = 1,nrec
                      read(kin,9007) xl,yl
                      call plot(xl,yl,-3)
                   enddo
          !-----------------------------------------------------------------------
                 case('FACTOR')
                   do i = 1,nrec
                      read(kin,9007) f
                      call factor(f)
                   enddo
          !-----------------------------------------------------------------------
                 case('END')
                   call factor(1.)
                   call plot(20.,0.,999)
                   exit NEXTREAD
          !-----------------------------------------------------------------------
                 case default
                   write(*,*)'unknown keyword ',trim(msg)
          !-----------------------------------------------------------------------
                end select
          !-----------------------------------------------------------------------
             enddo NEXTREAD
             close(unit=kin,status='delete')
          !-----------------------------------------------------------------------
          contains

          subroutine make_c_qa4()
          integer,parameter :: io=40
          open(unit=io,file='qa4.dat',action="write")
          write(io,'(a)')'  1                RECT'
          write(io,'(a)')' 1.        1.        9.         7.       0.         3'
          write(io,'(a)')'  7                SYMBOL'
          write(io,'(a)')' 1.5       9.5       .14      SAMPLE OF GENERAL SUBROUTINES PACKAGE    999'
          write(io,'(a)')' 0.        37'
          write(io,'(a)')' 2.25      9.        .105     CIRCL                                    999'
          write(io,'(a)')' 0.         6'
          write(io,'(a)')' 5.75      9.        .105     ELIPS                                    999'
          write(io,'(a)')' 0.         5'
          write(io,'(a)')' 2.25      6.5       .105     FIT, DASHP                               999'
          write(io,'(a)')' 0.        11'
          write(io,'(a)')' 5.75      6.5       .105     POLY                                     999'
          write(io,'(a)')' 0.         4'
          write(io,'(a)')' 3.75      4.25      .105     GRID, DASHL                              999'
          write(io,'(a)')' 0.        12'
          write(io,'(a)')' 2.        1.1       .07      THE BORDER IS DRAWN WITH RECT            999'
          write(io,'(a)')' 0.        29'
          write(io,'(a)')'  3                CIRCL'
          write(io,'(a)')' 3.25      8.        0.        720.      .75       .25       0.'
          write(io,'(a)')' 3.25      8.        0.        360.      .75       .25       1.'
          write(io,'(a)')' 3.35      8.        0.        360.      .85       .85       0.'
          write(io,'(a)')'  6                ELIPS'
          write(io,'(a)')' 6.5       8.        .5        .7        0.        0.        360.      3'
          write(io,'(a)')' 6.6       8.        .6        .6        0.        0.        360.      3'
          write(io,'(a)')' 6.7       8.        .7        .5        0.        0.        360.      3'
          write(io,'(a)')' 6.8       8.        .8        .4        0.        0.        360.      3'
          write(io,'(a)')' 6.9       8.        .9        .3        0.        0.        360.      3'
          write(io,'(a)')' 7.        8.        1.        .2        0.        0.        360.      3'
          write(io,'(a)')'  3                DATA'
          write(io,'(a)')' 0.        0.        1.5       5.        1.5       5.5       2.375     6.'
          write(io,'(a)')' 3.5       6.125     2.625     5.5       1.5       5.5       0.        0.'
          write(io,'(a)')' 1.5       5.        2.625     5.        3.5       5.625     0.        0.'
          write(io,'(a)')'  1                  DASHP'
          write(io,'(a)')' 2.375     5.625     .1'
          write(io,'(a)')'  1                  DATA'
          write(io,'(a)')' 1.5       5.        1.5       5.        1.5       5.        0.       0'
          write(io,'(a)')'  2                  DASHP'
          write(io,'(a)')' 2.375     5.625     .1'
          write(io,'(a)')' 2.375     6.125     .1'
          write(io,'(a)')'  2                FIT'
          write(io,'(a)')' 2.625     5.        2.5       5.25      2.625     5.5'
          write(io,'(a)')' 3.5       5.625     3.375     5.875     3.5       6.125'
          write(io,'(a)')' 10                  POLY'
          write(io,'(a)')' 5.75      5.        .35       3.        0.'
          write(io,'(a)')' 5.75      5.        .35       4.        0.'
          write(io,'(a)')' 5.75      5.        .35       5.        0.'
          write(io,'(a)')' 5.75      5.        .35       6.        0.'
          write(io,'(a)')' 5.75      5.        .35       7.        0.'
          write(io,'(a)')' 5.75      5.        .35       8.        0.'
          write(io,'(a)')' 5.75      5.        .35       9.        0.'
          write(io,'(a)')' 5.75      5.        .35       10.       0.'
          write(io,'(a)')' 5.75      5.        .35       11.       0.'
          write(io,'(a)')' 5.75      5.        .35       12.       0.'
          write(io,'(a)')'  2                GRID'
          write(io,'(a)')' 1.5       2.        .25       .25       24         8'
          write(io,'(a)')' 1.51      1.99      1.5       1.         4         2'
          write(io,'(a)')'  1                DASHL'
          write(io,'(a)')' 1.75      2.25       11       1'
          write(io,'(a)')' 2.5       3.75'
          write(io,'(a)')' 2.75      3.25'
          write(io,'(a)')' 3.        3.5'
          write(io,'(a)')' 3.5       2.75'
          write(io,'(a)')' 4.        2.5'
          write(io,'(a)')' 4.25      3.25'
          write(io,'(a)')' 5.25      2.75'
          write(io,'(a)')' 5.5       3.75'
          write(io,'(a)')' 6.5       2.5'
          write(io,'(a)')' 7.25      3.5'
          write(io,'(a)')'                    END'
          close(unit=io)
          end subroutine make_c_qa4

          end program demo_M_calcomp

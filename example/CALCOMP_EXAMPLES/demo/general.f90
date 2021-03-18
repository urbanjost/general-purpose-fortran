PROGRAM QA4
! (LICENSE:Public Domain)
   USE M_calcomp
   ! 07/30/69
   DIMENSION X(104), Y(104)
   CHARACTER*40 MSG
   EQUIVALENCE(X(1),XL),(Y(1),YL)
9001 FORMAT(2(1X,I2,7X),A40)
9007 FORMAT(7(1X,F9.3),F7.1)
9009 FORMAT(2(1X,F9.3),1X,I3,7X,I1)
9012 FORMAT(7(1X,F9.3),1X,I1)
9014 FORMAT(4(1X,F9.3),2(1X,I2,7X))
9016 FORMAT(3(1X,F9.3), A40,1X,I3)
9017 FORMAT(1X,F9.3,1X,I2)
9021 FORMAT(5(1X,F9.3),1X,I2)
!
   call make_c_qa4()   ! create datafile
   KIN = 50
   F = 1.0
   IPN = 2
   CALL PLOTS(0.0,8.5,0.0,11.0)
!-----------------------------------------------------------------------
   open(unit=kin,file='qa4.dat')
1  CONTINUE
   READ(KIN,9001) KLAS,NREC, MSG
   IF(KLAS) 1,100,40
40 GOTO (200,300,400,500,600,700,800,900,1000,1100,1200,1, 1400,1500,1) , KLAS
!-----------------------------------------------------------------------
100 continue
   DO I = 1,NREC
      READ(KIN,9007) X(1),Y(1),X(2),Y(2),X(3),Y(3),X(4),Y(4)
      DO J = 1,4
         IF(X(J)) 140,120,140
120      IF(Y(J)) 140,130,140
130      IPN = 3
         cycle
140      CALL PLOT(X(J),Y(J),IPN)
         IPN = 2
      enddo
   enddo
   GOTO 1
!-----------------------------------------------------------------------
200 continue
   DO I = 1,NREC
      READ(KIN,9007) XL,YL,THO,THF,RO,RF,DI
      CALL CIRCL(XL,YL,THO,THF,RO,RF,DI)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
300 continue
   DO I = 1,NREC
      READ(KIN,9009) X(1),Y(1),NPTS,INC
      J1 = INC+1
      J2 = INC*NPTS+1-INC
      DO J = J1,J2,INC
         READ(KIN,9007) X(J),Y(J)
      enddo
      J = J2+INC
      X(J) = 0.
      Y(J) = 0.
      J = J+INC
      X(J) = 1.
      Y(J) = 1.
      CALL DASHL(X,Y,NPTS,INC)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
400 continue
   DO I = 1,NREC
      READ(KIN,9007) XL,YL,D
      CALL DASHP(XL,YL,D)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
500 continue
   DO I = 1,NREC
      READ(KIN,9012) XL,YL,RMA,RMI,A,TH0,THF,IPEN
      CALL ELIPS(XL,YL,RMA,RMI,A,THO,THF,IPEN)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
600 continue
   DO I = 1,NREC
      READ(KIN,9007) X(1),Y(1),X(2),Y(2),X(3),Y(3)
      CALL FIT(X(1),Y(1),X(2),Y(2),X(3),Y(3))
   enddo
   GOTO 1
!-----------------------------------------------------------------------
700 continue
   DO I = 1,NREC
      READ(KIN,9014) XL,YL,DX,DY,NX,NY
      CALL GRID(XL,YL,DX,DY,NX,NY)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
800 continue
   DO I = 1,NREC
      READ(KIN,9007) XL,YL,SL,SN,A
      CALL POLY(XL,YL,SL,SN,A)
   ENDDO
   GOTO 1
!-----------------------------------------------------------------------
900 continue
   DO I = 1,NREC
      READ(KIN,9021) XL,YL,H,W,A,IPEN
      CALL RECT(XL,YL,H,W,A,IPEN)
   ENDDO
   GOTO 1
!-----------------------------------------------------------------------
1000 continue
   DO I = 1,NREC
      READ(KIN,9016) XL,YL,H,MSG,INC
      READ(KIN,9017) A,NC
      IF(INC) 1,1030,1040
1040  CONTINUE
1030  CONTINUE
      CALL SYMBOL(XL,YL,H,MSG,INC,A,NC)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
1100 continue
   DO I = 1,NREC
      READ(KIN,9007) XL,YL
      CALL PLOT(XL,YL,-3)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
1200 continue
   DO I = 1,NREC
      READ(KIN,9007) F
      CALL FACTOR(F)
   enddo
   GOTO 1
!-----------------------------------------------------------------------
1400 continue
   CALL FACTOR(1.)
   CALL PLOT(20.,0.,999)
   STOP
!-----------------------------------------------------------------------
1500 continue
   DO I = 1,NREC
   ENDDO
   GOTO 1
!-----------------------------------------------------------------------
contains

   subroutine make_c_qa4()
      integer,parameter :: io=40
      open(unit=io,file='qa4.dat')
      write(io,'(a)')'  8         1                RECT'
      write(io,'(a)')' 1.        1.        9.         7.       0.         3'
      write(io,'(a)')'  9         7                SYMBOL'
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
      write(io,'(a)')'  1         3                CIRCL'
      write(io,'(a)')' 3.25      8.        0.        720.      .75       .25       0.'
      write(io,'(a)')' 3.25      8.        0.        360.      .75       .25       1.'
      write(io,'(a)')' 3.35      8.        0.        360.      .85       .85       0.'
      write(io,'(a)')'  4         6                ELIPS'
      write(io,'(a)')' 6.5       8.        .5        .7        0.        0.        360.      3'
      write(io,'(a)')' 6.6       8.        .6        .6        0.        0.        360.      3'
      write(io,'(a)')' 6.7       8.        .7        .5        0.        0.        360.      3'
      write(io,'(a)')' 6.8       8.        .8        .4        0.        0.        360.      3'
      write(io,'(a)')' 6.9       8.        .9        .3        0.        0.        360.      3'
      write(io,'(a)')' 7.        8.        1.        .2        0.        0.        360.      3'
      write(io,'(a)')'            3                DATA'
      write(io,'(a)')' 0.        0.        1.5       5.        1.5       5.5       2.375     6.'
      write(io,'(a)')' 3.5       6.125     2.625     5.5       1.5       5.5       0.        0.'
      write(io,'(a)')' 1.5       5.        2.625     5.        3.5       5.625     0.        0.'
      write(io,'(a)')'  3         1                  DASHP'
      write(io,'(a)')' 2.375     5.625     .1'
      write(io,'(a)')'            1                  DATA'
      write(io,'(a)')' 1.5       5.        1.5       5.        1.5       5.        0.       0'
      write(io,'(a)')'  3         2                  DASHP'
      write(io,'(a)')' 2.375     5.625     .1'
      write(io,'(a)')' 2.375     6.125     .1'
      write(io,'(a)')'  5         2                FIT'
      write(io,'(a)')' 2.625     5.        2.5       5.25      2.625     5.5'
      write(io,'(a)')' 3.5       5.625     3.375     5.875     3.5       6.125'
      write(io,'(a)')'  7        10                  POLY'
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
      write(io,'(a)')'  6         2                GRID'
      write(io,'(a)')' 1.5       2.        .25       .25       24         8'
      write(io,'(a)')' 1.51      1.99      1.5       1.         4         2'
      write(io,'(a)')'  2         1                DASHL'
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
      write(io,'(a)')' 13                           END'
      close(unit=io)
   end subroutine make_c_qa4

END PROGRAM QA4

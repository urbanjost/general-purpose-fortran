










!-------------------------------------------------------------------------------
subroutine dparanoia()
implicit none
character(len=*),parameter :: ident="@(#)dparanoia(3f): test doubleprecisions operations in programming environment"

!!!!!   COMMON /STDIO/ IN, OUT
        INTEGER IN, OUT
!!!!!   COMMON /PGNUM/PGNUMB
        INTEGER       PGNUMB
!!!!!   COMMON /ROUNDN/  R1, R2, R3, R4, STICKY
        DOUBLE PRECISION R1, R2, R3, R4, STICKY
!
!!!!!   COMMON /I3TYPE/ IEEE
        INTEGER         IEEE
!        IEEE   ... FLAG WHETHER GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED
!
!!!!!   COMMON /GLOBAL/ FAILS, SDEFCT, DEFECT, FLAWS, RADIX, ULPPLS,
!!!!!~        ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        INTEGER         FAILS, SDEFCT, DEFECT, FLAWS
        DOUBLE PRECISION RADIX, ULPPLS, ULPMIN, PRECIS, W, MULGRD, DIVGRD, SUBGRD, A1, ONEMIN
        LOGICAL FLAGF
!
!        FAILS  ... NUMBER OF FAILURES
!        SDEFCT ... NUMBER OF SERIOUS DEFECTS
!        DEFECT ... NUMBER OF DEFECTS
!        FLAWS  ... NUMBER OF FLAWS
!
!        RADIX  ... COMPUTED RADIX OF THE MACHINE
!        ULPPLS ... ONE UNIT IN THE LAST PLACE (ULP) OF 1+EPSILON
!        ULPMIN ... ONE UNIT IN THE LAST PLACE (ULP) OF 1-EPSILON
!        PRECIS ... COMPUTED PRECISION OF THE MACHINE
!        W      ... RADIX ** PRECIS
!        MULGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN MULTIPLY
!        DIVGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN DIVIDE
!        SUBGRD ... USED TO TEST FOR USE OF GUARD DIGITS IN ADD/SUBTRACT
!        A1     ...
!        ONEMIN ... ONE MINUS ULPMIN = ~.99999999999999
!
!!!!!   COMMON /CONST/   FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32,
!!!!!~                   HALF, MINUS1
        DOUBLE PRECISION FP0, FP1, FP2, FP3, FP4, FP8, FP9, FP27, FP32, HALF, MINUS1
!
!!!!!   COMMON /ERRDAT/ OVRFLW, UNDFLW, DIVZER, ERRFLG
!       INTEGER         OVRFLW, UNDFLW, DIVZER, ERRFLG
!        OVRFLW ... NUMBER OF OVERFLOWS SINCE LAST RESET
!        UNDFLW ... NUMBER OF UNDERFLOWS SINCE LAST RESET
!        DIVZER ... NUMBER OF DIVISIONS BY ZERO SINCE LAST RESET
!        ERRFLG ... FLAG THAT IS SET WHENEVER AN ERROR OCCURS
!
!
!!!!!   COMMON /OVUNFL/  C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
        DOUBLE PRECISION C1, H1, MINPOS, NULPS, UFLTHR, PHONY0
!        C1     ... 1/C ~= RADIX^LARGE_INTEGER
!        H1     ... MAX (2,RADIX)
!        MINPOS ... MINIMUM POSITIVE NUMBER FOUND BY MULT./DIVISION
!        NULPS  ... (SMALL INTEGER) * 1 UNIT IN LAST PLACE OF 1+ ... E9
!        UFLTHR ... THE UNDERFLOW THRESHOLD U0
!        PHONY0 ... CANDIDATE AND FINAL PHONY ZERO IF IT EXISTS. Z0
!
!#######################################################################
        call runtests()
        close(3,status='delete')
        close(4,status='delete')
        contains

!-------------------------------------------------------------------------------
        subroutine runtests()
!#######################################################################
!
!       CHECK FOR RESTART
!
!#######################################################################
        INTEGER START
!        ... FLAG TO TELL WHETHER WE ARE RESTARTING OR
!        ... STARTING FROM SCRATCH
        INTEGER TEMP
        INTEGER MILES, NUMTRY, FROM
!        MILES  ... NUMBER OF MILESTONE REACHED SO FAR IN TESTING
!        NUMTRY ... NUMBER OF TIMES TO TRY RANDOM TRIALS
!        FROM   ... NUMBER OF MILESTONE TO RETURN TO ON RESTART
!
! DISABLE INTERRUPTS FOR LAHEY F77L.
!       LOGICAL FLAG
!       CALL INVALOP(FLAG)
!       CALL OVEFL(FLAG)
!       CALL UNDFL(FLAG)
!       CALL DVCHK(FLAG)
!
! DISABLE INTERRUPTS FOR MICROSOFT FORTRAN  (MASK MUST BE INTEGER*2)
!       INTEGER*2 MASK
!       MASK=4927
!       CALL LCWRQQ(MASK)
! DISABLE INTERRUPTS FOR WATFIV
!       CALL TRAPS(100,100,100,100,100)
! NO SPECIAL CALL IS REQUIRED FOR FORTVS.
!
        FROM = 0
!
! IN = INPUT UNIT, OUT = OUTPUT UNIT -- YOU MAY HAVE TO CHANGE THESE.
        IN = 5
        OUT = 6
! THE FOLLOWING OPENS MAY BE NEEDED FOR SOME VERSIONS.
!       OPEN(IN,FILE='CON')
!       OPEN(OUT,FILE='CON')
        WRITE(OUT,100)
        WRITE(OUT,110)
100     FORMAT(' Is this a program restart after failure (1)')
110     FORMAT(' or a start from scratch (0) ?')
        READ(IN,120) START
120     FORMAT(I1)
        FLAGF = START .NE. 0
!  ***  FOR FORTRAN 66 AND FORTRAN 77 SUBSET, COMMENT OUT THE INQUIRE:
        INQUIRE(FILE='TST',EXIST=FLAGF)
        IF(FLAGF) THEN
            OPEN(3,FILE='TST',FORM='UNFORMATTED',STATUS='OLD')
            REWIND 3
            OPEN(4,FILE='LOG',FORM='UNFORMATTED',STATUS='OLD')
            REWIND 4
        ELSE
            OPEN(3,FILE='TST',FORM='UNFORMATTED',STATUS='NEW')
            OPEN(4,FILE='LOG',FORM='UNFORMATTED',STATUS='NEW')
        ENDIF
        IF (START .EQ. 0) GO TO 10000
        READ(4) FP0,FP1,FP2,FP3,FP4,FP8,FP9,FP27,FP32,HALF,MINUS1
        READ(4) FAILS,SDEFCT,DEFECT,FLAWS,RADIX,ULPPLS,ULPMIN,PRECIS
        READ(4) W, MULGRD,DIVGRD, SUBGRD,A1,ONEMIN
        READ(4) C1, H1, MINPOS, NULPS, UFLTHR, PHONY0,IEEE
        READ(4) R1,R2,R3,R4,STICKY
        READ(4) PGNUMB,MILES
        REWIND 4
        FROM = MILES
        WRITE(OUT,10001)FROM
10001   FORMAT(' Restarting from milestone ',I5,'.')
        IF (FROM .EQ.   7) GO TO   881
        IF (FROM .EQ.  79) GO TO  3959
        IF (FROM .EQ.  90) GO TO  3960
        IF (FROM .GE. 105 .AND. FROM .LE. 109) GO TO 10100
        IF (FROM .EQ. 115) GO TO 10100
        IF (FROM .GE. 120 .AND. FROM .LE. 125) GO TO 10100
        IF (FROM .EQ. 131) GO TO 10100
        IF (FROM .EQ. 161) GO TO 10200
        IF (FROM .GE. 201 .AND. FROM .LE. 205) GO TO 10200
        IF (FROM .EQ. 211) GO TO 10300
        IF (FROM .EQ. 212) GO TO 10300
        CALL BADMIL
!
!             FIRST TWO ASSIGNMENTS USE INTEGERS ON RIGHT HAND SIDE
!
10000   continue
        FP0 = 0
        FP1 = 1
        FP2 = FP1 + FP1
        FP3 = FP2 + FP1
        FP4 = FP3 + FP1
        MINUS1 = -FP1
        HALF = FP1 / FP2
        FP8 = FP4 + FP4
        FP9 = FP3 * FP3
        FP27 = FP9 * FP3
        FP32 = FP8 * FP4
!
        WRITE(OUT, 10)
        WRITE(OUT, 40)
10      FORMAT(' A  Paranoid  Program  to  Diagnose  Floating-point',' Arithmetic')
40      FORMAT('          ... Double-Precision Version  ...')
!
!#######################################################################
!
        NUMTRY = 20
!        ...  NUMTRY = #( RANDOM TRIALS OF X*Y=Y*X , ETC.)
        PGNUMB=0
!        ... PGNUMB = #( PAGE OF DIAGNOSIS ); MILES=MILESTONE IN PROGRAM
        MILES=0
!        ... COUNT FAILURES, SERIOUS DEFECTS, DEFECTS, FLAWS
        FAILS = 0
        SDEFCT = 0
        DEFECT = 0
        FLAWS = 0
!#######################################################################
!
!       PRINT BIG INTRO MESSAGES
!
!#######################################################################
        CALL INTRO (MILES)
!#######################################################################
!
!       SMALL INTEGER TESTING
!
!#######################################################################
        MILES = 7
        CALL PAGE (MILES)
881     continue
        CALL SMLINT (MILES,FROM)
        FROM = 0
!#######################################################################
!
!       FIND RADIX B AND PRECISION P
!
!#######################################################################
        CALL RADX (MILES)
!#######################################################################
!
!       TEST FOR EXTRA PRECISION IN SUBEXPRESSIONS
!
!#######################################################################
        MILES = 30
        CALL EXTRA
        CALL PAGE (MILES)
!#######################################################################
!
!       CHECK FOR GUARD DIGITS AND NORMALIZATION IN SUBTRACTION
!
!#######################################################################
        MILES = 35
        CALL GUARD
        MILES = 40
        CALL PAGE (MILES)
!#######################################################################
!
!       TEST ROUNDING IN MULTIPLY, DIVIDE, AND ADD/SUBTRACT.
!
!#######################################################################
        CALL ROUND (MILES)
        MILES = 60
!#######################################################################
!
!       TEST FOR COMMUTATIVE MULTIPLICATION
!
!#######################################################################
        CALL COMMUT (NUMTRY)
        MILES = 70
!#######################################################################
!
!       TEST SQUARE ROOT
!
!#######################################################################
3959    continue
        CALL SQUARE (FROM, MILES, NUMTRY)
        FROM = 0
3960    continue
        MILES = 90
        CALL PAGE (MILES)
!#######################################################################
!
!       TEST Y TO POWER X
!
!#######################################################################
        CALL POWER (MILES,FROM)
        FROM = 0
!#######################################################################
!
!       TEST UNDERFLOW THRESHOLDS
!
!#######################################################################
10100   continue
        CALL UNDERF (MILES,NUMTRY,FROM)
        FROM = 0
        CALL PAGE (MILES)
!#######################################################################
!
!       TEST OVERFLOW THRESHOLDS
!
!#######################################################################
10200   continue
        CALL OVERF (MILES, FROM)
        FROM = 0
        MILES = 210
!
10300   continue
        CALL ZEROS(MILES,FROM)
        FROM = 0
        CALL PAGE (MILES)
        IF (FAILS .GT. 0) WRITE(OUT, 6151) FAILS
6151    FORMAT (' The number of  FAILUREs  encountered =       ',I4)
        IF (SDEFCT .GT. 0) WRITE(OUT, 6161) SDEFCT
6161    FORMAT (' The number of  SERIOUS DEFECTs  discovered = ',I4)
        IF (DEFECT .GT. 0) WRITE(OUT, 6171) DEFECT
6171    FORMAT (' The number of  DEFECTs  discovered =         ',I4)
        IF (FLAWS .GT. 0) WRITE(OUT, 6181) FLAWS
6181    FORMAT (' The number of  FLAWs  discovered =           ',I4)
        IF (FAILS+SDEFCT+DEFECT+FLAWS .GT. 0) GOTO 6270
        WRITE(OUT, 6200)
6200    FORMAT(' No failures, defects nor flaws have been discovered.')
        IF (R1+R2+R3+R4 .LT. FP4) GOTO 6260
        IF (STICKY .LT. FP1 .OR. (RADIX-FP2)*(RADIX-FP9-FP1) .NE. FP0) GOTO 6250
        TEMP = 854
        IF (RADIX .EQ. FP2 .AND.  (PRECIS - FP4*FP3*FP2) * (PRECIS - FP27-FP27+FP1) .EQ. FP0) TEMP = 754
        WRITE(OUT,6240) TEMP
6240    FORMAT (' Rounding appears to conform to the proposed', ' IEEE standard  P', I3)
        IF (IEEE .EQ. 0) WRITE(OUT, 6241)
6241    FORMAT (' except possibly for Double Rounding during Gradual', ' Underflow.')
6250    continue
        WRITE(OUT, 6251)
6251    FORMAT(' The arithmetic diagnosed appears to be Excellent!')
        GOTO 6310
6260    continue
        WRITE(OUT, 6261)
6261    FORMAT(' The arithmetic diagnosed seems Satisfactory.')
        GOTO 6310
6270    continue
        IF (FAILS+SDEFCT+DEFECT .EQ. 0 .AND. FLAWS .GT. 0) WRITE(OUT, 6271)
6271    FORMAT(' The arithmetic diagnosed seems Satisfactory though', ' flawed.')
        IF (FAILS+SDEFCT .EQ. 0 .AND. DEFECT .GT. 0) WRITE(OUT, 6281)
6281    FORMAT(' The arithmetic diagnosed may be Acceptable despite', ' inconvenient Defects.')
        IF (FAILS+SDEFCT .GT. 0) WRITE(OUT, 6291)
6291    FORMAT(' The arithmetic diagnosed has unacceptable Serious', ' Defects.')
        IF (FAILS .GT. 0) WRITE(OUT, 6301)
6301    FORMAT(' Potentially fatal FAILURE may have spoiled this', ' program''s subsequent diagnoses.')
6310    continue
        WRITE(OUT, 6311)
6311    FORMAT(' End of Test.')
        return
        end subroutine runtests
!-------------------------------------------------------------------------------
        SUBROUTINE COMMUT( NUMTRY)
      implicit none
!
!!!!!!
        INTEGER NUMTRY
        DOUBLE PRECISION R9, X, X9, Y, Y9, Z, Z9
        INTEGER I, NN
!
        WRITE(OUT,2921) NUMTRY
2921    FORMAT(/' Does multiplication commute?', ' Testing if  x*y = y*x  for', I4,' random pairs:')
        R9 = DSQRT(FP3)
        I = NUMTRY + 1
        X9 = FP0 / FP3
2960    continue
        CALL RANDOM (X, Y, X9, R9)
        Y9=X9
        CALL RANDOM (X, Y, X9, R9)
        Z=X9*Y9
        Y=Y9*X9
        Z9=Z-Y
        I=I-1
        IF (I .GT. 0 .AND. Z9 .EQ. FP0) GOTO 2960
        IF (I .GT. 0) GOTO 3000
        X9=FP0+HALF/FP3
        Y9=(ULPPLS+ULPMIN)+FP0
        Z=X9*Y9
        Y=Y9*X9
        Z9=(FP0+HALF/FP3)*((ULPPLS+ULPMIN)+FP0) -((ULPPLS+ULPMIN)+FP0)*(FP0+HALF/FP3)
        IF (Z9 .NE. FP0) GOTO 3000
        WRITE(OUT,2990) NUMTRY
2990    FORMAT(' No failure found in ',I4,' randomly chosen pairs.')
        RETURN
3000    continue
        DEFECT=DEFECT+1
        WRITE(OUT, 3001) X9, Y9
        WRITE(OUT, 3002) Z, Y, Z9
        NN=NUMTRY-I+1
        WRITE(OUT, 3003) NN
3001    FORMAT(' DEFECT:  x*y = y*x  violated at  x = ',E15.7,', y = ', E15.7)
3002    FORMAT('  x*y =',E15.7,',  y*x =',E15.7,',  x*y-y*x =',E15.7)
3003    FORMAT('    ... pair no.', I4)
        RETURN
        END SUBROUTINE COMMUT
!-------------------------------------------------------------------------------
        SUBROUTINE RANDOM (X, Y, X9, R9)
      implicit none
        DOUBLE PRECISION X, Y, X9, R9
        X=X9+R9
        Y=X*X
        Y=Y*Y
        X=X*Y
        Y=X-DINT(X)
        X9=Y+X*.000005
        RETURN
        END SUBROUTINE RANDOM
!-------------------------------------------------------------------------------
        SUBROUTINE EXTRA
      implicit none
!!!!!
        DOUBLE PRECISION Q, X, X1, Y, Y1, Z, Z1, Z2, XX
!
        WRITE(OUT,1681)
1681    FORMAT (' Test for extra-precise subexpressions:')
!
        X = DABS( ((FP4 / FP3 - FP1) - FP1 / FP4) * FP3 - FP1 / FP4)
1700    continue
        Z2 = X
        X = (FP1 + (HALF * Z2 + FP32 * Z2 * Z2)) - FP1
        IF (Z2 .GT. X .AND. X .GT. FP0) GOTO 1700
        Y = DABS( (FP3/FP4 - FP2/FP3) * FP3 - FP1/FP4)
        Z=Y
        X=Y
1720    continue
        Z1=Z
        Z=(FP1/FP2 - ((FP1/FP2-(HALF*Z1 + FP32*Z1*Z1))+FP1/FP2)) + FP1/ FP2
        IF (Z1 .GT. Z .AND. Z .GT. FP0) GOTO 1720
1730    continue
        Y1=Y
        Y=(HALF - ((HALF-(HALF*Y1 + FP32*Y1*Y1))+HALF)) + HALF
        IF (Y1 .GT. Y .AND. Y .GT. FP0) GOTO 1730
        X1=X
        X=((HALF*X1+FP32*X1*X1)-ONEMIN)+ONEMIN
        IF (X1 .GT. X  .AND. X  .GT. FP0) GOTO 1730
        IF (X1 .EQ. Y1 .AND. X1 .EQ. Z1)  GOTO 1780
        SDEFCT=SDEFCT+1
        WRITE(OUT, 1761)
        WRITE(OUT, 1762) X1, Y1, Z1
        WRITE(OUT, 1763)
        WRITE(OUT, 1770)
        WRITE(OUT, 1771)
        WRITE(OUT, 1772)
1761    FORMAT(' SERIOUS DEFECT: disagreements among the values  X1, Y1, Z1')
1762    FORMAT(' respectively ',E15.7,',    ',E15.7,',    ',E15.7)
1763    FORMAT(' are symptoms of inconsistencies introduced by extra-precise')
1770    FORMAT(' evaluation of allegedly  "optimized"  arithmetic')
1771    FORMAT(' subexpressions.  Possibly some part of this')
1772    FORMAT(' test is inconsistent; PLEASE NOTIFY KARPINSKI !')
        IF (X1 .EQ. ULPMIN .OR. Y1 .EQ. ULPMIN .OR. Z1 .EQ. ULPMIN) GOTO 1850
1780    continue
        IF (Z1 .NE. ULPMIN .OR. Z2 .NE. ULPPLS) GOTO 1790
        WRITE(OUT, 1781)
1781    FORMAT(' Subexpressions do not appear to be calculated with extra precision.')
        RETURN
1790    continue
        IF (Z1 .LT. ULPMIN .AND. Z2 .LT. ULPPLS) GOTO 1810
        FAILS=FAILS+1
        WRITE(OUT, 1801)
        WRITE(OUT, 1802)
        XX=Z1-ULPMIN
        WRITE(OUT, 1803) ULPMIN, XX
        XX=Z2-ULPPLS
        WRITE(OUT, 1804) ULPPLS, XX
1801    FORMAT(' FAILURE: precision test is inconsistent.')
1802    FORMAT(' PLEASE NOTIFY KARPINSKI !')
1803    FORMAT(' ulpmin =  ', E15.7, '    z1 - ulpmin = ', E15.7)
1804    FORMAT(' ulppls =  ', E15.7, '    z1 - ulppls = ', E15.7)
        RETURN
1810    continue
        IF (Z1 .GT. FP0 .AND. Z2 .GT. FP0) GOTO 1830
        WRITE(OUT, 1821)     RADIX
        WRITE(OUT, 1822)
        WRITE(OUT, 1823) Z1, Z2
        WRITE(OUT, 1824)
        WRITE(OUT, 1825)
1821    FORMAT(' Because of an unusual radix  b =', F4.0,',')
1822    FORMAT(' or exact rational arithmetic,')
1823    FORMAT(' a result  z1 =',E15.7,'  or  z2 =',E15.7)
1824    FORMAT(' of an extra-precision test is inconsistent.')
1825    FORMAT(' PLEASE NOTIFY KARPINSKI !')
        IF (Z1 .EQ. Z2) GOTO 1850
1830    continue
        X = Z1/ULPMIN
        Y = Z2/ULPPLS
        IF (Y .GT. X) X=Y
        Q = -DLOG(X)
        WRITE(OUT, 1841)
        XX=Q/DLOG(RADIX)
        WRITE(OUT, 1842) XX
        XX=Q/DLOG(FP8+FP2)
        WRITE(OUT, 1843) XX
1841    FORMAT(' Some subexpressions appear to be calculated extra-precisely')
1842    FORMAT(' with about   ',E15.7,' extra base b digits, i.e.')
1843    FORMAT(' roughly ',E15.7,' extra significant decimals.')
1850    continue
        WRITE(OUT, 1851)
1851    FORMAT(' That feature is not tested further by this program.')
        RETURN
        END SUBROUTINE EXTRA
!-------------------------------------------------------------------------------
        SUBROUTINE GUARD
      implicit none
!!!!!
!        ... LOCAL VARIABLES
        DOUBLE PRECISION R, S, T, X, Y, Z
!        ... CONSTANTS
        DOUBLE PRECISION B9
!
        B9 = RADIX - ULPPLS
!
        MULGRD = 1.0
        DIVGRD = 1.0
        SUBGRD = 1.0
!
        IF (RADIX .LT. FP2) GOTO 1920
        X = W / (RADIX * RADIX)
        Y = X + FP1
        Z = Y - X
        T = Z + ULPPLS
        X = T - Z
        IF (X .EQ. ULPPLS) GOTO 1910
        FAILS = FAILS + 1
        WRITE(OUT, 1905)
1905    FORMAT(' FAILURE: subtraction is not normalized',' so  x=y  does not imply  x+z=y+z !')
        GOTO 1920
1910    continue
        WRITE(OUT,1911)
1911    FORMAT(' Subtraction appears to be normalized', ' as it should.')
1920    continue
        WRITE(OUT,1930)
1930    FORMAT(' Checking for guard digits in multiply', ' divide and subtract.')
        Y=ONEMIN*FP1
        Z=FP1*ONEMIN
        X=ONEMIN-HALF
        Y=(Y-HALF)-X
        Z=(Z-HALF)-X
        X=FP1+ULPPLS
        T = X * RADIX
        R = RADIX * X
        X=T-RADIX
        X=X-RADIX*ULPPLS
        T=R-RADIX
        T=T-RADIX*ULPPLS
        X=X*(RADIX-FP1)
        T=T*(RADIX-FP1)
        IF (X .EQ. FP0 .AND. Y .EQ. FP0 .AND. Z .EQ. FP0 .AND.  T .EQ. FP0) GOTO 1980
        SDEFCT=SDEFCT+1
        MULGRD=FP0
        WRITE(OUT, 1971)
1971    FORMAT(' SERIOUS DEFECT: multiplication lacks a guard digit',' violating  1*x = x .')
1980    continue
        Z = RADIX * ULPPLS
        X=FP1+Z
        Y=DABS((X+Z)-X*X)-ULPPLS
        X=FP1-ULPPLS
        Z=DABS((X-ULPPLS)-X*X)-ULPMIN
        IF (Y .LE. FP0 .AND. Z .LE. FP0) GOTO 2000
        FAILS=FAILS+1
        WRITE(OUT, 1991)
1991    FORMAT(' FAILURE: multiplication  gets too many last digits',' wrong.')
2000    continue
        Y=FP1-ULPPLS
        X=FP1+ULPPLS
        Z=FP1/Y
        Y=Z-X
        X = FP1/FP3
        Z = FP3/FP9
        X=X-Z
        T = FP9/FP27
        Z=Z-T
        IF (X .EQ. FP0 .AND. Y .EQ. FP0 .AND. Z .EQ. FP0) GOTO 2040
        DEFECT=DEFECT+1
        DIVGRD=FP0
        WRITE(OUT,2031)
        WRITE(OUT,2032)
2031    FORMAT(' DEFECT: division lacks a guard digit',' so error can exceed 1 ulp')
2032    FORMAT(' or  1/3  and  3/9  and  9/27  may disagree.')
2040    continue
        Y=ONEMIN/FP1
        X=ONEMIN-HALF
        Y=(Y-HALF)-X
        X=FP1+ULPPLS
        T=X/FP1
        X=T-X
        IF (X .EQ. FP0 .AND. Y .EQ. FP0) GOTO 2070
        SDEFCT = SDEFCT + 1
        DEFECT = DEFECT - 1 + DIVGRD
        DIVGRD=FP0
        WRITE(OUT, 2061)
2061    FORMAT(' SERIOUS DEFECT:  division lacks a guard digit',' violating  x/1 = x .')
2070    continue
        X=FP1/(FP1+ULPPLS)
        Y=X-HALF-HALF
        IF (Y .LT. FP0) GOTO 2100
        SDEFCT=SDEFCT+1
        WRITE(OUT, 2091)
        WRITE(OUT, 2092)
2091    FORMAT(' VERY SERIOUS DEFECT:  computed value of  1/1.00...001')
2092    FORMAT(' is not less than  1 .')
2100    continue
        X=FP1-ULPPLS
        Y = FP1 + RADIX * ULPPLS
        Z = X * RADIX
        T = Y * RADIX
        R = Z / RADIX
        S = T / RADIX
        X = R - X
        Y = S - Y
        IF (X .EQ. FP0 .AND. Y .EQ. FP0) GOTO 2130
        FAILS = FAILS + 1
        WRITE(OUT, 2120)
        WRITE(OUT, 2121)
2120    FORMAT(' FAILURE: multiplication  and/or  division')
2121    FORMAT(' gets too many last digits wrong.')
2130    continue
        Y = FP1-ULPMIN
        X = FP1-ONEMIN
        Y = FP1-Y
        T = RADIX - ULPPLS
        Z = RADIX - B9
        T = RADIX - T
        IF (X .EQ. ULPMIN .AND. Y .EQ. ULPMIN .AND. Z .EQ. ULPPLS .AND. T .EQ. ULPPLS) GOTO 2230
        SDEFCT=SDEFCT+1
        SUBGRD=FP0
        WRITE(OUT, 2161)
2161    FORMAT(' SERIOUS DEFECT: subtraction lacks a guard digit', ' so cancellation is obscured.')
        IF (ONEMIN .EQ. FP1 .OR. ONEMIN-FP1 .LT. FP0) RETURN
        SDEFCT=SDEFCT+1
        WRITE(OUT, 2190)
        WRITE(OUT, 2191)
        WRITE(OUT, 2200)
        WRITE(OUT, 2210)
        WRITE(OUT, 2220)
2190    FORMAT(' VERY SERIOUS DEFECT:')
2191    FORMAT('   comparison alleges  (1-ulpmin) < 1  although')
2200    FORMAT('   subtraction yields  (1-ulpmin) - 1 = 0  , thereby vitiating')
2210    FORMAT('   such precautions against division by zero as')
2220    FORMAT('   ...  if (x=1.0) then ..... else .../(x-1.0)...')
!
2230    continue
        IF (MULGRD * DIVGRD * SUBGRD .EQ. FP1) WRITE(OUT, 2231)
2231    FORMAT(' These operations appear to have guard digits',' as they should.')
        RETURN
        END SUBROUTINE GUARD
!-------------------------------------------------------------------------------
        SUBROUTINE INTRO(MILES)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!
!       PRINT THE LARGE BANNER INTRODUCTION - GOES ON FOR SEVERAL
!       PAGES
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none
        INTEGER MILES
!!!!!
!        ... NUMBER OF MILESTONE TO PRINTOUT AS WE GO ALONG
        WRITE(OUT,170)
        WRITE(OUT,180)
        CALL PAGE(MILES)
        WRITE(OUT,190)
        CALL PAGE(MILES)
        WRITE(OUT,200)
170     FORMAT(                                                         &
     &    ' Lest this program stop prematurely, i.e. before displaying' &
     &   /'   "End of Test"   '                                         &
     &   /' try to persuade the computer NOT to terminate execution'    &
     &   /' whenever an error such as Over/Underflow or Division by'    &
     &   /' Zero occurs, but rather to persevere with a surrogate value'&
     &   /' after, perhaps, displaying some warning.  If persuasion'    &
     &   /' avails naught, don''t despair but run this program anyway')
180     FORMAT(                                                         &
     &    ' to see how many milestones it passes, and then run it'      &
     &   /' again.  It should pick up just beyond the error and'        &
     &   /' continue.  If it does not, it needs further debugging.'     &
     &  //' Users are invited to help debug and augment this program'   &
     &   /' so that it will cope with unanticipated and newly found'    &
     &   /' compilers and arithmetic pathologies.')
190     FORMAT(                                                         &
     &   /' Please send suggestions and interesting results to'         &
     &        /9X,'Richard Karpinski'                                   &
     &        /9X,'Computer Center U-76'                                &
     &        /9X,'University of California'                            &
     &        /9X,'San Francisco, CA 94143-0704'                        &
     &        /9X,'USA'/                                                &
     &        /' In doing so, please include the following information:'&
     &        /9X,'Precision:   Double;'/9X,'Version: 31 July 1986;'    &
     &        /9X,'Computer:'//9X,'Compiler:'//9X,'Optimization level:'/&
     &        /9X,'Other relevant compiler options:'/)
200     FORMAT(                                                         &
     &        /' BASIC version (C) 1983 by Prof. W. M. Kahan.'          &
     &        /' Translated to FORTRAN by T. Quarles and G. Taylor.'    &
     &        /' Modified to ANSI 66/ANSI 77 compatible subset by'      &
     &        /' Daniel Feenberg and David Gay.'                        &
     &        /' You may redistribute this program freely if you'       &
     &        /' acknowledge the source.')
!#####################################################################
        WRITE(OUT,480)
        CALL PAGE (MILES)
        WRITE(OUT,530)
        WRITE(OUT,590)
        WRITE(OUT,640)
480     FORMAT(//                                                       &
     &  ' Running this program should reveal these characteristics:'//  &
     &  ' b = radix ( 1, 2, 4, 8, 10, 16, 100, 256, or ... ) .'/        &
     &  ' p = precision, the number of significant  b-digits carried.'/ &
     &  ' u2 = b/b^p = one ulp (unit in the last place) of 1.000xxx..'/ &
     &  ' u1 = 1/b^p = one ulp of numbers a little less than 1.0.')
530     FORMAT(' g1, g2, g3 tell whether adequate guard digits are carried;'/  &
     &  ' 1 = yes, 0 = no;  g1 for mult.,  g2 for div., g3 for subt.'/  &
     &  ' r1,r2,r3,r4  tell whether arithmetic is rounded or chopped;'/ &
     &  ' 0=chopped, 1=correctly rounded, -1=some other rounding;'/     &
     &  ' r1 for mult., r2 for div., r3 for add/subt., r4 for sqrt.'/   &
     &  ' s=1 when a sticky bit is used correctly in rounding; else s=0 &
     &.')
590     FORMAT(' u0 = an underflow threshold.'/                         &
     & ' e0 and z0 tell whether underflow is abrupt, gradual or fuzzy'/ &
     & ' v = an overflow threshold, roughly.'/                          &
     & ' v0  tells, roughly, whether  infinity  is represented.'/       &
     & ' Comparisons are checked for consistency with subtraction')
640     FORMAT('        and for contamination by pseudo-zeros.'/        &
     & ' Sqrt is tested. so is  y^x  for (mostly) integers  x .'/       &
     & ' Extra-precise subexpressions are revealed but not yet tested.' &
     & /' Decimal-binary conversion is not yet tested for accuracy.')
!#####################################################################
        CALL PAGE (MILES)
        WRITE(OUT,690)
        WRITE(OUT,760)
        WRITE(OUT,780)
        WRITE(OUT,820)
690     FORMAT(' The program attempts to discriminate among:'/          &
     &         '     >FLAWs, like lack of a sticky bit, '/              &
     &         '     >SERIOUS DEFECTs, like lack of a guard digit, and'/&
     &         '     >FAILUREs, like  2+2 = 5 .'/                       &
     &         ' Failures may confound subsequent diagnoses.')
760     FORMAT(/                                                        &
     &  ' The diagnostic capabilities of this program go beyond an'/    &
     &  ' earlier program called  "Machar", which can be found at the'/ &
     &' end of the book "Software Manual for the Elementary Functions"')
780     FORMAT(                                                         &
     &  ' (1980) by W. J. Cody and W. Waite. Although both programs'/   &
     &  ' try to discover the radix (b), precision (p) and         '/   &
     &  ' range (over/underflow thresholds) of the arithmetic, this'/   &
     &  ' program tries to cope with a wider variety of pathologies')
820     FORMAT(                                                         &
     &  ' and to say how well the arithmetic is implemented.'/          &
     &  ' The program is based upon a conventional radix'/              &
     &  ' representation for floating-point numbers,'/                  &
     &  ' but also allows for logarithmic encoding (b = 1)'/            &
     &  ' as used by certain early wang machines.'/)
        RETURN
        END SUBROUTINE INTRO
!-------------------------------------------------------------------------------
        SUBROUTINE LOGIT (MILE)
      implicit none
!
        INTEGER MILE
!        ... MILESTONE REACHED - PASSED THROUGH BY THE SUCCESSFUL CODE
!!!!!
!        THIS ROUTINE FORCES A CHECKPOINT BY  WRITING ALL GLOBAL
!        INFORMATION TO A FILE FOR LATER RESTARTING.
!
        WRITE(4) FP0,FP1,FP2,FP3,FP4,FP8,FP9,FP27,FP32,HALF,MINUS1
        WRITE(4) FAILS,SDEFCT,DEFECT,FLAWS,RADIX,ULPPLS,ULPMIN,PRECIS
        WRITE(4) W, MULGRD,DIVGRD, SUBGRD,A1,ONEMIN
        WRITE(4) C1, H1, MINPOS, NULPS, UFLTHR, PHONY0, IEEE
        WRITE(4) R1, R2, R3, R4, STICKY
        WRITE(4) PGNUMB,MILE
        REWIND 4
        RETURN
        END SUBROUTINE LOGIT
!-------------------------------------------------------------------------------
!       SUBROUTINE TO TEST IF  Y = X
!
        SUBROUTINE CMPXY (X, Y, Z, Q, N)
      implicit none
!!!!!
        DOUBLE PRECISION FP0, X, XX, Y, Z
        INTEGER Q, N
        DATA FP0/0.D0/
        Y=Z**Q
        IF (Y .EQ. X) GOTO 4080
        IF (Z .GT. FP0) GO TO 4050
        IF (Q .GT. FP0) GO TO 4050
        WRITE(OUT, 40401) Z, Q, Y
40401   FORMAT(' WARNING: computed  (',E16.8,')^(',I3,') = ',E16.8)
        GO TO 40601
4050    continue
        IF (N .GT. 0) GOTO 4070
        DEFECT = DEFECT + 1
        WRITE(OUT, 4061) Z, Q, Y
40601   continue
        WRITE(OUT, 4062) X
        XX=Y-X
        WRITE(OUT, 4063) XX
4061    FORMAT(' DEFECT: computed  (',E16.8,')^(',I3,') = ',E16.8)
4062    FORMAT('    compares unequal to correct ',E16.8)
4063    FORMAT('    they differ by  ',E16.8)
!                       INCREMENT COUNT OF DISCREPANCIES
4070    continue
        N = N + 1
4080    continue
        RETURN
        END SUBROUTINE CMPXY
!-------------------------------------------------------------------------------
!       SUBROUTINE TO PRINT N AND PAUSE IF N > 0.
!
        SUBROUTINE PRT2 (N, MILES)
      implicit none
        INTEGER N, MILES
!!!!!
        IF (N .EQ. 0) WRITE(OUT, 4291)
4291    FORMAT(' No discrepancies found.'/)
        IF (N .GT. 0) CALL PAGE(MILES)
!       ---- PAUSE ----
        RETURN
        END SUBROUTINE PRT2
!-------------------------------------------------------------------------------
!       SUBROUTINE TO COMPARE  Z^I  WITH  X = Z*Z*...*Z  ( I TIMES )
!
        SUBROUTINE PWRCMP (X, Z, I, M, N)
      implicit none
!!!!!
        DOUBLE PRECISION X, Z
        INTEGER I, M, N
        DOUBLE PRECISION Y
        INTEGER Q
3990    continue
        Y = Z ** I
        Q = I
!                               TEST WHETHER  Y=X
        CALL CMPXY (X, Y, Z, Q, N)
        I = I + 1
!                               WITH  X = Z^M
        IF (I .GT. M) RETURN
        X = Z * X
        IF (X .LT. W) GOTO 3990
        RETURN
        END SUBROUTINE
!-------------------------------------------------------------------------------
!       SUBROUTINE TO PRINT COUNT  N  OF DISCREPANCIES
!
        SUBROUTINE PRTCNT (N)
      implicit none
        INTEGER N
!!!!!
        IF (N .GT. 0) WRITE(OUT, 4101) N
4101    FORMAT(' Similar discrepancies have occurred ',I4,' times.')
        RETURN
        END SUBROUTINE PRTCNT
!-------------------------------------------------------------------------------
        SUBROUTINE BADSQR(SFLAG,Z,Y)
      implicit none
        INTEGER SFLAG
!        ... INTEGER FLAG TO INDICATE NEED TO USE PREFIX "SERIOUS"
        DOUBLE PRECISION Z
!        ... SQUARE OF SQUARE ROOT OF Z (WRONG)
        DOUBLE PRECISION Y
!        ... DOUBLE PRECISION NUMBER WHOSE SQUARE ROOT SQUARED IS WRONG
!!!!!
        IF(SFLAG .EQ. 1) GO TO 5745
        WRITE(OUT,5740) Z
5740    FORMAT(' DEFECT:  comparison alleges that what prints as  z = ', E16.8)
        GO TO 5748
5745    continue
        WRITE(OUT,5747) Z
5747    FORMAT(' SERIOUS DEFECT:  comparison alleges that what prints as z = ',E16.8)
5748    continue
        WRITE(OUT,5749) Y
5749    FORMAT(17X,'is too far from  sqrt(z)^2 = ',E16.8,'.')
        RETURN
        END SUBROUTINE BADSQR
!-------------------------------------------------------------------------------
        SUBROUTINE OVERF (MILES, FROM)
      implicit none
!!!!!
!        ... NUMBER OF MILESTONES PASSED
        INTEGER MILES
!        ... COUNTER OF MILESTONES PREVIOUSLY REACHED
        INTEGER FROM
!
!        ... LOCAL VARIABLES
        INTEGER I
        DOUBLE PRECISION V9, X, Y, Z, TEMP
!        ... SATURATION VALUE AFTER FLOATING POINT OVERFLOW
        DOUBLE PRECISION SAT
!        ... OVERFLOW THRESHOLD
        DOUBLE PRECISION V
!        ... FLAG TO INDICATE WHETHER DEFECT IS SERIOUS OR NOT
        INTEGER ZFLAG
!
        INTEGER I0
!
!       IBM        SIGN-MAGNITUDE, SATURATION VALUE, NO TRAP
!       PRIME      TWOS-COMPLEMENT, SATURATION VALUE, NO TRAP
!       VAX        SIGN-MAGNITUDE, TRAP
!       ELXSI      SIGN-MAGNITUDE, TRAP OR INFINITY SYMBOL IF NO TRAP
!       CDC        SIGN-MAGNITUDE, INFINITY SYMBOL, NO TRAP
!
!           CALL TO UNIX TO ENABLE TRAPS (FAULTS)
!           ON FP UNDERFLOW AND FIXED POINT OVERFLOW
!
        IF (FROM .EQ. 0) GOTO 5500
!
!           REASSIGN VALUES TO VARIABLES USING THE LOG FILE,
!           THEN GO TO RESTART POINT
!
        READ(3) I,V,SAT,V9,X,Y,Z,ZFLAG
        REWIND 3
        IF (FROM .EQ. 161) GOTO 5582
        IF (FROM .EQ. 170) GOTO 5680
        IF (FROM .EQ. 175) GOTO 5810
        IF (FROM .GE. 201 .AND. FROM .LE. 205) GO TO 5999
        CALL BADMIL
!
5500    continue
        WRITE(OUT,5510)
5510    FORMAT(' Searching for overflow threshold:')
        MILES = 161
        CALL LOGIT (MILES)
!
!                  SET Y TO -1 * A LARGE POWER OF THE RADIX
        Y = -C1
        V9 = H1 * Y
!
!                  MULTIPLY BY RADIX (H1) UNTIL OVERFLOW OCCURS
!
5530    continue
        V = Y
        Y = V9
        WRITE(3) I,V,SAT,V9,X,Y,Z,ZFLAG
        REWIND 3
        V9 = H1 * Y
        IF (V9 .LT. Y) GOTO 5530
!
!       SYSTEM DOES NOT TRAP ON OVERFLOW
!
!           POSSIBILITIES:
!               V9 > Y,  V9 IS THE VALUE RETURNED AFTER OVERFLOW
!                        Y IS THE LARGEST POWER OF THE RADIX
!                        V IS THE SECOND LARGEST POWER OF THE RADIX
!
!               V9 == Y, BOTH ARE SATURATION VALUE
!                        V IS THE LARGEST POWER OF THE RADIX
!
!               V9 == Y, BOTH ARE INFINITY SYMBOL
!                        V IS THE LARGEST POWER OF THE RADIX
!
!       TEST 1: VALUE RETURNED AFTER OVERFLOW SHRINKS IN MAGNITUDE
!
        IF (V9 .EQ. Y) GOTO 5545
        SDEFCT = SDEFCT + 1
        WRITE(OUT, 5541) Y, V9
5541    FORMAT(' SERIOUS DEFECT: overflow past  ', 1PE16.8, '  shrinks to ', 1PE16.8)
!
!       TEST 2: TWO'S COMPLEMENT MACHINE SATURATES AT NEGATIVE
!               LARGEST POWER OF THE RADIX
!               NEED TO DISTINGUISH SYSTEM WITH OVERFLOW SYMBOLS
!               FROM ONE WITHOUT THEM
!
5545    continue
        WRITE(OUT,5546) Y
5546    FORMAT(' Can " z = -y " overflow?  trying it on  y = ',1PE16.8)
        SAT = -Y
        IF (V - Y .EQ. V + SAT) GOTO 5560
        FLAWS = FLAWS + 1
        WRITE(OUT, 5551)
5551    FORMAT(' Finds a FLAW:  -(-y) differs from y.')
        GOTO 5590
5560    continue
        WRITE(OUT, 5561)
5561    FORMAT(' Seems O.K.')
        GOTO 5590
!
!       RESTART POINT FOR SYSTEMS THAT TRAP ON OVERFLOW
!
!             V9 = Y =  -(LARGEST POWER OF RADIX)
!             V      =  -(SECOND LARGEST POWER OF RADIX)
!
!       TEST 2: TWO'S COMPLEMENT MACHINE
!
5582    continue
        WRITE(OUT,5583) Y
5583    FORMAT(' Can " z = -y " overflow?  trying it on  y = ',1PE16.8)
!
!           PUT SOMETHING HERE TO HANDLE THE TRAP
!
        SAT = -Y
        IF (V - Y .EQ. V + SAT) GOTO 5585
        FLAWS = FLAWS + 1
        WRITE(OUT, 5584)
5584    FORMAT('  Finds a FLAW:  -(-y) differs from y.')
        GOTO 5587
5585    continue
        WRITE(OUT, 5586)
5586    FORMAT(' Seems O.K.')
!
!           THIS CODE WORKS FOR A SIGN-MAGNITUDE MACHINE,
!           BUT FAILS FOR A TWOS-COMPLEMENT ONE
!
5587    continue
        V = Y * (H1 * ULPPLS - H1)
        V = V + Y * ((FP1 - H1) * ULPPLS)
!
        WRITE(OUT, 5588) V
5588    FORMAT(' Overflow threshold is  v = ',1PE16.8)
        WRITE(OUT, 5589)
5589    FORMAT(' There is no saturation value because'/ ' the system traps on overflow.')
        GOTO 5640
!
!       NON-TRAPPING SYSTEMS (CONTINUED)
!
5590    continue
        Y = V * (H1 * ULPPLS - H1)
        Z = Y + V * ((FP1 - H1) * ULPPLS)
        IF (Z .LT. SAT) Y = Z
        IF (Y .LT. SAT) V = Y
!
!                  THE OVERFLOW THRESHOLD EQUALS THE SATURATION VALUE
!                  IF THE LATTER BEHAVES AS A NUMBER RATHER THAN AN
!                  OVERFLOW SYMBOL.  AN OVERFLOW SYMBOL IS NOT
!                  CHANGED WHEN ANY NUMBER IS ADDED TO OR SUBTRACTED
!                  FROM IT.
!
        IF (SAT - V .LT. SAT) V = SAT
!
        WRITE(OUT, 5620) V
5620    FORMAT(' Overflow threshold is  v = ',1PE16.8)
        WRITE(OUT, 5630) SAT
5630    FORMAT(' Overflow saturates at  sat = ',1PE16.8)
!
!
!
5640    continue
        MILES = 163
        WRITE(OUT, 5641)
5641    FORMAT(' No overflow should be signaled for  v*1 = ')
        TEMP = V * FP1
        WRITE(OUT, 5642) TEMP
5642    FORMAT('                                           ',1PE16.8)
        WRITE(OUT, 5643)
5643    FORMAT('                            nor for  v/1 = ')
        TEMP = V / FP1
        WRITE(OUT, 5644) TEMP
5644    FORMAT('                                           ',1PE16.8)
        WRITE(OUT,5649)
5649    FORMAT(' Any overflow signal separating this  *  from one above is a DEFECT.')
!
!       NEED TO ADD CODE HERE TO HANDLE OVERFLOWS JUST ABOVE
!
!
        MILES=170
!
!       PROBLEM: SAT NOT DEFINED IF WE TRAPPED ON OVERFLOW ABOVE
!
        IF (-V .LT. V .AND. -SAT .LT. SAT .AND. -UFLTHR .LT. V .AND.  UFLTHR .LT. V) GOTO 5680
        FAILS = FAILS + 1
        WRITE(OUT,5672)
5672    FORMAT(' FAILURE: comparisons are confused by overflow.')
!
5680    continue
        MILES = 175
        I = 0
        Z = UFLTHR
5700    continue
        I = I + 1
        IF (Z .EQ. FP0) GO TO 5770
        V9=DSQRT(Z)
        Y=V9*V9
        IF (.NOT. (Y/(FP1-RADIX * NULPS) .LT. Z .OR. Y .GT. (FP1+RADIX*NULPS)*Z))GOTO 5770
        IF (V9 .GT. ULPMIN) GOTO 5750
        ZFLAG=0
        DEFECT=1+DEFECT
        GOTO 5760
5750    continue
        ZFLAG=1
        SDEFCT=1+SDEFCT
5760    continue
        CALL BADSQR(ZFLAG,Z,Y)
5770    continue
        GOTO (5780, 5790, 5800),I
5780    continue
        Z = MINPOS
        GOTO 5700
5790    continue
        Z = PHONY0
        GOTO 5700
!
5800    continue
        I=0
        Z=V
5810    continue
        MILES=180
        IF (RADIX .NE. 2. .OR. PRECIS .NE. 56. .OR. PHONY0 .EQ. 0. .OR. -FP0 .EQ. FP0) GOTO 5850
        FAILS=1+FAILS
! FAILURE: ATTEMPTS TO EVALUATE  SQR(OVERFLOW THRESHOLD V)  IN DOUBLE
! PRECISION IN  BASIC  ON THE  IBM PC  DISPLAY THE WORD  " OVERFLOW "
! AND THEN DISABLE THE KEYBOARD !  THIS IS DISASTROUS.
        GOTO 5920
!
5850    continue
        V9 = DSQRT(Z)
        X = (FP1 - RADIX * NULPS) * V9
        V9 = V9 * X
        IF (.NOT. (V9 .LT. (FP1-FP2*RADIX*NULPS)*Z .OR. V9 .GT. Z)) GOTO 5900
        Y=V9
        IF (X .LT. W) GO TO 5880
        ZFLAG = 0
        DEFECT = DEFECT + 1
        GOTO 5890
5880    continue
        ZFLAG = 1
        SDEFCT = SDEFCT + 1
5890    continue
        I = 1
        CALL BADSQR(ZFLAG,Z,Y)
5900    continue
        IF (I .EQ. 1) GO TO 5920
        I = 1
        Z = SAT
        GOTO 5850
!
5920    continue
        MILES = 190
        CALL PAGE (MILES)
        X = UFLTHR * V
        Y = RADIX * RADIX
        IF (X * Y .GE. FP1 .AND. X .LE. Y) GOTO 5990
        IF (X * Y .GE. ULPMIN .AND. X .LE. Y / ULPMIN) GOTO 5970
        DEFECT = DEFECT + 1
        WRITE(OUT, 5961) X
5961    FORMAT(' DEFECT: badly unbalanced range;','  UFLTHR * V  =', E13.5,' IS TOO FAR FROM 1.')
        GOTO 5990
5970    continue
        FLAWS = FLAWS + 1
        WRITE(OUT, 5971) X
5971    FORMAT(' FLAW: unbalanced range;', '  UFLTHR * V  =', E13.5,' IS TOO FAR FROM 1.')
!
!                               TEST  X/X   VS.  1
5990    continue
        I0 = 1
        GO TO 6000
5999    continue
        I0 = FROM - 200
!
6000    continue
        DO 6100 I = I0, 5
           X = ONEMIN
           IF (I .EQ. 2) X = FP1 + ULPPLS
           IF (I .EQ. 3) X = V
           IF (I .EQ. 4) X = UFLTHR
           IF (I .EQ. 5) X = RADIX
           MILES = 200 + I
           IF (FROM .NE. MILES) GO TO 6050
           SDEFCT = SDEFCT + 1
           WRITE(OUT,6011) X
6011       FORMAT(' SERIOUS DEFECT: x/x traps when x = ',E13.5)
           GO TO 6100
6050    continue
        Y = X
        CALL LOGIT(MILES)
        V9 = (Y / X - HALF) - HALF
        IF (V9 .EQ. FP0) GOTO 6100
        IF (V9 .EQ. -ULPMIN .AND. I .LT. 5) GOTO 6080
        FAILS = FAILS + 1
        WRITE(OUT, 6071) X
6071    FORMAT(' FAILURE:  x/x differs from 1 when x = ',E13.5)
        GOTO 6090
6080    continue
        SDEFCT = SDEFCT + 1
        WRITE(OUT, 6081) X
6081    FORMAT(' SERIOUS DEFECT:  x/x differs from 1 when x = ',E13.5)
6090    continue
        WRITE(OUT, 6091) V9
6091    FORMAT('           instead,  x/x - 1/2 - 1/2 = ',E13.5)
6100    continue
        CONTINUE
        RETURN
        END SUBROUTINE OVERF
!-------------------------------------------------------------------------------
        SUBROUTINE PAGE(MILE)
      implicit none
!!!!!
        INTEGER MILE
!        ... MILESTONE REACHED - PASSED THROUGH BY THE SUCCESSFUL CODE
!
!        THIS PROGRAM RUNS INTERACTIVELY PUTTING ALL OUTPUT TO A SCREEN.
!        THE NEXT SUBPROGRAM PAUSES, ALLOWING YOU TO READ THE SCREEN OR
!        COPY IT.
        WRITE(OUT,110)
110     FORMAT(/' To continue diagnosis, press return.')
        READ(IN,111)
111     FORMAT (A4)
        PGNUMB=PGNUMB+1
        WRITE(OUT,112) MILE, PGNUMB
112     FORMAT(' Diagnosis resumes after milestone  #',I5,',    ... page ',I5/)
        WRITE(4) FP0,FP1,FP2,FP3,FP4,FP8,FP9,FP27,FP32,HALF,MINUS1
        WRITE(4) FAILS,SDEFCT,DEFECT,FLAWS,RADIX,ULPPLS,ULPMIN,PRECIS
        WRITE(4) W, MULGRD,DIVGRD, SUBGRD,A1,ONEMIN
        WRITE(4) C1, H1, MINPOS, NULPS, UFLTHR, PHONY0, IEEE
        WRITE(4) R1, R2, R3, R4, STICKY
        WRITE(4) PGNUMB,MILE
        REWIND 4
        MILE=MILE+1
        RETURN
        END SUBROUTINE PAGE
!-------------------------------------------------------------------------------
        SUBROUTINE PARTUF (Z, ZNAME, MILES, PARTU, RESTRT)
      implicit none
        LOGICAL RESTRT
!!!!!
        INTEGER PARTU
!        ... FLAG TO INDICATE THE PRESENCE OF PARTIAL UNDERFLOW
        DOUBLE PRECISION Z
!        ... VALUE TO TEST FOR PARTIAL UNDERFLOW
        character(len=8) :: ZNAME
!        ... NAME OF THE VARIABLE Z (IN A8 FORMAT) FOR OUTPUT
        INTEGER MILES
!        ... NUMBER OF MILESTONE REACHED SO FAR FOR OUTPUT
        DOUBLE PRECISION DUMMY
!        ... TEMPORARY VARIABLE TO HOLD A RESULT THAT MAY ACTUALLY
!        ... UNDERFLOW
        DOUBLE PRECISION MULTP1
!        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
!        ... MULT BY 1
        DOUBLE PRECISION MULTP2
!        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
!        ... 1 * SMALL
        DOUBLE PRECISION DIVTMP
!        ... TEMP. VARIABLE TO HOLD RESULT OF TEST FOR UNDERFLOW ON
!        ... DIV BY 1
!       ___ SUBROUTINE TO TEST  Z  FOR PARTIAL UNDERFLOW ___
        PARTU=0
        IF (RESTRT) GO TO 4740
        CALL LOGIT(MILES)
        IF (Z .EQ. FP0) GOTO 4850
        WRITE(OUT, 4660) ZNAME, ZNAME, ZNAME, ZNAME
4660    FORMAT(' Since comparison denies  ',A8,' = 0, evaluating  (',A8,' + ',A8,') / ',A8,'  should be safe;')
        DUMMY = (Z + Z) / Z
        WRITE(OUT, 4665) ZNAME, ZNAME, ZNAME, DUMMY
4665    FORMAT(' what the machine gets for  (',A8,' + ',A8,') / ',A8,'  is'/10X, E15.7)
        IF (DABS(DUMMY-FP2) .LT. RADIX*ULPPLS) GO TO 4750
        IF (DUMMY .LT. FP1 .OR. DUMMY .GT. FP2) GOTO 4740
        PARTU=1
        DEFECT=DEFECT+1
        WRITE(OUT,4675)
4675    FORMAT(' This is a DEFECT.'/)
        GOTO 4760
4740    continue
        PARTU=1
        SDEFCT=SDEFCT+1
        WRITE(OUT,4745)
4745    FORMAT(' This is a VERY SERIOUS DEFECT.')
        GOTO 4760
4750    continue
        WRITE(OUT,4751)
4751    FORMAT(' This is O.K. provided over/underflow has not just been signaled.')
4760    continue
        MULTP1=Z*FP1
        MULTP2=FP1*Z
        DIVTMP=Z/FP1
        IF (Z .EQ. MULTP1 .AND. Z .EQ. MULTP2 .AND. Z .EQ. DIVTMP)GO TO 4840
        PARTU=1
        DEFECT=DEFECT+1
        WRITE(OUT,4780)ZNAME,Z
4780    FORMAT(' DEFECT:  what prints as  ',A8,' =',E16.8,' compares different from')
        IF (.NOT. (Z .EQ. MULTP1)) WRITE(OUT,4795)ZNAME,MULTP1
4795    FORMAT('           ',A8,'*1 = ',E16.8)
        IF (.NOT. (Z .EQ. MULTP2 .OR. MULTP2 .EQ. MULTP1)) WRITE(OUT,4805) ZNAME,MULTP2
4805    FORMAT('           1*',A8,' = ',E16.8)
        IF (.NOT. (Z .EQ. DIVTMP)) WRITE(OUT,4815)ZNAME,DIVTMP
4815    FORMAT('           ',A8,'/1 = ',E16.8)
        IF (MULTP2 .EQ. MULTP1) GO TO 4840
        DEFECT=DEFECT+1
        WRITE(OUT,4831)
4831    FORMAT(' DEFECT: multiplication does not commute; comparison alleges that')
        WRITE(OUT,4833)ZNAME,MULTP2,ZNAME,MULTP1
4833    FORMAT('         1*',A8,' =',E16.8,'  differs from  ',A8,'*1 =',E16.8)
4840    continue
        IF (PARTU .GT. 0) CALL PAGE(MILES)
!       ---- PAUSE ----
4850    continue
        RETURN
        END SUBROUTINE PARTUF
!-------------------------------------------------------------------------------
        SUBROUTINE POWER(MILES,FROM)
      implicit none
!!!!!
        INTEGER MILES,FROM
!       ... LOCAL VARIABLES
        INTEGER I, M, N, N1
        INTEGER NUMTRY
        DOUBLE PRECISION X, Z, A
!       ... CONSTANTS
        DOUBLE PRECISION FP0, FP1, FP2, FP3, FP4, FP8, MINUS1
!
        FP0 = 0.0
        FP1 = 1.0
        FP2 = FP1 + FP1
        FP3 = FP2 + FP1
        FP4 = FP3 + FP1
        FP8 = FP4 + FP4
        MINUS1 = -FP1
        A = FP1 / A1
        NUMTRY = 20
!
        WRITE(OUT,3971)
3971    FORMAT(' Testing powers  z^i  for small integers  z  and  i :')
        IF(FROM .NE. 90) WRITE(OUT,3972)
3972    FORMAT(' Start with 0.**0 .')
        N = 0
        I = 0
        Z = -FP0
        M = 3
        IF(FROM.EQ.90) GOTO 4160
        IF (FROM .NE. 0) CALL BADMIL
!                       TEST POWERS OF ZERO
4130    continue
        X = FP1
        CALL PWRCMP (X, Z, I, M, N)
        IF (I .GT. 10) GOTO 4150
        I = 1023
        CALL PWRCMP (X, Z, I, M, N)
4150    continue
        IF (Z .EQ. MINUS1) GOTO  4170
!                       IF (-1)^N IS INVALID, REPLACE 'MINUS1' BY 'FP1'
4160    continue
        Z = MINUS1
        I = -4
        GOTO 4130
!                       PRINT  N  IF  N > 0.
4170    continue
        CALL PRTCNT (N)
        N1 = N
        N = 0
        Z = A1
        M = IDINT(FP2 * DLOG(W) / DLOG(A1) )
!
!                       LOOP
!
4190    continue
        X = Z
        I = 1
        CALL PWRCMP (X, Z, I, M, N)
        IF (Z .EQ. A) GOTO 4210
        Z = A
        GOTO 4190
!
!       POWERS OF RADIX  B  HAVE BEEN TESTED; NEXT TRY A FEW PRIMES.
!
4210    continue
        MILES=100
        M = NUMTRY
        Z = FP3
4230    continue
        X = Z
        I = 1
        CALL PWRCMP (X, Z, I, M, N)
4240    continue
        Z = Z + FP2
        IF (FP3 * DINT (Z / FP3) .EQ. Z) GOTO 4240
        IF (Z .LT. FP8 * FP3) GOTO 4230
        IF (N .GT. 0) WRITE(OUT,4261)
4261    FORMAT(' Error like this may invalidate financial calculations involving interest rates.')
        CALL PRTCNT (N)
        N = N + N1
        CALL PRT2 (N, MILES)
        RETURN
        END SUBROUTINE POWER
!-------------------------------------------------------------------------------
        SUBROUTINE RADX(MILES)
      implicit none
!!!!!
        INTEGER MILES
!        ... COUNT OF MILESTONES PASSED
        DOUBLE PRECISION X,Y,Z
!        ... SCRATCH VARIABLES
        DOUBLE PRECISION ULPMSV,RADSAV
!        ... TEMPS TO SAVE ULPMIN, ULPPLS, RADIX, PRECIS IN WHILE
!        ... RECOMPUTING
        DOUBLE PRECISION THIRD,SIXTH
!        ... TEMPS TO HOLD APPROX VALUES OF 1/3 AND 1/6 IN WHILE
!        ... ACCUMULATING ERROR FOR RADIX COMPUTATIONS.
        DOUBLE PRECISION B9, T8
        INTEGER I
!
        T8 = 240.0
!
        WRITE(OUT,1160)
1160    FORMAT(/' Searching for radix and precision...')
!       LOOKING FOR W TO BE BIG ENOUGH TO MAKE 1 INSIGNIFICANT AT THE
!       PRECISION AVAILABLE.  INCREASE BY POWERS OF 2 UNTIL WE FIND IT.
        W=FP1
1180    continue
        W=W+W
        Y=W+FP1
        Z=Y-W
        Y=Z-FP1
        IF ((-FP1+DABS(Y)) .LT. FP0) GOTO 1180
!       ... NOW  W  IS JUST BIG ENOUGH THAT  |((W+1)-W)-1| >= 1 ...
!        I.E. 1 IS INSIGNIFICANT RELATIVE TO W.
        PRECIS=FP0
        Y=FP1
1210    continue
        RADIX=W+Y
        Y=Y+Y
        RADIX=RADIX-W
        IF (RADIX .EQ. FP0) GOTO 1210
!
        IF (RADIX .GE. FP2) GOTO 1235
        RADIX = FP1
        WRITE(OUT,1230)     RADIX
1230    FORMAT(' Radix = ',F7.0)
        GOTO 1270
!          BASE IS 1, SO IT IS NOT CHARACTERIZED BY A PRECISION, SO
!          DON'T BOTHER TO HUNT FOR IT..
!
!               ... RADIX >= 2 ...
!        NOW TRY TO FIND THE PRECISION (# SIG. DIGITS)
1235    continue
        WRITE(OUT,1236) RADIX
1236    FORMAT(' Radix = ',F4.0)
        W=FP1
1250    continue
        PRECIS=PRECIS+FP1
        W=W*RADIX
        Y=W+FP1
        Z=Y-W
        IF (Z .EQ. FP1) GOTO 1250
!        ... NOW  W = RADIX^PRECIS  IS BARELY TOO BIG TO SATISFY
!        ... (W+1)-W = 1  .
1270    continue
        ULPMIN=FP1/W
        ULPPLS=RADIX*ULPMIN
        WRITE(OUT,1271) ULPMIN
        WRITE(OUT,1280)
1271    FORMAT(' Closest relative separation found is ',1PE16.8)
1280    FORMAT(' Recalculating radix and precision ')
        RADSAV=RADIX
        ULPMSV=ULPMIN
!                                               ...  SAVE OLD VALUES
        X=FP4/3.0E0
        THIRD=X-FP1
        SIXTH=(FP1/FP2)-THIRD
        X=SIXTH+SIXTH
        X=DABS(X-THIRD)
        IF (X .LT. ULPPLS) X=ULPPLS
!       ... NOW  X = (UNKNOWN NO.) ULPS OF  1 + ...
1320    continue
        ULPPLS=X
        Y=(FP1/FP2)*ULPPLS+3.2E1*ULPPLS*ULPPLS
        Y=FP1+Y
        X=Y-FP1
!       X=> ((X/2) + EPSILON) MOD (1 ULP OF 1+)
        IF (ULPPLS .GT. X .AND. X .GT. FP0) GOTO 1320
!        IF X DOES NOT UNDERFLOW TO 0, THEN IT IS STILL (UNKNOWN) * ULP
!        SO TRY AGAIN....  OTHERWISE, PREVIOUS VALUE (ULPPLS) IS 1 ULP
!       ... NOW  ULPPLS = 1 ULP OF  1 + ...
        X=FP2/3.0E0
        SIXTH=X-(FP1/FP2)
        THIRD=SIXTH+SIXTH
        X=THIRD-(FP1/FP2)
        X=DABS(X+SIXTH)
        IF (X .LT. ULPMIN) X=ULPMIN
!       ... NOW  X = (UNKNOWN NO.) ULPS OF  1 - ...
1360    continue
        ULPMIN=X
        Y=(FP1/FP2)*ULPMIN+3.2E1*ULPMIN*ULPMIN
        Y=(FP1/FP2)-Y
        X=(FP1/FP2)+Y
        Y=(FP1/FP2)-X
        X=(FP1/FP2)+Y
!        X => (X/2 = EPSILON) MOD (1 ULP OF 1-)
        IF (ULPMIN .GT. X .AND. X .GT. FP0) GOTO 1360
!       ... NOW  ULPMIN = 1 ULP OF  1 - ...
!
!       NOW TO SUMMARIZE THE RESULTS
!
        IF (ULPMIN .EQ. ULPMSV) WRITE(OUT,1381)
1381    FORMAT(' confirms closest relative separation .')
        IF (ULPMIN .NE. ULPMSV) WRITE(OUT,1391) ULPMIN
1391    FORMAT(' gets better closest relative separation = ', E13.5)
        W=FP1/ULPMIN
        ONEMIN = (HALF - ULPMIN) + HALF
!       ... = 1 - ULPMIN = NEXTAFTER(1.0, 0)
        RADIX=DINT(.01 + ULPPLS/ULPMIN)
        IF (RADIX .EQ. RADSAV) WRITE(OUT,1411)
1411    FORMAT(' Radix confirmed.')
        IF (RADIX .NE. RADSAV) WRITE(OUT,1421) RADIX
1421    FORMAT(' mystery: recalculated radix = ', E13.5)
!
!       ... RADICES 1, 2 AND 10 PASS MUSTER
!
        IF (RADIX .EQ. FP2 .OR. RADIX .EQ. 1.0E1 .OR. RADIX .EQ. FP1)GOTO 1470
        IF (RADIX .GT. 1.6E1) GOTO 1460
        FLAWS=FLAWS+1
        WRITE(OUT,1451) RADIX
1451    FORMAT(' FLAW: radix =',F4.0,' is not so good as 2 or 10.')
        GOTO 1470
1460    continue
        DEFECT=DEFECT+1
        WRITE(OUT,1461) RADIX
1461    FORMAT(' DEFECT: radix =',F4.0,' is so big that roundoff propagates capriciously.')
1470    continue
        MILES=20
!       TEST FOR FUZZY COMPARISON ... ==================================
        IF (ONEMIN-HALF .LT. HALF) GOTO 1510
        FAILS=FAILS+1
        WRITE(OUT,1500)
1500    FORMAT(' FAILURE: (1-u1)-1/2 < 1/2  is false, so this program may malfunction.')
1510    continue
        X=ONEMIN
        I=1
1520    continue
        Y=X-HALF
        Z=Y-HALF
        IF (X .NE. FP1 .OR. Z .EQ. FP0) GOTO 1540
        FAILS=FAILS+1
        WRITE(OUT,1535)
1535    FORMAT(' FAILURE: comparison is fuzzy; it alleges x=1 although')
        WRITE(OUT,1537)Z
1537    FORMAT('         subtraction yields  (x - 1/2) - 1/2 = ',D16.8)
1540    continue
        IF (I .EQ. 0) GOTO 1560
        X=FP1+ULPPLS
        I=0
        GOTO 1520
1560    continue
        MILES=25
!       END OF TEST FOR FUZZY COMPARISON.===============================
        B9 = RADIX - FP1
        B9=(B9-ULPPLS)+FP1
        IF (RADIX .EQ. FP1) GO TO 1610
!       ... B9 = NEXTAFTER(RADIX, 0)
        X=-T8*DLOG(ULPMIN)/DLOG(RADIX)
        Y=DINT(HALF+X)
        IF ( DABS(X-Y)*FP4 .LT. FP1 ) X=Y
        PRECIS = X/T8
        Y=DINT(HALF + PRECIS)
        IF ( DABS(PRECIS-Y)*T8 .LT. HALF ) PRECIS=Y
!       PURIFY INTEGERS.
        IF (PRECIS .EQ. DINT(PRECIS)) GOTO 1640
1610    continue
        WRITE(OUT,1611)
1611    FORMAT(' Precision cannot be characterized by an integer number of sig. digits;')
        IF (RADIX .GT. FP1) GOTO 1625
        WRITE(OUT,1620)
1620    FORMAT(' Logarithmic encoding (radix=1) has precision characterized solely by  u1 .')
        GOTO 1650
1625    continue
        WRITE(OUT,1630)
1630    FORMAT(' but, by itself, this is a minor flaw.')
1640    continue
        WRITE(OUT,1641) RADIX,PRECIS
1641    FORMAT(' The number of significant digits of radix ',F4.0,' is ' , F6.2)
1650    continue
        IF (ULPPLS*FP9*FP9*T8 .LT. FP1) GO TO 1670
        SDEFCT=SDEFCT+1
        WRITE(OUT,1665)
1665    FORMAT(' SERIOUS DEFECT: precision worse than  5 sig. dec. is usually inadequate.')
1670    continue
        RETURN
        END SUBROUTINE RADX
!-------------------------------------------------------------------------------
        SUBROUTINE ROUND (MILES)
      implicit none
        INTEGER MILES
!!!!!
!       ... LOCAL VARIABLES
        DOUBLE PRECISION A, B1, Q, S1, T, X, Y, Y1, Y2, Z
!       ... CONSTANTS
        DOUBLE PRECISION B2, T5, B9
!
        B2 = RADIX / FP2
        T5 = FP1 + HALF
        B9 = ((RADIX - FP1) - ULPPLS) + FP1
!
        WRITE(OUT,2250)
2250    FORMAT(' Checking for rounding in multiply,',' divide and add/subtract:')
        R1 = MINUS1
        R2 = MINUS1
        R3 = MINUS1
!               IS  RADIX  A POWER OF  2  OR  10 ?
        A1 = FP2
2280    continue
        A = RADIX
2290    continue
        X = A
        A = A / A1
        IF (DINT(A) .EQ. A) GOTO 2290
        IF (X .EQ. FP1) GOTO 2340
!               RADIX  IS A POWER OF  A1; IF RADIX=1 THEN  A1=2.
        IF (A1 .GT. FP3) GOTO 2330
        A1 = FP9 + FP1
        GOTO 2280
!               IS  RADIX  A POWER OF  10 ?
2330    continue
        A1 = RADIX
!               UNLESS  B  IS A POWER OF  A1  AND  A1 = 2 OR 10.
2340    continue
        A=FP1/A1
        X=A1
        Y=A
2350    continue
        Z=X*Y-HALF
        IF (Z .EQ. HALF) GOTO 2370
        FAILS=FAILS+1
        WRITE(OUT,2361) X, Y, X, X
2361    FORMAT(' FAILURE:  1/',E13.5,' = ',E13.5,', and  ',E13.5,'*(1/',E13.5,') differs from  1.')
2370    continue
        IF (X .EQ. RADIX) GOTO 2390
        X = RADIX
        Y = FP1 / X
        GOTO 2350
2390    continue
        Y2=FP1+ULPPLS
        Y1=FP1-ULPPLS
        X=T5-ULPPLS
        Y=T5+ULPPLS
        Z=(X-ULPPLS)*Y2
        T=Y*Y1
        Z=Z-X
        T=T-X
        X=X*Y2
        Y=(Y+ULPPLS)*Y1
        X=X-T5
        Y=Y-T5
        IF (.NOT. ( X .EQ. FP0 .AND. Y .EQ. FP0 .AND. Z .EQ. FP0 .AND. T .LE. FP0 )) GOTO 2460
        X=(T5+ULPPLS)*Y2
        Y=T5-ULPPLS-ULPPLS
        Z=T5+ULPPLS+ULPPLS
        T=(T5-ULPPLS)*Y1
        X=X-(Z+ULPPLS)
        STICKY = Y * Y1
        S1 = Z * Y2
        T = T - Y
        Y = (ULPPLS-Y) + STICKY
        Z = S1 - (Z+ULPPLS+ULPPLS)
        STICKY = (Y2+ULPPLS) * Y1
        Y1 = Y2 * Y1
        STICKY = STICKY - Y2
        Y1 = Y1 - HALF
        IF ( X .EQ. FP0 .AND. Y .EQ. FP0 .AND. Z  .EQ. FP0 .AND. T .EQ. FP0 .AND. STICKY .EQ. FP0 .AND. Y1 .EQ. HALF ) R1 = FP1
        IF (X+ULPPLS.EQ.FP0.AND.Y.LT.FP0.AND.Z+ULPPLS.EQ.FP0.AND.T.LT.FP0.AND.STICKY+ULPPLS.EQ.FP0.AND.Y1.LT.HALF) R1 = FP0
        IF (R1 .EQ. FP0) WRITE(OUT,2431)
2431    FORMAT(' Multiplication appears to be chopped.')
        IF (R1 .EQ. FP1) WRITE(OUT,2441)
2441    FORMAT (' Multiplication appears to be correctly rounded.')
        IF (R1-MULGRD .EQ. FP1) WRITE(OUT,2451)
2451    FORMAT(' FAILURE: multiplication test is inconsistent; PLEASE NOTIFY KARPINSKI !')
2460    continue
        IF (R1 .EQ. MINUS1) WRITE(OUT,2461)
2461    FORMAT(' Multiplication is neither chopped nor correctly rounded.')
        MILES=45
!       ================================================================
        Y2=FP1+ULPPLS
        Y1=FP1-ULPPLS
        Z=T5+ULPPLS+ULPPLS
        X=Z/Y2
        T=T5-ULPPLS-ULPPLS
        Y=(T-ULPPLS)/Y1
        Z=(Z+ULPPLS)/Y2
        X=X-T5
        Y=Y-T
        T=T/Y1
        Z=Z-(T5+ULPPLS)
        T=(ULPPLS-T5)+T
        IF ( X .GT. FP0 .OR. Y .GT. FP0 .OR. Z .GT. FP0 .OR. T .GT. FP0) GOTO 2540
        X=T5/Y2
        Y=T5-ULPPLS
        Z=T5+ULPPLS
        X=X-Y
        T=T5/Y1
        Y=Y/Y1
        T=T-(Z+ULPPLS)
        Y=Y-Z
        Z=Z/Y2
        Y1=(Y2+ULPPLS)/Y2
        Z=Z-T5
        Y2=Y1-Y2
        Y1=(ONEMIN-ULPMIN)/ONEMIN
        IF(X.EQ.FP0.AND.Y.EQ.FP0.AND.Z.EQ.FP0.AND.T.EQ.FP0.AND.Y2.EQ.FP0.AND.Y1-HALF.EQ.ONEMIN-HALF) R2=FP1
        IF(X.LT.FP0.AND.Y.LT.FP0.AND.Z.LT.FP0.AND.T.LT.FP0.AND.Y2.LT.FP0.AND.Y1-HALF.LT.ONEMIN-HALF) R2=FP0
        IF (R2 .EQ. FP0) WRITE(OUT,2511)
2511    FORMAT (' Division appears to be chopped.')
        IF (R2 .EQ. FP1) WRITE(OUT,2521)
2521    FORMAT (' Division appears to be correctly rounded.')
        IF (R2-DIVGRD .EQ. FP1) WRITE(OUT,2531)
2531    FORMAT(' FAILURE:  division test is inconsistent; PLEASE NOTIFY KARPINSKI !')
2540    continue
        IF (R2 .EQ. MINUS1) WRITE(OUT,2541)
2541    FORMAT (' Division is neither chopped nor correctly rounded.')
!       ================================================================
        B1 = FP1 / RADIX
        IF (B1 * RADIX - HALF .EQ. HALF) GOTO 2580
        FAILS=FAILS+1
        WRITE(OUT,2570)
2570    FORMAT(' FAILURE:  radix * (1 / radix)  differs from  1.')
2580    continue
        MILES=50
!       ================================================================
        IF ( (ONEMIN + ULPMIN) - HALF .EQ. HALF         .AND.  (B9 + ULPPLS) - FP1  .EQ. RADIX - FP1 ) GOTO 2610
        FAILS=FAILS+1
        WRITE(OUT,2601)
2601    FORMAT(' FAILURE: incomplete carry-propagation in addition.')
2610    continue
        X = FP1 - ULPMIN * ULPMIN
        Y = FP1 + ULPPLS * (FP1 - ULPPLS)
        Z = ONEMIN - HALF
        X = (X - HALF) - Z
        Y = Y - FP1
        IF (X .NE. FP0 .OR. Y .NE. FP0) GOTO 2640
        R3 = FP0
        WRITE(OUT,2631)
2631    FORMAT (' Add/subtract appears to be chopped.')
2640    continue
        IF (SUBGRD .EQ. FP0) GOTO 2710
        X = (HALF + ULPPLS) * ULPPLS
        Y = (HALF - ULPPLS) * ULPPLS
        X = FP1 + X
        Y = FP1 + Y
        X = (FP1 + ULPPLS) - X
        Y = FP1 - Y
        IF (X .NE. FP0 .OR. Y .NE. FP0) GOTO 2710
        X = (HALF + ULPPLS) * ULPMIN
        Y = (HALF - ULPPLS) * ULPMIN
        X = FP1 - X
        Y = FP1 - Y
        X = ONEMIN - X
        Y = FP1 - Y
        IF (X .NE. FP0 .OR. Y .NE. FP0) GOTO 2710
        R3 = MINUS1 - FP2 * R3
        WRITE(OUT,2691)
2691    FORMAT (' Add/subtract appears to be correctly rounded.')
        IF (R3 - SUBGRD .EQ. FP1) WRITE(OUT,2701)
2701    FORMAT(' FAILURE:  add/subtract test is inconsistent; PLEASE NOTIFY KARPINSKI !')
2710    continue
        IF (R3 .EQ. MINUS1) WRITE(OUT,2711)
2711    FORMAT(' Add/subtract neither chopped nor correctly rounded.')
        S1 = FP1
        X = FP1+HALF*(FP1+HALF)
        Y = (FP1+ULPPLS)*HALF
        Z = X-Y
        T = Y-X
        STICKY = Z+T
        IF (STICKY .EQ. FP0) GOTO 2770
        S1=FP0
        FLAWS=FLAWS+1
        WRITE(OUT,2750) STICKY
        WRITE(OUT,2760) X, Y
2750    FORMAT(' FLAW: nonzero  (x-y)+(y-x) = ',E16.8,' when')
2760    FORMAT('      x = ',E16.8,'  and  y = ',E16.8)
!       ================================================================
2770    continue
        STICKY = FP0
        IF (MULGRD*DIVGRD*SUBGRD .LT. FP1 .OR.R1 .LT. FP1.OR.R2 .LT. FP1.OR.R3 .LT. FP1.OR.DINT(B2) .NE. B2) GOTO 2890
        WRITE(OUT,2780)
2780    FORMAT(' checking for sticky bit:')
        X=(HALF+ULPMIN)*ULPPLS
        Y=HALF*ULPPLS
        Z=FP1+Y
        T=FP1+X
        IF (Z-FP1 .GT. FP0 .OR. T-FP1 .LT. ULPPLS) GOTO 2890
        Z=T+Y
        Y=Z-X
        IF (Z-T .LT. ULPPLS .OR. Y-T .NE. FP0) GOTO 2890
        X=(HALF+ULPMIN)*ULPMIN
        Y=HALF*ULPMIN
        Z=FP1-Y
        T=FP1-X
        IF (Z-FP1 .NE. FP0 .OR. T-ONEMIN .NE. FP0) GOTO 2890
        Z=(HALF-ULPMIN)*ULPMIN
        T=ONEMIN-Z
        Q=ONEMIN-Y
        IF (T-ONEMIN .NE. FP0 .OR. (ONEMIN-ULPMIN)-Q .NE. FP0) GOTO 2890
        Z = (FP1 + ULPPLS) * T5
        T = (T5 + ULPPLS) - Z + ULPPLS
        X = FP1 + HALF / RADIX
        Y = FP1 + RADIX * ULPPLS
        Z = X * Y
        IF (T .NE. FP0 .OR. (X + RADIX * ULPPLS) - Z .NE. FP0) GOTO 2890
        IF (RADIX .EQ. FP2) GOTO 2870
        X = FP2 + ULPPLS
        Y = X / FP2
        IF (Y - FP1 .NE. FP0) GOTO 2890
2870    continue
        STICKY = S1
        IF (STICKY .EQ. FP1) WRITE(OUT,2881)
2881    FORMAT (' Sticky bit appears to be used correctly.')
2890    continue
        IF (STICKY .EQ. FP0)  WRITE(OUT,2891)
2891    FORMAT (' Sticky bit used incorrectly or not at all.')
        IF(MULGRD*DIVGRD*SUBGRD.EQ.FP0.OR.R1.LT.FP0.OR.R2.LT.FP0.OR.R3.LT.FP0) THEN
           FLAWS=FLAWS+1
           WRITE(OUT,29001)
29001      FORMAT(' FLAW: lack(s) of guard digits or failure(s) to correctly round or chop',/, &
         &  ' (noted above) count as one flaw in the final tally below.')
           END IF
        RETURN
        END SUBROUTINE ROUND
!-------------------------------------------------------------------------------
        SUBROUTINE SMLINT(MILES,FROM)
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!       TESTS ON SMALL INTEGERS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      implicit none
!
        INTEGER FROM
!!!!!
        INTEGER IPARTU
        INTEGER MILES
!        ... INTEGER NUMBER IF MILESTONES REACHED
        character(len=8) :: CHARZ          !        ... CHARACTER CONSTANT 'Z'
        DOUBLE PRECISION MINONE    !        ... TEMPORARY TO HOLD MINUS ONE
        DOUBLE PRECISION HALF      !        ... TEMPORARY TO HOLD ONE HALF
        DOUBLE PRECISION FIVE      !        ... TEMPORARY TO HOLD FIVE
        DOUBLE PRECISION EIGHT     !        ... TEMPORARY TO HOLD EIGHT
        DOUBLE PRECISION NINE      !        ... TEMPORARY TO HOLD NINE
        DOUBLE PRECISION TEMP12    !        ... TEMPORARY TO HOLD 12
        DOUBLE PRECISION TEMP20    !        ... TEMPORARY TO HOLD 20
        DOUBLE PRECISION TEMP27    !        ... TEMPORARY TO HOLD 27
        DOUBLE PRECISION TEMP32    !        ... TEMPORARY TO HOLD 32
        DOUBLE PRECISION TEMP48    !        ... TEMPORARY TO HOLD 48
        DOUBLE PRECISION TEMP60    !        ... TEMPORARY TO HOLD 60
        DOUBLE PRECISION TEMP80    !        ... TEMPORARY TO HOLD 80
        DOUBLE PRECISION TMP240    !        ... TEMPORARY TO HOLD 240
        DOUBLE PRECISION TEMP
!        ... TEMPORARY VARIABLE TO HOLD VARIOUS VERY SHORT TERM VALUES
        DOUBLE PRECISION TEMPZ
!        ... TEMPORARY VARIABLE TO HOLD A TEMP. PREVIOUSLY KNOWN AS Z

!        ... INITIALIZE SOME CONSTANTS
        DATA CHARZ/'Z'/
        IF (FROM .EQ. 7) GO TO 951
        IF (FROM .NE. 0) CALL BADMIL
        WRITE(OUT,891)
891     FORMAT(' Program is now RUNNING tests on small integers:')
!
!       ... LOOK FOR SOME OBVIOUS MISTAKES
        IF(0.0E0+0.0E0.EQ.0.0E0.AND.1.0E0-1.0E0.EQ.0.0E0.AND.1.0E0.GT.0.0E0.AND.1.0E0+1.0E0.EQ.2.0E0) GOTO 930
        FAILS=FAILS+1
        WRITE(OUT,920)
920     FORMAT(' FAILURE: violation of  0+0=0  or  1-1=0  or  1>0  or 1+1 = 2.')
930     continue
        TEMP=0.0E0
        TEMPZ=-TEMP
        IF (TEMPZ .EQ. 0.0E0) GOTO 960
        FAILS=FAILS+1
        WRITE(OUT,940)
        WRITE(OUT,941)
940     FORMAT(' FAILURE: comparison alleges that minus zero, obtained by')
941     FORMAT(' setting  x = 0.  and then  z = -x ,  is nonzero!')
!        ... CALL TO ROUTINE TO CHECK FOR PARTIAL UNDERFLOW USING MINUS
!        ... ZERO DON'T REALLY HAVE INFO ON WHAT A UNIT IN THE LAST
!        ... PLACE IS OR WHAT THE RADIX IS SINCE WE HAVEN'T GOTTEN TO
!        ... SUCH SOPHISTICATED STUFF YET, SO PICK SOME ARBITRARY VALUES
!        ... FOR NOW TO GET US THROUGH THIS NEXT TEST.
        ULPPLS=.001
        RADIX=1
951     continue
        CALL PARTUF(TEMPZ, CHARZ, MILES, IPARTU, FROM .EQ. 7)
!
!
960     continue
        IF (4.0E0+2.0E0*(-2.0E0) .EQ. 0.0E0 .AND. (4.0E0-3.0E0)-1.0E0.EQ. 0.0E0) GOTO 980
        FAILS=FAILS+1
        WRITE(OUT,971)
971     FORMAT(' FAILURE: violation of   3+1 = 2*2 .')
!
980     continue
        MINONE=-1.0E0
        IF(MINONE+1.0E0.EQ.0.0E0.AND.1.0E0+MINONE.EQ.0.0E0.AND.MINONE+DABS(MINONE).EQ.0.0E0.AND.MINONE+MINONE*MINONE.EQ.0.0E0) &
       & GOTO 1000
        FAILS=FAILS+1
        WRITE(OUT,991)
991     FORMAT(' FAILURE: violation of   -1 + 1 = 0 .')
!
1000    continue
        HALF=1.0E0/2.0E0
        IF (HALF+MINONE+HALF .EQ. 0.0E0) GOTO 1020
        FAILS=FAILS+1
        WRITE(OUT,1011)
1011    FORMAT(' FAILURE: violation of   1/2 - 1 + 1/2 = 0 .')
1020    continue
        MILES=10
        NINE=3.0E0*3.0E0
        TEMP27=NINE*3.0E0
        EIGHT=4.0E0+4.0E0
        TEMP32=4.0E0*EIGHT
        IF (TEMP32-TEMP27-4.0E0-1.0E0 .EQ. 0.0E0) GOTO 1120
        FAILS=FAILS+1
        WRITE(OUT,1111)
1111    FORMAT(' FAILURE: violation of   32 - 27 - 4 - 1 = 0 .')
!
1120    continue
        FIVE=4.0E0+1.0E0
        TEMP20=4.0E0*FIVE
        TEMP12=3.0E0*4.0E0
        TMP240=TEMP20*TEMP12
        TEMP80=TMP240/3.0E0
        TEMP60=TMP240/4.0E0
        TEMP48=TMP240/FIVE
        TEMP80=TEMP80-4.0E0*TEMP20
        TEMP60=TEMP60-FIVE*TEMP12
        TEMP48=TEMP48-4.0E0*TEMP12
        IF ( TEMP80 .EQ. 0.0E0 .AND. TEMP60 .EQ. 0.0E0 .AND. TEMP48 .EQ. 0.0E0 ) GOTO 1150
        FAILS=FAILS+1
        WRITE(OUT,1141)
1141    FORMAT(' FAILURE: violation of 240/3 = 80 or 240/4 = 60 or 240/5 = 48 .')
1150    continue
        IF (FAILS .NE. 0) GOTO 1160
        WRITE(OUT,1151)
1151    FORMAT (' -1, 0, 1/2 , 1, 2, 3, 4, 5, 9, 27, 32 & 240 are O.K.')
1160    continue
        RETURN

        END SUBROUTINE SMLINT
!-------------------------------------------------------------------------------
        SUBROUTINE SQUARE (FROM, MILES, NUMTRY)
      implicit none
!
        INTEGER FROM, MILES, NUMTRY
!       ... LOCAL VARIABLES
        DOUBLE PRECISION D, D4, E5, E6, E7, Q, U, X, X1, X8, Y, Y1, Y2, Z, Z1, Z2
        DOUBLE PRECISION TEMP,TEMP1,TEMP2
        INTEGER I, J
!       ... CONSTANTS
        DOUBLE PRECISION B2
        DOUBLE PRECISION B9, B1
!
!                       TRAP INTEGER OVERFLOWS
!                       OTHER EXCEPTIONS ARE TRAPPED BY DEFAULT
        B2 = RADIX / FP2
        B9 = ((RADIX - FP1) - ULPPLS) + FP1
        B1 = FP1 / RADIX
        IF (FROM .EQ. 79) GO TO 3058
        IF (FROM .NE. 0) CALL BADMIL
!
        WRITE(OUT,3021)
3021    FORMAT(/' Running tests of square root...')
        MILES = 79
        CALL LOGIT(MILES)
        X = FP0
        I = 0
3030    continue
        Y = DSQRT(X)
        IF (Y .EQ. X .AND. Y - HALF .EQ. X - HALF) GOTO 3050
        FAILS = FAILS + 1
        WRITE(OUT,3041) X, Y
3041    FORMAT(' FAILURE:  sqrt(',E9.1,'), miscalculated as ',E15.7)
3050    continue
        X = -X
        I = I + 1
        IF (I .EQ. 1) GOTO 3030
        GO TO 3060
3058    continue
        WRITE(OUT,3059)
3059    FORMAT(' FAILURE:  sqrt(-0.0) stops the machine.')
        FAILS = FAILS + 1
        I = 2
3060    continue
        X = FP1
        I = I + 1
        IF (I .EQ. 3) GOTO 3030
!       ... RECORD MIN AND MAX ERRORS.
        E5 = FP0
        E7 = FP0
!       ... TEST WHETHER DSQRT(X*X)  =  X
        J = 0
        X = RADIX
        U = ULPPLS
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
        X = B1
        U = B1 * ULPMIN
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
        X = W
        U = FP1
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
        X = ULPMIN
        U = ULPMIN * ULPMIN
        CALL SQRERR (X, U, J, E5, E7, .TRUE.)
!                       IF SQRT HAS SERIOUS DEFECTS, THEN PAUSE
        IF (J .EQ. 0) GOTO 3210
        SDEFCT = SDEFCT + J
        CALL PAGE(MILES)
!
!
3210    continue
        WRITE(OUT,3211) NUMTRY
3211    FORMAT(' Testing if  sqrt(x*x)  =  x  for  ',I4,' integers  x.')
        J = 0
        X = FP2
        Y = RADIX
        IF (RADIX .EQ. FP1) GOTO 3240
!                       LOOP TO DETERMINE ??
3230    continue
        X = Y
        Y = RADIX * X
        IF (Y - X .LT. NUMTRY) GOTO 3230
3240    continue
        U = X*ULPPLS
!
        DO 3260 I = 1,NUMTRY
        X = X + FP1
        CALL SQRERR (X, U, J, E5, E7, .FALSE.)
        IF (J .GT. 0) GOTO 3280
3260    CONTINUE
!
        WRITE(OUT,3270)
3270    FORMAT(' Found no discrepancies.')
        GOTO 3300
3280    continue
        DEFECT = DEFECT + J
!
!                       TEST SQRT FOR MONOTONICITY.
!
3300    continue
        I = -1
        X = B9
        Y = RADIX
        Z = RADIX + RADIX * ULPPLS
!
!                       LOOP
!
3310    continue
        I = I + 1
        X = DSQRT(X)
        Q = DSQRT(Y)
        Z = DSQRT(Z)
        IF (.NOT. (X .GT. Q .OR. Q .GT. Z)) GOTO 3330
        DEFECT = DEFECT + 1
        WRITE(OUT,3321) Y
3321    FORMAT(' DEFECT:  sqrt(x) is non - monotonic for  x  near ',E15.7)
        GOTO 3390
3330    continue
        Q = DINT(Q + HALF)
        IF (.NOT. (I .GT. 0 .OR. Q*Q .EQ. RADIX)) GOTO 3380
        IF (I .GT. 0) GOTO 3360
        Y = Q
        X = Y - ULPPLS
        Z = Y + ULPPLS
        GOTO 3310
!
3360    continue
        IF (I .GT. 1) GOTO 3380
        Y = Y * B1
        X = Y - ULPMIN
        Z = Y + ULPMIN
        GOTO 3310
3380    continue
        WRITE(OUT,3381)
3381    FORMAT(' Sqrt has passed a test for monotonicity.')
3390    continue
        MILES = 80
!
!       TEST SQRT FOR ACCURACY  =====================================
!               E5 = MIN{ERROR + 1/2}
!               E7 = MAX{ERROR - 1/2}
!
        E5 = E5 + HALF
        E7 = E7 - HALF
        Y = (DSQRT(FP1 + ULPPLS) - FP1)/ULPPLS
        E6 = (Y - FP1) + ULPPLS/FP8
        IF (E6 .GT. E7) E7 = E6
        E6 = Y + ULPPLS/FP8
        IF (E6 .LT. E5) E5 = E6
        Y = ((DSQRT(ONEMIN) - ULPPLS) - (FP1 - ULPPLS))/ULPMIN
        E6 = Y + ULPMIN/FP8
        IF (E6 .GT. E7) E7 = E6
        E6 = (Y + FP1) + ULPMIN/FP8
        IF (E6 .LT. E5) E5 = E6
        I = 0
        U = ULPPLS
        X = U
!
!                       LOOP
!
        do
           I = I + 1
           Y = DSQRT((X + ULPMIN + X) + ONEMIN)
           Y = ((Y - ULPPLS) - ((FP1 - ULPPLS) + X))/U
           Z = ((ULPMIN - X) + ONEMIN)*HALF*X*X/U
           E6 = (Y + HALF) + Z
           IF (E6 .LT. E5) E5 = E6
           E6 = (Y - HALF) + Z
           IF (E6 .GT. E7) E7 = E6
           IF (I .EQ. 4) GOTO 3530
           IF (I .EQ. 2) GOTO 3520
           X = U * DSIGN(FP1,X) * DINT( FP8 / (FP9 * DSQRT(U)) )
        cycle
!
3520       continue
           U = ULPMIN
           X = -U
        enddo
!
3530    continue
        MILES = 85
        R4 = MINUS1
        IF (RADIX .EQ. FP1) GOTO 3900
        WRITE(OUT,3551)
3551    FORMAT(' Testing whether  sqrt  is rounded or chopped:')
        D = DINT(HALF + RADIX ** (FP1 + PRECIS - DINT(PRECIS)))
!
!       ...  =  B^(1 + FRACT)  IF  P  =  INTEGER  +  FRACT.
!
        X = D / RADIX
        Y = D / A1
        IF (X .NE. DINT(X) .OR. Y .NE. DINT(Y)) GOTO 3700
        X = FP0
        Z2 = X
        Y = FP1
        Y2 = Y
        Z1 = RADIX - FP1
        D4 = FP4 * D
!
!       LOOP: FOR  Y  =  1, 3, 5, ...  MAXIMIZE  Y2  =  Y*Y MOD 4D .
!
3600    continue
        IF (.NOT. (Y2 .GT. Z2)) GOTO 3650
        Q = RADIX
        Y1 = Y
!                       IF NEW Y2 > OLD, CHECK THAT  GCD(Y,B)  =  1
3620    continue
        TEMP = HALF - Q / Y1
        TEMP1 = DINT(TEMP)
        IF (TEMP1 .GT. TEMP) TEMP1 = TEMP1 - FP1
        X1 = DABS(Q + TEMP1 * Y1)
        Q = Y1
        Y1 = X1
        IF (X1 .GT. FP0) GOTO 3620
!
        IF (Q .GT. FP1) GOTO 3650
!                       IF GCD(Y,B)  .GT.  1 THEN SKIP OVER Y ;  ELSE
        Z2 = Y2
        Z = Y
!                       AND GCD(Z, RADIX)  = 1
3650    continue
        Y = Y + FP2
        X = X + FP8
        Y2 = Y2 + X
        IF (.NOT. (Y2 .LT. D4)) Y2 = Y2 - D4
!                       =  Y*Y MOD 4D
        IF (Y .LT. D) GOTO 3600
!                       ELSE  0 < Z < D  &  Z2 = Z^2 MOD 4D  IS MAXIMAL
        X8 = D4 - Z2
        Q = (X8 + Z * Z) / D4
        X8 = X8 / FP8
        IF (Q .NE. DINT(Q)) GOTO 3700
3680    continue
        X = Z1 * Z
        X = X - DINT(X / RADIX) * RADIX
        IF (X .EQ. FP1) GOTO 3800
!                       WITH  1  =  Z*Z1 MOD B
        Z1 = Z1 - FP1
        IF (Z1 .GT. FP0) GOTO 3680
!                       ELSE FAILURE!
3700    continue
        FAILS = FAILS + 1
        WRITE(OUT,3701) W
        WRITE(OUT,3702)
3701    FORMAT(' FAILURE: anomalous arithmetic with integers < b^p  = ', E15.7)
3702    FORMAT ('         foils test whether  sqrt  rounds or chops.')
        GOTO 3940
!
!                       - B/2   <=   Z1 == 1/Z MOD B   <=   B/2
!
3800    continue
        IF (Z1 .GT. B2) Z1 = Z1 - RADIX
!
!                       LOOP UNTIL  D  =  B^(P - 1) .
!
3810    continue
        CALL NEWD (X, Z1, Q, Z, D)
        IF (ULPPLS * D .LT. ONEMIN) GOTO 3810
!
        IF (D * RADIX - D .NE. W - D) GOTO 3700
        Z2 = D
        I = 0
!               COUNT HOW MANY TESTS OF DSQRT(D*X) = Y YIELD RESULTS.
        Y = D + (FP1 + Z) * HALF
        X = D + Z + Q
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
        Y = D + (FP1 - Z) * HALF + D
        X = D - Z + D
        X = X + Q + X
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
        CALL NEWD (X, Z1, Q, Z, D)
        IF (D - Z2 .NE. W - Z2) GOTO 3700
        Y = (D - Z2) + (Z2 + (FP1 - Z) * HALF)
        X = (D - Z2) + (Z2 - Z + Q)
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
        Y = (FP1 + Z) * HALF
        X = Q
        CALL SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
        IF (I .EQ. 0) GOTO 3700
3900    continue
        IF (E5 .LT. 0 .OR. E7 .GT. 0) GOTO 3920
        R4 = FP1
        WRITE(OUT,3911)
3911    FORMAT (' Square root appears to be correctly rounded.')
        RETURN
!
3920    continue
        IF (E7 + ULPPLS .GT. ULPPLS - HALF .OR.  E5          .GT. HALF          .OR.  E5 + RADIX  .LT. HALF) GOTO 3940
        R4 = FP0
        WRITE(OUT,3931)
3931    FORMAT (' Square root appears to be chopped.')
        RETURN
3940    continue
        WRITE(OUT,3941)
        TEMP=E5-HALF
        TEMP2=HALF+E7
        WRITE(OUT,3942) TEMP, TEMP2
3941    FORMAT (' Square root is neither chopped nor correctly',' rounded.')
3942    FORMAT(' Observed errors run from  ',E15.7,'  to  ',E15.7,' ulps.')
        IF (E7 - E5 .LT. RADIX * RADIX) RETURN
        SDEFCT = SDEFCT + 1
        WRITE(OUT,3951)
3951    FORMAT(' SERIOUS DEFECT: sqrt gets too many last digits wrong.')
        RETURN
        END SUBROUTINE SQUARE
!-------------------------------------------------------------------------------
!       ____ SUBROUTINE TO ASSESS ERROR  DSQRT(X*X) - X  IN ULPS. ____
!
        SUBROUTINE SQRERR (X, U, J, E5, E7, SEROUS)
      implicit none
!!!!!
        INTEGER J
        DOUBLE PRECISION X, U,    E5, E7
        LOGICAL SEROUS
        DOUBLE PRECISION E6, B1
        B1 = 1.0 / RADIX
        E6 = ((DSQRT(X * X) - X * B1) - (X - X * B1)) / U
        IF (E6 .EQ. 0.0) RETURN
        IF (E6 .LT. E5) E5 = E6
        IF (E6 .GT. E7) E7 = E6
        J = J + 1
        IF (.NOT. SEROUS) WRITE(OUT,31210) X*X, X, U*E6
        IF (SEROUS) WRITE(OUT,31211) X*X, X, U*E6
31210   FORMAT (' DEFECT: sqrt(', E15.7,') - ',E15.7,'  =  ', E15.7)
31211   FORMAT (' SERIOUS DEFECT: sqrt(', E15.7,') - ',E15.7,'  =  ',E15.7)
        WRITE(OUT,3122)
3122    FORMAT(' instead of correct value  0 .')
        RETURN
        END SUBROUTINE SQRERR
!-------------------------------------------------------------------------------
!       THIS SUBROUTINE PUTS  NEWD = B*D  AND
!                             NEWZ^2 MOD NEWD = Z^2 MOD D
!
        SUBROUTINE NEWD (X, Z1, Q, Z, D)
      implicit none
!!!!!
        DOUBLE PRECISION X, Z1, Q, Z, D
        DOUBLE PRECISION TEMP, TEMP1
!
        X = Z1 * Q
        TEMP = HALF - X / RADIX
        TEMP1 = DINT(TEMP)
        IF (TEMP1 .GT. TEMP) TEMP1 = TEMP1 - FP1
        X = TEMP1 * RADIX + X
        Q = (Q - X*Z) / RADIX + X * X * (D / RADIX)
        Z = Z - FP2 * X * D
        IF (Z .GT. FP0) GOTO 3740
        Z = -Z
        Z1 = -Z1
3740    continue
        D = RADIX * D
        RETURN
        END SUBROUTINE NEWD
!-------------------------------------------------------------------------------
!       THIS SUBROUTINE TESTS IF
!               DSQRT(D*X) = DSQRT((Y - 1/2)^2 + X8/2) ROUNDS TO  Y
!
        SUBROUTINE SQRTDX (X, Z2, I, D, Y2, Y, X8, E5, E7)
      implicit none
!
!!!!!
!
        DOUBLE PRECISION X, Z2, X2, D, Y2, Y, X8, E5, E7
        INTEGER I
        DOUBLE PRECISION E6
!
        IF (X - RADIX .LT. Z2 - RADIX .OR. X - Z2 .GT. W - Z2) RETURN
        I = I + 1
        X2 = DSQRT(X * D)
        Y2 = (X2 - Z2) - (Y - Z2)
        X2 = X8/(Y - HALF)
        X2 = X2 - HALF * X2 * X2
        E6 = (Y2 + HALF) + (HALF - X2)
        IF (E6 .LT. E5) E5 = E6
        E6 = Y2 - X2
        IF (E6 .GT. E7) E7 = E6
        RETURN
        END SUBROUTINE SQRTDX
!-------------------------------------------------------------------------------
        SUBROUTINE UNDERF(MILES, NUMTRY, FROM)
      implicit none
        INTEGER           MILES, NUMTRY, FROM
!        MILES  ... NUMBER OF MILESTONE REACHED SO FAR IN TESTING
!        NUMTRY ... NUMBER OF TIMES TO TRY RANDOM TRIALS
!        FROM   ... NUMBER OF MILESTONE TO RETURN TO ON RESTART
!!!!!
        INTEGER ACCUR, ERROR, I, IQ, PARTU
!        ACCUR  ... FLAG TO INDICATE SUCCESS/FAILURE OF ACCURACY TESTS
!        ERROR  ... COUNT OF ERRORS DETECTED TESTING POWERS.
!        I      ... SCRATCH FOR ENUMERATING CASES
!        IQ     ... TEMPORARY FOR HOLDING INTEGER EXPONENTS
!        PARTU  ... FLAG TO INDICATE THE DETECTION OF PARTIAL UNDERFLOW
!
        DOUBLE PRECISION C, EPSP1, EXP2, H, MINDIF
!        C      ... 1/(RADIX^LARGE_INTEGER)
!        EPSP1  ... EPSILON + 1 (1 + (SMALL INTEGER)* 1 ULP OF 1+...)
!        EXP2   ... VALUE OF E ^ 2
!        H      ... MIN (1/RADIX, 1/2)
!        MINDIF ... MINIMUM POSITIVE NUMBER FOUND BY ADDITION/SUBTR.
!
!        ... LOCAL VARIABLES
        DOUBLE PRECISION D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9,TEMP
        character(len=8) :: CHARZ0, CHARE0
!        CHARZ0 ... CHARACTER CONSTANT 'Z0'
!        CHARE0 ... CHARACTER CONSTANT 'E0'
        DATA CHARZ0/ ' PHONY0'/,CHARE0/ ' MINPOS'/
!
        IF(FROM .EQ. 0 ) GO TO 4330
!          WE MUST BE DOING A RESTART.  FIGURE OUT WHERE, AND GO DO IT
!       MUST READ THE LOG FILE BACK IN.
        READ(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        READ(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        IF (FROM .EQ. 105) GO TO 4390
        IF (FROM .EQ. 106) GO TO 4410
        IF (FROM .EQ. 107) GO TO 4450
        IF (FROM .EQ. 108) PHONY0 = 0
        IF (FROM .EQ. 108) GO TO 4522
        IF (FROM .EQ. 109) GO TO 4860
        IF (FROM .EQ. 115) GO TO 4631
        IF (FROM .EQ. 120) GO TO 4890
        IF (FROM .EQ. 121) GO TO 4941
        IF (FROM .EQ. 122) GO TO 5011
        IF (FROM .EQ. 123) GO TO 5160
        IF (FROM .EQ. 124) GO TO 5190
        IF (FROM .EQ. 125) GO TO 5175
        IF (FROM .EQ. 131) GO TO 53021
!               MAKES NO SENSE TO TALK ABOUT UNDERFLOW STICKING, SINCE
!               UNDERFLOW ABORTS THE PROGRAM....
        CALL BADMIL
4330    continue
        WRITE(OUT,4335)
4335    FORMAT(' Seeking underflow threshold and min positive number:')
        MILES = 105
        CALL LOGIT(MILES)
        D = ULPMIN
        IF (PRECIS .EQ. DINT(PRECIS)) GOTO 4370
        D = FP1 / RADIX
        X = PRECIS
4360    continue
        D = D / RADIX
        X = X - FP1
        IF (X .GT. FP0) GO TO 4360
!       IF NON-INTEGRAL PRECISION NOW HAVE D = 1 RIGHT SHIFTED BY PRECIS
!       DIGITS (IN BASE "RADIX")
!       IF INTEGRAL PRECISION, ULPMIN IS THIS NUMBER - PRE-COMPUTED.
4370    continue
        Y = FP1
        Z = D
!       ... D = A POWER OF  1/RADIX < 1
4380    continue
        C=Y
        Y=Z
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Z=Y*Y
        IF (Y .GT. Z .AND. Z+Z .GT. Z) GO TO 4380
!          MILESTONE 106
4390    continue
        MILES = 106
        CALL LOGIT(MILES)
        Y=C
        Z=Y*D
4400    continue
        C=Y
        Y=Z
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Z=Y*D
        IF (Y .GT. Z .AND. Z+Z .GT. Z) GO TO 4400
!       MILESTONE 107
4410    continue
        MILES = 107
        CALL LOGIT(MILES)
        H1=RADIX
        IF (H1 .LT. FP2) H1=FP2
        H=FP1/H1
!        ... 1/H1 = H = MIN{ 1/RADIX, 1/2 }
        C1=FP1/C
        MINPOS=C
        Z=MINPOS*H
!       ... C = 1/RADIX^(BIG INTEGER) << 1 << C1 = 1/C
4440    continue
        Y=MINPOS
        MINPOS=Z
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Z=MINPOS*H
        IF (MINPOS .GT. Z .AND. Z+Z .GT. Z) GO TO 4440
!       MILESTONE 108
4450    continue
        MILES = 108
        CALL LOGIT(MILES)
        UFLTHR = MINPOS
        MINDIF=FP0
        Q=FP0
        NULPS=ULPPLS
        EPSP1=FP1+NULPS
        D=C*EPSP1
        IF (D .GT. C) GO TO 4490
        NULPS=RADIX*ULPPLS
        EPSP1=FP1+NULPS
        D=C*EPSP1
        IF (D .GT. C) GO TO 4490
        WRITE(OUT,4470)
4470    FORMAT(' FAILURE: multiplication  gets too many last digits wrong.')
!       ... MULTIPLICATION IS TOO CRUDE
        FAILS=FAILS+1
        T0 = MINPOS
        Y1=FP0
        PHONY0 = Z
        CALL PAGE(MILES)
        GOTO 4570
4490   continue
        T0=D
        PHONY0=T0*H
        UFLTHR=FP0
4500    continue
        Y1=T0
        T0=PHONY0
        IF (MINDIF+MINDIF .GT. MINDIF) GO TO 4520
        Y2 = T0 * H1
        MINDIF=DABS(Y1-Y2)
        Q=Y1
        IF (UFLTHR .EQ. FP0 .AND. Y1 .NE. Y2) UFLTHR=Y1
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        MILES = 108
        CALL LOGIT(MILES)
4520    continue
        PHONY0=T0*H
        IF (T0 .GT. PHONY0 .AND. PHONY0+PHONY0 .GT. PHONY0) GO TO 4500
4522    continue
        MILES = 109
        CALL LOGIT(MILES)
!        ... NOW  1 >> C=1/RADIX^(INTEGER)  >=   Y    >   MINPOS=Y*H
!                  >~ Z:=MINPOS*H >~ 0 ,
!        ... AND  1 >> D=(1+NULPS)*C >= UFLTHR >= Q >= Y1 > T0:=Y1*H
!                  >~ PHONY0:=T0*H >~ 0 ,
!        ... AND  UFLTHR = D/RADIX^INTEGER  IS FIRST TO VIOLATE
!                  (UFLTHR*H)/H=UFLTHR , ELSE  UFLTHR=0 ;
!        ... AND  Q:=UFLTHR/RADIX^INTEGER  IS FIRST WITH  MINDIF :=
!                  |(Q*H)/H - Q| > 0, ELSE Q=Y1.
4570    continue
        IF (PHONY0 .EQ. FP0) GO TO 4860
!        ... TEST  PHONY0  FOR 'PHONEY-ZERO' VIOLATING  PHONY0<T0 OR
!                  PHONY0<PHONY0+PHONY0  ...
        WRITE(OUT,4590)
4590    FORMAT(/)
        Z=PHONY0
        IF (PHONY0 .GT. FP0) GOTO 4620
        FAILS=FAILS+1
        WRITE(OUT,4601)
4601    FORMAT(' FAILURE:  positive expressions can underflow to an allegedly')
        WRITE(OUT,4602)PHONY0
4602    FORMAT('          negative value z0 that prints out as ',E16.8)
        X=-PHONY0
        IF (X .GT. 0) GO TO 4630
        WRITE(OUT,4610)X
4610    FORMAT('          but  -z0, which should then be positive, isn''t; it prints out as', E16.8)
        GOTO 4630
4620    continue
        FLAWS=FLAWS+1
        WRITE(OUT,4623)
4623    FORMAT(' FLAW: underflow can stick at an allegedly positive value  z0')
        WRITE(OUT,4626)PHONY0
4626    FORMAT( '       that prints out as ', E16.8)
4630    continue
        MILES=115
!       PARTUF INCLUDES CALL LOGIT(MILES)
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
4631    continue
        CALL PARTUF (Z, CHARZ0, MILES, PARTU, FROM .EQ. 115)
!       ... END OF TEST FOR 'PHONEY-ZERO'.
4860    continue
        MILES=120
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
!       ==============================================================
        IF (C1*Y .LE. C1*Y1) GO TO 4890
!       ... AS HAPPENS ON MOST MACHINES.
        EPSP1=H*EPSP1
        MINPOS=T0
!       = LEAST POSITIVE NO. ON HP 3000
4890    continue
        IF (MINDIF .EQ. 0 .OR. MINDIF .EQ. MINPOS) GO TO 4930
        IF (MINDIF .LT. MINPOS) GO TO 4920
        DEFECT=DEFECT+1
        WRITE(OUT,4912)
4912    FORMAT(' DEFECT: differences underflow at a higher threshold than products.')
        GOTO 4930
4920    continue
        DEFECT=DEFECT+1
        WRITE(OUT,4922)
4922    FORMAT(' DEFECT: products underflow at a higher threshold than differences.')
        IF (PHONY0 .EQ. FP0) MINPOS=MINDIF
!       ... BUT NOT IF PSEUDO-ZEROS EXIST.
4930    continue
        WRITE(OUT,4935) MINPOS
4935    FORMAT(' Smallest strictly positive number found is  minpos  =',1PE16.8)
        Z = MINPOS
        MILES = 121
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
4941    continue
        CALL PARTUF (Z, CHARE0, MILES, PARTU, FROM .EQ. 121)
        T0=MINPOS
        IF (PARTU .EQ. 1) T0=Y
!       FOR CDC 7600
        I=4
        IF (MINDIF .EQ. FP0) I=3
!       ...  I=1 IF MINDIF=0=UFLTHR  ,   I=2 IF MINDIF>0=UFLTHR  ,
        IF (UFLTHR .EQ. FP0) I=I-2
!           ...  I=3 IF MINDIF=0<UFLTHR  ,   I=4 IF MINDIF>0 & UFLTHR>0
        GOTO (4980, 5090, 5010, 5130),I
!       ... CASE STATEMENT
4980    continue
        UFLTHR=T0
        IF (C1*Q .EQ. (C1*Y)*EPSP1) GO TO 5010
        FAILS=FAILS+1
        UFLTHR=Y
        WRITE(OUT,4993)
4993    FORMAT(' FAILURE: either accuracy deteriorates as numbers approach a threshold')
        WRITE(OUT,4996)UFLTHR,C
4996    FORMAT(' of ',E16.8,' coming down from  ',E16.8,',')
        WRITE(OUT,4997)
4997    FORMAT(' or else  multiplication  gets too many last digits wrong.')
        CALL PAGE(MILES)
!
!        ___ TEST FOR  X-Z = 0  ALTHOUGH  X  .NE.  Z ___
!
5010    continue
        MILES = 122
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        R = DSQRT(T0 / UFLTHR)
        GO TO 5012
5011    continue
        R = FP1
5012    continue
        IF (R .GT. H) GOTO 5030
        Z=R*UFLTHR
        X=Z*(FP1+R*H*(FP1+H))
        GOTO 5040
5030    continue
        Z=UFLTHR
        X=Z*(FP1+H*H*(FP1+H))
5040    continue
        MILES = 123
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        IF (X .EQ. Z .OR. X-Z .NE. FP0) GO TO 5160
        FLAWS=FLAWS+1
        WRITE(OUT,5055)X,Z
5055    FORMAT(' FLAW:  x =',E16.8,' is unequal to  z =',E16.8,' ,')
        Z9 = X - Z
        WRITE(OUT,5057) Z9
5057    FORMAT(' yet  x-z  yields ', E15.7)
        WRITE(OUT,5060)
5060    FORMAT(' Should this not signal underflow, this is a SERIOUS DEFECT that causes confusion when innocent statements like')
        WRITE(OUT,5063)
5063    FORMAT(' if (x.eq.z) then ... else ... ( f(x)-f(z) )/(x-z) ...')
        WRITE(OUT,5070)+(X/Z-HALF)-HALF
5070    FORMAT(' encounter division by zero although actually  x/z = 1 +',E16.8)
        GO TO 5160
!       ... END OF TEST FOR  X-Z = 0  &  X  .NE.  Z
5090    CONTINUE
!        CASE I=2
!       UFLTHR = 0 < MINDIF  !
        FAILS=FAILS+1
        WRITE(OUT,5102)
5102    FORMAT(' FAILURE: underflow confuses comparison, which alleges that  q = y ')
        WRITE(OUT,5104)
5104    FORMAT('         while denying that  |q-y| = 0 ; these values print out as')
        TEMP=DABS(Q-Y2)
        WRITE(OUT,5106)Q,Y2,TEMP
5106    FORMAT(' q =',E16.8,',  y =',E16.8,',  |q-y| =',E16.8,' ,')
        TEMP = Q/Y2 - HALF
        WRITE(OUT,5110) TEMP - HALF
5110    FORMAT(' and  q/y = 1 + ',E16.8)
        UFLTHR=Q
        GOTO 5010
!        CASE I=4 ;  UFLTHR > 0  &  MINDIF > 0
5130    CONTINUE
        IF (.NOT. (Q .EQ. UFLTHR .AND. MINDIF .EQ. MINPOS .AND. DABS(UFLTHR-MINDIF/NULPS) .LE. MINDIF)) GO TO 5010
        WRITE(OUT,5150)
        WRITE(OUT,5155)
5150    FORMAT(' Underflow is gradual; it incurs  absolute error = ')
5155    FORMAT(' (roundoff in underflow threshold) < minpos.')
        Y=MINPOS*C1
        Y=Y*(1.5E0+ULPPLS)
        X=C1*(FP1+ULPPLS)
        Y=Y/X
        IEEE=0
        IF (Y .EQ. MINPOS) IEEE=1
!       ... IEEE=1 UNLESS GRADUAL UNDERFLOWS ARE DOUBLY ROUNDED.)
5160    continue
        WRITE(OUT,5163)UFLTHR
5163    FORMAT(' The  underflow threshold is ',E16.8,' , below which')
        WRITE(OUT,5165)
5165    FORMAT(' calculation may suffer larger relative error than merely roundoff.')
        MILES = 124
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Y2=ULPMIN*ULPMIN
        Y=Y2*Y2
        MILES = 125
        WRITE(3) ACCUR, C, EPSP1, ERROR, EXP2, H, I, IQ, MINDIF, PARTU
        WRITE(3) D, Q, R, T0, V9, X, Y, Y1, Y2, Z, Z9
        REWIND 3
        CALL LOGIT(MILES)
        Y2=Y*ULPMIN
5175    continue
        IF (Y2 .GT. UFLTHR) GO TO 5220
        IF (Y .GT. MINPOS) GO TO 5200
5190    continue
        SDEFCT=SDEFCT+1
        I=4
        WRITE(OUT,5195)
5195    FORMAT(' SERIOUS ')
        GOTO 5210
5200    continue
        DEFECT=DEFECT+1
        I=5
5210    continue
        WRITE(OUT,5212)I
5212    FORMAT(' DEFECT:  range is too narrow;   ulpmin^',I5,'  underflows.')
5220    continue
        MILES=130
        CALL PAGE(MILES)
!       ---- PAUSE ---- ==================================
        Y = -DINT(HALF - 240.0 * DLOG(UFLTHR) / DLOG(H1)) / 240
        Y2=Y+Y
        WRITE(OUT,5240)H1,Y
5240    FORMAT(' since underflow occurs below the threshold  ='/10X,'(',1PE16.8,')^(',1PE16.8,') ,')
        WRITE(OUT,5245)H1,Y2
5245    FORMAT(' only underflow should afflict the expression'/10X,'(',1PE16.8,')^(',1PE16.8,') ;')
        WRITE(OUT,5247)
5247    FORMAT(' actually calculating it yields   ')
        MILES = 131
        CALL LOGIT(MILES)
        V9 = H1 ** (Y2)
        WRITE(OUT,5255) V9
5255    FORMAT(1X,E16.8)
        IF (V9 .GE. FP0 .AND. V9 .LE. (RADIX+RADIX*NULPS)*UFLTHR)GO TO 5270
        SDEFCT=SDEFCT+1
        WRITE(OUT,5263)
5263    FORMAT(' SERIOUS')
        GOTO 5300
5270    continue
        IF (V9 .GT. UFLTHR*(FP1+NULPS)) GO TO 5290
        WRITE(OUT,5280)
5280    FORMAT(' This computed value is O.K.')
        GOTO 5310
5290    continue
        DEFECT=DEFECT+1
5300    continue
        WRITE(OUT,5302)UFLTHR
5302    FORMAT(' DEFECT: this is not between 0 and  underflow threshold=',E16.8)
        GO TO 5310
53021   continue
        FLAWS = FLAWS + 1
        WRITE(OUT,53022)
53022   FORMAT(' FLAW: underflow trap from ** .')
5310    continue
        MILES=140
!       ======================================================
!       CALCULATE  EXP2 = EXP(2) = 7.389056099...
        X=FP0
        I=2
        Y=FP2*FP3
        Q=FP0
        ACCUR=0
5340    continue
        Z=X
        I=I+1
        Y=Y/(I+I)
        R=Y+Q
        X=Z+R
        Q=(Z-X)+R
        IF (X .GT. Z) GO TO 5340
        Z=(1.5E0+FP1/FP8)+X/(1.5E0 * FP32)
        X=Z*Z
        EXP2=X*X
        X=ONEMIN
        Y=X-ULPMIN
        WRITE(OUT,5360) EXP2
5360    FORMAT(' Testing  x^((x+1)/(x-1)) vs. exp(2) = ',E16.8,'  as  x-> 1.')
5370    continue
        DO 5415 I=1 , NUMTRY
        Z=X-(1/RADIX)
        Z=(X+FP1)/(Z-(FP1-(1/RADIX)))
        Q=X**Z-EXP2
        IF (DABS(Q) .GT. 240. * ULPPLS) GO TO 5420
        Z=(Y-X)*FP2+Y
        X=Y
        Y=Z
        Z = FP1+(X-ONEMIN)*(X-ONEMIN)
        IF (Z .LE. FP1) GOTO 5400
5415    CONTINUE
5400    continue
        IF (X .GT. FP1) GO TO 5440
        X=FP1+ULPPLS
        Y=ULPPLS+ULPPLS+X
        GOTO 5370
5420    continue
        ACCUR=1
        DEFECT=DEFECT+1
        TEMP=+(X-(1/RADIX))-(FP1-(1/RADIX))
        WRITE(OUT,5425)TEMP,Z
5425    FORMAT(' DEFECT:  calculated  (1 + (',E16.8,'))^(',E16.8,')')
        WRITE(OUT,5427)Q
5427    FORMAT('         differs from correct value by  ',E16.8)
        WRITE(OUT,5430)
5430    FORMAT(' This much error may spoil financial calculations involving tiny interest rates.')
        GOTO 5450
5440    continue
        IF (ACCUR .EQ. 0) WRITE(OUT,5445)
5445    FORMAT(' Accuracy seems adequate.')
5450    continue
        MILES=150
!       =======================================================
        WRITE(OUT,5460)
5460    FORMAT(' Testing powers  z^q  at four nearly extreme values:')
        ERROR=0
        Z=A1
        IQ =IDINT(HALF-DLOG(C) / DLOG(A1))
5470    continue
        X=C1
        CALL CMPXY(X,Y,Z,IQ,ERROR)
        IQ=-IQ
        X=C
        CALL CMPXY(X,Y,Z,IQ,ERROR)
        IF (Z .LT. FP1) GO TO 5490
        continue
        Z=1/A1
        GOTO 5470
5490    continue
        CALL PRTCNT (ERROR)
        CALL PRT2(ERROR,MILES)
!       ... PRINT COUNT OF DISCREPANCIES.
        continue
        MILES=160
        RETURN
        END SUBROUTINE UNDERF
!-------------------------------------------------------------------------------
        SUBROUTINE ZEROS(MILES,FROM)
      implicit none
        INTEGER MILES
!!!!!
!       MILESTONE REACHED SO FAR
        INTEGER FROM
!       MILESTONE TO RESTART AT
        DOUBLE PRECISION Q9
!       TEMPORARY TO THROW RANDOM STUFF INTO.
        IF(FROM .EQ. 0) GO TO 6110
!       MUST BE DOING A RESTART.  FIGURE OUT WHERE, AND GO DO IT.
!       DON'T NEED A LOG FILE FOR THIS ROUTINE.
        IF (FROM .EQ. 211) GO TO 7000
        IF (FROM .EQ. 212) GO TO 6130
        CALL BADMIL
6110    continue
        WRITE(OUT,6120)
6120    FORMAT (/' What messages and/or values does',' division by zero produce?')
        WRITE(OUT,6123)
6123    FORMAT(' About to compute 1/0...')
        MILES = 211
        CALL LOGIT(MILES)
        Q9 = FP1 / FP0
        WRITE(OUT,6121) Q9
6121    FORMAT(' Trying to compute  1/0  produces ', 1PE15.7)
7000    continue
        MILES = 212
        WRITE(OUT,6124)
6124    FORMAT(' About to compute 0/0...')
        CALL LOGIT(MILES)
        Q9 = FP0 / FP0
        WRITE(OUT,6122) Q9
6122    FORMAT (' Trying to compute  0/0  produces ', 1PE15.7/)
6130    continue
        MILES = 220
        RETURN
        END SUBROUTINE ZEROS
!-------------------------------------------------------------------------------
      SUBROUTINE BADMIL
      implicit none
      WRITE(OUT,11110)
11110 FORMAT(' Unrecognized restart milestone - PLEASE NOTIFY ','KARPINSKI !')
      STOP
      END SUBROUTINE BADMIL
!-------------------------------------------------------------------------------
      INTEGER FUNCTION IDINT(X)
        implicit none
        DOUBLE PRECISION X
        REAL Y
        Y=X
        IDINT=INT(Y)
        RETURN
      END FUNCTION IDINT
!-------------------------------------------------------------------------------
end subroutine dparanoia
!-------------------------------------------------------------------------------

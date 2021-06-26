!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_bessel
use M_journal, only : journal
private
public bes
public besi
public besj
public besj0
public besj1
public besk
public besy
public besy0
public test_suite_M_bessel
integer,parameter :: dp=kind(0.0d0)
contains
!*!implicit doubleprecision (a-h, o-z)
!*!implicit real(kind=kind(0.0d0)) (a-h, o-z)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!! bes(3f) - [M_bessel] calculate Bessel functions J(X), Y(X), I(X), K(X) for doubleprecision arguments and integer orders
!!
!!##SYNOPSIS
!!
!!    subroutine bes(x,no,kode,rslt1,rslt2,t1,t2,ierr)
!!
!!##DESCRIPTION
!!    This routine calculates the Bessel functions J(X),Y(X), I(X), or K(X)
!!    for doubleprecision arguments and integer orders. Backwards recurrence
!!    techniques are used for the J(X) and I(X) functions except for very
!!    small arguments, where double precision series evaluation is used.
!!    Forward recurrence is used for the Y(X) and K(X) functions with double
!!    precision Chebyshev approximations used for function initialization.
!!    Accuracy is between thirteen and fourteen significant figures.
!!    For specific functions J0(), J1(), Y0(), Y1(), See BESJ0(), BESJ1(),
!!    BESY0(), and BESY1().
!!
!!##OPTIONS
!!
!! Description of parameters
!!
!!    X     = input,doubleprecision argument of the Bessel function.
!!            The argument may be positive,zero, or negative
!!            (neg. arg. for Y(X) or K(X) produces error
!!            message since results may be complex.)
!!            Restriction on range is
!!
!!               FOR J(X), -1100.0 .LE. X .LE. 1100.0
!!               FOR Y(X), 0.0 .LE. X .LE. 1100.0
!!               FOR I(X), -600.0 .LE. X .LE. 600.0
!!               FOR K(X), 0.0 .LE. X .LE. 600.0
!!
!!    NO    = Input,integer order of function desired for a
!!            single value to be returned, or the maximum order
!!            desired (+ or -) if an array of values is to be
!!            returned.
!!            Let XX = abs(X). Then bounds on orders are
!!
!!            1. For 0.0 .LE. XX .LE. 0.025,
!!                 The absolute value of maximum order, ANO,
!!                 and argument supplied (ABS(X)) must
!!                 satisfy the relation
!!
!!                 log(GAMMA(ANO))-ANO*log(XX/2.0)
!!                 + log(XX)/2.0  .LE. 679.0
!!
!!                 For a given argument and an order greater
!!                 than that allowed by the above relation
!!
!!                 JN(X) = 0.0, N.NE.0, AND =1.0 FOR N=0
!!                 YN(X) = -INF
!!                 IN(X) = 0.0, N.NE.0, AND =1.0 FOR N=0
!!                 KN(X) = INF
!!            2. FOR 0.025 .LT. XX .LE. 0.20,
!!                 ABS(NO) .LE. INT(140.0*XX + 83.0)
!!            3. FOR 0.20 .LT. XX .LE. 1.0,
!!                ABS(NO) .LE. INT(42.0*XX + 102.0)
!!            4. FOR 1.0 .LT. XX .LE. 20.0,
!!                 ABS(NO) .LE. INT(0.02*XX**3 - 0.86*XX**2 +
!!                                  17.15*XX + 124.0)
!!            5. FOR 20.0 .LT. XX .LE. 100.0,
!!                 ABS(NO) .LE. INT(2.75*XX + 228.0)
!!            6. FOR 100.0 .LT. XX .LE. 400.0,
!!                 ABS(NO) .LE. INT(1.67*XX + 336.0)
!!            7. FOR 400.0 .LT. XX .LE. 1100.0,
!!                 ABS(NO) .LE. INT(1.33*XX + 470.0)
!!
!!    KODE  = Input,integer indicator for the particular
!!            function to be computed.
!!
!!              KODE = 10 -- FUNCTION J(X) ONLY
!!                   = 11 --          Y(X) ONLY
!!                   = 12 --          J(X) AND Y(X)
!!
!!                   = 20 --          I(X) ONLY
!!                   = 21 --          K(X) ONLY
!!                   = 22 --          I(X) AND K(X)
!!
!!    RSLT1 = output,contains the doubleprecision function value for J(X)
!!            or I(X) corresponding to the order and argument
!!            supplied, depending on the KODE value. This
!!            parameter would contain the result if only one
!!            function value is to be returned.
!!
!!    RSLT2 = output,contains the doubleprecision function value for Y(X)
!!            or K(X) in a manner similar to RSLT1.
!!
!!    T1    = output,a work area which will contain the array oF
!!            doubleprecision function values for J(X) or I(X) of orders
!!            zero through NO, depending on KODE.
!!            T1 must be dimensioned in the calling program and
!!            must contain at least M cells of storage, where
!!
!!                M = MAX(ABS(NO),INT(2*ABS(X))) + 51
!!
!!            In using the array, T1(1) = function of order 0,
!!            --- T1(NO+1) = function of order NO.
!!
!!    T2    = Output,similar to T1 for the functions Y(X) or
!!            K(X). an exception is that if only J(X) or I(X)
!!            are called, then T2 needs no dimension in the
!!            calling program, but the parameter must still
!!            appear in the calling sequence. Otherwise, T2
!!            must be dimensioned at least M.
!!
!!    IERR  = Output,error flag for the conditions
!!
!!             -- Normal code
!!                =0, Normal - No errors
!!             -- Abnormal codes
!!                =1, Argument out of range
!!                =2, Order too large for argument supplied
!!                =3, Argument too large for I(X) and K(X)
!!                =4, Negative arguments for Y(X) or K(X)
!!                =5, Incorrect parameter KODE
!!
!!      BS is documented for the original version in SC-M-69-336
!!##AUTHORS
!!   Based on routines originally from
!!
!!       Sandia Mathematical Program Library
!!       Applied Mathematics Division 2642
!!       Sandia Laboratories
!!       P. O. Box 5800
!!       Albuquerque, New Mexico  87115
!!       Control Data 6600 Version 5.1, 10 December 1973
!!
!!       Written by Ronald D. Halbgewachs, July, 1968.
!!       Modified by RDH for increased accuracy, May 8,1973.
!!
!!##EXAMPLE
!!
SUBROUTINE BES(X,NO,KODE,RSLT1,RSLT2,T1,T2,IERR)
implicit none

character(len=*),parameter::ident_1="&
&@(#)M_bessel::bes(3f):calculate Bessel functions J(X), Y(X), I(X), K(X) for doubleprecision arguments and integer orders"

integer         :: i
integer         :: i2
integer         :: iend
integer         :: ierr
integer         :: ik
integer         :: iloop
integer         :: imo
integer         :: j
integer         :: jo
integer         :: kend
integer         :: ko
integer         :: kode
integer         :: largor
integer         :: mask1
integer         :: mask2
integer         :: mo
integer         :: n
integer         :: nk
integer         :: no
integer         :: nord
doubleprecision :: ano
doubleprecision :: ax
doubleprecision :: bigval
doubleprecision :: f
doubleprecision :: rslt1
doubleprecision :: rslt2
doubleprecision :: sign
doubleprecision :: sum
doubleprecision :: sumj1
doubleprecision :: sumj2
doubleprecision :: sumjn
doubleprecision :: t1
doubleprecision :: t2
doubleprecision :: x
doubleprecision :: xchk
doubleprecision :: xk
doubleprecision :: xn
doubleprecision :: xnkm1
doubleprecision :: xord
doubleprecision :: xsqfr
doubleprecision :: xx
doubleprecision DX,DXX,DX2,TEMP,DCHK,PI,EULER,A0,A,B,C
doubleprecision  SUMJIN,DXORD,DXK
DIMENSION  T1(*),T2(*),A(18),B(21),C(19)
DATA  PI/3.1415926535897932384626434D0/,                            &
   &      EULER/0.57721566490153286060651209D0/
DATA (A(I),I=1,18)/           0.15999999999999999996D+02,            &
   & 0.96000000000000000392D+02,  0.20859259259259257733D+03,            &
   & 0.23703703703703736105D+03,  0.16626725925925503356D+03,            &
   & 0.79290469135839064596D+02,  0.27400431054739774751D+02,            &
   & 0.71803471197186985165D+01,  0.14763245818980230758D+01,            &
   & 0.24456169711179137024D+00,  0.33342447857340252160D-01,            &
   & 0.380697152755597312D-02,    0.36933105872797696D-03,               &
   & 0.30849206583296D-04,        0.222445483065344D-05,                 &
   & 0.14811194720256D-06,        0.635655159808D-08,                    &
   & 0.68719476736D-09/
DATA (B(I),I=1,21)/           0.98813927043864915927D+00,            &
   & -.11277407316570291310D-01,  0.5340716774420596D-03,                &
   & -.435456758226194D-04,       0.488456084594416D-05,                 &
   & -.68181429589264D-06,        0.11199290865952D-06,                  &
   & -.2089895303616D-07,         0.4325898624D-08,                      &
   & -.97628537856D-09,           0.23715879424D-09,                     &
   & -.6140542976D-10,            0.1680852992D-10,                      &
   & -.481501184D-11,             0.144621568D-11,                       &
   & -.47808512D-12,              0.1572864D-12,                         &
   & -.31457280D-13,              0.9175040D-14,                         &
   & -.1310720D-13,               0.524288D-14/
DATA (C(I),I=1,19)/          -0.73804295108687506715D-01,            &
   & 0.11366785079620443739D+02, -0.65838973034256501712D+02,            &
   & 0.14119145750221817396D+03, -0.15929975325701922684D+03,            &
   & 0.11122328958866232246D+03, -0.52866443153661476803D+02,            &
   & 0.18223597971689250243D+02, -0.47661469297599122637D+01,            &
   & 0.97840283604837466112D+00, -0.16191400580768858112D+00,            &
   & 0.2212712874183229440D-01,  -0.2606907391286968320D-02,             &
   & 0.316831265267384320D-03,   -0.6102072906743808D-04,                &
   & 0.1658373309202432D-04,     -0.3439710458347520D-05,                &
   & 0.338099825541120D-06,      -0.343597383680D-09/
   IERR=0
!      bigval=1.0d322
   bigval=1.0d307
   IF (KODE.LT.10) GO TO 600
   SIGN = 1.0d0
   KO = IABS(NO) + 1
   XSQFR = 0.25d0*X*X
!     ---------
!     INITIAL CHECK OF ORDER-ARGUMENT RANGE TO DETERMINE IF ORDER IS OUT
!     OF RANGE FOR THE GIVEN ARGUMENT.
!
!     ---------
   IF (X) 3,27,3
3  XCHK = ABS(X)
   IF (XCHK-1100.0d0) 4,18,608
4  IF (NO.EQ.0) GO TO 25
   IF (XCHK - 0.025d0) 5,5,6
5  ANO = IABS(NO)
   AX = ANO*log(2.0d0*ANO/XCHK) + 0.5d0*log(2.0d0*dble(PI)/ANO) - ANO + 1.0d0/(12.0d0*ANO) + log(XCHK)/2.0d0
   RSLT1 = AX
   IF (AX-679.0d0) 25,25,606
6  IF (XCHK-0.20d0) 7,7,8
7  LARGOR =  int(140.0d0*XCHK + 83.0d0)
   GO TO 24
8  IF (XCHK-1.0d0) 9,9,10
9  LARGOR =  int(42.0d0*XCHK + 102.0d0)
   GO TO 24
10 IF (XCHK-20.0d0) 11,11,12
11 LARGOR =  int(((0.02d0*XCHK-0.86d0)*XCHK+17.15d0)*XCHK+124.0d0)
   GO TO 24
12 IF (XCHK-100.0d0) 13,13,14
13 LARGOR =  int(2.75d0*XCHK + 228.0d0)
   GO TO 24
14 IF (XCHK-400.0d0) 16,16,18
16 LARGOR =  int(1.67d0*XCHK + 336.0d0)
   GO TO 24
18 LARGOR =  int(1.33d0*XCHK + 470.0d0)
24 IF (IABS(NO)-LARGOR) 25,25,606
25 XX=X
!     ---------
!     DETERMINE WHICH SET OF FUNCTIONS IS TO BE CALCULATED.
!     ---------
27 MASK1 = KODE/10
   IF (MASK1-2) 30,31,600
30 MASK2 = KODE-10
   GO TO 32
31 MASK2 = KODE-20
32 IF (MASK2-2) 34,36,600
34 IF (MASK2) 600,37,42
!     ---------
!     CHECK FUNCTIONS J(X) AND I(X) FOR ZERO ARGUMENT.
!     ---------
36 IF (X) 604,38,59
37 IF (X) 58,38,59
38 IF (NO) 54,40,54
40 T1(1) = 1.0d0
   RSLT1 = 1.0d0
   IF (MASK2.EQ.0) RETURN
!     ---------
!     CHECK FUNCTIONS Y(X) AND K(X) FOR ZERO ARGUMENT.
!     The functions y0, y1, and yn have logarithmic  singularities
!     at the origin, so they treat zero and negative arguments the
!     way log does. Such  arguments  are unexceptional for j0, j1, and jn.
!     ---------
42 IF (X) 604,44,59
44 continue
   IF (MASK1.EQ.2) GO TO 50
   DO IK=1,KO
      T2(IK) = -bigval
   enddo
   RSLT2  = -bigval
   RETURN
50 continue
   DO IK=1,KO
      T2(IK) = bigval
   enddo
   RSLT2  = bigval
   RETURN
!     ---------
!     FILL OUT ARRAY FOR J(X) OR I(X) WHEN (NO.NE.0).
!     ---------
54 continue
   DO IK=2,KO
      T1(IK) = 0.0d0
   enddo
   RSLT1 = 0.0d0
   T1(1) = 1.0d0
   IF (MASK2.EQ.0) RETURN
   GO TO 44
58 continue
   X = ABS(X)
59 continue
   MO = IABS(NO)
   IMO = MO
   IF (X-1.0d0) 60,71,71
60 continue
   IF (MASK1.EQ.2) GO TO 175
!     ---------
!     USE SERIES TO DETERMINE J(N) AND J(N-1) WHEN ARGUMENT IS SMALL,
!     THEN USE RECURRENCE TO DETERMINE REMAINING FUNCTION VALUES.
!     ---------
   XORD = MO
   DXORD = XORD
   IF (MO.GT.1) GO TO 61
   DX2 = 1.0D0
   A0 = 1.0D0
   ILOOP = 1
   GO TO 63
61 continue
   A0 = 1.0D0
   IEND = MO-1
   DO IK=1,IEND
      XK = IK
      DXK = XK
      A0 = A0*DXK
   enddo
   DX2 = 1.0D0/(A0*DXORD)
   A0 = 1.0D0/A0
   ILOOP = 1
63 continue
   SUMJIN = DX2
   DX = X
   DXX = 0.25D0*DX*DX
   DO IK=1,200
      XK = IK
      DXK = XK
      TEMP = -DX2*DXX/(DXK*(DXORD+DXK))
      SUMJIN = SUMJIN + TEMP
      IF (SUMJIN) 64,65,64
64    DCHK = abs(TEMP/SUMJIN)
      IF (DCHK - 1.0D0-20.0d0) 67,65,65
65    DX2 = TEMP
   enddo
67 IF (ILOOP.GT.1) GO TO 68
   T1(KO) = dble(SUMJIN*(0.5D0*DX)**MO)
   IF (MO.EQ.0) GO TO 83
   ILOOP = 2
   DX2 = A0
   XORD = MO-1
   DXORD = XORD
   GO TO 63
68 T1(KO-1) = dble(SUMJIN*(0.5D0*DX)**(MO-1))
   IF (KO.LE.2) GO TO 83
   IEND = KO-2
   DO IK=1,IEND
      NK = KO-IK
      XNKM1 = NK-1
      T1(NK-1) = 2.0d0*XNKM1 *T1(NK)/X - T1(NK+1)
   enddo
   GO TO 83
71 IF (MASK2.EQ.0) GO TO 74
!     ---------
!     DETERMINE STARTING LOCATION OF RECURRENCE IF Y(X) OR K(X)
!     ARE TO BE FOUND.
!     ---------
   JO = 2* int(X)
   IF (IMO - JO) 72,73,73
72 IMO = JO
73 IMO = IMO + 51
   GO TO 78
!     ---------
!     DETERMINE STARTING LOCATION FOR RECURRENCE OF J(X).
!     ---------
74 JO = 2* int(X)
   IF (IMO - JO) 75,76,76
75 IMO = JO
76 IMO = IMO + 51
!     ---------
!     INITIALIZE VALUES FOR J(X) AND Y(X)
!     ---------
78 T1(IMO) = 0.0d0
   T1(IMO-1) = 1.0d0-200
   IF (MASK1.EQ.2) GO TO 151
   F = 2*(IMO-1)
   IMO = IMO - 3
   I2 = IMO
79 F = F - 2.0d0
!     ---------
!     RECURRENCE USED FOR FUNCTION VALUES.
!     VARIABLE SUM IS USED TO DETERMINE ADJUSTMENT FACTOR
!     ON RECURRED VALUES.
!     ---------
   T1(I2+1) = F/X*T1(I2+2) - T1(I2+3)
   IF (I2) 80,81,80
80 I2 = I2-1
   GO TO 79
81 SUM = T1(1)
   DO J=3,IMO,2
      SUM = SUM + 2.0d0*T1(J)
   enddo
   F = 1.0d0/SUM
83 IF (NO) 86,84,84
84 IF (XX) 90,32,92
86 IF (XX) 92,32,90
90 SIGN = -SIGN
92 IF (MASK2.EQ.0) GO TO 93
   GO TO 300
93 IF (X - 1.0d0) 96,94,94
94 continue
   DO J=1,KO
     T1(J) = T1(J)*F
   enddo
96 IF (MO.EQ.0) GO TO 98
   DO J=2,KO,2
      T1(J) = T1(J)*SIGN
   enddo
98 RSLT1 = T1(KO)
   X = XX
   RETURN
!     ---------
!     INITIALIZE STARTING VALUES FOR I(X) AND K(X) RECURRENCE.
!     ---------
151 IF (X-600.0d0) 152,152,602
152 F = 2*(IMO-1) - 2
   IMO = IMO - 3
   I2 = IMO
153 T1(I2+1) = F/X*T1(I2+2) + T1(I2+3)
   IF (I2) 154,155,154
154 I2 = I2-1
   F=F-2.0d0
   GO TO 153
155 SUM = T1(1)
   DO J=2,IMO
      SUM = SUM + 2.0d0*T1(J)
   enddo
   F = 1.0d0/SUM*EXP(X)
   IF (XX) 171,32,172
171 SIGN = -SIGN
172 continue
   DO J=1,KO,2
      T1(J) = T1(J)*F
      T1(J+1) = T1(J+1)*F*SIGN
   enddo
   RSLT1 = T1(KO)
   IF (MASK2.NE.0) GO TO 400
   X = XX
   RETURN
175 XORD = MO
   DXORD = XORD
   IF (MO.GT.1) GO TO 177
   DX2 = 1.0D0
   A0 = 1.0D0
   ILOOP = 1
   GO TO 180
177 A0 = 1.0D0
   IEND = MO - 1
   DO IK=1,IEND
      XK = IK
      DXK = XK
      A0 = A0*DXK
   enddo
   DX2 = 1.0D0/(A0*DXORD)
   A0 = 1.0D0/A0
   ILOOP = 1
180 SUMJIN = DX2
   DX = X
   DXX = 0.25D0*DX*DX
   DO IK=1,200
      XK = IK
      DXK = XK
      TEMP = DX2*DXX/(DXK*(DXORD+DXK))
      SUMJIN = SUMJIN + TEMP
      DCHK = abs(TEMP/SUMJIN)
      IF (DCHK - 1.0D0-20) 184,181,181
181   continue
    DX2 = TEMP
   enddo
184 continue
    IF (ILOOP.GT.1) GO TO 185
   T1(KO) = dble(SUMJIN*(0.5D0*DX)**MO)
   IF (MO.EQ.0) GO TO 188
   ILOOP = 2
   DX2 = A0
   XORD = MO-1
   DXORD = XORD
   GO TO 180
185 continue
    T1(KO-1) = dble(SUMJIN*(0.5D0*DX)**(MO-1))
   IF (KO.LE.2) GO TO 188
   IEND = KO-2
   DO IK=1,IEND
      NK = KO-IK
      XNKM1 = NK-1
      T1(NK-1) = 2.0d0*XNKM1 *T1(NK)/X + T1(NK+1)
   enddo
188 IF (XX) 189,32,190
189 SIGN = -SIGN
190 IF (MO.EQ.0) GO TO 194
   DO J=2,KO,2
      T1(J) = T1(J)*SIGN
   enddo
194 continue
    RSLT1 = T1(KO)
   IF (MASK2.NE.0) GO TO 400
   X = XX
   RETURN
!     ---------
!     EVALUATE Y0 AND Y1 TO START RECURRENCE.
!     ---------
300 IF (X-1.0d0) 3001,301,301
3001 DX = X
   DXX = DX*DX/64.0D0
   DX2 = 1.0D0
   TEMP = C(1)
   DO J=2,19
      DX2 = DX2*DXX
      A0 = C(J)*DX2
      TEMP = TEMP + A0
      IF (TEMP) 3005,3010,3005
3005  DCHK = abs(A0/TEMP)
      IF (DCHK - 1.0D0-20) 3015,3010,3010
3010 CONTINUE
   enddo
3015 A0 = (2.0D0/PI)*LOG(DX)
   T2(1) = A0*T1(1) + TEMP
   GO TO 321
301 continue
   DO J=1,IMO
      T1(J) = T1(J)*F
   enddo
   SUMJ1 = 0.0d0
   SUMJ2 = 0.0d0
   IF (IMO.LE.80) GO TO 305
   IF (KO - JO) 303,304,304
303 KEND = JO/2
   GO TO 306
304 KEND = KO/2
   GO TO 306
305 KEND = IMO/2
306 continue
    DO N=1,KEND,2
      XN = N
      SUMJ1 = SUMJ1 + T1(2*N+1)/XN
   enddo
   DO N=2,KEND,2
      XN = N
      SUMJ2 = SUMJ2 + T1(2*N+1)/XN
   enddo
   SUMJN = 2.0d0*(SUMJ2-SUMJ1)
   T2(1) = 2.0d0/PI*(T1(1)*(log(X/2.0d0) + EULER) - SUMJN)
321 continue
    IF (MO.GT.0) GO TO 309
   RSLT1 = T1(1)
   RSLT2 = T2(1)
   X = XX
   RETURN
309 continue
    T2(2) = (T1(2)*T2(1) - 2.0d0/(PI*X))/T1(1)
   IF (MO.EQ.1) GO TO 311
   NORD = KO-1
   DO N=2,NORD
      XN = N-1
      T2(N+1) = (2.0d0*XN)/X*T2(N) - T2(N-1)
    enddo
311 continue
    DO J=2,KO,2
      T2(J) = T2(J)*SIGN
   enddo
   RSLT2 = T2(KO)
   IF (MASK2.EQ.1) GO TO 315
   DO J=2,KO,2
      T1(J) = T1(J)*SIGN
   enddo
   RSLT1 = T1(KO)
315 X = XX
   RETURN
400 DX = X
   DX2 = 1.0D0
   IF (X-6.0d0)  410,410,440
410 DXX = DX*DX/64.0D0
   TEMP = 0.0D0
   DO J=1,18
      DX2 = DX2*DXX
      A0 = A(J)*DX2
      TEMP = TEMP + A0
      DCHK = A0/TEMP
      IF (DCHK - 1.0D0-20) 430,420,420
420 CONTINUE
    enddo
430 A0 = -(EULER + LOG(0.5D0*DX))
   T2(1) = A0*T1(1) + TEMP
   IF (NO.EQ.0) GO TO 490
   GO TO 480
440 DXX = 10.0D0/DX - 1.0D0
   TEMP = B(1)
   DO J=2,21
      DX2 = DX2*DXX
      A0 = B(J)*DX2
      TEMP = TEMP + A0
      IF (TEMP) 450,460,450
450   DCHK = abs(A0/TEMP)
      IF (DCHK - 1.0D0-20) 470,460,460
460 CONTINUE
   enddo
470 T2(1) = SQRT(PI/(2.0d0*DX))*exp(-DX)*TEMP
   IF (NO.EQ.0) GO TO 490
480 T2(2) = (1.0d0/X - T2(1)*T1(2))/T1(1)
   IF (KO.LE.2) GO TO 490
   NORD = KO-1
   DO N=2,NORD
      XN = N-1
      T2(N+1) = (2.0d0*XN)/X *T2(N) + T2(N-1)
   enddo
490 continue
   RSLT2 = T2(KO)
   X = XX
   RETURN
600 continue
   IERR=5
   RETURN
602 continue
   X=XX
   IERR=3
   RETURN
604 continue
   IERR=4
   RETURN
606 continue
   IERR=2
   RETURN
608 continue
   IERR=1
   RETURN
END SUBROUTINE BES
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    besi(3f) - [M_bessel] compute the I Bessel function for a given argument and order
!!
!!##SYNOPSIS
!!
!!    subroutine besi(X,N,BI,IER)
!!
!!##DESCRIPTION
!!    Computes I Bessel function using series or asymptotic
!!    approximation depending on range of arguments.
!!
!!    REMARKS
!!
!!       N and X must be .GE. zero
!!
!!       Checks for errors IN N and X and exits if any are present
!!
!!    SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!!
!!         none
!!
!!
!!##OPTIONS
!!
!!    Description of parameters
!!
!!         X    The argument of the I Bessel function desired
!!         N    The order of the I Bessel function desired
!!         BI   The resultant I Bessel function
!!         IER  Resultant error code where
!!
!!               IER=0 No error
!!               IER=1 N is negative
!!               IER=2 X is negative
!!               IER=3 underflow, BI .LT. 1.E-69, BI set to 0.0
!!               IER=4 overflow, X .GT. 170 where X .GT. N
!!##EXAMPLE
!!
SUBROUTINE BESI(X,N, BI,IER)
implicit none

character(len=*),parameter::ident_2="@(#)M_bessel::besi(3f):compute the I Bessel function for a given argument and order"

integer         :: n
doubleprecision :: x
doubleprecision :: bi
integer         :: ier

integer         :: i
integer         :: k
doubleprecision :: fi
doubleprecision :: fk
doubleprecision :: fn
doubleprecision :: pi
doubleprecision :: term
doubleprecision :: tol
doubleprecision :: xx
   IER=0
   BI=1.0d0
!  Checks for errors IN N and X and exits if any are present
   IF(N)150,15,10
10 IF(X)160,20,20
15 IF(X)160,17,20
17 continue
   RETURN
!
!     DEFINE TOLERANCE
!
20 continue
   TOL=1.d-6
!
!     IF ARGUMENT GT 12 AND GT N, USE ASYMPTOTIC FORM
!
   IF(X-12.0d0)40,40,30
30 continue
   IF(X-dble(N))40,40,110
!
!     COMPUTE FIRST TERM OF SERIES AND SET INITIAL VALUE OF THE SUM
!
40 continue
   XX=X/2.0d0
   TERM=1.00d0
   IF(N) 70,70,55
55 continue
   DO I=1,N
      FI=I
      IF(ABS(TERM)-1.0d-68)56,60,60
56    continue
      IER=3
      BI=0.0d0
      RETURN
60    continue
      TERM=TERM*XX/FI
   enddo
70 continue
   BI=TERM
   XX=XX*XX
!
!     COMPUTE TERMS, STOPPING WHEN ABS(TERM) LE ABS(SUM OF TERMS)
!     TIMES TOLERANCE
!
   DO K=1,1000
      IF(ABS(TERM)-ABS(BI*TOL))100,100,80
80    continue
      FK=K*(N+K)
      TERM=TERM*(XX/FK)
      BI=BI+TERM
   enddo
!
!     RETURN BI AS ANSWER
!
100 RETURN
!
!     X GT 12 AND X GT N, SO USE ASYMPTOTIC APPROXIMATION
!
110 continue
   FN=4*N*N
   IF(X-170.0d0)115,111,111
111 continue
   IER=4
   RETURN
115 XX=1.0d0/(8.0d0*X)
   TERM=1.0d0
   BI=1.0d0
   DO K=1,30
      IF(ABS(TERM)-ABS(TOL*BI))140,140,120
120   continue
      FK=(2*K-1)**2
      TERM=TERM*XX*(FK-FN)/dble(K)
      BI=BI+TERM
   enddo
!
!     SIGNIFICANCE LOST AFTER 30 TERMS, TRY SERIES
!
   GO TO 40
140 PI=3.141592653d0
   BI=BI*EXP(X)/SQRT(2.0d0*PI*X)
   GO TO 100
150 IER=1
   GO TO 100
160 IER=2
   GO TO 100
END SUBROUTINE BESI
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    besj(3f) - [M_bessel] compute the J Bessel function for a given argument and order
!!
!!##SYNOPSIS
!!
!!    subroutine besj(x,n,bj,d,ier)
!!
!!##DESCRIPTION
!!
!!         REMARKS
!!            N must be greater than or equal to zero, but it must be
!!            less than
!!               20+10*X-X** 2/3   For X less than or equal to 15
!!               90+X/2           For X greater than 15
!!
!!         SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!!            none
!!
!!         METHOD
!!            Recurrence relation technique described by H. Goldstein and
!!            R.M. Thaler,"Recurrence Techniques for the Calculation of
!!            Bessel Functions",M.T.A.C.,V.13,PP.102-108 and I.A. Stegun
!!            and M. Abramowitz,"Generation of Bessel Functions on High
!!            Speed Computers",M.T.A.C.,V.11,1957,PP.255-257
!!
!!##OPTIONS
!!
!!         Description of Parameters
!!
!!            X    The argument of the J Bessel function desired
!!            N    The order of the J Bessel function desired
!!            BJ   The resultant J Bessel function
!!            D    Required accuracy
!!            IER  Resultant error code where
!!
!!                  IER=0  No error
!!                  IER=1  N is negative
!!                  IER=2  X is negative or zero
!!                  IER=3  Required accuracy not obtained
!!                  IER=4  Range of N compared to X not correct (see remarks)
!!##EXAMPLE
!!
SUBROUTINE BESJ(X,N,BJ,D,IER)
implicit none

character(len=*),parameter::ident_3="@(#)M_bessel::besj(3f):compute the J Bessel function for a given argument and order"

integer         :: ier
integer         :: jt
integer         :: k
integer         :: m
integer         :: m2
integer         :: ma
integer         :: mb
integer         :: mk
integer         :: mmax
integer         :: mzero
integer         :: n
integer         :: n1
integer         :: ntest
doubleprecision :: alpha
doubleprecision :: bj
doubleprecision :: bmk
doubleprecision :: bprev
doubleprecision :: d
doubleprecision :: fm
doubleprecision :: fm1
doubleprecision :: s
doubleprecision :: x
   BJ=0.0d0
   IF(N)10,20,20
10 IER=1
   RETURN
20 IF(X)30,30,31
30 IER=2
   RETURN
31 IF(X-15.0d0)32,32,34
32 NTEST=20.0d0+10.0d0*X-X**2/3
   GO TO 36
34 NTEST=90.0d0+X/2.0d0
36 IF(N-NTEST)40,38,38
38 IER=4
   RETURN
40 IER=0
   N1=N+1
   BPREV=0.0d0
!
!     COMPUTE STARTING VALUE OF M
!
   IF(X-5.0d0)50,60,60
50 MA=X+6.0d0
   GO TO 70
60 MA=1.4d0*X+60.0d0/X
70 MB=N+int(X)/4+2
   MZERO=MAX0(MA,MB)
!
!     SET UPPER LIMIT OF M
!
   MMAX=NTEST
   DO M=MZERO,MMAX,3
!
!     SET F(M),F(M-1)
!
      FM1=1.0d0-28.0d0
      FM=0.0d0
      ALPHA=0.0d0
      IF(M-(M/2)*2)120,110,120
110   JT=-1
      GO TO 130
120   JT=1
130   M2=M-2
      DO K=1,M2
         MK=M-K
         BMK=2.0d0*dble(MK)*FM1/X-FM
         FM=FM1
         FM1=BMK
         IF(MK-N-1)150,140,150
140      BJ=BMK
150      JT=-JT
         S=1.0d0+JT
         ALPHA=ALPHA+BMK*S
      enddo
      BMK=2.0d0*FM1/X-FM
      IF(N)180,170,180
170   BJ=BMK
180   ALPHA=ALPHA+BMK
      BJ=BJ/ALPHA
      IF(ABS(BJ-BPREV)-ABS(D*BJ))200,200,190
190 continue
    BPREV=BJ
    enddo
   IER=3
200 RETURN
END SUBROUTINE BESJ
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    besj0(3f) - [M_bessel] calculates the Bessel function J(X) of order zero.
!!
!!##SYNOPSIS
!!
!!    function besj0(xx)
!!
!!##DESCRIPTION
!!    Series evaluation is used for small arguments, recurrence techniques
!!    are used for midrange, and Hankel-S Asymptotic Expansion is used
!!    for large arguments. Accuracy is between thirteen and fourteen
!!    correct significant figures except for large arguments where the
!!    accuracy eventually falls to eight figures.
!!
!!##OPTIONS
!!
!!      XX may be any doubleprecision argument.
!!##AUTHORS
!!  Originally from
!!
!!    Sandia Mathematical Program Library
!!    Applied Mathematics Division 2642
!!    Sandia Laboratories
!!    P. O. box 5800
!!    Albuquerque, new Mexico  87115
!!    Control Data 6600 Version 5.1, 10 December 1973
!!
!!        Written by    Ronald D. Halbgewachs, October 1,1971.
!!
!!    IMPLEMENTATION
!!    minor changes have been made to improve the portability or to
!!    confirm to modern Fortran standards
!!
!!##EXAMPLE
!!
FUNCTION BESJ0(XX)
implicit none

character(len=*),parameter::ident_4="@(#)M_bessel::besj0(3f):calculates the Bessel function J(X) of order zero."

doubleprecision :: besj0
doubleprecision :: chi
doubleprecision :: eightx
doubleprecision :: exfour
doubleprecision :: exsix
doubleprecision :: exsq
doubleprecision :: factor
integer         :: ierr
doubleprecision :: p
doubleprecision :: p1
doubleprecision :: p2
doubleprecision :: p3
doubleprecision :: piov4
doubleprecision :: q
doubleprecision :: q1
doubleprecision :: q2
doubleprecision :: q3
doubleprecision :: r2
doubleprecision :: t1
doubleprecision :: t2
doubleprecision :: twovpi
doubleprecision :: x
doubleprecision :: xx
DIMENSION T1(101)
DIMENSION T2(101) ! for bug in Intel 11.1.046 compiler Sun Aug 23 15:27:46 EDT 2009
DATA TWOVPI/0.63661977236758d0/,PIOV4/0.78539816339745d0/
DATA P1/4.5d0/,P2/4.59375d02/,P3/1.500778125d05/
DATA Q1/37.5d0/,Q2/7.441875d03/,Q3/3.623307187d06/
   X = ABS(XX)
   IF (X - 25.0d0) 20,20,40
20 CALL BES(XX,0,10,BESJ0,R2,T1,T2,IERR)
   RETURN
40 CHI = X - PIOV4
   FACTOR = SQRT(TWOVPI/X)
   EIGHTX = 0.125d0/X
   EXSQ = EIGHTX*EIGHTX
   EXFOUR = EXSQ*EXSQ
   EXSIX = EXFOUR*EXSQ
   P = 1.0d0 - P1*EXSQ + P2*EXFOUR - P3*EXSIX
   Q = EIGHTX*(-1.0d0 + Q1*EXSQ - Q2*EXFOUR + Q3*EXSIX)
   BESJ0 = FACTOR*(P*COS(CHI) - Q*SIN(CHI))
   RETURN
END FUNCTION BESJ0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    besj1(3f) - [M_bessel] calculates the Bessel function J(X) of order one.
!!
!!##SYNOPSIS
!!
!!    function besj1(xx)
!!
!!##DESCRIPTION
!!     Series evaluation is used for small arguments, recurrence
!!     techniques are used for midrange, and Hankel-S asymptotic
!!     expansion is used for large arguments.
!!     Accuracy is between thirteen and fourteen correct significant
!!     figures except for large arguments where the accuracy
!!     eventually falls to eight figures.
!!
!!##OPTIONS
!!
!!      X may be any doubleprecision argument
!!
!!##AUTHORS
!!
!!  Origin
!!
!!      Sandia Mathematical Program Library
!!      Applied Mathematics Division 2642
!!      Sandia Laboratories
!!      P. O. Box 5800
!!      Albuquerque, New Mexico  87115
!!      Control Data 6600 Version 5.1, 10 December 1973
!!
!!          Written by    Ronald D. Halbgewachs, October 1,1971.
!!
!!     IMPLEMENTATION
!!        Changes were made for portability and to conform to
!!        newer Fortran standards.
!!##EXAMPLE
!!
FUNCTION BESJ1(XX)
implicit none

character(len=*),parameter::ident_5="@(#)M_bessel::besj1(3f): calculates the Bessel function J(X) of order one."

integer         :: ierr
doubleprecision :: besj1
doubleprecision :: chi
doubleprecision :: eightx
doubleprecision :: exfour
doubleprecision :: exsix
doubleprecision :: exsq
doubleprecision :: factor
doubleprecision :: p
doubleprecision :: p1
doubleprecision :: p2
doubleprecision :: p3
doubleprecision :: pi
doubleprecision :: q
doubleprecision :: q1
doubleprecision :: q2
doubleprecision :: q3
doubleprecision :: r2
doubleprecision :: t1
doubleprecision :: t2
doubleprecision :: twovpi
doubleprecision :: x
doubleprecision :: xx
DIMENSION T1(101)
DIMENSION T2(101) ! for Intel compiler bug 11.1.046 Sun Aug 23 2009
DATA  PI/3.1415926535898d0/,TWOVPI/0.63661977236758d0/
DATA P1/7.5d0/,P2/5.90625d02/,P3/1.773646875d05/
DATA Q1/5.25d01/,Q2/9.095625d03/,Q3/4.180739062d06/
   X = ABS(XX)
   IF (X - 25.0d0) 20,20,40
20 CALL BES(XX,1,10,BESJ1,R2,T1,T2,IERR)
   RETURN
40 CHI = X - 0.75d0*PI
   FACTOR = SQRT(TWOVPI/X)
   EIGHTX = 0.125d0/X
   EXSQ = EIGHTX*EIGHTX
   EXFOUR = EXSQ*EXSQ
   EXSIX = EXFOUR*EXSQ
   P = 1.0d0 + P1*EXSQ - P2*EXFOUR + P3*EXSIX
   Q = EIGHTX*(3.0d0 - Q1*EXSQ + Q2*EXFOUR - Q3*EXSIX)
   BESJ1 = FACTOR*(P*COS(CHI) - Q*SIN(CHI))
   IF (XX) 60,80,80
60 BESJ1 = -BESJ1
80 RETURN
END FUNCTION BESJ1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    besk(3f) - [M_bessel] compute the K Bessel function for a given argument and order
!!
!!##SYNOPSIS
!!
!!    subroutine besk(x,n,bk,ier)
!!
!!##DESCRIPTION
!!
!!      METHOD
!!         Computes zero order and first order Bessel functions using
!!         series approximations and then computes Nth order function
!!         using recurrence relation.
!!         Recurrence relation and polynomial approximation technique
!!         as described by A.J.M.Hitchcock,"Polynomial Approximations
!!         to Bessel Functions of Order Zero and One and to Related
!!         Functions", M.T.A.C., V.11,1957,PP.86-88, and G.N. Watson,
!!         "A Treatise on the Theory of Bessel Functions", Cambridge
!!         University Press, 1958, P. 62
!!##OPTIONS
!!      DESCRIPTION OF PARAMETERS
!!         X    The argument of the K Bessel function desired
!!         N    The order of the K Bessel function desired
!!         BK   The resultant K Bessel function
!!         IER  resultant error code where
!!
!!                 IER=0  No error
!!                 IER=1  N is negative
!!                 IER=2  X is zero or negative
!!                 IER=3  X .GT. 170, Machine range exceeded
!!                 IER=4  BK .GT. 10**70
!!
!!      REMARKS
!!         N must be greater than or equal to zero
!!
!!##SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!!         none
!!##EXAMPLE
!!
SUBROUTINE BESK(X,N,BK,IER)
implicit none

character(len=*),parameter::ident_6="@(#)M_bessel::besk(3f):compute the K Bessel function for a given argument and order"

integer :: ier
integer :: j
integer :: l
integer :: n
doubleprecision :: a
doubleprecision :: b
doubleprecision :: bk
doubleprecision :: c
doubleprecision :: fact
doubleprecision :: g0
doubleprecision :: g1
doubleprecision :: gj
doubleprecision :: hj
doubleprecision :: rj
doubleprecision :: t(12)
doubleprecision :: x
doubleprecision :: x2j
   BK=0.0d0
   IF(N)10,11,11
10 IER=1
   RETURN
11 IF(X)12,12,20
12 IER=2
   RETURN
20 IF(X-170.00d0)22,22,21
21 IER=3
   RETURN
22 IER=0
   IF(X-1.0d0)36,36,25
25 A=EXP(-X)
   B=1.0d0/X
   C=SQRT(B)
   T(1)=B
   DO L=2,12
      T(L)=T(L-1)*B
   enddo
   IF(N-1)27,29,27
!
!     COMPUTE KO USING POLYNOMIAL APPROXIMATION
!
27 G0=A*(1.25331414d0-.15666418d0*T(1)+.088111278d0*T(2)-.091390954d0*T(3)    &
   &+.13445962d0*T(4)-.22998503d0*T(5)+.37924097d0*T(6)-.52472773d0*T(7)       &
   &+.55753684d0*T(8)-.42626329d0*T(9)+.21845181d0*T(10)-.066809767d0*T(11)    &
   &+.009189383d0*T(12))*C
   IF(N)20,28,29
28 BK=G0
   RETURN
!
!     COMPUTE K1 USING POLYNOMIAL APPROXIMATION
!
29 G1=A*(1.2533141d0+.46999270d0*T(1)-.14685830d0*T(2)+.12804266d0*T(3)       &
   &-.17364316d0*T(4)+.28476181d0*T(5)-.45943421d0*T(6)+.62833807d0*T(7)       &
   &-.66322954d0*T(8)+.50502386d0*T(9)-.25813038d0*T(10)+.078800012d0*T(11)    &
   &-.010824177d0*T(12))*C
   IF(N-1)20,30,31
30 BK=G1
   RETURN
!
!     FROM KO,K1 COMPUTE KN USING RECURRENCE RELATION
!
31 continue
   DO J=2,N
      GJ=2.d0*(dble(J)-1.d0)*G1/X+G0
      IF(GJ-1.0d70)33,33,32
32    IER=4
      GO TO 34
33    G0=G1
      G1=GJ
   enddo
34 BK=GJ
   RETURN
36 B=X/2.d0
   A=.57721566d0+log(B)
   C=B*B
   IF(N-1)37,43,37
!
!     COMPUTE KO USING SERIES EXPANSION
!
37 G0=-A
   X2J=1.0d0
   FACT=1.0d0
   HJ=0.0d0
   DO J=1,6
      RJ=1.0d0/dble(J)
      X2J=X2J*C
      FACT=FACT*RJ*RJ
      HJ=HJ+RJ
      G0=G0+X2J*FACT*(HJ-A)
   enddo
   IF(N)43,42,43
42 BK=G0
   RETURN
!
!     COMPUTE K1 USING SERIES EXPANSION
!
43 X2J=B
   FACT=1.0d0
   HJ=1.0d0
   G1=1.0d0/X+X2J*(0.5d0+A-HJ)
   DO J=2,8
      X2J=X2J*C
      RJ=1.0d0/dble(J)
      FACT=FACT*RJ*RJ
      HJ=HJ+RJ
      G1=G1+X2J*FACT*(0.50d0+(A-HJ)*dble(J))
   enddo
   IF(N-1)31,52,31
52 continue
   BK=G1
   RETURN
END SUBROUTINE BESK
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    besy(3f) - [M_bessel] compute the Y Bessel function for a given argument and order
!!
!!##SYNOPSIS
!!
!!    subroutine besy(x,n,by,ier)
!!
!!##DESCRIPTION
!!    METHOD
!!       Recurrence relation and polynomial approximation technique
!!       as described by A.J.M.Hitchcock,"Polynomial Approximations
!!       to Bessel Functions of Order Zero and One and to Related
!!       Functions", M.T.A.C., V.11,1957,PP.86-88, and G.N. Watson,
!!       "A Treatise on the Theory of Bessel Functions", Cambridge
!!       University Press, 1958, P. 62
!!##OPTIONS
!!   Description of Parameters
!!
!!      X    the argument of the Y Bessel function desired
!!      N    the order of the Y Bessel function desired
!!      BY   the resultant Y Bessel function
!!      IER  resultant error code where
!!
!!            IER=0  no error
!!            IER=1  N is negative
!!            IER=2  X is negative or zero
!!            IER=3  BY has exceeded magnitude of 10**70
!!
!!   REMARKS
!!      Very small values of X may cause the range of the library
!!      function LOG() to be exceeded
!!
!!         X must be greater than zero
!!         N must be greater than or equal to zero
!!
!!##SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!!      none
!!
!!##EXAMPLE
!!
SUBROUTINE BESY(X,N,BY,IER)
implicit none

character(len=*),parameter::ident_7="@(#)M_bessel::besy(3f):compute the Y Bessel function for a given argument and order"

integer         :: ier
integer         :: k
integer         :: l
integer         :: n
doubleprecision :: a
doubleprecision :: b
doubleprecision :: by
doubleprecision :: fl
doubleprecision :: fl1
doubleprecision :: p0
doubleprecision :: p1
doubleprecision :: pi
doubleprecision :: pi2
doubleprecision :: q0
doubleprecision :: q1
doubleprecision :: sum
doubleprecision :: t
doubleprecision :: term
doubleprecision :: ts
doubleprecision :: x
doubleprecision :: x2
doubleprecision :: xx
doubleprecision :: y0
doubleprecision :: y1
doubleprecision :: ya
doubleprecision :: yb
doubleprecision :: yc
!     CHECK FOR ERRORS IN N AND X
!
   IF(N)180,10,10
10 IER=0
   IF(X)190,190,20
20 PI=3.141592653d0
!
!     BRANCH IF X LESS THAN OR EQUAL 4
!
   IF(X-4.0d0)40,40,30
!
!       COMPUTE Y0 AND Y1 FOR X GREATER THAN 4
!
30 T=4.0d0/X
   P0=0.3989422793d0
   Q0=-0.0124669441d0
   P1=0.3989422819d0
   Q1=0.0374008364d0
   A=T*T
   B=A
   P0=P0-0.0017530620d0*A
   Q0=Q0+0.0004564324d0*A
   P1=P1+0.0029218256d0*A
   Q1=Q1-0.00063904d0*A
   A=A*A
   P0=P0+.00017343d0*A
   Q0=Q0-.0000869791d0*A
   P1=P1-.000223203d0*A
   Q1=Q1+.0001064741d0*A
   A=A*B
   P0=P0-.0000487613d0*A
   Q0=Q0+.0000342468d0*A
   P1=P1+.0000580759d0*A
   Q1=Q1-.0000398708d0*A
   A=A*B
   P0=P0+.0000173565d0*A
   Q0=Q0-.0000142078d0*A
   P1=P1-.000020092d0*A
   Q1=Q1+.00001622d0*A
   A=A*B
   P0=P0-.0000037043d0*A
   Q0=Q0+.0000032312d0*A
   P1=P1+.0000042414d0*A
   Q1=Q1-.0000036594d0*A
   A=SQRT(2.0d0*PI)
   B=4.0d0*A
   P0=A*P0
   Q0=B*Q0/X
   P1=A*P1
   Q1=B*Q1/X
   A=X-PI/4.0d0
   B=SQRT(2.0d0/(PI*X))
   Y0=B*(P0*SIN(A)+Q0*COS(A))
   Y1=B*(-P1*COS(A)+Q1*SIN(A))
   GO TO 90
!
!       COMPUTE Y0 AND Y1 FOR X LESS THAN OR EQUAL TO 4
!
40 XX=X/2.0d0
   X2=XX*XX
   T=log(XX)+.5772156649d0
   SUM=0.0d0
   TERM=T
   Y0=T
   DO L=1,15
      IF(L-1)50,60,50
50    SUM=SUM+1.0d0/dble(L-1)
60    FL=L
      TS=T-SUM
      TERM=(TERM*(-X2)/FL**2)*(1.0d0-1.0d0/(FL*TS))
      Y0=Y0+TERM
   enddo
   TERM = XX*(T-0.5d0)
   SUM=0.0d0
   Y1=TERM
   DO L=2,16
      SUM=SUM+1.0d0/dble(L-1)
      FL=L
      FL1=FL-1.0d0
      TS=T-SUM
      TERM=(TERM*(-X2)/(FL1*FL))*((TS-0.5d0/FL)/(TS+0.5d0/FL1))
      Y1=Y1+TERM
   enddo
   PI2=2.0d0/PI
   Y0=PI2*Y0
   Y1=-PI2/X+PI2*Y1
!
!     CHECK IF ONLY Y0 OR Y1 IS DESIRED
!
90 continue
   IF(N-1)100,100,130
!
!     RETURN EITHER Y0 OR Y1 AS REQUIRED
!
100 IF(N)110,120,110
110 BY=Y1
   GO TO 170
120 BY=Y0
   GO TO 170
!
!    PERFORM RECURRENCE OPERATIONS TO FIND YN(X)
!
130 YA=Y0
   YB=Y1
   K=1
140 T=dble(2*K)/X
   YC=T*YB-YA
   IF(ABS(YC)-1.0d70)145,145,141
141 IER=3
   RETURN
145 K=K+1
   IF(K-N)150,160,150
150 YA=YB
   YB=YC
   GO TO 140
160 BY=YC
170 RETURN
180 IER=1
   RETURN
190 IER=2
   RETURN
END SUBROUTINE BESY
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    besy0(3f) - [M_bessel] calculates the Bessel function Y(X) of order zero.
!!
!!##SYNOPSIS
!!
!!    function besy0(x)
!!
!!##DESCRIPTION
!!    Series evaluation is used for small arguments, recurrence
!!    techniques are used for midrange, and Hankel-S asymptotic
!!    expansion is used for large arguments.
!!    Accuracy is between thirteen and fourteen correct significant
!!    figures except for large arguments where the accuracy
!!    eventually falls to eight figures.
!!##AUTHORS
!!
!!  Origin
!!
!!    Sandia Mathematical Program Library
!!    Applied Mathematics Division 2642
!!    Sandia Laboratories
!!    P. O. Box 5800
!!    Albuquerque, New Mexico  87115
!!    Control Data 6600 Version 5.1, 10 December 1973
!!
!!       Written by    Ronald D. Halbgewachs, October 1,1971.
!!
!!     IMPLEMENTATION
!!        Changes were made for portability and to conform to
!!        newer Fortran standards.
!!##OPTIONS
!!    X may be any nonnegative doubleprecision argument.
!!
!!##EXAMPLE
!!
FUNCTION BESY0(X)
implicit none

character(len=*),parameter::ident_8="@(#)M_bessel::besy0(3f): calculates the Bessel function Y(X) of order zero."

!-----------------------------------------------------------------------------------------------------------------------------------
integer           :: ierr
real(kind=dp)     :: besy0
real(kind=dp)     :: chi
real(kind=dp)     :: eightx
real(kind=dp)     :: exfour
real(kind=dp)     :: exsix
real(kind=dp)     :: exsq
real(kind=dp)     :: factor
real(kind=dp)     :: p
real(kind=dp)     :: q
real(kind=dp)     :: r1
real(kind=dp)     :: t1(101)
real(kind=dp)     :: t2(101)
real(kind=dp)     :: x
!-----------------------------------------------------------------------------------------------------------------------------------
!  DATA  TWOVPI/0.63661977236758/, PIOV4/0.78539816339745/
!  DATA  P1/4.5/, P2/4.59375E02/, P3/1.500778125E05/
!  DATA  Q1/37.5/, Q2/7.441875E03/, Q3/3.623307187E06/
real(kind=dp),parameter :: TWOVPI=  0.63661977236758d0
real(kind=dp),parameter :: PIOV4 =  0.78539816339745d0
real(kind=dp),parameter :: P1    =  4.5d0
real(kind=dp),parameter :: P2    =  4.59375d02
real(kind=dp),parameter :: P3    =  1.500778125d05
real(kind=dp),parameter :: Q1    = 37.5d0
real(kind=dp),parameter :: Q2    =  7.441875d03
real(kind=dp),parameter :: Q3    =  3.623307187d06
!-----------------------------------------------------------------------------------------------------------------------------------
   if(x.lt.0.0d0)then
      besy0=-10.0d0-32.0d0   ! bad value assigned so compiler does not complain unassigned
      call journal('*besy0* ERROR: negative input value')
   elseif ((x - 25.0_dp) .lt. 0_dp)then
      CALL BES(X,0,11,R1,BESY0,T1,T2,IERR)
   else
      CHI    = X - PIOV4
      FACTOR = SQRT( TWOVPI / X )
      EIGHTX = 0.125_dp / X
      EXSQ   = EIGHTX * EIGHTX
      EXFOUR = EXSQ * EXSQ
      EXSIX  = EXFOUR * EXSQ
      P      = 1.0_dp - P1*EXSQ + P2*EXFOUR - P3*EXSIX
      Q      = EIGHTX * ( -1.0_dp + Q1*EXSQ - Q2*EXFOUR + Q3*EXSIX )
      BESY0  = FACTOR * ( P*SIN(CHI) + Q*COS(CHI) )
   endif
END FUNCTION BESY0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_bessel()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
implicit none
!*! setup
   call test_bes()
   call test_besi()
   call test_besj()
   call test_besj0()
   call test_besj1()
   call test_besk()
   call test_besy()
   call test_besy0()
!*! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bes()

   call unit_check_start('bes',msg='')
   !*!call unit_check('bes', 0.eq.0, 'checking',100)
   call unit_check_done('bes',msg='')
end subroutine test_bes
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besi()

   call unit_check_start('besi',msg='')
   !*!call unit_check('besi', 0.eq.0, 'checking',100)
   call unit_check_done('besi',msg='')
end subroutine test_besi
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj()

   call unit_check_start('besj',msg='')
   !*!call unit_check('besj', 0.eq.0, 'checking',100)
   call unit_check_done('besj',msg='')
end subroutine test_besj
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj0()

   call unit_check_start('besj0',msg='')
   !*!call unit_check('besj0', 0.eq.0, 'checking',100)
   call unit_check_done('besj0',msg='')
end subroutine test_besj0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besj1()

   call unit_check_start('besj1',msg='')
   !*!call unit_check('besj1', 0.eq.0, 'checking',100)
   call unit_check_done('besj1',msg='')
end subroutine test_besj1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besk()

   call unit_check_start('besk',msg='')
   !*!call unit_check('besk', 0.eq.0, 'checking',100)
   call unit_check_done('besk',msg='')
end subroutine test_besk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besy()

   call unit_check_start('besy',msg='')
   !*!call unit_check('besy', 0.eq.0, 'checking',100)
   call unit_check_done('besy',msg='')
end subroutine test_besy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_besy0()

   call unit_check_start('besy0',msg='')
   !*!call unit_check('besy0', 0.eq.0, 'checking',100)
   call unit_check_done('besy0',msg='')
end subroutine test_besy0
!===================================================================================================================================
end subroutine test_suite_M_bessel
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_bessel
!===================================================================================================================================

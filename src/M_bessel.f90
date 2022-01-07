!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_bessel
use M_journal, only : journal
implicit none
private
public bes
public besi
public besj
public besj0
public besj1
public besk
public besy
public besy0
integer,parameter :: dp=kind(0.0d0)
contains
!*!implicit doubleprecision (a-h, o-z)
!*!implicit real(kind=kind(0.0d0)) (a-h, o-z)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!! bes(3f) - [M_bessel::INTRO] calculate Bessel functions J(X), Y(X), I(X), K(X) for doubleprecision arguments and integer orders
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
subroutine bes(x,no,kode,rslt1,rslt2,t1,t2,ierr)

! ident_1="@(#)M_bessel::bes(3f):calculate Bessel functions J(x), Y(x), I(x), K(x) for doubleprecision arguments and integer orders"

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
doubleprecision dx,dxx,dx2,temp,dchk,pi,euler,a0,a,b,c
doubleprecision  sumjin,dxord,dxk
dimension  t1(*),t2(*),a(18),b(21),c(19)
data  pi/3.1415926535897932384626434d0/,                            &
   &      euler/0.57721566490153286060651209d0/
data (a(i),i=1,18)/           0.15999999999999999996d+02,            &
   & 0.96000000000000000392d+02,  0.20859259259259257733d+03,            &
   & 0.23703703703703736105d+03,  0.16626725925925503356d+03,            &
   & 0.79290469135839064596d+02,  0.27400431054739774751d+02,            &
   & 0.71803471197186985165d+01,  0.14763245818980230758d+01,            &
   & 0.24456169711179137024d+00,  0.33342447857340252160d-01,            &
   & 0.380697152755597312d-02,    0.36933105872797696d-03,               &
   & 0.30849206583296d-04,        0.222445483065344d-05,                 &
   & 0.14811194720256d-06,        0.635655159808d-08,                    &
   & 0.68719476736d-09/
data (b(i),i=1,21)/           0.98813927043864915927d+00,            &
   & -.11277407316570291310d-01,  0.5340716774420596d-03,                &
   & -.435456758226194d-04,       0.488456084594416d-05,                 &
   & -.68181429589264d-06,        0.11199290865952d-06,                  &
   & -.2089895303616d-07,         0.4325898624d-08,                      &
   & -.97628537856d-09,           0.23715879424d-09,                     &
   & -.6140542976d-10,            0.1680852992d-10,                      &
   & -.481501184d-11,             0.144621568d-11,                       &
   & -.47808512d-12,              0.1572864d-12,                         &
   & -.31457280d-13,              0.9175040d-14,                         &
   & -.1310720d-13,               0.524288d-14/
data (c(i),i=1,19)/          -0.73804295108687506715d-01,            &
   & 0.11366785079620443739d+02, -0.65838973034256501712d+02,            &
   & 0.14119145750221817396d+03, -0.15929975325701922684d+03,            &
   & 0.11122328958866232246d+03, -0.52866443153661476803d+02,            &
   & 0.18223597971689250243d+02, -0.47661469297599122637d+01,            &
   & 0.97840283604837466112d+00, -0.16191400580768858112d+00,            &
   & 0.2212712874183229440d-01,  -0.2606907391286968320d-02,             &
   & 0.316831265267384320d-03,   -0.6102072906743808d-04,                &
   & 0.1658373309202432d-04,     -0.3439710458347520d-05,                &
   & 0.338099825541120d-06,      -0.343597383680d-09/
   ierr=0
!      bigval=1.0d322
   bigval=1.0d307
   if (kode.lt.10) go to 600
   sign = 1.0d0
   ko = iabs(no) + 1
   xsqfr = 0.25d0*x*x
!     ---------
!     INITIAL CHECK OF ORDER-ARGUMENT RANGE TO DETERMINE IF ORDER IS OUT
!     OF RANGE FOR THE GIVEN ARGUMENT.
!
!     ---------
   if (x) 3,27,3
3  xchk = abs(x)
   if (xchk-1100.0d0) 4,18,608
4  if (no.eq.0) go to 25
   if (xchk - 0.025d0) 5,5,6
5  ano = iabs(no)
   ax = ano*log(2.0d0*ano/xchk) + 0.5d0*log(2.0d0*dble(pi)/ano) - ano + 1.0d0/(12.0d0*ano) + log(xchk)/2.0d0
   rslt1 = ax
   if (ax-679.0d0) 25,25,606
6  if (xchk-0.20d0) 7,7,8
7  largor =  int(140.0d0*xchk + 83.0d0)
   go to 24
8  if (xchk-1.0d0) 9,9,10
9  largor =  int(42.0d0*xchk + 102.0d0)
   go to 24
10 if (xchk-20.0d0) 11,11,12
11 largor =  int(((0.02d0*xchk-0.86d0)*xchk+17.15d0)*xchk+124.0d0)
   go to 24
12 if (xchk-100.0d0) 13,13,14
13 largor =  int(2.75d0*xchk + 228.0d0)
   go to 24
14 if (xchk-400.0d0) 16,16,18
16 largor =  int(1.67d0*xchk + 336.0d0)
   go to 24
18 largor =  int(1.33d0*xchk + 470.0d0)
24 if (iabs(no)-largor) 25,25,606
25 xx=x
!     ---------
!     DETERMINE WHICH SET OF FUNCTIONS IS TO BE CALCULATED.
!     ---------
27 mask1 = kode/10
   if (mask1-2) 30,31,600
30 mask2 = kode-10
   go to 32
31 mask2 = kode-20
32 if (mask2-2) 34,36,600
34 if (mask2) 600,37,42
!     ---------
!     CHECK FUNCTIONS J(X) AND I(X) FOR ZERO ARGUMENT.
!     ---------
36 if (x) 604,38,59
37 if (x) 58,38,59
38 if (no) 54,40,54
40 t1(1) = 1.0d0
   rslt1 = 1.0d0
   if (mask2.eq.0) return
!     ---------
!     CHECK FUNCTIONS Y(X) AND K(X) FOR ZERO ARGUMENT.
!     The functions y0, y1, and yn have logarithmic  singularities
!     at the origin, so they treat zero and negative arguments the
!     way log does. Such  arguments  are unexceptional for j0, j1, and jn.
!     ---------
42 if (x) 604,44,59
44 continue
   if (mask1.eq.2) go to 50
   do ik=1,ko
      t2(ik) = -bigval
   enddo
   rslt2  = -bigval
   return
50 continue
   do ik=1,ko
      t2(ik) = bigval
   enddo
   rslt2  = bigval
   return
!     ---------
!     FILL OUT ARRAY FOR J(X) OR I(X) WHEN (NO.NE.0).
!     ---------
54 continue
   do ik=2,ko
      t1(ik) = 0.0d0
   enddo
   rslt1 = 0.0d0
   t1(1) = 1.0d0
   if (mask2.eq.0) return
   go to 44
58 continue
   x = abs(x)
59 continue
   mo = iabs(no)
   imo = mo
   if (x-1.0d0) 60,71,71
60 continue
   if (mask1.eq.2) go to 175
!     ---------
!     USE SERIES TO DETERMINE J(N) AND J(N-1) WHEN ARGUMENT IS SMALL,
!     THEN USE RECURRENCE TO DETERMINE REMAINING FUNCTION VALUES.
!     ---------
   xord = mo
   dxord = xord
   if (mo.gt.1) go to 61
   dx2 = 1.0d0
   a0 = 1.0d0
   iloop = 1
   go to 63
61 continue
   a0 = 1.0d0
   iend = mo-1
   do ik=1,iend
      xk = ik
      dxk = xk
      a0 = a0*dxk
   enddo
   dx2 = 1.0d0/(a0*dxord)
   a0 = 1.0d0/a0
   iloop = 1
63 continue
   sumjin = dx2
   dx = x
   dxx = 0.25d0*dx*dx
   do ik=1,200
      xk = ik
      dxk = xk
      temp = -dx2*dxx/(dxk*(dxord+dxk))
      sumjin = sumjin + temp
      if (sumjin) 64,65,64
64    dchk = abs(temp/sumjin)
      if (dchk - 1.0d0-20.0d0) 67,65,65
65    dx2 = temp
   enddo
67 if (iloop.gt.1) go to 68
   t1(ko) = dble(sumjin*(0.5d0*dx)**mo)
   if (mo.eq.0) go to 83
   iloop = 2
   dx2 = a0
   xord = mo-1
   dxord = xord
   go to 63
68 t1(ko-1) = dble(sumjin*(0.5d0*dx)**(mo-1))
   if (ko.le.2) go to 83
   iend = ko-2
   do ik=1,iend
      nk = ko-ik
      xnkm1 = nk-1
      t1(nk-1) = 2.0d0*xnkm1 *t1(nk)/x - t1(nk+1)
   enddo
   go to 83
71 if (mask2.eq.0) go to 74
!     ---------
!     DETERMINE STARTING LOCATION OF RECURRENCE IF Y(X) OR K(X)
!     ARE TO BE FOUND.
!     ---------
   jo = 2* int(x)
   if (imo - jo) 72,73,73
72 imo = jo
73 imo = imo + 51
   go to 78
!     ---------
!     DETERMINE STARTING LOCATION FOR RECURRENCE OF J(X).
!     ---------
74 jo = 2* int(x)
   if (imo - jo) 75,76,76
75 imo = jo
76 imo = imo + 51
!     ---------
!     INITIALIZE VALUES FOR J(X) AND Y(X)
!     ---------
78 t1(imo) = 0.0d0
   t1(imo-1) = 1.0d0-200
   if (mask1.eq.2) go to 151
   f = 2*(imo-1)
   imo = imo - 3
   i2 = imo
79 f = f - 2.0d0
!     ---------
!     RECURRENCE USED FOR FUNCTION VALUES.
!     VARIABLE SUM IS USED TO DETERMINE ADJUSTMENT FACTOR
!     ON RECURRED VALUES.
!     ---------
   t1(i2+1) = f/x*t1(i2+2) - t1(i2+3)
   if (i2) 80,81,80
80 i2 = i2-1
   go to 79
81 sum = t1(1)
   do j=3,imo,2
      sum = sum + 2.0d0*t1(j)
   enddo
   f = 1.0d0/sum
83 if (no) 86,84,84
84 if (xx) 90,32,92
86 if (xx) 92,32,90
90 sign = -sign
92 if (mask2.eq.0) go to 93
   go to 300
93 if (x - 1.0d0) 96,94,94
94 continue
   do j=1,ko
     t1(j) = t1(j)*f
   enddo
96 if (mo.eq.0) go to 98
   do j=2,ko,2
      t1(j) = t1(j)*sign
   enddo
98 rslt1 = t1(ko)
   x = xx
   return
!     ---------
!     INITIALIZE STARTING VALUES FOR I(X) AND K(X) RECURRENCE.
!     ---------
151 if (x-600.0d0) 152,152,602
152 f = 2*(imo-1) - 2
   imo = imo - 3
   i2 = imo
153 t1(i2+1) = f/x*t1(i2+2) + t1(i2+3)
   if (i2) 154,155,154
154 i2 = i2-1
   f=f-2.0d0
   go to 153
155 sum = t1(1)
   do j=2,imo
      sum = sum + 2.0d0*t1(j)
   enddo
   f = 1.0d0/sum*exp(x)
   if (xx) 171,32,172
171 sign = -sign
172 continue
   do j=1,ko,2
      t1(j) = t1(j)*f
      t1(j+1) = t1(j+1)*f*sign
   enddo
   rslt1 = t1(ko)
   if (mask2.ne.0) go to 400
   x = xx
   return
175 xord = mo
   dxord = xord
   if (mo.gt.1) go to 177
   dx2 = 1.0d0
   a0 = 1.0d0
   iloop = 1
   go to 180
177 a0 = 1.0d0
   iend = mo - 1
   do ik=1,iend
      xk = ik
      dxk = xk
      a0 = a0*dxk
   enddo
   dx2 = 1.0d0/(a0*dxord)
   a0 = 1.0d0/a0
   iloop = 1
180 sumjin = dx2
   dx = x
   dxx = 0.25d0*dx*dx
   do ik=1,200
      xk = ik
      dxk = xk
      temp = dx2*dxx/(dxk*(dxord+dxk))
      sumjin = sumjin + temp
      dchk = abs(temp/sumjin)
      if (dchk - 1.0d0-20) 184,181,181
181   continue
    dx2 = temp
   enddo
184 continue
    if (iloop.gt.1) go to 185
   t1(ko) = dble(sumjin*(0.5d0*dx)**mo)
   if (mo.eq.0) go to 188
   iloop = 2
   dx2 = a0
   xord = mo-1
   dxord = xord
   go to 180
185 continue
    t1(ko-1) = dble(sumjin*(0.5d0*dx)**(mo-1))
   if (ko.le.2) go to 188
   iend = ko-2
   do ik=1,iend
      nk = ko-ik
      xnkm1 = nk-1
      t1(nk-1) = 2.0d0*xnkm1 *t1(nk)/x + t1(nk+1)
   enddo
188 if (xx) 189,32,190
189 sign = -sign
190 if (mo.eq.0) go to 194
   do j=2,ko,2
      t1(j) = t1(j)*sign
   enddo
194 continue
    rslt1 = t1(ko)
   if (mask2.ne.0) go to 400
   x = xx
   return
!     ---------
!     EVALUATE Y0 AND Y1 TO START RECURRENCE.
!     ---------
300 if (x-1.0d0) 3001,301,301
3001 dx = x
   dxx = dx*dx/64.0d0
   dx2 = 1.0d0
   temp = c(1)
   do j=2,19
      dx2 = dx2*dxx
      a0 = c(j)*dx2
      temp = temp + a0
      if (temp) 3005,3010,3005
3005  dchk = abs(a0/temp)
      if (dchk - 1.0d0-20) 3015,3010,3010
3010 continue
   enddo
3015 a0 = (2.0d0/pi)*log(dx)
   t2(1) = a0*t1(1) + temp
   go to 321
301 continue
   do j=1,imo
      t1(j) = t1(j)*f
   enddo
   sumj1 = 0.0d0
   sumj2 = 0.0d0
   if (imo.le.80) go to 305
   if (ko - jo) 303,304,304
303 kend = jo/2
   go to 306
304 kend = ko/2
   go to 306
305 kend = imo/2
306 continue
    do n=1,kend,2
      xn = n
      sumj1 = sumj1 + t1(2*n+1)/xn
   enddo
   do n=2,kend,2
      xn = n
      sumj2 = sumj2 + t1(2*n+1)/xn
   enddo
   sumjn = 2.0d0*(sumj2-sumj1)
   t2(1) = 2.0d0/pi*(t1(1)*(log(x/2.0d0) + euler) - sumjn)
321 continue
    if (mo.gt.0) go to 309
   rslt1 = t1(1)
   rslt2 = t2(1)
   x = xx
   return
309 continue
    t2(2) = (t1(2)*t2(1) - 2.0d0/(pi*x))/t1(1)
   if (mo.eq.1) go to 311
   nord = ko-1
   do n=2,nord
      xn = n-1
      t2(n+1) = (2.0d0*xn)/x*t2(n) - t2(n-1)
    enddo
311 continue
    do j=2,ko,2
      t2(j) = t2(j)*sign
   enddo
   rslt2 = t2(ko)
   if (mask2.eq.1) go to 315
   do j=2,ko,2
      t1(j) = t1(j)*sign
   enddo
   rslt1 = t1(ko)
315 x = xx
   return
400 dx = x
   dx2 = 1.0d0
   if (x-6.0d0)  410,410,440
410 dxx = dx*dx/64.0d0
   temp = 0.0d0
   do j=1,18
      dx2 = dx2*dxx
      a0 = a(j)*dx2
      temp = temp + a0
      dchk = a0/temp
      if (dchk - 1.0d0-20) 430,420,420
420 continue
    enddo
430 a0 = -(euler + log(0.5d0*dx))
   t2(1) = a0*t1(1) + temp
   if (no.eq.0) go to 490
   go to 480
440 dxx = 10.0d0/dx - 1.0d0
   temp = b(1)
   do j=2,21
      dx2 = dx2*dxx
      a0 = b(j)*dx2
      temp = temp + a0
      if (temp) 450,460,450
450   dchk = abs(a0/temp)
      if (dchk - 1.0d0-20) 470,460,460
460 continue
   enddo
470 t2(1) = sqrt(pi/(2.0d0*dx))*exp(-dx)*temp
   if (no.eq.0) go to 490
480 t2(2) = (1.0d0/x - t2(1)*t1(2))/t1(1)
   if (ko.le.2) go to 490
   nord = ko-1
   do n=2,nord
      xn = n-1
      t2(n+1) = (2.0d0*xn)/x *t2(n) + t2(n-1)
   enddo
490 continue
   rslt2 = t2(ko)
   x = xx
   return
600 continue
   ierr=5
   return
602 continue
   x=xx
   ierr=3
   return
604 continue
   ierr=4
   return
606 continue
   ierr=2
   return
608 continue
   ierr=1
   return
end subroutine bes
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
subroutine besi(x,n, bi,ier)

! ident_2="@(#)M_bessel::besi(3f):compute the I Bessel function for a given argument and order"

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
   ier=0
   bi=1.0d0
!  Checks for errors IN N and X and exits if any are present
   if(n)150,15,10
10 if(x)160,20,20
15 if(x)160,17,20
17 continue
   return
!
!     DEFINE TOLERANCE
!
20 continue
   tol=1.d-6
!
!     IF ARGUMENT GT 12 AND GT N, USE ASYMPTOTIC FORM
!
   if(x-12.0d0)40,40,30
30 continue
   if(x-dble(n))40,40,110
!
!     COMPUTE FIRST TERM OF SERIES AND SET INITIAL VALUE OF THE SUM
!
40 continue
   xx=x/2.0d0
   term=1.00d0
   if(n) 70,70,55
55 continue
   do i=1,n
      fi=i
      if(abs(term)-1.0d-68)56,60,60
56    continue
      ier=3
      bi=0.0d0
      return
60    continue
      term=term*xx/fi
   enddo
70 continue
   bi=term
   xx=xx*xx
!
!     COMPUTE TERMS, STOPPING WHEN ABS(TERM) LE ABS(SUM OF TERMS)
!     TIMES TOLERANCE
!
   do k=1,1000
      if(abs(term)-abs(bi*tol))100,100,80
80    continue
      fk=k*(n+k)
      term=term*(xx/fk)
      bi=bi+term
   enddo
!
!     RETURN BI AS ANSWER
!
100 return
!
!     X GT 12 AND X GT N, SO USE ASYMPTOTIC APPROXIMATION
!
110 continue
   fn=4*n*n
   if(x-170.0d0)115,111,111
111 continue
   ier=4
   return
115 xx=1.0d0/(8.0d0*x)
   term=1.0d0
   bi=1.0d0
   do k=1,30
      if(abs(term)-abs(tol*bi))140,140,120
120   continue
      fk=(2*k-1)**2
      term=term*xx*(fk-fn)/dble(k)
      bi=bi+term
   enddo
!
!     SIGNIFICANCE LOST AFTER 30 TERMS, TRY SERIES
!
   go to 40
140 pi=3.141592653d0
   bi=bi*exp(x)/sqrt(2.0d0*pi*x)
   go to 100
150 ier=1
   go to 100
160 ier=2
   go to 100
end subroutine besi
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
subroutine besj(x,n,bj,d,ier)

! ident_3="@(#)M_bessel::besj(3f):compute the J Bessel function for a given argument and order"

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
   bj=0.0d0
   if(n)10,20,20
10 ier=1
   return
20 if(x)30,30,31
30 ier=2
   return
31 if(x-15.0d0)32,32,34
32 ntest=20.0d0+10.0d0*x-x**2/3
   go to 36
34 ntest=90.0d0+x/2.0d0
36 if(n-ntest)40,38,38
38 ier=4
   return
40 ier=0
   n1=n+1
   bprev=0.0d0
!
!     COMPUTE STARTING VALUE OF M
!
   if(x-5.0d0)50,60,60
50 ma=x+6.0d0
   go to 70
60 ma=1.4d0*x+60.0d0/x
70 mb=n+int(x)/4+2
   mzero=max0(ma,mb)
!
!     SET UPPER LIMIT OF M
!
   mmax=ntest
   do m=mzero,mmax,3
!
!     SET F(M),F(M-1)
!
      fm1=1.0d0-28.0d0
      fm=0.0d0
      alpha=0.0d0
      if(m-(m/2)*2)120,110,120
110   jt=-1
      go to 130
120   jt=1
130   m2=m-2
      do k=1,m2
         mk=m-k
         bmk=2.0d0*dble(mk)*fm1/x-fm
         fm=fm1
         fm1=bmk
         if(mk-n-1)150,140,150
140      bj=bmk
150      jt=-jt
         s=1.0d0+jt
         alpha=alpha+bmk*s
      enddo
      bmk=2.0d0*fm1/x-fm
      if(n)180,170,180
170   bj=bmk
180   alpha=alpha+bmk
      bj=bj/alpha
      if(abs(bj-bprev)-abs(d*bj))200,200,190
190 continue
    bprev=bj
    enddo
   ier=3
200 return
end subroutine besj
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
function besj0(xx)

! ident_4="@(#)M_bessel::besj0(3f):calculates the Bessel function J(X) of order zero."

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
dimension t1(101)
dimension t2(101) ! for bug in Intel 11.1.046 compiler Sun Aug 23 15:27:46 EDT 2009
data twovpi/0.63661977236758d0/,piov4/0.78539816339745d0/
data p1/4.5d0/,p2/4.59375d02/,p3/1.500778125d05/
data q1/37.5d0/,q2/7.441875d03/,q3/3.623307187d06/
   x = abs(xx)
   if (x - 25.0d0) 20,20,40
20 call bes(xx,0,10,besj0,r2,t1,t2,ierr)
   return
40 chi = x - piov4
   factor = sqrt(twovpi/x)
   eightx = 0.125d0/x
   exsq = eightx*eightx
   exfour = exsq*exsq
   exsix = exfour*exsq
   p = 1.0d0 - p1*exsq + p2*exfour - p3*exsix
   q = eightx*(-1.0d0 + q1*exsq - q2*exfour + q3*exsix)
   besj0 = factor*(p*cos(chi) - q*sin(chi))
   return
end function besj0
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
function besj1(xx)

! ident_5="@(#)M_bessel::besj1(3f): calculates the Bessel function J(X) of order one."

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
dimension t1(101)
dimension t2(101) ! for Intel compiler bug 11.1.046 Sun Aug 23 2009
data  pi/3.1415926535898d0/,twovpi/0.63661977236758d0/
data p1/7.5d0/,p2/5.90625d02/,p3/1.773646875d05/
data q1/5.25d01/,q2/9.095625d03/,q3/4.180739062d06/
   x = abs(xx)
   if (x - 25.0d0) 20,20,40
20 call bes(xx,1,10,besj1,r2,t1,t2,ierr)
   return
40 chi = x - 0.75d0*pi
   factor = sqrt(twovpi/x)
   eightx = 0.125d0/x
   exsq = eightx*eightx
   exfour = exsq*exsq
   exsix = exfour*exsq
   p = 1.0d0 + p1*exsq - p2*exfour + p3*exsix
   q = eightx*(3.0d0 - q1*exsq + q2*exfour - q3*exsix)
   besj1 = factor*(p*cos(chi) - q*sin(chi))
   if (xx) 60,80,80
60 besj1 = -besj1
80 return
end function besj1
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
subroutine besk(x,n,bk,ier)

! ident_6="@(#)M_bessel::besk(3f):compute the K bessel function for a given argument and order"

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
   bk=0.0d0
   if(n)10,11,11
10 ier=1
   return
11 if(x)12,12,20
12 ier=2
   return
20 if(x-170.00d0)22,22,21
21 ier=3
   return
22 ier=0
   if(x-1.0d0)36,36,25
25 a=exp(-x)
   b=1.0d0/x
   c=sqrt(b)
   t(1)=b
   do l=2,12
      t(l)=t(l-1)*b
   enddo
   if(n-1)27,29,27
!
!     COMPUTE KO USING POLYNOMIAL APPROXIMATION
!
27 g0=a*(1.25331414d0-.15666418d0*t(1)+.088111278d0*t(2)-.091390954d0*t(3)    &
   &+.13445962d0*t(4)-.22998503d0*t(5)+.37924097d0*t(6)-.52472773d0*t(7)       &
   &+.55753684d0*t(8)-.42626329d0*t(9)+.21845181d0*t(10)-.066809767d0*t(11)    &
   &+.009189383d0*t(12))*c
   if(n)20,28,29
28 bk=g0
   return
!
!     COMPUTE K1 USING POLYNOMIAL APPROXIMATION
!
29 g1=a*(1.2533141d0+.46999270d0*t(1)-.14685830d0*t(2)+.12804266d0*t(3)       &
   &-.17364316d0*t(4)+.28476181d0*t(5)-.45943421d0*t(6)+.62833807d0*t(7)       &
   &-.66322954d0*t(8)+.50502386d0*t(9)-.25813038d0*t(10)+.078800012d0*t(11)    &
   &-.010824177d0*t(12))*c
   if(n-1)20,30,31
30 bk=g1
   return
!
!     FROM KO,K1 COMPUTE KN USING RECURRENCE RELATION
!
31 continue
   do j=2,n
      gj=2.d0*(dble(j)-1.d0)*g1/x+g0
      if(gj-1.0d70)33,33,32
32    ier=4
      go to 34
33    g0=g1
      g1=gj
   enddo
34 bk=gj
   return
36 b=x/2.d0
   a=.57721566d0+log(b)
   c=b*b
   if(n-1)37,43,37
!
!     COMPUTE KO USING SERIES EXPANSION
!
37 g0=-a
   x2j=1.0d0
   fact=1.0d0
   hj=0.0d0
   do j=1,6
      rj=1.0d0/dble(j)
      x2j=x2j*c
      fact=fact*rj*rj
      hj=hj+rj
      g0=g0+x2j*fact*(hj-a)
   enddo
   if(n)43,42,43
42 bk=g0
   return
!
!     COMPUTE K1 USING SERIES EXPANSION
!
43 x2j=b
   fact=1.0d0
   hj=1.0d0
   g1=1.0d0/x+x2j*(0.5d0+a-hj)
   do j=2,8
      x2j=x2j*c
      rj=1.0d0/dble(j)
      fact=fact*rj*rj
      hj=hj+rj
      g1=g1+x2j*fact*(0.50d0+(a-hj)*dble(j))
   enddo
   if(n-1)31,52,31
52 continue
   bk=g1
   return
end subroutine besk
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
subroutine besy(x,n,by,ier)

! ident_7="@(#)M_bessel::besy(3f):compute the Y Bessel function for a given argument and order"

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
   if(n)180,10,10
10 ier=0
   if(x)190,190,20
20 pi=3.141592653d0
!
!     BRANCH IF X LESS THAN OR EQUAL 4
!
   if(x-4.0d0)40,40,30
!
!       COMPUTE Y0 AND Y1 FOR X GREATER THAN 4
!
30 t=4.0d0/x
   p0=0.3989422793d0
   q0=-0.0124669441d0
   p1=0.3989422819d0
   q1=0.0374008364d0
   a=t*t
   b=a
   p0=p0-0.0017530620d0*a
   q0=q0+0.0004564324d0*a
   p1=p1+0.0029218256d0*a
   q1=q1-0.00063904d0*a
   a=a*a
   p0=p0+.00017343d0*a
   q0=q0-.0000869791d0*a
   p1=p1-.000223203d0*a
   q1=q1+.0001064741d0*a
   a=a*b
   p0=p0-.0000487613d0*a
   q0=q0+.0000342468d0*a
   p1=p1+.0000580759d0*a
   q1=q1-.0000398708d0*a
   a=a*b
   p0=p0+.0000173565d0*a
   q0=q0-.0000142078d0*a
   p1=p1-.000020092d0*a
   q1=q1+.00001622d0*a
   a=a*b
   p0=p0-.0000037043d0*a
   q0=q0+.0000032312d0*a
   p1=p1+.0000042414d0*a
   q1=q1-.0000036594d0*a
   a=sqrt(2.0d0*pi)
   b=4.0d0*a
   p0=a*p0
   q0=b*q0/x
   p1=a*p1
   q1=b*q1/x
   a=x-pi/4.0d0
   b=sqrt(2.0d0/(pi*x))
   y0=b*(p0*sin(a)+q0*cos(a))
   y1=b*(-p1*cos(a)+q1*sin(a))
   go to 90
!
!       COMPUTE Y0 AND Y1 FOR X LESS THAN OR EQUAL TO 4
!
40 xx=x/2.0d0
   x2=xx*xx
   t=log(xx)+.5772156649d0
   sum=0.0d0
   term=t
   y0=t
   do l=1,15
      if(l-1)50,60,50
50    sum=sum+1.0d0/dble(l-1)
60    fl=l
      ts=t-sum
      term=(term*(-x2)/fl**2)*(1.0d0-1.0d0/(fl*ts))
      y0=y0+term
   enddo
   term = xx*(t-0.5d0)
   sum=0.0d0
   y1=term
   do l=2,16
      sum=sum+1.0d0/dble(l-1)
      fl=l
      fl1=fl-1.0d0
      ts=t-sum
      term=(term*(-x2)/(fl1*fl))*((ts-0.5d0/fl)/(ts+0.5d0/fl1))
      y1=y1+term
   enddo
   pi2=2.0d0/pi
   y0=pi2*y0
   y1=-pi2/x+pi2*y1
!
!     CHECK IF ONLY Y0 OR Y1 IS DESIRED
!
90 continue
   if(n-1)100,100,130
!
!     RETURN EITHER Y0 OR Y1 AS REQUIRED
!
100 if(n)110,120,110
110 by=y1
   go to 170
120 by=y0
   go to 170
!
!    PERFORM RECURRENCE OPERATIONS TO FIND YN(X)
!
130 ya=y0
   yb=y1
   k=1
140 t=dble(2*k)/x
   yc=t*yb-ya
   if(abs(yc)-1.0d70)145,145,141
141 ier=3
   return
145 k=k+1
   if(k-n)150,160,150
150 ya=yb
   yb=yc
   go to 140
160 by=yc
170 return
180 ier=1
   return
190 ier=2
   return
end subroutine besy
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
function besy0(x)

! ident_8="@(#)M_bessel::besy0(3f): calculates the Bessel function Y(X) of order zero."

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
real(kind=dp),parameter :: twovpi=  0.63661977236758d0
real(kind=dp),parameter :: piov4 =  0.78539816339745d0
real(kind=dp),parameter :: p1    =  4.5d0
real(kind=dp),parameter :: p2    =  4.59375d02
real(kind=dp),parameter :: p3    =  1.500778125d05
real(kind=dp),parameter :: q1    = 37.5d0
real(kind=dp),parameter :: q2    =  7.441875d03
real(kind=dp),parameter :: q3    =  3.623307187d06
!-----------------------------------------------------------------------------------------------------------------------------------
   if(x.lt.0.0d0)then
      besy0=-10.0d0-32.0d0   ! bad value assigned so compiler does not complain unassigned
      call journal('*besy0* ERROR: negative input value')
   elseif ((x - 25.0_dp) .lt. 0_dp)then
      call bes(x,0,11,r1,besy0,t1,t2,ierr)
   else
      chi    = x - piov4
      factor = sqrt( twovpi / x )
      eightx = 0.125_dp / x
      exsq   = eightx * eightx
      exfour = exsq * exsq
      exsix  = exfour * exsq
      p      = 1.0_dp - p1*exsq + p2*exfour - p3*exsix
      q      = eightx * ( -1.0_dp + q1*exsq - q2*exfour + q3*exsix )
      besy0  = factor * ( p*sin(chi) + q*cos(chi) )
   endif
end function besy0
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_bessel
!===================================================================================================================================

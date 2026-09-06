!>
!!  Copyright (c) 1967, 1990, 1992,1993,1995  Westinghouse Electric
!!  Corporation.  All Rights Reserved.
!!
!!  All copies of the software package should include this notice  and  any
!!  documentation,  advertising  materials,  and  other materials  related
!!  to such  distribution  and use should  acknowledge that the software was
!!  developed by  Westinghouse  Corporate  Computer Services.  THIS  SOFTWARE
!!  IS  PROVIDED  ``AS  IS'' AND  WITHOUT  ANY EXPRESS OR IMPLIED  WARRANTIES
!!  OF  MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE.
module M_steam67
!!implicit double precision(A-H, O-Z)
implicit none
private
! ident_1="@(#) M_steam67 steam67(3f) 1967 ASME Steam Table Library"
! core
public  ::  condv67   !  FUNCTION    CONDV67   (P,T)
public  ::  condl67   !  ENTRY       CONDL67   (P,T)
!private ::  DO67      !  FUNCTION    DO67      (A,X,ISTART,ISTOP)
public  ::  cpl67     !  FUNCTION    cpl67     (PF,TF)
public  ::  cpv67     !  FUNCTION    cpv67     (PF,TF,VF)
public  ::  crflo67   !  FUNCTION    crflo67   (PRES,ENTH,SHT)
public  ::  critvs67  !  FUNCTION    CRITVS67  (PRESS,ENTH,GAMMA)
public  ::  critvw67  !  ENTRY       CRITVW67  (PRESS,ENTH,GAMMA)
public  ::  crvel67   !  FUNCTION    crvel67   (P,H,GA)
public  ::  hcsl67    !  FUNCTION    hcsl67    (P,T,V,S,IGO)
public  ::  hcslv167  !  FUNCTION    hcslv167  (P,T,V,S,IGO)
public  ::  hcslv267  !  ENTRY       HCSLV267  (P,T,V,S,IGO)
private ::  vest67    !  SUBROUTINE  vest67    (PBARS,TC,VOUT)
public  ::  hss67     !  FUNCTION    HSS67     (P,T,S,V)
public  ::  hss167    !  ENTRY       HSS167    (P,T,S,V)
public  ::  psl67     !  FUNCTION    PSL67     (T)
public  ::  psl167    !  ENTRY       PSL167    (T)
public  ::  psv67     !  FUNCTION    PSV67     (SS)
public  ::  psv167    !  ENTRY       PSV167    (SS)
public  ::  psv267    !  ENTRY       PSV267    (SS)
public  ::  pls67     !  ENTRY       PLS67     (SS)
public  ::  sssiss67  !  FUNCTION    sssiss67  (PRES,ENTH,TEMP,V,X)
public  ::  sisiss67  !  ENTRY       sisiss67  (PRES,ENTH,TEMP,V,X)
public  ::  hisiss67  !  ENTRY       hisiss67  (PRES,ENTH,TEMP,V,X)
public  ::  hssiss67  !  ENTRY       hssiss67  (PRES,ENTH,TEMP,V,X)
public  ::  spsiss67  !  ENTRY       spsiss67  (PRES,ENTH,TEMP,V,X)
public  ::  hpsiss67  !  ENTRY       hpsiss67  (PRES,ENTH,TEMP,V,X)
public  ::  tps67     !  FUNCTION    TPS67     (PRES,ENTR)
public  ::  tph67     !  ENTRY       TPH67     (PRES,ENTR)
public  ::  tpsl67    !  FUNCTION    tpsl67    (PRES,S)
public  ::  tphl67    !  ENTRY       TPHL67    (PRES,S)
public  ::  tsl67     !  FUNCTION    tsl67     (PIN)
public  ::  tsl167    !  ENTRY       tsl167    (PIN)
public  ::  tslh67    !  FUNCTION    tslh67    (ENTH)
public  ::  visl67    !  FUNCTION    visl67    (P,T)
public  ::  visv67    !  ENTRY       visv67    (P,T)
! simple
public  ::  hcl67     !  FUNCTION    hcl67     (P,T,S)
public  ::  hsl67     !  FUNCTION    hsl67     (T)
public  ::  ssl67     !  ENTRY       ssl67     (T)
public  ::  vsl67     !  ENTRY       vsl67     (T)
public  ::  hsv67     !  FUNCTION    hsv67     (P,T,S,V)
public  ::  prliq67   !  FUNCTION    prliq67   (P,T)
public  ::  prstm67   !  FUNCTION    prstm67   (P,T)
public  ::  sssicl67  !  FUNCTION    sssicl67  (PRES,H,TEMP)
public  ::  hssicl67  !  ENTRY       hssicl67  (PRES,S,TEMP)
!
public  ::  vcl67     !  FUNCTION    vcl67     (p,t)
private ::  gr167     !  FUNCTION    GR167     (T,X,N)
private ::  grs67     !  FUNCTION    GRS67     (X,NDX,Y,NDY,XV,N,NRANGE)
private ::  p23t67    !  FUNCTION    p23t67    (TIN)
private ::  vliq67    !  FUNCTION    vliq67    (PIN,TIN,VMIN,VMAX)
!
public  ::  zsdh67    !  FUNCTION    ZSDH67    (P1,P2,H1,S,T1,T2,X1,X2,V1,V2)
public  ::  zsdt67    !  FUNCTION    zsdt67    (P1,P2,T1,S,H1,T2,X1,X2,V1,V2)
public  ::  zsrh67    !  FUNCTION    zsrh67    (P1,P2,H1,S,T1,T2)
public  ::  zsrt67    !  FUNCTION    zsrt67    (P1,P2,T1,S,H1,T2)
public  ::  steamv67  !  SUBROUTINE  steamv67  ()

! print error messages
private ::  gotoer67  !  SUBROUTINE  GOTOER67  (NAME,I)
private ::  ster67    !  SUBROUTINE  STER67    (NAME,I,A,B)
private ::  zqjrem67  !  subroutine  zqjrem67  (string)

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     condv67(3f) - [M_steam67] compute thermal conductivity of water at pressure P and temperature T
!!
!!##SYNOPSIS
!!
!!    FUNCTION CONDV67(P,T) result(condv67_result)
!!
!!##DESCRIPTION
!!    function CONDV67 will compute the thermal conductivity of water at
!!    pressure P and temperature T.
!!
!!    DEFINITION AND PURPOSE:
!!
!!       o K = f (P,T)
!!       o Calculates thermal conductivity
!!
!!    REGION OF APPLICATION:
!!
!!         Region 2 with: P[max] = 500 bars = 7251.8869 psia. T[max] = 700 C =
!!         1292 F
!!         See Section 3.3
!!
!!    CALLING SEQUENCE:
!!
!!         K = CONDV(P,T)
!!         (Note that this statement should not be used in FORTRAN, unless K is
!!         declared to be REAL.)
!!
!!##OPTIONS
!!    P         water pressure, in PSIA.
!!    T         water temperature, in degrees F.
!!
!!##RETURN VALUES
!!    CONDV67   conductivity in BTU/HR-FT-DEG.F
function condv67(p,t) result(condv67_result)
!external VCL67
!SAVE
! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS

! ident_2="@(#) M_steam67 condv67(3f) compute thermal conductivity of water at pressure P and temperature T"
! ident_3="@(#) M_steam67 condl67(3f) return thermal conductivity given P T"

doubleprecision,intent(in) :: p, t
doubleprecision :: g(44),b(6),c(8)
doubleprecision :: tau1,tau, psat, dens
doubleprecision :: condv67_result
doubleprecision :: c8
integer         :: in(8),id(8)
integer         :: i
!-JU  A
      data (g(i),i=41,44)/             &
     &   -4.51d-2,                     &
     &    1.40957195d0,                &
     &   -8.210057814d-1,              &
     &    1.024476774d1/
!-JU  B
      data (b(i),i=1,6)/               &
     &   8.984504672d3,                &
     &  -2.890032838d4,                &
     &   3.443892141d4,                &
     &  -1.814175922d4,                &
     &   4.245698830d3,                &
     &   2.699696542d2/
!-JU  D1
      data (g(i),i=32,36)/             &
     &   7.180025637d6,                &
     &  -1.776928510d8,                &
     &   1.617702015d9,                &
     &  -6.269159344d9,                &
     &   8.370013127d9/
!-JU  D2
      data (g(i),i=37,40)/             &
     &  -1.827766246d-2,               &
     &   3.045585779d-1,               &
     &  -1.425775726d0,                &
     &   1.0d0/
!-JU  d1
      data (g(i),i=20,28)/             &
     &   1.434898860d5,                &
     &  -5.596033887d6,                &
     &   9.619210400d7,                &
     &  -9.533605635d+8,               &
     &   5.993649877d9,                &
     &  -2.479491662d10,               &
     &   6.715871576d10,               &
     &  -1.103409249d11,               &
     &   8.315570232d10/
!-JU  d2
      data (g(i),i=29,31)/             &
     &   2.336550603d-2,               &
     &  -3.055656614d-1,               &
     &   1.0d0/
!-JU  F1
      data (g(i),i=6,14)/              &
     &  -7.835564159d4,                &
     &   3.314220164d6,                &
     &  -6.167236570d7,                &
     &   6.603888081d8,                &
     &  -4.473203373d9,                &
     &   1.979540831d10,               &
     &  -5.639923016d10,               &
     &   9.483915397d10,               &
     &  -7.115154241d10/
!-JU  F2
      data (g(i),i=15,19)/             &
     &   6.606768113d-5,               &
     &  -3.923316061d-3,               &
     &   6.625711877d-2,               &
     &  -4.368537279d-1,               &
     &   1.0d0/
      data (g(i),i=1,5)/               &
     &   5.873976935d7,                &
     &  -9.12673946000d8,              &
     &   5.690390782d9,                &
     &  -1.771196944d10,               &
     &   2.148955071d10/
      data (in(i),i=1,8)/1,6,15,20,29,32,37,41/
      data (id(i),i=1,8)/5,14,19,28,31,36,40,44/
      c(1)=hss67(p,t,c(2),dens)
      go to 5
!
!     THE RESULTS OBTAINED ARE WELL WITHIN THE TOLERANCES FOR VALUES
!     WITHIN THE RANGE OF THE SKELETON TABLES.  THE RESULTS FOR VALUES
!     OUTSIDE THE RANGE OF THE SKELETON TABLE APPEAR TO BE REASONABLE
!     FOR TEMPERATURES UP TO 820C AND PRESSURES TO 700 BARS
!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     condl67(3f) - [M_steam67] return thermal conductivity given P,T
!!##SYNOPSIS
!!
!!       ENTRY CONDL67(P,T) result(condv67_result)
!!##DESCRIPTION
!!
!!     DEFINITION AND PURPOSE:
!!
!!      o K = f(P,T)
!!      o Computes thermal conductivity
!!
!!     REGION OF APPLICATION:
!!
!!         Region 1 with P[max] = 500 bars , 7251.8869 psia.
!!         See Section 3.3.
!!
!!     CALLING SEQUENCE:
!!         K = CONDL(P,T)
!!         (Note that this statement should not be used in FORTRAN, unless K is
!!         declared to be REAL.)
!!
!!##OPTIONS
      entry condl67(p,t) result(condv67_result)
      dens=vcl67(p,t)
5     continue
      dens=0.0160184634d0/dens
      c8=0.d0
      tau1=(t-32.0d0)/1.8d0
      if (tau1.lt.0.5d0) tau1=0.5d0
      tau=tau1*1.0d-2+2.7315d0
      if (tau1.ge.700.0d0) go to 15
!     IF TAU1 IS .GT. 650. AND .LT. 700. INTERPOLATION IS NECESSARY
!     AND TAU=9.2315 IS THE VALUE FOR TAU1=650.
      if (tau1.le.650.0d0) go to 15
      c(2)=1202.0d0
      tau=9.2315d0
10    continue
      c(1)=hss67(p,c(2),c(3),dens)
      dens=0.0160184634d0/dens
15    continue
      do i=1,8
         c(i)=do67(g(1),tau,in(i),id(i))
      enddo
      c(2)=c(2)/c(3)+1.135d-92*tau**107
      c(3)=c(4)/c(5)
      c(4)=c(6)/c(7)*tau
      c(5)=c(8)+do67(b(1),dens,1,6)*dens
      c(6)=do67(c(1),dens,1,4)
      condv67_result=c(5)+c(6)*dens*dens/tau**7
      if (tau1.gt.425.0d0) go to 25
      if (tau1.lt.350.0d0) go to 50
      psat=3208.234d0
      if (t.lt.705.47d0) psat=psl67(t)
      if (p.gt.psat) go to 45
25    continue
      if (tau1.le.650.0d0) go to 50
      if (c8.ne.0.0d0) go to 40
      if (tau1-700.0d0) 30,50,35
30    continue
      tau=9.7315d0
      c8=condv67_result
      c(2)=1292.0d0
      go to 10
35    continue
      condv67_result=c(8)+((103.51d0+tau1*(0.4198d0-2.771d-5*tau1))+2.1482d14*dens/tau1**4.2d0)*dens
      go to 50
40    continue
      condv67_result=c8+(condv67_result-c8)*(tau1-650.0d0)*0.020d0
      go to 50
45    continue
      if ( c(5) .lt. condv67_result)condv67_result=c(5)
50    continue
      condv67_result=condv67_result*5.777893d-4
!==================================================================================================================================!
      contains
!==================================================================================================================================!
function do67(a, x, istart,istop)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.

! ident_4="@(#) M_steam67 do67(3f)L summation of a polynomial"

!     part of procedure for solving nth order polynomial with a do loop
!       **     DOUBLE PRECISION FUNCTION IS CALLED TO CALCULATE  **
!       **     THE SUMMATION OF A DOUBLE PRECISION POLYNOMIAL.   **
integer,intent(in)         :: istop    ! final location in A array
doubleprecision,intent(in) :: a(istop) ! coefficients of array
doubleprecision,intent(in) :: x        ! multiplier on the coefficients
doubleprecision            :: do67
integer,intent(in)         :: istart   ! starting location in A array
integer                    :: l
integer                    :: k
   do67 = a(istart)
   l = istart + 1
   do k = l, istop
      do67 = do67*x + a(k)
   enddo
   end function do67
!==================================================================================================================================!
   end function condv67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     cpl67(3f) - [M_steam67] CPL67 calculates specific isobaric head capacity of water at pressure P and temperature T
!!
!!##SYNOPSIS
!!
!!   FUNCTION cpl67(PF,TF)
!!
!!##DESCRIPTION
!!
!!     DEFINITION AND PURPOSE:
!!        o CP = f(P,T)
!!        o Calculates specific isobaric heat capacity
!!
!!     REGION OF APPLICATION:
!!         Region 1 with P[max] = 1000 bars = 14,503.7738 psia.
!!         See Section 3.4.
!!
!!     CALLING SEQUENCE:
!!         CP = CPL(P,T)
!!##OPTIONS
function cpl67(pf,tf)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_5="@(#) M_steam67 cpl67(3f) calculates specific isobaric head capacity of water at pressure P and temperature T"

doubleprecision,intent(in)   :: pf    ! WATER PRESSURE, IN PSIA.
doubleprecision,intent(in)   :: tf    ! WATER TEMPERATURE, IN DEGREES F.
doubleprecision              :: cpl67 ! OUTPUT - HEAT CAPACITY IN BTU/LB-DEG. F.
doubleprecision :: c
doubleprecision :: c1
doubleprecision :: c2
doubleprecision :: dumo1
doubleprecision :: h
doubleprecision :: h1
integer         :: i
integer         :: ind
doubleprecision :: p
doubleprecision :: s
doubleprecision :: t
doubleprecision :: tsat
!external hcl67
save
dimension c(2)
      ind = 0
      t = tf
      p = pf
      tsat = 705.47d0
      if (p .lt. 3208.234d0)then
         tsat = tsl67(p)
      endif
      h = 2.0d0
      if (.not. (p .lt. 3000.0d0 .or. p .gt. 4350.0d0)) then
         if (.not. (t .lt. 657.0d0 .or. t .gt. 677.0d0)) then
            t = 657.0d0
            do i = 1, 2
               c(i)=(hcl67(p,t-2.0d0*h,s)-hcl67(p,t + 2.0d0*h,s)+ 8.0d0*(hcl67(p,t+h,s)-hcl67(p,t-h,s)))/(12.0d0*h)
               t = t + 20.0d0
            enddo
            cpl67 = c(1) + (tf - 657.0d0)*((c(2) - c(1))/20.0d0)
            goto 999
         endif
      endif
      if (p .gt. 4350.0d0 .and. t .gt. 657.0d0) then
         ind = 1
         t = 657.0d0
      else
         h1 = (tsat - t)/2.0d0
         if (h1 .lt. h) then
            h = h1
         endif
         if (h .lt. 0.125d0) then
            h = 0.075d0
            t = tsat - 0.25d0
         endif
      endif
      cpl67 = (hcl67(p, t - 2.0d0*h, s) - hcl67(p,t+2.0d0*h,s)+ 8.0d0*(hcl67(p, t + h, s) - hcl67(p,t-h,s)))/(12.0d0*h)
      if (ind .ne. 0) then
         c1 = cpl67
         c2 = cpv67(p, 707.0d0, dumo1)
         cpl67 = c1 + (tf - 657.0d0)*((c2 - c1)/50.0d0)
      endif
999   continue
      end function cpl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     cpv67(3f) - [M_steam67] calculates specific isobaric heat capacity and specific volume of water at pressure P and temperature T
!!##SYNOPSIS
!!
!!   FUNCTION cpv67(PF, TF, VF)
!!
!!##DESCRIPTION
!!
!!     DEFINITION AND PURPOSE:
!!        o CP,V = f(P,T)
!!        o Calculates specific isobaric heat capacity and specific volume.
!!
!!     REGION OF APPLICATION:
!!         Region 2 with P[max] = 1000 bars = 14,503.7738 psia. T[max] = 800 C =
!!         1472 F
!!         See Section 3.4.
!!
!!     CALLING SEQUENCE:
!!
!!         CP = CPV(P,T,V)
!!##OPTIONS
function cpv67(pf, tf, vf)
!  Copyright (c) 1969,1990 Westinghouse Electric Corporation.

! ident_6="@(#) M_steam67 cpv67(3f) calculates specific isobaric heat capacity and specific volume of water at pressure P and temperature T"

doubleprecision,intent(in)  :: pf    ! WATER PRESSURE, IN PSIA.
doubleprecision,intent(in)  :: tf    ! WATER TEMPERATURE, IN DEGREES F.
doubleprecision,intent(out) :: vf    ! SPECIFIC VOLUME OF WATER.
doubleprecision             :: cpv67 ! OUTPUT - HEAT CAPACITY IN BTU/LB-DEG. F.
doubleprecision :: h
doubleprecision :: h1
doubleprecision :: hq
doubleprecision :: p
doubleprecision :: s
doubleprecision :: t
doubleprecision :: tsat
doubleprecision :: v
doubleprecision :: v1
doubleprecision :: v2
   t = tf                               ! mutable copy of tf
   p = pf                               ! mutable copy of pf
   tsat = 705.47d0
   if (p .lt. 3208.234d0)then
      tsat = tsl67(p)
   endif
   h = 2.0d0
   h1 = (t - tsat)/2.0d0
   if (h1 .lt. h) then
      h = h1
   endif
   if (h .lt. 0.125d0) then
      h = 0.075d0
      t = tsat + 0.25d0
   endif
   cpv67=(hss67(p,t- 2.0d0*h,s,v)-hss67(p,t+2.0d0*h,s,v)+8.0d0*(hss67(p,t+h,s,v1)-hss67(p,t-h,s,v2)))/(12.0d0*h)
   vf = (v1 + v2)/2.0d0
   if (h .le. 2.0d0) then
      hq = hss67(p, tf, s, vf)
   endif
end function cpv67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     crflo67(3f) - [M_steam67] critical flow and degrees superheat of wet/superheated steam at PRES,ENTH
!!
!!##SYNOPSIS
!!
!!   function crflo67 (pres, enth, sht)
!!
!!    doubleprecision,intent(in)  :: pres
!!    doubleprecision,intent(in)  :: enth
!!    doubleprecision,intent(out) :: sht
!!    doubleprecision             :: crflo67
!!
!!##DESCRIPTION
!!    function crflo67(3f) will calculate the critical flow
!!    and degrees superheat of wet or superheated steam
!!    at pressure PRES and enthalpy ENTH.
!!
!!     DEFINITION AND PURPOSE:
!!        o FC,DEGS = f(P,H)
!!        o Calculates critical flow and degrees superheat of wet or
!!          superheated steam.
!!
!!     REGION OF APPLICATION:
!!
!!         See Section 3.11
!!
!!     CALLING SEQUENCE:
!!         FC=CRFLO(P,H,DEGS)
!!
!!##OPTIONS
!!    PRES     water pressure, in PSIA.
!!    ENTH     enthalpy, in BTU / POUND.
!!
!!##RETURN VALUES
!!    SHT      degrees superheat of steam.
!!    CRFLO67  critical flow level.
function crflo67 (pres,enth,sht)
! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS RESERVED

! ident_7="@(#) M_steam67 crflo67(3f) critical flow and degrees superheat of wet/superheated steam at PRES ENTH"

doubleprecision :: crflo67
doubleprecision :: a
doubleprecision :: ag
doubleprecision :: ag1
doubleprecision :: ak
doubleprecision :: ak1
doubleprecision :: amult
doubleprecision :: amult1
doubleprecision :: b
doubleprecision :: c
doubleprecision :: d
doubleprecision :: dum
doubleprecision :: enth
doubleprecision :: h
integer         :: i
integer         :: irtn
integer         :: j
integer         :: k
integer         :: l
doubleprecision :: p
doubleprecision :: p1
doubleprecision :: pres
doubleprecision :: s1
doubleprecision :: s2
doubleprecision :: s3
doubleprecision :: s4
doubleprecision :: sht
doubleprecision :: spt
doubleprecision :: x
doubleprecision :: x1
doubleprecision :: y
      save
      dimension a(14), b(20), c(75), d(35), p1(14)
doubleprecision :: v1
doubleprecision :: v2
doubleprecision :: v3
doubleprecision :: c1
doubleprecision :: c2
doubleprecision :: c3
doubleprecision :: ans
ans(v1,v2,v3,c1,c2,c3)=((c1-v2)*v1+(v2-c2)*v3)/c3
      data (a(i),i=1,14) /  &
     &    5.97809224d-5,    &
     &   -5.58149514d-4,    &
     &    2.97995844d-3,    &
     &   -5.43009676d-3,    &
     &   -1.48510729d-2,    &
     &   -1.56631735d0,     &
     &    5.87990177d1,     &
     &    3.44798320d-21,   &
     &   -3.34457869d-17,   &
     &    1.34235767d-13,   &
     &   -2.80928186d-10,   &
     &    3.40055866d-7,    &
     &   -1.97170007d-4,    &
     &    3.95196430d0 /

      data (b(i),i=1,20) /  &
     &   -6.0468064d-11,    &
     &    4.9558253d-10,    &
     &   -9.8507850d-10 ,   &
     &    7.1810415d-10,    &
     &   -1.6817220d-10,    &
     &    9.3741148d-7,     &
     &   -3.8989980d-6,     &
     &    5.8170388d-6,     &
     &   -3.6451604d-6,     &
     &    7.8992151d-7,     &
     &   -1.9209965d-3,     &
     &    6.7772179d-3,     &
     &   -8.8597097d-3,     &
     &    4.9530523d-3,     &
     &   -9.4994779d-4,     &
     &    0.61158138d0,     &
     &   -1.9601550d0,      &
     &    2.1925435d0,      &
     &   -0.46058803d0,     &
     &    0.61673715d0 /

      data (c(i),i=1,75) /  &
     &    0.0d0,            &
     &    4.8520220d-13,    &
     &   -1.0393010d-10,    &
     &    8.5657341d-9 ,    &
     &   -3.0598002d-7,     &
     &    4.4277849d-6,     &
     &    0.0d0,            &
     &   -9.6129475d-12,    &
     &    1.9568051d-9,     &
     &   -1.4002011d-7,     &
     &    4.0778587d-6,     &
     &   -4.0659471d-5,     &
     &    0.0d0,            &
     &    5.0823066d-11,    &
     &   -1.0294079d-8,     &
     &    6.9635260d-7,     &
     &   -1.7879717d-5,     &
     &    1.6061007d-4,     &
     &    0.0d0,            &
     &   -8.5443778d-11,    &
     &    2.0203379d-8,     &
     &   -1.4789595d-6,     &
     &    3.5296447d-5,     &
     &   -1.6499227d-4,     &
     &    1.0003208d0,      &
     &    5.2633418d-15,    &
     &   -1.3541176d-12,    &
     &    6.0918348d-10,    &
     &   -9.6148968d-8,     &
     &    5.1154831d-6,     &
     &    4.8340419d-5,     &
     &   -5.1991373d-13,    &
     &    8.4981863d-12,    &
     &    6.5866513d-9,     &
     &   -1.8173822d-8,     &
     &   -2.9997433d-5,     &
     &   -1.5728962d-3,     &
     &    6.9436287d-12,    &
     &   -4.4343193d-10,    &
     &   -7.9256493d-8,     &
     &    6.7046970d-6,     &
     &   -9.1497556d-5,     &
     &    1.2844753d-2,     &
     &   -1.5579361d-11,    &
     &    0.0d0,            &
     &    4.3873766d-7,     &
     &   -3.9988763d-5,     &
     &    1.2418648d-3,     &
     &   -4.2904469d-2,     &
     &    1.0845803d0,      &
     &    1.0934180d-15,    &
     &   -2.0921553d-13,    &
     &    4.8773090d-12,    &
     &    1.3618987d-9,     &
     &   -1.1193129d-7,     &
     &    3.1557330d-6,     &
     &   -5.5498755d-14,    &
     &    1.1479562d-11,    &
     &   -4.1127898d-10,    &
     &   -6.0077542d-8,     &
     &    5.6366756d-6,     &
     &   -1.6658398d-4,     &
     &    8.8612693d-13,    &
     &   -2.0689990d-10,    &
     &    1.0592549d-8,     &
     &    8.0040435d-7,     &
     &   -9.4453409d-5,     &
     &    3.0232322d-3,     &
     &   -1.1002016d-12,    &
     &    0.0d0,            &
     &    8.4390066d-8,     &
     &   -1.3765822d-5,     &
     &    8.5227853d-4,     &
     &   -2.2388905d-2,     &
     &    1.0260617d0 /

      data (p1(i),i=1,14) /         &
     &   20.0d0,         &
     &    0.0d0,         &
     &  200.0d0,         &
     &    0.0d0,         &
     &  500.0d0,         &
     &    0.0d0,         &
     & 2000.0d0,         &
     &    0.0d0,         &
     & 3000.0d0,         &
     &    0.0d0,         &
     & 4000.0d0,         &
     &    0.0d0,         &
     & 8000.0d0,         &
     &    0.0d0 /

      data (d(i),i=1,35) /   &
     &    2.07427760d-10,    &
     &   -1.27568986d-6,     &
     &    2.97012603d-3 ,    &
     &   -3.12927182d0,      &
     &    1.30577034d3,      &
     &    4.59003396d-11,    &
     &   -3.29797881d-7,     &
     &    9.00005908d-4,     &
     &   -1.12243584d0,      &
     &    5.78622686d2,      &
     &   -8.02713067d-13,    &
     &   -3.81487400d-8,     &
     &    2.18930161d-4,     &
     &   -4.17673826d-1,     &
     &    3.06051609d2,      &
     &    4.05868791d-12,    &
     &   -6.05488740d-8,     &
     &    2.52812636d-4,     &
     &   -4.34322248d-1,     &
     &    3.05939881d2,      &
     &    1.81570319d-11,    &
     &   -1.45915145d-7,     &
     &    4.46513926d-4,     &
     &   -6.29875206d-1,     &
     &    3.80269460d2,      &
     &    5.26123035d-11,    &
     &   -3.56234096d-7,     &
     &    9.26657804d-4,     &
     &   -1.11599895d0,      &
     &    5.64611398d2,      &
     &    6.16032603d-11,    &
     &   -4.05508530d-7,     &
     &    1.02931296d-3,     &
     &   -1.21388757d0,      &
     &    6.01441544d2 /
      p=pres
      h=enth
      if (p.gt.3208.234759d0) go to 1
      s1=sssiss67(p,h,x,s2,s3)
      spt=x-tsl67(p)
      go to 2
1     continue
      spt=400.d0
2     continue
      irtn=0
      if (spt.ge.85.d0) go to 20
      dum=log(p)
      j=1
      if (p.le.210.d0) go to 4
      irtn=1
3     continue
      j=8
      dum=p
4     continue
      ag=(((((a(j)*dum+a(j+1))*dum+a(j+2))*dum+a(j+3))*dum+a(j+4))*dum+a(j+5))*dum+a(j+6)
      if (j.eq.8) ag=exp(ag)
      if (p.gt.200.d0) if (irtn-1) 5,7,6
      go to 7
5     continue
      ag1=ag
      irtn=2
      go to 3
6     continue
      ag=ans(ag1,p,ag,210.d0,200.d0,10.d0)
7     continue
      irtn=0
      if (spt.ne.0.0d0) go to 9
      dum=sssiss67(p,h,s2,s3,x1)
      x=x1
      if (x1.gt.0.999d0) x=0.999d0
      s1=0.0d0
      s2=0.0d0
      s3=0.0d0
      s4=0.0d0
      do i=1,5
         s4=s4*x+b(i)
         s3=s3*x+b(i+5)
         s2=s2*x+b(i+10)
         s1=s1*x+b(i+15)
      enddo
      amult=((s4*p+s3)*p+s2)*p+s1
      if (x1.gt.0.999d0) amult=1.0d0-1000.0d0*(1.0d0-x1)*(1.0d0-amult)
      go to 19
9     continue
      x=spt
      if (spt.lt.15.d0) x=15.d0
      y=log(p)
      j=1
      if (p.lt.250.d0) go to 12
      irtn=2
      if (p.lt.1600.d0) go to 11
      irtn=4
10    continue
      j=51
      y=p/100.d0
      go to 12
11    continue
      j=26
12    continue
      s1=c(j+18)
      s2=0.0d0
      s3=0.0d0
      s4=0.0d0
      k=j+5
      do i=j,k
         s4=(s4+c(i))*x
         s3=(s3+c(i+6))*x
         s2=(s2+c(i+12))*x
         s1=s1*x+c(i+19)
      enddo
      amult=((s4*y+s3)*y+s2)*y+s1
      if (spt.le.15.0d0) amult=1.0d0+spt*(amult-1.0d0)/15.0d0
      if (irtn-1) 14,15,16
14    continue
      if (p.le.200.0d0) go to 19
      amult1=amult
      irtn=1
      go to 11
15    continue
      amult=ans(amult1,p,amult,250.0d0,200.0d0,50.0d0)
      go to 19
16    continue
      if (irtn-3) 17,18,19
17    continue
      if (p.le.1500.0d0) go to 19
      amult1=amult
      irtn=3
      go to 10
18    continue
      amult=ans(amult1,p,amult,1600.0d0,1500.0d0,100.0d0)
19    continue
      ak=ag/amult
      if (spt.le.80.0d0) go to 24
      irtn=1

20    continue

      do i=1,7
         k=i*5
         j=k-4
         p1(2*i)=0
         l=j
         do j=l,k
            p1(2*i)=p1(2*i)*h+d(j)
         enddo
      enddo

      ak1=p1(2)
      if (p.le.20.0d0) go to 22
      ak1=gr167(p1(1),p,7)
22    continue
      if (irtn.eq.1) go to 23
      ak=ak1
      go to 24
23    continue
      ak=ans(ak,spt,ak1,85.0d0,80.0d0,5.0d0)
24    continue
      crflo67=ak*p
      sht=spt
      end function crflo67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     critvs67(3f) - [M_steam67] function critvs67(press, enth, gamma)
!!
!!##SYNOPSIS
!!
!!   function critvs67(press, enth, gamma)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
function critvs67(press, enth, gamma)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_8="@(#) M_steam67 function critvs67(press enth gamma)"
! ident_9="@(#) M_steam67 entry critvw67(press enth gamma)"

doubleprecision :: critvs67
doubleprecision :: critvw67
doubleprecision :: crit
doubleprecision :: critvs
doubleprecision :: cs
doubleprecision :: cw
doubleprecision :: enth
doubleprecision :: gamma
doubleprecision :: h
integer         :: i
integer         :: j
integer         :: k
doubleprecision :: p
doubleprecision :: press
doubleprecision :: s1
doubleprecision :: s2
doubleprecision :: s3
doubleprecision :: s4
doubleprecision :: y
      save
      dimension cs(30), cw(30)
      data(cs(i), i = 1, 30)/                             &
     &     1.9543201d16,                                  &
     &    -2.4663243d13,                                  &
     &    -6.6345523d9,                                   &
     &     1.8028324d7,                                   &
     &    -5.6379104d3,                                   &
     &    -6.4190838d13,                                  &
     &     1.6488459d11,                                  &
     &    -1.5645112d8,                                   &
     &     6.5002598d4,                                   &
     &    -9.9704233d0,                                   &
     &     2.1373847d-10,                                 &
     &     1.4322008d-8,                                  &
     &    -3.8126955d-3,                                  &
     &     9.8978537d0,                                   &
     &    -5.1844383d3,                                   &
     &     4.1138795d10,                                  &
     &    -6.4237818d7,                                   &
     &     2.4093686d4,                                   &
     &    -4.7190851d11,                                  &
     &     7.7717936d8,                                   &
     &    -2.9882762d5,                                   &
     &     1.8214123d12,                                  &
     &    -2.9662983d9,                                   &
     &     1.1421810d6,                                   &
     &     7.7446464d-13,                                 &
     &    -2.7932978d-9,                                  &
     &     7.5494037d-7,                                  &
     &     6.8225967d-3,                                  &
     &    -6.0519557d0,                                   &
     &     1.6592308d3/
      data(cw(i), i = 1, 30)/                             &
     &    -4.9126433d-9,                                  &
     &     1.1415318d-5,                                  &
     &    -7.2185269d-3,                                  &
     &     6.2891516d-9,                                  &
     &    -8.5736185d-6,                                  &
     &     4.0630561d-3,                                  &
     &    -7.8818094d-8,                                  &
     &     1.6591108d-4,                                  &
     &    -5.7534295d-2,                                  &
     &     5.6040654d-12,                                 &
     &    -2.4938310d-8,                                  &
     &     4.4655419d-5,                                  &
     &    -4.05491d-2,                                    &
     &     1.9452968d1,                                   &
     &    -3.0340921d3,                                   &
     &     0.0d0,                                         &
     &     3.4946932d-5,                                  &
     &    -6.2922547d-2,                                  &
     &    -3.0219693d-7,                                  &
     &     6.0681572d-5,                                  &
     &     7.2433551d-1,                                  &
     &     3.0382206d-6,                                  &
     &    -3.0399779d-3,                                  &
     &    -2.8231954d0,                                   &
     &     2.7379195d-11,                                 &
     &    -1.3186293d-7,                                  &
     &     2.4534322d-4,                                  &
     &    -2.3294188d-1,                                  &
     &     1.2113702d2,                                   &
     &    -2.2141160d4/
      p = press
      h = enth
      if (p .lt. 1100.0d0) then
         s1 = 0.0d0
         s2 = 0.0d0
         s3 = 0.0d0
         do i = 1, 5
            s2 = (s2 + cs(i))/h
            s3 = (s3 + cs(i + 5))/h
            s1 = s1*h + cs(i + 10)
         enddo
         crit = (s3*p + s2)*p + s1
         critvs67 = crit
         if (p .le. 1000.0d0) goto 7
      endif
      if (p .gt. 8000.0d0) then
         call ster67('CRITVS67', 13, press, enth)
         goto 9
      else
         s2 = 0.0d0
         s3 = 0.0d0
         s4 = 0.0d0
         s1 = (cs(25)*h + cs(26))*h + cs(27)
         do i = 1, 3
            s4 = (s4 + cs(i + 15))/h
            s3 = (s3 + cs(i + 18))/h
            s2 = (s2 + cs(i + 21))/h
            s1 = s1*h + cs(i + 27)
         enddo
         y = p/1000.0d0
         critvs67 = ((s4*y + s3)*y + s2)*y + s1
         if(p.lt.1100.0d0)critvs67=((1100.0d0-p)*crit+(p-1000.0d0) *critvs67)/100.0d0
         goto 7
      endif
!==================================================================================================================================!
!>
!!##NAME
!!     critvw67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!       entry critvw67(press, enth, gamma)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
      entry critvw67(press, enth, gamma)
      h = enth
      cw(16) = 4.8121071d-95*h**27
      p = press
      y = log(p)
      i = 1
      if (p .lt. 225.0d0) goto 5
4     continue
      crit = critvs
      i = 16
5     continue
      j = i + 2
      s1 = (cw(i + 9)*h + cw(i + 10))*h + cw(i + 11)
      s2 = 0.0d0
      s3 = 0.0d0
      s4 = 0.0d0
      do k = i, j
         s4 = (s4 + cw(k))*h
         s3 = (s3 + cw(k + 3))*h
         s2 = (s2 + cw(k + 6))*h
         s1 = s1*h + cw(k + 12)
      enddo
      critvs = ((s4*y + s3)*y + s2)*y + s1
      if (p .ge. 225.0d0 .or. p .le. 200.0d0) goto 7
      if (i .eq. 1) goto 4
      critvs = ((225.0d0 - p)*crit + (p - 200.0d0)*critvs)/25.0d0
7     continue
      gamma = 0.0d0
9     continue
      end function critvs67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     crvel67(3f) - [M_steam67] critical flow velocity and isentropic exponent of wet/superheated steam
!!##SYNOPSIS
!!
!!   function crvel67(p, h, ga)
!!
!!    doubleprecision,intent(in)  :: p
!!    doubleprecision,intent(in)  :: h
!!    doubleprecision,intent(out) :: ga
!!    doubleprecision             :: crvel67
!!
!!##DESCRIPTION
!!    Function crvel67 will calculate the critical flow
!!    velocity and isentropic exponent of wet of super
!!    heated steam at pressure P amd enthalpy H.
!!
!!     DEFINITION AND PURPOSE:
!!        o VC,GAMMA=f(P,H)
!!        o Calculates critical velocity and isentropic exponents (GAMMA) of
!!          wet or superheated steam. GAMMA is defined such that PV**GAMMA = C.
!!
!!     REGION OF APPLICATION:
!!
!!         See Section 3.12
!!
!!     CALLING SEQUENCE:
!!         VC = CRVEL(P,H,GAMMA)
!!
!!##OPTIONS
!!
!!  P         water pressure, in psia.
!!  H         enthalpy, in BTU/pound.
!!
!!##RETURN VALUES
!!
!!  GA        isentropic exponent.
!!  CRVEL67   critical flow velocity.
function crvel67(p, h, ga)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_10="@(#) M_steam67 critical flow(3f) velocity and isentropic exponent of wet/superheated steam"

doubleprecision :: crvel67
doubleprecision :: a
doubleprecision :: ddh
doubleprecision :: dp
doubleprecision :: dpdt
doubleprecision :: ga
doubleprecision :: h
integer         :: i
doubleprecision :: p
doubleprecision :: pk
doubleprecision :: psat
doubleprecision :: s
doubleprecision :: score
doubleprecision :: t
doubleprecision :: tcore
doubleprecision :: v
doubleprecision :: vcore
doubleprecision :: x
save
common /nust/tcore, score, vcore, dpdt
dimension pk(9), v(9), a(10)
data(a(i), i = 1, 10)/                              &
     &   -2.083333333d0,                              &
     &    4.0d0,                              &
     &   -3.0d0,                              &
     &    1.33333333d0,                              &
     &   -0.25d0,                              &
     &   -0.83333333d0,                              &
     &    1.5d0,                              &
     &   -0.5d0,                              &
     &    0.083333333d0,                              &
     &   -0.66666667d0 /
      s = sisiss67(p, h, t, v(1), x)
      psat = 0
      dp = .01d0
      if (s .eq. 1.0618559d0) then
         psat = 3208.234759d0
      else
         if (s .gt. 1.0618559d0) then
            psat = psv67(s)
         else
            psat = pls67(s)
            ddh = .027d0
            if (p .lt. 50.0d0) ddh = .0027d0
            if (p .lt. 8000.0d0) dp = max(.02d0, ddh/p/v(1))
            if ((p-4.0d0*p*dp).lt.0.0886d0)dp=(p-0.0887d0)/4.0d0/p
         endif
         if (p .gt. 2500.0d0 .and. h .gt. 1114.0d0)psat=p23t67(t)
      endif
      do i = 1, 9
         pk(i) = p + dp*dble(i - 5)*p
         x = hisiss67(pk(i), s, t, v(i), ga)
      enddo
      if(pk(5).gt.(200.0d0-4.0d0*dp).and.pk(5).le.(200.0d0+4.0d0*dp)) then
         if (psat .gt. 200.0d0 .and. pk(5) .gt. 200.0d0) then
            if (pk(9) .gt. psat) goto 7
         elseif (.not. (psat .gt. 200.0d0 .and. pk(5) .le. 200.0d0))then
            if (psat .le. pk(5)) then
               psat = pk(5)*0.9999999d0
               goto 7
            endif
         endif
         psat = pk(5)*1.00000001d0
      endif
7     continue
      x = 0
!     pk(1) equals term of old routine
      pk(1) = dp/v(5)
      if (p .lt. 3208.234759d0) then
         if (pk(5) .ne. psat) then
            if (pk(5) .gt. psat) then
               if (pk(3) .ge. psat) goto 10
               if (pk(4) .lt. psat) goto 16
            elseif (pk(7) .ge. psat) then
               pk(1) =  - pk(1)
               t = v(6)
               v(6) = v(4)
               v(7) = v(3)
               v(8) = v(2)
               if (pk(6) .ge. psat) then
                  v(9) = v(1)
                  goto 16
               else
                  v(4) = t
               endif
            else
               goto 10
            endif
            do i = 5, 9
               x = x + a(i)/v(i - 1)
            enddo
            goto 18
         endif
16       continue
         do i = 1, 5
            x = x + a(i)/v(i + 4)
         enddo
         goto 18
      endif
10    continue
      x = a(9)/v(3) - a(9)/v(7) + a(10)/v(4) - a(10)/v(6)
18    continue
      ga = pk(1)/x
      crvel67 = sqrt(4633.06d0*ga*p*v(5))
end function crvel67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     hcsl67(3f) - [M_steam67] calculate specific enthalpy/volume and entropy of liquid(P,T)
!!##SYNOPSIS
!!
!!   function hcsl67 (p,t,v,s,igo)
!!
!!##DESCRIPTION
!!    function hcsl67 will calculate specific enthalpy,
!!    specific volume, and entropy of the liquid at
!!    pressure P and temperature T.
!!
!!##OPTIONS
!!     P        water pressure, in PSIA.
!!     T        water temperature, in degrees F.
!!     IGO      flag indicating calling routine.
!!
!!                (=1) HSL67.  (=2) SSL67.  (=3) VSL67.
!!                (=4) HCL67.  (=5) VCL.    (=6) DIR.
!!
!!##RETURN VALUES
!!     V        specific volume.
!!     S        specific entropy, in BTU/LB-DEGF
!!     hcsl67   specific enthalpy, in BTU/LB.
function hcsl67 (p,t,v,s,igo)
! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS

! ident_11="@(#) M_steam67 hcsl67(3f) calculate specific enthalpy/volume and entropy of liquid(P T)"

doubleprecision :: hcsl67
integer         :: i
integer         :: igo
doubleprecision :: a
doubleprecision :: b2
doubleprecision :: b3
doubleprecision :: beta
doubleprecision :: p
doubleprecision :: q1
doubleprecision :: q2
doubleprecision :: r
doubleprecision :: r1
doubleprecision :: r2
doubleprecision :: r3
doubleprecision :: s
doubleprecision :: sa
doubleprecision :: sum
doubleprecision :: t
doubleprecision :: theta
doubleprecision :: v
doubleprecision :: x
doubleprecision :: y
doubleprecision :: ypri
doubleprecision :: z
doubleprecision :: z1
!external STER67, GOTOER67
save
!     9.1 SUB-REGION 1
!     ENTERED FROM HSL67,SSL67,VSL67,HCL67,VCL WHEN IGO =1,2,3,4,5 RESPECTIVELY
!     CALLED DIRECTLY WHEN IGO=6
dimension theta(16), a(23), sa(12)
data (a(i),i=1,23) /          &
     &    6.824687741d3,            &
     &   -5.422063673d2,            &
     &   -2.096666205d4 ,           &
     &    3.941286787d4,            &
     &   -6.733277739d4,            &
     &    9.902381028d4,            &
     &   -1.093911774d5,            &
     &    8.590841667d4,            &
     &   -4.511168742d4,            &
     &    1.418138926d4,            &
     &   -2.017271113d3,            &
     &    7.982692717d0,            &
     &   -2.616571843d-2,           &
     &    1.522411790d-3,           &
     &    2.284279054d-2,           &
     &    2.421647003d2,            &
     &    1.269716088d-10,          &
     &    2.074838328d-7,           &
     &    2.174020350d-8,           &
     &    1.105710498d-9,           &
     &    1.293441934d1,            &
     &    1.308119072d-5,           &
     &    6.047626338d-14 /
      data (sa(i),i=1,12) /         &
     &    0.8438375405d0,           &
     &    5.362162162d-4,           &
     &    1.720000000d0,            &
     &    0.07342278489d0,          &
     &    0.04975858870d0,          &
     &    0.6537154300d0,           &
     &    1.150000000d-6,           &
     &    1.510800000d-5,           &
     &    0.1418800000d0,           &
     &    7.002753165d0,            &
     &    2.995284926d-4,           &
     &    0.2040000000d0 /
      if (t.gt.705.47d0) go to 13
      if (t.gt.662.0d0) go to 10
!-----------------------------------------------------------------------
!     THETA   =((T-32.0d0)/1.8d0+273.15d0)/647.3d0
      theta(1)=8.582659595d-4*t+0.3945191136d0
      beta=p/3208.234759d0
!     X AND R ARE BOTH USED
      x=sa(6)-theta(1)
      r=x**9
!     THIS DO LOOP GIVES THETA**2 THRU THETA**11
      do i=2,11
         theta(i)=theta(i-1)*theta(1)
      enddo
!     THETA(12)=THETA**17
      theta(12)=theta(8)*theta(9)
!     THIS DO LOOP GIVES THETA**18 THRU THETA**21
      do i=13,16
         theta(i)=theta(i-1)*theta(1)
      enddo
      b2=beta**2
      b3=b2*beta
      q2=(sa(10)+beta)**4
      q1=(sa(10)+beta)/q2
      y=1.0d0-sa(1)*theta(2)-sa(2)/theta(6)
      z=y+sqrt(sa(3)*y**2-2.0d0*(sa(4)*theta(1)-sa(5)*beta))
!     Z1=Z**(5.d0/17.d0)
      z1=z**0.2941176471d0
      r1=sa(7)+theta(14)
      r2=sa(8)+theta(11)
      r3=(((a(20)*beta+a(19))*beta+a(18))*beta)/r2**2
      ypri=-2.0d0*sa(1)*theta(1)+6.0d0*sa(2)/theta(7)
!-----------------------------------------------------------------------
      go to (8,5,3,5,3,3), igo
!-----------------------------------------------------------------------
      call gotoer67( 'hcsl67 (A), IGO',igo)
!     SUB"REGION 1 REDUCED VOLUME SUBSCRIPT OF A IS ONE MORE
!     THAN REPORT SUBSCRIPT NUMBER
3     continue
      v=(a(12)*sa(5)/z1+a(13)+a(14)* theta(1)+a(15)* theta(2)+a(16)*r*x+a(17)/r1-(a(18)+2.0d0*a(19)* &
     & beta+3.0d0*a(20)*b2)/r2)*5.077852889d-2
      if (t.le.382.d0) go to 4
!-----------------------------------------------------------------------
      v=v+(-a(21)*theta(13)*(sa(9)+theta(2))*(-3.0d0/q2+sa(11))         &
     &+3.0d0*a(22)*(sa(12)-theta(1))*b2+4.0d0*a(23)/theta(15)*b3)         &
     &*5.077852889d-2
!     SUB"REGION 1 REDUCED ENTROPY
!-----------------------------------------------------------------------
4     continue
      go to (11,11,12,11,12,5), igo
!-----------------------------------------------------------------------
      call gotoer67( 'hcsl67 (B), IGO',igo)
5     continue
      sum=a(3)
      do i=1,8
         sum=sum+dble(i+1)*a(i+3)*theta(i)
      enddo
      s=(1.583237108d-6+a(1)*log(theta(1))-sum+a(12)/z1*((0.4166666667d0         &
     &* z-0.720d0*y)*ypri+sa(4))+beta*(-a(14)-2*a(15)*theta(1)+10.0d0         &
     &*a(16)*r+ 19.0d0*a(17)/r1**2*theta(13))-11.0d0*theta(10)*r3)         &
     &*2.587358228d-2
!-----------------------------------------------------------------------
      if (t.le.382.d0) go to 7
!-----------------------------------------------------------------------
      s=s+(a(21)*theta(12)*(18.0d0*sa(9)+20.d0*theta(2))*(q1+sa(11)*beta)+b3*(a(22)+20.0d0*a(23)/theta(16)*beta))*2.587358228d-2
!     SUB"REGION 1 REDUCED ENTHALPY
!-----------------------------------------------------------------------
7     continue
      go to (11,12,11,8,11,8), igo
!-----------------------------------------------------------------------
      call gotoer67( 'hcsl67 (C), IGO',igo)
8     continue
      sum=-a(2)
!     SECOND TERM OMITTED SINCE IT = 0
      do i=2,9
         sum=sum+dble(i-1)*a(i+2)*theta(i)
      enddo
      hcsl67=                                                               &
     &(2.156561703d-7+a(1)*theta(1)-sum+a(12)/z1*(z*(0.5862068965d0         &
     &*z -1.416666667d0*y+0.4166666667d0*theta(1)*ypri)+sa(4)*theta(1)      &
     &-0.720d0*theta(1)*y*ypri)+beta*(a(13)-a(15)*theta(2)+a(16)*(9.0d0     &
     &*theta(1)+sa(6))*r+a(17)*(20.0d0*theta(14)+sa(7))/r1**2)-(12.0d0      &
     &*theta(11)+sa(8))*r3)*30.14634566d0
!-----------------------------------------------------------------------
      if (t.le.382.0d0) go to 12
!-----------------------------------------------------------------------
      hcsl67=hcsl67+(a(21)*theta(13)*(17.0d0*sa(9)+19.0d0*theta(2))*        &
     &(q1+sa(11)*beta)+(a(22)*sa(12)+21.0d0*a(23)/theta(15)*beta)*b2)       &
     &*30.14634566d0
!-----------------------------------------------------------------------
      go to 12
!-----------------------------------------------------------------------
10    continue
      hcsl67=hcslv167(p,t,v,s,igo)
      go to 12
!-----------------------------------------------------------------------
11    continue
      print 14,igo
      stop
!-----------------------------------------------------------------------
12    continue
      return
!-----------------------------------------------------------------------
13    call ster67('hcsl67',2,t,0.0d0)
! never called. STER67() stops, but programming tools complain hcsl67 undefined
      stop
!-----------------------------------------------------------------------
!
14    format (10x,'WRONG STATEMENT IN hcsl67,IGO=',i3)
!-----------------------------------------------------------------------
      end function hcsl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     hcslvl67(3f) - [M_steam67] calculate V,S,specific enthalpy for liquid given (P,T)
!!##SYNOPSIS
!!
!!   function hcslv167(p, t, v, s, igo)
!!##DESCRIPTION
!!
!!    function hcslv167 will calculate specific enthalpy,
!!    specific volume, and entropy of the liquid at
!!    pressure p and temperature t.
!!
!!##OPTIONS
!!     P     water pressure, in PSIA.
!!     T     water temperature, in degrees F.
!!     IGO   flag indicating calling routine.
!!
!!            (=1) HSL67.  (=2) SSL67.  (=3) VSL67.
!!            (=4) HCL67.  (=5) VCL.    (=6) DIR.
!!##RETURN VALUES
!!     V          specific volume.
!!     S          specific entropy, in BTU/LB-DEGF
!!     hcslv167   specific enthalpy, in BTU/LB.
function hcslv167(p, t, v, s, igo)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_12="@(#) M_steam67 hcslvl67(3f) calculate V S specific enthalpy for liquid given (P T)"
! ident_13="@(#) M_steam67 hcslv267(3f) calculate specific enthalpy specific volume and entropy of the liquid at P and T"

doubleprecision :: hcslv167
doubleprecision :: hcslv267
doubleprecision :: b
doubleprecision :: beta
doubleprecision :: br
doubleprecision :: c0
doubleprecision :: c1
doubleprecision :: c2
doubleprecision :: c3
doubleprecision :: c40
doubleprecision :: c41
doubleprecision :: c50
doubleprecision :: c6
doubleprecision :: c7
doubleprecision :: d3
doubleprecision :: d4
doubleprecision :: d50
doubleprecision :: d51
doubleprecision :: d52
doubleprecision :: df
doubleprecision :: dv
doubleprecision :: g
integer         :: i
integer         :: igo
integer         :: int
integer         :: k
integer         :: nt
doubleprecision :: p
doubleprecision :: pb
doubleprecision :: pct
doubleprecision :: pctp
doubleprecision :: pprev
doubleprecision :: q
doubleprecision :: r
doubleprecision :: s
doubleprecision :: sum
doubleprecision :: sum1
doubleprecision :: sum2
doubleprecision :: sum3
doubleprecision :: t
doubleprecision :: tc
doubleprecision :: theta
doubleprecision :: tol
doubleprecision :: v
doubleprecision :: vmax
doubleprecision :: vmin
doubleprecision :: vprev
doubleprecision :: vtry
doubleprecision :: x
doubleprecision :: y
!external STER67, GOTOER67
save
!     entered from HSL67,SSL67,VSL67,HCL67,vcl when igo =1,2,3,4,5 respectively
!     called directly when igo=6
!-ju  theta dimensioned to 10 instead of 9 --- code not corrected
dimension sum(16), theta(10), q(9), x(9), c0(12), c1(10), c2(10), c3(10), c6(9), c7(9)
dimension vtry(3), pct(3)
dimension y(5), d3(5), d4(5)
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
!-ju  theta(10) set to 1 and assumed static (saved)
data theta(10)/1.0d0/
data c40/2.759717760d-6/

data(c0(i), i = 1, 12)/               &
     &   -0.01722604200d0,                  &
     &   -7.771750390d0,                    &
     &    4.204607520d0,                    &
     &   -2.768070380d0,                    &
     &    2.104197070d0,                    &
     &   -1.146495880d0,                    &
     &    0.2231380850d0,                   &
     &    0.1162503630d0,                   &
     &   -0.08209005440d0,                  &
     &    0.01941292390d0,                  &
     &   -1.694705760d-3,                   &
     &   -4.311577033d0/

data(c1(i),                           &
     &  i = 1, 10)/                         &
     &    0.7086360850d0,                   &
     &   12.36794550d0,                     &
     &  -12.03890040d0,                     &
     &    5.404374220d0,                    &
     &   -0.9938650430d0,                   &
     &    0.06275231820d0,                  &
     &    0.0d0,                            &
     &    0.0d0,                            &
     &    0.0d0,                            &
     &   -7.747430160d0/

data(c2(i), i = 1, 10)/               &
     &   -4.298850920d0,                    &
     &   43.14305380d0,                     &
     &  -14.16193130d0,                     &
     &    4.041724590d0,                    &
     &    1.555463260d0,                    &
     &   -1.665689350d0,                    &
     &    0.3248811580d0,                   &
     &    0.0d0,                            &
     &    0.0d0,                            &
     &   29.36553250d0/

data(c3(i), i = 1, 10)/               &
     &    7.948418420d-6,                   &
     &   80.88597470d0,                     &
     &  -83.61533800d0,                     &
     &   35.86365170d0,                     &
     &    7.518959540d0,                    &
     &  -12.61606400d0,                     &
     &    1.097174620d0,                    &
     &    2.121454920d0,                    &
     &   -0.5465295660d0,                   &
     &    8.328754130d0/

data(c6(i), i = 1, 9)/                &
     &    0.05528935335d0,                  &
     &   -0.2336365955d0,                   &
     &    0.3697071420d0,                   &
     &   -0.2596415470d0,                   &
     &    0.06828087013d0,                  &
     &    0.0d0,                            &
     &    0.0d0,                            &
     &    0.0d0,                            &
     &    0.0d0/

data(c7(i), i = 1, 9)/                &
     & -257.1600553d0,                      &
     & -151.8783715d0,                      &
     &   22.20723208d0,                     &
     & -180.2039570d0,                      &
     & 2357.096220d0,                       &
     &   -1.462335698d4,                    &
     &    4.542916630d4,                    &
     &   -7.053556432d4,                    &
     &    4.381571428d4/

data c41/-5.090739850d-4/

data c50/2.106363320d2/

data(d3(i), i = 1, 5)/                &
     &   -1.717616747d0,                    &
     &    3.526389875d0,                    &
     &   -2.690899373d0,                    &
     &    0.9070982605d0,                   &
     &   -0.1138791156d0/

data(d4(i), i = 1, 5)/                &
     &    1.301023613d0,                    &
     &   -2.642777743d0,                    &
     &    1.996765362d0,                    &
     &   -0.6661557013d0,                   &
     &    0.08270860589d0/

data d50/3.426663535d-4/

data d51/-1.236521258d-3/

data d52/1.155018309d-3/

data g/80.4099390d0/

data r/107.2132505d0/
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
!     sub-region 3and 4
      int = 1
!      remove calc. for pb and tc when vliq67 is made part of prog.
      pb = p/14.503773773d0
      tc = (t - 32.0d0)/1.8d0
!     v from vliq67 is in cc/gm
      vtry(1) = vliq67(pb, tc, vmin, vmax)
      vmax = 3.17d0
      tol = 3.0d-8
      goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     hcslv267(3f) - [M_steam67] calculate specific enthalpy, specific volume, and entropy of the liquid at P and T
!!
!!##SYNOPSIS
!!
!!   entry hcslv267(P, T, v, s, igo)
!!
!!##DESCRIPTION
!!
!!    function hcslv267 will calculate specific enthalpy,
!!    specific volume, and entropy of the liquid at
!!    pressure P and temperature T.
!!
!!##OPTIONS
!!   P      water pressure, in PSIA.
!!   T      water temperature, in degrees F.
!!   IGO    flag indicating calling routine.
!!
!!##RETURN VALUES
!!   V          specific volume.
!!   S          specific entropy, in BTU/LB-DEGF
!!   hcslv167   specific enthalpy, in BTU/LB.
      entry hcslv267(p, t, v, s, igo)
      int = 0
!     v from vest67 is in cc/gm
      call vest67(p, t, vtry(1))
      vmax = 8.9d0
      vmin = 1.28d0
      tol = 5.0d-9
1     continue
      k = 1
      nt = 1
      beta = p/3208.234759d0
!     theta   =((t-32.0d0)/1.8d0+273.15d0)/647.3d0
      theta(1) = 8.582659595d-4*t + 0.3945191136d0
      df = 0.25d0
      pctp = 0
2     continue
      if (vtry(1) .gt. vmax) then
         vtry(1) = vmax
         df = 1.0d0
      elseif (vtry(1) .lt. vmin) then
         vtry(1) = vmin
         df = 1.0d0
      endif
!     set-up for sub-region 3 paragraph 9.3 for pressure calculation
      x(1) = vtry(k)/3.17d0
      do i = 1, 16
         sum(i) = 0
      enddo
      q(1) = theta(1) - 1.0d0
      do i = 2, 9
         q(i) = q(i - 1)*q(1)
         x(i) = x(i - 1)*x(1)
         theta(i) = theta(i - 1)*theta(1)
         b = dble(1 - i)/x(i)
         sum(1) = sum(1) + b*c0(i)
         sum(2) = sum(2) + b*c1(i)
         sum(3) = sum(3) + b*c2(i)
         sum(4) = sum(4) + b*c3(i)
         sum(5) = sum(5) + c6(i - 1)/theta(i)
      enddo
!     the following are the last two terms of sum(1)
      sum(1) = sum(1) - (9.0d0*c0(10)/x(1) + 10.0d0*c0(11)/x(2))/x(9)
!     the following are theta**22 and theta**23
      theta(8) = theta(7)*theta(7)*theta(7)*theta(1)
      theta(9) = theta(8)*theta(1)
!     set-up for sub-region 4 paragraph 9.4
      if (int .eq. 0) then
         br = 0.0d0
         s = 0.0d0
         hcslv167 = 0.0d0
      else
         y(1) = (1.0d0 - theta(1))/3.730882125d-2
         sum1 = 0
         sum2 = 3.0d0*d3(1) + 4.0d0*d4(1)*y(1)
         sum3 = d3(1)*(-2.0d0*y(1) + g) + d4(1)*( - 3.0d0*y(1) + r)*y(1)
         do i = 2, 5
            y(i) = y(i - 1)*y(1)
            sum1 = sum1 + (dble(i) - 1.0d0)*(d3(i) + y(1)*d4(i))/x(i)
            sum2 = sum2 + (3.0d0*d3(i) + 4.0d0*d4(i)*y(1))/x(i - 1)
            sum3 = sum3 + (d3(i)*(dble(i - 3)*y(1) + g) + d4(i)* (dble(i - 4)*y(1) + r)*y(1))/x(i - 1)
         enddo
         sum1 = sum1*y(3)
         sum2 = sum2*y(2)
         sum3 = sum3*y(2)
         y(5) = y(5)**6
!     p from sub-region 4 paragraph 9.4
         br = sum1 - y(5)*y(2)*(d51 + 2.0d0*d52*x(1))
!     h from sub-region 4 paragraph 9.4
         hcslv167 = sum3-y(5)*y(1)*((31.0d0*d50+32.0d0*d51*x(1)+33.0d0 *d52*x(2))*y(1) - (d50 + d51*x(1) + d52*x(2))*857.7060043d0)
!     s from sub-region 4 paragraph 9.4
         s =  + (sum2 + 32.0d0*y(5)*y(1)*(d50 + d51*x(1) + d52*x(2))) /3.730882125d-2
      endif
!     p from sub-region 3 paragraph 9.3
      br = ( - c0(1) - sum(1) - c0(12)/x(1) - (c1(1) + sum(2) + c1(10)     &
     & /x(1))*q(1) - (c2(1) + sum(3) + c2(10)/x(1))*q(2) - (c3(1) + sum    &
     & (4)+c3(10)/x(1))*q(3) + 5.0d0*c41/x(6)*q(1)/theta(9) - 6.0d0*x(5)    &
     & *sum(5) + br)*3208.234759d0
      pct(k) = (br - p)/p
      if (abs(pct(k)) .le. tol) then
!     replace this conversion if vliq67 is changed to use engrg. units
         v = vtry(k)/62.4279605761d0
         sum(10) = c7(1)
         sum(16) = c7(1)
         do i = 2, 9
            sum(6) = sum(6) + c1(i)/x(i - 1)
            sum(7) = sum(7) + c2(i)/x(i - 1)
            sum(8) = sum(8) + c3(i)/x(i - 1)
!-ju
!-ju  this causes theta(10) to be used when i=9, but theta only 9 big,
!-ju  but c6(8) is zero so theta(10) was created and set to 1. authors
!-ju  assumed 0/anything was 0, which is not safe (0/0, 0/indefinite).
!-ju
            sum(9) = sum(9) + dble(i)*c6(i - 1)/theta(i + 1)
            sum(10) = sum(10) + dble(i)*c7(i)*q(i - 1)
            b = dble(i)
            sum(11) = sum(11) + b*c0(i)/x(i - 1)
            sum(12) = sum(12) + (b - 1.0d0)*c1(i)/x(i - 1)
            sum(13) = sum(13) + (b - 2.0d0)*c2(i)/x(i - 1)
            sum(14) = sum(14) + (b - 3.0d0)*c3(i)/x(i - 1)
            sum(15) = sum(15) + (b - 5.0d0)*c6(i - 1)/theta(i)
            sum(16)=sum(16)+ c7(i)*(1.0d0 + (b - 1.0d0)*theta(1))*q(i-1)
         enddo
         sum(11) = sum(11) + (10.0d0*c0(10) + 11.0d0*c0(11)/x(1))/x(9)
         if (igo .eq. 1 .or. igo .eq. 4 .or. igo .eq. 6) goto 18
         if (igo .eq. 2) goto 19
         if (igo .eq. 3 .or. igo .eq. 5) goto 21
         call gotoer67('hcslv167 (A), IGO', igo)
      endif
      if (nt .gt. 1000) goto 20
!---------------------------------------------------------------------------------------
      if (nt .ge. 2) then
         if (pctp*pct(1) .gt. 0) then
            if (abs(pct(1)) .gt. abs(0.3d0*pctp)) then
               df = 1.5d0*df
               goto 16
            endif
         endif
         df = 0.67d0*df
         dv = (vtry(1) - vprev)*(p - br)/(br - pprev)
         goto 17
      endif
16    continue
      dv = vtry(1)*pct(1)*df
17    continue
!---------------------------------------------------------------------------------------
      vprev = vtry(1)
      pprev = br
      pctp = pct(1)
      vtry(1) = vtry(1) + dv
      nt = nt + 1
      goto 2
!     h from sub-region 3 paragraph 9.3
18    continue
      hcslv167 =     &
     & ( - 213.164655d0 - c1(1)*x(1) + sum(11) - sum(6) + (c0(12)             &
     & - c1(10))*log(x(1))+q(1)*( - c1(10) - c50 - (c1(1) + 2.0d0*c2(1))      &
     & *x(1)+sum(12) - 2.0d0*(sum(7) + c2(10)*log(x(1)))) + q(2)*( - c2       &
     & (10)-(2.0d0*c2(1)+3.0d0*c3(1))*x(1)+sum(13) - 3.0d0*sum(8) - (c2       &
     & (10) + 3.0d0*c3(10))*log(x(1))) + q(3)*( - 3.0d0*c3(1)*x(1) + sum      &
     & (14) - c3(10)*(1.0d0+2.0d0*log(x(1))))+(c40*(23.0d0- 24.0d0/theta      &
     & (1))-c41*(-28.0d0+ 29.0d0/theta(1))/x(5))/theta(8) + x(6)*sum(15)      &
     & - sum(16) + hcslv167)*30.14634566d0
      if ((igo .ge. 1 .and. igo .le. 3) .or. igo .eq. 5) goto 21
      if (.not. (igo .eq. 4 .or. igo .eq. 6)) call gotoer67('hcslv167 (B), IGO', igo)
!     s from sub-region 3 paragraph 9.3
19    continue
      s = (1.583237108d-6 - c1(1)*x(1) - sum(6) - c1(10)*log(x(1)) -          &
     & c50-2.0d0*q(1)*(c2(1)*x(1) + sum(7) + c2(10)*log(x(1))) - 3.0d0*q      &
     & (2)*(c3(1)*x(1) + sum(8) + c3(10)*log(x(1))) + (c40 + c41/x(5))*       &
     & (22.0d0-23.0d0/theta(1))/theta(9)-c50*log(theta(1)) + x(6)*sum(9)      &
     & - sum(10) + s)*2.587358228d-2
      goto 21
20    continue
      call ster67('hcslv167', -12, p, t)
21    continue
      end function hcslv167
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     vest67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   subroutine vest67(pbars, tc, vout)
!!
!!##DESCRIPTION
!!      SUBROUTINE vest67 WILL CALCULATE
!!
!!##OPTIONS
!!     PBARS    water pressure, in atmospheres.  **
!!     TC       water temperature, in degrees C. **
!!
!!##RETURN VALUES
!!     VOUT     output
subroutine vest67(pbars, tc, vout)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

doubleprecision :: a
doubleprecision :: b
integer         :: i
doubleprecision :: p
doubleprecision :: pbars
doubleprecision :: s1
doubleprecision :: s2
doubleprecision :: s3
doubleprecision :: t
doubleprecision :: tc
doubleprecision :: vout
save
dimension a(10), b(11)
data(a(i), i = 1, 10)/    &
     &    -1.1081080d1,         &
     &     3.2788736d-2,        &
     &    -1.1560893d-59,       &
     &    -3.0184685d-8,        &
     &     7.8976204d1,         &
     &    -2.4914446d-1,        &
     &     1.9435772d-4,        &
     &    -3.8913564d4,         &
     &     1.1542136d2,         &
     &    -8.4902827d-2/
      data(b(i), i = 1, 11)/    &
     &     2.7449451d1,         &
     &    -1.2338849d5,         &
     &     5.7668847d2,         &
     &    -8.7256055d-1,        &
     &     3.4473356d-4,        &
     &     2.1473469d4,         &
     &    -1.5646970d2,         &
     &     2.0968062d-1,        &
     &    -3.2079651d7,         &
     &     1.1201540d5,         &
     &    -9.7660051d1/
      p = pbars
      t = tc + 273.15d0
      if (tc .gt. 0.32d0 * p + 294.0d0 ) then
         s1 = ((b(11)*t + b(10))*t + b(9))*t
         s2 = ((b(8)*t + b(7))*t + b(6))*t
         s3 = ((b(5)*t + b(4))*t + b(3))*t + b(2)
         vout = ((s1/p + s2)/p + s3)/p + b(1)
      else
         s1 = ((a(10)*t + a(9))*t + a(8))*t
         s2 = ((a(7)*t + a(6))*t + a(5))*t
         s3 = ((a(4)*t + a(3)*t**18)*t + a(2))*t
         vout = (s1/p + s2)/p + s3 + a(1)
      endif
      end subroutine vest67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     hss67(3f) - [M_steam67] specific enthalpy = HSS67(P,T,s,v)
!!##SYNOPSIS
!!
!!       function hss67(p,t,s,v)
!!##DESCRIPTION
!!
!!    function hss67 will calculate specific enthalpy, specific entropy,
!!    and specific volume of water at pressure p and temperature t.
!!
!!     DEFINITION AND PURPOSE:
!!           o H,S,V=f(P,T)
!!           o Calculates specific enthalpy, specific entropy, and specific
!!             volume.
!!
!!     REGION OF APPLICATION:
!!         Region 2 with P[max] = 1000 bars = 14503.7738 psia T[max] = 800 C =
!!         1472 F
!!
!!     CALLING SEQUENCE:
!!
!!         H=HSS(P,T,S,V)
!!
!!##OPTIONS
!!    P       water pressure, in PSIA.
!!    T       water temperature, in degrees F.
!!
!!##RETURN VALUES
!!    S       specific entropy, in BTU/LB-DEGF
!!    V       specific volume.
!!    HSS67   specific enthalpy, in BTU/LB.
function hss67(p,t,s,v)

! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS RESERVED

! ident_14="@(#) M_steam67 hss67(3f) specific enthalpy = HSS67(P T s v)"
! ident_15="@(#) M_steam67 hsl67(3f) specific enthalpy = HSS67(P T s v)"

doubleprecision :: hss67
doubleprecision :: hss167
doubleprecision :: aa
doubleprecision :: b0
doubleprecision :: b1
doubleprecision :: b6
doubleprecision :: b9
doubleprecision :: beta
doubleprecision :: betai
doubleprecision :: bl
doubleprecision :: bt
doubleprecision :: h1
doubleprecision :: h2
doubleprecision :: h3
integer         :: i
integer         :: icnt
integer         :: ix6
integer         :: iz1
integer         :: iz6
integer         :: j
integer         :: jkl
integer         :: k
doubleprecision :: p
doubleprecision :: q
doubleprecision :: s
doubleprecision :: s1
doubleprecision :: s2
doubleprecision :: s3
doubleprecision :: sb
doubleprecision :: sb6
doubleprecision :: t
doubleprecision :: theta
doubleprecision :: tmin
doubleprecision :: v
doubleprecision :: v1
doubleprecision :: v2
doubleprecision :: v3
doubleprecision :: x
doubleprecision :: xyz
doubleprecision :: z
save
dimension iz1(5,3), iz6(3,2), q(6), z(4), ix6(3,2), b0(6), b1(3,5), b6(2,3), b9(7), sb6(2,3), aa(10)
data (aa(i), i=1, 10) /                                           &
     &     290.69767d0,                                                 &
     &   -1160.2d0,                                                     &
     &    1214.5749d0,                                                  &
     &   -8311.8d0,                                                     &
     &     179.6814d0,                                                  &
     &    -666.2d0,                                                     &
     &     249.4970d0,                                                  &
     &   -1220.4d0,                                                     &
     &     676.7607d0,                                                  &
     &   -5142.6d0 /
data (b0(i), i=1, 6) /                                            &
     &       0.08565182058d0,                                           &
     &      -0.6547711697d0,                                            &
     &       0.4330662834d0,                                            &
     &     -54.38923329d0,                                              &
     &      28.56067796d0,                                              &
     &      16.83599274d0 /
data ((b1(i, j), i=1, 3), j=1, 5) /                               &
     &       0.06670375918d0,                                           &
     &       1.388983801d0,                                             &
     &       0.0d0,                                                     &
     &       0.08390104328d0,                                           &
     &       0.02614670893d0,                                           &
     &      -0.03373439453d0,                                           &
     &       0.4520918904d0,                                            &
     &       0.1069036614d0,                                            &
     &       0.0d0,                                                     &
     &      -0.5975336707d0,                                            &
     &      -0.08847535804d0,                                           &
     &       0.0d0,                                                     &
     &       0.5958051609d0,                                            &
     &      -0.5159303373d0,                                            &
     &       0.2075021122d0 /
data ((b6(i,j),i=1,2),j=1,3) /                                    &
     &       0.1190610271d0,                                            &
     &      -0.09867174132d0,                                           &
     &       0.1683998803d0,                                            &
     &      -0.05809438001d0,                                           &
     &       6.552390126d-3,                                            &
     &       5.710218649d-4 /
data (b9(i), i=1, 7) /                                            &
     &     523.5718623d0,                                               &
     &   -2693.088365d0,                                                &
     &    5745.984054d0,                                                &
     &   -6508.211677d0,                                                &
     &    4126.607219d0,                                                &
     &   -1388.522425d0,                                                &
     &     193.6587558d0 /
data sb / .7633333333d0 /
data ((sb6(i,j),i=1,2),j=1,3)/                                    &
     &       0.4006073948d0,                                            &
     &       0.0d0,                                                     &
     &       0.08636081627d0,                                           &
     &       0.0d0,                                                     &
     &      -0.8532322921d0,                                            &
     &       0.3460208861d0/

data ((iz1(i,j),i=1,5),j=1,3) / 13,18,18,25,32,3,2,10,14,28,0,1,0,0,24 /
data ((iz6(i,j),i=1,3),j=1,2) /12,24,24,11,18,14/
data ((ix6(i,j),i=1,3),j=1,2) /14,19,54,0,0,27/

      icnt=4
      jkl=0
      goto 1
!
!
!===================================================================================================================================
!>
!!##NAME
!!     hss167(3f) - [M_steam67]
!!##SYNOPSIS
!!
!!   entry hss167(p,t,s,v)
!!##DESCRIPTION
!!##OPTIONS
entry hss167(p,t,s,v)
      icnt=3
      jkl=1
1     continue
      beta=p/3208.2347590d0
      theta=8.582659595d-4*t+0.3945191136d0
      bl=p23t67(t)/3208.234759d0
      if (beta.gt.bl) then
         hss67=hcslv267(p,t,v,s,icnt)
         goto 999
      endif
      xyz=log(p)
      x=exp(sb*(1.0d0-theta))
      bt=sb*theta
      v1=0.0d0
      s1=0.0d0
      h1=0.0d0
      !----------------------------------------------------------------------
      do i=1,5
         do j=1,3
            q(j)=b1(j,i)*x**iz1(i,j)
            z(j)=dble(iz1(i,j))
         enddo
         betai=beta**i
         v1=v1+dble(i)*(betai/beta)*(q(1)+q(2)+q(3))
         if (jkl.eq.1) cycle
         s1=s1+betai*(z(1)*q(1)+z(2)*q(2)+z(3)*q(3))
         h1=h1+betai*((1.0d0+z(1)*bt)*q(1)+(1.0d0+z(2)*bt)*q(2)+(1.0d0+z(3)*bt)*q(3))
      enddo
      !----------------------------------------------------------------------
      v2=0.0d0
      s2=0.0d0
      h2=0.0d0
      v3=0.0d0
      s3=0.0d0
      h3=0.0d0
      if (p.le.200.0d0) goto 10
      k=1
      if (p.le.2300.0d0) goto 4
      k=3
      if (p.ge.3500.0d0) goto 5
4     continue
      tmin=aa(k)*xyz+aa(k+1)
      if (t.ge.tmin) goto 10
5     continue
      !----------------------------------------------------------------------
      do i=1,3
         do j=1,2
            q(j)=b6(j,i)*x**iz6(i,j)
            z(j)=dble(iz6(i,j))
            q(j+2)=sb6(j,i)*x**ix6(i,j)
            z(j+2)=dble(ix6(i,j))
         enddo
         betai=beta**(-3-i)
         q(5)=betai+q(3)+q(4)
         v2=v2+(dble(i+3)*betai/beta*(q(1)+q(2)))/q(5)**2
         if (jkl.eq.1) cycle
         q(6)=(z(3)*q(3)+z(4)*q(4))/q(5)
         s2=s2+(q(1)*(z(1)-q(6))+q(2)*(z(2)-q(6)))/q(5)
         h2=h2+(q(1)*((1.0d0+z(1)*bt)-bt*q(6))+q(2)*((1.0d0+z(2)*bt)-bt*q(6)))/q(5)
      enddo
      !----------------------------------------------------------------------
      if (p.le.1000.0d0) goto 10
      k=5
      if (p.le.2800.0d0) goto 8
      k=7
      if (p.gt.9700.0d0) k=9
8     continue
      tmin=aa(k)*xyz+aa(k+1)
      if (t.ge.tmin) goto 10
      q(1)=(386.27614140d0*theta-341.70619780d0)/bl
      !----------------------------------------------------------------------
      do i=1,7
         v3=v3*x+b9(i)
         if (jkl.eq.1) cycle
         q(2)=q(1)+dble(7-i)*sb
         s3=s3*x+q(2)*b9(i)
         h3=h3*x+(1.0d0+theta*q(2))*b9(i)
      enddo
      !----------------------------------------------------------------------
      q(1)=(beta/bl)**10
10    continue
      v=(4.2603211480d0*theta/beta-v1-v2+11.0d0*q(1)*v3)*.050778528890d0
      q(2)=0.0d0
      q(3)=3.0d0*b0(1)
      !----------------------------------------------------------------------
      do i=1,4
         q(2)=q(2)*theta+dble(5-i)*b0(i)
         q(3)=q(3)*theta+dble(3-i)*b0(i+1)
      enddo
      !----------------------------------------------------------------------
      s=(1.583237108d-6-4.260321148d0*log(beta)+b0(6)*log(theta)-q(2)-sb*s1-sb*s2+beta*q(1)*s3)*.02587358228d0
      hss67=(2.156561703d-7+b0(6)*theta-q(3)-h1-h2+beta*q(1)*h3)*30.14634566d0
999   continue
      return
      end function hss67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     psl67(3f) - [M_steam67] calculate saturation pressure of the saturated liquid at temperature T
!!
!!##SYNOPSIS
!!
!!    function psl67(T)
!!
!!##DESCRIPTION
!!
!!    function psl67 will calculate saturation pressure
!!    of the saturated liquid at temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!           o P=f(T)
!!           o Calculates saturation pressure
!!
!!     REGION OF APPLICATION:
!!         Region 3. 32 .le. T .le. 705.47 F
!!
!!     CALLING SEQUENCE:
!!         P=PSL(T)
!!
!!##OPTIONS
!!     T        water temperature, in degrees F.
!!##RETURN VALUES
!!     PSL67    saturation pressure, in PSIA.
function psl67(t)
!  Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_16="@(#) M_steam67 psl67(3f) calculate saturation pressure of the saturated liquid at temperature T"
! ident_17="@(#) M_steam67 psl167(3f) calculate saturation pressure of the saturated liquid at temperature T."

doubleprecision :: psl67
doubleprecision :: psl167
doubleprecision :: ak
doubleprecision :: ak2t2
doubleprecision :: ak3t3
doubleprecision :: ak4t4
doubleprecision :: ak5t5
doubleprecision :: ak7t2
doubleprecision :: ak8t2
doubleprecision :: b
doubleprecision :: dbbdt
doubleprecision :: dbdt
doubleprecision :: den1
doubleprecision :: den2
doubleprecision :: dpdt
doubleprecision :: dsdt
integer         :: i
integer         :: j
integer         :: m6i
doubleprecision :: score
doubleprecision :: t
doubleprecision :: tcore
doubleprecision :: theta
doubleprecision :: vcore
doubleprecision :: x
doubleprecision :: y
save
common /nust/tcore, score, vcore, dpdt
dimension ak(9)
data(ak(i), i = 1, 9)/ &
     &    -7.691234564d0,    &
     &   -26.08023696d0,     &
     &  -168.1706546d0,      &
     &    64.23285504d0,     &
     &  -118.9646225d0,      &
     &     4.167117320d0,    &
     &    20.97506760d0,     &
     &     1.0d9,            &
     &     6.0d0/
data ak2t2, ak3t3/-5.21604739d1, -5.04511964d2/
data ak4t4, ak5t5/ + 2.56931420d2, -5.94823113d2/
data ak7t2, ak8t2/ + 4.19501352d1,  + 2.0d9/
   j = 1
   goto 1
!===================================================================================================================================
!>
!!##NAME
!!     psl167(3f) - [M_steam67] calculate saturation pressure of the saturated liquid at temperature T.
!!
!!##SYNOPSIS
!!
!!    entry psl167(T)
!!
!!##DESCRIPTION
!!     function psl167 will calculate saturation pressure
!!     of the saturated liquid at temperature T.
!!
!!##OPTIONS
!!      T        water temperature, in degrees F.
!!
!!##RESULT
!!      PSL167   saturation pressure, in PSIA.
entry psl167(t)
   j = 2
   goto 1
!===================================================================================================================================
1  continue
   if (t .gt. 705.47d0) then
      call ster67('PSL67', 2, t, 0.0d0)
   else
!     theta   =((t-32.0d0)/1.8d0+273.15d0)/647.3d0
      theta = (t + 459.67d0)/1165.14d0
      x = 1.0d0 - theta
      y = 0.0d0
      do i = 1, 5
         m6i = 6 - i
         y = (y + ak(m6i))*x
      enddo
!     the  k function (saturation line) page 12 par. 5
      den1 = 1.0d0 + x*(ak(6) + ak(7)*x)
      den2 = ak(8)*x*x + ak(9)
      psl67 = exp(y/theta/den1 - x/den2)*3208.234759d0
      if (j .eq. 2) then
         dsdt =  - (ak(1) + x*(ak2t2 + x*(ak3t3 + x*(ak4t4 + ak5t5 *x))))
         b = theta*den1
         dbdt = den1 - theta*(ak(6) + ak7t2*x)
         dbbdt =  - ak8t2*x
         dpdt=(psl67/1165.14d0)*(((b*dsdt-y*dbdt)/(b*b)) + ((den2 + x*dbbdt)/(den2*den2)))
      endif
   endif
end function psl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     psv67(3f) - [M_steam67] calculate saturation pressure of the saturated liquid at entropy SS in psia
!!
!!##SYNOPSIS
!!
!!    function psv67(SS)
!!
!!       doubleprecision,intent(in) :: ss
!!
!!##DESCRIPTION
!!   function psv67 will calculate saturation pressure
!!   of the saturated liquid at entropy SS in psia
!!
!!     DEFINITION AND PURPOSE:
!!           o P=f(S)
!!           o Calculates saturation pressure
!!
!!     REGION OF APPLICATION:
!!         Vapor side of Region 3. Approximate limits are 1.062 .le. S .le. 2.187
!!
!!     CALLING SEQUENCE:
!!         P=PSV(S)
!!
!!##OPTIONS
!!    SS     specific entropy, in btu/lb-degf
!!
!!##RETURN VALUES
!!    PSV67  output
function psv67(ss)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_18="@(#) M_steam67 psv67(3f) calculate saturation pressure of the saturated liquid at entropy SS in psia"
! ident_19="@(#) M_steam67 psv167(3f) calculate saturation pressure of the saturated liquid at entropy SS in psia"
! ident_20="@(#) M_steam67 psv267(3f) calculate saturation pressure in psia of the saturated liquid at entropy SS."
! ident_21="@(#) M_steam67 pls67(3f) saturation pressure of the saturated liquid at entropy SS."

doubleprecision,intent(in) :: ss !  SPECIFIC ENTROPY, IN BTU/LB-DEGF
doubleprecision :: psv67
doubleprecision :: pls67
doubleprecision :: psv267
doubleprecision :: psv167
doubleprecision :: a
doubleprecision :: del
doubleprecision :: f
integer         :: i
integer         :: j
integer         :: k
integer         :: m
integer         :: na
integer         :: nt
doubleprecision :: p
doubleprecision :: s
doubleprecision :: t
doubleprecision :: tol
doubleprecision :: x
!external HCL67
save
dimension a(34), x(4)
      data(a(i), i = 1, 34)/    &
     &   -8.80394946d2,         &
     &    7.89786031d3,         &
     &   -2.31946509d4,         &
     &    4.91578728d3,         &
     &    1.17623217d5,         &
     &   -2.71662376d5,         &
     &    2.51082767d5,         &
     &   -8.56048166d4,         &
     &    4.56627272d2,         &
     &   -3.19052204d1,         &
     &   -4.37419785d3,         &
     &    3.95021735d3,         &
     &    1.05909094d4,         &
     &   -2.13579899d4,         &
     &    1.41118848d4,         &
     &   -2.64409937d3,         &
     &    1.430d0,              &
     &   -5.41526744d3,         &
     &    5.80623668d4,         &
     &   -2.25605970d5,         &
     &    3.01957952d5,         &
     &    3.50330052d5,         &
     &   -1.59876151d6,         &
     &    1.85517187d6,         &
     &   -7.52009736d5,         &
     &   -8.11246322d1,         &
     &    5.03050898d2,         &
     &   -1.21601533d3,         &
     &    1.26364010d3,         &
     &   -1.87526996d1,         &
     &   -9.53928692d2,         &
     &    1.02995746d3,         &
     &   -4.94841187d2,         &
     &    0.690d0/
      j = 0
      goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     psv167(3f) - [M_steam67] calculate saturation pressure in psia of the saturated liquid at entropy SS.
!!
!!##SYNOPSIS
!!
!!   entry psv167(SS)
!!
!!##DESCRIPTION
!!     function psv167 will calculate saturation pressure
!!     in psia of the saturated liquid at entropy SS.
!!
!!##OPTIONS
!!      SS       specific entropy, in BTU/LB-DEGF
!!
!!##RETURN VALUES
!!      PSV167   saturation pressure, in PSIA.
   entry psv167(ss)
   j = 1
   goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     psv267(3f) - [M_steam67] calculate saturation pressure in psia of the saturated liquid at entropy SS.
!!
!!##SYNOPSIS
!!
!!   calculate saturation pressure of the saturated liquid at entropy SS.
!!
!!##DESCRIPTION
!!    function psv267 will calculate saturation pressure
!!    of the saturated liquid at entropy SS.
!!
!!##OPTIONS
!!      SS      specific entropy, in BTU/LB-DEGF
!!
!!##RETURN VALUES
!!      PSV267  saturation pressure, in PSIA.
entry psv267(ss)
   j = 2
goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     pls67(3f) - [M_steam67] saturation pressure of the saturated liquid at entropy SS.
!!
!!##SYNOPSIS
!!
!!   entry pls67(ss)
!!
!!##DESCRIPTION
!!     function pls67 will calculate saturation pressure
!!     of the saturated liquid at entropy SS.
!!
!!##OPTIONS
!!     SS       specific entropy, in BTU/LB-DEGF
!!
!!##RETURN VALUES
!!     PLS67    saturation pressure, in PSIA.
entry pls67(ss)

   s = 1.0d0 + ss
   j = 3
   i = 18
   goto 2
   1 continue
   i = 1
   s = ss
   2 continue
   psv67 = 3208.234759d0
   tol = 0.00002d0
   if(.not.(ss .ge. 1.06116005d0 .and. ss .le. 1.0618559d0))then
      nt = 1
      na = 5
      if (ss .gt. 2.1872d0 .or. ss .lt. 0.0d0) then
         psv67 = 0.0d0
      else
         del = 0.1d0
         if (ss .ne. 1.0618559d0) then
            if (ss .gt. 1.0618559d0) then
               if (j .ne. 3) goto 5
            elseif (j .ge. 3) then
               goto 5
            endif
            call ster67(' PSV67', 5, ss, 0.0d0)
            goto 18
   5        continue
            if (ss .lt. a(i + 16)) i = i + 8
            x(2) = a(i)
            k = i + 6
            do m = i, k
               x(2) = x(2)*s + a(m + 1)
            enddo
   7        continue
            if (x(2) .gt. 705.47d0) x(2) = 705.47d0
            psv67 = psl67(x(2))
            if (j .ne. 1) then
               if (j .eq. 3) then
                  t = hcl67(psv67, x(2), x(1))
                  if (psv67 .lt. 1900.0d0) tol = .000002d0
               else
                  t = hss67(psv67, x(2), x(1), x(3))
               endif
               if (abs(x(1) - ss) .lt. tol) goto 20
               if (psv67 .le. 3100.0d0) then
                  if (j .ne. 3) then
                     if (nt .eq. 1) goto 15
                  endif
               endif
               x(4) = x(2) - del
   11          continue
               p = psl67(x(4))
               if (j .eq. 3) then
                  t = hcl67(p, x(4), x(3))
               else
                  t = hss67(p, x(4), x(3), f)
               endif
               if (abs(x(2) - x(4)) .lt. 0.002d0) then
                  x(4) = x(4) - del
                  nt = nt + 1
                  if (nt .gt. 300) goto 18
                  goto 11
               endif
               x(2) = gr167(x(1), ss, 2)
               if (nt .gt. 300) goto 18
               nt = nt + 1
               del = 0.9d0*del
               if (nt .eq. na) na = na + 5
               goto 7
            endif
            psv67 = psv67*1.003d0
            goto 20
   15       continue
            f = 1.058d0 - 0.364d-3*psv67
            if (psv67 .le. 300.0d0) f = 1.33d0
            if (psv67 .gt. 2200.0d0) f = 0.782d0 - 0.239d-3*psv67
            psv67=psv67-5.4d0*(x(2) + 459.67d0)*(ss - x(1))/x(3)*f
            goto 20
   18       continue
            print 21, psv67, ss
         endif
      endif
   endif
   20 continue
   !
   21 format ('0PSV ', f12.6, 'ENTR ', f8.6)
end function psv67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     sssiss67(3f) - [M_steam67] calculate specific entropy, temperature, specific volume, and steam quality of the liquid
!!                    at pressure P and enthalpy H.
!!##SYNOPSIS
!!
!!   function sssiss67(pres, enth, temp, v, x)
!!
!!##DESCRIPTION
!!
!!     function sssiss67 will calculate specific entropy,
!!     temperature, specific volume, and steam quality
!!     of the liquid at pressure P and enthalpy H.
!!
!!     DEFINITION AND PURPOSE:
!!       o S.T,V,X=f(P,H)
!!       o Calculates specific entropy, temperature. specific volume, and
!!         quality.
!!
!!     REGION OF APPLICATION:
!!         Region 2 with P[max] = 1000 bars = 14,503.7738 psia T[max] = 800 C =
!!         1472 F
!!         See Section 4.5 for limits on H.
!!
!!     CALLING SEQUENCE:
!!         S = SSSISS(P,H,T,V,X)
!!
!!##OPTIONS
!!     P     water pressure, in PSIA.
!!     H     specific enthalpy, in BTU/POUND.
!!
!!##RETURN VALUES
!!     T         water temperature, in degrees F.
!!     V         specific volume.
!!     X         fraction of steam quality.
!!     sssiss67  specific entropy, in BTU/LB-DEGF
function sssiss67(pres, enth, temp, v, x)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_22="@(#) M_steam67 sssiss67(3f) specific entropy temperature specific volume steam quality of liquid at pressure P and enthalpy H"
! ident_23="@(#) M_steam67 hssiss67(3f) specific enthalpy temperature specific volume steam quality of water at pressure P and entropy S"

doubleprecision :: sssiss67
doubleprecision :: hpsiss67
doubleprecision :: spsiss67
doubleprecision :: hssiss67
doubleprecision :: hisiss67
doubleprecision :: sisiss67
doubleprecision :: aa
doubleprecision :: dpdt
doubleprecision :: enth
doubleprecision :: factor
integer         :: i
integer         :: ii
integer         :: jj
integer         :: k
integer         :: kk
integer         :: m3i
doubleprecision :: p
doubleprecision :: pp
doubleprecision :: pres
doubleprecision :: score
doubleprecision :: sum
doubleprecision :: t
doubleprecision :: tcore
doubleprecision :: temp
doubleprecision :: v
doubleprecision :: vcore
doubleprecision :: x
doubleprecision :: x1
doubleprecision :: x2
doubleprecision :: xf
doubleprecision :: xgi
doubleprecision :: y
!external HCL67,HSL67,VCL67
!external STER67, GOTOER67
save
common /nust/tcore, score, vcore, dpdt
common /liqu/factor
dimension xgi(2), xf(2), aa(10)
data(aa(i), i = 1, 10)/          &
     &    -7.78689284d-12,             &
     &     4.55590147d-8,              &
     &    -1.11909400d-4,              &
     &     7.92788503d-2,              &
     &  1191.0d0,                      &
     &    -4.96754782d-2,              &
     &     2.61315144d-1,              &
     &     7.30483351d-1,              &
     &     1.33710342d1,               &
     &  1108.68299d0/
      i = 1
      goto 2
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     sisiss67(3f) - [M_steam67] calculate specific entropy, temperature, specific volume, and steam quality of the liquid
!!                  at pressure P and enthalpy H.
!!
!!##SYNOPSIS
!!
!!    entry sisiss67(pres, enth, temp, v, x)
!!
!!##DESCRIPTION
!!
!!    function sisiss67 will calculate specific entropy,
!!    temperature, specific volume, and steam quality
!!    of the liquid at pressure P and enthalpy H.
!!
!!##OPTIONS
!!    P         water pressure, in PSIA.
!!    H         specific enthalpy, in BTU/POUND.
!!##RETURN VALUES
!!    T         water temperature, in degrees F.
!!    V         specific volume.
!!    X         fraction of steam quality.
!!    sisiss67  specific entropy, in BTU/LB-DEGF
entry sisiss67(pres, enth, temp, v, x)
   i = 1
   goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     hisiss67(3f) - [M_steam67] calculate specific enthalpy, temperature, specific volume, and steam quality of water at P and S.
!!
!!##SYNOPSIS
!!
!!   entry hisiss67(pres, enth, temp, v, x)
!!
!!##DESCRIPTION
!!
!!    function hisiss67 will calculate specific enthalpy,
!!    temperature, specific volume, and steam quality
!!    of water at pressure P and entropy S.
!!
!!##OPTIONS
!!      P         water pressure, in PSIA.
!!      S         specific entropy, in BTU/LB-DEGF
!!
!!##RETURN VALUES
!!      T         water temperature, in degrees F.
!!      V         specific volume.
!!      X         fraction of steam quality.
!!      hisiss67  specific enthalpy, IN BTU/LB.
entry hisiss67(pres, enth, temp, v, x)
   i = 2
   goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     hssiss67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   ENTRY hssiss67(PRES, ENTH, TEMP, V, X)
!!
!!##DESCRIPTION
!!   function HSSISS67 will calculate specific enthalpy,
!!   temperature, specific volume, and steam quality
!!   of water at pressure P and entropy S.
!!
!!     DEFINITION AND PURPOSE:
!!           o H,T,V,X=f(P,S)
!!           o Calculates specific enthalpy, temperature, specific volume, and
!!             quality.
!!
!!     REGION OF APPLICATION:
!!         Region 2 with P[max] = 1000 bars = 14,503.7738 psia T[max] = 800 C =
!!         1472 F
!!         See Section 4.6 for limits on S.
!!
!!     CALLING SEQUENCE:
!!         H=HSSISS(P,S,T,V,X)
!!
!!##OPTIONS
!!    P     water pressure, in PSIA.
!!    S     specific entropy, in BTU/LB-DEGF
!!
!!##RETURN VALUES
!!    T         water temperature, in degrees F.
!!    V         specific volume.
!!    X         fraction of steam quality.
!!    hssiss67  specific enthalpy, in BTU/LB.
entry hssiss67(pres, enth, temp, v, x)

      i = 2
      goto 2
1     continue
      kk = 2
      goto 3
2     continue
      kk = 0
3     continue
      p = pres
      x = 1.0d0
      if (p .lt. 3208.234759d0) then
         if (i .eq. 1) then
            if (enth .gt. 1204.79d0) goto 6
            pp = p
            ii = 1
            if (p .lt. 3100.0d0) then
               if (p .lt. 400.0d0) then
                  pp = log(p)
                  ii = 6
               endif
               jj = ii + 3
               sum = 0.0d0
               do k = ii, jj
                  sum = (sum + aa(k))*pp
               enddo
               sum = sum + aa(jj + 1)
               if (enth .ge. sum) goto 6
            elseif (enth .ge. 993.4d0) then
               goto 6
            endif
         elseif (enth .ge. 1.06185d0) then
            pp = psv167(enth)
            if (p .gt. pp) goto 7
         endif
         y = enth
         k = 3 - i
         t = tsl67(p)
         if (kk .eq. 2) t = tsl167(p)
         xgi(1) = hss67(p, t, xgi(2), v)
         sssiss67 = xgi(k)
         x2 = t + 459.67d0
         if (y .eq. xgi(i)) goto 9
         if (y .le. xgi(i)) then
            if (kk .eq. 2) then
               xf(1) = hcl67(p, t, xf(2))
            else
               xf(1) = hsl67(t)
            endif
            x1 = xgi(1) - xf(1)
            ! sfg is calculated from hfg/tabs
            xf(2) = xgi(2) - x1/x2
            if (y .lt. xf(i)) goto 20
            x = (y - xf(i))/(xgi(i) - xf(i))
            sssiss67 = xf(k) + x*(xgi(k) - xf(k))
            ! vfg is calculated from clapeyron eq. using hfg, tabs, and dpdt
            x2 = x1/x2*778.169262d0/144.0d0/dpdt
            if (kk .eq. 2) x2 = v - vcl67(p, t)
            v = v - (1.0d0 - x)*x2
            goto 9
         endif
      elseif (kk .eq. 2) then
         goto 5
      endif
4     continue
      if (i .eq. 1) goto 6
      if (i .eq. 2) goto 7
      call gotoer67('sssiss67 (A), I', i)
5     continue
      t = 705.47d0
      xgi(1) = hss67(p, t, xgi(2), vcore)
      m3i = 3 - i
      score = xgi(m3i)
      if (enth .ge. xgi(i)) then
         if (enth .gt. xgi(i)) goto 4
         goto 8
      endif
20    continue
      factor = 1.0d0
      if (i .eq. 2) then
         t = tpsl67(p, enth)
      else
         t = tphl67(p, enth)
      endif
      v = vcl67(p, t)
      sssiss67 = score
      x = 0.0d0
      goto 9
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     spsiss67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   ENTRY spsiss67(PRES, ENTH, TEMP, V, X)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURN VALUES
      entry spsiss67(pres, enth, temp, v, x)
6     continue
      t = tph67(pres, enth)
      goto 8
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     hpsiss67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   ENTRY hpsiss67(PRES, ENTH, TEMP, V, X)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURN VALUES
entry hpsiss67(pres, enth, temp, v, x)
7     continue
      t = tps67(pres, enth)
8     continue
      sssiss67 = score
      v = vcore
9     continue
      temp = t
      factor = 0.0d0
end function sssiss67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     tps67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!    function tps67 will calculate the temperature of
!!    the liquid that is at pressure P, and entropy S.
!!
!!##OPTIONS
!!     P       water pressure, in PSIA.
!!     S       specific entropy, in BTU/LB-DEGF
!!
!!##RETURN VALUES
!!     TPS67   water temperature, in degrees F.
function tps67(pres,entr)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation. All rights reserved
doubleprecision :: tps67
doubleprecision :: tph67
integer         :: i
integer         :: icnt
integer         :: j
integer         :: j1
integer         :: jj
integer         :: k
integer         :: kk
integer         :: l
integer         :: m3i
doubleprecision :: a
doubleprecision :: a1
doubleprecision :: a2
doubleprecision :: cp
doubleprecision :: csmax
doubleprecision :: dmt
doubleprecision :: dpdt
doubleprecision :: entr
doubleprecision :: p
doubleprecision :: pl
doubleprecision :: pres
doubleprecision :: s1
doubleprecision :: s2
doubleprecision :: s3
doubleprecision :: s4
doubleprecision :: score
doubleprecision :: smax
doubleprecision :: t
doubleprecision :: t0
doubleprecision :: t1
doubleprecision :: tcore
doubleprecision :: tol
doubleprecision :: vcore
doubleprecision :: vx
doubleprecision :: x0
doubleprecision :: x1
doubleprecision :: x2
doubleprecision :: xgi
doubleprecision :: y
doubleprecision :: y1
save
dimension a(240), csmax(7), t0(2), tol(2), xgi(2)
common /nust/ tcore,score,vcore,dpdt
equivalence (cp,t0(2))
data (csmax(i),i=1,7) /            &
     &     7.20894478d-26,               &
     &    -5.47437942d-21,               &
     &     1.61833683d-16,               &
     &    -2.43468381d-12,               &
     &     2.03081212d-8,                &
     &    -9.99741636d-5,                &
     &     1.55077535d0 /
data (tol(i),i=1,2) / 0.00004d0,0.04d0 /
data (a(i),i=1,94) /               &
     &   -5.5599547d0,                   &
     &    8.4393608d1,                   &
     &    3.3188124d3,                   &
     &    4.1729928d3,                   &
     &    1.4274930d2,                   &
     &   -8.9414612d2,                   &
     &    3.5194578d3,                   &
     &   -6.6260987d3,                   &
     &    1.4658774d2,                   &
     &   -1.2815649d3,                   &
     &    4.3273965d3,                   &
     &   -6.2889053d3,                   &
     &    2.2784044d1,                   &
     &   -1.5924637d2,                   &
     &    3.8896515d2,                   &
     &   -3.5619013d2,                   &
     &    2.0012448d-1,                  &
     &    1.0286436d0,                   &
     &   -8.3416863d0,                   &
     &    1.4028976d1,                   &
     &    6.4722100d-1,                  &
     &   -1.4489854d0,                   &
     &   -5.0723444d1,                   &
     &   -1.0993061d1,                   &
     &    1.8154273d-1,                  &
     &   -3.2470171d-1,                  &
     &   -4.2259137d0,                   &
     &    1.4904444d+1,                  &
     &   -1.8579714d0,                   &
     &    1.7225725d1,                   &
     &   -5.9391845d1,                   &
     &    9.0095328d1,                   &
     &   -3.2738337d-1,                  &
     &    2.3324606d0,                   &
     &   -5.7786684d0,                   &
     &    5.5620787d0,                   &
     &    4.1568901d-2,                  &
     &   -3.4564095d-1,                  &
     &    1.0502158d0,                   &
     &   -1.3734380d0,                   &
     &   -1.3134447d2,                   &
     &    3.9587121d2,                   &
     &   -9.2018314d3,                   &
     &    1.0072505d5,                   &
     &   -2.5375386d3,                   &
     &    2.1533054d4,                   &
     &   -8.6016253d3,                   &
     &   -9.3565682d4,                   &
     &    9.8456073d2,                   &
     &   -1.6188292d4,                   &
     &    4.0030015d4,                   &
     &   -2.1049336d4,                   &
     &   +3.4693378d2,                   &
     &    1.6096410d2,                   &
     &   -2.8904072d3,                   &
     &    2.5752809d3,                   &
     &   -6.3028211d1,                   &
     &    2.4423632d2,                   &
     &   -3.5653712d2,                   &
     &    2.8491840d2,                   &
     &    1.7102850d1,                   &
     &   -3.0769524d2,                   &
     &    1.7752283d3,                   &
     &   -2.9130164d3,                   &
     &    1.2217045d3,                   &
     &  - 5.1533288d3,                   &
     &    5.8643947d3,                   &
     &    6.1764060d2,                   &
     &   -3.5878724d2,                   &
     &    1.2906743d3,                   &
     &   -6.3650853d2,                   &
     &   -1.9218128d3,                   &
     &    3.0252521d1,                   &
     &   -7.1558335d1,                   &
     &   -1.2971825d2,                   &
     &    4.5863082d2,                   &
     &   -4.0893192d-1,                  &
     &   -2.0645432d0,                   &
     &    1.6706904d1,                   &
     &   -3.0438749d1,                   &
     &   -1.1237737d2,                   &
     &    3.3263663d3,                   &
     &   -1.8512639d4,                   &
     &   -1.9898216d4,                   &
     &   -1.3535718d5,                   &
     &    3.9496060d5,                   &
     &   -3.6894107d5,                   &
     &    1.2708393d5,                   &
     &    3.8318594d4,                   &
     &   -8.7001026d4,                   &
     &    2.7367770d4,                   &
     &    4.0676726d4,                   &
     &   -4.2670264d3,                   &
     &    8.4396154d3 /
data (a(i),i=95,184) /            &
     &   8.2152762d2,                   &
     &  -8.4112326d3,                   &
     &   1.9116854d2,                   &
     &  -4.2089024d2,                   &
     &   9.5779218d1,                   &
     &   2.4995624d2,                   &
     &   1.2834331d0,                   &
     &  -2.6288833d1,                   &
     &   6.8871131d1,                   &
     &   5.3195659d2,                   &
     &  -2.1829230d2,                   &
     &   5.3902353d2,                   &
     &   2.1739479d2,                   &
     &  -1.0782359d3,                   &
     &   9.1983670d0,                   &
     &  -1.2885895d2,                   &
     &   2.5511831d2,                   &
     &  -2.0298959d2,                   &
     &   1.0920588d1 ,                  &
     &  -1.3238754d1,                   &
     &  -2.7013782d1,                   &
     &   5.5598058d1,                   &
     &  -1.0663413d0,                   &
     &   2.5035364d0,                   &
     &  -7.5214513d-1,                  &
     &  -1.9705139d0,                   &
     &   4.2013621d-4,                  &
     &  -4.1932327d-1,                  &
     &   1.5141077d2,                   &
     &  -2.2520652d3,                   &
     &   1.6315437d-2,                  &
     &  -9.0026536d-1,                  &
     &   1.4958716d1,                   &
     &   1.3537487d2,                   &
     &   2.5496260d-3,                  &
     &  -1.5974572d-1,                  &
     &   3.7433516d0,                   &
     &  -3.8901889d1,                   &
     &  -7.6370789d-6,                  &
     &   4.7094182d-4,                  &
     &  -1.0839362d-2,                  &
     &   1.1035609d-1,                  &
     &   7.8347134d-9,                  &
     &  -4.8065505d-7,                  &
     &   1.1001119d-5,                  &
     &  -1.1131805d-4,                  &
     &  -4.5961443d-6,                  &
     &   5.6966913d-3,                  &
     &  -1.8185381d0,                   &
     &  -2.6382451d0,                   &
     &  -8.2159334d-5,                  &
     &   5.5603397d-3,                  &
     &  -1.3651842d-1,                  &
     &   1.3810071d0,                   &
     &  -2.9350851d-5,                  &
     &   1.8619709d-3,                  &
     &  -4.4134420d-2,                  &
     &   4.6334716d-1,                  &
     &   9.8211689d-8,                  &
     &  -6.1439152d-6,                  &
     &   1.4339483d-4,                  &
     &  -1.4797535d-3,                  &
     &  -8.1475954d-11,                 &
     &   5.065903d-9,                   &
     &  -1.174533d-7,                   &
     &   1.2033309d-6,                  &
     &   2.8184432d2,                   &
     &  -4.4287667d3,                   &
     &   8.5634785d3,                   &
     &   7.6694126d4,                   &
     &   7.1091721d0,                   &
     &  -3.4572140d2,                   &
     &   5.8253774d+3,                  &
     & - 3.8532743d4,                   &
     &  -1.8596406d0,                   &
     &   8.0852779d1,                   &
     &  -1.0932898d3,                   &
     &   3.8776773d3,                   &
     &   1.4805832d-1,                  &
     &  -5.1626729d0,                   &
     &   3.0234601d1,                   &
     &   5.0693384d2,                   &
     &  -3.3055893d-3,                  &
     &   5.2671178d-2,                  &
     &   2.1051041d0,                   &
     &  -5.11079460d1,                  &
     &   2.2012258d1,                   &
     &  -2.8538743d2,                   &
     &   7.0853882d2,                   &
     &   1.3999782d3 /
data (a(i),i=185,240) /           &
     &   1.3622183d-1,                  &
     &  -6.7476003d0,                   &
     &   1.1617100d2,                   &
     &  -7.7782575d2,                   &
     &  -2.8456317d-2,                  &
     &   1.1431543d0,                   &
     &  -1.1751925d1,                   &
     &  -3.1902536d1,                   &
     &  -6.85192920d-5,                 &
     &   7.9112291d-2,                  &
     &  -3.6173341d0,                   &
     &   5.5686105d1,                   &
     &   1.9088950d-4,                  &
     &  -1.4791147d-2,                  &
     &   4.1358268d-1,                  &
     &  -4.9861745d+0,                  &
     &  -9.8087464d1,                   &
     &   2.3851888d3,                   &
     &  -1.5671273d4,                   &
     &   1.8569414d4,                   &
     &  -5.4356746d0,                   &
     &   1.6782851d2,                   &
     &  -1.2495261d3,                   &
     &  -4.2978594d2,                   &
     &   7.7948297d-1,                  &
     &  -1.0613781d1,                   &
     &  -2.8315794d2,                   &
     &   4.5569231d3,                   &
     &   1.2254461d-3,                  &
     &  -2.5404449d0,                   &
     &   8.6301218d1,                   &
     &  -8.2996502d2,                   &
     &  -2.2794293d-3,                  &
     &   1.8392351d-1,                  &
     &  -4.4186711d0,                   &
     &   3.7062186d1,                   &
     &  -1.7783957d0,                   &
     &   5.3926146d1,                   &
     &  -5.5686542d2,                   &
     &   1.9301093d3,                   &
     &  -5.1102788d-2,                  &
     &  -1.2963868d0,                   &
     &   6.3401252d1,                   &
     &  -6.4294549d2,                   &
     &   1.8898834d-2,                  &
     &   3.0206288d-1,                  &
     &  -1.8141145d1,                   &
     &   1.8676113d2,                   &
     &  -2.2454851d-3,                  &
     &  -2.2029177d-2,                  &
     &   1.7407411d0,                   &
     &  -1.8205145d1,                   &
     &   8.4498144d-5,                  &
     &   5.7095877d-4,                  &
     &  -5.7515932d-2,                  &
     &   6.0582787d-1 /

      j=15
      i=1
      dmt=0.00080d0
      goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     tph67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   entry tph67(pres,entr)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURN VALUES
      entry tph67(pres,entr)
      j=13
      i=2
      dmt=0.80d0
!-----------------------------------------------------------------------
1     continue
      p=pres
      j1=0
      a1=0.50d0
      a2=1.0d0
      y=entr
      y1=y
      x0=log(p)
      if (p.gt.3208.2340d0) goto 3
!-----------------------------------------------------------------------
      t=32.0d0
      goto 4
!-----------------------------------------------------------------------
2     continue
      tps67=t
      m3i=3-i
      score=xgi(m3i)
      return
!-----------------------------------------------------------------------
3     continue
      t=705.470d0
4     continue
      icnt=1
      if (p.gt.16000.0d0) goto 22
!-----------------------------------------------------------------------
      if (i.gt.1) goto 6
!-----------------------------------------------------------------------
5     continue
      pl=x0
      if (i-1) 7,8,7
!-----------------------------------------------------------------------
6     continue
      if (entr-1800.0d0) 5,5,22
!-----------------------------------------------------------------------
7     continue
      j=121
      y1=y/100.0d0
      smax=1280.0d0
      goto 10
!-----------------------------------------------------------------------
8     continue
      smax=csmax(1)
      do j=1,6
         smax=p*smax+csmax(j+1)
      enddo
      j=1
10    continue
      if (p.gt.500.0d0) j=j+40
      if (p.gt.2500.0d0.and.y.lt.smax) j=j+40
      if (p.gt.2500.0d0.and.y.lt.smax) j1=j
      if (j.eq.121) x0=p
      do l=1,2
         s1=0.0d0
         s2=0.0d0
         s3=0.0d0
         s4=0.0d0
         k=j+7
         kk=j+4
         do jj=kk,k
            s4=(s4+a(jj))*y1
            s3=(s3+a(jj+4))*y1
            s2=(s2+a(jj+8))*y1
            s1=(s1+a(jj+12))*y1
         enddo
         t0(l)=((a(j)*x0+a(j+1))*x0+a(j+2))*x0+a(j+3)+s4+x0*(s3+x0*(s2+x0*s1))
         j=j+20
         if (t0(1).lt.t) t0(1)=t
      enddo
!-----------------------------------------------------------------------
      if (t0(1).gt.1600.0d0) t0(1)=1600.0d0
      xgi(2)=hss67(p,t0(1),xgi(1),vcore)
      x0=xgi(i)
      if (p.gt.800.0d0.and.t0(1).lt.700.0d0) dmt=0.50d0*dmt
      pl=1.0d0
      s1=vcore
      s2=0.0d0
      s3=cp
      if (abs(y-x0).lt.tol(i)) goto 16
!-----------------------------------------------------------------------
      t1=t0(1)+cp*(y-x0)
      t=t1
      if (i.eq.2) goto 13
!-----------------------------------------------------------------------
      pl=t0(1)+459.670d0
      t1=t0(1)+(pl)*cp*(y-x0)*(1.0d0+cp*(y-x0)/2.0d0)
      t=t1
13    continue
      if (abs(t0(1)-t1).lt.0.0050d0.and.j1.eq.0) goto 18
!-----------------------------------------------------------------------
      if (abs(y-x0).lt.dmt.and.j1.eq.0) goto 18
!-----------------------------------------------------------------------
      xgi(2)=hss67(p,t1,xgi(1),vcore)
      x1=xgi(i)
      x2=x1
      s2=(vcore-s1)/(t1-t0(1))
      s1=vcore
14    continue
      s3=(t0(1)-t1)/(x0-x1)/pl
      if (icnt.gt.10) goto 20
!-----------------------------------------------------------------------
15    continue
      cp=a1*(a2*cp+s3)
      if (i.eq.1) pl=t1+459.670d0
      if (abs(y-x1).lt.tol(i).or.abs(y-x1).lt.0.20d0*dmt) goto 17
!-----------------------------------------------------------------------
      t=t1+cp*(y-x1)*pl
      if (t.gt.1600.0d0) t=1600.0d0
      xgi(2)=hss67(p,t,xgi(1),vcore)
      x2=xgi(i)
      s2=(vcore-s1)/(t-t1)
      s1=vcore
      if (abs(y-x2).lt.tol(i)) goto 17
!-----------------------------------------------------------------------
      icnt=icnt+1
      if (icnt.gt.100) goto 21
!-----------------------------------------------------------------------
      x0=x1
      t0(1)=t1
      x1=x2
      t1=t
      goto 14
!-----------------------------------------------------------------------
16    continue
      if (i.eq.1) pl=t0(1)+459.670d0
      x2=x0
      t=t0(1)
17    continue
      t0(1)=t
      t=t+s3*(y-x2)*pl
18    continue
      vx=t0(1)+0.50d0*(t-t0(1))+459.670d0
      if (i.eq.2) vx=1.0d0/vx
      m3i=3-i
      xgi(m3i)=xgi(m3i)+(y-xgi(i))*vx
      if (s2.eq.0.0d0) goto 19
!-----------------------------------------------------------------------
      vcore=vcore+s2*(t-t0(1))
      goto 2
!-----------------------------------------------------------------------
19    continue
      x1=hss167(p,t,x2,vcore)
      goto 2
!-----------------------------------------------------------------------
20    continue
      a1=0.10d0
      a2=9.0d0
      if (icnt.lt.30) goto 15
!-----------------------------------------------------------------------
      a1=0.020d0
      a2=49.0d0
      cp=cp*0.9750d0
      goto 15
!-----------------------------------------------------------------------
21    continue
      j=-15
      if (i.eq.1) goto 22
      j=-13
22    continue
      call ster67('TPS67',j,pres,entr)
!     cannot get here but tools complain TSP not defined if stop not added
      stop
!-----------------------------------------------------------------------
      end
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     tpsl67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!    FUNCTION tpsl67 (PRES,S)
!!
!!##DESCRIPTION
!!    function TPSL67 will calculate the temperature of
!!    the liquid that is at pressure P, and entropy S.
!!
!!##OPTIONS
!!
!!##RETURN VALUES
function tpsl67 (pres,s)
!  COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS
!  CORRECTED TOLERANCE CHECK SO THAT STATEMENT 9 DOES NOT BECOME
!  DIVIDE BY ZERO
doubleprecision            :: pres   ! WATER PRESSURE, IN PSIA.
doubleprecision,intent(in) :: s      ! SPECIFIC ENTROPY, IN BTU/LB-DEGF
doubleprecision            :: tpsl67 ! OUTPUT - TEMPERATURE, IN DEGREES F.
doubleprecision            :: tphl67
doubleprecision :: a
doubleprecision :: a1
doubleprecision :: a2
doubleprecision :: cp
doubleprecision :: dpdt
doubleprecision :: dt
doubleprecision :: factor
integer         :: i
integer         :: icnt
integer         :: j
integer         :: jj
integer         :: k
integer         :: l
integer         :: m3i
doubleprecision :: p
doubleprecision :: pl
doubleprecision :: pp
doubleprecision :: s1
doubleprecision :: s2
doubleprecision :: s3
doubleprecision :: s4
doubleprecision :: s5
doubleprecision :: score
doubleprecision :: ss
doubleprecision :: t
doubleprecision :: t0
doubleprecision :: t1
doubleprecision :: tcore
doubleprecision :: tol
doubleprecision :: tolera
doubleprecision :: tr
doubleprecision :: vcore
doubleprecision :: vx
doubleprecision :: x
doubleprecision :: x0
doubleprecision :: x1
doubleprecision :: x2
doubleprecision :: xgi
doubleprecision :: xm
doubleprecision :: xxi
doubleprecision :: y
doubleprecision :: z
!external hcl67
save
dimension tol(2), a(80), t0(2), xgi(2), xxi(2)
common /nust/ tcore,score,vcore,dpdt
common /liqu/ factor
equivalence (cp,t0(2))
data (tol(i),i=1,2) / 0.00004d0,0.04d0/
   data (a(i),i=1,76) /              &
     &    8.1268196d3,          &
     &   -4.4863246d4,          &
     &    9.0526738d4,          &
     &   -7.9007785d4,          &
     &   -2.8079503d3,          &
     &    1.4980173d4,          &
     &   -2.9022174d4,          &
     &    2.4390540d4,          &
     &    2.0379601d2,          &
     &   -9.9938507d2,          &
     &    1.7178961d3,          &
     &   -1.2293245d3,          &
     &    1.3948239d-1,         &
     &   -8.2580789d0,          &
     &    3.5190736d1,          &
     &   -4.8618442d1,          &
     &    2.1639986d1,          &
     &    3.0606384d2,          &
     &   -7.5414816d3,          &
     &    2.5265214d4,          &
     &    2.1113723d2,          &
     &   -1.0439922d3,          &
     &    1.9223348d3,          &
     &   -1.5629656d3,          &
     &   -7.3249464d1,          &
     &    3.5942109d2,          &
     &   -6.5679507d2,          &
     &    5.3009717d2,          &
     &    8.3095606d0,          &
     &   -4.0475485d1,          &
     &    7.3407222d1,          &
     &   -5.8801886d1,          &
     &   -3.0963437d-1,         &
     &    1.4964420d0,          &
     &   -2.6912588d0,          &
     &    2.1369643d0,          &
     &   -6.3245551d-1,         &
     &    1.7564286d1,          &
     &   -1.5952330d2,          &
     &    4.7464053d2,          &
     &    2.1407282d0,          &
     &   -5.9860018d1,          &
     &    6.2749847d2,          &
     &   -2.7772729d3,          &
     &   -8.1761199d-1,         &
     &    2.2555810d1,          &
     &   -2.3243170d2,          &
     &    1.0411113d3,          &
     &    9.1815203d-2,         &
     &   -2.4594782d0,          &
     &    2.4377800d1,          &
     &   -1.0342967d2,          &
     &   -3.0787656d-3,         &
     &    7.7767787d-2,         &
     &   -7.0824702d-1,         &
     &    2.6285284d0,          &
     &   -3.4059872d0,          &
     &    1.5917166d2,          &
     &   -1.7034799d3,          &
     &    4.3632563d3,          &
     &    3.5167232d-2,         &
     &   -9.0182976d-1,         &
     &    8.5488856d0,          &
     &   -3.5588020d1,          &
     &   -1.3105842d-2,         &
     &    3.3818633d-1,         &
     &   -3.2394273d0,          &
     &    1.3690536d1,          &
     &    1.6353816d-3,         &
     &   -4.2810332d-2,         &
     &    4.1778996d-1,         &
     &   -1.8068583d0,          &
     &   -6.7935319d-5,         &
     &    1.8092333d-3,         &
     &   -1.8019238d-2,         &
     &    7.9735881d-2 /
      data (a(i), i=77, 80) /   &
     &   -1.3189235d-1,         &
     &    2.9212226d0,          &
     &   -2.1568287d1,          &
     &    5.5997480d1 /
!-------------------------------------------------------------------
      j=1
      i=1
      z=1.0d0+s
      goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     tphl67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   entry tphl67(pres,s)
!!
!!##DESCRIPTION
!!
!!##OPTIONS
!!
!!##RETURN VALUES
entry tphl67(pres,s)
      i=2
      j=41
      z=(500.0d0+s)/100.0d0
!-------------------------------------------------------------------
1  continue
   tr=705.47d0
   a1=0.5d0
   a2=1.0d0
   dt=0.0005d0
   tolera=tol(i)
   x=s
   p=pres
   y=log(p)
   if (p.lt.200.0d0) y=log(200.0d0)
   if (p.gt.3208.2340d0) goto 2
   tr=tsl67(p)
   if (i.eq.1) goto 2
   if (hcl67(p,tr,ss).ge.s) goto 2
   tr=tslh67(x)
   pp=psl67(tr)
   if (p.lt.pp) pres=pp
   if (p.lt.pp) p=pres
   if (tr.gt.705.470d0) tr=705.470d0
2  continue
   icnt=1
   do l=1,2
      k=j+3
      s1=0.0d0
      s2=0.0d0
      s3=0.0d0
      s4=0.0d0
      s5=0.0d0
      do jj=j,k
         s5=s5*y+a(jj+16)
         s4=(s4+a(jj+12))*z
         s3=(s3+a(jj+8))*z
         s2=(s2+a(jj+4))*z
         s1=(s1+a(jj))*z
      enddo
      t0(l)=((s4*y+s3)*y+s2)*y+s1+s5
      j=j+20
      if (t0(1).gt.tr) t0(1)=tr
   enddo
   xgi(2)=hcl67(p,t0(1),xgi(1))
   x0=xgi(i)
   pl=1.0d0
   t1=t0(1)-cp*(x0-x)
   t=t1
   if (i.ne.1) goto 5
   pl=t0(1)+459.670d0
   t1=t0(1)-pl*cp*(x0-x)*(1.0d0-cp*(x0-x)/2.0d0)
   t=t1
5  continue
   if (factor.eq.1.0d0) goto 7
   if (t0(1).gt.480.0d0.and.i.eq.1) goto 8
   if (t0(1).le.650.0d0) goto 11
   if (t0(1).le.695.0d0) goto 8
   xxi(2)=hcl67(p,tr,xxi(1))
   xm=xxi(i)
   if (x.le.xm) goto 8
   j=15
   if (i.lt.2) goto 6
   j=13
6  continue
   call ster67 ('tpsl67',j,pres,s)
7  continue
   if (pres.le.800.0d0) tolera=tolera/10.0d0
   if (pres.lt.50.0d0) tolera=tolera/10.0d0
8  continue
   if (t1.gt.tr) t1=tr
   if (abs(t0(1)-t1).lt.0.0010d0) t1=t0(1)-0.0010d0
   xgi(2)=hcl67(p,t1,xgi(1))
   x1=xgi(i)
   x2=x1
   t=t1
   if (abs(x1-x).lt.tolera.or.abs(x0-x1).lt.tolera) goto 10
9  continue
   cp=a1*(a2*cp+(t0(1)-t1)/(x0-x1)/pl)
   if (i.eq.1) pl=t1+459.670d0
   t=t1+cp*(x-x1)*pl
   if (t.gt.tr) t=tr
   xgi(2)=hcl67(p,t,xgi(1))
   x2=xgi(i)
   if (abs(x2-x).lt.tolera) goto 10
   icnt=icnt+1
   if (icnt.gt.50) goto 12
   x0=x1
   t0(1)=t1
   x1=x2
   t1=t
   if (icnt.lt.5) goto 9
   a1=0.020d0
   a2=49.0d0
   cp=cp*0.9750d0
   if (t.eq.tr.and.x2.lt.x) tr=tr+dt
   pp=psl67(tr)
   if (pp.gt.p) tr=tr-dt
   if (tr.gt.705.470d0) tr=705.470d0
   dt=0.90d0*dt
   goto 9
10 continue
   t0(1)=t
   t=t+cp*(x-x2)*pl
11 continue
   vx=t0(1)+459.670d0+0.50d0*(t-t0(1))
   if (i.eq.2) vx=1.0d0/vx
   m3i=3-i
   xgi(m3i)=xgi(m3i)+(x-xgi(i))*vx
   goto 13
12 continue
   j=-15
   if (i.lt.2) goto 6
   j=-13
   goto 6
13 continue
   tpsl67=t
   m3i=3-i
   score=xgi(m3i)
   end function tpsl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     tsl67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   function tsl67(pin)
!!
!!##DESCRIPTION
!!    function tsl67 will calculate the saturation temperature
!!    of the saturated liquid at pressure p.
!!
!!     DEFINITION AND PURPOSE:
!!      o T=f(P)
!!      o Calculates saturation temperature.
!!
!!     REGION OF APPLICATION:
!!        Region 3. P .le. 3208.234 psia
!!
!!     CALLING SEQUENCE:
!!        T=TSL(P)
!!
!!##OPTIONS
!!      P      SATURATION PRESSURE, IN PSIA.
!!
!!##RETURN VALUES
!!      tsl67  WATER TEMPERATURE, IN DEGREES F.
function tsl67(pin)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: tsl167
doubleprecision :: tsl67
integer         :: i
integer         :: k
integer         :: n
doubleprecision :: b(12)
doubleprecision :: dp
doubleprecision :: dpdt
doubleprecision :: f
doubleprecision :: pa
doubleprecision :: pin
doubleprecision :: pr
doubleprecision :: score
doubleprecision :: tcore
doubleprecision :: tol
doubleprecision :: tsldum
doubleprecision :: tx
doubleprecision :: ty
doubleprecision :: tz
doubleprecision :: vcore
doubleprecision :: w
doubleprecision :: y
!external psl1
save
common /nust/tcore, score, vcore, dpdt
data(b(i), i = 1, 12)/     &
     &     1.52264682686d0,      &
     &    -0.682309517937d0,     &
     &     0.164114951728d0,     &
     &    -2.02321648831d-3,     &
     &    -1.92391110748d-3,     &
     &    -5.74549418696d-4,     &
     &     6.84115542402d-5,     &
     &     3.36500068426d-5,     &
     &    -1.23422483951d-5,     &
     &     1.48265501702d-6,     &
     &    -1.02116445578d-6,     &
     &    -4.09080904092d-6/
      k = 1
      goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     tsl167(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!       ENTRY tsl167(PIN)
!!
!!##DESCRIPTION
!!    function TSL167 will calculate the saturation
!!    temperature of the liquid at pressure P.
!!
!!##OPTIONS
!!    P        water pressure, in PSIA.
!!
!!##RETURN VALUES
!!    tsl167   water temperature, in degrees F.
      entry tsl167(pin)
      k = 2
1     continue
      i = 1
      tol = 1.0d-4
      if (k .eq. 2) tol = 1.0d-6
      if (pin .gt. 2700.0d0) tol = 1.d-9
      if (pin .gt. 3200.0d0) tol = 1.d-10
      f = 1.0d0
      if (pin .le. 3208.234765d0) then
         tsl67 = 705.47d0
         if (pin .lt. 3208.2347d0) then
            i =  -1
            tx = 1.0d0
            ty = (log(3529.058235d0/pin)**0.4d0 - 1.48047125d0)/( - 1.089944005d0)
            y = 2.0d0*ty
            w = b(1) + ty*b(2)
            do n = 3, 12
               tz = y*ty - tx
               w = w + tz*b(n)
               tx = ty
               ty = tz
            enddo
            tsl67 = 1165.14d0/w - 459.67d0
         endif
         ty = .01d0
         y = 1.0d0
         if (dble(tsl67) .gt. 705.47d0) tsl67 = 705.47d0
4        continue
         tsldum=tsl67  ! passing tsl67 hits a bug in the gfortran compiler (Nov 3 2006)
         pa = psl167(tsldum)
         dp = pin - pa
         pr = dp/pin
         tsl67 = tsl67 + f*dp/dpdt
         if (abs(pr) .lt. tol) goto 8
         if (y .le. 29.0d0) then
            y = y + 1.0d0
            f = f*0.99d0
            if (dble(tsl67 - 705.47d0) .ge. 0.0d0) then
               ty = 0.9d0*ty
               tsl67 = tsl67 - ty
            endif
            goto 4
         endif
      endif
      call ster67('tsl67', i, pin, 0.0d0)
8     continue
      end function tsl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     tslh67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   FUNCTION tslh67(ENTH)
!!
!!##DESCRIPTION
!!
!!     function tslh67 will calculate saturation temperature of the saturated
!!     liquid at enthalpy enth.
!!
!!     DEFINITION AND PURPOSE:
!!       o T=f(H)
!!       o Calculates saturation temperature
!!
!!     REGION OF APPLICATION:
!!         Liquid side of Region 3. Approximate limits are 0 .le. H .le. 906 BTU/lb
!!
!!     CALLING SEQUENCE:
!!         T=TSLH(H)
!!
!!##OPTIONS
!!    ENTH     specific enthalpy, in BTU/LB.
!!
!!##RETURN VALUES
!!    tslh67   water temperature, in degrees F.
function tslh67(enth)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: tslh67
doubleprecision :: a
doubleprecision :: cp
doubleprecision :: cpc
doubleprecision :: enth
doubleprecision :: h
doubleprecision :: h0
doubleprecision :: h1
doubleprecision :: h2
doubleprecision :: ht
integer         :: i
integer         :: j
doubleprecision :: t0
doubleprecision :: t1
      !external HSL67
      save
      dimension a(21), cpc(3), ht(3)
!     each group of 7 stored backward so that we can nest in do loop
      data(a(i), i = 1, 21)/    &
     &   6.50624227d-15,        &
     &   2.20388183d-14,        &
     &   1.89289928d-14,        &
     &  -7.64746293d-12,        &
     &  -7.31560395d-11,        &
     &  -5.74763307d-11,        &
     &   3.31206684d-9,         &
     &   9.96511247d-8,         &
     &   6.14790362d-8,         &
     &  -9.26975032d-7,         &
     &  -7.19728045d-5,         &
     &  -2.33527489d-5,         &
     &   1.19084945d-4,         &
     &   2.87953806d-2,         &
     &  -3.77447374d-3,         &
     &   9.95122605d-1,         &
     &  -5.08143066d0,          &
     &   5.73286854d0,          &
     &   3.20081040d1,          &
     &   5.62152207d2,          &
     &  -8.29637833d2/
      data(ht(i), i = 1, 3)/375.0d0, 707.0d0, 906.0d0/
      data(cpc(i), i = 1, 3)/.96195681d0, 7.82249768d-4, -2.09499151d-6/
      h = enth
      do j = 1, 3
         if (h .le. ht(j)) goto 3
      enddo
      call ster67('tslh67', 3, enth, 0.0d0)
      goto 2
3     continue
      if (h .lt. 904.0d0) then
         t0 = a(j)
         do i = j, 18, 3
            t0 = t0*h + a(i + 3)
         enddo
         tslh67 = t0
         if (h .le. 716.0d0) then
            if (h .lt. 337.0d0 .or. h .gt. 356.0d0) goto 2
         endif
         if (t0 .gt. 705.47d0) t0 = 705.47d0
         t1 = 0.0d0
         h0 = hsl67(t0)
         h2 = h0
         cp = (cpc(3)*h + cpc(2))*h + cpc(1)
         if (cp .lt. .01d0) cp = .01d0
9        continue
         if (abs(h2 - h) .ge. 0.04d0) then
            if (t1 .le. 0.0d0) then
               t1 = t0 + cp*(h - h0)
               tslh67 = t1
               if (t1 .gt. 705.47d0) t1 = 705.47d0
               h1 = hsl67(t1)
               if (abs(h1 - h) .lt. 0.04d0) goto 2
               cp = (t0 - t1)/(h0 - h1)
            else
               if (tslh67 .lt. 705.3d0) then
                  cp = 0.5d0*(cp + (t1 - tslh67)/(h1 - h2))
               else
                  cp = 0.9d0*cp
               endif
               t0 = t1
               h0 = h1
               t1 = tslh67
               h1 = h2
            endif
            tslh67 = t0 + (h - h0)*cp
            if (tslh67 .gt. 705.47d0) tslh67 = 705.47d0
            h2 = hsl67(tslh67)
            goto 9
         endif
      else
         tslh67 = 705.46831d0 + (h - 904.0d0)*.000845d0
      endif
2     continue
      end function tslh67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     visl67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   FUNCTION visl67(P, T)
!!
!!##DESCRIPTION
!!    function VISL67 will calculate the viscosity of
!!    the liquid at pressure P and temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!       o VISC=f(P,T)
!!       o Calculates viscosity
!!
!!     REGION OF APPLICATION:
!!         Region 1 with P[max] = 800 bars = 11,603.01904 psia
!!
!!     CALLING SEQUENCE:
!!         VISC = VISL(P,T)
!!
!!##OPTIONS
!!    P        water pressure, in PSIA.
!!    T        water temperature, in degrees F.
!!
!!##RETURN VALUES
!!    visl67   viscosity, in LB/FT-SEC.
function visl67(p, t)
doubleprecision :: visl67
doubleprecision :: visv67
doubleprecision :: b
doubleprecision :: c0
doubleprecision :: c0t
doubleprecision :: c1
doubleprecision :: c1t
doubleprecision :: c2
doubleprecision :: c2t
doubleprecision :: c3t
doubleprecision :: dens
integer         :: i
doubleprecision :: p
doubleprecision :: t
doubleprecision :: tau
doubleprecision :: tau10
doubleprecision :: u1
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
      !external VCL67
      save
      dimension b(3), c0(5), c1(8), c2(10)
      data(b(i), i = 1, 3)/    &
     &      1.021d2,           &
     &      6.765d2,           &
     &      3.53d2/
      data(c0(i), i = 1, 5)/   &
     &      7.5213978d1,       &
     &     -1.1879721d3,       &
     &      6.6840006d3,       &
     &     -1.5488609d4,       &
     &      1.1802979d4/
      data(c1(i), i = 1, 8)/   &
     &     -4.1416918d1,       &
     &      9.1177974d2,       &
     &     -8.2709230d3,       &
     &      3.9905545d4,       &
     &     -1.0974987d5,       &
     &      1.6919860d5,       &
     &     -1.3050794d5,       &
     &      3.6396424d4/
      data(c2(i), i = 1, 10)/  &
     &     -1.1651331d-1,      &
     &      1.1075709d1,       &
     &     -1.7932076d2,       &
     &      1.1112731d3,       &
     &     -2.0826382d3,       &
     &     -8.1306942d3,       &
     &      5.0618906d4,       &
     &     -1.0609301d5,       &
     &      9.8673349d4,       &
     &     -3.1905419d4/
      dens = vcl67(p, t)
      goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     visv67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   ENTRY visv67(P, T)
!!
!!##DESCRIPTION
!!
!!    function VISV67 will calculate the viscosity of
!!    the liquid at pressure P and temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!           o VISC=f(P,T)
!!           o Calculates viscosity
!!
!!     REGION OF APPLICATION:
!!         Region 2 with P[max] = 800 bars = 11,603.01904 psia T[max] = 700 C =
!!         1292 F
!!
!!     CALLING SEQUENCE:
!!         VISC=VISV(P,T)
!!
!!##OPTIONS
!!    P        water pressure, in PSIA.
!!    T        water temperature, in degrees F.
!!##RETURN VALUES
!!    visv67   viscosity, in LB/FT-SEC.
      entry visv67(p, t)
      c1t = hss67(p, t, c2t, dens)
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
1     continue
      tau = (t - 32.0d0)/180.0d0
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      dens = 0.0160184634d0/dens
      u1 = 0.0d0
      do i = 1, 3
         u1 = (u1 + b(i))*dens
      enddo
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      tau10 = tau**10
      c3t = 1.0d0 + 5.739225024d-32*tau10*tau10*tau10*tau10*tau10*tau**5
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      c1t = c1(1)
      do i = 2, 8
         c1t = c1t*tau + c1(i)
      enddo
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      c0t = c0(1)
      do i = 2, 5
         c0t = c0t*tau + c0(i)
      enddo
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      c2t = c2(1)
      do i = 2, 10
         c2t = c2t*tau + c2(i)
      enddo
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      u1= u1 + 80.4d0 + 40.7d0 * tau + (-0.8d0 + dens * ((c2t * dens + c1t) * dens + c0t)) / c3t
      visl67 = 0.671968975d-7*u1
      end function visl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     hcl67(3f) - [M_steam67] calculate specific enthalpy and entropy of water at pressure P and temp T
!!
!!##SYNOPSIS
!!
!!    FUNCTION hcl67(P, T, S)
!!
!!##DESCRIPTION
!!   function HCL67 will calculate the specific enthalpy
!!   and entropy of water at pressure P and temp T.
!!
!!     DEFINITION AND PURPOSE:
!!           o H,S=f(P,T)
!!           o Calculates specific enthalpy and specific entropy
!!
!!     REGION OF APPLICATION:
!!         Region 1 with P[max] = 1000 bars = 14,503.7738 psia
!!
!!     CALLING SEQUENCE:
!!         H = HCL(P,T,S)
!!
!!##OPTIONS
!!
!!##RETURN VALUES
function hcl67(p, t, s)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_24="@(#) M_steam67 hcl67(3f) calculate specific enthalpy and entropy of water at pressure P and temp T"

doubleprecision             :: hcl67  ! specific enthalpy, in BTU/LB.
doubleprecision,intent(in)  :: p      ! water pressure, in PSIA.
doubleprecision,intent(in)  :: t      ! water temperature, in degrees F.
doubleprecision,intent(out) :: s      ! specific entropy, in BTU/LB-DEGF
doubleprecision             :: dum
save
   if (p .gt. 16000.0d0) then
      call ster67('hcl67', 12, p, t)
   else
      hcl67 = hcsl67(p, t, dum, s, 4)
   endif
end function hcl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     hsl67(3f) - [M_steam67] calculate specific enthalpy of the saturated liquid at temperature T
!!
!!##SYNOPSIS
!!
!!    FUNCTION hsl67(T)
!!
!!##DESCRIPTION
!!    function hsl67 will calculate the specific enthalpy
!!    of the saturated liquid at temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!       o H = f(T)
!!       o Calculates specific enthalpy of saturated liquid.
!!
!!     REGION OF APPLICATION:
!!         Liquid Side of Region 3 with T such that 32 .le. T .le. 705.47 F
!!
!!     CALLING SEQUENCE:
!!         H=HSL(T)
!!
!!##OPTIONS
!!
!!##RETURN VALUES
function hsl67(t)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision,intent(in) :: t     ! WATER TEMPERATURE, IN DEGREES F
doubleprecision            :: hsl67 ! OUTPUT - SPECIFIC ENTHALPY, IN BTU/LB.
doubleprecision            :: ssl67 ! OUTPUT - SPECIFIC ENTROPY, IN BTU/LB-DEGF
doubleprecision            :: vsl67 ! OUTPUT - SPECIFIC VOLUME.
integer                    :: i
doubleprecision            :: xg(3)
   i = 1
   goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     ssl67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!    ENTRY ssl67(T) calculate the specific entropy of the saturated liquid at temperature T
!!
!!##DESCRIPTION
!!    function ssl67 will calculate the specific entropy
!!    of the saturated liquid at temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!           o S=f(T)
!!           o Calculates specific entropy of saturated liquid
!!
!!     REGION OF APPLICATION:
!!         Liquid side of Region 3. 32 .le. T .le. 705.47 F
!!
!!     CALLING SEQUENCE:
!!         S=SSL(T)
!!
!!##OPTIONS
!!
!!##RETURN VALUES
   entry ssl67(t)
   i = 2
   goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     vsl67(3f) - [M_steam67] calculate specific volume on the liquid side of the saturation line
!!
!!##SYNOPSIS
!!
!!    ENTRY vsl67(T) calculate specific volume on the liquid side of the saturation line of the liquid at temperature T
!!
!!##DESCRIPTION
!!    function VSL67 will calculate the specific volume
!!    on the liquid side of the saturation line of the
!!    liquid at temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!       o V=f(T)
!!       o Calculates specific volume on liquid side of saturation line
!!
!!     REGION OF APPLICATION:
!!         Liquid side of Region 3. 32 .le. T .le. 705.47 F
!!
!!     CALLING SEQUENCE:
!!         V=VSL(T)
!!
!!##OPTIONS
!!
!!##RETURN VALUES
entry vsl67(t)
   i = 3
   goto 1
!-----------------------------------------------------------------------
1  continue
   xg(1) = hcsl67(psl67(t), t, xg(3), xg(2), i)
   hsl67 = xg(i)
end function hsl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     hsv67(3f) - [M_steam67] specific enthalpy, temperature, specific entropy, and specific volume of saturated vapor at pressure P.
!!
!!##SYNOPSIS
!!
!!    FUNCTION hsv67(P, T, S, V)
!!
!!##DESCRIPTION
!!    function HSV67 will calculate specific enthalpy,
!!    temperature, specific entropy, and the specific
!!    volume of saturated vapor at pressure P.
!!
!!     DEFINITION AND PURPOSE:
!!           o H,T,S.V,=f(P)
!!           o Calculates specific enthalpy, temperature, specific entropy, and
!!             specific volume of saturated vapor.
!!
!!     REGION OF APPLICATION:
!!         Vapor side of Region 3. P .le. 3208.234 psia
!!
!!     CALLING SEQUENCE:
!!         H=HSV(P,T,S,V)
!!
!!##OPTIONS
!!    P        water pressure, in PSIA.
!!
!!##RETURN VALUES
!!    T        water temperature, in degrees F.
!!    S        specific entropy, in BTU/LB-DEGF
!!    V        specific volume.
!!    hsv67    specific enthalpy, in BTU/LB.
function hsv67(p, t, s, v)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: hsv67
doubleprecision :: p
doubleprecision :: t
doubleprecision :: s
doubleprecision :: v
save
   if (p .gt. 3208.234765d0) then
      call ster67('hsv67', 1, p, 0.0d0)
   else
      t = tsl67(p)
      hsv67 = hss67(p, t, s, v)
   endif
end function hsv67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     prliq67(3f) - [M_steam67] compute the Prandtl number of the liquid at pressure P and temperature T in regsion 1
!!
!!##SYNOPSIS
!!
!!    FUNCTION prliq67(P, T)
!!
!!##DESCRIPTION
!!    Function prliq67 will compute the Prandtl number
!!    of the liquid at pressure P and temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!           o PR=f(P,T)
!!           o Calculates Prandtl number
!!
!!     REGION OF APPLICATION:
!!         Region 1 with P[max] = 500 bars = 7251.8869 psia
!!
!!     CALLING SEQUENCE:
!!         PR=PRLIQ(P,T)
!!
!!##OPTIONS
!!
!!##RETURN VALUES
function prliq67(p, t)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision            :: prliq67 ! output - prandtl number (dimensionless)
doubleprecision,intent(in) :: p       ! water pressure, in PSIA.
doubleprecision,intent(in) :: t       ! water temperature, in degrees F.
   prliq67 = 3600.0d0*cpl67(p, t)*visl67(p, t)/condl67(p, t)
end function prliq67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     prstm67(3f) - [M_steam67] compute the Prandtl number of the liquid at pressure P and temperature T. in Region 2
!!
!!##SYNOPSIS
!!
!!   FUNCTION prstm67(P, T)
!!
!!##DESCRIPTION
!!    function PRSTM67 will compute the Prandtl number
!!    of the liquid at pressure P and temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!           o PR=f(P,T)
!!           o Calculates Prandtl number
!!
!!     REGION OF APPLICATION:
!!         Region 2 with P[max] = 500 bars = 7251.8869 psia T[max] = 700 C = 1292 F
!!
!!     CALLING SEQUENCE:
!!         PR=PRSTM(P,T)
!!
!!##OPTIONS
!!   P        water pressure, in PSIA.
!!   T        water temperature, in degrees F.
!!
!!##RETURN VALUES
!!   prstm67  Prandtl number (dimensionless)
function prstm67(p, t)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: prstm67
doubleprecision :: p
doubleprecision :: t
doubleprecision :: v
   prstm67 = 3600.0d0*cpv67(p,t,v)*visv67(p,t)/condv67(p,t)
end function prstm67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     sssicl67(3f) - [M_steam67] calculate specific entropy and temperature of the liquid at pressure P and enthalpy H.
!!
!!##SYNOPSIS
!!
!!  FUNCTION sssicl67(PRES, H, TEMP)
!!
!!##DESCRIPTION
!!    function sssicl67 will calculate specific entropy
!!    and temperature of the liquid at pressure P
!!    and enthalpy H.
!!
!!     DEFINITION AND PURPOSE:
!!      o S,T=f(P,H)
!!      o Calculates specific entropy and temperature
!!
!!     REGION OF APPLICATION:
!!         Region 1 with P[max] = 1000 bars.
!!         See Section 4.5 for limits on H
!!
!!     CALLING SEQUENCE:
!!         S=SSICL(P,H,T)
!!
!!##OPTIONS
!!    P       water pressure, in PSIA.
!!    H       specific enthalpy, in BTU/POUND.
!!
!!##RETURN VALUES
!!    T          water temperature, in degrees F.
!!    sssicl67   specific entropy, in BTU/LB-DEGF
function sssicl67(pres, h, temp)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: hssicl67
doubleprecision :: sssicl67
doubleprecision :: dpdt
doubleprecision :: factor
doubleprecision :: h
doubleprecision :: p
doubleprecision :: pres
doubleprecision :: s
doubleprecision :: score
doubleprecision :: t
doubleprecision :: tcore
doubleprecision :: temp
doubleprecision :: vcore
save
common /nust/tcore, score, vcore, dpdt
common /liqu/factor
   p = pres
   factor = 0.0d0
   t = tphl67(p, h)
goto 1
!==================================================================================================================================!
!||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||!
!==================================================================================================================================!
!>
!!##NAME
!!     hssicl67(3f) - [M_steam67] compute specific enthalpy and temperature of water at pressure P and entropy S
!!
!!##SYNOPSIS
!!
!!   ENTRY hssicl67(PRES, S, TEMP)
!!
!!##DESCRIPTION
!!    function hssicl67 will compute specific enthalpy
!!    and temperature of water at pressure P and
!!    entropy S.
!!
!!     DEFINITION AND PURPOSE:
!!           o H,T,V,X = f(P,S)
!!           o Calculates specific enthalpy and temperature
!!
!!     REGION OF APPLICATION:
!!         Region 1 with P[max] = 1000 bars.
!!         See Section 4.6 for limits on S.
!!
!!     CALLING SEQUENCE:
!!         H=HSSICL(P,S,T)
!!
!!##OPTIONS
!!    PRES       water pressure, in PSIA.
!!    S          specific entropy, in BTU/LB-DEGF
!!
!!##RETURN VALUES
!!    TEMP       temperature, in degrees F.
!!    hssicl67   enthalpy, in BTU/POUND.
entry hssicl67(pres, s, temp)
   p = pres
   factor = 0.0d0
   t = tpsl67(p, s)
1  continue
   temp = t
   sssicl67 = score
end function sssicl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     vcl67(3f) - [M_steam67] compute the specific volume of the liquid at pressure P and temperature T
!!
!!##SYNOPSIS
!!
!!   FUNCTION vcl67(p, t)
!!
!!    doubleprecision,intent(in) :: p
!!    doubleprecision,intent(in) :: t
!!    doubleprecision            :: vcl67
!!
!!##DESCRIPTION
!!    Function vcl67 will compute the specific volume
!!    of the liquid at pressure P and temperature T.
!!
!!     DEFINITION AND PURPOSE:
!!        o V=f(P,T)
!!        o Calculates specific volume
!!
!!     REGION OF APPLICATION:
!!         Region 1 with P[max] = 1000 bars = 14,503.7738 psia
!!
!!     CALLING SEQUENCE:
!!         V=VCL(P,T)
!!
!!##OPTIONS
!!    p      water pressure, in PSIA.
!!    t      water temperature, in degrees F.
!!
!!##RETURN VALUES
!!    vcl67  specific volume.
function vcl67(p, t)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision,intent(in) :: p     !  water pressure, in PSIA.
doubleprecision,intent(in) :: t     !  water temperature, in degrees F.
doubleprecision            :: vcl67 !  specific volume.
doubleprecision            :: dum
doubleprecision            :: vcldum
   dum = hcsl67(p, t, vcldum, dum, 5)
   vcl67= vcldum
end function vcl67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     gr167(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!       FUNCTION GR167(T, X, N)
!!
!!##DESCRIPTION
!!      function gr167
!!
!!##OPTIONS
!!      T(2)   input
!!      X      input
!!      N      input
!!
!!##RETURN VALUES
!!      GR167  output
function gr167(t, x, n)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision,intent(in) :: x
integer,intent(in)         :: n
doubleprecision,intent(in) :: t(2*n) !  was dimension t(2), but highest subscript needed is (2*n) : JSU

integer                    :: i
integer                    :: k
doubleprecision            :: gr167
!-----------------------------------------------------------------------------------------------------------------------------------
   do i = 2, n
      k = i + i
      if (x .le. t(k - 1)) exit
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if(n.lt.2)then
      call gotoer67('GOTOER67 (A) N is too small',n)
   else
      gr167 = t(k-2) + (t(k) - t(k-2)) / (t(k-1) - t(k-3)) * (x-t(k-3))
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
end function gr167
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     grs67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!      function grs67
!!
!!##OPTIONS
!!      X       input
!!      NDX     input
!!      Y       input
!!      NDY     input
!!      XV      input
!!      N       input
!!
!!##RETURN VALUES
!!      NRANGE  output
!!      GRS67   output
function grs67(x, ndx, y, ndy, xv, n, nrange)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: grs67
doubleprecision :: dx
doubleprecision :: dy
integer         :: i
integer         :: n
integer         :: n1
integer         :: n2
integer         :: n3
integer         :: n4
integer         :: ndx
integer         :: ndy
integer         :: np
integer         :: nrange
doubleprecision :: r
doubleprecision :: x
doubleprecision :: xv
doubleprecision :: y
doubleprecision :: yp
   save
!-ju  DIMENSION X(NDX,3), Y(NDY,3), DX(3), DY(3), YP(2)
   dimension x(ndx, *), y(ndy, *), dx(3), dy(3), yp(2)
   nrange = 0
   if (xv .lt. x(1, 1)) nrange =  - 1
   if (xv .gt. x(1, n)) nrange =  + 1
   do i = 1, n
      if (xv .lt. x(1, i)) goto 2
      if (xv .eq. x(1, i)) goto 8
   enddo
   i = n
2  continue
   if (i .gt. 2) then
      if (i .lt. n) then
         np = 4
         n4 = i + 1
      else
         np = 3
      endif
      n1 = i - 2
      n2 = i - 1
      n3 = i
   else
      n1 = 3
      n2 = 2
      n3 = 1
      np = 3
   endif
   dx(1) = x(1, n2) - x(1, n1)
   dy(1) = y(1, n2) - y(1, n1)
   dx(2) = x(1, n3) - x(1, n2)
   dy(2) = y(1, n3) - y(1, n2)
   r = (xv - x(1, n2))/dx(2)
   yp(1) = (dy(1)*dx(2)**2 + dy(2)*dx(1)**2)/(dx(1)*(dx(1) + dx(2)))
   if (np .eq. 4) then
      dx(3) = x(1, n4) - x(1, n3)
      dy(3) = y(1, n4) - y(1, n3)
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      yp(2)=(dy(2)*dx(3)**2+dy(3)*dx(2)**2)/(dx(3)*(dx(2)+dx(3)))
      grs67 = y(1,n2)+r*(yp(1)+r*(3.0d0*dy(2)-2.0d0*yp(1)-yp(2)+r *(yp(1) + yp(2) - 2.0d0*dy(2))))
   else
      grs67 = y(1, n2) + r*(yp(1) + r*(dy(2) - yp(1)))
   endif
   goto 9
8  continue
   grs67 = y(1, i)
9  continue
end function grs67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     p23t67(3f) - [M_steam67] calculate the pressure at the boundary of regions 2 and 3, given temperature T.
!!
!!##SYNOPSIS
!!
!!   FUNCTION p23t67(TIN)
!!
!!##DESCRIPTION
!!    function p23t67 will calculate the pressure at the
!!    boundary of regions 2 and 3, given temperature T.
!!
!!##OPTIONS
!!    T         temperature, in degrees F.
!!
!!##RETURN VALUES
!!    p23t67    pressure at boundary, in PSIA.
function p23t67(tin)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

! ident_25="@(#) [M_steam67] calculate the pressure at the boundary of regions 2 and 3 given temperature T."

doubleprecision :: p23t67
doubleprecision :: tin
doubleprecision :: t
doubleprecision :: theta
save
!     argument is temperature
!     returns with pressure
!     boundary between regions 2 and 3
   t = tin
   p23t67 = 0.0d0
   if (t .ge. 32.0d0) then
      if (t .gt. 1600.0d0) call ster67(' p23t67', 2, t, 0.0d0)
      theta = (t + 459.67d0)/1165.14d0
      p23t67 = (1.574373327d1 + theta*( - 3.417061978d1 + theta *1.931380707d1))*3208.234759d0
   endif
end function p23t67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     vliq67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!     FUNCTION vliq67 WILL CALCULATE
!!
!!##OPTIONS
!!     PIN       water pressure, in ATMOSPHERES.
!!     TIN       water temperature, in DEGREES C.
!!
!!##RETURN VALUES
!!     VMIN      output
!!     VMAX      output
!!     vliq67    output
function vliq67(pin, tin, vmin, vmax)
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: vliq67
integer         :: i
integer         :: j
doubleprecision :: pin
doubleprecision :: pt
doubleprecision :: t
doubleprecision :: tin
doubleprecision :: tt
doubleprecision :: v
doubleprecision :: vmax
doubleprecision :: vmin
doubleprecision :: vt
doubleprecision :: vx
save
dimension t(8), v(8), vt(30), pt(6), tt(6), vx(6)
data(vt(i), i = 1, 30)/    &
     & 1.568d0,  &
     & 1.612d0,  &
     & 1.661d0,  &
     & 1.717d0,  &
     & 1.803d0,  &
     & 1.864d0,  &
     & 1.480d0,  &
     & 1.504d0,  &
     & 1.535d0,  &
     & 1.567d0,  &
     & 1.598d0,  &
     & 1.628d0,  &
     & 1.424d0,  &
     & 1.443d0,  &
     & 1.462d0,  &
     & 1.487d0,  &
     & 1.512d0,  &
     & 1.529d0,  &
     & 1.349d0,  &
     & 1.362d0,  &
     & 1.374d0,  &
     & 1.393d0,  &
     & 1.416d0,  &
     & 1.420d0,  &
     & 1.30d0,   &
     & 1.312d0,  &
     & 1.324d0,  &
     & 1.336d0,  &
     & 1.354d0,  &
     & 1.376d0/
      data(pt(i), i = 1, 6)/     &
     &    0.0d0,         &
     &  276.0d0,         &
     &  414.0d0,         &
     &  552.0d0,         &
     &  828.0d0,         &
     & 1000.0d0/
      data(tt(i), i = 1, 6)/     &
     &  348.89d0,        &
     &  354.45d0,        &
     &  360.0d0,         &
     &  365.55d0,        &
     &  371.11d0,        &
     &  375.0d0/
      data(t(i), i=1, 8)/        &
     &  350.0d0,         &
     &  360.0d0,         &
     &  370.0d0,         &
     &  371.0d0,         &
     &  372.0d0,         &
     &  373.0d0,         &
     &  374.0d0,         &
     &  374.15d0/
      data(v(i), i = 1, 8)/      &
     &    1.741d0,       &
     &    1.894d0,       &
     &    2.22d0,        &
     &    2.29d0,        &
     &    2.38d0,        &
     &    2.51d0,        &
     &    2.80d0,        &
     &    3.17d0/
!     pin must be in bars and tc in degrees c
!     v will be in cc/gm
   pt(1) = (psl67(tin*1.8d0 + 32.0d0))/14.503773773d0
   vx(1) = grs67(t(1), 1, v(1), 1, tin, 8, i)
   do i = 2, 6
      vx(i) = grs67(tt(1), 1, vt(6*i - 11), 1, tin, 6, j)
   enddo
   vliq67 = grs67(pt(1), 1, vx(1), 1, pin, 6, i)
   vmin = 0.98d0*vx(6)
   vmax = vx(1)*1.01d0
end function vliq67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     zsdh67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!       FUNCTION ZSDH67(P1, P2, H1, S, T1, T2, X1, X2, V1, V2)
!!
!!##DESCRIPTION
!!
!!    function zsdh67 will calculate remaining variables
!!    at the extremities of an isentropic process given
!!    inlet and exit pressures, and inlet enthalpy in
!!    the superheated or wet steam region.
!!
!!##OPTIONS
!!   P1      inlet pressure in PSIA
!!   P2      exit pressure in PSIA
!!   H1      inlet enthalpy in BTU/LB.
!!
!!##RETURN VALUES
!!   zsdh67  exit enthalpy in BTU/LB.
!!   S       entropy in BTU/LB.-DEGREES F.
!!   T1      inlet temperature in DEGREES F.
!!   T2      exit temperature in DEGREES F.
!!   X1      inlet quality
!!   X2      exit quality
!!   V1      inlet specific volume in CUBIC FEET/LB.
!!   V2      exit specific volume in CUBIC FEET/LB.
function zsdh67(p1, p2, h1, s, t1, t2, x1, x2, v1, v2)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: zsdh67
doubleprecision :: h1
doubleprecision :: p1
doubleprecision :: p2
doubleprecision :: s
doubleprecision :: t1
doubleprecision :: t2
doubleprecision :: v1
doubleprecision :: v2
doubleprecision :: x1
doubleprecision :: x2
   s = sssiss67(p1, h1, t1, v1, x1)
   zsdh67 = hssiss67(p2, s, t2, v2, x2)
end function zsdh67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     zsdt67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   FUNCTION zsdt67(P1, P2, T1, S, H1, T2, X1, X2, V1, V2)
!!
!!##DESCRIPTION
!!
!!    function zsdt67 will calculate remaining variables
!!    at the extremities of an isentropic process given
!!    inlet and exit pressures, and inlet temperature
!!    in the superheated or wet steam region.
!!
!!
!!##OPTIONS
!!    P1  inlet pressure in PSIA
!!    P2  exit pressure in PSIA
!!    T1  inlet temperature in degrees F.
!!
!!##RETURN VALUES
!!    zsdt67  exit enthalpy in BTU/LB.
!!    S       entropy in BTU/LB.-DEGREES F.
!!    H1      inlet enthalpy in BTU/LB.
!!    T2      exit temperature in degrees F.
!!    X1      inlet quality
!!    X2      exit quality
!!    V1      inlet specific volume in CUBIC FEET/LB.
!!    V2      exit specific volume in CUBIC FEET/LB.
function zsdt67(p1, p2, t1, s, h1, t2, x1, x2, v1, v2)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: zsdt67
doubleprecision :: h1
doubleprecision :: p1
doubleprecision :: p2
doubleprecision :: s
doubleprecision :: t1
doubleprecision :: t2
doubleprecision :: v1
doubleprecision :: v2
doubleprecision :: x1
doubleprecision :: x2
   x1 = 1.0d0
   h1 = hss67(p1, t1, s, v1)
   zsdt67 = hssiss67(p2, s, t2, v2, x2)
end function zsdt67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     zsrh67(3f) - [M_steam67] calculate remaining variables at extremities of isentropic process
!!                  given inlet,exit P, inlet enthalpy in compressed liquid region.
!!
!!##SYNOPSIS
!!
!!   FUNCTION zsrh67(P1, P2, H1, S, T1, T2)
!!
!!##DESCRIPTION
!!
!!    function zsrh67 will calculate remaining variables
!!    at the extremities of an isentropic process given
!!    inlet and exit pressures, and inlet enthalpy in
!!    the compressed liquid region.
!!
!!          H2 = zsrh67(P1,P2,H1,S,T1,T2)
!!
!!##OPTIONS
!!    P1      inlet pressure in PSIA
!!    P2      exit pressure in PSIA
!!    H1      inlet enthalpy in BTU/LB.
!!
!!##RETURN VALUES
!!    zerh67  exit enthalpy in BTU/LB.
!!    S       entropy in BTU/LB.-DEGREES F.
!!    T1      inlet temperature in degrees F.
!!    T2      exit temperature in degrees F.
function zsrh67(p1, p2, h1, s, t1, t2)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.

! ident_26="@(#) M_steam67 zsrh67(3f) calculate remaining variables at extremities of isentropic process in compressed liquid region."

doubleprecision :: h1
doubleprecision :: p1
doubleprecision :: p2
doubleprecision :: s
doubleprecision :: t1
doubleprecision :: t2
doubleprecision :: zsrh67
   s = sssicl67(p1, h1, t1)
   zsrh67 = hssicl67(p2, s, t2)
end function zsrh67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     zsrt67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!   FUNCTION zsrt67(P1, P2, T1, S, H1, T2)
!!
!!##DESCRIPTION
!!    function zsrt67 will calculate remaining variables
!!    at the extremities of an isentropic process given
!!    inlet and exit pressures, and inlet temperature
!!    in the compressed liquid region.
!!
!!##OPTIONS
!!    P1      inlet pressure in PSIA
!!    P2      exit pressure in PSIA
!!    T1      inlet temperature in degrees F.
!!
!!##RETURN VALUES
!!    zert67  exit enthalpy in BTU/LB.
!!    S       entropy in BTU/LB.-DEGREES F.
!!    H1      inlet enthalpy in BTU/LB.
!!    T2      exit temperature in degrees F.
function zsrt67(p1, p2, t1, s, h1, t2)
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
!external HCL67
doubleprecision :: zsrt67
doubleprecision :: h1
doubleprecision :: p1
doubleprecision :: p2
doubleprecision :: s
doubleprecision :: t1
doubleprecision :: t2
save
   h1 = hcl67(p1, t1, s)
   zsrt67 = hssicl67(p2, s, t2)
end function zsrt67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!
!   XXXXXXX XXXXXX  XXXXXX    XXX   XXXXXX          XXXXXX    XXX   XXX XXX XXXXXXX  XXXXX  XX  XXX XXXXXXX  XXXXX
!    X    X  X    X  X    X  X   X   X    X          X    X  X   X   X   X  X  X  X    X     X   X   X    X X     X
!    X       X    X  X    X X     X  X    X          X    X X     X  X   X     X       X     XX  X   X      X
!    X  X    X    X  X    X X     X  X    X          X    X X     X  X   X     X       X     XX  X   X  X   X
!    XXXX    XXXXX   XXXXX  X     X  XXXXX           XXXXX  X     X  X   X     X       X     X X X   XXXX    XXXXX
!    X  X    X  X    X  X   X     X  X  X            X  X   X     X  X   X     X       X     X  XX   X  X         X
!    X       X  X    X  X   X     X  X  X            X  X   X     X  X   X     X       X     X  XX   X            X
!    X    X  X   X   X   X   X   X   X   X           X   X   X   X   X   X     X       X     X   X   X    X X     X
!   XXXXXXX XXX  XX XXX  XX   XXX   XXX  XX         XXX  XX   XXX     XXX     XXX    XXXXX  XXX  X  XXXXXXX  XXXXX
!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     gotoer67(3f) - [M_steam67] print error message and stop program
!!
!!##SYNOPSIS
!!
!!   SUBROUTINE GOTOER67(NAME, I)
!!
!!    CHARACTER(len=*),intent(in) :: NAME
!!    INTEGER,intent(in)          :: I
!!
!!##DESCRIPTION
!!    print error message and stop program
!!
!!##OPTIONS
!!
!!##RETURN VALUES
subroutine gotoer67(name, i)
! Copyright (c) 1990 Westinghouse Electric Corporation.
!
!     when an error occurs in a computed goto, print a string
!     of the form " ** error in routine_name (letter) varname = iiiii "
!     to standard error on unicos, to logfile on cos

! ident_27="@(#) M_steam67 gotoer67(3fp) print error message and stop program"

character(len=*) :: name, string*80
integer i
   write (string, "(' ** ERROR IN ', A, I5)") name(:max(len(name), 70)), i
   call zqjrem67(' STEAM TABLE ERROR')
   call zqjrem67(string)
   stop
end subroutine gotoer67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     ster67(3f) - [M_steam67]
!!
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!      subroutine ster67 is called from throughout the
!!      steam table routines to print error messages
!!      associated with steam table problems. ster67 will
!!      terminate execution of the executing program.
!!
!!##OPTIONS
!!      NAME   name of the calling routine.
!!      I      flag for data to be printed.
!!      A      datum item 1 to be printed.
!!      B      datum item 2 to be printed.
!!
!!##RETURN VALUES
subroutine ster67(name, i, a, b)
!  Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: a
doubleprecision :: b
integer         :: i
integer         :: i1
integer         :: i2
integer         :: im
integer         :: j
character ma(5)*6, m(2)*18, name*(*), mp*6
save ma, m
data m/' OUT OF RANGE IN  ', 'NON CONVERGENT IN '/
data(ma(j), j = 1, 5)/'PRESS=','TEMP=','ENTH=','VOL=','ENTR='/
   im = 1
   if (i .lt. 0) im = 2
   i2 = iabs(i)
   i1 = i2/10
   i2 = i2 - i1*10
   mp = ma(i2)
   if (i1 .ne. 0) mp = ma(i1)
   print 2, m(im), name, mp, a
   if (i1 .ne. 0) print 3, ma(i2), b
   call zqjrem67(' STEAM TABLE ERROR')
   stop
2  format (1x, a18, a6, 2x, a6, g20.13)
3  format (27x, a6, 1x, g20.13)
end subroutine ster67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     zqjrem67(3f) - [M_steam67] write an error message to stdout
!!
!!##SYNOPSIS
!!
!!   subroutine zqjrem67(string)
!!
!!    character(len=*),intent(in)  :: string
!!
!!##DESCRIPTION
!!    write an error message to stdout
!!
!!##OPTIONS
!!    string   the message to write
subroutine zqjrem67(string)
character(len=*),intent(in)  :: string
   write(*,'(1x,a)')trim(string)
end subroutine zqjrem67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     steamv67(3f) - [M_steam67] print version information to unit and return version string
!!
!!##SYNOPSIS
!!
!!   subroutine steamv67(io,version)
!!
!!    integer,intent(in)           :: io
!!    character(len=*),intent(out) :: version
!!
!!##DESCRIPTION
!!    Print information on the programming environment used to compile the
!!    library and return a version string.
!!
!!##OPTIONS
!!   IO        Logical unit to write output to
!!
!!##RETURN VALUES
!!   VERSION   short version string
!!
!!##EXAMPLES
!!
!!
!!    A sample  program  that  calls  STEAMV and the  resulting
!!    output follows:
!!
!!       program demo_steamv
!!       use M_steam67, only : steamv67
!!       implicit none
!!       character(len=20) :: version
!!       call steamv67(6,version)
!!       end program demo_steamv
!!
!!        ******************************************************
!!        *        Westinghouse Electric Corporation           *
!!        *        Steam Table Routines, Version 1.0           *
!!        ******************************************************
!!        Compiled & QAed on:
!!        System...........HP-UX
!!        O.S. Release.....A.08.07
!!        O.S. Version.....A
!!        Hardware Name....9000/750
!!        ******************************************************
!!        Nodename.........daisy
!!        Machine ID.......2010465874
!!        ******************************************************
!!        Compile Date:Tue Aug 24 23:02:11 EDT 1993
!!        ******************************************************
!!
!!    The compile date (similar to the output from the Unix date(1) command)
!!    tells when it was compiled. The other information (similar to that
!!    produced by the uname(1) command) identifies where the compilation
!!    took place and what level of Operating System was used.
subroutine steamv67(io,version)

! ident_28="@(#) M_steam67 steam67(3f) print pedigree of library and return version value"

integer,intent(in)           :: io
character(len=*),intent(out) :: version
   version='2.2'
!   if(io.ge.0)then
!$BLOCK SHELL
!################################################################################
!(
!CONFIG=${1:-UNCONFIGURED}
!case "$CONFIG" in
!UNCONFIGURED)
!cat <<EOF
!   write(io,*)'******************************************************'
!   write(io,*)'*      Westinghouse Electric Company, LLC.           *'
!   write(io,*)'*        ASME-67 Steam Table Routines                *'
!   write(io,*)'******************************************************'
!   write(io,*)'* NOTE: This is an unconfigured version of the       *'
!   write(io,*)'* Westinghouse Steam Table library.                  *'
!   write(io,*)'******************************************************'
!   write(io,*)'Compiled on:'
!   write(io,*)'Build Target ....`systemtype -r`'
!   write(io,*)'System...........`uname -s`'
!   write(io,*)'O.S. Release.....`uname -r`'
!   write(io,*)'O.S. Version..... ' &
!!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!!EOF
!;;
!#===============================================================================
!CONFIGURED|*)
!cat <<EOF
!   write(io,*)'******************************************************'
!   write(io,*)'*        Westinghouse Electric Company, LLC.         *'
!   write(io,*)'*        Steam Table Routines, Version 2.1           *'
!   write(io,*)'******************************************************'
!   write(io,*)'Compiled & QAed on:'
!   write(io,*)'Build Target ....`systemtype -r`'
!   write(io,*)'System...........`uname -s`'
!   write(io,*)'O.S. Release.....`uname -r`'
!   write(io,*)'O.S. Version.....' &
!!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!EOF
!;;
!esac
!################################################################################
!for name in `uname -v`
!do
!   for short in `echo "$name"|fold -60`
!   do
!      echo "     &//'$short ' &"
!   done
!done
!echo "     &//' ' "
!cat <<EOF
!      write(io,*)' Hardware Name...`uname -m`'
!EOF
!################################################################################
!cat <<EOF
!   write(io,*)'Compile Date:`date`'
!   write(io,*)'******************************************************'
!   endif
!EOF
!################################################################################
!)|expand
!################################################################################
end subroutine steamv67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module m_steam67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

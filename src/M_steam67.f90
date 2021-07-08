










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
character(len=*),parameter::ident_1="@(#)M_steam67::steam67(3f): 1967 ASME Steam Table Library"
! core
public  ::  CONDV67   !  FUNCTION    CONDV67   (P,T)
public  ::  CONDL67   !  ENTRY       CONDL67   (P,T)
!private ::  DO67      !  FUNCTION    DO67      (A,X,ISTART,ISTOP)
public  ::  cpl67     !  FUNCTION    cpl67     (PF,TF)
public  ::  cpv67     !  FUNCTION    cpv67     (PF,TF,VF)
public  ::  crflo67   !  FUNCTION    crflo67   (PRES,ENTH,SHT)
public  ::  CRITVS67  !  FUNCTION    CRITVS67  (PRESS,ENTH,GAMMA)
public  ::  CRITVW67  !  ENTRY       CRITVW67  (PRESS,ENTH,GAMMA)
public  ::  crvel67   !  FUNCTION    crvel67   (P,H,GA)
public  ::  hcsl67    !  FUNCTION    hcsl67    (P,T,V,S,IGO)
public  ::  hcslv167  !  FUNCTION    hcslv167  (P,T,V,S,IGO)
public  ::  HCSLV267  !  ENTRY       HCSLV267  (P,T,V,S,IGO)
private ::  vest67    !  SUBROUTINE  vest67    (PBARS,TC,VOUT)
public  ::  HSS67     !  FUNCTION    HSS67     (P,T,S,V)
public  ::  HSS167    !  ENTRY       HSS167    (P,T,S,V)
public  ::  PSL67     !  FUNCTION    PSL67     (T)
public  ::  PSL167    !  ENTRY       PSL167    (T)
public  ::  PSV67     !  FUNCTION    PSV67     (SS)
public  ::  PSV167    !  ENTRY       PSV167    (SS)
public  ::  PSV267    !  ENTRY       PSV267    (SS)
public  ::  PLS67     !  ENTRY       PLS67     (SS)
public  ::  sssiss67  !  FUNCTION    sssiss67  (PRES,ENTH,TEMP,V,X)
public  ::  sisiss67  !  ENTRY       sisiss67  (PRES,ENTH,TEMP,V,X)
public  ::  hisiss67  !  ENTRY       hisiss67  (PRES,ENTH,TEMP,V,X)
public  ::  hssiss67  !  ENTRY       hssiss67  (PRES,ENTH,TEMP,V,X)
public  ::  spsiss67  !  ENTRY       spsiss67  (PRES,ENTH,TEMP,V,X)
public  ::  hpsiss67  !  ENTRY       hpsiss67  (PRES,ENTH,TEMP,V,X)
public  ::  TPS67     !  FUNCTION    TPS67     (PRES,ENTR)
public  ::  TPH67     !  ENTRY       TPH67     (PRES,ENTR)
public  ::  tpsl67    !  FUNCTION    tpsl67    (PRES,S)
public  ::  TPHL67    !  ENTRY       TPHL67    (PRES,S)
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
private ::  GR167     !  FUNCTION    GR167     (T,X,N)
private ::  GRS67     !  FUNCTION    GRS67     (X,NDX,Y,NDY,XV,N,NRANGE)
private ::  p23t67    !  FUNCTION    p23t67    (TIN)
private ::  vliq67    !  FUNCTION    vliq67    (PIN,TIN,VMIN,VMAX)
!
public  ::  ZSDH67    !  FUNCTION    ZSDH67    (P1,P2,H1,S,T1,T2,X1,X2,V1,V2)
public  ::  zsdt67    !  FUNCTION    zsdt67    (P1,P2,T1,S,H1,T2,X1,X2,V1,V2)
public  ::  zsrh67    !  FUNCTION    zsrh67    (P1,P2,H1,S,T1,T2)
public  ::  zsrt67    !  FUNCTION    zsrt67    (P1,P2,T1,S,H1,T2)
public  ::  steamv67  !  SUBROUTINE  steamv67  ()

! print error messages
private ::  GOTOER67  !  SUBROUTINE  GOTOER67  (NAME,I)
private ::  STER67    !  SUBROUTINE  STER67    (NAME,I,A,B)
private ::  zqjrem67  !  subroutine  zqjrem67  (string)

public test_suite_M_steam67

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
FUNCTION CONDV67(P,T) result(condv67_result)
implicit none
!external VCL67
!SAVE
! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS

character(len=*),parameter::ident_2="&
&@(#)M_steam67::condv67(3f): compute thermal conductivity of water at pressure P and temperature T"
character(len=*),parameter::ident_3="@(#)M_steam67::condl67(3f): return thermal conductivity given P,T"

doubleprecision,intent(in) :: P, T
doubleprecision :: G(44),B(6),C(8)
doubleprecision :: TAU1,TAU, PSAT, DENS
doubleprecision :: condv67_result
doubleprecision :: c8
integer         :: IN(8),ID(8)
integer         :: i
!-JU  A
      DATA (G(I),I=41,44)/             &
     &   -4.51d-2,                     &
     &    1.40957195d0,                &
     &   -8.210057814d-1,              &
     &    1.024476774d1/
!-JU  B
      DATA (B(I),I=1,6)/               &
     &   8.984504672d3,                &
     &  -2.890032838d4,                &
     &   3.443892141d4,                &
     &  -1.814175922d4,                &
     &   4.245698830d3,                &
     &   2.699696542d2/
!-JU  D1
      DATA (G(I),I=32,36)/             &
     &   7.180025637d6,                &
     &  -1.776928510d8,                &
     &   1.617702015d9,                &
     &  -6.269159344d9,                &
     &   8.370013127d9/
!-JU  D2
      DATA (G(I),I=37,40)/             &
     &  -1.827766246d-2,               &
     &   3.045585779d-1,               &
     &  -1.425775726d0,                &
     &   1.0d0/
!-JU  d1
      DATA (G(I),I=20,28)/             &
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
      DATA (G(I),I=29,31)/             &
     &   2.336550603d-2,               &
     &  -3.055656614d-1,               &
     &   1.0d0/
!-JU  F1
      DATA (G(I),I=6,14)/              &
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
      DATA (G(I),I=15,19)/             &
     &   6.606768113d-5,               &
     &  -3.923316061d-3,               &
     &   6.625711877d-2,               &
     &  -4.368537279d-1,               &
     &   1.0d0/
      DATA (G(I),I=1,5)/               &
     &   5.873976935d7,                &
     &  -9.12673946000d8,              &
     &   5.690390782d9,                &
     &  -1.771196944d10,               &
     &   2.148955071d10/
      DATA (IN(I),I=1,8)/1,6,15,20,29,32,37,41/
      DATA (ID(I),I=1,8)/5,14,19,28,31,36,40,44/
      C(1)=HSS67(P,T,C(2),DENS)
      GO TO 5
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
      ENTRY CONDL67(P,T) result(condv67_result)
      DENS=VCL67(P,T)
5     CONTINUE
      DENS=0.0160184634d0/DENS
      C8=0.d0
      TAU1=(T-32.0d0)/1.8d0
      IF (TAU1.LT.0.5d0) TAU1=0.5d0
      TAU=TAU1*1.0d-2+2.7315d0
      IF (TAU1.GE.700.0d0) GO TO 15
!     IF TAU1 IS .GT. 650. AND .LT. 700. INTERPOLATION IS NECESSARY
!     AND TAU=9.2315 IS THE VALUE FOR TAU1=650.
      IF (TAU1.LE.650.0d0) GO TO 15
      C(2)=1202.0d0
      TAU=9.2315d0
10    CONTINUE
      C(1)=HSS67(P,C(2),C(3),DENS)
      DENS=0.0160184634d0/DENS
15    CONTINUE
      DO I=1,8
         C(I)=DO67(G(1),TAU,IN(I),ID(I))
      ENDDO
      C(2)=C(2)/C(3)+1.135d-92*TAU**107
      C(3)=C(4)/C(5)
      C(4)=C(6)/C(7)*TAU
      C(5)=C(8)+DO67(B(1),DENS,1,6)*DENS
      C(6)=DO67(C(1),DENS,1,4)
      CONDV67_result=C(5)+C(6)*DENS*DENS/TAU**7
      IF (TAU1.GT.425.0d0) GO TO 25
      IF (TAU1.LT.350.0d0) GO TO 50
      PSAT=3208.234d0
      IF (T.LT.705.47d0) PSAT=PSL67(T)
      IF (P.GT.PSAT) GO TO 45
25    CONTINUE
      IF (TAU1.LE.650.0d0) GO TO 50
      IF (C8.NE.0.0d0) GO TO 40
      IF (TAU1-700.0d0) 30,50,35
30    CONTINUE
      TAU=9.7315d0
      C8=CONDV67_result
      C(2)=1292.0d0
      GO TO 10
35    CONTINUE
      CONDV67_result=C(8)+((103.51d0+TAU1*(0.4198d0-2.771d-5*TAU1))+2.1482d14*DENS/TAU1**4.2d0)*DENS
      GO TO 50
40    CONTINUE
      CONDV67_result=C8+(CONDV67_result-C8)*(TAU1-650.0d0)*0.020d0
      GO TO 50
45    CONTINUE
      IF ( C(5) .LT. CONDV67_result)CONDV67_result=C(5)
50    CONTINUE
      CONDV67_result=CONDV67_result*5.777893d-4
!==================================================================================================================================!
      CONTAINS
!==================================================================================================================================!
FUNCTION DO67(A, X, ISTART,ISTOP)
implicit none
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_4="@(#)M_steam67::do67(3f)L summation of a polynomial"

!     part of procedure for solving nth order polynomial with a do loop
!       **     DOUBLE PRECISION FUNCTION IS CALLED TO CALCULATE  **
!       **     THE SUMMATION OF A DOUBLE PRECISION POLYNOMIAL.   **
integer,intent(in)         :: ISTOP    ! final location in A array
doubleprecision,intent(in) :: A(ISTOP) ! coefficients of array
doubleprecision,intent(in) :: X        ! multiplier on the coefficients
doubleprecision            :: DO67
integer,intent(in)         :: ISTART   ! starting location in A array
integer                    :: L
integer                    :: K
   do67 = a(istart)
   l = istart + 1
   do k = l, istop
      do67 = do67*x + a(k)
   enddo
   end function do67
!==================================================================================================================================!
   END FUNCTION CONDV67
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
FUNCTION cpl67(PF,TF)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_5="&
&@(#)M_steam67::cpl67(3f):  calculates specific isobaric head capacity of water at pressure P and temperature T"

doubleprecision,intent(in)   :: PF    ! WATER PRESSURE, IN PSIA.
doubleprecision,intent(in)   :: TF    ! WATER TEMPERATURE, IN DEGREES F.
doubleprecision              :: CPL67 ! OUTPUT - HEAT CAPACITY IN BTU/LB-DEG. F.
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
SAVE
DIMENSION C(2)
      IND = 0
      T = TF
      P = PF
      TSAT = 705.47d0
      IF (P .LT. 3208.234d0)THEN
         TSAT = TSL67(P)
      ENDIF
      H = 2.0d0
      IF (.NOT. (P .LT. 3000.0d0 .OR. P .GT. 4350.0d0)) THEN
         IF (.NOT. (T .LT. 657.0d0 .OR. T .GT. 677.0d0)) THEN
            T = 657.0d0
            DO I = 1, 2
               C(I)=(HCL67(P,T-2.0d0*H,S)-HCL67(P,T + 2.0d0*H,S)+ 8.0d0*(HCL67(P,T+H,S)-HCL67(P,T-H,S)))/(12.0d0*H)
               T = T + 20.0d0
            ENDDO
            cpl67 = C(1) + (TF - 657.0d0)*((C(2) - C(1))/20.0d0)
            GOTO 999
         ENDIF
      ENDIF
      IF (P .GT. 4350.0d0 .AND. T .GT. 657.0d0) THEN
         IND = 1
         T = 657.0d0
      ELSE
         H1 = (TSAT - T)/2.0d0
         IF (H1 .LT. H) THEN
            H = H1
         ENDIF
         IF (H .LT. 0.125d0) THEN
            H = 0.075d0
            T = TSAT - 0.25d0
         ENDIF
      ENDIF
      cpl67 = (HCL67(P, T - 2.0d0*H, S) - HCL67(P,T+2.0d0*H,S)+ 8.0d0*(HCL67(P, T + H, S) - HCL67(P,T-H,S)))/(12.0d0*H)
      IF (IND .NE. 0) THEN
         C1 = cpl67
         C2 = CPV67(P, 707.0d0, DUMO1)
         cpl67 = C1 + (TF - 657.0d0)*((C2 - C1)/50.0d0)
      ENDIF
999   CONTINUE
      END FUNCTION cpl67
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
FUNCTION cpv67(PF, TF, VF)
implicit none
!  Copyright (c) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_6="&
&@(#)M_steam67::cpv67(3f): calculates specific isobaric heat capacity and specific volume of water at pressure P and temperature T"

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
   T = TF                               ! mutable copy of tf
   P = PF                               ! mutable copy of pf
   TSAT = 705.47d0
   IF (P .LT. 3208.234d0)then
      TSAT = TSL67(P)
   endif
   H = 2.0d0
   H1 = (T - TSAT)/2.0d0
   IF (H1 .LT. H) then
      H = H1
   endif
   IF (H .LT. 0.125d0) THEN
      H = 0.075d0
      T = TSAT + 0.25d0
   ENDIF
   cpv67=(HSS67(P,T- 2.0d0*H,S,V)-HSS67(P,T+2.0d0*H,S,V)+8.0d0*(HSS67(P,T+H,S,V1)-HSS67(P,T-H,S,V2)))/(12.0d0*H)
   VF = (V1 + V2)/2.0d0
   IF (H .LE. 2.0d0) then
      HQ = HSS67(P, TF, S, VF)
   endif
END FUNCTION cpv67
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
FUNCTION crflo67 (PRES,ENTH,SHT)
implicit none
! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS RESERVED

character(len=*),parameter::ident_7="&
&@(#)M_steam67::crflo67(3f): critical flow and degrees superheat of wet/superheated steam at PRES,ENTH"

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
      SAVE
      DIMENSION A(14), B(20), C(75), D(35), P1(14)
doubleprecision :: v1
doubleprecision :: v2
doubleprecision :: v3
doubleprecision :: c1
doubleprecision :: c2
doubleprecision :: c3
doubleprecision :: ans
ANS(V1,V2,V3,C1,C2,C3)=((C1-V2)*V1+(V2-C2)*V3)/C3
      DATA (A(I),I=1,14) /  &
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

      DATA (B(I),I=1,20) /  &
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

      DATA (C(I),I=1,75) /  &
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

      DATA (P1(I),I=1,14) /         &
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

      DATA (D(I),I=1,35) /   &
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
      P=PRES
      H=ENTH
      IF (P.GT.3208.234759d0) GO TO 1
      S1=SSSISS67(P,H,X,S2,S3)
      SPT=X-TSL67(P)
      GO TO 2
1     CONTINUE
      SPT=400.d0
2     CONTINUE
      IRTN=0
      IF (SPT.GE.85.d0) GO TO 20
      DUM=LOG(P)
      J=1
      IF (P.LE.210.d0) GO TO 4
      IRTN=1
3     CONTINUE
      J=8
      DUM=P
4     CONTINUE
      AG=(((((A(J)*DUM+A(J+1))*DUM+A(J+2))*DUM+A(J+3))*DUM+A(J+4))*DUM+A(J+5))*DUM+A(J+6)
      IF (J.EQ.8) AG=EXP(AG)
      IF (P.GT.200.d0) IF (IRTN-1) 5,7,6
      GO TO 7
5     CONTINUE
      AG1=AG
      IRTN=2
      GO TO 3
6     CONTINUE
      AG=ANS(AG1,P,AG,210.d0,200.d0,10.d0)
7     CONTINUE
      IRTN=0
      IF (SPT.NE.0.0d0) GO TO 9
      DUM=SSSISS67(P,H,S2,S3,X1)
      X=X1
      IF (X1.GT.0.999d0) X=0.999d0
      S1=0.0d0
      S2=0.0d0
      S3=0.0d0
      S4=0.0d0
      DO I=1,5
         S4=S4*X+B(I)
         S3=S3*X+B(I+5)
         S2=S2*X+B(I+10)
         S1=S1*X+B(I+15)
      enddo
      AMULT=((S4*P+S3)*P+S2)*P+S1
      IF (X1.GT.0.999d0) AMULT=1.0d0-1000.0d0*(1.0d0-X1)*(1.0d0-AMULT)
      GO TO 19
9     CONTINUE
      X=SPT
      IF (SPT.LT.15.d0) X=15.d0
      Y=LOG(P)
      J=1
      IF (P.LT.250.d0) GO TO 12
      IRTN=2
      IF (P.LT.1600.d0) GO TO 11
      IRTN=4
10    CONTINUE
      J=51
      Y=P/100.d0
      GO TO 12
11    CONTINUE
      J=26
12    CONTINUE
      S1=C(J+18)
      S2=0.0d0
      S3=0.0d0
      S4=0.0d0
      K=J+5
      DO I=J,K
         S4=(S4+C(I))*X
         S3=(S3+C(I+6))*X
         S2=(S2+C(I+12))*X
         S1=S1*X+C(I+19)
      enddo
      AMULT=((S4*Y+S3)*Y+S2)*Y+S1
      IF (SPT.LE.15.0d0) AMULT=1.0d0+SPT*(AMULT-1.0d0)/15.0d0
      IF (IRTN-1) 14,15,16
14    CONTINUE
      IF (P.LE.200.0d0) GO TO 19
      AMULT1=AMULT
      IRTN=1
      GO TO 11
15    CONTINUE
      AMULT=ANS(AMULT1,P,AMULT,250.0d0,200.0d0,50.0d0)
      GO TO 19
16    CONTINUE
      IF (IRTN-3) 17,18,19
17    CONTINUE
      IF (P.LE.1500.0d0) GO TO 19
      AMULT1=AMULT
      IRTN=3
      GO TO 10
18    CONTINUE
      AMULT=ANS(AMULT1,P,AMULT,1600.0d0,1500.0d0,100.0d0)
19    CONTINUE
      AK=AG/AMULT
      IF (SPT.LE.80.0d0) GO TO 24
      IRTN=1

20    continue

      DO I=1,7
         K=I*5
         J=K-4
         P1(2*I)=0
         L=J
         DO J=L,K
            P1(2*I)=P1(2*I)*H+D(J)
         enddo
      enddo

      AK1=P1(2)
      IF (P.LE.20.0d0) GO TO 22
      AK1=GR167(P1(1),P,7)
22    CONTINUE
      IF (IRTN.EQ.1) GO TO 23
      AK=AK1
      GO TO 24
23    CONTINUE
      AK=ANS(AK,SPT,AK1,85.0d0,80.0d0,5.0d0)
24    continue
      crflo67=AK*P
      SHT=SPT
      END FUNCTION crflo67
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
FUNCTION CRITVS67(PRESS, ENTH, GAMMA)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_8="@(#)M_steam67::function critvs67(press, enth, gamma)"
character(len=*),parameter::ident_9="@(#)M_steam67::entry critvw67(press, enth, gamma)"

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
      SAVE
      DIMENSION CS(30), CW(30)
      DATA(CS(I), I = 1, 30)/                             &
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
      DATA(CW(I), I = 1, 30)/                             &
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
      P = PRESS
      H = ENTH
      IF (P .LT. 1100.0d0) THEN
         S1 = 0.0d0
         S2 = 0.0d0
         S3 = 0.0d0
         DO I = 1, 5
            S2 = (S2 + CS(I))/H
            S3 = (S3 + CS(I + 5))/H
            S1 = S1*H + CS(I + 10)
         ENDDO
         CRIT = (S3*P + S2)*P + S1
         CRITVS67 = CRIT
         IF (P .LE. 1000.0d0) GOTO 7
      ENDIF
      IF (P .GT. 8000.0d0) THEN
         CALL STER67('CRITVS67', 13, PRESS, ENTH)
         GOTO 9
      ELSE
         S2 = 0.0d0
         S3 = 0.0d0
         S4 = 0.0d0
         S1 = (CS(25)*H + CS(26))*H + CS(27)
         DO I = 1, 3
            S4 = (S4 + CS(I + 15))/H
            S3 = (S3 + CS(I + 18))/H
            S2 = (S2 + CS(I + 21))/H
            S1 = S1*H + CS(I + 27)
         ENDDO
         Y = P/1000.0d0
         CRITVS67 = ((S4*Y + S3)*Y + S2)*Y + S1
         IF(P.LT.1100.0d0)CRITVS67=((1100.0d0-P)*CRIT+(P-1000.0d0) *CRITVS67)/100.0d0
         GOTO 7
      ENDIF
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
      ENTRY CRITVW67(PRESS, ENTH, GAMMA)
      H = ENTH
      CW(16) = 4.8121071d-95*H**27
      P = PRESS
      Y = LOG(P)
      I = 1
      IF (P .LT. 225.0d0) GOTO 5
4     CONTINUE
      CRIT = CRITVS
      I = 16
5     CONTINUE
      J = I + 2
      S1 = (CW(I + 9)*H + CW(I + 10))*H + CW(I + 11)
      S2 = 0.0d0
      S3 = 0.0d0
      S4 = 0.0d0
      DO K = I, J
         S4 = (S4 + CW(K))*H
         S3 = (S3 + CW(K + 3))*H
         S2 = (S2 + CW(K + 6))*H
         S1 = S1*H + CW(K + 12)
      ENDDO
      CRITVS = ((S4*Y + S3)*Y + S2)*Y + S1
      IF (P .GE. 225.0d0 .OR. P .LE. 200.0d0) GOTO 7
      IF (I .EQ. 1) GOTO 4
      CRITVS = ((225.0d0 - P)*CRIT + (P - 200.0d0)*CRITVS)/25.0d0
7     CONTINUE
      GAMMA = 0.0d0
9     CONTINUE
      END FUNCTION CRITVS67
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
FUNCTION crvel67(P, H, GA)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_10="@(#)M_steam67::critical flow(3f): velocity and isentropic exponent of wet/superheated steam"

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
SAVE
COMMON /NUST/TCORE, SCORE, VCORE, DPDT
DIMENSION PK(9), V(9), A(10)
DATA(A(I), I = 1, 10)/                              &
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
      S = SISISS67(P, H, T, V(1), X)
      PSAT = 0
      DP = .01d0
      IF (S .EQ. 1.0618559d0) THEN
         PSAT = 3208.234759d0
      ELSE
         IF (S .GT. 1.0618559d0) THEN
            PSAT = PSV67(S)
         ELSE
            PSAT = PLS67(S)
            DDH = .027d0
            IF (P .LT. 50.0d0) DDH = .0027d0
            IF (P .LT. 8000.0d0) DP = MAX(.02d0, DDH/P/V(1))
            IF ((P-4.0d0*P*DP).LT.0.0886d0)DP=(P-0.0887d0)/4.0d0/P
         ENDIF
         IF (P .GT. 2500.0d0 .AND. H .GT. 1114.0d0)PSAT=P23T67(T)
      ENDIF
      DO I = 1, 9
         PK(I) = P + DP*DBLE(I - 5)*P
         X = HISISS67(PK(I), S, T, V(I), GA)
      ENDDO
      IF(PK(5).GT.(200.0d0-4.0d0*DP).AND.PK(5).LE.(200.0d0+4.0d0*DP)) THEN
         IF (PSAT .GT. 200.0d0 .AND. PK(5) .GT. 200.0d0) THEN
            IF (PK(9) .GT. PSAT) GOTO 7
         ELSEIF (.NOT. (PSAT .GT. 200.0d0 .AND. PK(5) .LE. 200.0d0))THEN
            IF (PSAT .LE. PK(5)) THEN
               PSAT = PK(5)*0.9999999d0
               GOTO 7
            ENDIF
         ENDIF
         PSAT = PK(5)*1.00000001d0
      ENDIF
7     CONTINUE
      X = 0
!     pk(1) equals term of old routine
      PK(1) = DP/V(5)
      IF (P .LT. 3208.234759d0) THEN
         IF (PK(5) .NE. PSAT) THEN
            IF (PK(5) .GT. PSAT) THEN
               IF (PK(3) .GE. PSAT) GOTO 10
               IF (PK(4) .LT. PSAT) GOTO 16
            ELSEIF (PK(7) .GE. PSAT) THEN
               PK(1) =  - PK(1)
               T = V(6)
               V(6) = V(4)
               V(7) = V(3)
               V(8) = V(2)
               IF (PK(6) .GE. PSAT) THEN
                  V(9) = V(1)
                  GOTO 16
               ELSE
                  V(4) = T
               ENDIF
            ELSE
               GOTO 10
            ENDIF
            DO I = 5, 9
               X = X + A(I)/V(I - 1)
            ENDDO
            GOTO 18
         ENDIF
16       CONTINUE
         DO I = 1, 5
            X = X + A(I)/V(I + 4)
         ENDDO
         GOTO 18
      ENDIF
10    CONTINUE
      X = A(9)/V(3) - A(9)/V(7) + A(10)/V(4) - A(10)/V(6)
18    CONTINUE
      GA = PK(1)/X
      crvel67 = SQRT(4633.06d0*GA*P*V(5))
END FUNCTION crvel67
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
FUNCTION hcsl67 (P,T,V,S,IGO)
implicit none
! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS

character(len=*),parameter::ident_11="@(#)M_steam67::hcsl67(3f): calculate specific enthalpy/volume and entropy of liquid(P,T)"

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
SAVE
!     9.1 SUB-REGION 1
!     ENTERED FROM HSL67,SSL67,VSL67,HCL67,VCL WHEN IGO =1,2,3,4,5 RESPECTIVELY
!     CALLED DIRECTLY WHEN IGO=6
DIMENSION THETA(16), A(23), SA(12)
DATA (A(I),I=1,23) /          &
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
      DATA (SA(I),I=1,12) /         &
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
      IF (T.GT.705.47d0) GO TO 13
      IF (T.GT.662.0d0) GO TO 10
!-----------------------------------------------------------------------
!     THETA   =((T-32.0d0)/1.8d0+273.15d0)/647.3d0
      THETA(1)=8.582659595d-4*T+0.3945191136d0
      BETA=P/3208.234759d0
!     X AND R ARE BOTH USED
      X=SA(6)-THETA(1)
      R=X**9
!     THIS DO LOOP GIVES THETA**2 THRU THETA**11
      DO I=2,11
         THETA(I)=THETA(I-1)*THETA(1)
      enddo
!     THETA(12)=THETA**17
      THETA(12)=THETA(8)*THETA(9)
!     THIS DO LOOP GIVES THETA**18 THRU THETA**21
      DO I=13,16
         THETA(I)=THETA(I-1)*THETA(1)
      enddo
      B2=BETA**2
      B3=B2*BETA
      Q2=(SA(10)+BETA)**4
      Q1=(SA(10)+BETA)/Q2
      Y=1.0d0-SA(1)*THETA(2)-SA(2)/THETA(6)
      Z=Y+SQRT(SA(3)*Y**2-2.0d0*(SA(4)*THETA(1)-SA(5)*BETA))
!     Z1=Z**(5.d0/17.d0)
      Z1=Z**0.2941176471d0
      R1=SA(7)+THETA(14)
      R2=SA(8)+THETA(11)
      R3=(((A(20)*BETA+A(19))*BETA+A(18))*BETA)/R2**2
      YPRI=-2.0d0*SA(1)*THETA(1)+6.0d0*SA(2)/THETA(7)
!-----------------------------------------------------------------------
      GO TO (8,5,3,5,3,3), IGO
!-----------------------------------------------------------------------
      CALL GOTOER67( 'hcsl67 (A), IGO',IGO)
!     SUB"REGION 1 REDUCED VOLUME SUBSCRIPT OF A IS ONE MORE
!     THAN REPORT SUBSCRIPT NUMBER
3     CONTINUE
      V=(A(12)*SA(5)/Z1+A(13)+A(14)* THETA(1)+A(15)* THETA(2)+A(16)*R*X+A(17)/R1-(A(18)+2.0d0*A(19)* &
     & BETA+3.0d0*A(20)*B2)/R2)*5.077852889d-2
      IF (T.LE.382.d0) GO TO 4
!-----------------------------------------------------------------------
      V=V+(-A(21)*THETA(13)*(SA(9)+THETA(2))*(-3.0d0/Q2+SA(11))         &
     &+3.0d0*A(22)*(SA(12)-THETA(1))*B2+4.0d0*A(23)/THETA(15)*B3)         &
     &*5.077852889d-2
!     SUB"REGION 1 REDUCED ENTROPY
!-----------------------------------------------------------------------
4     CONTINUE
      GO TO (11,11,12,11,12,5), IGO
!-----------------------------------------------------------------------
      CALL GOTOER67( 'hcsl67 (B), IGO',IGO)
5     CONTINUE
      SUM=A(3)
      DO I=1,8
         SUM=SUM+DBLE(I+1)*A(I+3)*THETA(I)
      enddo
      S=(1.583237108d-6+A(1)*LOG(THETA(1))-SUM+A(12)/Z1*((0.4166666667d0         &
     &* Z-0.720d0*Y)*YPRI+SA(4))+BETA*(-A(14)-2*A(15)*THETA(1)+10.0d0         &
     &*A(16)*R+ 19.0d0*A(17)/R1**2*THETA(13))-11.0d0*THETA(10)*R3)         &
     &*2.587358228d-2
!-----------------------------------------------------------------------
      IF (T.LE.382.d0) GO TO 7
!-----------------------------------------------------------------------
      S=S+(A(21)*THETA(12)*(18.0d0*SA(9)+20.d0*THETA(2))*(Q1+SA(11)*BETA)+B3*(A(22)+20.0d0*A(23)/THETA(16)*BETA))*2.587358228d-2
!     SUB"REGION 1 REDUCED ENTHALPY
!-----------------------------------------------------------------------
7     CONTINUE
      GO TO (11,12,11,8,11,8), IGO
!-----------------------------------------------------------------------
      CALL GOTOER67( 'hcsl67 (C), IGO',IGO)
8     CONTINUE
      SUM=-A(2)
!     SECOND TERM OMITTED SINCE IT = 0
      DO I=2,9
         SUM=SUM+DBLE(I-1)*A(I+2)*THETA(I)
      enddo
      hcsl67=                                                               &
     &(2.156561703d-7+A(1)*THETA(1)-SUM+A(12)/Z1*(Z*(0.5862068965d0         &
     &*Z -1.416666667d0*Y+0.4166666667d0*THETA(1)*YPRI)+SA(4)*THETA(1)      &
     &-0.720d0*THETA(1)*Y*YPRI)+BETA*(A(13)-A(15)*THETA(2)+A(16)*(9.0d0     &
     &*THETA(1)+SA(6))*R+A(17)*(20.0d0*THETA(14)+SA(7))/R1**2)-(12.0d0      &
     &*THETA(11)+SA(8))*R3)*30.14634566d0
!-----------------------------------------------------------------------
      IF (T.LE.382.0d0) GO TO 12
!-----------------------------------------------------------------------
      hcsl67=hcsl67+(A(21)*THETA(13)*(17.0d0*SA(9)+19.0d0*THETA(2))*        &
     &(Q1+SA(11)*BETA)+(A(22)*SA(12)+21.0d0*A(23)/THETA(15)*BETA)*B2)       &
     &*30.14634566d0
!-----------------------------------------------------------------------
      GO TO 12
!-----------------------------------------------------------------------
10    continue
      hcsl67=HCSLV167(P,T,V,S,IGO)
      GO TO 12
!-----------------------------------------------------------------------
11    continue
      PRINT 14,IGO
      STOP
!-----------------------------------------------------------------------
12    continue
      RETURN
!-----------------------------------------------------------------------
13    CALL STER67('hcsl67',2,T,0.0d0)
! never called. STER67() stops, but programming tools complain hcsl67 undefined
      stop
!-----------------------------------------------------------------------
!
14    FORMAT (10X,'WRONG STATEMENT IN hcsl67,IGO=',I3)
!-----------------------------------------------------------------------
      END FUNCTION hcsl67
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
FUNCTION hcslv167(P, T, V, S, IGO)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_12="@(#)M_steam67::hcslvl67(3f): calculate V,S,specific enthalpy for liquid given (P,T)"
character(len=*),parameter::ident_13="&
&@(#)M_steam67::hcslv267(3f): calculate specific enthalpy, specific volume, and entropy of the liquid at P and T"

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
SAVE
!     entered from HSL67,SSL67,VSL67,HCL67,vcl when igo =1,2,3,4,5 respectively
!     called directly when igo=6
!-ju  theta dimensioned to 10 instead of 9 --- code not corrected
DIMENSION SUM(16), THETA(10), Q(9), X(9), C0(12), C1(10), C2(10), C3(10), C6(9), C7(9)
DIMENSION VTRY(3), PCT(3)
DIMENSION Y(5), D3(5), D4(5)
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
!-ju  theta(10) set to 1 and assumed static (saved)
DATA THETA(10)/1.0d0/
DATA C40/2.759717760d-6/

DATA(C0(I), I = 1, 12)/               &
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

DATA(C1(I),                           &
     &  I = 1, 10)/                         &
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

DATA(C2(I), I = 1, 10)/               &
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

DATA(C3(I), I = 1, 10)/               &
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

DATA(C6(I), I = 1, 9)/                &
     &    0.05528935335d0,                  &
     &   -0.2336365955d0,                   &
     &    0.3697071420d0,                   &
     &   -0.2596415470d0,                   &
     &    0.06828087013d0,                  &
     &    0.0d0,                            &
     &    0.0d0,                            &
     &    0.0d0,                            &
     &    0.0d0/

DATA(C7(I), I = 1, 9)/                &
     & -257.1600553d0,                      &
     & -151.8783715d0,                      &
     &   22.20723208d0,                     &
     & -180.2039570d0,                      &
     & 2357.096220d0,                       &
     &   -1.462335698d4,                    &
     &    4.542916630d4,                    &
     &   -7.053556432d4,                    &
     &    4.381571428d4/

DATA C41/-5.090739850d-4/

DATA C50/2.106363320d2/

DATA(D3(I), I = 1, 5)/                &
     &   -1.717616747d0,                    &
     &    3.526389875d0,                    &
     &   -2.690899373d0,                    &
     &    0.9070982605d0,                   &
     &   -0.1138791156d0/

DATA(D4(I), I = 1, 5)/                &
     &    1.301023613d0,                    &
     &   -2.642777743d0,                    &
     &    1.996765362d0,                    &
     &   -0.6661557013d0,                   &
     &    0.08270860589d0/

DATA D50/3.426663535d-4/

DATA D51/-1.236521258d-3/

DATA D52/1.155018309d-3/

DATA G/80.4099390d0/

DATA R/107.2132505d0/
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
!     sub-region 3and 4
      INT = 1
!      remove calc. for pb and tc when vliq67 is made part of prog.
      PB = P/14.503773773d0
      TC = (T - 32.0d0)/1.8d0
!     v from vliq67 is in cc/gm
      VTRY(1) = VLIQ67(PB, TC, VMIN, VMAX)
      VMAX = 3.17d0
      TOL = 3.0d-8
      GOTO 1
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
      ENTRY HCSLV267(P, T, V, S, IGO)
      INT = 0
!     v from vest67 is in cc/gm
      CALL VEST67(P, T, VTRY(1))
      VMAX = 8.9d0
      VMIN = 1.28d0
      TOL = 5.0d-9
1     CONTINUE
      K = 1
      NT = 1
      BETA = P/3208.234759d0
!     theta   =((t-32.0d0)/1.8d0+273.15d0)/647.3d0
      THETA(1) = 8.582659595d-4*T + 0.3945191136d0
      DF = 0.25d0
      PCTP = 0
2     CONTINUE
      IF (VTRY(1) .GT. VMAX) THEN
         VTRY(1) = VMAX
         DF = 1.0d0
      ELSEIF (VTRY(1) .LT. VMIN) THEN
         VTRY(1) = VMIN
         DF = 1.0d0
      ENDIF
!     set-up for sub-region 3 paragraph 9.3 for pressure calculation
      X(1) = VTRY(K)/3.17d0
      DO I = 1, 16
         SUM(I) = 0
      ENDDO
      Q(1) = THETA(1) - 1.0d0
      DO I = 2, 9
         Q(I) = Q(I - 1)*Q(1)
         X(I) = X(I - 1)*X(1)
         THETA(I) = THETA(I - 1)*THETA(1)
         B = DBLE(1 - I)/X(I)
         SUM(1) = SUM(1) + B*C0(I)
         SUM(2) = SUM(2) + B*C1(I)
         SUM(3) = SUM(3) + B*C2(I)
         SUM(4) = SUM(4) + B*C3(I)
         SUM(5) = SUM(5) + C6(I - 1)/THETA(I)
      ENDDO
!     the following are the last two terms of sum(1)
      SUM(1) = SUM(1) - (9.0d0*C0(10)/X(1) + 10.0d0*C0(11)/X(2))/X(9)
!     the following are theta**22 and theta**23
      THETA(8) = THETA(7)*THETA(7)*THETA(7)*THETA(1)
      THETA(9) = THETA(8)*THETA(1)
!     set-up for sub-region 4 paragraph 9.4
      IF (INT .EQ. 0) THEN
         BR = 0.0d0
         S = 0.0d0
         hcslv167 = 0.0d0
      ELSE
         Y(1) = (1.0d0 - THETA(1))/3.730882125d-2
         SUM1 = 0
         SUM2 = 3.0d0*D3(1) + 4.0d0*D4(1)*Y(1)
         SUM3 = D3(1)*(-2.0d0*Y(1) + G) + D4(1)*( - 3.0d0*Y(1) + R)*Y(1)
         DO I = 2, 5
            Y(I) = Y(I - 1)*Y(1)
            SUM1 = SUM1 + (DBLE(I) - 1.0d0)*(D3(I) + Y(1)*D4(I))/X(I)
            SUM2 = SUM2 + (3.0d0*D3(I) + 4.0d0*D4(I)*Y(1))/X(I - 1)
            SUM3 = SUM3 + (D3(I)*(DBLE(I - 3)*Y(1) + G) + D4(I)* (DBLE(I - 4)*Y(1) + R)*Y(1))/X(I - 1)
         ENDDO
         SUM1 = SUM1*Y(3)
         SUM2 = SUM2*Y(2)
         SUM3 = SUM3*Y(2)
         Y(5) = Y(5)**6
!     p from sub-region 4 paragraph 9.4
         BR = SUM1 - Y(5)*Y(2)*(D51 + 2.0d0*D52*X(1))
!     h from sub-region 4 paragraph 9.4
         hcslv167 = SUM3-Y(5)*Y(1)*((31.0d0*D50+32.0d0*D51*X(1)+33.0d0 *D52*X(2))*Y(1) - (D50 + D51*X(1) + D52*X(2))*857.7060043d0)
!     s from sub-region 4 paragraph 9.4
         S =  + (SUM2 + 32.0d0*Y(5)*Y(1)*(D50 + D51*X(1) + D52*X(2))) /3.730882125d-2
      ENDIF
!     p from sub-region 3 paragraph 9.3
      BR = ( - C0(1) - SUM(1) - C0(12)/X(1) - (C1(1) + SUM(2) + C1(10)     &
     & /X(1))*Q(1) - (C2(1) + SUM(3) + C2(10)/X(1))*Q(2) - (C3(1) + SUM    &
     & (4)+C3(10)/X(1))*Q(3) + 5.0d0*C41/X(6)*Q(1)/THETA(9) - 6.0d0*X(5)    &
     & *SUM(5) + BR)*3208.234759d0
      PCT(K) = (BR - P)/P
      IF (ABS(PCT(K)) .LE. TOL) THEN
!     replace this conversion if vliq67 is changed to use engrg. units
         V = VTRY(K)/62.4279605761d0
         SUM(10) = C7(1)
         SUM(16) = C7(1)
         DO I = 2, 9
            SUM(6) = SUM(6) + C1(I)/X(I - 1)
            SUM(7) = SUM(7) + C2(I)/X(I - 1)
            SUM(8) = SUM(8) + C3(I)/X(I - 1)
!-ju
!-ju  this causes theta(10) to be used when i=9, but theta only 9 big,
!-ju  but c6(8) is zero so theta(10) was created and set to 1. authors
!-ju  assumed 0/anything was 0, which is not safe (0/0, 0/indefinite).
!-ju
            SUM(9) = SUM(9) + DBLE(I)*C6(I - 1)/THETA(I + 1)
            SUM(10) = SUM(10) + DBLE(I)*C7(I)*Q(I - 1)
            B = DBLE(I)
            SUM(11) = SUM(11) + B*C0(I)/X(I - 1)
            SUM(12) = SUM(12) + (B - 1.0d0)*C1(I)/X(I - 1)
            SUM(13) = SUM(13) + (B - 2.0d0)*C2(I)/X(I - 1)
            SUM(14) = SUM(14) + (B - 3.0d0)*C3(I)/X(I - 1)
            SUM(15) = SUM(15) + (B - 5.0d0)*C6(I - 1)/THETA(I)
            SUM(16)=SUM(16)+ C7(I)*(1.0d0 + (B - 1.0d0)*THETA(1))*Q(I-1)
         ENDDO
         SUM(11) = SUM(11) + (10.0d0*C0(10) + 11.0d0*C0(11)/X(1))/X(9)
         IF (IGO .EQ. 1 .OR. IGO .EQ. 4 .OR. IGO .EQ. 6) GOTO 18
         IF (IGO .EQ. 2) GOTO 19
         IF (IGO .EQ. 3 .OR. IGO .EQ. 5) GOTO 21
         CALL GOTOER67('hcslv167 (A), IGO', IGO)
      ENDIF
      IF (NT .GT. 1000) GOTO 20
!---------------------------------------------------------------------------------------
      IF (NT .GE. 2) THEN
         IF (PCTP*PCT(1) .GT. 0) THEN
            IF (ABS(PCT(1)) .GT. ABS(0.3d0*PCTP)) THEN
               DF = 1.5d0*DF
               GOTO 16
            ENDIF
         ENDIF
         DF = 0.67d0*DF
         DV = (VTRY(1) - VPREV)*(P - BR)/(BR - PPREV)
         GOTO 17
      ENDIF
16    CONTINUE
      DV = VTRY(1)*PCT(1)*DF
17    CONTINUE
!---------------------------------------------------------------------------------------
      VPREV = VTRY(1)
      PPREV = BR
      PCTP = PCT(1)
      VTRY(1) = VTRY(1) + DV
      NT = NT + 1
      GOTO 2
!     h from sub-region 3 paragraph 9.3
18    CONTINUE
      hcslv167 =     &
     & ( - 213.164655d0 - C1(1)*X(1) + SUM(11) - SUM(6) + (C0(12)             &
     & - C1(10))*LOG(X(1))+Q(1)*( - C1(10) - C50 - (C1(1) + 2.0d0*C2(1))      &
     & *X(1)+SUM(12) - 2.0d0*(SUM(7) + C2(10)*LOG(X(1)))) + Q(2)*( - C2       &
     & (10)-(2.0d0*C2(1)+3.0d0*C3(1))*X(1)+SUM(13) - 3.0d0*SUM(8) - (C2       &
     & (10) + 3.0d0*C3(10))*LOG(X(1))) + Q(3)*( - 3.0d0*C3(1)*X(1) + SUM      &
     & (14) - C3(10)*(1.0d0+2.0d0*LOG(X(1))))+(C40*(23.0d0- 24.0d0/THETA      &
     & (1))-C41*(-28.0d0+ 29.0d0/THETA(1))/X(5))/THETA(8) + X(6)*SUM(15)      &
     & - SUM(16) + hcslv167)*30.14634566d0
      IF ((IGO .GE. 1 .AND. IGO .LE. 3) .OR. IGO .EQ. 5) GOTO 21
      IF (.NOT. (IGO .EQ. 4 .OR. IGO .EQ. 6)) CALL GOTOER67('hcslv167 (B), IGO', IGO)
!     s from sub-region 3 paragraph 9.3
19    CONTINUE
      S = (1.583237108d-6 - C1(1)*X(1) - SUM(6) - C1(10)*LOG(X(1)) -          &
     & C50-2.0d0*Q(1)*(C2(1)*X(1) + SUM(7) + C2(10)*LOG(X(1))) - 3.0d0*Q      &
     & (2)*(C3(1)*X(1) + SUM(8) + C3(10)*LOG(X(1))) + (C40 + C41/X(5))*       &
     & (22.0d0-23.0d0/THETA(1))/THETA(9)-C50*LOG(THETA(1)) + X(6)*SUM(9)      &
     & - SUM(10) + S)*2.587358228d-2
      GOTO 21
20    CONTINUE
      CALL STER67('hcslv167', -12, P, T)
21    CONTINUE
      END FUNCTION hcslv167
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
SUBROUTINE vest67(PBARS, TC, VOUT)
implicit none
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
SAVE
DIMENSION A(10), B(11)
DATA(A(I), I = 1, 10)/    &
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
      DATA(B(I), I = 1, 11)/    &
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
      P = PBARS
      T = TC + 273.15d0
      IF (TC .GT. 0.32d0 * P + 294.0d0 ) THEN
         S1 = ((B(11)*T + B(10))*T + B(9))*T
         S2 = ((B(8)*T + B(7))*T + B(6))*T
         S3 = ((B(5)*T + B(4))*T + B(3))*T + B(2)
         VOUT = ((S1/P + S2)/P + S3)/P + B(1)
      ELSE
         S1 = ((A(10)*T + A(9))*T + A(8))*T
         S2 = ((A(7)*T + A(6))*T + A(5))*T
         S3 = ((A(4)*T + A(3)*T**18)*T + A(2))*T
         VOUT = (S1/P + S2)/P + S3 + A(1)
      ENDIF
      END SUBROUTINE vest67
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
FUNCTION HSS67(P,T,S,V)
implicit none

! COPYRIGHT (C) 1969,1990 WESTINGHOUSE ELECTRIC CORPORATION. ALL RIGHTS RESERVED

character(len=*),parameter::ident_14="@(#)M_steam67::hss67(3f): specific enthalpy = HSS67(P,T,s,v)"
character(len=*),parameter::ident_15="@(#)M_steam67::hsl67(3f): specific enthalpy = HSS67(P,T,s,v)"

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
SAVE
DIMENSION IZ1(5,3), IZ6(3,2), Q(6), Z(4), IX6(3,2), B0(6), B1(3,5), B6(2,3), B9(7), SB6(2,3), AA(10)
DATA (AA(I), I=1, 10) /                                           &
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
DATA (B0(I), I=1, 6) /                                            &
     &       0.08565182058d0,                                           &
     &      -0.6547711697d0,                                            &
     &       0.4330662834d0,                                            &
     &     -54.38923329d0,                                              &
     &      28.56067796d0,                                              &
     &      16.83599274d0 /
DATA ((B1(I, J), I=1, 3), J=1, 5) /                               &
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
DATA ((B6(I,J),I=1,2),J=1,3) /                                    &
     &       0.1190610271d0,                                            &
     &      -0.09867174132d0,                                           &
     &       0.1683998803d0,                                            &
     &      -0.05809438001d0,                                           &
     &       6.552390126d-3,                                            &
     &       5.710218649d-4 /
DATA (B9(I), I=1, 7) /                                            &
     &     523.5718623d0,                                               &
     &   -2693.088365d0,                                                &
     &    5745.984054d0,                                                &
     &   -6508.211677d0,                                                &
     &    4126.607219d0,                                                &
     &   -1388.522425d0,                                                &
     &     193.6587558d0 /
DATA SB / .7633333333d0 /
DATA ((SB6(I,J),I=1,2),J=1,3)/                                    &
     &       0.4006073948d0,                                            &
     &       0.0d0,                                                     &
     &       0.08636081627d0,                                           &
     &       0.0d0,                                                     &
     &      -0.8532322921d0,                                            &
     &       0.3460208861d0/

DATA ((IZ1(I,J),I=1,5),J=1,3) / 13,18,18,25,32,3,2,10,14,28,0,1,0,0,24 /
DATA ((IZ6(I,J),I=1,3),J=1,2) /12,24,24,11,18,14/
DATA ((IX6(I,J),I=1,3),J=1,2) /14,19,54,0,0,27/

      ICNT=4
      JKL=0
      GOTO 1
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
ENTRY HSS167(P,T,S,V)
      ICNT=3
      JKL=1
1     CONTINUE
      BETA=P/3208.2347590d0
      THETA=8.582659595d-4*T+0.3945191136d0
      BL=P23T67(T)/3208.234759d0
      IF (BETA.GT.BL) THEN
         HSS67=HCSLV267(P,T,V,S,ICNT)
         GOTO 999
      endif
      XYZ=LOG(P)
      X=EXP(SB*(1.0d0-THETA))
      BT=SB*THETA
      V1=0.0d0
      S1=0.0d0
      H1=0.0d0
      !----------------------------------------------------------------------
      DO I=1,5
         DO J=1,3
            Q(J)=B1(J,I)*X**IZ1(I,J)
            Z(J)=DBLE(IZ1(I,J))
         enddo
         BETAI=BETA**I
         V1=V1+DBLE(I)*(BETAI/BETA)*(Q(1)+Q(2)+Q(3))
         IF (JKL.EQ.1) CYCLE
         S1=S1+BETAI*(Z(1)*Q(1)+Z(2)*Q(2)+Z(3)*Q(3))
         H1=H1+BETAI*((1.0d0+Z(1)*BT)*Q(1)+(1.0d0+Z(2)*BT)*Q(2)+(1.0d0+Z(3)*BT)*Q(3))
      ENDDO
      !----------------------------------------------------------------------
      V2=0.0d0
      S2=0.0d0
      H2=0.0d0
      V3=0.0d0
      S3=0.0d0
      H3=0.0d0
      IF (P.LE.200.0d0) GOTO 10
      K=1
      IF (P.LE.2300.0d0) GOTO 4
      K=3
      IF (P.GE.3500.0d0) GOTO 5
4     continue
      TMIN=AA(K)*XYZ+AA(K+1)
      IF (T.GE.TMIN) GOTO 10
5     continue
      !----------------------------------------------------------------------
      DO I=1,3
         DO J=1,2
            Q(J)=B6(J,I)*X**IZ6(I,J)
            Z(J)=DBLE(IZ6(I,J))
            Q(J+2)=SB6(J,I)*X**IX6(I,J)
            Z(J+2)=DBLE(IX6(I,J))
         enddo
         BETAI=BETA**(-3-I)
         Q(5)=BETAI+Q(3)+Q(4)
         V2=V2+(DBLE(I+3)*BETAI/BETA*(Q(1)+Q(2)))/Q(5)**2
         IF (JKL.EQ.1) CYCLE
         Q(6)=(Z(3)*Q(3)+Z(4)*Q(4))/Q(5)
         S2=S2+(Q(1)*(Z(1)-Q(6))+Q(2)*(Z(2)-Q(6)))/Q(5)
         H2=H2+(Q(1)*((1.0d0+Z(1)*BT)-BT*Q(6))+Q(2)*((1.0d0+Z(2)*BT)-BT*Q(6)))/Q(5)
      ENDDO
      !----------------------------------------------------------------------
      IF (P.LE.1000.0d0) GOTO 10
      K=5
      IF (P.LE.2800.0d0) GOTO 8
      K=7
      IF (P.GT.9700.0d0) K=9
8     continue
      TMIN=AA(K)*XYZ+AA(K+1)
      IF (T.GE.TMIN) GOTO 10
      Q(1)=(386.27614140d0*THETA-341.70619780d0)/BL
      !----------------------------------------------------------------------
      DO I=1,7
         V3=V3*X+B9(I)
         IF (JKL.EQ.1) cycle
         Q(2)=Q(1)+DBLE(7-I)*SB
         S3=S3*X+Q(2)*B9(I)
         H3=H3*X+(1.0d0+THETA*Q(2))*B9(I)
      enddo
      !----------------------------------------------------------------------
      Q(1)=(BETA/BL)**10
10    continue
      V=(4.2603211480d0*THETA/BETA-V1-V2+11.0d0*Q(1)*V3)*.050778528890d0
      Q(2)=0.0d0
      Q(3)=3.0d0*B0(1)
      !----------------------------------------------------------------------
      DO I=1,4
         Q(2)=Q(2)*THETA+DBLE(5-I)*B0(I)
         Q(3)=Q(3)*THETA+DBLE(3-I)*B0(I+1)
      enddo
      !----------------------------------------------------------------------
      S=(1.583237108d-6-4.260321148d0*LOG(BETA)+B0(6)*LOG(THETA)-Q(2)-SB*S1-SB*S2+BETA*Q(1)*S3)*.02587358228d0
      HSS67=(2.156561703d-7+B0(6)*THETA-Q(3)-H1-H2+BETA*Q(1)*H3)*30.14634566d0
999   continue
      RETURN
      END FUNCTION HSS67
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
FUNCTION PSL67(T)
implicit none
!  Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_16="&
&@(#)M_steam67::psl67(3f): calculate saturation pressure of the saturated liquid at temperature T"
character(len=*),parameter::ident_17="&
&@(#)M_steam67::psl167(3f): calculate saturation pressure of the saturated liquid at temperature T."

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
SAVE
COMMON /NUST/TCORE, SCORE, VCORE, DPDT
DIMENSION AK(9)
DATA(AK(I), I = 1, 9)/ &
     &    -7.691234564d0,    &
     &   -26.08023696d0,     &
     &  -168.1706546d0,      &
     &    64.23285504d0,     &
     &  -118.9646225d0,      &
     &     4.167117320d0,    &
     &    20.97506760d0,     &
     &     1.0d9,            &
     &     6.0d0/
DATA AK2T2, AK3T3/-5.21604739d1, -5.04511964d2/
DATA AK4T4, AK5T5/ + 2.56931420d2, -5.94823113d2/
DATA AK7T2, AK8T2/ + 4.19501352d1,  + 2.0d9/
   J = 1
   GOTO 1
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
ENTRY PSL167(T)
   J = 2
   GOTO 1
!===================================================================================================================================
1  CONTINUE
   IF (T .GT. 705.47d0) THEN
      CALL STER67('PSL67', 2, T, 0.0d0)
   ELSE
!     theta   =((t-32.0d0)/1.8d0+273.15d0)/647.3d0
      THETA = (T + 459.67d0)/1165.14d0
      X = 1.0d0 - THETA
      Y = 0.0d0
      DO I = 1, 5
         M6I = 6 - I
         Y = (Y + AK(M6I))*X
      enddo
!     the  k function (saturation line) page 12 par. 5
      DEN1 = 1.0d0 + X*(AK(6) + AK(7)*X)
      DEN2 = AK(8)*X*X + AK(9)
      PSL67 = EXP(Y/THETA/DEN1 - X/DEN2)*3208.234759d0
      IF (J .EQ. 2) THEN
         DSDT =  - (AK(1) + X*(AK2T2 + X*(AK3T3 + X*(AK4T4 + AK5T5 *X))))
         B = THETA*DEN1
         DBDT = DEN1 - THETA*(AK(6) + AK7T2*X)
         DBBDT =  - AK8T2*X
         DPDT=(PSL67/1165.14d0)*(((B*DSDT-Y*DBDT)/(B*B)) + ((DEN2 + X*DBBDT)/(DEN2*DEN2)))
      ENDIF
   ENDIF
END FUNCTION PSL67
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
FUNCTION PSV67(SS)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_18="&
&@(#)M_steam67::psv67(3f):  calculate saturation pressure of the saturated liquid at entropy SS in psia"
character(len=*),parameter::ident_19="&
&@(#)M_steam67::psv167(3f): calculate saturation pressure of the saturated liquid at entropy SS in psia"
character(len=*),parameter::ident_20="&
&@(#)M_steam67::psv267(3f): calculate saturation pressure in psia of the saturated liquid at entropy SS."
character(len=*),parameter::ident_21="@(#)M_steam67::pls67(3f):  saturation pressure of the saturated liquid at entropy SS."

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
SAVE
DIMENSION A(34), X(4)
      DATA(A(I), I = 1, 34)/    &
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
      J = 0
      GOTO 1
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
ENTRY PSV267(SS)
   J = 2
GOTO 1
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
ENTRY PLS67(SS)

   S = 1.0d0 + SS
   J = 3
   I = 18
   GOTO 2
   1 CONTINUE
   I = 1
   S = SS
   2 CONTINUE
   PSV67 = 3208.234759d0
   TOL = 0.00002d0
   IF(.NOT.(SS .GE. 1.06116005d0 .AND. SS .LE. 1.0618559d0))THEN
      NT = 1
      NA = 5
      IF (SS .GT. 2.1872d0 .OR. SS .LT. 0.0d0) THEN
         PSV67 = 0.0d0
      ELSE
         DEL = 0.1d0
         IF (SS .NE. 1.0618559d0) THEN
            IF (SS .GT. 1.0618559d0) THEN
               IF (J .NE. 3) GOTO 5
            ELSEIF (J .GE. 3) THEN
               GOTO 5
            ENDIF
            CALL STER67(' PSV67', 5, SS, 0.0d0)
            GOTO 18
   5        CONTINUE
            IF (SS .LT. A(I + 16)) I = I + 8
            X(2) = A(I)
            K = I + 6
            DO M = I, K
               X(2) = X(2)*S + A(M + 1)
            enddo
   7        CONTINUE
            IF (X(2) .GT. 705.47d0) X(2) = 705.47d0
            PSV67 = PSL67(X(2))
            IF (J .NE. 1) THEN
               IF (J .EQ. 3) THEN
                  T = HCL67(PSV67, X(2), X(1))
                  IF (PSV67 .LT. 1900.0d0) TOL = .000002d0
               ELSE
                  T = HSS67(PSV67, X(2), X(1), X(3))
               ENDIF
               IF (ABS(X(1) - SS) .LT. TOL) GOTO 20
               IF (PSV67 .LE. 3100.0d0) THEN
                  IF (J .NE. 3) THEN
                     IF (NT .EQ. 1) GOTO 15
                  ENDIF
               ENDIF
               X(4) = X(2) - DEL
   11          CONTINUE
               P = PSL67(X(4))
               IF (J .EQ. 3) THEN
                  T = HCL67(P, X(4), X(3))
               ELSE
                  T = HSS67(P, X(4), X(3), F)
               ENDIF
               IF (ABS(X(2) - X(4)) .LT. 0.002d0) THEN
                  X(4) = X(4) - DEL
                  NT = NT + 1
                  IF (NT .GT. 300) GOTO 18
                  GOTO 11
               ENDIF
               X(2) = GR167(X(1), SS, 2)
               IF (NT .GT. 300) GOTO 18
               NT = NT + 1
               DEL = 0.9d0*DEL
               IF (NT .EQ. NA) NA = NA + 5
               GOTO 7
            ENDIF
            PSV67 = PSV67*1.003d0
            GOTO 20
   15       CONTINUE
            F = 1.058d0 - 0.364d-3*PSV67
            IF (PSV67 .LE. 300.0d0) F = 1.33d0
            IF (PSV67 .GT. 2200.0d0) F = 0.782d0 - 0.239d-3*PSV67
            PSV67=PSV67-5.4d0*(X(2) + 459.67d0)*(SS - X(1))/X(3)*F
            GOTO 20
   18       CONTINUE
            PRINT 21, PSV67, SS
         ENDIF
      ENDIF
   ENDIF
   20 CONTINUE
   !
   21 FORMAT ('0PSV ', F12.6, 'ENTR ', F8.6)
END FUNCTION PSV67
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
FUNCTION sssiss67(PRES, ENTH, TEMP, V, X)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_22="&
&@(#)M_steam67::sssiss67(3f): specific entropy, temperature, specific volume, steam quality of liquid at pressure P and enthalpy H"
character(len=*),parameter::ident_23="&
&@(#)M_steam67::hssiss67(3f):  specific enthalpy, temperature, specific volume, steam quality of water at pressure P and entropy S"

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
SAVE
COMMON /NUST/TCORE, SCORE, VCORE, DPDT
COMMON /LIQU/FACTOR
DIMENSION XGI(2), XF(2), AA(10)
DATA(AA(I), I = 1, 10)/          &
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
      I = 1
      GOTO 2
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
ENTRY sisiss67(PRES, ENTH, TEMP, V, X)
   I = 1
   GOTO 1
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
ENTRY hisiss67(PRES, ENTH, TEMP, V, X)
   I = 2
   GOTO 1
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
ENTRY hssiss67(PRES, ENTH, TEMP, V, X)

      I = 2
      GOTO 2
1     continue
      KK = 2
      GOTO 3
2     continue
      KK = 0
3     continue
      P = PRES
      X = 1.0d0
      IF (P .LT. 3208.234759d0) THEN
         IF (I .EQ. 1) THEN
            IF (ENTH .GT. 1204.79d0) GOTO 6
            PP = P
            II = 1
            IF (P .LT. 3100.0d0) THEN
               IF (P .LT. 400.0d0) THEN
                  PP = LOG(P)
                  II = 6
               ENDIF
               JJ = II + 3
               SUM = 0.0d0
               DO K = II, JJ
                  SUM = (SUM + AA(K))*PP
               enddo
               SUM = SUM + AA(JJ + 1)
               IF (ENTH .GE. SUM) GOTO 6
            ELSEIF (ENTH .GE. 993.4d0) THEN
               GOTO 6
            ENDIF
         ELSEIF (ENTH .GE. 1.06185d0) THEN
            PP = PSV167(ENTH)
            IF (P .GT. PP) GOTO 7
         ENDIF
         Y = ENTH
         K = 3 - I
         T = TSL67(P)
         IF (KK .EQ. 2) T = TSL167(P)
         XGI(1) = HSS67(P, T, XGI(2), V)
         sssiss67 = XGI(K)
         X2 = T + 459.67d0
         IF (Y .EQ. XGI(I)) GOTO 9
         IF (Y .LE. XGI(I)) THEN
            IF (KK .EQ. 2) THEN
               XF(1) = HCL67(P, T, XF(2))
            ELSE
               XF(1) = HSL67(T)
            ENDIF
            X1 = XGI(1) - XF(1)
            ! sfg is calculated from hfg/tabs
            XF(2) = XGI(2) - X1/X2
            IF (Y .LT. XF(I)) GOTO 20
            X = (Y - XF(I))/(XGI(I) - XF(I))
            sssiss67 = XF(K) + X*(XGI(K) - XF(K))
            ! vfg is calculated from clapeyron eq. using hfg, tabs, and dpdt
            X2 = X1/X2*778.169262d0/144.0d0/DPDT
            IF (KK .EQ. 2) X2 = V - VCL67(P, T)
            V = V - (1.0d0 - X)*X2
            GOTO 9
         ENDIF
      ELSEIF (KK .EQ. 2) THEN
         GOTO 5
      ENDIF
4     continue
      IF (I .EQ. 1) GOTO 6
      IF (I .EQ. 2) GOTO 7
      CALL GOTOER67('sssiss67 (A), I', I)
5     continue
      T = 705.47d0
      XGI(1) = HSS67(P, T, XGI(2), VCORE)
      M3I = 3 - I
      SCORE = XGI(M3I)
      IF (ENTH .GE. XGI(I)) THEN
         IF (ENTH .GT. XGI(I)) GOTO 4
         GOTO 8
      ENDIF
20    continue
      FACTOR = 1.0d0
      IF (I .EQ. 2) THEN
         T = TPSL67(P, ENTH)
      ELSE
         T = TPHL67(P, ENTH)
      ENDIF
      V = VCL67(P, T)
      sssiss67 = SCORE
      X = 0.0d0
      GOTO 9
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
      ENTRY spsiss67(PRES, ENTH, TEMP, V, X)
6     continue
      T = TPH67(PRES, ENTH)
      GOTO 8
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
ENTRY hpsiss67(PRES, ENTH, TEMP, V, X)
7     continue
      T = TPS67(PRES, ENTH)
8     continue
      sssiss67 = SCORE
      V = VCORE
9     continue
      TEMP = T
      FACTOR = 0.0d0
END FUNCTION sssiss67
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
FUNCTION TPS67(PRES,ENTR)
implicit none
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
SAVE
DIMENSION A(240), CSMAX(7), T0(2), TOL(2), XGI(2)
COMMON /NUST/ TCORE,SCORE,VCORE,DPDT
EQUIVALENCE (CP,T0(2))
DATA (CSMAX(I),I=1,7) /            &
     &     7.20894478d-26,               &
     &    -5.47437942d-21,               &
     &     1.61833683d-16,               &
     &    -2.43468381d-12,               &
     &     2.03081212d-8,                &
     &    -9.99741636d-5,                &
     &     1.55077535d0 /
DATA (TOL(I),I=1,2) / 0.00004d0,0.04d0 /
DATA (A(I),I=1,94) /               &
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
DATA (A(I),I=95,184) /            &
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
DATA (A(I),I=185,240) /           &
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

      J=15
      I=1
      DMT=0.00080d0
      GOTO 1
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
      ENTRY TPH67(PRES,ENTR)
      J=13
      I=2
      DMT=0.80d0
!-----------------------------------------------------------------------
1     CONTINUE
      P=PRES
      J1=0
      A1=0.50d0
      A2=1.0d0
      Y=ENTR
      Y1=Y
      X0=LOG(P)
      IF (P.GT.3208.2340d0) GOTO 3
!-----------------------------------------------------------------------
      T=32.0d0
      GOTO 4
!-----------------------------------------------------------------------
2     CONTINUE
      TPS67=T
      M3I=3-I
      SCORE=XGI(M3I)
      RETURN
!-----------------------------------------------------------------------
3     CONTINUE
      T=705.470d0
4     CONTINUE
      ICNT=1
      IF (P.GT.16000.0d0) GOTO 22
!-----------------------------------------------------------------------
      IF (I.GT.1) GOTO 6
!-----------------------------------------------------------------------
5     CONTINUE
      PL=X0
      IF (I-1) 7,8,7
!-----------------------------------------------------------------------
6     CONTINUE
      IF (ENTR-1800.0d0) 5,5,22
!-----------------------------------------------------------------------
7     CONTINUE
      J=121
      Y1=Y/100.0d0
      SMAX=1280.0d0
      GOTO 10
!-----------------------------------------------------------------------
8     CONTINUE
      SMAX=CSMAX(1)
      DO J=1,6
         SMAX=P*SMAX+CSMAX(J+1)
      ENDDO
      J=1
10    CONTINUE
      IF (P.GT.500.0d0) J=J+40
      IF (P.GT.2500.0d0.AND.Y.LT.SMAX) J=J+40
      IF (P.GT.2500.0d0.AND.Y.LT.SMAX) J1=J
      IF (J.EQ.121) X0=P
      DO L=1,2
         S1=0.0d0
         S2=0.0d0
         S3=0.0d0
         S4=0.0d0
         K=J+7
         KK=J+4
         DO JJ=KK,K
            S4=(S4+A(JJ))*Y1
            S3=(S3+A(JJ+4))*Y1
            S2=(S2+A(JJ+8))*Y1
            S1=(S1+A(JJ+12))*Y1
         ENDDO
         T0(L)=((A(J)*X0+A(J+1))*X0+A(J+2))*X0+A(J+3)+S4+X0*(S3+X0*(S2+X0*S1))
         J=J+20
         IF (T0(1).LT.T) T0(1)=T
      ENDDO
!-----------------------------------------------------------------------
      IF (T0(1).GT.1600.0d0) T0(1)=1600.0d0
      XGI(2)=HSS67(P,T0(1),XGI(1),VCORE)
      X0=XGI(I)
      IF (P.GT.800.0d0.AND.T0(1).LT.700.0d0) DMT=0.50d0*DMT
      PL=1.0d0
      S1=VCORE
      S2=0.0d0
      S3=CP
      IF (ABS(Y-X0).LT.TOL(I)) GOTO 16
!-----------------------------------------------------------------------
      T1=T0(1)+CP*(Y-X0)
      T=T1
      IF (I.EQ.2) GOTO 13
!-----------------------------------------------------------------------
      PL=T0(1)+459.670d0
      T1=T0(1)+(PL)*CP*(Y-X0)*(1.0d0+CP*(Y-X0)/2.0d0)
      T=T1
13    CONTINUE
      IF (ABS(T0(1)-T1).LT.0.0050d0.AND.J1.EQ.0) GOTO 18
!-----------------------------------------------------------------------
      IF (ABS(Y-X0).LT.DMT.AND.J1.EQ.0) GOTO 18
!-----------------------------------------------------------------------
      XGI(2)=HSS67(P,T1,XGI(1),VCORE)
      X1=XGI(I)
      X2=X1
      S2=(VCORE-S1)/(T1-T0(1))
      S1=VCORE
14    CONTINUE
      S3=(T0(1)-T1)/(X0-X1)/PL
      IF (ICNT.GT.10) GOTO 20
!-----------------------------------------------------------------------
15    CONTINUE
      CP=A1*(A2*CP+S3)
      IF (I.EQ.1) PL=T1+459.670d0
      IF (ABS(Y-X1).LT.TOL(I).OR.ABS(Y-X1).LT.0.20d0*DMT) GOTO 17
!-----------------------------------------------------------------------
      T=T1+CP*(Y-X1)*PL
      IF (T.GT.1600.0d0) T=1600.0d0
      XGI(2)=HSS67(P,T,XGI(1),VCORE)
      X2=XGI(I)
      S2=(VCORE-S1)/(T-T1)
      S1=VCORE
      IF (ABS(Y-X2).LT.TOL(I)) GOTO 17
!-----------------------------------------------------------------------
      ICNT=ICNT+1
      IF (ICNT.GT.100) GOTO 21
!-----------------------------------------------------------------------
      X0=X1
      T0(1)=T1
      X1=X2
      T1=T
      GOTO 14
!-----------------------------------------------------------------------
16    CONTINUE
      IF (I.EQ.1) PL=T0(1)+459.670d0
      X2=X0
      T=T0(1)
17    CONTINUE
      T0(1)=T
      T=T+S3*(Y-X2)*PL
18    CONTINUE
      VX=T0(1)+0.50d0*(T-T0(1))+459.670d0
      IF (I.EQ.2) VX=1.0d0/VX
      M3I=3-I
      XGI(M3I)=XGI(M3I)+(Y-XGI(I))*VX
      IF (S2.EQ.0.0d0) GOTO 19
!-----------------------------------------------------------------------
      VCORE=VCORE+S2*(T-T0(1))
      GOTO 2
!-----------------------------------------------------------------------
19    CONTINUE
      X1=HSS167(P,T,X2,VCORE)
      GOTO 2
!-----------------------------------------------------------------------
20    CONTINUE
      A1=0.10d0
      A2=9.0d0
      IF (ICNT.LT.30) GOTO 15
!-----------------------------------------------------------------------
      A1=0.020d0
      A2=49.0d0
      CP=CP*0.9750d0
      GOTO 15
!-----------------------------------------------------------------------
21    CONTINUE
      J=-15
      IF (I.EQ.1) GOTO 22
      J=-13
22    CONTINUE
      CALL STER67('TPS67',J,PRES,ENTR)
!     cannot get here but tools complain TSP not defined if stop not added
      stop
!-----------------------------------------------------------------------
      END
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
FUNCTION tpsl67 (PRES,S)
implicit none
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
SAVE
DIMENSION TOL(2), A(80), T0(2), XGI(2), XXI(2)
COMMON /NUST/ TCORE,SCORE,VCORE,DPDT
COMMON /LIQU/ FACTOR
EQUIVALENCE (CP,T0(2))
DATA (TOL(I),I=1,2) / 0.00004d0,0.04d0/
   DATA (A(I),I=1,76) /              &
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
      DATA (A(I), I=77, 80) /   &
     &   -1.3189235d-1,         &
     &    2.9212226d0,          &
     &   -2.1568287d1,          &
     &    5.5997480d1 /
!-------------------------------------------------------------------
      J=1
      I=1
      Z=1.0d0+S
      GOTO 1
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
ENTRY TPHL67(PRES,S)
      I=2
      J=41
      Z=(500.0d0+S)/100.0d0
!-------------------------------------------------------------------
1  CONTINUE
   TR=705.47d0
   A1=0.5d0
   A2=1.0d0
   DT=0.0005d0
   TOLERA=TOL(I)
   X=S
   P=PRES
   Y=LOG(P)
   IF (P.LT.200.0d0) Y=LOG(200.0d0)
   IF (P.GT.3208.2340d0) GOTO 2
   TR=TSL67(P)
   IF (I.EQ.1) GOTO 2
   IF (HCL67(P,TR,SS).GE.S) GOTO 2
   TR=TSLH67(X)
   PP=PSL67(TR)
   IF (P.LT.PP) PRES=PP
   IF (P.LT.PP) P=PRES
   IF (TR.GT.705.470d0) TR=705.470d0
2  CONTINUE
   ICNT=1
   DO L=1,2
      K=J+3
      S1=0.0d0
      S2=0.0d0
      S3=0.0d0
      S4=0.0d0
      S5=0.0d0
      DO JJ=J,K
         S5=S5*Y+A(JJ+16)
         S4=(S4+A(JJ+12))*Z
         S3=(S3+A(JJ+8))*Z
         S2=(S2+A(JJ+4))*Z
         S1=(S1+A(JJ))*Z
      enddo
      T0(L)=((S4*Y+S3)*Y+S2)*Y+S1+S5
      J=J+20
      IF (T0(1).GT.TR) T0(1)=TR
   enddo
   XGI(2)=HCL67(P,T0(1),XGI(1))
   X0=XGI(I)
   PL=1.0d0
   T1=T0(1)-CP*(X0-X)
   T=T1
   IF (I.NE.1) GOTO 5
   PL=T0(1)+459.670d0
   T1=T0(1)-PL*CP*(X0-X)*(1.0d0-CP*(X0-X)/2.0d0)
   T=T1
5  continue
   IF (FACTOR.EQ.1.0d0) GOTO 7
   IF (T0(1).GT.480.0d0.AND.I.EQ.1) GOTO 8
   IF (T0(1).LE.650.0d0) GOTO 11
   IF (T0(1).LE.695.0d0) GOTO 8
   XXI(2)=HCL67(P,TR,XXI(1))
   XM=XXI(I)
   IF (X.LE.XM) GOTO 8
   J=15
   IF (I.LT.2) GOTO 6
   J=13
6  continue
   CALL STER67 ('tpsl67',J,PRES,S)
7  continue
   IF (PRES.LE.800.0d0) TOLERA=TOLERA/10.0d0
   IF (PRES.LT.50.0d0) TOLERA=TOLERA/10.0d0
8  continue
   IF (T1.GT.TR) T1=TR
   IF (ABS(T0(1)-T1).LT.0.0010d0) T1=T0(1)-0.0010d0
   XGI(2)=HCL67(P,T1,XGI(1))
   X1=XGI(I)
   X2=X1
   T=T1
   IF (ABS(X1-X).LT.TOLERA.OR.ABS(X0-X1).LT.TOLERA) GOTO 10
9  continue
   CP=A1*(A2*CP+(T0(1)-T1)/(X0-X1)/PL)
   IF (I.EQ.1) PL=T1+459.670d0
   T=T1+CP*(X-X1)*PL
   IF (T.GT.TR) T=TR
   XGI(2)=HCL67(P,T,XGI(1))
   X2=XGI(I)
   IF (ABS(X2-X).LT.TOLERA) GOTO 10
   ICNT=ICNT+1
   IF (ICNT.GT.50) GOTO 12
   X0=X1
   T0(1)=T1
   X1=X2
   T1=T
   IF (ICNT.LT.5) GOTO 9
   A1=0.020d0
   A2=49.0d0
   CP=CP*0.9750d0
   IF (T.EQ.TR.AND.X2.LT.X) TR=TR+DT
   PP=PSL67(TR)
   IF (PP.GT.P) TR=TR-DT
   IF (TR.GT.705.470d0) TR=705.470d0
   DT=0.90d0*DT
   GOTO 9
10 continue
   T0(1)=T
   T=T+CP*(X-X2)*PL
11 continue
   VX=T0(1)+459.670d0+0.50d0*(T-T0(1))
   IF (I.EQ.2) VX=1.0d0/VX
   M3I=3-I
   XGI(M3I)=XGI(M3I)+(X-XGI(I))*VX
   GOTO 13
12 continue
   J=-15
   IF (I.LT.2) GOTO 6
   J=-13
   GOTO 6
13 continue
   tpsl67=T
   M3I=3-I
   SCORE=XGI(M3I)
   END FUNCTION tpsl67
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
FUNCTION tsl67(PIN)
implicit none
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
SAVE
COMMON /NUST/TCORE, SCORE, VCORE, DPDT
DATA(B(I), I = 1, 12)/     &
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
      K = 1
      GOTO 1
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
      ENTRY tsl167(PIN)
      K = 2
1     CONTINUE
      I = 1
      TOL = 1.0d-4
      IF (K .EQ. 2) TOL = 1.0d-6
      IF (PIN .GT. 2700.0d0) TOL = 1.d-9
      IF (PIN .GT. 3200.0d0) TOL = 1.d-10
      F = 1.0d0
      IF (PIN .LE. 3208.234765d0) THEN
         tsl67 = 705.47d0
         IF (PIN .LT. 3208.2347d0) THEN
            I =  -1
            TX = 1.0d0
            TY = (LOG(3529.058235d0/PIN)**0.4d0 - 1.48047125d0)/( - 1.089944005d0)
            Y = 2.0d0*TY
            W = B(1) + TY*B(2)
            DO N = 3, 12
               TZ = Y*TY - TX
               W = W + TZ*B(N)
               TX = TY
               TY = TZ
            enddo
            tsl67 = 1165.14d0/W - 459.67d0
         ENDIF
         TY = .01d0
         Y = 1.0d0
         IF (DBLE(tsl67) .GT. 705.47d0) tsl67 = 705.47d0
4        CONTINUE
         TSLDUM=tsl67  ! passing tsl67 hits a bug in the gfortran compiler (Nov 3 2006)
         PA = PSL167(TSLDUM)
         DP = PIN - PA
         PR = DP/PIN
         tsl67 = tsl67 + F*DP/DPDT
         IF (ABS(PR) .LT. TOL) GOTO 8
         IF (Y .LE. 29.0d0) THEN
            Y = Y + 1.0d0
            F = F*0.99d0
            IF (DBLE(tsl67 - 705.47d0) .GE. 0.0d0) THEN
               TY = 0.9d0*TY
               tsl67 = tsl67 - TY
            ENDIF
            GOTO 4
         ENDIF
      ENDIF
      CALL STER67('tsl67', I, PIN, 0.0d0)
8     CONTINUE
      END FUNCTION tsl67
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
FUNCTION tslh67(ENTH)
implicit none
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
      SAVE
      DIMENSION A(21), CPC(3), HT(3)
!     each group of 7 stored backward so that we can nest in do loop
      DATA(A(I), I = 1, 21)/    &
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
      DATA(HT(I), I = 1, 3)/375.0d0, 707.0d0, 906.0d0/
      DATA(CPC(I), I = 1, 3)/.96195681d0, 7.82249768d-4, -2.09499151d-6/
      H = ENTH
      DO J = 1, 3
         IF (H .LE. HT(J)) GOTO 3
      ENDDO
      CALL STER67('tslh67', 3, ENTH, 0.0d0)
      GOTO 2
3     CONTINUE
      IF (H .LT. 904.0d0) THEN
         T0 = A(J)
         DO I = J, 18, 3
            T0 = T0*H + A(I + 3)
         ENDDO
         tslh67 = T0
         IF (H .LE. 716.0d0) THEN
            IF (H .LT. 337.0d0 .OR. H .GT. 356.0d0) GOTO 2
         ENDIF
         IF (T0 .GT. 705.47d0) T0 = 705.47d0
         T1 = 0.0d0
         H0 = HSL67(T0)
         H2 = H0
         CP = (CPC(3)*H + CPC(2))*H + CPC(1)
         IF (CP .LT. .01d0) CP = .01d0
9        CONTINUE
         IF (ABS(H2 - H) .GE. 0.04d0) THEN
            IF (T1 .LE. 0.0d0) THEN
               T1 = T0 + CP*(H - H0)
               tslh67 = T1
               IF (T1 .GT. 705.47d0) T1 = 705.47d0
               H1 = HSL67(T1)
               IF (ABS(H1 - H) .LT. 0.04d0) GOTO 2
               CP = (T0 - T1)/(H0 - H1)
            ELSE
               IF (tslh67 .LT. 705.3d0) THEN
                  CP = 0.5d0*(CP + (T1 - tslh67)/(H1 - H2))
               ELSE
                  CP = 0.9d0*CP
               ENDIF
               T0 = T1
               H0 = H1
               T1 = tslh67
               H1 = H2
            ENDIF
            tslh67 = T0 + (H - H0)*CP
            IF (tslh67 .GT. 705.47d0) tslh67 = 705.47d0
            H2 = HSL67(tslh67)
            GOTO 9
         ENDIF
      ELSE
         tslh67 = 705.46831d0 + (H - 904.0d0)*.000845d0
      ENDIF
2     CONTINUE
      END FUNCTION tslh67
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
FUNCTION visl67(P, T)
implicit none
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
      SAVE
      DIMENSION B(3), C0(5), C1(8), C2(10)
      DATA(B(I), I = 1, 3)/    &
     &      1.021d2,           &
     &      6.765d2,           &
     &      3.53d2/
      DATA(C0(I), I = 1, 5)/   &
     &      7.5213978d1,       &
     &     -1.1879721d3,       &
     &      6.6840006d3,       &
     &     -1.5488609d4,       &
     &      1.1802979d4/
      DATA(C1(I), I = 1, 8)/   &
     &     -4.1416918d1,       &
     &      9.1177974d2,       &
     &     -8.2709230d3,       &
     &      3.9905545d4,       &
     &     -1.0974987d5,       &
     &      1.6919860d5,       &
     &     -1.3050794d5,       &
     &      3.6396424d4/
      DATA(C2(I), I = 1, 10)/  &
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
      DENS = VCL67(P, T)
      GOTO 1
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
      ENTRY visv67(P, T)
      C1T = HSS67(P, T, C2T, DENS)
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
1     CONTINUE
      TAU = (T - 32.0d0)/180.0d0
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      DENS = 0.0160184634d0/DENS
      U1 = 0.0d0
      DO I = 1, 3
         U1 = (U1 + B(I))*DENS
      ENDDO
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      TAU10 = TAU**10
      C3T = 1.0d0 + 5.739225024d-32*TAU10*TAU10*TAU10*TAU10*TAU10*TAU**5
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      C1T = C1(1)
      DO I = 2, 8
         C1T = C1T*TAU + C1(I)
      ENDDO
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      C0T = C0(1)
      DO I = 2, 5
         C0T = C0T*TAU + C0(I)
      ENDDO
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      C2T = C2(1)
      DO I = 2, 10
         C2T = C2T*TAU + C2(I)
      ENDDO
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      U1= U1 + 80.4d0 + 40.7d0 * TAU + (-0.8d0 + DENS * ((C2T * DENS + C1T) * DENS + C0T)) / C3T
      visl67 = 0.671968975d-7*U1
      END FUNCTION visl67
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
FUNCTION hcl67(P, T, S)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_24="&
&@(#)M_steam67::hcl67(3f): calculate specific enthalpy and entropy of water at pressure P and temp T"

doubleprecision             :: hcl67  ! specific enthalpy, in BTU/LB.
doubleprecision,intent(in)  :: p      ! water pressure, in PSIA.
doubleprecision,intent(in)  :: t      ! water temperature, in degrees F.
doubleprecision,intent(out) :: s      ! specific entropy, in BTU/LB-DEGF
doubleprecision             :: dum
save
   IF (p .GT. 16000.0d0) THEN
      CALL ster67('hcl67', 12, p, t)
   ELSE
      hcl67 = hcsl67(p, t, dum, s, 4)
   ENDIF
END FUNCTION hcl67
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
FUNCTION hsl67(T)
implicit none
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision,intent(in) :: t     ! WATER TEMPERATURE, IN DEGREES F
doubleprecision            :: hsl67 ! OUTPUT - SPECIFIC ENTHALPY, IN BTU/LB.
doubleprecision            :: ssl67 ! OUTPUT - SPECIFIC ENTROPY, IN BTU/LB-DEGF
doubleprecision            :: vsl67 ! OUTPUT - SPECIFIC VOLUME.
integer                    :: i
doubleprecision            :: xg(3)
   I = 1
   GOTO 1
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
   ENTRY ssl67(T)
   I = 2
   GOTO 1
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
ENTRY vsl67(T)
   I = 3
   GOTO 1
!-----------------------------------------------------------------------
1  continue
   XG(1) = HCSL67(PSL67(T), T, XG(3), XG(2), I)
   hsl67 = XG(I)
END FUNCTION hsl67
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
FUNCTION hsv67(P, T, S, V)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: hsv67
doubleprecision :: p
doubleprecision :: t
doubleprecision :: s
doubleprecision :: v
SAVE
   IF (P .GT. 3208.234765d0) THEN
      CALL STER67('hsv67', 1, P, 0.0d0)
   ELSE
      T = TSL67(P)
      hsv67 = HSS67(P, T, S, V)
   ENDIF
END FUNCTION hsv67
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
FUNCTION prliq67(P, T)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
DOUBLEPRECISION            :: prliq67 ! output - prandtl number (dimensionless)
DOUBLEPRECISION,INTENT(IN) :: p       ! water pressure, in PSIA.
DOUBLEPRECISION,INTENT(IN) :: t       ! water temperature, in degrees F.
   prliq67 = 3600.0d0*cpl67(p, t)*visl67(p, t)/condl67(p, t)
END FUNCTION prliq67
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
FUNCTION prstm67(P, T)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: prstm67
doubleprecision :: P
doubleprecision :: T
doubleprecision :: v
   prstm67 = 3600.0d0*CPV67(P,T,V)*VISV67(P,T)/CONDV67(P,T)
END FUNCTION prstm67
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
FUNCTION sssicl67(PRES, H, TEMP)
implicit none
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
SAVE
COMMON /NUST/TCORE, SCORE, VCORE, DPDT
COMMON /LIQU/FACTOR
   P = PRES
   FACTOR = 0.0d0
   T = TPHL67(P, H)
GOTO 1
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
ENTRY hssicl67(PRES, S, TEMP)
   p = pres
   FACTOR = 0.0d0
   T = TPSL67(P, S)
1  continue
   TEMP = T
   sssicl67 = SCORE
END FUNCTION sssicl67
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
FUNCTION vcl67(p, t)
implicit none
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
doubleprecision,intent(in) :: p     !  water pressure, in PSIA.
doubleprecision,intent(in) :: t     !  water temperature, in degrees F.
doubleprecision            :: vcl67 !  specific volume.
doubleprecision            :: dum
doubleprecision            :: vcldum
   dum = hcsl67(p, t, vcldum, dum, 5)
   vcl67= vcldum
END FUNCTION vcl67
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
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision,intent(in) :: x
integer,intent(in)         :: n
doubleprecision,intent(in) :: t(2*N) !  was dimension t(2), but highest subscript needed is (2*n) : JSU

integer                    :: i
integer                    :: k
doubleprecision            :: gr167
!-----------------------------------------------------------------------------------------------------------------------------------
   do i = 2, N
      k = i + i
      if (X .le. T(k - 1)) exit
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   if(n.lt.2)then
      call GOTOER67('GOTOER67 (A) N is too small',n)
   else
      GR167 = T(k-2) + (T(k) - T(k-2)) / (T(k-1) - T(k-3)) * (X-T(k-3))
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
FUNCTION GRS67(X, NDX, Y, NDY, XV, N, NRANGE)
implicit none
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
   SAVE
!-ju  DIMENSION X(NDX,3), Y(NDY,3), DX(3), DY(3), YP(2)
   DIMENSION X(NDX, *), Y(NDY, *), DX(3), DY(3), YP(2)
   NRANGE = 0
   IF (XV .LT. X(1, 1)) NRANGE =  - 1
   IF (XV .GT. X(1, N)) NRANGE =  + 1
   DO I = 1, N
      IF (XV .LT. X(1, I)) GOTO 2
      IF (XV .EQ. X(1, I)) GOTO 8
   enddo
   I = N
2  continue
   IF (I .GT. 2) THEN
      IF (I .LT. N) THEN
         NP = 4
         N4 = I + 1
      ELSE
         NP = 3
      ENDIF
      N1 = I - 2
      N2 = I - 1
      N3 = I
   ELSE
      N1 = 3
      N2 = 2
      N3 = 1
      NP = 3
   ENDIF
   DX(1) = X(1, N2) - X(1, N1)
   DY(1) = Y(1, N2) - Y(1, N1)
   DX(2) = X(1, N3) - X(1, N2)
   DY(2) = Y(1, N3) - Y(1, N2)
   R = (XV - X(1, N2))/DX(2)
   YP(1) = (DY(1)*DX(2)**2 + DY(2)*DX(1)**2)/(DX(1)*(DX(1) + DX(2)))
   IF (NP .EQ. 4) THEN
      DX(3) = X(1, N4) - X(1, N3)
      DY(3) = Y(1, N4) - Y(1, N3)
!#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#=#
      YP(2)=(DY(2)*DX(3)**2+DY(3)*DX(2)**2)/(DX(3)*(DX(2)+DX(3)))
      GRS67 = Y(1,N2)+R*(YP(1)+R*(3.0d0*DY(2)-2.0d0*YP(1)-YP(2)+R *(YP(1) + YP(2) - 2.0d0*DY(2))))
   ELSE
      GRS67 = Y(1, N2) + R*(YP(1) + R*(DY(2) - YP(1)))
   ENDIF
   GOTO 9
8  continue
   GRS67 = Y(1, I)
9  continue
END FUNCTION GRS67
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
FUNCTION p23t67(TIN)
implicit none
! Copyright (C) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_25="@(#)M_steam67::p23t67(3f)"

doubleprecision :: p23t67
doubleprecision :: tin
doubleprecision :: t
doubleprecision :: theta
SAVE
!     argument is temperature
!     returns with pressure
!     boundary between regions 2 and 3
   T = TIN
   p23t67 = 0.0d0
   IF (T .GE. 32.0d0) THEN
      IF (T .GT. 1600.0d0) CALL STER67(' p23t67', 2, T, 0.0d0)
      THETA = (T + 459.67d0)/1165.14d0
      p23t67 = (1.574373327d1 + THETA*( - 3.417061978d1 + THETA *1.931380707d1))*3208.234759d0
   ENDIF
END FUNCTION p23t67
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
FUNCTION vliq67(PIN, TIN, VMIN, VMAX)
implicit none
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
SAVE
DIMENSION T(8), V(8), VT(30), PT(6), TT(6), VX(6)
DATA(VT(I), I = 1, 30)/    &
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
      DATA(PT(I), I = 1, 6)/     &
     &    0.0d0,         &
     &  276.0d0,         &
     &  414.0d0,         &
     &  552.0d0,         &
     &  828.0d0,         &
     & 1000.0d0/
      DATA(TT(I), I = 1, 6)/     &
     &  348.89d0,        &
     &  354.45d0,        &
     &  360.0d0,         &
     &  365.55d0,        &
     &  371.11d0,        &
     &  375.0d0/
      DATA(T(I), I=1, 8)/        &
     &  350.0d0,         &
     &  360.0d0,         &
     &  370.0d0,         &
     &  371.0d0,         &
     &  372.0d0,         &
     &  373.0d0,         &
     &  374.0d0,         &
     &  374.15d0/
      DATA(V(I), I = 1, 8)/      &
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
   PT(1) = (PSL67(TIN*1.8d0 + 32.0d0))/14.503773773d0
   VX(1) = GRS67(T(1), 1, V(1), 1, TIN, 8, I)
   DO I = 2, 6
      VX(I) = GRS67(TT(1), 1, VT(6*I - 11), 1, TIN, 6, J)
   enddo
   vliq67 = GRS67(PT(1), 1, VX(1), 1, PIN, 6, I)
   VMIN = 0.98d0*VX(6)
   VMAX = VX(1)*1.01d0
END FUNCTION vliq67
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
implicit none
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
implicit none
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
FUNCTION zsrh67(P1, P2, H1, S, T1, T2)
implicit none
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.

character(len=*),parameter::ident_26="&
&@(#)M_steam67::zsrh67(3f) calculate remaining variables at extremities of isentropic process in compressed liquid region."

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
FUNCTION zsrt67(P1, P2, T1, S, H1, T2)
implicit none
! Copyright (c) 1969,1990 Westinghouse Electric Corporation.
!external HCL67
doubleprecision :: zsrt67
doubleprecision :: h1
doubleprecision :: p1
doubleprecision :: p2
doubleprecision :: s
doubleprecision :: t1
doubleprecision :: t2
SAVE
   H1 = HCL67(P1, T1, S)
   zsrt67 = HSSICL67(P2, S, T2)
END FUNCTION zsrt67
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
SUBROUTINE GOTOER67(NAME, I)
implicit none
! Copyright (c) 1990 Westinghouse Electric Corporation.
!
!     when an error occurs in a computed goto, print a string
!     of the form " ** error in routine_name (letter) varname = iiiii "
!     to standard error on unicos, to logfile on cos

character(len=*),parameter::ident_27="@(#)M_steam67::gotoer67(3fp): print error message and stop program"

CHARACTER(len=*) :: NAME, STRING*80
INTEGER I
   WRITE (STRING, "(' ** ERROR IN ', A, I5)") NAME(:MAX(LEN(NAME), 70)), I
   CALL ZQJREM67(' STEAM TABLE ERROR')
   CALL ZQJREM67(STRING)
   STOP
END SUBROUTINE GOTOER67
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
SUBROUTINE STER67(NAME, I, A, B)
implicit none
!  Copyright (C) 1969,1990 Westinghouse Electric Corporation.
doubleprecision :: a
doubleprecision :: b
integer         :: i
integer         :: i1
integer         :: i2
integer         :: im
integer         :: j
CHARACTER MA(5)*6, M(2)*18, NAME*(*), MP*6
SAVE MA, M
DATA M/' OUT OF RANGE IN  ', 'NON CONVERGENT IN '/
DATA(MA(J), J = 1, 5)/'PRESS=','TEMP=','ENTH=','VOL=','ENTR='/
   IM = 1
   IF (I .LT. 0) IM = 2
   I2 = IABS(I)
   I1 = I2/10
   I2 = I2 - I1*10
   MP = MA(I2)
   IF (I1 .NE. 0) MP = MA(I1)
   PRINT 2, M(IM), NAME, MP, A
   IF (I1 .NE. 0) PRINT 3, MA(I2), B
   CALL zqjrem67(' STEAM TABLE ERROR')
   STOP
2  FORMAT (1X, A18, A6, 2X, A6, G20.13)
3  FORMAT (27X, A6, 1X, G20.13)
END SUBROUTINE STER67
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
implicit none
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
implicit none

character(len=*),parameter::ident_28="@(#)M_steam67::steam67(3f):print pedigree of library and return version value"

integer,intent(in)           :: io
character(len=*),intent(out) :: version
   version='2.2'
   if(io.ge.0)then
   write(io,*)'******************************************************'
   write(io,*)'*      Westinghouse Electric Company, LLC.           *'
   write(io,*)'*        ASME-67 Steam Table Routines                *'
   write(io,*)'******************************************************'
   write(io,*)'* NOTE: This is an unconfigured version of the       *'
   write(io,*)'* Westinghouse Steam Table library.                  *'
   write(io,*)'******************************************************'
   write(io,*)'Compiled on:'
   write(io,*)'Build Target ....Linux_ifort'
   write(io,*)'System...........Linux'
   write(io,*)'O.S. Release.....5.4.0-66-generic'
   write(io,*)'O.S. Version..... ' &
!#cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
     &//'#74-Ubuntu ' &
     &//'SMP ' &
     &//'Wed ' &
     &//'Jan ' &
     &//'27 ' &
     &//'22:54:38 ' &
     &//'UTC ' &
     &//'2021 ' &
     &//' '
      write(io,*)' Hardware Name...x86_64'
   write(io,*)'Compile Date:Thu 01 Jul 2021 08:50:48 AM EDT'
   write(io,*)'******************************************************'
   endif
   END subroutine steamv67
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_steam67()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_condl67()
   call test_condv67()
   call test_cpl67()
   call test_cpv67()
   call test_crflo67()
   call test_critvs67()
   call test_critvw67()
   call test_crvel67()
   call test_hcl67()
   call test_hcsl67()
   call test_hcslv167()
   call test_hcslv267()
   call test_hisiss67()
   call test_hpsiss67()
   call test_hsl67()
   call test_hss167()
   call test_hss67()
   call test_hssicl67()
   call test_hssiss67()
   call test_hsv67()
   call test_pls67()
   call test_prliq67()
   call test_prstm67()
   call test_psl167()
   call test_psl67()
   call test_psv167()
   call test_psv267()
   call test_psv67()
   call test_sisiss67()
   call test_spsiss67()
   call test_ssl67()
   call test_sssicl67()
   call test_sssiss67()
   call test_steamv67()
   call test_tph67()
   call test_tphl67()
   call test_tps67()
   call test_tpsl67()
   call test_tsl167()
   call test_tsl67()
   call test_tslh67()
   call test_vcl67()
   call test_visl67()
   call test_visv67()
   call test_vsl67()
   call test_zsdh67()
   call test_zsdt67()
   call test_zsrh67()
   call test_zsrt67()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_condl67()

   call unit_check_start('condl67',msg='')
   !!call unit_check('condl67', 0.eq.0, 'checking',100
   call unit_check_done('condl67',msg='')
end subroutine test_condl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_condv67()

   call unit_check_start('condv67',msg='')
   !!call unit_check('condv67', 0.eq.0, 'checking',100
   call unit_check_done('condv67',msg='')
end subroutine test_condv67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cpl67()

   call unit_check_start('cpl67',msg='')
   !!call unit_check('cpl67', 0.eq.0, 'checking',100
   call unit_check_done('cpl67',msg='')
end subroutine test_cpl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cpv67()

   call unit_check_start('cpv67',msg='')
   !!call unit_check('cpv67', 0.eq.0, 'checking',100
   call unit_check_done('cpv67',msg='')
end subroutine test_cpv67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_crflo67()

   call unit_check_start('crflo67',msg='')
   !!call unit_check('crflo67', 0.eq.0, 'checking',100
   call unit_check_done('crflo67',msg='')
end subroutine test_crflo67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_critvs67()

   call unit_check_start('critvs67',msg='')
   !!call unit_check('critvs67', 0.eq.0, 'checking',100
   call unit_check_done('critvs67',msg='')
end subroutine test_critvs67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_critvw67()

   call unit_check_start('critvw67',msg='')
   !!call unit_check('critvw67', 0.eq.0, 'checking',100
   call unit_check_done('critvw67',msg='')
end subroutine test_critvw67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_crvel67()

   call unit_check_start('crvel67',msg='')
   !!call unit_check('crvel67', 0.eq.0, 'checking',100
   call unit_check_done('crvel67',msg='')
end subroutine test_crvel67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hcl67()

   call unit_check_start('hcl67',msg='')
   !!call unit_check('hcl67', 0.eq.0, 'checking',100
   call unit_check_done('hcl67',msg='')
end subroutine test_hcl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hcsl67()

   call unit_check_start('hcsl67',msg='')
   !!call unit_check('hcsl67', 0.eq.0, 'checking',100
   call unit_check_done('hcsl67',msg='')
end subroutine test_hcsl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hcslv167()

   call unit_check_start('hcslv167',msg='')
   !!call unit_check('hcslv167', 0.eq.0, 'checking',100
   call unit_check_done('hcslv167',msg='')
end subroutine test_hcslv167
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hcslv267()

   call unit_check_start('hcslv267',msg='')
   !!call unit_check('hcslv267', 0.eq.0, 'checking',100
   call unit_check_done('hcslv267',msg='')
end subroutine test_hcslv267
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hisiss67()

   call unit_check_start('hisiss67',msg='')
   !!call unit_check('hisiss67', 0.eq.0, 'checking',100
   call unit_check_done('hisiss67',msg='')
end subroutine test_hisiss67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hpsiss67()

   call unit_check_start('hpsiss67',msg='')
   !!call unit_check('hpsiss67', 0.eq.0, 'checking',100
   call unit_check_done('hpsiss67',msg='')
end subroutine test_hpsiss67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hsl67()

   call unit_check_start('hsl67',msg='')
   !!call unit_check('hsl67', 0.eq.0, 'checking',100
   call unit_check_done('hsl67',msg='')
end subroutine test_hsl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hss167()

   call unit_check_start('hss167',msg='')
   !!call unit_check('hss167', 0.eq.0, 'checking',100
   call unit_check_done('hss167',msg='')
end subroutine test_hss167
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hss67()

   call unit_check_start('hss67',msg='')
   !!call unit_check('hss67', 0.eq.0, 'checking',100
   call unit_check_done('hss67',msg='')
end subroutine test_hss67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hssicl67()

   call unit_check_start('hssicl67',msg='')
   !!call unit_check('hssicl67', 0.eq.0, 'checking',100
   call unit_check_done('hssicl67',msg='')
end subroutine test_hssicl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hssiss67()

   call unit_check_start('hssiss67',msg='')
   !!call unit_check('hssiss67', 0.eq.0, 'checking',100
   call unit_check_done('hssiss67',msg='')
end subroutine test_hssiss67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hsv67()

   call unit_check_start('hsv67',msg='')
   !!call unit_check('hsv67', 0.eq.0, 'checking',100
   call unit_check_done('hsv67',msg='')
end subroutine test_hsv67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pls67()

   call unit_check_start('pls67',msg='')
   !!call unit_check('pls67', 0.eq.0, 'checking',100
   call unit_check_done('pls67',msg='')
end subroutine test_pls67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_prliq67()

   call unit_check_start('prliq67',msg='')
   !!call unit_check('prliq67', 0.eq.0, 'checking',100
   call unit_check_done('prliq67',msg='')
end subroutine test_prliq67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_prstm67()

   call unit_check_start('prstm67',msg='')
   !!call unit_check('prstm67', 0.eq.0, 'checking',100
   call unit_check_done('prstm67',msg='')
end subroutine test_prstm67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_psl167()

   call unit_check_start('psl167',msg='')
   !!call unit_check('psl167', 0.eq.0, 'checking',100
   call unit_check_done('psl167',msg='')
end subroutine test_psl167
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_psl67()

   call unit_check_start('psl67',msg='')
   !!call unit_check('psl67', 0.eq.0, 'checking',100
   call unit_check_done('psl67',msg='')
end subroutine test_psl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_psv167()

   call unit_check_start('psv167',msg='')
   !!call unit_check('psv167', 0.eq.0, 'checking',100
   call unit_check_done('psv167',msg='')
end subroutine test_psv167
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_psv267()

   call unit_check_start('psv267',msg='')
   !!call unit_check('psv267', 0.eq.0, 'checking',100
   call unit_check_done('psv267',msg='')
end subroutine test_psv267
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_psv67()

   call unit_check_start('psv67',msg='')
   !!call unit_check('psv67', 0.eq.0, 'checking',100
   call unit_check_done('psv67',msg='')
end subroutine test_psv67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sisiss67()

   call unit_check_start('sisiss67',msg='')
   !!call unit_check('sisiss67', 0.eq.0, 'checking',100
   call unit_check_done('sisiss67',msg='')
end subroutine test_sisiss67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_spsiss67()

   call unit_check_start('spsiss67',msg='')
   !!call unit_check('spsiss67', 0.eq.0, 'checking',100
   call unit_check_done('spsiss67',msg='')
end subroutine test_spsiss67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ssl67()

   call unit_check_start('ssl67',msg='')
   !!call unit_check('ssl67', 0.eq.0, 'checking',100
   call unit_check_done('ssl67',msg='')
end subroutine test_ssl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sssicl67()

   call unit_check_start('sssicl67',msg='')
   !!call unit_check('sssicl67', 0.eq.0, 'checking',100
   call unit_check_done('sssicl67',msg='')
end subroutine test_sssicl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sssiss67()

   call unit_check_start('sssiss67',msg='')
   !!call unit_check('sssiss67', 0.eq.0, 'checking',100
   call unit_check_done('sssiss67',msg='')
end subroutine test_sssiss67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_steamv67()

   call unit_check_start('steamv67',msg='')
   !!call unit_check('steamv67', 0.eq.0, 'checking',100
   call unit_check_done('steamv67',msg='')
end subroutine test_steamv67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tph67()

   call unit_check_start('tph67',msg='')
   !!call unit_check('tph67', 0.eq.0, 'checking',100
   call unit_check_done('tph67',msg='')
end subroutine test_tph67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tphl67()

   call unit_check_start('tphl67',msg='')
   !!call unit_check('tphl67', 0.eq.0, 'checking',100
   call unit_check_done('tphl67',msg='')
end subroutine test_tphl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tps67()

   call unit_check_start('tps67',msg='')
   !!call unit_check('tps67', 0.eq.0, 'checking',100
   call unit_check_done('tps67',msg='')
end subroutine test_tps67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tpsl67()

   call unit_check_start('tpsl67',msg='')
   !!call unit_check('tpsl67', 0.eq.0, 'checking',100
   call unit_check_done('tpsl67',msg='')
end subroutine test_tpsl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tsl167()

   call unit_check_start('tsl167',msg='')
   !!call unit_check('tsl167', 0.eq.0, 'checking',100
   call unit_check_done('tsl167',msg='')
end subroutine test_tsl167
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tsl67()

   call unit_check_start('tsl67',msg='')
   !!call unit_check('tsl67', 0.eq.0, 'checking',100
   call unit_check_done('tsl67',msg='')
end subroutine test_tsl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_tslh67()

   call unit_check_start('tslh67',msg='')
   !!call unit_check('tslh67', 0.eq.0, 'checking',100
   call unit_check_done('tslh67',msg='')
end subroutine test_tslh67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_vcl67()

   call unit_check_start('vcl67',msg='')
   !!call unit_check('vcl67', 0.eq.0, 'checking',100
   call unit_check_done('vcl67',msg='')
end subroutine test_vcl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_visl67()

   call unit_check_start('visl67',msg='')
   !!call unit_check('visl67', 0.eq.0, 'checking',100
   call unit_check_done('visl67',msg='')
end subroutine test_visl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_visv67()

   call unit_check_start('visv67',msg='')
   !!call unit_check('visv67', 0.eq.0, 'checking',100
   call unit_check_done('visv67',msg='')
end subroutine test_visv67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_vsl67()

   call unit_check_start('vsl67',msg='')
   !!call unit_check('vsl67', 0.eq.0, 'checking',100
   call unit_check_done('vsl67',msg='')
end subroutine test_vsl67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_zsdh67()

   call unit_check_start('zsdh67',msg='')
   !!call unit_check('zsdh67', 0.eq.0, 'checking',100
   call unit_check_done('zsdh67',msg='')
end subroutine test_zsdh67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_zsdt67()

   call unit_check_start('zsdt67',msg='')
   !!call unit_check('zsdt67', 0.eq.0, 'checking',100
   call unit_check_done('zsdt67',msg='')
end subroutine test_zsdt67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_zsrh67()

   call unit_check_start('zsrh67',msg='')
   !!call unit_check('zsrh67', 0.eq.0, 'checking',100
   call unit_check_done('zsrh67',msg='')
end subroutine test_zsrh67
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_zsrt67()

   call unit_check_start('zsrt67',msg='')
   !!call unit_check('zsrt67', 0.eq.0, 'checking',100
   call unit_check_done('zsrt67',msg='')
end subroutine test_zsrt67
!===================================================================================================================================
end subroutine test_suite_M_steam67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module m_steam67
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

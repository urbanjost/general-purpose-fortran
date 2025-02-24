










module M_LA
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
private

public mat_wlog
public mat_wdiv
public mat_watan

public :: mat_inverse_hilbert
public :: mat_magic
public :: mat_pythag
public :: mat_rref

! for other routines
public mat_flop
public mat_wasum
public mat_wdotcr
public mat_wdotci

public mat_wdotur
public mat_wcopy
public mat_wset
public mat_wswap
public mat_wsqrt
public mat_rswap
public mat_wrscal
public mat_wscal
public mat_wmul
public mat_rrot
public mat_rset
public mat_rat
public mat_urand
public mat_wnrm2
public mat_wdotui
public mat_iwamax
public mat_round
public mat_wpofa
public mat_rrotg
public mat_wsign

!public :: matx_waxpy
!public :: ml_comqr3
!public :: ml_corth
!public :: ml_htribk
!public :: ml_htridi
!public :: ml_imtql2
!public :: ml_wgeco
!public :: ml_wgedi
!public :: ml_wgefa
!public :: ml_wgesl
!public :: ml_wqrdc
!public :: ml_wqrsl
!public :: ml_wsvdc

public :: linspace
public :: elementcopy

integer,parameter,private:: sp=kind(1.0),dp=kind(1.0d0)

integer,save             :: LA_FLOP_COUNTER(2)=[0,0]

interface linspace
   module procedure  &
   & linspace_real128, linspace_real64, linspace_real32, &
   & linspace_int64,   linspace_int32,  linspace_int16,  linspace_int8
end interface linspace

interface elementcopy
   module procedure  &
   & elementcopy_real128, elementcopy_real64, elementcopy_real32, &
   & elementcopy_int64,   elementcopy_int32,  elementcopy_int16,  elementcopy_int8
end interface elementcopy

INTERFACE
   SUBROUTINE MATX_WAXPY(N, SR, SI, XR, XI, INCX, YR, YI, INCY)
      import  int32, real64
      integer(kind=int32), INTENT(IN) :: N
      real(kind=real64), INTENT(IN) :: SR
      real(kind=real64), INTENT(IN) :: SI
      real(kind=real64), INTENT(IN) :: XR(*)
      real(kind=real64), INTENT(IN) :: XI(*)
      integer(kind=int32), INTENT(IN) :: INCX
      real(kind=real64) :: YR(*)
      real(kind=real64) :: YI(*)
      integer(kind=int32), INTENT(IN) :: INCY
   END SUBROUTINE MATX_WAXPY
END INTERFACE

INTERFACE
   SUBROUTINE ML_COMQR3(NM, N, LOW, IGH, ORTR, ORTI, HR, HI, WR, WI, ZR, ZI, IERR, JOB)
      import  int32, real64
      integer(kind=int32) :: IGH
      integer(kind=int32) :: N
      integer(kind=int32) :: NM
      integer(kind=int32) :: LOW
      real(kind=real64) :: ORTR(IGH)
      real(kind=real64) :: ORTI(IGH)
      real(kind=real64) :: HR(NM, N)
      real(kind=real64) :: HI(NM, N)
      real(kind=real64) :: WR(N)
      real(kind=real64) :: WI(N)
      real(kind=real64) :: ZR(NM, N)
      real(kind=real64) :: ZI(NM, N)
      integer(kind=int32) :: IERR
      integer(kind=int32) :: JOB
   END SUBROUTINE ML_COMQR3
END INTERFACE

INTERFACE
   SUBROUTINE ML_CORTH(NM, N, LOW, IGH, AR, AI, ORTR, ORTI)
      import  int32, real64
      integer(kind=int32) :: IGH
      integer(kind=int32) :: N
      integer(kind=int32) :: NM
      integer(kind=int32) :: LOW
      real(kind=real64) :: AR(NM, N)
      real(kind=real64) :: AI(NM, N)
      real(kind=real64) :: ORTR(IGH)
      real(kind=real64) :: ORTI(IGH)
   END SUBROUTINE ML_CORTH
END INTERFACE

INTERFACE
   SUBROUTINE ML_HTRIBK(NM, N, AR, AI, TAU, M, ZR, ZI)
      import  int32, real64
      integer(kind=int32) :: M
      integer(kind=int32) :: N
      integer(kind=int32) :: NM
      real(kind=real64) :: AR(NM, N)
      real(kind=real64) :: AI(NM, N)
      real(kind=real64) :: TAU(2, N)
      real(kind=real64) :: ZR(NM, M)
      real(kind=real64) :: ZI(NM, M)
   END SUBROUTINE ML_HTRIBK
END INTERFACE

INTERFACE
   SUBROUTINE ML_HTRIDI(NM, N, AR, AI, D, E, E2, TAU)
      import  int32, real64
      integer(kind=int32) :: N
      integer(kind=int32) :: NM
      real(kind=real64) :: AR(NM, N)
      real(kind=real64) :: AI(NM, N)
      real(kind=real64) :: D(N)
      real(kind=real64) :: E(N)
      real(kind=real64) :: E2(N)
      real(kind=real64) :: TAU(2, N)
   END SUBROUTINE ML_HTRIDI
END INTERFACE

INTERFACE
   SUBROUTINE ML_IMTQL2(NM, N, D, E, Z, IERR, JOB)
      import  int32, real64
      integer(kind=int32) :: N
      integer(kind=int32) :: NM
      real(kind=real64) :: D(N)
      real(kind=real64) :: E(N)
      real(kind=real64) :: Z(NM, N)
      integer(kind=int32) :: IERR
      integer(kind=int32) :: JOB
   END SUBROUTINE ML_IMTQL2
END INTERFACE

INTERFACE
   SUBROUTINE ML_WGECO(AR, AI, LDA, N, IPVT, RCOND, ZR, ZI)
      import  int32, real64
      integer(kind=int32) :: LDA
      real(kind=real64) :: AR(LDA, *)
      real(kind=real64) :: AI(LDA, *)
      integer(kind=int32) :: N
      integer(kind=int32) :: IPVT(*)
      real(kind=real64) :: RCOND
      real(kind=real64) :: ZR(*)
      real(kind=real64) :: ZI(*)
   END SUBROUTINE ML_WGECO
END INTERFACE

INTERFACE
   SUBROUTINE ML_WGEDI(AR, AI, LDA, N, IPVT, DETR, DETI, WORKR, WORKI, JOB)
      import  int32, real64
      integer(kind=int32) :: LDA
      real(kind=real64) :: AR(LDA, *)
      real(kind=real64) :: AI(LDA, *)
      integer(kind=int32) :: N
      integer(kind=int32) :: IPVT(*)
      real(kind=real64) :: DETR(2)
      real(kind=real64) :: DETI(2)
      real(kind=real64) :: WORKR(*)
      real(kind=real64) :: WORKI(*)
      integer(kind=int32) :: JOB
   END SUBROUTINE ML_WGEDI
END INTERFACE

INTERFACE
   SUBROUTINE ML_WGEFA(AR, AI, LDA, N, IPVT, INFO)
      import  int32, real64
      integer(kind=int32) :: LDA
      real(kind=real64) :: AR(LDA, *)
      real(kind=real64) :: AI(LDA, *)
      integer(kind=int32) :: N
      integer(kind=int32) :: IPVT(*)
      integer(kind=int32) :: INFO
   END SUBROUTINE ML_WGEFA
END INTERFACE

INTERFACE
   SUBROUTINE ML_WGESL(AR, AI, LDA, N, IPVT, BR, BI, JOB)
      import  int32, real64
      integer(kind=int32) :: LDA
      real(kind=real64) :: AR(LDA, *)
      real(kind=real64) :: AI(LDA, *)
      integer(kind=int32) :: N
      integer(kind=int32) :: IPVT(*)
      real(kind=real64) :: BR(*)
      real(kind=real64) :: BI(*)
      integer(kind=int32) :: JOB
   END SUBROUTINE ML_WGESL
END INTERFACE

INTERFACE
   SUBROUTINE ML_WQRDC(XR, XI, LDX, N, P, QRAUXR, QRAUXI, JPVT, WORKR, WORKI, JOB)
      import  int32, real64
      integer(kind=int32) :: LDX
      real(kind=real64) :: XR(LDX, *)
      real(kind=real64) :: XI(LDX, *)
      integer(kind=int32) :: N
      integer(kind=int32) :: P
      real(kind=real64) :: QRAUXR(*)
      real(kind=real64) :: QRAUXI(*)
      integer(kind=int32) :: JPVT(*)
      real(kind=real64) :: WORKR(*)
      real(kind=real64) :: WORKI(*)
      integer(kind=int32) :: JOB
   END SUBROUTINE ML_WQRDC
END INTERFACE

INTERFACE
   SUBROUTINE ML_WQRSL(XR, XI, LDX, N, K, QRAUXR, QRAUXI, YR, YI, QYR, QYI, QTYR, QTYI, BR, BI, RSDR, RSDI, XBR, XBI, JOB, INFO)
      import  int32, real64
      integer(kind=int32) :: LDX
      real(kind=real64) :: XR(LDX, *)
      real(kind=real64) :: XI(LDX, *)
      integer(kind=int32) :: N
      integer(kind=int32) :: K
      real(kind=real64) :: QRAUXR(*)
      real(kind=real64) :: QRAUXI(*)
      real(kind=real64) :: YR(*)
      real(kind=real64) :: YI(*)
      real(kind=real64) :: QYR(*)
      real(kind=real64) :: QYI(*)
      real(kind=real64) :: QTYR(*)
      real(kind=real64) :: QTYI(*)
      real(kind=real64) :: BR(*)
      real(kind=real64) :: BI(*)
      real(kind=real64) :: RSDR(*)
      real(kind=real64) :: RSDI(*)
      real(kind=real64) :: XBR(*)
      real(kind=real64) :: XBI(*)
      integer(kind=int32) :: JOB
      integer(kind=int32) :: INFO
   END SUBROUTINE ML_WQRSL
END INTERFACE

INTERFACE
   SUBROUTINE ML_WSVDC(XR, XI, LDX, N, P, SR, SI, ER, EI, UR, UI, LDU, VR, VI, LDV, WORKR, WORKI, JOB, INFO)
      import  int32, real64
      integer(kind=int32) :: LDV
      integer(kind=int32) :: LDU
      integer(kind=int32) :: LDX
      real(kind=real64) :: XR(LDX, *)
      real(kind=real64) :: XI(LDX, *)
      integer(kind=int32) :: N
      integer(kind=int32) :: P
      real(kind=real64) :: SR(*)
      real(kind=real64) :: SI(*)
      real(kind=real64) :: ER(*)
      real(kind=real64) :: EI(*)
      real(kind=real64) :: UR(LDU, *)
      real(kind=real64) :: UI(LDU, *)
      real(kind=real64) :: VR(LDV, *)
      real(kind=real64) :: VI(LDV, *)
      real(kind=real64) :: WORKR(*)
      real(kind=real64) :: WORKI(*)
      integer(kind=int32) :: JOB
      integer(kind=int32) :: INFO
   END SUBROUTINE ML_WSVDC
END INTERFACE

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    elementcopy(3f) - [M_LA] copy elements from IN to OUT regardless
!!    of rank until hit end of one of them
!!
!!##SYNOPSIS
!!
!!     Subroutine elementcopy (IN, OUT)
!!
!!      ${TYPE} (kind=${KIND}), Intent (In) :: IN(..)
!!      ${TYPE} (kind=${KIND})              :: OUT(..)
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Real(kind=real128)
!!       o Integer(kind=int8)
!!       o Integer(kind=int16)
!!       o Integer(kind=int32)
!!       o Integer(kind=int64)
!!
!!##DESCRIPTION
!!
!!    Copy the elements from scalar or array IN to array or scalar OUT
!!    until either the end of IN or OUT is reached, regardless of rank
!!    of the arguments.
!!
!!##OPTIONS
!!     IN          input array or scalar
!!     OUT         output array or scalar
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_elementcopy
!!    use m_la, only : elementcopy
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0:,","))'
!!    real :: b, b1(3), b2(2,3), b3(2,2,2)
!!    real :: c8(8), c6(6), c3(3), c
!!    integer :: ib, ib1(3), ib2(2,3), ib3(2,2,2)
!!    integer :: ic8(8), ic6(6), ic3(3), ic
!!       ! default real
!!       call elementcopy(100.0,b)
!!       write(*,g)'b',b
!!       call elementcopy([1.0,2.0,3.0],b1)
!!       write(*,g)'b1',b1
!!       call elementcopy(reshape([1.0,2.0,3.0,4.0,5.0,6.0],[2,3]),b2)
!!       write(*,g)'b2',b2
!!       call elementcopy(reshape([1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0],[2,2,2]),b3)
!!       write(*,g)'b3',b3
!!       call elementcopy(b3,c8) ! pack
!!       write(*,g)'c8',c8
!!       call elementcopy(b3*10,c3) ! smaller
!!       write(*,g)'c3',c3
!!       call elementcopy(pack(b3*111.0,.true.),b) ! to scalar
!!       write(*,g)'b',b
!!       c6=-999.0
!!       call elementcopy(b1*10,c6) ! bigger
!!       write(*,g)'c6',c6
!!       call elementcopy(b3(2:,2,2),c) !  to scalar from vector
!!       write(*,g)'c',c
!!       call elementcopy(b3(2,1,1),c) !  to scalar from element
!!       write(*,g)'c',c
!!       call elementcopy(b3,c) !  to scalar
!!       write(*,g)'c',c
!!       ! default integer
!!       call elementcopy(100,ib)
!!       write(*,g)'ib',ib
!!       call elementcopy([1,2,3],ib1)
!!       write(*,g)'ib1',ib1
!!       call elementcopy(reshape([1,2,3,4,5,6],[2,3]),ib2)
!!       write(*,g)'ib2',ib2
!!       call elementcopy(reshape([1,2,3,4,5,6,7,8],[2,2,2]),ib3)
!!       write(*,g)'ib3',ib3
!!       call elementcopy(ib3,ic8) ! pack
!!       write(*,g)'ic8',ic8
!!       call elementcopy(ib3*10,ic3) ! smaller
!!       write(*,g)'ic3',ic3
!!       call elementcopy(pack(ib3*111,.true.),ib) ! to scalar
!!       write(*,g)'ib',ib
!!       ic6=-999
!!       call elementcopy(ib1*10,ic6) ! bigger
!!       write(*,g)'ic6',ic6
!!       call elementcopy(ib3(2:,2,2),ic) !  to scalar from vector
!!       write(*,g)'ic',ic
!!       call elementcopy(ib3(2,1,1),ic) !  to scalar from element
!!       write(*,g)'ic',ic
!!       call elementcopy(ib3,ic) !  to scalar
!!       write(*,g)'ic',ic
!!       !
!!       tesseract: block
!!       integer :: box(2,3,4,5)
!!       integer :: i
!!          call elementcopy([(i,i=1,size(box))],box)
!!          write(*,g)'box',box
!!       endblock tesseract
!!    end program demo_elementcopy
!!
!!   Results:
!!
!!    b,100.0000
!!    b1,1.00000,2.00000,3.00000
!!    b2,1.00000,2.00000,3.00000,4.00000,5.00000,6.00000
!!    b3,1.00000,2.00000,3.00000,4.00000,5.00000,6.00000,7.00000,8.00000
!!    c8,1.00000,2.00000,3.00000,4.00000,5.00000,6.00000,7.00000,8.00000
!!    c3,10.0000,20.0000,30.0000
!!    b,111.0000
!!    c6,10.00000,20.00000,30.00000,-999.0000,-999.0000,-999.0000
!!    c,8.000000
!!    c,2.000000
!!    c,1.000000
!!    ib,100
!!    ib1,1,2,3
!!    ib2,1,2,3,4,5,6
!!    ib3,1,2,3,4,5,6,7,8
!!    ic8,1,2,3,4,5,6,7,8
!!    ic3,10,20,30
!!    ib,111
!!    ic6,10,20,30,-999,-999,-999
!!    ic,8
!!    ic,2
!!    ic,1
!!    box,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,
!!    19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,
!!    36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,
!!    53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,
!!    70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,
!!    87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,
!!    103,104,105,106,107,108,109,110,111,112,113,114,115,116,
!!    117,118,119,120
!!
!!##AUTHOR
!!    John S. Urban, 2022.05.07
!!##LICENSE
!!    CC0-1.0
subroutine elementcopy_real32(a1,a2) ! using assumed rank
real(kind=real32),intent(in) :: a1(..)
real(kind=real32)            :: a2(..)
real(kind=real32)            :: one(1), two(1)
   SELECT RANK(a1)
   RANK(0)
      one=a1
      SELECT RANK(a2)
      RANK(0); call step2(one,1)
      RANK(1); call step2(one,1)
      RANK(2); call step2(one,1)
      RANK(3); call step2(one,1)
      RANK(4); call step2(one,1)
      RANK(5); call step2(one,1)
      RANK(6); call step2(one,1)
      RANK(7); call step2(one,1)
      RANK(8); call step2(one,1)
      RANK(9); call step2(one,1)
      RANK(10); call step2(one,1)
      RANK(11); call step2(one,1)
      RANK(12); call step2(one,1)
      RANK(13); call step2(one,1)
      RANK(14); call step2(one,1)
      RANK(15); call step2(one,1)
      END SELECT
   RANK(1)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(2)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(3)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   END SELECT
contains
subroutine step2(a3,isz)
integer :: isz
real(kind=real32),intent(in) :: a3(isz)
   SELECT RANK(a2)
   RANK(0); call ecopy(a3,1,two,1);a2=two(1)
   RANK(1); call ecopy(a3,size(a3),a2,size(a2))
   RANK(2); call ecopy(a3,size(a3),a2,size(a2))
   RANK(3); call ecopy(a3,size(a3),a2,size(a2))
   RANK(4); call ecopy(a3,size(a3),a2,size(a2))
   RANK(5); call ecopy(a3,size(a3),a2,size(a2))
   RANK(6); call ecopy(a3,size(a3),a2,size(a2))
   RANK(7); call ecopy(a3,size(a3),a2,size(a2))
   RANK(8); call ecopy(a3,size(a3),a2,size(a2))
   RANK(9); call ecopy(a3,size(a3),a2,size(a2))
   RANK(10); call ecopy(a3,size(a3),a2,size(a2))
   RANK(11); call ecopy(a3,size(a3),a2,size(a2))
   RANK(12); call ecopy(a3,size(a3),a2,size(a2))
   RANK(13); call ecopy(a3,size(a3),a2,size(a2))
   RANK(14); call ecopy(a3,size(a3),a2,size(a2))
   RANK(15); call ecopy(a3,size(a3),a2,size(a2))
   END SELECT
end subroutine step2

subroutine ecopy(a1,n,a2,m)
integer,intent(in) :: n,m
real(kind=real32),intent(in) :: a1(n) ! dimensioned with n, there is no rank/shape check
real(kind=real32)            :: a2(m) ! dimensioned with m, there is no rank/shape check
integer :: ismall
   ismall=min(n,m)       ! should warn as well
   a2(:ismall)=a1(:ismall)
end subroutine ecopy

end subroutine elementcopy_real32

subroutine elementcopy_real64(a1,a2) ! using assumed rank
real(kind=real64),intent(in) :: a1(..)
real(kind=real64)            :: a2(..)
real(kind=real64)            :: one(1), two(1)
   SELECT RANK(a1)
   RANK(0)
      one=a1
      SELECT RANK(a2)
      RANK(0); call step2(one,1)
      RANK(1); call step2(one,1)
      RANK(2); call step2(one,1)
      RANK(3); call step2(one,1)
      RANK(4); call step2(one,1)
      RANK(5); call step2(one,1)
      RANK(6); call step2(one,1)
      RANK(7); call step2(one,1)
      RANK(8); call step2(one,1)
      RANK(9); call step2(one,1)
      RANK(10); call step2(one,1)
      RANK(11); call step2(one,1)
      RANK(12); call step2(one,1)
      RANK(13); call step2(one,1)
      RANK(14); call step2(one,1)
      RANK(15); call step2(one,1)
      END SELECT
   RANK(1)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(2)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(3)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   END SELECT
contains
subroutine step2(a3,isz)
integer :: isz
real(kind=real64),intent(in) :: a3(isz)
   SELECT RANK(a2)
   RANK(0); call ecopy(a3,1,two,1);a2=two(1)
   RANK(1); call ecopy(a3,size(a3),a2,size(a2))
   RANK(2); call ecopy(a3,size(a3),a2,size(a2))
   RANK(3); call ecopy(a3,size(a3),a2,size(a2))
   RANK(4); call ecopy(a3,size(a3),a2,size(a2))
   RANK(5); call ecopy(a3,size(a3),a2,size(a2))
   RANK(6); call ecopy(a3,size(a3),a2,size(a2))
   RANK(7); call ecopy(a3,size(a3),a2,size(a2))
   RANK(8); call ecopy(a3,size(a3),a2,size(a2))
   RANK(9); call ecopy(a3,size(a3),a2,size(a2))
   RANK(10); call ecopy(a3,size(a3),a2,size(a2))
   RANK(11); call ecopy(a3,size(a3),a2,size(a2))
   RANK(12); call ecopy(a3,size(a3),a2,size(a2))
   RANK(13); call ecopy(a3,size(a3),a2,size(a2))
   RANK(14); call ecopy(a3,size(a3),a2,size(a2))
   RANK(15); call ecopy(a3,size(a3),a2,size(a2))
   END SELECT
end subroutine step2

subroutine ecopy(a1,n,a2,m)
integer,intent(in) :: n,m
real(kind=real64),intent(in) :: a1(n) ! dimensioned with n, there is no rank/shape check
real(kind=real64)            :: a2(m) ! dimensioned with m, there is no rank/shape check
integer :: ismall
   ismall=min(n,m)       ! should warn as well
   a2(:ismall)=a1(:ismall)
end subroutine ecopy

end subroutine elementcopy_real64

subroutine elementcopy_real128(a1,a2) ! using assumed rank
real(kind=real128),intent(in) :: a1(..)
real(kind=real128)            :: a2(..)
real(kind=real128)            :: one(1), two(1)
   SELECT RANK(a1)
   RANK(0)
      one=a1
      SELECT RANK(a2)
      RANK(0); call step2(one,1)
      RANK(1); call step2(one,1)
      RANK(2); call step2(one,1)
      RANK(3); call step2(one,1)
      RANK(4); call step2(one,1)
      RANK(5); call step2(one,1)
      RANK(6); call step2(one,1)
      RANK(7); call step2(one,1)
      RANK(8); call step2(one,1)
      RANK(9); call step2(one,1)
      RANK(10); call step2(one,1)
      RANK(11); call step2(one,1)
      RANK(12); call step2(one,1)
      RANK(13); call step2(one,1)
      RANK(14); call step2(one,1)
      RANK(15); call step2(one,1)
      END SELECT
   RANK(1)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(2)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(3)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   END SELECT
contains
subroutine step2(a3,isz)
integer :: isz
real(kind=real128),intent(in) :: a3(isz)
   SELECT RANK(a2)
   RANK(0); call ecopy(a3,1,two,1);a2=two(1)
   RANK(1); call ecopy(a3,size(a3),a2,size(a2))
   RANK(2); call ecopy(a3,size(a3),a2,size(a2))
   RANK(3); call ecopy(a3,size(a3),a2,size(a2))
   RANK(4); call ecopy(a3,size(a3),a2,size(a2))
   RANK(5); call ecopy(a3,size(a3),a2,size(a2))
   RANK(6); call ecopy(a3,size(a3),a2,size(a2))
   RANK(7); call ecopy(a3,size(a3),a2,size(a2))
   RANK(8); call ecopy(a3,size(a3),a2,size(a2))
   RANK(9); call ecopy(a3,size(a3),a2,size(a2))
   RANK(10); call ecopy(a3,size(a3),a2,size(a2))
   RANK(11); call ecopy(a3,size(a3),a2,size(a2))
   RANK(12); call ecopy(a3,size(a3),a2,size(a2))
   RANK(13); call ecopy(a3,size(a3),a2,size(a2))
   RANK(14); call ecopy(a3,size(a3),a2,size(a2))
   RANK(15); call ecopy(a3,size(a3),a2,size(a2))
   END SELECT
end subroutine step2

subroutine ecopy(a1,n,a2,m)
integer,intent(in) :: n,m
real(kind=real128),intent(in) :: a1(n) ! dimensioned with n, there is no rank/shape check
real(kind=real128)            :: a2(m) ! dimensioned with m, there is no rank/shape check
integer :: ismall
   ismall=min(n,m)       ! should warn as well
   a2(:ismall)=a1(:ismall)
end subroutine ecopy

end subroutine elementcopy_real128

subroutine elementcopy_int8(a1,a2) ! using assumed rank
integer(kind=int8),intent(in) :: a1(..)
integer(kind=int8)            :: a2(..)
integer(kind=int8)            :: one(1), two(1)
   SELECT RANK(a1)
   RANK(0)
      one=a1
      SELECT RANK(a2)
      RANK(0); call step2(one,1)
      RANK(1); call step2(one,1)
      RANK(2); call step2(one,1)
      RANK(3); call step2(one,1)
      RANK(4); call step2(one,1)
      RANK(5); call step2(one,1)
      RANK(6); call step2(one,1)
      RANK(7); call step2(one,1)
      RANK(8); call step2(one,1)
      RANK(9); call step2(one,1)
      RANK(10); call step2(one,1)
      RANK(11); call step2(one,1)
      RANK(12); call step2(one,1)
      RANK(13); call step2(one,1)
      RANK(14); call step2(one,1)
      RANK(15); call step2(one,1)
      END SELECT
   RANK(1)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(2)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(3)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   END SELECT
contains
subroutine step2(a3,isz)
integer :: isz
integer(kind=int8),intent(in) :: a3(isz)
   SELECT RANK(a2)
   RANK(0); call ecopy(a3,1,two,1);a2=two(1)
   RANK(1); call ecopy(a3,size(a3),a2,size(a2))
   RANK(2); call ecopy(a3,size(a3),a2,size(a2))
   RANK(3); call ecopy(a3,size(a3),a2,size(a2))
   RANK(4); call ecopy(a3,size(a3),a2,size(a2))
   RANK(5); call ecopy(a3,size(a3),a2,size(a2))
   RANK(6); call ecopy(a3,size(a3),a2,size(a2))
   RANK(7); call ecopy(a3,size(a3),a2,size(a2))
   RANK(8); call ecopy(a3,size(a3),a2,size(a2))
   RANK(9); call ecopy(a3,size(a3),a2,size(a2))
   RANK(10); call ecopy(a3,size(a3),a2,size(a2))
   RANK(11); call ecopy(a3,size(a3),a2,size(a2))
   RANK(12); call ecopy(a3,size(a3),a2,size(a2))
   RANK(13); call ecopy(a3,size(a3),a2,size(a2))
   RANK(14); call ecopy(a3,size(a3),a2,size(a2))
   RANK(15); call ecopy(a3,size(a3),a2,size(a2))
   END SELECT
end subroutine step2

subroutine ecopy(a1,n,a2,m)
integer,intent(in) :: n,m
integer(kind=int8),intent(in) :: a1(n) ! dimensioned with n, there is no rank/shape check
integer(kind=int8)            :: a2(m) ! dimensioned with m, there is no rank/shape check
integer :: ismall
   ismall=min(n,m)       ! should warn as well
   a2(:ismall)=a1(:ismall)
end subroutine ecopy

end subroutine elementcopy_int8

subroutine elementcopy_int16(a1,a2) ! using assumed rank
integer(kind=int16),intent(in) :: a1(..)
integer(kind=int16)            :: a2(..)
integer(kind=int16)            :: one(1), two(1)
   SELECT RANK(a1)
   RANK(0)
      one=a1
      SELECT RANK(a2)
      RANK(0); call step2(one,1)
      RANK(1); call step2(one,1)
      RANK(2); call step2(one,1)
      RANK(3); call step2(one,1)
      RANK(4); call step2(one,1)
      RANK(5); call step2(one,1)
      RANK(6); call step2(one,1)
      RANK(7); call step2(one,1)
      RANK(8); call step2(one,1)
      RANK(9); call step2(one,1)
      RANK(10); call step2(one,1)
      RANK(11); call step2(one,1)
      RANK(12); call step2(one,1)
      RANK(13); call step2(one,1)
      RANK(14); call step2(one,1)
      RANK(15); call step2(one,1)
      END SELECT
   RANK(1)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(2)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(3)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   END SELECT
contains
subroutine step2(a3,isz)
integer :: isz
integer(kind=int16),intent(in) :: a3(isz)
   SELECT RANK(a2)
   RANK(0); call ecopy(a3,1,two,1);a2=two(1)
   RANK(1); call ecopy(a3,size(a3),a2,size(a2))
   RANK(2); call ecopy(a3,size(a3),a2,size(a2))
   RANK(3); call ecopy(a3,size(a3),a2,size(a2))
   RANK(4); call ecopy(a3,size(a3),a2,size(a2))
   RANK(5); call ecopy(a3,size(a3),a2,size(a2))
   RANK(6); call ecopy(a3,size(a3),a2,size(a2))
   RANK(7); call ecopy(a3,size(a3),a2,size(a2))
   RANK(8); call ecopy(a3,size(a3),a2,size(a2))
   RANK(9); call ecopy(a3,size(a3),a2,size(a2))
   RANK(10); call ecopy(a3,size(a3),a2,size(a2))
   RANK(11); call ecopy(a3,size(a3),a2,size(a2))
   RANK(12); call ecopy(a3,size(a3),a2,size(a2))
   RANK(13); call ecopy(a3,size(a3),a2,size(a2))
   RANK(14); call ecopy(a3,size(a3),a2,size(a2))
   RANK(15); call ecopy(a3,size(a3),a2,size(a2))
   END SELECT
end subroutine step2

subroutine ecopy(a1,n,a2,m)
integer,intent(in) :: n,m
integer(kind=int16),intent(in) :: a1(n) ! dimensioned with n, there is no rank/shape check
integer(kind=int16)            :: a2(m) ! dimensioned with m, there is no rank/shape check
integer :: ismall
   ismall=min(n,m)       ! should warn as well
   a2(:ismall)=a1(:ismall)
end subroutine ecopy

end subroutine elementcopy_int16

subroutine elementcopy_int32(a1,a2) ! using assumed rank
integer(kind=int32),intent(in) :: a1(..)
integer(kind=int32)            :: a2(..)
integer(kind=int32)            :: one(1), two(1)
   SELECT RANK(a1)
   RANK(0)
      one=a1
      SELECT RANK(a2)
      RANK(0); call step2(one,1)
      RANK(1); call step2(one,1)
      RANK(2); call step2(one,1)
      RANK(3); call step2(one,1)
      RANK(4); call step2(one,1)
      RANK(5); call step2(one,1)
      RANK(6); call step2(one,1)
      RANK(7); call step2(one,1)
      RANK(8); call step2(one,1)
      RANK(9); call step2(one,1)
      RANK(10); call step2(one,1)
      RANK(11); call step2(one,1)
      RANK(12); call step2(one,1)
      RANK(13); call step2(one,1)
      RANK(14); call step2(one,1)
      RANK(15); call step2(one,1)
      END SELECT
   RANK(1)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(2)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(3)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   END SELECT
contains
subroutine step2(a3,isz)
integer :: isz
integer(kind=int32),intent(in) :: a3(isz)
   SELECT RANK(a2)
   RANK(0); call ecopy(a3,1,two,1);a2=two(1)
   RANK(1); call ecopy(a3,size(a3),a2,size(a2))
   RANK(2); call ecopy(a3,size(a3),a2,size(a2))
   RANK(3); call ecopy(a3,size(a3),a2,size(a2))
   RANK(4); call ecopy(a3,size(a3),a2,size(a2))
   RANK(5); call ecopy(a3,size(a3),a2,size(a2))
   RANK(6); call ecopy(a3,size(a3),a2,size(a2))
   RANK(7); call ecopy(a3,size(a3),a2,size(a2))
   RANK(8); call ecopy(a3,size(a3),a2,size(a2))
   RANK(9); call ecopy(a3,size(a3),a2,size(a2))
   RANK(10); call ecopy(a3,size(a3),a2,size(a2))
   RANK(11); call ecopy(a3,size(a3),a2,size(a2))
   RANK(12); call ecopy(a3,size(a3),a2,size(a2))
   RANK(13); call ecopy(a3,size(a3),a2,size(a2))
   RANK(14); call ecopy(a3,size(a3),a2,size(a2))
   RANK(15); call ecopy(a3,size(a3),a2,size(a2))
   END SELECT
end subroutine step2

subroutine ecopy(a1,n,a2,m)
integer,intent(in) :: n,m
integer(kind=int32),intent(in) :: a1(n) ! dimensioned with n, there is no rank/shape check
integer(kind=int32)            :: a2(m) ! dimensioned with m, there is no rank/shape check
integer :: ismall
   ismall=min(n,m)       ! should warn as well
   a2(:ismall)=a1(:ismall)
end subroutine ecopy

end subroutine elementcopy_int32

subroutine elementcopy_int64(a1,a2) ! using assumed rank
integer(kind=int64),intent(in) :: a1(..)
integer(kind=int64)            :: a2(..)
integer(kind=int64)            :: one(1), two(1)
   SELECT RANK(a1)
   RANK(0)
      one=a1
      SELECT RANK(a2)
      RANK(0); call step2(one,1)
      RANK(1); call step2(one,1)
      RANK(2); call step2(one,1)
      RANK(3); call step2(one,1)
      RANK(4); call step2(one,1)
      RANK(5); call step2(one,1)
      RANK(6); call step2(one,1)
      RANK(7); call step2(one,1)
      RANK(8); call step2(one,1)
      RANK(9); call step2(one,1)
      RANK(10); call step2(one,1)
      RANK(11); call step2(one,1)
      RANK(12); call step2(one,1)
      RANK(13); call step2(one,1)
      RANK(14); call step2(one,1)
      RANK(15); call step2(one,1)
      END SELECT
   RANK(1)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(2)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   RANK(3)
      SELECT RANK(a2)
      RANK(0); call step2(a1,size(a1))
      RANK(1); call step2(a1,size(a1))
      RANK(2); call step2(a1,size(a1))
      RANK(3); call step2(a1,size(a1))
      RANK(4); call step2(a1,size(a1))
      RANK(5); call step2(a1,size(a1))
      RANK(6); call step2(a1,size(a1))
      RANK(7); call step2(a1,size(a1))
      RANK(8); call step2(a1,size(a1))
      RANK(9); call step2(a1,size(a1))
      RANK(10); call step2(a1,size(a1))
      RANK(11); call step2(a1,size(a1))
      RANK(12); call step2(a1,size(a1))
      RANK(13); call step2(a1,size(a1))
      RANK(14); call step2(a1,size(a1))
      RANK(15); call step2(a1,size(a1))
      END SELECT
   END SELECT
contains
subroutine step2(a3,isz)
integer :: isz
integer(kind=int64),intent(in) :: a3(isz)
   SELECT RANK(a2)
   RANK(0); call ecopy(a3,1,two,1);a2=two(1)
   RANK(1); call ecopy(a3,size(a3),a2,size(a2))
   RANK(2); call ecopy(a3,size(a3),a2,size(a2))
   RANK(3); call ecopy(a3,size(a3),a2,size(a2))
   RANK(4); call ecopy(a3,size(a3),a2,size(a2))
   RANK(5); call ecopy(a3,size(a3),a2,size(a2))
   RANK(6); call ecopy(a3,size(a3),a2,size(a2))
   RANK(7); call ecopy(a3,size(a3),a2,size(a2))
   RANK(8); call ecopy(a3,size(a3),a2,size(a2))
   RANK(9); call ecopy(a3,size(a3),a2,size(a2))
   RANK(10); call ecopy(a3,size(a3),a2,size(a2))
   RANK(11); call ecopy(a3,size(a3),a2,size(a2))
   RANK(12); call ecopy(a3,size(a3),a2,size(a2))
   RANK(13); call ecopy(a3,size(a3),a2,size(a2))
   RANK(14); call ecopy(a3,size(a3),a2,size(a2))
   RANK(15); call ecopy(a3,size(a3),a2,size(a2))
   END SELECT
end subroutine step2

subroutine ecopy(a1,n,a2,m)
integer,intent(in) :: n,m
integer(kind=int64),intent(in) :: a1(n) ! dimensioned with n, there is no rank/shape check
integer(kind=int64)            :: a2(m) ! dimensioned with m, there is no rank/shape check
integer :: ismall
   ismall=min(n,m)       ! should warn as well
   a2(:ismall)=a1(:ismall)
end subroutine ecopy

end subroutine elementcopy_int64

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     linspace(3f) - [M_LA] return a vector of linearly spaced values
!!##SYNOPSIS
!!
!!    function linspace(x1,x2,n)
!!
!!     integer,intent(in)               :: n
!!     ${TYPE}(kind=${KIND}),intent(in) :: x1,x2
!!     ${TYPE}(kind=${KIND})            :: linspace
!!
!!    Where ${TYPE} may be real or integer and ${KIND} may be any
!!    supported kind for the corresponding type.
!!##USAGE
!!    Common usage:
!!
!!     y = linspace(x1,x2)
!!     y = linspace(x1,x2,n)
!!##DESCRIPTION
!!    linspace returns a vector of linearly spaced values from x1 to
!!    x2 inclusive. It gives direct control over the number of points
!!    and always includes the endpoints, the results being the same as
!!    [(x1+i*(x2-x1)/(n-1),i=0,n-1)] if n>1 and [x1,x2] if n<=1.
!!##OPTIONS
!!    X1,X2     X1 and X2 are the upper and lower bound of the values
!!              returned. The options can be of type REAL or INTEGER,
!!              but must be of the same type.
!!
!!    N         number of values to return
!!##RETURNS
!!    LINSPACE  The returned row vector starts with X1 and ends with X2,
!!              returning N evenly spaced values.
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_linspace
!!    use M_LA,  only : linspace
!!    implicit none
!!    character(len=*), parameter :: gen='(*(g0, 1x))'
!!       write( *, gen ) linspace(  0,      9,    10 )
!!       write( *, gen ) linspace( 10.0,   20.0,  11 )
!!       write( *, gen ) linspace( 11.1d0, 12.1d0, 5 )
!!       write( *, gen ) linspace( 11.1,   12.1,   5 )
!!    end program demo_linspace
!!   Results:
!!    0 1 2 3 4 5 6 7 8 9
!!    10.00 11.00 12.00 13.00 14.00 15.00 16.00 17.00 18.00 19.00 20.00
!!    11.1000000000 11.3500000000 11.6000000000 11.8500000000 12.100000000
!!    11.1000004 11.3500004 11.6000004 11.8500004 12.1000004
!!
!!   Results:
function linspace_real128(x1,x2,n)
integer,intent(in)               :: n
real(kind=real128),intent(in) :: x1,x2
real(kind=real128)            :: linspace_real128(n)
integer(kind=int64)              :: i
   if(n.le.1)then
      linspace_real128=[x1,x2]
   else
      linspace_real128=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_real128
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_real64(x1,x2,n)
integer,intent(in)               :: n
real(kind=real64),intent(in) :: x1,x2
real(kind=real64)            :: linspace_real64(n)
integer(kind=int64)              :: i
   if(n.le.1)then
      linspace_real64=[x1,x2]
   else
      linspace_real64=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_real64
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_real32(x1,x2,n)
integer,intent(in)               :: n
real(kind=real32),intent(in) :: x1,x2
real(kind=real32)            :: linspace_real32(n)
integer(kind=int64)              :: i
   if(n.le.1)then
      linspace_real32=[x1,x2]
   else
      linspace_real32=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_real32
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int64(x1,x2,n)
integer,intent(in)               :: n
integer(kind=int64),intent(in) :: x1,x2
integer(kind=int64)            :: linspace_int64(n)
integer(kind=int64)              :: i
   if(n.le.1)then
      linspace_int64=[x1,x2]
   else
      linspace_int64=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int64
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int32(x1,x2,n)
integer,intent(in)               :: n
integer(kind=int32),intent(in) :: x1,x2
integer(kind=int32)            :: linspace_int32(n)
integer(kind=int64)              :: i
   if(n.le.1)then
      linspace_int32=[x1,x2]
   else
      linspace_int32=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int32
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int16(x1,x2,n)
integer,intent(in)               :: n
integer(kind=int16),intent(in) :: x1,x2
integer(kind=int16)            :: linspace_int16(n)
integer(kind=int64)              :: i
   if(n.le.1)then
      linspace_int16=[x1,x2]
   else
      linspace_int16=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int16
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int8(x1,x2,n)
integer,intent(in)               :: n
integer(kind=int8),intent(in) :: x1,x2
integer(kind=int8)            :: linspace_int8(n)
integer(kind=int64)              :: i
   if(n.le.1)then
      linspace_int8=[x1,x2]
   else
      linspace_int8=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int8
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_inverse_hilbert(a,lda,n)

! ident_1="@(#) M_LA mat_inverse_hilbert(3fp) generate doubleprecision inverse hilbert matrix"
!
! References:
! Forsythe, G. E. and C. B. Moler. Computer Solution of Linear Algebraic Systems. Englewood Cliffs, NJ: Prentice-Hall, 1967.

integer,intent(in)          :: lda
integer,intent(in)          :: n
doubleprecision,intent(out) :: a(lda,n)

doubleprecision :: p
doubleprecision :: r
integer         :: i
integer         :: j
integer         :: ip1

   p = dble(n)

   do i = 1, n
      if (i.ne.1) p = (dble(n-i+1) * p * dble(n+i-1)) / dble(i-1)**2
      r = p * p
      a(i,i) = r / dble(2*i-1)
      if (i.eq.n) cycle
      ip1 = i + 1
      do j = ip1, n
         r = (-1) * (dble(n-j+1) * r * (n+j-1)) / dble(j-1)**2
         a(i,j) = r/ dble(i+j-1)
         a(j,i) = a(i,j)
      enddo
   enddo

end subroutine mat_inverse_hilbert

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   mat_magic(3f) - [M_LA] create an N x N magic square array, N>2
!!##SYNOPSIS
!!
!!    subroutine mat_magic(a,rows,n)
!!
!!       integer         :: rows
!!       integer         :: n
!!       doubleprecision :: a(rows,n)
!!
!!##DESCRIPTION
!!    This procedure returns the values to create a magic squares array,
!!    an n by n matrix in which each integer 1, 2, ..., n*n appears exactly
!!    once; and all columns, rows, and diagonals sum to the same number.
!!
!!##OPTIONS
!!    A             An array to fill with the magic square values. The
!!                  smallest dimension should be >= 3. Since a square is
!!                  required only the first N will be filled,
!!                  where n=min(rows,columns).
!!    ROWS          size of a row of A; must be >= N
!!    N             size of an edge of the magic square. A() must have at
!!                  least this many columns.
!!
!!##PEDIGREE
!!   Based on an algorithm for magic squares from
!!
!!     Mathematical Recreations and Essays, 12th ed.,
!!     by W. W. Rouse Ball and H. S. M. Coxeter
!!##EXAMPLES
!!
!!   Sample program
!!
!!    program demo_mat_magic
!!    use M_LA, only : mat_magic
!!    implicit none
!!    integer,parameter :: isize=10
!!    doubleprecision   :: arr(isize,isize)
!!    integer           :: i, j, k
!!       do k=1,isize
!!          write(*,'(*(g0,1x))')'K=',k
!!          call mat_magic(arr,size(arr,dim=1),k)
!!          do i=1,k
!!             write(*,'(i2,":",*(i5):)')i,&
!!              (nint(arr(i,j)),j=1,k),&
!!              nint(sum(arr(k,:k)))
!!          enddo
!!       enddo
!!    end program demo_mat_magic
!!
!!   Results:
!!
!!     K= 1
!!     1:    1    1
!!     K= 2
!!     1:    1    3    6
!!     2:    4    2    6
!!     K= 3
!!     1:    8    1    6   15
!!     2:    3    5    7   15
!!     3:    4    9    2   15
!!     K= 4
!!     1:   16    2    3   13   34
!!     2:    5   11   10    8   34
!!     3:    9    7    6   12   34
!!     4:    4   14   15    1   34
!!     K= 5
!!     1:   17   24    1    8   15   65
!!     2:   23    5    7   14   16   65
!!     3:    4    6   13   20   22   65
!!     4:   10   12   19   21    3   65
!!     5:   11   18   25    2    9   65
!!     K= 6
!!     1:   35    1    6   26   19   24  111
!!     2:    3   32    7   21   23   25  111
!!     3:   31    9    2   22   27   20  111
!!     4:    8   28   33   17   10   15  111
!!     5:   30    5   34   12   14   16  111
!!     6:    4   36   29   13   18   11  111
!!     K= 7
!!     1:   30   39   48    1   10   19   28  175
!!     2:   38   47    7    9   18   27   29  175
!!     3:   46    6    8   17   26   35   37  175
!!     4:    5   14   16   25   34   36   45  175
!!     5:   13   15   24   33   42   44    4  175
!!     6:   21   23   32   41   43    3   12  175
!!     7:   22   31   40   49    2   11   20  175
!!     K= 8
!!     1:   64    2    3   61   60    6    7   57  260
!!     2:    9   55   54   12   13   51   50   16  260
!!     3:   17   47   46   20   21   43   42   24  260
!!     4:   40   26   27   37   36   30   31   33  260
!!     5:   32   34   35   29   28   38   39   25  260
!!     6:   41   23   22   44   45   19   18   48  260
!!     7:   49   15   14   52   53   11   10   56  260
!!     8:    8   58   59    5    4   62   63    1  260
!!     K= 9
!!     1:   47   58   69   80    1   12   23   34   45  369
!!     2:   57   68   79    9   11   22   33   44   46  369
!!     3:   67   78    8   10   21   32   43   54   56  369
!!     4:   77    7   18   20   31   42   53   55   66  369
!!     5:    6   17   19   30   41   52   63   65   76  369
!!     6:   16   27   29   40   51   62   64   75    5  369
!!     7:   26   28   39   50   61   72   74    4   15  369
!!     8:   36   38   49   60   71   73    3   14   25  369
!!     9:   37   48   59   70   81    2   13   24   35  369
!!     K= 10
!!     1:   92   99    1    8   15   67   74   51   58   40  505
!!     2:   98   80    7   14   16   73   55   57   64   41  505
!!     3:    4   81   88   20   22   54   56   63   70   47  505
!!     4:   85   87   19   21    3   60   62   69   71   28  505
!!     5:   86   93   25    2    9   61   68   75   52   34  505
!!     6:   17   24   76   83   90   42   49   26   33   65  505
!!     7:   23    5   82   89   91   48   30   32   39   66  505
!!     8:   79    6   13   95   97   29   31   38   45   72  505
!!     9:   10   12   94   96   78   35   37   44   46   53  505
!!    10:   11   18  100   77   84   36   43   50   27   59  505
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_magic(a,rows,n)
!
! ident_2="@(#) M_LA mat_magic(3fp) Algorithms for magic squares"

integer,intent(in) :: rows
integer,intent(in) :: n
doubleprecision    :: a(rows,n)

doubleprecision    :: t
integer            :: i, j, k, m
integer            :: i1, j1, m1, m2
integer            :: im, jm, mm

   if (mod(n,4) .eq. 0)then
      !
      !     double even order
      !
      k = 1
      do i = 1, n
         do j = 1, n
            a(i,j) = k
            if (mod(i,4)/2 .eq. mod(j,4)/2) a(i,j) = n*n+1 - k
            k = k+1
         enddo
      enddo
      return
   endif
   if (mod(n,2) .eq. 0) m = n/2
   if (mod(n,2) .ne. 0) m = n
   !
   !     odd order or upper corner of even order
   !
   do j = 1,m
      do i = 1,m
         a(i,j) = 0
      enddo
   enddo
   i = 1
   j = (m+1)/2
   mm = m*m
   do k = 1, mm
      a(i,j) = k
      i1 = i-1
      j1 = j+1
      if(i1.lt.1) i1 = m
      if(j1.gt.m) j1 = 1
      if(int(a(i1,j1)).ne.0) then
         i1 = i+1
         j1 = j
      endif
      i = i1
      j = j1
   enddo
   if (mod(n,2) .ne. 0) return

   !
   !     rest of even order
   !
   t = dble(m*m)
   do i = 1, m
      do j = 1, m
         im = i+m
         jm = j+m
         a(i,jm) = a(i,j) + 2*t
         a(im,j) = a(i,j) + 3*t
         a(im,jm) = a(i,j) + t
      enddo
   enddo
   m1 = (m-1)/2
   if (m1.eq.0) return

   do j = 1, m1
      call mat_rswap(m,a(1,j),1,a(m+1,j),1)
   enddo
   m1 = (m+1)/2
   m2 = m1 + m
   call mat_rswap(1,a(m1,1),1,a(m2,1),1)
   call mat_rswap(1,a(m1,m1),1,a(m2,m1),1)
   m1 = n+1-(m-3)/2
   if(m1.gt.n) return

   do j = m1, n
      call mat_rswap(m,a(1,j),1,a(m+1,j),1)
   enddo
end subroutine mat_magic
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rref(ar,ai,lda,m,n,eps)
integer,intent(in) :: lda
doubleprecision    :: ar(lda,*)
doubleprecision    :: ai(lda,*)
integer            :: m
integer            :: n
doubleprecision    :: eps

doubleprecision    :: tol
doubleprecision    :: tr
doubleprecision    :: ti
integer            :: i, j, k, l

   tol = 0.0d0
   do j = 1, n
      tol = dmax1(tol,mat_wasum(m,ar(1,j),ai(1,j),1))
   enddo
   tol = eps*dble(2*max0(m,n))*tol
   k = 1
   l = 1
   INFINITE: do
      IF (K.GT.M .OR. L.GT.N) RETURN

      i = mat_iwamax(m-k+1,ar(k,l),ai(k,l),1) + k-1
      if (dabs(ar(i,l))+dabs(ai(i,l)) .le. tol)then
         call mat_wset(m-k+1,0.0d0,0.0d0,ar(k,l),ai(k,l),1)
         l = l+1
         cycle INFINITE
      endif

      call mat_wswap(n-l+1,ar(i,l),ai(i,l),lda,ar(k,l),ai(k,l),lda)
      call mat_wdiv(1.0d0,0.0d0,ar(k,l),ai(k,l),tr,ti)
      call mat_wscal(n-l+1,tr,ti,ar(k,l),ai(k,l),lda)
      ar(k,l) = 1.0d0
      ai(k,l) = 0.0d0
      do i = 1, m
         tr = -ar(i,l)
         ti = -ai(i,l)
         if (i .ne. k) call matX_waxpy(n-l+1,tr,ti,ar(k,l),ai(k,l),lda,ar(i,l),ai(i,l),lda)
      enddo
      K = K+1
      L = L+1
   enddo INFINITE
end subroutine mat_rref
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_pythag(a,b)
doubleprecision :: a
doubleprecision :: b

doubleprecision :: p
doubleprecision :: q
doubleprecision :: r
doubleprecision :: s
doubleprecision :: t

   p = dmax1(dabs(a),dabs(b))
   q = dmin1(dabs(a),dabs(b))

   if (q .ne. 0.0d0) then

      INFINITE : do
         r = (q/p)**2
         t = 4.0d0 + r
         if (t .eq. 4.0d0) exit INFINITE
         s = r/t
         p = p + 2.0d0*p*s
         q = q*s
      enddo INFINITE

   endif

   mat_pythag = p
end function mat_pythag
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotur(n,xr,xi,incx,yr,yi,incy)
integer,intent(in) :: n
doubleprecision    :: xr(*)
doubleprecision    :: xi(*)
integer            :: incx
doubleprecision    :: yr(*)
doubleprecision    :: yi(*)
integer            :: incy

doubleprecision    :: s
integer            :: ix
integer            :: iy
integer            :: i

   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = mat_flop(s + xr(ix)*yr(iy) - xi(ix)*yi(iy))
         ix = ix + incx
         iy = iy + incy
      enddo
   endif

   mat_wdotur = s

end function mat_wdotur
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wcopy(number_of_values,xr,xi,incx,yr,yi,incy)
integer,intent(in)          :: number_of_values
doubleprecision,intent(in)  :: xr(*)
doubleprecision,intent(in)  :: xi(*)
integer,intent(in)          :: incx
doubleprecision,intent(out) :: yr(*)
doubleprecision,intent(out) :: yi(*)
integer,intent(in)          :: incy

integer                     :: ix
integer                     :: iy
integer                     :: i
   if (number_of_values .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-number_of_values+1)*incx + 1
      if (incy.lt.0) iy = (-number_of_values+1)*incy + 1
      do i = 1, number_of_values
         yr(iy) = xr(ix)
         yi(iy) = xi(ix)
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
end subroutine mat_wcopy
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wset(n,xr,xi,yr,yi,incy)

! ident_18="@(#)M_LA::mat_set(3f):"

integer,intent(in)         :: n     ! number of Y values to set
doubleprecision,intent(in) :: xr    ! constant to assign Y real values to
doubleprecision,intent(in) :: xi    ! constant to assign Y imaginary values to
doubleprecision            :: yr(*) ! Y real component to set to XR
doubleprecision            :: yi(*) ! Y imaginary component to set to XI
integer                    :: incy  ! stride to take while setting output values

integer         :: iy
integer         :: i
   iy = 1
   if (n .le. 0 ) return
   do i = 1,n
      yr(iy) = xr
      yi(iy) = xi
      iy = iy + incy
   enddo
end subroutine mat_wset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wswap(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

doubleprecision :: t

integer         :: i
integer         :: ix
integer         :: iy

   if (n .le. 0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx + 1
   if (incy.lt.0) iy = (-n+1)*incy + 1
   do i = 1, n
      t = xr(ix)
      xr(ix) = yr(iy)
      yr(iy) = t
      t = xi(ix)
      xi(ix) = yi(iy)
      yi(iy) = t
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine mat_wswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wsqrt(xr,xi,yr,yi)

! ident_21="@(#)M_LA::mat_wsqrt(3fp): y = sqrt(x) with yr .ge. 0.0 and sign(yi) .eq. sign(xi)"

doubleprecision,intent(in)  :: xr
doubleprecision,intent(in)  :: xi
doubleprecision,intent(out) :: yr
doubleprecision,intent(out) :: yi
doubleprecision             :: s
doubleprecision             :: tr
doubleprecision             :: ti
!
   tr = xr
   ti = xi
   s = dsqrt(0.5d0*(mat_pythag(tr,ti) + dabs(tr)))
   if (tr .ge. 0.0d0) yr = mat_flop(s)
   if (ti .lt. 0.0d0) s = -s
   if (tr .le. 0.0d0) yi = mat_flop(s)
   if (tr .lt. 0.0d0) yr = mat_flop(0.5d0*(ti/yi))
   if (tr .gt. 0.0d0) yi = mat_flop(0.5d0*(ti/yr))
end subroutine mat_wsqrt
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rswap(n,x,incx,y,incy)
integer         :: n
doubleprecision :: x(*)
integer         :: incx
doubleprecision :: y(*)
integer         :: incy

doubleprecision :: t
integer         :: ix
integer         :: iy
integer         :: i

   if (n .le. 0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx+1
   if (incy.lt.0) iy = (-n+1)*incy+1
   do i = 1, n
      t = x(ix)
      x(ix) = y(iy)
      y(iy) = t
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine mat_rswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wrscal(n,s,xr,xi,incx)
integer         :: n
doubleprecision :: s
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx

integer         :: ix
integer         :: i
   if (n .le. 0) return
   ix = 1
   do i = 1, n
      xr(ix) = mat_flop(s*xr(ix))
      if (xi(ix) .ne. 0.0d0) xi(ix) = mat_flop(s*xi(ix))
      ix = ix + incx
   enddo
end subroutine mat_wrscal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wscal(n,sr,si,xr,xi,incx)
integer,intent(in)         :: n
doubleprecision,intent(in) :: sr
doubleprecision,intent(in) :: si
doubleprecision            :: xr(*)
doubleprecision            :: xi(*)
integer                    :: incx
integer                    :: ix
integer                    :: i
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         call mat_wmul(sr,si,xr(ix),xi(ix),xr(ix),xi(ix))
         ix = ix + incx
      enddo
   endif
end subroutine mat_wscal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wmul(ar,ai,br,bi,cr,ci)

! ident_25="@(#)M_LA::mat_wmul(3fp) c = a*b"

doubleprecision,intent(in)  :: ar
doubleprecision,intent(in)  :: ai
doubleprecision,intent(in)  :: br
doubleprecision,intent(in)  :: bi
doubleprecision,intent(out) :: cr
doubleprecision,intent(out) :: ci

doubleprecision :: t
   t = ar*bi + ai*br
   if (t .ne. 0.0d0) t = mat_flop(t)
   cr = mat_flop(ar*br - ai*bi)
   ci = t
end subroutine mat_wmul
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rrot(n,dx,incx,dy,incy,c,s)

! ident_27="@(#)M_LA::mat_rrot(3f): Applies a plane rotation."

integer         :: n
doubleprecision :: dx(*)
integer         :: incx
doubleprecision :: dy(*)
integer         :: incy
doubleprecision :: c
doubleprecision :: s

doubleprecision :: dtemp
integer         :: i
integer         :: ix
integer         :: iy
!
   if (n.gt.0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1,n
           dtemp = mat_flop(c*dx(ix) + s*dy(iy))
           dy(iy) = mat_flop(c*dy(iy) - s*dx(ix))
           dx(ix) = dtemp
           ix = ix + incx
           iy = iy + incy
      enddo
   endif
end subroutine mat_rrot
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rset(n,dx,dy,incy)

! ident_28="@(#)M_LA::mat_rset(3f): copies a scalar, dx, to a vector, dy."

integer         :: n
doubleprecision :: dx,dy(*)
integer         :: incy
integer         :: i
integer         :: iy

   if (n.gt.0) then
      iy = 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1,n
         dy(iy) = dx
         iy = iy + incy
      enddo
   endif
end subroutine mat_rset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rat(x,len,maxd,a,b,d)

! ident_39="@(#)M_LA::mat_rat(3fp): A/B = continued fraction approximation to X using  len  terms each less than MAXD"

integer         :: len,maxd
doubleprecision :: x,a,b,d(len)
doubleprecision :: s,t,z
integer         :: i
integer         :: ib
integer         :: k
   z = x
   k=0  ! preset to illegal value
   if(len.lt.1)then
      write(*,*)'*mat_rat* internal error -- len<1'
      return
   endif
   do i = 1, len
      k = i
      d(k) = mat_round(z)
      z = z - d(k)
      if (dabs(z)*dble(maxd) .le. 1.0d0) exit
      z = 1.0d0/z
   enddo
   t = d(k)
   s = 1.0d0
   if (k .ge. 2) then
      do ib = 2, k
         i = k+1-ib
         z = t
         t = d(i)*t + s
         s = z
      enddo
   endif
   if (s .lt. 0.0d0) t = -t
   if (s .lt. 0.0d0) s = -s

   a = t
   b = s
end subroutine mat_rat
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_urand(iy)
!>
!!##NAME
!!    mat_urand(3f) - [] uniform random number generator
!!    LICENSE(MIT)
!!
!!##SYNOPSIS
!!
!!     doubleprecision function mat_urand(iy)
!!
!!      integer,intent(inout) :: iy
!!
!!##DESCRIPTION
!!    mat_urand(3f) is a uniform random number generator based on theory and
!!    suggestions given in D.E. Knuth (1969), Vol 2. The integer IY should
!!    be initialized to an arbitrary integer prior to the first call to
!!    mat_urand(3f). The calling program should not alter the value of IY
!!    between subsequent calls to mat_urand(3f). Values of mat_urand(3f) will
!!    be returned in the interval (0,1).
!!
!!##OPTIONS
!!    IY seed for generating a sequence.
!!
!!##EXAMPLE
!!
integer              :: iy
integer,save         :: ia
integer,save         :: ic
integer,save         :: itwo=2
integer,save         :: m2=0
integer              :: m
integer,save         :: mic
doubleprecision      :: halfm
doubleprecision,save :: s
doubleprecision      :: datan
doubleprecision      :: dsqrt
!-----------------------------------------------------------------------
   if (m2 .eq. 0) then                                ! if first entry, compute machine integer word length
      m = 1
      INFINITE : do
         m2 = m
         m = itwo*m2
         if (m .le. m2) exit INFINITE
      enddo INFINITE
      halfm = m2
      ia = 8*int(halfm*datan(1.d0)/8.d0) + 5          ! compute multiplier and increment for linear congruential method
      ic = 2*int(halfm*(0.5d0-dsqrt(3.d0)/6.d0)) + 1
      mic = (m2 - ic) + m2
      s = 0.5d0/halfm                                 ! s is the scale factor for converting to floating point
   endif
   ! compute next random number
   iy = iy*ia

   if (iy .gt. mic) iy = (iy - m2) - m2     ! this statement is for computers which do not allow integer overflow on addition

   iy = iy + ic

   if (iy/2 .gt. m2) iy = (iy - m2) - m2    ! this statement is for computers where the word length for addition is greater than
                                            ! for multiplication
   if (iy .lt. 0) iy = (iy + m2) + m2       ! this statement is for computers where integer overflow affects the sign bit

   mat_urand = dble(iy)*s
end function mat_urand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wnrm2(n,xr,xi,incx)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
integer         :: ix
integer         :: i
   !     norm2(x)
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         s = mat_pythag(s,xr(ix))
         s = mat_pythag(s,xi(ix))
         ix = ix + incx
      enddo
   endif
   mat_wnrm2 = s
   end function mat_wnrm2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wasum(n,xr,xi,incx)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
integer         :: ix
integer         :: i

   !     norm1(x)
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         s = mat_flop(s + dabs(xr(ix)) + dabs(xi(ix)))
         ix = ix + incx
      enddo
   endif
   mat_wasum = s
end function mat_wasum
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotui(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy
doubleprecision :: s
integer         :: ix
integer         :: iy
integer         :: i
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = s + xr(ix)*yi(iy) + xi(ix)*yr(iy)
         if (s .ne. 0.0d0) s = mat_flop(s)
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
   mat_wdotui = s
end function mat_wdotui
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotcr(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

doubleprecision :: s
integer         :: ix
integer         :: iy
integer         :: i

   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = mat_flop(s + xr(ix)*yr(iy) + xi(ix)*yi(iy))
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
   mat_wdotcr = s
end function mat_wdotcr
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotci(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

integer         :: ix
integer         :: iy
integer         :: i
doubleprecision :: s

   s = 0.0d0

   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1

      do i = 1, n
         s = s + xr(ix)*yi(iy) - xi(ix)*yr(iy)
         if (s .ne. 0.0d0) s = mat_flop(s)
         ix = ix + incx
         iy = iy + incy
      enddo

   endif

   mat_wdotci = s
end function mat_wdotci
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
integer function mat_iwamax(n,xr,xi,incx)

! ident_41="@(#)M_LA::mat_iwamax(3fp):index of norminf(x)"

integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
doubleprecision :: p
integer         :: i, k
integer         :: ix

   k = 0
   if (n .gt. 0) then
      k = 1
      s = 0.0d0
      ix = 1
      do i = 1, n
         p = dabs(xr(ix)) + dabs(xi(ix))
         if (p .gt. s) k = i
         if (p .gt. s) s = p
         ix = ix + incx
      enddo
   endif
   mat_iwamax = k
end function mat_iwamax
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_flop(x)
!>
!!##NAME
!!    mat_flop(3fp) - [M_LA] count and possibly chop each floating point operation
!!    LICENSE(MIT)
!!
!!##SYNOPSIS
!!
!!
!!##DESCRIPTION
!!    Count and possibly chop each floating point operation.
!!
!!    this is a system-dependent function
!!##OPTIONS
!!
!!##NOTES
!!    FLP(1)  is flop counter
!!    FLP(2)  is number of places to be chopped
doubleprecision,intent(in) :: x
doubleprecision            :: mask(14),xx,mm
integer                    :: k
logical                    :: lx(2),lm(2)
equivalence (lx(1),xx),(lm(1),mm)
equivalence (mask(1),mas(1,1))
!>>>>>>>>>>>>>>>>>>
!*!GFORTRAN BUG in 8.3
!*!real,save                  :: mas(2,14)=reshape([ &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'fff0ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'ff00ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'f000ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000fff0',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000ff00',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000f000',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'fff0ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'ff00ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'f000ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000fff0',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000ff80',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0))],shape(mas))
integer :: i,j
logical,save :: setup=.false.
real,save                  :: mas(2,14)
character(len=8),save      :: setmas(2,14)=reshape([ &
   & 'ffffffff','fff0ffff', &
   & 'ffffffff','ff00ffff', &
   & 'ffffffff','f000ffff', &
   & 'ffffffff','0000ffff', &
   & 'ffffffff','0000fff0', &
   & 'ffffffff','0000ff00', &
   & 'ffffffff','0000f000', &
   & 'ffffffff','00000000', &
   & 'fff0ffff','00000000', &
   & 'ff00ffff','00000000', &
   & 'f000ffff','00000000', &
   & '0000ffff','00000000', &
   & '0000fff0','00000000', &
   & '0000ff80','00000000'],shape(mas))
   if(.not.setup)then
      do i=1,2
         do j=1,14
            read(setmas(i,j),'(z8)')mas(i,j)
         enddo
      enddo
      setup=.true.
   endif
!<<<<<<<<<<<<<<<<<<

   LA_FLOP_COUNTER(1) = LA_FLOP_COUNTER(1) + 1
   k = LA_FLOP_COUNTER(2)

   select case(k)
   case(:0)
      mat_flop = x
   case(1:15)
      mat_flop = 0.0d0
   case default
      xx = x
      mm = mask(k)
      lx(1) = lx(1) .and. lm(1)
      lx(2) = lx(2) .and. lm(2)
      mat_flop = xx
   end select

end function mat_flop
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_round(x)
doubleprecision           :: x,y,z,e
doubleprecision,parameter :: h=1.0d9
   z = dabs(x)
   y = z + 1.0d0
   if (y .ne. z)then
      y = 0.0d0
      e = h
      do
         if (e .ge. z) exit
         e = 2.0d0*e
      enddo
      do
         if (e .le. h) exit
         if (e .le. z) y = y + e
         if (e .le. z) z = z - e
         e = e/2.0d0
      enddo
      z = int(z + 0.5d0)
      y = y + z
      if (x .lt. 0.0d0) y = -y
      mat_round = y
   else
      mat_round = x
   endif
end function mat_round
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==mat_wpofa.f90 processed by SPAG 8.01RF 01:46 13 Dec 2024
SUBROUTINE mat_wpofa(Ar,Ai,Lda,N,Info)
IMPLICIT NONE
INTEGER :: Lda
DOUBLE PRECISION :: Ar(Lda,*)
DOUBLE PRECISION :: Ai(Lda,*)
INTEGER :: N
INTEGER :: Info

DOUBLE PRECISION :: s
DOUBLE PRECISION :: tr
DOUBLE PRECISION :: ti
INTEGER :: j
INTEGER :: jm1
INTEGER :: k

   DO j = 1 , N
      Info = j
      s = 0.0D0
      jm1 = j - 1
      IF ( jm1>=1 ) THEN
         DO k = 1 , jm1
            tr = Ar(k,j) - mat_wdotcr(k-1,Ar(1,k),Ai(1,k),1,Ar(1,j),Ai(1,j),1)
            ti = Ai(k,j) - mat_wdotci(k-1,Ar(1,k),Ai(1,k),1,Ar(1,j),Ai(1,j),1)
            CALL mat_wdiv(tr,ti,Ar(k,k),Ai(k,k),tr,ti)
            Ar(k,j) = tr
            Ai(k,j) = ti
            s = s + tr*tr + ti*ti
         ENDDO
      ENDIF
      s = Ar(j,j) - s
      IF ( s<=0.0D0 .OR. Ai(j,j)/=0.0D0 ) RETURN
      Ar(j,j) = dsqrt(s)
   ENDDO
   Info = 0
END SUBROUTINE mat_wpofa
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rrotg(da,db,c,s)

! ident_48="@(#)M_LA::mat_rrotg(3fp): construct Givens plane rotation."

doubleprecision :: da
doubleprecision :: db
doubleprecision :: c
doubleprecision :: s

doubleprecision :: rho
doubleprecision :: r
doubleprecision :: z

   rho = db
   if ( dabs(da) .gt. dabs(db) ) rho = da
   c = 1.0d0
   s = 0.0d0
   z = 1.0d0
   r = mat_flop(dsign(mat_pythag(da,db),rho))
   if (r .ne. 0.0d0) c = mat_flop(da/r)
   if (r .ne. 0.0d0) s = mat_flop(db/r)
   if ( dabs(da) .gt. dabs(db) ) z = s
   if (dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0)z = mat_flop(1.0d0/c)
   da = r
   db = z
end subroutine mat_rrotg
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wsign(xr,xi,yr,yi,zr,zi)

! ident_49="@(#)M_LA::mat_wsign(3fp): if y .ne. 0, z = x*y/abs(y)"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi
doubleprecision :: zr
doubleprecision :: zi
doubleprecision :: t
   t = mat_pythag(yr,yi)
   zr = xr
   zi = xi
   if (t .ne. 0.0d0) call mat_wmul(yr/t,yi/t,zr,zi,zr,zi)
end subroutine mat_wsign
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wdiv(ar,ai,br,bi,cr,ci)

! ident_17="@(#)M_LA::mat_wdiv(3fp): c = a/b"

doubleprecision :: ar
doubleprecision :: ai
doubleprecision :: br
doubleprecision :: bi
doubleprecision :: cr
doubleprecision :: ci

doubleprecision :: s
doubleprecision :: d
doubleprecision :: ars
doubleprecision :: ais
doubleprecision :: brs
doubleprecision :: bis

   s = dabs(br) + dabs(bi)
   if (s .eq. 0.0d0) then
      call la_err(27)
      return
   endif
   ars = ar/s
   ais = ai/s
   brs = br/s
   bis = bi/s
   d = brs**2 + bis**2
   cr = mat_flop((ars*brs + ais*bis)/d)
   ci = (ais*brs - ars*bis)/d
   if (ci .ne. 0.0d0) ci = mat_flop(ci)
end subroutine mat_wdiv
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wlog(in_real,in_imag,out_real,out_imag)

! ident_22="@(#)M_LA::mat_wlog(3fp): y = log(x)"

doubleprecision :: in_real, in_imag
doubleprecision :: out_real, out_imag
doubleprecision :: t
doubleprecision :: r
   r = mat_pythag(in_real,in_imag)

   if (r .eq. 0.0d0) then
      call la_err(32) !  Singularity of LOG or ATAN
   else
      t = datan2(in_imag,in_real)
      if (in_imag.eq.0.0d0 .and. in_real.lt.0.0d0) t = dabs(t)
      out_real = dlog(r)
      out_imag = t
   endif

end subroutine mat_wlog
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_watan(xr,xi,yr,yi)

! ident_47="@(#)M_LA::mat_watan(3fp): y = atan(x) = (i/2)*log((i+x)/(i-x))"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi
doubleprecision :: tr
doubleprecision :: ti

   if (xi .eq. 0.0d0) then
      yr = datan2(xr,1.0d0)
      yi = 0.0d0
   elseif (xr.ne.0.0d0 .or. dabs(xi).ne.1.0d0) then
      call mat_wdiv(xr,1.0d0+xi,-xr,1.0d0-xi,tr,ti)
      call mat_wlog(tr,ti,tr,ti)
      yr = -(ti/2.0d0)
      yi = tr/2.0d0
   else
      call la_err(32) ! Singularity of LOG or ATAN
   endif

end subroutine mat_watan
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine la_err(n)

! ident_3="@(#) M_matrix la_err(3fp) given error number write associated error message"

integer,intent(in)   :: n
character(len=255)   :: msg
   select case(n)
    case(27); msg='Division by zero is a NO-NO'
    case(32); msg='Singularity of LOG or ATAN'
    case default
       write(msg,'(a,i0)')'<ERROR>:*la_err* internal error: unknown error code=',n
   end select

   write(*,*)'<ERROR>:'//msg

end subroutine la_err
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_LA
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine matX_waxpy(N,SR,SI,xr,xi,INCX,yr,yi,INCY)
use M_LA
implicit none
integer,intent(in)         :: n
doubleprecision,intent(in) :: sr
doubleprecision,intent(in) :: si
doubleprecision,intent(in) :: xr(*)
doubleprecision,intent(in) :: xi(*)
integer,intent(in)         :: incx
integer,intent(in)         :: incy

doubleprecision            :: yr(*)
doubleprecision            :: yi(*)
integer                    :: ix, iy

integer                    :: i

   if (n .le. 0) return
   if (sr .eq. 0.0d0 .and. si .eq. 0.0d0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx + 1
   if (incy.lt.0) iy = (-n+1)*incy + 1
   do i = 1, n
      yr(iy) = mat_flop(yr(iy) + sr*xr(ix) - si*xi(ix))
      yi(iy) = yi(iy) + sr*xi(ix) + si*xr(ix)
      if (yi(iy) .ne. 0.0d0) yi(iy) = mat_flop(yi(iy))
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine matX_waxpy
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==ml_wgeco.f90 processed by SPAG 8.01RF 01:46 13 Dec 2024
SUBROUTINE ml_wgeco(Ar,Ai,Lda,N,Ipvt,Rcond,Zr,Zi)
   USE m_la
   IMPLICIT NONE
   INTEGER Lda , N , Ipvt(*)
   DOUBLE PRECISION Ar(Lda,*) , Ai(Lda,*) , Zr(*) , Zi(*)
   DOUBLE PRECISION Rcond
!
!     WGECO FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION
!     AND ESTIMATES THE CONDITION OF THE MATRIX.
!
!     IF  RCOND  IS NOT NEEDED, WGEFA IS SLIGHTLY FASTER.
!     TO SOLVE  A*X = B , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  INVERSE(A)*C , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  DETERMINANT(A) , FOLLOW WGECO BY WGEDI.
!     TO COMPUTE  INVERSE(A) , FOLLOW WGECO BY WGEDI.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        RCOND   DOUBLEPRECISION
!                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
!                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
!                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
!                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
!                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
!        1.0 + RCOND .EQ. 1.0
!                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
!                PRECISION. IN PARTICULAR,  RCOND  IS ZERO  IF
!                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
!                UNDERFLOWS.
!
!        Z       DOUBLE-COMPLEX(N)
!                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
!                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
!                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
!                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     LINPACK WGEFA
!     BLAS WAXPY,WDOTC,mat_wasum
!     FORTRAN DABS,DMAX1
!
!     INTERNAL VARIABLES
!
   DOUBLE PRECISION ekr , eki , tr , ti , wkr , wki , wkmr , wkmi
   DOUBLE PRECISION anorm , s , sm , ynorm
   INTEGER info , j , k , kb , kp1 , l
!
   DOUBLE PRECISION zdumr , zdumi
   DOUBLE PRECISION cabs1
   cabs1(zdumr,zdumi) = dabs(zdumr) + dabs(zdumi)
!
!     COMPUTE 1-NORM OF A
!
   anorm = 0.0D0
   DO j = 1 , N
      anorm = dmax1(anorm,mat_wasum(N,Ar(1,j),Ai(1,j),1))
   ENDDO
!
!     FACTOR
!
   CALL ml_wgefa(Ar,Ai,Lda,N,Ipvt,info)
!
!     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  CTRANS(A)*Y = E .
!     CTRANS(A)  IS THE CONJUGATE TRANSPOSE OF A .
!     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
!     GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(U)*W = E .
!     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
!
!     SOLVE CTRANS(U)*W = E
!
   ekr = 1.0D0
   eki = 0.0D0
   DO j = 1 , N
      Zr(j) = 0.0D0
      Zi(j) = 0.0D0
   ENDDO
   DO k = 1 , N
      CALL mat_wsign(ekr,eki,-Zr(k),-Zi(k),ekr,eki)
      IF ( cabs1(ekr-Zr(k),eki-Zi(k))>cabs1(Ar(k,k),Ai(k,k)) ) THEN
         s = cabs1(Ar(k,k),Ai(k,k))/cabs1(ekr-Zr(k),eki-Zi(k))
         CALL mat_wrscal(N,s,Zr,Zi,1)
         ekr = s*ekr
         eki = s*eki
      ENDIF
      wkr = ekr - Zr(k)
      wki = eki - Zi(k)
      wkmr = -ekr - Zr(k)
      wkmi = -eki - Zi(k)
      s = cabs1(wkr,wki)
      sm = cabs1(wkmr,wkmi)
      IF ( cabs1(Ar(k,k),Ai(k,k))==0.0D0 ) THEN
         wkr = 1.0D0
         wki = 0.0D0
         wkmr = 1.0D0
         wkmi = 0.0D0
      ELSE
         CALL mat_wdiv(wkr,wki,Ar(k,k),-Ai(k,k),wkr,wki)
         CALL mat_wdiv(wkmr,wkmi,Ar(k,k),-Ai(k,k),wkmr,wkmi)
      ENDIF
      kp1 = k + 1
      IF ( kp1<=N ) THEN
         DO j = kp1 , N
            CALL mat_wmul(wkmr,wkmi,Ar(k,j),-Ai(k,j),tr,ti)
            sm = mat_flop(sm+cabs1(Zr(j)+tr,Zi(j)+ti))
            CALL matx_waxpy(1,wkr,wki,[Ar(k,j)],[-Ai(k,j)],1,Zr(j),Zi(j),1)
            s = mat_flop(s+cabs1(Zr(j),Zi(j)))
         ENDDO
         IF ( s<sm ) THEN
            tr = wkmr - wkr
            ti = wkmi - wki
            wkr = wkmr
            wki = wkmi
            DO j = kp1 , N
               CALL matx_waxpy(1,tr,ti,[Ar(k,j)],[-Ai(k,j)],1,Zr(j),Zi(j),1)
            ENDDO
         ENDIF
      ENDIF
      Zr(k) = wkr
      Zi(k) = wki
   ENDDO
   s = 1.0D0/mat_wasum(N,Zr,Zi,1)
   CALL mat_wrscal(N,s,Zr,Zi,1)
!
!     SOLVE CTRANS(L)*Y = W
!
   DO kb = 1 , N
      k = N + 1 - kb
      IF ( k<N ) THEN
         Zr(k) = Zr(k) + mat_wdotcr(N-k,Ar(k+1,k),Ai(k+1,k),1,Zr(k+1),Zi(k+1),1)
         Zi(k) = Zi(k) + mat_wdotci(N-k,Ar(k+1,k),Ai(k+1,k),1,Zr(k+1),Zi(k+1),1)
      ENDIF
      IF ( cabs1(Zr(k),Zi(k))>1.0D0 ) THEN
         s = 1.0D0/cabs1(Zr(k),Zi(k))
         CALL mat_wrscal(N,s,Zr,Zi,1)
      ENDIF
      l = Ipvt(k)
      tr = Zr(l)
      ti = Zi(l)
      Zr(l) = Zr(k)
      Zi(l) = Zi(k)
      Zr(k) = tr
      Zi(k) = ti
   ENDDO
   s = 1.0D0/mat_wasum(N,Zr,Zi,1)
   CALL mat_wrscal(N,s,Zr,Zi,1)
!
   ynorm = 1.0D0
!
!     SOLVE L*V = Y
!
   DO k = 1 , N
      l = Ipvt(k)
      tr = Zr(l)
      ti = Zi(l)
      Zr(l) = Zr(k)
      Zi(l) = Zi(k)
      Zr(k) = tr
      Zi(k) = ti
      IF ( k<N ) CALL matx_waxpy(N-k,tr,ti,Ar(k+1,k),Ai(k+1,k),1,Zr(k+1),Zi(k+1),1)
      IF ( cabs1(Zr(k),Zi(k))<=1.0D0 ) CYCLE
      s = 1.0D0/cabs1(Zr(k),Zi(k))
      CALL mat_wrscal(N,s,Zr,Zi,1)
      ynorm = s*ynorm
   ENDDO
   s = 1.0D0/mat_wasum(N,Zr,Zi,1)
   CALL mat_wrscal(N,s,Zr,Zi,1)
   ynorm = s*ynorm
!
!     SOLVE  U*Z = V
!
   DO kb = 1 , N
      k = N + 1 - kb
      IF ( cabs1(Zr(k),Zi(k))>cabs1(Ar(k,k),Ai(k,k)) ) THEN
         s = cabs1(Ar(k,k),Ai(k,k))/cabs1(Zr(k),Zi(k))
         CALL mat_wrscal(N,s,Zr,Zi,1)
         ynorm = s*ynorm
      ENDIF
      IF ( cabs1(Ar(k,k),Ai(k,k))/=0.0D0 ) CALL mat_wdiv(Zr(k),Zi(k),Ar(k,k),Ai(k,k),Zr(k),Zi(k))
      IF ( cabs1(Ar(k,k),Ai(k,k))==0.0D0 ) THEN
         Zr(k) = 1.0D0
         Zi(k) = 0.0D0
      ENDIF
      tr = -Zr(k)
      ti = -Zi(k)
      CALL matx_waxpy(k-1,tr,ti,Ar(1,k),Ai(1,k),1,Zr(1),Zi(1),1)
   ENDDO
!     MAKE ZNORM = 1.0
   s = 1.0D0/mat_wasum(N,Zr,Zi,1)
   CALL mat_wrscal(N,s,Zr,Zi,1)
   ynorm = s*ynorm
!
   IF ( anorm/=0.0D0 ) Rcond = ynorm/anorm
   IF ( anorm==0.0D0 ) Rcond = 0.0D0
END SUBROUTINE ml_wgeco
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==ml_wgefa.f90 processed by SPAG 8.01RF 01:46 13 Dec 2024
SUBROUTINE ml_wgefa(Ar,Ai,Lda,N,Ipvt,Info)
   USE m_la
   IMPLICIT NONE
   INTEGER Lda , N , Ipvt(*) , Info
   DOUBLE PRECISION Ar(Lda,*) , Ai(Lda,*)
!
!     WGEFA FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION.
!
!     WGEFA IS USUALLY CALLED BY WGECO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
!     (TIME FOR WGECO) = (1 + 9/N)*(TIME FOR WGEFA) .
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 . THIS IS NOT AN ERROR
!  CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!  INDICATE THAT WGESL OR WGEDI WILL DIVIDE BY ZERO
!  IF CALLED. USE  RCOND  IN WGECO FOR A RELIABLE
!  INDICATION OF SINGULARITY.
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,mat_wscal,mat_iwamax
!     FORTRAN DABS
!
!     INTERNAL VARIABLES
!
   DOUBLE PRECISION tr , ti
   INTEGER j , k , kp1 , l , nm1
!
   DOUBLE PRECISION zdumr , zdumi
   DOUBLE PRECISION cabs1
   cabs1(zdumr,zdumi) = dabs(zdumr) + dabs(zdumi)
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!
   Info = 0
   nm1 = N - 1
   IF ( nm1>=1 ) THEN
      DO k = 1 , nm1
         kp1 = k + 1
!
!        FIND L = PIVOT INDEX
!
         l = mat_iwamax(N-k+1,Ar(k,k),Ai(k,k),1) + k - 1
         Ipvt(k) = l
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
         IF ( cabs1(Ar(l,k),Ai(l,k))==0.0D0 ) THEN
            Info = k
         ELSE
!
!           INTERCHANGE IF NECESSARY
!
            IF ( l/=k ) THEN
               tr = Ar(l,k)
               ti = Ai(l,k)
               Ar(l,k) = Ar(k,k)
               Ai(l,k) = Ai(k,k)
               Ar(k,k) = tr
               Ai(k,k) = ti
            ENDIF
!
!           COMPUTE MULTIPLIERS
!
            CALL mat_wdiv(-1.0D0,0.0D0,Ar(k,k),Ai(k,k),tr,ti)
            CALL mat_wscal(N-k,tr,ti,Ar(k+1,k),Ai(k+1,k),1)
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            DO j = kp1 , N
               tr = Ar(l,j)
               ti = Ai(l,j)
               IF ( l/=k ) THEN
                  Ar(l,j) = Ar(k,j)
                  Ai(l,j) = Ai(k,j)
                  Ar(k,j) = tr
                  Ai(k,j) = ti
               ENDIF
               CALL matx_waxpy(N-k,tr,ti,Ar(k+1,k),Ai(k+1,k),1,Ar(k+1,j),Ai(k+1,j),1)
            ENDDO
         ENDIF
      ENDDO
   ENDIF
   Ipvt(N) = N
   IF ( cabs1(Ar(N,N),Ai(N,N))==0.0D0 ) Info = N
END SUBROUTINE ml_wgefa
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==ml_wgesl.f90 processed by SPAG 8.01RF 01:46 13 Dec 2024
SUBROUTINE ml_wgesl(Ar,Ai,Lda,N,Ipvt,Br,Bi,Job)
   USE m_la
   IMPLICIT NONE
   INTEGER Lda , N , Ipvt(*) , Job
   DOUBLE PRECISION Ar(Lda,*) , Ai(Lda,*) , Br(*) , Bi(*)
!
!     WGESL SOLVES THE DOUBLE-COMPLEX SYSTEM
!     A * X = B  OR  CTRANS(A) * X = B
!     USING THE FACTORS COMPUTED BY WGECO OR WGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE OUTPUT FROM WGECO OR WGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM WGECO OR WGEFA.
!
!        B       DOUBLE-COMPLEX(N)
!                THE RIGHT HAND SIDE VECTOR.
!
!        JOB     INTEGER
!                = 0         TO SOLVE  A*X = B ,
!                = NONZERO   TO SOLVE  CTRANS(A)*X = B  WHERE
!         CTRANS(A)  IS THE CONJUGATE TRANSPOSE.
!
!     ON RETURN
!
!        B       THE SOLUTION VECTOR  X .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
!        ZERO ON THE DIAGONAL. TECHNICALLY THIS INDICATES SINGULARITY
!        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
!        SETTING OF LDA . IT WILL NOT OCCUR IF THE SUBROUTINES ARE
!        CALLED CORRECTLY AND IF WGECO HAS SET RCOND .GT. 0.0
!        OR WGEFA HAS SET INFO .EQ. 0 .
!
!     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
!     WITH  P  COLUMNS
!           CALL ML_WGECO(A,LDA,N,IPVT,RCOND,Z)
!           IF (RCOND IS TOO SMALL) GOTO ...
!           DO J = 1, P
!              CALL ML_WGESL(A,LDA,N,IPVT,C(1,J),0)
!           enddo
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WDOTC
!
!     INTERNAL VARIABLES
!
   DOUBLE PRECISION tr , ti
   INTEGER k , kb , l , nm1
!
   nm1 = N - 1
   IF ( Job/=0 ) THEN
!
!  JOB = NONZERO, SOLVE  CTRANS(A) * X = B
!  FIRST SOLVE  CTRANS(U)*Y = B
!
      DO k = 1 , N
         tr = Br(k) - mat_wdotcr(k-1,Ar(1,k),Ai(1,k),1,Br(1),Bi(1),1)
         ti = Bi(k) - mat_wdotci(k-1,Ar(1,k),Ai(1,k),1,Br(1),Bi(1),1)
         CALL mat_wdiv(tr,ti,Ar(k,k),-Ai(k,k),Br(k),Bi(k))
      ENDDO
!
!        NOW SOLVE CTRANS(L)*X = Y
!
      IF ( nm1>=1 ) THEN
         DO kb = 1 , nm1
            k = N - kb
            Br(k) = Br(k) + mat_wdotcr(N-k,Ar(k+1,k),Ai(k+1,k),1,Br(k+1),Bi(k+1),1)
            Bi(k) = Bi(k) + mat_wdotci(N-k,Ar(k+1,k),Ai(k+1,k),1,Br(k+1),Bi(k+1),1)
            l = Ipvt(k)
            IF ( l==k ) CYCLE
            tr = Br(l)
            ti = Bi(l)
            Br(l) = Br(k)
            Bi(l) = Bi(k)
            Br(k) = tr
            Bi(k) = ti
         ENDDO
      ENDIF
   ELSE
!
!        JOB = 0 , SOLVE  A * X = B
!        FIRST SOLVE  L*Y = B
!
      IF ( nm1>1 ) THEN
         DO k = 1 , nm1
            l = Ipvt(k)
            tr = Br(l)
            ti = Bi(l)
            IF ( l/=k ) THEN
               Br(l) = Br(k)
               Bi(l) = Bi(k)
               Br(k) = tr
               Bi(k) = ti
            ENDIF
            CALL matx_waxpy(N-k,tr,ti,Ar(k+1,k),Ai(k+1,k),1,Br(k+1),Bi(k+1),1)
         ENDDO
      ENDIF
!
!        NOW SOLVE  U*X = Y
!
      DO kb = 1 , N
         k = N + 1 - kb
         CALL mat_wdiv(Br(k),Bi(k),Ar(k,k),Ai(k,k),Br(k),Bi(k))
         tr = -Br(k)
         ti = -Bi(k)
         CALL matx_waxpy(k-1,tr,ti,Ar(1,k),Ai(1,k),1,Br(1),Bi(1),1)
      ENDDO
   ENDIF
END SUBROUTINE ml_wgesl
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    WGEDI(3f) - [M_LA] computes the determinant and inverse of a matrix
!!                using the factors computed by WGECO(3f) or WGEFA(3f).
!!##SYNOPSIS
!!
!!    SUBROUTINE ML_WGEDI(AR,AI,LDA,N,IPVT,DETR,DETI,WORKR,WORKI,JOB)
!!
!!      INTEGER(KIND=4) :: LDA
!!      REAL(KIND=8) :: AR(LDA,*)
!!      REAL(KIND=8) :: AI(LDA,*)
!!      INTEGER(KIND=4) :: N
!!      INTEGER(KIND=4) :: IPVT(*)
!!      REAL(KIND=8) :: DETR(2)
!!      REAL(KIND=8) :: DETI(2)
!!      REAL(KIND=8) :: WORKR(*)
!!      REAL(KIND=8) :: WORKI(*)
!!      INTEGER(KIND=4) :: JOB
!!
!!##DESCRIPTION
!!    WGEDI(3f) computes the determinant and inverse of a matrix
!!    using the factors computed by WGECO(3f) or WGEFA(3f).
!!
!!##ON ENTRY
!!
!!     A       Double-Complex(LDA, N)
!!             The output from WGECO or WGEFA.
!!
!!     LDA     Integer
!!             The leading dimension of the array A.
!!
!!     N       Integer
!!             The order of the matrix A.
!!
!!     IPVT    Integer(N)
!!             The pivot vector from WGECO(3f) or WGEFA(3f).
!!
!!     WORK    Double-Complex(N)
!!             Work vector. Contents destroyed.
!!
!!     JOB     Integer
!!
!!              = 11   Both determinant and inverse.
!!              = 01   Inverse only.
!!              = 10   Determinant only.
!!
!!##ON RETURN
!!
!!     A       Inverse of original matrix if requested.
!!             Otherwise unchanged.
!!
!!     DET     Double-complex(2)
!!             Determinant of original matrix if requested.
!!             Otherwise not referenced.
!!
!!              DETERMINANT = DET(1) * 10.0**DET(2)
!!              with 1.0 .le. CABS1(DET(1) .lt. 10.0
!!              or DET(1) .eq. 0.0 .
!!
!!##ERROR CONDITION
!!
!!    A division by zero will occur if the input factor contains a zero
!!    on the diagonal and the inverse is requested. It will not occur if
!!    the subroutines are called correctly and if WGECO(3f) has set RCOND
!!    .gt. 0.0 or WGEFA(3f) has set INFO .eq. 0 .
!!
!!      LINPACK. THIS VERSION DATED 07/01/79 .
!!      CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!!
!!##SUBROUTINES AND FUNCTIONS
!!
!!      BLAS WAXPY,mat_wscal,mat_wswap
!!      FORTRAN DABS,MOD
!*==ml_wgedi.f90 processed by SPAG 8.01RF 01:46 13 Dec 2024
SUBROUTINE ml_wgedi(Ar,Ai,Lda,N,Ipvt,Detr,Deti,Workr,Worki,Job)
   USE m_la
   IMPLICIT NONE
   INTEGER Lda , N , Ipvt(*) , Job
   DOUBLE PRECISION Ar(Lda,*) , Ai(Lda,*) , Detr(2) , Deti(2) , Workr(*) , Worki(*)
!     INTERNAL VARIABLES
!
   DOUBLE PRECISION tr , ti
   DOUBLE PRECISION ten
   INTEGER i , j , k , kb , kp1 , l , nm1
!
   DOUBLE PRECISION zdumr , zdumi
   DOUBLE PRECISION cabs1
   cabs1(zdumr,zdumi) = dabs(zdumr) + dabs(zdumi)
!
!     COMPUTE DETERMINANT
!
   IF ( Job/10/=0 ) THEN
      Detr(1) = 1.0D0
      Deti(1) = 0.0D0
      Detr(2) = 0.0D0
      Deti(2) = 0.0D0
      ten = 10.0D0
      SPAG_Loop_1_1: DO i = 1 , N
         IF ( Ipvt(i)/=i ) THEN
            Detr(1) = -Detr(1)
            Deti(1) = -Deti(1)
         ENDIF
         CALL mat_wmul(Ar(i,i),Ai(i,i),Detr(1),Deti(1),Detr(1),Deti(1))
!          ...EXIT
!       ...EXIT
         IF ( cabs1(Detr(1),Deti(1))==0.0D0 ) EXIT SPAG_Loop_1_1
         DO WHILE ( cabs1(Detr(1),Deti(1))<1.0D0 )
            Detr(1) = ten*Detr(1)
            Deti(1) = ten*Deti(1)
            Detr(2) = Detr(2) - 1.0D0
            Deti(2) = Deti(2) - 0.0D0
         ENDDO
         DO WHILE ( cabs1(Detr(1),Deti(1))>=ten )
            Detr(1) = Detr(1)/ten
            Deti(1) = Deti(1)/ten
            Detr(2) = Detr(2) + 1.0D0
            Deti(2) = Deti(2) + 0.0D0
         ENDDO
      ENDDO SPAG_Loop_1_1
   ENDIF
!
!     COMPUTE INVERSE(U)
!
   IF ( mod(Job,10)/=0 ) THEN
      DO k = 1 , N
         CALL mat_wdiv(1.0D0,0.0D0,Ar(k,k),Ai(k,k),Ar(k,k),Ai(k,k))
         tr = -Ar(k,k)
         ti = -Ai(k,k)
         CALL mat_wscal(k-1,tr,ti,Ar(1,k),Ai(1,k),1)
         kp1 = k + 1
         IF ( N<kp1 ) CYCLE
         DO j = kp1 , N
            tr = Ar(k,j)
            ti = Ai(k,j)
            Ar(k,j) = 0.0D0
            Ai(k,j) = 0.0D0
            CALL matx_waxpy(k,tr,ti,Ar(1,k),Ai(1,k),1,Ar(1,j),Ai(1,j),1)
         ENDDO
      ENDDO
!
!        FORM INVERSE(U)*INVERSE(L)
!
      nm1 = N - 1
      IF ( nm1>=1 ) THEN
         DO kb = 1 , nm1
            k = N - kb
            kp1 = k + 1
            DO i = kp1 , N
               Workr(i) = Ar(i,k)
               Worki(i) = Ai(i,k)
               Ar(i,k) = 0.0D0
               Ai(i,k) = 0.0D0
            ENDDO
            DO j = kp1 , N
               tr = Workr(j)
               ti = Worki(j)
               CALL matx_waxpy(N,tr,ti,Ar(1,j),Ai(1,j),1,Ar(1,k),Ai(1,k),1)
            ENDDO
            l = Ipvt(k)
            IF ( l/=k ) CALL mat_wswap(N,Ar(1,k),Ai(1,k),1,Ar(1,l),Ai(1,l),1)
         ENDDO
      ENDIF
   ENDIF
END SUBROUTINE ml_wgedi
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==ml_htridi.f90 processed by SPAG 8.01RF 01:46 13 Dec 2024
SUBROUTINE ml_htridi(Nm,N,Ar,Ai,D,E,E2,Tau)
   USE m_la
   IMPLICIT NONE
!
   INTEGER i , j , k , l , N , ii , Nm , jp1
   DOUBLE PRECISION Ar(Nm,N) , Ai(Nm,N) , D(N) , E(N) , E2(N) , Tau(2,N)
   DOUBLE PRECISION f , g , h , fi , gi , hh , si , scale
   INTEGER :: spag_nextblock_1
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRED1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE REDUCES A COMPLEX HERMITIAN MATRIX
!     TO A REAL SYMMETRIC TRIDIAGONAL MATRIX USING
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX HERMITIAN INPUT MATRIX.
!          ONLY THE LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION IN THEIR FULL LOWER
!          TRIANGLES. THEIR STRICT UPPER TRIANGLES AND THE
!          DIAGONAL OF AR ARE UNALTERED.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE THE TRIDIAGONAL MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
!          MATRIX IN ITS LAST N-1 POSITIONS. E(1) IS SET TO ZERO.
!
!        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
!          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
   Tau(1,N) = 1.0D0
   Tau(2,N) = 0.0D0
!
   DO i = 1 , N
      D(i) = Ar(i,i)
   ENDDO
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
   DO ii = 1 , N
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            i = N + 1 - ii
            l = i - 1
            h = 0.0D0
            scale = 0.0D0
            IF ( l>=1 ) THEN
!     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
               DO k = 1 , l
                  scale = mat_flop(scale+dabs(Ar(i,k))+dabs(Ai(i,k)))
               ENDDO
!
               IF ( scale/=0.0D0 ) THEN
!
                  DO k = 1 , l
                     Ar(i,k) = mat_flop(Ar(i,k)/scale)
                     Ai(i,k) = mat_flop(Ai(i,k)/scale)
                     h = mat_flop(h+Ar(i,k)*Ar(i,k)+Ai(i,k)*Ai(i,k))
                  ENDDO
!
                  E2(i) = mat_flop(scale*scale*h)
                  g = mat_flop(dsqrt(h))
                  E(i) = mat_flop(scale*g)
                  f = mat_pythag(Ar(i,l),Ai(i,l))
!     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T ..........
                  IF ( f==0.0D0 ) THEN
                     Tau(1,l) = -Tau(1,i)
                     si = Tau(2,i)
                     Ar(i,l) = g
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     Tau(1,l) = mat_flop((Ai(i,l)*Tau(2,i)-Ar(i,l)*Tau(1,i))/f)
                     si = mat_flop((Ar(i,l)*Tau(2,i)+Ai(i,l)*Tau(1,i))/f)
                     h = mat_flop(h+f*g)
                     g = mat_flop(1.0D0+g/f)
                     Ar(i,l) = mat_flop(g*Ar(i,l))
                     Ai(i,l) = mat_flop(g*Ai(i,l))
                     IF ( l/=1 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ELSE
                  Tau(1,l) = 1.0D0
                  Tau(2,l) = 0.0D0
               ENDIF
            ENDIF
            E(i) = 0.0D0
            E2(i) = 0.0D0
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         CASE (2)
            f = 0.0D0
!
            DO j = 1 , l
               g = 0.0D0
               gi = 0.0D0
!     .......... FORM ELEMENT OF A*U ..........
               DO k = 1 , j
                  g = mat_flop(g+Ar(j,k)*Ar(i,k)+Ai(j,k)*Ai(i,k))
                  gi = mat_flop(gi-Ar(j,k)*Ai(i,k)+Ai(j,k)*Ar(i,k))
               ENDDO
!
               jp1 = j + 1
               IF ( l>=jp1 ) THEN
!
                  DO k = jp1 , l
                     g = mat_flop(g+Ar(k,j)*Ar(i,k)-Ai(k,j)*Ai(i,k))
                     gi = mat_flop(gi-Ar(k,j)*Ai(i,k)-Ai(k,j)*Ar(i,k))
                  ENDDO
               ENDIF
!     .......... FORM ELEMENT OF P ..........
               E(j) = mat_flop(g/h)
               Tau(2,j) = mat_flop(gi/h)
               f = mat_flop(f+E(j)*Ar(i,j)-Tau(2,j)*Ai(i,j))
            ENDDO
!
            hh = mat_flop(f/(h+h))
!     .......... FORM REDUCED A ..........
            DO j = 1 , l
               f = Ar(i,j)
               g = mat_flop(E(j)-hh*f)
               E(j) = g
               fi = -Ai(i,j)
               gi = mat_flop(Tau(2,j)-hh*fi)
               Tau(2,j) = -gi
!
               DO k = 1 , j
                  Ar(j,k) = mat_flop(Ar(j,k)-f*E(k)-g*Ar(i,k)+fi*Tau(2,k)+gi*Ai(i,k))
                  Ai(j,k) = mat_flop(Ai(j,k)-f*Tau(2,k)-g*Ai(i,k)-fi*E(k)-gi*Ar(i,k))
               ENDDO
            ENDDO
            spag_nextblock_1 = 3
         CASE (3)
!
            DO k = 1 , l
               Ar(i,k) = mat_flop(scale*Ar(i,k))
               Ai(i,k) = mat_flop(scale*Ai(i,k))
            ENDDO
!
            Tau(2,l) = -si
            spag_nextblock_1 = 4
         CASE (4)
            hh = D(i)
            D(i) = Ar(i,i)
            Ar(i,i) = hh
            Ai(i,i) = mat_flop(scale*dsqrt(h))
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
!
END SUBROUTINE ml_htridi
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==ml_htribk.f90 processed by SPAG 8.01RF 01:46 13 Dec 2024
SUBROUTINE ml_htribk(Nm,N,Ar,Ai,Tau,M,Zr,Zi)
   USE m_la
   IMPLICIT NONE
!
   INTEGER i , j , k , l , M , N , Nm
   DOUBLE PRECISION Ar(Nm,N) , Ai(Nm,N) , Tau(2,N) , Zr(Nm,M) , Zi(Nm,M)
   DOUBLE PRECISION h , s , si
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRBAK1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX HERMITIAN
!     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
!     REAL SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  HTRIDI.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  HTRIDI  IN THEIR
!          FULL LOWER TRIANGLES EXCEPT FOR THE DIAGONAL OF AR.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED.
!
!        ZR CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED
!          IN ITS FIRST M COLUMNS.
!
!     ON OUTPUT.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS
!          IN THEIR FIRST M COLUMNS.
!
!     NOTE THAT THE LAST COMPONENT OF EACH RETURNED VECTOR
!     IS REAL AND THAT VECTOR EUCLIDEAN NORMS ARE PRESERVED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
   IF ( M/=0 ) THEN
!     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC
!                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN
!                TRIDIAGONAL MATRIX. ..........
      DO k = 1 , N
         DO j = 1 , M
            Zi(k,j) = mat_flop(-(Zr(k,j)*Tau(2,k)))
            Zr(k,j) = mat_flop(Zr(k,j)*Tau(1,k))
         ENDDO
      ENDDO
!
      IF ( N/=1 ) THEN
!     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES ..........
         SPAG_Loop_1_1: DO i = 2 , N
            l = i - 1
            h = Ai(i,i)
            IF ( h==0.0D0 ) EXIT SPAG_Loop_1_1
            DO j = 1 , M
               s = 0.0D0
               si = 0.0D0
               DO k = 1 , l
                  s = mat_flop(s+Ar(i,k)*Zr(k,j)-Ai(i,k)*Zi(k,j))
                  si = mat_flop(si+Ar(i,k)*Zi(k,j)+Ai(i,k)*Zr(k,j))
               ENDDO
!     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW ..........
               s = mat_flop((s/h)/h)
               si = mat_flop((si/h)/h)
               DO k = 1 , l
                  Zr(k,j) = mat_flop(Zr(k,j)-s*Ar(i,k)-si*Ai(i,k))
                  Zi(k,j) = mat_flop(Zi(k,j)-si*Ar(i,k)+s*Ai(i,k))
               ENDDO
            ENDDO
         ENDDO SPAG_Loop_1_1
      ENDIF
   ENDIF
!
END SUBROUTINE ml_htribk
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==ml_comqr3.f90 processed by SPAG 8.01RF 01:28 13 Dec 2024
subroutine ml_comqr3(nm,n,low,igh,ortr,orti,hr,hi,wr,wi,zr,zi,ierr,job)
use M_la
implicit none
!*****
!     MODIFICATION OF EISPACK COMQR2 TO ADD JOB PARAMETER
!     JOB = 0  OUTPUT H = SCHUR TRIANGULAR FORM, Z NOT USED
!         = 1  OUTPUT H = SCHUR FORM, Z = UNITARY SIMILARITY
!         = 2  SAME AS COMQR2
!         = 3  OUTPUT H = HESSENBERG FORM, Z = UNITARY SIMILARITY
!     ALSO ELIMINATE MACHEP
!     C. MOLER, 11/22/78 AND 09/14/80
!     OVERFLOW CONTROL IN EIGENVECTOR BACKSUBSTITUTION, 3/16/82
!*****
!
!
!     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE
!     ALGOL PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS
!     AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
!     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS
!     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE QR
!     METHOD. THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX
!     CAN ALSO BE FOUND IF  CORTH  HAS BEEN USED TO REDUCE
!     THIS GENERAL MATRIX TO HESSENBERG FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL. IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        ORTR AND ORTI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  CORTH, IF PERFORMED.
!          ONLY ELEMENTS LOW THROUGH IGH ARE USED. IF THE EIGENVECTORS
!          OF THE HESSENBERG MATRIX ARE DESIRED, SET ORTR(J) AND
!          ORTI(J) TO 0.0D0 FOR THESE ELEMENTS.
!
!        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.
!          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN FURTHER
!          INFORMATION ABOUT THE TRANSFORMATIONS WHICH WERE USED IN THE
!          REDUCTION BY  CORTH, IF PERFORMED. IF THE EIGENVECTORS OF
!          THE HESSENBERG MATRIX ARE DESIRED, THESE ELEMENTS MAY BE
!          ARBITRARY.
!
!     ON OUTPUT.
!
!        ORTR, ORTI, AND THE UPPER HESSENBERG PORTIONS OF HR AND HI
!          HAVE BEEN DESTROYED.
!
!        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVALUES. IF AN ERROR
!          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,...,N.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVECTORS. THE EIGENVECTORS
!          ARE UNNORMALIZED. IF AN ERROR EXIT IS MADE, NONE OF
!          THE EIGENVECTORS HAS BEEN FOUND.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER A TOTAL OF 30*N ITERATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
   integer i, j, k, l, m, n, en, ii, ll, nm, nn, igh, ip1, itn, its, low, lp1, enm1, iend, ierr
   double precision hr(nm,n), hi(nm,n), wr(n), wi(n), zr(nm,n), zi(nm,n), ortr(igh), orti(igh)
   double precision si, sr, ti, tr, xi, xr, yi, yr, zzi, zzr, norm
   integer :: job
   integer :: jj
   integer :: spag_nextblock_1
   spag_nextblock_1 = 1
   spag_dispatchloop_1: do
      select case (spag_nextblock_1)
       case (1)
         ierr = 0
         !*****
         if ( job/=0 ) then
            !*****
!     .......... INITIALIZE EIGENVECTOR MATRIX ..........
            do i = 1, n
               do j = 1, n
                  zr(i,j) = 0.0d0
                  zi(i,j) = 0.0d0
                  if ( i==j ) zr(i,j) = 1.0d0
               enddo
            enddo
!     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS
!                FROM THE INFORMATION LEFT BY CORTH ..........
            iend = igh - low - 1
            if ( iend<0 ) then
               spag_nextblock_1 = 2
               cycle spag_dispatchloop_1
            endif
            if ( iend/=0 ) then
!     .......... for i=igh-1 step -1 until low+1 do -- ..........
               do ii = 1, iend
                  i = igh - ii
                  if ( ortr(i)==0.0d0 .and. orti(i)==0.0d0 ) cycle
                  if ( hr(i,i-1)==0.0d0 .and. hi(i,i-1)==0.0d0 ) cycle
!     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH ..........
                  norm = mat_flop(hr(i,i-1)*ortr(i)+hi(i,i-1)*orti(i))
                  ip1 = i + 1

                  do k = ip1, igh
                     ortr(k) = hr(k,i-1)
                     orti(k) = hi(k,i-1)
                  enddo

                  do j = i, igh
                     sr = 0.0d0
                     si = 0.0d0

                     do k = i, igh
                        sr = mat_flop(sr+ortr(k)*zr(k,j)+orti(k)*zi(k,j))
                        si = mat_flop(si+ortr(k)*zi(k,j)-orti(k)*zr(k,j))
                     enddo

                     sr = mat_flop(sr/norm)
                     si = mat_flop(si/norm)

                     do k = i, igh
                        zr(k,j) = mat_flop(zr(k,j)+sr*ortr(k)-si*orti(k))
                        zi(k,j) = mat_flop(zi(k,j)+sr*orti(k)+si*ortr(k))
                     enddo

                  enddo

               enddo
               !*****
               if ( job==3 ) return
            endif
         endif
         !*****
!     .......... CREATE REAL SUBDIAGONAL ELEMENTS ..........
         l = low + 1

         do i = l, igh
            ll = min0(i+1,igh)
            if ( hi(i,i-1)==0.0d0 ) cycle
            norm = mat_pythag(hr(i,i-1),hi(i,i-1))
            yr = mat_flop(hr(i,i-1)/norm)
            yi = mat_flop(hi(i,i-1)/norm)
            hr(i,i-1) = norm
            hi(i,i-1) = 0.0d0

            do j = i, n
               si = mat_flop(yr*hi(i,j)-yi*hr(i,j))
               hr(i,j) = mat_flop(yr*hr(i,j)+yi*hi(i,j))
               hi(i,j) = si
            enddo

            do j = 1, ll
               si = mat_flop(yr*hi(j,i)+yi*hr(j,i))
               hr(j,i) = mat_flop(yr*hr(j,i)-yi*hi(j,i))
               hi(j,i) = si
            enddo
            !*****
            if ( job==0 ) cycle
            !*****
            do j = low, igh
               si = mat_flop(yr*zi(j,i)+yi*zr(j,i))
               zr(j,i) = mat_flop(yr*zr(j,i)-yi*zi(j,i))
               zi(j,i) = si
            enddo

         enddo
         spag_nextblock_1 = 2
       case (2)
!     .......... STORE ROOTS ISOLATED BY CBAL ..........
         do i = 1, n
            if ( i>=low .and. i<=igh ) cycle
            wr(i) = hr(i,i)
            wi(i) = hi(i,i)
         enddo

         en = igh
         tr = 0.0d0
         ti = 0.0d0
         itn = 30*n
         spag_nextblock_1 = 3
       case (3)
!     .......... SEARCH FOR NEXT EIGENVALUE ..........
         if ( en<low ) then
!     .......... ALL ROOTS FOUND. BACKSUBSTITUTE TO FIND
!                VECTORS OF UPPER TRIANGULAR FORM ..........
!
!*****  THE FOLLOWING SECTION CHANGED FOR OVERFLOW CONTROL
!       C. MOLER, 3/16/82
!
            if ( job==2 ) then

               norm = 0.0d0
               do i = 1, n
                  do j = i, n
                     tr = mat_flop(dabs(hr(i,j))) + mat_flop(dabs(hi(i,j)))
                     if ( tr>norm ) norm = tr
                  enddo
               enddo
               if ( n/=1 .and. norm/=0.0d0 ) then
!     .......... FOR EN=N STEP -1 UNTIL 2 DO -- ..........
                  do nn = 2, n
                     en = n + 2 - nn
                     xr = wr(en)
                     xi = wi(en)
                     hr(en,en) = 1.0d0
                     hi(en,en) = 0.0d0
                     enm1 = en - 1
!     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
                     do ii = 1, enm1
                        i = en - ii
                        zzr = 0.0d0
                        zzi = 0.0d0
                        ip1 = i + 1
                        do j = ip1, en
                           zzr = mat_flop(zzr+hr(i,j)*hr(j,en)-hi(i,j)*hi(j,en))
                           zzi = mat_flop(zzi+hr(i,j)*hi(j,en)+hi(i,j)*hr(j,en))
                        enddo
                        yr = mat_flop(xr-wr(i))
                        yi = mat_flop(xi-wi(i))
                        if ( yr==0.0d0 .and. yi==0.0d0 ) then
                           yr = norm
                           spag_loop_3_1: do
                              yr = mat_flop(yr/100.0d0)
                              yi = mat_flop(norm+yr)
                              if ( yi==norm ) then
                                 yi = 0.0d0
                                 exit spag_loop_3_1
                              endif
                           enddo spag_loop_3_1
                        endif
                        call mat_wdiv(zzr,zzi,yr,yi,hr(i,en),hi(i,en))
                        tr = mat_flop(dabs(hr(i,en))) + mat_flop(dabs(hi(i,en)))
                        if ( tr==0.0d0 ) cycle
                        if ( tr+1.0d0/tr>tr ) cycle
                        do j = i, en
                           hr(j,en) = mat_flop(hr(j,en)/tr)
                           hi(j,en) = mat_flop(hi(j,en)/tr)
                        enddo
                     enddo
                  enddo
!*****
!     .......... END BACKSUBSTITUTION ..........
                  enm1 = n - 1
!     .......... VECTORS OF ISOLATED ROOTS ..........
                  do i = 1, enm1
                     if ( i>=low .and. i<=igh ) cycle
                     ip1 = i + 1

                     do j = ip1, n
                        zr(i,j) = hr(i,j)
                        zi(i,j) = hi(i,j)
                     enddo

                  enddo
!     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
!                VECTORS OF ORIGINAL FULL MATRIX.
!                FOR J=N STEP -1 UNTIL LOW+1 DO -- ..........
                  do jj = low, enm1
                     j = n + low - jj
                     m = min0(j,igh)

                     do i = low, igh
                        zzr = 0.0d0
                        zzi = 0.0d0

                        do k = low, m
                           zzr = mat_flop(zzr+zr(i,k)*hr(k,j)-zi(i,k)*hi(k,j))
                           zzi = mat_flop(zzi+zr(i,k)*hi(k,j)+zi(i,k)*hr(k,j))
                        enddo

                        zr(i,j) = zzr
                        zi(i,j) = zzi
                     enddo
!
                  enddo
               endif
            endif
            return
         else
            its = 0
            enm1 = en - 1
         endif
         spag_nextblock_1 = 4
       case (4)
!     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
!                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
         spag_loop_1_2: do ll = low, en
            l = en + low - ll
            if ( l==low ) exit spag_loop_1_2
            !*****
            xr = mat_flop(dabs(hr(l-1,l-1))+dabs(hi(l-1,l-1))+dabs(hr(l,l))+dabs(hi(l,l)))
            yr = mat_flop(xr+dabs(hr(l,l-1)))
            if ( xr==yr ) exit spag_loop_1_2
            !*****
         enddo spag_loop_1_2
!     .......... FORM SHIFT ..........
         if ( l==en ) then
!     .......... A ROOT FOUND ..........
            hr(en,en) = mat_flop(hr(en,en)+tr)
            wr(en) = hr(en,en)
            hi(en,en) = mat_flop(hi(en,en)+ti)
            wi(en) = hi(en,en)
            en = enm1
            spag_nextblock_1 = 3
            cycle spag_dispatchloop_1
         elseif ( itn==0 ) then
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
            ierr = en
         else
            if ( its==10 .or. its==20 ) then
!     .......... FORM EXCEPTIONAL SHIFT ..........
               sr = mat_flop(dabs(hr(en,enm1))+dabs(hr(enm1,en-2)))
               si = 0.0d0
            else
               sr = hr(en,en)
               si = hi(en,en)
               xr = mat_flop(hr(enm1,en)*hr(en,enm1))
               xi = mat_flop(hi(enm1,en)*hr(en,enm1))
               if ( xr/=0.0d0 .or. xi/=0.0d0 ) then
                  yr = mat_flop((hr(enm1,enm1)-sr)/2.0d0)
                  yi = mat_flop((hi(enm1,enm1)-si)/2.0d0)
                  call mat_wsqrt(yr**2-yi**2+xr,2.0d0*yr*yi+xi,zzr,zzi)
                  if ( yr*zzr+yi*zzi<0.0d0 ) then
                     zzr = -zzr
                     zzi = -zzi
                  endif
                  call mat_wdiv(xr,xi,yr+zzr,yi+zzi,zzr,zzi)
                  sr = mat_flop(sr-zzr)
                  si = mat_flop(si-zzi)
               endif
            endif

            do i = low, en
               hr(i,i) = mat_flop(hr(i,i)-sr)
               hi(i,i) = mat_flop(hi(i,i)-si)
            enddo

            tr = mat_flop(tr+sr)
            ti = mat_flop(ti+si)
            its = its + 1
            itn = itn - 1
!     .......... REDUCE TO TRIANGLE (ROWS) ..........
            lp1 = l + 1

            do i = lp1, en
               sr = hr(i,i-1)
               hr(i,i-1) = 0.0d0
               norm = mat_flop(dabs(hr(i-1,i-1))+dabs(hi(i-1,i-1))+dabs(sr))
               norm = mat_flop(norm*dsqrt((hr(i-1,i-1)/norm)**2+(hi(i-1,i-1)/norm)**2+(sr/norm)**2))
               xr = mat_flop(hr(i-1,i-1)/norm)
               wr(i-1) = xr
               xi = mat_flop(hi(i-1,i-1)/norm)
               wi(i-1) = xi
               hr(i-1,i-1) = norm
               hi(i-1,i-1) = 0.0d0
               hi(i,i-1) = mat_flop(sr/norm)

               do j = i, n
                  yr = hr(i-1,j)
                  yi = hi(i-1,j)
                  zzr = hr(i,j)
                  zzi = hi(i,j)
                  hr(i-1,j) = mat_flop(xr*yr+xi*yi+hi(i,i-1)*zzr)
                  hi(i-1,j) = mat_flop(xr*yi-xi*yr+hi(i,i-1)*zzi)
                  hr(i,j) = mat_flop(xr*zzr-xi*zzi-hi(i,i-1)*yr)
                  hi(i,j) = mat_flop(xr*zzi+xi*zzr-hi(i,i-1)*yi)
               enddo

            enddo

            si = hi(en,en)
            if ( si/=0.0d0 ) then
               norm = mat_pythag(hr(en,en),si)
               sr = mat_flop(hr(en,en)/norm)
               si = mat_flop(si/norm)
               hr(en,en) = norm
               hi(en,en) = 0.0d0
               if ( en/=n ) then
                  ip1 = en + 1

                  do j = ip1, n
                     yr = hr(en,j)
                     yi = hi(en,j)
                     hr(en,j) = mat_flop(sr*yr+si*yi)
                     hi(en,j) = mat_flop(sr*yi-si*yr)
                  enddo
               endif
            endif
!     .......... INVERSE OPERATION (COLUMNS) ..........
            do j = lp1, en
               xr = wr(j-1)
               xi = wi(j-1)

               do i = 1, j
                  yr = hr(i,j-1)
                  yi = 0.0d0
                  zzr = hr(i,j)
                  zzi = hi(i,j)
                  if ( i/=j ) then
                     yi = hi(i,j-1)
                     hi(i,j-1) = mat_flop(xr*yi+xi*yr+hi(j,j-1)*zzi)
                  endif
                  hr(i,j-1) = mat_flop(xr*yr-xi*yi+hi(j,j-1)*zzr)
                  hr(i,j) = mat_flop(xr*zzr+xi*zzi-hi(j,j-1)*yr)
                  hi(i,j) = mat_flop(xr*zzi-xi*zzr-hi(j,j-1)*yi)
               enddo
!*****
               if ( job==0 ) cycle
!*****
               do i = low, igh
                  yr = zr(i,j-1)
                  yi = zi(i,j-1)
                  zzr = zr(i,j)
                  zzi = zi(i,j)
                  zr(i,j-1) = mat_flop(xr*yr-xi*yi+hi(j,j-1)*zzr)
                  zi(i,j-1) = mat_flop(xr*yi+xi*yr+hi(j,j-1)*zzi)
                  zr(i,j) = mat_flop(xr*zzr+xi*zzi-hi(j,j-1)*yr)
                  zi(i,j) = mat_flop(xr*zzi-xi*zzr-hi(j,j-1)*yi)
               enddo

            enddo

            if ( si/=0.0d0 ) then

               do i = 1, en
                  yr = hr(i,en)
                  yi = hi(i,en)
                  hr(i,en) = mat_flop(sr*yr-si*yi)
                  hi(i,en) = mat_flop(sr*yi+si*yr)
               enddo
!*****
               if ( job/=0 ) then
!*****
                  do i = low, igh
                     yr = zr(i,en)
                     yi = zi(i,en)
                     zr(i,en) = mat_flop(sr*yr-si*yi)
                     zi(i,en) = mat_flop(sr*yi+si*yr)

                  enddo
               endif
            endif
            spag_nextblock_1 = 4
            cycle spag_dispatchloop_1
         endif
         exit spag_dispatchloop_1
      end select
   enddo spag_dispatchloop_1
end subroutine ml_comqr3
!*==ml_corth.f90 processed by SPAG 8.01RF 01:28 13 Dec 2024
subroutine ml_corth(nm,n,low,igh,ar,ai,ortr,orti)
use M_la
implicit none
!
   integer i, j, m, n, ii, jj, la, mp, nm, igh, kp1, low
   double precision ar(nm,n), ai(nm,n), ortr(igh), orti(igh)
   double precision f, g, h, fi, fr, scale
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE ORTHES, NUM. MATH. 12, 349-368(1968)
!     BY MARTIN AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
!
!     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE
!     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
!     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL. IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX INPUT MATRIX.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE HESSENBERG MATRIX. INFORMATION
!          ABOUT THE UNITARY TRANSFORMATIONS USED IN THE REDUCTION
!          IS STORED IN THE REMAINING TRIANGLES UNDER THE
!          HESSENBERG MATRIX.
!
!        ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE
!          TRANSFORMATIONS. ONLY ELEMENTS LOW THROUGH IGH ARE USED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
   la = igh - 1
   kp1 = low + 1
   if ( la>=kp1 ) then
!
      do m = kp1, la
         h = 0.0d0
         ortr(m) = 0.0d0
         orti(m) = 0.0d0
         scale = 0.0d0
!     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
         do i = m, igh
            scale = mat_flop(scale+dabs(ar(i,m-1))+dabs(ai(i,m-1)))
         enddo
!
         if ( scale==0.0d0 ) cycle
         mp = m + igh
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
         do ii = m, igh
            i = mp - ii
            ortr(i) = mat_flop(ar(i,m-1)/scale)
            orti(i) = mat_flop(ai(i,m-1)/scale)
            h = mat_flop(h+ortr(i)*ortr(i)+orti(i)*orti(i))
         enddo
!
         g = mat_flop(dsqrt(h))
         f = mat_pythag(ortr(m),orti(m))
         if ( f==0.0d0 ) then
!
            ortr(m) = g
            ar(m,m-1) = scale
         else
            h = mat_flop(h+f*g)
            g = mat_flop(g/f)
            ortr(m) = mat_flop((1.0d0+g)*ortr(m))
            orti(m) = mat_flop((1.0d0+g)*orti(m))
         endif
!     .......... FORM (I-(U*UT)/H)*A ..........
         do j = m, n
            fr = 0.0d0
            fi = 0.0d0
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
            do ii = m, igh
               i = mp - ii
               fr = mat_flop(fr+ortr(i)*ar(i,j)+orti(i)*ai(i,j))
               fi = mat_flop(fi+ortr(i)*ai(i,j)-orti(i)*ar(i,j))
            enddo
!
            fr = mat_flop(fr/h)
            fi = mat_flop(fi/h)
!
            do i = m, igh
               ar(i,j) = mat_flop(ar(i,j)-fr*ortr(i)+fi*orti(i))
               ai(i,j) = mat_flop(ai(i,j)-fr*orti(i)-fi*ortr(i))
            enddo
!
         enddo
!     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) ..........
         do i = 1, igh
            fr = 0.0d0
            fi = 0.0d0
!     .......... FOR J=IGH STEP -1 UNTIL M DO -- ..........
            do jj = m, igh
               j = mp - jj
               fr = mat_flop(fr+ortr(j)*ar(i,j)-orti(j)*ai(i,j))
               fi = mat_flop(fi+ortr(j)*ai(i,j)+orti(j)*ar(i,j))
            enddo
!
            fr = mat_flop(fr/h)
            fi = mat_flop(fi/h)
!
            do j = m, igh
               ar(i,j) = mat_flop(ar(i,j)-fr*ortr(j)-fi*orti(j))
               ai(i,j) = mat_flop(ai(i,j)+fr*orti(j)-fi*ortr(j))
            enddo
!
         enddo
!
         ortr(m) = mat_flop(scale*ortr(m))
         orti(m) = mat_flop(scale*orti(m))
         ar(m,m-1) = mat_flop(-(g*ar(m,m-1)))
         ai(m,m-1) = mat_flop(-(g*ai(m,m-1)))
      enddo
   endif
!
end subroutine ml_corth
!*==ml_imtql2.f90 processed by SPAG 8.01RF 01:28 13 Dec 2024
subroutine ml_imtql2(nm,n,d,e,z,ierr,job)
   use M_la
   implicit none
   integer i, j, k, l, m, n, ii, nm, mml, ierr
   integer :: job
   double precision d(n), e(n), z(nm,n)
   double precision b, c, f, g, p, r, s
   integer :: spag_nextblock_1
   integer :: spag_nextblock_2
   spag_nextblock_1 = 1
   spag_dispatchloop_1: do
      select case (spag_nextblock_1)
       case (1)
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,
!     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
!     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.
!     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
!     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
!     FULL MATRIX TO TRIDIAGONAL FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
!          IN ITS LAST N-1 POSITIONS. E(1) IS ARBITRARY.
!
!        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
!          REDUCTION BY  TRED2, IF PERFORMED. IF THE EIGENVECTORS
!          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
!          THE IDENTITY MATRIX.
!
!      ON OUTPUT.
!
!        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER. IF AN
!          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
!          UNORDERED FOR INDICES 1,2,...,IERR-1.
!
!        E HAS BEEN DESTROYED.
!
!        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
!          TRIDIAGONAL (OR FULL) MATRIX. IF AN ERROR EXIT IS MADE,
!          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
!          EIGENVALUES.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER 30 ITERATIONS.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
!
!*****
!     MODIFIED BY C. MOLER TO ELIMINATE MACHEP 11/22/78
!     MODIFIED TO ADD JOB PARAMETER 08/27/79
!*****
         ierr = 0
         if ( n/=1 ) then
!
            do i = 2, n
               e(i-1) = e(i)
            enddo
!
            e(n) = 0.0d0
!
            do l = 1, n
               spag_nextblock_2 = 1
               spag_dispatchloop_2: do
                  select case (spag_nextblock_2)
                   case (1)
                     j = 0
                     spag_nextblock_2 = 2
                   case (2)
!     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
                     spag_loop_2_1: do m = l, n
                        if ( m==n ) exit spag_loop_2_1
!*****
                        p = mat_flop(dabs(d(m))+dabs(d(m+1)))
                        s = mat_flop(p+dabs(e(m)))
                        if ( p==s ) exit spag_loop_2_1
!*****
                     enddo spag_loop_2_1
!
                     p = d(l)
                     if ( m/=l ) then
                        if ( j==30 ) then
                           spag_nextblock_1 = 2
                           cycle spag_dispatchloop_1
                        endif
                        j = j + 1
!     .......... FORM SHIFT ..........
                        g = mat_flop((d(l+1)-p)/(2.0d0*e(l)))
                        r = mat_flop(dsqrt(g*g+1.0d0))
                        g = mat_flop(d(m)-p+e(l)/(g+dsign(r,g)))
                        s = 1.0d0
                        c = 1.0d0
                        p = 0.0d0
                        mml = m - l
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
                        do ii = 1, mml
                           i = m - ii
                           f = mat_flop(s*e(i))
                           b = mat_flop(c*e(i))
                           if ( dabs(f)<dabs(g) ) then
                              s = mat_flop(f/g)
                              r = mat_flop(dsqrt(s*s+1.0d0))
                              e(i+1) = mat_flop(g*r)
                              c = mat_flop(1.0d0/r)
                              s = mat_flop(s*c)
                           else
                              c = mat_flop(g/f)
                              r = mat_flop(dsqrt(c*c+1.0d0))
                              e(i+1) = mat_flop(f*r)
                              s = mat_flop(1.0d0/r)
                              c = mat_flop(c*s)
                           endif
                           g = mat_flop(d(i+1)-p)
                           r = mat_flop((d(i)-g)*s+2.0d0*c*b)
                           p = mat_flop(s*r)
                           d(i+1) = g + p
                           g = mat_flop(c*r-b)
                           if ( job/=0 ) then
!     .......... FORM VECTOR ..........
                              do k = 1, n
                                 f = z(k,i+1)
                                 z(k,i+1) = mat_flop(s*z(k,i)+c*f)
                                 z(k,i) = mat_flop(c*z(k,i)-s*f)
                              enddo
                           endif
!
                        enddo
!
                        d(l) = mat_flop(d(l)-p)
                        e(l) = g
                        e(m) = 0.0d0
                        spag_nextblock_2 = 2
                        cycle spag_dispatchloop_2
                     endif
                     exit spag_dispatchloop_2
                  end select
               enddo spag_dispatchloop_2
            enddo
!     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
            spag_loop_1_3: do ii = 2, n
               i = ii - 1
               k = i
               p = d(i)
!
               spag_loop_2_2: do j = ii, n
                  if ( d(j)>=p ) exit spag_loop_2_2
                  k = j
                  p = d(j)
               enddo spag_loop_2_2
!
               if ( k==i ) exit spag_loop_1_3
               d(k) = d(i)
               d(i) = p
!
               if ( job==0 ) cycle
               do j = 1, n
                  p = z(j,i)
                  z(j,i) = z(j,k)
                  z(j,k) = p
               enddo
!
            enddo spag_loop_1_3
         endif
         return
       case (2)
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
         ierr = l
         exit spag_dispatchloop_1
      end select
   enddo spag_dispatchloop_1
end subroutine ml_imtql2
!*==ml_wqrdc.f90 processed by SPAG 8.01RF 01:28 13 Dec 2024
subroutine ml_wqrdc(xr,xi,ldx,n,p,qrauxr,qrauxi,jpvt,workr,worki,job)
   use M_la
   implicit none
   integer ldx, n, p, job
   integer jpvt(*)
   double precision xr(ldx,*), xi(ldx,*), qrauxr(*), qrauxi(*), workr(*), worki(*)
!
!     WQRDC uses Householder transformations to compute the QR
!     factorization of an N by P matrix X. column pivoting
!     based on the 2-norms of the reduced columns may be
!     performed at the users option.
!
!     ON ENTRY
!
!        X       DOUBLE-COMPLEX(LDX,P), where LDX .ge. N.
!                X contains the matrix whose decomposition is to be
!                computed.
!
!        LDX     INTEGER.
!                LDX is the leading dimension of the array X.
!
!        N       INTEGER.
!                N is the number of rows of the matrix X.
!
!        P       INTEGER.
!                P is the number of columns of the matrix X.
!
!        JPVT    INTEGER(P).
!                JPVT contains integers that control the selection
!                of the pivot columns. The K-th column X(K) of X
!                is placed in one of three classes according to the
!                value of JPVT(K).
!
!                   If JPVT(K) .gt. 0, then X(K) is an initial
!                   column.
!
!                   If JPVT(K) .eq. 0, then X(K) is a free column.
!
!                   If JPVT(K) .lt. 0, then X(K) is a final column.
!
!                Before the decomposition is computed, initial columns
!                are moved to the beginning of the array X and final
!                columns to the end. Both initial and final columns
!                are frozen in place during the computation and only
!                free columns are moved. At the K-th stage of the
!                reduction, if X(K) is occupied by a free column
!                it is interchanged with the free column of largest
!                reduced norm. JPVT is not referenced if
!                JOB .eq. 0.
!
!        WORK    double-complex(P).
!                Work is a work array. work is not referenced if
!                JOB .eq. 0.
!
!        JOB     integer.
!                Job is an integer that initiates column pivoting.
!                If JOB .eq. 0, no pivoting is done.
!                If JOB .ne. 0, pivoting is done.
!
!     ON RETURN
!
!        X       X contains in its upper triangle the upper
!                triangular matrix R of the QR factorization.
!                below its diagonal X contains information from
!                which the unitary part of the decomposition
!                can be recovered. Note that if pivoting has
!                been requested, the decomposition is not that
!                of the original matrix X but that of X
!                with its columns permuted as described by JPVT.
!
!        QRAUX   DOUBLE-COMPLEX(P).
!                QRAUX contains further information required to recover
!                the unitary part of the decomposition.
!
!        JPVT    JPVT(K) contains the index of the column of the
!                original matrix that has been interchanged into
!                the K-th column, if pivoting was requested.
!
!     LINPACK. This version dated 07/03/79 .
!     G.W. Stewart, University of Maryland, Argonne National Lab.
!
!     WQRDC uses the following functions and subprograms.
!
!     BLAS matX_waxpy,mat_pythag,mat_wdotcr,mat_wdotci,mat_wscal
!     blas mat_wswap ,mat_wnrm2
!     FORTRAN DABS,DIMAG,DMAX1,MIN0
!
!     INTERNAL VARIABLES
!
   integer :: jj
   integer j, jp, l, lp1, lup, maxj, pl, pu
   double precision maxnrm, tt
   double precision nrmxlr, nrmxli, tr, ti
   logical negj, swapj
!
   double precision zdumr, zdumi
   double precision cabs1
   cabs1(zdumr,zdumi) = dabs(zdumr) + dabs(zdumi)
!
   pl = 1
   pu = 0
   if ( job/=0 ) then
!
!        PIVOTING HAS BEEN REQUESTED. REARRANGE THE COLUMNS
!        ACCORDING TO JPVT.
!
      do j = 1, p
         swapj = jpvt(j)>0
         negj = jpvt(j)<0
         jpvt(j) = j
         if ( negj ) jpvt(j) = -j
         if ( swapj ) then
            if ( j/=pl ) call mat_wswap(n,xr(1,pl),xi(1,pl),1,xr(1,j),xi(1,j),1)
            jpvt(j) = jpvt(pl)
            jpvt(pl) = j
            pl = pl + 1
         endif
      enddo
      pu = p
      do jj = 1, p
         j = p - jj + 1
         if ( jpvt(j)<0 ) then
            jpvt(j) = -jpvt(j)
            if ( j/=pu ) then
               call mat_wswap(n,xr(1,pu),xi(1,pu),1,xr(1,j),xi(1,j),1)
               jp = jpvt(pu)
               jpvt(pu) = jpvt(j)
               jpvt(j) = jp
            endif
            pu = pu - 1
         endif
      enddo
   endif
!
!     COMPUTE THE NORMS OF THE FREE COLUMNS.
!
   if ( pu>=pl ) then
      do j = pl, pu
         qrauxr(j) = mat_wnrm2(n,xr(1,j),xi(1,j),1)
         qrauxi(j) = 0.0d0
         workr(j) = qrauxr(j)
         worki(j) = qrauxi(j)
      enddo
   endif
!
!     PERFORM THE HOUSEHOLDER REDUCTION OF X.
!
   lup = min0(n,p)
   do l = 1, lup
      if ( l>=pl .and. l<pu ) then
!
!           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
!           INTO THE PIVOT POSITION.
!
         maxnrm = 0.0d0
         maxj = l
         do j = l, pu
            if ( qrauxr(j)<=maxnrm ) cycle
            maxnrm = qrauxr(j)
            maxj = j
         enddo
         if ( maxj/=l ) then
            call mat_wswap(n,xr(1,l),xi(1,l),1,xr(1,maxj),xi(1,maxj),1)
            qrauxr(maxj) = qrauxr(l)
            qrauxi(maxj) = qrauxi(l)
            workr(maxj) = workr(l)
            worki(maxj) = worki(l)
            jp = jpvt(maxj)
            jpvt(maxj) = jpvt(l)
            jpvt(l) = jp
         endif
      endif
      qrauxr(l) = 0.0d0
      qrauxi(l) = 0.0d0
      if ( l/=n ) then
!
!           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
!
         nrmxlr = mat_wnrm2(n-l+1,xr(l,l),xi(l,l),1)
         nrmxli = 0.0d0
         if ( cabs1(nrmxlr,nrmxli)/=0.0d0 ) then
            if ( cabs1(xr(l,l),xi(l,l))/=0.0d0 ) call mat_wsign(nrmxlr,nrmxli,xr(l,l),xi(l,l),nrmxlr,nrmxli)
            call mat_wdiv(1.0d0,0.0d0,nrmxlr,nrmxli,tr,ti)
            call mat_wscal(n-l+1,tr,ti,xr(l,l),xi(l,l),1)
            xr(l,l) = mat_flop(1.0d0+xr(l,l))
!
!             APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
!             UPDATING THE NORMS.
!
            lp1 = l + 1
            if ( p>=lp1 ) then
               do j = lp1, p
                  tr = -mat_wdotcr(n-l+1,xr(l,l),xi(l,l),1,xr(l,j),xi(l,j),1)
                  ti = -mat_wdotci(n-l+1,xr(l,l),xi(l,l),1,xr(l,j),xi(l,j),1)
                  call mat_wdiv(tr,ti,xr(l,l),xi(l,l),tr,ti)
                  call matx_waxpy(n-l+1,tr,ti,xr(l,l),xi(l,l),1,xr(l,j),xi(l,j),1)
                  if ( j>=pl .and. j<=pu ) then
                     if ( cabs1(qrauxr(j),qrauxi(j))/=0.0d0 ) then
                        tt = 1.0d0 - (mat_pythag(xr(l,j),xi(l,j))/qrauxr(j))**2
                        tt = dmax1(tt,0.0d0)
                        tr = mat_flop(tt)
                        tt = mat_flop(1.0d0+0.05d0*tt*(qrauxr(j)/workr(j))**2)
                        if ( tt==1.0d0 ) then
                           qrauxr(j) = mat_wnrm2(n-l,xr(l+1,j),xi(l+1,j),1)
                           qrauxi(j) = 0.0d0
                           workr(j) = qrauxr(j)
                           worki(j) = qrauxi(j)
                        else
                           qrauxr(j) = qrauxr(j)*dsqrt(tr)
                           qrauxi(j) = qrauxi(j)*dsqrt(tr)
                        endif
                     endif
                  endif
               enddo
            endif
!
!              SAVE THE TRANSFORMATION.
!
            qrauxr(l) = xr(l,l)
            qrauxi(l) = xi(l,l)
            xr(l,l) = -nrmxlr
            xi(l,l) = -nrmxli
         endif
      endif
   enddo
end subroutine ml_wqrdc
!*==ml_wqrsl.f90 processed by SPAG 8.01RF 01:28 13 Dec 2024
subroutine ml_wqrsl(xr,xi,ldx,n,k,qrauxr,qrauxi,yr,yi,qyr,qyi,qtyr,qtyi,br,bi,rsdr,rsdi,xbr,xbi,job,info)
use M_la
implicit none
integer ldx, n, k, job, info
double precision xr(ldx,*), xi(ldx,*), qrauxr(*), qrauxi(*), yr(*), yi(*), qyr(*), qyi(*), qtyr(*), qtyi(*), br(*),   &
& bi(*), rsdr(*), rsdi(*), xbr(*), xbi(*)
!
!     WQRSL APPLIES THE OUTPUT OF WQRDC TO COMPUTE COORDINATE
!     TRANSFORMATIONS, PROJECTIONS, AND LEAST SQUARES SOLUTIONS.
!     FOR K .LE. MIN(N,P), LET XK BE THE MATRIX
!
!            XK = (X(JPVT(1)),X(JPVT(2)), ... ,X(JPVT(K)))
!
!     FORMED FROM COLUMNS JPVT(1), ... ,JPVT(K) OF THE ORIGINAL
!     N X P MATRIX X THAT WAS INPUT TO WQRDC (IF NO PIVOTING WAS
!     DONE, XK CONSISTS OF THE FIRST K COLUMNS OF X IN THEIR
!     ORIGINAL ORDER). WQRDC PRODUCES A FACTORED UNITARY MATRIX Q
!     AND AN UPPER TRIANGULAR MATRIX R SUCH THAT
!
!              XK = Q * (R)
!    (0)
!
!     THIS INFORMATION IS CONTAINED IN CODED FORM IN THE ARRAYS
!     X AND QRAUX.
!
!     ON ENTRY
!
!        X      DOUBLE-COMPLEX(LDX,P).
!               X CONTAINS THE OUTPUT OF WQRDC.
!
!        LDX    INTEGER.
!               LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N      INTEGER.
!               N IS THE NUMBER OF ROWS OF THE MATRIX XK. IT MUST
!               HAVE THE SAME VALUE AS N IN WQRDC.
!
!        K      INTEGER.
!               K IS THE NUMBER OF COLUMNS OF THE MATRIX XK. K
!               MUST NNOT BE GREATER THAN MIN(N,P), WHERE P IS THE
!               SAME AS IN THE CALLING SEQUENCE TO WQRDC.
!
!        QRAUX  DOUBLE-COMPLEX(P).
!               QRAUX CONTAINS THE AUXILIARY OUTPUT FROM WQRDC.
!
!        Y      DOUBLE-COMPLEX(N)
!               Y CONTAINS AN N-VECTOR THAT IS TO BE MANIPULATED
!               BY WQRSL.
!
!        JOB    INTEGER.
!               JOB SPECIFIES WHAT IS TO BE COMPUTED. JOB HAS
!               THE DECIMAL EXPANSION ABCDE, WITH THE FOLLOWING
!               MEANING.
!
! IF A.NE.0, COMPUTE QY.
! IF B,C,D, OR E .NE. 0, COMPUTE QTY.
! IF C.NE.0, COMPUTE B.
! IF D.NE.0, COMPUTE RSD.
! IF E.NE.0, COMPUTE XB.
!
!               NOTE THAT A REQUEST TO COMPUTE B, RSD, OR XB
!               AUTOMATICALLY TRIGGERS THE COMPUTATION OF QTY, FOR
!               WHICH AN ARRAY MUST BE PROVIDED IN THE CALLING
!               SEQUENCE.
!
!     ON RETURN
!
!        QY     DOUBLE-COMPLEX(N).
!               QY CONTAINS Q*Y, IF ITS COMPUTATION HAS BEEN
!               REQUESTED.
!
!        QTY    DOUBLE-COMPLEX(N).
!               QTY CONTAINS CTRANS(Q)*Y, IF ITS COMPUTATION HAS
!               BEEN REQUESTED. HERE CTRANS(Q) IS THE CONJUGATE
!               TRANSPOSE OF THE MATRIX Q.
!
!        B      DOUBLE-COMPLEX(K)
!               B CONTAINS THE SOLUTION OF THE LEAST SQUARES PROBLEM
!
! MINIMIZE NORM2(Y - XK*B),
!
!               IF ITS COMPUTATION HAS BEEN REQUESTED. (NOTE THAT
!               IF PIVOTING WAS REQUESTED IN WQRDC, THE J-TH
!               COMPONENT OF B WILL BE ASSOCIATED WITH COLUMN JPVT(J)
!               OF THE ORIGINAL MATRIX X THAT WAS INPUT INTO WQRDC.)
!
!        RSD    DOUBLE-COMPLEX(N).
!               RSD CONTAINS THE LEAST SQUARES RESIDUAL Y - XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED. RSD IS
!               ALSO THE ORTHOGONAL PROJECTION OF Y ONTO THE
!               ORTHOGONAL COMPLEMENT OF THE COLUMN SPACE OF XK.
!
!        XB     DOUBLE-COMPLEX(N).
!               XB CONTAINS THE LEAST SQUARES APPROXIMATION XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED. XB IS ALSO
!               THE ORTHOGONAL PROJECTION OF Y ONTO THE COLUMN SPACE
!               OF X.
!
!        INFO   INTEGER.
!               INFO IS ZERO UNLESS THE COMPUTATION OF B HAS
!               BEEN REQUESTED AND R IS EXACTLY SINGULAR. IN
!               THIS CASE, INFO IS THE INDEX OF THE FIRST ZERO
!               DIAGONAL ELEMENT OF R AND B IS LEFT UNALTERED.
!
!     THE PARAMETERS QY, QTY, B, RSD, AND XB ARE NOT REFERENCED
!     IF THEIR COMPUTATION IS NOT REQUESTED AND IN THIS CASE
!     CAN BE REPLACED BY DUMMY VARIABLES IN THE CALLING PROGRAM.
!     TO SAVE STORAGE, THE USER MAY IN SOME CASES USE THE SAME
!     ARRAY FOR DIFFERENT PARAMETERS IN THE CALLING SEQUENCE. A
!     FREQUENTLY OCCURRING EXAMPLE IS WHEN ONE WISHES TO COMPUTE
!     ANY OF B, RSD, OR XB AND DOES NOT NEED Y OR QTY. IN THIS
!     CASE ONE MAY IDENTIFY Y, QTY, AND ONE OF B, RSD, OR XB, WHILE
!     PROVIDING SEPARATE ARRAYS FOR ANYTHING ELSE THAT IS TO BE
!     COMPUTED. THUS THE CALLING SEQUENCE
!
!          CALL ML_WQRSL(X,LDX,N,K,QRAUX,Y,DUM,Y,B,Y,DUM,110,INFO)
!
!     WILL RESULT IN THE COMPUTATION OF B AND RSD, WITH RSD
!     OVERWRITING Y. MORE GENERALLY, EACH ITEM IN THE FOLLOWING
!     LIST CONTAINS GROUPS OF PERMISSIBLE IDENTIFICATIONS FOR
!     A SINGLE CALLING SEQUENCE.
!
!          1. (Y,QTY,B) (RSD) (XB) (QY)
!
!          2. (Y,QTY,RSD) (B) (XB) (QY)
!
!          3. (Y,QTY,XB) (B) (RSD) (QY)
!
!          4. (Y,QY) (QTY,B) (RSD) (XB)
!
!          5. (Y,QY) (QTY,RSD) (B) (XB)
!
!          6. (Y,QY) (QTY,XB) (B) (RSD)
!
!     IN ANY GROUP THE VALUE RETURNED IN THE ARRAY ALLOCATED TO
!     THE GROUP CORRESPONDS TO THE LAST MEMBER OF THE GROUP.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     ML_WQRSL USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS matX_waxpy,mat_wcopy,mat_wdotcr,mat_wdotci
!     FORTRAN DABS,DIMAG,MIN0,MOD
!
!     INTERNAL VARIABLES
!
   integer i, j, jj, ju, kp1
   double precision tr, ti, tempr, tempi
   logical cb, cqy, cqty, cr, cxb
!
   double precision zdumr, zdumi
   double precision cabs1
   cabs1(zdumr,zdumi) = dabs(zdumr) + dabs(zdumi)
!
!     SET INFO FLAG.
!
   info = 0
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
   cqy = job/10000/=0
   cqty = mod(job,10000)/=0
   cb = mod(job,1000)/100/=0
   cr = mod(job,100)/10/=0
   cxb = mod(job,10)/=0
   ju = min0(k,n-1)
!
!     SPECIAL ACTION WHEN N=1.
!
   if ( ju/=0 ) then
!
!        SET UP TO COMPUTE QY OR QTY.
!
      if ( cqy ) call mat_wcopy(n,yr,yi,1,qyr,qyi,1)
      if ( cqty ) call mat_wcopy(n,yr,yi,1,qtyr,qtyi,1)
      if ( cqy ) then
!
!           COMPUTE QY.
!
         do jj = 1, ju
            j = ju - jj + 1
            if ( cabs1(qrauxr(j),qrauxi(j))==0.0d0 ) cycle
            tempr = xr(j,j)
            tempi = xi(j,j)
            xr(j,j) = qrauxr(j)
            xi(j,j) = qrauxi(j)
            tr = -mat_wdotcr(n-j+1,xr(j,j),xi(j,j),1,qyr(j),qyi(j),1)
            ti = -mat_wdotci(n-j+1,xr(j,j),xi(j,j),1,qyr(j),qyi(j),1)
            call mat_wdiv(tr,ti,xr(j,j),xi(j,j),tr,ti)
            call matx_waxpy(n-j+1,tr,ti,xr(j,j),xi(j,j),1,qyr(j),qyi(j),1)
            xr(j,j) = tempr
            xi(j,j) = tempi
         enddo
      endif
      if ( cqty ) then
!
!           COMPUTE CTRANS(Q)*Y.
!
         do j = 1, ju
            if ( cabs1(qrauxr(j),qrauxi(j))==0.0d0 ) cycle
            tempr = xr(j,j)
            tempi = xi(j,j)
            xr(j,j) = qrauxr(j)
            xi(j,j) = qrauxi(j)
            tr = -mat_wdotcr(n-j+1,xr(j,j),xi(j,j),1,qtyr(j),qtyi(j),1)
            ti = -mat_wdotci(n-j+1,xr(j,j),xi(j,j),1,qtyr(j),qtyi(j),1)
            call mat_wdiv(tr,ti,xr(j,j),xi(j,j),tr,ti)
            call matx_waxpy(n-j+1,tr,ti,xr(j,j),xi(j,j),1,qtyr(j),qtyi(j),1)
            xr(j,j) = tempr
            xi(j,j) = tempi
         enddo
      endif
!
!        SET UP TO COMPUTE B, RSD, OR XB.
!
      if ( cb ) call mat_wcopy(k,qtyr,qtyi,1,br,bi,1)
      kp1 = k + 1
      if ( cxb ) call mat_wcopy(k,qtyr,qtyi,1,xbr,xbi,1)
      if ( cr .and. k<n ) call mat_wcopy(n-k,qtyr(kp1),qtyi(kp1),1,rsdr(kp1),rsdi(kp1),1)
      if ( .not.(.not.cxb .or. kp1>n) ) then
         do i = kp1, n
            xbr(i) = 0.0d0
            xbi(i) = 0.0d0
         enddo
      endif
      if ( cr ) then
         do i = 1, k
            rsdr(i) = 0.0d0
            rsdi(i) = 0.0d0
         enddo
      endif
      if ( cb ) then
!
!           COMPUTE B.
!
         spag_loop_1_1: do jj = 1, k
            j = k - jj + 1
            if ( cabs1(xr(j,j),xi(j,j))/=0.0d0 ) then
               call mat_wdiv(br(j),bi(j),xr(j,j),xi(j,j),br(j),bi(j))
               if ( j/=1 ) then
                  tr = -br(j)
                  ti = -bi(j)
                  call matx_waxpy(j-1,tr,ti,xr(1,j),xi(1,j),1,br,bi,1)
               endif
            else
               info = j
!                 ......EXIT
!           ......EXIT
               exit spag_loop_1_1
            endif
         enddo spag_loop_1_1
      endif
      if ( .not.(.not.cr .and. .not.cxb) ) then
!
!           COMPUTE RSD OR XB AS REQUIRED.
!
         do jj = 1, ju
            j = ju - jj + 1
            if ( cabs1(qrauxr(j),qrauxi(j))==0.0d0 ) cycle
            tempr = xr(j,j)
            tempi = xi(j,j)
            xr(j,j) = qrauxr(j)
            xi(j,j) = qrauxi(j)
            if ( cr ) then
               tr = -mat_wdotcr(n-j+1,xr(j,j),xi(j,j),1,rsdr(j),rsdi(j),1)
               ti = -mat_wdotci(n-j+1,xr(j,j),xi(j,j),1,rsdr(j),rsdi(j),1)
               call mat_wdiv(tr,ti,xr(j,j),xi(j,j),tr,ti)
               call matx_waxpy(n-j+1,tr,ti,xr(j,j),xi(j,j),1,rsdr(j),rsdi(j),1)
            endif
            if ( cxb ) then
               tr = -mat_wdotcr(n-j+1,xr(j,j),xi(j,j),1,xbr(j),xbi(j),1)
               ti = -mat_wdotci(n-j+1,xr(j,j),xi(j,j),1,xbr(j),xbi(j),1)
               call mat_wdiv(tr,ti,xr(j,j),xi(j,j),tr,ti)
               call matx_waxpy(n-j+1,tr,ti,xr(j,j),xi(j,j),1,xbr(j),xbi(j),1)
            endif
            xr(j,j) = tempr
            xi(j,j) = tempi
         enddo
      endif
   else
      if ( cqy ) then
         qyr(1) = yr(1)
         qyi(1) = yi(1)
      endif
      if ( cqty ) then
         qtyr(1) = yr(1)
         qtyi(1) = yi(1)
      endif
      if ( cxb ) then
         xbr(1) = yr(1)
         xbi(1) = yi(1)
      endif
      if ( cb ) then
         if ( cabs1(xr(1,1),xi(1,1))/=0.0d0 ) then
            call mat_wdiv(yr(1),yi(1),xr(1,1),xi(1,1),br(1),bi(1))
         else
            info = 1
         endif
      endif
      if ( cr ) then
         rsdr(1) = 0.0d0
         rsdi(1) = 0.0d0
      endif
   endif
end subroutine ml_wqrsl
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!*==ml_wsvdc.f90 processed by SPAG 8.01RF 01:28 13 Dec 2024
subroutine ml_wsvdc(xr,xi,ldx,n,p,sr,si,er,ei,ur,ui,ldu,vr,vi,ldv,workr,worki,job,info)
use M_la
implicit none
integer ldx, n, p, ldu, ldv, job, info
double precision xr(ldx,*), xi(ldx,*), sr(*), si(*), er(*), ei(*), ur(ldu,*), ui(ldu,*), vr(ldv,*), vi(ldv,*), workr(*)&
&, worki(*)
!
!
!     WSVDC IS A SUBROUTINE TO REDUCE A DOUBLE-COMPLEX NXP MATRIX X BY
!     UNITARY TRANSFORMATIONS U AND V TO DIAGONAL FORM. THE
!     DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X. THE
!     COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,
!     AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.
!
!     ON ENTRY
!
!         X         DOUBLE-COMPLEX(LDX,P), WHERE LDX.GE.N.
!                   X CONTAINS THE MATRIX WHOSE SINGULAR VALUE
!                   DECOMPOSITION IS TO BE COMPUTED. X IS
!                   DESTROYED BY WSVDC.
!
!         LDX       INTEGER.
!                   LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!         N         INTEGER.
!                   N IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!         P         INTEGER.
!                   P IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!         LDU       INTEGER.
!                   LDU IS THE LEADING DIMENSION OF THE ARRAY U
!                   (SEE BELOW).
!
!         LDV       INTEGER.
!                   LDV IS THE LEADING DIMENSION OF THE ARRAY V
!                   (SEE BELOW).
!
!         WORK      DOUBLE-COMPLEX(N).
!                   WORK IS A SCRATCH ARRAY.
!
!         JOB       INTEGER.
!                   JOB CONTROLS THE COMPUTATION OF THE SINGULAR
!                   VECTORS. IT HAS THE DECIMAL EXPANSION AB
!                   WITH THE FOLLOWING MEANING
!
!     A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR
!               VECTORS.
!     A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS
!               IN U.
!     A.GE.2    RETURNS THE FIRST MIN(N,P)
!               LEFT SINGULAR VECTORS IN U.
!     B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR
!               VECTORS.
!     B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS
!               IN V.
!
!     ON RETURN
!
!         S         DOUBLE-COMPLEX(MM), WHERE MM=MIN(N+1,P).
!                   THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE
!                   SINGULAR VALUES OF X ARRANGED IN DESCENDING
!                   ORDER OF MAGNITUDE.
!
!         E         DOUBLE-COMPLEX(P).
!                   E ORDINARILY CONTAINS ZEROS. HOWEVER SEE THE
!                   DISCUSSION OF INFO FOR EXCEPTIONS.
!
!         U         DOUBLE-COMPLEX(LDU,K), WHERE LDU.GE.N.
!                   IF JOBA.EQ.1 THEN K.EQ.N,
!                   IF JOBA.EQ.2 THEN K.EQ.MIN(N,P).
!                   U CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   U IS NOT REFERENCED IF JOBA.EQ.0. IF N.LE.P
!                   OR IF JOBA.GT.2, THEN U MAY BE IDENTIFIED WITH X
!                   IN THE SUBROUTINE CALL.
!
!         V         DOUBLE-COMPLEX(LDV,P), WHERE LDV.GE.P.
!                   V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   V IS NOT REFERENCED IF JOBB.EQ.0. IF P.LE.N,
!                   THEN V MAY BE IDENTIFIED WITH X IN THE
!                   SUBROUTINE ML_CALL.
!
!         INFO      INTEGER.
!                   THE SINGULAR VALUES (AND THEIR CORRESPONDING
!                   SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)
!                   ARE CORRECT (HERE M=MIN(N,P)). THUS IF
!                   INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR
!                   VECTORS ARE CORRECT. IN ANY EVENT, THE MATRIX
!                   B = CTRANS(U)*X*V IS THE BIDIAGONAL MATRIX
!                   WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE
!                   ELEMENTS OF E ON ITS SUPER-DIAGONAL (CTRANS(U)
!                   IS THE CONJUGATE-TRANSPOSE OF U). THUS THE
!                   SINGULAR VALUES OF X AND B ARE THE SAME.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS    matX_waxpy,mat_pythag,mat_wdotcr,mat_wdotci,mat_wscal,mat_wswap,
!             mat_rrotg,mat_wnrm2
!     FORTRAN DABS,DIMAG,DMAX1
!     FORTRAN MAX0,MIN0,MOD,DSQRT
!
!     INTERNAL VARIABLES
!
integer i, iter, j, jobu, k, kase, kk, l, ll, lls, lm1, lp1, ls, lu, m, maxit, mm, mm1, mp1, nct, nctp1,    &
& ncu, nrt, nrtp1
double precision tr, ti, rr, ri
double precision b, c, cs, el, emm1, f, g, scale, shift, sl, sm, sn, smm1, t1, test, ztest, small
logical wantu, wantv
!
double precision zdumr, zdumi
double precision cabs1
cabs1(zdumr,zdumi) = dabs(zdumr) + dabs(zdumi)
integer :: spag_nextblock_1
!
!     SET THE MAXIMUM NUMBER OF ITERATIONS.
!
   maxit = 75
!
!     SMALL NUMBER, ROUGHLY MACHINE EPSILON, USED TO AVOID UNDERFLOW
!
   small = 1.d0/2.d0**48
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
   wantu = .false.
   wantv = .false.
   jobu = mod(job,100)/10
   ncu = n
   if ( jobu>1 ) ncu = min0(n,p)
   if ( jobu/=0 ) wantu = .true.
   if ( mod(job,10)/=0 ) wantv = .true.
!
!     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS
!     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.
!
   info = 0
   nct = min0(n-1,p)
   nrt = max0(0,min0(p-2,n))
   lu = max0(nct,nrt)
   if ( lu>=1 ) then
      do l = 1, lu
         lp1 = l + 1
         if ( l<=nct ) then
!
!           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND
!           PLACE THE L-TH DIAGONAL IN S(L).
!
            sr(l) = mat_wnrm2(n-l+1,xr(l,l),xi(l,l),1)
            si(l) = 0.0d0
            if ( cabs1(sr(l),si(l))/=0.0d0 ) then
               if ( cabs1(xr(l,l),xi(l,l))/=0.0d0 ) call mat_wsign(sr(l),si(l),xr(l,l),xi(l,l),sr(l),si(l))
               call mat_wdiv(1.0d0,0.0d0,sr(l),si(l),tr,ti)
               call mat_wscal(n-l+1,tr,ti,xr(l,l),xi(l,l),1)
               xr(l,l) = mat_flop(1.0d0+xr(l,l))
            endif
            sr(l) = -sr(l)
            si(l) = -si(l)
         endif
         if ( p>=lp1 ) then
            do j = lp1, p
               if ( l<=nct ) then
                  if ( cabs1(sr(l),si(l))/=0.0d0 ) then
!
!              APPLY THE TRANSFORMATION.
!
                     tr = -mat_wdotcr(n-l+1,xr(l,l),xi(l,l),1,xr(l,j),xi(l,j),1)
                     ti = -mat_wdotci(n-l+1,xr(l,l),xi(l,l),1,xr(l,j),xi(l,j),1)
                     call mat_wdiv(tr,ti,xr(l,l),xi(l,l),tr,ti)
                     call matx_waxpy(n-l+1,tr,ti,xr(l,l),xi(l,l),1,xr(l,j),xi(l,j),1)
                  endif
               endif
!
!           PLACE THE L-TH ROW OF X INTO  E FOR THE
!           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION.
!
               er(j) = xr(l,j)
               ei(j) = -xi(l,j)
            enddo
         endif
         if ( .not.(.not.wantu .or. l>nct) ) then
!
!           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK
!           MULTIPLICATION.
!
            do i = l, n
               ur(i,l) = xr(i,l)
               ui(i,l) = xi(i,l)
            enddo
         endif
         if ( l<=nrt ) then
!
!           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE
!           L-TH SUPER-DIAGONAL IN E(L).
!
            er(l) = mat_wnrm2(p-l,er(lp1),ei(lp1),1)
            ei(l) = 0.0d0
            if ( cabs1(er(l),ei(l))/=0.0d0 ) then
               if ( cabs1(er(lp1),ei(lp1))/=0.0d0 ) call mat_wsign(er(l),ei(l),er(lp1),ei(lp1),er(l),ei(l))
               call mat_wdiv(1.0d0,0.0d0,er(l),ei(l),tr,ti)
               call mat_wscal(p-l,tr,ti,er(lp1),ei(lp1),1)
               er(lp1) = mat_flop(1.0d0+er(lp1))
            endif
            er(l) = -er(l)
            ei(l) = +ei(l)
            if ( lp1<=n .and. cabs1(er(l),ei(l))/=0.0d0 ) then
!
!              APPLY THE TRANSFORMATION.
!
               do i = lp1, n
                  workr(i) = 0.0d0
                  worki(i) = 0.0d0
               enddo
               do j = lp1, p
                  call matx_waxpy(n-l,er(j),ei(j),xr(lp1,j),xi(lp1,j),1,workr(lp1),worki(lp1),1)
               enddo
               do j = lp1, p
                  call mat_wdiv(-er(j),-ei(j),er(lp1),ei(lp1),tr,ti)
                  call matx_waxpy(n-l,tr,-ti,workr(lp1),worki(lp1),1,xr(lp1,j),xi(lp1,j),1)
               enddo
            endif
            if ( wantv ) then
!
!              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT
!              BACK MULTIPLICATION.
!
               do i = lp1, p
                  vr(i,l) = er(i)
                  vi(i,l) = ei(i)
               enddo
            endif
         endif
      enddo
   endif
!
!     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.
!
   m = min0(p,n+1)
   nctp1 = nct + 1
   nrtp1 = nrt + 1
   if ( nct<p ) then
      sr(nctp1) = xr(nctp1,nctp1)
      si(nctp1) = xi(nctp1,nctp1)
   endif
   if ( n<m ) then
      sr(m) = 0.0d0
      si(m) = 0.0d0
   endif
   if ( nrtp1<m ) then
      er(nrtp1) = xr(nrtp1,m)
      ei(nrtp1) = xi(nrtp1,m)
   endif
   er(m) = 0.0d0
   ei(m) = 0.0d0
!
!     IF REQUIRED, GENERATE U.
!
   if ( wantu ) then
      if ( ncu>=nctp1 ) then
         do j = nctp1, ncu
            do i = 1, n
               ur(i,j) = 0.0d0
               ui(i,j) = 0.0d0
            enddo
            ur(j,j) = 1.0d0
            ui(j,j) = 0.0d0
         enddo
      endif
      if ( nct>=1 ) then
         do ll = 1, nct
            l = nct - ll + 1
            if ( cabs1(sr(l),si(l))==0.0d0 ) then
               do i = 1, n
                  ur(i,l) = 0.0d0
                  ui(i,l) = 0.0d0
               enddo
               ur(l,l) = 1.0d0
               ui(l,l) = 0.0d0
            else
               lp1 = l + 1
               if ( ncu>=lp1 ) then
                  do j = lp1, ncu
                     tr = -mat_wdotcr(n-l+1,ur(l,l),ui(l,l),1,ur(l,j),ui(l,j),1)
                     ti = -mat_wdotci(n-l+1,ur(l,l),ui(l,l),1,ur(l,j),ui(l,j),1)
                     call mat_wdiv(tr,ti,ur(l,l),ui(l,l),tr,ti)
                     call matx_waxpy(n-l+1,tr,ti,ur(l,l),ui(l,l),1,ur(l,j),ui(l,j),1)
                  enddo
               endif
               call mat_wrscal(n-l+1,-1.0d0,ur(l,l),ui(l,l),1)
               ur(l,l) = mat_flop(1.0d0+ur(l,l))
               lm1 = l - 1
               if ( lm1>=1 ) then
                  do i = 1, lm1
                     ur(i,l) = 0.0d0
                     ui(i,l) = 0.0d0
                  enddo
               endif
            endif
         enddo
      endif
   endif
!
!     IF IT IS REQUIRED, GENERATE V.
!
   if ( wantv ) then
      do ll = 1, p
         l = p - ll + 1
         lp1 = l + 1
         if ( l<=nrt ) then
            if ( cabs1(er(l),ei(l))/=0.0d0 ) then
               do j = lp1, p
                  tr = -mat_wdotcr(p-l,vr(lp1,l),vi(lp1,l),1,vr(lp1,j),vi(lp1,j),1)
                  ti = -mat_wdotci(p-l,vr(lp1,l),vi(lp1,l),1,vr(lp1,j),vi(lp1,j),1)
                  call mat_wdiv(tr,ti,vr(lp1,l),vi(lp1,l),tr,ti)
                  call matx_waxpy(p-l,tr,ti,vr(lp1,l),vi(lp1,l),1,vr(lp1,j),vi(lp1,j),1)
               enddo
            endif
         endif
         do i = 1, p
            vr(i,l) = 0.0d0
            vi(i,l) = 0.0d0
         enddo
         vr(l,l) = 1.0d0
         vi(l,l) = 0.0d0
      enddo
   endif
!
!     TRANSFORM S AND E SO THAT THEY ARE REAL.
!
   spag_loop_1_1: do i = 1, m
      tr = mat_pythag(sr(i),si(i))
      if ( tr/=0.0d0 ) then
         rr = sr(i)/tr
         ri = si(i)/tr
         sr(i) = tr
         si(i) = 0.0d0
         if ( i<m ) call mat_wdiv(er(i),ei(i),rr,ri,er(i),ei(i))
         if ( wantu ) call mat_wscal(n,rr,ri,ur(1,i),ui(1,i),1)
      endif
!     ...EXIT
      if ( i==m ) exit spag_loop_1_1
      tr = mat_pythag(er(i),ei(i))
      if ( tr/=0.0d0 ) then
         call mat_wdiv(tr,0.0d0,er(i),ei(i),rr,ri)
         er(i) = tr
         ei(i) = 0.0d0
         call mat_wmul(sr(i+1),si(i+1),rr,ri,sr(i+1),si(i+1))
         if ( wantv ) call mat_wscal(p,rr,ri,vr(1,i+1),vi(1,i+1),1)
      endif
   enddo spag_loop_1_1
!
!     MAIN ITERATION LOOP FOR THE SINGULAR VALUES.
!
   mm = m
   iter = 0
!
!        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.
!
!     ...EXIT
   spag_loop_1_5: do while ( m/=0 )
      spag_nextblock_1 = 1
      spag_dispatchloop_1: do
         select case (spag_nextblock_1)
          case (1)
!
!        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET
!        FLAG AND RETURN.
!
            if ( iter<maxit ) then
!
!        THIS SECTION OF THE PROGRAM INSPECTS FOR
!        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS. ON
!        COMPLETION THE VARIABLE KASE IS SET AS FOLLOWS.
!
!           KASE = 1     IF SR(M) AND ER(L-1) ARE NEGLIGIBLE AND L.LT.M
!           KASE = 2     IF SR(L) IS NEGLIGIBLE AND L.LT.M
!           KASE = 3     IF ER(L-1) IS NEGLIGIBLE, L.LT.M, AND
!     SR(L), ..., SR(M) ARE NOT NEGLIGIBLE (QR STEP).
!           KASE = 4     IF ER(M-1) IS NEGLIGIBLE (CONVERGENCE).
!
               spag_loop_2_2: do ll = 1, m
                  l = m - ll
!        ...EXIT
                  if ( l==0 ) exit spag_loop_2_2
                  test = mat_flop(dabs(sr(l))+dabs(sr(l+1)))
                  ztest = mat_flop(test+dabs(er(l))/2.0d0)
                  if ( small*ztest==small*test ) then
                     er(l) = 0.0d0
!        ......EXIT
                     exit spag_loop_2_2
                  endif
               enddo spag_loop_2_2
               if ( l/=m-1 ) then
                  lp1 = l + 1
                  mp1 = m + 1
                  spag_loop_2_3: do lls = lp1, mp1
                     ls = m - lls + lp1
!           ...EXIT
                     if ( ls==l ) exit spag_loop_2_3
                     test = 0.0d0
                     if ( ls/=m ) test = mat_flop(test+dabs(er(ls)))
                     if ( ls/=l+1 ) test = mat_flop(test+dabs(er(ls-1)))
                     ztest = mat_flop(test+dabs(sr(ls))/2.0d0)
                     if ( small*ztest==small*test ) then
                        sr(ls) = 0.0d0
!           ......EXIT
                        exit spag_loop_2_3
                     endif
                  enddo spag_loop_2_3
                  if ( ls==l ) then
                     kase = 3
                  elseif ( ls/=m ) then
                     kase = 2
                     l = ls
                  else
                     kase = 1
                  endif
               else
                  kase = 4
               endif
               l = l + 1
!
!        PERFORM THE TASK INDICATED BY KASE.
!
               if ( kase==2 ) then
!
!        SPLIT AT NEGLIGIBLE SR(L).
!
                  f = er(l-1)
                  er(l-1) = 0.0d0
                  do k = l, m
                     t1 = sr(k)
                     call mat_rrotg(t1,f,cs,sn)
                     sr(k) = t1
                     f = mat_flop(-(sn*er(k)))
                     er(k) = mat_flop(cs*er(k))
                     if ( wantu ) call mat_rrot(n,ur(1,k),1,ur(1,l-1),1,cs,sn)
                     if ( wantu ) call mat_rrot(n,ui(1,k),1,ui(1,l-1),1,cs,sn)
                  enddo
               elseif ( kase==3 ) then
!
!        PERFORM ONE QR STEP.
!
!
!           CALCULATE THE SHIFT.
!
                  scale = dmax1(dabs(sr(m)),dabs(sr(m-1)),dabs(er(m-1)),dabs(sr(l)),dabs(er(l)))
                  sm = sr(m)/scale
                  smm1 = sr(m-1)/scale
                  emm1 = er(m-1)/scale
                  sl = sr(l)/scale
                  el = er(l)/scale
                  b = mat_flop(((smm1+sm)*(smm1-sm)+emm1**2)/2.0d0)
                  c = mat_flop((sm*emm1)**2)
                  shift = 0.0d0
                  if ( b/=0.0d0 .or. c/=0.0d0 ) then
                     shift = mat_flop(dsqrt(b**2+c))
                     if ( b<0.0d0 ) shift = -shift
                     shift = mat_flop(c/(b+shift))
                  endif
                  f = mat_flop((sl+sm)*(sl-sm)-shift)
                  g = mat_flop(sl*el)
!
!           CHASE ZEROS.
!
                  mm1 = m - 1
                  do k = l, mm1
                     call mat_rrotg(f,g,cs,sn)
                     if ( k/=l ) er(k-1) = f
                     f = mat_flop(cs*sr(k)+sn*er(k))
                     er(k) = mat_flop(cs*er(k)-sn*sr(k))
                     g = mat_flop(sn*sr(k+1))
                     sr(k+1) = mat_flop(cs*sr(k+1))
                     if ( wantv ) call mat_rrot(p,vr(1,k),1,vr(1,k+1),1,cs,sn)
                     if ( wantv ) call mat_rrot(p,vi(1,k),1,vi(1,k+1),1,cs,sn)
                     call mat_rrotg(f,g,cs,sn)
                     sr(k) = f
                     f = mat_flop(cs*er(k)+sn*sr(k+1))
                     sr(k+1) = mat_flop(-(sn*er(k))+cs*sr(k+1))
                     g = mat_flop(sn*er(k+1))
                     er(k+1) = mat_flop(cs*er(k+1))
                     if ( wantu .and. k<n ) call mat_rrot(n,ur(1,k),1,ur(1,k+1),1,cs,sn)
                     if ( wantu .and. k<n ) call mat_rrot(n,ui(1,k),1,ui(1,k+1),1,cs,sn)
                  enddo
                  er(m-1) = f
                  iter = iter + 1
               elseif ( kase==4 ) then
!
!        CONVERGENCE
!
!
!           MAKE THE SINGULAR VALUE  POSITIVE
!
                  if ( sr(l)<0.0d0 ) then
                     sr(l) = -sr(l)
                     if ( wantv ) call mat_wrscal(p,-1.0d0,vr(1,l),vi(1,l),1)
                  endif
!
!           ORDER THE SINGULAR VALUE.
!
                  spag_loop_2_4: do while ( l/=mm )
!           ...EXIT
                     if ( sr(l)>=sr(l+1) ) exit spag_loop_2_4
                     tr = sr(l)
                     sr(l) = sr(l+1)
                     sr(l+1) = tr
                     if ( wantv .and. l<p ) call mat_wswap(p,vr(1,l),vi(1,l),1,vr(1,l+1),vi(1,l+1),1)
                     if ( wantu .and. l<n ) call mat_wswap(n,ur(1,l),ui(1,l),1,ur(1,l+1),ui(1,l+1),1)
                     l = l + 1
                  enddo spag_loop_2_4
                  spag_nextblock_1 = 2
                  cycle spag_dispatchloop_1
               else
!
!        DEFLATE NEGLIGIBLE SR(M).
!
                  mm1 = m - 1
                  f = er(m-1)
                  er(m-1) = 0.0d0
                  do kk = l, mm1
                     k = mm1 - kk + l
                     t1 = sr(k)
                     call mat_rrotg(t1,f,cs,sn)
                     sr(k) = t1
                     if ( k/=l ) then
                        f = mat_flop(-(sn*er(k-1)))
                        er(k-1) = mat_flop(cs*er(k-1))
                     endif
                     if ( wantv ) call mat_rrot(p,vr(1,k),1,vr(1,m),1,cs,sn)
                     if ( wantv ) call mat_rrot(p,vi(1,k),1,vi(1,m),1,cs,sn)
                  enddo
               endif
               cycle
            else
               info = m
!     ......EXIT
               exit spag_loop_1_5
            endif
          case (2)
            iter = 0
            m = m - 1
            exit spag_dispatchloop_1
         end select
      enddo spag_dispatchloop_1
   enddo spag_loop_1_5
end subroutine ml_wsvdc
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

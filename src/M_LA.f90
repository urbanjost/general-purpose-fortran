module m_la
use,intrinsic :: iso_fortran_env, only : stderr=>error_unit, stdin=>input_unit, stdout=>output_unit
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
public mat_wpow
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

integer,save             :: la_flop_counter(2)=[0,0]

intrinsic :: sign, sqrt, abs, atan, atan2, max, dble

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

interface
   subroutine matx_waxpy(n, sr, si, xr, xi, incx, yr, yi, incy)
      import  int32, real64
      integer(kind=int32), intent(in) :: n
      real(kind=real64), intent(in) :: sr
      real(kind=real64), intent(in) :: si
      real(kind=real64), intent(in) :: xr(*)
      real(kind=real64), intent(in) :: xi(*)
      integer(kind=int32), intent(in) :: incx
      real(kind=real64) :: yr(*)
      real(kind=real64) :: yi(*)
      integer(kind=int32), intent(in) :: incy
   end subroutine matx_waxpy
end interface

interface
   subroutine ml_comqr3(nm, n, low, igh, ortr, orti, hr, hi, wr, wi, zr, zi, ierr, job)
      import  int32, real64
      integer(kind=int32) :: igh
      integer(kind=int32) :: n
      integer(kind=int32) :: nm
      integer(kind=int32) :: low
      real(kind=real64) :: ortr(igh)
      real(kind=real64) :: orti(igh)
      real(kind=real64) :: hr(nm, n)
      real(kind=real64) :: hi(nm, n)
      real(kind=real64) :: wr(n)
      real(kind=real64) :: wi(n)
      real(kind=real64) :: zr(nm, n)
      real(kind=real64) :: zi(nm, n)
      integer(kind=int32) :: ierr
      integer(kind=int32) :: job
   end subroutine ml_comqr3
end interface

interface
   subroutine ml_corth(nm, n, low, igh, ar, ai, ortr, orti)
      import  int32, real64
      integer(kind=int32) :: igh
      integer(kind=int32) :: n
      integer(kind=int32) :: nm
      integer(kind=int32) :: low
      real(kind=real64) :: ar(nm, n)
      real(kind=real64) :: ai(nm, n)
      real(kind=real64) :: ortr(igh)
      real(kind=real64) :: orti(igh)
   end subroutine ml_corth
end interface

interface
   subroutine ml_htribk(nm, n, ar, ai, tau, m, zr, zi)
      import  int32, real64
      integer(kind=int32) :: m
      integer(kind=int32) :: n
      integer(kind=int32) :: nm
      real(kind=real64) :: ar(nm, n)
      real(kind=real64) :: ai(nm, n)
      real(kind=real64) :: tau(2, n)
      real(kind=real64) :: zr(nm, m)
      real(kind=real64) :: zi(nm, m)
   end subroutine ml_htribk
end interface

interface
   subroutine ml_htridi(nm, n, ar, ai, d, e, e2, tau)
      import  int32, real64
      integer(kind=int32) :: n
      integer(kind=int32) :: nm
      real(kind=real64) :: ar(nm, n)
      real(kind=real64) :: ai(nm, n)
      real(kind=real64) :: d(n)
      real(kind=real64) :: e(n)
      real(kind=real64) :: e2(n)
      real(kind=real64) :: tau(2, n)
   end subroutine ml_htridi
end interface

interface
   subroutine ml_imtql2(nm, n, d, e, z, ierr, job)
      import  int32, real64
      integer(kind=int32) :: n
      integer(kind=int32) :: nm
      real(kind=real64) :: d(n)
      real(kind=real64) :: e(n)
      real(kind=real64) :: z(nm, n)
      integer(kind=int32) :: ierr
      integer(kind=int32) :: job
   end subroutine ml_imtql2
end interface

interface
   subroutine ml_wgeco(ar, ai, lda, n, ipvt, rcond, zr, zi)
      import  int32, real64
      integer(kind=int32) :: lda
      real(kind=real64) :: ar(lda, *)
      real(kind=real64) :: ai(lda, *)
      integer(kind=int32) :: n
      integer(kind=int32) :: ipvt(*)
      real(kind=real64) :: rcond
      real(kind=real64) :: zr(*)
      real(kind=real64) :: zi(*)
   end subroutine ml_wgeco
end interface

interface
   subroutine ml_wgedi(ar, ai, lda, n, ipvt, detr, deti, workr, worki, job)
      import  int32, real64
      integer(kind=int32) :: lda
      real(kind=real64) :: ar(lda, *)
      real(kind=real64) :: ai(lda, *)
      integer(kind=int32) :: n
      integer(kind=int32) :: ipvt(*)
      real(kind=real64) :: detr(2)
      real(kind=real64) :: deti(2)
      real(kind=real64) :: workr(*)
      real(kind=real64) :: worki(*)
      integer(kind=int32) :: job
   end subroutine ml_wgedi
end interface

interface
   subroutine ml_wgefa(ar, ai, lda, n, ipvt, info)
      import  int32, real64
      integer(kind=int32) :: lda
      real(kind=real64) :: ar(lda, *)
      real(kind=real64) :: ai(lda, *)
      integer(kind=int32) :: n
      integer(kind=int32) :: ipvt(*)
      integer(kind=int32) :: info
   end subroutine ml_wgefa
end interface

interface
   subroutine ml_wgesl(ar, ai, lda, n, ipvt, br, bi, job)
      import  int32, real64
      integer(kind=int32) :: lda
      real(kind=real64) :: ar(lda, *)
      real(kind=real64) :: ai(lda, *)
      integer(kind=int32) :: n
      integer(kind=int32) :: ipvt(*)
      real(kind=real64) :: br(*)
      real(kind=real64) :: bi(*)
      integer(kind=int32) :: job
   end subroutine ml_wgesl
end interface

interface
   subroutine ml_wqrdc(xr, xi, ldx, n, p, qrauxr, qrauxi, jpvt, workr, worki, job)
      import  int32, real64
      integer(kind=int32) :: ldx
      real(kind=real64) :: xr(ldx, *)
      real(kind=real64) :: xi(ldx, *)
      integer(kind=int32) :: n
      integer(kind=int32) :: p
      real(kind=real64) :: qrauxr(*)
      real(kind=real64) :: qrauxi(*)
      integer(kind=int32) :: jpvt(*)
      real(kind=real64) :: workr(*)
      real(kind=real64) :: worki(*)
      integer(kind=int32) :: job
   end subroutine ml_wqrdc
end interface

interface
   subroutine ml_wqrsl(xr, xi, ldx, n, k, qrauxr, qrauxi, yr, yi, qyr, qyi, qtyr, qtyi, br, bi, rsdr, rsdi, xbr, xbi, job, info)
      import  int32, real64
      integer(kind=int32) :: ldx
      real(kind=real64) :: xr(ldx, *)
      real(kind=real64) :: xi(ldx, *)
      integer(kind=int32) :: n
      integer(kind=int32) :: k
      real(kind=real64) :: qrauxr(*)
      real(kind=real64) :: qrauxi(*)
      real(kind=real64) :: yr(*)
      real(kind=real64) :: yi(*)
      real(kind=real64) :: qyr(*)
      real(kind=real64) :: qyi(*)
      real(kind=real64) :: qtyr(*)
      real(kind=real64) :: qtyi(*)
      real(kind=real64) :: br(*)
      real(kind=real64) :: bi(*)
      real(kind=real64) :: rsdr(*)
      real(kind=real64) :: rsdi(*)
      real(kind=real64) :: xbr(*)
      real(kind=real64) :: xbi(*)
      integer(kind=int32) :: job
      integer(kind=int32) :: info
   end subroutine ml_wqrsl
end interface

interface
   subroutine ml_wsvdc(xr, xi, ldx, n, p, sr, si, er, ei, ur, ui, ldu, vr, vi, ldv, workr, worki, job, info)
      import  int32, real64
      integer(kind=int32) :: ldv
      integer(kind=int32) :: ldu
      integer(kind=int32) :: ldx
      real(kind=real64) :: xr(ldx, *)
      real(kind=real64) :: xi(ldx, *)
      integer(kind=int32) :: n
      integer(kind=int32) :: p
      real(kind=real64) :: sr(*)
      real(kind=real64) :: si(*)
      real(kind=real64) :: er(*)
      real(kind=real64) :: ei(*)
      real(kind=real64) :: ur(ldu, *)
      real(kind=real64) :: ui(ldu, *)
      real(kind=real64) :: vr(ldv, *)
      real(kind=real64) :: vi(ldv, *)
      real(kind=real64) :: workr(*)
      real(kind=real64) :: worki(*)
      integer(kind=int32) :: job
      integer(kind=int32) :: info
   end subroutine ml_wsvdc
end interface

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
      tol = max(tol,mat_wasum(m,ar(1,j),ai(1,j),1))
   enddo
   tol = eps*dble(2*max(m,n))*tol
   k = 1
   l = 1
   infinite: do
      if (k.gt.m .or. l.gt.n) return

      i = mat_iwamax(m-k+1,ar(k,l),ai(k,l),1) + k-1
      if (abs(ar(i,l))+abs(ai(i,l)) .le. tol)then
         call mat_wset(m-k+1,0.0d0,0.0d0,ar(k,l),ai(k,l),1)
         l = l+1
         cycle infinite
      endif

      call mat_wswap(n-l+1,ar(i,l),ai(i,l),lda,ar(k,l),ai(k,l),lda)
      call mat_wdiv(1.0d0,0.0d0,ar(k,l),ai(k,l),tr,ti)
      call mat_wscal(n-l+1,tr,ti,ar(k,l),ai(k,l),lda)
      ar(k,l) = 1.0d0
      ai(k,l) = 0.0d0
      do i = 1, m
         tr = -ar(i,l)
         ti = -ai(i,l)
         if (i .ne. k) call matx_waxpy(n-l+1,tr,ti,ar(k,l),ai(k,l),lda,ar(i,l),ai(i,l),lda)
      enddo
      k = k+1
      l = l+1
   enddo infinite
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

   p = max(abs(a),abs(b))
   q = min(abs(a),abs(b))

   if (q .ne. 0.0d0) then

      infinite : do
         r = (q/p)**2
         t = 4.0d0 + r
         if (t .eq. 4.0d0) exit infinite
         s = r/t
         p = p + 2.0d0*p*s
         q = q*s
      enddo infinite

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
subroutine mat_wpow(in_real,in_imag,out_real,out_imag,power_real,power_imag)

! ident_21="@(#)M_LA::mat_wpow(3fp): y = x**p

doubleprecision,intent(in)  :: in_real
doubleprecision,intent(in)  :: in_imag
doubleprecision,intent(in)  :: power_real
doubleprecision,intent(in)  :: power_imag
doubleprecision,intent(out) :: out_real
doubleprecision,intent(out) :: out_imag
complex(kind=real64)        :: t
   ! placeholder method, just using Fortran
   t=cmplx(in_real,in_imag,kind=real64)**cmplx(power_real,power_imag,kind=real64)
   out_real=mat_flop(real(t,kind=real64))
   out_imag=mat_flop(aimag(t))
end subroutine mat_wpow
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wsqrt(x_real,x_imag,y_real,y_imag)

! ident_21="@(#)M_LA::mat_wsqrt(3fp): y = sqrt(x) with y_real .ge. 0.0 and sign(y_imag) .eq. sign(x_imag)"

doubleprecision,intent(in)  :: x_real
doubleprecision,intent(in)  :: x_imag
doubleprecision,intent(out) :: y_real
doubleprecision,intent(out) :: y_imag
doubleprecision             :: s
doubleprecision             :: tr
doubleprecision             :: ti
!
   tr = x_real
   ti = x_imag
   s = sqrt(0.5d0*(mat_pythag(tr,ti) + abs(tr)))
   if (tr .ge. 0.0d0) y_real = mat_flop(s)
   if (ti .lt. 0.0d0) s = -s
   if (tr .le. 0.0d0) y_imag = mat_flop(s)
   if (tr .lt. 0.0d0) y_real = mat_flop(0.5d0*(ti/y_imag))
   if (tr .gt. 0.0d0) y_imag = mat_flop(0.5d0*(ti/y_real))
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
      if (abs(z)*dble(maxd) .le. 1.0d0) exit
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
!-----------------------------------------------------------------------
   if (m2 .eq. 0) then                                ! if first entry, compute machine integer word length
      m = 1
      infinite : do
         m2 = m
         m = itwo*m2
         if (m .le. m2) exit infinite
      enddo infinite
      halfm = m2
      ia = 8*int(halfm*atan(1.d0)/8.d0) + 5          ! compute multiplier and increment for linear congruential method
      ic = 2*int(halfm*(0.5d0-sqrt(3.d0)/6.d0)) + 1
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
         s = mat_flop(s + abs(xr(ix)) + abs(xi(ix)))
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
         p = abs(xr(ix)) + abs(xi(ix))
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

   la_flop_counter(1) = la_flop_counter(1) + 1
   k = la_flop_counter(2)

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
   z = abs(x)
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
subroutine mat_wpofa(ar,ai,lda,n,info)
implicit none
integer          :: lda
double precision :: ar(lda,*)
double precision :: ai(lda,*)
integer          :: n
integer          :: info

double precision :: s
double precision :: tr
double precision :: ti
integer          :: j
integer          :: jm1
integer          :: k

   do j = 1 , n
      info = j
      s = 0.0d0
      jm1 = j - 1
      if ( jm1>=1 ) then
         do k = 1 , jm1
            tr = ar(k,j) - mat_wdotcr(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
            ti = ai(k,j) - mat_wdotci(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
            call mat_wdiv(tr,ti,ar(k,k),ai(k,k),tr,ti)
            ar(k,j) = tr
            ai(k,j) = ti
            s = s + tr*tr + ti*ti
         enddo
      endif
      s = ar(j,j) - s
      if ( s<=0.0d0 .or. ai(j,j)/=0.0d0 ) return
      ar(j,j) = sqrt(s)
   enddo
   info = 0
end subroutine mat_wpofa
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
   if ( abs(da) .gt. abs(db) ) rho = da
   c = 1.0d0
   s = 0.0d0
   z = 1.0d0
   r = mat_flop(sign(mat_pythag(da,db),rho))
   if (r .ne. 0.0d0) c = mat_flop(da/r)
   if (r .ne. 0.0d0) s = mat_flop(db/r)
   if ( abs(da) .gt. abs(db) ) z = s
   if (abs(db) .ge. abs(da) .and. c .ne. 0.0d0)z = mat_flop(1.0d0/c)
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

doubleprecision,intent(in)  :: ar
doubleprecision,intent(in)  :: ai
doubleprecision,intent(in)  :: br
doubleprecision,intent(in)  :: bi
doubleprecision,intent(out) :: cr
doubleprecision,intent(out) :: ci

doubleprecision :: s
doubleprecision :: d
doubleprecision :: ars
doubleprecision :: ais
doubleprecision :: brs
doubleprecision :: bis

   s = abs(br) + abs(bi)
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
      t = atan2(in_imag,in_real)
      if (in_imag.eq.0.0d0 .and. in_real.lt.0.0d0) t = abs(t)
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
      yr = atan2(xr,1.0d0)
      yi = 0.0d0
   elseif (xr.ne.0.0d0 .or. abs(xi).ne.1.0d0) then
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
end module m_la












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

integer,parameter,private:: sp=kind(1.0),dp=kind(1.0d0)

integer,save             :: LA_FLOP_COUNTER(2)=[0,0]

interface linspace
   module procedure  &
   & linspace_real128, linspace_real64, linspace_real32, &
   & linspace_int64,   linspace_int32,  linspace_int16,  linspace_int8
end interface linspace

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     linspace(3f) - [M_LA] - return a vector of linearly spaced values
!!##SYNOPSIS
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
!!##RETURNS
!!    LINSPACE  The returned row vector starts with X1 and ends with X2,
!!              returning N evenly spaced values.
!!##EXAMPLE
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
integer,intent(in)             :: n
real(kind=real128),intent(in)  :: x1,x2
real(kind=real128)             :: linspace_real128(n)
integer(kind=int64)            :: i
   if(n.le.1)then
      linspace_real128=[x1,x2]
   else
      linspace_real128=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_real128
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_real64(x1,x2,n)
integer,intent(in)             :: n
real(kind=real64),intent(in)   :: x1,x2
real(kind=real64)              :: linspace_real64(n)
integer(kind=int64)            :: i
   if(n.le.1)then
      linspace_real64=[x1,x2]
   else
      linspace_real64=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_real64
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_real32(x1,x2,n)
integer,intent(in)             :: n
real(kind=real32),intent(in)   :: x1,x2
real(kind=real32)              :: linspace_real32(n)
integer(kind=int64)            :: i
   if(n.le.1)then
      linspace_real32=[x1,x2]
   else
      linspace_real32=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_real32
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int64(x1,x2,n)
integer,intent(in)             :: n
integer(kind=int64),intent(in) :: x1,x2
integer(kind=int64)            :: linspace_int64(n)
integer(kind=int64)            :: i
   if(n.le.1)then
      linspace_int64=[x1,x2]
   else
      linspace_int64=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int64
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int32(x1,x2,n)
integer,intent(in)             :: n
integer(kind=int32),intent(in) :: x1,x2
integer(kind=int32)            :: linspace_int32(n)
integer(kind=int64)            :: i
   if(n.le.1)then
      linspace_int32=[x1,x2]
   else
      linspace_int32=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int32
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int16(x1,x2,n)
integer,intent(in)             :: n
integer(kind=int16),intent(in) :: x1,x2
integer(kind=int16)            :: linspace_int16(n)
integer(kind=int64)            :: i
   if(n.le.1)then
      linspace_int16=[x1,x2]
   else
      linspace_int16=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int16
!-----------------------------------------------------------------------------------------------------------------------------------
function linspace_int8(x1,x2,n)
integer,intent(in)             :: n
integer(kind=int8),intent(in)  :: x1,x2
integer(kind=int8)             :: linspace_int8(n)
integer(kind=int64)            :: i
   if(n.le.1)then
      linspace_int8=[x1,x2]
   else
   linspace_int8=[(x1+i*(x2-x1)/(n-1),i=0,n-1)]
   endif
end function linspace_int8
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_inverse_hilbert(a,lda,n)

! ident_1="@(#)M_LA::mat_inverse_hilbert(3fp): generate doubleprecision inverse hilbert matrix"
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
subroutine mat_magic(a,lda,n)
!
! ident_2="@(#)M_LA::mat_magic(3fp): Algorithms for magic squares"

!        Algorithms taken from
!        Mathematical Recreations and Essays, 12th Ed.,
!        by W. W. Rouse Ball and H. S. M. Coxeter
!
integer         :: lda
integer         :: n
doubleprecision :: a(lda,n)

doubleprecision :: t
integer         :: i
integer         :: j
integer         :: k
integer         :: m
integer         :: mm
integer         :: i1
integer         :: im
integer         :: j1
integer         :: jm
integer         :: m1
integer         :: m2
!
   if (mod(n,4) .eq. 0) goto 100
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
      if(int(a(i1,j1)).eq.0) goto 30
      i1 = i+1
      j1 = j
30    continue
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
   return
!
!     double even order
!
100 continue
   k = 1
   do i = 1, n
      do j = 1, n
         a(i,j) = k
         if (mod(i,4)/2 .eq. mod(j,4)/2) a(i,j) = n*n+1 - k
         k = k+1
      enddo
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
doubleprecision,intent(in) :: xi    ! constant to assing Y imaginary values to
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
subroutine mat_wpofa(ar,ai,lda,n,info)
integer          :: lda
doubleprecision  :: ar(lda,*)
doubleprecision  :: ai(lda,*)
integer          :: n
integer          :: info

doubleprecision  :: s
doubleprecision  :: TR
doubleprecision  :: TI
integer          :: j
integer          :: jm1
integer          :: k

   do j = 1, n
      info = j
      s = 0.0d0
      jm1 = j-1
      if (jm1 .ge. 1) then
         do k = 1, jm1
           tr=ar(k,j)-mat_wdotcr(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
           ti=ai(k,j)-mat_wdotci(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
           call mat_wdiv(tr,ti,ar(k,k),ai(k,k),tr,ti)
           ar(k,j) = tr
           ai(k,j) = ti
           s = s + tr*tr + ti*ti
         enddo
      endif
      s = ar(j,j) - s
      if (s.le.0.0d0 .or. ai(j,j).ne.0.0d0) goto 40
      ar(j,j) = dsqrt(s)
   enddo
   info = 0
40 continue
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

! ident_3="@(#)M_matrix::la_err(3fp): given error number, write associated error message"

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
      SUBROUTINE ML_WGECO(AR,AI,LDA,N,IPVT,RCOND,ZR,ZI)
      use M_LA
      INTEGER LDA,N,IPVT(*)
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*),ZR(*),ZI(*)
      DOUBLEPRECISION RCOND
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
      DOUBLEPRECISION EKR,EKI,TR,TI,WKR,WKI,WKMR,WKMI
      DOUBLEPRECISION ANORM,S,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE 1-NORM OF A
!
      ANORM = 0.0D0
      DO J = 1, N
         ANORM = DMAX1(ANORM,mat_wasum(N,AR(1,J),AI(1,J),1))
      enddo
!
!     FACTOR
!
      CALL ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
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
      EKR = 1.0D0
      EKI = 0.0D0
      DO J = 1, N
         ZR(J) = 0.0D0
         ZI(J) = 0.0D0
      enddo
      DO 110 K = 1, N
         CALL mat_wsign(EKR,EKI,-ZR(K),-ZI(K),EKR,EKI)
         IF (CABS1(EKR-ZR(K),EKI-ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 40
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(EKR-ZR(K),EKI-ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            EKR = S*EKR
            EKI = S*EKI
   40    CONTINUE
         WKR = EKR - ZR(K)
         WKI = EKI - ZI(K)
         WKMR = -EKR - ZR(K)
         WKMI = -EKI - ZI(K)
         S = CABS1(WKR,WKI)
         SM = CABS1(WKMR,WKMI)
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 50
            CALL mat_wdiv(WKR,WKI,AR(K,K),-AI(K,K),WKR,WKI)
            CALL mat_wdiv(WKMR,WKMI,AR(K,K),-AI(K,K),WKMR,WKMI)
         GOTO 60
   50    CONTINUE
            WKR = 1.0D0
            WKI = 0.0D0
            WKMR = 1.0D0
            WKMI = 0.0D0
   60    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GOTO 100
            DO J = KP1, N
               CALL mat_wmul(WKMR,WKMI,AR(K,J),-AI(K,J),TR,TI)
               SM = mat_flop(SM + CABS1(ZR(J)+TR,ZI(J)+TI))
               CALL matX_waxpy(1,WKR,WKI,[AR(K,J)],[-AI(K,J)],1,ZR(J),ZI(J),1)
               S = mat_flop(S + CABS1(ZR(J),ZI(J)))
            enddo
            IF (S .GE. SM) GOTO 90
               TR = WKMR - WKR
               TI = WKMI - WKI
               WKR = WKMR
               WKI = WKMI
               DO J = KP1, N
                  CALL matX_waxpy(1,TR,TI,[AR(K,J)],[-AI(K,J)],1,ZR(J),ZI(J),1)
               enddo
   90       CONTINUE
  100    CONTINUE
         ZR(K) = WKR
         ZI(K) = WKI
  110 CONTINUE
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
!
!     SOLVE CTRANS(L)*Y = W
!
      DO KB = 1, N
         K = N + 1 - KB
         IF (K .GE. N) GOTO 120
            ZR(K) = ZR(K) + mat_wdotcr(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
            ZI(K) = ZI(K) + mat_wdotci(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
  120    CONTINUE
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) GOTO 130
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
  130    CONTINUE
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
      enddo
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
!
      YNORM = 1.0D0
!
!     SOLVE L*V = Y
!
      DO K = 1, N
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
         IF (K .LT. N) CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) cycle
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            YNORM = S*YNORM
      enddo
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
!     SOLVE  U*Z = V
!
      DO KB = 1, N
         K = N + 1 - KB
         IF (CABS1(ZR(K),ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 170
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            YNORM = S*YNORM
  170    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 180
            CALL mat_wdiv(ZR(K),ZI(K),AR(K,K),AI(K,K),ZR(K),ZI(K))
  180    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .NE. 0.0D0) GOTO 190
            ZR(K) = 1.0D0
            ZI(K) = 0.0D0
  190    CONTINUE
         TR = -ZR(K)
         TI = -ZI(K)
         CALL matX_waxpy(K-1,TR,TI,AR(1,K),AI(1,K),1,ZR(1),ZI(1),1)
      enddo
!     MAKE ZNORM = 1.0
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      END SUBROUTINE ML_WGECO
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
      use M_LA
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*)
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
      DOUBLEPRECISION TR,TI
      INTEGER J,K,KP1,L,NM1
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GOTO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
!
!        FIND L = PIVOT INDEX
!
         L = mat_iwamax(N-K+1,AR(K,K),AI(K,K),1) + K - 1
         IPVT(K) = L
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
         IF (CABS1(AR(L,K),AI(L,K)) .EQ. 0.0D0) GOTO 40
!
!           INTERCHANGE IF NECESSARY
!
            IF (L .EQ. K) GOTO 10
               TR = AR(L,K)
               TI = AI(L,K)
               AR(L,K) = AR(K,K)
               AI(L,K) = AI(K,K)
               AR(K,K) = TR
               AI(K,K) = TI
   10       CONTINUE
!
!           COMPUTE MULTIPLIERS
!
            CALL mat_wdiv(-1.0D0,0.0D0,AR(K,K),AI(K,K),TR,TI)
            CALL mat_wscal(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1)
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            DO J = KP1, N
               TR = AR(L,J)
               TI = AI(L,J)
               IF (L .EQ. K) GOTO 20
                  AR(L,J) = AR(K,J)
                  AI(L,J) = AI(K,J)
                  AR(K,J) = TR
                  AI(K,J) = TI
   20          CONTINUE
               CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,AR(K+1,J),AI(K+1,J),1)
            enddo
         GOTO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (CABS1(AR(N,N),AI(N,N)) .EQ. 0.0D0) INFO = N
      END SUBROUTINE ML_WGEFA
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGESL(AR,AI,LDA,N,IPVT,BR,BI,JOB)
use M_LA
INTEGER LDA,N,IPVT(*),JOB
DOUBLEPRECISION AR(LDA,*),AI(LDA,*),BR(*),BI(*)
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
DOUBLEPRECISION TR,TI
INTEGER K,KB,L,NM1
!
   NM1 = N - 1
   IF (JOB .NE. 0) GOTO 50
!
!        JOB = 0 , SOLVE  A * X = B
!        FIRST SOLVE  L*Y = B
!
   IF (NM1 .GT. 1) then
      DO K = 1, NM1
         L = IPVT(K)
         TR = BR(L)
         TI = BI(L)
         IF (L .NE. K) then
            BR(L) = BR(K)
            BI(L) = BI(K)
            BR(K) = TR
            BI(K) = TI
         endif
         CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
      enddo
   endif
!
!        NOW SOLVE  U*X = Y
!
   DO KB = 1, N
      K = N + 1 - KB
      CALL mat_wdiv(BR(K),BI(K),AR(K,K),AI(K,K),BR(K),BI(K))
      TR = -BR(K)
      TI = -BI(K)
      CALL matX_waxpy(K-1,TR,TI,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
   enddo
   GOTO 100
50 CONTINUE
!
!  JOB = NONZERO, SOLVE  CTRANS(A) * X = B
!  FIRST SOLVE  CTRANS(U)*Y = B
!
   DO K = 1, N
      TR = BR(K) - mat_wdotcr(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
      TI = BI(K) - mat_wdotci(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
      CALL mat_wdiv(TR,TI,AR(K,K),-AI(K,K),BR(K),BI(K))
   enddo
!
!        NOW SOLVE CTRANS(L)*X = Y
!
   IF (NM1 .GE. 1) then
      DO KB = 1, NM1
         K = N - KB
         BR(K) = BR(K) + mat_wdotcr(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
         BI(K) = BI(K) + mat_wdotci(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
         L = IPVT(K)
         IF (L .EQ. K) cycle
         TR = BR(L)
         TI = BI(L)
         BR(L) = BR(K)
         BI(L) = BI(K)
         BR(K) = TR
         BI(K) = TI
      enddo
   endif
100 continue
end subroutine ml_wgesl
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGEDI(ar,ai,LDA,N,ipvt,detr,deti,workr,worki,JOB)
      use M_LA
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*),DETR(2),DETI(2),WORKR(*),WORKI(*)
!
!     WGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX
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
!        WORK    DOUBLE-COMPLEX(N)
!                WORK VECTOR. CONTENTS DESTROYED.
!
!        JOB     INTEGER
!                = 11   BOTH DETERMINANT AND INVERSE.
!                = 01   INVERSE ONLY.
!                = 10   DETERMINANT ONLY.
!
!     ON RETURN
!
!        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE UNCHANGED.
!
!        DET     DOUBLE-COMPLEX(2)
!                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE NOT REFERENCED.
!                DETERMINANT = DET(1) * 10.0**DET(2)
!                WITH  1.0 .LE. CABS1(DET(1) .LT. 10.0
!                OR  DET(1) .EQ. 0.0 .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
!        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
!        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
!        AND IF WGECO HAS SET RCOND .GT. 0.0 OR WGEFA HAS SET
!        INFO .EQ. 0 .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,mat_wscal,mat_wswap
!     FORTRAN DABS,MOD
!
!     INTERNAL VARIABLES
!
      DOUBLEPRECISION TR,TI
      DOUBLEPRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE DETERMINANT
!
      IF (JOB/10 .EQ. 0) GOTO 80
         DETR(1) = 1.0D0
         DETI(1) = 0.0D0
         DETR(2) = 0.0D0
         DETI(2) = 0.0D0
         TEN = 10.0D0
         DO 60 I = 1, N
           IF (IPVT(I) .EQ. I) GOTO 10
              DETR(1) = -DETR(1)
              DETI(1) = -DETI(1)
   10      CONTINUE
           CALL mat_wmul(AR(I,I),AI(I,I),DETR(1),DETI(1),DETR(1),DETI(1))
!          ...EXIT
!       ...EXIT
           IF (CABS1(DETR(1),DETI(1)) .EQ. 0.0D0) GOTO 70
   20      IF (CABS1(DETR(1),DETI(1)) .GE. 1.0D0) GOTO 30
              DETR(1) = TEN*DETR(1)
              DETI(1) = TEN*DETI(1)
              DETR(2) = DETR(2) - 1.0D0
              DETI(2) = DETI(2) - 0.0D0
           GOTO 20
   30      CONTINUE
   40      IF (CABS1(DETR(1),DETI(1)) .LT. TEN) GOTO 50
              DETR(1) = DETR(1)/TEN
              DETI(1) = DETI(1)/TEN
              DETR(2) = DETR(2) + 1.0D0
              DETI(2) = DETI(2) + 0.0D0
           GOTO 40
   50      CONTINUE
   60    CONTINUE
   70    CONTINUE
   80 CONTINUE
!
!     COMPUTE INVERSE(U)
!
      IF (MOD(JOB,10) .EQ. 0) GOTO 160
         DO K = 1, N
            CALL mat_wdiv(1.0D0,0.0D0,AR(K,K),AI(K,K),AR(K,K),AI(K,K))
            TR = -AR(K,K)
            TI = -AI(K,K)
            CALL mat_wscal(K-1,TR,TI,AR(1,K),AI(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) cycle
            DO J = KP1, N
              TR = AR(K,J)
              TI = AI(K,J)
              AR(K,J) = 0.0D0
              AI(K,J) = 0.0D0
              CALL matX_waxpy(K,TR,TI,AR(1,K),AI(1,K),1,AR(1,J),AI(1,J),1)
            enddo
         enddo
!
!        FORM INVERSE(U)*INVERSE(L)
!
         NM1 = N - 1
         IF (NM1 .LT. 1) GOTO 150
         DO KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO I = KP1, N
               WORKR(I) = AR(I,K)
               WORKI(I) = AI(I,K)
               AR(I,K) = 0.0D0
               AI(I,K) = 0.0D0
            enddo
            DO J = KP1, N
              TR = WORKR(J)
              TI = WORKI(J)
              CALL matX_waxpy(N,TR,TI,AR(1,J),AI(1,J),1,AR(1,K),AI(1,K),1)
            enddo
            L = IPVT(K)
            IF (L .NE. K)CALL mat_wswap(N,AR(1,K),AI(1,K),1,AR(1,L),AI(1,L),1)
         enddo
  150    CONTINUE
  160 CONTINUE
      END SUBROUTINE ML_WGEDI
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_HTRIDI(NM,N,AR,AI,D,E,E2,TAU)
      use M_LA
!
      INTEGER I,J,K,L,N,II,NM,JP1
      DOUBLEPRECISION AR(NM,N),AI(NM,N),D(N),E(N),E2(N),TAU(2,N)
      DOUBLEPRECISION F,G,H,FI,GI,HH,SI,SCALE
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
      TAU(1,N) = 1.0D0
      TAU(2,N) = 0.0D0
!
      DO I = 1, N
         D(I) = AR(I,I)
      enddo
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0D0
         SCALE = 0.0D0
         IF (L .LT. 1) GOTO 130
!     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO K = 1, L
            SCALE = mat_flop(SCALE + DABS(AR(I,K)) + DABS(AI(I,K)))
         enddo
!
         IF (SCALE .NE. 0.0D0) GOTO 140
         TAU(1,L) = 1.0D0
         TAU(2,L) = 0.0D0
  130    E(I) = 0.0D0
         E2(I) = 0.0D0
         GOTO 290
!
  140    continue
         DO K = 1, L
            AR(I,K) = mat_flop(AR(I,K)/SCALE)
            AI(I,K) = mat_flop(AI(I,K)/SCALE)
            H = mat_flop(H + AR(I,K)*AR(I,K) + AI(I,K)*AI(I,K))
         enddo
!
         E2(I) = mat_flop(SCALE*SCALE*H)
         G = mat_flop(DSQRT(H))
         E(I) = mat_flop(SCALE*G)
         F = mat_pythag(AR(I,L),AI(I,L))
!     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T ..........
         IF (F .EQ. 0.0D0) GOTO 160
         TAU(1,L) = mat_flop((AI(I,L)*TAU(2,I) - AR(I,L)*TAU(1,I))/F)
         SI = mat_flop((AR(I,L)*TAU(2,I) + AI(I,L)*TAU(1,I))/F)
         H = mat_flop(H + F*G)
         G = mat_flop(1.0D0 + G/F)
         AR(I,L) = mat_flop(G*AR(I,L))
         AI(I,L) = mat_flop(G*AI(I,L))
         IF (L .EQ. 1) GOTO 270
         GOTO 170
  160    TAU(1,L) = -TAU(1,I)
         SI = TAU(2,I)
         AR(I,L) = G
  170    F = 0.0D0
!
         DO J = 1, L
            G = 0.0D0
            GI = 0.0D0
!     .......... FORM ELEMENT OF A*U ..........
            DO K = 1, J
               G = mat_flop(G + AR(J,K)*AR(I,K) + AI(J,K)*AI(I,K))
               GI = mat_flop(GI - AR(J,K)*AI(I,K) + AI(J,K)*AR(I,K))
            enddo
!
            JP1 = J + 1
            IF (L .LT. JP1) GOTO 220
!
            DO K = JP1, L
               G = mat_flop(G + AR(K,J)*AR(I,K) - AI(K,J)*AI(I,K))
               GI = mat_flop(GI - AR(K,J)*AI(I,K) - AI(K,J)*AR(I,K))
            enddo
!     .......... FORM ELEMENT OF P ..........
  220       continue
            E(J) = mat_flop(G/H)
            TAU(2,J) = mat_flop(GI/H)
            F = mat_flop(F + E(J)*AR(I,J) - TAU(2,J)*AI(I,J))
         enddo
!
         HH = mat_flop(F/(H + H))
!     .......... FORM REDUCED A ..........
         DO J = 1, L
            F = AR(I,J)
            G = mat_flop(E(J) - HH*F)
            E(J) = G
            FI = -AI(I,J)
            GI = mat_flop(TAU(2,J) - HH*FI)
            TAU(2,J) = -GI
!
            DO K = 1, J
               AR(J,K) = mat_flop(AR(J,K) - F*E(K) - G*AR(I,K) + FI*TAU(2,K) + GI*AI(I,K))
               AI(J,K) = mat_flop(AI(J,K) - F*TAU(2,K) - G*AI(I,K) - FI*E(K) - GI*AR(I,K))
            enddo
         enddo
!
  270    continue
         DO K = 1, L
            AR(I,K) = mat_flop(SCALE*AR(I,K))
            AI(I,K) = mat_flop(SCALE*AI(I,K))
         enddo
!
         TAU(2,L) = -SI
  290    HH = D(I)
         D(I) = AR(I,I)
         AR(I,I) = HH
         AI(I,I) = mat_flop(SCALE*DSQRT(H))
  300 CONTINUE
!
END SUBROUTINE ML_HTRIDI
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_HTRIBK(NM,N,AR,AI,TAU,M,ZR,ZI)
use M_LA
!
INTEGER I,J,K,L,M,N,NM
DOUBLEPRECISION AR(NM,N),AI(NM,N),TAU(2,N),ZR(NM,M),ZI(NM,M)
DOUBLEPRECISION H,S,SI
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
   IF (M .EQ. 0) GOTO 200
!     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC
!                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN
!                TRIDIAGONAL MATRIX. ..........
   DO K = 1, N
      DO J = 1, M
         ZI(K,J) = mat_flop(-(ZR(K,J)*TAU(2,K)))
         ZR(K,J) = mat_flop(ZR(K,J)*TAU(1,K))
      enddo
   enddo
!
   IF (N .EQ. 1) GOTO 200
!     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES ..........
   DO I = 2, N
      L = I - 1
      H = AI(I,I)
      IF (H .EQ. 0.0D0) exit
      DO J = 1, M
         S = 0.0D0
         SI = 0.0D0
         DO K = 1, L
            S = mat_flop(S + AR(I,K)*ZR(K,J) - AI(I,K)*ZI(K,J))
            SI = mat_flop(SI + AR(I,K)*ZI(K,J) + AI(I,K)*ZR(K,J))
         enddo
!     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW ..........
         S = mat_flop((S/H)/H)
         SI = mat_flop((SI/H)/H)
         DO K = 1, L
            ZR(K,J) = mat_flop(ZR(K,J) - S*AR(I,K) - SI*AI(I,K))
            ZI(K,J) = mat_flop(ZI(K,J) - SI*AR(I,K) + S*AI(I,K))
         enddo
      enddo
   enddo
!
200 continue
END SUBROUTINE ML_HTRIBK
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_IMTQL2(NM,N,D,E,Z,IERR,JOB)
      use M_LA
      IMPLICIT NONE
      INTEGER I,J,K,L,M,N,II,NM,MML,IERR
      integer :: job
      DOUBLEPRECISION D(N),E(N),Z(NM,N)
      DOUBLEPRECISION B,C,F,G,P,R,S
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
      IERR = 0
      IF (N .EQ. 1) GOTO 1001
!
      DO I = 2, N
         E(I-1) = E(I)
      enddo
!
      E(N) = 0.0D0
!
      DO 240 L = 1, N
         J = 0
!     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    continue
         DO M = L, N
            IF (M .EQ. N) GOTO 120
!*****
            P = mat_flop(DABS(D(M)) + DABS(D(M+1)))
            S = mat_flop(P + DABS(E(M)))
            IF (P .EQ. S) GOTO 120
!*****
         enddo
!
  120    continue
         P = D(L)
         IF (M .EQ. L) GOTO 240
         IF (J .EQ. 30) GOTO 1000
         J = J + 1
!     .......... FORM SHIFT ..........
         G = mat_flop((D(L+1) - P)/(2.0D0*E(L)))
         R = mat_flop(DSQRT(G*G+1.0D0))
         G = mat_flop(D(M) - P + E(L)/(G + DSIGN(R,G)))
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
         MML = M - L
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = mat_flop(S*E(I))
            B = mat_flop(C*E(I))
            IF (DABS(F) .LT. DABS(G)) GOTO 150
            C = mat_flop(G/F)
            R = mat_flop(DSQRT(C*C+1.0D0))
            E(I+1) = mat_flop(F*R)
            S = mat_flop(1.0D0/R)
            C = mat_flop(C*S)
            GOTO 160
  150       S = mat_flop(F/G)
            R = mat_flop(DSQRT(S*S+1.0D0))
            E(I+1) = mat_flop(G*R)
            C = mat_flop(1.0D0/R)
            S = mat_flop(S*C)
  160       G = mat_flop(D(I+1) - P)
            R = mat_flop((D(I) - G)*S + 2.0D0*C*B)
            P = mat_flop(S*R)
            D(I+1) = G + P
            G = mat_flop(C*R - B)
            IF (JOB .EQ. 0) GOTO 185
!     .......... FORM VECTOR ..........
            DO K = 1, N
               F = Z(K,I+1)
               Z(K,I+1) = mat_flop(S*Z(K,I) + C*F)
               Z(K,I) = mat_flop(C*Z(K,I) - S*F)
            enddo
  185       CONTINUE
!
  200    CONTINUE
!
         D(L) = mat_flop(D(L) - P)
         E(L) = G
         E(M) = 0.0D0
         GOTO 105
  240 CONTINUE
!     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO II = 2, N
         I = II - 1
         K = I
         P = D(I)
!
         DO J = II, N
            IF (D(J) .GE. P) exit
            K = J
            P = D(J)
         enddo
!
         IF (K .EQ. I) exit
         D(K) = D(I)
         D(I) = P
!
         IF (JOB .EQ. 0) cycle
         DO J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
         enddo
      enddo
!
      GOTO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 CONTINUE
      IERR = L
 1001 CONTINUE
      RETURN
      END SUBROUTINE ML_IMTQL2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_CORTH(NM,N,LOW,IGH,AR,AI,ORTR,ORTI)
use M_LA
!
INTEGER I,J,M,N,II,JJ,LA,MP,NM,IGH,KP1,LOW
DOUBLEPRECISION AR(NM,N),AI(NM,N),ORTR(IGH),ORTI(IGH)
DOUBLEPRECISION F,G,H,FI,FR,SCALE
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
   LA = IGH - 1
   KP1 = LOW + 1
   IF (LA .LT. KP1) GOTO 200
!
   DO M = KP1, LA
      H = 0.0D0
      ORTR(M) = 0.0D0
      ORTI(M) = 0.0D0
      SCALE = 0.0D0
!     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
      DO I = M, IGH
         SCALE = mat_flop(SCALE + DABS(AR(I,M-1)) + DABS(AI(I,M-1)))
      enddo
!
      IF (SCALE .EQ. 0.0D0) cycle
      MP = M + IGH
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
      DO II = M, IGH
         I = MP - II
         ORTR(I) = mat_flop(AR(I,M-1)/SCALE)
         ORTI(I) = mat_flop(AI(I,M-1)/SCALE)
         H = mat_flop(H + ORTR(I)*ORTR(I) + ORTI(I)*ORTI(I))
      enddo
!
      G = mat_flop(DSQRT(H))
      F = mat_pythag(ORTR(M),ORTI(M))
      IF (F .EQ. 0.0D0) GOTO 103
      H = mat_flop(H + F*G)
      G = mat_flop(G/F)
      ORTR(M) = mat_flop((1.0D0 + G)*ORTR(M))
      ORTI(M) = mat_flop((1.0D0 + G)*ORTI(M))
      GOTO 105
!
103   continue
      ORTR(M) = G
      AR(M,M-1) = SCALE
!     .......... FORM (I-(U*UT)/H)*A ..........
105   continue
      DO J = M, N
         FR = 0.0D0
         FI = 0.0D0
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
         DO II = M, IGH
            I = MP - II
            FR = mat_flop(FR + ORTR(I)*AR(I,J) + ORTI(I)*AI(I,J))
            FI = mat_flop(FI + ORTR(I)*AI(I,J) - ORTI(I)*AR(I,J))
         enddo
!
         FR = mat_flop(FR/H)
         FI = mat_flop(FI/H)
!
         DO I = M, IGH
            AR(I,J) = mat_flop(AR(I,J) - FR*ORTR(I) + FI*ORTI(I))
            AI(I,J) = mat_flop(AI(I,J) - FR*ORTI(I) - FI*ORTR(I))
         enddo
!
      enddo
!     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) ..........
      DO I = 1, IGH
         FR = 0.0D0
         FI = 0.0D0
!     .......... FOR J=IGH STEP -1 UNTIL M DO -- ..........
         DO JJ = M, IGH
            J = MP - JJ
            FR = mat_flop(FR + ORTR(J)*AR(I,J) - ORTI(J)*AI(I,J))
            FI = mat_flop(FI + ORTR(J)*AI(I,J) + ORTI(J)*AR(I,J))
         enddo
!
         FR = mat_flop(FR/H)
         FI = mat_flop(FI/H)
!
         DO J = M, IGH
            AR(I,J) = mat_flop(AR(I,J) - FR*ORTR(J) - FI*ORTI(J))
            AI(I,J) = mat_flop(AI(I,J) + FR*ORTI(J) - FI*ORTR(J))
         enddo
!
      enddo
!
      ORTR(M) = mat_flop(SCALE*ORTR(M))
      ORTI(M) = mat_flop(SCALE*ORTI(M))
      AR(M,M-1) = mat_flop(-(G*AR(M,M-1)))
      AI(M,M-1) = mat_flop(-(G*AI(M,M-1)))
   enddo
!
200 continue
END SUBROUTINE ML_CORTH
subroutine ml_comqr3(nm,n,low,igh,ortr,orti,hr,hi,wr,wi,zr,zi,ierr ,job)
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
use M_LA
integer i,j,k,l,m,n,en,ii,ll,nm,nn,igh,ip1,itn,its,low,lp1,enm1,iend,ierr
doubleprecision hr(nm,n),hi(nm,n),wr(n),wi(n),zr(nm,n),zi(nm,n),ortr(igh),orti(igh)
doubleprecision si,sr,ti,tr,xi,xr,yi,yr,zzi,zzr,norm
integer :: job
integer :: jj
   ierr = 0
   !*****
   if (job .eq. 0) goto 150
   !*****
!     .......... INITIALIZE EIGENVECTOR MATRIX ..........
   do i = 1, n
      do j = 1, n
         zr(i,j) = 0.0d0
         zi(i,j) = 0.0d0
         if (i .eq. j) zr(i,j) = 1.0d0
      enddo
   enddo
!     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS
!                FROM THE INFORMATION LEFT BY CORTH ..........
   iend = igh - low - 1
   if (iend) 180, 150, 105
!     .......... for i=igh-1 step -1 until low+1 do -- ..........
105 continue
   do ii = 1, iend
      i = igh - ii
      if (ortr(i) .eq. 0.0d0 .and. orti(i) .eq. 0.0d0) cycle
      if (hr(i,i-1) .eq. 0.0d0 .and. hi(i,i-1) .eq. 0.0d0) cycle
!     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH ..........
      norm = mat_flop(hr(i,i-1)*ortr(i) + hi(i,i-1)*orti(i))
      ip1 = i + 1

      do k = ip1, igh
         ortr(k) = hr(k,i-1)
         orti(k) = hi(k,i-1)
      enddo

      do j = i, igh
         sr = 0.0d0
         si = 0.0d0

         do k = i, igh
            sr = mat_flop(sr + ortr(k)*zr(k,j) + orti(k)*zi(k,j))
            si = mat_flop(si + ortr(k)*zi(k,j) - orti(k)*zr(k,j))
         enddo

         sr = mat_flop(sr/norm)
         si = mat_flop(si/norm)

         do k = i, igh
            zr(k,j) = mat_flop(zr(k,j) + sr*ortr(k) - si*orti(k))
            zi(k,j) = mat_flop(zi(k,j) + sr*orti(k) + si*ortr(k))
         enddo

      enddo

   enddo
   !*****
   if (job .eq. 3) goto 1001
   !*****
!     .......... CREATE REAL SUBDIAGONAL ELEMENTS ..........
150 continue
   l = low + 1

   do i = l, igh
      ll = min0(i+1,igh)
      if (hi(i,i-1) .eq. 0.0d0) cycle
      norm = mat_pythag(hr(i,i-1),hi(i,i-1))
      yr = mat_flop(hr(i,i-1)/norm)
      yi = mat_flop(hi(i,i-1)/norm)
      hr(i,i-1) = norm
      hi(i,i-1) = 0.0d0

      do j = i, n
         si = mat_flop(yr*hi(i,j) - yi*hr(i,j))
         hr(i,j) = mat_flop(yr*hr(i,j) + yi*hi(i,j))
         hi(i,j) = si
      enddo

      do j = 1, ll
         si = mat_flop(yr*hi(j,i) + yi*hr(j,i))
         hr(j,i) = mat_flop(yr*hr(j,i) - yi*hi(j,i))
         hi(j,i) = si
      enddo
      !*****
      if (job .eq. 0) cycle
      !*****
      do j = low, igh
         si = mat_flop(yr*zi(j,i) + yi*zr(j,i))
         zr(j,i) = mat_flop(yr*zr(j,i) - yi*zi(j,i))
         zi(j,i) = si
      enddo

   enddo
!     .......... STORE ROOTS ISOLATED BY CBAL ..........
180 continue
   do i = 1, n
      if (i .ge. low .and. i .le. igh) cycle
      wr(i) = hr(i,i)
      wi(i) = hi(i,i)
   enddo

   en = igh
   tr = 0.0d0
   ti = 0.0d0
   itn = 30*n
!     .......... SEARCH FOR NEXT EIGENVALUE ..........
220 continue
   if (en .lt. low) goto 680
   its = 0
   enm1 = en - 1
!     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
!                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
240 continue
   do ll = low, en
      l = en + low - ll
      if (l .eq. low) exit
      !*****
      xr = mat_flop(dabs(hr(l-1,l-1)) + dabs(hi(l-1,l-1)) + dabs(hr(l,l)) +dabs(hi(l,l)))
      yr = mat_flop(xr + dabs(hr(l,l-1)))
      if (xr .eq. yr) exit
      !*****
   enddo
!     .......... FORM SHIFT ..........
   if (l .eq. en) goto 660
   if (itn .eq. 0) goto 1000
   if (its .eq. 10 .or. its .eq. 20) goto 320
   sr = hr(en,en)
   si = hi(en,en)
   xr = mat_flop(hr(enm1,en)*hr(en,enm1))
   xi = mat_flop(hi(enm1,en)*hr(en,enm1))
   if (xr .eq. 0.0d0 .and. xi .eq. 0.0d0) goto 340
   yr = mat_flop((hr(enm1,enm1) - sr)/2.0d0)
   yi = mat_flop((hi(enm1,enm1) - si)/2.0d0)
   call mat_wsqrt(yr**2-yi**2+xr,2.0d0*yr*yi+xi,zzr,zzi)
   if (yr*zzr + yi*zzi .ge. 0.0d0) goto 310
   zzr = -zzr
   zzi = -zzi
310 continue
   call mat_wdiv(xr,xi,yr+zzr,yi+zzi,zzr,zzi)
   sr = mat_flop(sr - zzr)
   si = mat_flop(si - zzi)
   goto 340
!     .......... FORM EXCEPTIONAL SHIFT ..........
320 continue
   sr = mat_flop(dabs(hr(en,enm1)) + dabs(hr(enm1,en-2)))
   si = 0.0d0

340 continue
   do i = low, en
      hr(i,i) = mat_flop(hr(i,i) - sr)
      hi(i,i) = mat_flop(hi(i,i) - si)
   enddo

   tr = mat_flop(tr + sr)
   ti = mat_flop(ti + si)
   its = its + 1
   itn = itn - 1
!     .......... REDUCE TO TRIANGLE (ROWS) ..........
   lp1 = l + 1

   do i = lp1, en
      sr = hr(i,i-1)
      hr(i,i-1) = 0.0d0
      norm= mat_flop(dabs(hr(i-1,i-1)) + dabs(hi(i-1,i-1)) + dabs(sr))
      norm= mat_flop(norm*dsqrt((hr(i-1,i-1)/norm)**2 + (hi(i-1,i-1)/norm)**2 + (sr/norm)**2))
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
         hr(i-1,j) = mat_flop(xr*yr + xi*yi + hi(i,i-1)*zzr)
         hi(i-1,j) = mat_flop(xr*yi - xi*yr + hi(i,i-1)*zzi)
         hr(i,j) = mat_flop(xr*zzr - xi*zzi - hi(i,i-1)*yr)
         hi(i,j) = mat_flop(xr*zzi + xi*zzr - hi(i,i-1)*yi)
      enddo

   enddo

   si = hi(en,en)
   if (si .eq. 0.0d0) goto 540
   norm = mat_pythag(hr(en,en),si)
   sr = mat_flop(hr(en,en)/norm)
   si = mat_flop(si/norm)
   hr(en,en) = norm
   hi(en,en) = 0.0d0
   if (en .eq. n) goto 540
   ip1 = en + 1

   do j = ip1, n
      yr = hr(en,j)
      yi = hi(en,j)
      hr(en,j) = mat_flop(sr*yr + si*yi)
      hi(en,j) = mat_flop(sr*yi - si*yr)
   enddo
!     .......... INVERSE OPERATION (COLUMNS) ..........
540 continue
   do j = lp1, en
      xr = wr(j-1)
      xi = wi(j-1)

      do i = 1, j
         yr = hr(i,j-1)
         yi = 0.0d0
         zzr = hr(i,j)
         zzi = hi(i,j)
         if (i .eq. j) goto 560
         yi = hi(i,j-1)
         hi(i,j-1) = mat_flop(xr*yi + xi*yr + hi(j,j-1)*zzi)
560      continue
         hr(i,j-1) = mat_flop(xr*yr - xi*yi + hi(j,j-1)*zzr)
         hr(i,j) = mat_flop(xr*zzr + xi*zzi - hi(j,j-1)*yr)
         hi(i,j) = mat_flop(xr*zzi - xi*zzr - hi(j,j-1)*yi)
      enddo
!*****
      if (job .eq. 0) cycle
!*****
      do i = low, igh
         yr = zr(i,j-1)
         yi = zi(i,j-1)
         zzr = zr(i,j)
         zzi = zi(i,j)
         zr(i,j-1) = mat_flop(xr*yr - xi*yi + hi(j,j-1)*zzr)
         zi(i,j-1) = mat_flop(xr*yi + xi*yr + hi(j,j-1)*zzi)
         zr(i,j) = mat_flop(xr*zzr + xi*zzi - hi(j,j-1)*yr)
         zi(i,j) = mat_flop(xr*zzi - xi*zzr - hi(j,j-1)*yi)
      enddo

   enddo

   if (si .eq. 0.0d0) goto 240

   do i = 1, en
      yr = hr(i,en)
      yi = hi(i,en)
      hr(i,en) = mat_flop(sr*yr - si*yi)
      hi(i,en) = mat_flop(sr*yi + si*yr)
   enddo
!*****
   if (job .eq. 0) goto 240
!*****
   do i = low, igh
      yr = zr(i,en)
      yi = zi(i,en)
      zr(i,en) = mat_flop(sr*yr - si*yi)
      zi(i,en) = mat_flop(sr*yi + si*yr)
   enddo

   goto 240
!     .......... A ROOT FOUND ..........
660 continue
   hr(en,en) = mat_flop(hr(en,en) + tr)
   wr(en) = hr(en,en)
   hi(en,en) = mat_flop(hi(en,en) + ti)
   wi(en) = hi(en,en)
   en = enm1
   goto 220
!     .......... ALL ROOTS FOUND. BACKSUBSTITUTE TO FIND
!                VECTORS OF UPPER TRIANGULAR FORM ..........
!
!*****  THE FOLLOWING SECTION CHANGED FOR OVERFLOW CONTROL
!       C. MOLER, 3/16/82
!
680 continue
   if (job .ne. 2) goto 1001

   norm = 0.0d0
   do i = 1, n
      do j = i, n
         tr = mat_flop(dabs(hr(i,j))) + mat_flop(dabs(hi(i,j)))
         if (tr .gt. norm) norm = tr
      enddo
   enddo
   if (n .eq. 1 .or. norm .eq. 0.0d0) goto 1001
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
            zzr = mat_flop(zzr + hr(i,j)*hr(j,en) - hi(i,j)*hi(j,en))
            zzi = mat_flop(zzi + hr(i,j)*hi(j,en) + hi(i,j)*hr(j,en))
         enddo
         yr = mat_flop(xr - wr(i))
         yi = mat_flop(xi - wi(i))
         if (yr .ne. 0.0d0 .or. yi .ne. 0.0d0) goto 765
         yr = norm
760      continue
         yr = mat_flop(yr/100.0d0)
         yi = mat_flop(norm + yr)
         if (yi .ne. norm) goto 760
         yi = 0.0d0
765      continue
         call mat_wdiv(zzr,zzi,yr,yi,hr(i,en),hi(i,en))
         tr = mat_flop(dabs(hr(i,en))) + mat_flop(dabs(hi(i,en)))
         if (tr .eq. 0.0d0) cycle
         if (tr + 1.0d0/tr .gt. tr)cycle
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
   do  i = 1, enm1
      if (i .ge. low .and. i .le. igh) cycle
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
            zzr = mat_flop(zzr + zr(i,k)*hr(k,j) - zi(i,k)*hi(k,j))
            zzi = mat_flop(zzi + zr(i,k)*hi(k,j) + zi(i,k)*hr(k,j))
         enddo

         zr(i,j) = zzr
         zi(i,j) = zzi
      enddo
   enddo
!
   goto 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
1000 continue
   ierr = en
1001 continue
end subroutine ml_comqr3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WSVDC(xr,xi,LDX,N,P,sr,si,er,ei,ur,ui,LDU,vr,vi,LDV,workr,worki,JOB,INFO)
      use M_LA
      INTEGER LDX,N,P,LDU,LDV,JOB,INFO
      DOUBLEPRECISION XR(LDX,*),XI(LDX,*),SR(*),SI(*),ER(*),EI(*), UR(LDU,*),UI(LDU,*),VR(LDV,*),VI(LDV,*), WORKR(*),WORKI(*)
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
!                   THEN V MAY BE IDENTIFIED WHTH X IN THE
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
      INTEGER I,ITER,J,JOBU,K,KASE,KK,L,LL,LLS,LM1,LP1,LS,LU,M,MAXIT,MM,MM1,MP1,NCT,NCTP1,NCU,NRT,NRTP1
      DOUBLEPRECISION TR,TI,RR,RI
      DOUBLEPRECISION B,C,CS,EL,EMM1,F,G,SCALE,SHIFT,SL,SM,SN,SMM1,T1,TEST,ZTEST,SMALL
      LOGICAL WANTU,WANTV
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET THE MAXIMUM NUMBER OF ITERATIONS.
!
      MAXIT = 75
!
!     SMALL NUMBER, ROUGHLY MACHINE EPSILON, USED TO AVOID UNDERFLOW
!
      SMALL = 1.D0/2.D0**48
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
      WANTU = .FALSE.
      WANTV = .FALSE.
      JOBU = MOD(JOB,100)/10
      NCU = N
      IF (JOBU .GT. 1) NCU = MIN0(N,P)
      IF (JOBU .NE. 0) WANTU = .TRUE.
      IF (MOD(JOB,10) .NE. 0) WANTV = .TRUE.
!
!     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS
!     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.
!
      INFO = 0
      NCT = MIN0(N-1,P)
      NRT = MAX0(0,MIN0(P-2,N))
      LU = MAX0(NCT,NRT)
      IF (LU .LT. 1) GOTO 190
      DO 180 L = 1, LU
         LP1 = L + 1
         IF (L .GT. NCT) GOTO 30
!
!           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND
!           PLACE THE L-TH DIAGONAL IN S(L).
!
            SR(L) = mat_wnrm2(N-L+1,XR(L,L),XI(L,L),1)
            SI(L) = 0.0D0
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 20
               IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 10
                  CALL mat_wsign(SR(L),SI(L),XR(L,L),XI(L,L),SR(L),SI(L))
   10          CONTINUE
               CALL mat_wdiv(1.0D0,0.0D0,SR(L),SI(L),TR,TI)
               CALL mat_wscal(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
               XR(L,L) = mat_flop(1.0D0 + XR(L,L))
   20       CONTINUE
            SR(L) = -SR(L)
            SI(L) = -SI(L)
   30    CONTINUE
         IF (P .LT. LP1) GOTO 60
         DO 50 J = LP1, P
            IF (L .GT. NCT) GOTO 40
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 40
!
!              APPLY THE TRANSFORMATION.
!
               TR= -mat_wdotcr(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               TI= -mat_wdotci(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               CALL mat_wdiv(TR,TI,XR(L,L),XI(L,L),TR,TI)
               CALL matX_waxpy(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
   40       CONTINUE
!
!           PLACE THE L-TH ROW OF X INTO  E FOR THE
!           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION.
!
            ER(J) = XR(L,J)
            EI(J) = -XI(L,J)
   50    CONTINUE
   60    CONTINUE
         IF (.NOT.WANTU .OR. L .GT. NCT) GOTO 80
!
!           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK
!           MULTIPLICATION.
!
            DO I = L, N
               UR(I,L) = XR(I,L)
               UI(I,L) = XI(I,L)
            enddo
   80    CONTINUE
         IF (L .GT. NRT) GOTO 170
!
!           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE
!           L-TH SUPER-DIAGONAL IN E(L).
!
            ER(L) = mat_wnrm2(P-L,ER(LP1),EI(LP1),1)
            EI(L) = 0.0D0
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 100
               IF (CABS1(ER(LP1),EI(LP1)) .EQ. 0.0D0) GOTO 90
                  CALL mat_wsign(ER(L),EI(L),ER(LP1),EI(LP1),ER(L),EI(L))
   90          CONTINUE
               CALL mat_wdiv(1.0D0,0.0D0,ER(L),EI(L),TR,TI)
               CALL mat_wscal(P-L,TR,TI,ER(LP1),EI(LP1),1)
               ER(LP1) = mat_flop(1.0D0 + ER(LP1))
  100       CONTINUE
            ER(L) = -ER(L)
            EI(L) = +EI(L)
            IF (LP1 .GT. N .OR. CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 140
!
!              APPLY THE TRANSFORMATION.
!
               DO I = LP1, N
                  WORKR(I) = 0.0D0
                  WORKI(I) = 0.0D0
               enddo
               DO J = LP1, P
                  CALL matX_waxpy(N-L,ER(J),EI(J),XR(LP1,J),XI(LP1,J),1, WORKR(LP1),WORKI(LP1),1)
               enddo
               DO J = LP1, P
                  CALL mat_wdiv(-ER(J),-EI(J),ER(LP1),EI(LP1),TR,TI)
                  CALL matX_waxpy(N-L,TR,-TI,WORKR(LP1),WORKI(LP1),1, XR(LP1,J),XI(LP1,J),1)
               enddo
  140       CONTINUE
            IF (.NOT.WANTV) GOTO 160
!
!              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT
!              BACK MULTIPLICATION.
!
               DO I = LP1, P
                  VR(I,L) = ER(I)
                  VI(I,L) = EI(I)
               enddo
  160       CONTINUE
  170    CONTINUE
  180 CONTINUE
  190 CONTINUE
!
!     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.
!
      M = MIN0(P,N+1)
      NCTP1 = NCT + 1
      NRTP1 = NRT + 1
      IF (NCT .GE. P) GOTO 200
         SR(NCTP1) = XR(NCTP1,NCTP1)
         SI(NCTP1) = XI(NCTP1,NCTP1)
  200 CONTINUE
      IF (N .GE. M) GOTO 210
         SR(M) = 0.0D0
         SI(M) = 0.0D0
  210 CONTINUE
      IF (NRTP1 .GE. M) GOTO 220
         ER(NRTP1) = XR(NRTP1,M)
         EI(NRTP1) = XI(NRTP1,M)
  220 CONTINUE
      ER(M) = 0.0D0
      EI(M) = 0.0D0
!
!     IF REQUIRED, GENERATE U.
!
      IF (.NOT.WANTU) GOTO 350
         IF (NCU .LT. NCTP1) GOTO 250
         DO J = NCTP1, NCU
            DO I = 1, N
               UR(I,J) = 0.0D0
               UI(I,J) = 0.0D0
            enddo
            UR(J,J) = 1.0D0
            UI(J,J) = 0.0D0
         enddo
  250    CONTINUE
         IF (NCT .LT. 1) GOTO 340
         DO LL = 1, NCT
            L = NCT - LL + 1
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 300
               LP1 = L + 1
               IF (NCU .LT. LP1) GOTO 270
               DO J = LP1, NCU
                  TR = -mat_wdotcr(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  TI = -mat_wdotci(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  CALL mat_wdiv(TR,TI,UR(L,L),UI(L,L),TR,TI)
                  CALL matX_waxpy(N-L+1,TR,TI,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
               enddo
  270          CONTINUE
               CALL mat_wrscal(N-L+1,-1.0D0,UR(L,L),UI(L,L),1)
               UR(L,L) = mat_flop(1.0D0 + UR(L,L))
               LM1 = L - 1
               IF (LM1 .LT. 1) GOTO 290
               DO I = 1, LM1
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
               enddo
  290          CONTINUE
            GOTO 320
  300       CONTINUE
               DO I = 1, N
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
               enddo
               UR(L,L) = 1.0D0
               UI(L,L) = 0.0D0
  320       CONTINUE
         enddo
  340    CONTINUE
  350 CONTINUE
!
!     IF IT IS REQUIRED, GENERATE V.
!
      IF (.NOT.WANTV) GOTO 400
         DO LL = 1, P
            L = P - LL + 1
            LP1 = L + 1
            IF (L .GT. NRT) GOTO 370
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 370
               DO J = LP1, P
                  TR = -mat_wdotcr(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                  TI = -mat_wdotci(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                  CALL mat_wdiv(TR,TI,VR(LP1,L),VI(LP1,L),TR,TI)
                  CALL matX_waxpy(P-L,TR,TI,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
               enddo
  370       CONTINUE
            DO I = 1, P
               VR(I,L) = 0.0D0
               VI(I,L) = 0.0D0
            enddo
            VR(L,L) = 1.0D0
            VI(L,L) = 0.0D0
         enddo
  400 CONTINUE
!
!     TRANSFORM S AND E SO THAT THEY ARE REAL.
!
      DO I = 1, M
            TR = mat_pythag(SR(I),SI(I))
            IF (TR .EQ. 0.0D0) GOTO 405
            RR = SR(I)/TR
            RI = SI(I)/TR
            SR(I) = TR
            SI(I) = 0.0D0
            IF (I .LT. M) CALL mat_wdiv(ER(I),EI(I),RR,RI,ER(I),EI(I))
            IF (WANTU) CALL mat_wscal(N,RR,RI,UR(1,I),UI(1,I),1)
  405    CONTINUE
!     ...EXIT
         IF (I .EQ. M) exit
            TR = mat_pythag(ER(I),EI(I))
            IF (TR .EQ. 0.0D0) GOTO 410
            CALL mat_wdiv(TR,0.0D0,ER(I),EI(I),RR,RI)
            ER(I) = TR
            EI(I) = 0.0D0
            CALL mat_wmul(SR(I+1),SI(I+1),RR,RI,SR(I+1),SI(I+1))
            IF (WANTV) CALL mat_wscal(P,RR,RI,VR(1,I+1),VI(1,I+1),1)
  410    CONTINUE
      enddo
!
!     MAIN ITERATION LOOP FOR THE SINGULAR VALUES.
!
      MM = M
      ITER = 0
  440 CONTINUE
!
!        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.
!
!     ...EXIT
         IF (M .EQ. 0) GOTO 700
!
!        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET
!        FLAG AND RETURN.
!
         IF (ITER .LT. MAXIT) GOTO 450
            INFO = M
!     ......EXIT
            GOTO 700
  450    CONTINUE
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
         DO LL = 1, M
            L = M - LL
!        ...EXIT
            IF (L .EQ. 0) GOTO 480
            TEST = mat_flop(DABS(SR(L)) + DABS(SR(L+1)))
            ZTEST = mat_flop(TEST + DABS(ER(L))/2.0D0)
            IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 460
               ER(L) = 0.0D0
!        ......EXIT
               GOTO 480
  460       CONTINUE
         enddo
  480    CONTINUE
         IF (L .NE. M - 1) GOTO 490
            KASE = 4
         GOTO 560
  490    CONTINUE
            LP1 = L + 1
            MP1 = M + 1
            DO LLS = LP1, MP1
               LS = M - LLS + LP1
!           ...EXIT
               IF (LS .EQ. L) GOTO 520
               TEST = 0.0D0
               IF (LS .NE. M) TEST = mat_flop(TEST + DABS(ER(LS)))
               IF (LS .NE. L + 1) TEST = mat_flop(TEST + DABS(ER(LS-1)))
               ZTEST = mat_flop(TEST + DABS(SR(LS))/2.0D0)
               IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 500
                  SR(LS) = 0.0D0
!           ......EXIT
                  GOTO 520
  500          CONTINUE
            enddo
  520       CONTINUE
            IF (LS .NE. L) GOTO 530
               KASE = 3
            GOTO 550
  530       CONTINUE
            IF (LS .NE. M) GOTO 540
               KASE = 1
            GOTO 550
  540       CONTINUE
               KASE = 2
               L = LS
  550       CONTINUE
  560    CONTINUE
         L = L + 1
!
!        PERFORM THE TASK INDICATED BY KASE.
!
         GOTO (570, 600, 620, 650), KASE
!
!        DEFLATE NEGLIGIBLE SR(M).
!
  570    CONTINUE
            MM1 = M - 1
            F = ER(M-1)
            ER(M-1) = 0.0D0
            DO KK = L, MM1
               K = MM1 - KK + L
               T1 = SR(K)
               CALL mat_rrotg(T1,F,CS,SN)
               SR(K) = T1
               IF (K .EQ. L) GOTO 580
                  F = mat_flop(-(SN*ER(K-1)))
                  ER(K-1) = mat_flop(CS*ER(K-1))
  580          CONTINUE
               IF (WANTV) CALL mat_rrot(P,VR(1,K),1,VR(1,M),1,CS,SN)
               IF (WANTV) CALL mat_rrot(P,VI(1,K),1,VI(1,M),1,CS,SN)
            enddo
         GOTO 690
!
!        SPLIT AT NEGLIGIBLE SR(L).
!
  600    CONTINUE
            F = ER(L-1)
            ER(L-1) = 0.0D0
            DO K = L, M
               T1 = SR(K)
               CALL mat_rrotg(T1,F,CS,SN)
               SR(K) = T1
               F = mat_flop(-(SN*ER(K)))
               ER(K) = mat_flop(CS*ER(K))
               IF (WANTU) CALL mat_rrot(N,UR(1,K),1,UR(1,L-1),1,CS,SN)
               IF (WANTU) CALL mat_rrot(N,UI(1,K),1,UI(1,L-1),1,CS,SN)
            enddo
         GOTO 690
!
!        PERFORM ONE QR STEP.
!
  620    CONTINUE
!
!           CALCULATE THE SHIFT.
!
            SCALE = DMAX1(DABS(SR(M)),DABS(SR(M-1)),DABS(ER(M-1)), DABS(SR(L)),DABS(ER(L)))
            SM = SR(M)/SCALE
            SMM1 = SR(M-1)/SCALE
            EMM1 = ER(M-1)/SCALE
            SL = SR(L)/SCALE
            EL = ER(L)/SCALE
            B = mat_flop(((SMM1 + SM)*(SMM1 - SM) + EMM1**2)/2.0D0)
            C = mat_flop((SM*EMM1)**2)
            SHIFT = 0.0D0
            IF (B .EQ. 0.0D0 .AND. C .EQ. 0.0D0) GOTO 630
               SHIFT = mat_flop(DSQRT(B**2+C))
               IF (B .LT. 0.0D0) SHIFT = -SHIFT
               SHIFT = mat_flop(C/(B + SHIFT))
  630       CONTINUE
            F = mat_flop((SL + SM)*(SL - SM) - SHIFT)
            G = mat_flop(SL*EL)
!
!           CHASE ZEROS.
!
            MM1 = M - 1
            DO K = L, MM1
               CALL mat_rrotg(F,G,CS,SN)
               IF (K .NE. L) ER(K-1) = F
               F = mat_flop(CS*SR(K) + SN*ER(K))
               ER(K) = mat_flop(CS*ER(K) - SN*SR(K))
               G = mat_flop(SN*SR(K+1))
               SR(K+1) = mat_flop(CS*SR(K+1))
               IF (WANTV) CALL mat_rrot(P,VR(1,K),1,VR(1,K+1),1,CS,SN)
               IF (WANTV) CALL mat_rrot(P,VI(1,K),1,VI(1,K+1),1,CS,SN)
               CALL mat_rrotg(F,G,CS,SN)
               SR(K) = F
               F = mat_flop(CS*ER(K) + SN*SR(K+1))
               SR(K+1) = mat_flop(-(SN*ER(K)) + CS*SR(K+1))
               G = mat_flop(SN*ER(K+1))
               ER(K+1) = mat_flop(CS*ER(K+1))
               IF (WANTU .AND. K .LT. N) CALL mat_rrot(N,UR(1,K),1,UR(1,K+1),1,CS,SN)
               IF (WANTU .AND. K .LT. N) CALL mat_rrot(N,UI(1,K),1,UI(1,K+1),1,CS,SN)
            enddo
            ER(M-1) = F
            ITER = ITER + 1
         GOTO 690
!
!        CONVERGENCE
!
  650    CONTINUE
!
!           MAKE THE SINGULAR VALUE  POSITIVE
!
            IF (SR(L) .GE. 0.0D0) GOTO 660
               SR(L) = -SR(L)
             IF (WANTV) CALL mat_wrscal(P,-1.0D0,VR(1,L),VI(1,L),1)
  660       CONTINUE
!
!           ORDER THE SINGULAR VALUE.
!
  670       IF (L .EQ. MM) GOTO 680
!           ...EXIT
               IF (SR(L) .GE. SR(L+1)) GOTO 680
               TR = SR(L)
               SR(L) = SR(L+1)
               SR(L+1) = TR
               IF (WANTV .AND. L .LT. P)CALL mat_wswap(P,VR(1,L),VI(1,L),1,VR(1,L+1),VI(1,L+1),1)
               IF (WANTU .AND. L .LT. N)CALL mat_wswap(N,UR(1,L),UI(1,L),1,UR(1,L+1),UI(1,L+1),1)
               L = L + 1
            GOTO 670
  680       CONTINUE
            ITER = 0
            M = M - 1
  690    CONTINUE
      GOTO 440
  700 CONTINUE
      END SUBROUTINE ML_WSVDC
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WQRDC(XR,XI,LDX,N,P,QRAUXR,QRAUXI,JPVT,WORKR,WORKI, JOB)
      use M_LA
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      DOUBLEPRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*), WORKR(*),WORKI(*)
!
!     WQRDC USES HOUSEHOLDER TRANSFORMATIONS TO COMPUTE THE QR
!     FACTORIZATION OF AN N BY P MATRIX X. COLUMN PIVOTING
!     BASED ON THE 2-NORMS OF THE REDUCED COLUMNS MAY BE
!     PERFORMED AT THE USERS OPTION.
!
!     ON ENTRY
!
!        X       DOUBLE-COMPLEX(LDX,P), WHERE LDX .GE. N.
!                X CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO BE
!                COMPUTED.
!
!        LDX     INTEGER.
!                LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N       INTEGER.
!                N IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!        P       INTEGER.
!                P IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!        JPVT    INTEGER(P).
!                JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION
!                OF THE PIVOT COLUMNS. THE K-TH COLUMN X(K) OF X
!                IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE
!                VALUE OF JPVT(K).
!
!                   IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL
!                   COLUMN.
!
!                   IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE COLUMN.
!
!                   IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL COLUMN.
!
!                BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL COLUMNS
!                ARE MOVED TO THE BEGINNING OF THE ARRAY X AND FINAL
!                COLUMNS TO THE END. BOTH INITIAL AND FINAL COLUMNS
!                ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
!                FREE COLUMNS ARE MOVED. AT THE K-TH STAGE OF THE
!                REDUCTION, IF X(K) IS OCCUPIED BY A FREE COLUMN
!                IT IS INTERCHANGED WITH THE FREE COLUMN OF LARGEST
!                REDUCED NORM. JPVT IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        WORK    DOUBLE-COMPLEX(P).
!                WORK IS A WORK ARRAY. WORK IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        JOB     INTEGER.
!                JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
!                IF JOB .EQ. 0, NO PIVOTING IS DONE.
!                IF JOB .NE. 0, PIVOTING IS DONE.
!
!     ON RETURN
!
!        X       X CONTAINS IN ITS UPPER TRIANGLE THE UPPER
!                TRIANGULAR MATRIX R OF THE QR FACTORIZATION.
!                BELOW ITS DIAGONAL X CONTAINS INFORMATION FROM
!                WHICH THE UNITARY PART OF THE DECOMPOSITION
!                CAN BE RECOVERED. NOTE THAT IF PIVOTING HAS
!                BEEN REQUESTED, THE DECOMPOSITION IS NOT THAT
!                OF THE ORIGINAL MATRIX X BUT THAT OF X
!                WITH ITS COLUMNS PERMUTED AS DESCRIBED BY JPVT.
!
!        QRAUX   DOUBLE-COMPLEX(P).
!                QRAUX CONTAINS FURTHER INFORMATION REQUIRED TO RECOVER
!                THE UNITARY PART OF THE DECOMPOSITION.
!
!        JPVT    JPVT(K) CONTAINS THE INDEX OF THE COLUMN OF THE
!                ORIGINAL MATRIX THAT HAS BEEN INTERCHANGED INTO
!                THE K-TH COLUMN, IF PIVOTING WAS REQUESTED.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WQRDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS matX_waxpy,mat_pythag,mat_wdotcr,mat_wdotci,mat_wscal
!     blas mat_wswap ,mat_wnrm2
!     FORTRAN DABS,DIMAG,DMAX1,MIN0
!
!     INTERNAL VARIABLES
!
integer :: jj
      INTEGER J,JP,L,LP1,LUP,MAXJ,PL,PU
      DOUBLEPRECISION MAXNRM,TT
      DOUBLEPRECISION NRMXLR,NRMXLI,TR,TI
      LOGICAL NEGJ,SWAPJ
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GOTO 60
!
!        PIVOTING HAS BEEN REQUESTED. REARRANGE THE COLUMNS
!        ACCORDING TO JPVT.
!
         DO 20 J = 1, P
            SWAPJ = JPVT(J) .GT. 0
            NEGJ = JPVT(J) .LT. 0
            JPVT(J) = J
            IF (NEGJ) JPVT(J) = -J
            IF (.NOT.SWAPJ) GOTO 10
               IF (J .NE. PL) CALL mat_wswap(N,XR(1,PL),XI(1,PL),1,XR(1,J),XI(1,J),1)
               JPVT(J) = JPVT(PL)
               JPVT(PL) = J
               PL = PL + 1
   10       CONTINUE
   20    CONTINUE
         PU = P
         DO 50 JJ = 1, P
            J = P - JJ + 1
            IF (JPVT(J) .GE. 0) GOTO 40
               JPVT(J) = -JPVT(J)
               IF (J .EQ. PU) GOTO 30
                  CALL mat_wswap(N,XR(1,PU),XI(1,PU),1,XR(1,J),XI(1,J),1)
                  JP = JPVT(PU)
                  JPVT(PU) = JPVT(J)
                  JPVT(J) = JP
   30          CONTINUE
               PU = PU - 1
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
!
!     COMPUTE THE NORMS OF THE FREE COLUMNS.
!
      IF (PU .LT. PL) GOTO 80
      DO 70 J = PL, PU
         QRAUXR(J) = mat_wnrm2(N,XR(1,J),XI(1,J),1)
         QRAUXI(J) = 0.0D0
         WORKR(J) = QRAUXR(J)
         WORKI(J) = QRAUXI(J)
   70 CONTINUE
   80 CONTINUE
!
!     PERFORM THE HOUSEHOLDER REDUCTION OF X.
!
      LUP = MIN0(N,P)
      DO 210 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GOTO 120
!
!           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
!           INTO THE PIVOT POSITION.
!
            MAXNRM = 0.0D0
            MAXJ = L
            DO J = L, PU
               IF (QRAUXR(J) .LE. MAXNRM) cycle
               MAXNRM = QRAUXR(J)
               MAXJ = J
            enddo
            IF (MAXJ .EQ. L) GOTO 110
              CALL mat_wswap(N,XR(1,L),XI(1,L),1,XR(1,MAXJ),XI(1,MAXJ),1)
              QRAUXR(MAXJ) = QRAUXR(L)
              QRAUXI(MAXJ) = QRAUXI(L)
              WORKR(MAXJ) = WORKR(L)
              WORKI(MAXJ) = WORKI(L)
              JP = JPVT(MAXJ)
              JPVT(MAXJ) = JPVT(L)
              JPVT(L) = JP
  110       CONTINUE
  120    CONTINUE
         QRAUXR(L) = 0.0D0
         QRAUXI(L) = 0.0D0
         IF (L .EQ. N) GOTO 200
!
!           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
!
            NRMXLR = mat_wnrm2(N-L+1,XR(L,L),XI(L,L),1)
            NRMXLI = 0.0D0
            IF (CABS1(NRMXLR,NRMXLI) .EQ. 0.0D0) GOTO 190
              IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 130
              CALL mat_wsign(NRMXLR,NRMXLI,XR(L,L),XI(L,L),NRMXLR,NRMXLI)
  130         CONTINUE
              CALL mat_wdiv(1.0D0,0.0D0,NRMXLR,NRMXLI,TR,TI)
              CALL mat_wscal(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
              XR(L,L) = mat_flop(1.0D0 + XR(L,L))
!
!             APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
!             UPDATING THE NORMS.
!
              LP1 = L + 1
              IF (P .LT. LP1) GOTO 180
              DO 170 J = LP1, P
                  TR = -mat_wdotcr(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  TI = -mat_wdotci(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  CALL mat_wdiv(TR,TI,XR(L,L),XI(L,L),TR,TI)
                  CALL matX_waxpy(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  IF (J .LT. PL .OR. J .GT. PU) GOTO 160
                  IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 160
                    TT=1.0D0 - (mat_pythag(XR(L,J),XI(L,J))/QRAUXR(J))**2
                    TT=DMAX1(TT,0.0D0)
                    TR=mat_flop(TT)
                    TT=mat_flop(1.0D0+0.05D0*TT*(QRAUXR(J)/WORKR(J))**2)
                    IF (TT .EQ. 1.0D0) GOTO 140
                     QRAUXR(J) = QRAUXR(J)*DSQRT(TR)
                     QRAUXI(J) = QRAUXI(J)*DSQRT(TR)
                     GOTO 150
  140                CONTINUE
                     QRAUXR(J) = mat_wnrm2(N-L,XR(L+1,J),XI(L+1,J),1)
                     QRAUXI(J) = 0.0D0
                     WORKR(J) = QRAUXR(J)
                     WORKI(J) = QRAUXI(J)
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
  180          CONTINUE
!
!              SAVE THE TRANSFORMATION.
!
               QRAUXR(L) = XR(L,L)
               QRAUXI(L) = XI(L,L)
               XR(L,L) = -NRMXLR
               XI(L,L) = -NRMXLI
  190       CONTINUE
  200    CONTINUE
  210 CONTINUE
      END SUBROUTINE ML_WQRDC
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WQRSL(XR,XI,LDX,N,K,QRAUXR,QRAUXI,YR,YI,QYR,QYI,QTYR,QTYI,BR,BI,RSDR,RSDI,XBR,XBI,JOB,INFO)
use M_LA
IMPLICIT NONE
INTEGER LDX,N,K,JOB,INFO
DOUBLEPRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*),YR(*),     &
   &                YI(*),QYR(*),QYI(*),QTYR(*),QTYI(*),BR(*),BI(*),   &
   &                RSDR(*),RSDI(*),XBR(*),XBI(*)
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
   INTEGER I,J,JJ,JU,KP1
   DOUBLEPRECISION TR,TI,TEMPR,TEMPI
   LOGICAL CB,CQY,CQTY,CR,CXB
!
   DOUBLEPRECISION ZDUMR,ZDUMI
   DOUBLEPRECISION CABS1
   CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET INFO FLAG.
!
   INFO = 0
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
   CQY = JOB/10000 .NE. 0
   CQTY = MOD(JOB,10000) .NE. 0
   CB = MOD(JOB,1000)/100 .NE. 0
   CR = MOD(JOB,100)/10 .NE. 0
   CXB = MOD(JOB,10) .NE. 0
   JU = MIN0(K,N-1)
!
!     SPECIAL ACTION WHEN N=1.
!
   IF (JU .NE. 0) GOTO 80
   IF (.NOT.CQY) GOTO 10
   QYR(1) = YR(1)
   QYI(1) = YI(1)
10 CONTINUE
   IF (.NOT.CQTY) GOTO 20
   QTYR(1) = YR(1)
   QTYI(1) = YI(1)
20 CONTINUE
   IF (.NOT.CXB) GOTO 30
   XBR(1) = YR(1)
   XBI(1) = YI(1)
30 CONTINUE
   IF (.NOT.CB) GOTO 60
   IF (CABS1(XR(1,1),XI(1,1)) .NE. 0.0D0) GOTO 40
   INFO = 1
   GOTO 50
40 CONTINUE
   CALL mat_wdiv(YR(1),YI(1),XR(1,1),XI(1,1),BR(1),BI(1))
50 CONTINUE
60 CONTINUE
   IF (.NOT.CR) GOTO 70
   RSDR(1) = 0.0D0
   RSDI(1) = 0.0D0
70 CONTINUE
   GOTO 290
80 CONTINUE
!
!        SET UP TO COMPUTE QY OR QTY.
!
   IF (CQY) CALL mat_wcopy(N,YR,YI,1,QYR,QYI,1)
   IF (CQTY) CALL mat_wcopy(N,YR,YI,1,QTYR,QTYI,1)
   IF (.NOT.CQY) GOTO 110
!
!           COMPUTE QY.
!
   DO JJ = 1, JU
      J = JU - JJ + 1
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) cycle
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      TR=-mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
      TI=-mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QYR(J), QYI(J),1)
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
   enddo
110 CONTINUE
   IF (.NOT.CQTY) GOTO 140
!
!           COMPUTE CTRANS(Q)*Y.
!
   DO J = 1, JU
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) cycle
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
   enddo
140 CONTINUE
!
!        SET UP TO COMPUTE B, RSD, OR XB.
!
   IF (CB) CALL mat_wcopy(K,QTYR,QTYI,1,BR,BI,1)
   KP1 = K + 1
   IF (CXB) CALL mat_wcopy(K,QTYR,QTYI,1,XBR,XBI,1)
   IF (CR .AND. K .LT. N)CALL mat_wcopy(N-K,QTYR(KP1),QTYI(KP1),1,RSDR(KP1),RSDI(KP1),1)
   IF (.NOT.CXB .OR. KP1 .GT. N) GOTO 160
   DO I = KP1, N
      XBR(I) = 0.0D0
      XBI(I) = 0.0D0
   enddo
160 CONTINUE
   IF (.NOT.CR) GOTO 180
   DO I = 1, K
      RSDR(I) = 0.0D0
      RSDI(I) = 0.0D0
   enddo
180 CONTINUE
   IF (.NOT.CB) GOTO 230
!
!           COMPUTE B.
!
   DO JJ = 1, K
      J = K - JJ + 1
      IF (CABS1(XR(J,J),XI(J,J)) .NE. 0.0D0) GOTO 190
      INFO = J
!                 ......EXIT
!           ......EXIT
      GOTO 220
190   CONTINUE
      CALL mat_wdiv(BR(J),BI(J),XR(J,J),XI(J,J),BR(J),BI(J))
      IF (J .EQ. 1) GOTO 200
      TR = -BR(J)
      TI = -BI(J)
      CALL matX_waxpy(J-1,TR,TI,XR(1,J),XI(1,J),1,BR,BI,1)
200   CONTINUE
   enddo
220 CONTINUE
230 CONTINUE
   IF (.NOT.CR .AND. .NOT.CXB) GOTO 280
!
!           COMPUTE RSD OR XB AS REQUIRED.
!
   DO JJ = 1, JU
      J = JU - JJ + 1
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) cycle
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      IF (CR) then
         TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
         TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
         CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
         CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
      endif
      IF (CXB) then
         TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
         TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
         CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
         CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
      endif
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
   enddo
280 CONTINUE
290 CONTINUE
END SUBROUTINE ML_WQRSL
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

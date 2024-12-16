










!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_math
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128, dp=>real64
implicit none
private

! ident_1="@(#) M_math M_math(3fm) module collecting various general math-related procedures"

  ! GEOMETRY
  public citer              ! determine various geometric properties of circle segment given radius and area of the segment.
  public envelope           ! Find the vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n.
  public inpolygon          ! Subroutine to determine whether or not a point is in a polygon
  public locpt              ! find if a point is inside a polygonal path
  public poly_intercept     ! find points where a line intersects a polygon
  public polyarea           ! find area of a polygon
  public polyarea_shoelace  ! find area of a polygon using shoelace algorithm
  public polyarea_mid_point ! find area of a polygon
  public closest            ! find point in <X,Y> arrays closest to target point
  public hypot              ! Euclidean distance
  ! FIT
  public julfit             ! linear least square fit
  public julfit1            ! linear least square fit(y=a*x+b)
  public lowess             ! data smoothing using locally weighted regression
  public splift             ! fits a spline to the n data points given in x and y
  public splint             ! interpolates and twice differentiates a cubic spline
  public linearint          ! linear interpolation
  ! FITS
  public ju_polfit
  public ju_pvalue
  public glstsq
  private gcsgau1
  private gcsgau2
  ! INTEGRATE
  public qhfg
  public qhsg
  public qtfg
  public trapezoidal_integral
  ! STATISTICS
  public extremum       ! find the minimum and maximum value in a real array
  public bds            ! basic descriptive statistics
  public ncr            ! number of combinations of size R from N cases
  public skekurx        ! skew and kurtosis variant
  public skekur1        ! skew and kurtosis variant
  public stddev         ! standard deviation
  ! PERMUTATIONS
  public nextp          ! next permutation of a previously sorted integer array
  ! SCALES AND RANGES
  public scale1         ! given xmin,xmax,n, find new range xminp xmaxp divisible into approximately n linear intervals of size dist
  public scale3         ! find nice log range, typically for an axis
  public bound          ! constrain a value to a a range, inclusive
  public in             ! test if value is within an expected range, inclusive
  ! MATRIX
  public invert_2x2     ! directly invert 2x2 matrix
  public invert_3x3     ! directly invert 3x3 matrix
  public invert_4x4     ! directly invert 4x4 matrix
  public magic_square   ! create magic squares
  ! POLYNOMIAL
  public quadratic      ! return roots of quadratic equation even if complex

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! From original:
!  In my experience, LAPACK is great when you wish to invert huge NxN
!  matrices, but it can be really slow for inverting smaller 2x2, 3x3,
!  and 4x4 matrices. For my use case, where I need to invert billions of
!  2x2 and 4x4 matrices instead of a few large NxN matrices, I got a 30%
!  speedup of my program replacing the LAPACK calls by direct calculations
!  of the matrix inversions. I have attached the code that I've used for
!  the 2x2, 3x3, and 4x4 cases below. The 2x2 version is quite easy
!  to derive analytically. The 3x3 and 4x4 versions are based on the
!  subroutines M33INV and M44INV by David G. Simpson; I just converted them
!  from subroutines to pure functions.
!===================================================================================================================================
!>
!!##NAME
!!    invert_2x2(3f) - [M_math] directly invert 2x2 matrix
!!
!!##SYNOPSIS
!!
!!   pure function invert_2x2(A) result(B)
!!
!!    ! integer,real,double,complex
!!
!!    integer,parameter         :: wp=kind(0|0.0|0.0d|(0.0,0.0))
!!    NNNNNN(kind=wp), intent(in) :: A(2,2)   !! Matrix
!!    NNNNNN(kind=wp)             :: B(2,2)   !! Inverse matrix
!!     where
!!    NNNNNN may be INTEGER,REAL,DOUBLEPRECISION,COMPLEX
!!
!!##DESCRIPTION
!!
!!    Directly invert 2x2 matrix for speed (versus using, e.g. LAPACK)
!!
!!##OPTIONS
!!
!!    A  original 2x2 matrix, may be INTEGER, REAL, DOUBLE, or COMPLEX
!!    B  inverted 2x2 matrix, of same type as input matrix A
!!
!!##EXAMPLE
!!
interface invert_2x2
   module procedure real_invert_2x2, integer_invert_2x2, complex_invert_2x2, double_invert_2x2
end interface invert_2x2
!===================================================================================================================================
!>
!!##NAME
!!
!!    invert_3x3(3f) - [M_math] directly invert 3x3 matrix
!!
!!##SYNOPSIS
!!
!!
!!   pure function invert_3x3(A) result(B)
!!
!!    ! integer,real,double,complex
!!    integer,parameter         :: wp=kind(0|0.0|0.0d|(0.0,0.0))
!!    NNNNNN(kind=wp), intent(in) :: A(2,2)   !! Matrix
!!    NNNNNN(kind=wp)             :: B(2,2)   !! Inverse matrix
!!     where
!!    NNNNNN may be INTEGER,REAL,DOUBLEPRECISION,COMPLEX
!!
!!##DESCRIPTION
!!    Directly invert 3x3 matrix for speed (versus using, e.g. LAPACK)
!!
!!##OPTIONS
!!    A  original 3x3 matrix, may be INTEGER, REAL, DOUBLE, or COMPLEX
!!    B  inverted 3x3 matrix, of same type as input matrix A
!!
!!##EXAMPLE
!!
interface invert_3x3
   module procedure real_invert_3x3, integer_invert_3x3, complex_invert_3x3, double_invert_3x3
end interface invert_3x3
!===================================================================================================================================
!>
!!##NAME
!!    invert_4x4(3f) - [M_math] directly invert 4x4 matrix
!!
!!##SYNOPSIS
!!
!!   pure function invert_4x4(A) result(B)
!!
!!    ! integer,real,double,complex
!!    integer,parameter         :: wp=kind(0|0.0|0.0d|(0.0,0.0))
!!    NNNNNN(kind=wp), intent(in) :: A(2,2)   !! Matrix
!!    NNNNNN(kind=wp)             :: B(2,2)   !! Inverse matrix
!!     where
!!    NNNNNN may be INTEGER,REAL,DOUBLEPRECISION,COMPLEX
!!
!!##DESCRIPTION
!!    Directly invert 4x4 matrix for speed (versus using, e.g. LAPACK)
!!
!!##OPTIONS
!!    A  original 4x4 matrix, may be INTEGER, REAL, DOUBLE, or COMPLEX
!!    B  inverted 4x4 matrix, of same type as input matrix A
!!
!!##EXAMPLE
!!
interface invert_4x4
   module procedure real_invert_4x4, integer_invert_4x4, complex_invert_4x4, double_invert_4x4
end interface invert_4x4
interface bound
   module procedure bound_r32
   module procedure bound_r64
   module procedure bound_r128
   module procedure bound_i8
   module procedure bound_i16
   module procedure bound_i32
   module procedure bound_i64
end interface bound

interface in
   module procedure in_r32
   module procedure in_r64
   module procedure in_r128
   module procedure in_i8
   module procedure in_i16
   module procedure in_i32
   module procedure in_i64
end interface in

contains

!>
!!##NAME
!!     julfit(3f) - [M_math:fit] linear least squares curve fits, destroys input arrays
!!
!!##SYNOPSIS
!!
!!   subroutine julfit(x,y,ixn,itype,a,b,r2)
!!
!!    integer,intent(in) :: ixn
!!    real               :: x(ixn),y(ixn)
!!    integer,intent(in) :: itype
!!    real,intent(out)   :: a,b,r2
!!
!!##DESCRIPTION
!!     use method of least squares to find a fit to the data.
!!     the expression being fitted is of one of several forms that
!!     have in common the fact that the expression will plot as
!!     a straight line if the proper axis type is selected.
!!
!!      type  x-axis y-axis   significance of a and b
!!
!!       1    linear linear   y=a*x+b         # linear function
!!       2    linear log      y=a*b**x        # exponential function
!!       3    log    linear   y=a*log10(x)+b  # logarithmic function
!!       4    log    log      y=a*x**b        # power functions:
!!                                                 hyperbolic if b <0;
!!                                                 parabolic if b > 0.
!!       5    linear log      y=a*e**(-b*x)   # a common variant of the
!!                                              exponential form.
!!
!!
!!##OPTIONS
!!   x      array of x values, input
!!   y      array of y values, input that are changed to hold the output
!!   ixn    number of points in arrays x and y to use
!!   itype  expression being solved
!!          1. Y=a*X+b
!!          2. Y=a*b**X
!!          3. Y=a*log10(X)+b
!!          4. Y=a*X**b
!!          5. Y=a*e*(-b**X)
!!
!!
!! NOTE: odd use of arrays specifically optimized for calling from USH
!!
!!##RETURNS
!!
!!   a      slope of linearized line
!!   b      y intercept of linearized line
!!   r2     correlation coefficient (1=perfect)
!!
!!          In general, if the correlation coefficient is <0.5 the correlation
!!          is regarded as insignificant. If it is >0.8 the derived linear fit
!!          is considered highly significant.
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        julfit(3f)
!! DESCRIPTION:    linear least squares curve fits, destroys input arrays
!! AUTHOR:         John S. Urban
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine julfit(x,y,ixn,itype,a,b,r2)
use M_framework__journal, only : journal
! ident_2="@(#) M_math julfit(3f) linear least squares curve fits destroys input arrays"
integer,intent(in) :: ixn
real               :: x(ixn)
real               :: y(ixn)
integer,intent(in) :: itype
real,intent(out)   :: a,b,r2
   real            :: xsmall, ysmall
   real            :: az,bz
   integer         :: i20, i30, i40, i50
!===================================================================================================================================
   if(ixn.le.1)then
      call journal('sc','*julfit* invalid number of points=',ixn)
      return
   endif
!===================================================================================================================================
   xsmall=minval(x)
   ysmall=minval(y)
!===================================================================================================================================
   select case(itype)
   case(1) !     y=a*x+b
   case(2) !     y=a*b**x
      if(ysmall.le.0)goto 999
      y(:ixn)=log10(y(:ixn))
   case(3) !     y=a*log10(x)+b
      if(xsmall.le.0)goto 999
      x(:ixn)=log10(x(:ixn))
   case(4) !     y=a*x**b
      if(ysmall.le.0)goto 999
      y(:ixn)=log10(y(:ixn))
      if(xsmall.le.0)goto 999
      x(:ixn)=log10(x(:ixn))
   case(5) !     y=a*e*(-b**x)
      if(ysmall.le.0)goto 999
      y(:ixn)=log(y(:ixn))
   case default
      call journal('sc','*julfit* invalid type=',itype)
      return
   end select
!===================================================================================================================================
   a=0.0
   b=0.0
   r2=0.0
   call julfit1(x,y,ixn,az,bz,r2)
!===================================================================================================================================
   select case(itype)
   case(1)                   ! y=a*x+b
      a=az
      b=bz
   case(2)                   ! y=a*b**x
      b=10**az
      a=10**bz
      do i20=1,ixn
      y(i20)=a*b**x(i20)
      enddo
   case(3)                   ! y=a*log10(x)+b
      b=bz
      a=az
      do i30=1,ixn
         y(i30)=a*x(i30)+b
         x(i30)=10**x(i30)
      enddo
   case(4)                   ! y=a*x**b
      b=az
      a=10**bz
      do i40=1,ixn
         x(i40)=10**x(i40)
         y(i40)=a*x(i40)**b
      enddo
   case(5)                   ! y=a*e*(-b**x)
      b=-az
      a=exp(bz)
      do i50=1,ixn
         y(i50)=a*exp(x(i50)*(-b))
      enddo
   case default
      call journal('sc','*julfit* cannot get here type=',itype)
      return
   end select
!===================================================================================================================================
   !write(*,*)'GOT HERE 3',a,b,r2
   !write(*,*)'GOT HERE 4',x(:ixn),y(:ixn)
   return
999   continue
   call journal('*lfit* cannot take log or inverse of values <= 0')
   call journal('*lfit* performing linear fit instead')
   call julfit1(x,y,ixn,a,b,r2)
end subroutine julfit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      julfit1(3f) - [M_math:fit] internal routine for linear least square fit(y=a*x+b), changes the y array
!!##SYNOPSIS
!!
!!
!!    subroutine julfit1(x,y,ixn,a,b,r2)
!!
!!       real,intent(in)    :: x(*)
!!       real               :: y(*)
!!       integer,intent(in) :: ixn
!!       real,intent(out)   :: a
!!       real,intent(out)   :: b
!!       real,intent(out)   :: r2
!!##DESCRIPTION
!!
!!    While the method of least squares often gives optimal estimates
!!    parameters for linear processes, it is very sensitive to the presence
!!    of unusual data points in the data used to fit a model, as the square
!!    of the distance from the resulting fit is used in the calculation.
!!
!!    That is, a few outliers can sometimes seriously skew the results of a
!!    least squares analysis; this makes model validation, especially with
!!    respect to outliers, critical to obtaining sound answers.
!!
!!##OPTIONS
!!     X     input X values
!!     Y     input Y values
!!     IXN   size of X and Y vectors
!!     A     multiplier
!!     B     y-intercept
!!     R2
!!##EXAMPLE
!!
!!   sample program
!!
!!       program demo_julfit1
!!          use M_math, only : julfit1
!!          implicit none
!!          intrinsic random_number
!!          integer :: points
!!          real    :: slope, intercept
!!          write(*,*)'For y=m*x+b enter M and B and number of points N:'
!!          read(*,*)slope,intercept,points
!!          call testit()
!!       contains
!!
!!          subroutine testit()
!!             real    :: x(points), y(points)
!!             real    :: slope_out, intercept_out, r2
!!             integer :: i
!!             real    :: rndnum
!!             do i=1,points
!!                x(i)=i*0.10
!!                ! assigned pseudorandom numbers from the uniform distribution in the interval 0  x < 1.
!!                call random_number(rndnum)
!!                y(i)=slope*(x(i)+4.0*(rndnum-0.5))+intercept
!!             enddo
!!             !write(*,*)(ii,x(ii),y(ii),new_line('A'),ii=1,points)
!!             call julfit1(x,y,points,slope_out,intercept_out,r2)
!!             write(*,*)'SLOPE AND INTERCEPT IN  ',slope,intercept
!!             write(*,*)'SLOPE AND INTERCEPT OUT ',slope_out,intercept_out,r2
!!          end subroutine testit
!!
!!       end program demo_julfit1
!!
!!   Results
!!
!!     $ xxx
!!     For y=m*x+b enter M and B and number of points N:
!!     10 20 1000000
!!     SLOPE AND INTERCEPT IN     10.0000000       20.0000000
!!     SLOPE AND INTERCEPT OUT    10.0000000       19.9998207       1.00000000
!!
!!     $ xxx
!!     For y=m*x+b enter M and B and number of points N:
!!     10 20 100
!!     SLOPE AND INTERCEPT IN     10.0000000       20.0000000
!!     SLOPE AND INTERCEPT OUT    9.62195778       23.3507996      0.850686848
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        julfit1(3f)
!! DESCRIPTION:    internal routine for linear least square fit(y=a*x+b), changes the y array
!!##VERSION:        1.0, 1980
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!! COPYRIGHT:      Copyright (C) 1980 John S. Urban
!! LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.
!!                 There is NO WARRANTY, to the extent permitted by law.
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine julfit1(x,y,ixn,a,b,r2)
implicit none
! ident_3="@(#) M_math julfit1(3f) linear least square fit of (y=a*x+b) changes the y array"
!implicit double precision(a-h,o-z)
!-----------------------------------------------------------------------------------------------------------------------------------
integer,intent(in) :: ixn
real,intent(in)    :: x(*)
real               :: y(*)

real,intent(out)   :: a
real,intent(out)   :: b
real,intent(out)   :: r2
!-----------------------------------------------------------------------------------------------------------------------------------
doubleprecision    :: sx, sy, sxy, sxx, sdel, sdelavg, xn, s, yi, ave, difi, del
integer            :: i10, i20, i30
!-----------------------------------------------------------------------------------------------------------------------------------
!  initialize
   sx=0.0d0                          ! sum of x
   sy=0.0d0                          ! sum of y
   sxy=0.0d0                         ! sum of products of x and y
   sxx=0.0d0                         ! sum of squared x values
   sdel=0.0d0                        ! sum of squares of differences between measured and calculated y values
   sdelavg=0.0d0                     ! sum of squares of differences between measured y values minus average y value
   xn=dble(ixn)
!-----------------------------------------------------------------------------------------------------------------------------------
   do i10=1,ixn                      ! calculate required sums
      sx=sx+x(i10)                   ! sum of x
      sy=sy+y(i10)                   ! sum of y
      sxy=sxy+x(i10)*y(i10)          ! sum of x*y
      sxx=sxx+x(i10)*x(i10)          ! sum of x*x
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   s=(xn*sxy-sx*sy)/(xn*sxx-sx*sx)   ! compute slope of best line thru points
   yi=(sy-s*sx)/xn                   ! compute y intercept of best line thru points
   ave=sy/xn
!-----------------------------------------------------------------------------------------------------------------------------------
   do i20=1,ixn,1                    ! calculate correlation coefficient
      difi=y(i20)-(s*x(i20)+yi)      ! linearized difference between actual and calculated
      sdel=sdel+difi*difi            ! sum of (differences**2)
      del=y(i20)-ave                 ! linearized difference between actual and calculated average
      sdelavg=sdelavg+del*del        ! sum of (difference_from_average**2)
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   r2=1.0d0-sdel/sdelavg
   a=s
   b=yi
   do i30=1,ixn
     y(i30)=a*x(i30)+b
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine julfit1
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     lowess(3f) - [M_math:fit] procedures for locally weighted regression
!!
!!##SYNOPSIS
!!
!!    Calling Sequence:
!!
!!     subroutine lowess(x, y, n, f, nsteps, delta, ys, rw, res)
!!     real,intent(in)    :: x(n)
!!     real,intent(in)    :: y(n)
!!     integer,intent(in) :: n
!!     real,intent(in)    :: f
!!     integer,intent(in) :: nsteps
!!     real,intent(in)    :: delta
!!
!!     real,intent(out)   :: ys(n)
!!     real,intent(out)   :: rw(n)
!!     real,intent(out)   :: rw(res)
!!
!!##PURPOSE
!!
!!    Lowess is a data analysis technique for producing a "smooth" set of
!!    values from a time series which has been contaminated with noise,
!!    or from a scatter plot with a "noisy" relationship between the two
!!    variables. In a time series context, the technique is an improvement
!!    over least squares smoothing when the data is not equally spaced
!!    (as least squares smoothing assumes).
!!
!!##DESCRIPTION
!!
!!    LOWESS stands for "locally weighted regression". LOWESS computes the
!!    smooth of a scatterplot of Y against X using robust locally weighted
!!    regression. Fitted values, YS, are computed at each of the values of
!!    the horizontal axis in X.
!!
!!    For lowess smoothing, the analyst can vary the size of the smoothing
!!    window. This size is given as the fraction (0 to 1) of the data that
!!    the window should cover. The default window size is .2 (which states
!!    that the smoothing window has a total width of 20% of the horizontal
!!    axis variable). The LOWESS fraction (F) controls the smoothness of the
!!    curve. For example, if it is 1.0, then the LOWESS curve is a single
!!    straight line. In general, the smaller the fraction, the more that
!!    LOWESS curve follows individual data points. To obtain a smoother
!!    LOWESS curve, increase the value of the LOWESS FRACTION.
!!
!!    This package consists of two FORTRAN procedures for smoothing
!!    scatterplots by robust locally weighted regression, or lowess. The
!!    principal routine is LOWESS which computes the smoothed values using
!!    the method described in "The Elements of Graphing Data", by William S.
!!    Cleveland (Wadsworth, 555 Morego Street, Monterey, California 93940).
!!
!!    LOWESS calls a support routine, LOWEST, the code for which is
!!    included. LOWESS also calls a routine SORT, which the user must provide.
!!
!!    To reduce the computations, LOWESS requires that the arrays X and Y,
!!    which are the horizontal and vertical coordinates, respectively, of the
!!    scatterplot, be such that X is sorted from smallest to largest. The
!!    user must therefore use another sort routine which will sort X and Y
!!    according to X.
!!
!!    To summarize the scatterplot, YS, the fitted values, should be plotted
!!    against X. No graphics routines are available in the package and must
!!    be supplied by the user.
!!
!!    The FORTRAN code for the routines LOWESS and LOWEST has been generated
!!    from higher level RATFOR programs (B. W. Kernighan, ``RATFOR: A
!!    Preprocessor for a Rational Fortran,'' Software Practice and Experience,
!!    Vol. 5 (1975).
!!
!!##OPTIONS
!!
!!    ARGUMENT DESCRIPTION
!!
!!          X =       Input; abscissas of the points on the
!!                    scatterplot; the values in X must be ordered
!!                    from smallest to largest.
!!          Y =       Input; ordinates of the points on the
!!                    scatterplot.
!!          N =       Input; dimension of X,Y,YS,RW, and RES.
!!          F =       Input; specifies the amount of smoothing; F is
!!                    the fraction of points used to compute each
!!                    fitted value; as F increases the smoothed values
!!                    become smoother; choosing F in the range .2 to
!!                    .8 usually results in a good fit; if you have no
!!                    idea which value to use, try F = .5.
!!          NSTEPS =  Input; the number of iterations in the robust
!!                    fit; if NSTEPS = 0, the nonrobust fit is
!!                    returned; setting NSTEPS equal to 2 should serve
!!                    most purposes.
!!          DELTA =   input; nonnegative parameter which may be used
!!                    to save computations; if N is less than 100, set
!!                    DELTA equal to 0.0; if N is greater than 100 you
!!                    should find out how DELTA works by reading the
!!                    additional instructions section.
!!          YS =      Output; fitted values; YS(I) is the fitted value
!!                    at X(I); to summarize the scatterplot, YS(I)
!!                    should be plotted against X(I).
!!          RW =      Output; robustness weights; RW(I) is the weight
!!                    given to the point (X(I),Y(I)); if NSTEPS = 0,
!!                    RW is not used.
!!          RES =     Output; residuals; RES(I) = Y(I)-YS(I).
!!
!!
!!    ADDITIONAL INSTRUCTIONS
!!
!!        DELTA can be used to save computations.
!!        Very roughly the algorithm is this:
!!        on the initial fit and on each of the NSTEPS iterations locally weighted regression fitted values are computed
!!        at points in X which are spaced, roughly, DELTA apart;
!!        then the fitted values at the remaining points are computed using linear interpolation.
!!        The first locally weighted regression (LWR) computation is carried out at X(1) and the last is carried out at X(N).
!!        Suppose the LWR computation is carried out at X(I).
!!        If X(I+1) is greater than or equal to X(I)+DELTA, the next LWR computation is carried out at X(I+1).
!!        If X(I+1) is less than X(I)+DELTA, the next LWR computation is carried out at the largest X(J) which is greater than
!!        or equal to X(I) but is not greater than X(I)+DELTA.
!!        Then the fitted values for X(K) between X(I) and X(J), if there are any, are computed by linear interpolation
!!        of the fitted values at X(I) and X(J).
!!        If N is less than 100 then DELTA can be set to 0.0 since the computation time will not be too great.
!!        For larger N it is typically not necessary to carry out the LWR computation for all points,
!!        so that much computation time can be saved by taking DELTA to be greater than 0.0.
!!        If DELTA = Range (X)/k then,
!!        if the values in X were uniformly scattered over the range,
!!        the full LWR computation would be carried out at approximately k points.
!!        Taking k to be 50 often works well.
!!
!!    METHOD
!!
!!        The fitted values are computed by using the nearest neighbor
!!        routine and robust locally weighted regression of degree 1
!!        with the tricube weight function. A few additional features
!!        have been added. Suppose r is FN truncated to an integer.
!!        Let h be the distance to the r-th nearest neighbor
!!        from X(I). All points within h of X(I) are used. Thus if
!!        the r-th nearest neighbor is exactly the same distance as
!!        other points, more than r points can possibly be used for
!!        the smooth at X(I). There are two cases where robust
!!        locally weighted regression of degree 0 is actually used at
!!        X(I). One case occurs when h is 0.0. The second case
!!        occurs when the weighted standard error of the X(I) with
!!        respect to the weights w(j) is less than .001 times the
!!        range of the X(I), where w(j) is the weight assigned to the
!!        j-th point of X (the tricube weight times the robustness
!!        weight) divided by the sum of all of the weights. Finally,
!!        if the w(j) are all zero for the smooth at X(I), the fitted
!!        value is taken to be Y(I).
!!
!!##DEPENDENCIES
!!
!!      o LOWEST
!!      o SORT
!!
!!    LOWEST
!!
!!    Calling sequence
!!
!!         CALL LOWEST(X,Y,N,XS,YS,NLEFT,NRIGHT,W,USERW,RW,OK)
!!
!!    PURPOSE
!!
!!        LOWEST is a support routine for LOWESS and ordinarily will
!!        not be called by the user. The fitted value, YS, is
!!        computed at the value, XS, of the horizontal axis.
!!        Robustness weights, RW, can be employed in computing the
!!        fit.
!!
!!    OPTIONS
!!         Argument description
!!
!!          X =       Input; abscissas of the points on the
!!                    scatterplot; the values in X must be ordered
!!                    from smallest to largest.
!!          Y =       Input; ordinates of the points on the
!!                    scatterplot.
!!          N =       Input; dimension of X,Y,W, and RW.
!!          XS =      Input; value of the horizontal axis at which the
!!                    smooth is computed.
!!          YS =      Output; fitted value at XS.
!!          NLEFT =   Input; index of the first point which should be
!!                    considered in computing the fitted value.
!!          NRIGHT =  Input; index of the last point which should be
!!                    considered in computing the fitted value.
!!          W =       Output; W(I) is the weight for Y(I) used in the
!!                    expression for YS, which is the sum from
!!                    I = NLEFT to NRIGHT of W(I)*Y(I); W(I) is
!!                    defined only at locations NLEFT to NRIGHT.
!!          USERW =   Input; logical variable; if USERW is .TRUE., a
!!                    robust fit is carried out using the weights in
!!                    RW; if USERW is .FALSE., the values in RW are
!!                    not used.
!!          RW =      Input; robustness weights.
!!          OK =      Output; logical variable; if the weights for the
!!                    smooth are all 0.0, the fitted value, YS, is not
!!                    computed and OK is set equal to .FALSE.; if the
!!                    fitted value is computed OK is set equal to
!!
!!    METHOD
!!
!!         The smooth at XS is computed using (robust) locally weighted
!!         regression of degree 1. The tricube weight function is used
!!         with h equal to the maximum of XS-X(NLEFT) and X(NRIGHT)-XS.
!!         Two cases where the program reverts to locally weighted
!!         regression of degree 0 are described in the documentation
!!         for LOWESS.
!!
!!##DEPENDENCIES
!!       o lowest
!!       o sort_shell ! user-supplied SORT
!!
!!##EXAMPLES
!!
!!
!!   Example program:
!!
!!       program demo_lowess
!!       use M_math, only : lowess
!!       !  test driver for lowess
!!       !  for expected output, see introduction
!!       real x(20), y(20), ys(20), rw(20), res(20)
!!       data x /1,2,3,4,5,10*6,8,10,12,14,50/
!!       data y /18,2,15,6,10,4,16,11,7,3,14,17,20,12,9,13,1,8,5,19/
!!       call lowess(x,y,20,.25,0,0.,ys,rw,res)
!!       write(*,*) ys
!!       call lowess(x,y,20,.25,0,3.,ys,rw,res)
!!       write(*,*) ys
!!       call lowess(x,y,20,.25,2,0.,ys,rw,res)
!!       write(*,*) ys
!!       end program demo_lowess
!!
!!       The following are data and output from LOWESS that can
!!       be used to check your implementation of the routines. The
!!       notation (10)v means 10 values of v.
!!
!!        X values:
!!          1  2  3  4  5  (10)6  8  10  12  14  50
!!
!!        Y values:
!!           18  2  15  6  10  4  16  11  7  3  14  17  20  12  9  13  1  8  5  19
!!
!!        YS values with F = .25, NSTEPS = 0, DELTA = 0.0
!!         13.659  11.145  8.701  9.722  10.000  (10)11.300  13.000  6.440  5.596
!!           5.456  18.998
!!
!!        YS values with F = .25, NSTEPS = 0 ,  DELTA = 3.0
!!          13.659  12.347  11.034  9.722  10.511  (10)11.300  13.000  6.440  5.596
!!            5.456  18.998
!!
!!        YS values with F = .25, NSTEPS = 2, DELTA = 0.0
!!          14.811  12.115  8.984  9.676  10.000  (10)11.346  13.000  6.734  5.744
!!          5.415  18.998
!!
!!##REFERENCE
!!     This routine is functionally based on the "netlib" routine lowess
!!     from netlib/go/lowess.f .
!!
!!     "Graphical Methods for Data Analysis", Chambers, Cleveland, Kleiner, and
!!     Tukey. Wadsworth, 1983.
!!
!!##APPLICATIONS
!!
!!     Time Series Analysis
!!
!!
!!##SEE ALSO
!!
!!    A multivariate version is available by "send dloess from a"
!!    from the NETLIB server.
!!
!!##AUTHOR
!!
!!    Bill Cleveland
!!
!!     research!alice!wsc Mon Dec 30 16:55 EST 1985
!!     W. S. Cleveland
!!     ATT Bell Laboratories
!!     Murray Hill NJ 07974
!>
!! AUTHOR:     Bill Cleveland
SUBROUTINE lowess(X,Y,N,F,Nsteps,Delta,Ys,Rw,Res)
   USE m_sort , only:sort_shell
   IMPLICIT NONE
   INTEGER N
   INTEGER Nsteps
   REAL X(N) , Y(N) , F , Delta , Ys(N) , Rw(N)
   REAL Res(N)
   INTEGER nright , i , j , iter , last , m1 , m2 , ns , nleft
   REAL cut , cmad , r , d1 , d2
   REAL c1 , c9 , alpha , denom
   LOGICAL ok
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
         IF ( N>=2 ) THEN
! at least two, at most n points
            ns = max0(min0(int(F*float(N)),N),2)
            iter = 1
         ELSE
            Ys(1) = Y(1)
            RETURN
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         IF ( iter>Nsteps+1 ) RETURN
! robustness iterations
         nleft = 1
         nright = ns
! index of prev estimated point
         last = 0
! index of current point
         i = 1
         spag_nextblock_1 = 3
      CASE (3)
         SPAG_Loop_1_1: DO WHILE ( nright<N )
! move nleft, nright to right if radius decreases
            d1 = X(i) - X(nleft)
! if d1<=d2 with x(nright+1)==x(nright), lowest fixes
            d2 = X(nright+1) - X(i)
            IF ( d1<=d2 ) EXIT SPAG_Loop_1_1
! radius will not decrease by move right
            nleft = nleft + 1
            nright = nright + 1
         ENDDO SPAG_Loop_1_1
! fitted value at x(i)
         CALL lowest(X,Y,N,X(i),Ys(i),nleft,nright,Res,iter>1,Rw,ok)
         IF ( .NOT.ok ) Ys(i) = Y(i)
! all weights zero - copy over value (all rw==0)
         IF ( last<i-1 ) THEN
            denom = X(i) - X(last)
! skipped points -- interpolate
! non-zero - proof?
            j = last + 1
            DO WHILE ( j<i )
               alpha = (X(j)-X(last))/denom
               Ys(j) = alpha*Ys(i) + (1.0-alpha)*Ys(last)
               j = j + 1
            ENDDO
         ENDIF
! last point actually estimated
         last = i
! x coord of close points
         cut = X(last) + Delta
         i = last + 1
         SPAG_Loop_1_2: DO WHILE ( i<=N )
! find close points
            IF ( X(i)>cut ) EXIT SPAG_Loop_1_2
! i one beyond last pt within cut
            IF ( X(i)==X(last) ) THEN
               Ys(i) = Ys(last)
! exact match in x
               last = i
            ENDIF
            i = i + 1
         ENDDO SPAG_Loop_1_2
! back 1 point so interpolation within delta, but always go forward
         i = max0(last+1,i-1)
         IF ( last<N ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
! residuals
         DO i = 1 , N
            Res(i) = Y(i) - Ys(i)
         ENDDO
         IF ( iter<=Nsteps ) THEN
! compute robustness weights except last time
            DO i = 1 , N
               Rw(i) = abs(Res(i))
            ENDDO
            CALL sort_shell(Rw,order='A')           ! sort in ascending order
            m1 = N/2 + 1
            m2 = N - m1 + 1
! 6 median abs resid
            cmad = 3.0*(Rw(m1)+Rw(m2))
            c9 = .999*cmad
            c1 = .001*cmad
            DO i = 1 , N
               r = abs(Res(i))
               IF ( r<=c1 ) THEN
! near 0, avoid underflow
                  Rw(i) = 1.
               ELSEIF ( r<=c9 ) THEN
                  Rw(i) = (1.0-(r/cmad)**2)**2
               ELSE
! near 1, avoid underflow
                  Rw(i) = 0.
               ENDIF
            ENDDO
            iter = iter + 1
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE lowess

SUBROUTINE lowest(X,Y,N,Xs,Ys,Nleft,Nright,W,Userw,Rw,Ok)
   IMPLICIT NONE
   INTEGER N
   INTEGER Nleft , Nright
   REAL X(N) , Y(N) , Xs , Ys , W(N) , Rw(N)
   LOGICAL Userw , Ok
   INTEGER nrt , j
   REAL a , b , c , h , r
   REAL h1 , sqrt , h9 , amax1 , range
   range = X(N) - X(1)
   h = amax1(Xs-X(Nleft),X(Nright)-Xs)
   h9 = .999*h
   h1 = .001*h
! sum of weights
   a = 0.0
   j = Nleft
   SPAG_Loop_1_1: DO WHILE ( j<=N )
! compute weights (pick up all ties on right)
      W(j) = 0.
      r = abs(X(j)-Xs)
      IF ( r>h9 ) THEN
         IF ( X(j)>Xs ) EXIT SPAG_Loop_1_1
         j = j + 1
! get out at first zero wt on right
      ELSE
         IF ( r<=h1 ) THEN
            W(j) = 1.
         ELSE
! small enough for non-zero weight
            W(j) = (1.0-(r/h)**3)**3
         ENDIF
         IF ( Userw ) W(j) = Rw(j)*W(j)
         a = a + W(j)
         j = j + 1
      ENDIF
   ENDDO SPAG_Loop_1_1
! rightmost pt (may be greater than nright because of ties)
   nrt = j - 1
   IF ( a>0.0 ) THEN
      Ok = .TRUE.
! weighted least squares
      DO j = Nleft , nrt
! make sum of w(j) == 1
         W(j) = W(j)/a
      ENDDO
      IF ( h>0. ) THEN
         a = 0.0
! use linear fit
         DO j = Nleft , nrt
! weighted center of x values
            a = a + W(j)*X(j)
         ENDDO
         b = Xs - a
         c = 0.0
         DO j = Nleft , nrt
            c = c + W(j)*(X(j)-a)**2
         ENDDO
         IF ( sqrt(c)>.001*range ) THEN
            b = b/c
! points are spread out enough to compute slope
            DO j = Nleft , nrt
               W(j) = W(j)*(b*(X(j)-a)+1.0)
            ENDDO
         ENDIF
      ENDIF
      Ys = 0.0
      DO j = Nleft , nrt
         Ys = Ys + W(j)*Y(j)
      ENDDO
   ELSE
      Ok = .FALSE.
   ENDIF
END SUBROUTINE lowest
!>
!!##NAME
!!    splift(3f) - [M_math:fit] fits a spline to the n data points given in x and y
!!                 and also returns first and second derivatives
!!##SYNOPSIS
!!
!!   subroutine splift(x,y,yp,ypp,n,ierr,a1,b1,an,bn)
!!
!!    real,intent(in)            :: x(n),y(n)
!!    real,intent(out)           :: yp(n),ypp(n)
!!    integer,intent(in)         :: n
!!    integer,intent(out)        :: ierr
!!    real,intent(in)            :: a1
!!    real,intent(in)            :: b1
!!    real,intent(in)            :: an
!!    real,intent(in)            :: bn
!!
!!##DESCRIPTION
!!    SPLIFT(3f) fits a spline to the N data points given in X and Y and returns
!!    the first and second derivatives in YP and YPP. The resulting spline,
!!    defined by the arrays X, Y, and YPP, may then be interpolated (if desired)
!!    using SPLINT(3f).
!!
!!    For a smoothing spline fit see SUBROUTINE SMOOTH.
!!##OPTIONS
!!
!!       X            array of abscissas (in increasing order)
!!       Y            array of ordinates
!!       N            number of data points (the dimension of X,Y,YP and YPP)
!!       A1,B1,AN,BN  end condition specifications
!!
!!                     The end conditions of the spline are
!!                          YPP(1) = A1*YPP(2) + B1
!!                     and
!!                          YPP(N) = AN*YPP(N-1) + BN,
!!                     where
!!                          ABS(A1).LT.1.0 and ABS(AN).LT.1.0.
!!
!!             The smoothest (i.e., least integral of square of
!!             second derivative) spline is obtained by A1=B1=AN=BN=0.
!!             If extrapolation outside the range from X(1) to X(N)
!!             is to be done (By SPLINT(3f), say), better results may
!!             be obtained by using A1=AN=0.5, B1=BN=0.
!!
!!##RETURNS
!!
!!       YP     Resulting derivative
!!       YPP    Resulting second derivative
!!       IERR   Error status.
!!              NORMAL CODES
!!                =0  means the requested spline was computed.
!!              ABNORMAL CODES
!!                =1  means N was too small (.LT.4).
!!                =2  means the abscissas were not strictly increasing.
!!
!!##EXAMPLE
!!
!!
!!##PEDIGREE
!!
!!    Original written by:
!!
!!      Rondall E. Jones
!!      Sandia Mathematical Program Library
!!      Applied Mathematics Division 2642
!!      Sandia Laboratories
!!      P. O. Box 5800
!!      Albuquerque, New Mexico  87115
!!      Control Data 6600 Version 5.1, 10 December 1973
!!
!!
!!      WARD implementation   S. J. Orbon        4/1/1974
!!
!!      F90+ Implementation   J. S. Urban
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        splift(3f)
!! DESCRIPTION:    fits a spline to the n data points given in x and y
!!##VERSION:        5.0: 20170129
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!===================================================================================================================================
subroutine splift(x,y,yp,ypp,n,ierr,a1,b1,an,bn)
!-----------------------------------------------------------------------------------------------------------------------------------
use M_framework__journal, only : journal
! ident_4="@(#) M_math splift(3f) fits a spline to the n data points given in x and y"
integer,intent(in)            :: n
real,intent(in)               :: x(n),y(n)
real,intent(out)              :: yp(n),ypp(n)
integer,intent(out)           :: ierr           ! error status.
real,intent(in)               :: a1
real,intent(in)               :: b1
real,intent(in)               :: an
real,intent(in)               :: bn

   character(len=255)         :: ctemp
   real                       :: w(n,3)         ! w is a work array that must be able to hold at least N*3 numbers
   integer                    :: i, j
   integer                    :: nm1
   integer                    :: nm2
   real                       :: dold
   real                       :: dnew
!-----------------------------------------------------------------------------------------------------------------------------------
   if (n.lt.4) then
      ierr=1
      call journal('*splift* number of abscissas too small (.lt.4)')
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   do i=2,n     ! make sure x(:) values are increasing monotonically
      if ( (x(i)-x(i-1)) .gt. 0 ) cycle
      ierr=2
      call journal('sc','*splift* abscissa not strictly increasing, index=',i)
      write(ctemp,"('*splift* x,y=',g20.13,1x,g20.13)")x(i),y(i)
      call journal(ctemp)
      return
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      nm1  = n-1                                                          ! SPLITF.62
      nm2  = n-2                                                          ! SPLITF.63
!-----------------------------------------------------------------------------------------------------------------------------------
!     DEFINE THE TRIDIAGONAL MATRIX                                       ! SPLITF.68
!                                                                         ! SPLITF.69
      w(1,3) = x(2)-x(1)                                                  ! SPLITF.70
      do i=2,nm1                                                          ! SPLITF.71
         w(i,2) = w(i-1,3)                                                ! SPLITF.72
         w(i,3) = x(i+1)-x(i)                                             ! SPLITF.73
         w(i,1) = 2.0*(w(i,2)+w(i,3))                                     ! SPLITF.74
      enddo
      w(1,1) = 4.0                                                        ! SPLITF.75
      w(1,3) =-4.0*a1                                                     ! SPLITF.76
      w(n,1) = 4.0                                                        ! SPLITF.77
      w(n,2) =-4.0*an                                                     ! SPLITF.78
!                                                                         ! SPLITF.79
!     L U DECOMPOSITION                                                   ! SPLITF.80
!                                                                         ! SPLITF.81
      do i=2,n                                                            ! SPLITF.82
         w(i-1,3) = w(i-1,3)/w(i-1,1)                                     ! SPLITF.83
         w(i,1)   = w(i,1) - w(i,2)*w(i-1,3)                              ! SPLITF.84
      enddo
!                                                                         ! SPLITF.85
!     DEFINE *CONSTANT* VECTOR                                            ! SPLITF.86
!                                                                         ! SPLITF.87
      ypp(1) = 4.0*b1                                                     ! SPLITF.88
      dold   = (y(2)-y(1))/w(2,2)                                         ! SPLITF.89
      do i=2,nm2                                                          ! SPLITF.90
         dnew   = (y(i+1) - y(i))/w(i+1,2)                                ! SPLITF.91
         ypp(i) = 6.0*(dnew - dold)                                       ! SPLITF.92
         yp(i)  = dold                                                    ! SPLITF.93
         dold   = dnew                                                    ! SPLITF.94
      enddo
      dnew   = (y(n)-y(n-1))/(x(n)-x(n-1))                                ! SPLITF.95
      ypp(nm1) = 6.0*(dnew - dold)                                        ! SPLITF.96
      ypp(n) = 4.0*bn                                                     ! SPLITF.97
      yp(nm1)= dold                                                       ! SPLITF.98
      yp(n)  = dnew                                                       ! SPLITF.99
!                                                                         ! SPLITF.100
!     FORWARD SUBSTITUTION                                                ! SPLITF.101
!                                                                         ! SPLITF.102
      ypp(1) = ypp(1)/w(1,1)                                              ! SPLITF.103
      do i=2,n                                                            ! SPLITF.104
         ypp(i) = (ypp(i) - w(i,2)*ypp(i-1))/w(i,1)                       ! SPLITF.105
      enddo
!                                                                         ! SPLITF.106
!     BACKWARD SUBSTITUTION                                               ! SPLITF.107
!                                                                         ! SPLITF.108
      do j=1,nm1                                                          ! SPLITF.109
         i = n-j                                                          ! SPLITF.110
         ypp(i) = ypp(i) - w(i,3)*ypp(i+1)                                ! SPLITF.111
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     COMPUTE FIRST DERIVATIVES                                           ! SPLITF.113
!                                                                         ! SPLITF.114
      yp(1)  = (y(2)-y(1))/(x(2)-x(1)) - (x(2)-x(1))*(2.0*ypp(1) + ypp(2))/6.0
      do i=2,nm1
         yp(i)  = yp(i) + w(i,2)*(ypp(i-1) + 2.0*ypp(i))/6.0              ! SPLITF.118
      enddo
      yp(n)  = yp(n) + (x(n)-x(nm1))*(ypp(nm1) + 2.0*ypp(n))/6.0          ! SPLITF.119
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
end subroutine splift
!===================================================================================================================================
!>
!!##NAME
!!    splint(3f) - [M_math:fit] interpolates and twice differentiates a cubic spline
!!##SYNOPSIS
!!
!!   subroutine splint (x,y,ypp,n,xi,yi,ypi,yppi,ni,kerr)
!!
!!    integer,intent(in) :: n, ni
!!    real,intent(in)    :: x(n),y(n),ypp(n),xi(ni)
!!    real,intent(out)   :: yi(ni),ypi(ni),yppi(ni)
!!
!!##DESCRIPTION
!!    SPLINT(3f) interpolates and twice differentiates a cubic spline
!!    defined by X, Y, and YPP at the abscissas in XI. The spline
!!    may have been determined by SPLIFT(3f) or SMOOTH(3f) or any other
!!    spline fitting routine which provides second derivatives.
!!
!!##OPTIONS
!!     X      array of data abscissas
!!     Y      array of data ordinates
!!     YPP    array of spline second derivatives
!!     N      number of data points (the dimension of X,Y, and YPP)
!!     XI     array of abscissas (in arbitrary order) at which
!!            the spline is to be interpolated.
!!     NI     dimension of XI, YI, YPI, and YPPI.
!!            (if NI=1, XI, YI, YPI and YPPI may be simple variables.)
!!##RETURNS
!!     YI     array of interpolated ordinates (OUTPUT)
!!     YPI    array of interpolated derivatives (OUTPUT)
!!     YPPI   array of interpolated second derivatives (OUTPUT)
!!     KERR   error status parameter (OUTPUT)
!!           NORMAL CODES
!!           =0  means the spline was evaluated at each abscissa
!!               in XI using only interpolation.
!!           =1  means the spline was evaluated at each abscissa
!!               in XI, but at least one extrapolation was performed.
!!           ABNORMAL CODE
!!           =2  means the requested number of interpolations, NI,
!!               was not positive.
!!##EXAMPLE
!!
!!
!!##PEDIGREE
!!
!!    Original written by:
!!
!!      Rondall E. Jones
!!      Sandia Mathematical Program Library
!!      Applied Mathematics Division 2642
!!      Sandia Laboratories
!!      P. O. Box 5800
!!      Albuquerque, New Mexico  87115
!!
!!    Control Data 6600 Version 5.1, 10 December 1973
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        splint(3f)
!! DESCRIPTION:    interpolates and twice differentiates a cubic spline
!!##VERSION:        5.0: 20170129
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!*==splint.f90 processed by SPAG 8.01RF 03:45 14 Dec 2024
subroutine splint(x,y,ypp,n,xi,yi,ypi,yppi,ni,kerr)
implicit none
! ident_5="@(#) M_math splint(3f) interpolates and twice differentiates a cubic spline"
integer, intent(in)  :: n
integer, intent(in)  :: ni
real, intent(in)     :: x(n), y(n), ypp(n), xi(ni)
real, intent(out)    :: yi(ni), ypi(ni), yppi(ni)
integer, intent(out) :: kerr

integer              :: nm1, k, il, ir, i
real                 :: h, h2, xx
real                 :: xr, xr2, xr3, xl, xl2, xl3
integer              :: spag_nextblock_1
   spag_nextblock_1 = 1
   spag_dispatchloop_1: do
      select case (spag_nextblock_1)
      case (1)
!  CHECK INPUT
         if ( ni<=0 ) then
            kerr = 2
            return
         endif
         kerr = 0
         nm1 = n - 1
!  K IS INDEX ON VALUE OF XI BEING WORKED ON.  XX IS THAT VALUE.
!  I IS CURRENT INDEX INTO X ARRAY.
         k = 1
         xx = xi(1)
         if ( xx<x(1) ) then
            spag_nextblock_1 = 3
            cycle spag_dispatchloop_1
         endif
         if ( xx<=x(n) ) then
            il = 1
            ir = n
!------------------------------------
!  BISECTION SEARCH
            do
               i = (il+ir)/2
               if ( i==il ) then
                  spag_nextblock_1 = 4
                  cycle spag_dispatchloop_1
               endif
               if ( xx<x(i) ) then
                  ir = i
               elseif ( xx==x(i) ) then
                  spag_nextblock_1 = 4
                  cycle spag_dispatchloop_1
               else
                  il = i
               endif
            enddo
         endif
         spag_nextblock_1 = 2
!------------------------------------
      case (2)
!     EXTRAPOLATION
         kerr = 1
         i = nm1
         spag_nextblock_1 = 4
         cycle spag_dispatchloop_1
      case (3)
         kerr = 1
         i = 1
         spag_nextblock_1 = 4
!------------------------------------
      case (4)
!     INTERPOLATION
         spag_loop_1_1: do
            h = x(i+1) - x(i)
            h2 = h*h
            xr = (x(i+1)-xx)/h
            xr2 = xr*xr
            xr3 = xr*xr2
            xl = (xx-x(i))/h
            xl2 = xl*xl
            xl3 = xl*xl2
            yi(k) = y(i)*xr + y(i+1)*xl - h2*(ypp(i)*(xr-xr3)+ypp(i+1)*(xl-xl3))/6.0
            ypi(k) = (y(i+1)-y(i))/h + h*(ypp(i)*(1.0-3.0*xr2)-ypp(i+1)*(1.0-3.0*xl2))/6.0
            yppi(k) = ypp(i)*xr + ypp(i+1)*xl
!------------------------------------
!     NEXT POINT
            if ( k>=ni ) exit spag_loop_1_1
            k = k + 1
            xx = xi(k)
            if ( xx<x(1) ) then
               spag_nextblock_1 = 3
               cycle spag_dispatchloop_1
            endif
            if ( xx>x(n) ) then
               spag_nextblock_1 = 2
               cycle spag_dispatchloop_1
            endif
            if ( xx<xi(k-1) ) then
               il = 1
               ir = i + 1
               exit spag_loop_1_1
            elseif ( xx/=xi(k-1) ) then
!------------------------------------
!     LINEAR FORWARD SEARCH
               do while ( xx>x(i+1) )
                  if ( i>=nm1 ) then
                     spag_nextblock_1 = 2
                     cycle spag_dispatchloop_1
                  endif
                  i = i + 1
               enddo
            endif
         enddo spag_loop_1_1
         exit spag_dispatchloop_1
      end select
   enddo spag_dispatchloop_1
end subroutine splint
!>
!!##NAME
!!      linearint(3f) - [M_math:fit] interpolates a curve defined by X(i),Y(i) using linear interpolation at given XI(j) values
!!##SYNOPSIS
!!
!!      SUBROUTINE linearint(X,Y,N,XI,YI,NI,KERR)
!!
!!       INTEGER,intent(in)  :: N, NI
!!       REAL,intent(in)     :: X(N),Y(N),XI(NI)
!!       REAL,intent(out)    :: YI(NI)
!!       INTEGER,intent(out) :: KERR
!!##DESCRIPTION
!!##OPTIONS
!!         X      array of data abscissas
!!         Y      array of data ordinates
!!         N      number of data points (the dimension of X,Y)
!!         XI     array of abscissas (in arbitrary order) at which the curve  is to be interpolated.
!!         YI     array of interpolated ordinates (OUTPUT)
!!         NI     dimension of XI, YI (if NI=1, XI, YI  may be simple variables.)
!!         KERR   error status parameter
!!                 NORMAL CODES:
!!                 =0  means the curve was evaluated at each abscissa in XI using only interpolation.
!!                 =1  means the curve was evaluated at each abscissa in XI, but at least one extrapolation was performed.
!!                 ABNORMAL CODES:
!!                 =2  means the requested number of interpolations, NI, was not positive.
!!##RETURNS
!!##EXAMPLE
!!
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        linearint(1)
!! DESCRIPTION:    interpolates a curve <X(i),Y(i)> using linear interpolation at given XI(j) values
!!##VERSION:        1.0, 20031123
!! AUTHOR:         John S. Urban (hacked from splint)
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
! ident_6="@(#) M_math linearint(3f) linear interpolation of curve X(i) Y(i) at given XI(j) values"
subroutine linearint(x,y,n,xi,yi,ni,kerr)
implicit none
!
integer, intent(in) :: n, ni
real, intent(in) :: x(n), y(n), xi(ni)
real, intent(out) :: yi(ni)
integer, intent(out) :: kerr

integer :: k, i, il, ir, nm1
real :: xx, delta, h, v, h2, delta2
integer :: spag_nextblock_1
   spag_nextblock_1 = 1
   spag_dispatchloop_1: do
      select case (spag_nextblock_1)
       case (1)

!     CHECK INPUT
         if ( ni<=0 ) then
            kerr = 2
            return
         endif
         kerr = 0
!
!     K IS INDEX ON VALUE OF XI BEING WORKED ON.  XX IS THAT VALUE.
!     I IS CURRENT INDEX INTO X ARRAY.
         k = 1
         xx = xi(1)
         if ( xx<x(1) ) then  ! extrapolation
            spag_nextblock_1 = 4
            cycle spag_dispatchloop_1
         endif
         if ( xx>x(n) ) then  ! extrapolation
            spag_nextblock_1 = 3
            cycle spag_dispatchloop_1
         endif
         il = 1
         ir = n
         nm1 = n - 1
         spag_nextblock_1 = 2
       case (2)
         do
!
!     BISECTION SEARCH
            i = (il+ir)/2
            if ( i==il ) then
               spag_nextblock_1 = 5
               cycle spag_dispatchloop_1
            endif
            delta = xx - x(i)
            if ( delta<0 ) then
               ir = i
            elseif ( delta>0 ) then
               il = i
            else
               spag_nextblock_1 = 5
               cycle spag_dispatchloop_1
            endif
         enddo
         spag_nextblock_1 = 3
       case (3)
!
!     EXTRAPOLATION
         kerr = 1
         i = nm1
         spag_nextblock_1 = 5
         cycle spag_dispatchloop_1
       case (4)
!
         kerr = 1
         i = 1
         spag_nextblock_1 = 5
       case (5)
         do
!
!     INTERPOLATION
            h = x(i+1) - x(i)
            v = y(i+1) - y(i)
            h2 = xx - x(i)
            yi(k) = y(i) + v*(h2/h)
!     NEXT POINT
            if ( k>=ni ) return
            k = k + 1
            xx = xi(k)
            if ( xx<x(1) ) then
               spag_nextblock_1 = 4
               cycle spag_dispatchloop_1
            endif
            if ( xx>x(n) ) then
               spag_nextblock_1 = 3
               cycle spag_dispatchloop_1
            endif
            delta2 = xx - xi(k-1)
            if ( delta2<0 ) then
               il = 1
               ir = i + 1
               spag_nextblock_1 = 2
               cycle spag_dispatchloop_1
            elseif ( delta2/=0 ) then
!
!     LINEAR FORWARD SEARCH
               do while ( xx>x(i+1) )
                  ! interpolation
                  if ( i>=nm1 ) then
                     ! extrapolation
                     spag_nextblock_1 = 3
                     cycle spag_dispatchloop_1
                  endif
                  i = i + 1        ! go forward again
               enddo
            endif
         enddo
         exit spag_dispatchloop_1
      end select
   enddo spag_dispatchloop_1
!
end subroutine linearint

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    gcsgau1(3f) - [M_math] solve a system of simultaneous linear equations
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine gcsgau1(n,a,b)
!!
!!    integer,parameter  :: dp=kind(0.0d0)
!!    integer,intent(in) :: n
!!    real(kind=dp)      :: a(11,11)
!!    real(kind=dp)      :: b(*)
!!##DESCRIPTION
!!    Solve a system of simultaneous linear equations of the form
!!
!!      **                           **  **    **   **    **
!!      * A(1,1)  A(1,2)  ...  A(1,N) *  * X(1) *   * B(1) *
!!      *                             *  *      *   *      *
!!      * A(2,1)  A(2,2)  ...  A(2,N) *  * X(2) *   * B(2) *
!!      *                             *  *      *   *      *
!!      *    .       .            .   *  *   .  * = *   .  *
!!      *    .       .            .   *  *   .  *   *   .  *
!!      *    .       .            .   *  *   .  *   *   .  *
!!      *                             *  *      *   *      *
!!      * A(N,1)  A(N,2)  ...  A(N,N) *  * X(N) *   * B(N) *
!!      **                           **  **    **   **    **
!!
!!    where matrices A and B are known and matrix X is the set of
!!    unknowns to be determined. N is the number of equations.
!!
!!##PEDIGREE
!!    Based on
!!
!!      Graphics Compatibility System
!!          3-D Device-Dependent
!!           Subroutine GCSGAU1
!!                 Level 7
!!
!!##WRITTEN BY
!!    Fred Taylor, Computer Analysis Branch USAEWES, Vicksburg, MS. 39180
!!##LICENSE
!!    Public Domain
!*==gcsgau1.f90 processed by SPAG 8.01RF 03:35 14 Dec 2024
SUBROUTINE gcsgau1(N,A,B)
   USE m_framework__journal , only:journal
   IMPLICIT NONE
   INTEGER , PARAMETER :: DP = kind(0.0D0)
   INTEGER , INTENT(IN) :: N
   REAL(KIND=DP) :: A(11,11)
   REAL(KIND=DP) :: B(*)

   REAL(KIND=DP) :: amx
   REAL(KIND=DP) :: d
   REAL(KIND=DP) :: eps
   REAL(KIND=DP) :: fa
   REAL(KIND=DP) :: fm
   REAL(KIND=DP) :: sum
   REAL(KIND=DP) :: x(11)
   INTEGER :: i , j , k
   INTEGER :: kpl1
   INTEGER :: m
   INTEGER :: mpl1
   INTEGER :: ncq
   INTEGER :: nm1
!-----------------------------------------------------------------------------------------------------------------------------------
   eps = 1.0D-30
   ! Obtain upper triangular matrix and modified b matrix.
   nm1 = N - 1
   DO k = 1 , nm1
      ! Perform K "th" step of Gauss Elimination.462C
      kpl1 = k + 1
      ! Perform partial pivoting.
      ! Find maximum element in absolute value of the elements, A(K,K),
      ! A(K+1,K), ... A(N,K).
      amx = 0.0D0
      DO i = k , N
         fa = abs(A(i,k))
         IF ( fa>amx ) THEN
            amx = fa
            ncq = i
         ENDIF
      ENDDO

      ! Check for no solution.
      IF ( amx<eps ) THEN

      ! The Gauss Elimination process has broken down because no pivot
      ! greater than the input tolerance could be found for !K! step

         CALL journal('*gauss* elimination process has broken down')
         RETURN
      ELSE

      ! Interchange rows K and NCQ.
         DO j = k , N
            d = A(k,j)
            A(k,j) = A(ncq,j)
            A(ncq,j) = d
         ENDDO
         d = B(k)
         B(k) = B(ncq)
         B(ncq) = d

      ! Perform elimination process.
         DO i = kpl1 , N
         ! Calculate multipliers.
            fm = -A(i,k)/A(k,k)
            DO j = kpl1 , N
               A(i,j) = A(k,j)*fm + A(i,j)
            ENDDO
            B(i) = B(k)*fm + B(i)
         ENDDO
      ENDIF
   ENDDO

   ! Check for no solution.
   IF ( abs(A(N,N))<eps ) THEN

   ! A(N,N) is smaller than the allowable tolerance for a pivot

      CALL journal('*gauss* elimination process has broken down')
      RETURN
   ELSE

   ! Calculate matrix X.

   ! Perform back substitution.
      x(N) = B(N)/A(N,N)
      DO k = 2 , N
         m = N - k + 1
         mpl1 = m + 1
         sum = 0.
         DO j = mpl1 , N
            sum = A(m,j)*x(j) + sum
         ENDDO
         x(m) = (B(m)-sum)/A(m,m)
      ENDDO

      DO i = 1 , N
         B(i) = x(i)
      ENDDO
   ENDIF

END SUBROUTINE gcsgau1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    glstsq(3f) - [M_math] least squares fit to a polynomial expression
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine glstsq(ideg,x,y,n0,d)
!!
!!    integer             :: ideg
!!    real                :: x(*)
!!    real                :: y(*)
!!    integer             :: n0
!!    doubleprecision     :: d(*)
!!
!!##DESCRIPTION
!!    least squares fit to a polynomial expression
!!##OPTIONS
!!    x     X values to fit
!!    y     Y values to fit
!!    ideg  is desired degree of least square fit and test
!!    n0    is number of points in curve arrays (x, y)
!!    d     is returned coefficient array
!!##PEDIGREE
!!   Based on
!!
!!      graphics compatibility system
!!                 basic
!!           subroutine ulstsq
!!               level 13
!!       written by       Fred  Tracy
!!       modified by      John S. Urban
!!##NOTES
!!    needs rewritten to normalize data so large numbers causing overflow
!!    are not generated.
!!##LICENSE
!! Public Domain
subroutine glstsq(ideg,x,y,n0,d)
use M_framework__journal, only : journal
implicit doubleprecision(a-h,o-z)

integer             :: ideg       !* ideg is  desired degree of least square fit and test
real                :: x(*)
real                :: y(*)
integer             :: n0         !* n0   is  number of points in curve arrays (x, y)
doubleprecision     :: d(*)       !* d    is  returned coefficient array
doubleprecision     :: add

doubleprecision     :: a(11,11)
integer             :: i, j, k
integer             :: n
integer             :: nppl1
integer             :: nppl2
integer             :: jm1
integer             :: iz
integer             :: iexp

   n=n0

   if((ideg.lt.1).or.(ideg.gt.10))then
      call journal('*fit* invalid polynomial degree for polynomial fit')
   elseif(n.le.ideg)then ! test if enough points to do desired fit
      call journal('*fit* insufficient points for desired fit')
   else

      nppl1=ideg+1
      nppl2=ideg+2
      do j=1,nppl1
!          calculate  (d)  matrix.
         jm1=j-1

         if (jm1 .le. 0)then
            iz=1
            add=0.0d0
            do i=1,n
               add=y(i)+add
            enddo
         else

            add=0.0d0
            do i=1,n
               add=x(i)**jm1*y(i)+add
            enddo
         endif

         d(j)=add

!     calculate ((a)) matrix.
         do k=j,nppl1
            iexp=jm1+k-1

            if(iz.ne.2)then
               add=n
               iz=2
            else

               add=0.0d0
               do i=1,n
                  add=x(i)**iexp+add
               enddo
            endif

            a(j,k)=add
            a(k,j)=add
         enddo
      enddo

!     solve system of equations
!        ((a)) * (c) = (d)
!      for (c).
!      coefficients will be in array d
      n = nppl1
      call gcsgau1(n,a,d) ! note that d is doubleprecision, a is doubleprecision
   endif
end subroutine glstsq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    gcsgau2(3f) - [M_math] solve a system of simultaneous linear equations
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine gcsgau2(n,a,b)
!!
!!    integer      :: n
!!    real         :: a(11,11)
!!    real         :: b(*)
!!##DESCRIPTION
!!    Solve a system of simultaneous linear equations
!!    of the form
!!
!!      **                           **  **    **   **    **
!!      * A(1,1)  A(1,2)  ...  A(1,N) *  * X(1) *   * B(1) *
!!      *                             *  *      *   *      *
!!      * A(2,1)  A(2,2)  ...  A(2,N) *  * X(2) *   * B(2) *
!!      *                             *  *      *   *      *
!!      *    .       .            .   *  *   .  * = *   .  *
!!      *    .       .            .   *  *   .  *   *   .  *
!!      *    .       .            .   *  *   .  *   *   .  *
!!      *                             *  *      *   *      *
!!      * A(N,1)  A(N,2)  ...  A(N,N) *  * X(N) *   * B(N) *
!!      **                           **  **    **   **    **
!!
!!    Where matrices A and B are known and matrix X is the set of
!!    unknowns to be determined. N is the number of equations.
!!
!!##PEDIGREE
!!    Derived from
!!
!!         Graphics Compatibility System
!!             3-D Device-Dependent
!!               Subroutine GCSGAU2
!!                    Level 7
!!##WRITTEN BY
!!    Fred Taylor ; A.K.A
!!
!!    F. T. Tracy, Computer Analysis Branch USAEWES, Vicksburg, MS. 39180
!!##LICENSE
!!    Public Domain
!*==gcsgau2.f90 processed by SPAG 8.01RF 03:35 14 Dec 2024
SUBROUTINE gcsgau2(N,A,B)
   USE m_framework__journal , only:journal
   IMPLICIT NONE
   INTEGER :: N
   REAL :: A(11,11)
   REAL :: B(*)

   INTEGER :: i
   INTEGER :: j
   INTEGER :: k
   INTEGER :: kpl1
   INTEGER :: m
   INTEGER :: mpl1
   INTEGER :: ncq
   INTEGER :: nm1
   REAL :: amx
   REAL :: d
   REAL :: eps
   REAL :: fa
   REAL :: fm
   REAL :: sum
   REAL :: x(11)
!-----------------------------------------------------------------------
   eps = 1.0D-30
!     OBTAIN UPPER TRIANGULAR MATRIX AND MODIFIED B MATRIX.
   nm1 = N - 1
   DO k = 1 , nm1
!     PERFORM K "TH" STEP OF GAUSS ELIMINATION.462C
      kpl1 = k + 1
!     PERFORM PARTIAL PIVOTING.
!     FIND MAXIMUM ELEMENT IN ABSOLUTE VALUE OF THE ELEMENTS, A(K,K),
!     A(K+1,K), ... A(N,K).
      amx = 0.0D0
      DO i = k , N
         fa = abs(A(i,k))
         IF ( fa>amx ) THEN
            amx = fa
            ncq = i
         ENDIF
      ENDDO
!
!     CHECK FOR NO SOLUTION.
      IF ( amx<eps ) THEN
!
!     THE GAUSS ELIMINATION PROCESS HAS BROKEN DOWN BECAUSE NO PIVOT
!     GREATER THAN THE INPUT TOLERANCE COULD BE FOUND FOR !K! STEP
!
         CALL journal('*gauss* elimination process has broken down')
         RETURN
      ELSE
!
!     INTERCHANGE ROWS K AND NCQ.
         DO j = k , N
            d = A(k,j)
            A(k,j) = A(ncq,j)
            A(ncq,j) = d
         ENDDO
         d = B(k)
         B(k) = B(ncq)
         B(ncq) = d
!
!     PERFORM ELIMINATION PROCESS.
         DO i = kpl1 , N
!
!     CALCULATE MULTIPLIERS.
            fm = -A(i,k)/A(k,k)
!
            DO j = kpl1 , N
               A(i,j) = A(k,j)*fm + A(i,j)
            ENDDO
            B(i) = B(k)*fm + B(i)
         ENDDO
      ENDIF
   ENDDO
!
!     CHECK FOR NO SOLUTION.
   IF ( abs(A(N,N))<eps ) THEN
!
!      A(N,N) IS SMALLER THAN THE ALLOWABLE
!      TOLERANCE FOR A PIVOT
!
      CALL journal('*gauss* elimination process has broken down')
      RETURN
   ELSE
!
!     CALCULATE MATRIX X.
!
!     PERFORM BACK SUBSTITUTION.
      x(N) = B(N)/A(N,N)
      DO k = 2 , N
         m = N - k + 1
         mpl1 = m + 1
         sum = 0.0
         DO j = mpl1 , N
            sum = A(m,j)*x(j) + sum
         ENDDO
         x(m) = (B(m)-sum)/A(m,m)
      ENDDO
!
      DO i = 1 , N
         B(i) = x(i)
      ENDDO
   ENDIF
!
END SUBROUTINE gcsgau2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    ju_polfit(3f) - [M_math] Fit discrete data in a least squares sense by polynomials in one variable.
!!##SYNOPSIS
!!
!!    SUBROUTINE JU_POLFIT (N, X, Y, W, MAXDEG, NDEG, EPS, R, IERR, A)
!!##DESCRIPTION
!!
!!    Given a collection of points X(I) and a set of values Y(I) which
!!    correspond to some function or measurement at each of the X(I),
!!    subroutine JU_POLFIT computes the weighted least-squares polynomial
!!    fits of all degrees up to some degree either specified by the user or
!!    determined by the routine. The fits thus obtained are in orthogonal
!!    polynomial form. Subroutine JU_PVALUE may then be called to evaluate
!!    the fitted polynomials and any of their derivatives at any point. The
!!    subroutine PCOEF may be used to express the polynomial fits as powers
!!    of (X-C) for any specified point C.
!!##OPTIONS
!!      The parameters for JU_POLFIT are
!!
!!          N -      the number of data points. The arrays X, Y and W
!!                   must be dimensioned at least N (N .GE. 1).
!!          X -      array of values of the independent variable. These
!!                   values may appear in any order and need not all be
!!                   distinct.
!!          Y -      array of corresponding function values.
!!          W -      array of positive values to be used as weights. If
!!                   W(1) is negative, JU_POLFIT will set all the weights
!!                   to 1.0, which means unweighted least squares error
!!                   will be minimized. To minimize relative error, the
!!                   user should set the weights to: W(I) = 1.0/Y(I)**2,
!!                   I = 1,...,N .
!!          MAXDEG - maximum degree to be allowed for polynomial fit.
!!                   MAXDEG may be any non-negative integer less than N.
!!                   Note -- MAXDEG cannot be equal to N-1 when a
!!                   statistical test is to be used for degree selection,
!!                   i.e., when input value of EPS is negative.
!!          EPS -    specifies the criterion to be used in determining
!!                   the degree of fit to be computed.
!!                    1. If EPS is input negative, JU_POLFIT chooses the
!!                       degree based on a statistical F test of
!!                       significance. One of three possible
!!                       significance levels will be used: .01, .05 or
!!                       .10. If EPS=-1.0 , the routine will
!!                       automatically select one of these levels based
!!                       on the number of data points and the maximum
!!                       degree to be considered. If EPS is input as
!!                       -.01, -.05, or -.10, a significance level of
!!                       .01, .05, or .10, respectively, will be used.
!!                    2. If EPS is set to 0., JU_POLFIT computes the
!!                        polynomials of degrees 0 through MAXDEG .
!!                    3. If EPS is input positive, EPS is the RMS
!!                       error tolerance which must be satisfied by the
!!                       fitted polynomial. JU_POLFIT will increase the
!!                       degree of fit until this criterion is met or
!!                       until the maximum degree is reached.
!!
!!##OUTPUT
!!          NDEG     degree of the highest degree fit computed.
!!          EPS      RMS error of the polynomial of degree NDEG .
!!          R        vector of dimension at least NDEG containing values
!!                   of the fit of degree NDEG at each of the X(I) .
!!                   Except when the statistical test is used, these
!!                   values are more accurate than results from subroutine
!!                   JU_PVALUE normally are.
!!          IERR     error flag with the following possible values.
!!
!!              1 -- indicates normal execution, i.e., either
!!                   (1)  the input value of EPS was negative, and the
!!                        computed polynomial fit of degree NDEG
!!                        satisfies the specified F test, or
!!                   (2)  the input value of EPS was 0., and the fits of
!!                        all degrees up to MAXDEG are complete, or
!!                   (3)  the input value of EPS was positive, and the
!!                        polynomial of degree NDEG satisfies the RMS
!!                        error requirement.
!!              2 -- invalid input parameter. At least one of the input
!!                   parameters has an illegal value and must be corrected
!!                   before JU_POLFIT can proceed. Valid input results
!!                   when the following restrictions are observed
!!                        N .GE. 1
!!                        0 .LE. MAXDEG .LE. N-1 for EPS .GE. 0.
!!                        0 .LE. MAXDEG .LE. N-2 for EPS .LT. 0.
!!                        W(1)=-1.0 or W(I) .GT. 0., I=1,...,N .
!!              3 -- cannot satisfy the RMS error requirement with a
!!                   polynomial of degree no greater than MAXDEG. Best
!!                   fit found is of degree MAXDEG .
!!              4 -- cannot satisfy the test for significance using
!!                   current value of MAXDEG . Statistically, the
!!                   best fit found is of order NORD . (In this case,
!!                   NDEG will have one of the values: MAXDEG-2,
!!                   MAXDEG-1, or MAXDEG). Using a higher value of
!!                   MAXDEG may result in passing the test.
!!          A        work and output array having at least 3N+3MAXDEG+3
!!                   locations
!!
!!   NOTE
!!    JU_POLFIT calculates all fits of degrees up to and including NDEG. Any
!!    or all of these fits can be evaluated or expressed as powers of (X-C)
!!    using JU_PVALUE and PCOEF after just one call to JU_POLFIT.
!!##PEDIGREE
!!    * PURPOSE   @(#) Fit discrete data in a least squares sense by polynomials in one variable.
!!    * LIBRARY   SLATEC
!!    * CATEGORY  K1A1A2
!!    * TYPE      SINGLE PRECISION (JU_POLFIT-S, DPOLFT-D)
!!    * KEYWORDS  CURVE FITTING, DATA FITTING, LEAST SQUARES, POLYNOMIAL FIT
!!    * AUTHOR    Shampine, L. F., (SNLA)
!!                Davenport, S. M., (SNLA)
!!                Huddleston, R. E., (SNLL)
!!
!! *REVISION HISTORY
!!   (YYMMDD)
!!
!!    740601  Date written
!!    890531  Changed all specific intrinsics to generic. (WRB)
!!    890531  REVISION DATE from Version 3.2
!!    891214  Prologue converted to Version 4.0 format. (BAB)
!!    900315  CALLs to XERROR changed to CALLs to JU_XERMSG. (THJ)
!!    920501  Reformatted the REFERENCES section. (WRB)
!!    920527  Corrected erroneous statements in DESCRIPTION. (WRB)
!!
!!##ROUTINES CALLED
!!    ju_pvalue(3f), ju_xermsg(3f)
!!##REFERENCES
!!    L. F. Shampine, S. M. Davenport and R. E. Huddleston,
!!    Curve fitting by polynomials in one variable, Report
!!    SLA-74-0270, Sandia Laboratories, June 1974.
subroutine ju_polfit(n,x,y,w,maxdeg,ndeg,eps,r,ierr,a)
implicit none
integer :: i100
integer :: i20
integer :: i200
integer :: i30
integer :: i300
integer :: i40
integer :: i400
integer :: i50
integer :: i500
integer :: i60
integer :: i70
integer :: i80
integer :: i88
integer :: i90
integer :: idegf
integer :: ierr
integer :: iii
integer :: j
integer :: jp1
integer :: jpas
integer :: k1
integer :: k1pj
integer :: k2
integer :: k2pj
integer :: k3
integer :: k3pi
integer :: k4
integer :: k4pi
integer :: k5
integer :: k5pi
integer :: ksig
integer :: m
integer :: maxdeg
integer :: mop1
integer :: n
integer :: ndeg
integer :: nder
integer :: nfail
real :: a
real :: co
real :: degf
real :: den
real :: eps
real :: etst
real :: f
real :: fcrit
real :: r
real :: sig
real :: sigj
real :: sigjm1
real :: sigpas
real :: temp
real :: w
real :: w1
real :: w11
real :: x
real :: xm
real :: y
real :: yp(0)
double precision temd1, temd2
dimension x(*), y(*), w(*), r(*), a(*)
dimension co(4,3)
save co
integer :: nextblock_1
data co(1,1), co(2,1), co(3,1), co(4,1), co(1,2), co(2,2), co(3,2), co(4,2), co(1,3), co(2,3), co(3,3), co(4,3)  &
& / - 13.086850, -2.4648165, -3.3846535, -1.2973162, -3.3381146, -1.7812271, -3.2578406, -1.6589279, -1.6282703, &
& -1.3152745, -3.2640179, -1.9829776/
   nextblock_1 = 1
   dispatchloop_1: do
!=======================================================================
    select case (nextblock_1)
!=======================================================================
       case (1)
         write (*,*) 'JU_POLFIT N=', n

         do i90 = 1, n
            write (*,*) i90, x(i90), y(i90), w(i90), r(i90)
         enddo

         write (*,*) 'MAXDEG=', maxdeg
         write (*,*) 'eps=', eps
         m = abs(n)
         if ( m==0 ) then
            nextblock_1 = 8
            cycle dispatchloop_1
         endif
         if ( maxdeg<0 ) then
            nextblock_1 = 8
            cycle dispatchloop_1
         endif
         a(1) = maxdeg
         mop1 = maxdeg + 1
         if ( m<mop1 ) then
            nextblock_1 = 8
            cycle dispatchloop_1
         endif
         if ( eps<0.0 .and. m==mop1 ) then
            nextblock_1 = 8
            cycle dispatchloop_1
         endif
         xm = m
         etst = eps*eps*xm
         if ( w(1)>=0.0 ) then
            do i20 = 1, m
               if ( w(i20)<=0.0 ) then
                  nextblock_1 = 8
                  cycle dispatchloop_1
               endif
            enddo
         else
            do i30 = 1, m
               w(i30) = 1.0
            enddo
         endif

         if ( eps<0.0 ) then
!
! DETERMINE SIGNIFICANCE LEVEL INDEX TO BE USED IN STATISTICAL TEST FOR
! CHOOSING DEGREE OF POLYNOMIAL FIT
!
            if ( eps>(-.55) ) then
               ksig = 1
               if ( eps<(-.03) ) ksig = 2
               if ( eps<(-.07) ) ksig = 3
            else
               idegf = m - maxdeg - 1
               ksig = 1
               if ( idegf<10 ) ksig = 2
               if ( idegf<5 ) ksig = 3
            endif
         endif
!
! INITIALIZE INDEXES AND COEFFICIENTS FOR FITTING
!
         k1 = maxdeg + 1
         k2 = k1 + maxdeg
         k3 = k2 + maxdeg + 2
         k4 = k3 + m
         k5 = k4 + m

         do i40 = 2, k4
            a(i40) = 0.0
         enddo

         w11 = 0.0
         if ( n<0 ) then
!
! CONSTRAINED CASE
!
            do i60 = 1, m
               k4pi = k4 + i60
               w11 = w11 + w(i60)*a(k4pi)**2
            enddo
         else
!
! UNCONSTRAINED CASE
!
            do i50 = 1, m
               k4pi = k4 + i50
               a(k4pi) = 1.0
               w11 = w11 + w(i50)

            enddo
         endif
!
! COMPUTE FIT OF DEGREE ZERO
!
         temd1 = 0.0d0
         do i70 = 1, m
            k4pi = k4 + i70
            temd1 = temd1 + dble(w(i70))*dble(y(i70))*dble(a(k4pi))
         enddo
         temd1 = temd1/dble(w11)
         a(k2+1) = temd1
         sigj = 0.0
         do i80 = 1, m
            k4pi = k4 + i80
            k5pi = k5 + i80
            temd2 = temd1*dble(a(k4pi))
            r(i80) = temd2
            a(k5pi) = temd2 - dble(r(i80))
            sigj = sigj + w(i80)*((y(i80)-r(i80))-a(k5pi))**2
         enddo
         j = 0
!
! SEE IF POLYNOMIAL OF DEGREE 0 SATISFIES THE DEGREE SELECTION CRITERION
!
         if ( eps<0 ) then
            nextblock_1 = 3
            cycle dispatchloop_1
         endif
         if ( eps==0 ) then
            nextblock_1 = 4
            cycle dispatchloop_1
         endif
         nextblock_1 = 5
         cycle dispatchloop_1
!=======================================================================
       case (2)
         loop_1_1: do
!
! INCREMENT DEGREE
!
            j = j + 1
            jp1 = j + 1
            k1pj = k1 + j
            k2pj = k2 + j
            sigjm1 = sigj
!
! COMPUTE NEW B COEFFICIENT EXCEPT WHEN J = 1
!
            if ( j>1 ) a(k1pj) = w11/w1
!
! COMPUTE NEW A COEFFICIENT
!
            temd1 = 0.0d0
            do i100 = 1, m
               k4pi = k4 + i100
               temd2 = a(k4pi)
               temd1 = temd1 + dble(x(i100))*dble(w(i100))*temd2*temd2
            enddo
            a(jp1) = temd1/dble(w11)
!
! EVALUATE ORTHOGONAL POLYNOMIAL AT DATA POINTS
!
            w1 = w11
            w11 = 0.0
            do i200 = 1, m
               k3pi = k3 + i200
               k4pi = k4 + i200
               temp = a(k3pi)
               a(k3pi) = a(k4pi)
               a(k4pi) = (x(i200)-a(jp1))*a(k3pi) - a(k1pj)*temp
               w11 = w11 + w(i200)*a(k4pi)**2
            enddo
!
! GET NEW ORTHOGONAL POLYNOMIAL COEFFICIENT USING PARTIAL DOUBLE
! PRECISION
!
            temd1 = 0.0d0
            do i300 = 1, m
               k4pi = k4 + i300
               k5pi = k5 + i300
               temd2 = dble(w(i300))*dble((y(i300)-r(i300))-a(k5pi))*dble(a(k4pi))
               temd1 = temd1 + temd2
            enddo
            temd1 = temd1/dble(w11)
            a(k2pj+1) = temd1
!
! UPDATE POLYNOMIAL EVALUATIONS AT EACH OF THE DATA POINTS, AND
! ACCUMULATE SUM OF SQUARES OF ERRORS.  THE POLYNOMIAL EVALUATIONS ARE
! COMPUTED AND STORED IN EXTENDED PRECISION.  FOR THE I-TH DATA POINT,
! THE MOST SIGNIFICANT BITS ARE STORED IN  R(I), AND THE LEAST
! SIGNIFICANT BITS ARE IN  A(K5PI) .
!
            sigj = 0.0
            do i400 = 1, m
               k4pi = k4 + i400
               k5pi = k5 + i400
               temd2 = dble(r(i400)) + dble(a(k5pi)) + temd1*dble(a(k4pi))
               r(i400) = temd2
               a(k5pi) = temd2 - dble(r(i400))
               sigj = sigj + w(i400)*((y(i400)-r(i400))-a(k5pi))**2
            enddo
!
! SEE IF DEGREE SELECTION CRITERION HAS BEEN SATISFIED OR IF DEGREE
! MAXDEG  HAS BEEN REACHED
!
!=======================================================================
            if ( eps<0 ) then
!
! COMPUTE F STATISTICS  (INPUT EPS .LT. 0.)
!
               if ( sigj==0.0 ) then
                  nextblock_1 = 7
                  cycle dispatchloop_1
               endif
               degf = m - j - 1
               den = (co(4,ksig)*degf+1.0)*degf
               fcrit = (((co(3,ksig)*degf)+co(2,ksig))*degf+co(1,ksig))/den
               fcrit = fcrit*fcrit
               f = (sigjm1-sigj)*degf/sigj
               if ( f>=fcrit ) exit loop_1_1
!=======================================================================
!
! POLYNOMIAL OF DEGREE J FAILS F TEST.  IF THERE HAVE BEEN THREE
! SUCCESSIVE FAILURES, A STATISTICALLY BEST DEGREE HAS BEEN FOUND.
!
               nfail = nfail + 1
               if ( nfail>=3 ) then
                  nextblock_1 = 7
                  cycle dispatchloop_1
               endif
               if ( maxdeg==j ) then
                  nextblock_1 = 9
                  cycle dispatchloop_1
               endif
               iii = 25
            elseif ( eps==0 ) then
               nextblock_1 = 4
               cycle dispatchloop_1
            else
               nextblock_1 = 5
               cycle dispatchloop_1
            endif
         enddo loop_1_1
         nextblock_1 = 3
       case (3)
!
! POLYNOMIAL OF DEGREE J SATISFIES F TEST
!
         sigpas = sigj
         jpas = j
         nfail = 0
         if ( maxdeg==j ) then
            nextblock_1 = 9
            cycle dispatchloop_1
         endif
         iii = 24
         nextblock_1 = 2
         cycle dispatchloop_1
!=======================================================================
       case (4)
!
! RAISE THE DEGREE IF DEGREE  MAXDEG  HAS NOT YET BEEN REACHED  (INPUT EPS = 0.)
!
         if ( maxdeg==j ) then
            nextblock_1 = 6
            cycle dispatchloop_1
         endif
         iii = 26
         nextblock_1 = 2
         cycle dispatchloop_1
!=======================================================================
       case (5)
!
! SEE IF RMS ERROR CRITERION IS SATISFIED  (INPUT EPS .GT. 0.)
!
         if ( sigj>etst ) then
            if ( maxdeg==j ) then
               ierr = 3
               ndeg = maxdeg
               sig = sigj
               nextblock_1 = 10
               cycle dispatchloop_1
            else
               iii = 27
               nextblock_1 = 2
               cycle dispatchloop_1
            endif
         endif
         nextblock_1 = 6
!=======================================================================
       case (6)
! RETURNS
         ierr = 1
         ndeg = j
         sig = sigj
         nextblock_1 = 10
         cycle dispatchloop_1
!=======================================================================
       case (7)
         ierr = 1
         ndeg = jpas
         sig = sigpas
         nextblock_1 = 10
         cycle dispatchloop_1
!=======================================================================
       case (8)
         ierr = 2
         call ju_xermsg('SLATEC','JU_POLFIT','INVALID INPUT PARAMETER.',2,1)
         nextblock_1 = 11
         cycle dispatchloop_1
!=======================================================================
       case (9)
         ierr = 4
         ndeg = jpas
         sig = sigpas
         nextblock_1 = 10
!=======================================================================
       case (10)
         a(k3) = ndeg
!
! WHEN STATISTICAL TEST HAS BEEN USED, EVALUATE THE BEST POLYNOMIAL AT
! ALL THE DATA POINTS IF  R  DOES NOT ALREADY CONTAIN THESE VALUES
!
         if ( eps>=0.0 .or. ndeg==maxdeg ) then
            eps = sqrt(sig/xm)
            nextblock_1 = 11
            cycle dispatchloop_1
         endif
         nder = 0
         do i500 = 1, m
            call ju_pvalue(ndeg,nder,x(i500),r(i500),yp,a)
         enddo
         eps = sqrt(sig/xm)
         nextblock_1 = 11
!=======================================================================
       case (11)
         write (*,*) 'exiting ju_polfit'
         write (*,*) 'ndeg=', ndeg
         write (*,*) 'eps(RMS error)=', eps
         do i88 = 1, ndeg
            write (*,*) i88, r(i88)
         enddo
         write (*,*) 'ierr=', ierr
         exit dispatchloop_1
      end select
!=======================================================================
   enddo dispatchloop_1

end subroutine ju_polfit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*DECK XERMSG
!
!   XERMSG processes a diagnostic message in a manner determined by the
!   value of LEVEL and the current value of the library error control
!   flag, KONTRL.  See subroutine XSETF for details.
!
!    LIBRAR   A character constant (or character variable) with the name
!             of the library.
!
!    SUBROU   A character constant (or character variable) with the name
!             of the routine that detected the error.  Usually it is the
!             name of the routine that is calling XERMSG.  There are
!             some instances where a user callable library routine calls
!             lower level subsidiary routines where the error is
!             detected.  In such cases it may be more informative to
!             supply the name of the routine the user called rather than
!             the name of the subsidiary routine that detected the
!             error.
!
!    MESSG    A character constant (or character variable) with the text
!             of the error or warning message.  In the example below,
!             the message is a character constant that contains a
!             generic message.
!
!                   CALL JU_XERMSG ('SLATEC', 'MMPY',
!                  *'THE ORDER OF THE MATRIX EXCEEDS THE ROW DIMENSION',
!                  *3, 1)
!
!
!    NERR     An integer value that is chosen by the library routine's
!             author.  It must be in the range -99 to 999 (three
!             printable digits).  Each distinct error should have its
!             own error number.  These error numbers should be described
!             in the machine readable documentation for the routine.
!             The error numbers need be unique only within each routine,
!             so it is reasonable for each routine to start enumerating
!             errors from 1 and proceeding to the next integer.
!
!    LEVEL    An integer value in the range 0 to 2 that indicates the
!             level (severity) of the error.  Their meanings are
!
!            -1  A warning message.  This is used if it is not clear
!                that there really is an error, but the user's attention
!                may be needed.  An attempt is made to only print this
!                message once.
!
!             0  A warning message.  This is used if it is not clear
!                that there really is an error, but the user's attention
!                may be needed.
!
!             1  A recoverable error.  This is used even if the error is
!                so serious that the routine cannot return any useful
!                answer.  If the user has told the error package to
!                return after recoverable errors, then XERMSG will
!                return to the Library routine which can then return to
!                the user's routine.  The user may also permit the error
!                package to terminate the program upon encountering a
!                recoverable error.
!
!             2  A fatal error.  XERMSG will not return to its caller
!                after it receives a fatal error.  This level should
!                hardly ever be used; it is much better to allow the
!                user a chance to recover.  An example of one of the few
!                cases in which it is permissible to declare a level 2
!                error is a reverse communication Library routine that
!                is likely to be called repeatedly until it integrates
!                across some interval.  If there is a serious error in
!                the input such that another step cannot be taken and
!                the Library routine is called again without the input
!                error having been corrected by the caller, the Library
!                routine will probably be called forever with improper
!                input.  In this case, it is reasonable to declare the
!                error to be fatal.
!
subroutine ju_xermsg (librar, subrou, messg, nerr, level)
!***PURPOSE  Process error messages for SLATEC and other libraries.
use M_framework__journal, only : journal
character(len=*),intent(in)  :: librar
character(len=*),intent(in)  :: subrou
character(len=*),intent(in)  :: messg
integer,intent(in)           :: nerr
integer,intent(in)           :: level
character(len=255)           :: line
character(len=255)           :: lineold=''
   write(line,101)librar,subrou,messg,nerr,level
   101 format('*',a,'*::routine:',a,':',a,' errno:',i3,' level:',i1)
   select case(level)
   case(-1)
      if(line.ne.lineold)then
         call journal(line)
         call journal('warning error')
      endif
   case(0)
      call journal(line)
      call journal('warning error')
   case(1)
      call journal(line)
      call journal('potentially recoverable warning error')
   case(2)
      call journal(line)
      call journal('fatal error')
      !*!call abort()
      stop
   case default
      call journal(line)
      call journal('sc','fatal error : unknown code =',level)
      !*!call abort()
      stop
   end select
end subroutine ju_xermsg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*DECK JU_PVALUE
subroutine ju_pvalue(l,nder,x,yfit,yp,a)
   implicit none
!***BEGIN PROLOGUE  JU_PVALUE
!***PURPOSE  Use the coefficients generated by POLFIT to evaluate the
!            polynomial fit of degree L, along with the first NDER of
!            its derivatives, at a specified point.
!***LIBRARY   SLATEC
!***CATEGORY  K6
!***TYPE      SINGLE PRECISION (JU_PVALUE-S, DP1VLU-D)
!***KEYWORDS  CURVE FITTING, LEAST SQUARES, POLYNOMIAL APPROXIMATION
!***AUTHOR  Shampine, L. F., (SNLA)
!           Davenport, S. M., (SNLA)
!***DESCRIPTION
!
!     Written by L. F. Shampine and S. M. Davenport.
!
!     Abstract
!
!     The subroutine  JU_PVALUE  uses the coefficients generated by  POLFIT
!     to evaluate the polynomial fit of degree  L, along with the first
!     NDER  of its derivatives, at a specified point.  Computationally
!     stable recurrence relations are used to perform this task.
!
!     The parameters for  JU_PVALUE  are
!
!     Input --
!         L -      the degree of polynomial to be evaluated.  L  may be
!                  any non-negative integer which is less than or equal
!                  to  NDEG, the highest degree polynomial provided
!                  by  POLFIT .
!         NDER -   the number of derivatives to be evaluated.  NDER
!                  may be 0 or any positive value.  If NDER is less
!                  than 0, it will be treated as 0.
!         X -      the argument at which the polynomial and its
!                  derivatives are to be evaluated.
!         A -      work and output array containing values from last
!                  call to  POLFIT .
!
!     Output --
!         YFIT -   value of the fitting polynomial of degree  L  at  X
!         YP -     array containing the first through  NDER  derivatives
!                  of the polynomial of degree  L .  YP  must be
!                  dimensioned at least  NDER  in the calling program.
!
!***REFERENCES  L. F. Shampine, S. M. Davenport and R. E. Huddleston,
!                 Curve fitting by polynomials in one variable, Report
!                 SLA-74-0270, Sandia Laboratories, June 1974.
!***ROUTINES CALLED  XERMSG
!***REVISION HISTORY  (YYMMDD)
!   740601  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890531  REVISION DATE from Version 3.2
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
!   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
!   920501  Reformatted the REFERENCES section.  (WRB)
!***END PROLOGUE  JU_PVALUE
   dimension yp(*), a(*)
   character*8 xern1, xern2
   integer :: i10
   integer :: i20
   integer :: i30
   integer :: i40
   integer :: i50
   integer :: ic
   integer :: ilo
   integer :: in
   integer :: inp1
   integer :: iup
   integer :: k1
   integer :: k1i
   integer :: k2
   integer :: k3
   integer :: k3p1
   integer :: k3pn
   integer :: k4
   integer :: k4p1
   integer :: k4pn
   integer :: kc
   integer :: l
   integer :: lm1
   integer :: lp1
   integer :: maxord
   integer :: nder
   integer :: ndo
   integer :: ndp1
   integer :: nord
   real :: a
   real :: cc
   real :: dif
   real :: val
   real :: x
   real :: yfit
   real :: yp
!***FIRST EXECUTABLE STATEMENT  JU_PVALUE
   if ( l<0 ) then
!=======================================================================
!
      call ju_xermsg('SLATEC','JU_PVALUE','INVALID INPUT PARAMETER.  ORDER OF POLYNOMIAL EVALUATION '//  &
      &'REQUESTED IS NEGATIVE -- EXECUTION TERMINATED.',2,2)
      return
   else
      ndo = max(nder,0)
      ndo = min(ndo,l)
      maxord = a(1) + 0.5
      k1 = maxord + 1
      k2 = k1 + maxord
      k3 = k2 + maxord + 2
      nord = a(k3) + 0.5
      if ( l<=nord ) then
         k4 = k3 + l + 1
         if ( nder>=1 ) then

            do i10 = 1, nder
               yp(i10) = 0.0
            enddo
         endif

         if ( l>=2 ) then
!
! L IS GREATER THAN 1
!
            ndp1 = ndo + 1
            k3p1 = k3 + 1
            k4p1 = k4 + 1
            lp1 = l + 1
            lm1 = l - 1
            ilo = k3 + 3
            iup = k4 + ndp1

            do i20 = ilo, iup
               a(i20) = 0.0
            enddo

            dif = x - a(lp1)
            kc = k2 + lp1
            a(k4p1) = a(kc)
            a(k3p1) = a(kc-1) + dif*a(k4p1)
            a(k3+2) = a(k4p1)
!
! EVALUATE RECURRENCE RELATIONS FOR FUNCTION VALUE AND DERIVATIVES
!
            do i30 = 1, lm1
               in = l - i30
               inp1 = in + 1
               k1i = k1 + inp1
               ic = k2 + in
               dif = x - a(inp1)
               val = a(ic) + dif*a(k3p1) - a(k1i)*a(k4p1)
               if ( ndo>0 ) then

                  do i50 = 1, ndo
                     k3pn = k3p1 + i50
                     k4pn = k4p1 + i50
                     yp(i50) = dif*a(k3pn) + i50*a(k3pn-1) - a(k1i)*a(k4pn)
                  enddo

!
! SAVE VALUES NEEDED FOR NEXT EVALUATION OF RECURRENCE RELATIONS
!
                  do i40 = 1, ndo
                     k3pn = k3p1 + i40
                     k4pn = k4p1 + i40
                     a(k4pn) = a(k3pn)
                     a(k3pn) = yp(i40)
                  enddo
               endif

               a(k4p1) = a(k3p1)
               a(k3p1) = val
            enddo
         elseif ( l==1 ) then
!
! L IS 1
!
            cc = a(k2+2)
            val = a(k2+1) + (x-a(2))*cc
            if ( nder>=1 ) yp(1) = cc
         else
!
! L IS 0
!
            val = a(k2+1)
         endif
!=======================================================================
!
! NORMAL RETURN OR ABORT DUE TO ERROR
!
         yfit = val
         return
      endif
   endif
!=======================================================================
   write (xern1,'(I8)') l
   write (xern2,'(I8)') nord
   call ju_xermsg('SLATEC','JU_PVALUE','THE ORDER OF POLYNOMIAL EVALUATION, L = '//xern1// &
   &' REQUESTED EXCEEDS THE HIGHEST ORDER FIT, NORD = '//xern2//', COMPUTED BY POLFIT -- EXECUTION TERMINATED.',8,2)
   return
!=======================================================================
end subroutine ju_pvalue
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    qhfg(3f) - [M_math:integral] compute integral values for given general table of argument, function, and derivative values.
!!##SYNOPSIS
!!
!!   subroutine qhfg(x,y,dery,z,ndim)
!!
!!    real          :: x(*)
!!    real          :: y(*)
!!    real          :: dery(*)
!!    real          :: z(*)
!!    integer       :: ndim
!!##PURPOSE
!!    Compute vector of integral values for given general table
!!    of argument, function, and derivative values.
!!
!!##DESCRIPTION OF PARAMETERS
!!       X       The input vector of argument values.
!!       Y       The input vector of function values.
!!       DERY    The input vector of derivative values.
!!       Z       The resulting vector of integral values. Z may be
!!               identical with X,Y or DERY.
!!       NDIM    The dimension of vectors X,Y,dery,Z.
!!
!!    REMARKS
!!       NO action in case ndim less than 1.
!!
!!    SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!!       None
!!
!!    METHOD
!!    Beginning with Z(1)=0, evaluation of vector Z is done by
!!    means of Hermitean fourth order integration formula.
!!    For reference, see
!!
!!    1. F.B.Hildebrand, Introduction to Numerical Analysis,
!!       McGraw-Hill, New York/Toronto/London, 1956, pp.314-319.
!!    2. R.Zurmuehl, Praktische Mathematik fuer Ingenieure und
!!       Physiker, Springer, Berlin/Goettingen/Heidelberg, 1963,
!!       pp.227-230.
subroutine qhfg(x,y,dery,z,ndim)
real    :: x(*)
real    :: y(*)
real    :: dery(*)
real    :: z(*)
integer :: ndim
integer :: i
real    :: sum1
real    :: sum2

   sum2=0.0
   if((ndim-1).eq.0)then
      z(ndim)=sum2
   elseif((ndim-1).gt.0)then
      ! integration loop
      do i=2,ndim
         sum1=sum2
         sum2=0.5*(x(i)-x(i-1))
         sum2=sum1+sum2*((y(i)+y(i-1))+0.3333333*sum2*(dery(i-1)-dery(i)))
         z(i-1)=sum1
      enddo
   endif

end subroutine qhfg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    qhsg(3f) - [M_math] Compute integral values for given table of
!!    argument, function, 1st derivative, and 2nd derivative values.
!!
!!##SYNOPSIS
!!
!!    subroutine qhsg(x,y,fdy,sdy,z,ndim)
!!
!!##PURPOSE
!!    Compute vector of integral values for given general table of argument,
!!    function, first derivative, and second derivative values.
!!##USAGE
!!    call qhsg (X,Y,FDY,SDY,Z,NDIM)
!!
!!  DESCRIPTION OF PARAMETERS
!!     X      - The input vector of argument values.
!!     Y      - The input vector of function values.
!!     FDY    - The input vector of first derivative.
!!     SDY    - The input vector of second derivative.
!!     Z      - The resulting vector of integral values. Z may be
!!              identical with X,Y,FDY or SDY.
!!     NDIM   - The dimension of vectors X,Y,FDY,SDY,Z.
!!
!!##REMARKS
!!    No action in case NDIM less than 1.
!!
!!##SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!!    None
!!
!!##METHOD
!!    Beginning with Z(1)=0, evaluation of vector Z is done by
!!    means of Hermitean sixth order integration formula.
!!    For reference, see
!!
!!       R.Zurmuehl, Praktische Mathematik fuer Ingenieure und
!!       Physiker, Springer, Berlin/Goettingen/Heidelberg, 1963,
!!       pp.227-230.
!*==qhsg.f90 processed by SPAG 8.01RF 04:52 14 Dec 2024
subroutine qhsg(x,y,fdy,sdy,z,ndim)
implicit none
real, intent(in)    :: x(*)
real, intent(in)    :: y(*)
real, intent(in)    :: fdy(*)
real, intent(in)    :: sdy(*)
real, intent(out)   :: z(*)
integer, intent(in) :: ndim
integer             :: i
real                :: sum1
real                :: sum2
   sum2 = 0.0

   if ( ndim < 1 ) return

   if ( ndim /= 1 ) then

!     integration loop
      do i = 2, ndim
         sum1 = sum2
         sum2 = .5*(x(i)-x(i-1))
         sum2 = sum1 + sum2*((y(i-1)+y(i))+.4*sum2*((fdy(i-1)-fdy(i))+0.1666667*sum2*(sdy(i-1)+sdy(i))))
         z(i-1) = sum1
      enddo
   endif

   z(ndim) = sum2
end subroutine qhsg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    qtfg(3f) - [M_math] Compute integral values for given general table of argument and function values.
!!
!!##PURPOSE
!!    Compute vector of integral values for given general table
!!    of argument and function values.
!!
!!##USAGE
!!    call qtfg (X,Y,Z,NDIM)
!!
!!    DESCRIPTION OF PARAMETERS
!!       X      - The input vector of argument values.
!!       Y      - The input vector of function values.
!!       Z      - The resulting vector of integral values. Z may be
!!                identical with X or Y.
!!       NDIM   - The dimension of vectors X,Y,Z.
!!
!!##REMARKS
!!    No action in case NDIM less than 1.
!!
!!##SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
!!    None
!!
!!##METHOD
!!    Beginning with Z(1)=0, evaluation of vector z is done by
!!    means of Trapezoidal Rule (Second Order Formula).
!!    For reference, see
!!
!!       F.B.Hildebrand, Introduction to Numerical Analysis,
!!       McGraw-Hill, New York/Toronto/London, 1956, pp.75.
subroutine qtfg(x,y,z,ndim)
integer,intent(in) :: ndim
real,intent(in)    :: x(ndim),y(ndim)
real,intent(out)   :: z(ndim)
integer            :: i
real               :: sum1, sum2

   if(ndim.le.0)then
      return
   elseif(ndim.eq.1)then
      z(1)=0.0
   else
      ! integration loop
      sum2=0.0
      do i=2,ndim
         sum1=sum2
         sum2=sum2+.5*(x(i)-x(i-1))*(y(i)+y(i-1))
         z(i-1)=sum1
      enddo
      z(ndim)=sum2
   endif
end subroutine qtfg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    trapezoidal_integral(3f) - [M_math] trapezoidal integration
!!
!!##SYNOPSIS
!!
!!     subroutine trapezoidal_integral(x,y,ivals,y2)
!!
!!      real,intent(in)  :: x(*), y(*)
!!      real,intent(out) :: y2(*)
!!      integer          :: ivals
!!
!!##DESCRIPTION
!!     Given the arrays X() and Y(), use trapezoidal integration
!!     to generate the integral of Y and return it as Y2().
!!##OPTIONS
!!     X      input X values
!!     Y      input Y values
!!     ivals  size of X and Y arrays
!!##RETURNS
!!     Y2     integral of the curve <X,Y>
!!##EXAMPLE
!!
subroutine trapezoidal_integral(x,y,ivals,y2)

! ident_7="@(#) trapezoidal integration"

real,intent(in)    :: x(*), y(*)
real,intent(out)   :: y2(*)
integer,intent(in) :: ivals
real               :: sum
integer            :: i
real               :: area
   sum=0.0
   y2(1)=0.0
   do i=2,ivals
      area=(y(i)+y(i-1))/2.0*(x(i)-x(i-1))
      sum=sum+area
      y2(i)=sum
   enddo
end subroutine trapezoidal_integral

!>
!!##NAME
!!    citer(3f) - [M_math:geometry] determine various geometric properties
!!                of a circle segment given radius and area of the segment.
!!##SYNOPSIS
!!
!!    Usage:
!!
!!       SUBROUTINE CITER(A,R,H,S,C,DADH)
!!       DOUBLEPRECISION,INTENT(IN)  :: A,R
!!       DOUBLEPRECISION,INTENT(OUT) :: H,S,C,DADH
!!
!!##DESCRIPTION
!!    This subroutine determines various geometric properties of a segment
!!    of a circle given the radius of the circle and area of the segment.
!!
!!    The figure below defines the geometry under consideration. This
!!    figure was taken directly from page 12 of the CRC Standard
!!    Mathematical Tables, 21st Edition, Published by the Chemical Rubber
!!    Company, Cleveland, OH. This page of the CRC Standard Mathematical
!!    Tables covers Mensuration Formulae for Circles.
!!
!!    In the figure below, the arc labeled "S" is the portion of the circle
!!    defined by angle "THETA". "C" is a secant, "H" is the height of the
!!    segment between "S" and "C", and "D" is the shortest distance
!!    from the center of the circle to the secant "C".
!!
!!    Specifically, this subroutine determines H, S, C, and the derivative
!!    of the segment area with respect to H given the radius of the circle,
!!    "R", and the area of the segment between "S" and "C".
!!
!!    Diagram:
!!
!!      >     _________________     S
!!      >        /|\         __---*****---__
!!      >         |       _--               --_
!!      >         H      *                     *
!!      >     ___\|/___ *_______________________*
!!      >        /|\     \          C          /
!!      >         |       \                   /
!!      >         |        \                 /
!!      >         |         \               /
!!      >         |          \             /
!!      >         |           \           /
!!      >         D            \         / R
!!      >         |             \ THETA /
!!      >         |              \     /
!!      >         |               \   /
!!      >         |                \ /
!!      >     ___\|/_______________ *
!!      >
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine citer(a,r,h,s,c,dadh)
use M_framework__journal, only : journal
! ident_8="@(#) M_math citer(3f) determine various geometric properties of circle segment given radius and area of the segment."
!
! COPYRIGHT (C) John S. Urban, 08/31/1995
!
!     Input and Output Variable Declarations
!
      double precision a,r,h,s,c,dadh
!
!     Internal Variable Declarations
!
doubleprecision           :: aors
doubleprecision           :: thetahi,thetalo,theta
doubleprecision           :: flo,fhi,f
doubleprecision,parameter :: pi=3.14159265358979d0
doubleprecision,parameter :: tol=1.d-10
doubleprecision           :: x
doubleprecision           :: funct
integer                   :: icount
!-----------------------------------------------------------------------
!
!     Statement Function Definition
!
!     The FUNCT statement function returns the scaled area of a segment
!     given the angle defining the segment.  The scaled area equals
!     the segment's area divided by the radius squared; it equals pi
!     when the input angle is 2*pi.
!
      funct(x)=(x-sin(x))/2.d0
!
!-----------------------------------------------------------------------
!
!     AORS = A Over R Squared
!          = Scaled area of segment
!          = pi when it represents the entire circle.
!
      aors=a/r**2
!
!     The following IF-THEN-ELSEIF block insures the input area is reasonable.
!
      if(aors.lt.tol) then
!
!       The input area is so low that the segment is virtually non-existent.
!
        theta=0.d0
        h=0.d0
        s=0.d0
        c=0.d0
        dadh=0.d0
        return
      elseif(aors.ge.pi) then
!
!       The input area is so high that the segment is virtually the entire circle
!
        theta=2.d0*pi
        h=2.d0*r
        s=2.d0*pi*r
        c=0.d0
        dadh=0.d0
        return
      endif
!
!     The following DO-WHILE loop solves for the value of THETA which
!     corresponds to the input segment area.  The loop employs an
!     interval reduction scheme.  THETALO is the lower bound of the
!     answer, and THETAHI is the upper bound of the answer.  FLO is the
!     scaled segment area corresponding to THETALO, and FHI is the
!     scaled segment area corresponding to THETAHI.
!
      thetalo=0.d0
      flo=0.d0
      thetahi=2.d0*pi
      fhi=pi
!
!     The very first estimate of THETA has to be done outside of the
!     loop.  The secant method is used to make the first estimate of
!     THETA.
!
      theta=thetalo+(thetahi-thetalo)*(aors-flo)/(fhi-flo)
      f=funct(theta)
      icount=1
      INFINITE: do
      if (abs(f-aors).gt.tol)then
        icount=icount+2
        if(icount.gt.100) then
!
!         The iteration has not converged in 100 steps.  Write a
!         message to the user and abort the run.
!
          call journal('*citer* did not convergence in 100 iterations')
          call journal('run aborted')
          call journal(' ')
          stop 1
        endif
!
!       Replace one of the bounds on THETA with the latest guess.
!
        if(f.gt.aors) then
          thetahi=theta
          fhi=f
        else
          thetalo=theta
          flo=f
        endif
!
!       Use the bisection method for the next guess of THETA.
!
        theta=(thetalo+thetahi)/2.0d0
        f=funct(theta)
!
!       Replace one of the bounds on THETA with the latest guess.
!
        if(f.gt.aors) then
          thetahi=theta
          fhi=f
        else
          thetalo=theta
          flo=f
        endif
!
!       Use the secant method for the next guess of THETA.
!
        theta=thetalo+(thetahi-thetalo)*(aors-flo)/(fhi-flo)
        f=funct(theta)
      else
         exit
      endif
      enddo INFINITE
!
!     The iteration on THETA has converged.
!
      h=r*(1.0d0-cos(theta/2.0d0))
      s=r*theta
      c=2.d0*r*sin(theta/2.d0)
      dadh=2.0d0*sqrt(2.0d0*r*h-h**2)
!
end subroutine citer
!>
!!##NAME
!!   envelope(3f) - [M_math:geometry] Find vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n.
!!##SYNOPSIS
!!
!!    subroutine envelope(x, y, n, vertex, nvert)
!!
!!     integer,intent(in) :: n
!!     real,intent(in)    :: x(n), y(n)
!!     integer :: vertex(n), nvert
!!
!!##DESCRIPTION
!!         Given the points composing a polygon find the points required to draw an envelope of the polygon
!!
!!##OPTIONS
!!         x,y     the vectors describing the polygon
!!         n       the number of elements in the input vectors
!!
!!##RETURNS
!!         vertex  the number of the vertices needed to generate the envelope
!!         nvert   number of vertices returned
!!
!!##EXAMPLE
!!
!!
!!   Draw a polygon and the envelope of the polygon, and find the area of
!!   each polygon. Also place a number of small circles in the plot area colored
!!   according to whether they fall within the border of the original polygon.
!!
!!    program demo_envelope
!!    use M_draw
!!    use M_math,     only : envelope        ! Find vertices (in clockwise order) of a polygon enclosing the points
!!    use M_math,     only : locpt           ! find if a point is inside a polygonal path
!!    use M_math,     only : polyarea        ! compute the area bounded by a closed polygonal curve
!!    implicit none
!!    integer,parameter :: n=6
!!    !   3--------------4   !
!!    !    \           /     !
!!    !      \       /       !
!!    !        \   /         !
!!    !          X 2,5       !
!!    !        /  \          !
!!    !      /      \        !
!!    !    /          \      !
!!    !   1--------------6   !
!!    real,parameter    :: x(n)=[-5.0, 0.0,-5.0, 5.0, 0.0, 5.0]
!!    real,parameter    :: y(n)=[-5.0, 0.0, 5.0, 5.0, 0.0,-5.0]
!!    real              :: xy(2,n)
!!    integer           :: vertex(n)
!!    integer           :: nvert
!!    integer           :: i
!!    integer           :: idum
!!       xy(1,:)=x
!!       xy(2,:)=y
!!       call vinit(' ')
!!       call page(-10.0,10.0,-10.0,10.0)
!!       call color(D_BLACK) ! set current color to black
!!       call clear()        ! clear to current color
!!       call polyfill(.true.)
!!       call color(D_BLUE)  ! we want to draw polygon in this color
!!       call poly2(n,xy)    ! draw filled polygon using points given
!!       idum=getkey()       ! pause for some input
!!       call color(D_CYAN)
!!       call polyhatch(.true.)
!!       call envelope(x, y, n, vertex, nvert)   ! calculate envelope
!!       call poly2(nvert,xy(:,vertex(1:nvert))) ! draw hatched envelope
!!       idum=getkey()       ! pause for some input
!!       call polyhatch(.false.)
!!       call linewidth(50)
!!       call color(D_WHITE)
!!       call poly2(n,xy)    ! draw line along original points
!!       idum=getkey()       ! pause for some input
!!       call random_seed()
!!       do i=1,70
!!          call pickrandom()
!!       enddo
!!       idum=getkey()       ! pause for some input
!!       call vexit()        ! wrap up and exit graphics mode
!!       write(*,*)'polyarea=',polyarea(x,y)
!!       write(*,*)'polyarea=',polyarea( xy(1,vertex(1:nvert)), xy(2,vertex(1:nvert)))
!!    contains
!!    subroutine pickrandom()
!!    ! randomly pick a point in the plot area and color it according to whether it is inside
!!    ! the original polygon
!!    real :: pointx, pointy
!!    integer :: l, m
!!       call random_number(pointx)
!!       call random_number(pointy)
!!       pointx=pointx*20.0-10.0
!!       pointy=pointy*20.0-10.0
!!       call locpt(pointx,pointy,x,y,n,l,m)
!!       select case(l)
!!        case(-1)
!!          call color(D_RED)
!!        case(0)
!!          call color(D_YELLOW)
!!        case(1)
!!          call color(D_GREEN)
!!        case default
!!          write(*,*)'*pickrandom* internal error: L value unknown'
!!          call color(D_WHITE)
!!       end select
!!       call circle(pointx,pointy,0.2)
!!    end subroutine pickrandom
!!    end program demo_envelope
!>
!! Programmer: Alan Miller
!! VERSION:    Latest revision - 12 September 1987
!! VERSION:    Fortran 90 version - 8 August 1996
subroutine envelope(x, y, n, vertex, nvert) !-- saved from url=(0048)http://users.bigpond.net.au/amiller/envelope.f90
implicit none

! ident_9="@(#) M_math envelope(3f) Find the vertices (in clockwise order) of a polygon enclosing the points (x(i) y(i) i=1 ... n."

integer,intent(in) :: n
real,intent(in)    :: x(n), y(n)
integer :: vertex(n), nvert
integer :: iwk(n)                  ! iwk() is an integer work array which must have dimension at least n

!  On output, vertex(i), i=1, ..., nvert contains the numbers of the vertices.

!       Local variables

integer :: next(n), i, i1, i2, j, jp1, jp2, i2save, i3, i2next
real    :: xmax, xmin, ymax, ymin, dist, dmax, dmin, x1, y1, dx, dy, x2, y2, dx1, dx2, dmax1, dmax2, dy1, dy2, temp, zero = 0.0

   if (n < 2) return

!  Choose the points with smallest & largest x- values as the
!  first two vertices of the polygon.

   if (x(1) > x(n)) then
      vertex(1) = n
      vertex(2) = 1
      xmin = x(n)
      xmax = x(1)
   else
      vertex(1) = 1
      vertex(2) = n
      xmin = x(1)
      xmax = x(n)
   endif

   do i = 2, n-1
      temp = x(i)
      if (temp < xmin) then
         vertex(1) = i
         xmin = temp
      else if (temp > xmax) then
         vertex(2) = i
         xmax = temp
      endif
   end do

!       Special case, xmax = xmin.

   if (xmax == xmin) then
      if (y(1) > y(n)) then
         vertex(1) = n
         vertex(2) = 1
         ymin = y(n)
         ymax = y(1)
      else
         vertex(1) = 1
         vertex(2) = n
         ymin = y(1)
         ymax = y(n)
      endif

      do i = 2, n-1
         temp = y(i)
         if (temp < ymin) then
            vertex(1) = i
            ymin = temp
         else if (temp > ymax) then
            vertex(2) = i
            ymax = temp
         endif
      end do

      nvert = 2
      if (ymax == ymin) nvert = 1
      return
   endif

!  Set up two initial lists of points; those points above & those below the
!  line joining the first two vertices.    next(i) will hold the pointer to the
!  point furthest from the line joining vertex(i) to vertex(i+1) on the left
!  hand side.

   i1 = vertex(1)
   i2 = vertex(2)
   iwk(i1) = -1
   iwk(i2) = -1
   dx = xmax - xmin
   y1 = y(i1)
   dy = y(i2) - y1
   dmax = zero
   dmin = zero
   next(1) = -1
   next(2) = -1

   do i = 1, n
      if (i == vertex(1) .or. i == vertex(2)) cycle
      dist = (y(i) - y1)*dx - (x(i) - xmin)*dy
      if (dist > zero) then
         iwk(i1) = i
         i1 = i
         if (dist > dmax) then
            next(1) = i
            dmax = dist
         endif
      else if (dist < zero) then
         iwk(i2) = i
         i2 = i
         if (dist < dmin) then
            next(2) = i
            dmin = dist
         endif
      endif
   end do

!  Ends of lists are indicated by pointers to -ve positions.

   iwk(i1) = -1
   iwk(i2) = -1
   nvert = 2

   j = 1

!  Start of main process.

!  Introduce new vertex between vertices j & j+1, if one has been found.
!  Otherwise increase j.   Exit if no more vertices.

   INFINITE: do
   if (next(j) < 0) then
      if (j == nvert) return
      j = j + 1
      cycle INFINITE
   endif

   jp1 = j + 1
   do i = nvert, jp1, -1
      vertex(i+1) = vertex(i)
      next(i+1) = next(i)
   end do
   jp2 = jp1 + 1
   nvert = nvert + 1
   if (jp2 > nvert) jp2 = 1
   i1 = vertex(j)
   i2 = next(j)
   i3 = vertex(jp2)
   vertex(jp1) = i2

!  Process the list of points associated with vertex j.   New list at vertex j
!  consists of those points to the left of the line joining it to the new
!  vertex (j+1).   Similarly for the list at the new vertex.
!  Points on or to the right of these lines are dropped.

   x1 = x(i1)
   x2 = x(i2)
   y1 = y(i1)
   y2 = y(i2)
   dx1 = x2 - x1
   dx2 = x(i3) - x2
   dy1 = y2 - y1
   dy2 = y(i3) - y2
   dmax1 = zero
   dmax2 = zero
   next(j) = -1
   next(jp1) = -1
   i2save = i2
   i2next = iwk(i2)
   i = iwk(i1)
   iwk(i1) = -1
   iwk(i2) = -1

   WORKFOREVER: do
      if (i /= i2save) then
         dist = (y(i) - y1)*dx1 - (x(i) - x1)*dy1
         if (dist > zero) then
            iwk(i1) = i
            i1 = i
            if (dist > dmax1) then
               next(j) = i
               dmax1 = dist
            endif
         else
            dist = (y(i) - y2)*dx2 - (x(i) - x2)*dy2
            if (dist > zero) then
               iwk(i2) = i
               i2 = i
               if (dist > dmax2) then
                  next(jp1) = i
                  dmax2 = dist
               endif
            endif
         endif
         i = iwk(i)
      else
         i = i2next
      endif

      if (i <= 0) exit WORKFOREVER
!  Get next point from old list at vertex j.
   enddo WORKFOREVER

!  End lists with -ve values.

   iwk(i1) = -1
   iwk(i2) = -1

   enddo INFINITE
end subroutine envelope
!>
!!##NAME
!!    inpolygon(3f) - [M_math:geometry] determine whether or not an integer point is in an integer polygon
!!
!!##SYNOPSIS
!!
!!     logical function inpolygon(xin, yin, xconv, yconv, nconv)
!!
!!      integer,intent(in)  xin, yin
!!      integer,intent(in)  nconv
!!      integer,intent(in)  xconv(nconv), yconv(nconv)
!!
!!##DESCRIPTION
!!   Given a closed polygon find if a point lies inside the polygon.
!!   Intended for integer values, like pixel images.
!!
!!##OPTIONS
!!    xin     the X coordinate of the point to be checked
!!    yin     the Y coordinate of the point to be checked
!!    xconv   contains the X coords of the polygon
!!    yconv   contains the Y coords of the polygon
!!    nconv   the number of points in the polygon
!!
!!##RESULT
!!    INPOLYGON returns .true if the point lies inside the polygon, otherwise
!!    it returns .false.
!!##EXAMPLE
!!
!!   Sample program
!!
!!   Draw a polygon and an envelope of the polygon and then calculate random
!!   points in the region and determine if they fall inside the polygon,
!!   within the accuracy of integer values.
!!
!!    program demo_inpolygon
!!    use M_draw
!!    use M_math,     only : envelope        ! Find vertices (in clockwise order) of a polygon enclosing the points
!!    use M_math,     only : inpolygon       ! find if a point is inside a polygonal path
!!    use M_math,     only : polyarea        ! compute the area bounded by a closed polygonal curve
!!    implicit none
!!    integer,parameter :: n=6
!!    !  3--------------4
!!    !   \           /
!!    !     \       /
!!    !       \   /
!!    !         X 2,5
!!    !       /  !!    !     /      !!    !   /          !!    !  1--------------6
!!    integer,parameter    :: x(n)=[-5, 0,-5, 5, 0, 5]
!!    integer,parameter    :: y(n)=[-5, 0, 5, 5, 0,-5]
!!    real              :: xy(2,n)
!!    integer           :: vertex(n)
!!    integer           :: nvert
!!    integer           :: i
!!    integer           :: idum
!!    xy(1,:)=x
!!    xy(2,:)=y
!!    call vinit(' ')
!!    call page(-10.0,10.0,-10.0,10.0)
!!    call color(D_BLACK) ! set current color to black
!!    call clear()        ! clear to current color
!!    call polyfill(.true.)
!!    call color(D_BLUE)  ! we want to draw polygon in this color
!!    call poly2(n,xy)    ! draw filled polygon using points given
!!    idum=getkey()       ! pause for some input
!!    call color(D_CYAN)
!!    call polyhatch(.true.)
!!    call envelope(real(x), real(y), n, vertex, nvert)   ! calculate envelope
!!
!!    call poly2(nvert,xy(:,vertex(1:nvert))) ! draw hatched envelope
!!    idum=getkey()       ! pause for some input
!!    call polyhatch(.false.)
!!    call linewidth(50)
!!    call color(D_WHITE)
!!    call poly2(n,xy)    ! draw line along original points
!!    idum=getkey()       ! pause for some input
!!    call random_seed()
!!    do i=1,70
!!       call pickrandom()
!!    enddo
!!    idum=getkey()       ! pause for some input
!!    call vexit()        ! wrap up and exit graphics mode
!!    write(*,*)'polyarea=',polyarea(real(x),real(y))
!!    write(*,*)'polyarea=',polyarea( xy(1,vertex(1:nvert)), xy(2,vertex(1:nvert)))
!!    contains
!!    subroutine pickrandom()
!!    ! randomly pick a point in the plot area and color it according to whether it is inside
!!    ! the original polygon
!!    real :: pointx, pointy
!!    integer :: l, m
!!       call random_number(pointx)
!!       call random_number(pointy)
!!       pointx=int(pointx*20.0-10.0)
!!       pointy=int(pointy*20.0-10.0)
!!       !call locpt(pointx,pointy,x,y,n,l,m)
!!       if(inpolygon(int(pointx),int(pointy),x,y,n))then
!!          call color(D_GREEN)
!!       else
!!          call color(D_RED)
!!       endif
!!       call circle(pointx,pointy,0.2)
!!    end subroutine pickrandom
!!    end program demo_inpolygon
logical function inpolygon(xin, yin, xconv, yconv, nconv)

! ident_10="@(#) M_math inpolygon(3f) Subroutine to determine whether or not an integer point is in a polygon of integer points"

integer,intent(in)  :: xin,yin                       ! coordinates of the point to be checked
integer,intent(in)  :: nconv                         !
integer             :: xconv(nconv), yconv(nconv)
real                :: x,y                           ! real copy of input point
real                :: temp                          ! real copy of input point
integer             :: z(4)= [-32701,-32701,32701,32701]
integer             :: i, j, m

   x=xin
   y=yin
   if (z(1).eq.-32701) then
      do i = 1,nconv
         z(1)=max(z(1),xconv(i))
         z(2)=max(z(2),yconv(i))
         z(3)=min(z(3),xconv(i))
         z(4)=min(z(4),yconv(i))
      enddo
   endif
   inpolygon=.true.
   if(x .lt. z(3) .or. x .gt. z(1)) inpolygon=.false.
   if(y .lt. z(4) .or. y .gt. z(2)) inpolygon=.false.
   if(.not. inpolygon) return

   j=0
   do i = 2,nconv
      m=0

      if ((yconv(i-1)-yin)*(yin-yconv(i)).lt.0) cycle

      select case(yconv(i-1)-yconv(i))
      case(:-1)
         m=m-1
      case(1:)
         m=m-2
         m=m-1
      case(0)
         if ((xconv(i-1)-x)*(x-xconv(i)).lt.0)then
            cycle
         else
            return
         endif
      end select

      m=m+2
      temp=(y-yconv(i-1))*(float(xconv(i))-xconv(i-1))/(yconv(i)-float(yconv(i-1)))+xconv(i-1)-x
      if (temp < 0)then
         cycle
      elseif(temp == 0 )then
         return
      endif
      j=j+m
   enddo

   inpolygon=.false.
   if(j/4*4 .ne. j) inpolygon=.true.
100 continue
end function inpolygon
! ident_11="@(#) M_math inpolygon(3f) Subroutine to determine whether or not an integer point is in a polygon of integer points"
!>
!!##NAME
!!   locpt(3f) - [M_math:geometry] find if a point is inside a polygonal path
!!##SYNOPSIS
!!
!!   Usage:
!!
!!    subroutine locpt (x0,y0,x,y,n,l,m)
!!
!!     real, intent(in)     :: x0, y0, x(:), y(:)
!!     integer, intent(in)  :: n
!!     integer, intent(out) :: l, m
!!
!!##DESCRIPTION
!!   Given a polygonal line connecting the vertices (X(I),Y(I)) (I = 1,...,N)
!!   taken in this order. it is assumed that the polygonal path is a loop,
!!   where (X(N),Y(N)) = (X(1),Y(1)) or there is an arc from (X(N),Y(N))
!!   to (X(1),Y(1)). N.B. The polygon may cross itself any number of times.
!!
!!   (X0,Y0) is an arbitrary point and l and m are variables.  On output,
!!   L and M are assigned the following values ...
!!
!!      L = -1   If (X0,Y0) is outside the polygonal path
!!      L =  0   If (X0,Y0) lies on the polygonal path
!!      L =  1   If (X0,Y0) is inside the polygonal path
!!
!!   M = 0 if (X0,Y0) is on or outside the path. If (X0,Y0) is inside the
!!   path then M is the winding number of the path around the point (X0,Y0).
!!
!!    o Fortran 66 version by A.H. Morris
!!    o Converted to ELF90 compatibility by Alan Miller, 15 February 1997
!!      saved from url=(0050)http://users.bigpond.net.au/amiller/NSWC/locpt.f90
!!
!!##EXAMPLE
!!
!!
!!   Draw a polygon and the envelope of the polygon, and find the area
!!   of each polygon. Also place a number of small circles in the plot
!!   area colored according to whether they fall within the border of the
!!   original polygon.
!!
!!    program demo_envelope
!!    use M_draw
!!    use M_math,     only : envelope        ! Find vertices (in clockwise order) of a polygon enclosing the points
!!    use M_math,     only : locpt           ! find if a point is inside a polygonal path
!!    use M_math,     only : polyarea        ! compute the area bounded by a closed polygonal curve
!!    implicit none
!!    integer,parameter :: n=6
!!    !  3--------------4
!!    !   \           /
!!    !     \       /
!!    !       \   /
!!    !         X 2,5
!!    !       /  !!    !     /      !!    !   /          !!    !  1--------------6
!!    real,parameter    :: x(n)=[-5.0, 0.0,-5.0, 5.0, 0.0, 5.0]
!!    real,parameter    :: y(n)=[-5.0, 0.0, 5.0, 5.0, 0.0,-5.0]
!!    real              :: xy(2,n)
!!    integer           :: vertex(n)
!!    integer           :: nvert
!!    integer           :: i
!!    integer           :: idum
!!       xy(1,:)=x
!!       xy(2,:)=y
!!       call vinit(' ')
!!       call page(-10.0,10.0,-10.0,10.0)
!!       call color(D_BLACK) ! set current color to black
!!       call clear()        ! clear to current color
!!       call polyfill(.true.)
!!       call color(D_BLUE)  ! we want to draw polygon in this color
!!       call poly2(n,xy)    ! draw filled polygon using points given
!!       idum=getkey()       ! pause for some input
!!       call color(D_CYAN)
!!       call polyhatch(.true.)
!!       call envelope(x, y, n, vertex, nvert)   ! calculate envelope
!!       call poly2(nvert,xy(:,vertex(1:nvert))) ! draw hatched envelope
!!       idum=getkey()       ! pause for some input
!!       call polyhatch(.false.)
!!       call linewidth(50)
!!       call color(D_WHITE)
!!       call poly2(n,xy)    ! draw line along original points
!!       idum=getkey()       ! pause for some input
!!       call random_seed()
!!       do i=1,70
!!          call pickrandom()
!!       enddo
!!       idum=getkey()       ! pause for some input
!!       call vexit()        ! wrap up and exit graphics mode
!!       write(*,*)'polyarea=',polyarea(x,y)
!!       write(*,*)'polyarea=',polyarea( xy(1,vertex(1:nvert)), xy(2,vertex(1:nvert)))
!!    contains
!!    subroutine pickrandom()
!!    ! randomly pick a point in the plot area and color it according to whether it is inside
!!    ! the original polygon
!!    real :: pointx, pointy
!!    integer :: l, m
!!       call random_number(pointx)
!!       call random_number(pointy)
!!       pointx=pointx*20.0-10.0
!!       pointy=pointy*20.0-10.0
!!       call locpt(pointx,pointy,x,y,n,l,m)
!!       select case(l)
!!        case(-1)
!!          call color(D_RED)
!!        case(0)
!!          call color(D_YELLOW)
!!        case(1)
!!          call color(D_GREEN)
!!        case default
!!          write(*,*)'*pickrandom* internal error: L value unknown'
!!          call color(D_WHITE)
!!       end select
!!       call circle(pointx,pointy,0.2)
!!    end subroutine pickrandom
!!    end program demo_envelope
!*==locpt.f90 processed by SPAG 8.01RF 04:02 14 Dec 2024
subroutine locpt(x0,y0,x,y,n,l,m)
implicit none

! ident_12="@(#) M_math locpt(3f) find if a point is inside a polygonal path"

real, intent(in)     :: x0, y0, x(:), y(:)
integer, intent(in)  :: n
integer, intent(out) :: l, m

integer              :: i, n0
real                 :: angle, eps, pi, pi2, sum, theta, theta1, thetai, tol, u, v

   eps = epsilon(1.0)   ! EPS is a machine dependent constant. EPS is the smallest number such that 1.0 + EPS > 1.0
   n0 = n
   if ( x(1)==x(n) .and. y(1)==y(n) ) n0 = n - 1
   pi = atan2(0.0,-1.0)
   pi2 = 2.0*pi
   tol = 4.0*eps*pi
   l = -1
   m = 0

   u = x(1) - x0
   v = y(1) - y0
   if ( u==0.0 .and. v==0.0 ) then
      l = 0
      return
   endif
   if ( n0<2 ) return
   theta1 = atan2(v,u)

   sum = 0.0
   theta = theta1
   do i = 2, n0
      u = x(i) - x0
      v = y(i) - y0
      if ( u==0.0 .and. v==0.0 ) then
         l = 0
         return
      endif
      thetai = atan2(v,u)

      angle = abs(thetai-theta)
      if ( abs(angle-pi)<tol ) then
         l = 0
         return
      endif
      if ( angle>pi ) angle = angle - pi2
      if ( theta>thetai ) angle = -angle
      sum = sum + angle
      theta = thetai
   enddo
   angle = abs(theta1-theta)
   if ( abs(angle-pi)<tol ) then
      l = 0
      return
   endif
   if ( angle>pi ) angle = angle - pi2
   if ( theta>theta1 ) angle = -angle
   sum = sum + angle            ! SUM = 2*PI*M WHERE M IS THE WINDING NUMBER
   m = int(abs(sum)/pi2+0.2)
   if ( m==0 ) return
   l = 1
   if ( sum<0.0 ) m = -m
   return
end subroutine locpt
!>
!!##NAME
!!      poly_intercept(3f) - [M_math:geometry] intersection of a straight line and polygonal path
!!##SYNOPSIS
!!
!!
!!   SUBROUTINE Poly_Intercept(a,b,x,y,n,u,v,m,num,ierr)
!!
!!    REAL, INTENT(IN)      :: a(2)
!!    REAL, INTENT(IN)      :: b(2)
!!    REAL, INTENT(IN)      :: x(:)
!!    REAL, INTENT(IN)      :: y(:)
!!    INTEGER, INTENT(IN)   :: n
!!
!!    REAL, INTENT(OUT)     :: u(:)
!!    REAL, INTENT(OUT)     :: v(:)
!!
!!    INTEGER, INTENT(IN)   :: m
!!
!!    INTEGER, INTENT(OUT)  :: num
!!    INTEGER, INTENT(OUT)  :: ierr
!!
!!##DESCRIPTION
!!    Calculates the points <U(1:num),V(1:num)> at which a line <A,B> crosses a
!!    polygon <X(1:n),Y(1:n)>, provided that number of points found (NUM)
!!    is less than or equal to the storage given the output vector <U,V>.
!!
!!    Based upon routine PFIND from the NSWC Mathematics Library.
!!
!!##OPTIONS
!!    a,b     points ( a(1),a(2) ) and ( b(1),b(2) ) defining a line
!!    x,y     the set of points ( x(i),y(i) ), i=1,2,3,....n  define a polygon
!!    n       the size of the x(:) and y(:) arrays
!!##RETURNS
!!    u,v     the arrays U and V contain the number of points at which the line
!!    m       the size of the U and V arrays
!!    num     number of intersection points found at which the line
!!            crosses the polygon in order (provided that m < size(u))
!!    ierr N  where N is ...
!!            o 0 no error detected
!!            o 1 if a = b
!!            o 2 U and V require more storage, i.e. num > m.
!!            o -i if the ith segment of the polygon is coincident with part of the line.
!!
!!##EXAMPLE
!!
!!##PEDIGREE
!!      o Based upon routine PFIND from the NSWC Mathematics Library.
!!      o Code converted using TO_F90 by Alan Miller, Date: 2000-07-04  Time: 12:24:01
!!      o Update Sun, Mar  5, 2017  8:04:46 AM
!>
!! PROCEDURE:    poly_intercept(3f)
!! DESCRIPTION:  intersections of a straight line and polygonal path
!! AUTHOR:       Code converted using TO_F90 by Alan Miller
!! VERSION:      Date: 2000-07-04  Time: 12:24:01
subroutine poly_intercept (a, b, x, y, n, u, v, m, num, ierr)
implicit none
! ident_13="@(#) M_math poly_intercept(3f) Calculates the points at which a line <A B> crosses a polygon"

real, intent(in)      :: a(2)
real, intent(in)      :: b(2)
real, intent(in)      :: x(:)
real, intent(in)      :: y(:)
integer, intent(in)   :: n
real, intent(out)     :: u(:)
real, intent(out)     :: v(:)
integer, intent(in)   :: m
integer, intent(out)  :: num
integer, intent(out)  :: ierr

! Local variables

integer  :: i, ind, nm1
real     :: d, diff, diff1, eps, h, hi, k, ki, onem, onep, p, q, s, t, tmax, tmin, tol, tol0

eps = epsilon(1.0)  ! EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE SMALLEST NUMBER SUCH THAT 1.0 + EPS .GT. 1.0 .
num = 0

if (n < 2) go to 200
   h = b(1) - a(1)
   k = b(2) - a(2)

if (h == 0.0 .and. k == 0.0) go to 200

   ierr = 0
   nm1 = n - 1
   tol = 4.0*eps
   tol0 = 2.0*eps
   onep = 1.0 + tol
   onem = 0.5 + (0.5 - tol0)

ind = 0

do i = 1, nm1
   hi = x(i + 1) - x(i)
   ki = y(i + 1) - y(i)
   if (hi == 0.0 .and. ki == 0.0) cycle
   ind = 1

! Check if the line from a to b and the i-th line in the path are parallel

  s = hi*k
  t = h*ki
  d = s - t

  if (abs(d) <= tol*max(abs(s), abs(t))) go to 40
!-----------------------------------------------------------------------
!                   THE LINES ARE NOT PARALLEL
!-----------------------------------------------------------------------
  p = x(i) - a(1)
  q = y(i) - a(2)
  s = hi*q
  t = ki*p
  diff = s - t
  if (abs(diff) <= tol*max(abs(s),abs(t))) diff = 0.0
  s = h*q
  t = k*p
  diff1 = s - t
  if (abs(diff1) <= tol*max(abs(s),abs(t))) diff1 = 0.0

  s = diff/d
  t = diff1/d

  if (s < 0.0 .or. s > onep) cycle
  if (t < 0.0 .or. t > onep) cycle
  if (num > 0 .and. t == 0.0) cycle
  if (s > 0.0) go to 20

!                   POINT A IS ON THE I-TH LINE

10 continue
  num = num + 1

  if (num > m) go to 210
     u(num) = a(1)
     v(num) = a(2)
     cycle

!                   POINT B IS ON THE I-TH LINE

20 continue
   if (s < onem) go to 30
21 continue
   num = num + 1
   if (num > m) go to 210
      u(num) = b(1)
      v(num) = b(2)
   cycle

!              THE INTERIOR OF THE LINE FROM A TO B INTERSECTS WITH THE I-TH LINE

30 continue
   num = num + 1
   if (num > m) go to 210
      u(num) = a(1) + s*h
      v(num) = a(2) + s*k
   cycle
!-----------------------------------------------------------------------
!                     THE LINES ARE PARALLEL
!-----------------------------------------------------------------------
40 continue
  if (abs(hi) > abs(ki)) go to 50

  d = a(2) - y(i)
  if (abs(d) <= tol0*max(abs(a(2)),abs(y(i)))) d = 0.0
  s = d/ki

  p = x(i) + s*hi
  if (abs(a(1) - p) > tol*max(abs(a(1)),abs(p))) cycle

  d = b(2) - y(i)
  if (abs(d) <= tol0*max(abs(b(2)),abs(y(i)))) d = 0.0
  t = d/ki
  go to 60

50 d = a(1) - x(i)
  if (abs(d) <= tol0*max(abs(a(1)),abs(x(i)))) d = 0.0
  s = d/hi

  p = y(i) + s*ki
  if (abs(p - a(2)) > tol*max(abs(p),abs(a(2)))) cycle

  d = b(1) - x(i)
  if (abs(d) <= tol0*max(abs(b(1)),abs(x(i)))) d = 0.0
  t = d/hi

!              THE 2 LINES ARE PORTIONS OF THE SAME STRAIGHT INFINITE LINE

60 continue
  if (s > 0.0 .and. s < onem) go to 220
  if (t > 0.0 .and. t < onem) go to 220
  tmin = min(s,t)
  tmax = max(s,t)
  if (tmax <= 0.0) go to 70
  if (tmin >= onem) go to 80
  go to 220

70 continue
  if (tmax < 0.0) cycle
  if (num > 0) cycle
  if (tmax == s) go to 10
  go to 21

80 continue
  if (tmin > 1.0) cycle
  if (tmin == s) go to 10
  go to 21

enddo

   if (ind == 0) go to 200

   if (num < 2) return
   if (u(num) == x(1) .and. v(num) == y(1)) num = num - 1
return

! ERROR RETURN

200 continue
   ierr = 1
   return

210 continue
   ierr = 2
   num = num - 1
return

220 continue
   ierr = -i

end subroutine poly_intercept
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!        polyarea(3f) - [M_math:geometry] compute the area bounded by a simple closed polygonal curve
!!
!!##SYNOPSIS
!!
!!    FUNCTION polyarea(x, y) RESULT(fn_val)
!!
!!       REAL, INTENT(IN)     :: x(:)
!!       REAL, INTENT(IN)     :: y(:)
!!       REAL                 :: fn_val
!!
!!##DESCRIPTION
!!    Given a sequence of points (X(I),Y(I)), polyarea(3f) computes the
!!    area bounded by the closed polygonal curve which passes through the
!!    points in the order that they are indexed. The final point of the
!!    curve is assumed to be the first point given. Therefore, it need
!!    not be listed at the end of X and Y. The polygon should be simple
!!    (e.g. It may not cross over itself).
!!
!!    If the vertices are given in counterclockwise order, the area will
!!    be positive. If the vertices are given in clockwise order, the area
!!    will be negative.
!!
!!##OPTIONS
!!    x   x coordinates of the points that define the simple polygon
!!    y   y coordinates of the points that define the simple polygon
!!
!!    The X and Y arrays are assumed to be of the same size.
!!
!!##RETURNS
!!    fn_val   the area of the simple polygon
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    !   (0,10) ########### (10,10)
!!    !          ##       #
!!    !          # #     #
!!    !          #  #   #
!!    !          #   # #
!!    !          #    #
!!    !          #   # #
!!    !          #  #   #
!!    !          # #     #
!!    !          ##       #
!!    !     (0,0)########### (10,0)
!!
!!    program demo_polyarea
!!    use M_math, only : polyarea
!!    implicit none
!!    !                          A  B      C    D      E    F
!!    real,allocatable :: x(:)
!!    real,allocatable :: y(:)
!!
!!    x=[ 0.0, 10.0,  0.0, 10.0,  0.0,  0.0]   !*! hourglass crosses itself. unexpected value
!!    y=[10.0, 10.0,  0.0,  0.0, 10.0, 10.0]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0, 10.0,  0.0,  0.0, 10.0, 0.0,  0.0] !*! crosses itself. maybe not what you expect
!!    y=[10.0, 10.0,  0.0, 10.0,  0.0, 0.0, 10.0]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0,  0.0, 10.0, 10.0,  0.0 ]     ! square
!!    y=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]     ! square
!!    y=[10.0, 10.0,  0.0,  0.0, 10.0 ]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    end program demo_polyarea
!!
!!  polyarea=   0.00000000
!!  polyarea=  -100.000000
!!  polyarea=  -100.000000
!!  polyarea=  -100.000000
!>
!! PROCEDURE:    polyarea(3f)
!! DESCRIPTION:  compute the area bounded by a closed polygonal curve
!! AUTHOR:       Code converted using TO_F90 by Alan Miller
!! VERSION:      2000-07-04  Time: 12:24:06
function polyarea(x, y) result(fn_val)
implicit none

! ident_14="@(#) M_math polyarea(3f) compute the area bounded by a closed polygonal curve"

real, intent(in)     :: x(:)
real, intent(in)     :: y(:)
real                 :: fn_val

integer  :: i, n, nm1
real     :: a

n = min(size(x),size(y))
if (x(1) == x(n) .and. y(1) == y(n))then
   n = n - 1
endif

   select case (n)
   case (:2)
      fn_val = 0.0
   case (3)
      fn_val= 0.5*((x(2) - x(1))*(y(3) - y(1)) - (x(3) - x(1))*(y(2) - y(1)))
   case default
     nm1 = n - 1
     a = x(1)*(y(2) - y(n)) + x(n)*(y(1) - y(nm1))

     do  i = 2, nm1
       a = a + x(i)*(y(i+1) - y(i-1))
     end do

     fn_val = 0.5*a
   end select
end function polyarea
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
doubleprecision function polyarea_mid_point(n,p)     !Calculates the area enclosed by the polygon P.

! from rosetta code Sunday, December 30th, 2018 7:07:10 PM UTC-05:00

! Uses the mid-point rule for integration. Consider the line joining (x1,y1) to (x2,y2)
! The area under that line (down to the x-axis) is the y-span midpoint (y1 + y2)/2 times the width (x2 - x1)
! This is the trapezoidal rule for a single interval, and follows from simple geometry.
! Now consider a sequence of such points heading in the +x direction: each successive interval's area is positive.
! Follow with a sequence of points heading in the -x direction, back to the first point: their areas are all negative.
! The resulting sum is the area below the +x sequence and above the -x sequence: the area of the polygon.
! The point sequence can wobble as it wishes and can meet the other side, but it must not cross itself
! as would be done in a figure 8 drawn with a crossover instead of a meeting.
! A clockwise traversal (as for an island) gives a positive area; use anti-clockwise for a lake.
integer, parameter :: dc = kind(0d0)    ! double precision
integer            :: n                 ! The number of points.
complex(kind=dc)   :: p(n)              ! The points.
complex(kind=dc)   :: pp,pc             ! Point Previous and Point Current.
complex(kind=dc)   :: w                 ! Polygon centre. Map coordinates usually have large offsets.
doubleprecision    :: a                 ! The area accumulator.
integer            :: i                 ! A stepper.
   if (n.lt.3) stop "*polyarea_mid_point* ERROR: at least three points are needed! "        !Good grief.
   w = (p(1) + p(n/3) + p(2*n/3))/3     ! An initial working average.
   w = sum(p(1:n) - w)/n + w            ! A good working average is the average itself.
   a = 0                                ! The area enclosed by the point sequence.
   pc = p(n) - w                        ! The last point is implicitly joined to the first.
   do i = 1,n                           ! Step through the positions.
      pp = pc                           ! Previous position.
      pc = p(i) - w                     ! Current position.
      a = (aimag(pc) + aimag(pp))*(dble(pc) - dble(pp)) + a  ! Area integral component.
   enddo                                ! On to the next position.
   polyarea_mid_point = a/2             ! Divide by two once.
end function polyarea_mid_point         ! The units are those of the points.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!        polyarea_shoelace(3f) - [M_math:geometry] compute area bounded by a simple closed polygon using the shoelace algorithm
!!
!!##SYNOPSIS
!!
!!   function polyarea_shoelace(x, y)
!!
!!    class(*), intent(in) :: x(:)
!!    class(*), intent(in) :: y(:)
!!    doubleprecision      :: polyarea_shoelace
!!
!!##DESCRIPTION
!!    Given a sequence of points (X(I),Y(I)), polyarea_shoelace(3f) computes the
!!    area bounded by the closed polygonal curve which passes through the
!!    points in the order that they are indexed. The final point of the
!!    curve is assumed to be the first point given. Therefore, it need
!!    not be listed at the end of X and Y. The polygon should be simple
!!    (e.g. It may not cross over itself).
!!
!!    If the vertices are given in counterclockwise order, the area will
!!    be positive. If the vertices are given in clockwise order, the area
!!    will be negative.
!!
!!##OPTIONS
!!    x   x coordinates of the points that define the simple polygon.
!!        May be any standard scalar numeric value type supported by
!!        M_anything::anyscalar_to_double(3f).
!!    y   y coordinates of the points that define the simple polygon.
!!        May be any standard scalar numeric value type supported by
!!        M_anything::anyscalar_to_double(3f).
!!
!!    The X and Y arrays are assumed to be of the same size.
!!
!!##RETURNS
!!    polyarea_shoelace   the area of the simple polygon
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    !   (0,10) ########### (10,10)
!!    !          ##       #
!!    !          # #     #
!!    !          #  #   #
!!    !          #   # #
!!    !          #    #
!!    !          #   # #
!!    !          #  #   #
!!    !          # #     #
!!    !          ##       #
!!    !     (0,0)########### (10,0)
!!
!!    program demo_polyarea_shoelace
!!    use M_math, only : polyarea_shoelace
!!    implicit none
!!    !                          A  B      C    D      E    F
!!    real,allocatable :: x(:)
!!    real,allocatable :: y(:)
!!
!!    x=[ 0.0, 10.0,  0.0, 10.0,  0.0,  0.0]   !*! hourglass crosses itself. unexpected value
!!    y=[10.0, 10.0,  0.0,  0.0, 10.0, 10.0]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    x=[ 0.0, 10.0,  0.0,  0.0, 10.0, 0.0,  0.0] !*! crosses itself. maybe not what you expect
!!    y=[10.0, 10.0,  0.0, 10.0,  0.0, 0.0, 10.0]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    x=[ 0.0,  0.0, 10.0, 10.0,  0.0 ]     ! square clockwise
!!
!!    y=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    x=[ 0.0, 0.0, 10.0,  10.0,  0.0 ]     ! square counterclockwise
!!    y=[10.0, 0.0,  0.0,  10.0, 10.0 ]
!!    write(*,*)'polyarea_shoelace=',polyarea_shoelace(x,y)
!!
!!    end program demo_polyarea_shoelace
!!
!!   Results:
!!
!!     polyarea_shoelace=   0.0000000000000000
!!     polyarea_shoelace=  -100.00000000000000
!!     polyarea_shoelace=  -100.00000000000000
!!     polyarea_shoelace=   100.00000000000000
doubleprecision function polyarea_shoelace(x,y)
use m_anything, only : anyscalar_to_double

! ident_15="@(#) Area enclosed by simple (non-intersecting) polygon P by the "shoelace" method."

class(*),intent(in)          :: x(:), y(:)

doubleprecision,allocatable  :: px(:)    ! The X values for the points.
doubleprecision,allocatable  :: py(:)    ! The Y values for the points.
integer                      :: n        ! The number of points.
integer                      :: ipx,ipy  ! size of PX and PY
doubleprecision              :: area     !
!----------------------------! determine number of points
   px=anyscalar_to_double(x) ! allow any scalar numeric type at cost of making copy
   py=anyscalar_to_double(y)
   ipx=ubound(px,dim=1)
   ipy=ubound(py,dim=1)
   if(ipx.ne.ipy)then
      write(*,*)'*polyarea_shoelace* WARNING: input arrays not same size'
   endif
   n=min(ipx,ipy)
!----------------------------!

   area = sum(px(1:n - 1)*py(2:n)) + px(n)*py(1) - sum(px(2:n)*py(1:n - 1)) - px(1)*py(n)
   polyarea_shoelace = area/2        ! The midpoint formula requires area halving.

end function polyarea_shoelace       ! Negative for clockwise, positive for counter-clockwise.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    closest(3f) - [M_math:geometry] find the data point that is closest to the target point
!!##SYNOPSIS
!!
!!   function closest(xtarget,ytarget,x,y) result(location)
!!
!!    real, dimension(:), intent(in)  :: x, y
!!    real                            :: xtarget, ytarget
!!    integer                         :: location
!!
!!##DESCRIPTION
!!    Given a set of X and Y values and a target point, find the index of the closest point to the target
!!    from the points described by the <X,Y> values. The X and Y arrays are assumed to be the same size.
!!##OPTIONS
!!    XTARGET   X coordinate of target point
!!    YTARGET   Y coordinate of target point
!!    X         array of X values that defines a set of points
!!    Y         array of Y values that defines a set of points
!!##RETURNS
!!   Sample program
!!
!!    program demo_closest
!!    use M_math, only : closest
!!    implicit none
!!    real,allocatable :: x(:),y(:)
!!    real             :: x1, y1
!!    integer          :: id
!!    x=[ 11.0,  100.0, -22.34, 26.4, -50.66 ]
!!    y=[-21.0,  150.0, -82.00, 40.0, 350.00 ]
!!    x1=30.0
!!    y1=44.0
!!    id=closest(x1,y1,x,y)
!!    write(*,*)'Closest point: ', x(id), y(id), ' at index ',id
!!    end program demo_closest
!!
!!##EXAMPLE
!!
function closest(xtarget,ytarget,x,y) result(location)

! ident_16="@(#) find the index of the data point in the <X Y> arrays that is closest to the target point"

real, dimension(:), intent(in)  :: x, y
real                            :: xtarget, ytarget
integer                         :: location
integer                         :: ind(1)
   if(size(x).eq.0.or.size(y).eq.0)then
      stop '*closest* input array has no values'
   endif
   ! probably creates a scratch array of the size of an input array
   ind = minloc( (x - xtarget) ** 2 + (y -ytarget) ** 2 )
   location=ind(1)
   !*! 'Closest point: ', x(location(1)), y(location(1))
end function closest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_closest()
use M_framework__verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_framework__verify, only : unit_check_level
   call unit_check_start('closest',msg='')
   !*!call unit_check('closest', 0.eq.0, 'checking',100)
   call unit_check_done('closest',msg='')
end subroutine test_closest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    hypot(3f) - [M_math:geometry] Euclidean distance function
!!##SYNOPSIS
!!
!!    pure function hypot(x,y) result(z)
!!
!!     real,intent(in):: x,y
!!     real:: z
!!##DESCRIPTION
!!    hypot(x,y) is the Euclidean distance function. It is equal to
!!    sqrt{X**2 + Y**2}, without undue underflow or overflow.
!!
!!    That is, solve for SQRT(x*x+y*y) carefully to avoid overflow
!!    (will be an intrinsic in Fortran 2008).
!!##OPTIONS
!!    X   The type shall be REAL.
!!    Y   The type and kind type parameter shall be the same as X.
!!##RESULT
!!    HYPOT  The return value has the same type and kind type parameter as X.
!!##STANDARD
!!        [[Fortran 2008]] and later
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_hypot
!!    real(4) :: x = 1.e0_4, y = 0.5e0_4
!!       x = hypot(x,y)
!!    end program demo_hypot
pure function hypot(x,y) result(z)
real,intent(in):: x,y
real  :: z
real  :: a,b
   a=ABS(x)
   b=ABS(y)

   if(a > b)then
      z=a*sqrt(1.0+(b/a)**2)
   elseif (b==0) then
      z=0.0
   else
      z=b*sqrt(1.0+(a/b)**2)
   endif

end function hypot

!>
!!##NAME
!!      extremum(3f) - [M_math:statistics] Finds the minimum and maximum value in a REAL array.
!!##SYNOPSIS
!!
!!   subroutine extremum(array,small,big)
!!
!!    real,intent(in)    :: array(:)
!!    real,intent(out)   :: small
!!    real,intent(out)   :: big
!!
!!##DESCRIPTION
!!    Finds the minimum and maximum value in a REAL array.
!!
!!##OPTIONS
!!    array  The array to find the extremes of
!!
!!##RETURNS
!!    small  least value found
!!    big    largest value found
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_extremum
!!    use M_math, only : extremum
!!    implicit none
!!    real,allocatable :: arr(:)
!!    real :: small, big
!!    arr=[-10.0,8.8,-5.0,0.0,5.0,10.0,-0.3]
!!    call extremum(arr,small,big)
!!    write(*,*)'ARRAY=',arr
!!    write(*,*)'SMALL=',small
!!    write(*,*)'BIG=',big
!!    end program demo_extremum
!!
!!   Results:
!!
!!     ARRAY= -10.000 8.80 -5.00 0.00 5.00 10.0 -0.300
!!     SMALL= -10.000
!!     BIG= 10.00
!! ================================================================================
!!
!!##SEE ALSO
!!    minval(3f), maxval(3f)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine extremum(array,small,big)
implicit none
! ident_17="@(#) M_math extremum(3f) Find the minimum and maximum value in a REAL array"

real,intent(in)            :: array(:)
real,intent(out),optional  :: small
real,intent(out),optional  :: big

integer                    :: i10
integer                    :: n
real                       :: local_small, local_big

n=size(array)

if(n.le.0)then
   local_small=0.0
   local_big=0.0
elseif(n.eq.1)then
   local_small=array(1)
   local_big=array(1)
else
   local_small=array(1)
   local_big=array(1)
   do i10=2,n
      if(local_small.gt.array(i10))then
         local_small=array(i10)
      elseif(local_big.lt.array(i10))then
         local_big=array(i10)
      endif
   enddo
endif

if(present(big))   big=local_big
if(present(small)) small=local_small

end subroutine extremum
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      bds(3f) - [M_math:statistics] Basic Statistical Measures
!!##SYNOPSIS
!!
!!
!!    subroutine bds(x,n,stat)
!!
!!     real,intent(in)    :: x(n)
!!     integer,intent(in) :: n
!!     real,intent(out)   :: STAT(13)
!!##DESCRIPTION
!!
!!     Given a vector of real values calculate common basic statistical measures for
!!     the array.
!!
!!##OPTIONS
!!      x      REAL input vector of values to determine statistical properties for
!!      n      size of input vector
!!##RETURNS
!!      stat   array of statistical measurements calculated
!!
!!         1.   mean
!!         2.   second moment about the mean
!!         3.   third moment about the mean
!!         4.   fourth moment about the mean
!!         5.   variance
!!         6.   standard deviation
!!         7.   skewness
!!         8.   kurtosis
!!         9.   sum
!!         10.  largest value
!!         11.  smallest value
!!         12.  location of largest value
!!         13.  location of smallest value
!!
!!##DEFINITIONS
!!      MEAN
!!
!!           A type of average, calculated by dividing the sum of
!!           a set of values by the number of values.
!!
!!              mean = Sum(Xi)/N
!!
!!      MEDIAN
!!
!!           A type of average, found by arranging the values in
!!           order and then selecting the one in the middle. If the
!!           total number of values in the sample is even, then the
!!           median is the mean of the two middle numbers.
!!
!!      MODE
!!
!!           The most frequent value in a group of values.
!!
!!      VARIANCE
!!
!!           The average of the square of the distance of each
!!           data point from the mean
!!
!!              variance = Sum((Xi-mean)^2))/N
!!
!!           for a population, or more commonly, for a sample the
!!           unbiased value is
!!
!!              variance = Sum((Xi-mean)^2))/(N-1)
!!
!!      STANDARD DEVIATION
!!
!!           The standard deviation is the square root of the
!!           variance.
!!
!!              sd = sqrt(variance)
!!
!!           It is the most commonly used measure of spread.
!!
!!      SKEWNESS
!!
!!           Skewness is a measure of symmetry, or more
!!           precisely, the lack of symmetry. A distribution, or
!!           data set, is symmetric if it looks the same to the left
!!           and right of the center point.
!!
!!              skewness = Sum{(X(i)-mean)^3} /((N-1)*SD^3)
!!
!!           Where SD is the standard deviation, and N is the number of
!!           samples. Some sources will use N instead of N-1 or they might
!!           present the formula in a slightly different mathematically
!!           equivalent format.
!!
!!           The skewness of symmetric data is zero
!!
!!      KURTOSIS
!!
!!           Kurtosis is a measure of whether the data are peaked
!!           or flat relative to a normal distribution. That is,
!!           data sets with high kurtosis tend to have a distinct
!!           peak near the mean, decline rather rapidly, and have
!!           heavy tails. Data sets with low kurtosis tend to have a
!!           flat top near the mean rather than a sharp peak. A
!!           uniform distribution would be the extreme case.
!!
!!              kurtosis = ( SUM{(X(i)-mean)^4} ) / ((N-1)*SD^4) -3
!!
!!           The standard normal distribution has a kurtosis of
!!           zero. Positive kurtosis indicates a "peaked"
!!           distribution and negative kurtosis indicates a "flat"
!!           distribution.
!!
!!           Although often called kurtosis, historically the above expression is
!!           for "excess kurtosis" because three is subtracted from the value.
!!           The purpose of this is to give the normal distribution a kurtosis
!!           of 0. In recent times, the term "excess kurtosis" is often simply
!!           called "kurtosis", so consider that whether to subtract 3 or not
!!           is merely a convention, not a right or wrong answer. When using a
!!           particular program, you just need to be aware of which convention.
!!
!!           Again, another frequent difference is whether they use N in
!!           the denominator or the bias corrected N-1.
!!
!!           The formulas for skewness and kurtosis are treated in
!!
!!              Sokal, R. R., &amp; Rohlf, F. J. (1995).
!!              Biometry: The principles and practice of statistics in biological
!!              research. (3rd ed.) New York: W. H. Freeman, pp. 114-115.
!!
!!           Similar formulas are given in
!!
!!              Zar, J. H., Biostatistical analysis (3rd ed)., Prentice
!!              Hall, 1996.
!!
!!           which refers to "machine formulas" and cites
!!
!!              Bennett and Franklin, Statistical analysis in chemistry and
!!              the chemical industry. NY: Wiley, 1954, at p. 81.
!!##EXAMPLES
!!
subroutine bds (x,n,stat)
! ident_18="@(#) M_math bds(3f) Basic Descriptive Statistics (based on a routine from the IBM collection)"
!  RETURN WITH M,U2,U3,U4,V,S,G1,G2,BIG,SMALL,IB,IS IN THAT ORDER IN LOCATIONS STAT(1) THROUGH STAT(13)
!-----------------------------------------------------------------------
!  nobody likes equivalences any more
   integer,parameter :: mean   =1  ! mean
   integer,parameter :: u2     =2  ! second moment about the mean
   integer,parameter :: u3     =3  ! third  moment about the mean
   integer,parameter :: u4     =4  ! fourth moment about the mean
   integer,parameter :: varnce =5  ! variance
   integer,parameter :: sd     =6  ! standard deviation
   integer,parameter :: skew   =7  ! skewness
   integer,parameter :: kurto  =8  ! kurtosis
   integer,parameter :: sum    =9  ! sum

   integer,parameter :: big    =10 ! highest value
   integer,parameter :: small  =11 ! lowest value
   integer,parameter :: ib     =12 ! location of highest value
   integer,parameter :: is     =13 ! location of lowest value

   integer,intent(in) :: n
   real,intent(in)    :: x(n)
   real,intent(out)   :: stat(13)
   real               :: deltafixed
   real               :: deltasum
   real               :: fln
   integer            :: i, i20, i30
!-----------------------------------------------------------------------
      fln=n
!-----------------------------------------------------------------------
!  SUM AND MEAN AND LARGEST AND SMALLEST AND LOCATION OF EXTREMES
      stat(big)=x(1)                   ! biggest value
      stat(small)=x(1)                 ! smallest value
      stat(is)=1                       ! location of smallest
      stat(ib)=1                       ! location of biggest
      stat(sum) =0.0                   ! sum of all values
      do i=1,n
         stat(sum)= stat(sum) +x(i)    ! add all values into SUM
         if(x(i).lt.stat(small))then   ! find smallest value
            stat(small)=x(i)
            stat(is)=i
         endif
         if(x(i).gt.stat(big))then     ! find biggest value
            stat(big)=x(i)
            stat(ib)=i
         endif
      enddo
      stat(mean)= stat(sum)/fln
!-----------------------------------------------------------------------
!  SECOND, THIRD, FOURTH MOMENTS ABOUT THE MEAN
      stat(u2) = 0.0
      stat(u3) = 0.0
      stat(u4) = 0.0
      do i20=1,n
         deltafixed = (x(i20) - stat(mean))
         deltasum = deltafixed
         do i30=2,4
            deltasum = deltasum * deltafixed
            stat(i30) = stat(i30) + deltasum
         enddo
      enddo
!-----------------------------------------------------------------------
      stat(u2) = stat(u2) / fln
      stat(u3) = stat(u3) / fln
      stat(u4) = stat(u4) / fln
!-----------------------------------------------------------------------
!  VARIANCE, STANDARD DEVIATION
      stat(varnce) = fln * stat(u2) / (fln-1.0)
      stat(sd) = sqrt(stat(varnce))
!-----------------------------------------------------------------------
!  SKEWNESS, KURTOSIS
      stat(skew) = stat(u3) /(stat(u2) * sqrt(stat(u2)))
      stat(kurto) = stat(u4) / (stat(u2) * stat(u2)) - 3.0
!-----------------------------------------------------------------------
end subroutine bds
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    skekur1(3f) - [M_math:statistics] variant on calculating skewness and kurtosis of an array
!!
!!##SYNOPSIS
!!
!!    SUBROUTINE SKEKUR1(Y,NHI,YSKEW,YKURT,IOPT)
!!
!!     real,intent(in)    ::  y(*)
!!     integer,intent(in) :: nhi
!!     real,intent(out)   :: yskew
!!     real,intent(out)   :: ykurt
!!     integer,intent(in) :: iopt
!!
!!##DESCRIPTION
!!    FOR: Computing SKEWNESS and KURTOSIS for entries 1 through NHI
!!         in vector Y. The values may be centered about either the
!!         MEAN (IOPT <> 0) or about ZERO (IOPT = 0). The traditional
!!         divisor of N (NOT N-1) is used when the MEAN is estimated.
!!
!!    CURRENT VERSION COMPLETED FEBRUARY 28, 1986
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
!!
!!##AUTHOR
!!    Written by Charles P. Reeve
subroutine skekur1(y,nhi,yskew,ykurt,iopt)
! ident_19="@(#) m_math skekur1(3f) variant on calculating skewness and kurtosis of an array"
real,intent(in)    ::  y(*)
integer,intent(in) :: nhi
real,intent(out)   :: yskew
real,intent(out)   :: ykurt
integer,intent(in) :: iopt
   real            :: rn
   real            :: s
   integer         :: i
   real            :: t2, t3, t4
   real            :: d
      rn = real(nhi)
      if (iopt.eq.0) then
         s = 0.0
      else
         s = 0.0
         do i = 1, nhi
            s = s+y(i)
         enddo
         s = s/rn
      endif
      t2 = 0.0
      t3 = 0.0
      t4 = 0.0
      do i = 1, nhi
         d = y(i)-s
         t2 = t2+d**2
         t3 = t3+d**3
         t4 = t4+d**4
      enddo
      yskew=sqrt(rn)*t3/t2**1.5
      ykurt=rn*t4/t2**2
end subroutine skekur1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    skekurx(3f) - [M_math:statistics] Compute unbiased estimator of the population SKEWNESS and KURTOSIS
!!##SYNOPSIS
!!
!!    SUBROUTINE SKEKURX(Y,N,YSKEW,YKURT)
!!
!!       integer,intent(in) :: n
!!       real,intent(in)    :: y(*)
!!       real,intent(out)   :: yskew
!!       real,intent(out)   :: ykurt
!!
!!##DESCRIPTION
!!    This routine calculates the unbiased estimator of the population kurtosis
!!    and skewness from a subset of samples
!!
!!
!!       kurt = {n*(n+1)/((n-1)*(n-2)*(n-3))*SUM[((x(i)-xbar)/stddev)**4]} -
!!                 3*(n-1)**2/((n-2)*(n-3))
!!
!!       skew =  ( n / ((n-1)*(n-2)) *SUM{((X(i) - xbar)/stddev)**3}
!!
!!    where xbar and stddev are the sample mean and standard deviation
!!
!!    Note that this is apparently the skewness and kurtosis calculated by the
!!    MicroSoft Excel product. I checked the Excel help and Excel uses the
!!    above formulas. No references are given in the Excel documentation. Note
!!    that this converges on the standard expression for excess kurtosis and
!!    skewness as N becomes large.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
!!##AUTHOR
!!      John S. Urban
!!
subroutine skekurx(y,n,yskew,ykurt)
! ident_20="@(#) M_math skekurx(3f) COMPUTE UNBIASED ESTIMATOR OF THE POPULATION SKEWNESS AND KURTOSIS"
      integer,intent(in) :: n
      real,intent(in)    :: y(*)
      real,intent(out)   :: yskew
      real,intent(out)   :: ykurt
      doubleprecision    :: xbar
      doubleprecision    :: rn
      doubleprecision    :: sum
      doubleprecision    :: sum3
      doubleprecision    :: sum4
      doubleprecision    :: stddev
      integer            :: i10,i20,i30
!-----------------------------------------------------------------------
      rn = n
!-----------------------------------------------------------------------
      ! GET AVERAGE
      xbar = 0.0d0
      do i10 = 1, n
         xbar = xbar+y(i10)
      enddo
      xbar = xbar/rn
!-----------------------------------------------------------------------
      ! GET STANDARD DEVIATION
      sum=0.0
      do i30=1,n
         sum=sum+(y(i30)-xbar)**2
      enddo
      stddev=sqrt(sum/(rn-1.0d0))
!-----------------------------------------------------------------------
      sum3=0.0
      sum4=0.0
      do i20=1,n
         sum3=sum3+((y(i20)-xbar)/stddev)**3
         sum4=sum4+((y(i20)-xbar)/stddev)**4
      enddo
!-----------------------------------------------------------------------
      yskew=rn/((rn-1.0d0)*(rn-2.0d0))*sum3
      ykurt= rn*(rn+1.0d0) / ((rn-1.0d0)*(rn-2.0d0)*(rn-3.0d0)) * sum4 - 3.0d0*(rn-1.0d0)**2/((rn-2.0d0)*(rn-3.0d0))
!-----------------------------------------------------------------------
end subroutine skekurx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!      ncr(3f) - [M_math] Calculate the number of unique combinations of r objects out of n.
!!
!!##SYNOPSIS
!!
!!   subroutine ncr(n,r,ncomb,ier)
!!
!!     integer, intent(in)     :: n
!!     integer, intent(in)     :: r
!!     !*!integer, parameter      :: dp = selected_real_kind(12, 60)
!!     integer, parameter      :: dp = kind(0.0d0)
!!     real (dp), intent(out)  :: ncomb
!!     integer, intent(out)    :: ier
!!
!!##DESCRIPTION
!!      Calculate the number of unique combinations of r objects out of n.
!!
!!##OPTIONS
!!      n      number of objects
!!      r      number of the objects to select in a set
!!      ncomp  returns number of unique combinations
!!      ier    returns error code
!!             * 0  if no error is detected
!!             * 1  if n < 1
!!             * 2  if r < 0
!!             * 3  if r > n
!!             * 4  if nCr > 1.e+308, i.e. if it overflows. In this case, the
!!                natural log of nCr is returned.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_ncr
!!    use m_math, only : ncr
!!    implicit none
!!    integer, parameter  :: dp = selected_real_kind(12, 60)
!!    integer             :: n, r, ier
!!    real (dp)           :: result
!!    !do
!!    !   write(*, '(a)', advance='no') ' Enter n, r : '
!!    !   read(*, *) n, r
!!       n=10
!!       r=2
!!       call ncr(n, r, result, ier)
!!       if (ier /= 0) then
!!          write(*, *) ' Error, IER = ', ier
!!          if (ier == 4) write(*, '(a, f12.5)') ' ln(ncr) = ', result
!!       else
!!          write(*, '(a, g16.8)') ' ncr = ', result
!!       endif
!!    !enddo
!!    end program demo_ncr
!!
!!   Results:
!!
!!     ncr =    45.000000
!!
!!##AUTHOR
!!    Alan Miller
subroutine ncr(n, r, ncomb, ier)
! Programmer: Alan.Miller @ cmis.csiro.au
! Latest revision - 28 July 1988 (Fortran 77 version)
! Code converted using TO_F90 by Alan Miller
! Date: 2000-01-20  Time: 18:08:52

implicit none
!*!INTEGER, PARAMETER      :: dp = SELECTED_REAL_KIND(12, 60)
integer, parameter      :: dp = kind(0.0d0)

integer, intent(in)     :: n
integer, intent(in)     :: r
real (dp), intent(out)  :: ncomb
integer, intent(out)    :: ier
integer :: rr, i, nn
   if (n < 1) then
      ier = 1
   elseif (r < 0) then
      ier = 2
   elseif (r > n) then
      ier = 3
   else
      ier = 0
   endif
   if (ier /= 0) return

   if (r <= n-r) then
      rr = r
   else
      rr = n - r
   endif

   if (rr == 0) then
      ncomb = 1.0_dp
      return
   endif

   if (rr > 25) then
      ncomb = lngamma(dble(n+1)) - lngamma(dble(r+1)) - lngamma(dble(n-r+1))
      if (ncomb > 709._dp) then
         ier = 4
      else
         ncomb = exp(ncomb)
      endif
      return
   endif

   ncomb = n
   i = 1
   nn = n
   do
      if (i == rr) return
      nn = nn - 1
      i = i + 1
      ncomb = (ncomb * nn) / real(i)
   enddo

end subroutine ncr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function lngamma(z) result(lanczos)

!  Uses Lanczos-type approximation to ln(gamma) for z > 0.
!  Reference:
!       Lanczos, C. 'A precision approximation of the gamma
!               function', J. SIAM Numer. Anal., B, 1, 86-96, 1964.
!  Accuracy: About 14 significant digits except for small regions
!            in the vicinity of 1 and 2.

!  Programmer: Alan Miller
!              1 Creswick Street, Brighton, Vic. 3187, Australia
!  Latest revision - 14 October 1996

   implicit none
   !*!INTEGER, PARAMETER    :: dp = SELECTED_REAL_KIND(12, 60)
    integer, parameter      :: dp = kind(0.0d0)
   real(dp), intent(in)  :: z
   real(dp)              :: lanczos

! Local variables

real(dp)  :: a(9) = (/ 0.9999999999995183d0, 676.5203681218835d0, &
   -1259.139216722289d0, 771.3234287757674d0, &
   -176.6150291498386d0, 12.50734324009056d0, &
   -0.1385710331296526d0, 0.9934937113930748d-05, &
   0.1659470187408462d-06 /), zero = 0.d0,   &
   one = 1.d0, lnsqrt2pi = 0.9189385332046727d0, &
   half = 0.5d0, sixpt5 = 6.5d0, seven = 7.d0, tmp
integer          :: j

   if (z <= zero) then
      write(*, *) 'Error: zero or -ve argument for lngamma'
      return
   end if

   lanczos = zero
   tmp = z + seven
   do j = 9, 2, -1
      lanczos = lanczos + a(j)/tmp
      tmp = tmp - one
   end do
   lanczos = lanczos + a(1)
   lanczos = log(lanczos) + lnsqrt2pi - (z + sixpt5) + (z - half)*log(z + sixpt5)
end function lngamma
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    stddev(3f) - [M_math:statistics] given a real vector and the vector average calculate the standard deviation
!!
!!##SYNTAX
!!    function stddev(vector,n,avg)
!!
!!     integer,intent(in) :: n
!!     real,intent(in)    :: vector(n)
!!     real,intent(in)    :: avg
!!     real               :: stddev
!!
!!##DESCRIPTION
!!    Clearly the average gives one number around which the n observations
!!    tend to cluster. And the standard deviation gives a measure of how the
!!    n observations vary or spread about this average. The square of the
!!    standard deviation is called the variance. If we consider a unit mass
!!    at each point x(i) , then the variance is equivalent to a moment of
!!    inertia about an axis through x(avg). It is readily seen that for a
!!    fixed value of x(avg), greater spreads from the average will produce
!!    larger values of the standard deviation s. The average and the standard
!!    deviation can be used jointly to summarize where the observations are
!!    concentrated. Tchebysheff's theorem states :
!!
!!     A fraction of at least 1 - (1/k**2) of the observations lie
!!     within k standard deviations of the average. The theorem
!!     guarantees lower bounds on the percentage of observations
!!     within k standard deviations of the average.
!!
!!##OPTIONS
!!    n            the size of the input vector
!!    vector(n)    the input vector
!!    avg          the average of the input vector
!!
!!##RETURNS
!!    stddev       the standard deviation of the vector
!!
!!##EXAMPLE
!!
!!   example:
!!
!!     program demo_stddev
!!     use M_math, only : stddev
!!     implicit none
!!     integer :: i
!!     real,parameter :: vals(*)=[(i*1.0,i=0,100)]
!!        !*!write(*,*)vals
!!        write(*,*)size(vals)
!!        write(*,*)sum(vals)/size(vals)
!!        write(*,*)stddev(vals,size(vals),sum(vals)/size(vals))
!!     end program demo_stddev
!!
!!   output:
!!
!!          101
!!    50.0000000
!!    29.3001709
!!
!!##AUTHOR
!!    1994 John S. Urban
!!
!!##REFERENCE
!!    From Mark's Handbook, page 17-19, 8th edition
function stddev(vector,n,avg)
implicit none
! ident_21="@(#) M_math stddev(3f) find standard deviation of a real array"
integer,intent(in) :: n            ! number of elements in input array (vector)
real,intent(in)    :: vector(n)    ! input vector
real,intent(in)    :: avg          ! average of array
real               :: stddev

   integer         :: i10
   real            :: sum

   sum=0.0
   do i10=1,n
      sum=sum+(vector(i10)-avg)**2
   enddo
   stddev=sqrt(sum/(n-1))
end function stddev

!>
!!##NAME
!!     scale1(3f) - [M_math] find new range xMINP XMAXP divisible into approximately N linear intervals of size DIST
!!
!!##SYNOPSIS
!!
!!   subroutine scale1(xmin, xmax, n, xminp, xmaxp, dist)
!!
!!    real,intent(in)      :: xmin, xmax
!!    integer,intent(in)   :: n
!!    real,intent(out)     :: xminp, xmaxp, dist
!!
!!##DESCRIPTION
!!
!!    Find new range divisible into approximately n linear intervals using
!!    "CACM Algorithm 463 scale1". Typically used to find nice ranges for
!!    axis scales.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_scale1
!!     use M_math, only : scale1
!!     implicit none
!!     real :: start, end
!!     real :: xminp, xmaxp, dist
!!     integer :: intervals
!!     intervals=5
!!     write(*,*)'Enter start and end values'
!!     do
!!       read(*,*,end=999)start,end
!!       call scale1(start,end,intervals,xminp,xmaxp,dist)
!!       write(*,'(a,g0,a,g0,a,i0,a,g0)') &
!!               & 'nice range is ',xminp,' to ',xmaxp,' by ', &
!!               & nint((xmaxp-xminp)/dist),' intervals of ',dist
!!     enddo
!!     999 continue
!!     end program demo_scale1
!!
!!    Example output
!!
!!     printf '3 87 \n 0.1 2.3 \n -20 30|demo_scale1
!!      Enter start and end values
!!     nice range is 0.00000000 to 100.000000 by 5 intervals of 20.0000000
!!     nice range is 0.00000000 to 2.50000000 by 5 intervals of 0.500000000
!!     nice range is -20.0000000 to 30.0000000 by 5 intervals of 10.0000000
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine scale1(xmin0, xmax0, n0, xminp, xmaxp, dist)
!-----------------------------------------------------------------------------------------------------------------------------------
use M_framework__journal, only : journal
implicit none
! ident_22="@(#) M_math scale1(3f) given xmin xmax n find new range xminp xmaxp divisible into approximately n linear intervals of size dist"
!-----------------------------------------------------------------------------------------------------------------------------------
   real,intent(in)      :: xmin0, xmax0
   integer,intent(in)   :: n0
   real,intent(out)     :: xminp, xmaxp, dist
!-----------------------------------------------------------------------------------------------------------------------------------
   integer              :: n
   doubleprecision      :: xmin, xmax, xhold, dist8

   ! vint is an array of acceptable values for dist (times an integer power of 10)
   doubleprecision,parameter :: vint(4)= [1.0d0, 2.0d0, 5.0d0, 10.0d0]

   ! sqr is used as break points to determine which vint value to use
   ! (sqr is an array of geometric means of adjacent values of vint).
   doubleprecision,parameter :: sqr(3)=  [sqrt(2.0d0), sqrt(10.0d0), sqrt(50.0d0)]

   doubleprecision           :: fn, a, al, b, fm1
   integer                   :: i, nal, m1, ivint
   doubleprecision,parameter :: del = 0.000002d0
!-----------------------------------------------------------------------------------------------------------------------------------
   xmin=dble(xmin0)
   xmax=dble(xmax0)
   n=n0
!-----------------------------------------------------------------------------------------------------------------------------------
!  check whether proper input values were supplied
   if(xmin.gt.xmax)then ! ensure xmin is less than xmax
      call journal('*scale1* max was less than min')
      xhold=xmin
      xmin=xmax
      xmax=xhold
   endif
   if(n.le.0)then
      call journal('*scale1* number of axis divisions <= 0')
      n=5
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  avoid problem of a scale of zero length
   if(xmin.eq.xmax)then
      xmax=xmax+del
      xmin=xmin-del
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   fn=dble(n)
!  find approximate interval size a
   a = (xmax - xmin) / fn
   if (abs(a) .lt. 1.0d-30) then
      xmax = xmax + 1.0d-30
      xmin = xmin - 1.0d-30
      a = (xmax - xmin) / fn
   endif
   al = log10(a) ! from above checks, a is always positive, non-zero
   nal = int(al)
   if (a .lt. 1.0d0)then
      nal = nal - 1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  a is scaled into variable named b between 1 and 10
!  try and use integer multiplication instead of logarithmic operations
   if(nal.ge.0)then
      b= a/10.0d0**nal
   else
      b=a*10.0d0**abs(nal)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  the closest permissible value for b is found
   CLOSEST: block
   do i=1,3
      if( b .le. sqr(i) )then
         ivint=i
         exit CLOSEST
      endif
   enddo
   ivint=4
   endblock CLOSEST
!-----------------------------------------------------------------------------------------------------------------------------------
!  the interval size is computed
   if(nal.gt.0)then
      dist8 = vint(ivint) * 10.0d0**nal
   else
      dist8 = vint(ivint) / 10.0d0**abs(nal)
   endif
   fm1 = xmin / dist8
   m1 = fm1
   if (fm1 .lt. 0.0)then
      m1 = m1 - 1
   endif
   if (abs((m1 + 1.0d0 - fm1)) .lt. del)then
      m1 = m1 + 1
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
!  the new minimum and maximum limits are found
   xminp = dist8 * dble(m1)
   fm1 = xmax / dist8
   m1 = int(fm1 + 1.0d0)
   if(fm1.lt.-1.0d0.or.abs((fm1 + 1.0d0 - dble(m1))).lt.del)then
      m1 = m1 - 1
   endif
   xmaxp = dist8 * dble(m1)
!-----------------------------------------------------------------------------------------------------------------------------------
!  adjust limits to account for round-off if necessary
   if (xminp .gt. xmin)then
      xminp = xmin
   endif
   if (xmaxp .lt. xmax)then
      xmaxp = xmax
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   dist=dist8
end subroutine scale1
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      scale3(3f) - [M_math] find nice log range
!!
!!##SYNOPSIS
!!
!!   subroutine scale3(xmin, xmax, n, xminp, xmaxp, dist)
!!
!!    real,intent(in)      :: xmin, xmax
!!    integer,intent(in)   :: n
!!    real,intent(out)     :: xminp, xmaxp, dist
!!
!!##DESCRIPTION
!!
!!    Find nice logarithmic range using "CACM Algorithm 463 scale3".
!!    Typically used to find nice ranges for axis scales. Given XMIN, XMAX
!!    and N, where N is greater than 1, find new log range. Finds a new
!!    range XMINP and XMAXP divisible into exactly N LOGARITHMIC intervals,
!!    where the ratio of adjacent uniformly spaced scale values is DIST.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_scale3
!!     use M_math, only : scale3
!!     implicit none
!!     real :: start, end
!!     real :: xminp, xmaxp, dist
!!     integer :: intervals
!!     integer :: iostat
!!     intervals=5
!!     write(*,'(a)',advance='no')'Enter start and end values:'
!!     do
!!       read(*,*,iostat=iostat)start,end
!!       if(iostat.ne.0)exit
!!       call scale3(start,end,intervals,xminp,xmaxp,dist)
!!       write(*,'(*(g0))')                                &
!!       & 'nice log range is 10**', log10(xminp),         &
!!       & ' to 10**',log10(xmaxp),                        &
!!       & ' by ', nint((log10(xmaxp)-log10(xminp))/dist), &
!!       & ' intervals of 10**',dist
!!     enddo
!!     end program demo_scale3
!===================================================================================================================================
subroutine scale3(xmin0, xmax0, n0 , xminp, xmaxp, dist)
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
use M_framework__journal, only : journal
implicit none
! ident_23="@(#) M_math scale3(3f) find nice log range."

real,intent(in)           :: xmin0, xmax0
integer,intent(in)        :: n0
real,intent(out)          :: xminp, xmaxp, dist

doubleprecision,parameter :: del = 0.000002d0
doubleprecision           :: xmin, xmax, hold
integer                   :: n
doubleprecision           :: xminl, xmaxl, fn, a, al, b, distl, fm1, fm2
integer                   :: nal, i, m1, m2, np, nx, iv
doubleprecision,save      :: vint(11) = [10.0d0,9.0d0,8.0d0,7.0d0,6.0d0,5.0d0,4.0d0,3.0d0,2.0d0,1.0d0,0.5d0]
!-----------------------------------------------------------------------------------------------------------------------------------
!  Check whether proper input values were supplied.
   xmin=xmin0
   xmax=xmax0
   n=n0
   if(xmin .gt. xmax)then
      hold=xmin
      xmin=xmax
      xmax=hold
      call journal('*scale3* max was less than min')
   endif
  if (n .le. 0)then
     call journal('*scale3* requested number of divisions <= 0')
     n=5
  endif
  if( xmin .le. 0.0 )then
     call journal('*scale3* zero or negative minimum value')
     xminp=xmin
     xmaxp=xmax
     dist=(xmax-xmin)/n
     return
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
! check what happens when xmin0=xmax0
  if (xmin .eq. xmax) then
     xmax=xmax*2
  endif
!-----------------------------------------------------------------------------------------------------------------------------------
! Values are translated from the linear region to the logarithmic
  xminl = log10(xmin)
  xmaxl = log10(xmax)
  fn = dble(n)
!===================================================================================================================================
! Find approximate interval size a
  a = (xmaxl - xminl) / fn
  al = log10(a)
  nal = int(al)
  if (a .lt. 1.0d0)then
     nal = nal - 1
  endif
! a is scaled into the variable named b between 1 and 10
  b = a / 10.00d0**nal
!===================================================================================================================================
! The closest permissible value for b is found
  CLOSEST: block
  do i = 1,9
     iv=i
     if (b .lt. (10.0d0 / vint(i) + del)) exit CLOSEST
  enddo
  iv = 10
  endblock CLOSEST
!===================================================================================================================================
!     The interval size is computed
   INTERVAL_SIZE: do
      distl = 10.0d0**(nal + 1) / vint(iv)
      fm1 = xminl / distl
      m1 = int(fm1)
      if (fm1 .lt. 0.0d0) then
         m1 = m1 -1
      endif
      if (abs(dble(m1) + 1.0d0 - fm1) .lt. del)then
         m1 = m1 - 1
      endif
   !
   !  The new minimum and maximum limits are found
      xminp = distl * dble(m1)
      fm2 = xmaxl / distl
      m2 = int(fm2 + 1.0d0)
      if (fm2 .lt. -1.0d0)then
         m2 = m2 -1
      endif
      if (abs(fm2 + 1.0d0 - dble(m2)) .lt. del)then
         m2 = m2 -1
      endif
      xmaxp = distl * dble(m2)
   !
   !  Check whether another pass is necessary
      np = m2 - m1
      iv = iv + 1
      if( np .le. n) exit INTERVAL_SIZE
   enddo INTERVAL_SIZE
!===================================================================================================================================
   nx = (n - np) / 2
   xminp = xminp - nx * distl
   xmaxp = xminp + n * distl
!===================================================================================================================================
!  Values are translated from the logarithmic into the linear region.
   dist = distl
   xminp = 10.0d0**dble(xminp)
   xmaxp = 10.0d0**dble(xmaxp)
!===================================================================================================================================
!  Adjust limits to account for round-off if necessary
   if(xminp .gt. xmin)then
      xminp = xmin
   endif
   if(xmaxp .lt. xmax)then
      xmaxp = xmax
   endif
!===================================================================================================================================
end subroutine scale3
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------

!>
!!##NAME
!!    quadratic(3f) - [M_math] calculate the roots of a quadratic formula even if they are complex
!!
!!##SYNOPSIS
!!
!!   subroutine quadratic(a,b,c,z1,z2,discriminant)
!!
!!    real,intent(in) :: a, b, c
!!    complex,intent(out) :: z1, z2
!!    real,intent(out) :: discriminant
!!
!!##DESCRIPTION
!!    Given the equation
!!
!!       a*x**2+b*x+c=0
!!
!!    Use the quadratic formula to determine the root values and the
!!    discriminant of the equation.
!!
!!##OPTIONS
!!    a,b,c  coefficients
!!
!!##RETURNS
!!    z1,z2  roots
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_quadratic
!!    use M_math, only : quadratic
!!    implicit none
!!    ! Calculate and print the roots of a quadratic formula
!!    ! even if they are complex
!!    real    :: a, b, c ! coefficients
!!    complex :: z1, z2  ! roots
!!    real    :: discriminant
!!       a = 4.0
!!       b = 8.0
!!       c = 21.0
!!       call quadratic(a,b,c,z1,z2,discriminant) !  Calculate the roots
!!       if (abs(discriminant) < 0) then
!!          write(*,*) "the roots are real and equal:"
!!       else if (discriminant > 0) then
!!          write(*,*) "the roots are real:"
!!       else
!!          write(*,*) "the roots are complex:"
!!       end if
!!    !  Print the roots
!!       print *, "The roots(ie. x-intercepts)  are:"
!!       print *, "z1 =", z1
!!       print *, "z2 =", z2
!!    end program demo_quadratic
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine quadratic(a,b,c,z1,z2,discriminant)
implicit none
! ident_24="@(#) M_math quadratic(3f) calculate the roots of a quadratic formula even if they are complex"
real,intent(in)     :: a, b, c         ! coefficients
complex,intent(out) :: z1, z2          ! roots
real,intent(out)    :: discriminant

!  Calculate the roots
if(a.ne.0)then
   z1 = (-b + sqrt (cmplx (b**2 - 4*a*c))) / (2*a)
   z2 = (-b - sqrt (cmplx (b**2 - 4*a*c))) / (2*a)
else
   ! Y=Bx+C
   ! 0=Bx+C
   ! -C=Bx
   ! -C/B
   if(B.ne.0)then
      write(*,*)'*quadratic* WARNING: If A=0 this is a linear, not quadratic, equation'
      z1 = -C/B
      z2 = -C/B
   elseif(C.eq.0)then
      write(*,*)'*quadratic* WARNING: If A,B,C are 0 this is a line on the x axis (with infinte roots), not a quadratic equation'
      z1 = 0.0
      z2 = 0.0
   else
      write(*,*)'*quadratic* WARNING: If A and B=0 this is a horizontal line not on the x axis (no roots), not a quadratic equation'
      z1 = 0.0
      z2 = 0.0
   endif
endif

   discriminant = b*b - 4.0*a*c

end subroutine quadratic

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!   magic_square(3f) - [M_math] create an N x N magic square array, N>2
!!##SYNOPSIS
!!
!!   subroutine magic_square(array)
!!
!!    class(*) :: array
!!
!!##DESCRIPTION
!!    This procedure returns a magic squares array, an n by n matrix in
!!    which each integer 1, 2, ..., n*n appears exactly once; and all columns,
!!    rows, and diagonals sum to the same number.
!!
!!##OPTIONS
!!    array  An array to fill with the magic square values. The
!!           smallest dimension should be >= 3. Since a square is required
!!           only the values array(1:n,1:n) will be filled, where
!!           n=min(rows,columns).
!!
!!           The array may be INTEGER, REAL, or DOUBLEPRECISION.
!!
!!           Note that the routine allocates an integer array of the size
!!           determined by the input routine during execution.
!!
!!##AUTHOR
!!   John S. Urban
!!
!!   Based on an algorithm for magic squares from
!!
!!     Mathematical Recreations and Essays, 12th ed.,
!!     by W. W. Rouse Ball and H. S. M. Coxeter
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_magic_square
!!    use M_math, only : magic_square
!!    implicit none
!!    integer           :: arr(15,15)
!!    integer           :: i, j, k
!!       do k=1,15
!!          write(*,*)'K=',k
!!          call magic_square(arr(:k,:k))
!!          do i=1,k
!!             write(*,'(i2,":",*(i5):)')i,(int(arr(i,j)),j=1,k),sum(arr(k,:k))
!!          enddo
!!       enddo
!!    end program demo_magic_square
subroutine magic_square(array)
implicit none

! ident_25="@(#) m_matrix magic_square(3f) create a magic square"

class(*)            :: array(:,:)
integer,allocatable :: iarray(:,:)
integer             :: cols
integer             :: rows
integer             :: n

integer             :: t
integer             :: i,  j,  m
integer             :: i1, j1, m1
integer             :: im, jm, mm
integer             :: k
integer             :: m2
!
   select type(array)
   type is (integer)
      rows=size(array,dim=1)
      cols=size(array,dim=2)
   type is (real)
      rows=size(array,dim=1)
      cols=size(array,dim=2)
   type is (doubleprecision)
      rows=size(array,dim=1)
      cols=size(array,dim=2)
   end select

   n=min(rows,cols)
   allocate(iarray(n,n))
   if (mod(n,4) .eq. 0) then
!
!  double even order
!
      k = 1
      do i = 1, n
         do j = 1, n
            iarray(i,j) = k
            if (mod(i,4)/2 .eq. mod(j,4)/2) iarray(i,j) = n*n+1 - k
            k = k+1
         enddo
      enddo
   else
      if (mod(n,2) .eq. 0) then
         m = n/2
      else
         m = n
      endif
!
!     odd order or upper corner of even order
!
      iarray(:m,:m)=0
      i = 1
      j = (m+1)/2
      mm = m*m
      do k = 1, mm
         iarray(i,j) = k
         i1 = i-1
         j1 = j+1
         if(i1.lt.1) i1 = m
         if(j1.gt.m) j1 = 1
         if(iarray(i1,j1).ne.0) then
            i1 = i+1
            j1 = j
         endif
         i = i1
         j = j1
      enddo
      if (mod(n,2) .eq. 0) then
!
!     rest of even order
!
         t = m*m
         do i = 1, m
            do j = 1, m
               im = i+m
               jm = j+m
               iarray(i,jm) = iarray(i,j) + 2*t
               iarray(im,j) = iarray(i,j) + 3*t
               iarray(im,jm) = iarray(i,j) + t
            enddo
         enddo
         m1 = (m-1)/2
         if (m1.ne.0) then
            do j = 1, m1
               call iswap(m,iarray(1:,j),iarray(m+1:,j))
            enddo
            m1 = (m+1)/2
            m2 = m1 + m
            call iswap(1,iarray(m1:m1,1),iarray(m2:m2,1))
            call iswap(1,iarray(m1:m1,m1),iarray(m2:m2,m1))
            m1 = n+1-(m-3)/2
            if(m1.le.n)then
               do j = m1, n
                  call iswap(m,iarray(1:,j),iarray(m+1:,j))
               enddo
            endif
         endif
      endif
   endif
   select type(array)
   type is (integer)
      array=iarray
   type is (real)
      array=real(iarray)
   type is (doubleprecision)
      array=dble(iarray)
   end select
   deallocate(iarray)
end subroutine magic_square
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine iswap(n,x,y)

! ident_26="@(#) m_matrix iswap(3f) swap two integer arrays"

integer,intent(in) :: n
integer            :: x(:),y(:)

integer            :: temp
integer            :: i

   if(n.gt.0)then
      do i = 1, n
         temp = x(n)
         x(n) = y(n)
         y(n) = temp
      enddo
   endif
end subroutine iswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    nextp(3f) - [M_math] next permutation of a previously sorted integer array
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine nextp(n,a)
!!
!!    integer,parameter     :: dp=kind(0.0d0)
!!    integer,intent(in)    :: n
!!    integer,intent(inout) :: a(:)
!!##DESCRIPTION
!!    Permutation generation in lexicographic order
!!
!!    There are many ways to systematically generate all permutations of a
!!    given sequence. One classic, simple, and flexible algorithm is based
!!    upon finding the next permutation in lexicographic ordering, if it
!!    exists. It can handle repeated values, for which case it generates each
!!    distinct multiset permutation once. Even for ordinary permutations
!!    it is significantly more efficient than generating values for the
!!    Lehmer code in lexicographic order (possibly using the factorial
!!    number system) and converting those to permutations.
!!
!!    It begins by sorting the sequence in (weakly) increasing order (which
!!    gives its lexicographically minimal permutation), and then repeats
!!    advancing to the next permutation as long as one is found. The method
!!    goes back to Narayana Pandita in 14th century India, and has been
!!    rediscovered frequently.
!!
!!    The following algorithm generates the next permutation
!!    lexicographically after a given permutation. It changes the given
!!    permutation in-place.
!!
!!     * Find the largest index k such that a[k] < a[k + 1]. If no such index
!!       exists, the permutation is the last permutation.
!!     * Find the largest index l greater than k such that a[k] < a[l].
!!     * Swap the value of a[k] with that of a[l].
!!     * Reverse the sequence from a[k + 1] up to and including the final
!!       element a[n].
!!
!!    For example, given the sequence [10, 20, 30, 40] (which is in increasing
!!    order), and given that the index is one-based, the steps are as follows:
!!
!!    1) Index k = 3, because 30 is placed at an index that satisfies condition
!!       of being the largest index that is still less than a[k + 1] which is 40.
!!       if no such index exists no more permutations exist
!!
!!    2) Index l = 4, because 40 is the only value in the sequence that is greater
!!       than 30 in order to satisfy the condition a[k] < a[l].
!!
!!    3) The values of a[3] and a[4] are swapped to form the new sequence
!!       [10,20,40,30].
!!
!!    4) The sequence after k-index a[3] to the final element is reversed. Because
!!       only one value lies after this index (the 30), the sequence remains
!!       unchanged in this instance. Thus the lexicographic successor of the
!!       initial state is permuted: [10,20,40,30].
!!
!!    5) Following this algorithm, the next lexicographic permutation will be
!!       [10,30,20,40], and the 24th permutation will be [40,30,20,10] at which point a[k]
!!       < a[k + 1] does not exist, indicating that this is the last permutation.
!!
!!    This method uses about 3 comparisons and 1.5 swaps per permutation,
!!    amortized over the whole sequence, not counting the initial sort.
!!##EXAMPLE
!!
!!
!! Sample program:
!!
!!       program demo_nextp
!!       use M_math, only : nextp
!!       integer,parameter :: n=4
!!       integer i,a(n)
!!       a=[(i,i=1,n)]  ! Must be sorted from smallest to largest
!!       do
!!          print *,(a(i),i=1,n)
!!          if(.not.nextp(n,a)) exit
!!       enddo
!!       end program demo_nextp
!!
!!##REFERENCES
!!    Wikipedia
!!
!!##WRITTEN BY
!!
!!##LICENSE
!!    Public Domain
function nextp(n,a)
implicit none
integer n , a , i , j , k , t
logical nextp
dimension a(n)
   i = n - 1
   do while ( a(i)>=a(i+1) )
      i = i - 1
      if ( i==0 ) exit
   enddo
   j = i + 1
   k = n
   PERMUTE: block
      do
         t = a(j)
         a(j) = a(k)
         a(k) = t
         j = j + 1
         k = k - 1
         if ( j>=k ) then
            j = i
            if ( j==0 ) exit
            do
               j = j + 1
               if ( a(j)>=a(i) ) then
                  t = a(i)
                  a(i) = a(j)
                  a(j) = t
                  nextp = .true.
                  exit PERMUTE
               endif
            enddo
         endif
      enddo
      nextp = .false.
      return
   endblock PERMUTE
end function nextp

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_2x2(a) result(b)
! ident_27="@(#) M_math invert_2x2(3f) performs a direct calculation of the inverse of a 2x2 matrix"
   integer,parameter         :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: a(2,2)   !! Matrix
   real(kind=wp)             :: b(2,2)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2) - a(1,2)*a(2,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * a(2,2)
   b(2,1) = -detinv * a(2,1)
   b(1,2) = -detinv * a(1,2)
   b(2,2) = +detinv * a(1,1)
end function double_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_3x3(a) result(b)
! ident_28="@(#) M_math invert_3x3(3f) performs a direct calculation of the inverse of a 3x3 matrix"
   integer,parameter         :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: a(3,3)   !! Matrix
   real(kind=wp)             :: b(3,3)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2)*a(3,3) - a(1,1)*a(2,3)*a(3,2)&
             - a(1,2)*a(2,1)*a(3,3) + a(1,2)*a(2,3)*a(3,1)&
             + a(1,3)*a(2,1)*a(3,2) - a(1,3)*a(2,2)*a(3,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * (a(2,2)*a(3,3) - a(2,3)*a(3,2))
   b(2,1) = -detinv * (a(2,1)*a(3,3) - a(2,3)*a(3,1))
   b(3,1) = +detinv * (a(2,1)*a(3,2) - a(2,2)*a(3,1))
   b(1,2) = -detinv * (a(1,2)*a(3,3) - a(1,3)*a(3,2))
   b(2,2) = +detinv * (a(1,1)*a(3,3) - a(1,3)*a(3,1))
   b(3,2) = -detinv * (a(1,1)*a(3,2) - a(1,2)*a(3,1))
   b(1,3) = +detinv * (a(1,2)*a(2,3) - a(1,3)*a(2,2))
   b(2,3) = -detinv * (a(1,1)*a(2,3) - a(1,3)*a(2,1))
   b(3,3) = +detinv * (a(1,1)*a(2,2) - a(1,2)*a(2,1))
end function double_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_4x4(a) result(b)
! ident_29="@(#) M_math invert_4x4(3f) performs a direct calculation of the inverse of a 4x4 matrix"
   integer,parameter            :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: a(4,4)   !! Matrix
   real(kind=wp)             :: b(4,4)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(a(1,1)*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))&
      - a(1,2)*(a(2,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))&
      + a(1,3)*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))&
      - a(1,4)*(a(2,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(2,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(2,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1))))

   ! Calculate the inverse of the matrix
   b(1,1) = detinv*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))
   b(2,1) = detinv*(a(2,1)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(2,3)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(2,4)*(a(3,3)*a(4,1)-a(3,1)*a(4,3)))
   b(3,1) = detinv*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(4,1) = detinv*(a(2,1)*(a(3,3)*a(4,2)-a(3,2)*a(4,3))+a(2,2)*(a(3,1)*a(4,3)-a(3,3)*a(4,1))+a(2,3)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(1,2) = detinv*(a(1,2)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(1,3)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(1,4)*(a(3,3)*a(4,2)-a(3,2)*a(4,3)))
   b(2,2) = detinv*(a(1,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(1,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(1,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))
   b(3,2) = detinv*(a(1,1)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(1,2)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(1,4)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(4,2) = detinv*(a(1,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(1,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(1,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(1,3) = detinv*(a(1,2)*(a(2,3)*a(4,4)-a(2,4)*a(4,3))+a(1,3)*(a(2,4)*a(4,2)-a(2,2)*a(4,4))+a(1,4)*(a(2,2)*a(4,3)-a(2,3)*a(4,2)))
   b(2,3) = detinv*(a(1,1)*(a(2,4)*a(4,3)-a(2,3)*a(4,4))+a(1,3)*(a(2,1)*a(4,4)-a(2,4)*a(4,1))+a(1,4)*(a(2,3)*a(4,1)-a(2,1)*a(4,3)))
   b(3,3) = detinv*(a(1,1)*(a(2,2)*a(4,4)-a(2,4)*a(4,2))+a(1,2)*(a(2,4)*a(4,1)-a(2,1)*a(4,4))+a(1,4)*(a(2,1)*a(4,2)-a(2,2)*a(4,1)))
   b(4,3) = detinv*(a(1,1)*(a(2,3)*a(4,2)-a(2,2)*a(4,3))+a(1,2)*(a(2,1)*a(4,3)-a(2,3)*a(4,1))+a(1,3)*(a(2,2)*a(4,1)-a(2,1)*a(4,2)))
   b(1,4) = detinv*(a(1,2)*(a(2,4)*a(3,3)-a(2,3)*a(3,4))+a(1,3)*(a(2,2)*a(3,4)-a(2,4)*a(3,2))+a(1,4)*(a(2,3)*a(3,2)-a(2,2)*a(3,3)))
   b(2,4) = detinv*(a(1,1)*(a(2,3)*a(3,4)-a(2,4)*a(3,3))+a(1,3)*(a(2,4)*a(3,1)-a(2,1)*a(3,4))+a(1,4)*(a(2,1)*a(3,3)-a(2,3)*a(3,1)))
   b(3,4) = detinv*(a(1,1)*(a(2,4)*a(3,2)-a(2,2)*a(3,4))+a(1,2)*(a(2,1)*a(3,4)-a(2,4)*a(3,1))+a(1,4)*(a(2,2)*a(3,1)-a(2,1)*a(3,2)))
   b(4,4) = detinv*(a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1)))
end function double_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_2x2(a) result(b)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter         :: wp=kind(0)
   integer(kind=wp), intent(in) :: a(2,2)   !! Matrix
   integer(kind=wp)             :: b(2,2)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2) - a(1,2)*a(2,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * a(2,2)
   b(2,1) = -detinv * a(2,1)
   b(1,2) = -detinv * a(1,2)
   b(2,2) = +detinv * a(1,1)
end function integer_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_3x3(a) result(b)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter         :: wp=kind(0)
   integer(kind=wp), intent(in) :: a(3,3)   !! Matrix
   integer(kind=wp)             :: b(3,3)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2)*a(3,3) - a(1,1)*a(2,3)*a(3,2)&
             - a(1,2)*a(2,1)*a(3,3) + a(1,2)*a(2,3)*a(3,1)&
             + a(1,3)*a(2,1)*a(3,2) - a(1,3)*a(2,2)*a(3,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * (a(2,2)*a(3,3) - a(2,3)*a(3,2))
   b(2,1) = -detinv * (a(2,1)*a(3,3) - a(2,3)*a(3,1))
   b(3,1) = +detinv * (a(2,1)*a(3,2) - a(2,2)*a(3,1))
   b(1,2) = -detinv * (a(1,2)*a(3,3) - a(1,3)*a(3,2))
   b(2,2) = +detinv * (a(1,1)*a(3,3) - a(1,3)*a(3,1))
   b(3,2) = -detinv * (a(1,1)*a(3,2) - a(1,2)*a(3,1))
   b(1,3) = +detinv * (a(1,2)*a(2,3) - a(1,3)*a(2,2))
   b(2,3) = -detinv * (a(1,1)*a(2,3) - a(1,3)*a(2,1))
   b(3,3) = +detinv * (a(1,1)*a(2,2) - a(1,2)*a(2,1))
end function integer_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_4x4(a) result(b)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind(0)
   integer(kind=wp), intent(in) :: a(4,4)   !! Matrix
   integer(kind=wp)             :: b(4,4)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(a(1,1)*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))&
      - a(1,2)*(a(2,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))&
      + a(1,3)*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))&
      - a(1,4)*(a(2,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(2,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(2,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1))))

   ! Calculate the inverse of the matrix
   b(1,1) = detinv*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))
   b(2,1) = detinv*(a(2,1)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(2,3)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(2,4)*(a(3,3)*a(4,1)-a(3,1)*a(4,3)))
   b(3,1) = detinv*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(4,1) = detinv*(a(2,1)*(a(3,3)*a(4,2)-a(3,2)*a(4,3))+a(2,2)*(a(3,1)*a(4,3)-a(3,3)*a(4,1))+a(2,3)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(1,2) = detinv*(a(1,2)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(1,3)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(1,4)*(a(3,3)*a(4,2)-a(3,2)*a(4,3)))
   b(2,2) = detinv*(a(1,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(1,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(1,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))
   b(3,2) = detinv*(a(1,1)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(1,2)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(1,4)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(4,2) = detinv*(a(1,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(1,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(1,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(1,3) = detinv*(a(1,2)*(a(2,3)*a(4,4)-a(2,4)*a(4,3))+a(1,3)*(a(2,4)*a(4,2)-a(2,2)*a(4,4))+a(1,4)*(a(2,2)*a(4,3)-a(2,3)*a(4,2)))
   b(2,3) = detinv*(a(1,1)*(a(2,4)*a(4,3)-a(2,3)*a(4,4))+a(1,3)*(a(2,1)*a(4,4)-a(2,4)*a(4,1))+a(1,4)*(a(2,3)*a(4,1)-a(2,1)*a(4,3)))
   b(3,3) = detinv*(a(1,1)*(a(2,2)*a(4,4)-a(2,4)*a(4,2))+a(1,2)*(a(2,4)*a(4,1)-a(2,1)*a(4,4))+a(1,4)*(a(2,1)*a(4,2)-a(2,2)*a(4,1)))
   b(4,3) = detinv*(a(1,1)*(a(2,3)*a(4,2)-a(2,2)*a(4,3))+a(1,2)*(a(2,1)*a(4,3)-a(2,3)*a(4,1))+a(1,3)*(a(2,2)*a(4,1)-a(2,1)*a(4,2)))
   b(1,4) = detinv*(a(1,2)*(a(2,4)*a(3,3)-a(2,3)*a(3,4))+a(1,3)*(a(2,2)*a(3,4)-a(2,4)*a(3,2))+a(1,4)*(a(2,3)*a(3,2)-a(2,2)*a(3,3)))
   b(2,4) = detinv*(a(1,1)*(a(2,3)*a(3,4)-a(2,4)*a(3,3))+a(1,3)*(a(2,4)*a(3,1)-a(2,1)*a(3,4))+a(1,4)*(a(2,1)*a(3,3)-a(2,3)*a(3,1)))
   b(3,4) = detinv*(a(1,1)*(a(2,4)*a(3,2)-a(2,2)*a(3,4))+a(1,2)*(a(2,1)*a(3,4)-a(2,4)*a(3,1))+a(1,4)*(a(2,2)*a(3,1)-a(2,1)*a(3,2)))
   b(4,4) = detinv*(a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1)))
end function integer_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_2x2(a) result(b)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter         :: wp=kind(0.0)
   real(kind=wp), intent(in) :: a(2,2)   !! Matrix
   real(kind=wp)             :: b(2,2)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2) - a(1,2)*a(2,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * a(2,2)
   b(2,1) = -detinv * a(2,1)
   b(1,2) = -detinv * a(1,2)
   b(2,2) = +detinv * a(1,1)
end function real_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_3x3(a) result(b)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter         :: wp=kind(0.0)
   real(kind=wp), intent(in) :: a(3,3)   !! Matrix
   real(kind=wp)             :: b(3,3)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2)*a(3,3) - a(1,1)*a(2,3)*a(3,2)&
             - a(1,2)*a(2,1)*a(3,3) + a(1,2)*a(2,3)*a(3,1)&
             + a(1,3)*a(2,1)*a(3,2) - a(1,3)*a(2,2)*a(3,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * (a(2,2)*a(3,3) - a(2,3)*a(3,2))
   b(2,1) = -detinv * (a(2,1)*a(3,3) - a(2,3)*a(3,1))
   b(3,1) = +detinv * (a(2,1)*a(3,2) - a(2,2)*a(3,1))
   b(1,2) = -detinv * (a(1,2)*a(3,3) - a(1,3)*a(3,2))
   b(2,2) = +detinv * (a(1,1)*a(3,3) - a(1,3)*a(3,1))
   b(3,2) = -detinv * (a(1,1)*a(3,2) - a(1,2)*a(3,1))
   b(1,3) = +detinv * (a(1,2)*a(2,3) - a(1,3)*a(2,2))
   b(2,3) = -detinv * (a(1,1)*a(2,3) - a(1,3)*a(2,1))
   b(3,3) = +detinv * (a(1,1)*a(2,2) - a(1,2)*a(2,1))
end function real_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_4x4(a) result(b)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind(0.0)
   real(kind=wp), intent(in) :: a(4,4)   !! Matrix
   real(kind=wp)             :: b(4,4)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(a(1,1)*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))&
      - a(1,2)*(a(2,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))&
      + a(1,3)*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))&
      - a(1,4)*(a(2,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(2,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(2,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1))))

   ! Calculate the inverse of the matrix
   b(1,1) = detinv*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))
   b(2,1) = detinv*(a(2,1)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(2,3)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(2,4)*(a(3,3)*a(4,1)-a(3,1)*a(4,3)))
   b(3,1) = detinv*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(4,1) = detinv*(a(2,1)*(a(3,3)*a(4,2)-a(3,2)*a(4,3))+a(2,2)*(a(3,1)*a(4,3)-a(3,3)*a(4,1))+a(2,3)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(1,2) = detinv*(a(1,2)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(1,3)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(1,4)*(a(3,3)*a(4,2)-a(3,2)*a(4,3)))
   b(2,2) = detinv*(a(1,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(1,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(1,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))
   b(3,2) = detinv*(a(1,1)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(1,2)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(1,4)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(4,2) = detinv*(a(1,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(1,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(1,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(1,3) = detinv*(a(1,2)*(a(2,3)*a(4,4)-a(2,4)*a(4,3))+a(1,3)*(a(2,4)*a(4,2)-a(2,2)*a(4,4))+a(1,4)*(a(2,2)*a(4,3)-a(2,3)*a(4,2)))
   b(2,3) = detinv*(a(1,1)*(a(2,4)*a(4,3)-a(2,3)*a(4,4))+a(1,3)*(a(2,1)*a(4,4)-a(2,4)*a(4,1))+a(1,4)*(a(2,3)*a(4,1)-a(2,1)*a(4,3)))
   b(3,3) = detinv*(a(1,1)*(a(2,2)*a(4,4)-a(2,4)*a(4,2))+a(1,2)*(a(2,4)*a(4,1)-a(2,1)*a(4,4))+a(1,4)*(a(2,1)*a(4,2)-a(2,2)*a(4,1)))
   b(4,3) = detinv*(a(1,1)*(a(2,3)*a(4,2)-a(2,2)*a(4,3))+a(1,2)*(a(2,1)*a(4,3)-a(2,3)*a(4,1))+a(1,3)*(a(2,2)*a(4,1)-a(2,1)*a(4,2)))
   b(1,4) = detinv*(a(1,2)*(a(2,4)*a(3,3)-a(2,3)*a(3,4))+a(1,3)*(a(2,2)*a(3,4)-a(2,4)*a(3,2))+a(1,4)*(a(2,3)*a(3,2)-a(2,2)*a(3,3)))
   b(2,4) = detinv*(a(1,1)*(a(2,3)*a(3,4)-a(2,4)*a(3,3))+a(1,3)*(a(2,4)*a(3,1)-a(2,1)*a(3,4))+a(1,4)*(a(2,1)*a(3,3)-a(2,3)*a(3,1)))
   b(3,4) = detinv*(a(1,1)*(a(2,4)*a(3,2)-a(2,2)*a(3,4))+a(1,2)*(a(2,1)*a(3,4)-a(2,4)*a(3,1))+a(1,4)*(a(2,2)*a(3,1)-a(2,1)*a(3,2)))
   b(4,4) = detinv*(a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1)))
end function real_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_2x2(a) result(b)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: a(2,2)   !! Matrix
   complex(kind=wp)             :: b(2,2)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2) - a(1,2)*a(2,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * a(2,2)
   b(2,1) = -detinv * a(2,1)
   b(1,2) = -detinv * a(1,2)
   b(2,2) = +detinv * a(1,1)
end function complex_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_3x3(a) result(b)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: a(3,3)   !! Matrix
   complex(kind=wp)             :: b(3,3)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(a(1,1)*a(2,2)*a(3,3) - a(1,1)*a(2,3)*a(3,2)&
             - a(1,2)*a(2,1)*a(3,3) + a(1,2)*a(2,3)*a(3,1)&
             + a(1,3)*a(2,1)*a(3,2) - a(1,3)*a(2,2)*a(3,1))

   ! Calculate the inverse of the matrix
   b(1,1) = +detinv * (a(2,2)*a(3,3) - a(2,3)*a(3,2))
   b(2,1) = -detinv * (a(2,1)*a(3,3) - a(2,3)*a(3,1))
   b(3,1) = +detinv * (a(2,1)*a(3,2) - a(2,2)*a(3,1))
   b(1,2) = -detinv * (a(1,2)*a(3,3) - a(1,3)*a(3,2))
   b(2,2) = +detinv * (a(1,1)*a(3,3) - a(1,3)*a(3,1))
   b(3,2) = -detinv * (a(1,1)*a(3,2) - a(1,2)*a(3,1))
   b(1,3) = +detinv * (a(1,2)*a(2,3) - a(1,3)*a(2,2))
   b(2,3) = -detinv * (a(1,1)*a(2,3) - a(1,3)*a(2,1))
   b(3,3) = +detinv * (a(1,1)*a(2,2) - a(1,2)*a(2,1))
end function complex_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_4x4(a) result(b)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: a(4,4)   !! Matrix
   complex(kind=wp)             :: b(4,4)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(a(1,1)*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))&
      - a(1,2)*(a(2,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))&
      + a(1,3)*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))&
      - a(1,4)*(a(2,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(2,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(2,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1))))

   ! Calculate the inverse of the matrix
   b(1,1) = detinv*(a(2,2)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(2,3)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(2,4)*(a(3,2)*a(4,3)-a(3,3)*a(4,2)))
   b(2,1) = detinv*(a(2,1)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(2,3)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(2,4)*(a(3,3)*a(4,1)-a(3,1)*a(4,3)))
   b(3,1) = detinv*(a(2,1)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(2,2)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(2,4)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(4,1) = detinv*(a(2,1)*(a(3,3)*a(4,2)-a(3,2)*a(4,3))+a(2,2)*(a(3,1)*a(4,3)-a(3,3)*a(4,1))+a(2,3)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(1,2) = detinv*(a(1,2)*(a(3,4)*a(4,3)-a(3,3)*a(4,4))+a(1,3)*(a(3,2)*a(4,4)-a(3,4)*a(4,2))+a(1,4)*(a(3,3)*a(4,2)-a(3,2)*a(4,3)))
   b(2,2) = detinv*(a(1,1)*(a(3,3)*a(4,4)-a(3,4)*a(4,3))+a(1,3)*(a(3,4)*a(4,1)-a(3,1)*a(4,4))+a(1,4)*(a(3,1)*a(4,3)-a(3,3)*a(4,1)))
   b(3,2) = detinv*(a(1,1)*(a(3,4)*a(4,2)-a(3,2)*a(4,4))+a(1,2)*(a(3,1)*a(4,4)-a(3,4)*a(4,1))+a(1,4)*(a(3,2)*a(4,1)-a(3,1)*a(4,2)))
   b(4,2) = detinv*(a(1,1)*(a(3,2)*a(4,3)-a(3,3)*a(4,2))+a(1,2)*(a(3,3)*a(4,1)-a(3,1)*a(4,3))+a(1,3)*(a(3,1)*a(4,2)-a(3,2)*a(4,1)))
   b(1,3) = detinv*(a(1,2)*(a(2,3)*a(4,4)-a(2,4)*a(4,3))+a(1,3)*(a(2,4)*a(4,2)-a(2,2)*a(4,4))+a(1,4)*(a(2,2)*a(4,3)-a(2,3)*a(4,2)))
   b(2,3) = detinv*(a(1,1)*(a(2,4)*a(4,3)-a(2,3)*a(4,4))+a(1,3)*(a(2,1)*a(4,4)-a(2,4)*a(4,1))+a(1,4)*(a(2,3)*a(4,1)-a(2,1)*a(4,3)))
   b(3,3) = detinv*(a(1,1)*(a(2,2)*a(4,4)-a(2,4)*a(4,2))+a(1,2)*(a(2,4)*a(4,1)-a(2,1)*a(4,4))+a(1,4)*(a(2,1)*a(4,2)-a(2,2)*a(4,1)))
   b(4,3) = detinv*(a(1,1)*(a(2,3)*a(4,2)-a(2,2)*a(4,3))+a(1,2)*(a(2,1)*a(4,3)-a(2,3)*a(4,1))+a(1,3)*(a(2,2)*a(4,1)-a(2,1)*a(4,2)))
   b(1,4) = detinv*(a(1,2)*(a(2,4)*a(3,3)-a(2,3)*a(3,4))+a(1,3)*(a(2,2)*a(3,4)-a(2,4)*a(3,2))+a(1,4)*(a(2,3)*a(3,2)-a(2,2)*a(3,3)))
   b(2,4) = detinv*(a(1,1)*(a(2,3)*a(3,4)-a(2,4)*a(3,3))+a(1,3)*(a(2,4)*a(3,1)-a(2,1)*a(3,4))+a(1,4)*(a(2,1)*a(3,3)-a(2,3)*a(3,1)))
   b(3,4) = detinv*(a(1,1)*(a(2,4)*a(3,2)-a(2,2)*a(3,4))+a(1,2)*(a(2,1)*a(3,4)-a(2,4)*a(3,1))+a(1,4)*(a(2,2)*a(3,1)-a(2,1)*a(3,2)))
   b(4,4) = detinv*(a(1,1)*(a(2,2)*a(3,3)-a(2,3)*a(3,2))+a(1,2)*(a(2,3)*a(3,1)-a(2,1)*a(3,3))+a(1,3)*(a(2,1)*a(3,2)-a(2,2)*a(3,1)))
end function complex_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     in(3f) - [M_math] test if a value occurs in an expected range
!!
!!##SYNOPSIS
!!
!!    elemental function in(bottom, middle, top)
!!
!!     ALLOWED_TYPE, intent(in) :: bottom, middle, top
!!     logical          :: in
!!
!!    where ALLOWED_TYPE may be REAL or INTEGER of default kind from the set
!!
!!      use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, &
!!      & real32, real64, real128, dp=>real64
!!
!!    All input values must be of the same KIND.
!!
!!##DESCRIPTION
!!    Test if a value is within the range BOTTOM to TOP inclusive.
!!    The user is responsible for ensuring BOTTOM is less than or equal to TOP.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_in
!!     use M_math, only : in
!!     implicit none
!!        write(*, *)in(1, 3, 10)
!!        write(*, *)in(1, -30, 10)
!!        write(*, *)in(1, 30, 10)
!!        write(*, *)in(-11.11, 5.5, 9.999)
!!     end program demo_in
!!
!!    Example output
!!
!!     T
!!     F
!!     F
!!     T
elemental function in_i8(bottom,middle,top)
integer(kind=int8),intent(in) :: bottom, middle, top
logical            :: in_i8
   in_i8=middle.ge.bottom.and.middle.le.top
end function in_i8
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function in_i16(bottom,middle,top)
integer(kind=int16),intent(in) :: bottom, middle, top
logical            :: in_i16
   in_i16=middle.ge.bottom.and.middle.le.top
end function in_i16
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function in_i32(bottom,middle,top)
integer(kind=int32),intent(in) :: bottom, middle, top
logical            :: in_i32
   in_i32=middle.ge.bottom.and.middle.le.top
end function in_i32
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function in_i64(bottom,middle,top)
integer(kind=int64),intent(in) :: bottom, middle, top
logical            :: in_i64
   in_i64=middle.ge.bottom.and.middle.le.top
end function in_i64
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function in_r32(bottom,middle,top)
real(kind=real32),intent(in) :: bottom, middle, top
logical         :: in_r32
   in_r32=middle.ge.bottom.and.middle.le.top
end function in_r32
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function in_r64(bottom,middle,top)
real(kind=real64),intent(in) :: bottom, middle, top
logical         :: in_r64
   in_r64=middle.ge.bottom.and.middle.le.top
end function in_r64
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function in_r128(bottom,middle,top)
real(kind=real128),intent(in) :: bottom, middle, top
logical         :: in_r128
   in_r128=middle.ge.bottom.and.middle.le.top
end function in_r128
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     bound(3f) - [M_math] constrain a value to a range
!!
!!##SYNOPSIS
!!
!!   elemental function bound(bottom, middle, top)
!!
!!    ALLOWED_TYPE, intent(in) :: bottom, middle, top
!!    ALLOWED_TYPE        :: bound
!!
!!   where ALLOWED_TYPE may be REAL or INTEGER of default kind
!!   from the set of kinds defined by
!!
!!     use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, &
!!     & real32, real64, real128, dp=>real64
!!
!!   All input values must be of the same KIND.
!!
!!##DESCRIPTION
!!    Constrain a value to the range from BOTTOM to TOP inclusive. The user
!!    is responsible for ensuring BOTTOM is less than or equal to TOP.
!!
!!    The results for input values of +-INF and NAN are the same as for the
!!    MAX(3f) and MIN(3f) intrinsic functions.
!!
!!##OPTIONS
!!    bottom  The minimum value to return.
!!    middle  The value to constrain to the range BOTTOM to TOP.
!!    top     The maximum value to return.
!!##RETURNS
!!    bound  The value of MIDDLE is returned except BOTTOM is returned when
!!           MIDDLE < BOTTOM and the value of TOP is returned when
!!           MIDDLE > TOP.
!!
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_bound
!!     use,intrinsic :: iso_fortran_env, only : &
!!      & int8, int16, int32, int64, &
!!      & real32, real64, real128, dp=>real64
!!     use M_math, only : bound
!!     implicit none
!!        write(*, *)bound(1, 3, 10)
!!        write(*, *)bound(1, -30, 10)
!!        write(*, *)bound(1, 30, 10)
!!        write(*, *)bound(1, [-11,0,6,11,22], 10)
!!        write(*, *)bound(-11.11, 5.5, 9.999)
!!     end program demo_bound
!!
!!   Results:
!!               3
!!               1
!!              10
!!               1           1           6          10          10
!!       5.50000000
elemental pure function bound_r32(bottom,middle,top)
real(kind=real32),intent(in) :: bottom, middle, top
real(kind=real32)            :: bound_r32
   bound_r32=min(max(bottom,middle),top)
end function bound_r32
!-----------------------------------------------------------------------------------------------------------------------------------
elemental pure function bound_r64(bottom,middle,top)
real(kind=real64),intent(in) :: bottom, middle, top
real(kind=real64)            :: bound_r64
   bound_r64=min(max(bottom,middle),top)
end function bound_r64
!-----------------------------------------------------------------------------------------------------------------------------------
elemental pure function bound_r128(bottom,middle,top)
real(kind=real128),intent(in) :: bottom, middle, top
real(kind=real128)            :: bound_r128
   bound_r128=min(max(bottom,middle),top)
end function bound_r128
!-----------------------------------------------------------------------------------------------------------------------------------
elemental pure function bound_i8(bottom,middle,top)
integer(kind=int8),intent(in) :: bottom, middle, top
integer(kind=int8)            :: bound_i8
   bound_i8=min(max(bottom,middle),top)
end function bound_i8
!-----------------------------------------------------------------------------------------------------------------------------------
elemental pure function bound_i16(bottom,middle,top)
integer(kind=int16),intent(in) :: bottom, middle, top
integer(kind=int16)            :: bound_i16
   bound_i16=min(max(bottom,middle),top)
end function bound_i16
!-----------------------------------------------------------------------------------------------------------------------------------
elemental pure function bound_i32(bottom,middle,top)
integer(kind=int32),intent(in) :: bottom, middle, top
integer(kind=int32)            :: bound_i32
   bound_i32=min(max(bottom,middle),top)
end function bound_i32
!-----------------------------------------------------------------------------------------------------------------------------------
elemental pure function bound_i64(bottom,middle,top)
integer(kind=int64),intent(in) :: bottom, middle, top
integer(kind=int64)            :: bound_i64
   bound_i64=min(max(bottom,middle),top)
end function bound_i64
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_math
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

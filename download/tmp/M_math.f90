module M_math
implicit none
private
  ! GEOMETRY
  public citer          ! determine various geometric properties of circle segment given radius and area of the segment.
  public envelope       ! Find the vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n.
  public inpolygon      ! Subroutine to determine whether or not a point is in a polygon
  public locpt          ! find if a point is inside a polygonal path
  public poly_intercept ! find points where a line intersects a polygon
  public polyarea       ! find area of a polygon
  ! FIT
  public julfit         ! linear least square fit
  public julfit1        ! linear least square fit(y=a*x+b)
  public lowess         ! data smoothing using locally weighted regression
  public splift         ! fits a spline to the n data points given in x and y
  public splint         ! interpolates and twice differentiates a cubic spline
  public linearint      ! linear interpolation
  ! STATISTICS
  public extremum       ! find the minimum and maximum value in a real array
  public bds            ! basic descriptive statistics
  public skekurx        ! skew and kurtosis variant
  public skekur1        ! skew and kurtosis variant
  public stddev         ! standard deviation
  ! COMPARING AND ROUNDING FLOATING POINT VALUES
  public accdig         ! compare two real numbers only up to a specified number of digits
  public almost         ! function compares two real numbers only up to a specified number of digits
  public dp_accdig      ! compare two double numbers only up to a specified number of digits
  public round          ! round val to specified number of significant digits
  public scale1         ! given xmin,xmax,n, find new range xminp xmaxp divisible into approximately n linear intervals of size dist
  public scale3         ! find nice log range, typically for an axis
  ! MATRIX
  public invert_2x2     ! directly invert 2x2 matrix
  public invert_3x3     ! directly invert 3x3 matrix
  public invert_4x4     ! directly invert 4x4 matrix

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
!===================================================================================================================================
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
!===================================================================================================================================
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
!===================================================================================================================================
interface invert_4x4
   module procedure real_invert_4x4, integer_invert_4x4, complex_invert_4x4, double_invert_4x4
end interface invert_4x4

contains

!>
!!##NAME
!!     julfit(3f) - [M_math:fit]linear least squares curve fits , destroys input arrays
!!
!!##SYNPOSIS
!!   subroutine julfit(x,y,ixn,itype,a,b,r2)
!!
!!    integer,intent(in) :: ixn
!!    real               :: x(ixn),y(ixn)
!!    integer,intent(in) :: itype
!!    real,intent(out)   :: a,b,r2
!!
!!##DESCRIPTION
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
!!   x      = array of x values, input
!!   y      = array of y values, input that are changed to hold the output
!!   ixn    = number of points in arrays x and y to use
!!   itype  = expression being solved
!!           (1) Y=a*X+b
!!           (2) Y=a*b**X
!!           (3) Y=a*log10(X)+b
!!           (4) Y=a*X**b
!!           (5) Y=a*e*(-b**X)
!!
!!
!! NOTE: odd use of arrays specifically optimized for calling from USH
!!
!!##RETURNS
!!
!!   a      = slope of linearized line
!!   b      = y intercept of linearized line
!!   r2     = correlation coefficient (1=perfect)
!!            In general, if the correlation coefficient is <0.5 the correlation
!!            is regarded as insignificant. If it is >0.8 the derived linear fit
!!            is considered highly significant.
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        julfit(3f)
!! DESCRIPTION:    linear least squares curve fits , destroys input arrays
!! AUTHOR:         John S. Urban
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine julfit(x,y,ixn,itype,a,b,r2)
use M_journal, only : journal
character(len=*),parameter :: ident="@(#)M_math::julfit(3f): linear least squares curve fits, destroys input arrays"
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
!!      julfit1(3f) - [M_math:fit]internal routine for linear least square fit(y=a*x+b), changes the y array
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
!!    While the method of least squares often gives optimal estimates parameters for linear processes,
!!    it is very sensitive to the presence of unusual data points in the data used to fit a model, as the square of the
!!    distance from the resulting fit is used in the calculation.
!!    That is, a few outliers can sometimes seriously skew the results of a least squares analysis;
!!    this makes model validation, especially with respect to outliers, critical to obtaining sound answers.
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
!!    program demo_julfit1
!!    implicit none
!!    intrinsic random_number
!!    integer :: points
!!    real    :: slope, intercept
!!    write(*,*)'For y=m*x+b enter M and B and number of points N:'
!!    read(*,*)slope,intercept,points
!!    call testit()
!!    contains
!!
!!    subroutine testit()
!!    real    :: x(points), y(points)
!!    real    :: slope_out, intercept_out, r2
!!    integer :: i, ii
!!    real    :: rndnum
!!       do i=1,points
!!          x(i)=i*0.10
!!          ! assigned pseudorandom numbers from the uniform distribution in the interval 0  x < 1.
!!          call random_number(rndnum)
!!          y(i)=slope*(x(i)+4.0*(rndnum-0.5))+intercept
!!       enddo
!!       !write(*,*)(ii,x(ii),y(ii),new_line('A'),ii=1,points)
!!       call julfit1(x,y,points,slope_out,intercept_out,r2)
!!       write(*,*)'SLOPE AND INTERCEPT IN  ',slope,intercept
!!       write(*,*)'SLOPE AND INTERCEPT OUT ',slope_out,intercept_out,r2
!!    end subroutine testit
!!
!!    end program demo_julfit1
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
!===================================================================================================================================
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
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine julfit1(x,y,ixn,a,b,r2)
implicit none
character(len=*),parameter :: ident="@(#)M_math::julfit1(3f): linear least square fit of (y=a*x+b), changes the y array"
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
!!     lowess(3f) - [M_math:fit]procedures for locally weighted regression
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
!!    window.  This size is given as the fraction (0 to 1) of the data that
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
!!       program T_lowess
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
!!       end
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
!===================================================================================================================================
!>
!! AUTHOR:     Bill Cleveland
!===================================================================================================================================
subroutine lowess(x, y, n, f, nsteps, delta, ys, rw, res)
use M_sort, only : sort_shell
character(len=*),parameter :: ident='@(#)M_math::lowess(3f): data smoothing using locally weighted regression'
integer n
integer nsteps
real x(n), y(n), f, delta, ys(n), rw(n)
real res(n)
integer nright, i, j, iter, last, m1, m2, ns, nleft
real cut, cmad, r, d1, d2
real c1, c9, alpha, denom
logical ok
      if (n .ge. 2) goto 1
         ys(1) = y(1)
         return
! at least two, at most n points
   1  ns = max0(min0(int(f*float(n)), n), 2)
      iter = 1
         goto  3
   2     iter = iter+1
   3     if (iter .gt. nsteps+1) goto  22
! robustness iterations
         nleft = 1
         nright = ns
! index of prev estimated point
         last = 0
! index of current point
         i = 1
   4        if (nright .ge. n) goto  5
! move nleft, nright to right if radius decreases
               d1 = x(i)-x(nleft)
! if d1<=d2 with x(nright+1)==x(nright), lowest fixes
               d2 = x(nright+1)-x(i)
               if (d1 .le. d2) goto  5
! radius will not decrease by move right
               nleft = nleft+1
               nright = nright+1
               goto  4
! fitted value at x(i)
   5        call lowest(x, y, n, x(i), ys(i), nleft, nright, res, iter .gt. 1, rw, ok)
            if (.not. ok) ys(i) = y(i)
! all weights zero - copy over value (all rw==0)
            if (last .ge. i-1) goto 9
               denom = x(i)-x(last)
! skipped points -- interpolate
! non-zero - proof?
               j = last+1
                  goto  7
   6              j = j+1
   7              if (j .ge. i) goto  8
                  alpha = (x(j)-x(last))/denom
                  ys(j) = alpha*ys(i)+(1.0-alpha)*ys(last)
                  goto  6
   8           continue
! last point actually estimated
   9        last = i
! x coord of close points
            cut = x(last)+delta
            i = last+1
               goto  11
  10           i = i+1
  11           if (i .gt. n) goto  13
! find close points
               if (x(i) .gt. cut) goto  13
! i one beyond last pt within cut
               if (x(i) .ne. x(last)) goto 12
                  ys(i) = ys(last)
! exact match in x
                  last = i
  12           continue
               goto  10
! back 1 point so interpolation within delta, but always go forward
  13        i = max0(last+1, i-1)
            if (last .lt. n) goto  4
! residuals
         do  15 i = 1, n
            res(i) = y(i)-ys(i)
  15        continue
         if (iter .gt. nsteps) goto  22
! compute robustness weights except last time
         do  16 i = 1, n
            rw(i) = abs(res(i))
  16        continue
         call sort_shell(rw, order='A')             ! sort in ascending order
         m1 = n/2+1
         m2 = n-m1+1
! 6 median abs resid
         cmad = 3.0*(rw(m1)+rw(m2))
         c9 = .999*cmad
         c1 = .001*cmad
         do  21 i = 1, n
            r = abs(res(i))
            if (r .gt. c1) goto 17
               rw(i) = 1.
! near 0, avoid underflow
               goto  20
  17           if (r .le. c9) goto 18
                  rw(i) = 0.
! near 1, avoid underflow
                  goto  19
  18              rw(i) = (1.0-(r/cmad)**2)**2
  19        continue
  20        continue
  21        continue
         goto  2
  22  return
end subroutine lowess
subroutine lowest(x, y, n, xs, ys, nleft, nright, w, userw, rw, ok)
integer n
integer nleft, nright
real x(n), y(n), xs, ys, w(n), rw(n)
logical userw, ok
integer nrt, j
real a, b, c, h, r
real h1, sqrt, h9, amax1, range
   range = x(n)-x(1)
   h = amax1(xs-x(nleft), x(nright)-xs)
   h9 = .999*h
   h1 = .001*h
! sum of weights
   a = 0.0
   j = nleft
         goto  2
   1     j = j+1
   2     if (j .gt. n) goto  7
! compute weights (pick up all ties on right)
         w(j) = 0.
         r = abs(x(j)-xs)
         if (r .gt. h9) goto 5
            if (r .le. h1) goto 3
               w(j) = (1.0-(r/h)**3)**3
! small enough for non-zero weight
               goto  4
   3           w(j) = 1.
   4        if (userw) w(j) = rw(j)*w(j)
            a = a+w(j)
            goto  6
   5        if (x(j) .gt. xs) goto  7
! get out at first zero wt on right
   6     continue
         goto  1
! rightmost pt (may be greater than nright because of ties)
   7  nrt = j-1
      if (a .gt. 0.0) goto 8
         ok = .false.
         goto  16
   8     ok = .true.
! weighted least squares
         do  9 j = nleft, nrt
! make sum of w(j) == 1
            w(j) = w(j)/a
   9        continue
         if (h .le. 0.) goto 14
            a = 0.0
! use linear fit
            do  10 j = nleft, nrt
! weighted center of x values
               a = a+w(j)*x(j)
  10           continue
            b = xs-a
            c = 0.0
            do  11 j = nleft, nrt
               c = c+w(j)*(x(j)-a)**2
  11           continue
            if (sqrt(c) .le. .001*range) goto 13
               b = b/c
! points are spread out enough to compute slope
               do  12 j = nleft, nrt
                  w(j) = w(j)*(b*(x(j)-a)+1.0)
  12              continue
  13        continue
  14     ys = 0.0
         do  15 j = nleft, nrt
            ys = ys+w(j)*y(j)
  15        continue
  16  return
end subroutine lowest
!>
!!##NAME
!!    splift(3f) - [M_math:fit]fits a spline to the n data points given in x and y
!!                 and also returns first and second derivitives
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
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        splift(3f)
!! DESCRIPTION:    fits a spline to the n data points given in x and y
!!##VERSION:        5.0: 20170129
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!===================================================================================================================================
!===================================================================================================================================
SUBROUTINE SPLIFT(X,Y,YP,YPP,N,ierr,a1,b1,an,bn)
!-----------------------------------------------------------------------------------------------------------------------------------
   use M_journal, only : journal
   character(len=*),parameter :: ident='@(#)M_math::splift(3f): fits a spline to the n data points given in x and y'
   integer,intent(in)         :: N
   real,intent(in)            :: X(N),Y(N)
   real,intent(out)           :: YP(N),YPP(N)
   integer,intent(out)        :: ierr           ! error status.
   real,intent(in)            :: a1
   real,intent(in)            :: b1
   real,intent(in)            :: an
   real,intent(in)            :: bn

   character(len=255)         :: ctemp
   real                       :: w(n,3)         ! w is a work array that must be able to hold at least N*3 numbers
   integer                    :: i, j
   integer                    :: NM1
   integer                    :: NM2
   real                       :: DOLD
   real                       :: DNEW
!-----------------------------------------------------------------------------------------------------------------------------------
   if (n.lt.4) then
      ierr=1
      call journal('*splift* number of abscissas too small (.lt.4)')
      return
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   DO I=2,N     ! make sure x(:) values are increasing monotonically
      IF ( (X(I)-X(I-1)) .gt. 0 ) cycle
      IERR=2
      call journal('sc','*splift* abscissa not strictly increasing, index=',i)
      write(ctemp,"('*splift* x,y=',g20.13,1x,g20.13)")x(i),y(i)
      call journal(ctemp)
      return
   enddo
!-----------------------------------------------------------------------------------------------------------------------------------
      NM1  = N-1                                                          ! SPLITF.62
      NM2  = N-2                                                          ! SPLITF.63
!-----------------------------------------------------------------------------------------------------------------------------------
!     DEFINE THE TRIDIAGONAL MATRIX                                       ! SPLITF.68
!                                                                         ! SPLITF.69
      W(1,3) = X(2)-X(1)                                                  ! SPLITF.70
      DO I=2,NM1                                                          ! SPLITF.71
         W(I,2) = W(I-1,3)                                                ! SPLITF.72
         W(I,3) = X(I+1)-X(I)                                             ! SPLITF.73
         W(I,1) = 2.0*(W(I,2)+W(I,3))                                     ! SPLITF.74
      enddo
      W(1,1) = 4.0                                                        ! SPLITF.75
      W(1,3) =-4.0*A1                                                     ! SPLITF.76
      W(N,1) = 4.0                                                        ! SPLITF.77
      W(N,2) =-4.0*AN                                                     ! SPLITF.78
!                                                                         ! SPLITF.79
!     L U DECOMPOSITION                                                   ! SPLITF.80
!                                                                         ! SPLITF.81
      DO I=2,N                                                            ! SPLITF.82
         W(I-1,3) = W(I-1,3)/W(I-1,1)                                     ! SPLITF.83
         W(I,1)   = W(I,1) - W(I,2)*W(I-1,3)                              ! SPLITF.84
      enddo
!                                                                         ! SPLITF.85
!     DEFINE *CONSTANT* VECTOR                                            ! SPLITF.86
!                                                                         ! SPLITF.87
      YPP(1) = 4.0*B1                                                     ! SPLITF.88
      DOLD   = (Y(2)-Y(1))/W(2,2)                                         ! SPLITF.89
      DO I=2,NM2                                                          ! SPLITF.90
         DNEW   = (Y(I+1) - Y(I))/W(I+1,2)                                ! SPLITF.91
         YPP(I) = 6.0*(DNEW - DOLD)                                       ! SPLITF.92
         YP(I)  = DOLD                                                    ! SPLITF.93
         DOLD   = DNEW                                                    ! SPLITF.94
      enddo
      DNEW   = (Y(N)-Y(N-1))/(X(N)-X(N-1))                                ! SPLITF.95
      YPP(NM1) = 6.0*(DNEW - DOLD)                                        ! SPLITF.96
      YPP(N) = 4.0*BN                                                     ! SPLITF.97
      YP(NM1)= DOLD                                                       ! SPLITF.98
      YP(N)  = DNEW                                                       ! SPLITF.99
!                                                                         ! SPLITF.100
!     FORWARD SUBSTITUTION                                                ! SPLITF.101
!                                                                         ! SPLITF.102
      YPP(1) = YPP(1)/W(1,1)                                              ! SPLITF.103
      DO I=2,N                                                            ! SPLITF.104
         YPP(I) = (YPP(I) - W(I,2)*YPP(I-1))/W(I,1)                       ! SPLITF.105
      enddo
!                                                                         ! SPLITF.106
!     BACKWARD SUBSTITUTION                                               ! SPLITF.107
!                                                                         ! SPLITF.108
      DO J=1,NM1                                                          ! SPLITF.109
         I = N-J                                                          ! SPLITF.110
         YPP(I) = YPP(I) - W(I,3)*YPP(I+1)                                ! SPLITF.111
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
!     COMPUTE FIRST DERIVATIVES                                           ! SPLITF.113
!                                                                         ! SPLITF.114
      YP(1)  = (Y(2)-Y(1))/(X(2)-X(1)) - (X(2)-X(1))*(2.0*YPP(1) + YPP(2))/6.0
      DO I=2,NM1
         YP(I)  = YP(I) + W(I,2)*(YPP(I-1) + 2.0*YPP(I))/6.0              ! SPLITF.118
      enddo
      YP(N)  = YP(N) + (X(N)-X(NM1))*(YPP(NM1) + 2.0*YPP(N))/6.0          ! SPLITF.119
!-----------------------------------------------------------------------------------------------------------------------------------
   ierr=0
end subroutine splift
!===================================================================================================================================
!>
!!##NAME
!!    splint(3f) - [M_math:fit]interpolates and twice differentiates a cubic spline
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
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        splint(3f)
!! DESCRIPTION:    interpolates and twice differentiates a cubic spline
!!##VERSION:        5.0: 20170129
!! AUTHOR:         John S. Urban
!! REPORTING BUGS: http://www.urbanjost.altervista.org/
!===================================================================================================================================
subroutine splint (x,y,ypp,n,xi,yi,ypi,yppi,ni,kerr)
character(len=*),parameter :: ident='@(#)M_math::splint(3f): interpolates and twice differentiates a cubic spline'
integer,intent(in)  :: n
integer,intent(in)  :: ni
real,intent(in)     :: x(n),y(n),ypp(n),xi(ni)
real,intent(out)    :: yi(ni),ypi(ni),yppi(ni)
integer,intent(out) :: kerr

integer            :: nm1, k, il, ir, i
real               :: h, h2, xx
real               :: xr, xr2, xr3, xl, xl2, xl3

!  CHECK INPUT
   if (ni .le. 0)then
      kerr=2
      return
   endif
   kerr=0
   nm1= n-1
!  K IS INDEX ON VALUE OF XI BEING WORKED ON.  XX IS THAT VALUE.
!  I IS CURRENT INDEX INTO X ARRAY.
   k  = 1
   xx = xi(1)
   if (xx.lt.x(1)) goto 90
   if (xx.gt.x(n)) goto 80
   il = 1
   ir = n
!------------------------------------
!  BISECTION SEARCH
   10 continue
      i  = (il+ir)/2
      if (i.eq.il) goto 100
      if (xx-x(i)) 20,100,30
   20 continue
      ir = i
      goto 10
   30 continue
      il = i
      goto 10
!------------------------------------
!     LINEAR FORWARD SEARCH
   50 continue
      if (xx-x(i+1)) 100,100,60
   60 continue
      if (i.ge.nm1) goto 80
      i  = i+1
      goto 50
!------------------------------------
!     EXTRAPOLATION
   80 continue
      kerr=1
      i  = nm1
      goto 100
   90 continue
      kerr=1
      i  = 1
!------------------------------------
!     INTERPOLATION
  100 continue
      h  = x(i+1) - x(i)
      h2 = h*h
      xr = (x(i+1)-xx)/h
      xr2= xr*xr
      xr3= xr*xr2
      xl = (xx-x(i))/h
      xl2= xl*xl
      xl3= xl*xl2
      yi(k) = y(i)*xr + y(i+1)*xl -h2*(ypp(i)*(xr-xr3) + ypp(i+1)*(xl-xl3))/6.0
      ypi(k) = (y(i+1)-y(i))/h +h*(ypp(i)*(1.0-3.0*xr2) - ypp(i+1)*(1.0-3.0*xl2))/6.0
      yppi(k) = ypp(i)*xr + ypp(i+1)*xl
!------------------------------------
!     NEXT POINT
      if (k.ge.ni) goto 120
      k = k+1
      xx = xi(k)
      if (xx.lt.x(1)) goto 90
      if (xx.gt.x(n)) goto 80
      if (xx-xi(k-1)) 110,100,50
  110 continue
      il = 1
      ir = i+1
  120 continue
END SUBROUTINE SPLINT
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
!===================================================================================================================================
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        linearint(1)
!! DESCRIPTION:    interpolates a curve <X(i),Y(i)> using linear interpolation at given XI(j) values
!!##VERSION:        1.0, 20031123
!! AUTHOR:         John S. Urban (hacked from splint)
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
!===================================================================================================================================
SUBROUTINE linearint(X,Y,N,XI,YI,NI,KERR)
implicit none
character(len=*),parameter ::lident="@(#)M_math::linearint(3f):linear interpolation of curve X(i),Y(i) at given XI(j) values"
!
   INTEGER,intent(in)  :: N, NI
   REAL,intent(in)     :: X(N),Y(N),XI(NI)
   REAL,intent(out)    :: YI(NI)
   INTEGER,intent(out) :: KERR
!=======================================================================
   integer             :: k, i, il, ir, nm1
   real                :: xx, delta, h, v, h2, delta2
!=======================================================================
!     CHECK INPUT
      if(ni.le.0)then
         kerr=2
         return
      endif
      kerr=0
!=======================================================================
!     K IS INDEX ON VALUE OF XI BEING WORKED ON.  XX IS THAT VALUE.
!     I IS CURRENT INDEX INTO X ARRAY.
      K  = 1
      XX = XI(1)
      IF (XX.LT.X(1)) GOTO 90 ! extrapolation
      IF (XX.GT.X(N)) GOTO 80 ! extrapolation
      IL = 1
      IR = N
      NM1= N-1
!=======================================================================
!     BISECTION SEARCH
   10 continue
      I  = (IL+IR)/2
      IF (I.EQ.IL) GOTO 100
      DELTA=XX-X(I)
      IF (DELTA.lt.0)then
        IR=I
        GOTO 10
      elseif(DELTA.gt.0)then
        IL = I
        GOTO 10
      else
        goto 100
      endif
!=======================================================================
!     LINEAR FORWARD SEARCH
   50 CONTINUE
      IF (XX-X(I+1).le.0)goto 100  ! interpolation
      IF (I.GE.NM1) GOTO 80        ! extrapolation
      I  = I+1                     ! go forward again
      GOTO 50
!=======================================================================
!     EXTRAPOLATION
   80 CONTINUE
      KERR=1
      I  = NM1
      GOTO 100
!=======================================================================
   90 KERR=1
      I  = 1
!=======================================================================
!     INTERPOLATION
  100 continue
      H  = X(I+1) - X(I)
      V  = Y(I+1) - Y(I)
      H2 = XX-X(I)
      YI(K) = Y(I) + V*(H2/H)
!     NEXT POINT
      IF (K.GE.NI) RETURN
      K = K+1
      XX = XI(K)
      IF (XX.LT.X(1)) GOTO 90
      IF (XX.GT.X(N)) GOTO 80
      DELTA2=XX-XI(K-1)
      IF (DELTA2.lt.0)then
         IL = 1
         IR = I+1
         GOTO 10
      elseif(delta2.eq.0)then
         goto 100         ! interpolate
      else
        goto 50           ! linear forward search
      endif
!=======================================================================
END SUBROUTINE linearint

!>
!!##NAME
!!    citer(3f) - [M_math:geometry]determine various geometric properties of circle segment
!!            given radius and area of the segment.
!!##SYNOPIS
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
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE CITER(A,R,H,S,C,DADH)
use M_journal, only : journal
character(len=*),parameter :: &
ident="@(#)M_math::citer(3f): determine various geometric properties of circle segment given radius and area of the segment."
!
! COPYRIGHT (C) John S. Urban, 08/31/1995
!
!     Input and Output Variable Declarations
!
      DOUBLE PRECISION A,R,H,S,C,DADH
!
!     Internal Variable Declarations
!
      DOUBLEPRECISION AORS
      DOUBLEPRECISION THETAHI,THETALO,THETA
      DOUBLEPRECISION FLO,FHI,F
      DOUBLEPRECISION PI,TOL,X
      DOUBLEPRECISION FUNCT
      INTEGER ICOUNT
      DATA PI/3.14159265358979D0/,TOL/1.D-10/
!
!-----------------------------------------------------------------------
!
!     Statement Function Definition
!
!     The FUNCT statement function returns the scaled area of a segment
!     given the angle defining the segment.  The scaled area equals
!     the segment's area divided by the radius squared; it equals pi
!     when the input angle is 2*pi.
!
      FUNCT(X)=(X-SIN(X))/2.D0
!
!-----------------------------------------------------------------------
!
!     AORS = A Over R Squared
!          = Scaled area of segment
!          = pi when it represents the entire circle.
!
      AORS=A/R**2
!
!     The following IF-THEN-ELSEIF block insures the input area is
!     reasonable.
!
      IF(AORS.LT.TOL) THEN
!
!       The input area is so low that the segment is virtually non-
!       existant.
!
        THETA=0.d0
        H=0.D0
        S=0.D0
        C=0.D0
        DADH=0.D0
        RETURN
      ELSEIF(AORS.GE.PI) THEN
!
!       The input area is so high that the segment is virtually the
!       entire circle
!
        THETA=2.D0*PI
        H=2.D0*R
        S=2.D0*PI*R
        C=0.D0
        DADH=0.D0
        RETURN
      ENDIF
!
!     The following DO-WHILE loop solves for the value of THETA which
!     corresponds to the input segment area.  The loop employs an
!     interval reduction scheme.  THETALO is the lower bound of the
!     answer, and THETAHI is the upper bound of the answer.  FLO is the
!     scaled segment area corresponding to THETALO, and FHI is the
!     scaled segment area corresponding to THETAHI.
!
      THETALO=0.D0
      FLO=0.D0
      THETAHI=2.D0*PI
      FHI=PI
!
!     The very first estimate of THETA has to be done outside of the
!     loop.  The secant method is used to make the first estimate of
!     THETA.
!
      THETA=THETALO+(THETAHI-THETALO)*(AORS-FLO)/(FHI-FLO)
      F=FUNCT(THETA)
      ICOUNT=1
10    continue
      if (abs(f-aors).gt.tol)then
        ICOUNT=ICOUNT+2
        IF(ICOUNT.GT.100) THEN
!
!         The iteration has not converged in 100 steps.  Write a
!         message to the user and abort the run.
!
          CALL journal('*citer* did not convergence in 100 iterations')
          CALL journal('run aborted')
          CALL journal(' ')
          stop 1
        ENDIF
!
!       Replace one of the bounds on THETA with the latest guess.
!
        IF(F.GT.AORS) THEN
          THETAHI=THETA
          FHI=F
        ELSE
          THETALO=THETA
          FLO=F
        ENDIF
!
!       Use the bisection method for the next guess of THETA.
!
        THETA=(THETALO+THETAHI)/2.0D0
        F=FUNCT(THETA)
!
!       Replace one of the bounds on THETA with the latest guess.
!
        IF(F.GT.AORS) THEN
          THETAHI=THETA
          FHI=F
        ELSE
          THETALO=THETA
          FLO=F
        ENDIF
!
!       Use the secant method for the next guess of THETA.
!
        THETA=THETALO+(THETAHI-THETALO)*(AORS-FLO)/(FHI-FLO)
        F=FUNCT(THETA)
        goto 10
      endif
!
!     The iteration on THETA has converged.
!
      H=R*(1.0D0-COS(THETA/2.0D0))
      S=R*THETA
      C=2.D0*R*SIN(THETA/2.D0)
      DADH=2.0D0*SQRT(2.0D0*R*H-H**2)
!
END SUBROUTINE CITER
!>
!!##NAME
!!   envelope(3f) - [M_math:geometry]Find the vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n.
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
!===================================================================================================================================
!>
!! Programmer: Alan Miller
!! VERSION:    Latest revision - 12 September 1987
!! VERSION:    Fortran 90 version - 8 August 1996
!===================================================================================================================================
SUBROUTINE envelope(x, y, n, vertex, nvert) !-- saved from url=(0048)http://users.bigpond.net.au/amiller/envelope.f90
IMPLICIT NONE
character(len=*),parameter :: &
ident="@(#)M_math::envelope(3f):Find the vertices (in clockwise order) of a polygon enclosing the points (x(i), y(i), i=1, ..., n."
INTEGER,INTENT(IN) :: n
REAL,INTENT(IN)    :: x(n), y(n)
INTEGER :: vertex(n), nvert
INTEGER :: iwk(n)                  ! iwk() is an integer work array which must have dimension at least n

!  On output, vertex(i), i=1, ..., nvert contains the numbers of the vertices.


!       Local variables

INTEGER :: next(n), i, i1, i2, j, jp1, jp2, i2save, i3, i2next
REAL    :: xmax, xmin, ymax, ymin, dist, dmax, dmin, x1, y1, dx, dy, x2, y2, &
         &  dx1, dx2, dmax1, dmax2, dy1, dy2, temp, zero = 0.0

IF (n < 2) RETURN

!  Choose the points with smallest & largest x- values as the
!  first two vertices of the polygon.

IF (x(1) > x(n)) THEN
  vertex(1) = n
  vertex(2) = 1
  xmin = x(n)
  xmax = x(1)
ELSE
  vertex(1) = 1
  vertex(2) = n
  xmin = x(1)
  xmax = x(n)
END IF

DO i = 2, n-1
  temp = x(i)
  IF (temp < xmin) THEN
    vertex(1) = i
    xmin = temp
  ELSE IF (temp > xmax) THEN
    vertex(2) = i
    xmax = temp
  END IF
END DO

!       Special case, xmax = xmin.

IF (xmax == xmin) THEN
  IF (y(1) > y(n)) THEN
    vertex(1) = n
    vertex(2) = 1
    ymin = y(n)
    ymax = y(1)
  ELSE
    vertex(1) = 1
    vertex(2) = n
    ymin = y(1)
    ymax = y(n)
  END IF

  DO i = 2, n-1
    temp = y(i)
    IF (temp < ymin) THEN
      vertex(1) = i
      ymin = temp
    ELSE IF (temp > ymax) THEN
      vertex(2) = i
      ymax = temp
    END IF
  END DO

  nvert = 2
  IF (ymax == ymin) nvert = 1
  RETURN
END IF

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

DO i = 1, n
  IF (i == vertex(1) .OR. i == vertex(2)) CYCLE
  dist = (y(i) - y1)*dx - (x(i) - xmin)*dy
  IF (dist > zero) THEN
    iwk(i1) = i
    i1 = i
    IF (dist > dmax) THEN
      next(1) = i
      dmax = dist
    END IF
  ELSE IF (dist < zero) THEN
    iwk(i2) = i
    i2 = i
    IF (dist < dmin) THEN
      next(2) = i
      dmin = dist
    END IF
  END IF
END DO

!  Ends of lists are indicated by pointers to -ve positions.

iwk(i1) = -1
iwk(i2) = -1
nvert = 2

j = 1

!  Start of main process.

!  Introduce new vertex between vertices j & j+1, if one has been found.
!  Otherwise increase j.   Exit if no more vertices.

40 IF (next(j) < 0) THEN
IF (j == nvert) RETURN
j = j + 1
GO TO 40
END IF

jp1 = j + 1
DO i = nvert, jp1, -1
  vertex(i+1) = vertex(i)
  next(i+1) = next(i)
END DO
jp2 = jp1 + 1
nvert = nvert + 1
IF (jp2 > nvert) jp2 = 1
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
DMAX1 = zero
dmax2 = zero
next(j) = -1
next(jp1) = -1
i2save = i2
i2next = iwk(i2)
i = iwk(i1)
iwk(i1) = -1
iwk(i2) = -1

60 IF (i /= i2save) THEN
  dist = (y(i) - y1)*dx1 - (x(i) - x1)*dy1
  IF (dist > zero) THEN
    iwk(i1) = i
    i1 = i
    IF (dist > DMAX1) THEN
      next(j) = i
      DMAX1 = dist
    END IF
  ELSE
    dist = (y(i) - y2)*dx2 - (x(i) - x2)*dy2
    IF (dist > zero) THEN
      iwk(i2) = i
      i2 = i
      IF (dist > dmax2) THEN
        next(jp1) = i
        dmax2 = dist
      END IF
    END IF
  END IF
  i = iwk(i)
ELSE
  i = i2next
END IF

!  Get next point from old list at vertex j.

IF (i > 0) GO TO 60

!  End lists with -ve values.

iwk(i1) = -1
iwk(i2) = -1

GO TO 40
END SUBROUTINE envelope
!>
!!##NAME
!!    inpolygon(3f) - [M_math:geometry]determine whether or not an integer point is in an integer polygon
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
!!      Given a closed polygon find if a point lies inside the
!!      polygon.
!!
!!##OPTIONS
!!    xin     the X coordinate of the point to be checked
!!    yin     the Y coordinate of the point to be checked
!!    xconv   contains the X coords of the polygon
!!    yconv   contains the Y coords of the polygon
!!    nconv   the number of points in the polygon
!!
!!##RESULT
!!    INPOLYGON returns .true if the point
!!    lies inside the polygon, otherwise it returns .false.
!===================================================================================================================================
LOGICAL FUNCTION INPOLYGON(XIN, YIN, XCONV, YCONV, NCONV)
character(len=*),parameter :: &
ident="@(#)M_math::inpolygon(3f):Subroutine to determine whether or not an integer point is in a polygon of integer points"
   integer,intent(in)  :: xin,yin                       ! coordinates of the point to be checked
   integer,intent(in)  :: nconv                         !
   INTEGER             :: XCONV(NCONV), YCONV(NCONV)
   REAL                :: X,Y                           ! real copy of input point
   integer             :: Z(4)= [-32701,-32701,32701,32701]
   integer             :: i, j, m

      X=XIN
      Y=YIN
      IF (Z(1).eq.-32701) then
         DO I = 1,NCONV
            Z(1)=MAX(Z(1),XCONV(I))
            Z(2)=MAX(Z(2),YCONV(I))
            Z(3)=MIN(Z(3),XCONV(I))
            Z(4)=MIN(Z(4),YCONV(I))
         enddo
      endif
      INPOLYGON=.TRUE.
      IF(X .LT. Z(3) .OR. X .GT. Z(1)) INPOLYGON=.FALSE.
      IF(Y .LT. Z(4) .OR. Y .GT. Z(2)) INPOLYGON=.FALSE.
      IF(.NOT. INPOLYGON) RETURN

      J=0
      DO 90 I = 2,NCONV
         M=0
   !-----------------------------------------------------
         select case ((YCONV(I-1)-YIN)*(YIN-YCONV(I)))
         case(:-1); CYCLE
         case(0)  ;
         case(1:) ;
         end select
   !-----------------------------------------------------
         select case (YCONV(I-1)-YCONV(I))
         case(:-1); M=M-1; GOTO 70
         case(0)  ;
         case(1:) ; M=M-2 ;M=M-1;GOTO 70
         end select
   !-----------------------------------------------------
         IF ((XCONV(I-1)-X)*(X-XCONV(I))) 90,100,100
   !-----------------------------------------------------
   70 continue
         M=M+2
         IF ((Y-YCONV(I-1))*(FLOAT(XCONV(I))-XCONV(I-1))/(YCONV(I)-FLOAT(YCONV(I-1)))+XCONV(I-1)-X) 90,100,80
   80 continue
         J=J+M
   90 continue

      INPOLYGON=.FALSE.
      IF(J/4*4 .NE. J) INPOLYGON=.TRUE.
  100 CONTINUE
      END FUNCTION INPOLYGON
!>
!!##NAME
!!   locpt - [M_math:geometry]find if a point is inside a polygonal path
!!##SYNOPIS
!!   Usage:
!!
!!    subroutine locpt (x0,y0,x,y,n,l,m)
!!    real, intent(in)     :: x0, y0, x(:), y(:)
!!    integer, intent(in)  :: n
!!    integer, intent(out) :: l, m
!!
!!##DESCRIPTION
!!   Given a polygonal line connecting the vertices (X(I),Y(I)) (I = 1,...,N)
!!   taken in this order. it is assumed that the polygonal path is a loop,
!!   where (X(N),Y(N)) = (X(1),Y(1)) or there is an arc from (X(N),Y(N)) to
!!   (X(1),Y(1)). N.B. The polygon may cross itself any number of times.
!!
!!   (X0,Y0) is an arbitrary point and l and m are variables.
!!   On output, L and M are assigned the following values ...
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
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE locpt (x0, y0, x, y, n, l, m)
IMPLICIT NONE
character(len=*),parameter :: ident="@(#)M_math::locpt(3f): find if a point is inside a polygonal path"
!-----------------------------------------------------------------------------------------------------------------------------------
   REAL, INTENT(IN)     :: x0, y0, x(:), y(:)
   INTEGER, INTENT(IN)  :: n
   INTEGER, INTENT(OUT) :: l, m

   !     Local variables
   INTEGER :: i, n0
   REAL    :: angle, eps, pi, pi2, sum, theta, theta1, thetai, tol, u, v
!-----------------------------------------------------------------------------------------------------------------------------------
   eps = EPSILON(1.0)   ! EPS is a machine dependent constant. EPS is the smallest number such that 1.0 + EPS > 1.0
   n0 = n
   IF (x(1) == x(n) .AND. y(1) == y(n))then
      n0 = n - 1
   endif
   pi = ATAN2(0.0, -1.0)
   pi2 = 2.0*pi
   tol = 4.0*eps*pi
   l = -1
   m = 0

   u = x(1) - x0
   v = y(1) - y0
   IF (u == 0.0 .AND. v == 0.0)then
      GOTO 20
   endif
   IF (n0 < 2)then
      RETURN
   endif
   theta1 = ATAN2(v, u)

   sum = 0.0
   theta = theta1
   DO i = 2, n0
      u = x(i) - x0
      v = y(i) - y0
      IF (u == 0.0 .AND. v == 0.0) then
         GOTO 20
      endif
      thetai = ATAN2(v, u)

      angle = ABS(thetai - theta)
      IF (ABS(angle - pi) < tol)then
         GOTO 20
      endif
      IF (angle > pi)then
         angle = angle - pi2
      endif
      IF (theta > thetai)then
         angle = -angle
      endif
      sum = sum + angle
      theta = thetai
   ENDDO
   angle = ABS(theta1 - theta)
   IF (ABS(angle - pi) < tol)then
      GOTO 20
   endif
   IF (angle > pi)then
      angle = angle - pi2
   endif
   IF (theta > theta1)then
      angle = -angle
   endif
   sum = sum + angle            ! SUM = 2*PI*M WHERE M IS THE WINDING NUMBER
   m = int(ABS(sum)/pi2 + 0.2)
   IF (m == 0) then
      RETURN
   endif
   l = 1
   IF (sum < 0.0)then
      m = -m
   endif
   RETURN

20 continue                     ! (X0, Y0) IS ON THE BOUNDARY OF THE PATH
   l = 0
END SUBROUTINE locpt
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      poly_intercept(3f) - [M_math:geometry]intersection of a straight line and polygonal path
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
!===================================================================================================================================
!>
!! PROCEDURE:    poly_intercept(3f)
!! DESCRIPTION:  intesections of a straight line and polygonal path
!! AUTHOR:       Code converted using TO_F90 by Alan Miller
!! VERSION:      Date: 2000-07-04  Time: 12:24:01
!===================================================================================================================================
SUBROUTINE Poly_Intercept (a, b, x, y, n, u, v, m, num, ierr)
IMPLICIT NONE
character(len=*),parameter::ident="@(#)M_math::poly_intercept(3f): Calculates the points at which a line <A,B> crosses a polygon"

REAL, INTENT(IN)      :: a(2)
REAL, INTENT(IN)      :: b(2)
REAL, INTENT(IN)      :: x(:)
REAL, INTENT(IN)      :: y(:)
INTEGER, INTENT(IN)   :: n
REAL, INTENT(OUT)     :: u(:)
REAL, INTENT(OUT)     :: v(:)
INTEGER, INTENT(IN)   :: m
INTEGER, INTENT(OUT)  :: num
INTEGER, INTENT(OUT)  :: ierr

! Local variables

INTEGER  :: i, ind, nm1
REAL     :: d, diff, diff1, eps, h, hi, k, ki, onem, onep, p, q, s, t, tmax, tmin, tol, tol0

eps = EPSILON(1.0)  ! EPS IS A MACHINE DEPENDENT CONSTANT. EPS IS THE SMALLEST NUMBER SUCH THAT 1.0 + EPS .GT. 1.0 .
num = 0

IF (n < 2) GO TO 200
   h = b(1) - a(1)
   k = b(2) - a(2)

IF (h == 0.0 .AND. k == 0.0) GO TO 200

   ierr = 0
   nm1 = n - 1
   tol = 4.0*eps
   tol0 = 2.0*eps
   onep = 1.0 + tol
   onem = 0.5 + (0.5 - tol0)

ind = 0

DO i = 1, nm1
   hi = x(i + 1) - x(i)
   ki = y(i + 1) - y(i)
   IF (hi == 0.0 .AND. ki == 0.0) CYCLE
   ind = 1

! Check if the line from a to b and the i-th line in the path are parallel

  s = hi*k
  t = h*ki
  d = s - t

  IF (ABS(d) <= tol*MAX(ABS(s), ABS(t))) GO TO 40
!-----------------------------------------------------------------------
!                   THE LINES ARE NOT PARALLEL
!-----------------------------------------------------------------------
  p = x(i) - a(1)
  q = y(i) - a(2)
  s = hi*q
  t = ki*p
  diff = s - t
  IF (ABS(diff) <= tol*MAX(ABS(s),ABS(t))) diff = 0.0
  s = h*q
  t = k*p
  diff1 = s - t
  IF (ABS(diff1) <= tol*MAX(ABS(s),ABS(t))) diff1 = 0.0

  s = diff/d
  t = diff1/d

  IF (s < 0.0 .OR. s > onep) CYCLE
  IF (t < 0.0 .OR. t > onep) CYCLE
  IF (num > 0 .AND. t == 0.0) CYCLE
  IF (s > 0.0) GO TO 20

!                   POINT A IS ON THE I-TH LINE

10 continue
  num = num + 1

  IF (num > m) GO TO 210
     u(num) = a(1)
     v(num) = a(2)
     CYCLE

!                   POINT B IS ON THE I-TH LINE

20 continue
   IF (s < onem) GO TO 30
21 continue
   num = num + 1
   IF (num > m) GO TO 210
      u(num) = b(1)
      v(num) = b(2)
   CYCLE

!              THE INTERIOR OF THE LINE FROM A TO B INTERSECTS WITH THE I-TH LINE

30 continue
   num = num + 1
   IF (num > m) GO TO 210
      u(num) = a(1) + s*h
      v(num) = a(2) + s*k
   CYCLE
!-----------------------------------------------------------------------
!                     THE LINES ARE PARALLEL
!-----------------------------------------------------------------------
40 continue
  IF (ABS(hi) > ABS(ki)) GO TO 50

  d = a(2) - y(i)
  IF (ABS(d) <= tol0*MAX(ABS(a(2)),ABS(y(i)))) d = 0.0
  s = d/ki

  p = x(i) + s*hi
  IF (ABS(a(1) - p) > tol*MAX(ABS(a(1)),ABS(p))) CYCLE

  d = b(2) - y(i)
  IF (ABS(d) <= tol0*MAX(ABS(b(2)),ABS(y(i)))) d = 0.0
  t = d/ki
  GO TO 60

50 d = a(1) - x(i)
  IF (ABS(d) <= tol0*MAX(ABS(a(1)),ABS(x(i)))) d = 0.0
  s = d/hi

  p = y(i) + s*ki
  IF (ABS(p - a(2)) > tol*MAX(ABS(p),ABS(a(2)))) CYCLE

  d = b(1) - x(i)
  IF (ABS(d) <= tol0*MAX(ABS(b(1)),ABS(x(i)))) d = 0.0
  t = d/hi

!              THE 2 LINES ARE PORTIONS OF THE SAME STRAIGHT INFINITE LINE

60 continue
  IF (s > 0.0 .AND. s < onem) GO TO 220
  IF (t > 0.0 .AND. t < onem) GO TO 220
  tmin = MIN(s,t)
  tmax = MAX(s,t)
  IF (tmax <= 0.0) GO TO 70
  IF (tmin >= onem) GO TO 80
  GO TO 220

70 continue
  IF (tmax < 0.0) CYCLE
  IF (num > 0) CYCLE
  IF (tmax == s) GO TO 10
  GO TO 21

80 continue
  IF (tmin > 1.0) CYCLE
  IF (tmin == s) GO TO 10
  GO TO 21

ENDDO

   IF (ind == 0) GO TO 200

   IF (num < 2) RETURN
   IF (u(num) == x(1) .AND. v(num) == y(1)) num = num - 1
RETURN

! ERROR RETURN

200 continue
   ierr = 1
   RETURN

210 continue
   ierr = 2
   num = num - 1
RETURN

220 continue
   ierr = -i

END SUBROUTINE Poly_Intercept
!>
!!##NAME
!!        polyarea(3f) - [M_math:geometry]compute the area bounded by a closed polygonal curve
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
!!    Given a sequence of points (X(I),Y(I)), polyarea(3f) computes the area
!!    bounded by the closed polygonal curve which passes through the points in
!!    the order that they are indexed. The final point of the curve is assumed
!!    to be the first point given. Therefore, it need not be listed at the end
!!    of X and Y. The curve is not required to be simple ( e.g. It may cross over
!!    itself).
!!
!!##OPTIONS
!!
!!##RETURNS
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
!!    x=[ 0.0, 10.0,  0.0,  0.0, 10.0, 0.0]
!!    y=[10.0, 10.0,  0.0, 10.0,  0.0, 0.0]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0,  0.0, 10.0, 10.0,  0.0 ]
!!    y=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    x=[ 0.0, 10.0, 10.0,  0.0,  0.0 ]
!!    y=[10.0, 10.0,  0.0,  0.0, 10.0 ]
!!    write(*,*)'polyarea=',polyarea(x,y)
!!
!!    end program demo_polyarea
!===================================================================================================================================
!>
!! PROCEDURE:    polyarea(3f)
!! DESCRIPTION:  compute the area bounded by a closed polygonal curve
!! AUTHOR:       Code converted using TO_F90 by Alan Miller
!! VERSION:      2000-07-04  Time: 12:24:06
!===================================================================================================================================
function polyarea(x, y) result(fn_val)
implicit none
character(len=*),parameter :: ident="@(#)M_math::polyarea(3f): compute the area bounded by a closed polygonal curve "

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

!>
!!##NAME
!!      extremum(3f) - [M_math:statistics]Finds the minimum and maximum value in a REAL array.
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
!!    program testit
!!    use M_math, only : extremum
!!    real,allocatable :: arr(:)
!!    arr=[-10.0,8.8,-5.0,0.0,5.0,10.0,-0.3]
!!    call extremum(arr,small,big)
!!    write(*,*)'SMALL=',small
!!    write(*,*)'BIG=',big
!!    end program testit
!!
!!##SEE ALSO
!!    minval(3f), maxval(3f)
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine extremum(array,small,big)
implicit none
character(len=:),parameter :: ident='@(#)M_math::extremum(3f):Find the minimum and maximum value in a REAL array'

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
!!     integer,intent(in) :: n
!!     real,intent(in)    :: x(n)
!!     real,intent(out)   :: STAT(13)
!!##DESCRIPTION
!!
!!     Given a vector of real values calculate common basic statistical meansures for
!!     the array.
!!
!!##OPTIONS
!!      x      REAL input vector of values to determine statistical properties for
!!      n      size of input vector
!!##RETURNS
!!      stat   array of statistical measurements calculated
!!
!!          1.  mean
!!          2.  second moment about the mean
!!          3.  third moment about the mean
!!          4.  fourth moment about the mean
!!          5.  variance
!!          6.  standard deviation
!!          7.  skewness
!!          8.  kurtosis
!!          9.  sum
!!          10. largest value
!!          11. smallest value
!!          12. location of largest value
!!          13. location of smallest value
!!
!!##DEFINITIONS
!!      MEAN
!!
!!             A type of average, calculated by dividing the sum of
!!             a set of values by the number of values.
!!
!!                mean = Sum(Xi)/N
!!
!!      MEDIAN
!!
!!             A type of average, found by arranging the values in
!!             order and then selecting the one in the middle. If the
!!             total number of values in the sample is even, then the
!!             median is the mean of the two middle numbers.
!!
!!      MODE
!!
!!             The most frequent value in a group of values.
!!
!!      VARIANCE
!!
!!             The average of the square of the distance of each
!!             data point from the mean
!!
!!                variance = Sum((Xi-mean)^2))/N
!!
!!             for a population, or more commonly, for a sample the
!!             unbiased value is
!!
!!                variance = Sum((Xi-mean)^2))/(N-1)
!!
!!      STANDARD DEVIATION
!!
!!             The standard deviation is the square root of the
!!             variance.
!!
!!                sd = sqrt(variance)
!!
!!             It is the most commonly used measure of spread.
!!
!!      SKEWNESS
!!
!!             Skewness is a measure of symmetry, or more
!!             precisely, the lack of symmetry. A distribution, or
!!             data set, is symmetric if it looks the same to the left
!!             and right of the center point.
!!
!!                skewness = Sum{(X(i)-mean)^3} /((N-1)*SD^3)
!!
!!             Where SD is the standard deviation, and N is the number of
!!             samples. Some sources will use N instead of N-1 or they might
!!             present the formula in a slightly different mathematically
!!             equivalent format.
!!
!!                The skewness of symmetric data is zero
!!
!!      KURTOSIS
!!
!!             Kurtosis is a measure of whether the data are peaked
!!             or flat relative to a normal distribution. That is,
!!             data sets with high kurtosis tend to have a distinct
!!             peak near the mean, decline rather rapidly, and have
!!             heavy tails. Data sets with low kurtosis tend to have a
!!             flat top near the mean rather than a sharp peak. A
!!             uniform distribution would be the extreme case.
!!
!!                kurtosis = ( SUM{(X(i)-mean)^4} ) / ((N-1)*SD^4) -3
!!
!!             The standard normal distribution has a kurtosis of
!!             zero. Positive kurtosis indicates a "peaked"
!!             distribution and negative kurtosis indicates a "flat"
!!             distribution.
!!
!!           Although often called kurtosis, historically the above expression is
!!           for "excess kurtosis" because three is subtracted from the value.
!!           The purpose of this is to give the normal distribution a kurtosis
!!           of 0. In recent times, the term "excess kurtosis" is often simply
!!           called "kurtosis", so consider that whether to subtract 3 or not
!!           is merely a convention, not a right or wrong answer. When using a
!!           particular program, you just need to be aware of which convention
!!
!!           Again, another freqent difference is whether they use N in
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
!===================================================================================================================================
SUBROUTINE BDS (X,N,STAT)
character(len=*),parameter::ident="@(#)M_math::bds(3f): Basic Descriptive Statistics (based on a routine from the IBM collection)"
!  RETURN WITH M,U2,U3,U4,V,S,G1,G2,BIG,SMALL,IB,IS IN THAT ORDER IN LOCATIONS STAT(1) THROUGH STAT(13)
!-----------------------------------------------------------------------
!  nobody likes equivalences any more
   integer,parameter :: MEAN   =1  ! mean
   integer,parameter :: U2     =2  ! second moment about the mean
   integer,parameter :: U3     =3  ! third  moment about the mean
   integer,parameter :: U4     =4  ! fourth moment about the mean
   integer,parameter :: varnce =5  ! variance
   integer,parameter :: Sd     =6  ! standard deviation
   integer,parameter :: skew   =7  ! skewness
   integer,parameter :: kurto  =8  ! kurtosis
   integer,parameter :: SUM    =9  ! sum

   integer,parameter :: BIG    =10 ! highest value
   integer,parameter :: SMALL  =11 ! lowest value
   integer,parameter :: IB     =12 ! location of highest value
   integer,parameter :: IS     =13 ! location of lowest value

   integer,intent(in) :: n
   real,intent(in)    :: x(n)
   real,intent(out)   :: STAT(13)
   real               :: deltafixed
   real               :: deltasum
   real               :: fln
   integer            :: i, i20, i30
!-----------------------------------------------------------------------
      FLN=N
!-----------------------------------------------------------------------
!  SUM AND MEAN AND LARGEST AND SMALLEST AND LOCATION OF EXTREMES
      STAT(BIG)=X(1)                   ! biggest value
      STAT(SMALL)=X(1)                 ! smallest value
      STAT(IS)=1                       ! location of smallest
      STAT(IB)=1                       ! location of biggest
      STAT(SUM) =0.0                   ! sum of all values
      DO I=1,N
         STAT(SUM)= STAT(SUM) +X(I)    ! add all values into SUM
         IF(X(I).LT.STAT(SMALL))THEN   ! find smallest value
            STAT(SMALL)=X(I)
            STAT(IS)=I
         ENDIF
         if(X(I).GT.STAT(BIG))THEN     ! find biggest value
            STAT(BIG)=X(I)
            STAT(IB)=I
         ENDIF
      enddo
      STAT(MEAN)= STAT(SUM)/FLN
!-----------------------------------------------------------------------
!  SECOND, THIRD, FOURTH MOMENTS ABOUT THE MEAN
      STAT(u2) = 0.0
      STAT(u3) = 0.0
      STAT(u4) = 0.0
      DO I20=1,N
         deltafixed = (X(I20) - STAT(mean))
         deltasum = deltafixed
         DO I30=2,4
            deltasum = deltasum * deltafixed
            STAT(I30) = STAT(I30) + deltasum
         enddo
      enddo
!-----------------------------------------------------------------------
      STAT(u2) = STAT(u2) / FLN
      STAT(u3) = STAT(u3) / FLN
      STAT(u4) = STAT(u4) / FLN
!-----------------------------------------------------------------------
!  VARIANCE, STANDARD DEVIATION
      STAT(varnce) = FLN * STAT(u2) / (FLN-1.0)
      STAT(sd) = SQRT(STAT(varnce))
!-----------------------------------------------------------------------
!  SKEWNESS, KURTOSIS
      STAT(skew) = STAT(u3) /(STAT(u2) * SQRT(STAT(u2)))
      STAT(kurto) = STAT(u4) / (STAT(u2) * STAT(u2)) - 3.0
!-----------------------------------------------------------------------
END SUBROUTINE BDS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    skekur1(3f) - [M_math:statistics] variant on calculating skewness and kurtosis of an arrray
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
!!         in vector Y.  The values may be centered about either the
!!         MEAN (IOPT <> 0) or about ZERO (IOPT = 0).  The traditional
!!         divisor of N (NOT N-1) is used when the MEAN is estimated.
!!
!!    SUBPROGRAMS CALLED: -NONE-
!!
!!    CURRENT VERSION COMPLETED FEBRUARY 28, 1986
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
!!
!!##AUTHOR
!!    Written by Charles P. Reeve
!===================================================================================================================================
SUBROUTINE SKEKUR1(Y,NHI,YSKEW,YKURT,IOPT)
character(len=*),parameter::ident="@(#)M_math::skekur1(3f): variant on calculating skewness and kurtosis of an arrray"
REAL,INTENT(IN)    ::  Y(*)
INTEGER,INTENT(IN) :: NHI
REAL,INTENT(OUT)   :: YSKEW
REAL,INTENT(OUT)   :: YKURT
INTEGER,INTENT(IN) :: IOPT
   REAL            :: RN
   REAL            :: S
   INTEGER         :: I
   REAL            :: T2, T3, T4
   REAL            :: D
      RN = REAL(NHI)
      IF (IOPT.EQ.0) THEN
         S = 0.0
      ELSE
         S = 0.0
         DO I = 1, NHI
            S = S+Y(I)
         enddo
         S = S/RN
      ENDIF
      T2 = 0.0
      T3 = 0.0
      T4 = 0.0
      DO I = 1, NHI
         D = Y(I)-S
         T2 = T2+D**2
         T3 = T3+D**3
         T4 = T4+D**4
      enddo
      YSKEW=SQRT(RN)*T3/T2**1.5
      YKURT=RN*T4/T2**2
END SUBROUTINE SKEKUR1
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
!!    Note that this is apparently The skewness and kurtosis calculated by the
!!    MicroSoft Excel product.  I checked the Excel help and Excel uses the
!!    above formulas.  No references are given in the Excel documentation. Note
!!    that this converges on the standard expression for excess kurtosis and
!!    skewness as N becomes large.
!!##OPTIONS
!!##RETURNS
!!##EXAMPLES
!!
!!##AUTHOR
!!      John S. Urban
!!
!===================================================================================================================================
SUBROUTINE SKEKURX(Y,N,YSKEW,YKURT)
character(len=*),parameter::ident="@(#)M_math::skekurx(3f): COMPUTE UNBIASED ESTIMATOR OF THE POPULATION SKEWNESS AND KURTOSIS"
      integer,intent(in) :: n
      REAL,intent(in)    :: Y(*)
      REAL,intent(out)   :: YSKEW
      REAL,intent(out)   :: YKURT
      doubleprecision    :: xbar
      doubleprecision    :: rn
      doubleprecision    :: sum
      doubleprecision    :: sum3
      doubleprecision    :: sum4
      doubleprecision    :: stddev
      integer            :: i10,i20,i30
!-----------------------------------------------------------------------
      RN = N
!-----------------------------------------------------------------------
      ! GET AVERAGE
      XBAR = 0.0d0
      DO I10 = 1, N
         XBAR = XBAR+Y(I10)
      enddo
      XBAR = XBAR/RN
!-----------------------------------------------------------------------
      ! GET STANDARD DEVIATION
      SUM=0.0
      DO I30=1,N
         SUM=SUM+(Y(I30)-XBAR)**2
      enddo
      STDDEV=SQRT(SUM/(RN-1.0d0))
!-----------------------------------------------------------------------
      SUM3=0.0
      SUM4=0.0
      DO I20=1,N
         SUM3=SUM3+((Y(I20)-XBAR)/STDDEV)**3
         SUM4=SUM4+((Y(I20)-XBAR)/STDDEV)**4
      enddo
!-----------------------------------------------------------------------
      YSKEW=RN/((RN-1.0d0)*(RN-2.0d0))*SUM3
      YKURT= RN*(RN+1.0d0) / ((RN-1.0d0)*(RN-2.0d0)*(RN-3.0d0)) * SUM4 - 3.0d0*(RN-1.0d0)**2/((RN-2.0d0)*(RN-3.0d0))
!-----------------------------------------------------------------------
END SUBROUTINE SKEKURX
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    stddev(3f) - [M_math:statistics]given a real vector and the vector average calculate the standard deviation
!!##SYNTAX
!!    function stddev(vector,n,avg)
!!
!!    integer,intent(in) :: n
!!    real,intent(in)    :: vector(n)
!!    real,intent(in)    :: avg
!!    real               :: stddev
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
!!       A fraction of at least 1 - (1/k**2) of the observations  lie within k
!!       standard  deviations  of the average.  The theorem  guarantees  lower
!!       bounds on the percentage of observations within k standard deviations
!!       of the average.
!!##OPTIONS
!!    n            the size of the input vector
!!    vector(n)    the input vector
!!    avg          the average of the input vector
!!
!!##RETURNS
!!    stddev       the standard deviation of the vector
!!##EXAMPLE
!!
!!##AUTHOR
!!    1994 John S. Urban
!!##REFERENCE
!!    From Mark's  Handbook,  page 17-19, 8th edition
!===================================================================================================================================
function stddev(vector,n,avg)
implicit none
character(len=*),parameter :: ident="@(#)M_math::stddev(3f): find standard deviation of a real array"
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

!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    almost(3f) - [M_math] return true or false if two numbers agree up to specified number of digits
!!##SYNOPSIS
!!
!!    function almost(x,y,digits)
!!    real,intent(in) :: x,y
!!    real,intent(in) :: rdigits
!!##DESCRIPTION
!!    Returns true or false depending on whether the two numbers given agree to within the specified
!!    number of digits as calculated by ACCDIG(3f).
!!##EXAMPLE
!!
!===================================================================================================================================
function almost(x,y,digits,verbose)
character(len=*),parameter::ident="&
&@(#)M_math::almost(3f): function to compare two real numbers only up to a specified number of digits by calling ACCDIG(3f)"
real,intent(in)             :: x,y
real,intent(in)             :: digits
logical,intent(in),optional :: verbose
logical                     :: almost

   logical                  :: verbose_local
   real                     :: acurcy
   integer                  :: ind

   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif

   call accdig(x,y,digits,acurcy,ind)

   if(verbose_local)then
      write(*,*)'*almost* for values ',x,y,' agreement of ',acurcy,' digits out of requested ',digits
   endif

   if(ind.eq.0)then
      almost=.true.
   else
      almost=.false.
   endif

end function almost
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      accdig(3f) - [M_math] compare two real numbers only up to a specified number of digits
!!
!!##SYNOPSIS
!!
!!       subroutine accdig(x,y,rdgits,acurcy,ind)
!!
!!        real,intent(in)     :: X
!!        real,intent(in)     :: Y
!!        real,intent(in)     :: DIGI0
!!        real,intent(out)    :: acurcy
!!        integer,intent(out) :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    the values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisified.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!            ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!            ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!            ACURCY=8                 if X=Y
!!
!!            ACURCY is never less than -8 or greater than 8
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_accdig ! fortran 90 example
!!    use M_math, only : accdig
!!    integer digi
!!    real vals(9)
!!    data vals/ &
!!      &1.234680,   1.2345378,  2.2234568, 1.2345678, &
!!      &1.2345679, -1.2345678, 76.234567,  2.4691356, &
!!      &0.0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0
!!          b=a+1.0/(10**i10)
!!          call accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0
!!          b=a+1.0/(10**i20)
!!          call accdig(a,b,real(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call accdig(1.2345678,vals(i30),8.0,acurcy1,ind1)
!!          call accdig(vals(i30),1.2345678,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_accdig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. accdig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!         o M_journal(),log10(), abs(1)
!!
!!##FILES
!!      o libjust4.a
!!##LEGAL RESTRICTIONS
!!      none
!!##QA
!!    o Authors: David Hogben, John S. Urban
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE accdig(X,Y,digi0,ACURCY,IND)
use M_journal, only : journal
implicit none
character(len=*),parameter::ident="@(#)M_math::accdig(3f): compare two real numbers only up to a specified number of digits"
!     INPUT ...
      real,intent(in) :: x           ! First  of two real numbers to be compared.
      real,intent(in) :: y           ! Second of two real numbers to be compared.
      real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
      integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
                                     ! = 1, If tolerance is not satisified.
      real,intent(out) :: acurcy     ! = - LOG10 (ABS((X-Y)/Y)))

      real     ::  diff
      real     ::  digi
      integer  ::  ireal_significant_digits
!     ==================================================================
      ireal_significant_digits=int(log10(2.**digits(0.0))) ! maximum number of significant digits in a real number.
      digi=digi0
      if(digi.le.0)then
         call journal('sc','*accdig* bad number of significant digits=',digi)
         digi=ireal_significant_digits
      else if(digi .gt. ireal_significant_digits)then
         call journal('sc','*accdig* significant digit request too high=',digi)
         digi=min(digi,real(ireal_significant_digits))
      endif
!     ..................................................................
      diff = x - y
      if (diff .eq. 0.0) then
         acurcy = digi
      else if (y .eq. 0.0) then
         acurcy = - log10 (abs (x))
      else
         acurcy = - log10 ( abs(diff) ) + log10 ( abs(y) )
      endif
!     ..................................................................
      if (acurcy .lt. digi ) then
         ind = 1
      else
         ind = 0
      endif
!     ..................................................................
END SUBROUTINE accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      dp_addig(3f) - [M_math] compare two DOUBLEPRECISION numbers only up to a specified number of digits
!!
!!##SYNOPSIS
!!
!!       subroutine dp_addig(x,y,rdgits,acurcy,ind)
!!
!!        doubleprecision,intent(in)     :: X
!!        doubleprecision,intent(in)     :: Y
!!        doubleprecision,intent(in)     :: DIGI0
!!        doubleprecision,intent(out)    :: acurcy
!!        integer,intent(out) :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call dp_addig(X,Y,DIGI0,ACURCY,IND)
!!
!!    the values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisified.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!            ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!            ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!            ACURCY=8                 if X=Y
!!
!!            ACURCY is never less than -8 or greater than 8
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare then  to 1.2345678 ...
!!
!!       ================================================
!!       A number     |    ACURCY       |   ACURCY
!!                    |    1.2345678=Y  |   1.2345678=X
!!       ================================================
!!        1.234680    |    3.7900571    |   3.7901275
!!        1.2345378   |    4.6144510    |   4.6144404
!!        2.2234568   |    0.096367393  |   0.35188114
!!        1.2345678   |    8.0000000    |   8.0000000
!!        1.2345679   |    7.0732967    |   7.0731968
!!       -1.2345678   |   -0.30103000   |  -0.30103000
!!       76.234567    |   -1.7835463    |   0.0070906729
!!        2.4691356   |    0.0          |   0.3010300
!!        0.0         |    0.0          |  -0.91514942.
!!
!!    Due to the typical limits of the log function, the number of
!!    significant digits in the result is best considered to be three.
!!
!!    Notice that 1.2345678=Y produces different values than 1.2345678=X
!!
!!    A negative result indicates the two values being compared either do
!!    not agree in the first digit or they differ with respect to sign. An
!!    example of two numbers which do not agree in their leading digit (and
!!    actually differ in order of magnitude) is given above by X=76.234567
!!    and Y=1.2345678; the accuracy reported is -1.7835463. An example of
!!    two numbers which do not agree in sign in X=-1.2345678 and Y=1.2345678;
!!    here the accuracy reported is -0.30103000.
!!
!!##EXAMPLE
!!
!!
!!   Example program:
!!
!!    program demo_dp_addig ! fortran 90 example
!!    use M_math, only : dp_addig
!!    integer digi
!!    real vals(9)
!!    data vals/ &
!!      &1.234680,   1.2345378,  2.2234568, 1.2345678, &
!!      &1.2345679, -1.2345678, 76.234567,  2.4691356, &
!!      &0.0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0
!!          b=a+1.0/(10**i10)
!!          call dp_addig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0
!!          b=a+1.0/(10**i20)
!!          call dp_addig(a,b,dble(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call dp_addig(1.2345678,vals(i30),8.0,acurcy1,ind1)
!!          call dp_addig(vals(i30),1.2345678,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_dp_addig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. dp_addig V 7.00  2/14/90. **
!!       David Hogben,
!!       Statistical Engineering Division,
!!       Center for Computing and Applied Mathematics,
!!       A337 Administration Building,
!!       National Institute of Standards and Technology,
!!       Gaithersburg, MD 20899
!!                      TELEPHONE 301-975-2845
!!           ORIGINAL VERSION -  October, 1969.
!!            CURRENT VERSION - February, 1990.
!!            JSU     VERSION - February, 1991.
!!
!!##DEPENDENCIES
!!         o M_journal(),log10(), abs(1)
!!
!!##FILES
!!      o libjust4.a
!!##LEGAL RESTRICTIONS
!!      none
!!##QA
!!    o Authors: David Hogben, John S. Urban
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE dp_accdig (X,Y,digi0,ACURCY,IND)
use M_journal, only : journal
implicit none
character(len=*),parameter::ident="@(#)M_math::dp_accdig(3f): compare two double values only up to a specified number of digits"
!     INPUT ...
      doubleprecision,intent(in)  :: x           ! FIRST  OF TWO DOUBLE NUMBERS TO BE COMPARED.
      doubleprecision,intent(in)  :: y           ! SECOND OF TWO DOUBLE NUMBERS TO BE COMPARED.
      doubleprecision,intent(in)  :: digi0       ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.
!     OUTPUT ...
      integer,intent(out)         :: ind         ! = 0, IF TOLERANCE IS     SATISFIED.
                                                 ! = 1, IF TOLERANCE IS NOT SATISIFIED.
      doubleprecision,intent(out) :: acurcy      ! = - LOG10 (ABS((X-Y)/Y)))
      doubleprecision             ::  diff
      doubleprecision             ::  digi
      integer                     ::  idble_significant_digits
!     ==================================================================
      idble_significant_digits=int(log10(2.0**digits(0.0d0))) ! MAXIMUM NUMBER OF SIGNIFICANT DIGITS IN A DOUBLE NUMBER.
      digi=digi0
      if(digi.le.0)then
         call journal('sc','*dp_accdig* bad number of significant digits=',dble(digi))
         digi=idble_significant_digits
      else if(digi .gt. idble_significant_digits)then
         call journal('sc','*dp_accdig* significant digit request too high=',dble(digi))
         digi=min(digi,dble(idble_significant_digits))
      endif
      diff = x - y
      if (diff .eq. 0.0) then
         acurcy = digi
      else if (y .eq. 0.0) then
         acurcy = - log10 (abs (x))
      else
         acurcy = - log10 ( abs(diff) ) + log10 ( abs(y) )
      endif
      if (acurcy .lt. digi ) then
         ind = 1
      else
         ind = 0
      endif
      end subroutine dp_accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
function round(val,idigits0)
implicit none
character(len=*),parameter :: ident="@(#)M_math::round(3f):round val to specified number of significant digits"
integer,parameter          :: dp=kind(0.0d0)
real(kind=dp),intent(in)   :: val
integer,intent(in)         :: idigits0
   integer                 :: idigits,ipow
   real(kind=dp)           :: aval,rnormal
   real(kind=dp)           :: round
!  this does not work very well because of round-off errors.
!  Make a better one, probably have to use machine-dependent bit shifting
   ! make sure a reasonable number of digits has been requested
   idigits=max(1,idigits0)
   aval=abs(val)
!  select a power that will normalize the number
!  (put it in the range 1 > abs(val) <= 0)
   if(aval.ge.1)then
      ipow=int(log10(aval)+1)
   else
      ipow=int(log10(aval))
   endif
   rnormal=val/(10.0d0**ipow)
   if(rnormal.eq.1)then
      ipow=ipow+1
   endif
   !normalize, multiply by 10*idigits to an integer, and so on
   round=real(anint(val*10.d0**(idigits-ipow)))*10.d0**(ipow-idigits)
end function round
!>
!!##NAME
!!     scale1(3f) - [M_math] find new range xminp xmaxp divisible into approximately n linear intervals of size dist
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
!!    "CACM Algorithm 463 scale1".  Typically used to find nice ranges for
!!    axis scales.
!!
!!##EXAMPLE
!!
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine scale1(xmin0, xmax0, n0, xminp, xmaxp, dist)
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
implicit none
character(len=*),parameter::ident="&
&@(#)M_math::scale1(3f):given xmin,xmax,n, find new range xminp xmaxp divisible into approximately n linear intervals of size dist"
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

   doubleprecision      :: fn, a, al, b, fm1
   integer              :: i, nal, m1, ivint
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
!!    Typically used to find nice ranges for axis scales.  Given XMIN, XMAX
!!    and N, where N is greater than 1, find new log range.  Finds a new
!!    range XMINP and XMAXP divisible into exactly N LOGARITHMIC intervals,
!!    where the ratio of adjacent uniformly spaced scale values
!!    is DIST.
!!
!!##EXAMPLE
!!
!===================================================================================================================================
!===================================================================================================================================
subroutine scale3(xmin0, xmax0, n0 , xminp, xmaxp, dist)
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
use M_journal, only : journal
implicit none
character(len=*),parameter::ident="@(#)M_math::scale3(3f):find nice log range."

real,intent(in)               :: xmin0, xmax0
integer,intent(in)            :: n0
real,intent(out)              :: xminp, xmaxp, dist

   doubleprecision,parameter  :: del = 0.000002d0
   doubleprecision xmin, xmax, hold
   integer n
   doubleprecision xminl, xmaxl, fn, a, al, b, distl, fm1, fm2
   integer     nal, i, m1, m2, np, nx, iv
   doubleprecision,save :: vint(11) = [10.0d0,9.0d0,8.0d0,7.0d0,6.0d0,5.0d0,4.0d0,3.0d0,2.0d0,1.0d0,0.5d0]
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
30    continue
   distl = 10.0d0**(nal + 1) / vint(iv)
   fm1 = xminl / distl
   m1 = int(fm1)
   if (fm1 .lt. 0.0d0) then
      m1 = m1 -1
   endif
   if (abs(dble(m1) + 1.0d0 - fm1) .lt. del)then
      m1 = m1 - 1
   endif
!===================================================================================================================================
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
!===================================================================================================================================
!  Check whether another pass is necessary
   np = m2 - m1
   iv = iv + 1
   if( np .gt. n) goto 30
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

!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_2x2(A) result(B)
character(len=*),parameter::ident="@(#)M_math::invert_2x2(3f): performs a direct calculation of the inverse of a 2x2 matrix"
   integer,parameter         :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: A(2,2)   !! Matrix
   real(kind=wp)             :: B(2,2)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function double_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_3x3(A) result(B)
character(len=*),parameter::ident="@(#)M_math::invert_3x3(3f): performs a direct calculation of the inverse of a 3x3 matrix"
   integer,parameter         :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: A(3,3)   !! Matrix
   real(kind=wp)             :: B(3,3)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function double_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function double_invert_4x4(A) result(B)
character(len=*),parameter::ident="@(#)M_math::invert_4x4(3f): performs a direct calculation of the inverse of a 4x4 matrix"
   integer,parameter            :: wp=kind(0.0d0)
   real(kind=wp), intent(in) :: A(4,4)   !! Matrix
   real(kind=wp)             :: B(4,4)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function double_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_2x2(A) result(B)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter         :: wp=kind(0)
   integer(kind=wp), intent(in) :: A(2,2)   !! Matrix
   integer(kind=wp)             :: B(2,2)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function integer_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_3x3(A) result(B)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter         :: wp=kind(0)
   integer(kind=wp), intent(in) :: A(3,3)   !! Matrix
   integer(kind=wp)             :: B(3,3)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function integer_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function integer_invert_4x4(A) result(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind(0)
   integer(kind=wp), intent(in) :: A(4,4)   !! Matrix
   integer(kind=wp)             :: B(4,4)   !! Inverse matrix
   integer(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function integer_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_2x2(A) result(B)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter         :: wp=kind(0.0)
   real(kind=wp), intent(in) :: A(2,2)   !! Matrix
   real(kind=wp)             :: B(2,2)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function real_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_3x3(A) result(B)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter         :: wp=kind(0.0)
   real(kind=wp), intent(in) :: A(3,3)   !! Matrix
   real(kind=wp)             :: B(3,3)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function real_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function real_invert_4x4(A) result(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind(0.0)
   real(kind=wp), intent(in) :: A(4,4)   !! Matrix
   real(kind=wp)             :: B(4,4)   !! Inverse matrix
   real(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function real_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_2x2(A) result(B)
   !! Performs a direct calculation of the inverse of a 2 x 2 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: A(2,2)   !! Matrix
   complex(kind=wp)             :: B(2,2)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * A(2,2)
   B(2,1) = -detinv * A(2,1)
   B(1,2) = -detinv * A(1,2)
   B(2,2) = +detinv * A(1,1)
end function complex_invert_2x2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_3x3(A) result(B)
   !! Performs a direct calculation of the inverse of a 3 x 3 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: A(3,3)   !! Matrix
   complex(kind=wp)             :: B(3,3)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
             - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
             + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

   ! Calculate the inverse of the matrix
   B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
   B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
   B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
   B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
   B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
   B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
   B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
   B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
   B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
end function complex_invert_3x3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
pure function complex_invert_4x4(A) result(B)
   !! Performs a direct calculation of the inverse of a 4 x 4 matrix.
   integer,parameter            :: wp=kind((0.0,0.0))
   complex(kind=wp), intent(in) :: A(4,4)   !! Matrix
   complex(kind=wp)             :: B(4,4)   !! Inverse matrix
   complex(kind=wp)             :: detinv

   ! Calculate the inverse determinant of the matrix
   detinv = &
     1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
      - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
      + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
      - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

   ! Calculate the inverse of the matrix
   B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
   B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
   B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
   B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
   B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
   B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
   B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
   B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
   B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
   B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
   B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
   B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
   B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
   B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))
end function complex_invert_4x4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

end module M_math

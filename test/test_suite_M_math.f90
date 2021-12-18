program program_test_suite_M_math
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level
use :: M_verify,   only : unit_check_stop
use M_math
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
   write(*,*)'STARTED M_math'
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*! setup
! GEOMETRY
   call test_citer()              ! determine various geometric properties of circle segment given radius and area of the segment.
   call test_envelope()           ! Find the vertices (in clockwise order) of a polygon enclosing the points [((x(i), y(i), i=1,n)].
   call test_inpolygon()          ! Subroutine to determine whether or not a point is in a polygon
   call test_locpt()              ! find if a point is inside a polygonal path
   call test_poly_intercept()     ! find points where a line intersects a polygon
   call test_polyarea()           ! find area of a polygon
   call test_polyarea_shoelace()  ! find area of a polygon using shoelace algorithm
   call test_polyarea_mid_point() ! find area of a polygon
   call test_closest()            ! find point closest to target
   call test_hypot()              ! Euclidean distance
! FIT
   call test_julfit()             ! linear least square fit
   call test_julfit1()            ! linear least square fit(y=a*x+b)
   call test_lowess()             ! data smoothing using locally weighted regression
   call test_splift()             ! fits a spline to the n data points given in x and y
   call test_splint()             ! interpolates and twice differentiates a cubic spline
   call test_linearint()          ! linear interpolation
! FITS
   call test_ju_polfit()
   call test_ju_pvalue()
   call test_glstsq()
!  private gcsgau1()
!  private gcsgau2()
! INTEGRATE
   call test_qhfg()
   call test_qhsg()
   call test_qtfg()
   call test_trapezoidal_integral()
! STATISTICS
   call test_extremum()       ! find the minimum and maximum value in a real array
   call test_bds()            ! basic descriptive statistics
   call test_ncr()            ! number of combinations of size R from N cases
   call test_skekurx()        ! skew and kurtosis variant
   call test_skekur1()        ! skew and kurtosis variant
   call test_stddev()         ! standard deviation
! SCALES
   call test_scale1()         ! given xmin,xmax,n, find new range xminp xmaxp divisible into ~ n linear intervals of size dist
   call test_scale3()         ! find nice log range, typically for an axis
! MATRIX
   call test_invert_2x2()     ! directly invert 2x2 matrix
   call test_invert_3x3()     ! directly invert 3x3 matrix
   call test_invert_4x4()     ! directly invert 4x4 matrix
   call test_complex_invert_2x2()
   call test_complex_invert_3x3()
   call test_complex_invert_4x4()
   call test_double_invert_2x2()
   call test_double_invert_3x3()
   call test_double_invert_4x4()
   call test_integer_invert_2x2()
   call test_integer_invert_3x3()
   call test_integer_invert_4x4()
   call test_real_invert_2x2()
   call test_real_invert_3x3()
   call test_real_invert_4x4()
   call test_magic_square()   ! create magic squares
! POLYNOMIAL
   call test_quadratic()      ! return roots of quadratic equation even if complex
!*! teardown
   call unit_check_stop()
   write(*,*)'COMPLETED M_math'
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invert_2x2()

   call unit_check_start('invert_2x2',msg='')
   !*!call unit_check('invert_2x2', 0.eq.0, 'checking', 100)
   call unit_check_done('invert_2x2',msg='')
end subroutine test_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invert_3x3()

   call unit_check_start('invert_3x3',msg='')
   !*!call unit_check('invert_3x3', 0.eq.0, 'checking', 100)
   call unit_check_done('invert_3x3',msg='')
end subroutine test_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_invert_4x4()

   call unit_check_start('invert_4x4',msg='')
   !*!call unit_check('invert_4x4', 0.eq.0, 'checking', 100)
   call unit_check_done('invert_4x4',msg='')
end subroutine test_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bds()

   call unit_check_start('bds',msg='')
   !*!call unit_check('bds', 0.eq.0, 'checking', 100)
   call unit_check_done('bds',msg='')
end subroutine test_bds
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_citer()

   call unit_check_start('citer',msg='')
   !*!call unit_check('citer', 0.eq.0, 'checking', 100)
   call unit_check_done('citer',msg='')
end subroutine test_citer
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_complex_invert_2x2()

   call unit_check_start('complex_invert_2x2',msg='')
   !*!call unit_check('complex_invert_2x2', 0.eq.0, 'checking', 100)
   call unit_check_done('complex_invert_2x2',msg='')
end subroutine test_complex_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_complex_invert_3x3()

   call unit_check_start('complex_invert_3x3',msg='')
   !*!call unit_check('complex_invert_3x3', 0.eq.0, 'checking', 100)
   call unit_check_done('complex_invert_3x3',msg='')
end subroutine test_complex_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_complex_invert_4x4()

   call unit_check_start('complex_invert_4x4',msg='')
   !*!call unit_check('complex_invert_4x4', 0.eq.0, 'checking', 100)
   call unit_check_done('complex_invert_4x4',msg='')
end subroutine test_complex_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_double_invert_2x2()

   call unit_check_start('double_invert_2x2',msg='')
   !*!call unit_check('double_invert_2x2', 0.eq.0, 'checking', 100)
   call unit_check_done('double_invert_2x2',msg='')
end subroutine test_double_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_double_invert_3x3()

   call unit_check_start('double_invert_3x3',msg='')
   !*!call unit_check('double_invert_3x3', 0.eq.0, 'checking', 100)
   call unit_check_done('double_invert_3x3',msg='')
end subroutine test_double_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_double_invert_4x4()

   call unit_check_start('double_invert_4x4',msg='')
   !*!call unit_check('double_invert_4x4', 0.eq.0, 'checking', 100)
   call unit_check_done('double_invert_4x4',msg='')
end subroutine test_double_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_envelope()

   call unit_check_start('envelope',msg='')
   !*!call unit_check('envelope', 0.eq.0, 'checking', 100)
   call unit_check_done('envelope',msg='')
end subroutine test_envelope
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_extremum()

   call unit_check_start('extremum',msg='')
   !*!call unit_check('extremum', 0.eq.0, 'checking', 100)
   call unit_check_done('extremum',msg='')
end subroutine test_extremum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_glstsq()

   call unit_check_start('glstsq',msg='')
   !*!call unit_check('glstsq', 0.eq.0, 'checking', 100)
   call unit_check_done('glstsq',msg='')
end subroutine test_glstsq
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inpolygon()

   call unit_check_start('inpolygon',msg='')
   !*!call unit_check('inpolygon', 0.eq.0, 'checking', 100)
   call unit_check_done('inpolygon',msg='')
end subroutine test_inpolygon
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_integer_invert_2x2()

   call unit_check_start('integer_invert_2x2',msg='')
   !*!call unit_check('integer_invert_2x2', 0.eq.0, 'checking', 100)
   call unit_check_done('integer_invert_2x2',msg='')
end subroutine test_integer_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_integer_invert_3x3()

   call unit_check_start('integer_invert_3x3',msg='')
   !*!call unit_check('integer_invert_3x3', 0.eq.0, 'checking', 100)
   call unit_check_done('integer_invert_3x3',msg='')
end subroutine test_integer_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_integer_invert_4x4()

   call unit_check_start('integer_invert_4x4',msg='')
   !*!call unit_check('integer_invert_4x4', 0.eq.0, 'checking', 100)
   call unit_check_done('integer_invert_4x4',msg='')
end subroutine test_integer_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ju_polfit()

   call unit_check_start('ju_polfit',msg='')
   !*!call unit_check('ju_polfit', 0.eq.0, 'checking', 100)
   call unit_check_done('ju_polfit',msg='')
end subroutine test_ju_polfit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ju_pvalue()

   call unit_check_start('ju_pvalue',msg='')
   !*!call unit_check('ju_pvalue', 0.eq.0, 'checking', 100)
   call unit_check_done('ju_pvalue',msg='')
end subroutine test_ju_pvalue
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_julfit()

   call unit_check_start('julfit',msg='')
   !*!call unit_check('julfit', 0.eq.0, 'checking', 100)
   call unit_check_done('julfit',msg='')
end subroutine test_julfit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_julfit1()

   call unit_check_start('julfit1',msg='')
   !*!call unit_check('julfit1', 0.eq.0, 'checking', 100)
   call unit_check_done('julfit1',msg='')
end subroutine test_julfit1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_linearint()

   call unit_check_start('linearint',msg='')
   !*!call unit_check('linearint', 0.eq.0, 'checking', 100)
   call unit_check_done('linearint',msg='')
end subroutine test_linearint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_locpt()

   call unit_check_start('locpt',msg='')
   !*!call unit_check('locpt', 0.eq.0, 'checking', 100)
   call unit_check_done('locpt',msg='')
end subroutine test_locpt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lowess()

   call unit_check_start('lowess',msg='')
   !*!call unit_check('lowess', 0.eq.0, 'checking', 100)
   call unit_check_done('lowess',msg='')
end subroutine test_lowess
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_magic_square()

   call unit_check_start('magic_square',msg='')
   !*!call unit_check('magic_square', 0.eq.0, 'checking', 100)
   call unit_check_done('magic_square',msg='')
end subroutine test_magic_square
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ncr()

   call unit_check_start('ncr',msg='')
   !*!call unit_check('ncr', 0.eq.0, 'checking', 100)
   call unit_check_done('ncr',msg='')
end subroutine test_ncr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_poly_intercept()

   call unit_check_start('poly_intercept',msg='')
   !*!call unit_check('poly_intercept', 0.eq.0, 'checking', 100)
   call unit_check_done('poly_intercept',msg='')
end subroutine test_poly_intercept
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyarea()

   call unit_check_start('polyarea',msg='')
   !*!call unit_check('polyarea', 0.eq.0, 'checking', 100)
   call unit_check_done('polyarea',msg='')
end subroutine test_polyarea
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyarea_mid_point()

   call unit_check_start('polyarea_mid_point',msg='')
   !*!call unit_check('polyarea_mid_point', 0.eq.0, 'checking', 100)
   call unit_check_done('polyarea_mid_point',msg='')
end subroutine test_polyarea_mid_point
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_polyarea_shoelace()

   call unit_check_start('polyarea_shoelace',msg='')
   !*!call unit_check('polyarea_shoelace', 0.eq.0, 'checking', 100)
   call unit_check_done('polyarea_shoelace',msg='')
end subroutine test_polyarea_shoelace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_closest()

   call unit_check_start('closest',msg='')
   !*!call unit_check('closest', 0.eq.0, 'checking', 100)
   call unit_check_done('closest',msg='')
end subroutine test_closest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_hypot()

   call unit_check_start('hypot',msg='')
   !*!call unit_check('hypot', 0.eq.0, 'checking', 100)
   call unit_check_done('hypot',msg='')
end subroutine test_hypot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_qhfg()

   call unit_check_start('qhfg',msg='')
   !*!call unit_check('qhfg', 0.eq.0, 'checking', 100)
   call unit_check_done('qhfg',msg='')
end subroutine test_qhfg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_qhsg()

   call unit_check_start('qhsg',msg='')
   !*!call unit_check('qhsg', 0.eq.0, 'checking', 100)
   call unit_check_done('qhsg',msg='')
end subroutine test_qhsg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_qtfg()

   call unit_check_start('qtfg',msg='')
   !*!call unit_check('qtfg', 0.eq.0, 'checking', 100)
   call unit_check_done('qtfg',msg='')
end subroutine test_qtfg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_trapezoidal_integral()

   call unit_check_start('trapezoidal_integral',msg='')
   !*!call unit_check('trapezoidal_integral', 0.eq.0, 'checking', 100)
   call unit_check_done('trapezoidal_integral',msg='')
end subroutine test_trapezoidal_integral
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_quadratic()

   call unit_check_start('quadratic',msg='')
   !*!call unit_check('quadratic', 0.eq.0, 'checking', 100)
   call unit_check_done('quadratic',msg='')
end subroutine test_quadratic
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_invert_2x2()

   call unit_check_start('real_invert_2x2',msg='')
   !*!call unit_check('real_invert_2x2', 0.eq.0, 'checking', 100)
   call unit_check_done('real_invert_2x2',msg='')
end subroutine test_real_invert_2x2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_invert_3x3()

   call unit_check_start('real_invert_3x3',msg='')
   !*!call unit_check('real_invert_3x3', 0.eq.0, 'checking', 100)
   call unit_check_done('real_invert_3x3',msg='')
end subroutine test_real_invert_3x3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_invert_4x4()

   call unit_check_start('real_invert_4x4',msg='')
   !*!call unit_check('real_invert_4x4', 0.eq.0, 'checking', 100)
   call unit_check_done('real_invert_4x4',msg='')
end subroutine test_real_invert_4x4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scale1()

   call unit_check_start('scale1',msg='')
   !*!call unit_check('scale1', 0.eq.0, 'checking', 100)
   call unit_check_done('scale1',msg='')
end subroutine test_scale1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_scale3()

   call unit_check_start('scale3',msg='')
   !*!call unit_check('scale3', 0.eq.0, 'checking', 100)
   call unit_check_done('scale3',msg='')
end subroutine test_scale3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_skekur1()

   call unit_check_start('skekur1',msg='')
   !*!call unit_check('skekur1', 0.eq.0, 'checking', 100)
   call unit_check_done('skekur1',msg='')
end subroutine test_skekur1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_skekurx()

   call unit_check_start('skekurx',msg='')
   !*!call unit_check('skekurx', 0.eq.0, 'checking', 100)
   call unit_check_done('skekurx',msg='')
end subroutine test_skekurx
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splift()

   call unit_check_start('splift',msg='')
   !*!call unit_check('splift', 0.eq.0, 'checking', 100)
   call unit_check_done('splift',msg='')
end subroutine test_splift
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_splint()

   call unit_check_start('splint',msg='')
   !*!call unit_check('splint', 0.eq.0, 'checking', 100)
   call unit_check_done('splint',msg='')
end subroutine test_splint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stddev()

   call unit_check_start('stddev',msg='')
   !*!call unit_check('stddev', 0.eq.0, 'checking', 100)
   call unit_check_done('stddev',msg='')
end subroutine test_stddev
!===================================================================================================================================
end program program_test_suite_M_math
!===================================================================================================================================

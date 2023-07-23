program test_suite_M_calculator
use, intrinsic :: iso_fortran_env, only: &
& stdin => input_unit,   &
& stdout => output_unit, &
& stderr => error_unit
use, intrinsic :: iso_fortran_env, only : compiler_version
use, intrinsic :: iso_fortran_env, only : compiler_options
use M_framework,  only : unit_test_start, unit_test, unit_test_end, unit_test_msg, unit_test_stop
use M_framework, only : unit_test_mode, unit_test_level, unit_test_flags
use M_framework,  only : almost
use M_calculator, only : calculator
! convenience routines
use M_calculator, only : inum0, rnum0, dnum0, snum0, expression
! constants
use M_calculator, only : iclen_calc, ixy_calc, icname_calc, x, y, values_len, values
!!use M_calculator, only : read_config
implicit none
logical, parameter :: T=.true., F=.false.
integer,parameter :: bug=0 ! gfortran-11 bug where function calls as arguments cause errors, but expressions do not

   !!EXAMPLE OF ALMOST JUST PRINTING VALUES!!if(almost(z,10*sin(3.1416d0/4.0d0),35,verbose=.true.))continue
   print '(4(a/))', &
      'This file was compiled by ', &
      compiler_version(),           &
      'using the options ',         &
      compiler_options()

! optional call to change default modes
   call unit_test_mode(   &
       keep_going=T,      &
       flags=[0],         &
       luns=[stdout],     &
       command='',        &
       brief=T,           &
       match='',          &
       interactive=F,     &
       CMDLINE=T,         &
       debug=F)

   unit_test_level=0
      
call test_calculator()

call test_dnum0()
call test_inum0()
call test_expression()
call test_rnum0()
call test_snum0()

call test_c()
call test_juown1()

call test_funcs()

call unit_test_stop()

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_calculator()
! iclen_calc : max length of expression or variable value as a string
integer, parameter         :: dp = kind(0.0d0)
character(len=iclen_calc) :: line
character(len=iclen_calc) :: outlin
character(len=iclen_calc) :: event
real(kind=dp)             :: rvalue
integer                   :: ierr

   call unit_test_start('calculator',msg='')

   ierr = 0
   call calculator('ownmode(1)', outlin, event, rvalue, ierr)

   ! activate user-defined function interface
   !!call unit_test('calculator', 0.eq.0, 'checking',100)

   line='1.3/sind(90)+1-20*2/3'
   call calculator(line, outlin, event, rvalue, ierr)
   select case (ierr)
      ! several different meanings to the error flag returned by calculator
   case (0)
      ! a numeric value was returned without error
      write (*, '(a,a,a)') trim(outlin), ' = ', trim(line)
   case (2)
      ! a string value was returned without error
      write (*, '(a)') trim(event(:int(rvalue)))
   case (1)
      ! a request for a message has been returned (from DUMP or FUNC)
      write (*, '(a,a)') 'message===>', trim(event(:len_trim(event)))
   case (-1)
      ! an error has occurred
      write (*, '(a,a)') 'error===>', trim(event(:len_trim(event)))
   case default
      ! this should not occur
      WRITE (6, '(A,i10)') '*CALCULATOR* UNEXPECTED IERR VALUE ', IERR
   end select

   call unit_test_end('calculator',msg='')
end subroutine test_calculator
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c()

   call unit_test_start('c',msg='')
   !!call unit_test('c', 0.eq.0, 'checking',100)
   call unit_test_end('c',msg='')
end subroutine test_c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_juown1()

   call unit_test_start('juown1',msg='')
   !!call unit_test('juown1', 0.eq.0, 'checking',100)
   call unit_test_end('juown1',msg='')
end subroutine test_juown1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dnum0()
doubleprecision :: y, z
   Y=DNUM0('CI = 10 * sin(3.1416/4)')
   Z=DNUM0('CI')
   call unit_test_start('dnum0',msg='')
   call unit_test('dnum0', y.eq.z.and.almost(y,10*sin(3.1416d0/4d0),15),&
           & 'checking CI',dnum0('CI')+bug,dnum0('10*sin(3.1416/4)')+bug,10*sin(3.1416d0/4.0d0))


   call unit_test_end('dnum0',msg='')
end subroutine test_dnum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_inum0()
character(len=:),allocatable :: string
   call unit_test_start('inum0',msg='')

   string='10/3'
   call unit_test('inum0', inum0(string).eq.3, 'checking',string,'==>',inum0(string)+bug,'expected',3)

   string='(444/111+1)*10-5.0'
   call unit_test('inum0', inum0(string).eq.45, 'checking',string,'==>',inum0(string)+bug,'expected',45)

   string='-10'
   call unit_test('inum0', inum0(string).eq.-10, 'checking',string,'==>',inum0(string)+bug,'expected',-10)

   string='+10'
   call unit_test('inum0', inum0(string).eq.+10, 'checking',string,'==>',inum0(string)+bug,'expected',+10)

   call unit_test_end('inum0',msg='')
end subroutine test_inum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_funcs()
character(len=iclen_calc) ::  outlin0
doubleprecision :: outval
integer :: ierr, ilen
character(len=:),allocatable :: string
!   call expression('A=3.4**5    ',outval,outlin0,ierr,ilen)
!   write(*,*)'value of expression is ',outval
!   write(*,*)'string representation of value is ',trim(outlin0)
!   write(*,*)'error flag value is ',ierr
!   write(*,*)'length of expression is ',ilen
   call unit_test_start('funcs',msg='')

   string='+10'

   call expression('xstore(1,10,20,30)',outval,outlin0,ierr,ilen)
   call unit_test('xstore',inum0('x(2)').eq.20)

   call unit_test('max',  inum0('  max(-100,0,20,40)   ').eq.40)
   call unit_test('min',  inum0('  min(-100,0,20,40)   ').eq.-100)
   call unit_test('sqrt', inum0('  sqrt(25)            ').eq.5)
   call unit_test('log10',inum0('  log10(1000)         ').eq.3)
   call unit_test('hypot',inum0('  hypot(3,4)          ').eq.5)

   call unit_test_end('funcs',msg='')
end subroutine test_funcs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expression()
character(len=iclen_calc) ::  outlin0
doubleprecision :: outval
character(len=:),allocatable :: string
integer :: ierr, ilen

   call unit_test_start('expression',msg='')

   string='A=3.4**5    '
   call expression(string,outval,outlin0,ierr,ilen)
   call unit_test('expression', ierr==0, 'error flag should be zero',ierr)
   call unit_test('expression', ilen==8, 'trimmed expression length should be eight',ilen)
   call unit_test_end('expression',msg='')

   string='$STR("The value is ",nint(40/4))'
   call expression(string,outval,outlin0,ierr,ilen)
   call unit_test('expression', ierr==2, 'string returned',trim(outlin0)//'')

   string='# this is a comment'
   !call unit_test('expression', ierr==1, 'expression is a comment',ierr)

end subroutine test_expression
!
!   string='A=sin(3.1416/5)'
!   !  -1 if an error occurred
!   !  0 if a numeric value is returned (value is in OUTVAL, string representation of the value is in OUTLIN2).
!   !  1 if no value was returned but a message was displayed (If a 'dump' or 'funcs' command was passed to the calculator).
!   !  2 if the expression evaluated to a string value instead of a numeric value (value is in OUTLIN0).
!   write(*,*)'value of expression is ',outval
!   write(*,*)'string representation of value is ',trim(outlin0)
!   
!   ! value of expression is    454.35424000000000
!   ! string representation of value is 454.35424
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rnum0()
character(len=:),allocatable :: string

   call unit_test_start('rnum0',msg='')
   string='10/2'
   call unit_test('rnum0', rnum0(string).eq.5, 'checking',string,'==>',rnum0(string)+bug,'expected',5)
   call unit_test_end('rnum0',msg='')
end subroutine test_rnum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_snum0()
character(len=:),allocatable :: string
   call unit_test_start('snum0',msg='')
   string='$str(10/2)'
   call unit_test('snum0', snum0(string).eq.'5', 'checking',string,'==>',snum0(string)//'','expected','5')
   call unit_test_end('snum0',msg='')
end subroutine test_snum0
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end program test_suite_M_calculator
!   The programmer (generally) uses just the CALCULATOR(3f) routine or several
!   convenience routines (INUM0,RNUM0,SNUM0,STRGAR2,EXPRESSION) that simplify
!   making the most common type of calls to CALCULATOR(3f).
! 
!    PROCEDURES
!   •  [1]calculator The procedure CALCULATOR(3f) acts like a calculator
! 
!   •  [2]getvalue given numeric variable name return double precision value
!      directly
! 
!   •  [3]igetvalue given numeric variable name return integer value directly
! 
!   •  [4]rgetvalue given numeric variable name return real value directly
! 
!   •  [5]stuff pass INTEGER|REAL|DOUBLEPRECISION value directly to calculator
!      dictionary
! 
!   •  [6]stuffa directly store a string into calculator variable name table
! 
!   •
! 
!    CONSTANTS
!   The variables used to hold the X,Y,$X $Y, ... arrays and the dictionaries of
!   variable names and string names and their associated values:
! 
!             + integer,parameter,public :: iclen_calc=512 ! max length of
!               expression or variable value as a string
!             + integer,parameter,public :: ixy_calc=55555 ! number of
!               variables in X() and Y() array
!             + integer,parameter,public :: icname_calc=20 ! max length of a
!               variable name
!             + real(kind=dp),save,public :: x(ixy_calc)=0.0_dp ! x array for
!               procedure funcs_
!             + real(kind=dp),save,public :: y(ixy_calc)=0.0_dp ! y array for
!               procedure funcs_
!             + integer,save,public,allocatable :: values_len(:) ! lengths of
!               the string variable values
!             + character(len=:),save,public,allocatable :: values(:) ! string
!               variable values
! 
!    LINKS
! 
!   •  The hierarchy of operations is the same as that of FORTRAN except that
!      adjacent exponents are done from left to right, not right to left [i.e.
!      in FORTRAN 3**2**4=3**(2**4), e.g. 3**2**4=(3**2)**4]; and +- strings are
!      resolved to a single sign (that is, 3+ -4 is acceptable instead of
!      3+(-4)).
!         Therefore, it is generally good practice to assume unquoted white-
!         space ends an expression.
! 
!      •        All numeric values are treated as FORTRAN type REAL variables.
! 
!      •        Input lines should not normally be over 255 characters long,
! 
!      •        There are three ways to store results to be used in future
!         calculations:
! 
!         1.  Variable names
! 
!         2.  The current-value
! 
!         3.  The x and y arrays Each of the types will be discussed separately.
! 
!    VARIABLE NAMES
!   Names must be 1 to 20 characters long, and are case-sensitive. The numbr of
!   names permitted is only limited by the available memory. Numeric variable
!   names should be composed of the letters a-z and underscores and numbers.
!   String variables are similar but start with a dollar sign($). Names must not
!   end in a "digit-E" combination. For example:
! 
!     A=sin(3.1416/2) big=200.333E200 $name="Thomas Jefferson"
! 
!                     Variables may be defined by equating them to an
!                     expression. To define or redefine a variable called FRED,
!                     simply enter:
! 
!     > FRED=300*4/500
! 
!                     The last value assigned to a variable will be used to
!                     evaluate the expression on the left of the equals sign
!                     when this expression redefines the variable. For example:
! 
!     > A=2 2 > A 2 > A=A+A 4 > A=A+A 8
! 
!                     To allow FORTRAN-type E-format numeric entry and yet not
!                     cause the calculator routine to do an excessive amount of
!                     checking, a variable name ending in the letter E must not
!                     have a digit (012345789) in front of that ending E.
!                     Attempting to define such a variable name will produce an
!                     error. This limitation prevents the calculator from
!                     becoming confused by whether 12E+3 is a variable called
!                     12E plus 3 or the exponential number 12E3=12000.
! 
!    CURRENT VALUE
!   The variable name '?' is automatically set by the program to contain the
!   last calculated value. This current-value register may be used like any
!   variable or number. It is 0 at program initialization. Example:
! 
!     > (300+500) 800 > (1/4)*?  200 > ?+?  400
! 
!    THE X AND Y ARRAYS
!   Two arrays called X and Y are available that can contain up to 55555 values
!   each. The arrays are originally initialized to all zeros. To set values in
!   the arrays, use the xstore (or ystore) command. The format of the commands
!   is
! 
!   xstore(start,ex1,ex2,ex3) ystore(start,ex1,ex2,ex3)
! 
!                   where start=array address to start storing at and ex(i)
!                   are expressions.
!                   The current value is assigned the last value stored.
!                   In addition there are similar string arrays and functions
!                   that can hold up to 50 255-character strings:
! 
!     •  $nstore(), $xstore(), $ystore()
! 
!     •  $n() ,$x() ,$y()
! 
!                     For example, to store into the locations 10,11,and 12 the
!                     values 1/10,2/10, and 3/10, the following could be
!                     entered:
! 
!     xstore( 10 , 1/10 , 2/20 , 3/10 ) ^ !  *-------Start storing evaluated
!     expressions sequentially, beginning at x(10).
! 
!    REFERENCING AN ARRAY VALUE
!   The values stored into the arrays may be referenced by subscript. For
!   example:
! 
!     > xstore(1,10,20,30) 30 > fred=x(1)+x(2)+x(3) 60
! 
!                     NOTES:
! 
!       1.  x and y array values cannot be used on the left of equal signs.
! 
!     x(10)=5
!       #  IS ILLEGAL
! 
!       2.  The current value is set to the value of the last expression by the
!           xstore and ystore commands
! 
!    INTRINSICS
!   supported Fortran intrinsics
! 
!                   The majority of intrinsic Fortran numeric functions are
!                   available. At a minimum the following are recognized
!                   (Deviations of the calculator routines from the standard
!                   intrinsics are noted):
! 
!   Arc or anti-trigonometric functions
! 
!                       # ACOS(arg) is a generic function that returns the
!                         arccosine of its argument in radians. The result of
!                         ACOS(real-arg) is real.
!                       # ASIN(arg) is a generic function that returns the
!                         arcsine of its argument in radians. The result of
!                         ASIN(real-arg) is real.
!                       # ATAN(arg) is a generic function that returns the
!                         arctangent of its argument in radians. The result of
!                         ATAN(real-arg) is real.
!                       # ATAN2(arg1, arg2) is a generic function that returns
!                         the arctangent of its argument in radians. The
!                         result of ATAN2(real-arg1, real-arg2) is real. The
!                         arguments must not both be 0.
!                       # ACOSH(arg) is a generic function that returns the
!                         hyperbolic arccosine of its argument in radians. The
!                         result of ACOSH(real-arg) is real.
!                       # ASINH(arg) is a generic function that returns the
!                         hyperbolic arcsine of its argument in radians. The
!                         result of ASIN(real-arg) is real.
!                       # ATANH(arg) is a generic function that returns the
!                         hyperbolic arctangent of its argument in radians.
!                         The result of ATANH(real-arg) is real.
!                       # ACOSD(arg) calculate arccosine of value in degrees
!                       # ASIND(arg) calculate arcsine of value in degrees
!                       # ATAND(arg) calculate arctangent of value in degrees
! 
!   Trigonometric functions
! 
!                       # COS(arg) is a generic function that returns the
!                         cosine of its argument in radians. The result of
!                         COS(real-arg) is real.
!                       # SIN(arg) is a generic function that returns the sine
!                         of its argument in radians. The result of
!                         SIN(real-arg) is real.
!                       # TAN(arg) is a generic function that returns the
!                         tangent of its argument in radians.
!                       # COSD(arg) calculate cosine of value in degrees
!                       # SIND(arg) calculate sine of value in degrees
!                       # TAND(arg) calculate tangent of value in degrees
! 
!   Hyperbolic trigonometric functions
! 
!                       # COSH(arg) is a generic function that returns the
!                         hyperbolic cosine of its argument. The result of
!                         COSH(real-arg) is real.
!                       # SINH(arg) is a generic function that returns the
!                         hyperbolic sine of its argument in radians. The
!                         result of SINH(real-arg) is real.
!                       # TANH(arg) is a generic function that returns the
!                         hyperbolic tangent of its argument in radians.
! 
!   Powers and logarithms
! 
!                       # EXP(arg) is a generic function that returns the
!                         exponential of its argument. The result of EXP(real-arg) is real.
!                       # LOG(arg) is a generic function that returns the
!                         natural logarithm (base e) of its argument. The
!                         result of LOG(real-arg) is real.
! 
!   Directly effecting sign of value
! 
!                       # ABS(arg) is a generic function that returns the
!                         absolute value of its argument. The result of
!                         ABS(real-arg) is real.
!                       # SIGN(arg1, arg2) is a generic function that returns
!                         a value after a sign transfer. The result of
!                         SIGN(real-arg1, real-arg2) is real. The result is
!                         |arg1| if arg2 >= 0. The result is -|arg1| if arg2 <
!                         0.
! 
!   Converting to a whole number
! 
!                       # INT(arg) is a generic function that converts its
!                         argument to integer type. The result of
!                         INT(real-arg) is zero if |real-arg| < 1. The result
!                         is the largest integer with the same sign as
!                         real-arg that does not exceed the magnitude of
!                         real-arg if |real-arg| >= 1.
!                       # AINT(arg) is a generic function that returns a whole
!                         number after truncation of its argument. The result
!                         of AINT(real-arg) is real. The result is 0 if |arg|
!                         < 1. The result is the largest integer with the same
!                         sign as arg that does not exceed the magnitude of
!                         arg if |arg| >= 1.
!                       # ANINT(arg) is a generic function that returns the
!                         nearest whole number of its argument. The result of
!                         ANINT(real-arg) is real.
!                       # NINT(arg) is a generic function that returns the
!                         integer that is nearest to its argument. The result
!                         of NINT(real-arg) is integer. If arg >= 0, the
!                         result is (INT(arg+.5)). If arg < 0, the result is
!                         (INT(arg-.5)).
! 
!   Bessel functions
! 
!                       # BESSEL_J0(X) - Bessel function of the first kind and
!                         order zero.
!                       # BESSEL_J1(X) - Bessel function of the first kind and
!                         order one.
!                       # BESSEL_Y0(X) - Bessel function of the second kind
!                         and order zero.
!                       # BESSEL_Y1(X) - Bessel function of the second kind
!                         and order one.
!                       # BESSEL_JN(N,X) - Bessel function of the first kind
!                         and order N.
!                       # BESSEL_YN(N,X) - Bessel function of the second kind
!                         and order N.
!                       # BESSEL_JN(N1,N2,X) - Bessel function of the first
!                         kind and order N.
!                       # BESSEL_YN(N1,N2,X) - Bessel function of the second
!                         kind and order N.
! 
!   system functions
! 
!                       # $GETENV(NAME),$GE(NAME) - get environment variable
!                         value
!                       # IFDEF(NAME) - detect if environment variable is set
!                       # $SH(COMMAND) - return output of system command
! 
!   Miscellaneous Fortran Intrinsics
! 
!                       # FRACTION - Fractional part of the model
!                         representation
!                       # EXPONENT - Exponent function
!                       # GAMMA - Gamma function
!                       # LOG_GAMMA - Logarithm of the Gamma function
!                       # MODULO - Modulo function
!                       # SCALE - Scale a real value
!                       # BTEST - Bit test function
!                       # TINY - Smallest positive number of a real kind
!                       # EPSILON - Epsilon function
!                       # HUGE - Largest number of a kind
!                       # ERFC_SCALED(X) - Exponentially-scaled complementary
!                         error function.
! 
!    _____________________________________________________
! ADDITIONAL PROCEDURES
!   In addition to standard Fortran intrinsics, many other functions are
!   supported ...
! 
!   conversion functions
! 
!     •  r2d(arg) - converts from radians to degrees
! 
!     •  d2r(arg) - converts from degrees to radians
! 
!     •  f2c() - convert Fahrenheit to Celsius
! 
!     •  c2f() - convert Celsius to Fahrenheit
! 
!   logical functions
! 
!     •  ge(val1,val2) - return TRUE if VAL1 is greater than or equal to VAL2,
!        else return FALSE
! 
!     •  gt(val1,val2) - return TRUE if VAL1 is greater than to VAL2, else
!        return FALSE
! 
!     •  eq(val1,val2) - return TRUE if VAL1 is equal to VAL2, else return FALSE
! 
!     •  le(val1,val2) - return TRUE if VAL1 is less than or equal to VAL2, else
!        return FALSE
! 
!     •  lt(val1,val2) - return TRUE if VAL1 is less than VAL2, else return
!        FALSE
! 
!     •  ne(val1,val2) - return TRUE if VAL1 is not equal to VAL2, else return
!        FALSE
! 
!     •  if(expression,val1,val2) - If expression is TRUE, return VAL1 else
!        return VAL2
! 
!        For example:
! 
!     a=if(ge(b,c),a,d)
! 
!                     means return a if b is greater than or equal to c else
!                     return d.
! 
!     lexical logical functions
! 
!       •  lge($str1,$str2) - return TRUE if $STR1 is lexically greater than or
!          equal to $STR2, else return FALSE
! 
!       •  lgt($str1,$str2) - return TRUE if $STR1 is lexically greater than to
!          $STR2, else return FALSE
! 
!       •  leq($str1,$strN) - return TRUE if $STR1 is lexically equal to any of
!          the other strings, else return FALSE
! 
!       •  lle($str1,$str2) - return TRUE if $STR1 is lexically less than or
!          equal to $STR2, else return FALSE
! 
!       •  llt($str1,$str2) - return TRUE if $STR1 is lexically less than $STR2,
!          else return FALSE
! 
!       •  lne($str1,$strN) - return TRUE if $STR1 is not equal to all following
!          strings.
! 
!       •  $if(expression,$str1,$str2) - If expression is TRUE, return $STR1
!          else return $STR2
! 
!   miscellaneous functions
! 
!     •  ownmode() - ownmode(3f) enables calls to user-supplied functions via
!        set_mysub(3f) and set_myfunc(3f).
! 
!     •  c(val1) - user-supplied function
! 
!     •  ceiling(val1) - ceiling(3f)or ceil(3f) returns the least integral value
!        greater than or equal to VAL1.
! 
!     •  floor(val1) - floor(3f) returns the greatest integral value less than
!        or equal to VAL1.
! 
!     •  in(val1,val2,val3) - returns TRUE if VAL1 is between VAL2 and VAL3 else
!        returns FALSE
! 
!     •  round(val1,val2) - round VAL1 to VAL2 significant digits.  Warning:
!        this function is not ready yet.
! 
!     •  same(x1,x2,idigits) - test if X1 and X2 are the same out to IDIGITS
!        digits. 0=TRUE.
! 
!     •  ifdef(variable_name) - given name of a variable as a string return 0 if
!        it exists, else -1 if it does not
! 
!   String-related
! 
!     •  $change($str1,"c/old/new") - substring substitution
! 
!     •  $char(v1,v2,....) - return characters indicated by numeric ADE (ASCII
!        decimal equivalent) values passed.
! 
!     •  delimx(istore,$str1,$delimiters) - parse string into tokens in array
!        $x()
! 
!     •  $f(fortran_format,value) - create string from value using specified
!        Fortran FORMAT statement
! 
!     •  $repeat(string,count) - repeat string COUNT times
! 
!     •  ichar($char) - return the ADE (ASCII Decimal Equivalent) value of a
!        letter
! 
!     •  index($str1,$str2) - return column number where $str2 begins in $str1
!        or zero(0).
! 
!     •  $l($str1) - convert string to lowercase
! 
!     •  len($str1) - return the length of the string
! 
!     •  $matchw($string,$pattern) - simple wild-card match of a string
! 
!     •  $modif($str1,$directive) - modify a string
! 
!     •  $(ex,ex,ex,...) or $str(ex,ex,ex,...) - generate a string from a series
!        of strings and numbers. The expressions may be numeric or string.
! 
!     •  str(ex,ex,ex,...) - same as $str() but convert resulting string to a
!        number IF the string is a simple numeric value
! 
!     •  $substr(string,i,j) - return a string that is columns i thru j of the
!        input string (first character is called column 1).
! 
!     •  $u($str1) - convert string to uppercase
! 
!   calendar(Time-related)
! 
!     •  ye(),year() - return current year
! 
!     •  mo(),month() - return current month
! 
!     •  da(),day() - return current day of month
! 
!     •  ho(),hour() - return current hour (0 -23)
! 
!     •  mi(),minute() - return current minute
! 
!     •  se(),second() - return current second
! 
!     •  dw() - days since Sunday (0 - 6) for current date
! 
!     •  ju() - days since January 1st (0 - 365) for current date
! 
!     •  $dw([0-7]) - day of week as a string
! 
!     •  $mo([1-12]) - month as a string
! 
!   Random numbers
! 
!     •  rand([itype]) - return random number from 0.0 to 1.0
! 
!     •  srand(number[,itype]) - set seed for rand(). Seeds should be whole
!        numbers
! 
!        If ITYPE is present different algorithms are used. The system routines
!        vary from platform to platform. The preferred algorithm is the
!        "Numerical Recipes" algorithm.  The ITYPE must agree between the
!        SRAND() call and the RAND() call. A call to SRAND() should precede use
!        of RAND().
! 
!   ITYPE meanings
! 
!     1.  The system C routine rand()
! 
!     2.  The Fortran intrinsic RANDOM_NUMBER()
! 
!    MISCELLANEOUS COMMANDS
!   Displaying variable values: dump The current value and all defined variable
!   names are displayed via the dump command.
! 
!   Listing Available Functions: funcs A display of all available functions can
!   be obtained when executing CALCULATOR(3f) by entering the command 'funcs'.
!   No descriptions are provided.
! 
! ADDING FUNCTIONS
!   Any program that calls CALCULATOR(3f) directly or indirectly (via
!   EXPRESSION(3f), STRGAR2(), INUM0(), RNUM0(), SNUM0()) can extend the
!   functions available by supplying two routines:
! 
!   1.  SUBSTITUTE_SUBROUTINE(3f) - This user-supplied routine is a hook for
!       programmers to add their own functions to CALCULATOR(3f) without having
!       to change CALCULATOR(3f) directly. It is passed the name of unknown
!       functions and their parameter lists if the expression 'ownmode(1)' is
!       passed to the calculator. If you do not need to add custom functions to
!       the calculator this is not required. A user-defined function call be
!       created and called with call set_mysub(SUBROUTINE_NAME) The routine must
!       be defined with an explicit interface available in the calling unit.
! 
!       2.  SUBSTITUTE_C(3f) - This user-supplied function is here to optimize
!           performance of a particular program and everyone else should
!           typically ignore it. In a special case a non-standard function
!           needed added that was called so heavily that it was important that
!           it be called more efficiently than a user defined function placed in
!           SUBSTITUTE_SUBROUTINE(3f) is. It allows for the function "c" to be
!           defined and given an array and an array size as arguments. By
!           default the "c" function just returns zero. A replacement can be
!           defined by creating a function with similar arguments and calling
!           call set_myfunc(FUNCTION_NAME). The routine must be defined with an
!           explicit interface available in the calling unit.
! 
!   The following program shows a simple but complete line-mode calculator
!   program.
! 
!     ./compute
!       # run example program a=10 a/2 3**4 sin(3.1416/4) PI=2*asin(1)
!       diameter=20.3+8/4 circumference=PI*diameter funcs dump # use end-of-file
!       (typically control-D) to exit program ctrl-D
! 

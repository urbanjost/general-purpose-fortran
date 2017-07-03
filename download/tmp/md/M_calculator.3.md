[CLI Home Page]

NAME
    M_calculator(3fm) - [M_calculator]module of routines for parsing expressions and returning values
SYNOPSIS

        use M_calculator, only : jucalc, getvalue, igetvalue, rgetvalue, stuff, stuffa
        use M_calculator, only : iclen_calc, ixy_calc, icname_calc, x, y, valuer, values

DESCRIPTION

    The M_calculator module and related functions evaluate CHARACTER strings containing FORTRAN-like expressions and returns
    numeric and string values.

    Using this interface it is easy to make free-format and order-independent input interfaces where values can be expressions and
    variable names instead of simple formatted numbers.

    The primary routine JUCALC acts like a powerful desk-top calculator. It supports named variables and has several arrays (of
    55555 elements each). Many standard FORTRAN functions are available, plus access to user-written functions is permitted via the
    user-supplied routine JUOWN1().

    The programmer (generally) uses just the JUCALC routine or several convenience routines (INUM0,RNUM0,SNUM0,STRGAR2,JUCALCX)
    that simplify making the most common type of calls to JUCALC.

PROCEDURES
      + jucalc The procedure JUCALC acts like a calculator
      + getvalue given numeric variable name return double precision value directly
      + igetvalue given numeric variable name return integer value directly
      + rgetvalue given numeric variable name return real value directly
      + stuff pass INTEGER|REAL|DOUBLEPRECISION value directly to calculator dictionary
      + stuffa directly store a string into calculator variable name table
CONSTANTS

       The variables used to hold the X,Y,$X
       $Y, ... arrays and the dictionaries of variable names and
       string names and their associated values:




          o  integer,parameter,public  :: iclen_calc=512     ! max length of expression or variable value as a string

          o  integer,parameter,public  :: ixy_calc=55555     ! number of variables in X() and Y() array

          o  integer,parameter,public  :: icname_calc=20     ! max length of a variable name

          o  real(kind=dp),save,public :: x(ixy_calc)=0.0_dp ! x array for procedure jufuns

          o  real(kind=dp),save,public :: y(ixy_calc)=0.0_dp ! y array for procedure jufuns

          o  integer,save,public       :: valuer(ic_calc)=0  ! lengths of the string variable values

          o  character(len=iclen_calc),save,public  :: values(ic_calc)=' '         ! string variable values



LINKS
      + User Guide for programs that call the calculator
      + Example Program
      + Programmer Notes
SEE ALSO
      + module M_noown
      + module M_calculator_plus
USAGE

                                                        Calculator Expressions

    SYNOPSIS
        The calculator interface allows input values to be numeric or string expressions using Fortran-like syntax instead of just
        simple variables.

        Named variables may be created. Several arrays of 55555 elements each exist. The majority of FORTRAN intrinsic functions
        are available, Custom routines may be made available for each application using the interface.
    DESCRIPTION
        A summary of the syntax rules for the expressions follows:
          o The hierarchy of operations is the same as that of FORTRAN except that adjacent exponents are done from left to right,
            not right to left [i.e. in FORTRAN 3**2**4=3**(2**4), e.g. 3**2**4=(3**2)**4]; and +- strings are resolved to a single
            sign (that is, 3+ -4 is acceptable instead of 3+(-4)).
          o Almost all the INTRINSIC mathematical functions defined in FORTRAN are available, as well as access to common
            extensions and user-written routines.
          o Embedded blanks are ignored during the processing of a calculation, but most applications using the calculator
            interface parse on spaces. Therefore, it is generally good practice to assume unquoted white-space ends an expression.
          o All numeric values are treated as FORTRAN type REAL variables.
          o Input lines should not normally be over 255 characters long,
          o There are three ways to store results to be used in future calculations:
             1. Variable names
             2. The current-value
             3. The x and y arrays
            Each of the types will be discussed separately.
    VARIABLE NAMES
        Names must be 1 to 20 characters long, and are case-sensitive. Up to 2345 names are permitted. Numeric variable names
        should be composed of the letters a-z and underscores and numbers. String variables are similar but start with a dollar
        sign($). Names must not end in a "digit-E" combination. For example:

              A=sin(3.1416/2)
              big=200.333E200
              $name="Thomas Jefferson"


        Variables may be defined by equating them to an expression. To define or redefine a variable called FRED, simply enter:

                > FRED=300*4/500


        The last value assigned to a variable will be used to evaluate the expression on the left of the equals sign when this
        expression redefines the variable. For example:

                > A=2
                  2
                > A
                  2
                > A=A+A
                  4
                > A=A+A
                  8


        To allow FORTRAN-type E-format numeric entry and yet not cause the calculator routine to do an excessive amount of
        checking, a variable name ending in the letter E must not have a digit (012345789) in front of that ending E. Attempting to
        define such a variable name will produce an error. This limitation prevents the calculator from becoming confused by
        whether 12E+3 is a variable called 12E plus 3 or the exponential number 12E3=12000.
    CURRENT VALUE
        The variable name '?' is automatically set by the program to contain the last calculated value. This current-value register
        may be used like any variable or number. It is 0 at program initialization. Example:

                  > (300+500)
                    800
                  > (1/4)*?
                    200
                  > ?+?
                    400


    THE X AND Y ARRAYS
        Two arrays called X and Y are available that can contain up to 55555 values each. The arrays are originally initialized to
        all zeros. To set values in the arrays, use the xstore (or ystore) command. The format of the commands is

            xstore(start,ex1,ex2,ex3)
            ystore(start,ex1,ex2,ex3)

        where start=array address to start storing at and ex(i) are expressions.
        The current value is assigned the last value stored.
        In addition there are similar string arrays and functions that can hold up to 50 255-character strings:
          o $nstore(), $xstore(), $ystore()
          o $n() ,$x() ,$y()
        For example, to store into the locations 10,11,and 12 the values 1/10,2/10, and 3/10, the following could be entered:


                xstore( 10 , 1/10 , 2/20 , 3/10 )
                        ^
                        !
                        *-------Start storing evaluated expressions sequentially,
                                beginning at x(10).


    REFERENCING AN ARRAY VALUE
        The values stored into the arrays may be referenced by subscript. For example:

              > xstore(1,10,20,30)
                30
              > fred=x(1)+x(2)+x(3)
                60


        NOTES:
         1. x and y array values cannot be used on the left of equal signs.

                     x(10)=5   #  IS ILLEGAL 


         2. The current value is set to the value of the last expression by the xstore and ystore commands
    INTRINSICS

        supported Fortran intrinsics

        The majority of intrinisic Fortran numeric functions are available. At a minimum the following are recognized (Deviations
        of the calculator routines from the standard intrinsics are noted):

        Arc or anti-trigonometric functions


              # ACOS(arg) is a generic function that returns the arccosine of its argument in radians. The result of ACOS(real-arg)
                is real.
              # ASIN(arg) is a generic function that returns the arcsine of its argument in radians. The result of ASIN(real-arg)
                is real.
              # ATAN(arg) is a generic function that returns the arctangent of its argument in radians. The result of ATAN
                (real-arg) is real.
              # ATAN2(arg1, arg2) is a generic function that returns the arctangent of its argument in radians. The result of ATAN2
                (real-arg1, real-arg2) is real. The arguments must not both be 0.
              # ACOSH(arg) is a generic function that returns the hyperbolic arccosine of its argument in radians. The result of
                ACOSH(real-arg) is real.
              # ASINH(arg) is a generic function that returns the hyperbolic arcsine of its argument in radians. The result of ASIN
                (real-arg) is real.
              # ATANH(arg) is a generic function that returns the hyperbolic arctangent of its argument in radians. The result of
                ATANH(real-arg) is real.
        Trigonometric functions


              # COS(arg) is a generic function that returns the cosine of its argument in radians. The result of COS(real-arg) is
                real.
              # SIN(arg) is a generic function that returns the sine of its argument in radians. The result of SIN(real-arg) is
                real.
              # TAN(arg) is a generic function that returns the tangent of its argument in radians.
        Hyperbolic trigonometric functions


              # COSH(arg) is a generic function that returns the hyperbolic cosine of its argument. The result of COSH(real-arg) is
                real.
              # SINH(arg) is a generic function that returns the hyperbolic sine of its argument in radians. The result of SINH
                (real-arg) is real.
              # TANH(arg) is a generic function that returns the hyperbolic tangent of its argument in radians.
        Powers and logarithms


              # EXP(arg) is a generic function that returns the exponential of its argument. The result of EXP(real-arg) is real.
              # LOG(arg) is a generic function that returns the natural logarithm (base e) of its argument. The result of LOG
                (real-arg) is real.
              # HYPOT(arg1,arg2) returns the Euclidean distance calculated at HYPOT(X,Y)==SQRT(X**2+Y**2)
              # LOG10(arg) is a generic function that returns the common logarithm (base 10) of its argument. The result of LOG10
                (real-arg) is real.
              # SQRT(arg) is a generic function that returns the principal square root of its argument. The result of SQRT
                (real-arg) is real.
        Maximum/Minimum


              # MAX(arg1, arg2 [,..., arg50]) is a generic function that returns the largest value in its argument list. The result
                of MAX(real-arg1, real-arg2 [,..., real-arg50]) is real. - (NON-STANDARD LIMIT: 50 instead of 500 parameters
                allowed)
              # MIN(arg1, arg2 [,..., arg50]) is a generic function that returns the smallest value in its argument list. The
                result of MIN(real-arg1, real-arg2 [,..., real-arg50]) is real. NON-STANDARD LIMIT: 50 instead of 500 parameters
                allowed)
        Directly effecting sign of value


              # ABS(arg) is a generic function that returns the absolute value of its argument. The result of ABS(real-arg) is
                real.
              # SIGN(arg1, arg2) is a generic function that returns a value after a sign transfer. The result of SIGN(real-arg1,
                real-arg2) is real. The result is |arg1| if arg2 >= 0. The result is -|arg1| if arg2 < 0.
        Converting to a whole number


              # INT(arg) is a generic function that converts its argument to integer type. The result of INT(real-arg) is zero if |
                real-arg| < 1. The result is the largest integer with the same sign as real-arg that does not exceed the magnitude
                of real-arg if |real-arg| >= 1.
              # AINT(arg) is a generic function that returns a whole number after truncation of its argument. The result of AINT
                (real-arg) is real. The result is 0 if |arg| < 1. The result is the largest integer with the same sign as arg that
                does not exceed the magnitude of arg if |arg| >= 1.
              # ANINT(arg) is a generic function that returns the nearest whole number of its argument. The result of ANINT
                (real-arg) is real.
              # NINT(arg) is a generic function that returns the integer that is nearest to its argument. The result of NINT
                (real-arg) is integer. If arg >= 0, the result is (INT(arg+.5)). If arg < 0, the result is (INT(arg-.5)).
        Bessel functions


              # BESSEL_J0(X) - Bessel function of the first kind and order zero.
              # BESSEL_J1(X) - Bessel function of the first kind and order one.
              # BESSEL_Y0(X) - Bessel function of the second kind and order zero.
              # BESSEL_Y1(X) - Bessel function of the second kind and order one.
              # BESSEL_JN(N,X) - Bessel function of the first kind and order N.
              # BESSEL_YN(N,X) - Bessel function of the second kind and order N.
              # BESSEL_JN(N1,N2,X) - Bessel function of the first kind and order N.
              # BESSEL_YN(N1,N2,X) - Bessel function of the second kind and order N.
        Miscellaneous


              # DIM(arg1, arg2) is a generic function that returns the positive difference of its arguments. The result of DIM
                (real-arg1, real-arg2) is real. The result is arg1-arg2 if arg1 > arg2, and the result is 0 if arg1 <= arg2.
              # MOD(arg1, arg2) is a generic function that returns the remainder of arg1 divided by arg2. The result of MOD
                (real-arg1, real-arg2) is real. The result is arg1 - (INT(arg1/arg2)*arg2). If arg2 = 0, the result is undefined.
                Arg1 and arg2 must not exceed 2**48-1.
              # REAL(arg) is a generic function that performs type conversion on its argument. The result of REAL(real-arg) is
                real.
        Error function


              # ERF(X) - Error function.
              # ERFC(X) - Complementary error function.
              # ERFC_SCALED(X) - Exponentially-scaled complementary error function.
        Error


              # ERF(X) - Error function.
              # ERFC(X) - Complementary error function.
              # ERFC_SCALED(X) - Exponentially-scaled complementary error function.

        ---------------------------------------------------------------------------------------------------------------------------
       
        ADDITIONAL PROCEDURES

        In addition to standard Fortran intrinsics, many other functions are supported ...

        conversion functions

          o r2d(arg) - converts from radians to degrees
          o d2r(arg) - converts from degrees to radians
          o f2c() - convert Fahrenheit to Celsius
          o c2f() - convert Celsius to Fahrenheit

        logical functions

          o ge(val1,val2) - return TRUE if VAL1 is greater than or equal to VAL2, else return FALSE
          o gt(val1,val2) - return TRUE if VAL1 is greater than to VAL2, else return FALSE
          o eq(val1,val2) - return TRUE if VAL1 is equal to VAL2, else return FALSE
          o le(val1,val2) - return TRUE if VAL1 is less than or equal to VAL2, else return FALSE
          o lt(val1,val2) - return TRUE if VAL1 is less than VAL2, else return FALSE
          o ne(val1,val2) - return TRUE if VAL1 is not equal to VAL2, else return FALSE
          o if(expression,val1,val2) - If expression is TRUE, return VAL1 else return VAL2
        For example:

             a=if(ge(b,c),a,d)


        means return a if b is greater than or equal to c else return d.

        lexical logical functions

          o lge($str1,$str2) - return TRUE if $STR1 is lexically greater than or equal to $STR2, else return FALSE
          o lgt($str1,$str2) - return TRUE if $STR1 is lexically greater than to $STR2, else return FALSE
          o leq($str1,$strN) - return TRUE if $STR1 is lexically equal to any of the other strings, else return FALSE
          o lle($str1,$str2) - return TRUE if $STR1 is lexically less than or equal to $STR2, else return FALSE
          o llt($str1,$str2) - return TRUE if $STR1 is lexically less than $STR2, else return FALSE
          o lne($str1,$strN) - return TRUE if $STR1 is not equal to all following strings.
          o $if(expression,$str1,$str2) - If expression is TRUE, return $STR1 else return $STR2

        miscellaneous functions

          o ownmode() - ownmode(3f) enables calls to user-supplied functions via JUOWN1() and C() routines.
          o c(val1) - user-supplied function
          o ceiling(val1) - ceiling(3f)or ceil(3f) returns the least integral value greater than or equal to VAL1.
          o floor(val1) - floor(3f) returns the greatest integral value less than or equal to VAL1.
          o in(val1,val2,val3) - returns TRUE if VAL1 is between VAL2 and VAL3 else returns FALSE
          o round(val1,val2) - round VAL1 to VAL2 significant digits. Warning: this function is not ready yet.
          o same(x1,x2,idigits) - test if X1 and X2 are the same out to IDIGITS digits. 0=TRUE.
          o ifdef(variable_name) - given name of a variable as a string return 0 if it exists, else -1 if it does not

        String-related

          o $change($str1,"c/old/new") - substring substitution
          o $char(v1,v2,....) - return characters indicated by numeric ADE (ASCII decimal equivalent) values passed.
          o delimx(istore,$str1,$delimiters) - parse string into tokens in array $x()
          o $f(fortran_format,value) - create string from value using specified Fortran FORMAT statement
          o ichar($char) - return the ADE (ASCII Decimal Equivalent) value of a letter
          o index($str1,$str2) - return column number where $str2 begins in $str1 or zero(0).
          o $l($str1) - convert string to lowercase
          o len($str1) - return the length of the string
          o $matchw($string,$pattern) - simple wild-card match of a string
          o $modif($str1,$directive) - modify a string
          o $(ex,ex,ex,...) or $str(ex,ex,ex,...) - generate a string from a series of strings and numbers. The expressions may be
            numeric or string.
          o str(ex,ex,ex,...) - same as $str() but convert resulting string to a number IF the string is a simple numeric value
          o $substr(string,i,j) - return a string that is columns i thru j of the input string (first character is called column
            1).
          o $u($str1) - convert string to uppercase

        calendar(Time-related)

          o ye() - return current year
          o mo() - return current month
          o da() - return current day of month
          o ho() - return current hour (0 -23)
          o mi() - return current minute
          o se() - return current second
          o dw() - days since Sunday (0 - 6) for current date
          o ju() - days since January 1st (0 - 365) for current date
          o $dw([0-7]) - day of week as a string
          o $mo([1-12]) - month as a string
          o $now(format) - extensive formatting of current time
          o $fmtdate(dat(8),format) - extensive formatting of date and time. $fmtdate("help") shows the available macros.
          o unix_to_date() - store Unix Epoch Time to date and time values in x(1) to x(8)
          o date_to_unix(y,m,d,z,h,m,s,x) - convert date and time values to Unix Epoch Time

        The $now() and $fmtdate() functions can output a date in a multitude of formats and can be used to perform simple date
        manipulations.

        Random numbers

          o rand([itype]) - return random number from 0.0 to 1.0
          o srand(number[,itype]) - set seed for rand(). Seeds should be whole numbers

        If ITYPE is present different algorithms are used. The system routines vary from platform to platform. The preferred
        algorithm is the "Numerical Recipes" algorithm. The ITYPE must agree between the SRAND() call and the RAND() call. A call
        to SRAND() should precede use of RAND().

        ITYPE meanings

         1. The system C routine rand()
         2. The Fortran intrinsic RANDOM_NUMBER()
         3. The algorithm purchased from Numerical Recipes

                This routine (RAN_MOD()), is a modified version of routine RAN1() from the book "Numerical Recipes in FORTRAN"
                (Cambridge University Press), Copyright (C) 1986, 1992 by Numerical Recipes Software. Used by permission. Use of
                this routine other than as an integral part of BELOCA-related programs requires an additional license from
                Numerical Recipes Software. Further distribution in any form is prohibited.
                RAN_MOD() is a "MINIMUM" random number generator of Park and Miller with Bays-Durham shuffle and added safeguards.
                It returns uniform random deviates between 0.0 and 1.0 (exclusive of the endpoint values). Do not alter the seed
                between successive deviates in a sequence.

    MISCELLANEOUS COMMANDS

        Displaying variable values: dump


            The current value and all defined variable names are displayed via the dump command.
        Listing Available Functions: funcs


            A display of all available functions can be obtained when executing JUCALC by entering the command 'funcs'. No
            descriptions are provided.


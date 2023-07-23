










module M_framework__approx
use, intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64 !  1           2           4           8
use, intrinsic :: iso_fortran_env,  only : real32, real64, real128   !  4           8          10
use, intrinsic :: iso_fortran_env,  only : ERROR_UNIT,OUTPUT_UNIT    ! access computing environment
implicit none
private
! COMPARING AND ROUNDING FLOATING POINT VALUES
public  :: almost          ! function compares two numbers only up to a specified number of digits
public  :: accdig          ! compare two real numbers only up to a specified number of digits
public  :: in_margin       ! check if two reals are approximately equal using a relative margin
public  :: round_to_power  ! round val to specified number of digits after the decimal point
public  :: round           ! round val to specified number of significant digits
public  :: significant     ! round val to specified number of significant digits
!===========================
! deprecated
public  :: sp_accdig       ! compare two real numbers only up to a specified number of digits
public  :: dp_accdig       ! compare two double numbers or other kinds only up to a specified number of digits
interface dp_accdig        ! for backward compatibility, accdig(3f) preferred
   module procedure accdig
end interface dp_accdig
!===========================

interface significant
   module procedure significant_real32
   module procedure significant_real64
end interface significant

private :: anyscalar_to_realbig_
private :: anyscalar_to_double_
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    almost(3f) - [M_framework__approx] return true or false if two numbers
!!    agree up to specified number of digits
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    elemental impure function almost(x,y,digits,verbose)
!!
!!     class(*),intent(in)         :: x,y
!!     class(*),intent(in)         :: rdigits
!!     logical,intent(in),optional :: verbose
!!     logical                     :: almost
!!
!!##DESCRIPTION
!!    Returns true or false depending on whether the two numbers given agree
!!    to within the specified number of digits as calculated by ACCDIG(3f).
!!##OPTIONS
!!    x,y      expected and calculated values to be compared. May be of
!!             type REAL, INTEGER, or DOUBLEPRECISION.
!!    rdigits  number of digits of precision to compare. May be INTEGER or
!!             REAL.
!!    verbose  optional value that specifies to print the results of the
!!             comparison when set to .TRUE..
!!##RETURNS
!!    almost   TRUE if the input values compare up to the specified number
!!             of values
!!##EXAMPLE
!!
!!   sample:
!!
!!    program demo_almost
!!    use M_framework__approx, only : almost
!!    implicit none
!!    real    :: x, y
!!    logical :: z
!!    integer :: i
!!    x=1.2345678
!!    y=1.2300000
!!    do i=1,8
!!       z=almost(x,y,i,verbose=.true.)
!!       write(*,*)i,z
!!    enddo
!!    end program demo_almost
!!
!!   Results:
!!
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 1.00000000
!!     >            1 T
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 2.00000000
!!     >            2 T
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 3.00000000
!!     >            3 F
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 4.00000000
!!     >            4 F
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 5.00000000
!!     >            5 F
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 6.00000000
!!     >            6 F
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 7.00000000
!!     >            7 F
!!     > *sp_accdig* significant digit request too high= 8.00000000
!!     > *almost* for values 1.23456776 1.23000002 agreement of 2.43020344 ...
!!     > digits out of requested 8.00000000
!!     >            8 F
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
elemental impure function almost(x,y,digits,verbose)
use M_framework__journal,  only : journal

! ident_1="@(#) M_framework__approx almost(3f) function compares two real numbers up to specified number of digits by calling ACCDIG(3f)"

class(*),intent(in)         :: x,y
class(*),intent(in)         :: digits
logical,intent(in),optional :: verbose
logical                     :: almost

logical                     :: verbose_local
real                        :: acurcy
real                        :: digits_local
integer                     :: ind

   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif

   digits_local=anyscalar_to_realbig_(digits)
   acurcy=0.0
   select type(x)
   type is(real)
      select type(y)
      type is(real)
         call accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      class default
         call accdig(x,y,digits_local,acurcy,ind)
         if(verbose_local)then
            call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
         endif
      end select
   class default
      call accdig(x,y,digits,acurcy,ind)
      if(verbose_local)then
         call journal('sc','*almost*','for values',x,y,'agreement of',acurcy,'digits out of requested',digits_local)
      endif
   end select

   if(ind == 0)then
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
!!    sp_accdig(3f) - [M_framework__approx] compare two real numbers of
!!    default kind only up to a specified number of digits
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       subroutine sp_accdig(x,y,digio,acurcy,ind)
!!
!!        real,intent(in)     :: X
!!        real,intent(in)     :: Y
!!        real,intent(in)     :: DIGI0
!!        real,intent(out)    :: acurcy
!!        integer,intent(out) :: ind
!!
!!##DESCRIPTION
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call sp_accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
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
!!    For example, Take some numbers and compare them to 1.2345678 ...
!!
!!        > ================================================
!!        > A number     |    ACURCY       |   ACURCY
!!        >              |    1.2345678=Y  |   1.2345678=X
!!        > ================================================
!!        >  1.234680    |    3.7900571    |   3.7901275
!!        >  1.2345378   |    4.6144510    |   4.6144404
!!        >  2.2234568   |    0.096367393  |   0.35188114
!!        >  1.2345678   |    8.0000000    |   8.0000000
!!        >  1.2345679   |    7.0732967    |   7.0731968
!!        > -1.2345678   |   -0.30103000   |  -0.30103000
!!        > 76.234567    |   -1.7835463    |   0.0070906729
!!        >  2.4691356   |    0.0          |   0.3010300
!!        >  0.0         |    0.0          |  -0.91514942.
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
!!    program demo_sp_accdig ! fortran 90 example
!!    use M_framework__approx, only : sp_accdig
!!    implicit none
!!    integer :: digi
!!    integer :: i10, i20, i30
!!    integer :: ind, ind1, ind2
!!    real    :: acurcy, acurcy1, acurcy2
!!    real    :: a, b
!!    real    :: vals(9)
!!    data vals/ &
!!      &1.234680,   1.2345378,  2.2234568, 1.2345678, &
!!      &1.2345679, -1.2345678, 76.234567,  2.4691356, &
!!      &0.0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0
!!          b=a+1.0/(10.0**i10)
!!          call sp_accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0
!!          b=a+1.0/(10.0**i20)
!!          call sp_accdig(a,b,real(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call sp_accdig(1.2345678,vals(i30),8.0,acurcy1,ind1)
!!          call sp_accdig(vals(i30),1.2345678,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_sp_accdig
!!
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
!!    o M_framework__journal(),log10(), abs(1)
!!
!!##AUTHOR
!!    David Hogben, John S. Urban
!!
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
SUBROUTINE sp_accdig(X,Y,digi0,ACURCY,IND)
use M_framework__journal, only : journal
implicit none

! ident_2="@(#) M_framework__approx sp_accdig(3f) compare two real numbers only up to a specified number of digits"

!     INPUT ...
real,intent(in) :: x           ! First  of two real numbers to be compared.
real,intent(in) :: y           ! Second of two real numbers to be compared.
real,intent(in) :: digi0       ! Number of digits to be satisfied in relative tolerance.
!     OUTPUT ...
real,intent(out)    :: acurcy  ! = -LOG10(ABS((X-Y)/Y)))
integer,intent(out) :: ind     ! = 0, If tolerance is     satisfied.
! = 1, If tolerance is not satisfied.

real     :: diff
real     :: digi
integer,parameter  :: ireal_significant_digits = int(log10(2.**digits(0.0))) ! maximum number of significant digits in a real number.

   digi=digi0
   if(digi <= 0)then
      call journal('sc','*sp_accdig* bad number of significant digits=',digi)
      digi=ireal_significant_digits
   elseif(digi  >  ireal_significant_digits)then
      call journal('sc','*sp_accdig* significant digit request too high=',digi)
      digi=min(digi,real(ireal_significant_digits))
   endif

   diff = x - y
   if(diff  ==  0.0) then
      acurcy = ireal_significant_digits
   elseif(y  ==  0.0) then
      acurcy = -log10(abs(x))
   else
      acurcy = -log10(abs(diff)) + log10(abs(y))
   endif

   if(acurcy  <  digi ) then
      ind = 1
   else
      ind = 0
   endif

END SUBROUTINE sp_accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      accdig(3f) - [M_framework__approx] compare two numbers only up to
!!      a specified number of digits
!!      (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!       elemental impure subroutine accdig(x,y,digio,acurcy,ind)
!!
!!        class(*),intent(in)  :: X
!!        class(*),intent(in)  :: Y
!!        class(*),intent(in)  :: DIGI0
!!        real,intent(out)     :: acurcy
!!        integer,intent(out)  :: ind
!!
!!##DESCRIPTION
!!
!!    This procedure is used to check how closely two numbers agree.
!!
!!       call accdig(X,Y,DIGI0,ACURCY,IND)
!!
!!    The values X and Y are the numbers to compare, and DIGI0 is the
!!    threshold number of digits to consider significant in returning IND.
!!
!!    If X and Y are considered equal within DIGI0 relative tolerance,
!!
!!        IND    = 0, if tolerance is     satisfied.
!!               = 1, if tolerance is not satisfied.
!!
!!    The result ACURCY gives a measure of the number of leading digits in X
!!    which are the same as the number of leading digits in Y.
!!
!!         ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
!!         ACURCY=-log10(X-Y)       if X != Y and Y = 0
!!         ACURCY=8                 if X=Y
!!
!!         ACURCY is never less than -8 or greater than 8 for 32-bit REAL values
!!
!!    TOLERANCE ...
!!         X and Y are considered equal within DIGI0 relative tolerance,
!!         if ACURCY is greater than DIGI0.
!!
!!    For example, Take some numbers and compare them  to 1.2345678 ...
!!
!!       >  ================================================
!!       >  A number     |    ACURCY       |   ACURCY
!!       >               |    1.2345678=Y  |   1.2345678=X
!!       >  ================================================
!!       >   1.234680    |    3.7900571    |   3.7901275
!!       >   1.2345378   |    4.6144510    |   4.6144404
!!       >   2.2234568   |    0.096367393  |   0.35188114
!!       >   1.2345678   |    8.0000000    |   8.0000000
!!       >   1.2345679   |    7.0732967    |   7.0731968
!!       >  -1.2345678   |   -0.30103000   |  -0.30103000
!!       >  76.234567    |   -1.7835463    |   0.0070906729
!!       >   2.4691356   |    0.0          |   0.3010300
!!       >   0.0         |    0.0          |  -0.91514942.
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
!!    use M_framework__approx, only : accdig
!!    implicit none
!!    integer         :: digi
!!    doubleprecision :: a, b
!!    integer         :: i10, i20, i30
!!    integer         :: ind, ind1, ind2
!!    real            :: acurcy, acurcy1, acurcy2
!!    doubleprecision :: vals(9)
!!    data vals/ &
!!      &1.234680d0,   1.2345378d0,  2.2234568d0, 1.2345678d0, &
!!      &1.2345679d0, -1.2345678d0, 76.234567d0,  2.4691356d0, &
!!      &0.0d0/
!!       write(*,*)'========================='
!!       do i10=0,16
!!          a=1.0d0
!!          b=a+1.0d0/(10.0d0**i10)
!!          call accdig(a,b,8.0,acurcy,ind)
!!          write(*,*)i10,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       digi=16
!!       do i20=0,digi
!!          a=1.0d0
!!          b=a+1.0d0/(10.0d0**i20)
!!          call accdig(a,b,dble(digi),acurcy,ind)
!!          write(*,*)i20,a,b,acurcy,ind
!!       enddo
!!       write(*,*)'========================='
!!       do i30=1,9
!!          call accdig(1.2345678d0,vals(i30),8.0,acurcy1,ind1)
!!          call accdig(vals(i30),1.2345678d0,8.0,acurcy2,ind2)
!!          write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
!!       enddo
!!    end program demo_accdig
!!
!!##NOTES
!!##REFERENCES
!!
!!   based on ...
!!
!!    NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. dp_accdig V 7.00  2/14/90. **
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
!!         o M_framework__journal(), log10(), abs(1)
!!
!!##AUTHORS
!!      David Hogben, John S. Urban
!!
!!##LICENSE
!!      Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
elemental impure SUBROUTINE accdig(x,y,digi0,ACURCY,IND)
use,intrinsic :: iso_fortran_env, only : wp=>real128
use M_framework__journal,  only : journal
implicit none

! ident_3="@(#) M_framework__approx accdig(3f) compare two values only up to a specified number of digits"

!  INPUT ...
class(*),intent(in)  :: x           ! FIRST  OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: y           ! SECOND OF TWO NUMBERS TO BE COMPARED.
class(*),intent(in)  :: digi0       ! NUMBER OF DIGITS TO BE SATISFIED IN RELATIVE TOLERANCE.
real(kind=wp)        :: x_local
real(kind=wp)        :: y_local
!  OUTPUT ...
real,intent(out)     :: acurcy      ! = -LOG10(ABS((x_local-y_local)/y_local)))
integer,intent(out)  :: ind         ! = 0, IF TOLERANCE IS     SATISFIED.
                                    ! = 1, IF TOLERANCE IS NOT SATISFIED.
real(kind=wp)        :: diff
real(kind=wp)        :: digi
                        ! Maximum number of significant digits in a number of biggest real kind.
integer,parameter    :: idble_significant_digits = int(log10(2.0_wp**digits(0.0_wp)))

   x_local=anyscalar_to_realbig_(x)
   y_local=anyscalar_to_realbig_(y)
   digi=anyscalar_to_realbig_(digi0)

   if(digi <= 0)then
      call journal('sc','*accdig* bad number of significant digits=',real(digi,kind=wp))
      digi=idble_significant_digits
   elseif(digi  >  idble_significant_digits)then
      call journal('sc','*accdig* significant digit request too high=',real(digi,kind=wp))
      digi=min(digi,real(idble_significant_digits,kind=wp))
   endif
   diff = x_local - y_local
   if(diff  ==  0.0_wp) then
      acurcy = idble_significant_digits
   elseif(y_local  ==  0.0_wp) then
      acurcy = -log10(abs(x_local))
   else
      acurcy = -log10(abs(diff)) + log10(abs(y_local))
   endif
   if(acurcy  <  digi ) then
      ind = 1
   else
      ind = 0
   endif
end subroutine accdig
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!   in_margin(3f) - [M_framework__approx] check if two reals are
!!   approximately equal using a relative margin
!!
!!##SYNOPSIS
!!
!!     elemental pure function in_margin( expected_value, measured_value,
!!     allowed_margin )
!!
!!      real, intent(in)    :: expected_value
!!      real, intent(in)    :: measured_value
!!      real, intent(in)    :: allowed_margin
!!      class(*),intent(in) :: invalue
!!
!!##DESCRIPTION
!!   Compare two values to see if they are relatively equal using the
!!   specified allowed margin. That is, see if VALUE_MEASURED is in
!!   the range VALUE_EXPECTED +- ALLOWED_ERROR where the allowed error
!!   varies with the magnitude of the values, such that the allowed error
!!   is margin * average magnitude of measured and expected).
!!
!!   So the allowed error is smaller when the magnitudes are smaller.
!!
!!##OPTIONS
!!   expected_value   First value
!!   measured_value   Second value
!!   allowed_margin   Allowed relative margin
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_in_margin
!!    use :: M_framework__approx, only : in_margin
!!    implicit none
!!    write(*,*) in_margin(4.00000,3.99999,0.000000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.00000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0000001)
!!    write(*,*) in_margin(4.00000,3.99999,0.000001)
!!
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], &
!!            & [3.9,39.9,399.9,3999.9,39999.9] ,0.000001)
!!    write(*,*) in_margin([4.0,40.0,400.0,4000.0,40000.0], &
!!            & [3.9,39.9,399.9,3999.9,39999.9] ,0.00001)
!!
!!    write(*,*) in_margin(4.00000,3.99999,0.00001)
!!    write(*,*) in_margin(4.00000,3.99999,0.0001)
!!    write(*,*) in_margin(4.00000,3.99999,0.001)
!!    write(*,*) in_margin(4.00000,3.99999,0.01)
!!
!!    end program demo_in_margin
!!
!!   Results:
!!
!!        > F
!!        > F
!!        > F
!!        > F
!!        > F F F F F
!!        > F F F F T
!!        > T
!!        > T
!!        > T
!!        > T
!===================================================================================================================================
elemental impure function in_margin(expected_value, measured_value, allowed_margin)
implicit none

! ident_4="@(#) M_framework__approx in_margin(3f) check if two reals are approximately equal using a relative margin"

class(*),intent(in) :: expected_value, measured_value, allowed_margin
logical             :: in_margin

   doubleprecision     :: expected, measured, margin

   expected=anyscalar_to_double_(expected_value)
   measured=anyscalar_to_double_(measured_value)
   margin=anyscalar_to_double_(allowed_margin)

   if ( abs(expected-measured) > 0.50d0 * margin * (abs(expected)+abs(measured)) ) then
      in_margin=.false.  ! values not comparable
   else
      in_margin=.true.   ! values comparable
   endif

end function in_margin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function round_to_power(val,n)

! ident_5="@(#) M_framework__approx round_to_power(3f) round val to specified given decimal (power) position"

real,intent(in) :: val
integer,intent(in) :: n
real :: round_to_power
   round_to_power = anint(val*10.0**n)/10.0**n
end function round_to_power
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function round(val,idigits0)
implicit none

! ident_6="@(#) M_framework__approx round(3f) round val to specified number of significant digits"

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
   if(aval >= 1)then
      ipow=int(log10(aval)+1)
   else
      ipow=int(log10(aval))
   endif
   rnormal=val/(10.0d0**ipow)
   if(rnormal == 1)then
      ipow=ipow+1
   endif
   !normalize, multiply by 10*idigits to an integer, and so on
   round=real(anint(val*10.d0**(idigits-ipow)))*10.d0**(ipow-idigits)
end function round
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   significant(3f) - [M_framework__approx] round val to specified number
!!   of significant digits
!!
!!##SYNOPSIS
!!
!!     pure elemental function significant(val,digits,round)
!!
!!      real,intent(in)                      :: val
!!      integer,intent(in)                   :: digits
!!      character(len=*),intent(in),optional :: round
!!      real                                 :: significant
!!
!!##DESCRIPTION
!!
!! Round real value to specified number of significant digits
!!
!!##OPTIONS
!!
!! val     value to round
!! digits  number of significant digits to produce
!! round   Use the round edit descriptor
!!
!!           RU  UP : the value resulting from conversion shall be the
!!                    smallest representable value that is greater than or
!!                    equal to the original value
!!           RD  DOWN : the value resulting from conversion shall be the
!!                    largest representable value that is less than or
!!                    equal to the original value
!!           RZ  ZERO : the value resulting from conversion shall be the value
!!                    closest to the original value and no greater in
!!                    magnitude than the original value.
!!           RN  NEAREST : modeis NEAREST,thevalueresulting from conversion
!!                        shall be the closer of the two nearest
!!                        representable values if one is closer than the
!!                        other. If the two nearest representable values
!!                        are equidistant from the original value, it is
!!                        processor dependent which one of them is chosen.
!!           RC  COMPATIBLE : the value resulting from conversion shall be
!!                          the closer of the two nearest representable
!!                          values or the value away from zero if halfway
!!                          between them.
!!           RP  PROCESSOR_DEFINED : rounding during conversion shall be
!!                                   a processor-dependent default mode,
!!                                   which may correspond to one of the
!!                                   other modes.
!!
!!##EXAMPLE
!!
!!  Sample program
!!
!!    program demo_significant
!!    use M_framework__approx, only : significant
!!    implicit none
!!    integer :: i
!!    real :: r, v
!!    character(len=*),parameter :: g='(*(g0.7,1x))'
!!
!!       write(*,g)significant([8765.43210,0.1234567890],5)
!!
!!       write(*,*)'default:',1.23456789012345
!!       write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9])
!!       write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RU'),'RU'
!!       write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RD'),'RD'
!!       write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RZ'),'RZ'
!!       write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RN'),'RN'
!!       write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RC'),'RC'
!!       write(*,g)significant(1.23456789012345,[1,2,3,4,5,6,7,8,9],'RP'),'RP'
!!    end program demo_significant
!!
!!   Results:
!!
!!       > 8765.400 .1234600
!!       >  default:   1.234568
!!       > 1.000000 1.200000 1.230000 1.235000 1.234600 1.234570 ...
!!       > 1.234568 1.234568 1.234568
!!       > 2.000000 1.300000 1.240000 1.235000 1.234600 1.234570 ...
!!       > 1.234568 1.234568 1.234568 RU
!!       > 1.000000 1.200000 1.230000 1.234000 1.234500 1.234560 ...
!!       > 1.234567 1.234568 1.234568 RD
!!       > 1.000000 1.200000 1.230000 1.234000 1.234500 1.234560 ...
!!       > 1.234567 1.234568 1.234568 RZ
!!       > 1.000000 1.200000 1.230000 1.235000 1.234600 1.234570 ...
!!       > 1.234568 1.234568 1.234568 RN
!!       > 1.000000 1.200000 1.230000 1.235000 1.234600 1.234570 ...
!!       > 1.234568 1.234568 1.234568 RC
!!       > 1.000000 1.200000 1.230000 1.235000 1.234600 1.234570 ...
!!       > 1.234568 1.234568 1.234568 RP
pure elemental function significant_real32(val,digits,round) result(significant)

! ident_7="@(#) M_framework__approx significant_real32(3f) round val to specified number of significant digits"

integer,parameter :: wp=real32
real(kind=wp),intent(in)             :: val
integer,intent(in)                   :: digits
character(len=*),intent(in),optional :: round
real(kind=wp)                        :: significant
character(len=80)                    :: line,fmt
   if(present(round))then
      write(fmt,'("(",a,",e0.",i0,")")')trim(round),digits ! build e0.N format to write specified number of digits as 0.NNNNN+EE
   else
      write(fmt,'("(e0.",i0,")")')digits ! build e0.N format to write specified number of digits as 0.NNNNN+EE
   endif
   write(line,fmt)val                  ! write with specified number of significant diguts
   read(line,'(e80.30)')significant    ! read back into a value
end function significant_real32
!-----------------------------------------------------------------------------------------------------------------------------------
pure elemental function significant_real64(val,digits,round) result(significant)

! ident_8="@(#) M_framework__approx significant_real64(3f) round val to specified number of significant digits"

integer,parameter :: wp=real64
real(kind=wp),intent(in)             :: val
integer,intent(in)                   :: digits
character(len=*),intent(in),optional :: round
real(kind=wp)                        :: significant
character(len=80)                    :: line,fmt
   if(present(round))then
      write(fmt,'("(",a,",d0.",i0,")")')trim(round),digits ! build e0.N format to write specified number of digits as 0.NNNNN+EE
   else
      write(fmt,'("(d0.",i0,")")')digits ! build e0.N format to write specified number of digits as 0.NNNNN+EE
   endif
   write(line,fmt)val                  ! write with specified number of significant diguts
   read(line,'(d80.30)')significant    ! read back into a value
end function significant_real64
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_realbig_(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
use,intrinsic :: iso_fortran_env, only : wp=>real128
implicit none

! ident_9="@(#) M_framework__approx anyscalar_to_realbig_(3f) convert integer or real parameter of any kind to real128 or biggest available"

class(*),intent(in)          :: valuein
real(kind=wp)           :: d_out
character(len=3)             :: readable
   select type(valuein)
   type is (integer(kind=int8));   d_out=real(valuein,kind=wp)
   type is (integer(kind=int16));  d_out=real(valuein,kind=wp)
   type is (integer(kind=int32));  d_out=real(valuein,kind=wp)
   type is (integer(kind=int64));  d_out=real(valuein,kind=wp)
   type is (real(kind=real32));    d_out=real(valuein,kind=wp)
   type is (real(kind=real64));    d_out=real(valuein,kind=wp)
   Type is (real(kind=real128));   d_out=valuein
   type is (logical);              d_out=merge(0.0_wp,1.0_wp,valuein)
   type is (character(len=*));     read(valuein,*) d_out
   class default
    !!d_out=huge(0.0_wp)
    readable='NaN'
    read(readable,*)d_out
    !!stop '*M_framework__approx::anyscalar_to_realbig_: unknown type'
   end select
end function anyscalar_to_realbig_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure elemental function anyscalar_to_double_(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_10="@(#) M_framework__approx anyscalar_to_double_(3f) convert integer or real parameter of any kind to doubleprecision"

class(*),intent(in)       :: valuein
doubleprecision           :: d_out
doubleprecision,parameter :: big=huge(0.0d0)
   select type(valuein)
   type is (integer(kind=int8));   d_out=dble(valuein)
   type is (integer(kind=int16));  d_out=dble(valuein)
   type is (integer(kind=int32));  d_out=dble(valuein)
   type is (integer(kind=int64));  d_out=dble(valuein)
   type is (real(kind=real32));    d_out=dble(valuein)
   type is (real(kind=real64));    d_out=dble(valuein)
   Type is (real(kind=real128))
      !!if(valuein > big)then
      !!   write(error_unit,*)'*anyscalar_to_double_* value too large ',valuein
      !!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   !type is (real(kind=real128))
   !   if(valuein > big)then
   !      write(error_unit,*)'*anyscalar_to_double_* value too large ',valuein
   !   endif
   !   d_out=dble(valuein)
   class default
     d_out=0.0d0
     !!stop '*M_framework__approx::anyscalar_to_double_: unknown type'
   end select
end function anyscalar_to_double_

end module M_framework__approx

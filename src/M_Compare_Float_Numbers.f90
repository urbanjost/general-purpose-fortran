
!>
!!##NAME
!!       M_Compare_Float_Numbers - [M_Compare_Float_Numbers::INTRO] perform relational comparisons on real numbers
!!       (LICENSE:GPL)
!!
!!##PURPOSE
!!       Module containing routines to perform equality and relational
!!       comparisons on floating point numbers.
!!
!!##CATEGORY
!!       Utility
!!
!!##LANGUAGE
!!       Fortran-95
!!
!!##CALLING SEQUENCE
!!       USE M_Compare_Float_Numbers
!!
!!##CONTAINS
!!       .EqualTo.       Relational operator to test the equality of
!!                       floating point numbers.
!!
!!       .GreaterThan.   Relational operator to test if one operand
!!                       is greater than another.
!!
!!       .LessThan.      Relational operator to test if one operand
!!                       is less than another.
!!
!!       Compare_Float:  Function to compare floating point scalars
!!                       and arrays with adjustable precision tolerance.
!!
!!##INCLUDE FILES
!!       None.
!!
!!##EXTERNALS
!!       None.
!!
!!##COMMON BLOCKS
!!       None.
!!
!!##CREATION HISTORY
!!    Written by:     Paul van Delst, CIMSS/SSEC 01-Apr-2003
!!                    paul.vandelst@ssec.wisc.edu
!!
!!    Copyright:      (C) 2003 Paul van Delst
!!
!!    This program is free software; you can redistribute it and/or modify it
!!    under the terms of the GNU General Public License as published by the
!!    Free Software Foundation; either version 2 of the License, or (at your
!!    option) any later version.
!!
!!    This program is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
!!    Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License along
!!    with this program; if not, write to the Free Software Foundation, Inc.,
!!    59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!------------------------------------------------------------------------------
MODULE M_Compare_Float_Numbers
IMPLICIT NONE ! Disable all implicit typing
PRIVATE       ! Default visibility
!===================================================================================================================================
! DEFINE TYPES
!------------------------------------------------------------------------------
!       hold specification kinds for variable declaration.
!
! OUTPUTS:
!       Integer Kind Types
!       ------------------
!
!       Byte:             Kind type for byte (1-byte) integer variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Short:            Kind type for short (2-byte) integer variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Long:             Kind type for long (4-byte) integer variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       LLong:            Kind type for double long (8-byte) integer variable
!                         If this kind type is not supported by a compiler, the
!                         value defaults to Long.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       IP_Kind:          Kind type for a user specified default integer.
!                         The actual kind type this value corresponds
!                         to is determined by the PRIVATE IIP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!
!       Floating point Kind Types
!       -------------------------
!
!       Single:           Kind type for single precision (4-byte) real variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Double:           Kind type for double precision (8-byte) real variable
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       Quad:             Kind type for quad precision (16-byte) real variable
!                         If this kind type is not supported by a compiler, the
!                         value defaults to Double.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       FP_Kind:          Kind for for a user specified default floating point
!                         variable. The actual kind type this value corresponds
!                         to is determined by the PRIVATE IFP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!
!       Integer Byte Sizes
!       ------------------
!
!       n_Bytes_Byte:     The expected size of a Byte kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Short:    The expected size of a Short kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Long:     The expected size of a Long kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_LLong:    The expected size of a LLong kind integer in units
!                         of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_IP_kind:  The expected size of the user specified default
!                         integer kind in units of 8-bit bytes. The actual
!                         kind type size this value corresponds to is
!                         determined by the PRIVATE IIP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!
!
!       Floating point Byte Sizes
!       -------------------------
!
!       n_Bytes_Single:   The expected size of a Single kind real variable
!                         in units of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Double:   The expected size of a Double kind real variable
!                         in units of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_Quad:     The expected size of a Quad kind real variable
!                         in units of 8-bit bytes.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!       n_Bytes_FP_kind:  The expected size of the user specified default
!                         real kind variable in units of 8-bit bytes. The
!                         actual kind type size this value corresponds to
!                         is determined by the PRIVATE IFP index.
!                         UNITS:      N/A.
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: PARAMETER, PUBLIC
!
!
! SIDE EFFECTS:
!       If the LLong or Quad kind types are not available they default to the
!       Long and Double kind specifications.
!
! RESTRICTIONS:
!       None
!
! EXAMPLE:
!       INTEGER( Long ) :: i, j
!       REAL( Single )  :: x, y
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
!
!  Copyright (C) 2000, 2004 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

  ! -------------------------------------------------------------------
  ! THE DEFAULT FLOATING POINT INDEX. Change the value of IFP for the
  ! required floating point kind. The following chart details the
  ! correspondence:
  !
  !    IFP     REAL( fp_kind )
  !  ==============================
  !     1       Single (4  bytes)
  !     2       Double (8  bytes)
  !     3       Quad   (16 bytes)  **IF AVAILABLE, Double OTHERWISE**
  !
  ! -------------------------------------------------------------------

  INTEGER, PARAMETER, PRIVATE :: IFP = 2  ! 1=Single, 2=Double, 3=Quad

  ! -------------------------------------------------------------------
  ! THE DEFAULT INTEGER INDEX. Change the value of IIP for the required
  ! integer kind. The following chart details the correspondence:
  !
  !    IIP     INTEGER( ip_kind )
  !  ==============================
  !     1        Byte
  !     2       Short (2 bytes)
  !     3        Long (4 bytes)
  !     4       LLong (8 bytes)  **IF AVAILABLE, Long OTHERWISE**
  !
  ! -------------------------------------------------------------------

  INTEGER, PARAMETER, PRIVATE :: IIP = 3  ! 1=Byte, 2=Short, 3=Long, 4=LLong

  ! -------------------
  ! Integer definitions
  ! -------------------

  ! -- Integer types
  INTEGER, PARAMETER, PUBLIC  :: Byte    = SELECTED_INT_KIND(1)   ! Byte  integer
  INTEGER, PARAMETER, PUBLIC  :: Short   = SELECTED_INT_KIND(4)   ! Short integer
  INTEGER, PARAMETER, PUBLIC  :: Long    = SELECTED_INT_KIND(8)   ! Long  integer
  INTEGER, PARAMETER, PRIVATE :: LLong_t = SELECTED_INT_KIND(16)  ! LLong integer
  INTEGER, PARAMETER, PUBLIC  :: LLong   = ( ( ( 1 + SIGN( 1, LLong_t ) ) / 2 ) * LLong_t ) + &
                                           ( ( ( 1 - SIGN( 1, LLong_t ) ) / 2 ) * Long    )

  ! -- Expected 8-bit byte sizes of the integer kinds
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Byte  = 1
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Short = 2
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Long  = 4
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_LLong = 8

  ! -- Define arrays for default definition
  INTEGER, PARAMETER, PRIVATE :: N_IP_KINDS = 4
  INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_KIND_TYPES = (/ Byte,  &
                                                                             Short, &
                                                                             Long,  &
                                                                             LLong  /)
  INTEGER, PARAMETER, DIMENSION( N_IP_KINDS ), PRIVATE :: IP_BYTE_SIZES = (/ n_Bytes_Byte,  &
                                                                             n_Bytes_Short, &
                                                                             n_Bytes_Long,  &
                                                                             n_Bytes_LLong  /)

  ! -- Default values
  INTEGER, PARAMETER, PUBLIC  :: IP_Kind         = IP_KIND_TYPES( IIP )
  INTEGER, PARAMETER, PUBLIC  :: n_Bytes_IP_Kind = IP_BYTE_SIZES( IIP )

  ! --------------------------
  ! Floating point definitions
  ! --------------------------

  ! -- Floating point types
  INTEGER, PARAMETER, PUBLIC  :: Single = SELECTED_REAL_KIND(6)  ! Single precision
  INTEGER, PARAMETER, PUBLIC  :: Double = SELECTED_REAL_KIND(15) ! Double precision
  INTEGER, PARAMETER, PRIVATE :: Quad_t = SELECTED_REAL_KIND(20) ! Quad precision
  INTEGER, PARAMETER, PUBLIC  :: Quad   = ( ( ( 1 + SIGN( 1, Quad_t ) ) / 2 ) * Quad_t ) + &
                                          ( ( ( 1 - SIGN( 1, Quad_t ) ) / 2 ) * Double )

  ! -- Expected 8-bit byte sizes of the floating point kinds
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Single = 4
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Double = 8
  INTEGER, PARAMETER, PUBLIC :: n_Bytes_Quad   = 16

  ! -- Define arrays for default definition
  INTEGER, PARAMETER, PRIVATE :: N_FP_KINDS = 3
  INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_KIND_TYPES = (/ Single, &
                                                                             Double, &
                                                                             Quad    /)
  INTEGER, PARAMETER, DIMENSION( N_FP_KINDS ), PRIVATE :: FP_BYTE_SIZES = (/ n_Bytes_Single, &
                                                                             n_Bytes_Double, &
                                                                             n_Bytes_Quad    /)

  ! -- Default values
  INTEGER, PARAMETER, PUBLIC  :: FP_Kind         = FP_KIND_TYPES( IFP )
  INTEGER, PARAMETER, PUBLIC  :: n_Bytes_FP_Kind = FP_BYTE_SIZES( IFP )
!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Type_Kinds.f90,v 2.13 2004/11/30 20:37:36 paulv Exp $
!
! $Date: 2004/11/30 20:37:36 $
!
! $Revision: 2.13 $
!
! $Name:  $
!
! $State: Exp $
!===================================================================================================================================
! END OF TYPES
!===================================================================================================================================

  ! ------------
  ! Visibilities
  ! ------------

  PUBLIC :: Compare_Float
  PUBLIC :: OPERATOR (.EqualTo.)
  PUBLIC :: OPERATOR (.GreaterThan.)
  PUBLIC :: OPERATOR (.LessThan.)
  PUBLIC :: test_suite_M_Compare_Float_numbers

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Compare_Float
    MODULE PROCEDURE Compare_Float_Single
    MODULE PROCEDURE Compare_Float_Double
  END INTERFACE Compare_Float

  INTERFACE OPERATOR (.EqualTo.)
    MODULE PROCEDURE Is_Equal_To_Single
    MODULE PROCEDURE Is_Equal_To_Double
  END INTERFACE OPERATOR (.EqualTo.)

  INTERFACE OPERATOR (.GreaterThan.)
    MODULE PROCEDURE Is_Greater_Than_Single
    MODULE PROCEDURE Is_Greater_Than_Double
  END INTERFACE OPERATOR (.GreaterThan.)

  INTERFACE OPERATOR (.LessThan.)
    MODULE PROCEDURE Is_Less_Than_Single
    MODULE PROCEDURE Is_Less_Than_Double
  END INTERFACE OPERATOR (.LessThan.)

  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER(len=*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: M_Compare_Float_Numbers.f90,v 2.3 2004/10/06 19:00:23 paulv Exp $'

CONTAINS
!----------------------------------------------------------------------------------
!S+
! NAME:
!       .EqualTo.
!
! PURPOSE:
!       Relational operator to test the equality of floating point numbers.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       IF ( x .EqualTo. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .EqualTo. y)    The result is a logical value indicating whether
!                          the operands are equal to within numerical precision
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., the numbers are considered equal.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Is_Equal_To_Single( x, y ) RESULT( Equal_To )
    REAL( Single ), INTENT( IN )  :: x, y
    LOGICAL :: Equal_To

    Equal_To = ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )

  END FUNCTION Is_Equal_To_Single
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Is_Equal_To_Double( x, y ) RESULT( Equal_To )
    REAL( Double ), INTENT( IN )  :: x, y
    LOGICAL :: Equal_To

    Equal_To = ABS( x - y ) < SPACING( MAX(ABS(x),ABS(y)) )

  END FUNCTION Is_Equal_To_Double
!----------------------------------------------------------------------------------
!S+
! NAME:
!       .GreaterThan.
!
! PURPOSE:
!       Relational operator to test if one operand is greater than another.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       IF ( x .GreaterThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .GreaterThan. y)    The result is a logical value indicating whether
!                              the operand x is greater than y by more than
!                              the spacing between representable floating point
!                              numbers.
!                              UNITS:      N/A
!                              TYPE:       LOGICAL
!                              DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( x - y ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered greater than y.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Is_Greater_Than_Single( x, y ) RESULT ( Greater_Than )
    REAL( Single ), INTENT( IN ) :: x, y
    LOGICAL :: Greater_Than

    IF ( (x - y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Single
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Is_Greater_Than_Double( x, y ) RESULT ( Greater_Than )
    REAL( Double ), INTENT( IN ) :: x, y
    LOGICAL :: Greater_Than

    IF ( (x - y) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Greater_Than = .TRUE.
    ELSE
      Greater_Than = .FALSE.
    END IF
  END FUNCTION Is_Greater_Than_Double
!----------------------------------------------------------------------------------
!S+
! NAME:
!       .LessThan.
!
! PURPOSE:
!       Relational operator to test if one operand is less than another.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       IF ( x .LessThan. y ) THEN
!         .....
!       END IF
!
! OPERANDS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!
! OPERATOR RESULT:
!       (x .LessThan. y)    The result is a logical value indicating whether
!                           the operand x is less than y by more than the
!                           spacing between representable floating point
!                           numbers.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Same as operands.
!
! PROCEDURE:
!       The test performed is
!
!         ( y - x ) >= SPACING( MAX(ABS(x),ABS(y)) )
!
!       If the result is .TRUE., x is considered less than y.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Is_Less_Than_Single( x, y ) RESULT ( Less_Than )
    REAL( Single ), INTENT( IN ) :: x, y
    LOGICAL :: Less_Than

    IF ( (y - x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Single
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Is_Less_Than_Double( x, y ) RESULT ( Less_Than )
    REAL( Double ), INTENT( IN ) :: x, y
    LOGICAL :: Less_Than

    IF ( (y - x) >= SPACING( MAX( ABS(x), ABS(y) ) ) ) THEN
      Less_Than = .TRUE.
    ELSE
      Less_Than = .FALSE.
    END IF
  END FUNCTION Is_Less_Than_Double
!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compare_Float
!
! PURPOSE:
!       Function to compare floating point scalars and arrays with adjustable
!       precision tolerance.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Result = Compare_Float( x, y,     &  ! Input
!                               ULP = ULP )  ! Optional input
!
! INPUT ARGUMENTS:
!       x, y:        Two congruent floating point data objects to compare.
!                    UNITS:      N/A
!                    TYPE:       REAL( Single )   [ == default real]
!                                  OR
!                                REAL( Double )
!                    DIMENSION:  Scalar, or any allowed rank array.
!                    ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP:         Unit of data precision. The acronym stands for "unit in
!                    the last place," the smallest possible increment or decrement
!                    that can be made using a machine's floating point arithmetic.
!                    A 0.5 ulp maximum error is the best you could hope for, since
!                    this corresponds to always rounding to the nearest representable
!                    floating-point number. Value must be positive - if a negative
!                    value is supplied, the absolute value is used.
!                    If not specified, the default value is 1.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Result:      The return value is a logical value indicating whether
!                    the inputs are equal (to within the required precision)
!                    .TRUE.  - if the floating point numbers are equal to
!                              within the specified tolerance.
!                    .FALSE. - if the floating point numbers are different.
!                    UNITS:      N/A
!                    TYPE:       LOGICAL
!                    DIMENSION:  Scalar
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The test performed is
!
!         ABS( x - y ) < ( ULP * SPACING( MAX(ABS(x),ABS(y)) ) )
!
!       If the result is .TRUE., the numbers are considered equal.
!
!       The intrinsic function SPACING(x) returns the absolute spacing of numbers
!       near the value of x,
!
!                      {     EXPONENT(x)-DIGITS(x)
!                      {  2.0                        for x /= 0
!         SPACING(x) = {
!                      {
!                      {  TINY(x)                    for x == 0
!
!       The ULP optional argument scales the comparison.
!
!       James Van Buskirk and James Giles suggested this method for floating
!       point comparisons in the comp.lang.fortran newsgroup.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Compare_Float_Single( x, y, ulp ) RESULT( Compare )
    REAL( Single ),           INTENT( IN )  :: x
    REAL( Single ),           INTENT( IN )  :: y
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    LOGICAL :: Compare

    REAL( Single ) :: Rel

    Rel = 1.0_Single
    IF ( PRESENT( ulp ) ) THEN
      Rel = REAL( ABS(ulp), Single )
    END IF

    Compare = ABS( x - y ) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )

  END FUNCTION Compare_Float_Single
!----------------------------------------------------------------------------------
  ELEMENTAL FUNCTION Compare_Float_Double( x, y, ulp ) RESULT( Compare )
    REAL( Double ),           INTENT( IN )  :: x
    REAL( Double ),           INTENT( IN )  :: y
    INTEGER,        OPTIONAL, INTENT( IN )  :: ulp
    LOGICAL :: Compare

    REAL( Double ) :: Rel

    Rel = 1.0_Double
    IF ( PRESENT( ulp ) ) THEN
      Rel = REAL( ABS(ulp), Double )
    END IF

    Compare = ABS( x - y ) < ( Rel * SPACING( MAX(ABS(x),ABS(y)) ) )

  END FUNCTION Compare_Float_Double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_Compare_Float_Numbers()
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify, only : unit_test_level
implicit none
!*! setup
   call test_compare_float_double()
   call test_compare_float_single()
   call test_is_equal_to_double()
   call test_is_equal_to_single()
   call test_is_greater_than_double()
   call test_is_greater_than_single()
   call test_is_less_than_double()
   call test_is_less_than_single()
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_compare_float_double()

   call unit_test_start('compare_float_double',msg='')
   !*!call unit_test('compare_float_double', 0.eq.0, 'checking',100)
   call unit_test_done('compare_float_double',msg='')
end subroutine test_compare_float_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_compare_float_single()

   call unit_test_start('compare_float_single',msg='')
   !*!call unit_test('compare_float_single', 0.eq.0, 'checking',100)
   call unit_test_done('compare_float_single',msg='')
end subroutine test_compare_float_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_equal_to_double()

   call unit_test_start('is_equal_to_double',msg='')
   !*!call unit_test('is_equal_to_double', 0.eq.0, 'checking',100)
   call unit_test_done('is_equal_to_double',msg='')
end subroutine test_is_equal_to_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_equal_to_single()

   call unit_test_start('is_equal_to_single',msg='')
   !*!call unit_test('is_equal_to_single', 0.eq.0, 'checking',100)
   call unit_test_done('is_equal_to_single',msg='')
end subroutine test_is_equal_to_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_greater_than_double()

   call unit_test_start('is_greater_than_double',msg='')
   !*!call unit_test('is_greater_than_double', 0.eq.0, 'checking',100)
   call unit_test_done('is_greater_than_double',msg='')
end subroutine test_is_greater_than_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_greater_than_single()

   call unit_test_start('is_greater_than_single',msg='')
   !*!call unit_test('is_greater_than_single', 0.eq.0, 'checking',100)
   call unit_test_done('is_greater_than_single',msg='')
end subroutine test_is_greater_than_single
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_less_than_double()

   call unit_test_start('is_less_than_double',msg='')
   !*!call unit_test('is_less_than_double', 0.eq.0, 'checking',100)
   call unit_test_done('is_less_than_double',msg='')
end subroutine test_is_less_than_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_is_less_than_single()

   call unit_test_start('is_less_than_single',msg='')
   !*!call unit_test('is_less_than_single', 0.eq.0, 'checking',100)
   call unit_test_done('is_less_than_single',msg='')
end subroutine test_is_less_than_single
!===================================================================================================================================
end subroutine test_suite_M_Compare_Float_Numbers
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

END MODULE M_Compare_Float_Numbers
!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: M_Compare_Float_Numbers.f90,v 2.3 2004/10/06 19:00:23 paulv Exp $
!
! $Date: 2004/10/06 19:00:23 $
!
! $Revision: 2.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: M_Compare_Float_Numbers.f90,v $
! Revision 2.3  2004/10/06 19:00:23  paulv
! - Cosmetic change only.
!
! Revision 2.2  2004/08/31 19:59:30  paulv
! - Added .EqualTo., .GreaterThan., and .LessThan. relational operators for
!   Single and Double floating point data objects.
!
! Revision 2.1  2004/08/16 16:25:41  paulv
! - Updated header documentation.
!
! Revision 2.0  2004/08/13 20:25:48  paulv
! - New version using elemental functions for Compare_Float.
!
! Revision 1.1  2003/04/02 14:35:30  paulv
! Initial checkin.
!

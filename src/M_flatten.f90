!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     M_flatten(3f) - [M_flatten::INTRO] Module for dealing with argument
!!     rank mismatch
!!     (LICENSE:MIT)
!!
!!##DESCRIPTION
!!
!! Standard methods for passing arguments of different rank are suitable for
!! many cases but when procedures have many arguments with independently
!! variable ranks techniques such as generic procedures can require
!! excessive duplication.
!!
!! The FLATTEN(3) procedure provided here can simply such interfaces
!! particularly when contiguous data is being passed.
!!
!!##SYNOPSIS
!!
!!
!!   public methods:
!!
!!    flatten    returns a rank one array pointer to a scalar or multi-dimensional array
!!
!!##EXAMPLES
!!
!!
!!  Sample program:
!!
!!    program demo_M_flatten
!!    use M_flatten, only : flatten
!!    implicit none
!!    integer :: a
!!    integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)
!!
!!       write(*,*)'WANTED:'
!!       a=0
!!       call wanted ( a, b0 )
!!       print *, 'a=', a, 'b0=', b0
!!       call wanted ( a, b1 )
!!       print *, 'a=', a, 'b1=', b1
!!       call wanted ( a, b2 )
!!       print *, 'a=', a, 'b2=', b2
!!       call wanted ( a, b3 )
!!       print *, 'a=', a, 'b3=', b3
!!
!!
!!       write(*,*)'WANTED1:'
!!       ! Alternatively, to avoid using pointers directly
!!       ! write the called routine to expect a flattened
!!       ! array and call the argument with flatten().
!!
!!       a=0
!!       call wanted1 ( a, flatten(b0) )
!!       print *, 'a=', a, 'b0=', b0
!!       call wanted1 ( a, flatten(b1) )
!!       print *, 'a=', a, 'b1=', b1
!!       call wanted1 ( a, flatten(b2) )
!!       print *, 'a=', a, 'b2=', b2
!!       call wanted1 ( a, flatten(b3) )
!!       print *, 'a=', a, 'b3=', b3
!!
!!    contains
!!
!!    subroutine wanted1( a, b)
!!    integer, intent(inout) :: a
!!    integer, intent(out)   :: b(:)
!!    integer                :: i
!!       do i=1,size(b)
!!          a = a + 1
!!          b(i) = a
!!       enddo
!!    end subroutine wanted1
!!
!!    subroutine wanted( a, b)
!!    ! This technique is known as pointer rank remapping (introduced in
!!    ! Fortran 2003 and expanded in Fortran 2008).
!!    ! requires the multi-dimensional target array is simply contiguous.
!!    integer, intent(inout)                  :: a
!!    integer,target, contiguous, intent(out) :: b(..)
!!    integer                                 :: i
!!    integer,pointer                         :: p_b(:)
!!       p_b=>flatten(b)
!!       do i=1,size(b)
!!          a = a + 1
!!          p_b(i) = a
!!       enddo
!!    end subroutine wanted
!!
!!    end program demo_M_flatten
!!
!!   Results:
!!
!!    >  WANTED:
!!    >  a=           1 b0=  1
!!    >  a=           4 b1=  2    3       4
!!    >  a=           8 b2=  5    6       7       8
!!    >  a=          12 b3=  9   10      11      12
!!    >  WANTED1:
!!    >  a=           1 b0=  1
!!    >  a=           4 b1=  2    3       4
!!    >  a=           8 b2=  5    6       7       8
!!    >  a=          12 b3=  9   10      11      12
!!
!!##DETAILS
!!
!! The M_flatten module provides the procedure FLATTEN(3) which
!! provides a function that returns a rank one array pointer which
!! points to a scalar or an array of any shape.
!!
!!    CALLING FLATTEN IN THE CALL TO THE USER PROCEDURE
!!
!! To avoid using pointers directly write the called routine to expect a
!! flattened array and pass the arguments with varying rank in a call to
!!##FLATTEN(3).
!!
!! Note that the called procedure WANTED(3) will not know the original rank
!! or shape unless it is passed, but will know the size of the input array.
!!
!! The argument to FLATTEN(3) should be a whole contiguous array. A
!! slice or subsection would almost certainly just create and alter a
!! temporary.
!!
!!       program arg
!!       use M_flatten, only : flatten
!!       implicit none
!!       integer :: a
!!       integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)
!!
!!          a=0
!!          call wanted ( a, flatten(b0) )
!!          print *, 'a=', a, 'b0=', b0
!!          call wanted ( a, flatten(b1) )
!!          print *, 'a=', a, 'b1=', b1
!!          call wanted ( a, flatten(b2) )
!!          print *, 'a=', a, 'b2=', b2
!!          call wanted ( a, flatten(b3) )
!!          print *, 'a=', a, 'b3=', b3
!!
!!       contains
!!
!!       subroutine wanted( a, b)
!!       integer, intent(inout) :: a
!!       integer, intent(out)   :: b(:)
!!       integer                :: i
!!          do i=1,size(b)
!!             a = a + 1
!!             b(i) = a
!!          enddo
!!       end subroutine wanted
!!       end program arg
!!
!!   Result
!!
!!     a=      1 b0=      1
!!     a=      4 b1=      2      3      4
!!     a=      8 b2=      5      6      7      8
!!     a=     12 b3=      9     10     11     12
!!
!!    CALLING FLATTEN IN THE USER PROCEDURE
!!
!! A minor use of pointers is required in this alternate use of
!! FLATTEN(3) but the called routine can query the original input
!! argument with RANK(3) and SHAPE(3) not just SIZE(3).
!!
!!       program elem
!!       implicit none
!!       integer :: a
!!       integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1), b4(2,2,2)
!!
!!          a=0
!!          call wanted ( a, b0 )
!!          print *, 'a=', a, 'b0=', b0
!!          call wanted ( a, b1 )
!!          print *, 'a=', a, 'b1=', b1
!!          call wanted ( a, b2 )
!!          print *, 'a=', a, 'b2=', b2
!!          call wanted ( a, b3 )
!!          print *, 'a=', a, 'b3=', b3
!!
!!          a=0
!!          b4=-99
!!          call wanted ( a, b4(2,:,:) )
!!          print *, 'a=', a, 'b4=', b4
!!
!!          a=0
!!          b4=-99
!!          call wanted ( a, b4(:,2,:) )
!!          print *, 'a=', a, 'b4=', b4
!!
!!       contains
!!
!!       subroutine wanted( a, b)
!!       ! This technique is known as pointer rank remapping (introduced in
!!       ! Fortran 2003 and expanded in Fortran 2008). See below for details
!!       use M_flatten, only : flatten
!!       integer, intent(inout)                  :: a
!!       integer,target, contiguous, intent(out) :: b(..)
!!       integer                                 :: i
!!       integer,pointer                         :: p_b(:)
!!          p_b=>flatten(b)
!!          do i=1,size(b)
!!             a = a + 1
!!             p_b(i) = a
!!          enddo
!!       end subroutine wanted
!!
!!       end program elem
!!
!!  Result
!!
!!    a=  1   b0=    1
!!    a=  4   b1=    2    3    4
!!    a=  8   b2=    5    6    7    8
!!    a=  12  b3=    9   10   11   12
!!    a=  4   b4=  -99    1  -99    2   -99    3  -99  4
!!    a=  4   b4=  -99  -99    1    2   -99  -99    3  4
!!
!! A single-dimension array pointer can point to a multi-dimensional array?
!!
!! Yes, a single-dimension (rank-1) array pointer can point to a
!! multi-dimensional array in Fortran, provided that the multi-dimensional
!! target array is simply contiguous.
!!
!! This technique is known as pointer rank remapping (introduced in Fortran
!! 2003 and expanded in Fortran 2008).
!!
!!   RULES AND REQUIREMENTS
!!
!!   Explicit Bounds: You must specify the explicit upper and lower
!!   bounds on the left-hand side of the pointer assignment. You cannot
!!   use a deferred colon (:).
!!
!!   Contiguity: The multi-dimensional target array must occupy
!!   a continuous block of memory. Regular allocated or static arrays
!!   are automatically contiguous, but array slices with strides (e.g.,
!!   matrix(::2, :)) are not.
!!
!!   Array Size: The size of the pointer must exactly match or
!!   be less than the total number of elements in the target array.
!!   Target Attribute: The multi-dimensional array must be declared with
!!   the target attribute.
!!
!!##SOMETHING AS GENERAL AS FLATTEN MAY NOT BE REQUIRED
!!
!!  ELEMENTAL PROCEDURES
!!
!! If you wish to make a procedure that can be called with a specific
!! argument having different ranks you can use elemental procedures if
!! all the arrays passed are of the same size and all the scalar arguments
!! are INTENT(IN).
!!
!! Since the scalars are not able to be changed this allows for pure procedures
!! to be called in parallel where each call to the scalar routine can be made
!! in any order.
!!
!!  GENERIC INTERFACE
!!
!! One could also write separate subroutines for each rank, and then make a
!! _generic interface_ for all of them. If there are only a few combinations
!! of arguments with varying rank it has the advantage that there are not
!! restrictions on the arrays all having the same rank or scalars being
!! only INTENT(IN) and unsupported ranks are detected at compile time rather
!! than run time. If there are multiple values that can be of different ranks
!! independent of each other the number of permutations and thus the number
!! of interface routines can get excessive. A preprocessor might help generate
!! a large number of interfaces but FLATTEN(3) provides a simple alternative
!! for such cases.
!!
!!  ASSUMED RANK AND SELECT RANK
!!
!! Another approach is to define a dummy argument to be assumed rank.
!! The programmer must then account for each rank manually. It is possible
!! to catch the assumed size case and treat it specially; in the following
!! code it just writes a message saying it is unsupported.
!!
!!    program elem
!!       implicit none
!!       integer :: a
!!       integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)
!!
!!       a=0
!!       call wanted( a, b0 )
!!       print *, 'a=', a, 'b0=', b0
!!       call wanted( a, b1 )
!!       print *, 'a=', a, 'b1=', b1
!!       call wanted( a, b2 )
!!       print *, 'a=', a, 'b2=', b2
!!       call wanted( a, b3 )
!!       print *, 'a=', a, 'b3=', b3
!!
!!    contains
!!
!!       subroutine wanted( a, b )
!!          integer, intent(inout)           :: a
!!          integer, contiguous, intent(out) :: b(..)
!!
!!          integer :: i, j, k
!!
!!          select rank (b)
!!          rank (0)
!!          a = a + 1
!!          b = a
!!
!!          rank (1)
!!          do i = 1, size(b)
!!             a = a + 1
!!             b(i) = a
!!          enddo
!!
!!          rank (2)
!!          do j = 1, size(b,dim=2)
!!             do i = 1, size(b,dim=1)
!!                a = a + 1
!!                b(i,j) = a
!!             enddo
!!          enddo
!!
!!          rank (3)
!!          do k = 1, size(b,dim=3)
!!             do j = 1, size(b,dim=2)
!!                do i = 1, size(b,dim=1)
!!                   a = a + 1
!!                   b(i,j,k) = a
!!                enddo
!!             enddo
!!          enddo
!!
!!          rank (*)
!!          print *, 'assumed size is unsupported'
!!          stop 1
!!
!!          rank default
!!          print *,  'unsupported rank'
!!          stop 2
!!
!!       end select
!!
!!     end subroutine wanted
!!     end program elem
!!
!!##ALTERNATIVE I
!!
!!  ARGUMENT MISMATCH
!!
!! This alternative method allows
!! argument mismatch and using assumed-size arrays (Fortran 77 Style)
!!
!! Modern compilers frequently treat rank mismatches as a fatal error
!! by default, which frequently impacts legacy Fortran 77 code. It was
!! extremely common for pre-f90 compilers to allow argument rank mismatch
!! and the extension was heavily used in many codes.
!!
!! So to allow a rank mismatch between an actual argument and a dummy
!! argument in Fortran, you can generally apply a compiler-specific flag to
!! bypass the error during compilation or (with most compilers) build the
!! routine with no explicit interface in a separate file from the calls to
!! the procedure.
!!
!! Explicit interfaces prevent a number of common errors and are a strongly
!! desirable feature, so allowing rank mismatches is not recommended in
!! general, but if you are compiling older code and cannot modify the source
!! to use a modern method, here are examples of appropriate flags to your
!! compiler to downgrade the error to a warning:
!!
!!     gfortran (GCC 10+): Add the flag -fallow-argument-mismatch. You
!!     can also use -std=legacy, though it may introduce other unintended
!!     behaviors.
!!
!!     Intel Fortran (ifort / ifx): Add the flag -warn noargument-mismatch
!!     or use directives like !DIR$ ATTRIBUTES NO_ARG_CHECK specifically
!!     before the dummy argument declaration to bypass checking on an
!!     individual routine.
!!
!! Now if you want to pass a multi-dimensional array or a specific element of
!! an array to a flat 1D sequence, you can use an _assumed-size array_.
!!
!!  * Declare the final dimension of the dummy argument with an asterisk (\*).
!!  * Pass the first element of the array or array section explicitly
!!    (known as _sequence association_).
!!
!!       program arbitrary
!!       implicit none
!!       integer :: a
!!       integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)
!!       external wanted
!!
!!          a=0
!!          call wanted ( a, b0 ,1)
!!          print *, 'a=', a, 'b0=', b0
!!          call wanted ( a, b1 ,size(b1))
!!          print *, 'a=', a, 'b0=', b1
!!          call wanted ( a, b2 ,size(b2))
!!          print *, 'a=', a, 'b0=', b2
!!          call wanted ( a, b3 ,size(b3))
!!          print *, 'a=', a, 'b0=', b3
!!       contains
!!
!!       end program arbitrary
!!
!!       subroutine wanted( a, b, n )
!!       integer, intent(inout) :: a
!!       integer, intent(out)   :: b(*)
!!       integer, intent(in)    :: n
!!       integer                :: i
!!          do i=1,n
!!             a = a + 1
!!             b(i) = a
!!          enddo
!!       end subroutine wanted
!!
!!
!!       $ gfortran arbitrary.f90 -fallow-argument-mismatch -o arbitrary
!!       arbitrary.f90:8:20:
!!
!!       8 |    call wanted ( a, b0 ,1)
!!         |                    1
!!       Warning: Rank mismatch in argument ‘b’ at (1) (rank-1 and scalar)
!!
!!       ./arbitrary
!!        a=       1 b0=       1
!!        a=       4 b1=       2       3       4
!!        a=       8 b2=       5       6       7       8
!!        a=      12 b3=       9      10      11      12
!!
!! Generally when using sequence association and rank mismatch:
!!
!!  + Place the "wanted" procedure in a separate file without an
!!    interface, like you often would with F77 code.
!!
!!  + Declare the dummy argument dimension with an asterisk (*).
!!
!!  + Pass the number of elements as a new argument.
!!
!!  + Pass the first element of the array or array section explicitly
!!    (this is the part known as sequence association).
!!
!! You might have to add a compiler-specific option depending on how much the
!! compiler wants to prevent you from using sequence association.
!!
!!
!!##ALTERNATIVE II
!!
!!  USING EXPERIMENTAL PROPOSED METHOD
!!
!! Using GNU Fortran (GCC) 16.0.0 20250727 (experimental) and a compiler
!! switch to allow using proposed features lets you try a proposed
!! feature now that simplifies using assumed rank targets, eliminating
!! the SELECT RANK requirement.
!!
!! By definition it is not certain this is how it will work in the next
!! Fortran standard release.
!!
!! If you have a recent gfortran release compile the following program
!! with
!!
!!     gfortran -std=f202y  point.f90
!!
!! Sample using proposed F202y feature
!!
!!       program proposed
!!       implicit none
!!       integer :: a
!!       integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)
!!
!!          a=0
!!          call wanted ( a, b0 )
!!          print *, 'a=', a, 'b0=', b0
!!          call wanted ( a, b1 )
!!          print *, 'a=', a, 'b1=', b1
!!          call wanted ( a, b2 )
!!          print *, 'a=', a, 'b2=', b2
!!          call wanted ( a, b3 )
!!          print *, 'a=', a, 'b3=', b3
!!
!!       contains
!!
!!       subroutine wanted( a, b)
!!       ! NOTE: This technique uses pointer rank remapping (introduced in
!!       ! Fortran 2003 and expanded in Fortran 2008), which requires the
!!       ! multi-dimensional target array be simply contiguous.
!!       integer, intent(inout)                  :: a
!!       integer,target, contiguous, intent(out) :: b(..)
!!       integer                                 :: n
!!       integer,pointer                         :: p_b(:)
!!          ! NOTE: The assumed rank target is an experimental F202y feature.
!!          p_b(1:n)=>b  ! will this be allowed outside of SELECT RANK
!!                       ! and work with a scalar?
!!          ! NOTE: Explicit bounds are required. That is, you must
!!          ! specify the explicit upper and lower bounds
!!          ! on the left-hand side of the pointer assignment.
!!          do n=1,size(b)
!!             a = a + 1
!!             p_b(n) = a
!!          enddo
!!       end subroutine wanted
!!
!!       end program proposed
!!
!!##SUMMARY
!!
!! The FLATTEN(3) procedure generically allows for multi-dimensional
!! arrays to be accessed as flattened arrays efficiently without having
!! to copy the data to and from other shapes.
!!
!! Consider other methods carefully to decide on whether an alternative to
!! FLATTEN(3) is more appropriate:
!!
!!  o elemental procedures
!!  o generic procedures
!!  + assumed rank arrays and SELECT CASE.
!!  o You can create a flattened copy of the arrays and pass the temporary
!!    and then store it back into the original, which can be lot of overhead.
!!  o use of intrinsics such as TRANSFER(3), RESHAPE(3), PACK(3), UNPACK(3)
!!    are often useful when transferring data to variables with a different shape.
!!  o (legacy) sequence association.
!!    Allowing argument rank mismatch was a de-facto standard behavior but never part of the standard
!!    so you generally need a compiler option to allow legacy behavior even if using an assumed size
!!    array. Probably should be avoided in new code.
!!  o (proposed) pointer rank remapping to an assumed rank target
!!    available on some compilers as an experimental F202Y feature
!!
!! For most cases you do not have to do handle very many ranks or types and kinds, so
!! generally the standard elemental, generic, and assume rank arrays are more reasonable
!! as long as you do not fall into the trap of making an interface for every type and
!! rank possible when there are only a few arguments. Where FLATTEN(3) is particularly
!! useful is when there are many arguments on a procedure, with multiple arguments needing
!! to support independent ranks.
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     MIT
module M_flatten
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64
!implicit none(type,external)
implicit none
private
public :: flatten
interface flatten
   module procedure flatten_int32
   module procedure flatten_int64
   module procedure flatten_real32
   module procedure flatten_real64
end interface flatten
contains

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     flatten(3f) - [M_flatten::array] Module for dealing with argument
!!     rank mismatch
!!     (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!
!!    function flatten(a) result(b)
!!
!!     integer(kind=int32),intent(in),contiguous,target :: a(..)
!!     ! or
!!     integer(kind=int64),intent(in),contiguous,target :: a(..)
!!     ! or
!!     real(kind=real32),intent(in),contiguous,target   :: a(..)
!!     ! or
!!     real(kind=real64),intent(in),contiguous,target   :: a(..)
!!
!!     type(TYPE,kind=KIND),pointer,intent(out) :: b(:)
!!
!!##CHARACTERISTICS
!!
!!     The type of the returned pointer is the same type as the input
!!     parameter A.
!!
!!
!!##DESCRIPTION
!!
!! FLATTEN(3) returns a rank one array pointer to a scalar or multi-dimensional
!! array
!!
!! Standard methods for passing arguments of different rank are suitable for
!! many cases but when procedures have many arguments with independently
!! variable ranks techniques such as generic procedures can require excessive
!! duplication.
!!
!! The FLATTEN(3) procedure provided here can simply such interfaces
!! particularly when contiguous data is being passed.
!!
!! The M_flatten module provides the procedure FLATTEN(3) which
!! provides a function that returns a rank one array pointer which points
!! to a scalar or an array of any shape.
!!
!! The FLATTEN(3) procedure generically allows for multi-dimensional
!! arrays to be accessed as flattened arrays efficiently without having to
!! copy the data to and from other shapes.
!!
!! Consider other methods carefully to decide on whether an alternative to
!! FLATTEN(3) is more appropriate:
!!
!!  o elemental procedures
!!  o generic procedures
!!  o assumed rank arrays and SELECT CASE.
!!  o You can create a flattened copy of the arrays and pass the temporary
!!    and then store it back into the original, which can be lot of overhead.
!!  o use of intrinsics such as TRANSFER(3), RESHAPE(3), PACK(3),
!!    UNPACK(3) are often useful when transfering data to variables with
!!    a different shape.
!!  o (legacy) sequence association. Allowing argument rank mismatch was
!!    a de-facto standard behavior but never part of the standard
!!    so you generally need a compiler option to allow legacy behavior even
!!    if using an assumed size array. Probably should be avoided in new code.
!!  o (proposed) pointer rank remapping to an assumed rank target
!!    available on some compilers as an experimental F202Y feature
!!
!! For most cases you do not have to do handle very many ranks or types
!! and kinds, so generally the standard elemental, generic, and assume rank
!! arrays are more reasonable as long as you do not fall into the trap of
!! making an interface for every type and rank possible when there are
!! only a few arguments. Where FLATTEN(3) is particularly useful is
!! when there are many arguments on a procedure, with multiple arguments
!! needing to support independent ranks.
!!
!!
!!##EXAMPLES
!!
!!
!!  Sample program:
!!
!!      program demo_flatten
!!      use M_flatten, only : flatten
!!      implicit none
!!      integer :: a
!!      integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)
!!         !
!!         write(*,*)'WANTED:'
!!         a=0
!!         call wanted ( a, b0 )
!!         print *, 'a=', a, 'b0=', b0
!!         call wanted ( a, b1 )
!!         print *, 'a=', a, 'b1=', b1
!!         call wanted ( a, b2 )
!!         print *, 'a=', a, 'b2=', b2
!!         call wanted ( a, b3 )
!!         print *, 'a=', a, 'b3=', b3
!!         !
!!         write(*,*)'WANTED1:'
!!         ! Alternatively, to avoid using pointers directly
!!         ! write the called routine to expect a flattened
!!         ! array and call the argument with flatten().
!!         !
!!         a=0
!!         call wanted1 ( a, flatten(b0) )
!!         print *, 'a=', a, 'b0=', b0
!!         call wanted1 ( a, flatten(b1) )
!!         print *, 'a=', a, 'b1=', b1
!!         call wanted1 ( a, flatten(b2) )
!!         print *, 'a=', a, 'b2=', b2
!!         call wanted1 ( a, flatten(b3) )
!!         print *, 'a=', a, 'b3=', b3
!!      contains
!!      subroutine wanted1( a, b)
!!      integer, intent(inout) :: a
!!      integer, intent(out)   :: b(:)
!!      integer                :: i
!!         do i=1,size(b)
!!            a = a + 1
!!            b(i) = a
!!         enddo
!!      end subroutine wanted1
!!      !
!!      subroutine wanted( a, b)
!!      ! This technique is known as pointer rank remapping (introduced in
!!      ! Fortran 2003 and expanded in Fortran 2008).
!!      ! requires the multi-dimensional target array is simply contiguous.
!!      integer, intent(inout)                  :: a
!!      integer,target, contiguous, intent(out) :: b(..)
!!      integer                                 :: i
!!      integer,pointer                         :: p_b(:)
!!         p_b=>flatten(b)
!!         do i=1,size(b)
!!            a = a + 1
!!            p_b(i) = a
!!         enddo
!!      end subroutine wanted
!!      !
!!      end program demo_flatten
!!
!!   Results:
!!
!!    >  WANTED:
!!    >  a=           1 b0=  1
!!    >  a=           4 b1=  2    3       4
!!    >  a=           8 b2=  5    6       7       8
!!    >  a=          12 b3=  9   10      11      12
!!    >  WANTED1:
!!    >  a=           1 b0=  1
!!    >  a=           4 b1=  2    3       4
!!    >  a=           8 b2=  5    6       7       8
!!    >  a=          12 b3=  9   10      11      12
!!
!!##AUTHOR
!!     John S. Urban
!!
!!##LICENSE
!!     MIT
function flatten_int32(arr) result(p_arr)
use, intrinsic :: iso_c_binding
integer(kind=int32),target, contiguous :: arr(..)
integer(kind=int32),pointer            :: p_arr(:)
integer                                :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr
   rank (2);     p_arr(1:n)=>arr
   rank (3);     p_arr(1:n)=>arr
   rank (4);     p_arr(1:n)=>arr
   rank (5);     p_arr(1:n)=>arr
   rank (6);     p_arr(1:n)=>arr
   rank (7);     p_arr(1:n)=>arr
   rank (8);     p_arr(1:n)=>arr
   rank (9);     p_arr(1:n)=>arr
   rank (10);    p_arr(1:n)=>arr
   rank (11);    p_arr(1:n)=>arr
   rank (12);    p_arr(1:n)=>arr
   rank (13);    p_arr(1:n)=>arr
   rank (14);    p_arr(1:n)=>arr
   rank (15);    p_arr(1:n)=>arr
   rank (*)
   print *, 'assumed size is unsupported'
   rank default
   print *, 'unsupported rank'
   end select
end function flatten_int32

function flatten_int64(arr) result(p_arr)
use, intrinsic :: iso_c_binding
integer(kind=int64),target, contiguous :: arr(..)
integer(kind=int64),pointer            :: p_arr(:)
integer                                :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr
   rank (2);     p_arr(1:n)=>arr
   rank (3);     p_arr(1:n)=>arr
   rank (4);     p_arr(1:n)=>arr
   rank (5);     p_arr(1:n)=>arr
   rank (6);     p_arr(1:n)=>arr
   rank (7);     p_arr(1:n)=>arr
   rank (8);     p_arr(1:n)=>arr
   rank (9);     p_arr(1:n)=>arr
   rank (10);    p_arr(1:n)=>arr
   rank (11);    p_arr(1:n)=>arr
   rank (12);    p_arr(1:n)=>arr
   rank (13);    p_arr(1:n)=>arr
   rank (14);    p_arr(1:n)=>arr
   rank (15);    p_arr(1:n)=>arr
   rank (*)
   print *, 'assumed size is unsupported'
   rank default
   print *, 'unsupported rank'
   end select
end function flatten_int64

function flatten_real32(arr) result(p_arr)
use, intrinsic :: iso_c_binding
real(kind=real32),target, contiguous :: arr(..)
real(kind=real32),pointer            :: p_arr(:)
integer                              :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr
   rank (2);     p_arr(1:n)=>arr
   rank (3);     p_arr(1:n)=>arr
   rank (4);     p_arr(1:n)=>arr
   rank (5);     p_arr(1:n)=>arr
   rank (6);     p_arr(1:n)=>arr
   rank (7);     p_arr(1:n)=>arr
   rank (8);     p_arr(1:n)=>arr
   rank (9);     p_arr(1:n)=>arr
   rank (10);    p_arr(1:n)=>arr
   rank (11);    p_arr(1:n)=>arr
   rank (12);    p_arr(1:n)=>arr
   rank (13);    p_arr(1:n)=>arr
   rank (14);    p_arr(1:n)=>arr
   rank (15);    p_arr(1:n)=>arr
   rank (*)
   print *, 'assumed size is unsupported'
   rank default
   print *, 'unsupported rank'
   end select
end function flatten_real32

function flatten_real64(arr) result(p_arr)
use, intrinsic :: iso_c_binding
real(kind=real64),target, contiguous :: arr(..)
real(kind=real64),pointer            :: p_arr(:)
integer                              :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr
   rank (2);     p_arr(1:n)=>arr
   rank (3);     p_arr(1:n)=>arr
   rank (4);     p_arr(1:n)=>arr
   rank (5);     p_arr(1:n)=>arr
   rank (6);     p_arr(1:n)=>arr
   rank (7);     p_arr(1:n)=>arr
   rank (8);     p_arr(1:n)=>arr
   rank (9);     p_arr(1:n)=>arr
   rank (10);    p_arr(1:n)=>arr
   rank (11);    p_arr(1:n)=>arr
   rank (12);    p_arr(1:n)=>arr
   rank (13);    p_arr(1:n)=>arr
   rank (14);    p_arr(1:n)=>arr
   rank (15);    p_arr(1:n)=>arr
   rank (*)
   print *, 'assumed size is unsupported'
   rank default
   print *, 'unsupported rank'
   end select
end function flatten_real64

end module M_flatten

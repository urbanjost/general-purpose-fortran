module M_sets
!>
!!##NAME
!!    M_sets(3f) - [M_sets::INTRO] functions reminiscent of Matlab set
!!    functions
!!
!!##SYNOPSIS
!!
!!    Procedure names and syntax:
!!
!!     use M_sets, only : &
!!     union, unique, intersect, setdiff, ismember, setxor
!!     use M_sets, only : &
!!     issorted
!!
!!##DESCRIPTION
!!
!! Set operations compare the elements in two sets to find commonalities
!! or differences. This includes Unions, Intersections, and Membership.
!!
!! M_set(3f) is a Fortran module comprising a small subset of set theory
!! functions reminiscent of Matlab functions.
!!
!! The functions currently support vectors of integer, default character,
!! and default real and doubleprecision type.
!!
!! float numbers (both kind=real32 and kind=real64) are allowed but "caveat
!! emptor", as comparing floats for equality has issues. You may have to
!! condition the float data by converting it to scaled integers or using
!! intrinsics such as NEAREST(3f) to produce the desired results.
!!
!! M_set(3f) primarily uses simple calls to the M_orderpack(3f) module to
!! provide the functionality. The functions  are not otherwise tuned for
!! performance and make loose use of memory allocation but are sufficient
!! for most uses, simple to use, and familiar to a large base of users.
!!
!! ## Functions
!!  + union(A,B,setOrder)     - Join two sets and remove duplicates of values
!!  + unique(A,setOrder)      - Remove duplicates of values from a set
!!  + intersect(A,B,setOrder) - Find the values common to both A and B
!!  + setdiff(A,B,setOrder)   - Find the values in A that are not in B
!!  + ismember(A,B,setOrder)  - Create a mask of A marking elements also in B
!!  + setxor(A,B,setOrder)    - Find values of A and B not in both arrays
!!  + issorted(A)             - Determine if array is already sorted
!!
!!  The subsequent data may be produced sorted, or left in the order
!!  encountered.
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_M_sets
!!    use M_sets, only: &
!!    & unique, intersect, union, setdiff, ismember, setxor, issorted
!!    character(len=*),parameter :: all='(*(g0,1x))'
!!    character(len=*),parameter :: nl=new_line('A')
!!    integer, allocatable      :: A(:)
!!    integer, allocatable      :: B(:)
!!    integer, allocatable      :: C(:)
!!
!!       A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
!!       !
!!       print all                                                   ,nl, &
!!       'UNIQUE','Find the unique elements of vector A.'            ,nl, &
!!       'A=', A                                                     ,nl, &
!!       'sorted=',unique(A)                                         ,nl, &
!!       'stable=',unique(A, setOrder='stable')
!!
!!       A=[5, 7, 1]
!!       B=[3, 1, 1]
!!       !
!!       print all                                                   ,nl, &
!!       'UNION', 'Find the union of vectors A and B.'               ,nl, &
!!       'A=', A                                                     ,nl, &
!!       'B=', B                                                     ,nl, &
!!       'sorted=',union(A, B, 'sorted')                             ,nl, &
!!       'stable=',union(A, B, 'stable')
!!
!!       A=[7, 1, 7, 7, 4]
!!       B=[7, 0, 4, 4, 0]
!!       !
!!       print all                                                   ,nl, &
!!       'INTERSECT', 'Find the values common to both A and B.'      ,nl, &
!!       'A=', A                                                     ,nl, &
!!       'B=', B                                                     ,nl, &
!!       'sorted=',intersect(A, B)                                   ,nl, &
!!       'stable=',intersect(A, B, setOrder='stable')
!!
!!       A=[3, 6, 2, 1, 5, 1, 1]
!!       B=[2, 4, 6]
!!       !
!!       print all                                                   ,nl, &
!!       'SETDIFF','Find the values in A that are not in B.'         ,nl, &
!!       'A=', A                                                     ,nl, &
!!       'B=', B                                                     ,nl, &
!!       'sorted=',setdiff(A, B, 'sorted')                           ,nl, &
!!       'stable=',setdiff(A, B, 'stable')
!!
!!       A=[5,3,4,2]
!!       B=[2,4,4,4,6,8]
!!       !
!!       print all                                                   ,nl, &
!!       'ISMEMBER','Determine which elements of A are also in B.'   ,nl, &
!!       'A=', A                                                     ,nl, &
!!       'B=', B                                                     ,nl, &
!!       'in A and B=',ismember(A,B)
!!
!!       A=[5,1,3,3,3]
!!       B=[4,1,2]
!!       !
!!       print all                                                   ,nl, &
!!       'SETXOR'                                                       , &
!!       'Find values of A and B not in their intersection.'         ,nl, &
!!       'A=', A                                                     ,nl, &
!!       'B=', B                                                     ,nl, &
!!       'sorted=',setxor(A,B)                                       ,nl, &
!!       'stable=',setxor(A,B,'stable')
!!
!!       A=[1,2,3,4,5]
!!       B=[5,4,3,2,1]
!!       !
!!       print all                                                   ,nl, &
!!       'ISSSORTED'                                                    , &
!!       'confirm whether array is sorted in ascending order or not' ,nl, &
!!       'A=', A                                                     ,nl, &
!!       'B=', B                                                     ,nl, &
!!       'is A sorted?',issorted(A)                                  ,nl, &
!!       'is B sorted?',issorted(B)
!!
!!    end program demo_M_sets
!!
!! Results:
!!
!!  >
!!  >  UNIQUE Find the unique elements of vector A.
!!  >  A= 10 -10 0 1 2 3 3 2 1 -10
!!  >  sorted= -10 0 1 2 3 10
!!  >  stable= 10 -10 0 1 2 3
!!  >
!!  >  UNION Find the union of vectors A and B.
!!  >  A= 5 7 1
!!  >  B= 3 1 1
!!  >  sorted= 1 3 5 7
!!  >  stable= 5 7 1 3
!!  >
!!  >  INTERSECT Find the values common to both A and B.
!!  >  A= 7 1 7 7 4
!!  >  B= 7 0 4 4 0
!!  >  sorted= 4 7
!!  >  stable= 7 4
!!  >
!!  >  SETDIFF Find the values in A that are not in B.
!!  >  A= 3 6 2 1 5 1 1
!!  >  B= 2 4 6
!!  >  sorted= 1 3 5
!!  >  stable= 3 1 5
!!  >
!!  >  ISMEMBER Determine which elements of A are also in B.
!!  >  A= 5 3 4 2
!!  >  B= 2 4 4 4 6 8
!!  >  in A and B= 0 0 1 1
!!  >
!!  >  SETXOR Find values of A and B not in their intersection.
!!  >  A= 5 1 3 3 3
!!  >  B= 4 1 2
!!  >  sorted= 2 3 4 5
!!  >  stable= 5 3 4 2
!!  >
!!  >  ISSSORTED confirm whether array is sorted in ascending order or not
!!  >  A= 1 2 3 4 5
!!  >  B= 5 4 3 2 1
!!  >  is A sorted? 1
!!  >  is B sorted? 0
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
! a subset of the functionality of set functions in Matlab are provided.
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32,real64,real128
use M_orderpack, only: unique_ => unique, occurrences_ => occurrences, sort_ => sort
use M_orderpack, only: rank_unique_ => rank_unique

private

public :: unique    ! C = unique(A,setOrder)      returns the same data as in A, but with no repetitions.
public :: union     ! C = union(A,B,setOrder)     returns the combined data from A and B with no repetitions.
public :: intersect ! C = intersect(A,B,setOrder) returns the data common to both A and B, with no repetitions.
public :: setdiff   ! C = setdiff(A,B,setOrder)   returns the data in A that is not in B, with no repetitions.
public :: ismember  ! C = ismember(A,B) returns   an array containing 1 (true) where the data in A is found in B. Elsewhere, 0.
public :: setxor    ! C = setxor(A,B,setOrder)    returns the data of A and B that are not in their intersection
                    !                             (the symmetric difference), with no repetitions. That is, setxor returns the
                    !                             data that occurs in A or B, but not both. C is in sorted order.
public :: issorted  ! C = issorted(A)             determine if A is in ascending order or not


interface unique;    module procedure unique_c;    end interface unique
interface union;     module procedure union_c;     end interface union
interface intersect; module procedure intersect_c; end interface intersect
interface setdiff;   module procedure setdiff_c;   end interface setdiff
interface ismember;  module procedure ismember_c;  end interface ismember
interface setxor;    module procedure setxor_c;    end interface setxor
interface issorted;  module procedure issorted_c;  end interface issorted

interface unique;    module procedure unique_int8;    end interface unique
interface union;     module procedure union_int8;     end interface union
interface intersect; module procedure intersect_int8; end interface intersect
interface setdiff;   module procedure setdiff_int8;   end interface setdiff
interface ismember;  module procedure ismember_int8;  end interface ismember
interface setxor;    module procedure setxor_int8;    end interface setxor
interface issorted;  module procedure issorted_int8;  end interface issorted

interface unique;    module procedure unique_int16;    end interface unique
interface union;     module procedure union_int16;     end interface union
interface intersect; module procedure intersect_int16; end interface intersect
interface setdiff;   module procedure setdiff_int16;   end interface setdiff
interface ismember;  module procedure ismember_int16;  end interface ismember
interface setxor;    module procedure setxor_int16;    end interface setxor
interface issorted;  module procedure issorted_int16;  end interface issorted

interface unique;    module procedure unique_int32;    end interface unique
interface union;     module procedure union_int32;     end interface union
interface intersect; module procedure intersect_int32; end interface intersect
interface setdiff;   module procedure setdiff_int32;   end interface setdiff
interface ismember;  module procedure ismember_int32;  end interface ismember
interface setxor;    module procedure setxor_int32;    end interface setxor
interface issorted;  module procedure issorted_int32;  end interface issorted

interface unique;    module procedure unique_int64;    end interface unique
interface union;     module procedure union_int64;     end interface union
interface intersect; module procedure intersect_int64; end interface intersect
interface setdiff;   module procedure setdiff_int64;   end interface setdiff
interface ismember;  module procedure ismember_int64;  end interface ismember
interface setxor;    module procedure setxor_int64;    end interface setxor
interface issorted;  module procedure issorted_int64;  end interface issorted

interface unique;    module procedure unique_real32;    end interface unique
interface union;     module procedure union_real32;     end interface union
interface intersect; module procedure intersect_real32; end interface intersect
interface setdiff;   module procedure setdiff_real32;   end interface setdiff
interface ismember;  module procedure ismember_real32;  end interface ismember
interface setxor;    module procedure setxor_real32;    end interface setxor
interface issorted;  module procedure issorted_real32;  end interface issorted

interface unique;    module procedure unique_real64;    end interface unique
interface union;     module procedure union_real64;     end interface union
interface intersect; module procedure intersect_real64; end interface intersect
interface setdiff;   module procedure setdiff_real64;   end interface setdiff
interface ismember;  module procedure ismember_real64;  end interface ismember
interface setxor;    module procedure setxor_real64;    end interface setxor
interface issorted;  module procedure issorted_real64;  end interface issorted

contains

!>
!!##NAME
!!    unique(3f) - [M_sets] return unique values in array A
!!
!!##SYNOPSIS
!!
!!
!!    unique(A,setOrder)
!!
!!##DESCRIPTION
!!
!! unique(3) returns the unique values found in an array. That is, it
!! eliminates all but one occurrence of each value.
!!
!! The result is in sorted order by default, but may be returned in the
!! order found.
!!
!!##OPTIONS
!!
!!     A         input array to extract unique values from
!!     setOrder  May be "sort" or "stable". If "stable" the values are
!!               returned in the order discovered. The default is
!!               "sorted", which returns the data in ascending order.
!!
!!##RETURNS
!!
!!     All the values that occur in the input occur in the output
!!     just once. All duplicates are removed.
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_unique
!!    use M_sets, only: unique
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer,allocatable        :: A(:)
!!
!!       write(*,g) 'UNIQUE','Find the unique elements of vector A.'
!!        A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
!!        write(*,g) 'A=', A
!!        write(*,g) unique(A)
!!        write(*,g) unique(A, setOrder='stable')
!!
!!    end program demo_unique
!!
!! Results:
!!
!!  > UNIQUE Find the unique elements of vector A.
!!  > A= 10 -10 0 1 2 3 3 2 1 -10
!!  > -10 0 1 2 3 10
!!  > 10 -10 0 1 2 3
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
!-----------------------------------------------------------------------------------------------------------------------------------
function unique_c(A, setOrder) result(answer)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
character(len=*), intent(in)           :: A(:)
character(len=*), intent(in), optional :: setOrder
character(len=:), allocatable          :: answer(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni
   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   endif
   answer = A
   call unique_(answer, nuni)
   answer = answer(:nuni)
   select case (lower(setOrder_))
   case ('stable')
   case ('sorted')
      call sort_(answer)
   case default
      stop '*unique* unknown setOrder '//setOrder_//' allowed are "stable" and "sorted"'
   end select
end function unique_c
!>
!!##NAME
!!    union(3f) - [M_sets] Join two sets and removes duplicates of values
!!
!!##SYNOPSIS
!!
!!
!!    union(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! The two sets are combined and repetitions are removed.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable". If "stable" the values are
!!               returned in the order discovered. The default is
!!               "sorted", which returns the data in ascending order.
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_union
!!    use M_sets, only: union
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer,allocatable        :: A(:)
!!    integer,allocatable        :: B(:)
!!
!!       write(*,g) 'UNION', 'Find the union of vectors A and B.'
!!       A=[5, 7, 1]
!!       B=[3, 1, 1]
!!       write(*,g) 'A=', A
!!       write(*,g) 'B=', B
!!       write(*,g) union(A,B)
!!
!!       A=[5, 5, 3]
!!       B=[1, 2, 5]
!!       write(*,g) 'A=', A
!!       write(*,g) 'B=', B
!!       write(*,g) union(A, B, 'sorted')
!!       write(*,g) union(A, B, 'stable')
!!
!!    end program demo_union
!! ```
!! Results:
!!
!!  > UNION Find the union of vectors A and B.
!!  > A= 5 7 1
!!  > B= 3 1 1
!!  > 1 3 5 7
!!  > A= 5 5 3
!!  > B= 1 2 5
!!  > 1 2 3 5
!!  > 5 3 1 2
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
!-----------------------------------------------------------------------------------------------------------------------------------
function union_c(A, B, setOrder) result(answer)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
character(len=*), intent(in)           :: A(:)
character(len=*), intent(in)           :: B(:)
character(len=*), intent(in), optional :: setOrder
character(len=:), allocatable          :: answer(:)
character(len=:), allocatable          :: kludge(:)
integer                                :: longest
longest=max(len(a),len(b))
   kludge = [character(len=longest) :: A, B]
   answer = unique(kludge, setOrder)
end function union_c
!>
!!##NAME
!!    intersect(3f) - [M_sets] Find the values common to both sets A and B
!!
!!##SYNOPSIS
!!
!!
!!    intersect(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! The values that occur at least once in each set are returned.
!!
!! That is, intersect(3f) returns the data common to both A and B, with
!! no repetitions.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable". If "stable" the values are
!!               returned in the order discovered. The default is
!!               "sorted", which returns the data in ascending order.
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!   program demo_intersect
!!   use M_sets, only: unique, intersect, union, setdiff, ismember, setxor
!!   character(len=*),parameter :: g='(*(g0,1x))'
!!   integer, allocatable      :: A(:)
!!   integer, allocatable      :: B(:)
!!
!!      write(*,g) 'INTERSECT', 'Find the values common to both A and B.'
!!       A=[7, 1, 7, 7, 4]
!!       B=[7, 0, 4, 4, 0]
!!       write(*,g) 'A=', A
!!       write(*,g) 'B=', B
!!       write(*,g) intersect(A, B)
!!       write(*,g) intersect(A, B, setOrder='stable')
!!   end program demo_intersect
!!
!! Results:
!!
!!  > INTERSECT Find the values common to both A and B.
!!  > A= 7 1 7 7 4
!!  > B= 7 0 4 4 0
!!  > 4 7
!!  > 7 4
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
!-----------------------------------------------------------------------------------------------------------------------------------
function intersect_c(A, B, setOrder) result(answer)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
character(len=*), intent(in)           :: A(:)
character(len=*), intent(in)           :: B(:)
character(len=*), intent(in), optional :: setOrder
character(len=:), allocatable          :: answer(:)
integer, allocatable                   :: iwrk(:)
character(len=:), allocatable          :: kludge(:)
integer                                :: longest
   longest=max(len(A),len(B))
   answer = [character(len=longest) :: unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   !--------------------------------
   !answer= unique(pack(answer//'',iwrk > 1),setOrder) ! add //' ' to avoid gfortran 13.1.0-8 bug
   kludge=pack(answer//'',iwrk > 1)
   answer = unique(kludge, setOrder)
   !--------------------------------
end function intersect_c
!>
!!##NAME
!!    setdiff(3f) - [M_sets] Find the values in A that are not in B
!!
!!##SYNOPSIS
!!
!!
!!    setdiff(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! setdiff(3f) returns the data in A that is not in B, with no repetitions.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable". If "stable" the values are
!!               returned in the order discovered. The default is
!!               "sorted", which returns the data in ascending order.
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_setdiff
!!    use M_sets, only: setdiff
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer, allocatable      :: A(:)
!!    integer, allocatable      :: B(:)
!!
!!       write(*,g) 'SETDIFF','Find the values in A that are not in B.'
!!        A=[3, 6, 2, 1, 5, 1, 1]
!!        B=[2, 4, 6]
!!        write(*,g) 'A=', A
!!        write(*,g) 'B=', B
!!        write(*,g) setdiff(A, B)
!!        write(*,g) setdiff([4, 1, 3, 2, 5], [2, 1], 'sorted')
!!        write(*,g) setdiff([4, 1, 3, 2, 5], [2, 1], 'stable')
!!
!!    end program demo_setdiff
!!
!! Results:
!!
!!  > SETDIFF Find the values in A that are not in B.
!!  > A= 3 6 2 1 5 1 1
!!  > B= 2 4 6
!!  > 1 3 5
!!  > 3 4 5
!!  > 4 3 5
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
!-----------------------------------------------------------------------------------------------------------------------------------
function setdiff_c(A, B, setOrder) result(answer)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
character(len=*), intent(in)           :: a(:)
character(len=*), intent(in)           :: b(:)
character(len=*), intent(in), optional :: setOrder
character(len=:), allocatable          :: answer(:),kludge(:)
integer         , allocatable          :: iwrk(:)
integer                                :: longest
   answer = unique(b, setOrder='stable')
   longest=max(len(a),len(b))
   answer = [character(len=longest) :: unique(a, setOrder='stable'), answer, answer] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate ( iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   !--------------------------------
   !answer = unique(pack(answer, iwrk  ==  1), setOrder)
   kludge = pack(answer//'', iwrk  ==  1)
   answer = unique(kludge, setOrder)
   !--------------------------------

end function setdiff_c
!>
!!##NAME
!!    ismember(3f) - [M_sets] Create a mask of A marking elements also in B
!!
!!##SYNOPSIS
!!
!!
!!    ismember(A,B)
!!
!!##DESCRIPTION
!!
!! Identifies elements of the first set that are members of the second set
!! as well.
!!
!! The returned array is a mask of the first array containing a 1
!! (aka. "true") where the data in A is found in B. Elsewhere, the array
!! contains 0 (aka. "false").
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array of values to find in vector A.
!!
!!##RETURNS
!!     A mask of array A with a 1 at locations where the value in that
!!     position in A is also a value that occurs in B, and with a 0 at
!!     locations where that value in A was not found in B.
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_ismember
!!    use M_sets, only: ismember
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer, allocatable      :: A(:)
!!    integer, allocatable      :: B(:)
!!
!!       write(*,g) 'ISMEMBER', 'Determine which elements of A are also in B.'
!!
!!        A=[5,3,4,2]
!!        B=[2,4,4,4,6,8]
!!        write(*,g) 'A=', A
!!        write(*,g) 'B=', B
!!        write(*,g) ismember(A,B)
!!
!!    end program demo_ismember
!!
!! Results:
!!
!!  > ISMEMBER Determine which elements of A are also in B.
!!  > A= 5 3 4 2
!!  > B= 2 4 4 4 6 8
!!  > 0 0 1 1
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
!-----------------------------------------------------------------------------------------------------------------------------------
function ismember_c(A, B) result(answer)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
character(len=*), intent(in)   :: a(:)
character(len=*), intent(in)   :: b(:)
integer         , allocatable  :: answer(:)
integer         , allocatable  :: iwrk1(:)
integer         , allocatable  :: iwrk2(:)
character(len=:), allocatable  :: cwrk1(:)
integer                        :: inums
integer                        :: longest
   longest=max(len(a),len(b))
   inums=size(a)
   cwrk1 = [ character(len=longest) :: a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(cwrk1)))
   call occurrences_(cwrk1, iwrk1)
   call occurrences_(a, iwrk2)
   answer=iwrk1(:inums)-iwrk2
   answer=merge(0,1,answer == 0)

end function ismember_c
!>
!!##NAME
!!    setxor(3f) - [M_sets] Find values of A and B not in both arrays
!!
!!##SYNOPSIS
!!
!!
!!    setxor(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! setxfor(3f) returns the exclusive OR of two arrays.  That is, it returns
!! the data of A and B that are not in their intersection (the symmetric
!! difference), with no repetitions.
!!
!! Another way of defining the result is that setxor(3f) returns the data
!! that occurs in A or B, but not both.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable". If "stable" the values are
!!               returned in the order discovered. The default is
!!               "sorted", which returns the data in ascending order.
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_setxor
!!    use M_sets, only: setxor
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer, allocatable      :: A(:)
!!    integer, allocatable      :: B(:)
!!
!!       write(*,g) 'SETXOR','Find values of A and B not in their intersection.'
!!       A = [5,1,3,3,3]
!!       B = [4,1,2]
!!       write(*,g) 'A=', A
!!       write(*,g) 'A=', B
!!       write(*,g) setxor(A,B)
!!       write(*,g) setxor(A,B,'stable')
!!
!!    end program demo_setxor
!!
!! Results:
!!
!!  > SETXOR Find values of A and B not in their intersection.
!!  > A= 5 1 3 3 3
!!  > A= 4 1 2
!!  > 2 3 4 5
!!  > 5 3 4 2
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
!-----------------------------------------------------------------------------------------------------------------------------------
function setxor_c(A, B, setOrder) result(answer)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
character(len=*), intent(in)           :: a(:)
character(len=*), intent(in)           :: b(:)
character(len=*), intent(in), optional :: setOrder
character(len=:), allocatable          :: answer(:), kludge(:)
integer         , allocatable          :: iwrk1(:)
integer         , allocatable          :: iwrk2(:)
integer         , allocatable          :: iwrk3(:)
integer                                :: inums
integer                                :: longest
   inums=size(a)
   longest=max(len(a),len(b))

   answer = [character(len=longest) ::  a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(answer)))
   call occurrences_(answer, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   kludge=pack(answer//'',iwrk1 == 0)
   answer=unique(kludge,setOrder)

end function setxor_c
!>
!!##NAME
!!    issorted(3f) - [M_sets] Report if A is sorted in ascending order or not.
!!
!!##SYNOPSIS
!!
!!
!!    issorted(A,setOrder)
!!
!!##DESCRIPTION
!!
!!    Report if A is sorted in ascending order or not.  A 1 (true) is
!!    returned when the elements of A are listed in ascending order and 0
!!    (false) otherwise.
!!
!!##OPTIONS
!!
!!     A     input array to test
!!
!!##RETURNS
!!
!!     1 if input array A is sorted in ascending order, 0 otherwise
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_issorted
!!    use M_sets, only: issorted
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer, allocatable      :: A(:)
!!
!!       write(*,g) 'ISSORTED','Find the issorted elements of vector A.'
!!        A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
!!        write(*,g) 'A=', A
!!        write(*,g) issorted(A)
!!        A = [-10, 10, 100, 201]
!!        write(*,g) 'A=', A
!!        write(*,g) issorted(A)
!!
!!    end program demo_issorted
!!
!! Results:
!!
!!  > ISSORTED Find the issorted elements of vector A.
!!  > A= 10 -10 0 1 2 3 3 2 1 -10
!!  > 0
!!  > A= -10 10 100 201
!!  > 1
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
!-----------------------------------------------------------------------------------------------------------------------------------
function issorted_c(A) result(answer)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
character(len=*), intent(in) :: A(:)
integer                      :: answer
integer                      :: i
answer=1
do i=1,size(a)-1
   if(A(i) > A(i+1))then
      answer=0
      exit
   endif
enddo
end function issorted_c
elemental pure function lower(str) result (string)

! ident_1="@(#) M_strings lower(3f) Changes a string to lowercase"

character(*), intent(in)    :: str
character(len(str))         :: string
integer                     :: i
integer,parameter           :: diff = iachar('A')-iachar('a')

   string = str

   do concurrent (i = 1:len_trim(str))                ! step thru each letter in the string in specified range

      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = achar(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select

   enddo

end function lower



function unique_int8(A, setOrder) result(answer)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
integer(kind=int8), intent(in)                    :: A(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int8),allocatable      :: answer(:)
integer,allocatable                    :: indices(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni

   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   endif
   answer = A
   select case (lower(setOrder_))
   case ('stable')
      call unique_(answer, nuni)
      answer = answer(:nuni)
   case ('sorted')
      if (allocated(indices)) deallocate (indices)
      allocate (indices(size(A)))
      call rank_unique_(A, indices, nuni)
      answer = A(indices(:nuni))
   case default
      stop '*unique* unknown setOrder '//setOrder_//'allowed are "stable" and "sorted"'
   end select
end function unique_int8

function union_int8(A, B, setOrder) result(answer)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
integer(kind=int8),intent(in)         :: A(:)
integer(kind=int8),intent(in)         :: B(:)
character(len=*),intent(in), optional  :: setOrder
integer(kind=int8),allocatable        :: answer(:)
   answer = unique([A, B], setOrder)
end function union_int8

function intersect_int8(A, B, setOrder) result(answer)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
integer(kind=int8), intent(in)                    :: A(:)
integer(kind=int8), intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int8), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = [unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  >  1), setOrder)
end function intersect_int8

function setdiff_int8(A, B, setOrder) result(answer)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
integer(kind=int8), intent(in)                    :: a(:)
integer(kind=int8), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int8), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = unique(b, setOrder='stable')
   answer = [unique(a, setOrder='stable'), answer, answer] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  ==  1), setOrder)

end function setdiff_int8

function ismember_int8(A, B) result(answer)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
integer(kind=int8), intent(in)   :: a(:)
integer(kind=int8), intent(in)   :: b(:)
integer(kind=int8), allocatable  :: ab(:)
integer, allocatable                :: answer(:)
integer, allocatable                :: iwrk1(:)
integer, allocatable                :: iwrk2(:)
integer                             :: inums
   inums=size(a)
   ab = [ a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(ab)))
   call occurrences_(ab, iwrk1)
   call occurrences_(a, iwrk2)
   answer=iwrk1(:inums)-iwrk2
   answer=merge(0,1,answer == 0)

end function ismember_int8

function setxor_int8(A, B, setOrder) result(answer)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
integer(kind=int8), intent(in)                    :: a(:)
integer(kind=int8), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int8), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk1(:)
integer, allocatable                                 :: iwrk2(:)
integer, allocatable                                 :: iwrk3(:)
integer                                :: inums
   inums=size(a)

   answer = [ a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(answer)))
   call occurrences_(answer, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   answer=pack(answer,iwrk1 == 0)
   answer=unique(answer,setOrder)

end function setxor_int8

function issorted_int8(A) result(answer)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
integer(kind=int8), intent(in) :: A(:)
integer(kind=int8)             :: answer
integer             :: i
answer=1
do i=1,size(a)-1
   if(A(i) > A(i+1))then
      answer=0
      exit
   endif
enddo
end function issorted_int8

function unique_int16(A, setOrder) result(answer)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
integer(kind=int16), intent(in)                    :: A(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int16),allocatable      :: answer(:)
integer,allocatable                    :: indices(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni

   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   endif
   answer = A
   select case (lower(setOrder_))
   case ('stable')
      call unique_(answer, nuni)
      answer = answer(:nuni)
   case ('sorted')
      if (allocated(indices)) deallocate (indices)
      allocate (indices(size(A)))
      call rank_unique_(A, indices, nuni)
      answer = A(indices(:nuni))
   case default
      stop '*unique* unknown setOrder '//setOrder_//'allowed are "stable" and "sorted"'
   end select
end function unique_int16

function union_int16(A, B, setOrder) result(answer)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
integer(kind=int16),intent(in)         :: A(:)
integer(kind=int16),intent(in)         :: B(:)
character(len=*),intent(in), optional  :: setOrder
integer(kind=int16),allocatable        :: answer(:)
   answer = unique([A, B], setOrder)
end function union_int16

function intersect_int16(A, B, setOrder) result(answer)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
integer(kind=int16), intent(in)                    :: A(:)
integer(kind=int16), intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int16), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = [unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  >  1), setOrder)
end function intersect_int16

function setdiff_int16(A, B, setOrder) result(answer)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
integer(kind=int16), intent(in)                    :: a(:)
integer(kind=int16), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int16), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = unique(b, setOrder='stable')
   answer = [unique(a, setOrder='stable'), answer, answer] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  ==  1), setOrder)

end function setdiff_int16

function ismember_int16(A, B) result(answer)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
integer(kind=int16), intent(in)   :: a(:)
integer(kind=int16), intent(in)   :: b(:)
integer(kind=int16), allocatable  :: ab(:)
integer, allocatable                :: answer(:)
integer, allocatable                :: iwrk1(:)
integer, allocatable                :: iwrk2(:)
integer                             :: inums
   inums=size(a)
   ab = [ a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(ab)))
   call occurrences_(ab, iwrk1)
   call occurrences_(a, iwrk2)
   answer=iwrk1(:inums)-iwrk2
   answer=merge(0,1,answer == 0)

end function ismember_int16

function setxor_int16(A, B, setOrder) result(answer)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
integer(kind=int16), intent(in)                    :: a(:)
integer(kind=int16), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int16), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk1(:)
integer, allocatable                                 :: iwrk2(:)
integer, allocatable                                 :: iwrk3(:)
integer                                :: inums
   inums=size(a)

   answer = [ a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(answer)))
   call occurrences_(answer, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   answer=pack(answer,iwrk1 == 0)
   answer=unique(answer,setOrder)

end function setxor_int16

function issorted_int16(A) result(answer)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
integer(kind=int16), intent(in) :: A(:)
integer(kind=int16)             :: answer
integer             :: i
answer=1
do i=1,size(a)-1
   if(A(i) > A(i+1))then
      answer=0
      exit
   endif
enddo
end function issorted_int16

function unique_int32(A, setOrder) result(answer)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
integer(kind=int32), intent(in)                    :: A(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int32),allocatable      :: answer(:)
integer,allocatable                    :: indices(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni

   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   endif
   answer = A
   select case (lower(setOrder_))
   case ('stable')
      call unique_(answer, nuni)
      answer = answer(:nuni)
   case ('sorted')
      if (allocated(indices)) deallocate (indices)
      allocate (indices(size(A)))
      call rank_unique_(A, indices, nuni)
      answer = A(indices(:nuni))
   case default
      stop '*unique* unknown setOrder '//setOrder_//'allowed are "stable" and "sorted"'
   end select
end function unique_int32

function union_int32(A, B, setOrder) result(answer)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
integer(kind=int32),intent(in)         :: A(:)
integer(kind=int32),intent(in)         :: B(:)
character(len=*),intent(in), optional  :: setOrder
integer(kind=int32),allocatable        :: answer(:)
   answer = unique([A, B], setOrder)
end function union_int32

function intersect_int32(A, B, setOrder) result(answer)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
integer(kind=int32), intent(in)                    :: A(:)
integer(kind=int32), intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int32), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = [unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  >  1), setOrder)
end function intersect_int32

function setdiff_int32(A, B, setOrder) result(answer)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
integer(kind=int32), intent(in)                    :: a(:)
integer(kind=int32), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int32), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = unique(b, setOrder='stable')
   answer = [unique(a, setOrder='stable'), answer, answer] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  ==  1), setOrder)

end function setdiff_int32

function ismember_int32(A, B) result(answer)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
integer(kind=int32), intent(in)   :: a(:)
integer(kind=int32), intent(in)   :: b(:)
integer(kind=int32), allocatable  :: ab(:)
integer, allocatable                :: answer(:)
integer, allocatable                :: iwrk1(:)
integer, allocatable                :: iwrk2(:)
integer                             :: inums
   inums=size(a)
   ab = [ a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(ab)))
   call occurrences_(ab, iwrk1)
   call occurrences_(a, iwrk2)
   answer=iwrk1(:inums)-iwrk2
   answer=merge(0,1,answer == 0)

end function ismember_int32

function setxor_int32(A, B, setOrder) result(answer)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
integer(kind=int32), intent(in)                    :: a(:)
integer(kind=int32), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int32), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk1(:)
integer, allocatable                                 :: iwrk2(:)
integer, allocatable                                 :: iwrk3(:)
integer                                :: inums
   inums=size(a)

   answer = [ a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(answer)))
   call occurrences_(answer, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   answer=pack(answer,iwrk1 == 0)
   answer=unique(answer,setOrder)

end function setxor_int32

function issorted_int32(A) result(answer)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
integer(kind=int32), intent(in) :: A(:)
integer(kind=int32)             :: answer
integer             :: i
answer=1
do i=1,size(a)-1
   if(A(i) > A(i+1))then
      answer=0
      exit
   endif
enddo
end function issorted_int32

function unique_int64(A, setOrder) result(answer)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
integer(kind=int64), intent(in)                    :: A(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int64),allocatable      :: answer(:)
integer,allocatable                    :: indices(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni

   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   endif
   answer = A
   select case (lower(setOrder_))
   case ('stable')
      call unique_(answer, nuni)
      answer = answer(:nuni)
   case ('sorted')
      if (allocated(indices)) deallocate (indices)
      allocate (indices(size(A)))
      call rank_unique_(A, indices, nuni)
      answer = A(indices(:nuni))
   case default
      stop '*unique* unknown setOrder '//setOrder_//'allowed are "stable" and "sorted"'
   end select
end function unique_int64

function union_int64(A, B, setOrder) result(answer)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
integer(kind=int64),intent(in)         :: A(:)
integer(kind=int64),intent(in)         :: B(:)
character(len=*),intent(in), optional  :: setOrder
integer(kind=int64),allocatable        :: answer(:)
   answer = unique([A, B], setOrder)
end function union_int64

function intersect_int64(A, B, setOrder) result(answer)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
integer(kind=int64), intent(in)                    :: A(:)
integer(kind=int64), intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int64), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = [unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  >  1), setOrder)
end function intersect_int64

function setdiff_int64(A, B, setOrder) result(answer)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
integer(kind=int64), intent(in)                    :: a(:)
integer(kind=int64), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int64), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = unique(b, setOrder='stable')
   answer = [unique(a, setOrder='stable'), answer, answer] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  ==  1), setOrder)

end function setdiff_int64

function ismember_int64(A, B) result(answer)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
integer(kind=int64), intent(in)   :: a(:)
integer(kind=int64), intent(in)   :: b(:)
integer(kind=int64), allocatable  :: ab(:)
integer, allocatable                :: answer(:)
integer, allocatable                :: iwrk1(:)
integer, allocatable                :: iwrk2(:)
integer                             :: inums
   inums=size(a)
   ab = [ a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(ab)))
   call occurrences_(ab, iwrk1)
   call occurrences_(a, iwrk2)
   answer=iwrk1(:inums)-iwrk2
   answer=merge(0,1,answer == 0)

end function ismember_int64

function setxor_int64(A, B, setOrder) result(answer)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
integer(kind=int64), intent(in)                    :: a(:)
integer(kind=int64), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer(kind=int64), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk1(:)
integer, allocatable                                 :: iwrk2(:)
integer, allocatable                                 :: iwrk3(:)
integer                                :: inums
   inums=size(a)

   answer = [ a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(answer)))
   call occurrences_(answer, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   answer=pack(answer,iwrk1 == 0)
   answer=unique(answer,setOrder)

end function setxor_int64

function issorted_int64(A) result(answer)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
integer(kind=int64), intent(in) :: A(:)
integer(kind=int64)             :: answer
integer             :: i
answer=1
do i=1,size(a)-1
   if(A(i) > A(i+1))then
      answer=0
      exit
   endif
enddo
end function issorted_int64

function unique_real32(A, setOrder) result(answer)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
real(kind=real32), intent(in)                    :: A(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real32),allocatable      :: answer(:)
integer,allocatable                    :: indices(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni

   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   endif
   answer = A
   select case (lower(setOrder_))
   case ('stable')
      call unique_(answer, nuni)
      answer = answer(:nuni)
   case ('sorted')
      if (allocated(indices)) deallocate (indices)
      allocate (indices(size(A)))
      call rank_unique_(A, indices, nuni)
      answer = A(indices(:nuni))
   case default
      stop '*unique* unknown setOrder '//setOrder_//'allowed are "stable" and "sorted"'
   end select
end function unique_real32

function union_real32(A, B, setOrder) result(answer)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
real(kind=real32),intent(in)         :: A(:)
real(kind=real32),intent(in)         :: B(:)
character(len=*),intent(in), optional  :: setOrder
real(kind=real32),allocatable        :: answer(:)
   answer = unique([A, B], setOrder)
end function union_real32

function intersect_real32(A, B, setOrder) result(answer)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
real(kind=real32), intent(in)                    :: A(:)
real(kind=real32), intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real32), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = [unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  >  1), setOrder)
end function intersect_real32

function setdiff_real32(A, B, setOrder) result(answer)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
real(kind=real32), intent(in)                    :: a(:)
real(kind=real32), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real32), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = unique(b, setOrder='stable')
   answer = [unique(a, setOrder='stable'), answer, answer] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  ==  1), setOrder)

end function setdiff_real32

function ismember_real32(A, B) result(answer)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
real(kind=real32), intent(in)   :: a(:)
real(kind=real32), intent(in)   :: b(:)
real(kind=real32), allocatable  :: ab(:)
integer, allocatable                :: answer(:)
integer, allocatable                :: iwrk1(:)
integer, allocatable                :: iwrk2(:)
integer                             :: inums
   inums=size(a)
   ab = [ a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(ab)))
   call occurrences_(ab, iwrk1)
   call occurrences_(a, iwrk2)
   answer=iwrk1(:inums)-iwrk2
   answer=merge(0,1,answer == 0)

end function ismember_real32

function setxor_real32(A, B, setOrder) result(answer)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
real(kind=real32), intent(in)                    :: a(:)
real(kind=real32), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real32), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk1(:)
integer, allocatable                                 :: iwrk2(:)
integer, allocatable                                 :: iwrk3(:)
integer                                :: inums
   inums=size(a)

   answer = [ a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(answer)))
   call occurrences_(answer, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   answer=pack(answer,iwrk1 == 0)
   answer=unique(answer,setOrder)

end function setxor_real32

function issorted_real32(A) result(answer)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
real(kind=real32), intent(in) :: A(:)
real(kind=real32)             :: answer
integer             :: i
answer=1
do i=1,size(a)-1
   if(A(i) > A(i+1))then
      answer=0
      exit
   endif
enddo
end function issorted_real32

function unique_real64(A, setOrder) result(answer)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
real(kind=real64), intent(in)                    :: A(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real64),allocatable      :: answer(:)
integer,allocatable                    :: indices(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni

   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   endif
   answer = A
   select case (lower(setOrder_))
   case ('stable')
      call unique_(answer, nuni)
      answer = answer(:nuni)
   case ('sorted')
      if (allocated(indices)) deallocate (indices)
      allocate (indices(size(A)))
      call rank_unique_(A, indices, nuni)
      answer = A(indices(:nuni))
   case default
      stop '*unique* unknown setOrder '//setOrder_//'allowed are "stable" and "sorted"'
   end select
end function unique_real64

function union_real64(A, B, setOrder) result(answer)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
real(kind=real64),intent(in)         :: A(:)
real(kind=real64),intent(in)         :: B(:)
character(len=*),intent(in), optional  :: setOrder
real(kind=real64),allocatable        :: answer(:)
   answer = unique([A, B], setOrder)
end function union_real64

function intersect_real64(A, B, setOrder) result(answer)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
real(kind=real64), intent(in)                    :: A(:)
real(kind=real64), intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real64), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = [unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  >  1), setOrder)
end function intersect_real64

function setdiff_real64(A, B, setOrder) result(answer)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
real(kind=real64), intent(in)                    :: a(:)
real(kind=real64), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real64), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk(:)
   answer = unique(b, setOrder='stable')
   answer = [unique(a, setOrder='stable'), answer, answer] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(answer)))
   call occurrences_(answer, iwrk)
   answer = unique(pack(answer, iwrk  ==  1), setOrder)

end function setdiff_real64

function ismember_real64(A, B) result(answer)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
real(kind=real64), intent(in)   :: a(:)
real(kind=real64), intent(in)   :: b(:)
real(kind=real64), allocatable  :: ab(:)
integer, allocatable                :: answer(:)
integer, allocatable                :: iwrk1(:)
integer, allocatable                :: iwrk2(:)
integer                             :: inums
   inums=size(a)
   ab = [ a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(ab)))
   call occurrences_(ab, iwrk1)
   call occurrences_(a, iwrk2)
   answer=iwrk1(:inums)-iwrk2
   answer=merge(0,1,answer == 0)

end function ismember_real64

function setxor_real64(A, B, setOrder) result(answer)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
real(kind=real64), intent(in)                    :: a(:)
real(kind=real64), intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
real(kind=real64), allocatable                   :: answer(:)
integer, allocatable                                 :: iwrk1(:)
integer, allocatable                                 :: iwrk2(:)
integer, allocatable                                 :: iwrk3(:)
integer                                :: inums
   inums=size(a)

   answer = [ a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(answer)))
   call occurrences_(answer, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   answer=pack(answer,iwrk1 == 0)
   answer=unique(answer,setOrder)

end function setxor_real64

function issorted_real64(A) result(answer)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
real(kind=real64), intent(in) :: A(:)
real(kind=real64)             :: answer
integer             :: i
answer=1
do i=1,size(a)-1
   if(A(i) > A(i+1))then
      answer=0
      exit
   endif
enddo
end function issorted_real64

end module M_sets

module M_sets
!>
!!##NAME
!!    M_sets(3f) - [M_sets::INTRO] functions reminiscent of Matlab set functions
!!
!!##SYNOPSIS
!!
!!    Procedure names and syntax:
!!
!!     use M_sets, only : & union, unique, intersect, setdiff, ismember, setxor
!!
!!##DESCRIPTION
!!
!! Unions, intersection, and set membership
!!
!! A small subset of set functions reminiscent of Matlab set functions. They
!! currently just work with vectors of default integer kind input and return
!! sets but not the subscripts of the original elements.
!!
!! It basically uses some simple calls to the M_ordersort(3f) module to
!! provide the functionality that are not tuned for performance and make
!! loose use of memory allocation and space.
!!
!! Set operations compare the elements in two sets to find commonalities
!! or differences. Currently the sets are arrays of integer numbers.
!!
!! ## Functions
!!  + union(A,B,setOrder)     - Set union of two arrays
!!  + unique(A,setOrder)      - Unique values in array
!!  + intersect(A,B,setOrder) - Set intersection of two arrays
!!  + setdiff(A,B,setOrder)   - Set difference of two arrays
!!  + ismember(A,B,setOrder)  - Array elements that are members of set array
!!  + setxor(A,B,setOrder)    - Set exclusive OR of two arrays
!!  + issorted(A)             - test if elements are in ascending order
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_M_sets
!!    use M_sets, only: unique, intersect, union, setdiff, ismember, setxor, issorted
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer, allocatable      :: A(:)
!!    integer, allocatable      :: B(:)
!!    integer, allocatable      :: C(:)
!!
!!       write(*,g) 'UNIQUE','Find the unique elements of vector A.'
!!        A = [10, -10, 0, 1, 2, 3, 3, 2, 1, -10]
!!        write(*,g) 'A=', A
!!        write(*,g) unique(A)
!!        write(*,g) unique(A, setOrder='stable')
!!       write(*,g) 'UNION', 'Find the union of vectors A and B.'
!!        call setab( [5, 7, 1], [3, 1, 1] )
!!        write(*,g) union(A,B)
!!        call setab( [5, 5, 3], [1, 2, 5] )
!!        write(*,g) union(A, B, 'sorted')
!!        write(*,g) union(A, B, 'stable')
!!       write(*,g) 'INTERSECT', 'Find the values common to both A and B.'
!!        call setab( [7, 1, 7, 7, 4], [7, 0, 4, 4, 0] )
!!        write(*,g) intersect(A, B)
!!        write(*,g) intersect(A, B, setOrder='stable')
!!       write(*,g) 'SETDIFF','Find the values in A that are not in B.'
!!        call setab( [3, 6, 2, 1, 5, 1, 1], [2, 4, 6] )
!!        write(*,g) setdiff(A, B)
!!        call setab( [4, 1, 3, 2, 5], [2, 1])
!!        write(*,g) setdiff(A, B, 'sorted')
!!        write(*,g) setdiff(A, B, 'stable')
!!       write(*,g) 'ISMEMBER', 'Determine which elements of A are also in B.'
!!        call setab( [5,3,4,2], [2,4,4,4,6,8] )
!!        write(*,g) ismember(A,B)
!!       write(*,g) 'SETXOR','Find values of A and B not in their intersection.'
!!        call setab( [5,1,3,3,3], [4,1,2] )
!!        write(*,g) setxor(A,B)
!!        write(*,g) setxor(A,B,'stable')
!!
!!        write(*,g) 'ISSSORTED','confirm whether array is sorted in ascending order or not'
!!        call setab([1,2,3,4,5],[5,4,3,2,1])
!!        write(*,g) issorted(A)
!!        write(*,g) issorted(B)
!!
!!    contains
!!    subroutine setab(ain,bin)
!!    integer,intent(in) :: ain(:)
!!    integer,intent(in) :: bin(:)
!!       A=ain
!!       B=bin
!!       write(*,g) 'A=', A
!!       write(*,g) 'B=', B
!!    end subroutine setab
!!
!!    end program demo_M_sets
!!
!! Results:
!!
!!  > UNIQUE Find the unique elements of vector A.
!!  > A= 10 -10 0 1 2 3 3 2 1 -10
!!  > -10 0 1 2 3 10
!!  > 10 -10 0 1 2 3
!!  > UNION Find the union of vectors A and B.
!!  > A= 5 7 1
!!  > B= 3 1 1
!!  > 1 3 5 7
!!  > A= 5 5 3
!!  > B= 1 2 5
!!  > 1 2 3 5
!!  > 5 3 1 2
!!  > INTERSECT Find the values common to both A and B.
!!  > A= 7 1 7 7 4
!!  > B= 7 0 4 4 0
!!  > 4 7
!!  > 7 4
!!  > SETDIFF Find the values in A that are not in B.
!!  > A= 3 6 2 1 5 1 1
!!  > B= 2 4 6
!!  > 1 3 5
!!  > A= 4 1 3 2 5
!!  > B= 2 1
!!  > 3 4 5
!!  > 4 3 5
!!  > ISMEMBER Determine which elements of A are also in B.
!!  > A= 5 3 4 2
!!  > B= 2 4 4 4 6 8
!!  > 0 0 1 1
!!  > SETXOR Find values of A and B not in their intersection.
!!  > A= 5 1 3 3 3
!!  > B= 4 1 2
!!  > 2 3 4 5
!!  > 5 3 4 2
!!
!!##AUTHORS
!!    John S. Urban, 2023-07-20
!!
!!##LICENSE
!!    CC0-1.0
! a subset of the functionality of set functions in Matlab are provided.
! currently, just integer vector input is provided.
use M_orderpack, only: unique_ => unique, occurrences_ => occurrences
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
!! Return unique values in array.  C = unique(A) returns the same data as
!! in A, but with no repetitions. C is in sorted order by default.
!!
!!##OPTIONS
!!
!!     A         input array to extract unique values from
!!     setOrder  May be "sort" or "stable"
!!
!!##RETURNS
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_unique
!!    use M_sets, only: unique
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer, allocatable      :: A(:)
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
function unique(A, setOrder) result(result)
! C = unique(A) returns the same data as in A, but with no repetitions. C is in sorted order.
integer, intent(in)                    :: A(:)
character(len=*), intent(in), optional :: setOrder
integer, allocatable                   :: result(:)
character(len=:), allocatable          :: setOrder_
integer                                :: nuni
   if (present(setOrder)) then
      setOrder_ = setOrder
   else
      setOrder_ = 'sorted'
   end if
   result = A
   select case (setOrder_)
   case ('stable')
      call unique_(result, nuni)
      result = result(:nuni)
   case ('sorted')
      if (allocated(result)) deallocate (result)
      allocate (result(size(A)))
      call rank_unique_(A, result, nuni)
      result = A(result(:nuni))
   case default
      stop '*unique* unknown setOrder '//setOrder_
   end select
end function unique
!>
!!##NAME
!!    union(3f) - [M_sets] return union values in array A
!!
!!##SYNOPSIS
!!
!!
!!    union(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! Set union of two arrays.  C = union(A,B) returns the combined data from
!! A and B with no repetitions. C is in sorted order.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable"
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!    program demo_union
!!    use M_sets, only: union
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer, allocatable      :: A(:)
!!    integer, allocatable      :: B(:)
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
function union(A, B, setOrder) result(result)
! C = union(A,B) returns the combined data from A and B with no repetitions. C is in sorted order.
integer, intent(in)                    :: A(:)
integer, intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
integer, allocatable                   :: result(:)
   result = unique([A, B], setOrder)
end function union

!>
!!##NAME
!!    intersect(3f) - [M_sets] return intersect values in array A
!!
!!##SYNOPSIS
!!
!!
!!    intersect(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! Set intersection of two arrays.  C = intersect(A,B) returns the data
!! common to both A and B, with no repetitions. C is in sorted order
!! by default.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable"
!!
!!##EXAMPLE
!!
!!
!!  sample program:
!!
!!      program demo_intersect
!!      use M_sets, only: unique, intersect, union, setdiff, ismember, setxor
!!      character(len=*),parameter :: g='(*(g0,1x))'
!!      integer, allocatable      :: A(:)
!!      integer, allocatable      :: B(:)
!!
!!         write(*,g) 'INTERSECT', 'Find the values common to both A and B.'
!!          A=[7, 1, 7, 7, 4]
!!          B=[7, 0, 4, 4, 0]
!!          write(*,g) 'A=', A
!!          write(*,g) 'B=', B
!!          write(*,g) intersect(A, B)
!!          write(*,g) intersect(A, B, setOrder='stable')
!!      end program demo_intersect
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
function intersect(A, B, setOrder) result(result)
! C = intersect(A,B) returns the data common to both A and B, with no repetitions. C is in sorted order by default
integer, intent(in)                    :: A(:)
integer, intent(in)                    :: B(:)
character(len=*), intent(in), optional :: setOrder
integer, allocatable                   :: result(:)
integer, allocatable                   :: iwrk(:)
   result = [unique(A, setOrder), unique(B, setOrder)]
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(result)))
   call occurrences_(result, iwrk)
   result = unique(pack(result, iwrk .gt. 1), setOrder)
end function intersect

!>
!!##NAME
!!    setdiff(3f) - [M_sets] return setdiff values in array A
!!
!!##SYNOPSIS
!!
!!
!!    setdiff(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! Set difference of two arrays.  C = setdiff(A,B) returns the data in A
!! that is not in B, with no repetitions. C is in sorted order by default.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable"
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
function setdiff(A, B, setOrder) result(result)
! C = setdiff(A,B) returns the data in A that is not in B, with no repetitions. C is in sorted order by default.
integer, intent(in)                    :: a(:)
integer, intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer, allocatable                   :: result(:)
integer, allocatable                   :: iwrk(:)
   result = unique(b, setOrder='stable')
   result = [unique(a, setOrder='stable'), result, result] ! potentially a lot of memory
   if (allocated(iwrk)) deallocate (iwrk)
   allocate (iwrk(size(result)))
   call occurrences_(result, iwrk)
   result = unique(pack(result, iwrk .eq. 1), setOrder)

end function setdiff

!>
!!##NAME
!!    ismember(3f) - [M_sets] return ismember values in array A
!!
!!##SYNOPSIS
!!
!!
!!    ismember(A,B)
!!
!!##DESCRIPTION
!!
!! Array elements that are members of set array.  C = ismember(A,B) returns
!! an array containing "logical" 1 (true) where the data in A is found in
!! B. Elsewhere, the array contains "logical" 0 (false).
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!
!!##RETURNS
!!     C
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
function ismember(A, B) result(result)
! C = ismember(A,B) returns an array containing 1 (true) where the data in A is found in B. Elsewhere, the array contains 0
integer, intent(in)   :: a(:)
integer, intent(in)   :: b(:)
integer, allocatable  :: result(:)
integer, allocatable  :: iwrk1(:)
integer, allocatable  :: iwrk2(:)
integer               :: inums
   inums=size(a)
   result = [ a, unique(b) ] ! potentially a lot of memory
   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(result)))
   call occurrences_(result, iwrk1)
   call occurrences_(a, iwrk2)
   result=iwrk1(:inums)-iwrk2
   result=merge(0,1,result.eq.0)

end function ismember

!>
!!##NAME
!!    setxor(3f) - [M_sets] return setxor values in array A
!!
!!##SYNOPSIS
!!
!!
!!    setxor(A,B, setOrder)
!!
!!##DESCRIPTION
!!
!! Set exclusive OR of two arrays.  C = setxor(A,B) returns the data of
!! A and B that are not in their intersection (the symmetric difference),
!! with no repetitions. That is, setxor(3f) returns the data that occurs
!! in A or B, but not both. C is in sorted order.
!!
!!##OPTIONS
!!
!!     A         input array
!!     B         input array
!!     setOrder  May be "sort" or "stable"
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
function setxor(A, B, setOrder) result(result)
! C = setxor(A,B,setOrder) returns the data of A and B that are not in their intersection
!                          (the symmetric difference), with no repetitions. That is, setxor returns the
!                          data that occurs in A or B, but not both. C is in sorted order.
integer, intent(in)                    :: a(:)
integer, intent(in)                    :: b(:)
character(len=*), intent(in), optional :: setOrder
integer, allocatable                   :: result(:)
integer, allocatable                   :: iwrk1(:)
integer, allocatable                   :: iwrk2(:)
integer, allocatable                   :: iwrk3(:)
integer                                :: inums
   inums=size(a)

   result = [ a, b ] ! potentially a lot of memory
   if (allocated(iwrk1)) deallocate (iwrk1)
   allocate (iwrk1(size(result)))
   call occurrences_(result, iwrk1)

   if (allocated(iwrk2)) deallocate (iwrk2)
   allocate (iwrk2(size(a)))
   call occurrences_(a, iwrk2)

   if (allocated(iwrk3)) deallocate (iwrk3)
   allocate (iwrk3(size(b)))
   call occurrences_(b, iwrk3)

   iwrk1=iwrk1-[iwrk2,iwrk3]

   result=pack(result,iwrk1.eq.0)
   result=unique(result,setOrder)

end function setxor
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
!!    Report if A is sorted in ascending order or not. TF = issorted(A)
!!    returns the logical scalar 1 (true) when the elements of A are listed
!!    in ascending order and 0 (false) otherwise.
!!
!!##OPTIONS
!!
!!     A     input array to test
!!
!!##RETURNS
!!
!!     TF    1 if input array A issorted in ascending order, 0 otherwise
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
function issorted(A) result(result)
! TF = issorted(A) returns the logical scalar 1 (true) when the elements of A are listed in ascending order and 0 (false) otherwise.
integer, intent(in) :: A(:)
integer             :: result
integer             :: i
result=1
do i=1,size(a)-1
   if(A(i).gt.A(i+1))then
      result=0
      exit
   endif
enddo
end function issorted

end module M_sets

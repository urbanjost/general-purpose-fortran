Module M_rinpar
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: rinpar
!>
!!##NAME
!!    prank_special(3f) - [orderpack:RANK:PARTIAL] partially ranks an array in
!!                 ASCENDING order (Insertion Sort)
!!
!!##SYNOPSIS
!!
!!     Subroutine Prank_Special (INVALS, IRNGT, NORD)
!!
!!      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)
!!      Integer, Intent (Out)               :: IRNGT(:)
!!      Integer, Intent (In)                :: NORD
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!    o Real(kind=real32)
!!    o Real(kind=real64)
!!    o Integer(kind=int32)
!!    o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!    Partially ranks INVALS(). Returns IRNGT(1:NORD) filled with the
!!    indices of the lowest values in the array INVALS. More technically,
!!    it does a partial ranking of the array INVALS of order NORD.
!!
!!    NORD is restricted to the range 1 to size(IRNGT).
!!
!!    Internally, this version is not optimized for performance, and is
!!    thus not as difficult to read as some other ones.
!!
!!    The subroutine uses an insertion sort, limiting insertion to the
!!    first NORD values. It does not use any work array and is fastest when
!!    NORD is very small (2-5). but worst case behavior can easily happen
!!    (ie. if INVALS initially is inverse sorted). Therefore, In many cases,
!!    the refined Quicksort method is faster.
!!
!!##OPTIONS
!!     INVALS   array to partially sort
!!     NORD    number of indices to return, restricted to 1 to size(IRNGT)
!!##RETURNS
!!     IRNGT   indices of requested number (NORD) of lowest values in INVALS
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_prank_special
!!    ! partially rank N lowest values in an array
!!    use M_orderpack, only : prank_special
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer,allocatable :: INVALS(:)
!!    integer,allocatable :: irngt(:)
!!    integer :: nord
!!    INVALS=[10,5,7,1,4,5,6,8,9,10,1]
!!    nord=5
!!    allocate(irngt(nord))
!!       write(*,g)'ORIGINAL:',INVALS
!!       call prank_special(INVALS,irngt,nord)
!!       write(*,g)'NUMBER OF INDICES TO RETURN:',nord
!!       write(*,g)'RETURNED INDICES:',irngt
!!       write(*,g)nord,'SMALLEST VALUES:',INVALS(irngt(:nord))
!!    end program demo_prank_special
!!
!!   Results:
!!
!!    ORIGINAL: 10 5 7 1 4 5 6 8 9 10 1
!!    NUMBER OF INDICES TO RETURN: 5
!!    RETURNED INDICES: 4 11 5 2 6
!!    5 SMALLEST VALUES: 1 1 4 5 5
!!
!!##AUTHOR
!!    Michel Olagnon - Feb. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface rinpar
   module procedure real64_rinpar, real32_rinpar, int32_rinpar, f_char_rinpar
end interface rinpar
contains
Subroutine real64_rinpar (INVALS, IRNGT, NORD)
!!__________________________________________________________
   Real (kind=real64), Dimension (:), Intent (In) :: INVALS
   Integer, Dimension (:), Intent (Out) :: IRNGT
   Integer, Intent (In) :: NORD
! __________________________________________________________
   Real (kind=real64) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
    IRNGT (1) = 1
    Do ICRS = 2, NORD
       XWRK = INVALS (ICRS)
       Do IDCR = ICRS - 1, 1, -1
          If (XWRK >= INVALS(IRNGT(IDCR))) Exit
          IRNGT (IDCR+1) = IRNGT (IDCR)
       End Do
       IRNGT (IDCR+1) = ICRS
    End Do
!
    XWRK1 = INVALS (IRNGT(NORD))
    Do ICRS = NORD + 1, SIZE (INVALS)
       If (INVALS(ICRS) < XWRK1) Then
          XWRK = INVALS (ICRS)
          Do IDCR = NORD - 1, 1, -1
             If (XWRK >= INVALS(IRNGT(IDCR))) Exit
             IRNGT (IDCR+1) = IRNGT (IDCR)
          End Do
          IRNGT (IDCR+1) = ICRS
          XWRK1 = INVALS (IRNGT(NORD))
       End If
    End Do
!
End Subroutine real64_rinpar
Subroutine real32_rinpar (INVALS, IRNGT, NORD)
!!__________________________________________________________
   Real (kind=real32), Dimension (:), Intent (In) :: INVALS
   Integer, Dimension (:), Intent (Out) :: IRNGT
   Integer, Intent (In) :: NORD
! __________________________________________________________
   Real (kind=real32) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
    IRNGT (1) = 1
    Do ICRS = 2, NORD
       XWRK = INVALS (ICRS)
       Do IDCR = ICRS - 1, 1, -1
          If (XWRK >= INVALS(IRNGT(IDCR))) Exit
          IRNGT (IDCR+1) = IRNGT (IDCR)
       End Do
       IRNGT (IDCR+1) = ICRS
    End Do
!
    XWRK1 = INVALS (IRNGT(NORD))
    Do ICRS = NORD + 1, SIZE (INVALS)
       If (INVALS(ICRS) < XWRK1) Then
          XWRK = INVALS (ICRS)
          Do IDCR = NORD - 1, 1, -1
             If (XWRK >= INVALS(IRNGT(IDCR))) Exit
             IRNGT (IDCR+1) = IRNGT (IDCR)
          End Do
          IRNGT (IDCR+1) = ICRS
          XWRK1 = INVALS (IRNGT(NORD))
       End If
    End Do
!
End Subroutine real32_rinpar
Subroutine int32_rinpar (INVALS, IRNGT, NORD)
!!__________________________________________________________
   Integer (kind=int32), Dimension (:), Intent (In) :: INVALS
   Integer, Dimension (:), Intent (Out) :: IRNGT
   Integer, Intent (In) :: NORD
! __________________________________________________________
   Integer (kind=int32) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
    IRNGT (1) = 1
    Do ICRS = 2, NORD
       XWRK = INVALS (ICRS)
       Do IDCR = ICRS - 1, 1, -1
          If (XWRK >= INVALS(IRNGT(IDCR))) Exit
          IRNGT (IDCR+1) = IRNGT (IDCR)
       End Do
       IRNGT (IDCR+1) = ICRS
    End Do
!
    XWRK1 = INVALS (IRNGT(NORD))
    Do ICRS = NORD + 1, SIZE (INVALS)
       If (INVALS(ICRS) < XWRK1) Then
          XWRK = INVALS (ICRS)
          Do IDCR = NORD - 1, 1, -1
             If (XWRK >= INVALS(IRNGT(IDCR))) Exit
             IRNGT (IDCR+1) = IRNGT (IDCR)
          End Do
          IRNGT (IDCR+1) = ICRS
          XWRK1 = INVALS (IRNGT(NORD))
       End If
    End Do
!
End Subroutine int32_rinpar
Subroutine f_char_rinpar (INVALS, IRNGT, NORD)
!!__________________________________________________________
   character (kind=f_char,len=*), Dimension (:), Intent (In) :: INVALS
   Integer, Dimension (:), Intent (Out) :: IRNGT
   Integer, Intent (In) :: NORD
! __________________________________________________________
   character (kind=f_char,len=len(INVALS)) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
    IRNGT (1) = 1
    Do ICRS = 2, NORD
       XWRK = INVALS (ICRS)
       Do IDCR = ICRS - 1, 1, -1
          If (XWRK >= INVALS(IRNGT(IDCR))) Exit
          IRNGT (IDCR+1) = IRNGT (IDCR)
       End Do
       IRNGT (IDCR+1) = ICRS
    End Do
!
    XWRK1 = INVALS (IRNGT(NORD))
    Do ICRS = NORD + 1, SIZE (INVALS)
       If (INVALS(ICRS) < XWRK1) Then
          XWRK = INVALS (ICRS)
          Do IDCR = NORD - 1, 1, -1
             If (XWRK >= INVALS(IRNGT(IDCR))) Exit
             IRNGT (IDCR+1) = IRNGT (IDCR)
          End Do
          IRNGT (IDCR+1) = ICRS
          XWRK1 = INVALS (IRNGT(NORD))
       End If
    End Do
!
End Subroutine f_char_rinpar

end module M_rinpar

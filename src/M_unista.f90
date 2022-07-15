Module M_unista
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_uniinv
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: unista
!>
!!##NAME
!!    unique(3f) - [orderpack:UNIQUE] removes duplicates from an array
!!                 otherwise retaining original order (i.e. it is "stable")
!!
!!##SYNOPSIS
!!
!!     Subroutine Unique (INOUTVALS, NUNI)
!!
!!      ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)
!!      Integer, Intent (Out)                  :: NUNI
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!    UNIQUE(3f) does a stable removal of duplicates from an array.
!!
!!    It leaves in the initial set only those entries that are unique,
!!    packing the array, and leaving the order of the retained values
!!    unchanged.
!!
!!    Internally this subroutine uses Merge-sort unique inverse ranking.
!!
!!##OPTIONS
!!     INOUTVALS   input array to reduce to unique values
!!     NUNI    number of values comprising the returned set of unique
!!             values
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_unique
!!    ! remove duplicates with remaining elements remaining in initial order
!!    use M_orderpack, only : unique
!!    implicit none
!!    character(len=*),parameter :: list= '(*(g0:,", "))'
!!    integer :: nuni
!!
!!    int : block
!!    integer,allocatable :: INOUTVALS(:)
!!     INOUTVALS=[44,33,33,33,22,11,33,44,55,33]
!!     print list,'ORIGINAL:',INOUTVALS
!!     call unique(INOUTVALS,nuni)
!!     INOUTVALS=INOUTVALS(:nuni)
!!     print list,'UNIQUE:',INOUTVALS
!!    endblock int
!!
!!    end program demo_unique
!!
!!   Results:
!!
!!    ORIGINAL:, 44, 33, 33, 33, 22, 11, 33, 44, 55, 33
!!    UNIQUE:, 44, 33, 22, 11, 55
!!
!!##AUTHOR
!!    Michel Olagnon - Feb. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface unista
  module procedure real64_unista, real32_unista, int32_unista, f_char_unista
end interface unista
contains
Subroutine real64_unista (INOUTVALS, NUNI)
! __________________________________________________________
   Real (kind=real64), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (Out) :: NUNI
! __________________________________________________________
   Integer, Dimension (Size(INOUTVALS)) :: IWRKT
   Logical, Dimension (Size(INOUTVALS)) :: IFMPTYT
   Integer :: ICRS
! __________________________________________________________
   Call UNIINV (INOUTVALS, IWRKT)
   IFMPTYT = .True.
   NUNI = 0
   Do ICRS = 1, Size(INOUTVALS)
      If (IFMPTYT(IWRKT(ICRS))) Then
         IFMPTYT(IWRKT(ICRS)) = .False.
         NUNI = NUNI + 1
         INOUTVALS (NUNI) = INOUTVALS (ICRS)
      End If
   End Do
!
End Subroutine real64_unista
Subroutine real32_unista (INOUTVALS, NUNI)
! __________________________________________________________
   Real (kind=real32), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (Out) :: NUNI
! __________________________________________________________
   Integer, Dimension (Size(INOUTVALS)) :: IWRKT
   Logical, Dimension (Size(INOUTVALS)) :: IFMPTYT
   Integer :: ICRS
! __________________________________________________________
   Call UNIINV (INOUTVALS, IWRKT)
   IFMPTYT = .True.
   NUNI = 0
   Do ICRS = 1, Size(INOUTVALS)
      If (IFMPTYT(IWRKT(ICRS))) Then
         IFMPTYT(IWRKT(ICRS)) = .False.
         NUNI = NUNI + 1
         INOUTVALS (NUNI) = INOUTVALS (ICRS)
      End If
   End Do
!
End Subroutine real32_unista
Subroutine int32_unista (INOUTVALS, NUNI)
! __________________________________________________________
   Integer (kind=int32), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (Out) :: NUNI
! __________________________________________________________
   Integer, Dimension (Size(INOUTVALS)) :: IWRKT
   Logical, Dimension (Size(INOUTVALS)) :: IFMPTYT
   Integer :: ICRS
! __________________________________________________________
   Call UNIINV (INOUTVALS, IWRKT)
   IFMPTYT = .True.
   NUNI = 0
   Do ICRS = 1, Size(INOUTVALS)
      If (IFMPTYT(IWRKT(ICRS))) Then
         IFMPTYT(IWRKT(ICRS)) = .False.
         NUNI = NUNI + 1
         INOUTVALS (NUNI) = INOUTVALS (ICRS)
      End If
   End Do
!
End Subroutine int32_unista
Subroutine f_char_unista (INOUTVALS, NUNI)
! __________________________________________________________
   character (kind=f_char,len=*), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (Out) :: NUNI
! __________________________________________________________
   Integer, Dimension (Size(INOUTVALS)) :: IWRKT
   Logical, Dimension (Size(INOUTVALS)) :: IFMPTYT
   Integer :: ICRS
! __________________________________________________________
   Call UNIINV (INOUTVALS, IWRKT)
   IFMPTYT = .True.
   NUNI = 0
   Do ICRS = 1, Size(INOUTVALS)
      If (IFMPTYT(IWRKT(ICRS))) Then
         IFMPTYT(IWRKT(ICRS)) = .False.
         NUNI = NUNI + 1
         INOUTVALS (NUNI) = INOUTVALS (ICRS)
      End If
   End Do
!
End Subroutine f_char_unista
end module M_unista

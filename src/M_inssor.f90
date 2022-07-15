Module M_inssor
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: inssor
!>
!!##NAME
!!    sort_special(3f) - [orderpack:SORT] Sorts array into ascending order
!!                 (Insertion sort, generally for small or nearly sorted
!!                 arrays)
!!##SYNOPSIS
!!
!!      Subroutine Sort_Special (INOUTVALS)
!!
!!       ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!    Sorts INOUTVALS() into ascending order (Insertion sort).
!!
!!    If certain requirements are met and performance is important this
!!    procedure can be far faster, but SORT(3f) and ranking routines
!!    RANK(3f) and RANK_BASIC(3f) are recommended for the general case.
!!
!!    This subroutine uses an Insertion sort. It does not use any work array
!!    and is faster when INOUTVALS() is of very small size (< 20), or already
!!    almost sorted; but worst case behavior can be triggered by commonly
!!    encountered data order (e.g. initially inverse sorted). Therefore,
!!    in many cases the Quicksort or Mergesort method is faster.
!!
!!##OPTIONS
!!     INOUTVALS      array to sort
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_sort_special
!!    ! sort an array using insertion sort
!!    use,intrinsic :: iso_fortran_env, only : int32, real32, real64
!!    use M_orderpack, only : sort_special
!!    implicit none
!!    ! an insertion sort is very efficient for very small arrays
!!    ! but generally slower than methods like quicksort and mergesort.
!!    integer,parameter :: isz=2000
!!    real(kind=real64) :: dd(isz), hi, low
!!       ! make an array of random values
!!       call random_seed()
!!       call random_number(dd)
!!       dd=dd*1000000.0-500000.0
!!       low= minval(dd)
!!       hi = maxval(dd)
!!       ! sort the data
!!       call sort_special(dd)
!!       ! cursory checks
!!       if(any(dd(1:isz-1) .gt. dd(2:isz)))stop 'ERROR: array not sorted'
!!       write(*,*)'check min:',dd(1).eq.low
!!       write(*,*)'check max:',dd(isz).eq.hi
!!       write(*,*)'PASSED: random array is now sorted'
!!    end program demo_sort_special
!!
!!   Results:
!!
!!     check min: T
!!     check max: T
!!     PASSED: random array is now sorted
!!
!!##AUTHOR
!!    Michel Olagnon - Apr. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface inssor
  module procedure real64_inssor, real32_inssor, int32_inssor, f_char_inssor
end interface inssor
contains
Subroutine real64_inssor (INOUTVALS)
! __________________________________________________________
      Real (kind=real64), Dimension (:), Intent (InOut) :: INOUTVALS
      Real (Kind=real64) :: XWRK, XMIN
! __________________________________________________________
      Integer :: ICRS, IDCR, NDON
!
      NDON = Size (INOUTVALS)
!
! We first bring the minimum to the first location in the array.
! That way, we will have a "guard", and when looking for the
! right place to insert a value, no loop test is necessary.
!
      If (INOUTVALS (1) < INOUTVALS (NDON)) Then
          XMIN = INOUTVALS (1)
      Else
          XMIN = INOUTVALS (NDON)
          INOUTVALS (NDON) = INOUTVALS (1)
      Endif
      Do IDCR = NDON-1, 2, -1
         XWRK = INOUTVALS(IDCR)
         IF (XWRK < XMIN) Then
            INOUTVALS (IDCR) = XMIN
            XMIN = XWRK
         End If
      End Do
      INOUTVALS (1) = XMIN
!
! The first value is now the minimum
! Loop over the array, and when a value is smaller than
! the previous one, loop down to insert it at its right place.
!
      Do ICRS = 3, NDON
         XWRK = INOUTVALS (ICRS)
         IDCR = ICRS - 1
         If (XWRK < INOUTVALS(IDCR)) Then
            INOUTVALS (ICRS) = INOUTVALS (IDCR)
            IDCR = IDCR - 1
            Do
               If (XWRK >= INOUTVALS(IDCR)) Exit
               INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
               IDCR = IDCR - 1
            End Do
            INOUTVALS (IDCR+1) = XWRK
         End If
      End Do
!
      Return
!
End Subroutine real64_inssor
Subroutine real32_inssor (INOUTVALS)
! __________________________________________________________
      Real (kind=real32), Dimension (:), Intent (InOut) :: INOUTVALS
      Real (Kind=real32) :: XWRK, XMIN
! __________________________________________________________
      Integer :: ICRS, IDCR, NDON
!
      NDON = Size (INOUTVALS)
!
! We first bring the minimum to the first location in the array.
! That way, we will have a "guard", and when looking for the
! right place to insert a value, no loop test is necessary.
!
      If (INOUTVALS (1) < INOUTVALS (NDON)) Then
          XMIN = INOUTVALS (1)
      Else
          XMIN = INOUTVALS (NDON)
          INOUTVALS (NDON) = INOUTVALS (1)
      Endif
      Do IDCR = NDON-1, 2, -1
         XWRK = INOUTVALS(IDCR)
         IF (XWRK < XMIN) Then
            INOUTVALS (IDCR) = XMIN
            XMIN = XWRK
         End If
      End Do
      INOUTVALS (1) = XMIN
!
! The first value is now the minimum
! Loop over the array, and when a value is smaller than
! the previous one, loop down to insert it at its right place.
!
      Do ICRS = 3, NDON
         XWRK = INOUTVALS (ICRS)
         IDCR = ICRS - 1
         If (XWRK < INOUTVALS(IDCR)) Then
            INOUTVALS (ICRS) = INOUTVALS (IDCR)
            IDCR = IDCR - 1
            Do
               If (XWRK >= INOUTVALS(IDCR)) Exit
               INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
               IDCR = IDCR - 1
            End Do
            INOUTVALS (IDCR+1) = XWRK
         End If
      End Do
!
      Return
!
End Subroutine real32_inssor
Subroutine int32_inssor (INOUTVALS)
! __________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (InOut) :: INOUTVALS
      Integer (Kind=int32) :: XWRK, XMIN
! __________________________________________________________
      Integer :: ICRS, IDCR, NDON
!
      NDON = Size (INOUTVALS)
!
! We first bring the minimum to the first location in the array.
! That way, we will have a "guard", and when looking for the
! right place to insert a value, no loop test is necessary.
!
      If (INOUTVALS (1) < INOUTVALS (NDON)) Then
          XMIN = INOUTVALS (1)
      Else
          XMIN = INOUTVALS (NDON)
          INOUTVALS (NDON) = INOUTVALS (1)
      Endif
      Do IDCR = NDON-1, 2, -1
         XWRK = INOUTVALS(IDCR)
         IF (XWRK < XMIN) Then
            INOUTVALS (IDCR) = XMIN
            XMIN = XWRK
         End If
      End Do
      INOUTVALS (1) = XMIN
!
! The first value is now the minimum
! Loop over the array, and when a value is smaller than
! the previous one, loop down to insert it at its right place.
!
      Do ICRS = 3, NDON
         XWRK = INOUTVALS (ICRS)
         IDCR = ICRS - 1
         If (XWRK < INOUTVALS(IDCR)) Then
            INOUTVALS (ICRS) = INOUTVALS (IDCR)
            IDCR = IDCR - 1
            Do
               If (XWRK >= INOUTVALS(IDCR)) Exit
               INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
               IDCR = IDCR - 1
            End Do
            INOUTVALS (IDCR+1) = XWRK
         End If
      End Do
!
      Return
!
End Subroutine int32_inssor
Subroutine f_char_inssor (INOUTVALS)
! __________________________________________________________
      character (kind=f_char,len=*), Dimension (:), Intent (InOut) :: INOUTVALS
      character (Kind=f_char,len=len(INOUTVALS)) :: XWRK, XMIN
! __________________________________________________________
      Integer :: ICRS, IDCR, NDON
!
      NDON = Size (INOUTVALS)
!
! We first bring the minimum to the first location in the array.
! That way, we will have a "guard", and when looking for the
! right place to insert a value, no loop test is necessary.
!
      If (INOUTVALS (1) < INOUTVALS (NDON)) Then
          XMIN = INOUTVALS (1)
      Else
          XMIN = INOUTVALS (NDON)
          INOUTVALS (NDON) = INOUTVALS (1)
      Endif
      Do IDCR = NDON-1, 2, -1
         XWRK = INOUTVALS(IDCR)
         IF (XWRK < XMIN) Then
            INOUTVALS (IDCR) = XMIN
            XMIN = XWRK
         End If
      End Do
      INOUTVALS (1) = XMIN
!
! The first value is now the minimum
! Loop over the array, and when a value is smaller than
! the previous one, loop down to insert it at its right place.
!
      Do ICRS = 3, NDON
         XWRK = INOUTVALS (ICRS)
         IDCR = ICRS - 1
         If (XWRK < INOUTVALS(IDCR)) Then
            INOUTVALS (ICRS) = INOUTVALS (IDCR)
            IDCR = IDCR - 1
            Do
               If (XWRK >= INOUTVALS(IDCR)) Exit
               INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
               IDCR = IDCR - 1
            End Do
            INOUTVALS (IDCR+1) = XWRK
         End If
      End Do
!
      Return
!
End Subroutine f_char_inssor
end module M_inssor

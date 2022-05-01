Module M_refsor
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: refsor
private :: real64_inssor, real32_inssor, int32_inssor, f_char_inssor
!>
!!##NAME
!!    sort(3f) - [orderpack:SORT] Sorts array into ascending order
!!                 (Quicksort)
!!
!!##SYNOPSIS
!!
!!     Subroutine Sort (INOUTVALS)
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
!!    Sorts INOUTVALS into ascending order (Quicksort)
!!
!!    This version is not optimized for performance, and is thus not as
!!    difficult to read as some other ones.
!!
!!    Internally, This subroutine uses Quick-sort in a recursive
!!    implementation, and insertion sort for the last steps with small
!!    subsets. It does not use any work array.
!!
!!    The Quick-sort
!!    chooses a "pivot" in the set, and explores the array from
!!    both ends, looking for a value > pivot with the increasing index,
!!    for a value <= pivot with the decreasing index, and swapping them
!!    when it has found one of each. The array is then subdivided in
!!    two subsets:
!!
!!        { values <= pivot} {pivot} {values > pivot}
!!
!!    It then recursively calls the procedure to sort each subset. When
!!    the size of the subarray is small enough, it switches to an insertion
!!    sort that is faster for very small sets.
!!
!!##OPTIONS
!!     INOUTVALS      array to sort
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_sort
!!    ! sort array in ascending order
!!    use,intrinsic :: iso_fortran_env, only : int32, real32, real64
!!    use M_orderpack, only : sort
!!    implicit none
!!    ! an insertion sort is very efficient for very small arrays
!!    ! but generally slower than methods like quicksort and mergesort.
!!    real(kind=real64) :: valsd(2000)
!!    integer           :: i
!!       call random_seed()
!!       call random_number(valsd)
!!       valsd=valsd*1000000.0-500000.0
!!       call sort(valsd)
!!       do i=1,size(valsd)-1
!!          if (valsd(i+1).lt.valsd(i))then
!!             write(*,*)'not sorted'
!!             stop 3
!!          endif
!!       enddo
!!       write(*,*)'random arrays are now sorted'
!!    end program demo_sort
!!
!!   Results:
!!
!!     random arrays are now sorted
!!
!!##AUTHOR
!!    Michel Olagnon - Apr. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface refsor
  module procedure real64_refsor, real32_refsor, int32_refsor, f_char_refsor
end interface refsor
contains
Subroutine real64_refsor (INOUTVALS)
! __________________________________________________________
      Real (kind=real64), Dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Call real64_subsor (INOUTVALS, 1, Size (INOUTVALS))
      Call real64_inssor (INOUTVALS)
End Subroutine real64_refsor

Recursive Subroutine real64_subsor (INOUTVALS, IDEB1, IFIN1)
!  Sorts INOUTVALS from IDEB1 to IFIN1
! __________________________________________________________
      Real(kind=real64), dimension (:), Intent (InOut) :: INOUTVALS
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Real(kind=real64) :: XPIV, XWRK
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
            XWRK = INOUTVALS (IDEB)
            INOUTVALS (IDEB) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
         End If
         If (INOUTVALS(IMIL) > INOUTVALS(IFIN)) Then
            XWRK = INOUTVALS (IFIN)
            INOUTVALS (IFIN) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
            If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
               XWRK = INOUTVALS (IDEB)
               INOUTVALS (IDEB) = INOUTVALS (IMIL)
               INOUTVALS (IMIL) = XWRK
            End If
         End If
         XPIV = INOUTVALS (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INOUTVALS (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (INOUTVALS(ICRS) > XPIV) Exit
            End Do
            Do
               If (INOUTVALS(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = INOUTVALS (IDCR)
            INOUTVALS (IDCR) = INOUTVALS (ICRS)
            INOUTVALS (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call real64_subsor (INOUTVALS, IDEB1, ICRS-1)
         Call real64_subsor (INOUTVALS, IDCR, IFIN1)
      End If

   End Subroutine real64_subsor

   Subroutine real64_inssor (INOUTVALS)
!  Sorts INOUTVALS into increasing order (Insertion sort)
! __________________________________________________________
      Real(kind=real64), dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Integer :: ICRS, IDCR
      Real(kind=real64) :: XWRK
!
      Do ICRS = 2, Size (INOUTVALS)
         XWRK = INOUTVALS (ICRS)
         If (XWRK >= INOUTVALS(ICRS-1)) Cycle
         INOUTVALS (ICRS) = INOUTVALS (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
      End Do
!
End Subroutine real64_inssor
Subroutine real32_refsor (INOUTVALS)
! __________________________________________________________
      Real (kind=real32), Dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Call real32_subsor (INOUTVALS, 1, Size (INOUTVALS))
      Call real32_inssor (INOUTVALS)
End Subroutine real32_refsor

Recursive Subroutine real32_subsor (INOUTVALS, IDEB1, IFIN1)
!  Sorts INOUTVALS from IDEB1 to IFIN1
! __________________________________________________________
      Real(kind=real32), dimension (:), Intent (InOut) :: INOUTVALS
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Real(kind=real32) :: XPIV, XWRK
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
            XWRK = INOUTVALS (IDEB)
            INOUTVALS (IDEB) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
         End If
         If (INOUTVALS(IMIL) > INOUTVALS(IFIN)) Then
            XWRK = INOUTVALS (IFIN)
            INOUTVALS (IFIN) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
            If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
               XWRK = INOUTVALS (IDEB)
               INOUTVALS (IDEB) = INOUTVALS (IMIL)
               INOUTVALS (IMIL) = XWRK
            End If
         End If
         XPIV = INOUTVALS (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INOUTVALS (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (INOUTVALS(ICRS) > XPIV) Exit
            End Do
            Do
               If (INOUTVALS(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = INOUTVALS (IDCR)
            INOUTVALS (IDCR) = INOUTVALS (ICRS)
            INOUTVALS (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call real32_subsor (INOUTVALS, IDEB1, ICRS-1)
         Call real32_subsor (INOUTVALS, IDCR, IFIN1)
      End If

   End Subroutine real32_subsor

   Subroutine real32_inssor (INOUTVALS)
!  Sorts INOUTVALS into increasing order (Insertion sort)
! __________________________________________________________
      Real(kind=real32), dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Integer :: ICRS, IDCR
      Real(kind=real32) :: XWRK
!
      Do ICRS = 2, Size (INOUTVALS)
         XWRK = INOUTVALS (ICRS)
         If (XWRK >= INOUTVALS(ICRS-1)) Cycle
         INOUTVALS (ICRS) = INOUTVALS (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
      End Do
!
End Subroutine real32_inssor
Subroutine int32_refsor (INOUTVALS)
! __________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Call int32_subsor (INOUTVALS, 1, Size (INOUTVALS))
      Call int32_inssor (INOUTVALS)
End Subroutine int32_refsor

Recursive Subroutine int32_subsor (INOUTVALS, IDEB1, IFIN1)
!  Sorts INOUTVALS from IDEB1 to IFIN1
! __________________________________________________________
      Integer(kind=int32), dimension (:), Intent (InOut) :: INOUTVALS
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer(kind=int32) :: XPIV, XWRK
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
            XWRK = INOUTVALS (IDEB)
            INOUTVALS (IDEB) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
         End If
         If (INOUTVALS(IMIL) > INOUTVALS(IFIN)) Then
            XWRK = INOUTVALS (IFIN)
            INOUTVALS (IFIN) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
            If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
               XWRK = INOUTVALS (IDEB)
               INOUTVALS (IDEB) = INOUTVALS (IMIL)
               INOUTVALS (IMIL) = XWRK
            End If
         End If
         XPIV = INOUTVALS (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INOUTVALS (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (INOUTVALS(ICRS) > XPIV) Exit
            End Do
            Do
               If (INOUTVALS(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = INOUTVALS (IDCR)
            INOUTVALS (IDCR) = INOUTVALS (ICRS)
            INOUTVALS (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call int32_subsor (INOUTVALS, IDEB1, ICRS-1)
         Call int32_subsor (INOUTVALS, IDCR, IFIN1)
      End If

   End Subroutine int32_subsor

   Subroutine int32_inssor (INOUTVALS)
!  Sorts INOUTVALS into increasing order (Insertion sort)
! __________________________________________________________
      Integer(kind=int32), dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Integer :: ICRS, IDCR
      Integer(kind=int32) :: XWRK
!
      Do ICRS = 2, Size (INOUTVALS)
         XWRK = INOUTVALS (ICRS)
         If (XWRK >= INOUTVALS(ICRS-1)) Cycle
         INOUTVALS (ICRS) = INOUTVALS (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
      End Do
!
End Subroutine int32_inssor
Subroutine f_char_refsor (INOUTVALS)
! __________________________________________________________
      character (kind=f_char,len=*), Dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Call f_char_subsor (INOUTVALS, 1, Size (INOUTVALS))
      Call f_char_inssor (INOUTVALS)
End Subroutine f_char_refsor

Recursive Subroutine f_char_subsor (INOUTVALS, IDEB1, IFIN1)
!  Sorts INOUTVALS from IDEB1 to IFIN1
! __________________________________________________________
      character(kind=f_char,len=*), dimension (:), Intent (InOut) :: INOUTVALS
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      character(kind=f_char,len=len(INOUTVALS)) :: XPIV, XWRK
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
            XWRK = INOUTVALS (IDEB)
            INOUTVALS (IDEB) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
         End If
         If (INOUTVALS(IMIL) > INOUTVALS(IFIN)) Then
            XWRK = INOUTVALS (IFIN)
            INOUTVALS (IFIN) = INOUTVALS (IMIL)
            INOUTVALS (IMIL) = XWRK
            If (INOUTVALS(IMIL) < INOUTVALS(IDEB)) Then
               XWRK = INOUTVALS (IDEB)
               INOUTVALS (IDEB) = INOUTVALS (IMIL)
               INOUTVALS (IMIL) = XWRK
            End If
         End If
         XPIV = INOUTVALS (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INOUTVALS (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (INOUTVALS(ICRS) > XPIV) Exit
            End Do
            Do
               If (INOUTVALS(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = INOUTVALS (IDCR)
            INOUTVALS (IDCR) = INOUTVALS (ICRS)
            INOUTVALS (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call f_char_subsor (INOUTVALS, IDEB1, ICRS-1)
         Call f_char_subsor (INOUTVALS, IDCR, IFIN1)
      End If

   End Subroutine f_char_subsor

   Subroutine f_char_inssor (INOUTVALS)
!  Sorts INOUTVALS into increasing order (Insertion sort)
! __________________________________________________________
      character(kind=f_char,len=*), dimension (:), Intent (InOut) :: INOUTVALS
! __________________________________________________________
      Integer :: ICRS, IDCR
      character(kind=f_char,len=len(INOUTVALS)) :: XWRK
!
      Do ICRS = 2, Size (INOUTVALS)
         XWRK = INOUTVALS (ICRS)
         If (XWRK >= INOUTVALS(ICRS-1)) Cycle
         INOUTVALS (ICRS) = INOUTVALS (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
      End Do
!
End Subroutine f_char_inssor
end module M_refsor

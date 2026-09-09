Module M_orderpack__refpar
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: refpar
!>
!!##NAME
!!    prank_basic(3f) - [M_orderpack:RANK:PARTIAL] partially ranks an array
!!                 (Quick-Sort)
!!
!!##SYNOPSIS
!!
!!     Subroutine Prank_Basic (INVALS, IRNGT, NORD)
!!
!!       ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)
!!       Integer, Intent (Out)               :: IRNGT(:)
!!       Integer, Intent (In)                :: NORD
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!    creates index IRNGT() which partially ranks input array INVALS(),
!!    up to order NORD.
!!
!!    This version is not optimized for performance, and is thus not as
!!    difficult to read as some other ones.
!!
!!    Internally this routine uses a pivoting strategy such as the one used
!!    in finding the median based on the Quick-Sort algorithm. It uses a
!!    temporary array, where it stores the partially ranked indices of the
!!    values. It iterates until it can bring the number of values lower
!!    than the pivot to exactly NORD, and then uses an Insertion-Sort to
!!    rank this set, since it is supposedly small.
!!
!!##OPTIONS
!!     INVALS      array to partially rank
!!     IRNGT      array to hold indices of ranked elements
!!     NORD       number of elements to rank
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_prank
!!    ! create index to lowest N values in input array in ascending order
!!    use,intrinsic :: iso_fortran_env, only : int32, real32, real64
!!    use M_orderpack, only : prank_basic
!!    implicit none
!!    real(kind=real32) :: valsr(2000)
!!    integer           :: indx(2000)
!!    integer           :: i
!!    real,allocatable  :: results(:)
!!       ! create some random data
!!       call random_seed()
!!       call random_number(valsr)
!!       valsr=valsr*1000000.0-500000.0
!!       ! get 300 lowest values sorted
!!       call prank_basic(valsr,indx,300)
!!       !
!!       results=valsr(indx(:300))
!!       ! check if sorted
!!       do i=1,300-1
!!          if (results(i+1).lt.results(i))then
!!             write(*,*)'ERROR: not sorted'
!!             stop 1
!!          endif
!!       enddo
!!       write(*,*)'random array now sorted'
!!    end program demo_prank
!!
!!   Results:
!!
!!     random array now sorted
!!
!!##AUTHOR
!!    Michel Olagnon - Feb. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface refpar
  module procedure real64_refpar, real32_refpar, int32_refpar, f_char_refpar
end interface refpar
contains
Subroutine real64_refpar (INVALS, IRNGT, NORD)
      Real (kind=real64), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Real (kind=real64) :: XPIV, XWRK
! __________________________________________________________
!
      Integer, Dimension (SIZE(INVALS)) :: IWRKT
      Integer :: NDON, ICRS, IDEB, IDCR, IFIN, IMIL, IWRK
!
      NDON = SIZE (INVALS)
!
      Do ICRS = 1, NDON
         IWRKT (ICRS) = ICRS
      End Do
      IDEB = 1
      IFIN = NDON
      Do
         If (IDEB >= IFIN) Exit
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
            IWRK = IWRKT (IDEB)
            IWRKT (IDEB) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
         End If
         If (INVALS(IWRKT(IMIL)) > INVALS(IWRKT(IFIN))) Then
            IWRK = IWRKT (IFIN)
            IWRKT (IFIN) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
            If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
               IWRK = IWRKT (IDEB)
               IWRKT (IDEB) = IWRKT (IMIL)
               IWRKT (IMIL) = IWRK
            End If
         End If
         If ((IFIN-IDEB) < 3) Exit
         XPIV = INVALS (IWRKT(IMIL))
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
!  the first  >  pivot is IWRKT(IDCR)
!  the last   <= pivot is IWRKT(ICRS-1)
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INVALS (IWRKT(IFIN)) > XPIV
!
                  Exit ECH2
!
               End If
               If (INVALS(IWRKT(ICRS)) > XPIV) Exit
            End Do
            Do
               If (INVALS(IWRKT(IDCR)) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always IWRKT(ICRS-1)
!
                  Exit ECH2
               End If
            End Do
!
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (ICRS)
            IWRKT (ICRS) = IWRK
         End Do ECH2
!
!  One restricts further processing to find the fractile value
!
         If (ICRS <= NORD) IDEB = ICRS
         If (ICRS > NORD) IFIN = ICRS - 1
      End Do
!
!  Now, we only need to complete ranking of the 1:NORD set
!  Assuming NORD is small, we use a simple insertion sort
!
      Do ICRS = 2, NORD
         IWRK = IWRKT (ICRS)
         XWRK = INVALS (IWRK)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK <= INVALS(IWRKT(IDCR))) Then
               IWRKT (IDCR+1) = IWRKT (IDCR)
            Else
               Exit
            End If
         End Do
         IWRKT (IDCR+1) = IWRK
      End Do
      IRNGT (1:NORD) = IWRKT (1:NORD)
      Return
!
End Subroutine real64_refpar

Subroutine real32_refpar (INVALS, IRNGT, NORD)
      Real (kind=real32), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Real (kind=real32) :: XPIV, XWRK
! __________________________________________________________
!
      Integer, Dimension (SIZE(INVALS)) :: IWRKT
      Integer :: NDON, ICRS, IDEB, IDCR, IFIN, IMIL, IWRK
!
      NDON = SIZE (INVALS)
!
      Do ICRS = 1, NDON
         IWRKT (ICRS) = ICRS
      End Do
      IDEB = 1
      IFIN = NDON
      Do
         If (IDEB >= IFIN) Exit
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
            IWRK = IWRKT (IDEB)
            IWRKT (IDEB) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
         End If
         If (INVALS(IWRKT(IMIL)) > INVALS(IWRKT(IFIN))) Then
            IWRK = IWRKT (IFIN)
            IWRKT (IFIN) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
            If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
               IWRK = IWRKT (IDEB)
               IWRKT (IDEB) = IWRKT (IMIL)
               IWRKT (IMIL) = IWRK
            End If
         End If
         If ((IFIN-IDEB) < 3) Exit
         XPIV = INVALS (IWRKT(IMIL))
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
!  the first  >  pivot is IWRKT(IDCR)
!  the last   <= pivot is IWRKT(ICRS-1)
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INVALS (IWRKT(IFIN)) > XPIV
!
                  Exit ECH2
!
               End If
               If (INVALS(IWRKT(ICRS)) > XPIV) Exit
            End Do
            Do
               If (INVALS(IWRKT(IDCR)) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always IWRKT(ICRS-1)
!
                  Exit ECH2
               End If
            End Do
!
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (ICRS)
            IWRKT (ICRS) = IWRK
         End Do ECH2
!
!  One restricts further processing to find the fractile value
!
         If (ICRS <= NORD) IDEB = ICRS
         If (ICRS > NORD) IFIN = ICRS - 1
      End Do
!
!  Now, we only need to complete ranking of the 1:NORD set
!  Assuming NORD is small, we use a simple insertion sort
!
      Do ICRS = 2, NORD
         IWRK = IWRKT (ICRS)
         XWRK = INVALS (IWRK)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK <= INVALS(IWRKT(IDCR))) Then
               IWRKT (IDCR+1) = IWRKT (IDCR)
            Else
               Exit
            End If
         End Do
         IWRKT (IDCR+1) = IWRK
      End Do
      IRNGT (1:NORD) = IWRKT (1:NORD)
      Return
!
End Subroutine real32_refpar

Subroutine int32_refpar (INVALS, IRNGT, NORD)
      Integer (kind=int32), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Integer (kind=int32) :: XPIV, XWRK
! __________________________________________________________
!
      Integer, Dimension (SIZE(INVALS)) :: IWRKT
      Integer :: NDON, ICRS, IDEB, IDCR, IFIN, IMIL, IWRK
!
      NDON = SIZE (INVALS)
!
      Do ICRS = 1, NDON
         IWRKT (ICRS) = ICRS
      End Do
      IDEB = 1
      IFIN = NDON
      Do
         If (IDEB >= IFIN) Exit
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
            IWRK = IWRKT (IDEB)
            IWRKT (IDEB) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
         End If
         If (INVALS(IWRKT(IMIL)) > INVALS(IWRKT(IFIN))) Then
            IWRK = IWRKT (IFIN)
            IWRKT (IFIN) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
            If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
               IWRK = IWRKT (IDEB)
               IWRKT (IDEB) = IWRKT (IMIL)
               IWRKT (IMIL) = IWRK
            End If
         End If
         If ((IFIN-IDEB) < 3) Exit
         XPIV = INVALS (IWRKT(IMIL))
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
!  the first  >  pivot is IWRKT(IDCR)
!  the last   <= pivot is IWRKT(ICRS-1)
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INVALS (IWRKT(IFIN)) > XPIV
!
                  Exit ECH2
!
               End If
               If (INVALS(IWRKT(ICRS)) > XPIV) Exit
            End Do
            Do
               If (INVALS(IWRKT(IDCR)) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always IWRKT(ICRS-1)
!
                  Exit ECH2
               End If
            End Do
!
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (ICRS)
            IWRKT (ICRS) = IWRK
         End Do ECH2
!
!  One restricts further processing to find the fractile value
!
         If (ICRS <= NORD) IDEB = ICRS
         If (ICRS > NORD) IFIN = ICRS - 1
      End Do
!
!  Now, we only need to complete ranking of the 1:NORD set
!  Assuming NORD is small, we use a simple insertion sort
!
      Do ICRS = 2, NORD
         IWRK = IWRKT (ICRS)
         XWRK = INVALS (IWRK)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK <= INVALS(IWRKT(IDCR))) Then
               IWRKT (IDCR+1) = IWRKT (IDCR)
            Else
               Exit
            End If
         End Do
         IWRKT (IDCR+1) = IWRK
      End Do
      IRNGT (1:NORD) = IWRKT (1:NORD)
      Return
!
End Subroutine int32_refpar

Subroutine f_char_refpar (INVALS, IRNGT, NORD)
      character (kind=f_char,len=*), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
! __________________________________________________________
      character (kind=f_char,len=len(INVALS)) :: XPIV, XWRK
! __________________________________________________________
!
      Integer, Dimension (SIZE(INVALS)) :: IWRKT
      Integer :: NDON, ICRS, IDEB, IDCR, IFIN, IMIL, IWRK
!
      NDON = SIZE (INVALS)
!
      Do ICRS = 1, NDON
         IWRKT (ICRS) = ICRS
      End Do
      IDEB = 1
      IFIN = NDON
      Do
         If (IDEB >= IFIN) Exit
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
            IWRK = IWRKT (IDEB)
            IWRKT (IDEB) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
         End If
         If (INVALS(IWRKT(IMIL)) > INVALS(IWRKT(IFIN))) Then
            IWRK = IWRKT (IFIN)
            IWRKT (IFIN) = IWRKT (IMIL)
            IWRKT (IMIL) = IWRK
            If (INVALS(IWRKT(IMIL)) < INVALS(IWRKT(IDEB))) Then
               IWRK = IWRKT (IDEB)
               IWRKT (IDEB) = IWRKT (IMIL)
               IWRKT (IMIL) = IWRK
            End If
         End If
         If ((IFIN-IDEB) < 3) Exit
         XPIV = INVALS (IWRKT(IMIL))
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
!  the first  >  pivot is IWRKT(IDCR)
!  the last   <= pivot is IWRKT(ICRS-1)
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if INVALS (IWRKT(IFIN)) > XPIV
!
                  Exit ECH2
!
               End If
               If (INVALS(IWRKT(ICRS)) > XPIV) Exit
            End Do
            Do
               If (INVALS(IWRKT(IDCR)) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always IWRKT(ICRS-1)
!
                  Exit ECH2
               End If
            End Do
!
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (ICRS)
            IWRKT (ICRS) = IWRK
         End Do ECH2
!
!  One restricts further processing to find the fractile value
!
         If (ICRS <= NORD) IDEB = ICRS
         If (ICRS > NORD) IFIN = ICRS - 1
      End Do
!
!  Now, we only need to complete ranking of the 1:NORD set
!  Assuming NORD is small, we use a simple insertion sort
!
      Do ICRS = 2, NORD
         IWRK = IWRKT (ICRS)
         XWRK = INVALS (IWRK)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK <= INVALS(IWRKT(IDCR))) Then
               IWRKT (IDCR+1) = IWRKT (IDCR)
            Else
               Exit
            End If
         End Do
         IWRKT (IDCR+1) = IWRK
      End Do
      IRNGT (1:NORD) = IWRKT (1:NORD)
      Return
!
End Subroutine f_char_refpar

end module M_orderpack__refpar

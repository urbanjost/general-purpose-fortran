Module M_inspar
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: inspar
!>
!!##NAME
!!    psort(3f) - [orderpack:SORT:PARTIAL] partially sorts an array
!!                (Insertion Sort, generally for small or nearly sorted
!!                arrays)
!!
!!##SYNOPSIS
!!
!!     Subroutine Psort (INOUTVALS, NORD)
!!
!!      ${TYPE} (kind=${KIND}), Intent (InOut) :: INOUTVALS(:)
!!      Integer, Intent (In)                   :: NORD
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!    PSORT(3f) partially sorts INOUTVALS, bringing the NORD lowest values
!!    to the beginning of the array.
!!
!!    Internally, this subroutine uses an insertion sort, limiting insertion
!!    to the first NORD values. It does not use any work array and is faster
!!    when NORD is very small (2-5), but worst case behavior can happen
!!    fairly probably (initially inverse sorted). Therefore, in many cases,
!!    the refined quicksort method is faster.
!!
!!##OPTIONS
!!     INOUTVALS      The array to partially sort
!!     NORD       number of sorted values to return.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_psort
!!    ! partially sort an array
!!    use M_orderpack, only : psort
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer :: nord
!!
!!    int: block
!!       integer,allocatable :: ia(:)
!!       ia=[10,5,7,1,4,5,6,8,9,10,1]
!!       nord=5
!!       write(*,g)'Original.................:',ia
!!       call psort(ia,nord)
!!       write(*,g)'Number of indices to sort:',nord
!!       write(*,g)nord,'Lowest values..........:',ia(:nord)
!!       write(*,g)'Entire array.............:',ia
!!       write(*,g)
!!    endblock int
!!    char: block
!!       character(len=:),allocatable :: ca(:)
!!       integer :: i
!!       ca=[character(len=20) :: 'fan','a','car','be','egg','dam','gas']
!!       nord=3
!!       write(*,g)'Original.................:',(trim(ca(i)),i=1,size(ca))
!!       call psort(ca,nord)
!!       write(*,g)'Number of indices to sort:',nord
!!       write(*,g)nord,'Lowest values..........:',(trim(ca(i)),i=1,nord)
!!       write(*,g)'Entire array.............:',(trim(ca(i)),i=1,size(ca))
!!       write(*,g)
!!    endblock char
!!
!!    end program demo_psort
!!
!!   Results:
!!
!!    Original.................: 10 5 7 1 4 5 6 8 9 10 1
!!    Number of indices to sort: 5
!!    5 Lowest values..........: 1 1 4 5 5
!!    Entire array.............: 1 1 4 5 5 10 7 8 9 10 6
!!
!!    Original.................: fan a car be egg dam gas
!!    Number of indices to sort: 3
!!    3 Lowest values..........: a be car
!!    Entire array.............: a be car fan egg dam gas
!!
!!##AUTHOR
!!    Michel Olagnon - Feb. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface inspar
  module procedure real64_inspar, real32_inspar, int32_inspar, f_char_inspar
end interface inspar
contains
Subroutine real64_inspar (INOUTVALS, NORD)
   Real (kind=real64), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (In) :: NORD
! __________________________________________________________
   Real (kind=real64) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
   Do ICRS = 2, NORD
      XWRK = INOUTVALS (ICRS)
      Do IDCR = ICRS - 1, 1, -1
         If (XWRK >= INOUTVALS(IDCR)) Exit
         INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
      End Do
      INOUTVALS (IDCR+1) = XWRK
   End Do
!
   XWRK1 = INOUTVALS (NORD)
   Do ICRS = NORD + 1, SIZE (INOUTVALS)
      If (INOUTVALS(ICRS) < XWRK1) Then
         XWRK = INOUTVALS (ICRS)
         INOUTVALS (ICRS) = XWRK1
         Do IDCR = NORD - 1, 1, -1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
         XWRK1 = INOUTVALS (NORD)
      End If
   End Do
!
End Subroutine real64_inspar
Subroutine real32_inspar (INOUTVALS, NORD)
   Real (kind=real32), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (In) :: NORD
! __________________________________________________________
   Real (kind=real32) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
   Do ICRS = 2, NORD
      XWRK = INOUTVALS (ICRS)
      Do IDCR = ICRS - 1, 1, -1
         If (XWRK >= INOUTVALS(IDCR)) Exit
         INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
      End Do
      INOUTVALS (IDCR+1) = XWRK
   End Do
!
   XWRK1 = INOUTVALS (NORD)
   Do ICRS = NORD + 1, SIZE (INOUTVALS)
      If (INOUTVALS(ICRS) < XWRK1) Then
         XWRK = INOUTVALS (ICRS)
         INOUTVALS (ICRS) = XWRK1
         Do IDCR = NORD - 1, 1, -1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
         XWRK1 = INOUTVALS (NORD)
      End If
   End Do
!
End Subroutine real32_inspar
Subroutine int32_inspar (INOUTVALS, NORD)
   Integer (kind=int32), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (In) :: NORD
! __________________________________________________________
   Integer (kind=int32) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
   Do ICRS = 2, NORD
      XWRK = INOUTVALS (ICRS)
      Do IDCR = ICRS - 1, 1, -1
         If (XWRK >= INOUTVALS(IDCR)) Exit
         INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
      End Do
      INOUTVALS (IDCR+1) = XWRK
   End Do
!
   XWRK1 = INOUTVALS (NORD)
   Do ICRS = NORD + 1, SIZE (INOUTVALS)
      If (INOUTVALS(ICRS) < XWRK1) Then
         XWRK = INOUTVALS (ICRS)
         INOUTVALS (ICRS) = XWRK1
         Do IDCR = NORD - 1, 1, -1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
         XWRK1 = INOUTVALS (NORD)
      End If
   End Do
!
End Subroutine int32_inspar
Subroutine f_char_inspar (INOUTVALS, NORD)
   character (kind=f_char,len=*), Dimension (:), Intent (InOut) :: INOUTVALS
   Integer, Intent (In) :: NORD
! __________________________________________________________
   character (kind=f_char,len=len(INOUTVALS)) :: XWRK, XWRK1
   Integer :: ICRS, IDCR
!
   Do ICRS = 2, NORD
      XWRK = INOUTVALS (ICRS)
      Do IDCR = ICRS - 1, 1, -1
         If (XWRK >= INOUTVALS(IDCR)) Exit
         INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
      End Do
      INOUTVALS (IDCR+1) = XWRK
   End Do
!
   XWRK1 = INOUTVALS (NORD)
   Do ICRS = NORD + 1, SIZE (INOUTVALS)
      If (INOUTVALS(ICRS) < XWRK1) Then
         XWRK = INOUTVALS (ICRS)
         INOUTVALS (ICRS) = XWRK1
         Do IDCR = NORD - 1, 1, -1
            If (XWRK >= INOUTVALS(IDCR)) Exit
            INOUTVALS (IDCR+1) = INOUTVALS (IDCR)
         End Do
         INOUTVALS (IDCR+1) = XWRK
         XWRK1 = INOUTVALS (NORD)
      End If
   End Do
!
End Subroutine f_char_inspar
end module M_inspar

Module M_fndnth
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: fndnth
!>
!!##NAME
!!    orderval_special(3f) - [orderpack:FRACTILE] Return VALUE of Nth
!!                           ordered element of array (InsertSort-like)
!!
!!##SYNOPSIS
!!
!!     Function Orderval_Special (INVALS, INORD)
!!
!!      ${TYPE} (Kind=${KIND}), Intent (In) :: INVALS(:)
!!      Integer, Intent (In)                :: INORD
!!      ${TYPE} (Kind=${KIND})              :: orderval_special
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!    o Real(kind=real32)
!!    o Real(kind=real64)
!!    o Integer(kind=int32)
!!
!!##DESCRIPTION
!!    ORDERVAL_SPECIAL(3f) returns the INORDth lowest value of INVALS(),
!!    i.e. the fractile of order INORD/SIZE(INVALS).
!!
!!    Internally, This subroutine uses an insertion sort, limiting insertion
!!    to the first INORD values and even less when one can know that the
!!    value that is considered will not be the INORDth.
!!
!!    An insertion sort is very fast when INORD is very small
!!    (2-5). Additionally, internally it requires only a work array of size
!!    INORD (and type of INVALS),
!!
!!    But worst case behavior can happen fairly probably (e.g., initially
!!    inverse sorted). Therefore, in many cases, the refined QuickSort
!!    method is faster.
!!
!!    so ORDERVAL_SPECIAL(3f) should be used when INORD is small and INVALS
!!    is likely to be a random array, otherwise consider using ORDERLOC(3f)
!!    or ORDERVAL(3f).
!!
!!##OPTIONS
!!     INVALS              input array of values
!!     INORD                specify Nth value of sorted INVALS array to
!!                         return, from 1 to size(INVALS).
!!##RETURNS
!!     ORDERVAL_SPECIAL    returned value
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_orderval_special
!!    ! return Nth ordered value of an array
!!    use M_orderpack, only : orderval_special, medianval
!!    implicit none
!!    character(len=*),parameter :: list= '(*(g0:,", "))',sp='(*(g0,1x))'
!!    integer,allocatable :: iarr(:)
!!    integer :: i
!!       iarr=[80,70,30,40,-50,60,20,10]
!!       print sp, 'ORIGINAL:',iarr
!!       ! can return the same values as intrinsics minval(3f) and maxval(3f)
!!       print sp, 'minval',orderval_special(iarr,1),          minval(iarr)
!!       print sp, 'maxval',orderval_special(iarr,size(iarr)), maxval(iarr)
!!       ! but more generally it can return the Nth lowest value.
!!       print sp, 'median',orderval_special(iarr,(size(iarr+1))/2), &
!!       & medianval(iarr)
!!       ! so only Nth ordered value can be found
!!       print sp,'inord=',3, ' fractile=',orderval_special(iarr,3)
!!       ! sorting the hard way
!!       print sp, 'ORIGINAL:',iarr
!!       do i=1,size(iarr)
!!          write(*,list)i,orderval_special(iarr,i)
!!       enddo
!!       print *
!!    end program demo_orderval_special
!!
!!   Results:
!!
!!    ORIGINAL: 80 70 30 40 -50 60 20 10
!!    minval -50 -50
!!    maxval 80 80
!!    median 30 30
!!    inord= 3  fractile= 20
!!    ORIGINAL: 80 70 30 40 -50 60 20 10
!!    1, -50
!!    2, 10
!!    3, 20
!!    4, 30
!!    5, 40
!!    6, 60
!!    7, 70
!!    8, 80
!!
!!##SEE ALSO
!!
!!    ORDERLOC(3f), ORDERVAL(3f)
!!
!!##AUTHOR
!!    Michel Olagnon - Aug. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface fndnth
  module procedure real64_fndnth, real32_fndnth, int32_fndnth !, f_char_fndnth
end interface fndnth
contains
Function real64_fndnth (INVALS, INORD) Result (FNDNTH)
! __________________________________________________________
      Real (Kind=real64), Dimension (:), Intent (In) :: INVALS
      Real (Kind=real64) :: FNDNTH
      Integer, Intent (In) :: INORD
! __________________________________________________________
      Real (Kind=real64), Dimension (INORD) :: XWRKT
      Real (Kind=real64) :: XWRK, XWRK1
!
      Integer :: ICRS, IDCR, ILOW, NDON
!
      XWRKT (1) = INVALS (1)
      Do ICRS = 2, INORD
         XWRK = INVALS (ICRS)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK >= XWRKT(IDCR)) Exit
            XWRKT (IDCR+1) = XWRKT (IDCR)
         End Do
         XWRKT (IDCR+1) = XWRK
      End Do
!
      NDON = SIZE (INVALS)
      XWRK1 = XWRKT (INORD)
      ILOW = 2*INORD - NDON
      Do ICRS = INORD + 1, NDON
         If (INVALS(ICRS) < XWRK1) Then
            XWRK = INVALS (ICRS)
            Do IDCR = INORD - 1, MAX (1, ILOW) , - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
            XWRK1 = XWRKT(INORD)
         End If
         ILOW = ILOW + 1
      End Do
      FNDNTH = XWRK1
!
End Function real64_fndnth
Function real32_fndnth (INVALS, INORD) Result (FNDNTH)
! __________________________________________________________
      Real (Kind=real32), Dimension (:), Intent (In) :: INVALS
      Real (Kind=real32) :: FNDNTH
      Integer, Intent (In) :: INORD
! __________________________________________________________
      Real (Kind=real32), Dimension (INORD) :: XWRKT
      Real (Kind=real32) :: XWRK, XWRK1
!
      Integer :: ICRS, IDCR, ILOW, NDON
!
      XWRKT (1) = INVALS (1)
      Do ICRS = 2, INORD
         XWRK = INVALS (ICRS)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK >= XWRKT(IDCR)) Exit
            XWRKT (IDCR+1) = XWRKT (IDCR)
         End Do
         XWRKT (IDCR+1) = XWRK
      End Do
!
      NDON = SIZE (INVALS)
      XWRK1 = XWRKT (INORD)
      ILOW = 2*INORD - NDON
      Do ICRS = INORD + 1, NDON
         If (INVALS(ICRS) < XWRK1) Then
            XWRK = INVALS (ICRS)
            Do IDCR = INORD - 1, MAX (1, ILOW) , - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
            XWRK1 = XWRKT(INORD)
         End If
         ILOW = ILOW + 1
      End Do
      FNDNTH = XWRK1
!
End Function real32_fndnth
Function int32_fndnth (INVALS, INORD) Result (FNDNTH)
! __________________________________________________________
      Integer (Kind=int32), Dimension (:), Intent (In) :: INVALS
      Integer (Kind=int32) :: FNDNTH
      Integer, Intent (In) :: INORD
! __________________________________________________________
      Integer (Kind=int32), Dimension (INORD) :: XWRKT
      Integer (Kind=int32) :: XWRK, XWRK1
!
      Integer :: ICRS, IDCR, ILOW, NDON
!
      XWRKT (1) = INVALS (1)
      Do ICRS = 2, INORD
         XWRK = INVALS (ICRS)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK >= XWRKT(IDCR)) Exit
            XWRKT (IDCR+1) = XWRKT (IDCR)
         End Do
         XWRKT (IDCR+1) = XWRK
      End Do
!
      NDON = SIZE (INVALS)
      XWRK1 = XWRKT (INORD)
      ILOW = 2*INORD - NDON
      Do ICRS = INORD + 1, NDON
         If (INVALS(ICRS) < XWRK1) Then
            XWRK = INVALS (ICRS)
            Do IDCR = INORD - 1, MAX (1, ILOW) , - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
            XWRK1 = XWRKT(INORD)
         End If
         ILOW = ILOW + 1
      End Do
      FNDNTH = XWRK1
!
End Function int32_fndnth

end module M_fndnth

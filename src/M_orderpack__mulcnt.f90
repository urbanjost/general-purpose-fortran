Module M_orderpack__mulcnt
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
Use M_orderpack__uniinv
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: mulcnt
!>
!!##NAME
!!    occurrences(3f) - [M_orderpack:MULTIPLICITY] Give the multiplicity for each
!!                 array value (number of times that it appears in the array)
!!
!!##SYNOPSIS
!!
!!     Subroutine Occurrences (INVALS, IMULT)
!!
!!       ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)
!!       Integer, Intent (Out)               :: IMULT(:)
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!     OCCURRENCES(3f) Gives, for each array element, its multiplicity
!!     (number of times that it appears in the array).
!!
!!     Internally, the number of times that a value appears in the array is
!!     computed by using inverse ranking, counting for each rank the number
!!     of values that "collide" to this rank, and returning this sum to
!!     the locations in the original set. It uses subroutine RANK_ORDERS(3f).
!!
!!##OPTIONS
!!     INVALS      input array
!!     IMULT      array containing how often the value in INVALS
!!                appears in INVALS
!!
!!##EXAMPLES
!!
!! Sample program:
!!
!!      program demo_occurrences
!!      use M_orderpack, only : occurrences
!!      ! determine how many times each value appears in an input array
!!      implicit none
!!      character(len=*),parameter    :: g='(*(g0,1x))'
!!      character(len=20),allocatable :: strings(:)
!!      integer,allocatable           :: cindx(:)
!!      integer                       :: csz
!!      integer                       :: i
!!         ! each name appears the number of times its name represents
!!         strings= [ character(len=20) ::                           &
!!         & 'two  ',  'four ', 'three', 'five',   'five',           &
!!         & 'two  ',  'four ', 'three', 'five',   'five',           &
!!         & 'four ',  'four ', 'three', 'one  ',  'five']
!!         csz=size(strings)
!!         if(allocated(cindx))deallocate(cindx)
!!         allocate(cindx(csz))
!!         call occurrences(strings,cindx)
!!         write(*,g)(trim(strings(i)),i=1,csz)
!!         write(*,g)cindx
!!      end program demo_occurrences
!!
!! Results:
!!
!!  two four three five five two four three five five four four three one five
!!  2   4    3     5    5    2   4    3     5    5    4    4    3     1   5
!!
!!##AUTHOR
!!    Michel Olagnon, Mar 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface mulcnt
  module procedure real64_mulcnt, real32_mulcnt, int32_mulcnt, f_char_mulcnt
end interface mulcnt
contains
Subroutine real64_mulcnt (INVALS, IMULT)
! __________________________________________________________
      Real (kind=real64), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(INVALS)) :: IWRKT
      Integer, Dimension (Size(INVALS)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (INVALS, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(INVALS)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(INVALS)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine real64_mulcnt
Subroutine real32_mulcnt (INVALS, IMULT)
! __________________________________________________________
      Real (kind=real32), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(INVALS)) :: IWRKT
      Integer, Dimension (Size(INVALS)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (INVALS, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(INVALS)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(INVALS)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine real32_mulcnt
Subroutine int32_mulcnt (INVALS, IMULT)
! __________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(INVALS)) :: IWRKT
      Integer, Dimension (Size(INVALS)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (INVALS, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(INVALS)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(INVALS)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine int32_mulcnt
Subroutine f_char_mulcnt (INVALS, IMULT)
! __________________________________________________________
      character (kind=f_char,len=*), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IMULT
! __________________________________________________________
      Integer, Dimension (Size(INVALS)) :: IWRKT
      Integer, Dimension (Size(INVALS)) :: ICNTT
      Integer :: ICRS
! __________________________________________________________
      Call UNIINV (INVALS, IWRKT)
      ICNTT = 0
      Do ICRS = 1, Size(INVALS)
            ICNTT(IWRKT(ICRS)) = ICNTT(IWRKT(ICRS)) + 1
      End Do
      Do ICRS = 1, Size(INVALS)
            IMULT(ICRS) = ICNTT(IWRKT(ICRS))
      End Do
!
End Subroutine f_char_mulcnt
end module M_orderpack__mulcnt

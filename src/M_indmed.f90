Module M_indmed
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: indmed
Integer, Allocatable, Dimension(:), Private, Save :: IDONT
!>
!!##NAME
!!    medianloc(3f) - [orderpack:MEDIAN] Returns median value's INDEX.
!!
!!##SYNOPSIS
!!
!!     Subroutine MedianLoc (INVALS, OUTORD)
!!
!!       ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)
!!       Integer, Intent (Out)               :: OUTORD
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!       o Character(kind=selected_char_kind("DEFAULT"),len=*)
!!
!!##DESCRIPTION
!!    MEDIANLOC(3f) Returns the index of the median (((Size(INVALS)+1))/2^th
!!    value).
!!
!!
!!    Internally, MEDIANLOC(3f) Finds the index of the median of the array
!!    INVALS() using the recursive procedure described in Knuth, The Art of
!!    Computer Programming, vol. 3, 5.3.3.
!!
!!    This procedure is linear in time, and does not require to be able
!!    to interpolate in the set as the one used in ORDERLOC(3f), which can
!!    also be used to calculate a median. It also has better worst-case
!!    behavior than ORDERLOC(3f), but is about 10% slower on average for
!!    random uniformly distributed values.
!!
!!##OPTIONS
!!     INVALS     array to find the median value of.
!!     OUTORD     index of the median value.
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_medianloc
!!    ! return index of median value
!!    use M_orderpack, only : medianloc
!!    implicit none
!!    real,allocatable :: INVALS(:)
!!    character(len=:),allocatable :: cdont(:)
!!    character(len=*),parameter :: fmt='(i5,t11,g0)'
!!    integer :: ii
!!       write(*,*) 'location  median'
!!
!!       INVALS=[80.0,70.0,20.0,10.0,1000.0]
!!       call medianloc(INVALS,ii)
!!       write(*,fmt) ii,INVALS(ii)
!!       !
!!       INVALS=[11, 22, 33, 44, 55, 66, 77, 88]
!!       call medianloc(INVALS,ii)
!!       write(*,fmt) ii,INVALS(ii)
!!       !
!!       INVALS=[11.0d0,77.0d0,22.0d0,66.0d0,33.0d0,88.0d0]
!!       call medianloc(INVALS,ii)
!!       write(*,fmt) ii,INVALS(ii)
!!       !
!!       cdont=[character(len=20) :: 'apple','bee','cherry','duck',&
!!               'elephant','finger','goose','h','insect','j']
!!       call medianloc(cdont,ii)
!!       write(*,fmt) ii,cdont(ii)
!!       !
!!    end program demo_medianloc
!!
!!   Results:
!!
!!     location  median
!!        2     70.00000
!!        4     44.00000
!!        5     33.00000
!!        5     elephant
!!
!!##AUTHOR
!!    Michel Olagnon, 2000-2012
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface indmed
  module procedure real64_indmed, real32_indmed, int32_indmed, f_char_indmed
end interface indmed
contains
Subroutine real64_indmed (INVALS, OUTORD)
      Real (kind=real64), Dimension (:), Intent (In) :: INVALS
      Integer, Intent (Out) :: OUTORD
! __________________________________________________________
      Integer :: IDON
!
      Allocate (IDONT (SIZE(INVALS)))
      Do IDON = 1, SIZE(INVALS)
         IDONT (IDON) = IDON
      End Do
!
      Call real64_med (INVALS, IDONT, OUTORD)
!
      Deallocate (IDONT)
End Subroutine real64_indmed

Recursive Subroutine real64_med (XDATT, IDATT, ires_med)
      Real (kind=real64), Dimension (:), Intent (In) :: XDATT
      Integer, Dimension (:), Intent (In) :: IDATT
      Integer, Intent (Out):: ires_med
! __________________________________________________________
!
      Real (kind=real64), Parameter :: XHUGE = -HUGE (XDATT)
      Real (kind=real64) :: XWRK, XWRK1, XMED7, XMAX, XMIN
!
      Integer, Dimension (7*(((Size (IDATT)+6)/7+6)/7)) :: ISTRT, IENDT, IMEDT
      Integer, Dimension (7*((Size(IDATT)+6)/7)) :: IWRKT
      Integer :: NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, ITMP, IDCR, ICRS, ICRS1, ICRS2, IMAX, IMIN
      Integer :: IWRK, IWRK1, IMED1, IMED7, NDAT
!
      NDAT = Size (IDATT)
      NMED = (NDAT+1) / 2
      IWRKT(:size(idatt)) = IDATT
!
!  If the number of values is small, then use insertion sort
!
     If (NDAT < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDAT
         If (XDATT (IWRKT (1)) < XDATT (IWRKT (IDCR))) Then
            IWRK = IWRKT (1)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (1)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, NDAT - 2
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                XWRK = XWRK1
                IWRK = IWRK1
            Endif
         End Do
         IWRKT (1) = IWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            IDCR = ICRS - 1
            Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         XWRK1 = XDATT (IWRKT (NMED))
         Do ICRS = NMED+1, NDAT
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            If (XWRK < XWRK1) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               IWRKT (IDCR+1) = IWRK
               XWRK1 = XDATT (IWRKT (NMED))
            End If
         End Do
         ires_med = IWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  the loop test in the insertion loop.
!
      IMAX = 1
      IMIN = 1
      XMAX = XDATT (IWRKT(IMAX))
      XMIN = XDATT (IWRKT(IMIN))
      DO IDEB = 1, NDAT-6, 7
         IDCR = IDEB + 6
         If (XDATT (IWRKT(IDEB)) < XDATT (IWRKT(IDCR))) Then
            IWRK = IWRKT(IDEB)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT(IDEB)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, 5
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                IWRK = IWRK1
                XWRK = XWRK1
            Endif
         End Do
         IWRKT (IDEB) = IWRK
         If (XWRK < XMIN) Then
             IMIN = IWRK
             XMIN = XWRK
         End If
         Do ICRS = IDEB+1, IDEB+5
            IWRK = IWRKT (ICRS+1)
            XWRK = XDATT (IWRK)
            IDON = IWRKT(ICRS)
            If (XWRK < XDATT(IDON)) Then
               IWRKT (ICRS+1) = IDON
               IDCR = ICRS
               IWRK1 = IWRKT (IDCR-1)
               XWRK1 = XDATT (IWRK1)
               Do
                  If (XWRK >= XWRK1) Exit
                  IWRKT (IDCR) = IWRK1
                  IDCR = IDCR - 1
                  IWRK1 = IWRKT (IDCR-1)
                  XWRK1 = XDATT (IWRK1)
               End Do
               IWRKT (IDCR) = IWRK
            EndIf
         End Do
         If (XWRK > XMAX) Then
             IMAX = IWRK
             XMAX = XWRK
         End If
      End Do
!
!  Add-up alternatively MAX and MIN values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDAT/7)
      NTRI = NDAT
      If (IDEB < NDAT) Then
!
         Do ICRS = IDEB+1, NDAT
            XWRK1 = XDATT (IWRKT (ICRS))
            IF (XWRK1 > XMAX) Then
               IMAX = IWRKT (ICRS)
               XMAX = XWRK1
            End If
            IF (XWRK1 < XMIN) Then
               IMIN = IWRKT (ICRS)
               XMIN = XWRK1
            End If
         End Do
         IWRK1 = IMAX
         Do ICRS = NDAT+1, IDEB+7
               IWRKT (ICRS) = IWRK1
               If (IWRK1 == IMAX) Then
                  IWRK1 = IMIN
               Else
                  NMED = NMED + 1
                  IWRK1 = IMAX
               End If
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            IWRK = IWRKT (ICRS)
            XWRK = XDATT (IWRK)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XDATT (IWRKT(IDCR))) Exit
               IWRKT (IDCR+1) = IWRKT (IDCR)
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
         NTRI = IDEB+7
      End If
!
!  Make the set of the indices of median values of each sorted subset
!
         IDON1 = 0
         Do IDON = 1, NTRI, 7
            IDON1 = IDON1 + 1
            IMEDT (IDON1) = IWRKT (IDON + 3)
         End Do
!
!  Find XMED7, the median of the medians
!
         Call real64_med (XDATT, IMEDT(1:IDON1), IMED7)
         XMED7 = XDATT (IMED7)
!
!  Count how many values are not higher than (and how many equal to) XMED7
!  This number is at least 4 * 1/2 * (N/7) : 4 values in each of the
!  subsets where the median is lower than the median of medians. For similar
!  reasons, we also have at least 2N/7 values not lower than XMED7. At the
!  same time, we find in each subset the index of the last value < XMED7,
!  and that of the first > XMED7. These indices will be used to restrict the
!  search for the median as the Kth element in the subset (> or <) where
!  we know it to be.
!
         IDON1 = 1
         NLEQ = 0
         NEQU = 0
         Do IDON = 1, NTRI, 7
            IMED = IDON+3
            If (XDATT (IWRKT (IMED)) > XMED7) Then
                  IMED = IMED - 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                  IMED = IMED + 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XDATT (IWRKT (IMED)) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XDATT (IWRKT (IMED)) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     NLEQ = NLEQ + 1
                     ISTRT (IDON1) = IMED1 + 1
                  Else
                     Exit
                  End If
               End Do
            Endif
            IDON1 = IDON1 + 1
         End Do
!
!  Carry out a partial insertion sort to find the Kth smallest of the
!  large values, or the Kth largest of the small values, according to
!  what is needed.
!
!
         If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                IWRK1 = IMAX
                XWRK1 = XDATT (IWRK1)
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) < XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT (ICRS1)
                            XWRK1 = XDATT (IWRK1)
                         Else
                           If (ICRS2 < NORD) Then
                              IWRKT (ICRS1) = IWRKT (ICRS)
                              IWRK1 = IWRKT (ICRS1)
                              XWRK1 = XDATT (IWRK1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) >= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT (ICRS1)
                         XWRK1 = XDATT (IWRK1)
                      End Do
                   End If
                End Do
                ires_med = IWRK1
                Return
            Else
                ires_med = IMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XDATT(IWRKT (ICRS)) > XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT(ICRS1)
                            XWRK1 = XDATT(IWRK1)
                         Else
                            If (ICRS2 < NORD) Then
                               IWRKT (ICRS1) = IWRKT (ICRS)
                               IWRK1 = IWRKT(ICRS1)
                               XWRK1 = XDATT(IWRK1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XDATT(IWRKT (ICRS)) <= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         IDCR = ICRS1 - 1
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT(ICRS1)
                         XWRK1 = XDATT(IWRK1)
                      End Do
                   Endif
                End Do
!
                ires_med = IWRK1
                Return
         End If
!
END Subroutine real64_med
Subroutine real32_indmed (INVALS, OUTORD)
      Real (kind=real32), Dimension (:), Intent (In) :: INVALS
      Integer, Intent (Out) :: OUTORD
! __________________________________________________________
      Integer :: IDON
!
      Allocate (IDONT (SIZE(INVALS)))
      Do IDON = 1, SIZE(INVALS)
         IDONT (IDON) = IDON
      End Do
!
      Call real32_med (INVALS, IDONT, OUTORD)
!
      Deallocate (IDONT)
End Subroutine real32_indmed

Recursive Subroutine real32_med (XDATT, IDATT, ires_med)
      Real (kind=real32), Dimension (:), Intent (In) :: XDATT
      Integer, Dimension (:), Intent (In) :: IDATT
      Integer, Intent (Out):: ires_med
! __________________________________________________________
!
      Real (kind=real32), Parameter :: XHUGE = -HUGE (XDATT)
      Real (kind=real32) :: XWRK, XWRK1, XMED7, XMAX, XMIN
!
      Integer, Dimension (7*(((Size (IDATT)+6)/7+6)/7)) :: ISTRT, IENDT, IMEDT
      Integer, Dimension (7*((Size(IDATT)+6)/7)) :: IWRKT
      Integer :: NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, ITMP, IDCR, ICRS, ICRS1, ICRS2, IMAX, IMIN
      Integer :: IWRK, IWRK1, IMED1, IMED7, NDAT
!
      NDAT = Size (IDATT)
      NMED = (NDAT+1) / 2
      IWRKT(:size(idatt)) = IDATT
!
!  If the number of values is small, then use insertion sort
!
     If (NDAT < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDAT
         If (XDATT (IWRKT (1)) < XDATT (IWRKT (IDCR))) Then
            IWRK = IWRKT (1)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (1)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, NDAT - 2
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                XWRK = XWRK1
                IWRK = IWRK1
            Endif
         End Do
         IWRKT (1) = IWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            IDCR = ICRS - 1
            Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         XWRK1 = XDATT (IWRKT (NMED))
         Do ICRS = NMED+1, NDAT
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            If (XWRK < XWRK1) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               IWRKT (IDCR+1) = IWRK
               XWRK1 = XDATT (IWRKT (NMED))
            End If
         End Do
         ires_med = IWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  the loop test in the insertion loop.
!
      IMAX = 1
      IMIN = 1
      XMAX = XDATT (IWRKT(IMAX))
      XMIN = XDATT (IWRKT(IMIN))
      DO IDEB = 1, NDAT-6, 7
         IDCR = IDEB + 6
         If (XDATT (IWRKT(IDEB)) < XDATT (IWRKT(IDCR))) Then
            IWRK = IWRKT(IDEB)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT(IDEB)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, 5
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                IWRK = IWRK1
                XWRK = XWRK1
            Endif
         End Do
         IWRKT (IDEB) = IWRK
         If (XWRK < XMIN) Then
             IMIN = IWRK
             XMIN = XWRK
         End If
         Do ICRS = IDEB+1, IDEB+5
            IWRK = IWRKT (ICRS+1)
            XWRK = XDATT (IWRK)
            IDON = IWRKT(ICRS)
            If (XWRK < XDATT(IDON)) Then
               IWRKT (ICRS+1) = IDON
               IDCR = ICRS
               IWRK1 = IWRKT (IDCR-1)
               XWRK1 = XDATT (IWRK1)
               Do
                  If (XWRK >= XWRK1) Exit
                  IWRKT (IDCR) = IWRK1
                  IDCR = IDCR - 1
                  IWRK1 = IWRKT (IDCR-1)
                  XWRK1 = XDATT (IWRK1)
               End Do
               IWRKT (IDCR) = IWRK
            EndIf
         End Do
         If (XWRK > XMAX) Then
             IMAX = IWRK
             XMAX = XWRK
         End If
      End Do
!
!  Add-up alternatively MAX and MIN values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDAT/7)
      NTRI = NDAT
      If (IDEB < NDAT) Then
!
         Do ICRS = IDEB+1, NDAT
            XWRK1 = XDATT (IWRKT (ICRS))
            IF (XWRK1 > XMAX) Then
               IMAX = IWRKT (ICRS)
               XMAX = XWRK1
            End If
            IF (XWRK1 < XMIN) Then
               IMIN = IWRKT (ICRS)
               XMIN = XWRK1
            End If
         End Do
         IWRK1 = IMAX
         Do ICRS = NDAT+1, IDEB+7
               IWRKT (ICRS) = IWRK1
               If (IWRK1 == IMAX) Then
                  IWRK1 = IMIN
               Else
                  NMED = NMED + 1
                  IWRK1 = IMAX
               End If
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            IWRK = IWRKT (ICRS)
            XWRK = XDATT (IWRK)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XDATT (IWRKT(IDCR))) Exit
               IWRKT (IDCR+1) = IWRKT (IDCR)
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
         NTRI = IDEB+7
      End If
!
!  Make the set of the indices of median values of each sorted subset
!
         IDON1 = 0
         Do IDON = 1, NTRI, 7
            IDON1 = IDON1 + 1
            IMEDT (IDON1) = IWRKT (IDON + 3)
         End Do
!
!  Find XMED7, the median of the medians
!
         Call real32_med (XDATT, IMEDT(1:IDON1), IMED7)
         XMED7 = XDATT (IMED7)
!
!  Count how many values are not higher than (and how many equal to) XMED7
!  This number is at least 4 * 1/2 * (N/7) : 4 values in each of the
!  subsets where the median is lower than the median of medians. For similar
!  reasons, we also have at least 2N/7 values not lower than XMED7. At the
!  same time, we find in each subset the index of the last value < XMED7,
!  and that of the first > XMED7. These indices will be used to restrict the
!  search for the median as the Kth element in the subset (> or <) where
!  we know it to be.
!
         IDON1 = 1
         NLEQ = 0
         NEQU = 0
         Do IDON = 1, NTRI, 7
            IMED = IDON+3
            If (XDATT (IWRKT (IMED)) > XMED7) Then
                  IMED = IMED - 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                  IMED = IMED + 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XDATT (IWRKT (IMED)) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XDATT (IWRKT (IMED)) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     NLEQ = NLEQ + 1
                     ISTRT (IDON1) = IMED1 + 1
                  Else
                     Exit
                  End If
               End Do
            Endif
            IDON1 = IDON1 + 1
         End Do
!
!  Carry out a partial insertion sort to find the Kth smallest of the
!  large values, or the Kth largest of the small values, according to
!  what is needed.
!
!
         If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                IWRK1 = IMAX
                XWRK1 = XDATT (IWRK1)
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) < XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT (ICRS1)
                            XWRK1 = XDATT (IWRK1)
                         Else
                           If (ICRS2 < NORD) Then
                              IWRKT (ICRS1) = IWRKT (ICRS)
                              IWRK1 = IWRKT (ICRS1)
                              XWRK1 = XDATT (IWRK1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) >= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT (ICRS1)
                         XWRK1 = XDATT (IWRK1)
                      End Do
                   End If
                End Do
                ires_med = IWRK1
                Return
            Else
                ires_med = IMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XDATT(IWRKT (ICRS)) > XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT(ICRS1)
                            XWRK1 = XDATT(IWRK1)
                         Else
                            If (ICRS2 < NORD) Then
                               IWRKT (ICRS1) = IWRKT (ICRS)
                               IWRK1 = IWRKT(ICRS1)
                               XWRK1 = XDATT(IWRK1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XDATT(IWRKT (ICRS)) <= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         IDCR = ICRS1 - 1
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT(ICRS1)
                         XWRK1 = XDATT(IWRK1)
                      End Do
                   Endif
                End Do
!
                ires_med = IWRK1
                Return
         End If
!
END Subroutine real32_med
Subroutine int32_indmed (INVALS, OUTORD)
      Integer (kind=int32), Dimension (:), Intent (In) :: INVALS
      Integer, Intent (Out) :: OUTORD
! __________________________________________________________
      Integer :: IDON
!
      Allocate (IDONT (SIZE(INVALS)))
      Do IDON = 1, SIZE(INVALS)
         IDONT (IDON) = IDON
      End Do
!
      Call int32_med (INVALS, IDONT, OUTORD)
!
      Deallocate (IDONT)
End Subroutine int32_indmed

Recursive Subroutine int32_med (XDATT, IDATT, ires_med)
      Integer (kind=int32), Dimension (:), Intent (In) :: XDATT
      Integer, Dimension (:), Intent (In) :: IDATT
      Integer, Intent (Out):: ires_med
! __________________________________________________________
!
      Integer (kind=int32), Parameter :: XHUGE = -HUGE (XDATT)
      Integer (kind=int32) :: XWRK, XWRK1, XMED7, XMAX, XMIN
!
      Integer, Dimension (7*(((Size (IDATT)+6)/7+6)/7)) :: ISTRT, IENDT, IMEDT
      Integer, Dimension (7*((Size(IDATT)+6)/7)) :: IWRKT
      Integer :: NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, ITMP, IDCR, ICRS, ICRS1, ICRS2, IMAX, IMIN
      Integer :: IWRK, IWRK1, IMED1, IMED7, NDAT
!
      NDAT = Size (IDATT)
      NMED = (NDAT+1) / 2
      IWRKT(:size(idatt)) = IDATT
!
!  If the number of values is small, then use insertion sort
!
     If (NDAT < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDAT
         If (XDATT (IWRKT (1)) < XDATT (IWRKT (IDCR))) Then
            IWRK = IWRKT (1)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (1)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, NDAT - 2
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                XWRK = XWRK1
                IWRK = IWRK1
            Endif
         End Do
         IWRKT (1) = IWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            IDCR = ICRS - 1
            Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         XWRK1 = XDATT (IWRKT (NMED))
         Do ICRS = NMED+1, NDAT
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            If (XWRK < XWRK1) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               IWRKT (IDCR+1) = IWRK
               XWRK1 = XDATT (IWRKT (NMED))
            End If
         End Do
         ires_med = IWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  the loop test in the insertion loop.
!
      IMAX = 1
      IMIN = 1
      XMAX = XDATT (IWRKT(IMAX))
      XMIN = XDATT (IWRKT(IMIN))
      DO IDEB = 1, NDAT-6, 7
         IDCR = IDEB + 6
         If (XDATT (IWRKT(IDEB)) < XDATT (IWRKT(IDCR))) Then
            IWRK = IWRKT(IDEB)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT(IDEB)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, 5
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                IWRK = IWRK1
                XWRK = XWRK1
            Endif
         End Do
         IWRKT (IDEB) = IWRK
         If (XWRK < XMIN) Then
             IMIN = IWRK
             XMIN = XWRK
         End If
         Do ICRS = IDEB+1, IDEB+5
            IWRK = IWRKT (ICRS+1)
            XWRK = XDATT (IWRK)
            IDON = IWRKT(ICRS)
            If (XWRK < XDATT(IDON)) Then
               IWRKT (ICRS+1) = IDON
               IDCR = ICRS
               IWRK1 = IWRKT (IDCR-1)
               XWRK1 = XDATT (IWRK1)
               Do
                  If (XWRK >= XWRK1) Exit
                  IWRKT (IDCR) = IWRK1
                  IDCR = IDCR - 1
                  IWRK1 = IWRKT (IDCR-1)
                  XWRK1 = XDATT (IWRK1)
               End Do
               IWRKT (IDCR) = IWRK
            EndIf
         End Do
         If (XWRK > XMAX) Then
             IMAX = IWRK
             XMAX = XWRK
         End If
      End Do
!
!  Add-up alternatively MAX and MIN values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDAT/7)
      NTRI = NDAT
      If (IDEB < NDAT) Then
!
         Do ICRS = IDEB+1, NDAT
            XWRK1 = XDATT (IWRKT (ICRS))
            IF (XWRK1 > XMAX) Then
               IMAX = IWRKT (ICRS)
               XMAX = XWRK1
            End If
            IF (XWRK1 < XMIN) Then
               IMIN = IWRKT (ICRS)
               XMIN = XWRK1
            End If
         End Do
         IWRK1 = IMAX
         Do ICRS = NDAT+1, IDEB+7
               IWRKT (ICRS) = IWRK1
               If (IWRK1 == IMAX) Then
                  IWRK1 = IMIN
               Else
                  NMED = NMED + 1
                  IWRK1 = IMAX
               End If
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            IWRK = IWRKT (ICRS)
            XWRK = XDATT (IWRK)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XDATT (IWRKT(IDCR))) Exit
               IWRKT (IDCR+1) = IWRKT (IDCR)
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
         NTRI = IDEB+7
      End If
!
!  Make the set of the indices of median values of each sorted subset
!
         IDON1 = 0
         Do IDON = 1, NTRI, 7
            IDON1 = IDON1 + 1
            IMEDT (IDON1) = IWRKT (IDON + 3)
         End Do
!
!  Find XMED7, the median of the medians
!
         Call int32_med (XDATT, IMEDT(1:IDON1), IMED7)
         XMED7 = XDATT (IMED7)
!
!  Count how many values are not higher than (and how many equal to) XMED7
!  This number is at least 4 * 1/2 * (N/7) : 4 values in each of the
!  subsets where the median is lower than the median of medians. For similar
!  reasons, we also have at least 2N/7 values not lower than XMED7. At the
!  same time, we find in each subset the index of the last value < XMED7,
!  and that of the first > XMED7. These indices will be used to restrict the
!  search for the median as the Kth element in the subset (> or <) where
!  we know it to be.
!
         IDON1 = 1
         NLEQ = 0
         NEQU = 0
         Do IDON = 1, NTRI, 7
            IMED = IDON+3
            If (XDATT (IWRKT (IMED)) > XMED7) Then
                  IMED = IMED - 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                  IMED = IMED + 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XDATT (IWRKT (IMED)) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XDATT (IWRKT (IMED)) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     NLEQ = NLEQ + 1
                     ISTRT (IDON1) = IMED1 + 1
                  Else
                     Exit
                  End If
               End Do
            Endif
            IDON1 = IDON1 + 1
         End Do
!
!  Carry out a partial insertion sort to find the Kth smallest of the
!  large values, or the Kth largest of the small values, according to
!  what is needed.
!
!
         If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                IWRK1 = IMAX
                XWRK1 = XDATT (IWRK1)
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) < XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT (ICRS1)
                            XWRK1 = XDATT (IWRK1)
                         Else
                           If (ICRS2 < NORD) Then
                              IWRKT (ICRS1) = IWRKT (ICRS)
                              IWRK1 = IWRKT (ICRS1)
                              XWRK1 = XDATT (IWRK1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) >= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT (ICRS1)
                         XWRK1 = XDATT (IWRK1)
                      End Do
                   End If
                End Do
                ires_med = IWRK1
                Return
            Else
                ires_med = IMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XDATT(IWRKT (ICRS)) > XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT(ICRS1)
                            XWRK1 = XDATT(IWRK1)
                         Else
                            If (ICRS2 < NORD) Then
                               IWRKT (ICRS1) = IWRKT (ICRS)
                               IWRK1 = IWRKT(ICRS1)
                               XWRK1 = XDATT(IWRK1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XDATT(IWRKT (ICRS)) <= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         IDCR = ICRS1 - 1
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT(ICRS1)
                         XWRK1 = XDATT(IWRK1)
                      End Do
                   Endif
                End Do
!
                ires_med = IWRK1
                Return
         End If
!
END Subroutine int32_med
Subroutine f_char_indmed (INVALS, OUTORD)
      character (kind=f_char,len=*), Dimension (:), Intent (In) :: INVALS
      Integer, Intent (Out) :: OUTORD
! __________________________________________________________
      Integer :: IDON
!
      Allocate (IDONT (SIZE(INVALS)))
      Do IDON = 1, SIZE(INVALS)
         IDONT (IDON) = IDON
      End Do
!
      Call f_char_med (INVALS, IDONT, OUTORD)
!
      Deallocate (IDONT)
End Subroutine f_char_indmed

Recursive Subroutine f_char_med (XDATT, IDATT, ires_med)
      character (kind=f_char,len=*), Dimension (:), Intent (In) :: XDATT
      Integer, Dimension (:), Intent (In) :: IDATT
      Integer, Intent (Out):: ires_med
! __________________________________________________________
!
      character (kind=f_char,len=len(XDATT)) :: XHUGE
      character (kind=f_char,len=len(XDATT)) :: XWRK, XWRK1, XMED7, XMAX, XMIN
!
      Integer, Dimension (7*(((Size (IDATT)+6)/7+6)/7)) :: ISTRT, IENDT, IMEDT
      Integer, Dimension (7*((Size(IDATT)+6)/7)) :: IWRKT
      Integer :: NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, ITMP, IDCR, ICRS, ICRS1, ICRS2, IMAX, IMIN
      Integer :: IWRK, IWRK1, IMED1, IMED7, NDAT
!
      XHUGE=repeat(char(0),len(xdatt))
      NDAT = Size (IDATT)
      NMED = (NDAT+1) / 2
      IWRKT(:size(idatt)) = IDATT
!
!  If the number of values is small, then use insertion sort
!
     If (NDAT < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDAT
         If (XDATT (IWRKT (1)) < XDATT (IWRKT (IDCR))) Then
            IWRK = IWRKT (1)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT (1)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, NDAT - 2
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                XWRK = XWRK1
                IWRK = IWRK1
            Endif
         End Do
         IWRKT (1) = IWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            IDCR = ICRS - 1
            Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         XWRK1 = XDATT (IWRKT (NMED))
         Do ICRS = NMED+1, NDAT
            XWRK = XDATT (IWRKT (ICRS))
            IWRK = IWRKT (ICRS)
            If (XWRK < XWRK1) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XDATT (IWRKT(IDCR))) Exit
                  IWRKT (IDCR+1) = IWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               IWRKT (IDCR+1) = IWRK
               XWRK1 = XDATT (IWRKT (NMED))
            End If
         End Do
         ires_med = IWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  the loop test in the insertion loop.
!
      IMAX = 1
      IMIN = 1
      XMAX = XDATT (IWRKT(IMAX))
      XMIN = XDATT (IWRKT(IMIN))
      DO IDEB = 1, NDAT-6, 7
         IDCR = IDEB + 6
         If (XDATT (IWRKT(IDEB)) < XDATT (IWRKT(IDCR))) Then
            IWRK = IWRKT(IDEB)
         Else
            IWRK = IWRKT (IDCR)
            IWRKT (IDCR) = IWRKT(IDEB)
         Endif
         XWRK = XDATT (IWRK)
         Do ITMP = 1, 5
            IDCR = IDCR - 1
            IWRK1 = IWRKT (IDCR)
            XWRK1 = XDATT (IWRK1)
            If (XWRK1 < XWRK) Then
                IWRKT (IDCR) = IWRK
                IWRK = IWRK1
                XWRK = XWRK1
            Endif
         End Do
         IWRKT (IDEB) = IWRK
         If (XWRK < XMIN) Then
             IMIN = IWRK
             XMIN = XWRK
         End If
         Do ICRS = IDEB+1, IDEB+5
            IWRK = IWRKT (ICRS+1)
            XWRK = XDATT (IWRK)
            IDON = IWRKT(ICRS)
            If (XWRK < XDATT(IDON)) Then
               IWRKT (ICRS+1) = IDON
               IDCR = ICRS
               IWRK1 = IWRKT (IDCR-1)
               XWRK1 = XDATT (IWRK1)
               Do
                  If (XWRK >= XWRK1) Exit
                  IWRKT (IDCR) = IWRK1
                  IDCR = IDCR - 1
                  IWRK1 = IWRKT (IDCR-1)
                  XWRK1 = XDATT (IWRK1)
               End Do
               IWRKT (IDCR) = IWRK
            EndIf
         End Do
         If (XWRK > XMAX) Then
             IMAX = IWRK
             XMAX = XWRK
         End If
      End Do
!
!  Add-up alternatively MAX and MIN values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDAT/7)
      NTRI = NDAT
      If (IDEB < NDAT) Then
!
         Do ICRS = IDEB+1, NDAT
            XWRK1 = XDATT (IWRKT (ICRS))
            IF (XWRK1 > XMAX) Then
               IMAX = IWRKT (ICRS)
               XMAX = XWRK1
            End If
            IF (XWRK1 < XMIN) Then
               IMIN = IWRKT (ICRS)
               XMIN = XWRK1
            End If
         End Do
         IWRK1 = IMAX
         Do ICRS = NDAT+1, IDEB+7
               IWRKT (ICRS) = IWRK1
               If (IWRK1 == IMAX) Then
                  IWRK1 = IMIN
               Else
                  NMED = NMED + 1
                  IWRK1 = IMAX
               End If
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            IWRK = IWRKT (ICRS)
            XWRK = XDATT (IWRK)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XDATT (IWRKT(IDCR))) Exit
               IWRKT (IDCR+1) = IWRKT (IDCR)
            End Do
            IWRKT (IDCR+1) = IWRK
         End Do
!
         NTRI = IDEB+7
      End If
!
!  Make the set of the indices of median values of each sorted subset
!
         IDON1 = 0
         Do IDON = 1, NTRI, 7
            IDON1 = IDON1 + 1
            IMEDT (IDON1) = IWRKT (IDON + 3)
         End Do
!
!  Find XMED7, the median of the medians
!
         Call f_char_med (XDATT, IMEDT(1:IDON1), IMED7)
         XMED7 = XDATT (IMED7)
!
!  Count how many values are not higher than (and how many equal to) XMED7
!  This number is at least 4 * 1/2 * (N/7) : 4 values in each of the
!  subsets where the median is lower than the median of medians. For similar
!  reasons, we also have at least 2N/7 values not lower than XMED7. At the
!  same time, we find in each subset the index of the last value < XMED7,
!  and that of the first > XMED7. These indices will be used to restrict the
!  search for the median as the Kth element in the subset (> or <) where
!  we know it to be.
!
         IDON1 = 1
         NLEQ = 0
         NEQU = 0
         Do IDON = 1, NTRI, 7
            IMED = IDON+3
            If (XDATT (IWRKT (IMED)) > XMED7) Then
                  IMED = IMED - 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                  IMED = IMED + 2
                  If (XDATT (IWRKT (IMED)) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XDATT (IWRKT (IMED)) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XDATT (IWRKT (IMED)) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XDATT (IWRKT (IMED)) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XDATT (IWRKT (IMED)) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XDATT (IWRKT (IMED1)) == XMED7) Then
                     NEQU = NEQU + 1
                     NLEQ = NLEQ + 1
                     ISTRT (IDON1) = IMED1 + 1
                  Else
                     Exit
                  End If
               End Do
            Endif
            IDON1 = IDON1 + 1
         End Do
!
!  Carry out a partial insertion sort to find the Kth smallest of the
!  large values, or the Kth largest of the small values, according to
!  what is needed.
!
!
         If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                IWRK1 = IMAX
                XWRK1 = XDATT (IWRK1)
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) < XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT (ICRS1)
                            XWRK1 = XDATT (IWRK1)
                         Else
                           If (ICRS2 < NORD) Then
                              IWRKT (ICRS1) = IWRKT (ICRS)
                              IWRK1 = IWRKT (ICRS1)
                              XWRK1 = XDATT (IWRK1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XDATT (IWRKT (ICRS)) >= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XDATT (IWRKT (IDCR))) Exit
                               IWRKT  (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT (ICRS1)
                         XWRK1 = XDATT (IWRK1)
                      End Do
                   End If
                End Do
                ires_med = IWRK1
                Return
            Else
                ires_med = IMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XDATT(IWRKT (ICRS)) > XWRK1) Then
                            IWRK = IWRKT (ICRS)
                            XWRK = XDATT (IWRK)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                            End Do
                            IWRKT (IDCR+1) = IWRK
                            IWRK1 = IWRKT(ICRS1)
                            XWRK1 = XDATT(IWRK1)
                         Else
                            If (ICRS2 < NORD) Then
                               IWRKT (ICRS1) = IWRKT (ICRS)
                               IWRK1 = IWRKT(ICRS1)
                               XWRK1 = XDATT(IWRK1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XDATT(IWRKT (ICRS)) <= XWRK1) Exit
                         IWRK = IWRKT (ICRS)
                         XWRK = XDATT (IWRK)
                         IDCR = ICRS1 - 1
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XDATT(IWRKT (IDCR))) Exit
                               IWRKT (IDCR+1) = IWRKT (IDCR)
                         End Do
                         IWRKT (IDCR+1) = IWRK
                         IWRK1 = IWRKT(ICRS1)
                         XWRK1 = XDATT(IWRK1)
                      End Do
                   Endif
                End Do
!
                ires_med = IWRK1
                Return
         End If
!
END Subroutine f_char_med
end module M_indmed

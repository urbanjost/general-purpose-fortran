Module M_valmed
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: valmed
!>
!!##NAME
!!    medianval(3f) - [orderpack:MEDIAN] Returns median VALUE.
!!
!!##SYNOPSIS
!!
!!     Recursive Function MedianVal (INVALS) Result (RES_MED)
!!
!!       ${TYPE} (kind=${KIND}),  Intent (In) :: INVALS(:)
!!       ${TYPE} (kind=${KIND})               :: RES_MED
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!
!!##DESCRIPTION
!!
!!    Finds out and returns the median (((Size(INVALS)+1))/2^th value)
!!    of INVALS.
!!
!!    Internally, it uses the recursive procedure described in Knuth,
!!    The Art of Computer Programming, vol. 3, 5.3.3 .
!!
!!    The procedure is linear in time, and does not require to be able to
!!    interpolate in the set as the one used in ORDERVAL(3f)/ORDERLOC(3f). It
!!    also has better worst case behavior than ORDERVAL(3f)/ORDERLOC(3f), and
!!    is about 20% faster in average for random uniformly distributed values.
!!
!!##OPTIONS
!!     INVALS      input array
!!
!!##RETURNS
!!     RES_MED    the median value of the array INVALS
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_medianval
!!    ! return median value
!!    use M_orderpack, only : medianval
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!       write(*,g)'real   ',&
!!       medianval( [80.0,70.0,20.0,10.0,1000.0] )
!!       write(*,g)'integer',&
!!       medianval( [11, 22, 33, 44, 55, 66, 77, 88] )
!!       write(*,g)'double ',&
!!       medianval( [11.0d0, 22.0d0, 33.0d0, 66.0d0, 77.0d0, 88.0d0] )
!!    end program demo_medianval
!!
!!   Results:
!!
!!    real    70.00000
!!    integer 44
!!    double  33.00000000000000
!!
!!##AUTHOR
!!    Michel Olagnon, 2000-2012
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface valmed
  module procedure real64_valmed, real32_valmed, int32_valmed
end interface valmed
contains
Recursive Function real64_valmed (INVALS) Result (res_med)
!!__________________________________________________________
      Real (kind=real64), Dimension (:), Intent (In) :: INVALS
      Real (kind=real64) :: res_med
! __________________________________________________________
      Real (kind=real64), Parameter :: XHUGE = HUGE (INVALS)
      Real (kind=real64), Dimension (SIZE(INVALS)+6) :: XWRKT
      Real (kind=real64) :: XWRK, XWRK1, XMED7
!
      Integer, Dimension ((SIZE(INVALS)+6)/7) :: ISTRT, IENDT, IMEDT
      Integer :: NDON, NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, IWRK, IDCR, ICRS, ICRS1, ICRS2, IMED1
!
      NDON = SIZE (INVALS)
      NMED = (NDON+1) / 2
!      write(unit=*,fmt=*) NMED, NDON
!
!  If the number of values is small, then use insertion sort
!
      If (NDON < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDON
         If (INVALS (1) < INVALS (NDON)) Then
            XWRK = INVALS (1)
            XWRKT (IDCR) = INVALS (IDCR)
         Else
            XWRK = INVALS (IDCR)
            XWRKT (IDCR) = INVALS (1)
         Endif
         Do IWRK = 1, NDON - 2
            IDCR = IDCR - 1
            XWRK1 = INVALS (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (1) = XWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XWRKT (ICRS)
               IDCR = ICRS - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         Do ICRS = NMED+1, NDON
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT (NMED)) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               XWRKT (IDCR+1) = XWRK
            End If
         End Do
         res_med = XWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  remove the loop test in the insertion loop.
!
      DO IDEB = 1, NDON-6, 7
         IDCR = IDEB + 6
         If (INVALS (IDEB) < INVALS (IDCR)) Then
            XWRK = INVALS (IDEB)
            XWRKT (IDCR) = INVALS (IDCR)
         Else
            XWRK = INVALS (IDCR)
            XWRKT (IDCR) = INVALS (IDEB)
         Endif
         Do IWRK = 1, 5
            IDCR = IDCR - 1
            XWRK1 = INVALS (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (IDEB) = XWRK
         Do ICRS = IDEB+2, IDEB+6
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT(ICRS-1)) Then
               XWRKT (ICRS) = XWRKT (ICRS-1)
               IDCR = ICRS - 1
               XWRK1 = XWRKT (IDCR-1)
               Do
                  If (XWRK >= XWRK1) Exit
                  XWRKT (IDCR) = XWRK1
                  IDCR = IDCR - 1
                  XWRK1 = XWRKT (IDCR-1)
               End Do
               XWRKT (IDCR) = XWRK
            EndIf
         End Do
      End Do
!
!  Add-up alternatively + and - HUGE values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDON/7)
      NTRI = NDON
      If (IDEB < NDON) Then
!
         XWRK1 = XHUGE
         Do ICRS = IDEB+1, IDEB+7
            If (ICRS <= NDON) Then
               XWRKT (ICRS) = INVALS (ICRS)
            Else
               If (XWRK1 /= XHUGE) NMED = NMED + 1
               XWRKT (ICRS) = XWRK1
               XWRK1 = - XWRK1
            Endif
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            XWRK = XWRKT (ICRS)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
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
            IMEDT (IDON1) = IDON + 3
         End Do
!
!  Find XMED7, the median of the medians
!
         XMED7 = real64_valmed (XWRKT (IMEDT))
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
            If (XWRKT (IMED) > XMED7) Then
                  IMED = IMED - 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XWRKT (IMED) < XMED7) Then
                  IMED = IMED + 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XWRKT (IMED) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XWRKT (IMED) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XWRKT (IMED) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XWRKT (IMED1) == XMED7) Then
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
        If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                XWRK1 = XHUGE
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
               Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) < XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                           If (ICRS2 < NORD) Then
                              XWRKT (ICRS1) = XWRKT (ICRS)
                              XWRK1 = XWRKT(ICRS1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) >= XWRK1) Exit
                         XWRK = XWRKT (ICRS)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                         End Do
                         XWRKT (IDCR+1) = XWRK
                         XWRK1 = XWRKT(ICRS1)
                      End Do
                   End If
                End Do
                res_med = XWRK1
                Return
            Else
                res_med = XMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = -XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            If (ICRS2 < NORD) Then
                               XWRKT (ICRS1) = XWRKT (ICRS)
                               XWRK1 = XWRKT (ICRS1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            Exit
                         End If
                      End Do
                   Endif
                End Do
!
                res_med = XWRK1
                Return
         End If
!
End Function real64_valmed
Recursive Function real32_valmed (INVALS) Result (res_med)
!!__________________________________________________________
      Real (kind=real32), Dimension (:), Intent (In) :: INVALS
      Real (kind=real32) :: res_med
! __________________________________________________________
      Real (kind=real32), Parameter :: XHUGE = HUGE (INVALS)
      Real (kind=real32), Dimension (SIZE(INVALS)+6) :: XWRKT
      Real (kind=real32) :: XWRK, XWRK1, XMED7
!
      Integer, Dimension ((SIZE(INVALS)+6)/7) :: ISTRT, IENDT, IMEDT
      Integer :: NDON, NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, IWRK, IDCR, ICRS, ICRS1, ICRS2, IMED1
!
      NDON = SIZE (INVALS)
      NMED = (NDON+1) / 2
!      write(unit=*,fmt=*) NMED, NDON
!
!  If the number of values is small, then use insertion sort
!
      If (NDON < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDON
         If (INVALS (1) < INVALS (NDON)) Then
            XWRK = INVALS (1)
            XWRKT (IDCR) = INVALS (IDCR)
         Else
            XWRK = INVALS (IDCR)
            XWRKT (IDCR) = INVALS (1)
         Endif
         Do IWRK = 1, NDON - 2
            IDCR = IDCR - 1
            XWRK1 = INVALS (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (1) = XWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XWRKT (ICRS)
               IDCR = ICRS - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         Do ICRS = NMED+1, NDON
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT (NMED)) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               XWRKT (IDCR+1) = XWRK
            End If
         End Do
         res_med = XWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  remove the loop test in the insertion loop.
!
      DO IDEB = 1, NDON-6, 7
         IDCR = IDEB + 6
         If (INVALS (IDEB) < INVALS (IDCR)) Then
            XWRK = INVALS (IDEB)
            XWRKT (IDCR) = INVALS (IDCR)
         Else
            XWRK = INVALS (IDCR)
            XWRKT (IDCR) = INVALS (IDEB)
         Endif
         Do IWRK = 1, 5
            IDCR = IDCR - 1
            XWRK1 = INVALS (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (IDEB) = XWRK
         Do ICRS = IDEB+2, IDEB+6
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT(ICRS-1)) Then
               XWRKT (ICRS) = XWRKT (ICRS-1)
               IDCR = ICRS - 1
               XWRK1 = XWRKT (IDCR-1)
               Do
                  If (XWRK >= XWRK1) Exit
                  XWRKT (IDCR) = XWRK1
                  IDCR = IDCR - 1
                  XWRK1 = XWRKT (IDCR-1)
               End Do
               XWRKT (IDCR) = XWRK
            EndIf
         End Do
      End Do
!
!  Add-up alternatively + and - HUGE values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDON/7)
      NTRI = NDON
      If (IDEB < NDON) Then
!
         XWRK1 = XHUGE
         Do ICRS = IDEB+1, IDEB+7
            If (ICRS <= NDON) Then
               XWRKT (ICRS) = INVALS (ICRS)
            Else
               If (XWRK1 /= XHUGE) NMED = NMED + 1
               XWRKT (ICRS) = XWRK1
               XWRK1 = - XWRK1
            Endif
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            XWRK = XWRKT (ICRS)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
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
            IMEDT (IDON1) = IDON + 3
         End Do
!
!  Find XMED7, the median of the medians
!
         XMED7 = real32_valmed (XWRKT (IMEDT))
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
            If (XWRKT (IMED) > XMED7) Then
                  IMED = IMED - 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XWRKT (IMED) < XMED7) Then
                  IMED = IMED + 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XWRKT (IMED) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XWRKT (IMED) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XWRKT (IMED) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XWRKT (IMED1) == XMED7) Then
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
        If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                XWRK1 = XHUGE
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
               Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) < XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                           If (ICRS2 < NORD) Then
                              XWRKT (ICRS1) = XWRKT (ICRS)
                              XWRK1 = XWRKT(ICRS1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) >= XWRK1) Exit
                         XWRK = XWRKT (ICRS)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                         End Do
                         XWRKT (IDCR+1) = XWRK
                         XWRK1 = XWRKT(ICRS1)
                      End Do
                   End If
                End Do
                res_med = XWRK1
                Return
            Else
                res_med = XMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = -XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            If (ICRS2 < NORD) Then
                               XWRKT (ICRS1) = XWRKT (ICRS)
                               XWRK1 = XWRKT (ICRS1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            Exit
                         End If
                      End Do
                   Endif
                End Do
!
                res_med = XWRK1
                Return
         End If
!
End Function real32_valmed
Recursive Function int32_valmed (INVALS) Result (res_med)
!!__________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (In) :: INVALS
      Integer (kind=int32) :: res_med
! __________________________________________________________
      Integer (kind=int32), Parameter :: XHUGE = HUGE (INVALS)
      Integer (kind=int32), Dimension (SIZE(INVALS)+6) :: XWRKT
      Integer (kind=int32) :: XWRK, XWRK1, XMED7
!
      Integer, Dimension ((SIZE(INVALS)+6)/7) :: ISTRT, IENDT, IMEDT
      Integer :: NDON, NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, IWRK, IDCR, ICRS, ICRS1, ICRS2, IMED1
!
      NDON = SIZE (INVALS)
      NMED = (NDON+1) / 2
!      write(unit=*,fmt=*) NMED, NDON
!
!  If the number of values is small, then use insertion sort
!
      If (NDON < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDON
         If (INVALS (1) < INVALS (NDON)) Then
            XWRK = INVALS (1)
            XWRKT (IDCR) = INVALS (IDCR)
         Else
            XWRK = INVALS (IDCR)
            XWRKT (IDCR) = INVALS (1)
         Endif
         Do IWRK = 1, NDON - 2
            IDCR = IDCR - 1
            XWRK1 = INVALS (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (1) = XWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XWRKT (ICRS)
               IDCR = ICRS - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         Do ICRS = NMED+1, NDON
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT (NMED)) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               XWRKT (IDCR+1) = XWRK
            End If
         End Do
         res_med = XWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  remove the loop test in the insertion loop.
!
      DO IDEB = 1, NDON-6, 7
         IDCR = IDEB + 6
         If (INVALS (IDEB) < INVALS (IDCR)) Then
            XWRK = INVALS (IDEB)
            XWRKT (IDCR) = INVALS (IDCR)
         Else
            XWRK = INVALS (IDCR)
            XWRKT (IDCR) = INVALS (IDEB)
         Endif
         Do IWRK = 1, 5
            IDCR = IDCR - 1
            XWRK1 = INVALS (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (IDEB) = XWRK
         Do ICRS = IDEB+2, IDEB+6
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT(ICRS-1)) Then
               XWRKT (ICRS) = XWRKT (ICRS-1)
               IDCR = ICRS - 1
               XWRK1 = XWRKT (IDCR-1)
               Do
                  If (XWRK >= XWRK1) Exit
                  XWRKT (IDCR) = XWRK1
                  IDCR = IDCR - 1
                  XWRK1 = XWRKT (IDCR-1)
               End Do
               XWRKT (IDCR) = XWRK
            EndIf
         End Do
      End Do
!
!  Add-up alternatively + and - HUGE values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDON/7)
      NTRI = NDON
      If (IDEB < NDON) Then
!
         XWRK1 = XHUGE
         Do ICRS = IDEB+1, IDEB+7
            If (ICRS <= NDON) Then
               XWRKT (ICRS) = INVALS (ICRS)
            Else
               If (XWRK1 /= XHUGE) NMED = NMED + 1
               XWRKT (ICRS) = XWRK1
               XWRK1 = - XWRK1
            Endif
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            XWRK = XWRKT (ICRS)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
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
            IMEDT (IDON1) = IDON + 3
         End Do
!
!  Find XMED7, the median of the medians
!
         XMED7 = int32_valmed (XWRKT (IMEDT))
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
            If (XWRKT (IMED) > XMED7) Then
                  IMED = IMED - 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XWRKT (IMED) < XMED7) Then
                  IMED = IMED + 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XWRKT (IMED) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XWRKT (IMED) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XWRKT (IMED) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XWRKT (IMED1) == XMED7) Then
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
        If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                XWRK1 = XHUGE
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
               Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) < XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                           If (ICRS2 < NORD) Then
                              XWRKT (ICRS1) = XWRKT (ICRS)
                              XWRK1 = XWRKT(ICRS1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) >= XWRK1) Exit
                         XWRK = XWRKT (ICRS)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                         End Do
                         XWRKT (IDCR+1) = XWRK
                         XWRK1 = XWRKT(ICRS1)
                      End Do
                   End If
                End Do
                res_med = XWRK1
                Return
            Else
                res_med = XMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = -XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            If (ICRS2 < NORD) Then
                               XWRKT (ICRS1) = XWRKT (ICRS)
                               XWRK1 = XWRKT (ICRS1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            Exit
                         End If
                      End Do
                   Endif
                End Do
!
                res_med = XWRK1
                Return
         End If
!
End Function int32_valmed
end module M_valmed

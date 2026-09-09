Module M_orderpack__rnkpar
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: rnkpar
!>
!!##NAME
!!    prank(3f) - [M_orderpack:RANK:PARTIAL] partially ranks an array
!!                (Quick-Sort-like)
!!
!!##SYNOPSIS
!!
!!     Subroutine Prank (INVALS, IRNGT, NORD)
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
!!
!!##DESCRIPTION
!!    Partially ranks array INVALS(), returning indices of the requested
!!    number of elements (NORD) in index array IRNGT(), where NORD is the
!!    order ( aka. the number of sorted elements requested).
!!
!!    This routine is refined for speed, and uses a pivoting strategy such
!!    as the one of finding the median based on the quick-sort algorithm,
!!    but we skew the pivot choice to try to bring it to NORD as fast as
!!    possible. It uses two temporary arrays, where it stores the indices of
!!    the values smaller than the pivot (ILOWT), and the indices of values
!!    larger than the pivot that we might still need later on (IHIGT). It
!!    iterates until it can bring the number of values in ILOWT to exactly
!!    NORD, and then uses an insertion sort to rank this set, since it is
!!    supposedly small.
!!
!!##OPTIONS
!!     INVALS      array to rank the elements of
!!     IRNGT      returned ranks
!!     NORD       number of rank values to return
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_prank
!!    ! partially rank array
!!    use,intrinsic :: iso_fortran_env, only : int32, real32, real64
!!    use M_orderpack, only : prank
!!    implicit none
!!    integer,parameter :: ivals=300
!!    real(kind=real32) :: valsr(2000)
!!    real(kind=real32) :: out(ivals)
!!    integer           :: indx(2000)
!!    integer           :: i
!!       call random_seed()
!!       call random_number(valsr)
!!       valsr=valsr*1000000.0-500000.0
!!       call prank(valsr,indx,ivals)
!!       out=valsr(indx(:ivals))
!!       do i=1,ivals-1
!!          if (out(i+1).lt.out(i))then
!!             write(*,*)'not sorted'
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
interface rnkpar
  module procedure real64_rnkpar, real32_rnkpar, int32_rnkpar !, f_char_rnkpar
end interface rnkpar
contains
Subroutine real64_rnkpar (INVALS, IRNGT, NORD)
!!__________________________________________________________
      Real (kind=real64), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Real (kind=real64) :: XPIV, XPIV0, XWRK, XWRK1, XMIN, XMAX
!
      Integer, Dimension (SIZE(INVALS)) :: ILOWT, IHIGT
      Integer :: NDON, JHIG, JLOW, IHIG, IWRK, IWRK1, IWRK2, IWRK3
      Integer :: IDEB, JDEB, IMIL, IFIN, NWRK, ICRS, IDCR, ILOW
      Integer :: JLM2, JLM1, JHM2, JHM1
!
      NDON = SIZE (INVALS)
!
!    First loop is used to fill-in ILOWT, IHIGT at the same time
!
      If (NDON < 2) Then
         If (NORD >= 1) IRNGT (1) = 1
         Return
      End If
!
!  One chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
      If (INVALS(2) < INVALS(1)) Then
         ILOWT (1) = 2
         IHIGT (1) = 1
      Else
         ILOWT (1) = 1
         IHIGT (1) = 2
      End If
!
      If (NDON < 3) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         Return
      End If
!
      If (INVALS(3) <= INVALS(IHIGT(1))) Then
         IHIGT (2) = IHIGT (1)
         If (INVALS(3) < INVALS(ILOWT(1))) Then
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = 3
         Else
            IHIGT (1) = 3
         End If
      Else
         IHIGT (2) = 3
      End If
!
      If (NDON < 4) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         Return
      End If
!
      If (INVALS(NDON) <= INVALS(IHIGT(1))) Then
         IHIGT (3) = IHIGT (2)
         IHIGT (2) = IHIGT (1)
         If (INVALS(NDON) < INVALS(ILOWT(1))) Then
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = NDON
         Else
            IHIGT (1) = NDON
         End If
      Else
         if (INVALS (NDON) < INVALS (IHIGT(2))) Then
            IHIGT (3) = IHIGT (2)
            IHIGT (2) = NDON
         else
            IHIGT (3) = NDON
         endif
      End If
!
      If (NDON < 5) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         If (NORD >= 4) IRNGT (4) = IHIGT (3)
         Return
      End If
!
      JDEB = 0
      IDEB = JDEB + 1
      JLOW = IDEB
      JHIG = 3
      XPIV = INVALS (ILOWT(IDEB)) + REAL(2*NORD)/REAL(NDON+NORD) * &
                                   (INVALS(IHIGT(3))-INVALS(ILOWT(IDEB)))
      If (XPIV >= INVALS(IHIGT(1))) Then
         XPIV = INVALS (ILOWT(IDEB)) + REAL(2*NORD)/REAL(NDON+NORD) * &
                                      (INVALS(IHIGT(2))-INVALS(ILOWT(IDEB)))
         If (XPIV >= INVALS(IHIGT(1))) &
             XPIV = INVALS (ILOWT(IDEB)) + REAL (2*NORD) / REAL (NDON+NORD) * &
                                          (INVALS(IHIGT(1))-INVALS(ILOWT(IDEB)))
      End If
      XPIV0 = XPIV
!
!  One puts values > pivot in the end and those <= pivot
!  at the beginning. This is split in 2 cases, so that
!  we can skip the loop test a number of times.
!  As we are also filling in the work arrays at the same time
!  we stop filling in the IHIGT array as soon as we have more
!  than enough values in ILOWT.
!
!
      If (INVALS(NDON) > XPIV) Then
         ICRS = 3
         Do
            ICRS = ICRS + 1
            If (INVALS(ICRS) > XPIV) Then
               If (ICRS >= NDON) Exit
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do
!
!  One restricts further processing because it is no use
!  to store more high values
!
         If (ICRS < NDON-1) Then
            Do
               ICRS = ICRS + 1
               If (INVALS(ICRS) <= XPIV) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = ICRS
               Else If (ICRS >= NDON) Then
                  Exit
               End If
            End Do
         End If
!
!
      Else
!
!  Same as above, but this is not as easy to optimize, so the
!  DO-loop is kept
!
         Do ICRS = 4, NDON - 1
            If (INVALS(ICRS) > XPIV) Then
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do
!
         If (ICRS < NDON-1) Then
            Do
               ICRS = ICRS + 1
               If (INVALS(ICRS) <= XPIV) Then
                  If (ICRS >= NDON) Exit
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = ICRS
               End If
            End Do
         End If
      End If
!
      JLM2 = 0
      JLM1 = 0
      JHM2 = 0
      JHM1 = 0
      Do
         if (JLOW == NORD) Exit
         If (JLM2 == JLOW .And. JHM2 == JHIG) Then
!
!   We are oscillating. Perturbate by bringing JLOW closer by one
!   to NORD
!
           If (NORD > JLOW) Then
                XMIN = INVALS (IHIGT(1))
                IHIG = 1
                Do ICRS = 2, JHIG
                   If (INVALS(IHIGT(ICRS)) < XMIN) Then
                      XMIN = INVALS (IHIGT(ICRS))
                      IHIG = ICRS
                   End If
                End Do
!
                JLOW = JLOW + 1
                ILOWT (JLOW) = IHIGT (IHIG)
                IHIGT (IHIG) = IHIGT (JHIG)
                JHIG = JHIG - 1
             Else
                ILOW = ILOWT (JLOW)
                XMAX = INVALS (ILOW)
                Do ICRS = 1, JLOW
                   If (INVALS(ILOWT(ICRS)) > XMAX) Then
                      IWRK = ILOWT (ICRS)
                      XMAX = INVALS (IWRK)
                      ILOWT (ICRS) = ILOW
                      ILOW = IWRK
                   End If
                End Do
                JLOW = JLOW - 1
             End If
         End If
         JLM2 = JLM1
         JLM1 = JLOW
         JHM2 = JHM1
         JHM1 = JHIG
!
!   We try to bring the number of values in the low values set
!   closer to NORD.
!
        Select Case (NORD-JLOW)
         Case (2:)
!
!   Not enough values in low part, at least 2 are missing
!
            Select Case (JHIG)
!!!!!           CASE DEFAULT
!!!!!              write (*,*) "Assertion failed"
!!!!!              STOP
!
!   We make a special case when we have so few values in
!   the high values set that it is bad performance to choose a pivot
!   and apply the general algorithm.
!
            Case (2)
               If (INVALS(IHIGT(1)) <= INVALS(IHIGT(2))) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (2)
               Else
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (2)
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
               End If
               Exit
!
            Case (3)
!
!
               IWRK1 = IHIGT (1)
               IWRK2 = IHIGT (2)
               IWRK3 = IHIGT (3)
               If (INVALS(IWRK2) < INVALS(IWRK1)) Then
                  IHIGT (1) = IWRK2
                  IHIGT (2) = IWRK1
                  IWRK2 = IWRK1
               End If
               If (INVALS(IWRK2) > INVALS(IWRK3)) Then
                  IHIGT (3) = IWRK2
                  IHIGT (2) = IWRK3
                  IWRK2 = IWRK3
                  If (INVALS(IWRK2) < INVALS(IHIGT(1))) Then
                     IHIGT (2) = IHIGT (1)
                     IHIGT (1) = IWRK2
                  End If
               End If
               JHIG = 0
               Do ICRS = JLOW + 1, NORD
                  JHIG = JHIG + 1
                  ILOWT (ICRS) = IHIGT (JHIG)
               End Do
               JLOW = NORD
               Exit
!
            Case (4:)
!
!
               XPIV0 = XPIV
               IFIN = JHIG
!
!  One chooses a pivot from the 2 first values and the last one.
!  This should ensure sufficient renewal between iterations to
!  avoid worst case behavior effects.
!
               IWRK1 = IHIGT (1)
               IWRK2 = IHIGT (2)
               IWRK3 = IHIGT (IFIN)
               If (INVALS(IWRK2) < INVALS(IWRK1)) Then
                  IHIGT (1) = IWRK2
                  IHIGT (2) = IWRK1
                  IWRK2 = IWRK1
               End If
               If (INVALS(IWRK2) > INVALS(IWRK3)) Then
                  IHIGT (IFIN) = IWRK2
                  IHIGT (2) = IWRK3
                  IWRK2 = IWRK3
                  If (INVALS(IWRK2) < INVALS(IHIGT(1))) Then
                     IHIGT (2) = IHIGT (1)
                     IHIGT (1) = IWRK2
                  End If
               End If
!
               JDEB = JLOW
               NWRK = NORD - JLOW
               IWRK1 = IHIGT (1)
               JLOW = JLOW + 1
               ILOWT (JLOW) = IWRK1
               XPIV = INVALS (IWRK1) + REAL (NWRK) / REAL (NORD+NWRK) * &
                                      (INVALS(IHIGT(IFIN))-INVALS(IWRK1))
!
!  One takes values <= pivot to ILOWT
!  Again, 2 parts, one where we take care of the remaining
!  high values because we might still need them, and the
!  other when we know that we will have more than enough
!  low values in the end.
!
               JHIG = 0
               Do ICRS = 2, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                     If (JLOW >= NORD) Exit
                  Else
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = IHIGT (ICRS)
                  End If
               End Do
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                  End If
               End Do
           End Select
!
!
         Case (1)
!
!  Only 1 value is missing in low part
!
            XMIN = INVALS (IHIGT(1))
            IHIG = 1
            Do ICRS = 2, JHIG
               If (INVALS(IHIGT(ICRS)) < XMIN) Then
                  XMIN = INVALS (IHIGT(ICRS))
                  IHIG = ICRS
               End If
            End Do
!
            JLOW = JLOW + 1
            ILOWT (JLOW) = IHIGT (IHIG)
            Exit
!
!
         Case (0)
!
!  Low part is exactly what we want
!
            Exit
!
!
         Case (-5:-1)
!
!  Only few values too many in low part
!
            IRNGT (1) = ILOWT (1)
            Do ICRS = 2, NORD
               IWRK = ILOWT (ICRS)
               XWRK = INVALS (IWRK)
               Do IDCR = ICRS - 1, 1, - 1
                  If (XWRK < INVALS(IRNGT(IDCR))) Then
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  Else
                     Exit
                  End If
               End Do
               IRNGT (IDCR+1) = IWRK
            End Do
!
            XWRK1 = INVALS (IRNGT(NORD))
            Do ICRS = NORD + 1, JLOW
               If (INVALS(ILOWT (ICRS)) < XWRK1) Then
                  XWRK = INVALS (ILOWT (ICRS))
                  Do IDCR = NORD - 1, 1, - 1
                     If (XWRK >= INVALS(IRNGT(IDCR))) Exit
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  End Do
                  IRNGT (IDCR+1) = ILOWT (ICRS)
                  XWRK1 = INVALS (IRNGT(NORD))
               End If
            End Do
!
            Return
!
!
         Case (:-6)
!
! last case: too many values in low part
!
            IDEB = JDEB + 1
            IMIL = (JLOW+IDEB) / 2
            IFIN = JLOW
!
!  One chooses a pivot from 1st, last, and middle values
!
            If (INVALS(ILOWT(IMIL)) < INVALS(ILOWT(IDEB))) Then
               IWRK = ILOWT (IDEB)
               ILOWT (IDEB) = ILOWT (IMIL)
               ILOWT (IMIL) = IWRK
            End If
            If (INVALS(ILOWT(IMIL)) > INVALS(ILOWT(IFIN))) Then
               IWRK = ILOWT (IFIN)
               ILOWT (IFIN) = ILOWT (IMIL)
               ILOWT (IMIL) = IWRK
               If (INVALS(ILOWT(IMIL)) < INVALS(ILOWT(IDEB))) Then
                  IWRK = ILOWT (IDEB)
                  ILOWT (IDEB) = ILOWT (IMIL)
                  ILOWT (IMIL) = IWRK
               End If
            End If
            If (IFIN <= 3) Exit
!
            XPIV = INVALS (ILOWT(1)) + REAL(NORD)/REAL(JLOW+NORD) * &
                                      (INVALS(ILOWT(IFIN))-INVALS(ILOWT(1)))
            If (JDEB > 0) Then
               If (XPIV <= XPIV0) &
                   XPIV = XPIV0 + REAL(2*NORD-JDEB)/REAL (JLOW+NORD) * &
                                  (INVALS(ILOWT(IFIN))-XPIV0)
            Else
               IDEB = 1
            End If
!
!  One takes values > XPIV to IHIGT
!  However, we do not process the first values if we have been
!  through the case when we did not have enough low values
!
            JHIG = 0
            JLOW = JDEB
!
            If (INVALS(ILOWT(IFIN)) > XPIV) Then
               ICRS = JDEB
               Do
                 ICRS = ICRS + 1
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                     If (ICRS >= IFIN) Exit
                  Else
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do
!
               If (ICRS < IFIN) Then
                  Do
                     ICRS = ICRS + 1
                     If (INVALS(ILOWT(ICRS)) <= XPIV) Then
                        JLOW = JLOW + 1
                        ILOWT (JLOW) = ILOWT (ICRS)
                     Else
                        If (ICRS >= IFIN) Exit
                     End If
                  End Do
               End If
           Else
               Do ICRS = IDEB, IFIN
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                  Else
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(ILOWT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                  End If
               End Do
            End If
!
         End Select
!
      End Do
!
!  Now, we only need to complete ranking of the 1:NORD set
!  Assuming NORD is small, we use a simple insertion sort
!
      IRNGT (1) = ILOWT (1)
      Do ICRS = 2, NORD
         IWRK = ILOWT (ICRS)
         XWRK = INVALS (IWRK)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK < INVALS(IRNGT(IDCR))) Then
               IRNGT (IDCR+1) = IRNGT (IDCR)
            Else
               Exit
            End If
         End Do
         IRNGT (IDCR+1) = IWRK
      End Do
     Return
!
!
End Subroutine real64_rnkpar
Subroutine real32_rnkpar (INVALS, IRNGT, NORD)
!!__________________________________________________________
      Real (kind=real32), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Real (kind=real32) :: XPIV, XPIV0, XWRK, XWRK1, XMIN, XMAX
!
      Integer, Dimension (SIZE(INVALS)) :: ILOWT, IHIGT
      Integer :: NDON, JHIG, JLOW, IHIG, IWRK, IWRK1, IWRK2, IWRK3
      Integer :: IDEB, JDEB, IMIL, IFIN, NWRK, ICRS, IDCR, ILOW
      Integer :: JLM2, JLM1, JHM2, JHM1
!
      NDON = SIZE (INVALS)
!
!    First loop is used to fill-in ILOWT, IHIGT at the same time
!
      If (NDON < 2) Then
         If (NORD >= 1) IRNGT (1) = 1
         Return
      End If
!
!  One chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
      If (INVALS(2) < INVALS(1)) Then
         ILOWT (1) = 2
         IHIGT (1) = 1
      Else
         ILOWT (1) = 1
         IHIGT (1) = 2
      End If
!
      If (NDON < 3) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         Return
      End If
!
      If (INVALS(3) <= INVALS(IHIGT(1))) Then
         IHIGT (2) = IHIGT (1)
         If (INVALS(3) < INVALS(ILOWT(1))) Then
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = 3
         Else
            IHIGT (1) = 3
         End If
      Else
         IHIGT (2) = 3
      End If
!
      If (NDON < 4) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         Return
      End If
!
      If (INVALS(NDON) <= INVALS(IHIGT(1))) Then
         IHIGT (3) = IHIGT (2)
         IHIGT (2) = IHIGT (1)
         If (INVALS(NDON) < INVALS(ILOWT(1))) Then
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = NDON
         Else
            IHIGT (1) = NDON
         End If
      Else
         if (INVALS (NDON) < INVALS (IHIGT(2))) Then
            IHIGT (3) = IHIGT (2)
            IHIGT (2) = NDON
         else
            IHIGT (3) = NDON
         endif
      End If
!
      If (NDON < 5) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         If (NORD >= 4) IRNGT (4) = IHIGT (3)
         Return
      End If
!
      JDEB = 0
      IDEB = JDEB + 1
      JLOW = IDEB
      JHIG = 3
      XPIV = INVALS (ILOWT(IDEB)) + REAL(2*NORD)/REAL(NDON+NORD) * &
                                   (INVALS(IHIGT(3))-INVALS(ILOWT(IDEB)))
      If (XPIV >= INVALS(IHIGT(1))) Then
         XPIV = INVALS (ILOWT(IDEB)) + REAL(2*NORD)/REAL(NDON+NORD) * &
                                      (INVALS(IHIGT(2))-INVALS(ILOWT(IDEB)))
         If (XPIV >= INVALS(IHIGT(1))) &
             XPIV = INVALS (ILOWT(IDEB)) + REAL (2*NORD) / REAL (NDON+NORD) * &
                                          (INVALS(IHIGT(1))-INVALS(ILOWT(IDEB)))
      End If
      XPIV0 = XPIV
!
!  One puts values > pivot in the end and those <= pivot
!  at the beginning. This is split in 2 cases, so that
!  we can skip the loop test a number of times.
!  As we are also filling in the work arrays at the same time
!  we stop filling in the IHIGT array as soon as we have more
!  than enough values in ILOWT.
!
!
      If (INVALS(NDON) > XPIV) Then
         ICRS = 3
         Do
            ICRS = ICRS + 1
            If (INVALS(ICRS) > XPIV) Then
               If (ICRS >= NDON) Exit
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do
!
!  One restricts further processing because it is no use
!  to store more high values
!
         If (ICRS < NDON-1) Then
            Do
               ICRS = ICRS + 1
               If (INVALS(ICRS) <= XPIV) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = ICRS
               Else If (ICRS >= NDON) Then
                  Exit
               End If
            End Do
         End If
!
!
      Else
!
!  Same as above, but this is not as easy to optimize, so the
!  DO-loop is kept
!
         Do ICRS = 4, NDON - 1
            If (INVALS(ICRS) > XPIV) Then
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do
!
         If (ICRS < NDON-1) Then
            Do
               ICRS = ICRS + 1
               If (INVALS(ICRS) <= XPIV) Then
                  If (ICRS >= NDON) Exit
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = ICRS
               End If
            End Do
         End If
      End If
!
      JLM2 = 0
      JLM1 = 0
      JHM2 = 0
      JHM1 = 0
      Do
         if (JLOW == NORD) Exit
         If (JLM2 == JLOW .And. JHM2 == JHIG) Then
!
!   We are oscillating. Perturbate by bringing JLOW closer by one
!   to NORD
!
           If (NORD > JLOW) Then
                XMIN = INVALS (IHIGT(1))
                IHIG = 1
                Do ICRS = 2, JHIG
                   If (INVALS(IHIGT(ICRS)) < XMIN) Then
                      XMIN = INVALS (IHIGT(ICRS))
                      IHIG = ICRS
                   End If
                End Do
!
                JLOW = JLOW + 1
                ILOWT (JLOW) = IHIGT (IHIG)
                IHIGT (IHIG) = IHIGT (JHIG)
                JHIG = JHIG - 1
             Else
                ILOW = ILOWT (JLOW)
                XMAX = INVALS (ILOW)
                Do ICRS = 1, JLOW
                   If (INVALS(ILOWT(ICRS)) > XMAX) Then
                      IWRK = ILOWT (ICRS)
                      XMAX = INVALS (IWRK)
                      ILOWT (ICRS) = ILOW
                      ILOW = IWRK
                   End If
                End Do
                JLOW = JLOW - 1
             End If
         End If
         JLM2 = JLM1
         JLM1 = JLOW
         JHM2 = JHM1
         JHM1 = JHIG
!
!   We try to bring the number of values in the low values set
!   closer to NORD.
!
        Select Case (NORD-JLOW)
         Case (2:)
!
!   Not enough values in low part, at least 2 are missing
!
            Select Case (JHIG)
!!!!!           CASE DEFAULT
!!!!!              write (*,*) "Assertion failed"
!!!!!              STOP
!
!   We make a special case when we have so few values in
!   the high values set that it is bad performance to choose a pivot
!   and apply the general algorithm.
!
            Case (2)
               If (INVALS(IHIGT(1)) <= INVALS(IHIGT(2))) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (2)
               Else
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (2)
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
               End If
               Exit
!
            Case (3)
!
!
               IWRK1 = IHIGT (1)
               IWRK2 = IHIGT (2)
               IWRK3 = IHIGT (3)
               If (INVALS(IWRK2) < INVALS(IWRK1)) Then
                  IHIGT (1) = IWRK2
                  IHIGT (2) = IWRK1
                  IWRK2 = IWRK1
               End If
               If (INVALS(IWRK2) > INVALS(IWRK3)) Then
                  IHIGT (3) = IWRK2
                  IHIGT (2) = IWRK3
                  IWRK2 = IWRK3
                  If (INVALS(IWRK2) < INVALS(IHIGT(1))) Then
                     IHIGT (2) = IHIGT (1)
                     IHIGT (1) = IWRK2
                  End If
               End If
               JHIG = 0
               Do ICRS = JLOW + 1, NORD
                  JHIG = JHIG + 1
                  ILOWT (ICRS) = IHIGT (JHIG)
               End Do
               JLOW = NORD
               Exit
!
            Case (4:)
!
!
               XPIV0 = XPIV
               IFIN = JHIG
!
!  One chooses a pivot from the 2 first values and the last one.
!  This should ensure sufficient renewal between iterations to
!  avoid worst case behavior effects.
!
               IWRK1 = IHIGT (1)
               IWRK2 = IHIGT (2)
               IWRK3 = IHIGT (IFIN)
               If (INVALS(IWRK2) < INVALS(IWRK1)) Then
                  IHIGT (1) = IWRK2
                  IHIGT (2) = IWRK1
                  IWRK2 = IWRK1
               End If
               If (INVALS(IWRK2) > INVALS(IWRK3)) Then
                  IHIGT (IFIN) = IWRK2
                  IHIGT (2) = IWRK3
                  IWRK2 = IWRK3
                  If (INVALS(IWRK2) < INVALS(IHIGT(1))) Then
                     IHIGT (2) = IHIGT (1)
                     IHIGT (1) = IWRK2
                  End If
               End If
!
               JDEB = JLOW
               NWRK = NORD - JLOW
               IWRK1 = IHIGT (1)
               JLOW = JLOW + 1
               ILOWT (JLOW) = IWRK1
               XPIV = INVALS (IWRK1) + REAL (NWRK) / REAL (NORD+NWRK) * &
                                      (INVALS(IHIGT(IFIN))-INVALS(IWRK1))
!
!  One takes values <= pivot to ILOWT
!  Again, 2 parts, one where we take care of the remaining
!  high values because we might still need them, and the
!  other when we know that we will have more than enough
!  low values in the end.
!
               JHIG = 0
               Do ICRS = 2, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                     If (JLOW >= NORD) Exit
                  Else
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = IHIGT (ICRS)
                  End If
               End Do
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                  End If
               End Do
           End Select
!
!
         Case (1)
!
!  Only 1 value is missing in low part
!
            XMIN = INVALS (IHIGT(1))
            IHIG = 1
            Do ICRS = 2, JHIG
               If (INVALS(IHIGT(ICRS)) < XMIN) Then
                  XMIN = INVALS (IHIGT(ICRS))
                  IHIG = ICRS
               End If
            End Do
!
            JLOW = JLOW + 1
            ILOWT (JLOW) = IHIGT (IHIG)
            Exit
!
!
         Case (0)
!
!  Low part is exactly what we want
!
            Exit
!
!
         Case (-5:-1)
!
!  Only few values too many in low part
!
            IRNGT (1) = ILOWT (1)
            Do ICRS = 2, NORD
               IWRK = ILOWT (ICRS)
               XWRK = INVALS (IWRK)
               Do IDCR = ICRS - 1, 1, - 1
                  If (XWRK < INVALS(IRNGT(IDCR))) Then
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  Else
                     Exit
                  End If
               End Do
               IRNGT (IDCR+1) = IWRK
            End Do
!
            XWRK1 = INVALS (IRNGT(NORD))
            Do ICRS = NORD + 1, JLOW
               If (INVALS(ILOWT (ICRS)) < XWRK1) Then
                  XWRK = INVALS (ILOWT (ICRS))
                  Do IDCR = NORD - 1, 1, - 1
                     If (XWRK >= INVALS(IRNGT(IDCR))) Exit
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  End Do
                  IRNGT (IDCR+1) = ILOWT (ICRS)
                  XWRK1 = INVALS (IRNGT(NORD))
               End If
            End Do
!
            Return
!
!
         Case (:-6)
!
! last case: too many values in low part
!
            IDEB = JDEB + 1
            IMIL = (JLOW+IDEB) / 2
            IFIN = JLOW
!
!  One chooses a pivot from 1st, last, and middle values
!
            If (INVALS(ILOWT(IMIL)) < INVALS(ILOWT(IDEB))) Then
               IWRK = ILOWT (IDEB)
               ILOWT (IDEB) = ILOWT (IMIL)
               ILOWT (IMIL) = IWRK
            End If
            If (INVALS(ILOWT(IMIL)) > INVALS(ILOWT(IFIN))) Then
               IWRK = ILOWT (IFIN)
               ILOWT (IFIN) = ILOWT (IMIL)
               ILOWT (IMIL) = IWRK
               If (INVALS(ILOWT(IMIL)) < INVALS(ILOWT(IDEB))) Then
                  IWRK = ILOWT (IDEB)
                  ILOWT (IDEB) = ILOWT (IMIL)
                  ILOWT (IMIL) = IWRK
               End If
            End If
            If (IFIN <= 3) Exit
!
            XPIV = INVALS (ILOWT(1)) + REAL(NORD)/REAL(JLOW+NORD) * &
                                      (INVALS(ILOWT(IFIN))-INVALS(ILOWT(1)))
            If (JDEB > 0) Then
               If (XPIV <= XPIV0) &
                   XPIV = XPIV0 + REAL(2*NORD-JDEB)/REAL (JLOW+NORD) * &
                                  (INVALS(ILOWT(IFIN))-XPIV0)
            Else
               IDEB = 1
            End If
!
!  One takes values > XPIV to IHIGT
!  However, we do not process the first values if we have been
!  through the case when we did not have enough low values
!
            JHIG = 0
            JLOW = JDEB
!
            If (INVALS(ILOWT(IFIN)) > XPIV) Then
               ICRS = JDEB
               Do
                 ICRS = ICRS + 1
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                     If (ICRS >= IFIN) Exit
                  Else
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do
!
               If (ICRS < IFIN) Then
                  Do
                     ICRS = ICRS + 1
                     If (INVALS(ILOWT(ICRS)) <= XPIV) Then
                        JLOW = JLOW + 1
                        ILOWT (JLOW) = ILOWT (ICRS)
                     Else
                        If (ICRS >= IFIN) Exit
                     End If
                  End Do
               End If
           Else
               Do ICRS = IDEB, IFIN
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                  Else
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(ILOWT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                  End If
               End Do
            End If
!
         End Select
!
      End Do
!
!  Now, we only need to complete ranking of the 1:NORD set
!  Assuming NORD is small, we use a simple insertion sort
!
      IRNGT (1) = ILOWT (1)
      Do ICRS = 2, NORD
         IWRK = ILOWT (ICRS)
         XWRK = INVALS (IWRK)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK < INVALS(IRNGT(IDCR))) Then
               IRNGT (IDCR+1) = IRNGT (IDCR)
            Else
               Exit
            End If
         End Do
         IRNGT (IDCR+1) = IWRK
      End Do
     Return
!
!
End Subroutine real32_rnkpar
Subroutine int32_rnkpar (INVALS, IRNGT, NORD)
!!__________________________________________________________
      Integer (kind=int32), Dimension (:), Intent (In) :: INVALS
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (In) :: NORD
! __________________________________________________________
      Integer (kind=int32) :: XPIV, XPIV0, XWRK, XWRK1, XMIN, XMAX
!
      Integer, Dimension (SIZE(INVALS)) :: ILOWT, IHIGT
      Integer :: NDON, JHIG, JLOW, IHIG, IWRK, IWRK1, IWRK2, IWRK3
      Integer :: IDEB, JDEB, IMIL, IFIN, NWRK, ICRS, IDCR, ILOW
      Integer :: JLM2, JLM1, JHM2, JHM1
!
      NDON = SIZE (INVALS)
!
!    First loop is used to fill-in ILOWT, IHIGT at the same time
!
      If (NDON < 2) Then
         If (NORD >= 1) IRNGT (1) = 1
         Return
      End If
!
!  One chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
      If (INVALS(2) < INVALS(1)) Then
         ILOWT (1) = 2
         IHIGT (1) = 1
      Else
         ILOWT (1) = 1
         IHIGT (1) = 2
      End If
!
      If (NDON < 3) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         Return
      End If
!
      If (INVALS(3) <= INVALS(IHIGT(1))) Then
         IHIGT (2) = IHIGT (1)
         If (INVALS(3) < INVALS(ILOWT(1))) Then
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = 3
         Else
            IHIGT (1) = 3
         End If
      Else
         IHIGT (2) = 3
      End If
!
      If (NDON < 4) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         Return
      End If
!
      If (INVALS(NDON) <= INVALS(IHIGT(1))) Then
         IHIGT (3) = IHIGT (2)
         IHIGT (2) = IHIGT (1)
         If (INVALS(NDON) < INVALS(ILOWT(1))) Then
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = NDON
         Else
            IHIGT (1) = NDON
         End If
      Else
         if (INVALS (NDON) < INVALS (IHIGT(2))) Then
            IHIGT (3) = IHIGT (2)
            IHIGT (2) = NDON
         else
            IHIGT (3) = NDON
         endif
      End If
!
      If (NDON < 5) Then
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         If (NORD >= 4) IRNGT (4) = IHIGT (3)
         Return
      End If
!
      JDEB = 0
      IDEB = JDEB + 1
      JLOW = IDEB
      JHIG = 3
      XPIV = INVALS (ILOWT(IDEB)) + REAL(2*NORD)/REAL(NDON+NORD) * &
                                   (INVALS(IHIGT(3))-INVALS(ILOWT(IDEB)))
      If (XPIV >= INVALS(IHIGT(1))) Then
         XPIV = INVALS (ILOWT(IDEB)) + REAL(2*NORD)/REAL(NDON+NORD) * &
                                      (INVALS(IHIGT(2))-INVALS(ILOWT(IDEB)))
         If (XPIV >= INVALS(IHIGT(1))) &
             XPIV = INVALS (ILOWT(IDEB)) + REAL (2*NORD) / REAL (NDON+NORD) * &
                                          (INVALS(IHIGT(1))-INVALS(ILOWT(IDEB)))
      End If
      XPIV0 = XPIV
!
!  One puts values > pivot in the end and those <= pivot
!  at the beginning. This is split in 2 cases, so that
!  we can skip the loop test a number of times.
!  As we are also filling in the work arrays at the same time
!  we stop filling in the IHIGT array as soon as we have more
!  than enough values in ILOWT.
!
!
      If (INVALS(NDON) > XPIV) Then
         ICRS = 3
         Do
            ICRS = ICRS + 1
            If (INVALS(ICRS) > XPIV) Then
               If (ICRS >= NDON) Exit
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do
!
!  One restricts further processing because it is no use
!  to store more high values
!
         If (ICRS < NDON-1) Then
            Do
               ICRS = ICRS + 1
               If (INVALS(ICRS) <= XPIV) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = ICRS
               Else If (ICRS >= NDON) Then
                  Exit
               End If
            End Do
         End If
!
!
      Else
!
!  Same as above, but this is not as easy to optimize, so the
!  DO-loop is kept
!
         Do ICRS = 4, NDON - 1
            If (INVALS(ICRS) > XPIV) Then
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do
!
         If (ICRS < NDON-1) Then
            Do
               ICRS = ICRS + 1
               If (INVALS(ICRS) <= XPIV) Then
                  If (ICRS >= NDON) Exit
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = ICRS
               End If
            End Do
         End If
      End If
!
      JLM2 = 0
      JLM1 = 0
      JHM2 = 0
      JHM1 = 0
      Do
         if (JLOW == NORD) Exit
         If (JLM2 == JLOW .And. JHM2 == JHIG) Then
!
!   We are oscillating. Perturbate by bringing JLOW closer by one
!   to NORD
!
           If (NORD > JLOW) Then
                XMIN = INVALS (IHIGT(1))
                IHIG = 1
                Do ICRS = 2, JHIG
                   If (INVALS(IHIGT(ICRS)) < XMIN) Then
                      XMIN = INVALS (IHIGT(ICRS))
                      IHIG = ICRS
                   End If
                End Do
!
                JLOW = JLOW + 1
                ILOWT (JLOW) = IHIGT (IHIG)
                IHIGT (IHIG) = IHIGT (JHIG)
                JHIG = JHIG - 1
             Else
                ILOW = ILOWT (JLOW)
                XMAX = INVALS (ILOW)
                Do ICRS = 1, JLOW
                   If (INVALS(ILOWT(ICRS)) > XMAX) Then
                      IWRK = ILOWT (ICRS)
                      XMAX = INVALS (IWRK)
                      ILOWT (ICRS) = ILOW
                      ILOW = IWRK
                   End If
                End Do
                JLOW = JLOW - 1
             End If
         End If
         JLM2 = JLM1
         JLM1 = JLOW
         JHM2 = JHM1
         JHM1 = JHIG
!
!   We try to bring the number of values in the low values set
!   closer to NORD.
!
        Select Case (NORD-JLOW)
         Case (2:)
!
!   Not enough values in low part, at least 2 are missing
!
            Select Case (JHIG)
!!!!!           CASE DEFAULT
!!!!!              write (*,*) "Assertion failed"
!!!!!              STOP
!
!   We make a special case when we have so few values in
!   the high values set that it is bad performance to choose a pivot
!   and apply the general algorithm.
!
            Case (2)
               If (INVALS(IHIGT(1)) <= INVALS(IHIGT(2))) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (2)
               Else
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (2)
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
               End If
               Exit
!
            Case (3)
!
!
               IWRK1 = IHIGT (1)
               IWRK2 = IHIGT (2)
               IWRK3 = IHIGT (3)
               If (INVALS(IWRK2) < INVALS(IWRK1)) Then
                  IHIGT (1) = IWRK2
                  IHIGT (2) = IWRK1
                  IWRK2 = IWRK1
               End If
               If (INVALS(IWRK2) > INVALS(IWRK3)) Then
                  IHIGT (3) = IWRK2
                  IHIGT (2) = IWRK3
                  IWRK2 = IWRK3
                  If (INVALS(IWRK2) < INVALS(IHIGT(1))) Then
                     IHIGT (2) = IHIGT (1)
                     IHIGT (1) = IWRK2
                  End If
               End If
               JHIG = 0
               Do ICRS = JLOW + 1, NORD
                  JHIG = JHIG + 1
                  ILOWT (ICRS) = IHIGT (JHIG)
               End Do
               JLOW = NORD
               Exit
!
            Case (4:)
!
!
               XPIV0 = XPIV
               IFIN = JHIG
!
!  One chooses a pivot from the 2 first values and the last one.
!  This should ensure sufficient renewal between iterations to
!  avoid worst case behavior effects.
!
               IWRK1 = IHIGT (1)
               IWRK2 = IHIGT (2)
               IWRK3 = IHIGT (IFIN)
               If (INVALS(IWRK2) < INVALS(IWRK1)) Then
                  IHIGT (1) = IWRK2
                  IHIGT (2) = IWRK1
                  IWRK2 = IWRK1
               End If
               If (INVALS(IWRK2) > INVALS(IWRK3)) Then
                  IHIGT (IFIN) = IWRK2
                  IHIGT (2) = IWRK3
                  IWRK2 = IWRK3
                  If (INVALS(IWRK2) < INVALS(IHIGT(1))) Then
                     IHIGT (2) = IHIGT (1)
                     IHIGT (1) = IWRK2
                  End If
               End If
!
               JDEB = JLOW
               NWRK = NORD - JLOW
               IWRK1 = IHIGT (1)
               JLOW = JLOW + 1
               ILOWT (JLOW) = IWRK1
               XPIV = INVALS (IWRK1) + REAL (NWRK) / REAL (NORD+NWRK) * &
                                      (INVALS(IHIGT(IFIN))-INVALS(IWRK1))
!
!  One takes values <= pivot to ILOWT
!  Again, 2 parts, one where we take care of the remaining
!  high values because we might still need them, and the
!  other when we know that we will have more than enough
!  low values in the end.
!
               JHIG = 0
               Do ICRS = 2, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                     If (JLOW >= NORD) Exit
                  Else
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = IHIGT (ICRS)
                  End If
               End Do
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                  End If
               End Do
           End Select
!
!
         Case (1)
!
!  Only 1 value is missing in low part
!
            XMIN = INVALS (IHIGT(1))
            IHIG = 1
            Do ICRS = 2, JHIG
               If (INVALS(IHIGT(ICRS)) < XMIN) Then
                  XMIN = INVALS (IHIGT(ICRS))
                  IHIG = ICRS
               End If
            End Do
!
            JLOW = JLOW + 1
            ILOWT (JLOW) = IHIGT (IHIG)
            Exit
!
!
         Case (0)
!
!  Low part is exactly what we want
!
            Exit
!
!
         Case (-5:-1)
!
!  Only few values too many in low part
!
            IRNGT (1) = ILOWT (1)
            Do ICRS = 2, NORD
               IWRK = ILOWT (ICRS)
               XWRK = INVALS (IWRK)
               Do IDCR = ICRS - 1, 1, - 1
                  If (XWRK < INVALS(IRNGT(IDCR))) Then
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  Else
                     Exit
                  End If
               End Do
               IRNGT (IDCR+1) = IWRK
            End Do
!
            XWRK1 = INVALS (IRNGT(NORD))
            Do ICRS = NORD + 1, JLOW
               If (INVALS(ILOWT (ICRS)) < XWRK1) Then
                  XWRK = INVALS (ILOWT (ICRS))
                  Do IDCR = NORD - 1, 1, - 1
                     If (XWRK >= INVALS(IRNGT(IDCR))) Exit
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  End Do
                  IRNGT (IDCR+1) = ILOWT (ICRS)
                  XWRK1 = INVALS (IRNGT(NORD))
               End If
            End Do
!
            Return
!
!
         Case (:-6)
!
! last case: too many values in low part
!
            IDEB = JDEB + 1
            IMIL = (JLOW+IDEB) / 2
            IFIN = JLOW
!
!  One chooses a pivot from 1st, last, and middle values
!
            If (INVALS(ILOWT(IMIL)) < INVALS(ILOWT(IDEB))) Then
               IWRK = ILOWT (IDEB)
               ILOWT (IDEB) = ILOWT (IMIL)
               ILOWT (IMIL) = IWRK
            End If
            If (INVALS(ILOWT(IMIL)) > INVALS(ILOWT(IFIN))) Then
               IWRK = ILOWT (IFIN)
               ILOWT (IFIN) = ILOWT (IMIL)
               ILOWT (IMIL) = IWRK
               If (INVALS(ILOWT(IMIL)) < INVALS(ILOWT(IDEB))) Then
                  IWRK = ILOWT (IDEB)
                  ILOWT (IDEB) = ILOWT (IMIL)
                  ILOWT (IMIL) = IWRK
               End If
            End If
            If (IFIN <= 3) Exit
!
            XPIV = INVALS (ILOWT(1)) + REAL(NORD)/REAL(JLOW+NORD) * &
                                      (INVALS(ILOWT(IFIN))-INVALS(ILOWT(1)))
            If (JDEB > 0) Then
               If (XPIV <= XPIV0) &
                   XPIV = XPIV0 + REAL(2*NORD-JDEB)/REAL (JLOW+NORD) * &
                                  (INVALS(ILOWT(IFIN))-XPIV0)
            Else
               IDEB = 1
            End If
!
!  One takes values > XPIV to IHIGT
!  However, we do not process the first values if we have been
!  through the case when we did not have enough low values
!
            JHIG = 0
            JLOW = JDEB
!
            If (INVALS(ILOWT(IFIN)) > XPIV) Then
               ICRS = JDEB
               Do
                 ICRS = ICRS + 1
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                     If (ICRS >= IFIN) Exit
                  Else
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do
!
               If (ICRS < IFIN) Then
                  Do
                     ICRS = ICRS + 1
                     If (INVALS(ILOWT(ICRS)) <= XPIV) Then
                        JLOW = JLOW + 1
                        ILOWT (JLOW) = ILOWT (ICRS)
                     Else
                        If (ICRS >= IFIN) Exit
                     End If
                  End Do
               End If
           Else
               Do ICRS = IDEB, IFIN
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                  Else
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(ILOWT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                  End If
               End Do
            End If
!
         End Select
!
      End Do
!
!  Now, we only need to complete ranking of the 1:NORD set
!  Assuming NORD is small, we use a simple insertion sort
!
      IRNGT (1) = ILOWT (1)
      Do ICRS = 2, NORD
         IWRK = ILOWT (ICRS)
         XWRK = INVALS (IWRK)
         Do IDCR = ICRS - 1, 1, - 1
            If (XWRK < INVALS(IRNGT(IDCR))) Then
               IRNGT (IDCR+1) = IRNGT (IDCR)
            Else
               Exit
            End If
         End Do
         IRNGT (IDCR+1) = IWRK
      End Do
     Return
!
!
End Subroutine int32_rnkpar
!$DEFINE CHARACTER
!$SET KIND f_char
!$SET TYPE character
!$POST rnkpar
!$UNDEFINE CHARACTER
end module M_orderpack__rnkpar

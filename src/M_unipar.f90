Module M_unipar
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
Private
integer,parameter :: f_char=selected_char_kind("DEFAULT")
public :: unipar
!>
!!##NAME
!!    prank_unique(3f) - [orderpack:RANK:PARTIAL:UNIQUE] partially ranks
!!                       an array removing duplicates
!!
!!##SYNOPSIS
!!
!!     Subroutine Prank_Unique (INVALS, IRNGT, NORD)
!!
!!      ${TYPE} (kind=${KIND}), Intent (In) :: INVALS(:)
!!      Integer, Intent (Out)               :: IRNGT(:)
!!      Integer, Intent (InOut)             :: NORD
!!
!!    Where ${TYPE}(kind=${KIND}) may be
!!
!!       o Real(kind=real32)
!!       o Real(kind=real64)
!!       o Integer(kind=int32)
!!
!!##DESCRIPTION
!!    Partially rank INVALS() up to order NORD at most, removing duplicate
!!    entries.
!!
!!    Internally, this routine uses a pivoting strategy such as the one of
!!    finding the median based on the quicksort algorithm, but we skew the
!!    pivot choice to try to bring it to NORD as quickly as possible. It
!!    uses two temporary arrays, where it stores the indices of the values
!!    smaller than the pivot (ILOWT), and the indices of values larger
!!    than the pivot that we might still need later on (IHIGT). It iterates
!!    until it can bring the number of values in ILOWT to exactly NORD, and
!!    then uses an insertion sort to rank this set, since it is supposedly
!!    small. At all times, the NORD first values in ILOWT correspond to
!!    distinct values of the input array.
!!
!!
!!##OPTIONS
!!     INVALS      array to partially sort
!!     IRNGT      indices returned that point to lowest values
!!     NORD       number of sorted values to determine before
!!                eliminating duplicates
!!
!!##EXAMPLES
!!
!!   Sample program:
!!
!!    program demo_prank_unique
!!    ! ranks array, removing duplicates
!!    use M_orderpack, only : prank_unique
!!    implicit none
!!    character(len=*),parameter :: g='(*(g0,1x))'
!!    integer,allocatable :: INVALS(:)
!!    integer,allocatable :: irngt(:)
!!    integer :: nord
!!    !
!!    write(*,g)'If enough values are unique, will return NORD indices'
!!    if(allocated(irngt))deallocate(irngt)
!!    INVALS=[10,5,7,1,4,5,6,8,9,10,1]
!!    nord=5
!!    allocate(irngt(nord))
!!    call printme()
!!    !
!!    !BUG!write(*,g)'If not enough values are unique, will change NORD'
!!    !BUG!INVALS=[-1,0,-1,0,-1,0,-1]
!!    !BUG!nord=5
!!    !BUG!if(allocated(irngt))deallocate(irngt)
!!    !BUG!allocate(irngt(nord))
!!    !BUG!call printme()
!!    contains
!!    subroutine printme()
!!       write(*,g)'ORIGINAL:',INVALS
!!       write(*,g)'NUMBER OF INDICES TO SORT:',nord
!!       call prank_unique(INVALS,irngt,nord)
!!       write(*,g)'NUMBER OF INDICES RETURNED:',nord
!!       write(*,g)'RETURNED INDICES:',irngt(:nord)
!!       write(*,g)nord,'SMALLEST UNIQUE VALUES:',INVALS(irngt(:nord))
!!    end subroutine
!!    end program demo_prank_unique
!!
!!   Results:
!!
!!    If enough values are unique, will return NORD indices
!!    ORIGINAL: 10 5 7 1 4 5 6 8 9 10 1
!!    NUMBER OF INDICES TO SORT: 5
!!    NUMBER OF INDICES RETURNED: 5
!!    RETURNED INDICES: 11 5 2 7 3
!!    5 SMALLEST UNIQUE VALUES: 1 4 5 6 7
!!    If not enough values are unique, will change NORD
!!    ORIGINAL: -1 0 -1 0 -1 0 -1
!!    NUMBER OF INDICES TO SORT: 5
!!    NUMBER OF INDICES RETURNED: 2
!!    RETURNED INDICES: 1 2
!!    2 SMALLEST UNIQUE VALUES: -1 0
!!
!!##AUTHOR
!!    Michel Olagnon - Feb. 2000
!!##MAINTAINER
!!    John Urban, 2022.04.16
!!##LICENSE
!!    CC0-1.0
interface unipar
   module procedure real64_unipar, real32_unipar, int32_unipar !, f_char_unipar
end interface unipar
contains
Subroutine real64_unipar (INVALS, IRNGT, NORD)
! __________________________________________________________
   Real (kind=real64), Dimension (:), Intent (In) :: INVALS
   Integer, Dimension (:), Intent (Out) :: IRNGT
   Integer, Intent (InOut) :: NORD
! __________________________________________________________
   Real (kind=real64) :: XPIV, XWRK, XWRK1, XMIN, XMAX, XPIV0
! __________________________________________________________
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
         If (NORD >= 1) Then
            NORD = 1
            IRNGT (1) = 1
         End If
         Return
      End If
!
!  One chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
     Do ICRS = 2, NDON
        If (INVALS(ICRS) == INVALS(1)) Then
          Cycle
        Else If (INVALS(ICRS) < INVALS(1)) Then
           ILOWT (1) = ICRS
           IHIGT (1) = 1
        Else
           ILOWT (1) = 1
           IHIGT (1) = ICRS
        End If
        Exit
     End Do
!
      If (NDON <= ICRS) Then
         NORD = Min (NORD, 2)
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         Return
      End If
!
      ICRS = ICRS + 1
      JHIG = 1
      If (INVALS(ICRS) < INVALS(IHIGT(1))) Then
         If (INVALS(ICRS) < INVALS(ILOWT(1))) Then
            JHIG = JHIG + 1
            IHIGT (JHIG) = IHIGT (1)
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = ICRS
         Else If (INVALS(ICRS) > INVALS(ILOWT(1))) Then
            JHIG = JHIG + 1
            IHIGT (JHIG) = IHIGT (1)
            IHIGT (1) = ICRS
         End If
      ElseIf (INVALS(ICRS) > INVALS(IHIGT(1))) Then
         JHIG = JHIG + 1
         IHIGT (JHIG) = ICRS
      End If
!
      If (NDON <= ICRS) Then
         NORD = Min (NORD, JHIG+1)
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         Return
      End If
!
      If (INVALS(NDON) < INVALS(IHIGT(1))) Then
         If (INVALS(NDON) < INVALS(ILOWT(1))) Then
            Do IDCR = JHIG, 1, -1
              IHIGT (IDCR+1) = IHIGT (IDCR)
            End Do
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = NDON
            JHIG = JHIG + 1
         ElseIf (INVALS(NDON) > INVALS(ILOWT(1))) Then
            Do IDCR = JHIG, 1, -1
              IHIGT (IDCR+1) = IHIGT (IDCR)
            End Do
            IHIGT (1) = NDON
            JHIG = JHIG + 1
         End If
      ElseIf (INVALS(NDON) > INVALS(IHIGT(1))) Then
         JHIG = JHIG + 1
         IHIGT (JHIG) = NDON
      End If
!
      If (NDON <= ICRS+1) Then
         NORD = Min (NORD, JHIG+1)
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
!  than enough values in ILOWT, i.e. one more than
!  strictly necessary so as to be able to come out of the
!  case where JLOWT would be NORD distinct values followed
!  by values that are exclusively duplicates of these.
!
!
      If (INVALS(NDON) > XPIV) Then
         lowloop1: Do
            ICRS = ICRS + 1
            If (INVALS(ICRS) > XPIV) Then
               If (ICRS >= NDON) Exit
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               Do ILOW = 1, JLOW
                 If (INVALS(ICRS) == INVALS(ILOWT(ILOW))) Cycle lowloop1
               End Do
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do lowloop1
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
         lowloop2: Do ICRS = ICRS + 1, NDON - 1
            If (INVALS(ICRS) > XPIV) Then
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               Do ILOW = 1, JLOW
                 If (INVALS(ICRS) == INVALS (ILOWT(ILOW))) Cycle lowloop2
               End Do
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do lowloop2
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
                IHIG = 0
                Do ICRS = 1, JHIG
                   If (INVALS(IHIGT (ICRS)) /= XMIN) then
                      IHIG = IHIG + 1
                      IHIGT (IHIG ) = IHIGT (ICRS)
                   End If
                End Do
                JHIG = IHIG
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
!   closer to NORD. In order to make better pivot choices, we
!   decrease NORD if we already know that we don't have that
!   many distinct values as a whole.
!
         IF (JLOW+JHIG < NORD) NORD = JLOW+JHIG
         Select Case (NORD-JLOW)
! ______________________________
         Case (2:)
!
!   Not enough values in low part, at least 2 are missing
!
            Select Case (JHIG)
!
!   Not enough values in high part either (too many duplicates)
!
            Case (0)
               NORD = JLOW
!
            Case (1)
               JLOW = JLOW + 1
               ILOWT (JLOW) = IHIGT (1)
               NORD = JLOW
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
               ElseIf (INVALS(IHIGT(1)) == INVALS(IHIGT(2))) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
                  NORD = JLOW
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
               JHIG = 1
               JLOW = JLOW + 1
               ILOWT (JLOW) = IHIGT (1)
               JHIG = JHIG + 1
               IF (INVALS(IHIGT(JHIG)) /= INVALS(ILOWT(JLOW))) Then
                 JLOW = JLOW + 1
                 ILOWT (JLOW) = IHIGT (JHIG)
               End If
               JHIG = JHIG + 1
               IF (INVALS(IHIGT(JHIG)) /= INVALS(ILOWT(JLOW))) Then
                 JLOW = JLOW + 1
                 ILOWT (JLOW) = IHIGT (JHIG)
               End If
               NORD = Min (JLOW, NORD)
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
               lowloop3: Do ICRS = 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     Do ILOW = 1, JLOW
                        If (INVALS(IHIGT(ICRS)) == INVALS (ILOWT(ILOW))) &
                            Cycle lowloop3
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                     If (JLOW > NORD) Exit
                  Else
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = IHIGT (ICRS)
                  End If
               End Do lowloop3
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                  End If
               End Do
           End Select
!
! ______________________________
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
! ______________________________
!
         Case (0)
!
!  Low part is exactly what we want
!
            Exit
!
! ______________________________
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
            insert1: Do ICRS = NORD + 1, JLOW
               If (INVALS(ILOWT (ICRS)) < XWRK1) Then
                  XWRK = INVALS (ILOWT (ICRS))
                  Do ILOW = 1, NORD - 1
                     If (XWRK <= INVALS(IRNGT(ILOW))) Then
                        If (XWRK == INVALS(IRNGT(ILOW))) Cycle insert1
                        Exit
                     End If
                  End Do
                  Do IDCR = NORD - 1, ILOW, - 1
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  End Do
                  IRNGT (IDCR+1) = ILOWT (ICRS)
                  XWRK1 = INVALS (IRNGT(NORD))
               End If
            End Do insert1
!
            Return
!
! ______________________________
!
         Case (:-6)
!
! last case: too many values in low part
!
            IDEB = JDEB + 1
            IMIL = MIN ((JLOW+IDEB) / 2, NORD)
            IFIN = MIN (JLOW, NORD+1)
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
            XPIV = INVALS (ILOWT(IDEB)) + REAL(NORD)/REAL(JLOW+NORD) * &
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
            IFIN = JLOW
            JLOW = JDEB
!
            If (INVALS(ILOWT(IFIN)) > XPIV) Then
               ICRS = JDEB
              lowloop4: Do
                 ICRS = ICRS + 1
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                     If (ICRS >= IFIN) Exit
                  Else
                     XWRK1 = INVALS(ILOWT(ICRS))
                     Do ILOW = IDEB, JLOW
                        If (XWRK1 == INVALS(ILOWT(ILOW))) &
                            Cycle lowloop4
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do lowloop4
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
              lowloop5: Do ICRS = IDEB, IFIN
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                  Else
                     XWRK1 = INVALS(ILOWT(ICRS))
                     Do ILOW = IDEB, JLOW
                        If (XWRK1 == INVALS(ILOWT(ILOW))) &
                            Cycle lowloop5
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do lowloop5
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
! ______________________________
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
End Subroutine real64_unipar
Subroutine real32_unipar (INVALS, IRNGT, NORD)
! __________________________________________________________
   Real (kind=real32), Dimension (:), Intent (In) :: INVALS
   Integer, Dimension (:), Intent (Out) :: IRNGT
   Integer, Intent (InOut) :: NORD
! __________________________________________________________
   Real (kind=real32) :: XPIV, XWRK, XWRK1, XMIN, XMAX, XPIV0
! __________________________________________________________
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
         If (NORD >= 1) Then
            NORD = 1
            IRNGT (1) = 1
         End If
         Return
      End If
!
!  One chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
     Do ICRS = 2, NDON
        If (INVALS(ICRS) == INVALS(1)) Then
          Cycle
        Else If (INVALS(ICRS) < INVALS(1)) Then
           ILOWT (1) = ICRS
           IHIGT (1) = 1
        Else
           ILOWT (1) = 1
           IHIGT (1) = ICRS
        End If
        Exit
     End Do
!
      If (NDON <= ICRS) Then
         NORD = Min (NORD, 2)
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         Return
      End If
!
      ICRS = ICRS + 1
      JHIG = 1
      If (INVALS(ICRS) < INVALS(IHIGT(1))) Then
         If (INVALS(ICRS) < INVALS(ILOWT(1))) Then
            JHIG = JHIG + 1
            IHIGT (JHIG) = IHIGT (1)
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = ICRS
         Else If (INVALS(ICRS) > INVALS(ILOWT(1))) Then
            JHIG = JHIG + 1
            IHIGT (JHIG) = IHIGT (1)
            IHIGT (1) = ICRS
         End If
      ElseIf (INVALS(ICRS) > INVALS(IHIGT(1))) Then
         JHIG = JHIG + 1
         IHIGT (JHIG) = ICRS
      End If
!
      If (NDON <= ICRS) Then
         NORD = Min (NORD, JHIG+1)
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         Return
      End If
!
      If (INVALS(NDON) < INVALS(IHIGT(1))) Then
         If (INVALS(NDON) < INVALS(ILOWT(1))) Then
            Do IDCR = JHIG, 1, -1
              IHIGT (IDCR+1) = IHIGT (IDCR)
            End Do
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = NDON
            JHIG = JHIG + 1
         ElseIf (INVALS(NDON) > INVALS(ILOWT(1))) Then
            Do IDCR = JHIG, 1, -1
              IHIGT (IDCR+1) = IHIGT (IDCR)
            End Do
            IHIGT (1) = NDON
            JHIG = JHIG + 1
         End If
      ElseIf (INVALS(NDON) > INVALS(IHIGT(1))) Then
         JHIG = JHIG + 1
         IHIGT (JHIG) = NDON
      End If
!
      If (NDON <= ICRS+1) Then
         NORD = Min (NORD, JHIG+1)
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
!  than enough values in ILOWT, i.e. one more than
!  strictly necessary so as to be able to come out of the
!  case where JLOWT would be NORD distinct values followed
!  by values that are exclusively duplicates of these.
!
!
      If (INVALS(NDON) > XPIV) Then
         lowloop1: Do
            ICRS = ICRS + 1
            If (INVALS(ICRS) > XPIV) Then
               If (ICRS >= NDON) Exit
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               Do ILOW = 1, JLOW
                 If (INVALS(ICRS) == INVALS(ILOWT(ILOW))) Cycle lowloop1
               End Do
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do lowloop1
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
         lowloop2: Do ICRS = ICRS + 1, NDON - 1
            If (INVALS(ICRS) > XPIV) Then
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               Do ILOW = 1, JLOW
                 If (INVALS(ICRS) == INVALS (ILOWT(ILOW))) Cycle lowloop2
               End Do
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do lowloop2
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
                IHIG = 0
                Do ICRS = 1, JHIG
                   If (INVALS(IHIGT (ICRS)) /= XMIN) then
                      IHIG = IHIG + 1
                      IHIGT (IHIG ) = IHIGT (ICRS)
                   End If
                End Do
                JHIG = IHIG
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
!   closer to NORD. In order to make better pivot choices, we
!   decrease NORD if we already know that we don't have that
!   many distinct values as a whole.
!
         IF (JLOW+JHIG < NORD) NORD = JLOW+JHIG
         Select Case (NORD-JLOW)
! ______________________________
         Case (2:)
!
!   Not enough values in low part, at least 2 are missing
!
            Select Case (JHIG)
!
!   Not enough values in high part either (too many duplicates)
!
            Case (0)
               NORD = JLOW
!
            Case (1)
               JLOW = JLOW + 1
               ILOWT (JLOW) = IHIGT (1)
               NORD = JLOW
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
               ElseIf (INVALS(IHIGT(1)) == INVALS(IHIGT(2))) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
                  NORD = JLOW
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
               JHIG = 1
               JLOW = JLOW + 1
               ILOWT (JLOW) = IHIGT (1)
               JHIG = JHIG + 1
               IF (INVALS(IHIGT(JHIG)) /= INVALS(ILOWT(JLOW))) Then
                 JLOW = JLOW + 1
                 ILOWT (JLOW) = IHIGT (JHIG)
               End If
               JHIG = JHIG + 1
               IF (INVALS(IHIGT(JHIG)) /= INVALS(ILOWT(JLOW))) Then
                 JLOW = JLOW + 1
                 ILOWT (JLOW) = IHIGT (JHIG)
               End If
               NORD = Min (JLOW, NORD)
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
               lowloop3: Do ICRS = 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     Do ILOW = 1, JLOW
                        If (INVALS(IHIGT(ICRS)) == INVALS (ILOWT(ILOW))) &
                            Cycle lowloop3
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                     If (JLOW > NORD) Exit
                  Else
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = IHIGT (ICRS)
                  End If
               End Do lowloop3
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                  End If
               End Do
           End Select
!
! ______________________________
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
! ______________________________
!
         Case (0)
!
!  Low part is exactly what we want
!
            Exit
!
! ______________________________
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
            insert1: Do ICRS = NORD + 1, JLOW
               If (INVALS(ILOWT (ICRS)) < XWRK1) Then
                  XWRK = INVALS (ILOWT (ICRS))
                  Do ILOW = 1, NORD - 1
                     If (XWRK <= INVALS(IRNGT(ILOW))) Then
                        If (XWRK == INVALS(IRNGT(ILOW))) Cycle insert1
                        Exit
                     End If
                  End Do
                  Do IDCR = NORD - 1, ILOW, - 1
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  End Do
                  IRNGT (IDCR+1) = ILOWT (ICRS)
                  XWRK1 = INVALS (IRNGT(NORD))
               End If
            End Do insert1
!
            Return
!
! ______________________________
!
         Case (:-6)
!
! last case: too many values in low part
!
            IDEB = JDEB + 1
            IMIL = MIN ((JLOW+IDEB) / 2, NORD)
            IFIN = MIN (JLOW, NORD+1)
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
            XPIV = INVALS (ILOWT(IDEB)) + REAL(NORD)/REAL(JLOW+NORD) * &
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
            IFIN = JLOW
            JLOW = JDEB
!
            If (INVALS(ILOWT(IFIN)) > XPIV) Then
               ICRS = JDEB
              lowloop4: Do
                 ICRS = ICRS + 1
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                     If (ICRS >= IFIN) Exit
                  Else
                     XWRK1 = INVALS(ILOWT(ICRS))
                     Do ILOW = IDEB, JLOW
                        If (XWRK1 == INVALS(ILOWT(ILOW))) &
                            Cycle lowloop4
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do lowloop4
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
              lowloop5: Do ICRS = IDEB, IFIN
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                  Else
                     XWRK1 = INVALS(ILOWT(ICRS))
                     Do ILOW = IDEB, JLOW
                        If (XWRK1 == INVALS(ILOWT(ILOW))) &
                            Cycle lowloop5
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do lowloop5
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
! ______________________________
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
End Subroutine real32_unipar
Subroutine int32_unipar (INVALS, IRNGT, NORD)
! __________________________________________________________
   Integer (kind=int32), Dimension (:), Intent (In) :: INVALS
   Integer, Dimension (:), Intent (Out) :: IRNGT
   Integer, Intent (InOut) :: NORD
! __________________________________________________________
   Integer (kind=int32) :: XPIV, XWRK, XWRK1, XMIN, XMAX, XPIV0
! __________________________________________________________
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
         If (NORD >= 1) Then
            NORD = 1
            IRNGT (1) = 1
         End If
         Return
      End If
!
!  One chooses a pivot, best estimate possible to put fractile near
!  mid-point of the set of low values.
!
     Do ICRS = 2, NDON
        If (INVALS(ICRS) == INVALS(1)) Then
          Cycle
        Else If (INVALS(ICRS) < INVALS(1)) Then
           ILOWT (1) = ICRS
           IHIGT (1) = 1
        Else
           ILOWT (1) = 1
           IHIGT (1) = ICRS
        End If
        Exit
     End Do
!
      If (NDON <= ICRS) Then
         NORD = Min (NORD, 2)
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         Return
      End If
!
      ICRS = ICRS + 1
      JHIG = 1
      If (INVALS(ICRS) < INVALS(IHIGT(1))) Then
         If (INVALS(ICRS) < INVALS(ILOWT(1))) Then
            JHIG = JHIG + 1
            IHIGT (JHIG) = IHIGT (1)
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = ICRS
         Else If (INVALS(ICRS) > INVALS(ILOWT(1))) Then
            JHIG = JHIG + 1
            IHIGT (JHIG) = IHIGT (1)
            IHIGT (1) = ICRS
         End If
      ElseIf (INVALS(ICRS) > INVALS(IHIGT(1))) Then
         JHIG = JHIG + 1
         IHIGT (JHIG) = ICRS
      End If
!
      If (NDON <= ICRS) Then
         NORD = Min (NORD, JHIG+1)
         If (NORD >= 1) IRNGT (1) = ILOWT (1)
         If (NORD >= 2) IRNGT (2) = IHIGT (1)
         If (NORD >= 3) IRNGT (3) = IHIGT (2)
         Return
      End If
!
      If (INVALS(NDON) < INVALS(IHIGT(1))) Then
         If (INVALS(NDON) < INVALS(ILOWT(1))) Then
            Do IDCR = JHIG, 1, -1
              IHIGT (IDCR+1) = IHIGT (IDCR)
            End Do
            IHIGT (1) = ILOWT (1)
            ILOWT (1) = NDON
            JHIG = JHIG + 1
         ElseIf (INVALS(NDON) > INVALS(ILOWT(1))) Then
            Do IDCR = JHIG, 1, -1
              IHIGT (IDCR+1) = IHIGT (IDCR)
            End Do
            IHIGT (1) = NDON
            JHIG = JHIG + 1
         End If
      ElseIf (INVALS(NDON) > INVALS(IHIGT(1))) Then
         JHIG = JHIG + 1
         IHIGT (JHIG) = NDON
      End If
!
      If (NDON <= ICRS+1) Then
         NORD = Min (NORD, JHIG+1)
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
!  than enough values in ILOWT, i.e. one more than
!  strictly necessary so as to be able to come out of the
!  case where JLOWT would be NORD distinct values followed
!  by values that are exclusively duplicates of these.
!
!
      If (INVALS(NDON) > XPIV) Then
         lowloop1: Do
            ICRS = ICRS + 1
            If (INVALS(ICRS) > XPIV) Then
               If (ICRS >= NDON) Exit
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               Do ILOW = 1, JLOW
                 If (INVALS(ICRS) == INVALS(ILOWT(ILOW))) Cycle lowloop1
               End Do
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do lowloop1
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
         lowloop2: Do ICRS = ICRS + 1, NDON - 1
            If (INVALS(ICRS) > XPIV) Then
               JHIG = JHIG + 1
               IHIGT (JHIG) = ICRS
            Else
               Do ILOW = 1, JLOW
                 If (INVALS(ICRS) == INVALS (ILOWT(ILOW))) Cycle lowloop2
               End Do
               JLOW = JLOW + 1
               ILOWT (JLOW) = ICRS
               If (JLOW >= NORD) Exit
            End If
         End Do lowloop2
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
                IHIG = 0
                Do ICRS = 1, JHIG
                   If (INVALS(IHIGT (ICRS)) /= XMIN) then
                      IHIG = IHIG + 1
                      IHIGT (IHIG ) = IHIGT (ICRS)
                   End If
                End Do
                JHIG = IHIG
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
!   closer to NORD. In order to make better pivot choices, we
!   decrease NORD if we already know that we don't have that
!   many distinct values as a whole.
!
         IF (JLOW+JHIG < NORD) NORD = JLOW+JHIG
         Select Case (NORD-JLOW)
! ______________________________
         Case (2:)
!
!   Not enough values in low part, at least 2 are missing
!
            Select Case (JHIG)
!
!   Not enough values in high part either (too many duplicates)
!
            Case (0)
               NORD = JLOW
!
            Case (1)
               JLOW = JLOW + 1
               ILOWT (JLOW) = IHIGT (1)
               NORD = JLOW
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
               ElseIf (INVALS(IHIGT(1)) == INVALS(IHIGT(2))) Then
                  JLOW = JLOW + 1
                  ILOWT (JLOW) = IHIGT (1)
                  NORD = JLOW
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
               JHIG = 1
               JLOW = JLOW + 1
               ILOWT (JLOW) = IHIGT (1)
               JHIG = JHIG + 1
               IF (INVALS(IHIGT(JHIG)) /= INVALS(ILOWT(JLOW))) Then
                 JLOW = JLOW + 1
                 ILOWT (JLOW) = IHIGT (JHIG)
               End If
               JHIG = JHIG + 1
               IF (INVALS(IHIGT(JHIG)) /= INVALS(ILOWT(JLOW))) Then
                 JLOW = JLOW + 1
                 ILOWT (JLOW) = IHIGT (JHIG)
               End If
               NORD = Min (JLOW, NORD)
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
               lowloop3: Do ICRS = 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     Do ILOW = 1, JLOW
                        If (INVALS(IHIGT(ICRS)) == INVALS (ILOWT(ILOW))) &
                            Cycle lowloop3
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                     If (JLOW > NORD) Exit
                  Else
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = IHIGT (ICRS)
                  End If
               End Do lowloop3
!
               Do ICRS = ICRS + 1, IFIN
                  If (INVALS(IHIGT(ICRS)) <= XPIV) Then
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = IHIGT (ICRS)
                  End If
               End Do
           End Select
!
! ______________________________
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
! ______________________________
!
         Case (0)
!
!  Low part is exactly what we want
!
            Exit
!
! ______________________________
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
            insert1: Do ICRS = NORD + 1, JLOW
               If (INVALS(ILOWT (ICRS)) < XWRK1) Then
                  XWRK = INVALS (ILOWT (ICRS))
                  Do ILOW = 1, NORD - 1
                     If (XWRK <= INVALS(IRNGT(ILOW))) Then
                        If (XWRK == INVALS(IRNGT(ILOW))) Cycle insert1
                        Exit
                     End If
                  End Do
                  Do IDCR = NORD - 1, ILOW, - 1
                     IRNGT (IDCR+1) = IRNGT (IDCR)
                  End Do
                  IRNGT (IDCR+1) = ILOWT (ICRS)
                  XWRK1 = INVALS (IRNGT(NORD))
               End If
            End Do insert1
!
            Return
!
! ______________________________
!
         Case (:-6)
!
! last case: too many values in low part
!
            IDEB = JDEB + 1
            IMIL = MIN ((JLOW+IDEB) / 2, NORD)
            IFIN = MIN (JLOW, NORD+1)
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
            XPIV = INVALS (ILOWT(IDEB)) + REAL(NORD)/REAL(JLOW+NORD) * &
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
            IFIN = JLOW
            JLOW = JDEB
!
            If (INVALS(ILOWT(IFIN)) > XPIV) Then
               ICRS = JDEB
              lowloop4: Do
                 ICRS = ICRS + 1
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                     If (ICRS >= IFIN) Exit
                  Else
                     XWRK1 = INVALS(ILOWT(ICRS))
                     Do ILOW = IDEB, JLOW
                        If (XWRK1 == INVALS(ILOWT(ILOW))) &
                            Cycle lowloop4
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do lowloop4
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
              lowloop5: Do ICRS = IDEB, IFIN
                  If (INVALS(ILOWT(ICRS)) > XPIV) Then
                     JHIG = JHIG + 1
                     IHIGT (JHIG) = ILOWT (ICRS)
                  Else
                     XWRK1 = INVALS(ILOWT(ICRS))
                     Do ILOW = IDEB, JLOW
                        If (XWRK1 == INVALS(ILOWT(ILOW))) &
                            Cycle lowloop5
                     End Do
                     JLOW = JLOW + 1
                     ILOWT (JLOW) = ILOWT (ICRS)
                     If (JLOW >= NORD) Exit
                  End If
               End Do lowloop5
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
! ______________________________
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
End Subroutine int32_unipar

end module M_unipar

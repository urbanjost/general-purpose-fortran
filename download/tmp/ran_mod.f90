! @(#) Test of random number consistency for ran_mod : 20111230 - John S. Urban
! compare output using numdiff utility to template file.
!-------------------------------------------------------------------------------
!From: "Ohkawa, Katsuhiro" <ohkawak@westinghouse.com>
!To: "John S. Urban" <urbanjost@comcast.net>;
!    "Gagnon, Andre F." <gagnonaf@westinghouse.com>
!Subject: RE: Random Numbers
!Date: Wednesday, December 28, 2011 6:15 PM
!John,
! As I promised, here is the random number generator I selected for our
! BELOCA programs.
!
!Kats
!-------------------------------------------------------------------------------
!Reference: Press, W. H., Teukolsky. S. A.. Vetterling. W. T., Flannery,
!B. P .. "Numerical Recipes in FORTRAN: Art of Scientific Computing",
!Second Edition, pp. 271, Cambridge University Press.
!-------------------------------------------------------------------------------
DOUBLEPRECISION FUNCTION RAN_MOD(IDUM)
!
!    Copyright (C) 1986, 1992 Numerical Recipes Software
!
!    This routine, ran_mod, is a modified routine ran1 from the book
!    "Numerical Recipes in FORTRAN" (Cambridge University Press),
!    Copyright (C) 1986, 1992 by Numerical Recipes Software. Used by
!    permission. Use of this routine other than as an integral part of
!    BELOCA requires an additional license from Numerical Recipes
!    Software. Further distribution in any form is prohibited.
!
      IMPLICIT NONE
      character(len=*),parameter :: ident="@(#)randmod(3f): random number generator"
!
      INTEGER,intent(inout) :: IDUM
      integer IA,IM,IQ,IR,NTAB,NDIV
      DOUBLE PRECISION AM,EPS,RNMX
      PARAMETER (IA=16807, IM=2147483647)
      PARAMETER (AM=1.D0/IM,IQ=127773,IR=2836)
      PARAMETER (NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2D-7,RNMX=1.D0-EPS)
!
! "MINIMUM" random number generator of Park and Miller with Bays-Durham
! shuffle and added safeguards. Returns a uniform random deviates between
! 0.0 and 1.0 (exclusive of the endpoint values). Call with idum, a
! negative integer to initialize; thereafter, do not alter idum between
! successive deviates in a sequence. RNMX should approximate the largest
! floating value that is less than 1.0.
!
      integer,dimension(ntab),save :: iv=0
      integer,save :: iy=0
      INTEGER J,K
!
      IF(IDUM.LE.0.OR.IY.EQ.0) THEN ! Initialize.
         IDUM=MAX(-IDUM,1)          ! Load the shuffle table (after 8 wa
         DO J=NTAB+8,1,-1
            K=IDUM/IQ
            IDUM=IA*(IDUM-K*IQ)-IR*K
            IF(IDUM.LT.0) IDUM=IDUM+IM
            IF(J.LE.NTAB) IV(J)=IDUM
         ENDDO
         IY=IV(1)
      ENDIF
!
      K=IDUM/IQ                     ! Start here when not initializing.
      IDUM=IA*(IDUM-K*IQ)-IR*K      ! Compute idum=mod(ia*idum,im) without
      IF(IDUM.LT.0) IDUM=IDUM+IM    !    overflows by Schrage's method.
      J=1+IY/NDIV                   ! Will be in the range 1:NTAB.
      IY=IV(J)                      ! Output previously stored value and
      IV(J)=IDUM                    !    the shuffle table.
      RAN_MOD=MIN(AM*IY,RNMX)       ! Because users don't expect endpoint

END FUNCTION RAN_MOD
!-------------------------------------------------------------------------------
!-!I had an original one. -- KO
!-------------------------------------------------------------------------------
!-!
!-!C                                                                       RAN1_1
!-!C @(#)ran1.f    1.2 97/07/02                                            RAN1_2
!-!C                                                                       RAN1_3
!-!      FUNCTION RAN1(IDUM)                                               RAN1_4
!-!      INTEGER IDUM,IA,IM,IQ,IR,NTAB,NDIV                                RAN1_5
!-!      REAL RAN1,AM,EPS,RNMX                                             RAN1_6
!-!      PARAMETER (IA=16807, IM=2147483647,AM=1./IM,IQ=127773,IR=2836,    RAN1_7
!-!     1           NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2E-7,RNMX=1.-EPS)     RAN1_8
!-!C                                                                       RAN1_9
!-!C "MINIMUM" random number generator of Park and Miller with Bays-Durham RAN1_10
!-!C shuffle and added safeguards. Returns a uniform random deviates betweeRAN1_11
!-!C 0.0 and 1.0 (exclusive of the endpoint values). Call with idum, a     RAN1_12
!-!C negative integer to initialize; thereafter, do not alter idum between RAN1_13
!-!C successive deviates in a sequence. RNMX should approximate the largestRAN1_14
!-!C floating value that is less than 1.0.                                 RAN1_15
!-!C                                                                       RAN1_16
!-!C                                                                       RAN1_17
!-!      INTEGER J,K,IV(NTAB),IY                                           RAN1_18
!-!      SAVE IV,IY                                                        RAN1_19
!-!      DATA IV /NTAB*0/, IY /0/                                          RAN1_20
!-!C                                                                       RAN1_21
!-!      IF(IDUM.LE.0.OR.IY.EQ.0) THEN ! Initialize.                       RAN1_22
!-!        IDUM=MAX(-IDUM,1)           ! Load the shuffle table (after 8 waRAN1_23
!-!        DO J=NTAB+8,1,-1                                                RAN1_24
!-!          K=IDUM/IQ                                                     RAN1_25
!-!          IDUM=IA*(IDUM-K*IQ)-IR*K                                      RAN1_26
!-!          IF(IDUM.LT.0) IDUM=IDUM+IM                                    RAN1_27
!-!          IF(J.LE.NTAB) IV(J)=IDUM                                      RAN1_28
!-!        ENDDO                                                           RAN1_29
!-!        IY=IV(1)                                                        RAN1_30
!-!      ENDIF                                                             RAN1_31
!-!C                                                                       RAN1_32
!-!      K=IDUM/IQ                     ! Start here when not initializing. RAN1_33
!-!      IDUM=IA*(IDUM-K*IQ)-IR*K      ! Compute idum=mod(ia*idum,im) withoRAN1_34
!-!      IF(IDUM.LT.0) IDUM=IDUM+IM    !    overflows by Schrage's method. RAN1_35
!-!      J=1+IY/NDIV                   ! Will be in the range 1:NTAB.      RAN1_36
!-!      IY=IV(J)                      ! Output previously stored value andRAN1_37
!-!      IV(J)=IDUM                    !    the shuffle table.             RAN1_38
!-!      RAN1=MIN(AM*IY,RNMX)          ! Because users don't expect endpoinRAN1_39
!-!      RETURN                                                            RAN1_40
!-!      END                                                               RAN1_41

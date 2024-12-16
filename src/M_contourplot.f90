MODULE M_contourplot__Smooth

   implicit none
   private

   integer,parameter,private:: sp=kind(1.0), dp=kind(1.0d0)
   public  :: SmoothSurface
   public  :: Polyx2
   private :: Hypot
   private :: SolveSVD,SVD,SVDbackSubstitution

CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE SmoothSurface(x, y, z, znew, iexp, jexp, ncoef, cnorm, ipowr, jpowr)
! ---------------------------------------------------------------------------
! PURPOSE - Performs the optional smoothing of data before triangulation
!  of the plane is initiated.  The surface defined by z = f(x,y) is
!  smoothed via a polynomial curve fit defined by a least squares criteria.

IMPLICIT NONE

REAL,INTENT(IN),DIMENSION(:):: x,y,z   ! arrays of values defining the known surface (points in space for the function z=f(x,y))
INTEGER,INTENT(IN):: iexp,jexp         ! exponents for the smoothing polynomial as selected by the user.
REAL,INTENT(OUT),DIMENSION(:):: znew   ! array of smoothed values for the function (znew will contain the original z data on
                                       ! return if the smoothing operation fails, in which case ncoef will be set to -1).
INTEGER,INTENT(OUT):: ncoef            ! the number of terms in the polynomial resulting from the values of i and j.
REAL,INTENT(OUT),DIMENSION(:) :: cnorm    ! array of ncoef computed coefficients
INTEGER,INTENT(OUT)           :: ipowr(:) ! array of i exponents for each term
INTEGER,INTENT(OUT)           :: jpowr(:) ! array of j exponents for each term (each element of c, ipowr and jpowr is associated
                                          ! with the ncoef terms of the polynomial, in order)
REAL,ALLOCATABLE,DIMENSION(:,:):: am
REAL,ALLOCATABLE,DIMENSION(:):: b
REAL,ALLOCATABLE,DIMENSION(:):: c,ave
!  REAL,DIMENSION(23):: h
INTEGER:: i,j
INTEGER:: i1,j1
!  INTEGER,PARAMETER:: IA=500
INTEGER:: ier
INTEGER:: iex,jex
INTEGER:: ii
!  INTEGER,DIMENSION(23):: ip
INTEGER:: k,l
!  INTEGER:: m
INTEGER:: jj
!  INTEGER:: kbasis
INTEGER:: kcol,krow
INTEGER:: ki1
INTEGER:: l1
INTEGER:: maxCoef
INTEGER:: n
REAL:: realn
!  REAL:: tol
REAL,ALLOCATABLE,DIMENSION(:):: xx
REAL:: x2,y2
REAL:: xp,yp
!----------------------------------------------------------------------------
! (A) initialize local variables and range check
!
      n=MIN(SIZE(x),SIZE(y),SIZE(z),SIZE(znew))
      realn=REAL(n)
      maxCoef=MIN(SIZE(ipowr),SIZE(jpowr))
      i=MAX(iexp,1)
      j=MAX(jexp,1)
      i1=i+1
      j1=j+1
      ncoef=0


! (B) Determine the x and y exponents to be used and save them in
! arrays ipowr and jpowr

      ncoef=0
      k=MAX(i1,j1)
!      IF (k.eq.0) GOTO 110
      DO ii=1,i1
         ki1=k-ii+1
         l=MIN(ki1,j1)
         DO jj=1,l
            ncoef=ncoef+1
            IF (ncoef > maxCoef) THEN
               znew(1:n)=z(1:n)           ! error, no room to store coeff.
               ncoef=-1
               RETURN
            ENDIF
            ipowr(ncoef)=ii-1
            jpowr(ncoef)=jj-1
         ENDDO
      ENDDO
      WRITE(2,'(3I5)') (k,ipowr(k),jpowr(k),k=1,ncoef)

! (C) Using the exponent lists from above and the known xy data points,
! construct the matrix am

      ALLOCATE(am(n,ncoef), ave(ncoef), b(n), c(ncoef), xx(ncoef) )
      DO kcol=1,ncoef
         iex=ipowr(kcol)
         jex=jpowr(kcol)
         DO krow=1,n
            x2=x(krow)
            IF (x2.eq.0.0) x2=1.0
            xp=x2**iex
            y2=y(krow)
            IF (y2.eq.0.0) y2=1.0
            yp=y2**jex
            am(krow,kcol)=xp*yp
         ENDDO
      ENDDO
      krow=ncoef


! (D) Normalize each value in each column of am by the column average

      ave(1)=1.0
      DO l1=2,ncoef
         ave(l1)=SUM(ABS(am(1:n,l1)) )  ! column l1
!        ave(l1)=0.0
!        DO l2=1,n
!          ave(l1)=ave(l1)+abs(am(l2,l1))
!        ENDDO
         ave(l1)=ave(l1)/realn
         IF (ave(l1).eq.0.) ave(l1)=1.0
         am(1:n,l1)=am(1:n,l1)/ave(l1)
!        DO l2=1,n
!          am(l2,l1)=am(l2,l1)/ave(l1)
!        ENDDO
      ENDDO


! (E,F,G) Use imsl routine llsqf to solve (via least-squares)
!        the system  am*c = z  for matrix c

!      m=n
      ier=0
!      kbasis=ncoef
!      tol=0.0
      b(1:n)=z(1:n)

      CALL SolveSVD(am,b,xx,ier)
!      CALL LLSQF (AM,IA,M,NCOEF,B,TOL,KBASIS,XX,H,IP,IER)
!      xx=0.0    !!!!!!!!!!!!!!!!!!!!!!!!!!!kludge
      DEALLOCATE(am,b)

      IF (ier/=0) THEN
         znew(1:n)=z(1:n)  ! error return
         ncoef=-1
         RETURN
      ENDIF



! (H) Divide out the scale factor from the solution matrix and
! establish the coefficients
      c(1:ncoef)=xx(1:ncoef)
      cnorm(1:ncoef)=c(1:ncoef)/ave(1:ncoef)
      write(2,'(3I5,2F15.6)') (k,ipowr(k),jpowr(k),c(k),cnorm(k),k=1,ncoef)
      DEALLOCATE(ave,c,xx)

!      DO 90 l3=1,ncoef
!        c(l3)=xx(l3)
!        cnorm(l3)=c(l3)/ave(l3)
!   90 CONTINUE


! (I) Establish the new z values by evaluating the polynomial for each
! known x-y pair

      DO k=1,n
         znew(k)=-1.0*Polyx2(0.0, x(k),y(k), cnorm,ipowr,jpowr,ncoef)
      ENDDO
      WRITE(2,'(I4,4F12.6)') (k,x(k),y(k),z(k),znew(k),k=1,n)

END Subroutine SmoothSurface
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*==polyx2.f90 processed by SPAG 8.01RF 15:51 13 Dec 2024
FUNCTION polyx2(Z,X,Y,C,Ipowr,Jpowr,Ncoef) RESULT(f)
! ---------------------------------------------------------------------------
! PURPOSE - Polyx2 is the polynomial evaluation function used when the
!  smoothing option has been invoked.  x and y are the known values of the
!  independent variables.  c is the list of coefficients for each term.
!  ipowr and jpowr are the exponents for each term and ncoef is the number
!  of terms in the polynomial. z is an offset term when evaluating for a
!  constant x value.
!
   IMPLICIT NONE
   REAL, INTENT(IN) :: Z, X, Y
   REAL, INTENT(IN), DIMENSION(:) :: C
   INTEGER, INTENT(IN), DIMENSION(:) :: Ipowr, Jpowr
   INTEGER, INTENT(IN) :: Ncoef

   REAL :: f
   INTEGER :: k
   REAL :: term1, term2

!----------------------------------------------------------------------------
   IF ( (X==0.0) .OR. (Y==0.0) ) THEN


      f = C(1)
      DO k = 2, Ncoef
         term1 = 1.0
         IF ( Ipowr(k)/=0 ) term1 = X**Ipowr(k)
         term2 = 1.0
         IF ( Jpowr(k)/=0 ) term2 = Y**Jpowr(k)
         f = f + term1*term2*C(k)
      ENDDO
      f = Z - f
      RETURN
   ENDIF
   f = C(1)
   DO k = 2, Ncoef
      f = f + (X**Ipowr(k))*(Y**Jpowr(k))*C(k)
   ENDDO
   f = Z - f
   RETURN
END FUNCTION polyx2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE SolveSVD(a,b,x,ier)
! ---------------------------------------------------------------------------
! PURPOSE - Solve the system Ax=b in least-squares sense using SVD
      IMPLICIT NONE
      REAL,INTENT(IN),DIMENSION(:,:):: a
      REAL,INTENT(IN),DIMENSION(:):: b
      REAL,INTENT(OUT),DIMENSION(:):: x
      INTEGER,INTENT(OUT):: ier

      INTEGER:: m,n
      REAL,ALLOCATABLE,DIMENSION(:,:):: u,v
      REAL,ALLOCATABLE,DIMENSION(:):: w

!----------------------------------------------------------------------------
      m=SIZE(a,1)
      n=SIZE(a,2)
      ier=0
      IF (SIZE(b)<n) ier=-10
      IF (SIZE(x)<n) ier=-11
      IF (ier < 0) RETURN
      ALLOCATE(u(m,n), w(n), v(n,n))

      CALL SVD(a,u,w,v,ier)
      IF (ier /= 0) RETURN

      CALL SVDbackSubstitution(u,w,v,b,x)
      DEALLOCATE(v,w,u)

END Subroutine SolveSVD
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*==svd.f90 processed by SPAG 8.01RF 15:51 13 Dec 2024
SUBROUTINE svd(A,U,W,V,Ierr)
!SUBROUTINE Svd(nm, m, n, a, w, matu, u, matv, v, ierr, rv1)
!----------------------------------------------------------------------------
! PURPOSE - Perform the singular value decomposition of a rectangular matrix.
!***LIBRARY   SLATEC
!***TYPE      SINGLE PRECISION (SVD-S)
!***AUTHOR  (UNKNOWN)

   IMPLICIT NONE

!!! nm, m, n, a, w, matu, u, matv, v, ierr, rv1)

   REAL, INTENT(IN), DIMENSION(:,:) :: A ! matrix to be decomposed
   REAL, INTENT(OUT), DIMENSION(:,:) :: U
                                           ! the matrix U
   REAL, INTENT(OUT), DIMENSION(:) :: W
   REAL, INTENT(OUT), DIMENSION(:,:) :: V
   INTEGER, INTENT(OUT) :: Ierr

!***DESCRIPTION
!
!     This subroutine is a translation of the ALGOL procedure SVD,
!     NUM. MATH. 14, 403-420(1970) by Golub and Reinsch.
!     HANDBOOK FOR AUTO. COMP., VOL II-LINEAR ALGEBRA, 134-151(1971).
!
!     This subroutine determines the singular value decomposition
!          T
!     A=USV  of a REAL M by N rectangular matrix.  Householder
!     bidiagonalization and a variant of the QR algorithm are used.
!
!     On Input
!
!        NM must be set to the row dimension of the two-dimensional
!          array parameters, A, U and V, as declared in the calling
!          program dimension statement.  NM is an INTEGER variable.
!          Note that NM must be at least as large as the maximum
!          of M and N.
!
!        M is the number of rows of A and U.
!
!        N is the number of columns of A and U and the order of V.
!
!        A contains the rectangular input matrix to be decomposed.  A is
!          a two-dimensional REAL array, dimensioned A(NM,N).
!
!        MATU should be set to .TRUE. if the U matrix in the
!          decomposition is desired, and to .FALSE. otherwise.
!          MATU is a LOGICAL variable.
!
!        MATV should be set to .TRUE. if the V matrix in the
!          decomposition is desired, and to .FALSE. otherwise.
!          MATV is a LOGICAL variable.
!
!     On Output
!
!        A is unaltered (unless overwritten by U or V).
!
!        W contains the N (non-negative) singular values of A (the
!          diagonal elements of S).  They are unordered.  If an
!          error exit is made, the singular values should be correct
!          for indices IERR+1, IERR+2, ..., N.  W is a one-dimensional
!          REAL array, dimensioned W(N).
!
!        U contains the matrix U (orthogonal column vectors) of the
!          decomposition if MATU has been set to .TRUE.  Otherwise,
!          U is used as a temporary array.  U may coincide with A.
!          If an error exit is made, the columns of U corresponding
!          to indices of correct singular values should be correct.
!          U is a two-dimensional REAL array, dimensioned U(NM,N).
!
!        V contains the matrix V (orthogonal) of the decomposition if
!          MATV has been set to .TRUE.  Otherwise, V is not referenced.
!          V may also coincide with A if U does not.  If an error
!          exit is made, the columns of V corresponding to indices of
!          correct singular values should be correct.  V is a two-
!          dimensional REAL array, dimensioned V(NM,N).
!
!        IERR is an INTEGER flag set to
!          Zero       for normal return,
!          K          if the K-th singular value has not been
!                     determined after 30 iterations.
!
!        RV1 is a one-dimensional REAL array used for temporary storage,
!          dimensioned RV1(N).
!
!     CALLS Hypot(A,B) for sqrt(A**2 + B**2).
!
!     Questions and comments should be directed to B. S. Garbow,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!     ------------------------------------------------------------------
!
!***SEE ALSO  EISDOC
!***ROUTINES CALLED  Hypot
!***REVISION HISTORY  (YYMMDD)
!   811101  DATE WRITTEN
!   890531  Changed all specific intrinsics to generic.  (WRB)
!   890831  Modified array declarations.  (WRB)
!   891214  Prologue converted to Version 4.0 format.  (BAB)
!   900402  Added TYPE section.  (WRB)
!   991125  Revised the subroutine arguments
!***END PROLOGUE  SVD
!
   INTEGER :: i, j, k, l, m, n, ii, i1, kk, k1, ll, l1, mn, its
!      REAL a(nm,*),w(*),u(nm,*),v(nm,*),rv1(*)
   REAL(sp) :: c
   REAL(sp) :: f, g, h
   REAL(sp) :: s, s1
   REAL(sp) :: scale
   REAL(sp) :: x, y, z
   REAL(sp), PARAMETER :: ZERO = 0.0, HALF = 0.5, ONE = 1.0

   REAL(sp), ALLOCATABLE, DIMENSION(:) :: rv1
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!----------------------------------------------------------------------------
         m = size(A,1)
         n = size(A,2)

         Ierr = 0
         IF ( size(U,1)<m ) Ierr = -1
         IF ( size(U,2)<n ) Ierr = -2
         IF ( size(W)<n ) Ierr = -3
         IF ( size(V,1)<n ) Ierr = -4
         IF ( size(V,2)<n ) Ierr = -5
         IF ( Ierr<0 ) RETURN

         ALLOCATE (rv1(n))

         U(1:m,1:n) = A
!      DO 10 i=1,m
!        DO 10 j=1,n
!        u(i,j)=a(i,j)
!   10 CONTINUE

!     .......... HOUSEHOLDER REDUCTION TO BIDIAGONAL FORM ..........
         g = ZERO
         scale = ZERO
         s1 = ZERO

         DO i = 1, n
            l = i + 1
            rv1(i) = scale*g
            g = ZERO
            s = ZERO
            scale = ZERO
            IF ( i<=m ) THEN

               DO k = i, m
                  scale = scale + abs(U(k,i))        ! scale=SUM(ABS(u(i:m,i)))
               ENDDO

               IF ( scale/=ZERO ) THEN

                  DO k = i, m
                     U(k,i) = U(k,i)/scale         ! u(i:m,i)=u(i:m,i)/scale
                     s = s + U(k,i)**2             ! s=SUM(u(i:m,i)**2)
                  ENDDO

                  f = U(i,i)
                  g = -sign(sqrt(s),f)
                  h = f*g - s
                  U(i,i) = f - g
                  IF ( i/=n ) THEN

                     DO j = l, n
                        s = ZERO
                        DO k = i, m
                                ! DOT_PRODUCT(u(i:m,i),u(i:m,j))
                           s = s + U(k,i)*U(k,j)
                        ENDDO

                        f = s/h

                        DO k = i, m
                           U(k,j) = U(k,j) + f*U(k,i)
                                        ! u(i:m,j)=u(i:m,j)+f*u(i:m,i)
                        ENDDO
                     ENDDO
                  ENDIF

                  DO k = i, m
                     U(k,i) = scale*U(k,i)
                                       ! u(i:m,i)=u(i:m,i)*scale
                  ENDDO
               ENDIF
            ENDIF

            W(i) = scale*g
            g = ZERO
            s = ZERO
            scale = ZERO
            IF ( i<=m .AND. i/=n ) THEN

               DO k = l, n
                  scale = scale + abs(U(i,k))        ! scale=SUM(ABS(u(i,1:n)))
               ENDDO

               IF ( scale/=ZERO ) THEN

                  DO k = l, n
                     U(i,k) = U(i,k)/scale
                     s = s + U(i,k)**2
                  ENDDO

                  f = U(i,l)
                  g = -sign(sqrt(s),f)
                  h = f*g - s
                  U(i,l) = f - g

                  DO k = l, n
                     rv1(k) = U(i,k)/h                    ! rv1(L:n)=u(i,L:n)/h
                  ENDDO

                  IF ( i/=m ) THEN

                     DO j = l, m
                        s = ZERO

                        DO k = l, n
                           s = s + U(j,k)*U(i,k)
                        ENDDO

                        DO k = l, n
                           U(j,k) = U(j,k) + s*rv1(k)
                        ENDDO
                     ENDDO
                  ENDIF
!
                  DO k = l, n
                     U(i,k) = scale*U(i,k)
                  ENDDO
               ENDIF
            ENDIF
!
            s1 = max(s1,abs(W(i))+abs(rv1(i)))
         ENDDO


!     .......... ACCUMULATION OF RIGHT-HAND TRANSFORMATIONS ..........
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
         DO ii = 1, n
            i = n + 1 - ii
            IF ( i/=n ) THEN
               IF ( g/=ZERO ) THEN

                  DO j = l, n
                     V(j,i) = (U(i,j)/U(i,l))/g
                                      !  DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
                  ENDDO

                  DO j = l, n
                     s = ZERO
                     DO k = l, n
                        s = s + U(i,k)*V(k,j)
                     ENDDO

                     DO k = l, n
                        V(k,j) = V(k,j) + s*V(k,i)
                     ENDDO
                  ENDDO
               ENDIF

               DO j = l, n
                  V(i,j) = ZERO
                  V(j,i) = ZERO
               ENDDO
            ENDIF

            V(i,i) = ONE
            g = rv1(i)
            l = i
         ENDDO

!     .......... ACCUMULATION OF LEFT-HAND TRANSFORMATIONS ..........

!     ..........FOR I=MIN(M,N) STEP -1 UNTIL 1 DO -- ..........
         mn = min(m,n)

         DO ii = 1, mn
            i = mn + 1 - ii
            l = i + 1
            g = W(i)
            IF ( i/=n ) THEN

               DO j = l, n
                  U(i,j) = ZERO
               ENDDO
            ENDIF

            IF ( g==ZERO ) THEN

               DO j = i, m
                  U(j,i) = ZERO
               ENDDO
            ELSE
               IF ( i/=mn ) THEN

                  DO j = l, n
                     s = ZERO
                     DO k = l, m
                        s = s + U(k,i)*U(k,j)
                     ENDDO

                     f = (s/U(i,i))/g
                             ! DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW

                     DO k = i, m
                        U(k,j) = U(k,j) + f*U(k,i)
                     ENDDO
                  ENDDO
               ENDIF

               DO j = i, m
                  U(j,i) = U(j,i)/g

               ENDDO
            ENDIF

            U(i,i) = U(i,i) + ONE
         ENDDO

!     .......... DIAGONALIZATION OF THE BIDIAGONAL FORM ..........
!!!  360 CONTINUE

!     .......... FOR K=N STEP -1 UNTIL 1 DO -- ..........
         DO kk = 1, n
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  k1 = n - kk
                  k = k1 + 1
                  its = 0
                  spag_nextblock_2 = 2
               CASE (2)
!     .......... TEST FOR SPLITTING.
!                FOR L=K STEP -1 UNTIL 1 DO -- ..........
                  SPAG_Loop_2_1: DO ll = 1, k
                     l1 = k - ll
                     l = l1 + 1
                     IF ( s1+abs(rv1(l))==s1 ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
!     .......... RV1(1) IS ALWAYS ZERO, SO THERE IS NO EXIT
!                THROUGH THE BOTTOM OF THE LOOP ..........
                     IF ( s1+abs(W(l1))==s1 ) EXIT SPAG_Loop_2_1
                  ENDDO SPAG_Loop_2_1
!     .......... CANCELLATION OF RV1(L) IF L GREATER THAN 1 ..........
                  c = ZERO
                  s = ONE

                  SPAG_Loop_2_2: DO i = l, k
                     f = s*rv1(i)
                     rv1(i) = c*rv1(i)
                     IF ( s1+abs(f)==s1 ) EXIT SPAG_Loop_2_2
                     g = W(i)
                     h = hypot(f,g)
                     W(i) = h
                     c = g/h
                     s = -f/h

                     DO j = 1, m
                        y = U(j,l1)
                        z = U(j,i)
                        U(j,l1) = y*c + z*s
                        U(j,i) = -y*s + z*c
                     ENDDO
                  ENDDO SPAG_Loop_2_2
                  spag_nextblock_2 = 3
               CASE (3)

!     .......... TEST FOR CONVERGENCE ..........
                  z = W(k)
                  IF ( l/=k ) THEN
!     .......... SHIFT FROM BOTTOM 2 BY 2 MINOR ..........
                     IF ( its==30 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     its = its + 1
                     x = W(l)
                     y = W(k1)
                     g = rv1(k1)
                     h = rv1(k)
                     f = HALF*(((g+z)/h)*((g-z)/y)+y/h-h/y)
                     g = hypot(f,ONE)
                     f = x - (z/x)*z + (h/x)*(y/(f+sign(g,f))-h)

!     .......... NEXT QR TRANSFORMATION ..........
                     c = ONE
                     s = ONE

                     DO i1 = l, k1
                        i = i1 + 1
                        g = rv1(i)
                        y = W(i)
                        h = s*g
                        g = c*g
                        z = hypot(f,h)
                        rv1(i1) = z
                        c = f/z
                        s = h/z
                        f = x*c + g*s
                        g = -x*s + g*c
                        h = y*s
                        y = y*c

                        DO j = 1, n
                           x = V(j,i1)
                           z = V(j,i)
                           V(j,i1) = x*c + z*s
                           V(j,i) = -x*s + z*c
                        ENDDO
!
                        z = hypot(f,h)
                        W(i1) = z

!     .......... ROTATION CAN BE ARBITRARY IF Z IS ZERO ..........
                        IF ( z/=ZERO ) THEN
                           c = f/z
                           s = h/z
                        ENDIF
                        f = c*g + s*y
                        x = -s*g + c*y

                        DO j = 1, m
                           y = U(j,i1)
                           z = U(j,i)
                           U(j,i1) = y*c + z*s
                           U(j,i) = -y*s + z*c
                        ENDDO

                     ENDDO

                     rv1(l) = ZERO
                     rv1(k) = f
                     W(k) = x
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2

!     .......... CONVERGENCE ..........
                  ELSEIF ( z<ZERO ) THEN
                     W(k) = -z
                      !     .......... w(k) is made non-negative ..........
                     DO j = 1, n
                        V(j,k) = -V(j,k)
                     ENDDO
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2

         ENDDO
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
!     .......... SET ERROR -- NO CONVERGENCE TO A
!                SINGULAR VALUE AFTER 30 ITERATIONS ..........
         Ierr = k
         spag_nextblock_1 = 3
      CASE (3)

         DEALLOCATE (rv1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE svd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
SUBROUTINE SVDbackSubstitution(u,w,v,b,x)
! ---------------------------------------------------------------------------
! PURPOSE -
      IMPLICIT NONE
      REAL,INTENT(IN),DIMENSION(:,:):: u
      REAL,INTENT(IN),DIMENSION(:)::   w
      REAL,INTENT(IN),DIMENSION(:,:):: v
      REAL,INTENT(IN),DIMENSION(:)::   b
      REAL,INTENT(OUT),DIMENSION(:)::  x

      REAL(SP),DIMENSION(SIZE(x)):: tmp
      REAL(SP),PARAMETER:: ZERO=0.0
!----------------------------------------------------------------------------
      WHERE (w/=ZERO)
         tmp=MATMUL(b,u)/w
      ELSEWHERE
         tmp=ZERO
      END WHERE

      x=MATMUL(v,tmp)
END Subroutine SVDbackSubstitution
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   PURE FUNCTION Hypot(x,y) RESULT(z)   ! a PURE function
! ---------------------------------------------------------------------------
! PURPOSE - Solve for SQRT(x*x+y*y) carefully to avoid overflow
!  (will be an intrinsic in Fortran 2008)
      REAL,INTENT(IN):: x,y
      REAL:: z

      REAL:: a,b
!----------------------------------------------------------------------------
      a=ABS(x)
      b=ABS(y)

      IF (a > b) THEN
         z=a*SQRT(1.0+(b/a)**2)
      ELSEIF (b==0) THEN
         z=0.0
      ELSE
         z=b*SQRT(1.0+(a/b)**2)
      ENDIF

   END Function Hypot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END MODULE M_contourplot__Smooth
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
MODULE M_ContourPlot

   USE M_contourplot__Smooth
   IMPLICIT NONE
   PRIVATE

   CHARACTER(LEN=*),PARAMETER:: CONPLOT_VERSION= "1.0 (5Oct99)"
   INTEGER,PARAMETER,PRIVATE:: SP=KIND(1.0), DP=KIND(1.0D0)

   PUBLIC  :: ContourLines
   PRIVATE :: Interpolate,Middle,Triangulate

   interface
      subroutine cntcrv (xx,yy,npoint,zcon)
         implicit none
         real,intent(in),dimension(:):: xx,yy
         integer,intent(in):: npoint
         real,intent(in):: zcon
      end subroutine cntcrv
   end interface

   public  :: test_suite_M_contourplot

CONTAINS
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!   contourlines(3f) - [M_contourplot] calculate contour lines from ungridded data f(x,y)
!!                      and call user-supplied routine with results
!!
!!##SYNOPSIS
!!
!!    SUBROUTINE ContourLines(x,y,z,ismopt,iexp,jexp,clist,epsilon,ierr,cntcrv)
!!
!!       real,intent(in),dimension(:) :: x
!!       real,intent(in),dimension(:) :: y
!!       real,intent(in),dimension(:) :: z
!!       integer,intent(in)           :: ismopt
!!       integer,intent(in)           :: iexp
!!       integer,intent(in)           :: jexp
!!       real,intent(in),dimension(:) :: clist
!!       real,intent(out)             :: epsilon
!!       integer,intent(out)          :: ierr
!!       external cntcrv
!!
!!##DESCRIPTION
!!
!!    contourlines(3f) is a general algorithm for the construction of
!!    contour plots. It computes contour lines of constant z for the
!!    function z = f(x,y).
!!
!!    From the original COSMIC description:
!!
!!    The graphical presentation of experimentally or theoretically generated
!!    data sets frequently involves the construction of contour plots. A
!!    general computer algorithm has been developed for the construction
!!    of contour plots. The algorithm provides for efficient and accurate
!!    contouring with a modular approach which allows flexibility in modifying
!!    the algorithm for special applications. The algorithm accepts as input
!!    data values at a set of points irregularly distributed over a plane. The
!!    algorithm is based on an interpolation scheme in which the points in
!!    the plane are connected by straight line segments to form a set of
!!    triangles. In general, the data is smoothed using a least-squares-error
!!    fit of the data to a bivariate polynomial. To construct the contours,
!!    interpolation along the edges of the triangles is performed, using the
!!    bivariable polynomial if data smoothing was performed. Once the contour
!!    points have been located, the contour may be drawn.
!!
!!##AUTHORS
!!
!!    This is a public domain routine from PDAS (Public Domain Aeronautical
!!    Software) converted to modules; which was derived from COSMIC Program
!!    Number ARC-11441; which originated at NASA Ames Research Center,
!!    circa 1981.
!!
!!##COMMENTS
!!
!!    From the original PDAS description:
!!
!!    There are many additional algorithms available for computation of contour lines.
!!    There are several in the Transactions for Mathematical Software and
!!    elsewhere. This routine was deemed worthy of inclusion in the NASA COSMIC
!!    collection, but I cannot swear to its status among contour generators.
!!
!!##OPTIONS
!!     x            input list of x values
!!     y            input list of y values
!!     z            input list of z values
!!     ismopt       smoothing option flag (0=no/off, 1=yes/on)
!!     iexp,jexp    i and j exponent value for smoothing
!!                  If smoothing is used, these values are used to define
!!                  a polynomial used for a least-squares fit of the data.
!!     clist        list of constant contour values
!!     epsilon      error function (normalized value) returned to caller
!!                  if ismopt is non-zero
!!     ierr         return error flag
!!                    0. for normal return
!!                    1. for invalid value for number of values in x,y,z
!!                    2. for number of ismopt coefficients greater
!!                       than 'maxcof' or number of values in x,y,z
!!##EXAMPLE
!!
!!
!!   TEST1:
!!
!!    ! program demo_contourlines and user routine
!!    program TestCase
!!    use M_ContourPlot, only : contourlines
!!    implicit none
!!    !
!!    integer,parameter    :: NPTS=121
!!    real,parameter,dimension(8):: c = &
!!          [0.1, 0.2, 0.5, 1.0, 2.0, 4.0, 6.0, 8.0]
!!    real                 :: eps
!!    integer              :: errCode
!!    integer,parameter    :: DBG=2
!!    integer              :: ierr
!!    !integer              :: iexp=0, jexp=0, ism=0
!!    integer              :: iexp=2, jexp=3, ism=1
!!    integer              :: i,j,k
!!    real,dimension(NPTS) :: x,y,z
!!    external             :: my_CntCrv
!!    !
!!       k=0
!!       do j=0,10
!!          do i=0,10
!!             k=k+1
!!             x(k)=0.1*real(i)
!!             y(k)=0.1*real(j)
!!          end do
!!       end do
!!       !
!!       z=(x-0.5)**2 + (y-0.5)**2
!!       z(:)=16.0*z(:)
!!       !
!!       ! write out the input values for inspection
!!       open(unit=dbg, file='test.log', status='replace', &
!!          & iostat=errCode, action='write', position='rewind')
!!       write(DBG,'(I4,3F12.6)') (k,x(k),y(k),z(k),k=1,npts)
!!       !
!!       call ContourLines(x,y,z, ism,iexp,jexp, c, eps,ierr,my_CntCrv)
!!    END PROGRAM TestCase
!!    ! ----------------------------------------------------------------
!!    subroutine my_CntCrv(x,y,n,z)
!!    ! User-supplied routine used to plot or process the contour lines
!!    implicit none
!!    integer,intent(in)          :: n
!!    real,intent(in),dimension(n):: x,y
!!    real,intent(in)             :: z
!!    !
!!    integer,save                :: gnu=0
!!    integer                     :: k
!!    integer                     :: errCode
!!       if(gnu.eq.0)then
!!          ! on first call, set up output file
!!          gnu=1
!!          open(unit=gnu, file='test1.gnu', status='replace', &
!!            & iostat=errCode, action='write', position='rewind')
!!          write(*,*) "File test1.gnu added to your directory"
!!       endif
!!       ! write a contour line out to a file followed by a blank line
!!       write(gnu,'("# level ",g0)') z
!!       write(gnu,'(2f12.5)') (x(k),y(k),k=1,n)
!!       write(gnu,'(a)') " "
!!    !
!!    end subroutine my_CntCrv
!!    ! end program demo_contourlines and user routine
!!
!!    #!/bin/sh
!!    # Example execution of gnuplot(1) to plot the output file
!!    gnuplot <<\EOF
!!    plot 'test1.gnu' with lines
!!    pause mouse "click mouse to exit ..."
!!    quit
!!    EOF
!*==contourlines.f90 processed by SPAG 8.01RF 15:51 13 Dec 2024
SUBROUTINE contourlines(X,Y,Z,Ismopt,Iexp,Jexp,Clist,Epsilon,Ierr,Cntcrv)
! ---------------------------------------------------------------------------
! PURPOSE - Driver program for computing and drawing contour lines of
!   constant z for the function z = f(x,y).

   IMPLICIT NONE

! ident_1="@(#) M_contourplot contourlines(3f) calculate contour lines from ungridded data f(x y) and call user-supplied routine with results"

   REAL, INTENT(IN), DIMENSION(:) :: X
                                      ! input list of x values
   REAL, INTENT(IN), DIMENSION(:) :: Y
                                      ! input list of y values
   REAL, INTENT(IN), DIMENSION(:) :: Z
                                      ! input list of z values
!INTEGER,INTENT(IN):: n               ! number of values in x,y and z
   INTEGER, INTENT(IN) :: Ismopt     ! smoothing option flag (0=no/off, 1=yes/on)
   INTEGER, INTENT(IN) :: Iexp       ! i exponent value for smoothing
   INTEGER, INTENT(IN) :: Jexp       ! j exponent value for smoothing
!INTEGER,INTENT(IN):: ncntrs          ! number of contour lines to be drawn (self computing if ncntrs.le.0)
   REAL, INTENT(IN), DIMENSION(:) :: Clist
                                      ! list of constant contour values
   REAL, INTENT(OUT) :: Epsilon      ! error function (normalized value) returned to caller if ismopt is non-zero
   INTEGER, INTENT(OUT) :: Ierr      ! return error flag
!                  = 0 for normal return
!                  = 1 for invalid value for n
!                  = 2 for number of ismopt coefficients greater
!                      than 'maxcof' or n
!
   EXTERNAL Cntcrv

   REAL(sp), DIMENSION(23) :: coef
   INTEGER :: j, k, i1, j1
   INTEGER, DIMENSION(1494) :: ibe, lambda
   INTEGER, DIMENSION(1494,2) :: ie
   INTEGER, DIMENSION(1494,4) :: ite
   INTEGER, DIMENSION(23) :: ipowr, jpowr
   INTEGER :: ledges
   INTEGER, PARAMETER :: MAXCOF = 23, MAXPTS = 500
   INTEGER :: n
   INTEGER :: nc
   INTEGER :: ncoef = 0
!INTEGER:: nmax,nmin
   REAL(sp), DIMENSION(1494) :: xi, eta
   REAL(sp) :: zcon, zmin, zmax
   REAL(sp), DIMENSION(500) :: znew
!-----------------------------------------------------------------------
!!! allocate  ibe,ie,ite,lambda,xi,eta,znew

! (A) initialize local variables and check inputs for errors!

   Ierr = 0
   Epsilon = 0.0
   n = min(size(X),size(Y),size(Z))
   IF ( n<3 .OR. n>MAXPTS ) THEN
      Ierr = 1
      RETURN
   ENDIF


! (B) triangulate x-y data points

   CALL triangulate(X,Y,n,ledges,ie,ibe,ite)

! (C) smooth data if requested

   IF ( Ismopt==0 ) THEN
      znew(1:n) = Z(1:n)
   ELSE
      i1 = Iexp + 1     ! (D)  check requested exponent values for errors
      j1 = Jexp + 1
!        nmin=MIN(i1,j1)
!        nmax=MAX(i1,j1)
      IF ( Jexp<Iexp ) THEN
         nc = (Jexp+1)*(Iexp+1-Jexp/2)
      ELSE
         nc = (Iexp+1)*(Jexp+1-Iexp/2)
      ENDIF

      IF ( nc>n .OR. nc>MAXCOF ) THEN
         znew(1:n) = Z(1:n)
         Ierr = 2
         RETURN
      ENDIF

      ipowr(:) = 0
      jpowr(:) = 0
      CALL smoothsurface(X,Y,Z,znew(1:n),Iexp,Jexp,ncoef,coef,ipowr,jpowr)
      IF ( ncoef>=0 ) THEN
         DO k = 1, n
            Epsilon = Epsilon + (Z(k)-znew(k))**2
         ENDDO
         Epsilon = sqrt(Epsilon)/real(n)
      ENDIF
   ENDIF
!

! (F) determine the range of the z data under consideration

   zmin = minval(Z(1:n))
   zmax = maxval(Z(1:n))

! (J) interpolate for contour line data points
! (K,L) any data points found? . .
!   call subroutine cntour to sort the interpolated points
!   on the contour line and draw it

   DO k = 1, size(Clist)
      zcon = Clist(k)
      IF ( zcon<zmin ) CYCLE
      IF ( zcon>zmax ) CYCLE
      CALL interpolate(X,Y,znew,zcon,ledges,ie,Ismopt,lambda,xi,eta,j,coef,ipowr,jpowr,ncoef)
      IF ( j>0 ) CALL cntour(zcon,xi,eta,lambda,j,ibe,ite,Cntcrv)
   ENDDO

END SUBROUTINE contourlines
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*==triangulate.f90 processed by SPAG 8.01RF 15:51 13 Dec 2024
SUBROUTINE triangulate(Xd,Yd,N,L,E,Be,Te)
! ------------------------------------------------------------------
! PURPOSE - A set of n data points are known (x(i),y(i),i=1,n)  They are to
!   be connected by lines to form a set of triangles (for n.LE. maxpts).
!   the final triangulation establishes a convex polygon defined by linked
!   lists of edge numbers, end points and boundary edges.
!
   IMPLICIT NONE
!      IMPLICIT INTEGER (P,B)   ! oh dear
!
!
!     SUBROUTINE INPUT
   REAL, INTENT(IN), DIMENSION(:) :: Xd  !  = array of abscissas
   REAL, INTENT(IN), DIMENSION(:) :: Yd  !  = array of ordinates
   INTEGER, INTENT(IN) :: N     ! = number of points in x and y
!
!     SUBROUTINE OUTPUT
   INTEGER, INTENT(OUT) :: L     ! = number of edges listed in e, be and te
   INTEGER, INTENT(OUT), DIMENSION(:,:) :: E   ! = list of indices of each triangle edge
   INTEGER, INTENT(OUT), DIMENSION(:) :: Be   ! = 1 if i of e is a boundary edge
   INTEGER, INTENT(OUT), DIMENSION(:,:) :: Te   != indices of neighboring edges for each triangle
!
!     LOCAL VARIABLES

   INTEGER, DIMENSION(500) :: b      ! index of points on the boundary ..  inorder
   INTEGER :: b1, b2
   INTEGER :: bflag
   INTEGER :: bj
   REAL(dp) :: d, d1
!  REAL:: dlxinv,dlyinv
   REAL :: dmin
   REAL :: dst
   INTEGER :: i1, i2
   INTEGER :: j        ! no. of values in list p
   INTEGER :: j1
   INTEGER :: jcnt
   INTEGER :: k        ! no. of points listed in array b
   INTEGER :: k0, k1, k2, k3
   INTEGER :: kcnt
   INTEGER :: kk
   INTEGER :: kknt
   INTEGER :: kl
   INTEGER :: km
   INTEGER :: kn
   INTEGER :: kp1
   INTEGER :: kt
   INTEGER :: l1, l2, l3
   INTEGER :: lambda
   INTEGER :: lcnt
   INTEGER :: lj, ll
   INTEGER :: m        ! no. of rows used in array t
   INTEGER :: mcnt
   INTEGER :: nq
   INTEGER, DIMENSION(500) :: p      ! indices of points outside the boundary
   INTEGER :: p1, p2
   INTEGER :: pj
   INTEGER :: pk3, pk0, pkl
   REAL(dp) :: s
   INTEGER, DIMENSION(995,3) :: t      ! indices of adjacent triangle edges
   REAL(dp) :: tc
   REAL(dp) :: term

   REAL, DIMENSION(500) :: x, y    ! arrays of scaled data
   REAL(dp) :: x12, y12
   REAL :: x1b, y1b
   REAL :: xmax, xmin, ymax, ymin
   REAL :: xqb, yqb
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)

!----------------------------------------------------------------------------

! (A) The procedure begins with no boundary, no edges, and all x-y data
!   points under consideration. Scale the x,y data and initialize local
!   variables.

         j = N
         k = 0
         L = 0
         m = 0
         kknt = 0
         DO jcnt = 1, size(x)
            p(jcnt) = jcnt
         ENDDO
         xmax = maxval(Xd(1:N))
         xmin = minval(Xd(1:N))
         ymax = maxval(Yd(1:N))
         ymin = minval(Yd(1:N))
         x(1:N) = Xd(1:N)/(xmax-xmin)
         y(1:N) = Yd(1:N)/(ymax-ymin)

!      dlxinv=1.0/(xmax-xmin)
!      dlyinv=1.0/(ymax-ymin)
!      DO 30 k=1,n
!        x(k)=xd(k)*dlxinv
!        y(k)=yd(k)*dlyinv
!   30 CONTINUE
!
!
! (B) Begin by taking the last pair of points (x(j),y(j)) in the
!   list to be the first boundary point.

         b(1) = j
         j = j - 1
!
!
! (C) from the remaining points, find the point nearest the first

         i2 = 1
         i1 = b(1)
         dmin = (x(i1)-x(1))**2 + (y(i1)-y(1))**2
         DO j1 = 2, j
            dst = (x(i1)-x(j1))**2 + (y(i1)-y(j1))**2
            IF ( dst>=dmin ) CYCLE
            i2 = j1
            dmin = dst
         ENDDO
!
! (D) now b(1) to b(i2) is the first edge.
!   there is one edge and two boundary points.

!
         j = j - 1
         DO jcnt = i2, j
            p(jcnt) = p(jcnt+1)
         ENDDO
         k = 2
         b(2) = i2
         L = 1
         E(1,1) = min(b(1),b(2))
         E(1,2) = max(b(1),b(2))
!
!
! (E) now begin circling around the boundary of the polygon, considering,
!   in order, each boundary edge.  maintain the following indices -
!     k1 = b array index of the current edge - point 1
!     k2 = b array index of the current edge - point 2
!     b1,b2 = indices of boundary point coordinates

         k1 = 0
         kt = 0
         spag_nextblock_1 = 2
      CASE (2)
         k1 = k1 + 1
         IF ( k1>k ) k1 = 1
         spag_nextblock_1 = 3
      CASE (3)
         k2 = k1 + 1
         IF ( k2>k ) k2 = 1
         b1 = b(k1)
         b2 = b(k2)
         kt = kt + 1
!
! (F) consider the boundary edge from b1 to b2.  for all points not yet
!     triangulated (the j points remaining in p), find the point that,
!     when triangulated with b1,b2, minimizes the length of the two new
!     edges to be drawn.


         d1 = 0.
         j1 = 0
         bflag = 0

         DO lj = 1, j
            pj = p(lj)
            term = (y(pj)-y(b1))*(x(b2)-x(b1)) - (x(pj)-x(b1))*(y(b2)-y(b1))
            IF ( term<=0. ) CYCLE
            d = sqrt((x(pj)-x(b1))**2+(y(pj)-y(b1))**2) + sqrt((x(pj)-x(b2))**2+(y(pj)-y(b2))**2)
            IF ( j1/=0 .AND. d1<d ) CYCLE
            j1 = lj
            d1 = d
         ENDDO
!
!        (G)
!        if less than three edges exist (no triangle defined yet),
!        then there are no adjacent boundary points to be considered.
!        so go to section J.
!
         IF ( k>3 ) THEN
!
!
!        (H)
!        consider the adjacent boundary point of the next edge of the
!        polygon.  call its index number k3 and see if its closer to
!        the current edge than p(j1).
!
            k3 = k2 + 1
                   ! was labelled 100
            IF ( k3>k ) k3 = 1
            pk3 = b(k3)
            term = (y(pk3)-y(b1))*(x(b2)-x(b1)) - (x(pk3)-x(b1))*(y(b2)-y(b1))
            IF ( term>0. ) THEN
               d = sqrt((x(pk3)-x(b1))**2+(y(pk3)-y(b1))**2) + sqrt((x(pk3)-x(b2))**2+(y(pk3)-y(b2))**2)
               IF ( j1==0 .OR. d1>=d ) THEN
                  j1 = k3
                  d1 = d
                  bflag = 1
               ENDIF
            ENDIF
!
!        (I)
!        consider the adjacent boundary point of the previous edge of
!        the polygon.  call its index number k0 and see if its closer
!        to the current edge than p(j1) and b(k3).
!
            k0 = k1 - 1
            IF ( k0<1 ) k0 = k
            pk0 = b(k0)
            term = (y(pk0)-y(b1))*(x(b2)-x(b1)) - (x(pk0)-x(b1))*(y(b2)-y(b1))
            IF ( term>0. ) THEN
               d = sqrt((x(pk0)-x(b1))**2+(y(pk0)-y(b1))**2) + sqrt((x(pk0)-x(b2))**2+(y(pk0)-y(b2))**2)
               IF ( j1==0 .OR. d1>=d ) THEN
                  j1 = k0
                  d1 = d
                  bflag = -1
               ENDIF
            ENDIF
         ENDIF

!        (J)
!        skip the next section if j1 is still zero, since a candidate
!        point for triangulation with edge b1,b2 was not found.

         IF ( j1/=0 ) THEN

! (K,L) If the search for a candidate point has already considered each
! boundary edge at least once (kt.gt.k) or if the boundary is being
!  checked for concave edges (j=0), then the next section
!        (section m) can be omitted.
!
            IF ( kt>k .OR. j==0 ) THEN
            ENDIF
         ENDIF
!
!        (M)
!        at this point the user may insert any additional constraint
!        on the triangle to be formed by the point pj1.  If the
!        candidate triangle fails the test, it is deleted from
!        consideration by setting the variable j1 to zero.
!
!!!  130 CONTINUE
!
!
!        (N,O)
!        the next procedure checks all boundary edges of the polygon
!        for intersection with the candidate triangle.  If any existing
!        boundary edge intersects any of the edges to be formed by the
!        candidate triangle, then the candidate point is rejected.  If
!        bflag is not zero, then the edge defined by j1=k0 or j1=k3 is
!        exempt form this test.
!
!        If there are three or less existing boundary edges or if
!        j1 has been set to zero, this test is omitted.
!
         IF ( k>3 .AND. j1/=0 ) THEN
            IF ( bflag==0 ) nq = p(j1)
            IF ( bflag==1 ) nq = b(k3)
            IF ( bflag==-1 ) nq = b(k0)
            SPAG_Loop_1_2: DO kcnt = 1, k
               IF ( kcnt/=k1 ) THEN
                  kn = kcnt + 1
                  IF ( kcnt==k ) kn = 1
                  IF ( .NOT.(bflag==-1 .AND. (kcnt==k0 .OR. kn==k0)) ) THEN
                     IF ( .NOT.(bflag==1 .AND. (kcnt==k3 .OR. kn==k3)) ) THEN
                        p1 = b(kcnt)
                        p2 = b(kn)
                        SPAG_Loop_2_1: DO jcnt = 1, 2
                           IF ( jcnt==1 .AND. (bflag==0 .OR. bflag==1) .AND. kcnt==k0 ) EXIT SPAG_Loop_2_1
                           IF ( jcnt==2 .AND. (bflag==0 .OR. bflag==-1) .AND. kcnt==k2 ) EXIT SPAG_Loop_2_1
                           bj = b1
                           IF ( jcnt==2 ) bj = b2
                           xqb = x(nq) - x(bj)
                           yqb = y(nq) - y(bj)
                           x12 = x(p1) - x(p2)
                           y12 = y(p1) - y(p2)
                           d = xqb*y12 - yqb*x12
                           IF ( d==0. ) CYCLE
                           x1b = x(p1) - x(bj)
                           y1b = y(p1) - y(bj)
                           s = (x1b*y12-y1b*x12)/d
                           IF ( s<0. .OR. s>1. ) CYCLE
                           tc = (xqb*y1b-yqb*x1b)/d
                           IF ( tc<0 .OR. tc>1. ) CYCLE
                           j1 = 0
                           EXIT SPAG_Loop_1_2
                        ENDDO SPAG_Loop_2_1
                        ! was 140
                     ENDIF
                  ENDIF
               ENDIF
               b1 = b1
            ENDDO SPAG_Loop_1_2
                       ! was 150
         ENDIF
!
!
!        (P,Q)
!        If j1 is zero, then the candidate point did not pass the above
!        tests or no point was found.  If bflag is not zero, then a
!        point on the boundary was found.
!
         IF ( j1/=0 ) THEN
!!!      IF (bflag) 240,170,210
            IF ( bflag>0 ) THEN
!
!
!        (S)
!        The triangulated point is the next point on the boundary.
!        Establish one new edge (from b(k1) to b(k3)), one new
!        triangle (from b(k1) to b(k2) to b(k3)), and delete one point
!        from the boundary (b(k2)).
!
!
               E(L+1,1) = min(b(k3),b(k1))
               E(L+1,2) = max(b(k3),b(k1))
               kk = 0
               kknt = 0
               kt = 0
               L = L + 1
               k = k - 1
               m = m + 1
               t(m,1) = min(b(k1),b(k2),b(k3))
               t(m,2) = middle(b(k1),b(k2),b(k3))
               t(m,3) = max(b(k1),b(k2),b(k3))

               DO kcnt = k2, k
                  b(kcnt) = b(kcnt+1)
               ENDDO

               IF ( k2==1 ) k1 = k1 - 1
            ELSE
               IF ( bflag/=0 ) THEN
                  IF ( bflag<0 ) THEN
!
!        (R)
!        The triangulated point is the previous point on the boundary.
!        Establish a new edge (from b(k0) to b(k2)), one new triangle
!        (from b(k0) to b(k1) to b(k2)), and delete one point from the
!        boundary (b(k1))
!
                     E(L+1,1) = min(b(k0),b(k2))
                     E(L+1,2) = max(b(k0),b(k2))
                     kk = 0
                     kknt = 0
                     kt = 0
                     L = L + 1
                     k = k - 1
                     m = m + 1
                     t(m,1) = min(b(k0),b(k1),b(k2))
                     t(m,2) = middle(b(k0),b(k1),b(k2))
                     t(m,3) = max(b(k0),b(k1),b(k2))

                     DO kcnt = k1, k
                        b(kcnt) = b(kcnt+1)
                     ENDDO
                     k1 = k1 - 1
                     IF ( k1<1 ) k1 = k
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
!
!
!        The triangulated point is outside the boundary.  Establish two
!        new edges, a new boundary point and delete one point from
!        outside the boundary.
!
!
               E(L+1,1) = min(p(j1),b(k1))
               E(L+1,2) = max(p(j1),b(k1))
               E(L+2,1) = min(p(j1),b(k2))
               E(L+2,2) = max(p(j1),b(k2))
               kt = 0
               L = L + 2
               m = m + 1
               t(m,1) = min(p(j1),b(k1),b(k2))
               t(m,2) = middle(p(j1),b(k1),b(k2))
               t(m,3) = max(p(j1),b(k1),b(k2))
               IF ( k1/=k ) THEN
                  km = k
                  kp1 = k1 + 1
                  SPAG_Loop_1_3: DO

                     b(km+1) = b(km)
                     km = km - 1
                     IF ( km<kp1 ) EXIT SPAG_Loop_1_3
                  ENDDO SPAG_Loop_1_3
               ENDIF

               b(k1+1) = p(j1)
               k = k + 1
               j = j - 1
               IF ( j1<=j ) THEN
                  DO jcnt = j1, j
                     p(jcnt) = p(jcnt+1)
                  ENDDO
               ENDIF
            ENDIF
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)

! (T) If there are any points remaining outside the boundary, then
! repeat the procedure for the next edge.

         IF ( j>0 .AND. j1/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( j>0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF


! (U,V,W) All points have been triangulated.  Check that all boundary
! edges form a concave polygon.

         IF ( kk==0 ) THEN
            kk = 1
            kl = 0
         ENDIF
         kknt = kknt + 1
         IF ( kknt<=N ) THEN
            SPAG_Loop_1_4: DO
               kl = kl + 1
               k2 = kl + 1
               IF ( k2>k ) k2 = 1
               k1 = kl - 1
               IF ( k1<1 ) k1 = k
               pkl = b(kl)
               b1 = b(k1)
               b2 = b(k2)
               term = (y(pkl)-y(b1))*(x(b2)-x(b1)) - (x(pkl)-x(b1))*(y(b2)-y(b1))
               IF ( term<0. ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( kl>=k ) EXIT SPAG_Loop_1_4
            ENDDO SPAG_Loop_1_4
         ENDIF

! (X) The triangulation is complete and has been checked for a
! concave boundary.  Now identify the boundary edges.

         DO lcnt = 1, L
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  Be(lcnt) = 0
                  kl = 0
                  spag_nextblock_2 = 2
               CASE (2)
                  kl = kl + 1
                  IF ( E(lcnt,1)==b(kl) ) THEN
                     k1 = kl + 1
                     IF ( k1>k ) k1 = 1
                     IF ( E(lcnt,2)/=b(k1) ) THEN
                        k1 = kl - 1
                        IF ( k1<1 ) k1 = k
                        IF ( E(lcnt,2)==b(k1) ) THEN
                           Be(lcnt) = 1
                           CYCLE
                    !!! GOTO 340
                        ENDIF
                     ELSE
                        Be(lcnt) = 1
                        CYCLE
                    !!! GOTO 340
                     ENDIF
                  ENDIF
                  IF ( kl<k ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO

! (Y) Finally, establish the indices of adjacent edges for each edge
! in the triangulation.  Each boundary edge will have two adjacent edges -
! each interior edge will have four.

         Te(1:L,1:4) = 0
!      DO 350 ll=1,4
!        DO 350 lcnt=1,l
!  350   te(lcnt,ll)=0

         DO mcnt = 1, m
            DO ll = 1, L
               IF ( E(ll,1)==t(mcnt,1) .AND. E(ll,2)==t(mcnt,2) ) l1 = ll
               IF ( E(ll,1)==t(mcnt,2) .AND. E(ll,2)==t(mcnt,3) ) l2 = ll
               IF ( E(ll,1)==t(mcnt,1) .AND. E(ll,2)==t(mcnt,3) ) l3 = ll
            ENDDO
            lambda = 0
            IF ( Te(l1,1)/=0 ) lambda = 2
            Te(l1,lambda+1) = l2
            Te(l1,lambda+2) = l3
            lambda = 0
            IF ( Te(l2,1)/=0 ) lambda = 2
            Te(l2,lambda+1) = l1
            Te(l2,lambda+2) = l3
            lambda = 0
            IF ( Te(l3,1)/=0 ) lambda = 2
            Te(l3,lambda+1) = l1
            Te(l3,lambda+2) = l2
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE triangulate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*==middle.f90 processed by SPAG 8.01RF 15:51 13 Dec 2024
FUNCTION middle(I,J,K) RESULT(m)
   IMPLICIT NONE
!
!
!        This function subprogram is used by the triangulation algorithm
!        to find the middle value of the three integer arguments (the
!        value which is neither a minimum or a maximum).  i, j and k are
!        are assumed to be discrete values with no two equal.
!
   INTEGER, INTENT(IN) :: I, J, K

   INTEGER :: m
!----------------------------------------------------------------------------
   IF ( J>=I .OR. I>=K ) THEN
      IF ( K>=I .OR. I>=J ) THEN
         IF ( I<J .AND. J<K ) THEN
            m = J
            RETURN
         ELSEIF ( K<J .AND. J<I ) THEN
            m = J
            RETURN
         ELSE
            m = K
            RETURN
         ENDIF
      ENDIF
   ENDIF
   m = I
   RETURN
END FUNCTION middle
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*==interpolate.f90 processed by SPAG 8.01RF 15:51 13 Dec 2024
SUBROUTINE interpolate(X,Y,U,Zcon,Ledges,Ie,Ismopt,Lambda,Xi,Eta,J,C,Ipowr,Jpowr,Ncoef)
   IMPLICIT NONE
! ---------------------------------------------------------------------------
! PURPOSE - Subroutine Interpolate is given a constant u value (bigu)
!  for which the contour line is to be drawn.  Check all given triangle
!  edges, (array ie) and check the values of u at the endpoints.
!  Interpolate for all possible values on the triangle edges.
!  If ismopt = 0, then use a linear interpolation, if ismopt not zero,
!  then evaluate for a non-linear surface using the coefficients
!  from smsrf and function subroutine Polyx2.
!
   REAL, INTENT(IN), DIMENSION(:) :: X, Y, U
                                                ! dependent and independent values
         ! for the relation u=f(x,y)

!  INTEGER,INTENT(IN):: n
   REAL, INTENT(IN) :: Zcon    ! CONSTANT VALUE OF Z FOR WHICH INTERPOLATION
         ! IS REQUIRED

   INTEGER, INTENT(IN) :: Ledges      ! NO. OF EDGES IN THE TRIANGULATION
   INTEGER, INTENT(IN), DIMENSION(:,:) :: Ie    ! edge endpoint indices from
         ! triangulation
   INTEGER, INTENT(IN) :: Ismopt      ! smoothing option flag,  0=off, 1=on
   INTEGER, INTENT(OUT), DIMENSION(:) :: Lambda    ! index of edges for interpolated points
   REAL, INTENT(OUT), DIMENSION(:) :: Xi    ! x-coordinates of interpolated points
   REAL, INTENT(OUT), DIMENSION(:) :: Eta   ! y-coordinates of interpolated points
   INTEGER, INTENT(OUT) :: J     ! length of xi and eta
   REAL, INTENT(IN), DIMENSION(:) :: C    ! coefficients of each term of the equation.
   INTEGER, INTENT(IN), DIMENSION(:) :: Ipowr, Jpowr
   INTEGER, INTENT(IN) :: Ncoef


!        ipowr,jpowr are the list of exponents for each term of
!        the polynomial used to smooth the data  (input).
!        ncoef     = number of terms in the polynomial
!                    (ipowr,jpowr,c, and ncoef are input)
!
!
!     ------------------------------------------------------------------
!
!
!      DIMENSION x(n), y(n), u(n)
!      DIMENSION ie(1494,2), xi(1494), eta(1494), lambda(1494)
!      DIMENSION ipowr(23), jpowr(23), c(23)

!
   REAL :: f1, fn
   INTEGER :: i1, i2
   INTEGER :: k
   INTEGER :: lcnt
   INTEGER :: n
   LOGICAL :: smoothed
   REAL :: t1, t2
   REAL :: temp
   REAL :: u1, u2
   REAL :: x1, x2
   REAL :: xn, yn
   REAL :: y1, y2
!----------------------------------------------------------------------------
   n = min(size(X),size(Y),size(U))
   smoothed = Ismopt/=0
   IF ( Ncoef<1 ) smoothed = .FALSE.

!!!      IF (ncoef.lt.1) ismopt=0   ! deal with this
   J = 0
!
   DO lcnt = 1, Ledges
!
!        (A)
!        Determine x,y,z for the endpoints of the nextedge - order them
!
      i1 = Ie(lcnt,1)
      i2 = Ie(lcnt,2)
      x1 = X(i1)
      x2 = X(i2)
      y1 = Y(i1)
      y2 = Y(i2)
      u1 = U(i1)
      u2 = U(i2)
!
!        (B)
!        Function values equal at endpoints or
!        constant zc not between them? . .
!
      IF ( u1==u2 ) CYCLE         !!! GOTO 70
      IF ( u1>=u2 ) THEN
         temp = u2
         u2 = u1
         u1 = temp
         temp = x2
         x2 = x1
         x1 = temp
         temp = y2
         y2 = y1
         y1 = temp
      ENDIF

      IF ( Zcon<u1 .OR. u2<Zcon ) CYCLE           !!! GOTO 70
      IF ( u2==Zcon ) u2 = 1.000001*Zcon
      J = J + 1
!
!        (C)
!        Has data been smoothed? . .
!        If not, goto section e (statement label 101)
!
      IF ( smoothed ) THEN
!         (D,F) Non-linear interpolation is required on this edge
         f1 = polyx2(Zcon,x1,y1,C,Ipowr,Jpowr,Ncoef)
         SPAG_Loop_2_1: DO k = 1, 10
            xn = 0.5*(x1+x2)
            yn = 0.5*(y1+y2)
            fn = polyx2(Zcon,xn,yn,C,Ipowr,Jpowr,Ncoef)
            IF ( fn==0.0 ) EXIT SPAG_Loop_2_1
            IF ( fn<0. .AND. f1<0. ) THEN
               x1 = xn
               y1 = yn
            ELSEIF ( fn>0. .AND. f1>0. ) THEN
               x1 = xn
               y1 = yn
            ELSE
               x2 = xn
               y2 = yn
            ENDIF
         ENDDO SPAG_Loop_2_1
         Xi(J) = (x1+x2)*0.5
         Eta(J) = (y1+y2)*0.5
      ELSE
!         (E,F) Linear interpolation is required for this edge
         t1 = (u2-Zcon)/(u2-u1)
         t2 = 1.0 - t1
         Xi(J) = t1*x1 + t2*x2
         Eta(J) = t1*y1 + t2*y2
      ENDIF
      Lambda(J) = lcnt
   ENDDO

END SUBROUTINE interpolate
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!*==cntour.f90 processed by SPAG 8.01RF 15:51 13 Dec 2024
SUBROUTINE cntour(Zcon,Xi,Eta,Lambda,J,Ibe,Ite,cntcrv)
   IMPLICIT NONE
! ---------------------------------------------------------------------------
! PURPOSE -
!     A set of j interpolated points for z=zcon  (xi(i),eta(i) on edge
!     lambda(i) for i=1,j), the contour lines must now be drawn.  There
!     may be several lines, either open or closed contours.  This
!     algorithm will use the triangulation relationships to sort out
!     each line in order.  As each contour line is established, user
!     supplied program CNTCRV is called to output it to the graphics
!     device being used.
!
!
!     ARGUMENTS (ALL ARE INPUTS) -
!        ZCON    = CONSTANT VALUE OF Z UNDER CONSIDERATION
!        XI(J)   = ARRAY OF X COORDINATES OF INTERPOLATED POINTS
!        ETA(J)  = ARRAY OF Y COORDINATES OF INTERPOLATED POINTS
!        LAMBDA(J) = ARRAY OF EDGE NUMBERS FOR J-TH INTERPOLATED POINT
!        J       = NUMBER OF POINTS IN THE LIST OF INTERPOLATED POINTS
!        IBE     = THE LIST OF BOUNDARY EDGES TAKEN FROM THE TRIANGULATION
!        ITE     = LINKED LIST OF INDICES OF ADJACENT EDGES PROVIDED
!                  BY THE TRIANGULATION PROCEDURE.

REAL,INTENT(IN)                    :: Zcon   ! z-value of the contour line
REAL,INTENT(INOUT),DIMENSION(:)    :: Xi     ! x coordinates of interpolated points
REAL,INTENT(INOUT),DIMENSION(:)    :: Eta    ! array of y coordinates of interpolated points
INTEGER,INTENT(INOUT),DIMENSION(:) :: Lambda ! array of edge numbers for j-th interpolated point
INTEGER,INTENT(INOUT)              :: J      ! number of points in the list of interpolated points
INTEGER,INTENT(IN),DIMENSION(:)    :: Ibe    ! the list of boundary edges from the triangulation
INTEGER,INTENT(IN),DIMENSION(:,:)  :: Ite    ! linked list of indices of adjacent edges provided by the triangulation procedure.
INTEGER                            :: i
INTEGER                            :: j1,l1
INTEGER                            :: j1big
INTEGER                            :: jcnt,lcnt
INTEGER                            :: jj,kk
INTEGER                            :: knt
INTEGER                            :: npoint
REAL,DIMENSION(1494)               :: xx,yy
EXTERNAL cntcrv
INTEGER                            :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)

!!!      DIMENSION  XI(1494),ETA(1494),LAMBDA(1494),IBE(1494),XX(1494),    &
!!!     &           YY(1494),ITE(1494,4)
!----------------------------------------------------------------------------

! (A) Initialize local variables

         IF ( J==0 ) RETURN
         spag_nextblock_1 = 2
      CASE (2)
         j1 = 0
         SPAG_Loop_1_1: DO

! (B,C) Search the list of edges for a boundary edge (be(i)=1)

            j1 = j1 + 1
            l1 = Lambda(j1)
            IF ( Ibe(l1)==1 ) THEN

! (D) Search for a boundary edge and put it at the top of the list.
!     Put this interpolated point at the top of the list for this contour,
!     set j1

               IF ( j1/=J ) THEN
                  Xi(J+1) = Xi(j1)
                  Eta(J+1) = Eta(j1)
                  Lambda(J+1) = Lambda(j1)
                  DO jcnt = j1, J
                     Xi(jcnt) = Xi(jcnt+1)
                     Eta(jcnt) = Eta(jcnt+1)
                     Lambda(jcnt) = Lambda(jcnt+1)
                  ENDDO
               ENDIF

! (E) Search the remaining points for an adjacent (common) edge

               j1big = J
               lcnt = l1
               EXIT SPAG_Loop_1_1
            ELSEIF ( j1>=J ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 3
      CASE (3)
         j1big = j1big - 1
         j1 = 0
         SPAG_Loop_1_2: DO
            j1 = j1 + 1
            l1 = Lambda(j1)
            DO i = 1, 4
               IF ( l1==Ite(lcnt,i) ) EXIT SPAG_Loop_1_2
            ENDDO

! (F) ERROR - THERE IS NO NEXT POINT.
            IF ( j1>=j1big ) RETURN
         ENDDO SPAG_Loop_1_2

! (G) Put this point at the top of the list.  Continue if it is
! not a boundary edge.

         Xi(J+1) = Xi(j1)
         Eta(J+1) = Eta(j1)
         Lambda(J+1) = Lambda(j1)
         DO jcnt = j1, J
            Xi(jcnt) = Xi(jcnt+1)
            Eta(jcnt) = Eta(jcnt+1)
            Lambda(jcnt) = Lambda(jcnt+1)
         ENDDO
         lcnt = l1
         IF ( Ibe(l1)/=1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF

! (H) Draw the open contour line through the points
!  xi(j1),eta(j1) ...... xi(j1+1),eta(j1+1) ...... xi(j),eta(j)
!  Then reset j and continue

         npoint = J - j1big + 1
         IF ( npoint>1 ) THEN
            WRITE (*,*) "calling cntcrv (open) with", npoint, "points"
            CALL cntcrv(Xi(j1big:J),Eta(j1big:J),npoint,Zcon)
         ENDIF

         J = j1big - 1
!
! (I)  are there any more points left? . .
!
!      IF (j) 200,200,10
         IF ( J<=0 ) RETURN
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)

! (J) Now draw internal lines (closed contours that do not start or stop
! at boundary edges).  The point at j1big=j in the list is chosen to
! start the contour.
!
         j1big = J + 1
         lcnt = Lambda(J)
         spag_nextblock_1 = 5
      CASE (5)

! (K,M,P) Find the next point for this contour (on an edge with a common
!  end point).  Put it at the top of the list, and repeat until no more
!  common edges remain for this line.

         j1big = j1big - 1
         j1 = 0
         IF ( j1big>J ) j1 = 1
         SPAG_Loop_1_3: DO
            j1 = j1 + 1
            l1 = Lambda(j1)
            DO i = 1, 4
               IF ( l1==Ite(lcnt,i) ) EXIT SPAG_Loop_1_3
            ENDDO
            IF ( j1>=j1big ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1

! (L) Otherwise, no adjacent edge was found.
! This contour line is complete, go draw it.
            ENDIF
         ENDDO SPAG_Loop_1_3

         Xi(J+1) = Xi(j1)
         Eta(J+1) = Eta(j1)
         Lambda(J+1) = Lambda(j1)
         DO jcnt = j1, J
            Xi(jcnt) = Xi(jcnt+1)
            Eta(jcnt) = Eta(jcnt+1)
            Lambda(jcnt) = Lambda(jcnt+1)
         ENDDO
         lcnt = l1
         IF ( j1big/=1 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 6
      CASE (6)

! (O) Draw the closed contour line, the interpolated line through
!  xi(j1),eta(j1) ...... xi(j),eta(j) ...... xi(j1),eta(j1)

         jj = j1big
         IF ( j1big/=1 ) jj = j1big + 1
         knt = 0
         DO kk = jj, J
            knt = knt + 1
            xx(knt) = Xi(kk)
            yy(knt) = Eta(kk)
         ENDDO

         xx(knt+1) = xx(1)
         yy(knt+1) = yy(1)
         npoint = knt + 1
         WRITE (*,*) "calling cntcrv (closed) with", npoint, " points"
         CALL cntcrv(xx(1:npoint),yy(1:npoint),npoint,Zcon)

! (P) Reset j.  Establish the next contour line for remaining points
!  or quit the procedure if no more points remain.

         J = j1big - 1
         IF ( J>0 ) J = J + 1
!      IF (j) 200,200,120
         IF ( J>0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cntour
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_contourplot()
use M_framework__verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_framework__verify, only : unit_check_level
!! setup
   call test_contourlines()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_contourlines()

   call unit_check_start('contourlines',msg='')
   !!call unit_check('contourlines', 0.eq.0, 'checking', 100)
   call unit_check_done('contourlines',msg='')
end subroutine test_contourlines
!===================================================================================================================================
end subroutine test_suite_M_contourplot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END Module M_ContourPlot
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

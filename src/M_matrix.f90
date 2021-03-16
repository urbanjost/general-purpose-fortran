










module M_matrix
!>
!! Originally based on a routine called MATLAB, although heavily modified
!! since. The original stated ...
!!
!!    MATLAB stands for MATrix LABoratory.  It is a FORTRAN package developed
!!    by Argonne National Laboratories for in-house use.  It provides
!!    comprehensive vector and tensor operations in a package which may be
!!    programmed, either through a macro language or through execution of
!!    script files.
!!
!!    Matlab is reentrant and recursive.  Functions supported include (but
!!    are not by any means limited to) sin, cos, tan, arcfunctions, upper
!!    triangular, lower triangular, determinants, matrix multiplication,
!!    identity, Hilbert matrices, eigenvalues and eigenvectors, matrix
!!    roots and products, inversion and so on and so forth.
!!
!!    The file available on the bulletin board as Matlab.arc contains an
!!    Amiga-ized executable copy of MATLAB and the online help file, as
!!    well as this intro.
!!
!!    If you want the source code (over 300K) and a manual, or if your
!!    bulletin board only has this message and not the package, send $5.00
!!    and a 3.5" disk to:
!!
!!                               Jim Locker
!!                               4443 N. Hyland Ave.
!!                               Dayton, OH 45424
!!
!!    The package is public domain, but of course postage and reproduction
!!    cost money.  Believe me, this package is a bargain at the price.
!!    Please feel free to distribute the package.
!!
!!    The source was taken off a VAX 11/780. It ran without modification
!!    (except the file handler and some minor error handling) on an Amiga
!!    1000 using ABSoft Fortran v2.2.  It will run in 512K environment.
!!    I have seen it on IBM mainframes and IBM PCs.
!! ================================================================================
!!    Changes per John S. Urban:
!!
!!    Converted to do most I/O via journal() so can be used with my codes
!!    more easily. Also allow additional comment indicator (# in column 1)
!!    so can read back in trail files made by DIARY command.
!!
!!    Added command history editor command ";"
!!
!!    Made call to system shell with SH command
!!
!!    Made case-sensitive
!!
!!    Made it take directives from string on routine call
!!
!!    Allow longer filenames
!!
!!    Partly converted program away from use of HOLLERITH towards use of ADE
!!    or maybe even character variables (enough to be able to use GNU g95
!!    compiler, anyway). Might have to change the way I make a letter
!!    "hollerith" on non little-endian non-32bit platforms.
!!
!!    changed RETURN command to QUIT
!!
!!    built-in help document
!!
!!    Sample compile commands:
!!
!!       g77 --no-backslash matrix.f90
!!       g95 matrix.f90
!!       gfortran -fno-range-check matrix.f90
use M_journal, only : journal
implicit none
integer,parameter :: BIGMEM=200005
public MAT88
public mat_str2buf
public mat_buf2str
public test_suite_M_matrix
! till get rid of type mismatches, the following are public
!==================================================================================================================================!
character(len=1024),save :: STRINGQ
integer,save             :: ISTRINGQ
integer,save             :: INITQ
!==================================================================================================================================!
INTEGER,PARAMETER        :: IALF=78
DOUBLEPRECISION          :: STKR(BIGMEM),STKI(BIGMEM)
INTEGER                  :: IDSTK(4,48),LSTK(48),MSTK(48),NSTK(48),VSIZE,LSIZE,BOT,TOP
INTEGER                  :: ALFA(IALF),ALFB(IALF),alflq,CASE
INTEGER                  :: IDS(4,32),PSTK(32),RSTK(32),PSIZE,PT,PTZ
INTEGER                  :: DDT,ERR,FMT,LCT(4),LIN(1024),LPT(6),HIO,RIO,RTE,WTE,FE
INTEGER                  :: SYM,SYN(4),BUF(1024),CHRA,FLP(2),FIN,FUN,LHS,RHS,RAN(2)
!==================================================================================================================================!

      !                                       0---------1---------2---------3---------4--------- 5---------6---------7-------
      !                                       01234567890123456789012345678901234567890123456789 0123456789012345678901234567
      character(len=ialf),parameter ::  CH_A='0123456789abcdefghijklmnopqrstuvwxyz ();:+-*/\=.,''<>ABCDEFGHIJKLMNOPQRSTUVWXYZ'

      ! ALTERNATE CHARACTER SET
      !                                       0---------1---------2---------3---------4---------5---------6---------7-------
      !                                       012345678901234567890123456789012345678901234567890123456789012345678901234567
      character(len=ialf),parameter ::  CH_B='0123456789abcdefghijklmnopqrstuvwxyz {};|+-*/$=@,"[]ABCDEFGHIJKLMNOPQRSTUVWXYZ'

!==================================================================================================================================!
integer,parameter :: eol=99
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     MAT88(3f) - [M_matrix] initialize and/or pass commands to matrix laboratory interpreter
!!##SYNOPSIS
!!
!!   subroutine MAT88(init,cmd)
!!
!!    integer,intent(in)          :: init
!!    character(len=*),intent(in) :: cmd
!!##DESCRIPTION
!!    MAT88(3f) is modeled on MATLAB(3f) (MATrix LABoratory), a FORTRAN
!!    package developed by Argonne National Laboratories for in-house use.
!!    It provides comprehensive vector and tensor operations in a package
!!    which may be programmed, either through a macro language or through
!!    execution of script files.
!!
!!    Matlab is reentrant and recursive. Functions supported include (but
!!    are not by any means limited to) sin, cos, tan, arcfunctions, upper
!!    triangular, lower triangular, determinants, matrix multiplication,
!!    identity, Hilbert matrices, eigenvalues and eigenvectors, matrix
!!    roots and products, inversion and so on and so forth.
!!
!!    The HELP command describes using the interpreter.
!!
!!##OPTIONS
!!    INIT   flag indicating purpose of call
!!
!!             0 For ordinary first entry with reads from stdin
!!            -1 negative for silent initialization (ignores CMD)
!!             1 positive for subsequent entries, enter command mode
!!               reading commands from stdin.
!!             2 subsequent entry , return after doing CMD
!!
!!    CMD     MAT88 command to perform
!!##EXAMPLE
!!
!!   Sample program
!!
!!       program demo_MAT88
!!       use M_matrix, only : mat88
!!       call MAT88(0,' ')
!!       end program demo_MAT88
!!       !-------------------------------------------------------------
!!       SUBROUTINE mat88_user(A,M,N,S,T)  ! sample mat88_user routine
!!       ! Allows personal  Fortran  subroutines  to  be  linked  into
!!       ! MAT88. The subroutine should have the heading
!!       !
!!       !               SUBROUTINE mat88_user(A,M,N,S,T)
!!       !               DOUBLEPRECISION A(M,N),S,T
!!       !
!!       ! The MAT88 statement Y = USER(X,s,t) results in a call to
!!       ! the subroutine with a copy of the matrix X stored in the
!!       ! argument A, its column and row dimensions in M and N,
!!       ! and the scalar parameters S and T stored in S and T.
!!       ! If S and T are omitted, they are set to 0.0. After
!!       ! the return, A is stored in Y. The dimensions M and
!!       ! N may be reset within the subroutine. The statement Y =
!!       ! USER(K) results in a call with M = 1, N = 1 and A(1,1) =
!!       ! FLOAT(K). After the subroutine has been written, it must
!!       ! be compiled and linked to the MAT88 object code within the
!!       ! local operating system.
!!       !
!!       integer M,N
!!       DOUBLEPRECISION A(M,N),S,T
!!       !
!!       if(s.eq.0.and.t.eq.0)then
!!          ! print statistics for matrix, for example
!!          write(*,*)'m=',m
!!          write(*,*)'n=',n
!!          write(*,*)'s=',s
!!          write(*,*)'t=',t
!!
!!          DO i10 = 1, M
!!             write(*,*)(a(i10,i20),i20=1,n)
!!          enddo
!!       else  ! a(i,j)=a(i,j)*s+t  in a linear fashion
!!          DO i30 = 1, M
!!             DO i40 = 1, N
!!                a(i30,i40)=a(i30,i40)*S+T
!!             enddo
!!          enddo
!!       endif
!!       END SUBROUTINE mat88_user
!!
!!   Example 2:
!!
!!    program bigmat
!!    use M_matrix, only : mat88
!!    ! pass strings to MAT88 but do not enter interactive mode
!!    call mat88(-1,' ')                    ! initialize
!!    call mat88( 2,'a=<1 2 3 4; 5 6 7 8>')
!!    call mat88( 2,'save("file1")')
!!    call mat88( 2,'help INTRO')
!!    end
!!
!!  Example inputs
!!
!!      avg:for i = 2:2:n, for j = 2:2:n, t = (a(i-1,j-1)+a(i-1,j)+a(i,j-1)+a(i,j))/4; ...
!!      avg:   a(i-1,j-1) = t; a(i,j-1) = t; a(i-1,j) = t; a(i,j) = t;
!!
!!      cdiv:// ======================================================
!!      cdiv:// cdiv
!!      cdiv:a=sqrt(random(8)
!!      cdiv:   ar = real(a); ai = imag(a); br = real(b); bi = imag(b);
!!      cdiv:   p = bi/br;
!!      cdiv:   t = (ai - p*ar)/(br + p*bi);
!!      cdiv:   cr = p*t + ar/br;
!!      cdiv:   ci = t;
!!      cdiv:   p2 = br/bi;
!!      cdiv:   t2 = (ai + p2*ar)/(bi + p2*br);
!!      cdiv:   ci2 = p2*t2 - ar/bi;
!!      cdiv:   cr2 = t2;
!!      cdiv:   s = abs(br) + abs(bi);
!!      cdiv:   ars = ar/s;
!!      cdiv:   ais = ai/s;
!!      cdiv:   brs = br/s;
!!      cdiv:   bis = bi/s;
!!      cdiv:   s = brs**2 + bis**2;
!!      cdiv:   cr3 = (ars*brs + ais*bis)/s;
!!      cdiv:   ci3 = (ais*brs - ars*bis)/s;
!!      cdiv:   <cr ci; cr2 ci2; cr3 ci3>
!!      cdiv:// ======================================================
!!
!!      exp:t = 0*x + eye; s = 0*eye(x); n = 1;
!!      exp:while abs(s+t-s) > 0, s = s+t, t = x*t/n, n = n + 1
!!
!!      four:n
!!      four:pi = 4*atan(1);
!!      four:i = sqrt(-1);
!!      four:w = exp(2*pi*i/n);
!!      four:F = <>;
!!      four:for k = 1:n, for j = 1:n, F(k,j) = w**((j-1)*(k-1));
!!      four:F = F/sqrt(n);
!!      four:alfa = r*pi;
!!      four:rho = exp(i*alfa);
!!      four:S = log(rho*F)/i - alfa*EYE;
!!      four:serr = norm(imag(S),1);
!!      four:S = real(S);
!!      four:serr = serr + norm(S-S',1)
!!      four:S = (S + S')/2;
!!      four:ferr = norm(F-exp(i*S),1)
!!
!!      gs:for k = 1:n, for j = 1:k-1, d = x(k,:)*x(j,:)'; x(k,:) = x(k,:) - d*x(j,:); ...
!!      gs:   end, s = norm(x(k,:)), x(k,:) = x(k,:)/s;
!!
!!      jacobi:<n, n> = size(A);
!!      jacobi:X = eye(n);
!!      jacobi:anorm = norm(A,'fro');
!!      jacobi:cnt = 1;
!!      jacobi:while cnt > 0,...
!!      jacobi:  cnt = 0;...
!!      jacobi:  for p = 1:n-1,...
!!      jacobi:    for q = p+1:n,...
!!      jacobi:      if anorm + abs(a(p,q)) > anorm,...
!!      jacobi:        cnt = cnt + 1;...
!!      jacobi:        exec('jacstep');...
!!      jacobi:      end,...
!!      jacobi:    end,...
!!      jacobi:  end,...
!!      jacobi:  display(rat(A)),...
!!      jacobi:end
!!
!!      jacstep:        d = (a(q,q)-a(p,p))*0.5/a(p,q);
!!      jacstep:        t = 1/(abs(d)+sqrt(d*d+1));
!!      jacstep:        if d < 0, t = -t; end;
!!      jacstep:        c = 1/sqrt(1+t*t);  s = t*c;
!!      jacstep:        R = eye(n); r(p,p)=c; r(q,q)=c; r(p,q)=s; r(q,p)=-s;
!!      jacstep:        X = X*R;
!!      jacstep:        A = R'*A*R;
!!
!!      kron://  C = Kronecker product of A and B
!!      kron:<m, n> = size(A);
!!      kron:for i = 1:m, ...
!!      kron:   ci = a(i,1)*B; ...
!!      kron:   for j = 2:n, ci = <ci a(i,j)*B>; end ...
!!      kron:   if i = 1, C = ci; else, C = <C; ci>;
!!
!!      lanczos:<n,n> = size(A);
!!      lanczos:q1 = rand(n,1);
!!      lanczos:ort
!!      lanczos:alfa = <>; beta = <>;
!!      lanczos:q = q1/norm(q1); r = A*q(:,1);
!!      lanczos:for j = 1:n, exec('lanstep',0);
!!
!!      lanstep:alfa(j) = q(:,j)'*r;
!!      lanstep:r = r - alfa(j)*q(:,j);
!!      lanstep:if ort <> 0, for k = 1:j-1, r = r - r'*q(:,k)*q(:,k);
!!      lanstep:beta(j) = norm(r);
!!      lanstep:q(:,j+1) = r/beta(j);
!!      lanstep:r = A*q(:,j+1) - beta(j)*q(:,j);
!!      lanstep:if j > 1, T = diag(beta(1:j-1),1); T = diag(alfa) + T + T'; eig(T)
!!
!!      mgs:for k = 1:n, s = norm(x(k,:)), x(k,:) = x(k,:)/s; ...
!!      mgs:   for j = k+1:n, d = x(j,:)*x(k,:)'; x(j,:) = x(j,:) - d*x(k,:);
!!
!!      net:C = <
!!      net:1 2 15 . . .
!!      net:2 1 3 . . .
!!      net:3 2 4 11 . .
!!      net:4 3 5 . . .
!!      net:5 4 6 7 . .
!!      net:6 5 8 . . .
!!      net:7 5 9 30 . .
!!      net:8 6 9 10 11 .
!!      net:9 7 8 30 . .
!!      net:10 8 12 30 31 34
!!      net:11 3 8 12 13 .
!!      net:12 10 11 34 36 .
!!      net:13 11 14 . . .
!!      net:14 13 15 16 38 .
!!      net:15 1 14 . . .
!!      net:16 14 17 20 35 37
!!      net:17 16 18 . . .
!!      net:18 17 19 . . .
!!      net:19 18 20 . . .
!!      net:20 16 19 21 . .
!!      net:21 20 22 . . .
!!      net:22 21 23 . . .
!!      net:23 22 24 35 . .
!!      net:24 23 25 39 . .
!!      net:25 24 . . . .
!!      net:26 27 33 39 . .
!!      net:27 26 32 . . .
!!      net:28 29 32 . . .
!!      net:29 28 30 . . .
!!      net:30 7 9 10 29 .
!!      net:31 10 32 . . .
!!      net:32 27 28 31 34 .
!!      net:33 26 34 . . .
!!      net:34 10 12 32 33 35
!!      net:35 16 23 34 36 .
!!      net:36 12 35 38 . .
!!      net:37 16 38 . . .
!!      net:38 14 36 37 . .
!!      net:39 24 26 . . .
!!      net:>;
!!      net:<n, m> = size(C);
!!      net:A = 0*ones(n,n);
!!      net:for i=1:n, for j=2:m, k=c(i,j); if k>0, a(i,k)=1;
!!      net:check = norm(A-A',1), if check > 0, quit
!!      net:<X,D> = eig(A+eye);
!!      net:D = diag(D);  D = D(n:-1:1)
!!      net:X = X(:,n:-1:1);
!!      net:<x(:,1)/sum(x(:,1)) x(:,2) x(:,3) x(:,19)>
!!
!!      pascal://Generate next Pascal matrix
!!      pascal:<k,k> = size(L);
!!      pascal:k = k + 1;
!!      pascal:L(k,1:k) = <L(k-1,:) 0> + <0 L(k-1,:)>;
!!
!!      pdq:alfa = <>; beta = 0; q = <>; p = p(:,1)/norm(p(:,1));
!!      pdq:t = A'*p(:,1);
!!      pdq:alfa(1) = norm(t);
!!      pdq:q(:,1) = t/alfa(1);
!!      pdq:X = p(:,1)*(alfa(1)*q(:,1))'
!!      pdq:e(1) = norm(A-X,1)
!!      pdq:for j = 2:r, exec('pdqstep',ip); ...
!!      pdq:   X = X + p(:,j)*(alfa(j)*q(:,j)+beta(j)*q(:,j-1))', ...
!!      pdq:   e(j) = norm(A-X,1)
!!
!!      pdqstep:t = A*q(:,j-1) - alfa(j-1)*p(:,j-1);
!!      pdqstep:   if ort>0, for i = 1:j-1, t = t - t'*p(:,i)*p(:,i);
!!      pdqstep:beta(j) = norm(t);
!!      pdqstep:p(:,j) = t/beta(j);
!!      pdqstep:t = A'*p(:,j) - beta(j)*q(:,j-1);
!!      pdqstep:   if ort>0, for i = 1:j-1, t = t - t'*q(:,i)*q(:,i);
!!      pdqstep:alfa(j) = norm(t);
!!      pdqstep:q(:,j) = t/alfa(j);
!!
!!      pop:y = < 75.995   91.972  105.711  123.203   ...
!!      pop:     131.669  150.697  179.323  203.212>'
!!      pop:t = < 1900:10:1970 >'
!!      pop:t = (t - 1940*ones(t))/40;   <t y>
!!      pop:n = 8;  A(:,1) = ones(t);  for j = 2:n, A(:,j) = t .* A(:,j-1);
!!      pop:A
!!      pop:c = A\y
!!
!!      qr:scale = s(m);
!!      qr:sm = s(m)/scale; smm1 = s(m-1)/scale; emm1 = e(m-1)/scale;
!!      qr:sl = s(l)/scale; el = e(l)/scale;
!!      qr:b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2;
!!      qr:c = (sm*emm1)**2;
!!      qr:shift = sqrt(b**2+c); if b < 0, shift = -shift;
!!      qr:shift = c/(b + shift)
!!      qr:f = (sl + sm)*(sl-sm) - shift
!!      qr:g = sl*el
!!      qr:for k = l: m-1, exec('qrstep',ip)
!!      qr:e(m-1) = f
!!
!!      qrstep:exec('rot');
!!      qrstep:if k <> l, e(k-1) = f
!!      qrstep:f = cs*s(k) + sn*e(k)
!!      qrstep:e(k) = cs*e(k) - sn*s(k)
!!      qrstep:g = sn*s(k+1)
!!      qrstep:s(k+1) = cs*s(k+1)
!!      qrstep:exec('rot');
!!      qrstep:s(k) = f
!!      qrstep:f = cs*e(k) + sn*s(k+1)
!!      qrstep:s(k+1) = -sn*e(k) + cs*s(k+1)
!!      qrstep:g = sn*e(k+1)
!!      qrstep:e(k+1) = cs*e(k+1)
!!
!!      rho://Conductivity example.
!!      rho://Parameters ---
!!      rho:   rho       //radius of cylindrical inclusion
!!      rho:   n         //number of terms in solution
!!      rho:   m         //number of boundary points
!!      rho://initialize operation counter
!!      rho:   flop = <0 0>;
!!      rho://initialize variables
!!      rho:   m1 = round(m/3);   //number of points on each straight edge
!!      rho:   m2 = m - m1;       //number of points with Dirichlet conditions
!!      rho:   pi = 4*atan(1);
!!      rho://generate points in Cartesian coordinates
!!      rho:   //right hand edge
!!      rho:   for i = 1:m1, x(i) = 1; y(i) = (1-rho)*(i-1)/(m1-1);
!!      rho:   //top edge
!!      rho:   for i = m2+1:m, x(i) = (1-rho)*(m-i)/(m-m2-1); y(i) = 1;
!!      rho:   //circular edge
!!      rho:   for i = m1+1:m2, t = pi/2*(i-m1)/(m2-m1+1); ...
!!      rho:      x(i) = 1-rho*sin(t);  y(i) = 1-rho*cos(t);
!!      rho://convert to polar coordinates
!!      rho:   for i = 1:m-1, th(i) = atan(y(i)/x(i));  ...
!!      rho:      r(i) = sqrt(x(i)**2+y(i)**2);
!!      rho:   th(m) = pi/2;  r(m) = 1;
!!      rho://generate matrix
!!      rho:   //Dirichlet conditions
!!      rho:   for i = 1:m2, for j = 1:n, k = 2*j-1; ...
!!      rho:      a(i,j) = r(i)**k*cos(k*th(i));
!!      rho:   //Neumann conditions
!!      rho:   for i = m2+1:m, for j = 1:n, k = 2*j-1; ...
!!      rho:      a(i,j) = k*r(i)**(k-1)*sin((k-1)*th(i));
!!      rho://generate right hand side
!!      rho:   for i = 1:m2, b(i) = 1;
!!      rho:   for i = m2+1:m, b(i) = 0;
!!      rho://solve for coefficients
!!      rho:   c = A$b
!!      rho://compute effective conductivity
!!      rho:   c(2:2:n) = -c(2:2:n)
!!      rho:   sigma = sum(c)
!!      rho://output total operation count
!!      rho:   ops = flop(2)
!!
!!      rogers.exec:exec('d.boug');                        // reads data
!!      rogers.exec:<g,k> = size(p);               // p is matrix of gene frequencies
!!      rogers.exec:wv = ncen/sum(ncen);           // ncen contains population sizes
!!      rogers.exec:pbar = wv*p;                   // weighted average of p
!!      rogers.exec:p = p - ones(g,1)*pbar;        // deviations from mean
!!      rogers.exec:p = sqrt(diag(wv)) * p;        // weight rows of p by sqrt of pop size
!!      rogers.exec:h = diag(pbar); h = h*(eye-h); // diagonal contains binomial variance: p*(1-p)
!!      rogers.exec:r = p*inv(h)*p'/k;             // normalized covariance matrix
!!      rogers.exec:eig(r)'
!!
!!      rosser:A  = <
!!      rosser:  611.  196. -192.  407.   -8.  -52.  -49.   29.
!!      rosser:  196.  899.  113. -192.  -71.  -43.   -8.  -44.
!!      rosser: -192.  113.  899.  196.   61.   49.    8.   52.
!!      rosser:  407. -192.  196.  611.    8.   44.   59.  -23.
!!      rosser:   -8.  -71.   61.    8.  411. -599.  208.  208.
!!      rosser:  -52.  -43.   49.   44. -599.  411.  208.  208.
!!      rosser:  -49.   -8.    8.   59.  208.  208.   99. -911.
!!      rosser:   29.  -44.   52.  -23.  208.  208. -911.   99.  >;
!!
!!      rot:// subexec rot(f,g,cs,sn)
!!      rot:      rho = g; if abs(f) > abs(g), rho = f;
!!      rot:      cs = 1.0; sn = 0.0; z = 1.0;
!!      rot:      r = norm(<f g>); if rho < 0, r = -r; r
!!      rot:      if r <> 0.0, cs = f/r
!!      rot:      if r <> 0.0, sn = g/r
!!      rot:      if abs(f) > abs(g), z = sn;
!!      rot:      if abs(g) >= abs(f), if cs <> 0, z = 1/cs;
!!      rot:      f = r;
!!      rot:      g = z;
!!
!!      rqi:rho = (x'*A*x)
!!      rqi:x = (A-rho*eye)\x;
!!      rqi:x = x/norm(x)
!!
!!      setup:diary('xxx')
!!      setup:!tail -f xxx > /dev/tty1 &
!!      setup:!tail -f xxx > /dev/tty2 &
!!
!!      sigma:RHO = .5  M = 20  N = 10   SIGMA =  1.488934271883534
!!      sigma:RHO = .5  M = 40  N = 20   SIGMA =  1.488920312974229
!!      sigma:RHO = .5  M = 60  N = 30   SIGMA =  1.488920697912116
!!
!!      strut.mat:// Structure problem, Forsythe, Malcolm and Moler, p. 62
!!      strut.mat:s =  sqrt(2)/2;
!!      strut.mat:A = <
!!      strut.mat:-s  .  .  1  s   .  .  .  .  .  .  .  .  .  .  .  .
!!      strut.mat:-s  . -1  . -s   .  .  .  .  .  .  .  .  .  .  .  .
!!      strut.mat: . -1  .  .  .   1  .  .  .  .  .  .  .  .  .  .  .
!!      strut.mat: .  .  1  .  .   .  .  .  .  .  .  .  .  .  .  .  .
!!      strut.mat: .  .  . -1  .   .  .  1  .  .  .  .  .  .  .  .  .
!!      strut.mat: .  .  .  .  .   . -1  .  .  .  .  .  .  .  .  .  .
!!      strut.mat: .  .  .  . -s -1  .  .  s  1  .  .  .   .  .  .  .
!!      strut.mat: .  .  .  .  s   .  1  .  s  .  .  .  .  .  .  .  .
!!      strut.mat: .  .  .  .  .   .  . -1 -s  .  .  1  s  .  .  .  .
!!      strut.mat: .  .  .  .  .   .  .  . -s  . -1  . -s  .  .  .  .
!!      strut.mat: .  .  .  .  .   .  .  .  . -1  .  .  .  1  .  .  .
!!      strut.mat: .  .  .  .  .   .  .  .  .  .  1  .  .  .  .  .  .
!!      strut.mat: .  .  .  .  .   .  .  .  .  .  . -1  .  .  .  s  .
!!      strut.mat: .  .  .  .  .   .  .  .  .  .  .  .  .  . -1 -s  .
!!      strut.mat: .  .  .  .  .   .  .  .  .  .  .  . -s -1  .  .  1
!!      strut.mat: .  .  .  .  .   .  .  .  .  .  .  .  s  .  1  .  .
!!      strut.mat: .  .  .  .  .   .  .  .  .  .  .  .  .  .  . -s -1>;
!!      strut.mat:b = <
!!      strut.mat: .  .  . 10  .   .  . 15  .  .  .  .  .  .  . 10  .>';
!!
!!      test1:// -----------------------------------------------------------------
!!      test1:// start a new log file
!!      test1:sh rm -fv log.txt
!!      test1:diary('log.txt')
!!      test1:// -----------------------------------------------------------------
!!      test1:titles=<'GNP deflator'
!!      test1: 'GNP         '
!!      test1: 'Unemployment'
!!      test1: 'Armed Force '
!!      test1: 'Population  '
!!      test1: 'Year        '
!!      test1: 'Employment  '>;
!!      test1:data = ...
!!      test1:< 83.0  234.289  235.6  159.0  107.608  1947  60.323
!!      test1:  88.5  259.426  232.5  145.6  108.632  1948  61.122
!!      test1:  88.2  258.054  368.2  161.6  109.773  1949  60.171
!!      test1:  89.5  284.599  335.1  165.0  110.929  1950  61.187
!!      test1:  96.2  328.975  209.9  309.9  112.075  1951  63.221
!!      test1:  98.1  346.999  193.2  359.4  113.270  1952  63.639
!!      test1:  99.0  365.385  187.0  354.7  115.094  1953  64.989
!!      test1: 100.0  363.112  357.8  335.0  116.219  1954  63.761
!!      test1: 101.2  397.469  290.4  304.8  117.388  1955  66.019
!!      test1: 104.6  419.180  282.2  285.7  118.734  1956  67.857
!!      test1: 108.4  442.769  293.6  279.8  120.445  1957  68.169
!!      test1: 110.8  444.546  468.1  263.7  121.950  1958  66.513
!!      test1: 112.6  482.704  381.3  255.2  123.366  1959  68.655
!!      test1: 114.2  502.601  393.1  251.4  125.368  1960  69.564
!!      test1: 115.7  518.173  480.6  257.2  127.852  1961  69.331
!!      test1: 116.9  554.894  400.7  282.7  130.081  1962  70.551>;
!!      test1:short
!!      test1:X = data;
!!      test1:<n,p> = size(X)
!!      test1:mu = ones(1,n)*X/n
!!      test1:X = X - ones(n,1)*mu;  X = X/diag(sqrt(diag(X'*X)))
!!      test1:corr = X'*X
!!      test1:y = data(:,p); X = <ones(y) data(:,1:p-1)>;
!!      test1:long e
!!      test1:beta = X\y
!!      test1:expected = < ...
!!      test1:   -3.482258634594421D+03
!!      test1:    1.506187227124484D-02
!!      test1:   -3.581917929257409D-02
!!      test1:   -2.020229803816908D-02
!!      test1:   -1.033226867173703D-02
!!      test1:   -5.110410565317738D-02
!!      test1:    1.829151464612817D+00
!!      test1:>
!!      test1:disp('EXPE and BETA should be the same')
!!
!!      tryall:diary('log.txt')
!!      tryall:a=magic(8)
!!      tryall:n=3
!!      tryall:exec('avg')
!!      tryall:b=random(8,8)
!!      tryall:exec('cdiv')
!!      tryall:exec('exp')
!!      tryall:exec('four')
!!      tryall:exec('gs')
!!      tryall:exec('jacobi')
!!      tryall:// jacstep
!!      tryall:exec('kron')
!!      tryall:exec('lanczos')
!!      tryall:// lanstep
!!      tryall:exec('longley')
!!      tryall:exec('mgs')
!!      tryall:exec('net')
!!      tryall:exec('pascal')
!!      tryall:exec('pdq')
!!      tryall:// pdqstep
!!      tryall:exec('pop')
!!      tryall:exec('qr')
!!      tryall:// qrstep
!!      tryall:exec('rho')
!!      tryall:exec('rosser')
!!      tryall:// rot
!!      tryall:exec('rqi')
!!      tryall:exec('setup')
!!      tryall:exec('sigma')
!!      tryall:exec('strut.mat')
!!      tryall:exec('w5')
!!      tryall:exec('rogers.exec
!!      tryall:exec('rogers.load
!!
!!      w5:w5    = <
!!      w5:          1.          1.          0.          0.          0.
!!      w5:        -10.          1.          1.          0.          0.
!!      w5:         40.          0.          1.          1.          0.
!!      w5:       -205.          0.          0.          1.          1.
!!      w5:       1024.          0.          0.          0.         -4.
!!      w5:         >
subroutine MAT88(init,string0)

! ident_1="@(#)M_matrix::mat88(3f): initialize and/or pass commands to matrix laboratory interpreter"

character(len=*),intent(in) :: string0
integer                     :: init
integer                     :: istring0
doubleprecision             :: s,t
integer,save                :: EPS(4)=   [14,25,28,36]
integer,save                :: FLOPS(4)= [15,21,24,25]
integer,save                :: EYE(4)=   [14,34,14,36]
integer,save                :: RAND(4)=  [27,10,23,13]
!
!     CHARACTER SET
!            0       10       20       30       40       50
!
!     0      0        A        K        U   COLON  :  LESS   <
!     1      1        B        L        V   PLUS   +  GREAT  >
!     2      2        C        M        W   MINUS  -
!     3      3        D        N        X   STAR   *
!     4      4        E        O        Y   SLASH  /
!     5      5        F        P        Z   BSLASH !     6      6        G        Q  BLANK     EQUAL  =
!     7      7        H        R  LPAREN (  DOT    .
!     8      8        I        S  RPAREN )  COMMA  ,
!     9      9        J        T  SEMI   ;  QUOTE  '
!----------------------------------------------------------------
      STRINGQ=STRING0
      ISTRING0=LEN(STRING0)
      ISTRINGQ=len_trim(STRING0(1:ISTRING0))
      IF(ISTRINGQ.LE.0.AND.INIT.EQ.2)THEN
         STRINGQ='quit'
         ISTRINGQ=4
      ENDIF
      INITQ=INIT
!----------------------------------------------------------------
      IF (INIT .NE. 0 .and. INIT .ne. -1) GOTO 90     ! already initialized
      RTE = 5                                         ! unit number for terminal input
      CALL mat_files(RTE,BUF)
      RIO = RTE                                       ! current file to read commands from
      WTE = 6                                         ! unit number for terminal  output
      CALL mat_files(WTE,BUF)

      IF (INIT .GE. 0) then                           ! initializing verbose
         call journal('  < M A T 8 8 >')
         call journal(' based on MATLAB Version of 05/25/82')
      endif

      HIO = 11                                        ! UNIT NUMBER FOR HELP FILE
      CALL mat_files(HIO,BUF)                         ! open HELP FILE
      RAN(1) = 0                                      ! RANDOM NUMBER SEED
      LCT(2) = 25                                     ! INITIAL LINE LIMIT

      alflq = IALF
      CASE = 0
!     CASE = 1 for file names in lower case
!------------------------------------------------------------------------
      call mat_str2buf(ch_a,alfa,alflq) ! convert string to hollerith
      call mat_str2buf(ch_b,alfb,alflq) ! convert string to hollerith
!------------------------------------------------------------------------
!
      VSIZE = BIGMEM
      LSIZE = 48
      PSIZE = 32
      BOT = LSIZE-3
      CALL mat_wset(5,0.0D0,0.0D0,STKR(VSIZE-4),STKI(VSIZE-4),1)
      CALL mat_putid(IDSTK(1,LSIZE-3),EPS)
      LSTK(LSIZE-3) = VSIZE-4
      MSTK(LSIZE-3) = 1
      NSTK(LSIZE-3) = 1

      S = 1.0D0
   30 continue
      S = S/2.0D0
      T = 1.0D0 + S
      IF (T .GT. 1.0D0) GOTO 30
      STKR(VSIZE-4) = 2.0D0*S

      CALL mat_putid(IDSTK(1,LSIZE-2),FLOPS)
      LSTK(LSIZE-2) = VSIZE-3
      MSTK(LSIZE-2) = 1
      NSTK(LSIZE-2) = 2
      CALL mat_putid(IDSTK(1,LSIZE-1), EYE)
      LSTK(LSIZE-1) = VSIZE-1
      MSTK(LSIZE-1) = -1
      NSTK(LSIZE-1) = -1
      STKR(VSIZE-1) = 1.0D0
      CALL mat_putid(IDSTK(1,LSIZE), RAND)
      LSTK(LSIZE) = VSIZE
      MSTK(LSIZE) = 1
      NSTK(LSIZE) = 1
      FMT = 1
      FLP(1) = 0
      FLP(2) = 0
      DDT = 0
      RAN(2) = 0
      PTZ = 0
      PT = PTZ
      ERR = 0
      IF (INIT .EQ. -1) RETURN
!
90    continue
      INFINITE : do
         CALL mat_parse(INIT)
         select case(fun)
         case(1) ; call mat_matfn1()
         case(2) ; call mat_matfn2()
         case(3) ; call mat_matfn3()
         case(4) ; call mat_matfn4()
         case(5) ; call mat_matfn5()
         case(6) ; call mat_matfn6()
         case(21); call mat_matfn1()
         case(99); exit INFINITE
         case default
         end select
      enddo INFINITE

end subroutine MAT88
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_err(n)
integer,intent(in) :: n
integer,parameter         :: linelen=255
character(len=linelen)    :: blh
character(len=linelen)    :: msg
character(len=linelen+20) :: mline
integer                   :: iend
integer                   :: k
integer                   :: kk
integer                   :: i
integer                   :: i9400
integer                   :: lb
integer                   :: lt

   blh(1:linelen)='        '
!
   k = lpt(2) - lpt(1)
   if (k .lt. 1) k = 1
   select case(n)
    case(1); msg='IMPROPER MULTIPLE ASSIGNMENT'
    case(2); msg='IMPROPER FACTOR'
    case(3); msg='EXPECT RIGHT PARENTHESIS'
!.......................................................................
    case(4);
      DO I9400 = 1, 4
         KK = IDS(I9400,PT+1)
         BUF(I9400) = ALFA(KK+1)
      enddo
      call mat_buf2str(msg,buf,4)
      msg='UNDEFINED VARIABLE: '//msg(1:4)
!.......................................................................
    case(5); msg='COLUMN LENGTHS DO NOT MATCH'
    case(6); msg='ROW LENGTHS DO NOT MATCH'
    case(7); msg='TEXT TOO LONG'
    case(8); msg='Incompatible for ADDITION'
    case(9); msg='Incompatible for SUBTRACTION'
    case(10); msg='Incompatible for MULTIPLICATION'
    case(11); msg='Incompatible for RIGHT DIVISION'
    case(12); msg='Incompatible for LEFT DIVISION'
    case(13); msg='Improper assignment to PERMANENT VARIABLE'
    case(14); msg='EYE-dentity undefined by CONTEXT'
    case(15); msg='IMPROPER ASSIGNMENT TO SUBMATRIX'
    case(16); msg='IMPROPER COMMAND'
!.......................................................................
    case(17)
      LB = VSIZE - LSTK(BOT) + 1
      LT = ERR + LSTK(BOT)
      call journal(' TOO MUCH MEMORY REQUIRED')
      WRITE(MLINE,'(1X,I7,'' VARIABLES,'',I7,'' TEMPORARIES,'',I7,'' AVAILABLE.'')') LB,LT,VSIZE
      call journal(MLINE)
      GOTO 999
!.......................................................................
    case(18); msg='TOO MANY NAMES'
    case(19); msg='MATRIX IS SINGULAR TO WORKING PRECISION'
    case(20); msg='MATRIX MUST BE SQUARE'
    case(21); msg='SUBSCRIPT OUT OF RANGE'
!.......................................................................
    case(22)
      WRITE(MLINE,122) (RSTK(I),I=1,PT)
122   FORMAT(1X,'RECURSION DIFFICULTIES',10I4)
      call journal(mline)
      GOTO 999
!.......................................................................
    case(23); msg='ONLY 1, 2 OR INF NORM OF MATRIX'
    case(24); msg='NO CONVERGENCE'
    case(25); msg='CAN NOT USE FUNCTION NAME AS VARIABLE'
    case(26); msg='TOO COMPLICATED (STACK OVERFLOW)'
    case(27); msg='DIVISION BY ZERO IS A NO-NO'
    case(28); msg='EMPTY MACRO'
    case(29); msg='NOT POSITIVE DEFINITE'
    case(30); msg='IMPROPER EXPONENT'
    case(31); msg='IMPROPER STRING'
    case(32); msg='SINGULARITY OF LOG OR ATAN'
    case(33); msg='TOO MANY COLONS'
    case(34); msg='IMPROPER FOR CLAUSE'
    case(35); msg='IMPROPER WHILE OR IF CLAUSE'
    case(36); msg='ARGUMENT OUT OF RANGE'
    case(37); msg='IMPROPER MACRO'
    case(38); msg='IMPROPER FILE NAME'
    case(39); msg='INCORRECT NUMBER OF ARGUMENTS'
    case(40); msg='EXPECT STATEMENT TERMINATOR'
!.......................................................................
    case default
    call journal('sc','*mat_err* unknown error code =',n)
    goto 999
!.......................................................................
   end select


   iend=max(1,len_trim(msg))

   if(k+iend.lt.len(mline))then
      write(mline,'(1x,a,''/^--ERROR:'',a)') blh(1:k),msg(1:iend)
      call journal(mline)
   else
      WRITE(mline,'(1x,a,''/^--ERROR:'')') blh(1:k)
      call journal(mline)
      call journal(msg)
   endif
   goto 999
!.......................................................................
999 continue
   err = n
end subroutine mat_err
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_files(lunit,iname)
integer :: lunit !  logical unit number
   ! if LUNIT is zero, return
   ! if LUNIT = standard input, return
   ! if LUNIT = standard output, return
   ! if LUNIT is 11, open the help file
   ! if LUNIT is -11 and HIO .ne. 0 , rewind the help file
   ! if LUNIT is positive, open the unit to file name INAME
   ! if LUNIT is negative, close the unit number
!     INAME = FILE NAME, 1 character per word
   ! how to know length of iname?
integer                      :: iname(256)
character(len=1024)          :: name
character(len=1024)          :: temp1
integer                      :: ios
integer                      :: itemp
integer                      :: itemp1
!  Amiga dependent stuff squeezes the NAME from one CHAR per word to one per byte
!.......................................................................
   if (ddt .eq. 1) then
      call journal('sc','*MLFILES* LUNIT=', LUNIT)
      name(1:10)='*MLFILES* INAME='
      call mat_buf2str(name(11:),iname,256)
      call journal(name)
   endif
!.......................................................................
   fe=0
   select case(lunit)
    case(0) ! error catcher
    case(5) ! if unit is standard input return
    case(6) ! if unit is standard output return
    case(8) ! diary file
       call mat_buf2str(name,iname,256)
       call journal('O',trim(name)) ! open up trail file
    case(11)                                                         ! HELP FILE
      temp1=' '
      call get_environment_variable('ML88_HELP',temp1)              ! get default name for helpfile to override built-in file
      if(temp1(:).eq.' ')then                                       ! create and open scratch help file
         !! temp1='MAT88_help.txt'
         open(11,status='scratch',iostat=ios)
         call mat_make_help(lunit)
         rewind(11,iostat=ios)
      else                                                          ! try to use user-specified help file instead of built-in file
         itemp1=max(1,len_trim(temp1))
         open(11,file=temp1(:itemp1),status='old',iostat=ios)       ! open help file
         if(ios.ne.0)then                                           ! HELP FILE NOT FOUND
            call journal('HELP IS NOT AVAILABLE ON FILE ...')
            call journal(temp1(1:itemp1))
            HIO = 0
         else
            !call journal('HELP is available')
         endif
      endif
    case(:-1)
      if( lunit .eq. -11 .and. hio .ne. 0)then                       ! SPECIAL CASE FOR HELP FILE
         rewind (11,iostat=ios)
      elseif(lunit.eq.-8)then
         call journal('O','')                                        ! close trail file
      else                                                           ! if LUNIT is negative, close the unit
         flush(unit=-lunit,iostat=ios)
         close(unit=-lunit,iostat=ios)
      endif
    case default                                                     !  ALL OTHER FILES
      call mat_buf2str(name,iname,256)
      itemp=len_trim(name)
      !call journal('filename='//name(:itemp)
      open(unit=lunit,file=name(:itemp),status='unknown',iostat=ios) ! open a file
      if(ios.ne.0)then                                               ! general file open failure
         call journal('*mat_files* OPEN FILE FAILED')
         call journal(name(1:len_trim(name)))
         fe=1                                                        ! set the I/O to terminal I/O
         rio=rte                                                     ! set current file to read input lines from/to RTE
      endif
   end select
end subroutine mat_files
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getsym()

! ident_2="@(#)m_matrix::mat_getsym(3fp): get a symbol"

character(len=80) ::  mline
integer,parameter :: a2=52
integer,parameter :: z2=77

doubleprecision   :: syv
doubleprecision   :: s

integer,save :: BLANK=36
integer,save :: Z=35
integer,save :: DOT=47

integer,parameter  :: d=13, d_up=55
integer,parameter  :: e=14, e_up=56

integer,save :: PLUS=41
integer,save :: MINUS=42
integer,save :: NAME=1
integer,save :: NUM=0
integer,save :: STAR=43
integer,save :: SLASH=44
integer,save :: BSLASH=45

integer      :: SIGN
integer      :: CHCNT
integer      :: SS
integer      :: i
!.......................................................................
   INFINITE : do
      if (chra .ne. blank) exit INFINITE
      call mat_getch() ! get next character
   enddo INFINITE
!.......................................................................
   lpt(2) = lpt(3)
   lpt(3) = lpt(4)
   if (chra .le. 9) goto 50                                 ! numeric character (0-9)
   if (chra .le. z.or.(chra.ge.a2.and.chra.le.z2)) goto 30  ! alphameric (A-Z OR a-z)
!.......................................................................
!     special character
   ss = sym
   sym = chra
   call mat_getch() ! get next character
   if (sym .ne. dot) goto 90
!
!     is dot part of number or operator
   syv = 0.0d0
   if (chra .le. 9) goto 55  ! a number character
   if (chra.eq.star.or.chra.eq.slash.or.chra.eq.bslash) goto 90
   if (ss.eq.star .or. ss.eq.slash .or. ss.eq.bslash) goto 90
   goto 55
!.......................................................................
!     name
30 continue
   sym = name
   syn(1) = chra
   chcnt = 1
40 continue
   call mat_getch() ! get next character
   chcnt = chcnt+1
   if (chra .ge. a2.and.chra.le.z2) goto  44! alternate case letter
   if (chra .gt. z) goto 45  ! a control character not alphanumeric and not special like eol
44 continue
   if (chcnt .le. 4) syn(chcnt) = chra
   goto 40
45 continue
   if (chcnt .gt. 4) goto 47
   do i = chcnt, 4
      syn(i) = blank
   enddo
47 continue
   goto 90
!.......................................................................
!     number
50 continue
   call mat_getval(syv)
   if (chra .ne. dot) goto 60
   call mat_getch() ! get next character
55 continue
   chcnt = lpt(4)
   call mat_getval(s)
   chcnt = lpt(4) - chcnt
   if (chra .eq. eol) chcnt = chcnt+1
   syv = syv + s/10.0d0**chcnt
60 continue
   if (chra.ne.d .and. chra.ne.e .and. chra.ne.d_up .and. chra.ne.e_up ) goto 70
   call mat_getch() ! get next character
   sign = chra
   if (sign.eq.minus .or. sign.eq.plus) call mat_getch() ! get next character
   call mat_getval(s)
   if (sign .ne. minus) syv = syv*10.0d0**s
   if (sign .eq. minus) syv = syv/10.0d0**s
70 continue
   stki(vsize) = mat_flop(syv)
   sym = num
!
90 continue
   if (chra .ne. blank) goto 99
   call mat_getch() ! get next character
   goto 90
99 continue
   if (ddt .ne. 1) return
   if (sym.gt.name .and. sym.lt.alflq) then
      call journal(char(alfa(sym+1)))
   endif
   if (sym .ge. alflq) call journal('eol')
   if (sym .eq. name) call mat_prntid(syn,1)
   if (sym .eq. num) then
      write(mline,'(1x,g8.2)') syv
      call journal(mline)
   endif
end subroutine mat_getsym
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_str2buf(string,buf,lrecl)

! ident_3="@(#)M_matrix::mat_str2buf(3fp) :: convert string to hollerith"

! g95 compiler does not support Hollerith, this is a KLUDGE to give time to think about it

character(len=*),intent(in) ::  string
integer,intent(in)          :: lrecl
integer,intent(out)         :: buf(lrecl)
integer                     :: i10

   do i10=1,lrecl
      buf(i10)=ichar(string(i10:i10))+538976304-48
   enddo

end subroutine mat_str2buf
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_buf2str(string,buf,lrecl)

! ident_4="@(#)M_matrix::mat_buf2string(3fp) :: convert hollerith to string"

integer,intent(in)           :: lrecl
integer,intent(in)           :: buf(lrecl)
character(len=*)             :: string
integer                      :: i10
   string(:)=' '
   do i10=1,lrecl
      if(buf(i10).eq.0)exit
      string(i10:i10)=char(buf(i10)-538976304+48)
   enddo
end subroutine mat_buf2str
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_hilber(a,lda,n)

! ident_5="@(#)M_matrix::ml_hilbr(3fp): generate inverse hilbert matrix"

integer         :: lda
integer         :: n
doubleprecision :: a(lda,n)
doubleprecision :: p
doubleprecision :: r
integer         :: i
integer         :: j
integer         :: ip1

   p = dble(n)

   do i = 1, n
      if (i.ne.1) p = (dble(n-i+1)*p*dble(n+i-1))/dble(i-1)**2
      r = p*p
      a(i,i) = r/dble(2*i-1)
      if (i.eq.n) cycle
      ip1 = i+1
      do j = ip1, n
         r = (-1)*(dble(n-j+1)*r*(n+j-1))/dble(j-1)**2
         a(i,j) = r/dble(i+j-1)
         a(j,i) = a(i,j)
      enddo
   enddo

end subroutine mat_hilber
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn6()
!
! ident_6="@(#)M_matrix::mat_matfn6(3f):evaluate utility functions"
!
integer :: i
integer :: ia
integer :: ib
integer :: j
integer :: ja
integer :: jb
integer :: k
integer :: l
integer :: la
integer :: lb
integer :: ld
integer :: lj
integer :: ll
integer :: ls
integer :: m
integer :: ma
integer :: mn
integer :: n
integer :: na
integer :: nn

integer,save      :: semi=39
integer,save      :: unifor(4) =[30,23,18,15]
integer,save      :: normal(4) =[23,24,27,22]
integer,save      :: seed(4)   =[28,14,14,13]
integer           :: id(4)
doubleprecision   :: eps0,eps,s,sr,si,t
character(len=80) :: mline
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN6* ',FIN)
!     FUNCTIONS/FIN
!     MAGI DIAG SUM  PROD USER EYE  RAND ONES CHOP SIZE KRON  TRIL TRIU
!       1    2    3    4    5    6    7    8    9   10  11-13  14   15
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      FUN6: select case(fin)
!===================================================================================================================================
!  COMMAND::KRONECKER PRODUCT
   case(11,12,13)
      IF (RHS .NE. 2) CALL mat_err(39)
      IF (ERR .GT. 0) RETURN
      TOP = TOP - 1
      L = LSTK(TOP)
      MA = MSTK(TOP)
      NA = NSTK(TOP)
      LA = L + MAX0(M*N*MA*NA,M*N+MA*NA)
      LB = LA + MA*NA
      ERR = LB + M*N - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
!     MOVE A AND B ABOVE RESULT
      CALL mat_wcopy(MA*NA+M*N,STKR(L),STKI(L),1,STKR(LA),STKI(LA),1)
      DO JA = 1, NA
        DO J = 1, N
          LJ = LB + (J-1)*M
          DO IA = 1, MA
!           GET J-TH COLUMN OF B
            CALL mat_wcopy(M,STKR(LJ),STKI(LJ),1,STKR(L),STKI(L),1)
!           ADDRESS OF A(IA,JA)
            LS = LA + IA-1 + (JA-1)*MA
            DO I = 1, M
!             A(IA,JA) OP B(I,J)
              IF (FIN .EQ. 11) CALL mat_wmul(STKR(LS),STKI(LS),STKR(L),STKI(L),STKR(L),STKI(L))
              IF (FIN .EQ. 12) CALL mat_wdiv(STKR(LS),STKI(LS),STKR(L),STKI(L),STKR(L),STKI(L))
              IF (FIN .EQ. 13) CALL mat_wdiv(STKR(L),STKI(L),STKR(LS),STKI(LS),STKR(L),STKI(L))
              IF (ERR .GT. 0) RETURN
              L = L + 1
            ENDDO
          enddo
        enddo
      enddo
      MSTK(TOP) = M*MA
      NSTK(TOP) = N*NA
!===================================================================================================================================
      case(9) ! COMMAND::CHOP
      EPS0 = 1.0D0
   61 continue
      EPS0 = EPS0/2.0D0
      T = mat_flop(1.0D0 + EPS0)
      IF (T .GT. 1.0D0) GOTO 61
      EPS0 = 2.0D0*EPS0
      FLP(2) = IDINT(STKR(L))
      IF (SYM .NE. SEMI) then
         WRITE(mline,'(''CHOP '',I2,'' PLACES.'')') FLP(2)
         call journal(mline)
      endif
      EPS = 1.0D0
   63 continue
      EPS = EPS/2.0D0
      T = mat_flop(1.0D0 + EPS)
      IF (T .GT. 1.0D0) GOTO 63
      EPS = 2.0D0*EPS
      T = STKR(VSIZE-4)
      IF (T.LT.EPS .OR. T.EQ.EPS0) STKR(VSIZE-4) = EPS
      MSTK(TOP) = 0
!===================================================================================================================================
      case(3) ! COMMAND::SUM
      SR = 0.0D0
      SI = 0.0D0
      MN = M*N
      DO I = 1, MN
         LS = L+I-1
         SR = mat_flop(SR+STKR(LS))
         SI = mat_flop(SI+STKI(LS))
      enddo
      STKR(L) = SR
      STKI(L) = SI
      MSTK(TOP) = 1
      NSTK(TOP) = 1
!===================================================================================================================================
      case(4) ! COMMAND::PROD
      SR = 1.0D0
      SI = 0.0D0
      MN = M*N
      DO I = 1, MN
         LS = L+I-1
         CALL mat_wmul(STKR(LS),STKI(LS),SR,SI,SR,SI)
      enddo
      STKR(L) = SR
      STKI(L) = SI
      MSTK(TOP) = 1
      NSTK(TOP) = 1
!===================================================================================================================================
      case(5) ! COMMAND::USER
      S = 0.0D0
      T = 0.0D0
      IF (RHS .LT. 2) GOTO 72
      IF (RHS .LT. 3) GOTO 71
      T = STKR(L)
      TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
   71 continue
      S = STKR(L)
      TOP = TOP-1
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
   72 continue
      CALL mat88_user(STKR(L),M,N,S,T)
      CALL mat_rset(M*N,0.0D0,STKI(L),1)
      MSTK(TOP) = M
      NSTK(TOP) = N
!===================================================================================================================================
      case(1) ! COMMAND::MAGIC
      N = MAX0(IDINT(STKR(L)),0)
      IF (N .EQ. 2) N = 0
      IF (N .GT. 0) CALL mat_magic(STKR(L),N,N)
      CALL mat_rset(N*N,0.0D0,STKI(L),1)
      MSTK(TOP) = N
      NSTK(TOP) = N
!===================================================================================================================================
      case(10) ! COMMAND::SIZE
      STKR(L) = M
      STKR(L+1) = N
      STKI(L) = 0.0D0
      STKI(L+1) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 2
      IF (LHS .EQ. 1) exit FUN6
      NSTK(TOP) = 1
      TOP = TOP + 1
      LSTK(TOP) = L+1
      MSTK(TOP) = 1
      NSTK(TOP) = 1
!===================================================================================================================================
      case(2,14,15) ! COMMAND::DIAG=2 ! COMMAND::TRIL=14 ! COMMAND::TRIU=15
      K = 0
      IF (RHS .NE. 2) GOTO 81
         K = IDINT(STKR(L))
         TOP = TOP-1
         L = LSTK(TOP)
         M = MSTK(TOP)
         N = NSTK(TOP)
   81 continue
      IF (FIN .GE. 14) GOTO 85
      IF (M .EQ. 1 .OR. N .EQ. 1) GOTO 83
      IF (K.GE.0) MN=MIN0(M,N-K)
      IF (K.LT.0) MN=MIN0(M+K,N)
      MSTK(TOP) = MAX0(MN,0)
      NSTK(TOP) = 1
      IF (MN .LE. 0) exit FUN6
      DO I = 1, MN
         IF (K.GE.0) LS = L+(I-1)+(I+K-1)*M
         IF (K.LT.0) LS = L+(I-K-1)+(I-1)*M
         LL = L+I-1
         STKR(LL) = STKR(LS)
         STKI(LL) = STKI(LS)
      enddo
      exit FUN6
!-----------------------------------------------------------------------------------------------------------------------------------
   83 continue
      N = MAX0(M,N)+IABS(K)
      ERR = L+N*N - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      MSTK(TOP) = N
      NSTK(TOP) = N
      DO JB = 1, N
         DO IB = 1, N
            J = N+1-JB
            I = N+1-IB
            SR = 0.0D0
            SI = 0.0D0
            IF (K.GE.0) LS = L+I-1
            IF (K.LT.0) LS = L+J-1
            LL = L+I-1+(J-1)*N
            IF (J-I .EQ. K) SR = STKR(LS)
            IF (J-I .EQ. K) SI = STKI(LS)
            STKR(LL) = SR
            STKI(LL) = SI
         enddo
      enddo
      exit FUN6
!-----------------------------------------------------------------------------------------------------------------------------------
! COMMAND::TRIL, COMMAND::TRIU
   85 continue
      DO J = 1, N
         LD = L + J - K - 1 + (J-1)*M
         IF (FIN .EQ. 14) LL = J - K - 1
         IF (FIN .EQ. 14) LS = LD - LL
         IF (FIN .EQ. 15) LL = M - J + K
         IF (FIN .EQ. 15) LS = LD + 1
         IF (LL .GT. 0) CALL mat_wset(LL,0.0D0,0.0D0,STKR(LS),STKI(LS),1)
      enddo
!===================================================================================================================================
      case(6,7,8) ! COMMAND::EYE, COMMAND::RAND, COMMAND::ONES
      IF (M.GT.1 .OR. RHS.EQ.0) GOTO 94
      IF (RHS .NE. 2) GOTO 91
        NN = IDINT(STKR(L))
        TOP = TOP-1
        L = LSTK(TOP)
        N = NSTK(TOP)
   91 continue
      IF (FIN.NE.7 .OR. N.LT.4) GOTO 93
      DO I = 1, 4
        LS = L+I-1
        ID(I) = IDINT(STKR(LS))
      enddo
      IF (mat_eqid(ID,UNIFOR).OR.mat_eqid(ID,NORMAL)) GOTO 97
      IF (mat_eqid(ID,SEED)) GOTO 98
   93 continue
      IF (N .GT. 1) GOTO 94
      M = MAX0(IDINT(STKR(L)),0)
      IF (RHS .EQ. 2) N = MAX0(NN,0)
      IF (RHS .NE. 2) N = M
      ERR = L+M*N - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      MSTK(TOP) = M
      NSTK(TOP) = N
      IF (M*N .EQ. 0) exit FUN6
   94 continue
      DO J = 1, N
         DO I = 1, M
           LL = L+I-1+(J-1)*M
           STKR(LL) = 0.0D0
           STKI(LL) = 0.0D0
           IF(I.EQ.J .OR. FIN.EQ.8) STKR(LL) = 1.0D0
           IF(FIN.EQ.7 .AND. RAN(2).EQ.0)STKR(LL)=mat_flop(mat_urand(RAN(1)))
           IF(FIN.NE.7 .OR. RAN(2).EQ.0) cycle
      95      continue
              SR = 2.0D0*mat_urand(RAN(1))-1.0D0
              SI = 2.0D0*mat_urand(RAN(1))-1.0D0
              T = SR*SR + SI*SI
              IF (T .GT. 1.0D0) GOTO 95
           STKR(LL) = mat_flop(SR*DSQRT((-(2.0D0*DLOG(T)))/T))
         enddo
      enddo
      exit FUN6
!-----------------------------------------------------------------------------------------------------------------------------------
!     SWITCH UNIFORM AND NORMAL
   97 continue
      RAN(2) = ID(1) - UNIFOR(1)
      MSTK(TOP) = 0
      exit FUN6
!-----------------------------------------------------------------------------------------------------------------------------------
   98 continue ! COMMAND::SEED
      IF (RHS .EQ. 2) RAN(1) = NN
      STKR(L) = RAN(1)
      MSTK(TOP) = 1
      IF (RHS .EQ. 2) MSTK(TOP) = 0
      NSTK(TOP) = 1
      exit FUN6
!===================================================================================================================================
      end select FUN6
end subroutine mat_matfn6
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_funs(id)

! ident_7="@(#)M_matrix::ml_funcs(3fp):scan function list"

integer,intent(in) :: id(4)
integer,parameter  :: funl=58                      ! number of functions
integer,save       :: funn(4,funl)                 ! names of functions encoded
integer,save       :: funp(funl)                   ! calling codes corresponding to the function names
integer            :: k
!
!    function names per letter position
   data funn/                                              &
   &  10,11,28,36, 10,29,10,23, 11,10,28,14, 12,17,10,27,  & !    1   ABS   ATAN  BASE  CHAR
   &  12,17,24,21, 12,17,24,25, 12,24,23,13, 12,24,23,19,  & !    2   CHOL  CHOP  COND  CONJ
   &  12,24,28,36, 13,14,29,36, 13,18,10,16, 13,18,10,27,  & !    3   COS   DET   DIAG  DIAR
   &  13,18,28,25, 14,18,16,36, 14,25,28,36, 14,33,14,12,  & !    4   DISP  EIG   EPS   EXEC
   &  14,33,25,36, 14,34,14,36, 15,21,24,25, 17,14,28,28,  & !    5   EXP   EYE   FLOP  HESS
   &  17,18,21,11, 18,22,10,16, 18,23,31,36, 20,27,24,23,  & !    6   HILB  IMAG  INV   KRON
   &  21,18,23,14, 21,24,10,13, 21,24,16,36, 21,30,36,36,  & !    7   LINE  LOAD  LOG   LU
   &  22,10,16,18, 23,24,27,22, 24,23,14,28, 24,27,29,17,  & !    8   MAGIC NORM  ONES  ORTH
   &  25,18,23,31, 25,21,24,29, 25,24,21,34, 25,27,18,23,  & !    9   PINV  PLOT  POLY  PRINT
   &  25,27,24,13, 26,27,36,36, 27,10,23,13, 27,10,23,20,  & !    10  PROD  QR    RAND  RANK
   &  27,10,29,36, 27,12,24,23, 27,14,10,21, 27,24,24,29,  & !    11  RAT   RCOND REAL  ROOT
   &  27,24,30,23, 27,27,14,15, 28,10,31,14, 28,12,17,30,  & !    12  ROUND RREF  SAVE  SCHUR
   &  28,18,23,36, 28,18,35,14, 28,26,27,29, 28,30,22,36,  & !    13  SIN   SIZE  SQRT  SUM
   &  28,31,13,36, 29,27,18,21, 29,27,18,30, 30,28,14,27,  & !    14  SVD   TRIL  TRIU  USER
   &  13,14,11,30, 13,24,12,36 /                             !    15  DEBUG DOC
!
!    determine what to call for each name.
!     o first digit indicates which routine to call (SUBROUTINE ML_MATFN[1-6])
!     o remaining digits indicate nth number in computed goto in called routine
!
   data funp/                                              &
   &  221,203,507,509, 106,609,303,225, 202,102,602,505,   &
   &  506,211,000,501, 204,606,000,213, 105,224,101,611,   &
   &  508,503,206,104, 601,304,608,402, 302,510,214,504,   &
   &  604,401,607,305, 511,103,223,215, 222,107,502,212,   &
   &  201,610,205,603, 301,614,615,605, 512,513 /
!
!  print function names and return
   if (id(1).eq.0) then
      call mat_prntid(funn,funl)
      return
   endif
!
!  find FUNP value for given function name
   do k = 1, funl
      if (mat_eqid(id,funn(1,k))) goto 20
   enddo
!  function name was not found
   fin = 0
   return
!  found name so great FIN and FUN value from corresponding FUNP code

20 continue
   fin = mod(funp(k),100) ! which routine to call (SUBROUTINE ML_MATFN[1-6])
   fun = funp(k)/100      ! which case to select in called procedure

   if (rhs.eq.0 .and. funp(k).eq.606) fin = 0
   if (rhs.eq.0 .and. funp(k).eq.607) fin = 0
end subroutine mat_funs
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_putid(x,y)

! ident_8="@(#)M_matrix::mat_putid(3fp): store a name"

integer,intent(out) :: x(4)
integer,intent(in)  :: y(4)
integer             :: i
      do i = 1, 4
         x(i) = y(i)
      enddo
end subroutine mat_putid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getval(s)

! ident_9="@(#)M_matrix::mat_getval(3fp): form numerical value"

doubleprecision,intent(out) :: s
      s = 0.0d0
      INFINITE: do
         if (chra .gt. 9) exit INFINITE
         s = 10.0d0*s + dble(chra)
         call mat_getch() ! get next character
      enddo INFINITE
end subroutine mat_getval
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getch()

! ident_10="@(#)M_matrix::mat_getch(3f): get next character from input line"

integer :: l

   l = lpt(4)
   chra = lin(l)
   if (chra .ne. eol) lpt(4) = l + 1

end subroutine mat_getch
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotur(n,xr,xi,incx,yr,yi,incy)
integer,intent(in)         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

doubleprecision :: s
integer         :: ix
integer         :: iy
integer         :: i

   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = mat_flop(s + xr(ix)*yr(iy) - xi(ix)*yi(iy))
         ix = ix + incx
         iy = iy + incy
      enddo
   endif

   mat_wdotur = s

end function mat_wdotur
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_appnum(rval,string,ilen,ierr)
use M_strings, only: value_to_string

! ident_11="@(#)M_matrix::mat_appnum(3fp): subroutine returns a string given a prefix string and a real value"

!     Input string should have at least 20 blank characters at end
!     03/16/87 J. S. Urban
!
!-------------------------------------------------------------------------------
      real,intent(in)                :: rval   ! input value to convert to characters and append to STRING
      character(len=*)               :: string ! string to append string value of RVAL to
      integer,intent(out)            :: ilen   ! new length of STRING on output
      integer,intent(out)            :: ierr   ! flag to indicate if error occurred
!-------------------------------------------------------------------------------
      intrinsic                      :: len_trim
!-------------------------------------------------------------------------------
      character(len=20)              :: chars  ! scratch string to store string representation of RVAL in
      integer                        :: ilen2  ! length of string created by converting RVAL to a string
!-------------------------------------------------------------------------------
      ierr=0                                   ! initialize error flag to indicate no errors
      chars=' '                                ! initialize scratch string to all blanks
      ilen=len_trim(string(:len(string)))      ! find last non-blank character in initial input string

      call value_to_string(rval,chars,ilen2,ierr)         ! convert RVAL to a string in CHARS
      if(ilen+ilen2.gt.len(string))then
         call journal('sc','*mat_appnum* error: input string variable too short to store output string')
         call journal('sc','*mat_appnum* '//string,rval)
         ierr=-1
      else
         string=string(:ilen)//chars(:ilen2)   ! append CHARS to STRING
         ilen=ilen+ilen2                       ! calculate length of new string
      endif
end subroutine mat_appnum
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wcopy(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

integer :: ix
integer :: iy
integer :: i
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         yr(iy) = xr(ix)
         yi(iy) = xi(ix)
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
end subroutine mat_wcopy
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wdiv(ar,ai,br,bi,cr,ci)

! ident_12="@(#)M_matrix::mat_wdiv(3fp): c = a/b"

doubleprecision :: ar
doubleprecision :: ai
doubleprecision :: br
doubleprecision :: bi
doubleprecision :: cr
doubleprecision :: ci

doubleprecision :: s
doubleprecision :: d
doubleprecision :: ars
doubleprecision :: ais
doubleprecision :: brs
doubleprecision :: bis

   s = dabs(br) + dabs(bi)
   if (s .eq. 0.0d0) call mat_err(27)
   if (s .eq. 0.0d0) return
   ars = ar/s
   ais = ai/s
   brs = br/s
   bis = bi/s
   d = brs**2 + bis**2
   cr = mat_flop((ars*brs + ais*bis)/d)
   ci = (ais*brs - ars*bis)/d
   if (ci .ne. 0.0d0) ci = mat_flop(ci)
end subroutine mat_wdiv
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wset(n,xr,xi,yr,yi,incy)
integer         :: n
doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

integer         :: iy
integer         :: i
   iy = 1
   if (n .le. 0 ) return
   do i = 1,n
      yr(iy) = xr
      yi(iy) = xi
      iy = iy + incy
   enddo
end subroutine mat_wset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_base(x,b,eps,s,n)
doubleprecision :: x
doubleprecision :: b
doubleprecision :: eps
doubleprecision :: s(*)
integer         :: n

doubleprecision :: t
!
! ident_13="@(#)M_matrix::mat_base(3fp): store base b representation of x in s(1:n)"
!
integer,save :: plus=41
integer,save :: minus=42
integer,save :: dot=47
integer,save :: zero=0
integer,save :: comma=48

integer      :: l
integer      :: j
integer      :: k
integer      :: m

   l = 1
   if (x .ge. 0.0d0) s(l) = plus
   if (x .lt. 0.0d0) s(l) = minus
   s(l+1) = zero
   s(l+2) = dot
   x = dabs(x)
   if (x .ne. 0.0d0) then
      k = dlog(x)/dlog(b)
   else
      k = 0
   endif
   if (x .gt. 1.0d0) k = k + 1
   x = x/b**k
   if (b*x .ge. b) k = k + 1
   if (b*x .ge. b) x = x/b
   if (eps .ne. 0.0d0)then
      m = (-1)*dlog(eps)/dlog(b) + 4
   else
      m = 54
   endif
   do l = 4, m
      x = b*x
      j = idint(x)
      s(l) = dble(j)
      x = x - s(l)
   enddo
   s(m+1) = comma
   if (k .ge. 0) s(m+2) = plus
   if (k .lt. 0) s(m+2) = minus
   t = dabs(dble(k))
   n = m + 3
   if (t .ge. b) n = n + idint(dlog(t)/dlog(b))
   l = n
20 continue
   j = idint(dmod(t,b))
   s(l) = dble(j)
   l = l - 1
   t = t/b
   if (l .ge. m+3) goto 20
end subroutine mat_base
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wswap(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

doubleprecision :: t

integer         :: i
integer         :: ix
integer         :: iy

   if (n .le. 0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx + 1
   if (incy.lt.0) iy = (-n+1)*incy + 1
   do i = 1, n
      t = xr(ix)
      xr(ix) = yr(iy)
      yr(iy) = t
      t = xi(ix)
      xi(ix) = yi(iy)
      yi(iy) = t
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine mat_wswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_pythag(a,b)
doubleprecision :: a
doubleprecision :: b

doubleprecision :: p
doubleprecision :: q
doubleprecision :: r
doubleprecision :: s
doubleprecision :: t

   p = dmax1(dabs(a),dabs(b))
   q = dmin1(dabs(a),dabs(b))

   !------- DEBUG
   if (ddt .eq. 25) then
      call journal('sc','*mat_pythag* a) P=',real(P)) ! debug 25
      call journal('sc','*mat_pythag* a) Q=',real(Q)) ! debug 25
   endif

   if (q .ne. 0.0d0) then

      INFINITE : do
         r = (q/p)**2
         t = 4.0d0 + r
         if (t .eq. 4.0d0) exit INFINITE
         s = r/t
         p = p + 2.0d0*p*s
         q = q*s
         !------- DEBUG
         if (ddt .eq. 25) then
            call journal('sc','*mat_pythag* b) P=',real(P)) ! debug 25
            call journal('sc','*mat_pythag* b) Q=',real(Q)) ! debug 25
         endif
      enddo INFINITE

   endif

   mat_pythag = p
end function mat_pythag
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_print(ID,K)

! ident_14="@(#)M_matrix::mat_print(3fp): primary output routine"

integer           :: id(4)
integer           :: k

character(len=81) :: mline
character(len=80) :: form
character(len=1)  :: ls_char

doubleprecision   :: s
doubleprecision   :: tr
doubleprecision   :: ti
doubleprecision   :: pr(12)
doubleprecision   :: pi(12)
integer           :: sig(12)
integer           :: typ
integer           :: f
integer           :: l,m,n,mn
integer           :: ks
integer           :: i
integer           :: ios
integer           :: istep
integer           :: j
integer           :: j1
integer           :: j2
integer           :: j3
integer           :: jinc
integer           :: jm
integer           :: ls
integer,save      :: plus=41
integer,save      :: minus=42
integer,save      :: fno(11)= [11,12,21,22,23,24,31,32,33,34,-1]
integer,save      :: fnl(11)= [12, 6, 8, 4, 6, 3, 4, 2, 3, 1, 1]


! FORMAT NUMBERS AND LENGTHS
! FMT   1       2       3       4       5
!      SHORT   LONG   SHORT E  LONG E    Z
! TYP   1       2       3
!    INTEGER  REAL   COMPLEX
!.......................................................................
   IF (LCT(1) .LT. 0) GOTO 99
!.......................................................................
   L = LSTK(K)
   M = MSTK(K)
   N = NSTK(K)
   MN = M*N
   TYP = 1
   S = 0.0D0
   DO I = 1, MN
      LS = L+I-1
      TR = STKR(LS)
      TI = STKI(LS)
      S = DMAX1(S,DABS(TR),DABS(TI))
      IF (mat_round(TR) .NE. TR) TYP = MAX0(2,TYP)
      IF (TI .NE. 0.0D0) TYP = 3
   enddo
   IF (S .NE. 0.0D0) S = DLOG10(S)
   KS = IDINT(S)
   IF (-2 .LE. KS .AND. KS .LE. 1) KS = 0
   IF (KS .EQ. 2 .AND. FMT .EQ. 1 .AND. TYP .EQ. 2) KS = 0

   F=0                          ! initialize to bad value
   IF (TYP .EQ. 1 )THEN         ! if output type is integer
      IF( KS .LE. 2 )THEN
         F = 1
      ELSE
         F = 2
      ENDIF
   ENDIF
   IF (TYP .EQ. 1 .AND. KS .GT. 9) TYP = 2  !change type from integer to real

   IF (TYP .EQ. 2) F = FMT + 2   ! if type is real
   IF (TYP .EQ. 3) F = FMT + 6   ! if type is complex
   if(f.eq.0)then
      call journal('*mat_print* internal error - bad type')
      goto 99
   endif

   IF (MN.EQ.1 .AND. KS.NE.0 .AND. FMT.LT.3 .AND. TYP.NE.1) F = F+2

   IF (FMT .EQ. 5) F = 11

   JINC = FNL(F)
   F = FNO(F)

   S = 1.0D0
   IF (F.EQ.21 .OR. F.EQ.22 .OR. F.EQ.31 .OR. F.EQ.32) S = 10.0D0**KS
   LS = ((N-1)/JINC+1)*M + 2
!.......................................................................
   IF (LCT(1) + LS .LE. LCT(2)) GOTO 20
   LCT(1) = 0

   WRITE(mline,43) LS
43 FORMAT(' AT LEAST ',I5,' MORE LINES.','  ENTER BLANK LINE TO CONTINUE OUTPUT.')
   call journal(mline)

   READ(RTE,'(a1)',END=19) LS_CHAR  ! read response to pause from standard input
   IF (LS_CHAR .EQ. ' ') GOTO 20         ! if blank or a return display the values
   LCT(1) = -1
   GOTO 99
19 CONTINUE
   CALL mat_files(-RTE,BUF)
20 CONTINUE
!.......................................................................
   call journal(' ')
   CALL mat_prntid(ID,-1)
   LCT(1) = LCT(1)+2
   IF (S .NE. 1.0D0)then
      WRITE(mline,'(''  '',1PD9.1," *")') S
      if(wte.eq.6)then
         call journal(mline)
      else
         write(wte,'(a)')mline(1:80)
      endif
   endif
   DO 80 J1 = 1, N, JINC
      J2 = MIN0(N, J1+JINC-1)
      IF (N .GT. JINC)then
         WRITE(mline,'(''     COLUMNS'',I6,'' THRU'',I6)') J1,J2
         if(wte.eq.6)then
            call journal(mline)
         else
            write(wte,'(a)')mline(1:80)
         endif
      endif
      DO 70 I = 1, M
         JM = J2-J1+1
         DO J = 1, JM
            LS = L+I-1+(J+J1-2)*M
            PR(J) = STKR(LS)/S
            PI(J) = DABS(STKI(LS)/S)
            SIG(J) = ALFA(PLUS+1)
            IF (STKI(LS) .LT. 0.0D0) SIG(J) = ALFA(MINUS+1)
         enddo
         goto(11,12)F-10
         goto(21,22,23,24)F-20
         goto(31,32,33,34)F-30
         IF (F .EQ. -1) THEN
            CALL mat_formz(WTE,STKR(LS),STKI(LS))
            goto 71
         endif
         call journal('*internal error*')
         goto 99
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
11       CONTINUE
         FORM='(1X,12F6.0)'  ! integer
         ISTEP=12
         goto 716
12       CONTINUE
         FORM='(1X,6F12.0)'  ! integer
         ISTEP=6
         goto 716

716      CONTINUE
         J3=1
7161     CONTINUE
         WRITE(mline,FORM)(PR(J),J=J3,MIN(J3+ISTEP-1,JM))
         if(wte.eq.6)then
            call journal(mline)
         else
            write(wte,'(a)')mline(1:80)
         endif
         J3=J3+ISTEP
         if(J3.le.JM)goto 7161
         GOTO 71
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
21       CONTINUE
         FORM='(1X,F9.4,7F10.4)'  ! 8 numbers
         ISTEP=8
         goto 714
22       CONTINUE
         FORM='(1X,F19.15,3F20.15)'  ! 4 numbers
         ISTEP=4
         goto 714
23       CONTINUE
         FORM='(1X,1P6D13.4)'   ! 6 numbers
         ISTEP=6
         goto 714
24       CONTINUE
         FORM='(1X,1P3D24.15)'  ! 3 numbers
         ISTEP=3
         GOTO 714

714      CONTINUE
         J3=1
7141     CONTINUE
         WRITE(mline,FORM)(PR(J),J=J3,MIN(J3+ISTEP,JM))
         if(wte.eq.6)then
            call journal(mline)
         else
            write(wte,'(a)')mline(1:80)
         endif
         J3=J3+ISTEP
         if(J3.le.JM)goto 7141
         goto 71
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
31       CONTINUE
         FORM='(1X,4(F9.4,1X,A1,F7.4,''i''))'  ! 4x3
         ISTEP=12
         goto 718
32       CONTINUE
         FORM='(1X,F19.15,A1,F18.15,''i'',F20.15,A1,F18.15,''i'')'  ! 6
         ISTEP=6
         goto 718
33       CONTINUE
         FORM='(1X,3(1PD13.4,1X,A1,1PD10.4,''i''))'  ! 9
         ISTEP=9
         goto 718
34       CONTINUE
         FORM='(1X,1PD24.15,1X,A1,1PD21.15,''i'')'  ! 3
         ISTEP=3

718      CONTINUE
         J3=1
7181     CONTINUE
         WRITE(mline,form)(PR(J),SIG(J),PI(J),J=J3,MIN(J3+ISTEP-1,JM))
         if(wte.eq.6)then
            call journal(mline)
         else
            write(wte,'(a)')mline(1:80)
         endif
         J3=J3+ISTEP
         if(J3.le.JM)goto 7181
         goto 71
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
71       CONTINUE
         LCT(1) = LCT(1)+1
70    CONTINUE
80 CONTINUE
   GOTO 99
!.......................................................................
99 CONTINUE
   if(wte.ne.6)flush(unit=wte,iostat=ios)
!
END SUBROUTINE mat_print
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wsqrt(xr,xi,yr,yi)

! ident_15="@(#)M_matrix::mat_wsqrt(3fp): y = sqrt(x) with yr .ge. 0.0 and sign(yi) .eq. sign(xi)"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi

doubleprecision :: s
doubleprecision :: tr
doubleprecision :: ti
!
   tr = xr
   ti = xi
   s = dsqrt(0.5d0*(mat_pythag(tr,ti) + dabs(tr)))
   if (tr .ge. 0.0d0) yr = mat_flop(s)
   if (ti .lt. 0.0d0) s = -s
   if (tr .le. 0.0d0) yi = mat_flop(s)
   if (tr .lt. 0.0d0) yr = mat_flop(0.5d0*(ti/yi))
   if (tr .gt. 0.0d0) yi = mat_flop(0.5d0*(ti/yr))
end subroutine mat_wsqrt
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wlog(xr,xi,yr,yi)

! ident_16="@(#)M_matrix::mat_wlog(3fp): y = log(x)"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi
doubleprecision :: t
doubleprecision :: r
   r = mat_pythag(xr,xi)
   if (r .eq. 0.0d0) call mat_err(32)
   if (r .eq. 0.0d0) return
   t = datan2(xi,xr)
   if (xi.eq.0.0d0 .and. xr.lt.0.0d0) t = dabs(t)
   yr = dlog(r)
   yi = t
end subroutine mat_wlog
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_formz(lunit,x,y)

! ident_17="@(#)M_matrix::mat_formz: system dependent routine to print with z format"

integer                    :: lunit
doubleprecision,intent(in) :: x,y

character(len=36)          :: mline

   if (y .ne. 0.0d0) then
      write(mline,'(2z18)') x,y
   else
      write(mline,'(z18)') x
   endif

   call journal(mline)

end subroutine mat_formz
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_prompt(pause)

! ident_18="@(#)issue interactive prompt with optional pause"

integer,intent(in) :: pause
character(len=1)   :: dummy

   ! paranoid checks
   if(wte.le.0)then
      call journal('*mat_prompt* internal error: wte <= 0')
   elseif(rte.lt.0)then
      call journal('*mat_prompt* internal error: rte <= 0')
   else
      ! write prompt using format that stays on current line
      if(wte.eq.6)then
         WRITE(WTE,'(''<>'')',advance='no')   ! write prompt to interactive input
      endif
      if (pause .eq. 1) read(rte,'(a1)') dummy
   endif

end subroutine mat_prompt
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_magic(a,lda,n)
!
! ident_19="@(#)M_matrix::mat_magic(3fp): Algorithms for magic squares"

!        Algorithms taken from
!        Mathematical Recreations and Essays, 12th Ed.,
!        by W. W. Rouse Ball and H. S. M. Coxeter
!
integer         :: lda
integer         :: n
doubleprecision :: a(lda,n)

doubleprecision :: t
integer         :: i
integer         :: j
integer         :: k
integer         :: m
integer         :: mm
integer         :: i1
integer         :: im
integer         :: j1
integer         :: jm
integer         :: m1
integer         :: m2
!
   if (mod(n,4) .eq. 0) goto 100
   if (mod(n,2) .eq. 0) m = n/2
   if (mod(n,2) .ne. 0) m = n
!
!     odd order or upper corner of even order
!
   do j = 1,m
      do i = 1,m
         a(i,j) = 0
      enddo
   enddo
   i = 1
   j = (m+1)/2
   mm = m*m
   do k = 1, mm
      a(i,j) = k
      i1 = i-1
      j1 = j+1
      if(i1.lt.1) i1 = m
      if(j1.gt.m) j1 = 1
      if(idint(a(i1,j1)).eq.0) goto 30
      i1 = i+1
      j1 = j
30    continue
      i = i1
      j = j1
   enddo
   if (mod(n,2) .ne. 0) return
!
!     rest of even order
!
   t = dble(m*m)
   do i = 1, m
      do j = 1, m
         im = i+m
         jm = j+m
         a(i,jm) = a(i,j) + 2*t
         a(im,j) = a(i,j) + 3*t
         a(im,jm) = a(i,j) + t
      enddo
   enddo
   m1 = (m-1)/2
   if (m1.eq.0) return
   do j = 1, m1
      call mat_rswap(m,a(1,j),1,a(m+1,j),1)
   enddo
   m1 = (m+1)/2
   m2 = m1 + m
   call mat_rswap(1,a(m1,1),1,a(m2,1),1)
   call mat_rswap(1,a(m1,m1),1,a(m2,m1),1)
   m1 = n+1-(m-3)/2
   if(m1.gt.n) return
   do j = m1, n
      call mat_rswap(m,a(1,j),1,a(m+1,j),1)
   enddo
   return
!
!     double even order
!
100 continue
   k = 1
   do i = 1, n
      do j = 1, n
         a(i,j) = k
         if (mod(i,4)/2 .eq. mod(j,4)/2) a(i,j) = n*n+1 - k
         k = k+1
      enddo
   enddo
end subroutine mat_magic
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rswap(n,x,incx,y,incy)
integer         :: n
doubleprecision :: x(*)
integer         :: incx
doubleprecision :: y(*)
integer         :: incy

doubleprecision :: t
integer         :: ix
integer         :: iy
integer         :: i

   if (n .le. 0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx+1
   if (incy.lt.0) iy = (-n+1)*incy+1
   do i = 1, n
      t = x(ix)
      x(ix) = y(iy)
      y(iy) = t
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine mat_rswap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rref(ar,ai,lda,m,n,eps)
integer,intent(in) :: lda
doubleprecision    :: ar(lda,*)
doubleprecision    :: ai(lda,*)
integer            :: m
integer            :: n
doubleprecision    :: eps

doubleprecision    :: tol
doubleprecision    :: tr
doubleprecision    :: ti
integer            :: i, j, k, l

   tol = 0.0d0
   do j = 1, n
      tol = dmax1(tol,mat_wasum(m,ar(1,j),ai(1,j),1))
   enddo
   tol = eps*dble(2*max0(m,n))*tol
   k = 1
   l = 1
   INFINITE: do
      IF (K.GT.M .OR. L.GT.N) RETURN

      i = mat_iwamax(m-k+1,ar(k,l),ai(k,l),1) + k-1
      if (dabs(ar(i,l))+dabs(ai(i,l)) .le. tol)then
         call mat_wset(m-k+1,0.0d0,0.0d0,ar(k,l),ai(k,l),1)
         l = l+1
         cycle INFINITE
      endif

      call mat_wswap(n-l+1,ar(i,l),ai(i,l),lda,ar(k,l),ai(k,l),lda)
      call mat_wdiv(1.0d0,0.0d0,ar(k,l),ai(k,l),tr,ti)
      call mat_wscal(n-l+1,tr,ti,ar(k,l),ai(k,l),lda)
      ar(k,l) = 1.0d0
      ai(k,l) = 0.0d0
      do i = 1, m
         tr = -ar(i,l)
         ti = -ai(i,l)
         if (i .ne. k) call matX_waxpy(n-l+1,tr,ti,ar(k,l),ai(k,l),lda,ar(i,l),ai(i,l),lda)
      enddo
      K = K+1
      L = L+1
   enddo INFINITE
end subroutine mat_rref
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wrscal(n,s,xr,xi,incx)
integer         :: n
doubleprecision :: s
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx

integer         :: ix
integer         :: i
   if (n .le. 0) return
   ix = 1
   do i = 1, n
      xr(ix) = mat_flop(s*xr(ix))
      if (xi(ix) .ne. 0.0d0) xi(ix) = mat_flop(s*xi(ix))
      ix = ix + incx
   enddo
end subroutine mat_wrscal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wscal(n,sr,si,xr,xi,incx)
integer         :: n
doubleprecision :: sr
doubleprecision :: si
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
integer         :: ix
integer         :: i
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         call mat_wmul(sr,si,xr(ix),xi(ix),xr(ix),xi(ix))
         ix = ix + incx
      enddo
   endif
end subroutine mat_wscal
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wmul(ar,ai,br,bi,cr,ci)

! ident_20="@(#)mat_wmul(3fp) [M_matrix] c = a*b"

doubleprecision :: ar
doubleprecision :: ai
doubleprecision :: br
doubleprecision :: bi
doubleprecision :: cr
doubleprecision :: ci

doubleprecision :: t
   t = ar*bi + ai*br
   if (t .ne. 0.0d0) t = mat_flop(t)
   cr = mat_flop(ar*br - ai*bi)
   ci = t
end subroutine mat_wmul
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack1(op)

! ident_21="@(#)M_matrix::mat_stack1(3f): Unary Operations"

integer           :: op
INTEGER,parameter :: QUOTE=49
integer           :: i
integer           :: j
integer           :: l
integer           :: ll
integer           :: ls
integer           :: m
integer           :: mn
integer           :: n

   IF (DDT .EQ. 1) call journal('sc','mat_stack1 ',OP)
   L = LSTK(TOP)
   M = MSTK(TOP)
   N = NSTK(TOP)
   MN = M*N
   IF (MN .EQ. 0) then
   elseIF (OP .ne. QUOTE) then                                 ! UNARY MINUS
      CALL mat_wrscal(MN,-1.0D0,STKR(L),STKI(L),1)
   else                                                        ! TRANSPOSE
      LL = L + MN
      ERR = LL+MN - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      CALL mat_wcopy(MN,STKR(L),STKI(L),1,STKR(LL),STKI(LL),1)
      M = NSTK(TOP)
      N = MSTK(TOP)
      MSTK(TOP) = M
      NSTK(TOP) = N
      DO I = 1, M
         DO J = 1, N
            LS = L+MN+(J-1)+(I-1)*N
            LL = L+(I-1)+(J-1)*M
            STKR(LL) = STKR(LS)
            STKI(LL) = -STKI(LS)
         enddo
      enddo
   endif
end subroutine mat_stack1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rrot(n,dx,incx,dy,incy,c,s)

! ident_22="@(#)M_matrix::mat_rrot(3f): Applies a plane rotation."

integer         :: n
doubleprecision :: dx(*)
integer         :: incx
doubleprecision :: dy(*)
integer         :: incy
doubleprecision :: c
doubleprecision :: s

doubleprecision :: dtemp
integer         :: i
integer         :: ix
integer         :: iy
!
   if (n.gt.0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1,n
           dtemp = mat_flop(c*dx(ix) + s*dy(iy))
           dy(iy) = mat_flop(c*dy(iy) - s*dx(ix))
           dx(ix) = dtemp
           ix = ix + incx
           iy = iy + incy
      enddo
   endif
end subroutine mat_rrot
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rset(n,dx,dy,incy)

! ident_23="@(#)M_matrix::mat_rset(3f): copies a scalar, dx, to a scalar, dy."

integer         :: n
doubleprecision :: dx,dy(*)
integer         :: incy
integer         :: i
integer         :: iy

   if (n.gt.0) then
      iy = 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1,n
         dy(iy) = dx
         iy = iy + incy
      enddo
   endif
end subroutine mat_rset
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_prntid(id,argcnt)

! ident_24="@(#)M_matrix::mat_prntid(3fp): print table of variable id names (up to) eight per line"

!     ID     IS ARRAY OF 4-CHARACTER IDS TO PRINT
!     ARGCNT IS NUMBER OF IDS TO PRINT
!            IF = -1, PRINT ONE ID WITH AN "  =" SUFFIX
!
integer :: id(4,*)
integer :: argcnt

character(len=80)   :: mline           ! scratch space for building line to print
integer             :: linebuf(256)    ! scratch buffer for building up line

integer,parameter   ::  ade_blank=32
integer,parameter   ::  ade_equal=61
integer             :: j1
integer             :: i, j, k, l

   j1 = 1                              ! which ID to start the line with
   INFINITE : do
      linebuf(1)=ade_blank                ! put a space at beginning of line
      l = 2                               ! pointer into output line being built
      do j = j1,min0(j1+7,iabs(argcnt))   ! copy up to eight names into buffer
         do i = 1, 4                      !    copy one name into buffer
            k = id(i,j)+1                 ! this is the kth letter of the set
            linebuf(l) = alfa(k)
            l = l+1                       ! increment pointer into output
         enddo
         linebuf(l+0)=ade_blank           ! put two space between names
         linebuf(l+1)=ade_blank
         l=l+2
      enddo
      if (argcnt .eq. -1) then            ! special flag to print one word and  =
         linebuf(l) = ade_equal           ! put value for equal sign into buffer
      else
         l=l-3                            ! was prepared for another ID with two blanks
      endif
      !-----------------------------------------------
      call mat_buf2str(mline,linebuf,l)   ! write LINEBUF(1:L) line to a character variable
      if(wte.eq.6)then
         call journal(mline)              ! print the line
      else
         write(wte,'(a)')mline(1:80)      ! print the line
      endif
      !-----------------------------------------------
      j1 = j1+8                           ! prepare to get up to eight more IDs
      if (j1 .gt. iabs(argcnt)) exit INFINITE   ! if not done do another line
   enddo INFINITE
end subroutine mat_prntid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stackp(id)

! ident_25="@(#)M_matrix::mat_stackp(3fp): put variables into storage"

integer             :: id(4)
character(len=100)  :: mline
integer,parameter   :: semi=39
integer             :: i
integer             :: ib
integer             :: j
integer             :: k
integer             :: km1
integer             :: l
integer             :: l1
integer             :: l2
integer             :: li
integer             :: lj
integer             :: lk
integer             :: ll
integer             :: ls
integer             :: lt
integer             :: m
integer             :: m1
integer             :: m2
integer             :: mk
integer             :: mn
integer             :: mn1
integer             :: mn2
integer             :: mnk
integer             :: mt
integer             :: n
integer             :: nk
integer             :: nt

   if (ddt .eq. 1) then
      write(mline,'('' STACKP '',4i4)') id
      call journal(mline)
   endif

   if (top .le. 0) call mat_err(1)
   if (err .gt. 0) return

   call mat_funs(id)
   if (fin .ne. 0) call mat_err(25)
   if (err .gt. 0) return

   m = mstk(top)
   n = nstk(top)
   if (m .gt. 0) l = lstk(top)
   if (m .lt. 0) call mat_err(14)
   if (err .gt. 0) return

   if (m .eq. 0 .and. n .ne. 0) goto 99
   mn = m*n
   lk = 0
   mk = 1
   nk = 0
   lt = 0
   mt = 0
   nt = 0
!
!     DOES VARIABLE ALREADY EXIST
   call mat_putid(idstk(1,bot-1),id)
   k = lsize+1
05 continue
   k = k-1
   if (.not.mat_eqid(idstk(1,k),id)) goto 05
   if (k .eq. bot-1) goto 30
   lk = lstk(k)
   mk = mstk(k)
   nk = nstk(k)
   mnk = mk*nk
   if (rhs .eq. 0) goto 20
   if (rhs .gt. 2) call mat_err(15)
   if (err .gt. 0) return
   mt = mk
   nt = nk
   lt = l + mn
   err = lt + mnk - lstk(bot)
   if (err .gt. 0) call mat_err(17)
   if (err .gt. 0) return
   call mat_wcopy(mnk,stkr(lk),stki(lk),1,stkr(lt),stki(lt),1)
!
!     DOES IT FIT
20 continue
   if (rhs.eq.0 .and. mn.eq.mnk) goto 40
   if (k .ge. lsize-3) call mat_err(13)
   if (err .gt. 0) return
!
!     SHIFT STORAGE
   if (k .eq. bot) goto 25
   ls = lstk(bot)
   ll = ls + mnk
   call mat_wcopy(lk-ls,stkr(ls),stki(ls),-1,stkr(ll),stki(ll),-1)
   km1 = k-1
   do ib = bot, km1
      i = bot+km1-ib
      call mat_putid(idstk(1,i+1),idstk(1,i))
      mstk(i+1) = mstk(i)
      nstk(i+1) = nstk(i)
      lstk(i+1) = lstk(i)+mnk
   enddo
!
!     DESTROY OLD VARIABLE
25 continue
   bot = bot+1
!
!     CREATE NEW VARIABLE
30 continue
   if (mn .eq. 0) goto 99
   if (bot-2 .le. top) call mat_err(18)
   if (err .gt. 0) return
   k = bot-1
   call mat_putid(idstk(1,k), id)
   if (rhs .eq. 1) goto 50
   if (rhs .eq. 2) goto 55
!
!     STORE
40 continue
   if (k .lt. lsize) lstk(k) = lstk(k+1) - mn
   mstk(k) = m
   nstk(k) = n
   lk = lstk(k)
   call mat_wcopy(mn,stkr(l),stki(l),-1,stkr(lk),stki(lk),-1)
   goto 90
!
!     VECT(ARG)
50 continue
   if (mstk(top-1) .lt. 0) goto 59
   mn1 = 1
   mn2 = 1
   l1 = 0
   l2 = 0
   if (n.ne.1 .or. nk.ne.1) goto 52
   l1 = lstk(top-1)
   m1 = mstk(top-1)
   mn1 = m1*nstk(top-1)
   m2 = -1
   goto 60
52 continue
   if (m.ne.1 .or. mk.ne.1) call mat_err(15)
   if (err .gt. 0) return
   l2 = lstk(top-1)
   m2 = mstk(top-1)
   mn2 = m2*nstk(top-1)
   m1 = -1
   goto 60
!
!     matrix(arg,arg)
55 continue
   if (mstk(top-1).lt.0 .and. mstk(top-2).lt.0) goto 59
   l2 = lstk(top-1)
   m2 = mstk(top-1)
   mn2 = m2*nstk(top-1)
   if (m2 .lt. 0) mn2 = n
   l1 = lstk(top-2)
   m1 = mstk(top-2)
   mn1 = m1*nstk(top-2)
   if (m1 .lt. 0) mn1 = m
   goto 60
!
59 continue
   if (mn .ne. mnk) call mat_err(15)
   if (err .gt. 0) return
   lk = lstk(k)
   call mat_wcopy(mn,stkr(l),stki(l),-1,stkr(lk),stki(lk),-1)
   goto 90
!
60 continue
   if (mn1.ne.m .or. mn2.ne.n) call mat_err(15)
   if (err .gt. 0) return
   ll = 1
   if (m1 .lt. 0) goto 62
   do i = 1, mn1
      ls = l1+i-1
      mk = max0(mk,idint(stkr(ls)))
      ll = min0(ll,idint(stkr(ls)))
   enddo
62 continue
   mk = max0(mk,m)
   if (m2 .lt. 0) goto 64
   do i = 1, mn2
      ls = l2+i-1
      nk = max0(nk,idint(stkr(ls)))
      ll = min0(ll,idint(stkr(ls)))
   enddo
64 continue
   nk = max0(nk,n)
   if (ll .lt. 1) call mat_err(21)
   if (err .gt. 0) return
   mnk = mk*nk
   lk = lstk(k+1) - mnk
   err = lt + mt*nt - lk
   if (err .gt. 0) call mat_err(17)
   if (err .gt. 0) return
   lstk(k) = lk
   mstk(k) = mk
   nstk(k) = nk
   call mat_wset(mnk,0.0d0,0.0d0,stkr(lk),stki(lk),1)
   if (nt .lt. 1) goto 67
   do j = 1, nt
      ls = lt+(j-1)*mt
      ll = lk+(j-1)*mk
      call mat_wcopy(mt,stkr(ls),stki(ls),-1,stkr(ll),stki(ll),-1)
   enddo
67 continue
   do j = 1, n
      do i = 1, m
         li = l1+i-1
         if (m1 .gt. 0) li = l1 + idint(stkr(li)) - 1
         lj = l2+j-1
         if (m2 .gt. 0) lj = l2 + idint(stkr(lj)) - 1
         ll = lk+li-l1+(lj-l2)*mk
         ls = l+i-1+(j-1)*m
         stkr(ll) = stkr(ls)
         stki(ll) = stki(ls)
      enddo
   enddo
   goto 90
!
!     PRINT IF DESIRED AND POP STACK
90 continue
   if (sym.ne.semi .and. lct(3).eq.0) call mat_print(id,k)
   if (sym.eq.semi .and. lct(3).eq.1) call mat_print(id,k)
   if (k .eq. bot-1) bot = bot-1
99 continue
   if (m .ne. 0) top = top - 1 - rhs
   if (m .eq. 0) top = top - 1
end subroutine mat_stackp
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wpofa(ar,ai,lda,n,info)
integer          :: lda
doubleprecision  :: ar(lda,*)
doubleprecision  :: ai(lda,*)
integer          :: n
integer          :: info

doubleprecision  :: s
doubleprecision  :: TR
doubleprecision  :: TI
integer          :: j
integer          :: jm1
integer          :: k

   do j = 1, n
      info = j
      s = 0.0d0
      jm1 = j-1
      if (jm1 .ge. 1) then
         do k = 1, jm1
           tr=ar(k,j)-mat_wdotcr(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
           ti=ai(k,j)-mat_wdotci(k-1,ar(1,k),ai(1,k),1,ar(1,j),ai(1,j),1)
           call mat_wdiv(tr,ti,ar(k,k),ai(k,k),tr,ti)
           ar(k,j) = tr
           ai(k,j) = ti
           s = s + tr*tr + ti*ti
         enddo
      endif
      s = ar(j,j) - s
      if (s.le.0.0d0 .or. ai(j,j).ne.0.0d0) goto 40
      ar(j,j) = dsqrt(s)
   enddo
   info = 0
40 continue
end subroutine mat_wpofa
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_watan(xr,xi,yr,yi)

! ident_26="@(#)M_matrix::mat_watan(3fp): y = atan(x) = (i/2)*log((i+x)/(i-x))"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi
doubleprecision :: tr
doubleprecision :: ti

   if (xi .eq. 0.0d0) then
      yr = datan2(xr,1.0d0)
      yi = 0.0d0
   elseif (xr.ne.0.0d0 .or. dabs(xi).ne.1.0d0) then
      call mat_wdiv(xr,1.0d0+xi,-xr,1.0d0-xi,tr,ti)
      call mat_wlog(tr,ti,tr,ti)
      yr = -(ti/2.0d0)
      yi = tr/2.0d0
   else
      call mat_err(32)
   endif

end subroutine mat_watan
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rrotg(da,db,c,s)

! ident_27="@(#)M_matrix::mat_rrotg(3fp): construct Givens plane rotation."

doubleprecision :: da
doubleprecision :: db
doubleprecision :: c
doubleprecision :: s

doubleprecision :: rho
doubleprecision :: r
doubleprecision :: z

   rho = db
   if ( dabs(da) .gt. dabs(db) ) rho = da
   c = 1.0d0
   s = 0.0d0
   z = 1.0d0
   r = mat_flop(dsign(mat_pythag(da,db),rho))
   if (r .ne. 0.0d0) c = mat_flop(da/r)
   if (r .ne. 0.0d0) s = mat_flop(db/r)
   if ( dabs(da) .gt. dabs(db) ) z = s
   if (dabs(db) .ge. dabs(da) .and. c .ne. 0.0d0)z = mat_flop(1.0d0/c)
   da = r
   db = z
end subroutine mat_rrotg
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_wsign(xr,xi,yr,yi,zr,zi)

! ident_28="@(#)M_matrix::mat_wsign(3fp): if y .ne. 0, z = x*y/abs(y)"

doubleprecision :: xr
doubleprecision :: xi
doubleprecision :: yr
doubleprecision :: yi
doubleprecision :: zr
doubleprecision :: zi
doubleprecision :: t
   t = mat_pythag(yr,yi)
   zr = xr
   zi = xi
   if (t .ne. 0.0d0) call mat_wmul(yr/t,yi/t,zr,zi,zr,zi)
end subroutine mat_wsign
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_parse(INIT)

integer            :: init
INTEGER,parameter  :: SEMI=39
integer,parameter  :: EQUAL=46
integer            :: ID(4)
integer            :: EXCNT
integer,parameter  :: LPAREN=37
integer,parameter  :: RPAREN=38
integer,parameter  :: COLON=40
integer            :: PTS
INTEGER,parameter  :: BLANK=36
integer,parameter  :: COMMA=48
integer,parameter  :: LESS=50
integer,parameter  :: GREAT=51
integer            :: NAME=1
integer,parameter  :: ANS(4) =[10,23,28,36]
integer,parameter  :: ENND(4)=[14,23,13,36]
integer,parameter  :: ELSE(4)=[14,21,28,14]
integer            :: P
integer            :: R
character(len=80)  :: mline
integer            :: i5
integer            :: ierr
integer            :: ilen
integer            :: j
integer            :: k
integer            :: l
integer            :: ls
integer            :: n
!
   01 CONTINUE
      R = 0
      IF (ERR .GT. 0) PTZ = 0
      IF (ERR.LE.0 .AND. PT.GT.PTZ) R = RSTK(PT)
      IF (DDT .EQ. 1) THEN
         WRITE(MLINE,'('' PARSE'',4I4)') PT,R,PTZ,ERR
         CALL JOURNAL(MLINE)
      ENDIF
      IF (R.EQ.15) GOTO 93
      IF (R.EQ.16 .OR. R.EQ.17) GOTO 94
      SYM = EOL
      TOP = 0
      IF (RIO .NE. RTE) CALL mat_files(-RIO,BUF)
      RIO = RTE
      LCT(3) = 0
      LCT(4) = 2
      LPT(1) = 1
   10 CONTINUE
      IF (SYM.EQ.EOL.AND.MOD(LCT(4)/2,2).EQ.1) CALL mat_prompt(LCT(4)/4)
      IF (SYM .EQ. EOL) CALL mat_getlin()
      ERR = 0
      PT = PTZ
   15 CONTINUE
      EXCNT = 0
      IF (DDT .EQ. 1) THEN
         MLINE='STATE'
         CALL mat_appnum(REAL(PT),MLINE,ILEN,IERR)
         CALL mat_appnum(REAL(TOP),MLINE,ILEN,IERR)
         call journal(mline)
      ENDIF
      LHS = 1
      CALL mat_putid(ID,ANS)
      CALL mat_getsym()
      IF (SYM.EQ.COLON .AND. CHRA.EQ.EOL) DDT = 1-DDT
      IF (SYM .EQ. COLON) CALL mat_getsym()
      IF (SYM.EQ.SEMI .OR. SYM.EQ.COMMA .OR. SYM.EQ.EOL) GOTO 80
      IF (SYM .EQ. NAME) GOTO 20
      IF (SYM .EQ. LESS) GOTO 40
      IF (SYM .EQ. GREAT) GOTO 45
      GOTO 50
!.......................................................................
!     LHS BEGINS WITH NAME
   20 CONTINUE
      CALL mat_comand(SYN)
      IF (ERR .GT. 0) GOTO 01
      IF (FUN .EQ. 99) GOTO 95
      IF (FIN .EQ. -15) GOTO 80
      IF (FIN .LT. 0) GOTO 91
      IF (FIN .GT. 0) GOTO 70
!     IF NAME IS A FUNCTION, MUST BE RHS
      RHS = 0
      CALL mat_funs(SYN)
      IF (FIN .NE. 0) GOTO 50
!     PEEK ONE CHARACTER AHEAD
      IF (CHRA.EQ.SEMI .OR. CHRA.EQ.COMMA .OR. CHRA.EQ.EOL) CALL mat_putid(ID,SYN)
      IF (CHRA .EQ. EQUAL) GOTO 25
      IF (CHRA .EQ. LPAREN) GOTO 30
      GOTO 50
!.......................................................................
!     LHS IS SIMPLE VARIABLE
   25 CONTINUE
      CALL mat_putid(ID,SYN)
      CALL mat_getsym()
      CALL mat_getsym()
      GOTO 50
!.......................................................................
!     LHS IS NAME(...)
   30 CONTINUE
      LPT(5) = LPT(4)
      CALL mat_putid(ID,SYN)
      CALL mat_getsym()
   32 CONTINUE
      CALL mat_getsym()
      EXCNT = EXCNT+1
      PT = PT+1
      CALL mat_putid(IDS(1,PT), ID)
      PSTK(PT) = EXCNT
      RSTK(PT) = 1
!     *CALL* EXPR
      GOTO 92
!.......................................................................
   35 CONTINUE
      CALL mat_putid(ID,IDS(1,PT))
      EXCNT = PSTK(PT)
      PT = PT-1
      IF (SYM .EQ. COMMA) GOTO 32
      IF (SYM .NE. RPAREN) CALL mat_err(3)
      IF (ERR .GT. 0) GOTO 01
      IF (ERR .GT. 0) RETURN
      IF (SYM .EQ. RPAREN) CALL mat_getsym()
      IF (SYM .EQ. EQUAL) GOTO 50
!     LHS IS REALLY RHS, FORGET SCAN JUST DONE
      TOP = TOP - EXCNT
      LPT(4) = LPT(5)
      CHRA = LPAREN
      SYM = NAME
      CALL mat_putid(SYN,ID)
      CALL mat_putid(ID,ANS)
      EXCNT = 0
      GOTO 50
!.......................................................................
!     MULTIPLE LHS
   40 CONTINUE
      LPT(5) = LPT(4)
      PTS = PT
      CALL mat_getsym()
   41 CONTINUE
      IF (SYM .NE. NAME) GOTO 43
      CALL mat_putid(ID,SYN)
      CALL mat_getsym()
      IF (SYM .EQ. GREAT) GOTO 42
      IF (SYM .EQ. COMMA) CALL mat_getsym()
      PT = PT+1
      LHS = LHS+1
      PSTK(PT) = 0
      CALL mat_putid(IDS(1,PT),ID)
      GOTO 41
   42 CONTINUE
      CALL mat_getsym()
      IF (SYM .EQ. EQUAL) GOTO 50
   43 CONTINUE
      LPT(4) = LPT(5)
      PT = PTS
      LHS = 1
      SYM = LESS
      CHRA = LPT(4)-1
      CHRA = LIN(CHRA)
      CALL mat_putid(ID,ANS)
      GOTO 50
!.......................................................................
!     MACRO STRING
   45 CONTINUE
      CALL mat_getsym()
      IF (DDT .EQ. 1) THEN
         MLINE='MACRO'
         CALL mat_appnum(REAL(PT),MLINE,ILEN,IERR)
         CALL mat_appnum(REAL(TOP),MLINE,ILEN,IERR)
      ENDIF
      IF (SYM.EQ.LESS .AND. CHRA.EQ.EOL) CALL mat_err(28)
      IF (ERR .GT. 0) GOTO 01
      PT = PT+1
      RSTK(PT) = 20
!     *CALL* EXPR
      GOTO 92
!.......................................................................
   46 CONTINUE
      PT = PT-1
      IF (SYM.NE.LESS .AND. SYM.NE.EOL) CALL mat_err(37)
      IF (ERR .GT. 0) GOTO 01
      IF (SYM .EQ. LESS) CALL mat_getsym()
      K = LPT(6)
      LIN(K+1) = LPT(1)
      LIN(K+2) = LPT(2)
      LIN(K+3) = LPT(6)
      LPT(1) = K + 4
!     TRANSFER STACK TO INPUT LINE
      K = LPT(1)
      L = LSTK(TOP)
      N = MSTK(TOP)*NSTK(TOP)
      DO J = 1, N
         LS = L + J-1
         LIN(K) = IDINT(STKR(LS))
         IF (LIN(K).LT.0 .OR. LIN(K).GE.IALF) CALL mat_err(37)
         IF (ERR .GT. 0) RETURN
         IF (K.LT.1024) K = K+1
         IF (K.EQ.1024) then
            WRITE(mline,47) K
            call journal(mline)
          endif
      enddo
   47 FORMAT(1X,'INPUT BUFFER LIMIT IS ',I4,' CHARACTERS.')
      TOP = TOP-1
      LIN(K) = EOL
      LPT(6) = K
      LPT(4) = LPT(1)
      LPT(3) = 0
      LPT(2) = 0
      LCT(1) = 0
      CHRA = BLANK
      PT = PT+1
      PSTK(PT) = LPT(1)
      RSTK(PT) = 21
!     *CALL* PARSE
      GOTO 15
!.......................................................................
   49 CONTINUE
      PT = PT-1
      IF (DDT .EQ. 1)then
         WRITE(mline,'('' MACEND '',2I4)') PT,TOP
         call journal(mline)
      endif
      K = LPT(1) - 4
      LPT(1) = LIN(K+1)
      LPT(4) = LIN(K+2)
      LPT(6) = LIN(K+3)
      CHRA = BLANK
      CALL mat_getsym()
      GOTO 80
!.......................................................................
!     LHS FINISHED, START RHS
   50 CONTINUE
      IF (SYM .EQ. EQUAL) CALL mat_getsym()
      PT = PT+1
      CALL mat_putid(IDS(1,PT),ID)
      PSTK(PT) = EXCNT
      RSTK(PT) = 2
!     *CALL* EXPR
      GOTO 92
   55 CONTINUE
      IF (SYM.EQ.SEMI .OR. SYM.EQ.COMMA .OR. SYM.EQ.EOL) GOTO 60
      IF (SYM.EQ.NAME .AND. mat_eqid(SYN,ELSE)) GOTO 60
      IF (SYM.EQ.NAME .AND. mat_eqid(SYN,ENND)) GOTO 60
      CALL mat_err(40)
      IF (ERR .GT. 0) GOTO 01
!
!     STORE RESULTS
   60 CONTINUE
      RHS = PSTK(PT)
      CALL mat_stackp(IDS(1,PT))
      IF (ERR .GT. 0) GOTO 01
      PT = PT-1
      LHS = LHS-1
      IF (LHS .GT. 0) GOTO 60
      GOTO 70
!.......................................................................
!
!     UPDATE AND POSSIBLY PRINT OPERATION COUNTS
   70 CONTINUE
      K = FLP(1)
      IF (K .NE. 0) STKR(VSIZE-3) = dble(K)
      STKR(VSIZE-2) = STKR(VSIZE-2) + dble(K)
      FLP(1) = 0
      IF (.NOT.(CHRA.EQ.COMMA .OR. (SYM.EQ.COMMA .AND. CHRA.EQ.EOL)))GOTO 80
      CALL mat_getsym()
      I5 = 10**5

      IF (K .EQ. 0) call journal('   no flops')
      IF (K .EQ. 1) call journal('    1 flop')
      IF (1.LT.K .AND. K.LT.100000)THEN
         WRITE(mline,'(1X,I5,'' flops'')') K
         call journal(mline)
      ENDIF
      IF (100000 .LE. K)then
         WRITE(mline,'(1x,i9,'' flops'')') K
         call journal(mline)
      endif
      GOTO 80
!.......................................................................
!
!     FINISH STATEMENT
   80 CONTINUE
      FIN = 0
      P = 0
      R = 0
      IF (PT .GT. 0) P = PSTK(PT)
      IF (PT .GT. 0) R = RSTK(PT)
      IF (DDT .EQ. 1)then
         WRITE(mline,'('' FINISH'',5I4)') PT,PTZ,P,R,LPT(1)
         call journal(mline)
      endif
      IF (SYM.EQ.COMMA .OR. SYM.EQ.SEMI) GOTO 15
      IF (R.EQ.21 .AND. P.EQ.LPT(1)) GOTO 49
      IF (PT .GT. PTZ) GOTO 91
      GOTO 10
!.......................................................................
!
!     SIMULATE RECURSION
   91 CONTINUE
      CALL mat_clause()
      IF (ERR .GT. 0) GOTO 01
      IF (PT .LE. PTZ) GOTO 15
      R = RSTK(PT)
      IF (R .EQ. 21) GOTO 49
      GOTO (99,99,92,92,92,99,99,99,99,99,99,99,15,15,99,99,99,99,99),R
!
   92 CONTINUE
      CALL mat_expr()
      IF (ERR .GT. 0) GOTO 01
      R = RSTK(PT)
      GOTO (35,55,91,91,91,93,93,99,99,94,94,99,99,99,99,99,99,94,94,46),R
!
   93 CONTINUE
      CALL mat_term()
      IF (ERR .GT. 0) GOTO 01
      R = RSTK(PT)
      GOTO (99,99,99,99,99,92,92,94,94,99,99,99,99,99,95,99,99,99,99),R
!
   94 CONTINUE
      CALL mat_factor()
      IF (ERR .GT. 0) GOTO 01
      R = RSTK(PT)
      GOTO (99,99,99,99,99,99,99,93,93,92,92,94,99,99,99,95,95,92,92),R
!.......................................................................
!
!     CALL ML_MATFNS BY RETURNING TO MAT88
   95 CONTINUE
      if(TOP.LT.1)then
         !call journal('*mat_parse* stack emptied',top)
      else
         IF (FIN.GT.0 .AND. MSTK(TOP).LT.0) CALL mat_err(14)
      endif
      IF (ERR .GT. 0) GOTO 01
      RETURN
!.......................................................................
!
   99 CONTINUE
      CALL mat_err(22)
      GOTO 01
END SUBROUTINE mat_parse
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_comand(id)

integer            :: id(4)
character(len=256) :: mline
integer            :: h(4)
integer,parameter  :: linelen=255
integer,save       :: LRECL
integer            :: cmd(4,17)
integer            :: ch
integer,save       :: cmdl=17
!                   0---------1---------2---------3---------4--------- 5---------6---------7-------
!                   012345678901234567890123456789012345678901234567890123456789012345678901234567
!                   0123456789abcdefghijklmnopqrstuvwxyz ();:+-*/\=.,'<>ABCDEFGHIJKLMNOPQRSTUVWXYZ
integer,parameter  :: a=10, a_up=52
integer,parameter  :: d=13, d_up=55
integer,parameter  :: e=14, e_up=56
integer,parameter  :: z=35, z_up=77

integer,parameter  :: semi=39
integer,parameter  :: comma=48
integer,parameter  :: blank=36
integer,parameter  :: name=1
integer,parameter  :: dot=47

integer            :: chr
integer            :: i
integer            :: ii
integer            :: istat
integer            :: j
integer            :: jj
integer            :: k
integer            :: l

!.......................................................................
!                         1         2         3         4         5
!       COUNT   01234567890123456789012345678901234567890123456789012
!               0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ ();:+-*/\=.,''<>
!
!       CLEAR ELSE  END   EXIT
!       FOR   HELP  IF    LONG
!       QUIT  SEMI
!       SHORT WHAT  WHILE
!       WHO   SH    LALA  SHELL
      DATA CMD/                                                       &
     &  12,21,14,10, 14,21,28,14, 14,23,13,36, 14,33,18,29,           &
     &  15,24,27,36, 17,14,21,25, 18,15,36,36, 21,24,23,16,           &
     &  26,30,18,29, 28,14,22,18,                                     &
     &  28,17,24,27, 32,17,10,29, 32,17,18,21,                        &
     &  32,17,24,36, 28,17,36,36, 21,10,21,10, 28,17,14,21/
      DATA LRECL/LINELEN/
!
      IF (DDT .EQ. 1)call journal('COMAND')
      FUN = 0
      DO K = 1, CMDL
        IF (mat_eqid(ID,CMD(1,K))) GOTO 20
      enddo
      FIN = 0
      RETURN
!
   20 CONTINUE
      IF (CHRA.EQ.COMMA .OR. CHRA.EQ.SEMI .OR. CHRA.EQ.EOL) GOTO 22
      IF ((CHRA.LE.Z.OR.(CHRA.GE.a_up.AND.CHRA.LE.z_up)) .OR. K.EQ.6)GOTO 22  ! if alphanumeric or K=6
      CALL mat_err(16) ! improper command
      RETURN
!
   22 CONTINUE
      FIN = 1
!===================================================================================================================================
      COMAND : select case(k)
!===================================================================================================================================
      case(1) ! COMMAND::CLEAR
      IF ((CHRA.GE.A.AND.CHRA.LE.Z).or.(CHRA.GE.a_up.AND.CHRA.LE.z_up)) then ! alphameric character
         CALL mat_getsym()
         TOP = TOP+1
         MSTK(TOP) = 0
         NSTK(TOP) = 0
         RHS = 0
         CALL mat_stackp(SYN)
         IF (ERR .GT. 0) RETURN
         FIN = 1
      else
         BOT = LSIZE-3
      endif
!===================================================================================================================================
!     COMMAND::FOR, WHILE, IF, ELSE, END
   case(5)   !!   30 continue
      FIN = -11
      GOTO 99
   case(13)  !!   32 continue
      FIN = -12
      GOTO 99
   case(7)   !!   34 continue
      FIN = -13
      GOTO 99
   case(2)   !!   36 continue
      FIN = -14
      GOTO 99
   case(3)   !!   38 continue
      FIN = -15
      GOTO 99
!===================================================================================================================================
   case(4) ! COMMAND::EXIT
      IF (PT .GT. PTZ) FIN = -16
      IF (PT .GT. PTZ) exit COMAND
      K = IDINT(STKR(VSIZE-2))
      call journal('sc',' total flops ',k)

      ! for serendipity's sake
      ii=mat_urand(RAN(1))*9
      select case(ii)
      case(1); call journal(' adios')
      case(2); call journal(' adieu')
      case(3); call journal(' arrivederci')
      case(4); call journal(' au revior')
      case(5); call journal(' so long')
      case(6); call journal(' sayonara')
      case(7); call journal(' auf wiedersehen')
      case default
         call journal(' cheerio')
      end select

      FUN = 99
!===================================================================================================================================
      case(9) ! COMMAND::QUIT
      K = LPT(1) - 7
      IF (K .LE. 0) FUN = 99
      IF (K .LE. 0) exit COMAND
      CALL mat_files(-RIO,BUF)
      LPT(1) = LIN(K+1)
      LPT(4) = LIN(K+2)
      LPT(6) = LIN(K+3)
      PTZ = LIN(K+4)
      RIO = LIN(K+5)
      LCT(4) = LIN(K+6)
      CHRA = BLANK
      SYM = COMMA
      GOTO 99
!===================================================================================================================================
      case(16) ! COMMAND::LALA
      call journal('QUIT SINGING AND GET BACK TO WORK.')
!===================================================================================================================================
      case(17) ! COMMAND::FOO
      call journal(' Your place or mine?')
!===================================================================================================================================
      case(11,8) ! COMMAND::SHORT, LONG
      if(k.eq.11)then
         FMT = 1
      else
         FMT = 2
      endif
      IF (CHRA.EQ.E .OR. CHRA.EQ.D .or. chra.eq.e_up .or. chr.eq.d_up ) FMT = FMT+2
      IF (CHRA .EQ. Z) FMT = 5
      IF (CHRA.EQ.E    .OR. CHRA.EQ.D    .OR. CHRA.EQ.Z   ) CALL mat_getsym()
      IF (CHRA.EQ.E_up .OR. CHRA.EQ.D_up .OR. CHRA.EQ.Z_up) CALL mat_getsym()
!===================================================================================================================================
      case(10) !     COMMAND::SEMI
      LCT(3) = 1 - LCT(3)
!===================================================================================================================================
      case(14) !     COMMAND::WHO
      call journal(' Your current variables are...')
      CALL mat_prntid(IDSTK(1,BOT),LSIZE-BOT+1)
      L = VSIZE-LSTK(BOT)+1
      WRITE(mline,161) L,VSIZE
  161 FORMAT(1X,'using ',I7,' out of ',I7,' elements.')
      call journal(mline)
!===================================================================================================================================
      case(12) !     COMMAND::WHAT
      call journal('The functions and commands are...')
      H(1) = 0
      CALL mat_funs(H)
      CALL mat_prntid(CMD,CMDL-2)
!===================================================================================================================================
      case(15) !     COMMAND::SH
                                   ! call system shell interactively or passing command
      IF (CHRA .eq. EOL )then      ! if next character on stack is end-of-line call interactive shell
          call execute_command_line('/bin/sh',cmdstat=istat) ! call shell interactively
      else                         ! there were characters after SH on the line
          call mat_buf2str(mline,buf(4),lrecl)                              ! pass ENTIRE line
          call execute_command_line(MLINE(:len_trim(mline)),cmdstat=istat)  ! call system shell
          CALL mat_getlin()                                                  ! start a new line because gave all of this one to shell
          if(istat.ne.0)then
             CALL JOURNAL('sc','*SH* RETURN=',ISTAT)
          endif
      endif
!===================================================================================================================================
      case(6) ! COMMAND::HELP
      IF (CHRA .EQ. EOL) THEN
         call journal('Type "help" followed by ...')
         call journal(' INTRO   (To get started)')
         call journal(' NEWS    (recent revisions)')
         H(1) = 0
         CALL mat_funs(H)
         CALL mat_prntid(CMD,CMDL-2)
         J = BLANK+2
         call journal(' ans   EDIT  FILE  FUN   MACRO')
         !-------------------------------------------------
         ! write ALFA(J) to ALFA(a_up-1) one string at a time
         INFINITE: do
            jj=j+16
            jj=min(jj,a_up-1)
            WRITE(MLINE,'(1X,17(A1,1X))')(CHAR(ALFA(I)),I=J,a_up-1)
            call journal(mline)
            IF(jj.ge.a_up-1)exit INFINITE
         enddo INFINITE
         !-------------------------------------------------
         exit COMAND
      ENDIF
!
      CALL mat_getsym()
      IF (SYM .NE. NAME) THEN
         IF (SYM .EQ. 0) SYM = DOT
         H(1) = ALFA(SYM+1)
         H(2) = ALFA(BLANK+1)
         H(3) = ALFA(BLANK+1)
         H(4) = ALFA(BLANK+1)
      ELSE
         DO I = 1, 4
           CH = SYN(I)
           H(I) = ALFA(CH+1)
         enddo
      ENDIF

84    CONTINUE
      IF(HIO .NE. 0) THEN
         READ(HIO,'(a)',END=89) mline   ! read line from help file
         call mat_str2buf(mline,buf,lrecl)  ! convert string to ADE array
         DO I = 1, 4                 ! look for match of 4 chars of topic in first 4 chars
            IF (H(I) .NE. BUF(I)) GOTO 84
         enddo
         call journal(' ')
   86    CONTINUE
         !-------------------------------------------------
         ! find last non-blank character in line
         K = LRECL + 1
   87    CONTINUE
         K = K - 1
         IF(K.LE.0) THEN  ! blank line
            K=1
         ELSEIF (BUF(K) .EQ. ALFA(BLANK+1)) THEN
            GOTO 87
         ENDIF
         !-------------------------------------------------
         call mat_buf2str(mline,buf,k)
         call journal(mline(1:k))
         READ(HIO,'(a)') mline
         call mat_str2buf(mline,buf,lrecl)
         IF (BUF(1) .EQ. ALFA(BLANK+1)) GOTO 86
         CALL mat_files(-HIO,BUF)
         exit COMAND
      ENDIF
!
   89 CONTINUE
      call mat_buf2str(mline,h,4)
      mline=' SORRY, NO HELP ON '//mline(1:4)
      call journal(mline)
      CALL mat_files(-HIO,BUF)
!===================================================================================================================================
      end select COMAND
      CALL mat_getsym()
   99 continue
end subroutine mat_comand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_plot(lplot,x,y,n,p,k)

! ident_29="@(#)M_matrix::mat_plot(3fp): Plot X vs. Y on LPLOT.  If K is nonzero, then P(1),...,P(K) are extra parameters"

integer           :: lplot
integer           :: n
doubleprecision   :: x(n)
doubleprecision   :: y(n)
doubleprecision   :: p(*)
integer           :: k

integer           :: lets(k)
character(len=k)  :: string
doubleprecision   :: xmin,ymin,xmax,ymax,dy,dx,y1,y0
character(len=79) :: pbuf                             ! work space for ascii plot
integer,parameter :: h=20,w=79                        ! h = height, w = width
integer           :: tlun
integer           :: ios
integer           :: ch
integer           :: i
integer           :: j
integer           :: jmax
integer           :: l

!!      if (k .gt. 0) write(lplot,01) (p(i), i=1,k)
!!   01 FORMAT('Extra parameters',1000(f5.1,/))

   xmin = x(1)
   xmax = x(1)
   ymin = y(1)
   ymax = y(1)

   do i = 1, n
      xmin = dmin1(xmin,x(i))
      xmax = dmax1(xmax,x(i))
      ymin = dmin1(ymin,y(i))
      ymax = dmax1(ymax,y(i))
   enddo

   dx = xmax - xmin
   if (dx .eq. 0.0d0) dx = 1.0d0
   dy = ymax - ymin
   write(lplot,'(80x)')
   do l = 1, h
      pbuf(:)=' '  ! blank out the line
      y1 = ymin + (h-l+1)*dy/h
      y0 = ymin + (h-l)*dy/h
      jmax = 1
      do i = 1, n
         if (y(i) .gt. y1) cycle
         if (l.ne.h .and. y(i).le.y0) cycle
         j = 1 + (w-1)*(x(i) - xmin)/dx
         pbuf(j:j) = '*'
         jmax = max0(jmax,j)
      enddo
      write(lplot,'(1x,a)') pbuf(1:jmax)
   enddo

   ! set up the data file
   open(newunit=tlun,file='xy.dat')
   do i=1,n
      write(tlun,*)x(i),y(i)
   enddo
   flush(tlun)

   string=' '
   lets=0
   do i=1,k
      ch=p(i)
      if ((ch.ge.0) .and. (ch.lt.alflq)) then
         lets(i) = alfa(ch+1)
      endif
   enddo
   call mat_buf2str(string,lets,k)

   ! call the external program xy(1) converting the parameters to a string of options
   call journal('sc','xy xy.dat ',trim(string))
   call execute_command_line('xy xy.dat '//trim(string))
   close(unit=tlun,status='delete',iostat=ios)

end subroutine mat_plot
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn1()

! ident_30="@(#)M_matrix::mat_matfn1(3fp): evaluate functions involving gaussian elimination"

doubleprecision   :: dtr(2)
doubleprecision   :: dti(2)
doubleprecision   :: sr(1)
doubleprecision   :: si(1)
doubleprecision   :: rcond
doubleprecision   :: t
doubleprecision   :: t0
doubleprecision   :: t1
doubleprecision   :: eps
character(len=80) ::  mline
integer           :: i
integer           :: info
integer           :: j
integer           :: k
integer           :: ka
integer           :: kb
integer           :: l
integer           :: l2
integer           :: l3
integer           :: li
integer           :: lj
integer           :: lk
integer           :: ll
integer           :: ls
integer           :: lu
integer           :: m
integer           :: m2
integer           :: n
integer           :: n2
integer           :: nn
!
   IF (DDT .EQ. 1) call journal('sc','*MATFN1* ', FIN)
!
   L = LSTK(TOP)
   M = MSTK(TOP)
   N = NSTK(TOP)
!===================================================================================================================================
   select case(FIN)
!===================================================================================================================================
    case(-1) ! MATRIX RIGHT DIVISION, A/A2
      l2 = lstk(top+1)
      m2 = mstk(top+1)
      n2 = nstk(top+1)
      if (m2 .ne. n2) call mat_err(20)
      if (err .gt. 0) return
      if (m*n .eq. 1) goto 16
      if (n .ne. n2) call mat_err(11)
      if (err .gt. 0) return
      l3 = l2 + m2*n2
      err = l3+n2 - lstk(bot)
      if (err .gt. 0) call mat_err(17)
      if (err .gt. 0) return
      call ml_wgeco(stkr(l2),stki(l2),m2,n2,buf,rcond,stkr(l3),stki(l3))
      if (rcond .eq. 0.0d0) call mat_err(19)
      if (err .gt. 0) return
      t = mat_flop(1.0d0 + rcond)
      if (t.eq.1.0d0 .and. fun.ne.21)then
         call journal('WARNING:')
         call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
         WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
         call journal(mline)
      endif
      if (t.eq.1.0d0 .and. fun.eq.21)then
         call journal('WARNING')
         call journal('EIGENVECTORS ARE BADLY CONDITIONED.')
         WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
         call journal(mline)
      endif
      do i = 1, m
         do j = 1, n
            ls = l+i-1+(j-1)*m
            ll = l3+j-1
            stkr(ll) = stkr(ls)
            stki(ll) = -stki(ls)
         enddo
         call ml_wgesl(stkr(l2),stki(l2),m2,n2,buf,stkr(l3),stki(l3),1)
         do j = 1, n
            ll = l+i-1+(j-1)*m
            ls = l3+j-1
            stkr(ll) = stkr(ls)
            stki(ll) = -stki(ls)
         enddo
      enddo
      if (fun .ne. 21) goto 99
!
!     CHECK FOR IMAGINARY ROUNDOFF IN MATRIX FUNCTIONS
      sr(1) = mat_wasum(n*n,stkr(l),stkr(l),1)
      si(1) = mat_wasum(n*n,stki(l),stki(l),1)
      eps = stkr(vsize-4)
      t = eps*sr(1)
      if (ddt .eq. 18)then
         WRITE(WTE,'('' SR,SI,EPS,T'',1P4D13.4)') SR(1),SI(1),EPS,T ! debug 18
      endif
      if (si(1) .le. eps*sr(1)) call mat_rset(n*n,0.0d0,stki(l),1)
      goto 99
!
16    continue
      sr(1) = stkr(l)
      si(1) = stki(l)
      n = n2
      m = n
      mstk(top) = n
      nstk(top) = n
      call mat_wcopy(n*n,stkr(l2),stki(l2),1,stkr(l),stki(l),1)
!===================================================================================================================================
    case(-2) ! MATRIX LEFT DIVISION A BACKSLASH A2
      l2 = lstk(top+1)
      m2 = mstk(top+1)
      n2 = nstk(top+1)
      if (m .ne. n) call mat_err(20)
      if (err .gt. 0) return
      if (m2*n2 .eq. 1) goto 26
      l3 = l2 + m2*n2
      err = l3+n - lstk(bot)
      if (err .gt. 0) call mat_err(17)
      if (err .gt. 0) return
      call ml_wgeco(stkr(l),stki(l),m,n,buf,rcond,stkr(l3),stki(l3))
      if (rcond .eq. 0.0d0) call mat_err(19)
      if (err .gt. 0) return
      t = mat_flop(1.0d0 + rcond)
      if (t .eq. 1.0d0) then
         call journal('WARNING:')
         call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
         WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
         call journal(mline)
      endif
      if (m2 .ne. n) call mat_err(12)
      if (err .gt. 0) return
      do j = 1, n2
         lj = l2+(j-1)*m2
         call ml_wgesl(stkr(l),stki(l),m,n,buf,stkr(lj),stki(lj),0)
      enddo
      nstk(top) = n2
      call mat_wcopy(m2*n2,stkr(l2),stki(l2),1,stkr(l),stki(l),1)
      goto 99
26    continue
      sr(1) = stkr(l2)
      si(1) = stki(l2)
!===================================================================================================================================
   end select
!===================================================================================================================================
   select case(FIN)
!===================================================================================================================================
    case(1) ! COMMAND::INV
      if (m .ne. n) call mat_err(20)
      if (err .gt. 0) return
      if (ddt .eq. 17) goto 32
      do j = 1, n
         do i = 1, n
            ls = l+i-1+(j-1)*n
            t0 = stkr(ls)
            t1 = mat_flop(1.0d0/(dble(i+j-1)))
            if (t0 .ne. t1) goto 32
         enddo
      enddo
      call mat_hilber(stkr(l),n,n)
      call mat_rset(n*n,0.0d0,stki(l),1)
      if (fin .lt. 0) call mat_wscal(n*n,sr(1),si(1),stkr(l),stki(l),1)
      goto 99
32    continue
      l3 = l + n*n
      err = l3+n - lstk(bot)
      if (err .gt. 0) call mat_err(17)
      if (err .gt. 0) return
      call ml_wgeco(stkr(l),stki(l),m,n,buf,rcond,stkr(l3),stki(l3))
      if (rcond .eq. 0.0d0) call mat_err(19)
      if (err .gt. 0) return
      t = mat_flop(1.0d0 + rcond)
      if (t .eq. 1.0d0) then
         call journal('warning:')
         call journal('matrix is close to singular or badly scaled.')
         write(mline,'(''results may be inaccurate. rcond='',1pd13.4)') rcond
         call journal(mline)
      endif
      call ml_wgedi(stkr(l),stki(l),m,n,buf,dtr,dti,stkr(l3),stki(l3),1)
      if (fin .lt. 0) call mat_wscal(n*n,sr(1),si(1),stkr(l),stki(l),1)
!===================================================================================================================================
    case (2) ! COMMAND::DET
      if (m .ne. n) call mat_err(20)
      if (err .gt. 0) return
      call ml_wgefa(stkr(l),stki(l),m,n,buf,info)
      !SUBROUTINE ML_WGEDI(ar,ai,LDA,N,ipvt,detr,deti,workr,worki,JOB)
      call ml_wgedi(stkr(l),stki(l),m,n,buf,dtr,dti,sr(1),si(1),10)
      k = idint(dtr(2))
      ka = iabs(k)+2
      t = 1.0d0
      do i = 1, ka
         t = t/10.0d0
         if (t .eq. 0.0d0) goto 42
      enddo
      stkr(l) = dtr(1)*10.d0**k
      stki(l) = dti(1)*10.d0**k
      mstk(top) = 1
      nstk(top) = 1
      goto 99
42    continue
      if (dti(1) .eq. 0.0d0)then
         write(mline,43) dtr(1),k
43       format(' det =  ',f7.4,7h * 10**,i4)
         call journal(mline)
      else
         write(mline,44) dtr(1),dti(1),k
44       format(' det =  ',f7.4,' + ',f7.4,' i ',7h * 10**,i4)
         call journal(mline)
      endif
      stkr(l) = dtr(1)
      stki(l) = dti(1)
      stkr(l+1) = dtr(2)
      stki(l+1) = 0.0d0
      mstk(top) = 1
      nstk(top) = 2
!===================================================================================================================================
    case(3) ! COMMAND::RCOND
      if (m .ne. n) call mat_err(20)
      if (err .gt. 0) return
      l3 = l + n*n
      err = l3+n - lstk(bot)
      if (err .gt. 0) call mat_err(17)
      if (err .gt. 0) return
      call ml_wgeco(stkr(l),stki(l),m,n,buf,rcond,stkr(l3),stki(l3))
      stkr(l) = rcond
      stki(l) = 0.0d0
      mstk(top) = 1
      nstk(top) = 1
      if (lhs .ne. 1)then
         l = l + 1
         call mat_wcopy(n,stkr(l3),stki(l3),1,stkr(l),stki(l),1)
         top = top + 1
         lstk(top) = l
         mstk(top) = n
         nstk(top) = 1
      endif
!===================================================================================================================================
    case(4) ! COMMAND::LU
      if (m .ne. n) call mat_err(20)
      if (err .gt. 0) return
      call ml_wgefa(stkr(l),stki(l),m,n,buf,info)
      if (lhs .ne. 2) goto 99
      nn = n*n
      if (top+1 .ge. bot) call mat_err(18)
      if (err .gt. 0) return
      top = top+1
      lstk(top) = l + nn
      mstk(top) = n
      nstk(top) = n
      err = l+nn+nn - lstk(bot)
      if (err .gt. 0) call mat_err(17)
      if (err .gt. 0) return
      do kb = 1, n
         k = n+1-kb
         do i = 1, n
            ll = l+i-1+(k-1)*n
            lu = ll + nn
            if (i .le. k) stkr(lu) = stkr(ll)
            if (i .le. k) stki(lu) = stki(ll)
            if (i .gt. k) stkr(lu) = 0.0d0
            if (i .gt. k) stki(lu) = 0.0d0
            if (i .lt. k) stkr(ll) = 0.0d0
            if (i .lt. k) stki(ll) = 0.0d0
            if (i .eq. k) stkr(ll) = 1.0d0
            if (i .eq. k) stki(ll) = 0.0d0
            if (i .gt. k) stkr(ll) = -stkr(ll)
            if (i .gt. k) stki(ll) = -stki(ll)
         enddo
         i = buf(k)
         if (i .eq. k) cycle
         li = l+i-1+(k-1)*n
         lk = l+k-1+(k-1)*n
         call mat_wswap(n-k+1,stkr(li),stki(li),n,stkr(lk),stki(lk),n)
      enddo
!===================================================================================================================================
    case(5) ! COMMAND::HILBERT
      n = idint(stkr(l))
      mstk(top) = n
      nstk(top) = n
      call mat_hilber(stkr(l),n,n)
      call mat_rset(n*n,0.0d0,stki(l),1)
      if (fin .lt. 0) call mat_wscal(n*n,sr(1),si(1),stkr(l),stki(l),1)
!===================================================================================================================================
    case(6) ! COMMAND::CHOLESKY
      if (m .ne. n) call mat_err(20)
      if (err .gt. 0) return
      call mat_wpofa(stkr(l),stki(l),m,n,err)
      if (err .ne. 0) call mat_err(29)
      if (err .gt. 0) return
      do j = 1, n
         ll = l+j+(j-1)*m
         call mat_wset(m-j,0.0d0,0.0d0,stkr(ll),stki(ll),1)
      enddo
!===================================================================================================================================
    case(7) ! COMMAND::RREF
      if (rhs .ge. 2)then
         top = top-1
         l = lstk(top)
         if (mstk(top) .ne. m) call mat_err(5)
         if (err .gt. 0) return
         n = n + nstk(top)
      endif
      call mat_rref(stkr(l),stki(l),m,m,n,stkr(vsize-4))
      nstk(top) = n
!===================================================================================================================================
   end select
!
99 continue
end subroutine mat_matfn1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_matfn2()
integer          :: i
integer          :: inc
integer          :: j
integer          :: job
integer          :: k
integer          :: l
integer          :: l1
integer          :: l2
integer          :: ld
integer          :: le
integer          :: lj
integer          :: ll
integer          :: ls
integer          :: lw
integer          :: m
integer          :: n
integer          :: nn
!
!     EVALUATE ELEMENTARY FUNCTIONS AND FUNCTIONS INVOLVING
!     EIGENVALUES AND EIGENVECTORS
!
      DOUBLEPRECISION TR(1),TI(1),SR,SI,POWR,POWI
      LOGICAL HERM,SCHUR,VECT,HESS
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN2* ', FIN)
!
!     FUNCTIONS/FIN
!     **   SIN  COS ATAN  EXP  SQRT LOG
!      0    1    2    3    4    5    6
!    EIG  SCHU HESS POLY ROOT
!     11   12   13   14   15
!    ABS  ROUN REAL IMAG CONJ
!     21   22   23   24   25
      IF (FIN .NE. 0) GOTO 05
         L = LSTK(TOP+1)
         POWR = STKR(L)
         POWI = STKI(L)
   05 continue
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      IF (FIN .GE. 11 .AND. FIN .LE. 13) GOTO 10
      IF (FIN .EQ. 14 .AND. (M.EQ.1 .OR. N.EQ.1)) GOTO 50
      IF (FIN .EQ. 14) GOTO 10
      IF (FIN .EQ. 15) GOTO 60
      IF (FIN .GT. 20) GOTO 40
      IF (M .EQ. 1 .OR. N .EQ. 1) GOTO 40
!===================================================================================================================================
!     EIGENVALUES AND VECTORS
   10 continue
      IF (M .NE. N) CALL mat_err(20)
      IF (ERR .GT. 0) RETURN
      SCHUR = FIN .EQ. 12
      HESS = FIN .EQ. 13
      VECT = LHS.EQ.2 .OR. FIN.LT.10
      NN = N*N
      L2 = L + NN
      LD = L2 + NN
      LE = LD + N
      LW = LE + N
      ERR = LW+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      CALL mat_wcopy(NN,STKR(L),STKI(L),1,STKR(L2),STKI(L2),1)
!
!     CHECK IF HERMITIAN
      HERM=.FALSE.
      DO J = 1, N
         DO I = 1, J
            LS = L+I-1+(J-1)*N
            LL = L+(I-1)*N+J-1
            HERM = STKR(LL).EQ.STKR(LS) .AND. STKI(LL).EQ.-STKI(LS)
            IF (.NOT. HERM) GOTO 30
         enddo
      enddo
!
!     HERMITIAN EIGENVALUE PROBLEM
      CALL mat_wset(NN,0.0D0,0.0D0,STKR(L),STKI(L),1)
      CALL mat_wset(N,1.0D0,0.0D0,STKR(L),STKI(L),N+1)
      CALL mat_wset(N,0.0D0,0.0D0,STKI(LD),STKI(LE),1)
      JOB = 0
      IF (VECT) JOB = 1
      CALL ML_HTRIDI(N,N,STKR(L2),STKI(L2),STKR(LD),STKR(LE),STKR(LE),STKR(LW))
      IF(.NOT.HESS)CALL ML_IMTQL2(N,N,STKR(LD),STKR(LE),STKR(L),ERR,JOB)
      IF (ERR .GT. 0) CALL mat_err(24)
      IF (ERR .GT. 0) RETURN
      IF (JOB .NE. 0) CALL ML_HTRIBK(N,N,STKR(L2),STKI(L2),STKR(LW),N,STKR(L),STKI(L))
      GOTO 31
!
!     NON-HERMITIAN EIGENVALUE PROBLEM
   30 continue
      CALL ML_CORTH(N,N,1,N,STKR(L2),STKI(L2),STKR(LW),STKI(LW))
      IF (.NOT.VECT .AND. HESS) GOTO 31
      JOB = 0
      IF (VECT) JOB = 2
      IF (VECT .AND. SCHUR) JOB = 1
      IF (HESS) JOB = 3
      CALL ML_COMQR3(N,N,1,N,STKR(LW),STKI(LW),STKR(L2),STKI(L2), STKR(LD),STKI(LD),STKR(L),STKI(L),ERR,JOB)
      IF (ERR .GT. 0) CALL mat_err(24)
      IF (ERR .GT. 0) RETURN
!
!     VECTORS
   31 continue
      IF (.NOT.VECT) GOTO 34
      IF (TOP+1 .GE. BOT) CALL mat_err(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = L2
      MSTK(TOP) = N
      NSTK(TOP) = N
!
!     DIAGONAL OF VALUES OR CANONICAL FORMS
   34 continue
      IF (.NOT.VECT .AND. .NOT.SCHUR .AND. .NOT.HESS) GOTO 37
      DO J = 1, N
         LJ = L2+(J-1)*N
         IF (SCHUR .AND. (.NOT.HERM)) LJ = LJ+J
         IF (HESS .AND. (.NOT.HERM)) LJ = LJ+J+1
         LL = L2+J*N-LJ
         CALL mat_wset(LL,0.0D0,0.0D0,STKR(LJ),STKI(LJ),1)
      enddo
      IF (.NOT.HESS .OR. HERM) CALL mat_wcopy(N,STKR(LD),STKI(LD),1,STKR(L2),STKI(L2),N+1)
      LL = L2+1
      IF (HESS .AND. HERM)CALL mat_wcopy(N-1,STKR(LE+1),STKI(LE+1),1,STKR(LL),STKI(LL),N+1)
      LL = L2+N
      IF (HESS .AND. HERM)CALL mat_wcopy(N-1,STKR(LE+1),STKI(LE+1),1,STKR(LL),STKI(LL),N+1)
      IF (FIN .LT. 10) GOTO 42
      IF (VECT .OR. .NOT.(SCHUR.OR.HESS)) GOTO 99
      CALL mat_wcopy(NN,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      GOTO 99
!
!     VECTOR OF EIGENVALUES
   37 continue
      IF (FIN .EQ. 14) GOTO 52
      CALL mat_wcopy(N,STKR(LD),STKI(LD),1,STKR(L),STKI(L),1)
      NSTK(TOP) = 1
      GOTO 99
!===================================================================================================================================
!     ELEMENTARY FUNCTIONS
!     FOR MATRICES.. X,D = EIG(A), FUN(A) = X*FUN(D)/X
   40 continue
      INC = 1
      N = M*N
      L2 = L
      GOTO 44
   42 continue
      INC = N+1
   44 continue
      DO J = 1, N
        LS = L2+(J-1)*INC
        SR = STKR(LS)
        SI = STKI(LS)
        TI = 0.0D0
        IF (FIN .NE. 0) GOTO 45
          CALL mat_wlog(SR,SI,SR,SI)
          CALL mat_wmul(SR,SI,POWR,POWI,SR,SI)
          TR(1) = DEXP(SR)*DCOS(SI)
          TI(1) = DEXP(SR)*DSIN(SI)
   45   IF (FIN .EQ. 1) TR(1) = DSIN(SR)*DCOSH(SI)
        IF (FIN .EQ. 1) TI(1) = DCOS(SR)*DSINH(SI)
        IF (FIN .EQ. 2) TR(1) = DCOS(SR)*DCOSH(SI)
        IF (FIN .EQ. 2) TI(1) = (-DSIN(SR))*DSINH(SI)
        IF (FIN .EQ. 3) CALL mat_watan(SR,SI,TR(1),TI(1))
        IF (FIN .EQ. 4) TR(1) = DEXP(SR)*DCOS(SI)
        IF (FIN .EQ. 4) TI(1) = DEXP(SR)*DSIN(SI)
        IF (FIN .EQ. 5) CALL mat_wsqrt(SR,SI,TR(1),TI(1))
        IF (FIN .EQ. 6) CALL mat_wlog(SR,SI,TR(1),TI(1))
        IF (FIN .EQ. 21) TR(1) = mat_pythag(SR,SI)
        IF (FIN .EQ. 22) TR(1) = mat_round(SR)
        IF (FIN .EQ. 23) TR(1) = SR
        IF (FIN .EQ. 24) TR(1) = SI
        IF (FIN .EQ. 25) TR(1) = SR
        IF (FIN .EQ. 25) TI(1) = -SI
        IF (ERR .GT. 0) RETURN
        STKR(LS) = mat_flop(TR(1))
        STKI(LS) = 0.0D0
        IF (TI(1) .NE. 0.0D0) STKI(LS) = mat_flop(TI(1))
      enddo
      IF (INC .EQ. 1) GOTO 99
      DO J = 1, N
        LS = L2+(J-1)*INC
        SR = STKR(LS)
        SI = STKI(LS)
        LS = L+(J-1)*N
        LL = L2+(J-1)*N
        CALL mat_wcopy(N,STKR(LS),STKI(LS),1,STKR(LL),STKI(LL),1)
        CALL mat_wscal(N,SR,SI,STKR(LS),STKI(LS),1)
      enddo
!     SIGNAL MATFN1 TO DIVIDE BY EIGENVECTORS
      FUN = 21
      FIN = -1
      TOP = TOP-1
      GOTO 99
!===================================================================================================================================
!     POLY
!     FORM POLYNOMIAL WITH GIVEN VECTOR AS ROOTS
   50 continue
      N = MAX0(M,N)
      LD = L+N+1
      CALL mat_wcopy(N,STKR(L),STKI(L),1,STKR(LD),STKI(LD),1)
!
!     FORM CHARACTERISTIC POLYNOMIAL
   52 continue
      CALL mat_wset(N+1,0.0D0,0.0D0,STKR(L),STKI(L),1)
      STKR(L) = 1.0D0
      DO J = 1, N
         CALL matX_waxpy(J,-STKR(LD),-STKI(LD),STKR(L),STKI(L),-1, STKR(L+1),STKI(L+1),-1)
         LD = LD+1
      enddo
      MSTK(TOP) = N+1
      NSTK(TOP) = 1
      GOTO 99
!===================================================================================================================================
!     ROOTS
   60 continue
      LL = L+M*N
      STKR(LL) = -1.0D0
      STKI(LL) = 0.0D0
      K = -1
   61 continue
      K = K+1
      L1 = L+K
      IF (DABS(STKR(L1))+DABS(STKI(L1)) .EQ. 0.0D0) GOTO 61
      N = MAX0(M*N - K-1, 0)
      IF (N .LE. 0) GOTO 65
      L2 = L1+N+1
      LW = L2+N*N
      ERR = LW+N - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      CALL mat_wset(N*N+N,0.0D0,0.0D0,STKR(L2),STKI(L2),1)
      DO J = 1, N
         LL = L2+J+(J-1)*N
         STKR(LL) = 1.0D0
         LS = L1+J
         LL = L2+(J-1)*N
         CALL mat_wdiv(-STKR(LS),-STKI(LS),STKR(L1),STKI(L1), STKR(LL),STKI(LL))
         IF (ERR .GT. 0) RETURN
      enddo
      CALL ML_COMQR3(N,N,1,N,STKR(LW),STKI(LW),STKR(L2),STKI(L2),STKR(L),STKI(L),TR,TI,ERR,0)
      IF (ERR .GT. 0) CALL mat_err(24)
      IF (ERR .GT. 0) RETURN
   65 continue
      MSTK(TOP) = N
      NSTK(TOP) = 1
      GOTO 99
!===================================================================================================================================
   99 continue
END SUBROUTINE mat_matfn2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn3()

! ident_31="@(#)M_matrix::mat_matfn3(3fp): evaluate functions involving singular value decomposition"

integer         :: i
integer         :: j
integer         :: jb
integer         :: job
integer         :: k
integer         :: l
integer         :: l1
integer         :: l2
integer         :: ld
integer         :: li
integer         :: lj
integer         :: ll
integer         :: ls
integer         :: lu
integer         :: lv
integer         :: m
integer         :: mn
integer         :: n
logical         :: fro,inf
doubleprecision :: p,s,t(1,1),tol,eps
!
   if (ddt .eq. 1) call journal('sc','*MATFN3* ', FIN)
!
   if (fin.eq.1 .and. rhs.eq.2) top = top-1
   l = lstk(top)
   m = mstk(top)
   n = nstk(top)
   mn = m*n
   !      SVD PINV COND NORM RANK
   !        1    2    3    4    5
   FUN3: select case(fin)
!===================================================================================================================================
    case(3) ! COMMAND::COND
      ld = l + m*n
      l1 = ld + min0(m+1,n)
      l2 = l1 + n
      err = l2+min0(m,n) - lstk(bot)
      if (err .gt. 0) call mat_err(17)
      if (err .gt. 0) return
      call ml_wsvdc(stkr(l),stki(l),m,m,n,stkr(ld),stki(ld),stkr(l1),stki(l1),t,t,1,t,t,1,stkr(l2),stki(l2),0,err)
      if (err .ne. 0) call mat_err(24)
      if (err .gt. 0) return
      s = stkr(ld)
      ld = ld + min0(m,n) - 1
      t(1,1) = stkr(ld)
      if (t(1,1) .ne. 0.0d0) then
         stkr(l) = mat_flop(s/t(1,1))
         stki(l) = 0.0d0
         mstk(top) = 1
         nstk(top) = 1
      else
         call journal(' CONDITION IS INFINITE')
         mstk(top) = 0
      endif
!===================================================================================================================================
    case(4) ! command::norm
      p = 2.0d0
      inf = .false.
      if (rhs .eq. 2)then
         fro = idint(stkr(l)).eq.15 .and. mn.gt.1
         inf = idint(stkr(l)).eq.18 .and. mn.gt.1
         if (.not. fro) p = stkr(l)
         top = top-1
         l = lstk(top)
         m = mstk(top)
         n = nstk(top)
         mn = m*n
         if (fro) m = mn
         if (fro) n = 1
      endif
      if (m .gt. 1 .and. n .gt. 1) goto 40
      if (p .eq. 1.0d0) goto 36
      if (p .eq. 2.0d0) goto 38
      i = mat_iwamax(mn,stkr(l),stki(l),1) + l - 1
      s = dabs(stkr(i)) + dabs(stki(i))
      if (inf .or. s .eq. 0.0d0) goto 49
      t(1,1) = 0.0d0
      do i = 1, mn
         ls = l+i-1
         t(1,1) = mat_flop(t(1,1) + (mat_pythag(stkr(ls),stki(ls))/s)**p)
      enddo
      if (p .ne. 0.0d0) p = 1.0d0/p
      s = mat_flop(s*t(1,1)**p)
      GOTO 49
36    continue
      S = mat_wasum(MN,STKR(L),STKI(L),1)
      GOTO 49
38    continue
      S = mat_wnrm2(MN,STKR(L),STKI(L),1)
      GOTO 49
40    continue !     MATRIX NORM
      IF (INF) GOTO 43
      IF (P .EQ. 1.0D0) GOTO 46
      IF (P .NE. 2.0D0) CALL mat_err(23)
      IF (ERR .GT. 0) RETURN
      LD = L + M*N
      L1 = LD + MIN0(M+1,N)
      L2 = L1 + N
      ERR = L2+MIN0(M,N) - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      call ml_wsvdc(stkr(l),stki(l),m,m,n,stkr(ld),stki(ld),stkr(l1),stki(l1),t,t,1,t,t,1,stkr(l2),stki(l2),0,err)
      IF (ERR .NE. 0) CALL mat_err(24)
      IF (ERR .GT. 0) RETURN
      S = STKR(LD)
      GOTO 49
43    continue
      S = 0.0D0
      DO I = 1, M
         LI = L+I-1
         T(1,1) = mat_wasum(N,STKR(LI),STKI(LI),M)
         S = DMAX1(S,T(1,1))
      enddo
      GOTO 49
46    continue
      S = 0.0D0
      DO J = 1, N
         LJ = L+(J-1)*M
         T(1,1) = mat_wasum(M,STKR(LJ),STKI(LJ),1)
         S = DMAX1(S,T(1,1))
      enddo
      GOTO 49
49    continue
      STKR(L) = S
      STKI(L) = 0.0D0
      MSTK(TOP) = 1
      NSTK(TOP) = 1
!===================================================================================================================================
    case(1) !     COMMAND::SVD
      IF (LHS .EQ. 3)then
         K = M
         IF (RHS .EQ. 2) K = MIN0(M,N)
         LU = L + M*N
         LD = LU + M*K
         LV = LD + K*N
         L1 = LV + N*N
         L2 = L1 + N
         ERR = L2+MIN0(M,N) - LSTK(BOT)
         IF (ERR .GT. 0) CALL mat_err(17)
         IF (ERR .GT. 0) RETURN
         JOB = 11
         IF (RHS .EQ. 2) JOB = 21
         call ml_wsvdc(stkr(l),stki(l),m,m,n,stkr(ld),stki(ld),stkr(l1),stki(l1),stkr(lu),stki(lu),m,stkr(lv),stki(lv), &
         &        n,stkr(l2),stki(l2),job,err)
         DO JB = 1, N
            DO I = 1, K
               J = N+1-JB
               LL = LD+I-1+(J-1)*K
               IF (I.NE.J) STKR(LL) = 0.0D0
               STKI(LL) = 0.0D0
               LS = LD+I-1
               IF (I.EQ.J) STKR(LL) = STKR(LS)
               LS = L1+I-1
               IF (ERR.NE.0 .AND. I.EQ.J-1) STKR(LL) = STKR(LS)
            enddo
         enddo
         IF (ERR .NE. 0) CALL mat_err(24)
         ERR = 0
         CALL mat_wcopy(M*K+K*N+N*N,STKR(LU),STKI(LU),1,STKR(L),STKI(L),1)
         MSTK(TOP) = M
         NSTK(TOP) = K
         IF (TOP+1 .GE. BOT) CALL mat_err(18)
         IF (ERR .GT. 0) RETURN
         TOP = TOP+1
         LSTK(TOP) = L + M*K
         MSTK(TOP) = K
         NSTK(TOP) = N
         IF (TOP+1 .GE. BOT) CALL mat_err(18)
         IF (ERR .GT. 0) RETURN
         TOP = TOP+1
         LSTK(TOP) = L + M*K + K*N
         MSTK(TOP) = N
         NSTK(TOP) = N
      else
         LD = L + M*N
         L1 = LD + MIN0(M+1,N)
         L2 = L1 + N
         ERR = L2+MIN0(M,N) - LSTK(BOT)
         IF (ERR .GT. 0) CALL mat_err(17)
         IF (ERR .GT. 0) RETURN
         call ml_wsvdc(stkr(l),stki(l),m,m,n,stkr(ld),stki(ld),stkr(l1),stki(l1),t,t,1,t,t,1,stkr(l2),stki(l2),0,err)
         IF (ERR .NE. 0) CALL mat_err(24)
         IF (ERR .GT. 0) RETURN
         K = MIN0(M,N)
         CALL mat_wcopy(K,STKR(LD),STKI(LD),1,STKR(L),STKI(L),1)
         MSTK(TOP) = K
         NSTK(TOP) = 1
      endif
!===================================================================================================================================
    case(2,5) ! COMMAND::PINV AND RANK
      TOL = -1.0D0
      IF (RHS .EQ. 2) then
         TOL = STKR(L)
         TOP = TOP-1
         L = LSTK(TOP)
         M = MSTK(TOP)
         N = NSTK(TOP)
      endif
      LU = L + M*N
      LD = LU + M*M
      IF (FIN .EQ. 5) LD = L + M*N
      LV = LD + M*N
      L1 = LV + N*N
      IF (FIN .EQ. 5) L1 = LD + N
      L2 = L1 + N
      ERR = L2+MIN0(M,N) - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      IF (FIN .EQ. 2) JOB = 11
      IF (FIN .EQ. 5) JOB = 0
      CALL ML_WSVDC(STKR(L),STKI(L),M,M,N,STKR(LD),STKI(LD),STKR(L1),STKI(L1),STKR(LU),STKI(LU),M,STKR(LV),STKI(LV), &
      &        N,STKR(L2),STKI(L2),JOB,ERR)
      IF (ERR .NE. 0) CALL mat_err(24)
      IF (ERR .GT. 0) RETURN
      EPS = STKR(VSIZE-4)
      IF (TOL .LT. 0.0D0) TOL = mat_flop(dble(MAX0(M,N))*EPS*STKR(LD))
      MN = MIN0(M,N)
      K = 0
      DO J = 1, MN
         LS = LD+J-1
         S = STKR(LS)
         IF (S .LE. TOL) exit
         K = J
         LL = LV+(J-1)*N
         IF (FIN .EQ. 2) CALL mat_wrscal(N,1.0D0/S,STKR(LL),STKI(LL),1)
      enddo
      if (fin .ne. 5) then
         do j = 1, m
            do i = 1, n
               ll = l+i-1+(j-1)*n
               l1 = lv+i-1
               l2 = lu+j-1
               stkr(ll) = mat_wdotcr(k,stkr(l2),stki(l2),m,stkr(l1),stki(l1),n)
               stki(ll) = mat_wdotci(k,stkr(l2),stki(l2),m,stkr(l1),stki(l1),n)
            enddo
         enddo
         mstk(top) = n
         nstk(top) = m
      else
         stkr(l) = dble(k)
         stki(l) = 0.0d0
         mstk(top) = 1
         nstk(top) = 1
      endif
!===================================================================================================================================
   end select FUN3
!
end subroutine mat_matfn3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_matfn4()

! ident_32="@(#)M_matrix::mat_matfn4(3fp): evaluate functions involving qr decomposition (least squares)"

integer           :: info
integer           :: j
integer           :: jb
integer           :: job
integer           :: k
integer           :: l
integer           :: l2
integer           :: l3
integer           :: l4
integer           :: le
integer           :: ll
integer           :: ls
integer           :: m
integer           :: m2
integer           :: mm
integer           :: mn
integer           :: n
integer           :: n2
character(len=81) ::  mline
DOUBLEPRECISION   :: T(1),TOL,EPS
INTEGER,parameter :: QUOTE= 49
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN4* ', FIN)
!
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)
      IF (FIN .EQ. -1) GOTO 10
      IF (FIN .EQ. -2) GOTO 20
      GOTO 40
!
!     RECTANGULAR MATRIX RIGHT DIVISION, A/A2
   10 continue
      L2 = LSTK(TOP+1)
      M2 = MSTK(TOP+1)
      N2 = NSTK(TOP+1)
      TOP = TOP + 1
      IF (N.GT.1 .AND. N.NE.N2) CALL mat_err(11)
      IF (ERR .GT. 0) RETURN
      CALL mat_stack1(QUOTE)
      IF (ERR .GT. 0) RETURN
      LL = L2+M2*N2
      CALL mat_wcopy(M*N,STKR(L),STKI(L),1,STKR(LL),STKI(LL),1)
      CALL mat_wcopy(M*N+M2*N2,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      LSTK(TOP) = L+M2*N2
      MSTK(TOP) = M
      NSTK(TOP) = N
      CALL mat_stack1(QUOTE)
      IF (ERR .GT. 0) RETURN
      TOP = TOP - 1
      M = N2
      N = M2
      GOTO 20
!
!     RECTANGULAR MATRIX LEFT DIVISION A BACKSLASH A2
!
   20 continue
      L2 = LSTK(TOP+1)
      M2 = MSTK(TOP+1)
      N2 = NSTK(TOP+1)
      IF (M2*N2 .GT. 1) GOTO 21
        M2 = M
        N2 = M
        ERR = L2+M*M - LSTK(BOT)
        IF (ERR .GT. 0) CALL mat_err(17)
        IF (ERR .GT. 0) RETURN
        CALL mat_wset(M*M-1,0.0D0,0.0D0,STKR(L2+1),STKI(L2+1),1)
        CALL mat_wcopy(M,STKR(L2),STKI(L2),0,STKR(L2),STKI(L2),M+1)
   21 continue
      IF (M2 .NE. M) CALL mat_err(12)
      IF (ERR .GT. 0) RETURN
      L3 = L2 + MAX0(M,N)*N2
      L4 = L3 + N
      ERR = L4 + N - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      IF (M .GT. N) GOTO 23
      DO JB = 1, N2
        J = N+1-JB
        LS = L2 + (J-1)*M
        LL = L2 + (J-1)*N
        CALL mat_wcopy(M,STKR(LS),STKI(LS),-1,STKR(LL),STKI(LL),-1)
      enddo
   23 continue
      DO J = 1, N
        BUF(J) = 0
      enddo
      CALL ML_WQRDC(STKR(L),STKI(L),M,M,N,STKR(L4),STKI(L4),BUF,STKR(L3),STKI(L3),1)
      K = 0
      EPS = STKR(VSIZE-4)
      T(1) = DABS(STKR(L))+DABS(STKI(L))
      TOL = mat_flop(dble(MAX0(M,N))*EPS*T(1))
      MN = MIN0(M,N)
      DO J = 1, MN
        LS = L+J-1+(J-1)*M
        T(1) = DABS(STKR(LS)) + DABS(STKI(LS))
        IF (T(1) .GT. TOL) K = J
      enddo
      IF (K .LT. MN) then
         WRITE(mline,'('' RANK DEFICIENT,  RANK ='',I4,'',  TOL ='',1PD13.4)') K,TOL
         call journal(mline)
      endif
      MN = MAX0(M,N)
      DO J = 1, N2
        LS = L2+(J-1)*MN
        CALL ML_WQRSL(STKR(L),STKI(L),M,M,K,STKR(L4),STKI(L4),STKR(LS),STKI(LS),T,T,STKR(LS),STKI(LS),STKR(LS), &
       & STKI(LS),T,T,T,T,100,INFO)
        LL = LS+K
        CALL mat_wset(N-K,0.0D0,0.0D0,STKR(LL),STKI(LL),1)
      enddo
      DO J = 1, N
        BUF(J) = -BUF(J)
      enddo
      DO J = 1, N
        IF (BUF(J) .GT. 0) cycle
        K = -BUF(J)
        BUF(J) = K
   33   CONTINUE
          IF (K .EQ. J) cycle
          LS = L2+J-1
          LL = L2+K-1
          CALL mat_wswap(N2,STKR(LS),STKI(LS),MN,STKR(LL),STKI(LL),MN)
          BUF(K) = -BUF(K)
          K = BUF(K)
          GOTO 33
      enddo
      DO J = 1, N2
        LS = L2+(J-1)*MN
        LL = L+(J-1)*N
        CALL mat_wcopy(N,STKR(LS),STKI(LS),1,STKR(LL),STKI(LL),1)
      enddo
      MSTK(TOP) = N
      NSTK(TOP) = N2
      IF (FIN .EQ. -1) CALL mat_stack1(QUOTE)
      IF (ERR .GT. 0) RETURN
      GOTO 99
!
!     QR
!
   40 continue
      MM = MAX0(M,N)
      LS = L + MM*MM
      IF (LHS.EQ.1 .AND. FIN.EQ.1) LS = L
      LE = LS + M*N
      L4 = LE + MM
      ERR = L4+MM - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
      IF (LS.NE.L) CALL mat_wcopy(M*N,STKR(L),STKI(L),1,STKR(LS),STKI(LS),1)
      JOB = 1
      IF (LHS.LT.3) JOB = 0
      DO J = 1, N
        BUF(J) = 0
      enddo
      CALL ML_WQRDC(STKR(LS),STKI(LS),M,M,N,STKR(L4),STKI(L4),BUF,STKR(LE),STKI(LE),JOB)
      IF (LHS.EQ.1 .AND. FIN.EQ.1) GOTO 99
      CALL mat_wset(M*M,0.0D0,0.0D0,STKR(L),STKI(L),1)
      CALL mat_wset(M,1.0D0,0.0D0,STKR(L),STKI(L),M+1)
      DO J = 1, M
        LL = L+(J-1)*M
        CALL ML_WQRSL(STKR(LS),STKI(LS),M,M,N,STKR(L4),STKI(L4),   &
     &             STKR(LL),STKI(LL),STKR(LL),STKI(LL),T,T,        &
     &             T,T,T,T,T,T,10000,INFO)
      enddo
      IF (FIN .EQ. 2) GOTO 99
      NSTK(TOP) = M
      DO J = 1, N
        LL = LS+J+(J-1)*M
        CALL mat_wset(M-J,0.0D0,0.0D0,STKR(LL),STKI(LL),1)
      enddo
      IF (TOP+1 .GE. BOT) CALL mat_err(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = LS
      MSTK(TOP) = M
      NSTK(TOP) = N
      IF (LHS .EQ. 2) GOTO 99
      CALL mat_wset(N*N,0.0D0,0.0D0,STKR(LE),STKI(LE),1)
      DO J = 1, N
        LL = LE+BUF(J)-1+(J-1)*N
        STKR(LL) = 1.0D0
      enddo
      IF (TOP+1 .GE. BOT) CALL mat_err(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
      LSTK(TOP) = LE
      MSTK(TOP) = N
      NSTK(TOP) = N
      GOTO 99
!
   99 continue
END SUBROUTINE mat_matfn4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn5()

! ident_33="@(#)M_matrix::mat_matfn5(3fp):file handling and other I/O"

character(len=256)  :: mline
character(len=256)  :: errmsg
character(len=1024) :: name
character(len=1)    :: ch_char
integer             :: temp_lun
integer             :: ios

integer,save        :: flag=0
integer,save        :: blank=36
integer,save        :: plus=41
integer,save        :: minus=42
integer,save        :: quote=49
integer,save        :: semi=39
integer,save        :: lrat=5
integer,save        :: mrat=100
integer             :: ch,top2,ch1(1)
integer             :: id(4)
doubleprecision     :: eps,b,s,t,tdum(2)
logical             :: text
integer             :: i
integer             :: img
integer             :: j
integer             :: job
integer             :: k
integer             :: l
integer             :: l2
integer             :: ll
integer             :: ls
integer             :: lun
integer             :: lunit
integer             :: lw
integer             :: lx
integer             :: ly
integer             :: m
integer             :: mn
integer             :: n
!
      IF (DDT .EQ. 1) call journal('sc','*MATFN5* ',FIN)
!     FUNCTIONS/FIN
!     EXEC SAVE LOAD PRIN DIAR DISP BASE LINE CHAR PLOT RAT  DEBU DOC
!      1    2    3    4    5    6    7    8    9   10   11   12   13
      L = LSTK(TOP)
      M = MSTK(TOP)
      N = NSTK(TOP)

      select case(fin)
      case(:5,13)
         ! CONVERT FILE NAME
         MN = M*N
         FLAG = 3
         IF (SYM .EQ. SEMI) FLAG = 0
         IF (RHS .GE. 2) THEN
            FLAG = IDINT(STKR(L))
            TOP2 = TOP
            TOP = TOP-1
            L = LSTK(TOP)
            MN = MSTK(TOP)*NSTK(TOP)
         ENDIF
         LUN = -1
         IF (MN.EQ.1 .AND. STKR(L).LT.10.0D0) LUN = IDINT(STKR(L))
         IF (LUN .LT. 0) THEN
             DO J = 1, 32
                LS = L+J-1
                IF (J .LE. MN) CH = IDINT(STKR(LS))
                IF (J .GT. MN) CH = BLANK
                IF (CH.LT.0 .OR. CH.GE.alflq) CALL mat_err(38)
                IF (ERR .GT. 0) RETURN
                IF (CASE .EQ. 0) BUF(J) = ALFA(CH+1)
                IF (CASE .EQ. 1) BUF(J) = ALFB(CH+1)
             enddo
         ENDIF
      case(6:12)
      case default
      end select
!===================================================================================================================================
      FUN5 : select case(fin)
!===================================================================================================================================
      case(1) !     COMMAND::EXEC
      IF (LUN .EQ. 0) THEN
!     EXEC(0)
         RIO = RTE
         ERR = 99
      else
         K = LPT(6)
         LIN(K+1) = LPT(1)
         LIN(K+2) = LPT(3)
         LIN(K+3) = LPT(6)
         LIN(K+4) = PTZ
         LIN(K+5) = RIO
         LIN(K+6) = LCT(4)
         LPT(1) = K + 7
         LCT(4) = FLAG
         PTZ = PT - 4
         IF (RIO .EQ. RTE) RIO = 12
         RIO = RIO + 1
         IF (LUN .GT. 0) RIO = LUN
         IF (LUN .LT. 0) CALL mat_files(RIO,BUF)
         IF (FLAG .GE. 4)call journal(' PAUSE MODE. ENTER BLANK LINES.')
         SYM = EOL
         MSTK(TOP) = 0
      endif
!===================================================================================================================================
      case(4) ! COMMAND::PRINT
      K = WTE
      WTE = LUN
      IF (LUN .LT. 0) WTE = 7
      IF (LUN .LT. 0) CALL mat_files(WTE,BUF)

      L = LCT(2)
      LCT(2) = 9999
      IF (RHS .GT. 1) CALL mat_print(SYN,TOP2)
      LCT(2) = L

      WTE = K
      MSTK(TOP) = 0
!===================================================================================================================================
      case(5) ! COMMAND::DIARY
      IF (LUN < 0) CALL mat_files(8,BUF)
      MSTK(TOP) = 0
!===================================================================================================================================
      case(2) ! COMMAND::SAVE
      IF (LUN .LT. 0) LUNIT = 1
      IF (LUN .LT. 0) CALL mat_files(LUNIT,BUF)
      IF (LUN .GT. 0) LUNIT = LUN
      K = LSIZE-4
      IF (K .LT. BOT) K = LSIZE
      IF (RHS .EQ. 2) K = TOP2
      IF (RHS .EQ. 2) CALL mat_putid(IDSTK(1,K),SYN)
   32 CONTINUE
      L = LSTK(K)
      M = MSTK(K)
      N = NSTK(K)
      DO I = 1, 4
         J = IDSTK(I,K)+1
         BUF(I) = ALFA(J)
      enddo
      IMG = 0
      IF (mat_wasum(M*N,STKI(L),STKI(L),1) .NE. 0.0D0) IMG = 1
      IF(FE .EQ. 0)CALL mat_savlod(LUNIT,BUF,M,N,IMG,0,STKR(L),STKI(L))
      K = K-1
      IF (K .GE. BOT) GOTO 32
      CALL mat_files(-LUNIT,BUF)
      MSTK(TOP) = 0
!===================================================================================================================================
      case(3) ! COMMAND::LOAD
      IF (LUN .LT. 0) LUNIT = 2
      IF (LUN .LT. 0) CALL mat_files(LUNIT,BUF) ! open the unit
      IF (LUN .GT. 0) LUNIT = LUN
   36 CONTINUE
      JOB = LSTK(BOT) - L
      IF(FE .EQ. 0)CALL mat_savlod(LUNIT,ID,MSTK(TOP),NSTK(TOP),IMG,JOB,STKR(L),STKI(L))
      MN = MSTK(TOP)*NSTK(TOP)
      IF (MN .EQ. 0) GOTO 39
      IF (IMG .EQ. 0) CALL mat_rset(MN,0.0D0,STKI(L),1)
      DO I = 1, 4
         J = 0
   37    CONTINUE
         J = J+1
         IF (ID(I).NE.ALFA(J) .AND. J.LE.BLANK) GOTO 37
         ID(I) = J-1
      enddo
      SYM = SEMI
      RHS = 0
      CALL mat_stackp(ID)
      TOP = TOP + 1
      GOTO 36

   39 CONTINUE
      CALL mat_files(-LUNIT,BUF) ! close unit
      MSTK(TOP) = 0
!===================================================================================================================================
      case(11) !     COMMAND::RAT
      if (rhs .ne. 2) then
         mn = m*n
         l2 = l
         if (lhs .eq. 2) l2 = l + mn
         lw = l2 + mn
         err = lw + lrat - lstk(bot)
         if (err .gt. 0) call mat_err(17)
         if (err .gt. 0) return
         if (lhs .eq. 2) top = top + 1
         lstk(top) = l2
         mstk(top) = m
         nstk(top) = n
         call mat_rset(lhs*mn,0.0d0,stki(l),1)
         do i = 1, mn
            call mat_rat(stkr(l),lrat,mrat,s,t,stkr(lw))
            stkr(l) = s
            stkr(l2) = t
            if (lhs .eq. 1) stkr(l) = mat_flop(s/t)
            l = l + 1
            l2 = l2 + 1
         enddo
      else
         mrat = idint(stkr(l))
         lrat = idint(stkr(l-1))
         top = top - 1
         mstk(top) = 0
      endif
!===================================================================================================================================
      case(9) !     COMMAND::CHAR
      K = IABS(IDINT(STKR(L)))
      IF (M*N.NE.1 .OR. K.GE.alflq) CALL mat_err(36)
      IF (ERR .GT. 0) exit FUN5
      CH = ALFA(K+1)
      IF (STKR(L) .LT. 0.0D0) CH = ALFB(K+1)
      WRITE(mline,'('' REPLACE CHARACTER '',A1)') CHAR(CH)
      call journal(mline)
      READ(RTE,'(A1)') CH_CHAR
      call mat_str2buf(ch_char,ch1,1); ch=ch1(1)
      IF (STKR(L) .GE. 0.0D0) ALFA(K+1) = CH
      IF (STKR(L) .LT. 0.0D0) ALFB(K+1) = CH
      MSTK(TOP) = 0
!===================================================================================================================================
      case(6,7) !     COMMAND::DISP
60    continue
      call journal(' ')
      if (FIN.eq.7)goto 65
      IF (RHS .EQ. 2) GOTO 65
      MN = M*N
      TEXT = .TRUE.
      DO I = 1, MN
        LS = L+I-1
        CH = IDINT(STKR(LS))
        TEXT = TEXT .AND. (CH.GE.0) .AND. (CH.LT.alflq)
        TEXT = TEXT .AND. (dble(CH).EQ.STKR(LS))
      enddo

      DO I = 1, M
         DO J = 1, N
           LS = L+I-1+(J-1)*M
           IF (STKR(LS) .EQ. 0.0D0) CH = BLANK
           IF (STKR(LS) .GT. 0.0D0) CH = PLUS
           IF (STKR(LS) .LT. 0.0D0) CH = MINUS
           IF (TEXT) CH = IDINT(STKR(LS))
           BUF(J) = ALFA(CH+1)
         enddo
         call mat_buf2str(mline,buf,n)
         call journal(mline)
      enddo
      MSTK(TOP) = 0
      exit FUN5
!. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     COMMAND::BASE
   65 CONTINUE
      IF (RHS .NE. 2) CALL mat_err(39)
      IF (STKR(L) .LE. 1.0D0) CALL mat_err(36)
      IF (ERR .GT. 0) exit FUN5
      B = STKR(L)
      L2 = L
      TOP = TOP-1
      RHS = 1
      L = LSTK(TOP)
      M = MSTK(TOP)*NSTK(TOP)
      EPS = STKR(VSIZE-4)
      DO I = 1, M
         LS = L2+(I-1)*N
         LL = L+I-1
         CALL mat_base(STKR(LL),B,EPS,STKR(LS),N)
      enddo
      CALL mat_rset(M*N,0.0D0,STKI(L2),1)
      CALL mat_wcopy(M*N,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
      MSTK(TOP) = N
      NSTK(TOP) = M
      CALL mat_stack1(QUOTE)
      IF (FIN .EQ. 6) GOTO 60
!===================================================================================================================================
      case(8)
!     COMMAND::LINES
      LCT(2) = IDINT(STKR(L))
      MSTK(TOP) = 0
!===================================================================================================================================
      case(10) !     COMMAND::PLOT
      IF (RHS .GE. 2) GOTO 82
      N = M*N
      DO I = 1, N
         LL = L+I-1
         STKI(LL) = dble(I)
      enddo
      CALL mat_plot(WTE,STKI(L),STKR(L),N,TDUM,0)
      MSTK(TOP) = 0
      exit FUN5
!. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

   82 continue
      IF (RHS .EQ. 2) K = 0
      IF (RHS .EQ. 3) K = M*N
      IF (RHS .GT. 3) K = RHS - 2
      TOP = TOP - (RHS - 1)
      N = MSTK(TOP)*NSTK(TOP)
      IF (MSTK(TOP+1)*NSTK(TOP+1) .NE. N) CALL mat_err(5)
      IF (ERR .GT. 0) exit FUN5
      LX = LSTK(TOP)
      LY = LSTK(TOP+1)
      IF (RHS .GT. 3) L = LSTK(TOP+2)
      CALL mat_plot(WTE,STKR(LX),STKR(LY),N,STKR(L),K)
      MSTK(TOP) = 0
!===================================================================================================================================
      case(12) !     COMMAND::DEBUG
      DDT = IDINT(STKR(L))
      call journal('sc',' DEBUG ',DDT)
      MSTK(TOP) = 0
!===================================================================================================================================
      case(13) !     COMMAND::DOC
      call mat_buf2str(name,buf,256)
         open(newunit=temp_lun,file=name,status='new',iostat=ios,iomsg=errmsg) ! open help file
         if(ios.eq.0)then
            call mat_make_manual(temp_lun)
            call mat_make_help(temp_lun)
            close(unit=-lunit,iostat=ios)
            call journal('sc','*doc* user guide including all help text in the Appendix is on file',trim(name) )
         else
            call journal(trim(errmsg))
         endif
      MSTK(TOP) = 0
!===================================================================================================================================
      end select FUN5
!===================================================================================================================================
end subroutine mat_matfn5
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_stackg(ID)

! ident_34="@(#)M_matrix::mat_stackg(3fp): get variables from storage"

integer       :: id(4)
integer       :: i
integer       :: j
integer       :: k
integer       :: l
integer       :: l2
integer       :: l3
integer       :: li
integer       :: lj
integer       :: lk
integer       :: ll
integer       :: ls
integer       :: m
integer       :: mk
integer       :: mn
integer       :: mnk
integer       :: n
      IF (DDT .EQ. 1)then
         call journal('sc','STACKG(1)=',ID(1))
         call journal('sc','STACKG(2)=',ID(2))
         call journal('sc','STACKG(3)=',ID(3))
         call journal('sc','STACKG(4)=',ID(4))
      endif
      CALL mat_putid(IDSTK(1,BOT-1), ID)
      K = LSIZE+1
!===================================================================================================================================
   10 continue
      K = K-1
      IF (.NOT.mat_eqid(IDSTK(1,K), ID)) GOTO 10
      IF (K .GE. LSIZE-1 .AND. RHS .GT. 0) GOTO 98
      IF (K .EQ. BOT-1) GOTO 98
      LK = LSTK(K)
      IF (RHS .EQ. 1) GOTO 40
      IF (RHS .EQ. 2) GOTO 60
      IF (RHS .GT. 2) CALL mat_err(21)
      IF (ERR .GT. 0) RETURN
      L = 1
      IF (TOP .GT. 0) L = LSTK(TOP) + MSTK(TOP)*NSTK(TOP)
      IF (TOP+1 .GE. BOT) CALL mat_err(18)
      IF (ERR .GT. 0) RETURN
      TOP = TOP+1
!
!     LOAD VARIABLE TO TOP OF STACK
      LSTK(TOP) = L
      MSTK(TOP) = MSTK(K)
      NSTK(TOP) = NSTK(K)
      MN = MSTK(K)*NSTK(K)
      ERR = L+MN - LSTK(BOT)
      IF (ERR .GT. 0) CALL mat_err(17)
      IF (ERR .GT. 0) RETURN
!     IF RAND, MATFN6 GENERATES RANDOM NUMBER
      IF (K .EQ. LSIZE) GOTO 97
      CALL mat_wcopy(MN,STKR(LK),STKI(LK),1,STKR(L),STKI(L),1)
      GOTO 99
!===================================================================================================================================
!     VECT(ARG)
   40 continue
      IF (MSTK(TOP) .EQ. 0) GOTO 99
      L = LSTK(TOP)
      MN = MSTK(TOP)*NSTK(TOP)
      MNK = MSTK(K)*NSTK(K)
      IF (MSTK(TOP) .LT. 0) MN = MNK
      DO I = 1, MN
        LL = L+I-1
        LS = LK+I-1
        IF (MSTK(TOP) .GT. 0) LS = LK + IDINT(STKR(LL)) - 1
        IF (LS .LT. LK .OR. LS .GE. LK+MNK) CALL mat_err(21)
        IF (ERR .GT. 0) RETURN
        STKR(LL) = STKR(LS)
        STKI(LL) = STKI(LS)
      enddo
      MSTK(TOP) = 1
      NSTK(TOP) = 1
      IF (MSTK(K) .GT. 1) MSTK(TOP) = MN
      IF (MSTK(K) .EQ. 1) NSTK(TOP) = MN
      GOTO 99
!===================================================================================================================================
!     MATRIX(ARG,ARG)
   60 continue
      TOP = TOP-1
      L = LSTK(TOP)
      IF (MSTK(TOP+1) .EQ. 0) MSTK(TOP) = 0
      IF (MSTK(TOP) .EQ. 0) GOTO 99
      L2 = LSTK(TOP+1)
      M = MSTK(TOP)*NSTK(TOP)
      IF (MSTK(TOP) .LT. 0) M = MSTK(K)
      N = MSTK(TOP+1)*NSTK(TOP+1)
      IF (MSTK(TOP+1) .LT. 0) N = NSTK(K)
      L3 = L2 + N
      MK = MSTK(K)
      MNK = MSTK(K)*NSTK(K)
      DO J = 1, N
         DO I = 1, M
           LI = L+I-1
           IF (MSTK(TOP) .GT. 0) LI = L + IDINT(STKR(LI)) - 1
           LJ = L2+J-1
           IF (MSTK(TOP+1) .GT. 0) LJ = L2 + IDINT(STKR(LJ)) - 1
           LS = LK + LI-L + (LJ-L2)*MK
           IF (LS.LT.LK .OR. LS.GE.LK+MNK) CALL mat_err(21)
           IF (ERR .GT. 0) RETURN
           LL = L3 + I-1 + (J-1)*M
           STKR(LL) = STKR(LS)
           STKI(LL) = STKI(LS)
         enddo
      enddo
      MN = M*N
      CALL mat_wcopy(MN,STKR(L3),STKI(L3),1,STKR(L),STKI(L),1)
      MSTK(TOP) = M
      NSTK(TOP) = N
      GOTO 99
!===================================================================================================================================
   97 continue
      FIN = 7
      FUN = 6
      RETURN
!===================================================================================================================================
   98 continue
      FIN = 0
      RETURN
!===================================================================================================================================
   99 continue
      FIN = -1
      FUN = 0
!===================================================================================================================================
END SUBROUTINE mat_stackg
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_stack2(OP)

! ident_35="@(#)M_matrix::ml_stackp(3fp): binary and ternary operations"

INTEGER           :: OP
DOUBLEPRECISION   :: SR,SI,E1,ST,E2

integer,parameter :: plus=41
integer,parameter :: MINUS=42
integer,parameter :: STAR=43
integer,parameter :: DSTAR=54
integer,parameter :: SLASH=44
integer,parameter :: BSLASH=45
integer,parameter :: DOT=47
integer,parameter :: COLON=40

integer           ::  i
integer           ::  j
integer           ::  k
integer           ::  k1
integer           ::  k2
integer           ::  kexp
integer           ::  l
integer           ::  l1
integer           ::  l2
integer           ::  l3
integer           ::  ll
integer           ::  ls
integer           ::  m
integer           ::  m2
integer           ::  mn
integer           ::  n
integer           ::  n2
integer           ::  nexp

   IF (DDT .EQ. 1) call journal('sc',',STACK2 ',OP)
   L2 = LSTK(TOP)
   M2 = MSTK(TOP)
   N2 = NSTK(TOP)
   TOP = TOP-1
   L = LSTK(TOP)
   M = MSTK(TOP)
   N = NSTK(TOP)
   FUN = 0
   IF (OP .EQ. PLUS) GOTO 01
   IF (OP .EQ. MINUS) GOTO 03
   IF (OP .EQ. STAR) GOTO 05
   IF (OP .EQ. DSTAR) GOTO 30
   IF (OP .EQ. SLASH) GOTO 20
   IF (OP .EQ. BSLASH) GOTO 25
   IF (OP .EQ. COLON) GOTO 60
   IF (OP .GT. 2*DOT) GOTO 80
   IF (OP .GT. DOT) GOTO 70
!
!     ADDITION
01 IF (M .LT. 0) GOTO 50
   IF (M2 .LT. 0) GOTO 52
   IF (M .NE. M2) CALL mat_err(8)
   IF (ERR .GT. 0) RETURN
   IF (N .NE. N2) CALL mat_err(8)
   IF (ERR .GT. 0) RETURN
   CALL matX_waxpy(M*N,1.0D0,0.0D0,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
   GOTO 99
!
!     SUBTRACTION
03 IF (M .LT. 0) GOTO 54
   IF (M2 .LT. 0) GOTO 56
   IF (M .NE. M2) CALL mat_err(9)
   IF (ERR .GT. 0) RETURN
   IF (N .NE. N2) CALL mat_err(9)
   IF (ERR .GT. 0) RETURN
   CALL matX_waxpy(M*N,-1.0D0,0.0D0,STKR(L2),STKI(L2),1,STKR(L),STKI(L),1)
   GOTO 99
!
!     MULTIPLICATION
05 continue
   IF (M2*M2*N2 .EQ. 1) GOTO 10
   IF (M*N .EQ. 1) GOTO 11
   IF (M2*N2 .EQ. 1) GOTO 10
   IF (N .NE. M2) CALL mat_err(10)
   IF (ERR .GT. 0) RETURN
   MN = M*N2
   LL = L + MN
   ERR = LL+M*N+M2*N2 - LSTK(BOT)
   IF (ERR .GT. 0) CALL mat_err(17)
   IF (ERR .GT. 0) RETURN
   CALL mat_wcopy(M*N+M2*N2,STKR(L),STKI(L),-1,STKR(LL),STKI(LL),-1)
   DO J = 1, N2
      DO I = 1, M
         K1 = L + MN + (I-1)
         K2 = L2 + MN + (J-1)*M2
         K = L + (I-1) + (J-1)*M
         STKR(K) = mat_wdotur(N,STKR(K1),STKI(K1),M,STKR(K2),STKI(K2),1)
         STKI(K) = mat_wdotui(N,STKR(K1),STKI(K1),M,STKR(K2),STKI(K2),1)
      enddo
   enddo
   NSTK(TOP) = N2
   GOTO 99
!
!     MULTIPLICATION BY SCALAR
10 continue
   SR = STKR(L2)
   SI = STKI(L2)
   L1 = L
   GOTO 13
11 continue
   SR = STKR(L)
   SI = STKI(L)
   L1 = L+1
   MSTK(TOP) = M2
   NSTK(TOP) = N2
13 continue
   MN = MSTK(TOP)*NSTK(TOP)
   CALL mat_wscal(MN,SR,SI,STKR(L1),STKI(L1),1)
   IF (L1.NE.L) CALL mat_wcopy(MN,STKR(L1),STKI(L1),1,STKR(L),STKI(L),1)
   GOTO 99
!
!     RIGHT DIVISION
20 continue
   IF (M2*N2 .EQ. 1) GOTO 21
   IF (M2 .EQ. N2) FUN = 1
   IF (M2 .NE. N2) FUN = 4
   FIN = -1
   RHS = 2
   GOTO 99
21 continue
   SR = STKR(L2)
   SI = STKI(L2)
   MN = M*N
   DO I = 1, MN
      LL = L+I-1
      CALL mat_wdiv(STKR(LL),STKI(LL),SR,SI,STKR(LL),STKI(LL))
      IF (ERR .GT. 0) RETURN
   enddo
   GOTO 99
!
!     LEFT DIVISION
25 continue
   IF (M*N .EQ. 1) GOTO 26
   IF (M .EQ. N) FUN = 1
   IF (M .NE. N) FUN = 4
   FIN = -2
   RHS = 2
   GOTO 99
26 continue
   SR = STKR(L)
   SI = STKI(L)
   MSTK(TOP) = M2
   NSTK(TOP) = N2
   MN = M2*N2
   DO I = 1, MN
      LL = L+I-1
      CALL mat_wdiv(STKR(LL+1),STKI(LL+1),SR,SI,STKR(LL),STKI(LL))
      IF (ERR .GT. 0) RETURN
   enddo
   GOTO 99
!
!     POWER
30 continue
   IF (M2*N2 .NE. 1) CALL mat_err(30)
   IF (ERR .GT. 0) RETURN
   IF (M .NE. N) CALL mat_err(20)
   IF (ERR .GT. 0) RETURN
   NEXP = IDINT(STKR(L2))
   IF (STKR(L2) .NE. dble(NEXP)) GOTO 39
   IF (STKI(L2) .NE. 0.0D0) GOTO 39
   IF (NEXP .LT. 2) GOTO 39
   MN = M*N
   ERR = L2+MN+N - LSTK(BOT)
   IF (ERR .GT. 0) CALL mat_err(17)
   IF (ERR .GT. 0) RETURN
   CALL mat_wcopy(MN,STKR(L),STKI(L),1,STKR(L2),STKI(L2),1)
   L3 = L2+MN
   DO KEXP = 2, NEXP
      DO J = 1, N
         LS = L+(J-1)*N
         CALL mat_wcopy(N,STKR(LS),STKI(LS),1,STKR(L3),STKI(L3),1)
         DO I = 1, N
            LS = L2+I-1
            LL = L+I-1+(J-1)*N
            STKR(LL)=mat_wdotur(N,STKR(LS),STKI(LS),N,STKR(L3),STKI(L3),1)
            STKI(LL)=mat_wdotui(N,STKR(LS),STKI(LS),N,STKR(L3),STKI(L3),1)
         enddo
      enddo
   enddo
   GOTO 99
!
!     NONINTEGER OR NONPOSITIVE POWER, USE EIGENVECTORS
39 continue
   FUN = 2
   FIN = 0
   GOTO 99
!
!     ADD OR SUBTRACT SCALAR
50 continue
   IF (M2 .NE. N2) CALL mat_err(8)
   IF (ERR .GT. 0) RETURN
   M = M2
   N = N2
   MSTK(TOP) = M
   NSTK(TOP) = N
   SR = STKR(L)
   SI = STKI(L)
   CALL mat_wcopy(M*N,STKR(L+1),STKI(L+1),1,STKR(L),STKI(L),1)
   GOTO 58
52 continue
   IF (M .NE. N) CALL mat_err(8)
   IF (ERR .GT. 0) RETURN
   SR = STKR(L2)
   SI = STKI(L2)
   GOTO 58
54 continue
   IF (M2 .NE. N2) CALL mat_err(9)
   IF (ERR .GT. 0) RETURN
   M = M2
   N = N2
   MSTK(TOP) = M
   NSTK(TOP) = N
   SR = STKR(L)
   SI = STKI(L)
   CALL mat_wcopy(M*N,STKR(L+1),STKI(L+1),1,STKR(L),STKI(L),1)
   CALL mat_wrscal(M*N,-1.0D0,STKR(L),STKI(L),1)
   GOTO 58
56 continue
   IF (M .NE. N) CALL mat_err(9)
   IF (ERR .GT. 0) RETURN
   SR = -STKR(L2)
   SI = -STKI(L2)
   GOTO 58
58 continue
   DO I = 1, N
      LL = L + (I-1)*(N+1)
      STKR(LL) = mat_flop(STKR(LL)+SR)
      STKI(LL) = mat_flop(STKI(LL)+SI)
   enddo
   GOTO 99
!
!     COLON
60 continue
   E2 = STKR(L2)
   ST = 1.0D0
   N = 0
   IF (RHS .LT. 3) GOTO 61
   ST = STKR(L)
   TOP = TOP-1
   L = LSTK(TOP)
   IF (ST .EQ. 0.0D0) GOTO 63
61 continue
   E1 = STKR(L)
!     CHECK FOR CLAUSE
   IF (RSTK(PT) .EQ. 3) GOTO 64
   ERR = L + MAX0(3,IDINT((E2-E1)/ST)) - LSTK(BOT)
   IF (ERR .GT. 0) CALL mat_err(17)
   IF (ERR .GT. 0) RETURN
62 continue
   IF (ST .GT. 0.0D0 .AND. STKR(L) .GT. E2) GOTO 63
   IF (ST .LT. 0.0D0 .AND. STKR(L) .LT. E2) GOTO 63
   N = N+1
   L = L+1
   STKR(L) = E1 + dble(N)*ST
   STKI(L) = 0.0D0
   GOTO 62
63 continue
   NSTK(TOP) = N
   MSTK(TOP) = 1
   IF (N .EQ. 0) MSTK(TOP) = 0
   GOTO 99
!
!     FOR CLAUSE
64 continue
   STKR(L) = E1
   STKR(L+1) = ST
   STKR(L+2) = E2
   MSTK(TOP) = -3
   NSTK(TOP) = -1
   GOTO 99
!
!     ELEMENTWISE OPERATIONS
70 continue
   OP = OP - DOT
   IF (M.NE.M2 .OR. N.NE.N2) CALL mat_err(10)
   IF (ERR .GT. 0) RETURN
   MN = M*N
   DO I = 1, MN
      J = L+I-1
      K = L2+I-1
      IF (OP .EQ. STAR)CALL mat_wmul(STKR(J),STKI(J),STKR(K),STKI(K),STKR(J),STKI(J))
      IF (OP .EQ. SLASH)CALL mat_wdiv(STKR(J),STKI(J),STKR(K),STKI(K),STKR(J),STKI(J))
      IF (OP .EQ. BSLASH)CALL mat_wdiv(STKR(K),STKI(K),STKR(J),STKI(J),STKR(J),STKI(J))
      IF (ERR .GT. 0) RETURN
   enddo
   GOTO 99
!
!     KRONECKER
80 continue
   FIN = OP - 2*DOT - STAR + 11
   FUN = 6
   TOP = TOP + 1
   RHS = 2
   GOTO 99
!
99 continue
END SUBROUTINE mat_stack2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getlin() ! get a new input line
use m_history,only : redo

character(len=1024) :: mline

integer,parameter   :: linelen=255
INTEGER,save        :: LRECL=linelen
integer,parameter   :: SLASH=44
integer,parameter   :: BSLASH=45
integer,parameter   :: DOT=47
integer,parameter   :: BLANK=36
integer,parameter   :: RETU(4)=[26,30,18,29]
integer             :: i
integer             :: j
integer             :: k
integer             :: l
integer             :: n
!.......................................................................
   10 CONTINUE
      L = LPT(1)
!.......................................................................
   11 CONTINUE
      DO J = 1, LRECL            ! blank out buffer before reading it
         BUF(J) = ALFA(BLANK+1)
      enddo
!.......................................................................
      N = LRECL+1
!     get line of input
      IF(ISTRINGQ.GT.0)THEN                  ! read from string instead of file
         CALL REDO(stringq,'.')              ! This is a no-op except for storing the line into the input history
         call mat_str2buf(stringq,buf,lrecl)     ! read input line from MAT88 call string
         call journal('c',stringq)           ! write as a comment
         IF(INITQ.EQ.2)THEN                  ! terminate after processing STRINGQ
            ISTRINGQ=4
            STRINGQ='quit'
         ELSE                                ! go to normal MAT88 mode after processing STRINGQ
            ISTRINGQ=0
         ENDIF
      ELSE
         mline(:)=' '

         READ(RIO,'(A)',END=50,ERR=15) mline ! read input line from file
         CALL REDO(mline,'.')  ! pass line to REDO(3f). This is a no-op except for storing the line into the input history
                               ! (unless the input line is the "r" command)

         if(RIO.eq.5)then
            call journal('t',mline)          ! reading from standard input, so copy to trail file
         else
            call journal('c',mline)          ! reading from an exec() command, so write as a comment
         endif
         if(mline(1:1).eq.'#')goto 11        ! ignore lines with a # in column 1
         call mat_str2buf(mline,buf,lrecl)       ! read input line from file
      ENDIF
!.......................................................................
   15 CONTINUE
      N = N-1
      IF(N.LT.1)THEN
         N=1
      else IF (BUF(N) .EQ. ALFA(BLANK+1))then
         GOTO 15 ! trim off trailing spaces
      endif
!.......................................................................
      IF (MOD(LCT(4),2) .EQ. 1) then
              call mat_buf2str(mline,buf,n) ! convert ADE buffer to character
              call journal('s',mline) ! just to standard output
      endif
!.......................................................................
      DO J = 1, N
         DO K = 1, alflq  ! make sure this letter is in set of MAT88 characters and get its MAT88 number
           IF (BUF(J).EQ.ALFA(K) .OR. BUF(J).EQ.ALFB(K)) GOTO 30
         enddo
         call journal('sc','UNKNOWN CHARACTER AT COLUMN ',J) ! this is not a known character
         K = EOL+1
         IF (K .GT. EOL) GOTO 10   ! UNKNOWN CHARACTER , K NOT CHANGED. get new line
         IF (K .EQ. EOL) GOTO 45
         IF (K .EQ. -1) L = L-1
         IF (K .LE. 0) cycle
!
   30    CONTINUE
         K = K-1   ! K is index into ALF*, should be in range 0 to 51
         IF (K.EQ.SLASH .AND. BUF(J+1).EQ.BUF(J)) GOTO 45  ! if // rest is comment
         IF (K.EQ.DOT .AND. BUF(J+1).EQ.BUF(J)) GOTO 11    ! if .. line continuation
         IF (K.EQ.BSLASH .AND. N.EQ.1) GOTO 60             ! if \ in column 1
         LIN(L) = K
         IF (L.LT.1024) L = L+1
         IF (L.EQ.1024) call journal('sc','input buffer limit exceeded=',L)
      enddo
!.......................................................................
   45 CONTINUE      ! line is ready, reset line pointers
      LIN(L) = EOL
      LPT(6) = L
      LPT(4) = LPT(1)
      LPT(3) = 0
      LPT(2) = 0
      LCT(1) = 0
      CALL mat_getch() ! load first character onto CHRA
      RETURN
!.......................................................................
   50 CONTINUE ! hit end of file
      CALL mat_putid(LIN(L),RETU) ! store RETU onto LIN(L) to simulate RETURN command
      L = L + 4
      GOTO 45
!.......................................................................
   60 CONTINUE
      N = LPT(6) - LPT(1)
      DO I = 1, N
         J = L+I-1
         K = LIN(J)
         BUF(I) = ALFA(K+1)
         IF (CASE.EQ.1 .AND. K.LT.36) BUF(I) = ALFB(K+1)
      enddo    ! edit command history
      !!CALL ML_EDIT(BUF,N)
      !!N = N + 1
      GOTO 15
!.......................................................................
end subroutine mat_getlin
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_clause()
character(len=256) :: mline
DOUBLEPRECISION    :: E1,E2
INTEGER            :: OP
integer,parameter  :: NAME=1
integer            :: r
integer,parameter  :: SEMI=39
integer,parameter  :: EQUAL=46
integer,parameter  :: BLANK=36
integer,parameter  :: COMMA=48
integer,parameter  :: LESS=50
integer,parameter  :: GREAT=51
integer,parameter  :: FOR(4)=[15,24,27,36]
integer,parameter  :: WHILE(4)=[32,17,18,21]
integer,parameter  :: IFF(4)=[18,15,36,36]
integer,parameter  :: ELSE(4)=[14,21,28,14]
integer,parameter  :: ENND(4)=[14,23,13,36]
integer,parameter  :: DO(4)=[13,24,36,36]
integer,parameter  :: THENN(4)=[29,17,14,23]
integer            :: i
integer            :: j
integer            :: kount
integer            :: l
integer            :: l2
integer            :: lj
integer            :: m
integer            :: n

   R = -FIN-10
   FIN = 0
   IF (DDT .EQ. 1)then
      WRITE(mline,'('' CLAUSE '',3I4)') PT,RSTK(PT),R
      call journal(mline)
   endif
   IF (R.LT.1 .OR. R.GT.6) GOTO 01
   GOTO (02,30,30,80,99,90),R
01 continue
   R = RSTK(PT)
   GOTO (99,99,05,40,45,99,99,99,99,99,99,99,15,55,99,99,99),R
   call journal('*mat_clause* -- internal error')
   goto 99
!.......................................................................
!     FOR
!
02 continue
   CALL mat_getsym()
   IF (SYM .NE. NAME) CALL mat_err(34) ! improper for clause
   IF (ERR .GT. 0) RETURN
   PT = PT+2
   CALL mat_putid(IDS(1,PT),SYN)
   CALL mat_getsym()
   IF (SYM .NE. EQUAL) CALL mat_err(34) ! improper for clause
   IF (ERR .GT. 0) RETURN
   CALL mat_getsym()
   RSTK(PT) = 3
!     *CALL* EXPR
   RETURN
05 continue
   PSTK(PT-1) = 0
   PSTK(PT) = LPT(4) - 1
   IF (mat_eqid(SYN,DO)) SYM = SEMI
   IF (SYM .EQ. COMMA) SYM = SEMI
   IF (SYM .NE. SEMI) CALL mat_err(34) ! improper for clause
   IF (ERR .GT. 0) RETURN
10 continue
   J = PSTK(PT-1)
   LPT(4) = PSTK(PT)
   SYM = SEMI
   CHRA = BLANK
   J = J+1
   L = LSTK(TOP)
   M = MSTK(TOP)
   N = NSTK(TOP)
   LJ = L+(J-1)*M
   L2 = L + M*N
   IF (M .NE. -3) GOTO 12
   LJ = L+3
   L2 = LJ
   STKR(LJ) = STKR(L) + dble(J-1)*STKR(L+1)
   STKI(LJ) = 0.0d0
   IF (STKR(L+1).GT.0.0D0 .AND. STKR(LJ).GT.STKR(L+2)) GOTO 20
   IF (STKR(L+1).LT.0.0D0 .AND. STKR(LJ).LT.STKR(L+2)) GOTO 20
   M = 1
   N = J
12 continue
   IF (J .GT. N) GOTO 20
   IF (TOP+1 .GE. BOT) CALL mat_err(18) ! too many names
   IF (ERR .GT. 0) RETURN
   TOP = TOP+1
   LSTK(TOP) = L2
   MSTK(TOP) = M
   NSTK(TOP) = 1
   ERR = L2+M - LSTK(BOT)
   IF (ERR .GT. 0) CALL mat_err(17)   ! too much memory required
   IF (ERR .GT. 0) RETURN
   CALL mat_wcopy(M,STKR(LJ),STKI(LJ),1,STKR(L2),STKI(L2),1)
   RHS = 0
   CALL mat_stackp(IDS(1,PT))
   IF (ERR .GT. 0) RETURN
   PSTK(PT-1) = J
   PSTK(PT) = LPT(4)
   RSTK(PT) = 13
!     *CALL* PARSE
   RETURN
15 continue
   GOTO 10
20 continue
   MSTK(TOP) = 0
   NSTK(TOP) = 0
   RHS = 0
   CALL mat_stackp(IDS(1,PT))
   IF (ERR .GT. 0) RETURN
   PT = PT-2
   GOTO 80
!.......................................................................
!
!     WHILE OR IF
!
30 continue
   PT = PT+1
   CALL mat_putid(IDS(1,PT),SYN)
   PSTK(PT) = LPT(4)-1
35 continue
   LPT(4) = PSTK(PT)
   CHRA = BLANK
   CALL mat_getsym()
   RSTK(PT) = 4
!     *CALL* EXPR
   RETURN
40 continue
   IF (SYM.NE.EQUAL .AND. SYM.NE.LESS .AND. SYM.NE.GREAT)CALL mat_err(35)    ! improper WHILE or IF clause
   IF (ERR .GT. 0) RETURN
   OP = SYM
   CALL mat_getsym()
   IF (SYM.EQ.EQUAL .OR. SYM.EQ.GREAT) OP = OP + SYM
   IF (OP .GT. GREAT) CALL mat_getsym()
   PSTK(PT) = 256*PSTK(PT) + OP
   RSTK(PT) = 5
!     *CALL* EXPR
   RETURN
45 continue
   OP = MOD(PSTK(PT),256)
   PSTK(PT) = PSTK(PT)/256
   L = LSTK(TOP-1)
   E1 = STKR(L)
   L = LSTK(TOP)
   E2 = STKR(L)
   TOP = TOP - 2
   IF (mat_eqid(SYN,DO) .OR. mat_eqid(SYN,THENN)) SYM = SEMI
   IF (SYM .EQ. COMMA) SYM = SEMI
   IF (SYM .NE. SEMI) CALL mat_err(35) ! improper WHILE or IF clause
   IF (ERR .GT. 0) RETURN
   IF (OP.EQ.EQUAL         .AND. E1.EQ.E2) GOTO 50
   IF (OP.EQ.LESS          .AND. E1.LT.E2) GOTO 50
   IF (OP.EQ.GREAT         .AND. E1.GT.E2) GOTO 50
   IF (OP.EQ.(LESS+EQUAL)  .AND. E1.LE.E2) GOTO 50
   IF (OP.EQ.(GREAT+EQUAL) .AND. E1.GE.E2) GOTO 50
   IF (OP.EQ.(LESS+GREAT)  .AND. E1.NE.E2) GOTO 50
   PT = PT-1
   GOTO 80
50 continue
   RSTK(PT) = 14
!     *CALL* PARSE
   RETURN
55 continue
   IF (mat_eqid(IDS(1,PT),WHILE)) GOTO 35
   PT = PT-1
   IF (mat_eqid(SYN,ELSE)) GOTO 80
   RETURN
!.......................................................................
!
!     SEARCH FOR MATCHING END OR ELSE
80 continue
   KOUNT = 0
   CALL mat_getsym()
82 continue
   IF (SYM .EQ. EOL) RETURN
   IF (SYM .NE. NAME) GOTO 83
   IF (mat_eqid(SYN,ENND) .AND. KOUNT.EQ.0) RETURN
   IF (mat_eqid(SYN,ELSE) .AND. KOUNT.EQ.0) RETURN
   IF (mat_eqid(SYN,ENND) .OR. mat_eqid(SYN,ELSE))KOUNT = KOUNT-1
   IF (mat_eqid(SYN,FOR) .OR. mat_eqid(SYN,WHILE).OR.mat_eqid(SYN,IFF)) KOUNT = KOUNT+1
83 continue
   CALL mat_getsym()
   GOTO 82
!.......................................................................
!
!     EXIT FROM LOOP
90 continue
   IF (DDT .EQ. 1)then
      WRITE(mline,'('' EXIT '',10I4)') (RSTK(I),I=1,PT)
      call journal(mline)
   endif
   IF (RSTK(PT) .EQ. 14) PT = PT-1
   IF (PT .LE. PTZ) RETURN
   IF (RSTK(PT) .EQ. 14) PT = PT-1
   IF (PT-1 .LE. PTZ) RETURN
   IF (RSTK(PT) .EQ. 13) TOP = TOP-1
   IF (RSTK(PT) .EQ. 13) PT = PT-2
   GOTO 80
!.......................................................................
!
99 continue
   CALL mat_err(22)    ! recursion difficulties
   IF (ERR .GT. 0) RETURN
END SUBROUTINE mat_clause
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_rat(x,len,maxd,a,b,d)

! ident_36="@(#)M_matrix::mat_rat(3fp): A/B = continued fraction approximation to X using  len  terms each less than MAXD"

integer         :: len,maxd
doubleprecision :: x,a,b,d(len)
doubleprecision :: s,t,z
integer         :: i
integer         :: ib
integer         :: k
   z = x
   k=0  ! preset to illegal value
   if(len.lt.1)then
      call journal('*mat_rat* internal error -- len<1')
      return
   endif
   do i = 1, len
      k = i
      d(k) = mat_round(z)
      z = z - d(k)
      if (dabs(z)*dble(maxd) .le. 1.0d0) exit
      z = 1.0d0/z
   enddo
   t = d(k)
   s = 1.0d0
   if (k .ge. 2) then
      do ib = 2, k
         i = k+1-ib
         z = t
         t = d(i)*t + s
         s = z
      enddo
   endif
   if (s .lt. 0.0d0) t = -t
   if (s .lt. 0.0d0) s = -s
   if (ddt .eq. 27)then
      write(wte,50) x,t,s,(d(i),i=1,k) ! debug 27
50    format(/1x,1pd23.15,0pf8.0,' /',f8.0,4x,6f5.0/(1x,45x,6f5.0))
   endif
   a = t
   b = s
end subroutine mat_rat
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_expr()
character(len=80) :: mline
integer           :: r
integer           :: sign
integer,parameter :: COLON=40
integer,parameter :: BLANK=36
integer,parameter :: PLUS=41
integer,parameter :: MINUS=42
integer,parameter :: NAME=1
integer,parameter :: EYE(4)=[14,34,14,36]
integer           :: kount
integer           :: ls
integer           :: op

   if (ddt .eq. 1) then
      write(mline,'('' EXPR '',2I4)') pt,rstk(pt)
      call journal(mline)
   endif
   r = rstk(pt)
!===================================================================================================================================
   GOTO (01,01,01,01,01,05,25,99,99,01,01,99,99,99,99,99,99,01,01,01),R
!===================================================================================================================================
01 continue
   if (sym .eq. colon) call mat_putid(syn,eye)
   if (sym .eq. colon) sym = name
   kount = 1
02 continue
   sign = plus
   if (sym .eq. minus) sign = minus
   if (sym.eq.plus .or. sym.eq.minus) call mat_getsym()
   pt = pt+1
   if (pt .gt. psize-1) call mat_err(26) ! too complicated (stack overflow)
   if (err .gt. 0) return
   pstk(pt) = sign + 256*kount
   rstk(pt) = 6
!     *call* term
   return
05 continue
   sign = mod(pstk(pt),256)
   kount = pstk(pt)/256
   pt = pt-1
   if (sign .eq. minus) call mat_stack1(minus)
   if (err .gt. 0) return
10 continue
   if (sym.eq.plus .or. sym.eq.minus) goto 20
   goto 50
20 continue
   if (rstk(pt) .ne. 10) goto 21
!     blank is delimiter inside angle brackets
   ls = lpt(3) - 2
   if (lin(ls) .eq. blank) goto 50
21 continue
   op = sym
   call mat_getsym()
   pt = pt+1
   pstk(pt) = op + 256*kount
   rstk(pt) = 7
!     *call* term
   return
25 continue
   op = mod(pstk(pt),256)
   kount = pstk(pt)/256
   pt = pt-1
   call mat_stack2(op)
   if (err .gt. 0) return
   goto 10
50 continue
   if (sym .ne. colon) goto 60
   call mat_getsym()
   kount = kount+1
   goto 02
60 continue
   if (kount .gt. 3) call mat_err(33)  ! too many colons
   if (err .gt. 0) return
   rhs = kount
   if (kount .gt. 1) call mat_stack2(colon)
   if (err .gt. 0) return
   return
99 continue
   call mat_err(22)     ! recursion difficulties
   if (err .gt. 0) return
end subroutine mat_expr
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_factor()
character(len=80) :: mline
INTEGER           :: R
INTEGER           :: ID(4)
INTEGER           :: EXCNT
integer,parameter :: DSTAR=54
integer,parameter :: SEMI=39
integer,parameter :: BLANK=36
integer,parameter :: STAR=43
integer,parameter :: COMMA=48
integer,parameter :: LPAREN=37
integer,parameter :: RPAREN=38
integer,parameter :: LESS=50
integer,parameter :: GREAT=51
integer,parameter :: QUOTE=49
integer,parameter :: NUM=0
integer,parameter :: NAME=1
integer           :: i
integer           :: j
integer           :: k
integer           :: l
integer           :: ln
integer           :: ls
integer           :: n

   IF (DDT .EQ. 1) then
      WRITE(mline,'('' FACTOR '',3I4)') PT,RSTK(PT),SYM
      call journal(mline)
   endif
   R = RSTK(PT)
   GOTO (99,99,99,99,99,99,99,01,01,25,45,65,99,99,99,55,75,32,37),R
01 CONTINUE
   IF (SYM.EQ.NUM .OR. SYM.EQ.QUOTE .OR.  SYM.EQ.LESS) GOTO 10
   IF (SYM .EQ. GREAT) GOTO 30
   EXCNT = 0
   IF (SYM .EQ. NAME) GOTO 40
   ID(1) = BLANK
   IF (SYM .EQ. LPAREN) GOTO 42
   CALL mat_err(2)
   IF (ERR .GT. 0) RETURN
!
!     PUT SOMETHING ON THE STACK
10 CONTINUE
   L = 1
   IF (TOP .GT. 0) L = LSTK(TOP) + MSTK(TOP)*NSTK(TOP)
   IF (TOP+1 .GE. BOT) CALL mat_err(18)
   IF (ERR .GT. 0) RETURN
   TOP = TOP+1
   LSTK(TOP) = L
   IF (SYM .EQ. QUOTE) GOTO 15
   IF (SYM .EQ. LESS) GOTO 20
!
!     SINGLE NUMBER, GETSYM STORED IT IN STKI
   MSTK(TOP) = 1
   NSTK(TOP) = 1
   STKR(L) = STKI(VSIZE)
   STKI(L) = 0.0D0
   CALL mat_getsym()
   GOTO 60
!
!     STRING
15 CONTINUE
   N = 0
   LPT(4) = LPT(3)
   CALL mat_getch()  ! get next character
16 CONTINUE
   IF (CHRA .EQ. QUOTE) GOTO 18
17 CONTINUE
   LN = L+N
   IF (CHRA .EQ. EOL) CALL mat_err(31)
   IF (ERR .GT. 0) RETURN
   STKR(LN) = dble(CHRA)
   STKI(LN) = 0.0D0
   N = N+1
   CALL mat_getch()  ! get next character
   GOTO 16
18 CONTINUE
   CALL mat_getch()  ! get next character
   IF (CHRA .EQ. QUOTE) GOTO 17
   IF (N .LE. 0) CALL mat_err(31)
   IF (ERR .GT. 0) RETURN
   MSTK(TOP) = 1
   NSTK(TOP) = N
   CALL mat_getsym()
   GOTO 60
!
!     EXPLICIT MATRIX
20 CONTINUE
   MSTK(TOP) = 0
   NSTK(TOP) = 0
21 CONTINUE
   TOP = TOP + 1
   LSTK(TOP) = LSTK(TOP-1) + MSTK(TOP-1)*NSTK(TOP-1)
   MSTK(TOP) = 0
   NSTK(TOP) = 0
   CALL mat_getsym()
22 CONTINUE
   IF (SYM.EQ.SEMI .OR. SYM.EQ.GREAT .OR. SYM.EQ.EOL) GOTO 27
   IF (SYM .EQ. COMMA) CALL mat_getsym()
   PT = PT+1
   RSTK(PT) = 10
!     *CALL* EXPR
   RETURN
25 CONTINUE
   PT = PT-1
   TOP = TOP - 1
   IF (MSTK(TOP) .EQ. 0) MSTK(TOP) = MSTK(TOP+1)
   IF (MSTK(TOP) .NE. MSTK(TOP+1)) CALL mat_err(5)
   IF (ERR .GT. 0) RETURN
   NSTK(TOP) = NSTK(TOP) + NSTK(TOP+1)
   GOTO 22
27 CONTINUE
   IF (SYM.EQ.SEMI .AND. CHRA.EQ.EOL) CALL mat_getsym()
   CALL mat_stack1(QUOTE)
   IF (ERR .GT. 0) RETURN
   TOP = TOP - 1
   IF (MSTK(TOP) .EQ. 0) MSTK(TOP) = MSTK(TOP+1)
   IF (MSTK(TOP).NE.MSTK(TOP+1).AND.MSTK(TOP+1).GT.0)CALL mat_err(6)
   IF (ERR .GT. 0) RETURN
   NSTK(TOP) = NSTK(TOP) + NSTK(TOP+1)
   IF (SYM .EQ. EOL) CALL mat_getlin()
   IF (SYM .NE. GREAT) GOTO 21
   CALL mat_stack1(QUOTE)
   IF (ERR .GT. 0) RETURN
   CALL mat_getsym()
   GOTO 60
!
!     MACRO STRING
30 CONTINUE
   CALL mat_getsym()
   IF (SYM.EQ.LESS .AND. CHRA.EQ.EOL) CALL mat_err(28)
   IF (ERR .GT. 0) RETURN
   PT = PT+1
   RSTK(PT) = 18
!     *CALL* EXPR
   RETURN
32 CONTINUE
   PT = PT-1
   IF (SYM.NE.LESS .AND. SYM.NE.EOL) CALL mat_err(37)
   IF (ERR .GT. 0) RETURN
   IF (SYM .EQ. LESS) CALL mat_getsym()
   K = LPT(6)
   LIN(K+1) = LPT(1)
   LIN(K+2) = LPT(2)
   LIN(K+3) = LPT(6)
   LPT(1) = K + 4
!     TRANSFER STACK TO INPUT LINE
   K = LPT(1)
   L = LSTK(TOP)
   N = MSTK(TOP)*NSTK(TOP)
   DO J = 1, N
      LS = L + J-1
      LIN(K) = IDINT(STKR(LS))
      IF (LIN(K).LT.0 .OR. LIN(K).GE.IALF) CALL mat_err(37)
      IF (ERR .GT. 0) RETURN
      IF (K.LT.1024) K = K+1
      IF (K.EQ.1024)call journal('sc','INPUT BUFFER CHAR LIMIT EXCEEDED=',K)
   enddo
   TOP = TOP-1
   LIN(K) = EOL
   LPT(6) = K
   LPT(4) = LPT(1)
   LPT(3) = 0
   LPT(2) = 0
   LCT(1) = 0
   CHRA = BLANK
   CALL mat_getsym()
   PT = PT+1
   RSTK(PT) = 19
!     *CALL* EXPR
   RETURN
37 CONTINUE
   PT = PT-1
   K = LPT(1) - 4
   LPT(1) = LIN(K+1)
   LPT(4) = LIN(K+2)
   LPT(6) = LIN(K+3)
   CHRA = BLANK
   CALL mat_getsym()
   GOTO 60
!
!     FUNCTION OR MATRIX ELEMENT
40 CONTINUE
   CALL mat_putid(ID,SYN)
   CALL mat_getsym()
   IF (SYM .EQ. LPAREN) GOTO 42
   RHS = 0
   CALL mat_funs(ID)
   IF (FIN .NE. 0) CALL mat_err(25)
   IF (ERR .GT. 0) RETURN
   CALL mat_stackg(ID)
   IF (ERR .GT. 0) RETURN
   IF (FIN .EQ. 7) GOTO 50
   IF (FIN .EQ. 0) CALL mat_putid(IDS(1,PT+1),ID)
   IF (FIN .EQ. 0) CALL mat_err(4)
   IF (ERR .GT. 0) RETURN
   GOTO 60
!
42 CONTINUE
   CALL mat_getsym()
   EXCNT = EXCNT+1
   PT = PT+1
   PSTK(PT) = EXCNT
   CALL mat_putid(IDS(1,PT),ID)
   RSTK(PT) = 11
!     *CALL* EXPR
   RETURN
!.......................................................................
45 CONTINUE
   CALL mat_putid(ID,IDS(1,PT))
   EXCNT = PSTK(PT)
   PT = PT-1
   IF (SYM .EQ. COMMA) GOTO 42
   IF (SYM .NE. RPAREN) CALL mat_err(3)
   IF (ERR .GT. 0) RETURN
   IF (SYM .EQ. RPAREN) CALL mat_getsym()
   IF (ID(1) .EQ. BLANK) GOTO 60
   RHS = EXCNT
   CALL mat_stackg(ID)
   IF (ERR .GT. 0) RETURN
   IF (FIN .EQ. 0) CALL mat_funs(ID)
   IF (FIN .EQ. 0) CALL mat_err(4)
   IF (ERR .GT. 0) RETURN
!
!     EVALUATE MATRIX FUNCTION
50 CONTINUE
   PT = PT+1
   RSTK(PT) = 16
!     *CALL* MATFN
   RETURN
55 CONTINUE
   PT = PT-1
   GOTO 60
!
!     CHECK FOR QUOTE (TRANSPOSE) AND ** (POWER)
60 CONTINUE
   IF (SYM .NE. QUOTE) GOTO 62
   I = LPT(3) - 2
   IF (LIN(I) .EQ. BLANK) GOTO 90
   CALL mat_stack1(QUOTE)
   IF (ERR .GT. 0) RETURN
   CALL mat_getsym()
62 CONTINUE
   IF (SYM.NE.STAR .OR. CHRA.NE.STAR) GOTO 90
   CALL mat_getsym()
   CALL mat_getsym()
   PT = PT+1
   RSTK(PT) = 12
!     *CALL* FACTOR
   GOTO 01
65 CONTINUE
   PT = PT-1
   CALL mat_stack2(DSTAR)
   IF (ERR .GT. 0) RETURN
   IF (FUN .NE. 2) GOTO 90
!     MATRIX POWER, USE EIGENVECTORS
   PT = PT+1
   RSTK(PT) = 17
!     *CALL* MATFN
   RETURN
75 CONTINUE
   PT = PT-1
90 CONTINUE
   RETURN
99 CONTINUE
   CALL mat_err(22)
   IF (ERR .GT. 0) RETURN
END SUBROUTINE mat_factor
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_term()

character(len=256) ::  mline
INTEGER            :: R
INTEGER            :: OP
integer,parameter  :: BSLASH=45
integer,parameter  :: STAR=43
integer,parameter  :: SLASH=44
integer,parameter  :: DOT=47
integer            :: ierr
integer            :: ilen

   IF (DDT .EQ. 1) then
      mline='TERM '
      call mat_appnum(real(pt),mline,ilen,ierr)
      call mat_appnum(real(rstk(pt)),mline,ilen,ierr)
      call journal(mline)
   endif
   R = RSTK(PT)
   GOTO (99,99,99,99,99, 1, 1, 5,25,99,99,99,99,99,35,99,99,99,99),R
1  continue
   PT = PT+1
   RSTK(PT) = 8
!     *CALL* FACTOR
   RETURN
!.......................................................................
5  CONTINUE
   PT = PT-1
10 CONTINUE
   OP = 0
   IF (SYM .EQ. DOT) OP = DOT
   IF (SYM .EQ. DOT) CALL mat_getsym
   IF (SYM.EQ.STAR .OR. SYM.EQ.SLASH .OR. SYM.EQ.BSLASH) GOTO 20
   RETURN
!.......................................................................
20 CONTINUE
   OP = OP + SYM
   CALL mat_getsym()
   IF (SYM .EQ. DOT) OP = OP + SYM
   IF (SYM .EQ. DOT) CALL mat_getsym()
   PT = PT+1
   PSTK(PT) = OP
   RSTK(PT) = 9
!     *CALL* FACTOR
   RETURN
!.......................................................................
25 CONTINUE
   OP = PSTK(PT)
   PT = PT-1
   CALL mat_stack2(OP)
   IF (ERR .GT. 0) RETURN
!     SOME BINARY OPS DONE IN MATFNS
   IF (FUN .EQ. 0) GOTO 10
   PT = PT+1
   RSTK(PT) = 15
!     *CALL* MATFN
   RETURN
!.......................................................................
35 CONTINUE
   PT = PT-1
   GOTO 10
!.......................................................................
99 CONTINUE
   CALL mat_err(22)
   IF (ERR .GT. 0) RETURN
!.......................................................................
END SUBROUTINE mat_term
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_savlod(LSAVE,ID,M,N,IMG,JOB,XREAL,XIMAG)

integer                      :: lsave
integer                      :: id(4)
integer                      :: m
integer                      :: n
integer                      :: img
integer                      :: job
doubleprecision              :: xreal(*)
doubleprecision              :: ximag(*)
character(len=4)             :: cid
integer                      :: i,j,k,l

! ident_37="@(#)M_matrix::mat_savlod(3fp): read next variable from a save file or write next variable to it"

      ! should report I/O errors (disk full, unwritable, ....)
!
!     IMPLEMENT SAVE AND LOAD
!     LSAVE = LOGICAL UNIT NUMBER
!     ID = NAME, FORMAT 4A1
!     M, N = DIMENSIONS
!     IMG = NONZERO IF XIMAG IS NONZERO
!           RETURNED ON A LOAD
!     JOB = 0     FOR SAVE
!         = SPACE AVAILABLE FOR LOAD
!     XREAL, XIMAG = REAL AND OPTIONAL IMAGINARY PARTS
!.......................................................................
      IF (JOB .LE. 0) THEN   ! save
         call mat_buf2str(cid,id,4) ! convert ID to a character string
         WRITE(LSAVE,101) CID,M,N,IMG
         DO J = 1, N
            K = (J-1)*M+1
            L = J*M
            WRITE(LSAVE,102) (XREAL(I),I=K,L)                 ! real
            IF (IMG .NE. 0) WRITE(LSAVE,102) (XIMAG(I),I=K,L) ! imaginary
         enddo
!.......................................................................
      ELSE                   ! load
         READ(LSAVE,101,END=30) cid,M,N,IMG
         call mat_str2buf(cid,id,4) ! convert character string to and ID
         IF (M*N .GT. JOB) GOTO 30
         DO J = 1, N
            K = (J-1)*M+1
            L = J*M
            READ(LSAVE,102,END=30) (XREAL(I),I=K,L)   ! real
            IF (IMG .NE. 0) READ(LSAVE,102,END=30) (XIMAG(I),I=K,L) !imaginary
         enddo
      ENDIF
!.......................................................................
      RETURN
!.......................................................................
!     END OF FILE
   30 CONTINUE
      M = 0
      N = 0
!.......................................................................
!     SYSTEM DEPENDENT FORMATS
  101 FORMAT(A4,3I4)  ! ID, MxN dimensions of ID, IMAGINARY OR REAL FLAG
  102 FORMAT(4Z18)    ! format for data
      END SUBROUTINE mat_savlod
!==================================================================================================================================!
! INSERT HERE JSU
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mat_urand(3f) - [] uniform random number generator
!!
!!##SYNOPSIS
!!
!!   doubleprecision function mat_urand(iy)
!!
!!    integer :: iy
!!
!!##DESCRIPTION
!!    mat_urand(3f) is a uniform random number generator based on theory and
!!    suggestions given in D.E. Knuth (1969), Vol 2. The integer IY should
!!    be initialized to an arbitrary integer prior to the first call to
!!    mat_urand(3f). The calling program should not alter the value of IY
!!    between subsequent calls to mat_urand(3f). Values of mat_urand(3f) will
!!    be returned in the interval (0,1).
!!
!!##OPTIONS
!!
!!    IY seed for generating a sequence.
!!
!!##EXAMPLE
!!
doubleprecision function mat_urand(iy)
integer              :: iy
integer,save         :: ia
integer,save         :: ic
integer,save         :: itwo=2
integer,save         :: m2=0
integer              :: m
integer,save         :: mic
doubleprecision      :: halfm
doubleprecision,save :: s
doubleprecision      :: datan
doubleprecision      :: dsqrt
!-----------------------------------------------------------------------
   if (m2 .eq. 0) then                                  !  IF FIRST ENTRY, COMPUTE MACHINE INTEGER WORD LENGTH
      m = 1
      INFINITE : do
         m2 = m
         m = itwo*m2
         if (m .le. m2) exit INFINITE
      enddo INFINITE
      halfm = m2
      ia = 8*idint(halfm*datan(1.d0)/8.d0) + 5          !  COMPUTE MULTIPLIER AND INCREMENT FOR LINEAR CONGRUENTIAL METHOD
      ic = 2*idint(halfm*(0.5d0-dsqrt(3.d0)/6.d0)) + 1
      mic = (m2 - ic) + m2
      s = 0.5d0/halfm                                   !  S IS THE SCALE FACTOR FOR CONVERTING TO FLOATING POINT
   endif
!  COMPUTE NEXT RANDOM NUMBER
   iy = iy*ia
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHICH DO NOT ALLOW INTEGER OVERFLOW ON ADDITION
   if (iy .gt. mic) iy = (iy - m2) - m2
   iy = iy + ic
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE THE WORD LENGTH FOR ADDITION IS GREATER THAN FOR MULTIPLICATION
   if (iy/2 .gt. m2) iy = (iy - m2) - m2
!  THE FOLLOWING STATEMENT IS FOR COMPUTERS WHERE INTEGER OVERFLOW AFFECTS THE SIGN BIT
   if (iy .lt. 0) iy = (iy + m2) + m2
   mat_urand = dble(iy)*s
end function mat_urand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wnrm2(n,xr,xi,incx)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
integer         :: ix
integer         :: i
   !     norm2(x)
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         s = mat_pythag(s,xr(ix))
         s = mat_pythag(s,xi(ix))
         ix = ix + incx
      enddo
   endif
   mat_wnrm2 = s
   end function mat_wnrm2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wasum(n,xr,xi,incx)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
integer         :: ix
integer         :: i

   !     norm1(x)
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      do i = 1, n
         s = mat_flop(s + dabs(xr(ix)) + dabs(xi(ix)))
         ix = ix + incx
      enddo
   endif
   mat_wasum = s
end function mat_wasum
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotui(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy
doubleprecision :: s
integer         :: ix
integer         :: iy
integer         :: i
   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = s + xr(ix)*yi(iy) + xi(ix)*yr(iy)
         if (s .ne. 0.0d0) s = mat_flop(s)
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
   mat_wdotui = s
end function mat_wdotui
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotcr(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

doubleprecision :: s
integer         :: ix
integer         :: iy
integer         :: i

   s = 0.0d0
   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1
      do i = 1, n
         s = mat_flop(s + xr(ix)*yr(iy) + xi(ix)*yi(iy))
         ix = ix + incx
         iy = iy + incy
      enddo
   endif
   mat_wdotcr = s
end function mat_wdotcr
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_wdotci(n,xr,xi,incx,yr,yi,incy)
integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: incy

integer         :: ix
integer         :: iy
integer         :: i
doubleprecision :: s

   s = 0.0d0

   if (n .gt. 0) then
      ix = 1
      iy = 1
      if (incx.lt.0) ix = (-n+1)*incx + 1
      if (incy.lt.0) iy = (-n+1)*incy + 1

      do i = 1, n
         s = s + xr(ix)*yi(iy) - xi(ix)*yr(iy)
         if (s .ne. 0.0d0) s = mat_flop(s)
         ix = ix + incx
         iy = iy + incy
      enddo

   endif

   mat_wdotci = s
end function mat_wdotci
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
integer function mat_iwamax(n,xr,xi,incx)

! ident_38="@(#)M_matrix::mat_iwamax(3fp):index of norminf(x)"

integer         :: n
doubleprecision :: xr(*)
doubleprecision :: xi(*)
integer         :: incx
doubleprecision :: s
doubleprecision :: p
integer         :: i, k
integer         :: ix

   k = 0
   if (n .gt. 0) then
      k = 1
      s = 0.0d0
      ix = 1
      do i = 1, n
         p = dabs(xr(ix)) + dabs(xi(ix))
         if (p .gt. s) k = i
         if (p .gt. s) s = p
         ix = ix + incx
      enddo
   endif
   mat_iwamax = k
end function mat_iwamax
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mat_flop(3fp) - [M_matrix] count and possibly chop each floating point operation
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    Count and possibly chop each floating point operation.
!!
!!    SYSTEM DEPENDENT FUNCTION
!!##OPTIONS
!!
!!##NOTES
!!    FLP(1)  IS FLOP COUNTER
!!    FLP(2)  IS NUMBER OF PLACES TO BE CHOPPED
doubleprecision function mat_flop(x)
doubleprecision,intent(in) :: x
doubleprecision            :: mask(14),xx,mm
integer                    :: k
logical                    :: lx(2),lm(2)
equivalence (lx(1),xx),(lm(1),mm)
equivalence (mask(1),mas(1,1))
!>>>>>>>>>>>>>>>>>>
!*!GFORTRAN BUG in 8.3
!*!real,save                  :: mas(2,14)=reshape([ &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'fff0ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'ff00ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'f000ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000ffff',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000fff0',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000ff00',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'0000f000',kind=kind(0.0)),     &
!*!   & real(Z'ffffffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'fff0ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'ff00ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'f000ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000ffff',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000fff0',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0)),     &
!*!   & real(Z'0000ff80',kind=kind(0.0)),real(Z'00000000',kind=kind(0.0))],shape(mas))
integer :: i,j
logical,save :: setup=.false.
real,save                  :: mas(2,14)
character(len=8),save      :: setmas(2,14)=reshape([ &
   & 'ffffffff','fff0ffff', &
   & 'ffffffff','ff00ffff', &
   & 'ffffffff','f000ffff', &
   & 'ffffffff','0000ffff', &
   & 'ffffffff','0000fff0', &
   & 'ffffffff','0000ff00', &
   & 'ffffffff','0000f000', &
   & 'ffffffff','00000000', &
   & 'fff0ffff','00000000', &
   & 'ff00ffff','00000000', &
   & 'f000ffff','00000000', &
   & '0000ffff','00000000', &
   & '0000fff0','00000000', &
   & '0000ff80','00000000'],shape(mas))
   if(.not.setup)then
      do i=1,2
         do j=1,14
            read(setmas(i,j),'(z8)')mas(i,j)
         enddo
      enddo
      setup=.true.
   endif
!<<<<<<<<<<<<<<<<<<

   flp(1) = flp(1) + 1
   k = flp(2)

   select case(k)
   case(:0)
      mat_flop = x
   case(1:15)
      mat_flop = 0.0d0
   case default
      xx = x
      mm = mask(k)
      lx(1) = lx(1) .and. lm(1)
      lx(2) = lx(2) .and. lm(2)
      mat_flop = xx
   end select

end function mat_flop
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
logical function mat_eqid(x,y)

!     check for equality of two names

integer,intent(in) :: x(4)
integer,intent(in) :: y(4)

integer            :: i

   mat_eqid = .true.

   do i = 1, 4
      mat_eqid = mat_eqid .and. (x(i).eq.y(i))
   enddo

end function mat_eqid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
doubleprecision function mat_round(x)
doubleprecision      :: x,y,z,e
doubleprecision,save :: h=1.0d9
      z = dabs(x)
      y = z + 1.0d0
      if (y .eq. z) goto 40
      y = 0.0d0
      e = h
   10 continue
      if (e .ge. z) goto 20
         e = 2.0d0*e
         goto 10
   20 continue
      if (e .le. h) goto 30
         if (e .le. z) y = y + e
         if (e .le. z) z = z - e
         e = e/2.0d0
         goto 20
   30 continue
      z = idint(z + 0.5d0)
      y = y + z
      if (x .lt. 0.0d0) y = -y
      mat_round = y
      return
   40 continue
      mat_round = x
end function mat_round
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_make_help(io)
integer,intent(in) :: io
write(io,'(a)')'INTRO Welcome to MAT88.'
write(io,'(a)')''
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | HELP Sections     | Available Topics                           |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Documentation     |   help    NEWS   what                      |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Syntax            |      <       >      (      )      =      . |'
write(io,'(a)')'       |                   |      ,       ;   semi      \      /      '' |'
write(io,'(a)')'       |                   |      +       -      *      :               |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Variables         |    ans   clear    who                      |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Macros            |  macro                                     |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Basic Functions   |    FUN    atan    cos    exp    log    sin |'
write(io,'(a)')'       |                   |   sqrt    abs   round   real   imag  conjg |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | High Level        |           chop   cond  conjg    det   diag |'
write(io,'(a)')'       | Functions         |    eig     eye   hess   hilb           inv |'
write(io,'(a)')'       |                   |   kron      lu  magic   ones   orth   pinv |'
write(io,'(a)')'       |                   |   poly    prod     qr   rand   rank  rcond |'
write(io,'(a)')'       |                   |    rat           rref  roots         schur |'
write(io,'(a)')'       |                   |   size     sum    svd   tril   triu   user |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Flow Control      |   else     end     if    for  while   exit |'
write(io,'(a)')'       |                   |   quit                                     |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Accessing Files   |   FILE   diary   exec   load  print   save |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Output Options    |  lines    long  short   disp   plot        |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Performance Info. |  flops    flps                             |'
write(io,'(a)')'       #-------------------#--------------------------------------------#'
write(io,'(a)')'       | Miscellaneous     |   char    EDIT  debug    eps     sh        |'
write(io,'(a)')'       #----------------------------------------------------------------#'
write(io,'(a)')'      Here are a few sample statements:'
write(io,'(a)')''
write(io,'(a)')'       A = <1 2; 3 4>'
write(io,'(a)')'       b = <5 6>'''
write(io,'(a)')'       x = A\b'
write(io,'(a)')'       <V,D> = eig(A),  norm(A-V*D/V)'
write(io,'(a)')'       help \ , help eig'
write(io,'(a)')'       exec(''demo'',7)'
write(io,'(a)')''
write(io,'(a)')'      For more information, generate the MAT88 Users'' Guide'
write(io,'(a)')'      using'
write(io,'(a)')''
write(io,'(a)')'        doc(''manual.txt'')'
write(io,'(a)')'================================================================================'
write(io,'(a)')'DOCUMENTATION'
write(io,'(a)')''
write(io,'(a)')'help  HELP gives assistance.'
write(io,'(a)')'      HELP HELP obviously prints this message.'
write(io,'(a)')'      To see all the HELP messages, generate the manual'
write(io,'(a)')'      using "doc(''help.txt'')".'
write(io,'(a)')''
write(io,'(a)')'NEWS  MAT88 NEWS dated May, 1981.'
write(io,'(a)')''
write(io,'(a)')'      This is a port of the Argonne National Lab. FORTRAN 77 MATLAB'
write(io,'(a)')'      routine circa 1981. This port is intended to be used primarily'
write(io,'(a)')'      by families of FORTRAN programs that wish to add a consistent'
write(io,'(a)')'      interactive "calculator" mode.'
write(io,'(a)')''
write(io,'(a)')'      COMMAND HISTORY'
write(io,'(a)')''
write(io,'(a)')'      A "." on a line by itself calls the command history mode. Enter'
write(io,'(a)')'      "?" after entering the mode for details.'
write(io,'(a)')''
write(io,'(a)')'what  Lists commands and functions currently available.'
write(io,'(a)')''
write(io,'(a)')'sh    Starts the command shell interactively.'
write(io,'(a)')'      If any characters follow the remainder of the line (restricted to'
write(io,'(a)')'      the MAT88 character set) is passed to the sh shell.'
write(io,'(a)')'================================================================================'
write(io,'(a)')'SYNTAX'
write(io,'(a)')''
write(io,'(a)')'<     < > Brackets used in forming vectors and matrices.'
write(io,'(a)')'      <6.9 9.64 SQRT(-1)> is a vector with three elements'
write(io,'(a)')'      separated by blanks. <1+I 2-I 3> and <1 +I 2 -I 3> are not'
write(io,'(a)')'      the same. The first has three elements, the second has five.'
write(io,'(a)')'      <11 12 13; 21 22 23> is a 2 by 3 matrix. The semicolon ends'
write(io,'(a)')'      the first row.'
write(io,'(a)')''
write(io,'(a)')'      Vectors and matrices can be used inside < > brackets.'
write(io,'(a)')'      <A B; C> is allowed if the number of rows of A equals'
write(io,'(a)')'      the number of rows of B and the number of columns of A'
write(io,'(a)')'      plus the number of columns of B equals the number of'
write(io,'(a)')'      columns of C. This rule generalizes in a hopefully'
write(io,'(a)')'      obvious way to allow fairly complicated constructions.'
write(io,'(a)')''
write(io,'(a)')'      A = < > stores an empty matrix in A, thereby removing it'
write(io,'(a)')'      from the list of current variables.'
write(io,'(a)')''
write(io,'(a)')'      For the use of < and > on the left of the = in multiple'
write(io,'(a)')'      assignment statements, see LU, EIG, SVD and so on.'
write(io,'(a)')''
write(io,'(a)')'      In WHILE and IF clauses, <> means less than or greater'
write(io,'(a)')'      than, i.e. not equal, < means less than, > means greater'
write(io,'(a)')'      than, <= means less than or equal, >= means greater than or'
write(io,'(a)')'      equal.'
write(io,'(a)')''
write(io,'(a)')'      For the use of > and < to delineate macros, see MACRO.'
write(io,'(a)')''
write(io,'(a)')'>     See < . Also see MACRO.'
write(io,'(a)')''
write(io,'(a)')'(     ( ) Used to indicate precedence in arithmetic expressions'
write(io,'(a)')'      in the usual way. Used to enclose arguments of functions'
write(io,'(a)')'      in the usual way. Used to enclose subscripts of vectors'
write(io,'(a)')'      and matrices in a manner somewhat more general than the'
write(io,'(a)')'      usual way. If X and V are vectors, then X(V) is'
write(io,'(a)')'      <X(V(1)), X(V(2)), ..., X(V(N))>. The components of V'
write(io,'(a)')'      are rounded to nearest integers and used as subscripts. An'
write(io,'(a)')'      error occurs if any such subscript is less than 1 or'
write(io,'(a)')'      greater than the dimension of X. Some examples:'
write(io,'(a)')''
write(io,'(a)')'         X(3) is the third element of X .'
write(io,'(a)')'         X(<1 2 3>) is the first three elements of X. So is'
write(io,'(a)')'         X(<SQRT(2), SQRT(3), 4*ATAN(1)>) .'
write(io,'(a)')'         If X has N components, X(N:-1:1) reverses them.'
write(io,'(a)')''
write(io,'(a)')'      The same indirect subscripting is used in matrices. If V'
write(io,'(a)')'      has M components and W has N components, then A(V,W)'
write(io,'(a)')'      is the M by N matrix formed from the elements of A whose'
write(io,'(a)')'      subscripts are the elements of V and W. For example...'
write(io,'(a)')'      A(<1,5>,:) = A(<5,1>,:) interchanges rows 1 and 5 of A.'
write(io,'(a)')''
write(io,'(a)')')     See ( .'
write(io,'(a)')''
write(io,'(a)')'=     Used in assignment statements and to mean equality in WHILE'
write(io,'(a)')'      and IF clauses.'
write(io,'(a)')''
write(io,'(a)')'.     Decimal point. 314/100, 3.14 and .314E1 are all the'
write(io,'(a)')'      same.'
write(io,'(a)')''
write(io,'(a)')'      Element-by-element multiplicative operations are obtained'
write(io,'(a)')'      using .* , ./ , or .\ . For example, C = A ./ B is the'
write(io,'(a)')'      matrix with elements c(i,j) = a(i,j)/b(i,j) .'
write(io,'(a)')''
write(io,'(a)')'      Kronecker tensor products and quotients are obtained with'
write(io,'(a)')'      .*. , ./. and .\. . See KRON.'
write(io,'(a)')''
write(io,'(a)')'      Two or more points at the end of the line indicate'
write(io,'(a)')'      continuation. The total line length limit is 1024'
write(io,'(a)')'      characters.'
write(io,'(a)')''
write(io,'(a)')',     Used to separate matrix subscripts and function arguments.'
write(io,'(a)')'      Used at the end of FOR, WHILE and IF clauses. Used to'
write(io,'(a)')'      separate statements in multi-statement lines. In this'
write(io,'(a)')'      situation, it may be replaced by a semicolon to suppress'
write(io,'(a)')'      printing.'
write(io,'(a)')''
write(io,'(a)')';     Used inside brackets to end rows.'
write(io,'(a)')''
write(io,'(a)')'      Used after an expression or statement to suppress printing.'
write(io,'(a)')'      See SEMI.'
write(io,'(a)')''
write(io,'(a)')'\     Backslash or matrix left division. A\B is roughly the'
write(io,'(a)')'      same as INV(A)*B, except it is computed in a different'
write(io,'(a)')'      way. If A is an N by N matrix and B is a column vector'
write(io,'(a)')'      with N components, or a matrix with several such columns,'
write(io,'(a)')'      then X = A\B is the solution to the equation A*X = B'
write(io,'(a)')'      computed by Gaussian elimination. A warning message is'
write(io,'(a)')'      printed if A is badly scaled or nearly singular.'
write(io,'(a)')'      A\EYE produces the inverse of A .'
write(io,'(a)')''
write(io,'(a)')'      If A is an M by N matrix with M < or > N and B is a'
write(io,'(a)')'      column vector with M components, or a matrix with several'
write(io,'(a)')'      such columns, then X = A\B is the solution in the least'
write(io,'(a)')'      squares sense to the under- or overdetermined system of'
write(io,'(a)')'      equations A*X = B. The effective rank, K, of A is'
write(io,'(a)')'      determined from the QR decomposition with pivoting. A'
write(io,'(a)')'      solution X is computed which has at most K nonzero'
write(io,'(a)')'      components per column. If K < N this will usually not be'
write(io,'(a)')'      the same solution as PINV(A)*B .'
write(io,'(a)')'      A\EYE produces a generalized inverse of A.'
write(io,'(a)')''
write(io,'(a)')'      If A and B have the same dimensions, then A .\ B has'
write(io,'(a)')'      elements a(i,j)\b(i,j) .'
write(io,'(a)')''
write(io,'(a)')'      Also, see EDIT.'
write(io,'(a)')''
write(io,'(a)')'/     Slash or matrix right division. B/A is roughly the same'
write(io,'(a)')'      as B*INV(A) . More precisely, B/A = (A''\B'')'' . See \ .'
write(io,'(a)')''
write(io,'(a)')'      IF A and B have the same dimensions, then A ./ B has'
write(io,'(a)')'      elements a(i,j)/b(i,j) .'
write(io,'(a)')''
write(io,'(a)')'      Two or more slashes together on a line indicate a logical'
write(io,'(a)')'      end of line. Any following text is ignored.'
write(io,'(a)')''
write(io,'(a)')'''     Transpose. X'' is the complex conjugate transpose of X .'
write(io,'(a)')'      Quote. ''ANY TEXT'' is a vector whose components are the'
write(io,'(a)')'      MAT88 internal codes for the characters. A quote within'
write(io,'(a)')'      the text is indicated by two quotes. See DISP and FILE .'
write(io,'(a)')''
write(io,'(a)')'+     Addition. X + Y . X and Y must have the same dimensions.'
write(io,'(a)')''
write(io,'(a)')'-     Subtraction. X - Y . X and Y must have the same'
write(io,'(a)')'      dimensions.'
write(io,'(a)')''
write(io,'(a)')'*     Matrix multiplication, X*Y . Any scalar (1 by 1 matrix)'
write(io,'(a)')'      may multiply anything. Otherwise, the number of columns of'
write(io,'(a)')'      X must equal the number of rows of Y .'
write(io,'(a)')''
write(io,'(a)')'      Element-by-element multiplication is obtained with X .* Y .'
write(io,'(a)')''
write(io,'(a)')'      The Kronecker tensor product is denoted by X .*. Y .'
write(io,'(a)')''
write(io,'(a)')'      Powers. X**p is X to the p power. p must be a'
write(io,'(a)')'      scalar. If X is a matrix, see FUN .'
write(io,'(a)')''
write(io,'(a)')':     Colon. Used in subscripts, FOR iterations and possibly'
write(io,'(a)')'      elsewhere.'
write(io,'(a)')''
write(io,'(a)')'         J:K  is the same as  <J, J+1, ..., K>'
write(io,'(a)')'         J:K  is empty if  J > K .'
write(io,'(a)')'         J:I:K  is the same as  <J, J+I, J+2I, ..., K>'
write(io,'(a)')'         J:I:K  is empty if  I > 0 and J > K or if I < 0 and J < K .'
write(io,'(a)')''
write(io,'(a)')'      The colon notation can be used to pick out selected rows,'
write(io,'(a)')'      columns and elements of vectors and matrices.'
write(io,'(a)')''
write(io,'(a)')'         A(:) is all the elements of A, regarded as a single column.'
write(io,'(a)')'         A(:,J)  is the J-th column of A'
write(io,'(a)')'         A(J:K)  is A(J),A(J+1),...,A(K)'
write(io,'(a)')'         A(:,J:K)  is A(:,J),A(:,J+1),...,A(:,K) and so on.'
write(io,'(a)')''
write(io,'(a)')'      For the use of the colon in the FOR statement, See FOR .'
write(io,'(a)')''
write(io,'(a)')'semi  SEMI toggles the action of semicolons at the end of lines.'
write(io,'(a)')'      It will make semicolons cause rather than suppress printing.'
write(io,'(a)')'      A second SEMI restores the initial interpretation.'
write(io,'(a)')'================================================================================'
write(io,'(a)')'VARIABLES'
write(io,'(a)')''
write(io,'(a)')'ans   Variable created automatically when expressions are not'
write(io,'(a)')'      assigned to anything else.'
write(io,'(a)')''
write(io,'(a)')'clear  Erases all variables, except EPS, FLOP, EYE and RAND.'
write(io,'(a)')'       X = <> erases only variable X . So does CLEAR X .'
write(io,'(a)')''
write(io,'(a)')'who   Lists current variables.'
write(io,'(a)')'================================================================================'
write(io,'(a)')'MACROS'
write(io,'(a)')''
write(io,'(a)')'MACRO  The macro facility involves text and inward pointing angle'
write(io,'(a)')'       brackets. If STRING is the source text for any MAT88'
write(io,'(a)')'       expression or statement, then'
write(io,'(a)')''
write(io,'(a)')'             t = ''STRING'';'
write(io,'(a)')'       encodes the text as a vector of integers and stores that'
write(io,'(a)')'       vector in t. DISP(t) will print the text and'
write(io,'(a)')''
write(io,'(a)')'             >t<'
write(io,'(a)')'       causes the text to be interpreted, either as a statement or'
write(io,'(a)')'       as a factor in an expression. For example'
write(io,'(a)')''
write(io,'(a)')'             t = ''1/(i+j-1)'';'
write(io,'(a)')'             disp(t)'
write(io,'(a)')'             for i = 1:n, for j = 1:n, a(i,j) = >t<;'
write(io,'(a)')''
write(io,'(a)')'       generates the Hilbert matrix of order n.'
write(io,'(a)')'       Another example showing indexed text,'
write(io,'(a)')''
write(io,'(a)')'             S = <''x = 3            '''
write(io,'(a)')'                  ''y = 4            '''
write(io,'(a)')'                  ''z = sqrt(x*x+y*y)''>'
write(io,'(a)')'             for k = 1:3, >S(k,:)<'
write(io,'(a)')''
write(io,'(a)')'       It is necessary that the strings making up the "rows" of'
write(io,'(a)')'       the "matrix" S have the same lengths.'
write(io,'(a)')''
write(io,'(a)')'================================================================================'
write(io,'(a)')'BASIC FUNCTIONS'
write(io,'(a)')''
write(io,'(a)')'FUN   For matrix arguments X , the functions SIN, COS, ATAN,'
write(io,'(a)')'      SQRT, LOG, EXP and X**p are computed using eigenvalues D'
write(io,'(a)')'      and eigenvectors V . If <V,D> = EIG(X) then f(X) ='
write(io,'(a)')'      V*f(D)/V . This method may give inaccurate results if V'
write(io,'(a)')'      is badly conditioned. Some idea of the accuracy can be'
write(io,'(a)')'      obtained by comparing X**1 with X .'
write(io,'(a)')'      For vector arguments, the function is applied to each'
write(io,'(a)')'      component.'
write(io,'(a)')''
write(io,'(a)')'atan  ATAN(X) is the arctangent of X . See FUN .'
write(io,'(a)')''
write(io,'(a)')'cos   COS(X) is the cosine of X . See FUN .'
write(io,'(a)')''
write(io,'(a)')'exp   EXP(X) is the exponential of X , e to the X . See FUN.'
write(io,'(a)')''
write(io,'(a)')'log   LOG(X) is the natural logarithm of X. See FUN.'
write(io,'(a)')'      Complex results are produced if X is not positive, or has'
write(io,'(a)')'      nonpositive eigenvalues.'
write(io,'(a)')''
write(io,'(a)')'sin   SIN(X) is the sine of X. See FUN.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'sqrt  SQRT(X) is the square root of X. See FUN. Complex'
write(io,'(a)')'      results are produced if X is not positive, or has'
write(io,'(a)')'      nonpositive eigenvalues.'
write(io,'(a)')''
write(io,'(a)')'================================================================================'
write(io,'(a)')'HIGH LEVEL FUNCTIONS'
write(io,'(a)')''
write(io,'(a)')'abs   ABS(X) is the absolute value, or complex modulus, of the'
write(io,'(a)')'      elements of X .'
write(io,'(a)')''
write(io,'(a)')'base  BASE(X,B) is a vector containing the base B representation'
write(io,'(a)')'      of X. This is often used in conjunction with DISPLAY.'
write(io,'(a)')'      DISPLAY(X,B) is the same as DISPLAY(BASE(X,B)). For'
write(io,'(a)')'      example, DISP(4*ATAN(1),16) prints the hexadecimal'
write(io,'(a)')'      representation of pi.'
write(io,'(a)')''
write(io,'(a)')'chol  Cholesky factorization. CHOL(X) uses only the diagonal'
write(io,'(a)')'      and upper triangle of X. The lower triangular is assumed'
write(io,'(a)')'      to be the (complex conjugate) transpose of the upper. If'
write(io,'(a)')'      X is positive definite, then R = CHOL(X) produces an'
write(io,'(a)')'      upper triangular R so that R''*R = X . If X is not'
write(io,'(a)')'      positive definite, an error message is printed.'
write(io,'(a)')''
write(io,'(a)')'chop  Truncate arithmetic. CHOP(P) causes P places to be chopped'
write(io,'(a)')'      off after each arithmetic operation in subsequent'
write(io,'(a)')'      computations. This means P hexadecimal digits on some'
write(io,'(a)')'      computers and P octal digits on others. CHOP(0) restores'
write(io,'(a)')'      full precision.'
write(io,'(a)')''
write(io,'(a)')'cond  Condition number in 2-norm. COND(X) is the ratio of the'
write(io,'(a)')'      largest singular value of X to the smallest.'
write(io,'(a)')''
write(io,'(a)')'conjg  CONJG(X) is the complex conjugate of X .'
write(io,'(a)')''
write(io,'(a)')'det   DET(X) is the determinant of the square matrix X .'
write(io,'(a)')''
write(io,'(a)')'diag  If V is a row or column vector with N components,'
write(io,'(a)')'      DIAG(V,K) is a square matrix of order N+ABS(K) with the'
write(io,'(a)')'      elements of V on the K-th diagonal. K = 0 is the main'
write(io,'(a)')'      diagonal, K > 0 is above the main diagonal and K < 0 is'
write(io,'(a)')'      below the main diagonal. DIAG(V) simply puts V on the'
write(io,'(a)')'      main diagonal.'
write(io,'(a)')'      eg. DIAG(-M:M) + DIAG(ONES(2*M,1),1) + DIAG(ONES(2*M,1),-1)'
write(io,'(a)')'      produces a tridiagonal matrix of order 2*M+1 .'
write(io,'(a)')'      If X is a matrix, DIAG(X,K) is a column vector formed'
write(io,'(a)')'      from the elements of the K-th diagonal of X.'
write(io,'(a)')'      DIAG(X) is the main diagonal of X.'
write(io,'(a)')'      DIAG(DIAG(X)) is a diagonal matrix .'
write(io,'(a)')''
write(io,'(a)')'eig   Eigenvalues and eigenvectors.'
write(io,'(a)')'      EIG(X) is a vector containing the eigenvalues of a square'
write(io,'(a)')'      matrix X.'
write(io,'(a)')'      <V,D> = EIG(X) produces a diagonal matrix D of'
write(io,'(a)')'      eigenvalues and a full matrix V whose columns are the'
write(io,'(a)')'      corresponding eigenvectors so that X*V = V*D .'
write(io,'(a)')''
write(io,'(a)')'eye   Identity matrix. EYE(N) is the N by N identity matrix.'
write(io,'(a)')'      EYE(M,N) is an M by N matrix with 1''s on the diagonal and'
write(io,'(a)')'      zeros elsewhere. EYE(A) is the same size as A. EYE'
write(io,'(a)')'      with no arguments is an identity matrix of whatever order'
write(io,'(a)')'      is appropriate in the context. For example A + 3*EYE'
write(io,'(a)')'      adds 3 to each diagonal element of A.'
write(io,'(a)')''
write(io,'(a)')'hess  Hessenberg form. The Hessenberg form of a matrix is zero'
write(io,'(a)')'      below the first subdiagonal. If the matrix is symmetric or'
write(io,'(a)')'      Hermitian, the form is tridiagonal. <P,H> = HESS(A)'
write(io,'(a)')'      produces a unitary matrix P and a Hessenberg matrix H so'
write(io,'(a)')'      that A = P*H*P''. By itself, HESS(A) returns H.'
write(io,'(a)')''
write(io,'(a)')'hilb  Inverse Hilbert matrix. HILB(N) is the inverse of the N'
write(io,'(a)')'      by N  matrix with elements 1/(i+j-1), which is a famous'
write(io,'(a)')'      example of a badly conditioned matrix. The result is exact'
write(io,'(a)')'      for N less than about 15, depending upon the computer.'
write(io,'(a)')''
write(io,'(a)')'imag  IMAG(X) is the imaginary part of X .'
write(io,'(a)')''
write(io,'(a)')'inv   INV(X) is the inverse of the square matrix X . A warning'
write(io,'(a)')'      message is printed if X is badly scaled or nearly'
write(io,'(a)')'      singular.'
write(io,'(a)')''
write(io,'(a)')'kron  KRON(X,Y) is the Kronecker tensor product of X and Y. It'
write(io,'(a)')'      is also denoted by X .*. Y . The result is a large matrix'
write(io,'(a)')'      formed by taking all possible products between the elements'
write(io,'(a)')'      of X and those of Y . For example, if X is 2 by 3, then'
write(io,'(a)')'      X .*. Y is'
write(io,'(a)')''
write(io,'(a)')'            < x(1,1)*Y  x(1,2)*Y  x(1,3)*Y'
write(io,'(a)')'              x(2,1)*Y  x(2,2)*Y  x(2,3)*Y >'
write(io,'(a)')''
write(io,'(a)')'      The five-point discrete Laplacian for an n-by-n grid can be'
write(io,'(a)')'      generated by'
write(io,'(a)')''
write(io,'(a)')'            T = diag(ones(n-1,1),1);  T = T + T'';  I = EYE(T);'
write(io,'(a)')'            A = T.*.I + I.*.T - 4*EYE;'
write(io,'(a)')''
write(io,'(a)')'      Just in case they might be useful, MAT88 includes'
write(io,'(a)')'      constructions called Kronecker tensor quotients, denoted by'
write(io,'(a)')'      X ./. Y and X .\. Y . They are obtained by replacing the'
write(io,'(a)')'      elementwise multiplications in X .*. Y with divisions.'
write(io,'(a)')''
write(io,'(a)')'lu    Factors from Gaussian elimination. <L,U> = LU(X) stores a'
write(io,'(a)')'      upper triangular matrix in U and a ''psychologically lower'
write(io,'(a)')'      triangular matrix'', i.e. a product of lower triangular and'
write(io,'(a)')'      permutation matrices, in L , so that X = L*U . By itself,'
write(io,'(a)')'      LU(X) returns the output from CGEFA .'
write(io,'(a)')''
write(io,'(a)')'magic  Magic square. MAGIC(N) is an N by N matrix constructed'
write(io,'(a)')'       from the integers 1 through N**2 with equal row and column'
write(io,'(a)')'       sums.'
write(io,'(a)')''
write(io,'(a)')'norm  For matrices..'
write(io,'(a)')''
write(io,'(a)')'       NORM(X)  is the largest singular value of X .'
write(io,'(a)')'       NORM(X,1)  is the 1-norm of X .'
write(io,'(a)')'       NORM(X,2)  is the same as NORM(X) .'
write(io,'(a)')'       NORM(X,''INF'')  is the infinity norm of X .'
write(io,'(a)')'       NORM(X,''FRO'')  is the F-norm, i.e. SQRT(SUM(DIAG(X''*X))) .'
write(io,'(a)')''
write(io,'(a)')'      For vectors..'
write(io,'(a)')''
write(io,'(a)')'       NORM(V,P) = (SUM(V(I)**P))**(1/P) .'
write(io,'(a)')'       NORM(V) = NORM(V,2) .'
write(io,'(a)')'       NORM(V,''INF'') = MAX(ABS(V(I))) .'
write(io,'(a)')''
write(io,'(a)')'ones  All ones. ONES(N) is an N by N matrix of ones. ONES(M,N)'
write(io,'(a)')'      is an M by N matrix of ones . ONES(A) is the same size as'
write(io,'(a)')'      A and all ones .'
write(io,'(a)')''
write(io,'(a)')'orth  Orthogonalization. Q = ORTH(X) is a matrix with'
write(io,'(a)')'      orthonormal columns, i.e. Q''*Q = EYE, which span the same'
write(io,'(a)')'      space as the columns of X .'
write(io,'(a)')''
write(io,'(a)')'pinv  Pseudoinverse.'
write(io,'(a)')''
write(io,'(a)')'      X = PINV(A) produces a matrix X of the same dimensions as A'''
write(io,'(a)')'      so that A*X*A = A , X*A*X = X and AX and XA are Hermitian . The'
write(io,'(a)')'      computation is based on SVD(A) and any singular values less'
write(io,'(a)')'      than a tolerance are treated as zero. The default tolerance is'
write(io,'(a)')'      NORM(size(A),''inf'')*NORM(A)*EPS. This tolerance may be overridden'
write(io,'(a)')'      with X = PINV(A,tol). See RANK.'
write(io,'(a)')''
write(io,'(a)')'poly  Characteristic polynomial.'
write(io,'(a)')''
write(io,'(a)')'      If A is an N by N matrix, POLY(A) is a column vector with'
write(io,'(a)')'      N+1 elements which are the coefficients of the characteristic'
write(io,'(a)')'      polynomial, DET(lambda*EYE - A) .'
write(io,'(a)')''
write(io,'(a)')'      If V is a vector, POLY(V) is a vector whose elements are the'
write(io,'(a)')'      coefficients of the polynomial whose roots are the elements of V'
write(io,'(a)')'      . For vectors, ROOTS and POLY are inverse functions of each other,'
write(io,'(a)')'      up to ordering, scaling, and roundoff error.'
write(io,'(a)')''
write(io,'(a)')'      ROOTS(POLY(1:20)) generates Wilkinson''s famous example.'
write(io,'(a)')''
write(io,'(a)')'prod  PROD(X) is the product of all the elements of X .'
write(io,'(a)')''
write(io,'(a)')'qr    Orthogonal-triangular decomposition.'
write(io,'(a)')'      <Q,R> = QR(X) produces an upper triangular matrix R of'
write(io,'(a)')'      the same dimension as X and a unitary matrix Q so that'
write(io,'(a)')'      X = Q*R .'
write(io,'(a)')''
write(io,'(a)')'      <Q,R,E> = QR(X) produces a permutation matrix E, an'
write(io,'(a)')'      upper triangular R with decreasing diagonal elements and'
write(io,'(a)')'      a unitary Q so that X*E = Q*R .'
write(io,'(a)')'      By itself, QR(X) returns the output of CQRDC. TRIU(QR(X))'
write(io,'(a)')'      is R .'
write(io,'(a)')''
write(io,'(a)')'rand  Random numbers and matrices. RAND(N) is an N by N matrix'
write(io,'(a)')'      with random entries. RAND(M,N) is an M by N matrix with'
write(io,'(a)')'      random entries. RAND(A) is the same size as A. RAND'
write(io,'(a)')'      with no arguments is a scalar whose value changes each time'
write(io,'(a)')'      it is referenced.'
write(io,'(a)')''
write(io,'(a)')'      Ordinarily, random numbers are uniformly distributed in'
write(io,'(a)')'      the interval (0.0,1.0). RAND(''NORMAL'') switches to a'
write(io,'(a)')'      normal distribution with mean 0.0 and variance 1.0.'
write(io,'(a)')'      RAND(''UNIFORM'') switches back to the uniform distribution.'
write(io,'(a)')'      RAND(''SEED'') returns the current value of the seed for the'
write(io,'(a)')'      generator. RAND(''SEED'',n) sets the seed to n.'
write(io,'(a)')'      RAND(''SEED'',0) resets the seed to 0, its value when MAT88'
write(io,'(a)')'      is first entered.'
write(io,'(a)')''
write(io,'(a)')'rank  Rank. K = RANK(X) is the number of singular values of X'
write(io,'(a)')'      that are larger than NORM(size(X),''inf'')*NORM(X)*EPS.'
write(io,'(a)')'      K = RANK(X,tol) is the number of singular values of X that'
write(io,'(a)')'      are larger than tol.'
write(io,'(a)')''
write(io,'(a)')'rcond  RCOND(X) is an estimate for the reciprocal of the'
write(io,'(a)')'       condition of X in the 1-norm obtained by the LINPACK'
write(io,'(a)')'       condition estimator. If X is well conditioned, RCOND(X)'
write(io,'(a)')'       is near 1.0. If X is badly conditioned, RCOND(X) is'
write(io,'(a)')'       near 0.0.'
write(io,'(a)')'       <R, Z> = RCOND(A) sets R to RCOND(A) and also produces a'
write(io,'(a)')'       vector Z so that'
write(io,'(a)')''
write(io,'(a)')'                 NORM(A*Z,1) = R*NORM(A,1)*NORM(Z,1)'
write(io,'(a)')''
write(io,'(a)')'       So, if RCOND(A) is small, then Z is an approximate null'
write(io,'(a)')'       vector.'
write(io,'(a)')''
write(io,'(a)')'rat   An experimental function which attempts to remove the'
write(io,'(a)')'      roundoff error from results that should be "simple"'
write(io,'(a)')'      rational numbers.'
write(io,'(a)')'      RAT(X) approximates each element of X by a continued'
write(io,'(a)')'      fraction of the form'
write(io,'(a)')''
write(io,'(a)')'                a/b = d1 + 1/(d2 + 1/(d3 + ... + 1/dk))'
write(io,'(a)')''
write(io,'(a)')'      with k <= len, integer di and abs(di) <= max . The default'
write(io,'(a)')'      values of the parameters are len = 5 and max = 100.'
write(io,'(a)')'      RAT(len,max) changes the default values. Increasing either'
write(io,'(a)')'      len or max increases the number of possible fractions.'
write(io,'(a)')'      <A,B> = RAT(X) produces integer matrices A and B so that'
write(io,'(a)')''
write(io,'(a)')'                A ./ B  =  RAT(X)'
write(io,'(a)')''
write(io,'(a)')'      Some examples:'
write(io,'(a)')''
write(io,'(a)')'            long'
write(io,'(a)')'            T = hilb(6), X = inv(T)'
write(io,'(a)')'            <A,B> = rat(X)'
write(io,'(a)')'            H = A ./ B, S = inv(H)'
write(io,'(a)')''
write(io,'(a)')'            short e'
write(io,'(a)')'            d = 1:8,  e = ones(d),  A = abs(d''*e - e''*d)'
write(io,'(a)')'            X = inv(A)'
write(io,'(a)')'            rat(X)'
write(io,'(a)')'            display(ans)'
write(io,'(a)')''
write(io,'(a)')'real  REAL(X) is the real part of X.'
write(io,'(a)')''
write(io,'(a)')'rref  RREF(A) is the reduced row echelon form of the rectangular'
write(io,'(a)')'      matrix. RREF(A,B) is the same as RREF(<A,B>) .'
write(io,'(a)')''
write(io,'(a)')'roots  Find polynomial roots. ROOTS(C) computes the roots of the'
write(io,'(a)')'       polynomial whose coefficients are the elements of the'
write(io,'(a)')'       vector C . If C has N+1 components, the polynomial is'
write(io,'(a)')'       C(1)*X**N + ... + C(N)*X + C(N+1) . See POLY.'
write(io,'(a)')''
write(io,'(a)')'round  ROUND(X) rounds the elements of X to the nearest integers.'
write(io,'(a)')''
write(io,'(a)')'schur  Schur decomposition. <U,T> = SCHUR(X) produces an upper'
write(io,'(a)')'       triangular matrix T , with the eigenvalues of X on the'
write(io,'(a)')'       diagonal, and a unitary matrix U so that X = U*T*U'' and'
write(io,'(a)')'       U''*U = EYE . By itself, SCHUR(X) returns T .'
write(io,'(a)')''
write(io,'(a)')'size  If X is an M by N matrix, then size(X) is <M, N> .'
write(io,'(a)')'      Can also be used with a multiple assignment,'
write(io,'(a)')'            <M, N> = size(X) .'
write(io,'(a)')''
write(io,'(a)')'sum   SUM(X) is the sum of all the elements of X.'
write(io,'(a)')'      SUM(DIAG(X)) is the trace of X.'
write(io,'(a)')''
write(io,'(a)')'svd   Singular value decomposition. <U,S,V> = SVD(X) produces a'
write(io,'(a)')'      diagonal matrix S , of the same dimension as X and with'
write(io,'(a)')'      nonnegative diagonal elements in decreasing order, and'
write(io,'(a)')'      unitary matrices U and V so that X = U*S*V'' .'
write(io,'(a)')'      By itself, SVD(X) returns a vector containing the singular'
write(io,'(a)')'      values.'
write(io,'(a)')'      <U,S,V> = SVD(X,0) produces the "economy size"'
write(io,'(a)')'      decomposition. If X is m by n with m > n, then only the'
write(io,'(a)')'      first n columns of U are computed and S is n by n .'
write(io,'(a)')''
write(io,'(a)')'tril  Lower triangle. TRIL(X) is the lower triangular part of X.'
write(io,'(a)')'      TRIL(X,K) is the elements on and below the K-th diagonal of'
write(io,'(a)')'      X. K = 0 is the main diagonal, K > 0 is above the main'
write(io,'(a)')'      diagonal and K < 0 is below the main diagonal.'
write(io,'(a)')''
write(io,'(a)')'triu  Upper triangle. TRIU(X) is the upper triangular part of X.'
write(io,'(a)')'      TRIU(X,K) is the elements on and above the K-th diagonal of'
write(io,'(a)')'      X. K = 0 is the main diagonal, K > 0 is above the main'
write(io,'(a)')'      diagonal and K < 0 is below the main diagonal.'
write(io,'(a)')''
write(io,'(a)')'user  Allows personal Fortran subroutines to be linked into'
write(io,'(a)')'      MAT88. The subroutine should have the heading'
write(io,'(a)')''
write(io,'(a)')'               SUBROUTINE USER(A,M,N,S,T)'
write(io,'(a)')'               REAL or DOUBLEPRECISION A(M,N),S,T'
write(io,'(a)')''
write(io,'(a)')'      The MAT88 statement Y = USER(X,s,t) results in a call to'
write(io,'(a)')'      the subroutine with a copy of the matrix X stored in the'
write(io,'(a)')'      argument A , its column and row dimensions in M and N ,'
write(io,'(a)')'      and the scalar parameters s and t stored in S and T'
write(io,'(a)')'      . If s and t are omitted, they are set to 0.0 . After'
write(io,'(a)')'      the return, A is stored in Y . The dimensions M and'
write(io,'(a)')'      N may be reset within the subroutine. The statement Y ='
write(io,'(a)')'      USER(K) results in a call with M = 1, N = 1 and A(1,1) ='
write(io,'(a)')'      FLOAT(K) . After the subroutine has been written, it must'
write(io,'(a)')'      be compiled and linked to the MAT88 object code within the'
write(io,'(a)')'      local operating system.'
write(io,'(a)')''
write(io,'(a)')'================================================================================'
write(io,'(a)')'FLOW CONTROL'
write(io,'(a)')''
write(io,'(a)')'else  Used with IF.'
write(io,'(a)')''
write(io,'(a)')'end   Terminates the scope of FOR, WHILE and IF statements.'
write(io,'(a)')'      Without END''s, FOR and WHILE repeat all statements up to'
write(io,'(a)')'      the end of the line. Each END is paired with the closest'
write(io,'(a)')'      previous unpaired FOR or WHILE and serves to terminate its'
write(io,'(a)')'      scope. The line'
write(io,'(a)')''
write(io,'(a)')'         FOR I=1:N, FOR J=1:N, A(I,J)=1/(I+J-1); A'
write(io,'(a)')''
write(io,'(a)')'      would cause A to be printed N**2 times, once for each new'
write(io,'(a)')'      element. On the other hand, the line'
write(io,'(a)')''
write(io,'(a)')'         FOR I=1:N, FOR J=1:N, A(I,J)=1/(I+J-1); END, END, A'
write(io,'(a)')''
write(io,'(a)')'      will lead to only the final printing of A.'
write(io,'(a)')'      Similar considerations apply to WHILE.'
write(io,'(a)')'      EXIT terminates execution of loops or of MAT88 itself.'
write(io,'(a)')''
write(io,'(a)')'if    Conditionally execute statements'
write(io,'(a)')''
write(io,'(a)')'      SIMPLE FORM'
write(io,'(a)')'       Enter'
write(io,'(a)')''
write(io,'(a)')'         if expression rop expression, statements'
write(io,'(a)')''
write(io,'(a)')'      where rop is =, <, >, <=, >=, o <> (not equal). The'
write(io,'(a)')'      statements are executed once if the indicated comparison'
write(io,'(a)')'      between the real parts of the first components of the two'
write(io,'(a)')'      expressions is true, otherwise the statements are skipped.'
write(io,'(a)')''
write(io,'(a)')'      EXAMPLE'
write(io,'(a)')'        Enter'
write(io,'(a)')''
write(io,'(a)')'         if abs(i-j) = 1, a(i,j) = -1;'
write(io,'(a)')''
write(io,'(a)')'      More complicated forms use END in the same way it is used'
write(io,'(a)')'      with FOR and WHILE and use ELSE as an abbreviation for END,'
write(io,'(a)')''
write(io,'(a)')'         if expression not rop expression'
write(io,'(a)')''
write(io,'(a)')'      EXAMPLE'
write(io,'(a)')'        Enter'
write(io,'(a)')''
write(io,'(a)')'         for i = 1:n, for j = 1:n, ...'
write(io,'(a)')'            if i = j, a(i,j) = 2; else if abs(i-j) = 1, a(i,j) = -1; ...'
write(io,'(a)')'            else a(i,j) = 0;'
write(io,'(a)')''
write(io,'(a)')'      An easier way to accomplish the same thing is'
write(io,'(a)')''
write(io,'(a)')'         a = 2*eye(n);'
write(io,'(a)')'         for i = 1:n-1, a(i,i+1) = -1; a(i+1,i) = -1;'
write(io,'(a)')''
write(io,'(a)')'for   Repeat statements a specific number of times.'
write(io,'(a)')''
write(io,'(a)')'         FOR variable = expr, statement, ..., statement, END'
write(io,'(a)')''
write(io,'(a)')'      The END at the end of a line may be omitted. The comma before the'
write(io,'(a)')'      END may also be omitted. The columns of the expression are stored'
write(io,'(a)')'      one at a time in the variable and then the following statements,'
write(io,'(a)')'      up to the END, are executed.  The expression is often of the form'
write(io,'(a)')'      X:Y, in which case its columns are simply scalars. Some examples'
write(io,'(a)')'      (assume N has already been assigned a value).'
write(io,'(a)')''
write(io,'(a)')'       FOR I = 1:N, FOR J = 1:N, A(I,J) = 1/(I+J-1);'
write(io,'(a)')'       FOR J = 2:N-1, A(J,J) = J; END; A'
write(io,'(a)')'       FOR S = 1.0: -0.1: 0.0, ... steps S with increments of -0.1 .'
write(io,'(a)')'       FOR E = EYE(N), ... sets E to the unit N-vectors.'
write(io,'(a)')'       FOR V = A, ... has the same effect as'
write(io,'(a)')'       FOR J = 1:N, V = A(:,J); ... except J is also set here.'
write(io,'(a)')''
write(io,'(a)')'while  Repeat statements an indefinite number of times.'
write(io,'(a)')''
write(io,'(a)')'          WHILE expr rop expr, statement, ..., statement, END'
write(io,'(a)')''
write(io,'(a)')'       where rop is =, <, >, <=, >=, or <> (not equal). The END'
write(io,'(a)')'       at the end of a line may be omitted. The comma before the'
write(io,'(a)')'       END may also be omitted. The commas may be replaced by'
write(io,'(a)')'       semicolons to avoid printing. The statements are'
write(io,'(a)')'       repeatedly executed as long as the indicated comparison'
write(io,'(a)')'       between the real parts of the first components of the two'
write(io,'(a)')'       expressions is true.'
write(io,'(a)')''
write(io,'(a)')'       EXAMPLE'
write(io,'(a)')'       (assume a matrix A is already defined).'
write(io,'(a)')''
write(io,'(a)')'        E = 0*A; F = E + EYE; N = 1;'
write(io,'(a)')'        WHILE NORM(E+F-E,1) > 0, E = E + F; F = A*F/N; N = N + 1;'
write(io,'(a)')'        E'
write(io,'(a)')''
write(io,'(a)')'exit  Causes termination of a FOR or WHILE loop.'
write(io,'(a)')'      If not in a loop, terminates execution of MAT88.'
write(io,'(a)')'      Also see QUIT.'
write(io,'(a)')''
write(io,'(a)')'quit  From the terminal, causes return to the operating system'
write(io,'(a)')'      or other program which invoked MAT88. From inside an'
write(io,'(a)')'      EXEC, causes return to the invoking EXEC, or to the'
write(io,'(a)')'      terminal.'
write(io,'(a)')''
write(io,'(a)')'================================================================================'
write(io,'(a)')'ACCESSING FILES'
write(io,'(a)')''
write(io,'(a)')'FILE  The EXEC, SAVE, LOAD, DIARY, and PRINT functions access'
write(io,'(a)')'      files. The ''file'' parameter takes different forms for'
write(io,'(a)')'      different operating systems. On most systems, ''file'' may'
write(io,'(a)')'      be a string of up to 1024 characters in quotes. For example,'
write(io,'(a)')'      SAVE(''A'') or EXEC(''MAT88/demo.exec'') . The string will be'
write(io,'(a)')'      used as the name of a file in the local operating system.'
write(io,'(a)')'      On all systems, ''file'' may be a positive integer k less'
write(io,'(a)')'      than 10 which will be used as a FORTRAN logical unit'
write(io,'(a)')'      number. Some systems then automatically access a file with'
write(io,'(a)')'      a name like FORT.k or FORk.DAT. Other systems require a'
write(io,'(a)')'      file with a name like FT0kF001 to be assigned to unit k'
write(io,'(a)')'      before MAT88 is executed. Check your local installation'
write(io,'(a)')'      for details.'
write(io,'(a)')''
write(io,'(a)')'      The filename must be composed of recognized characters. See'
write(io,'(a)')'      CHAR.'
write(io,'(a)')''
write(io,'(a)')'      Also see QUIT and EXIT.'
write(io,'(a)')''
write(io,'(a)')'exec  EXEC(''file'',k) obtains subsequent MAT88 input from an'
write(io,'(a)')'      external file. The printing of input is controlled by the'
write(io,'(a)')'      optional parameter k .'
write(io,'(a)')'      If k = 1 , the input is echoed.'
write(io,'(a)')'      If k = 2 , the MAT88 prompt <> is printed.'
write(io,'(a)')'      If k = 4 , MAT88 pauses before each prompt and waits for a'
write(io,'(a)')'      null line to continue.'
write(io,'(a)')'      If k = 0 , there is no echo, prompt or pause. This is the'
write(io,'(a)')'      default if the exec command is followed by a semicolon.'
write(io,'(a)')'      If k = 7 , there will be echos, prompts and pauses. This is'
write(io,'(a)')'      useful for demonstrations on video terminals.'
write(io,'(a)')'      If k = 3 , there will be echos and prompts, but no pauses.'
write(io,'(a)')'      This is the the default if the exec command is not followed'
write(io,'(a)')'      by a semicolon.'
write(io,'(a)')'      EXEC(0) causes subsequent input to be obtained from the'
write(io,'(a)')'      terminal. An end-of-file has the same effect.'
write(io,'(a)')'      EXEC''s may be nested, i.e. the text in the file may contain'
write(io,'(a)')'      EXEC of another file. EXEC''s may also be driven by FOR and'
write(io,'(a)')'      WHILE loops.'
write(io,'(a)')''
write(io,'(a)')'load  LOAD(''file'') retrieves all the variables from the file .'
write(io,'(a)')'      See FILE and SAVE for more details. To prepare your own'
write(io,'(a)')'      file for LOADing, change the READs to WRITEs in the code'
write(io,'(a)')'      given under SAVE.'
write(io,'(a)')''
write(io,'(a)')'print  PRINT(''file'',X) prints X on the file using the current'
write(io,'(a)')'       format determined by SHORT, LONG Z, etc. See FILE.'
write(io,'(a)')''
write(io,'(a)')'doc   DOC(''file'') stores the user guide and all the help text'
write(io,'(a)')'      (as an Appendix) in the specified file if the file does not'
write(io,'(a)')'      already exist.'
write(io,'(a)')''
write(io,'(a)')'save  SAVE(''file'') stores all the current variables in a file.'
write(io,'(a)')'      SAVE(''file'',X) saves only X . See FILE .'
write(io,'(a)')'      The variables may be retrieved later by LOAD(''file'') or by'
write(io,'(a)')'      your own program using the following code for each matrix.'
write(io,'(a)')'      The lines involving XIMAG may be eliminated if everything'
write(io,'(a)')'      is known to be real.'
write(io,'(a)')''
write(io,'(a)')'            ! attach lunit to ''file'', then ...'
write(io,'(a)')'            REAL or DOUBLEPRECISION XREAL(MMAX,NMAX)'
write(io,'(a)')'            REAL or DOUBLEPRECISION XIMAG(MMAX,NMAX)'
write(io,'(a)')'            READ(lunit,101) ID,M,N,IMG'
write(io,'(a)')'            DO J = 1, N'
write(io,'(a)')'               READ(lunit,102) (XREAL(I,J), I=1,M)'
write(io,'(a)')'               IF (IMG .NE. 0) READ(lunit,102) (XIMAG(I,J),I=1,M)'
write(io,'(a)')'            enddo'
write(io,'(a)')''
write(io,'(a)')'      The formats used are system dependent. The following are'
write(io,'(a)')'      typical. See SUBROUTINE mat_savlod in your local'
write(io,'(a)')'      implementation of MAT88.'
write(io,'(a)')''
write(io,'(a)')'        101 FORMAT(4A1,3I4)'
write(io,'(a)')'        102 FORMAT(4Z18)'
write(io,'(a)')'        102 FORMAT(4O20)'
write(io,'(a)')'        102 FORMAT(4D25.18)'
write(io,'(a)')''
write(io,'(a)')'================================================================================'
write(io,'(a)')'OUTPUT OPTIONS'
write(io,'(a)')'      ( Also see FILE (EXEC, LOAD, PRINT, SAVE ))'
write(io,'(a)')''
write(io,'(a)')'lines  An internal count is kept of the number of lines of output'
write(io,'(a)')'       since the last input. Whenever this count approaches a'
write(io,'(a)')'       limit, the user is asked whether or not to suppress'
write(io,'(a)')'       printing until the next input. Initially the limit is 25.'
write(io,'(a)')'       LINES(N) resets the limit to N .'
write(io,'(a)')''
write(io,'(a)')'long  Determine output format. All computations are done in'
write(io,'(a)')'      complex arithmetic and double precision if it is available.'
write(io,'(a)')'      SHORT and LONG merely switch between different output'
write(io,'(a)')'      formats.'
write(io,'(a)')''
write(io,'(a)')'       SHORT    Scaled fixed point format with about 5 digits.'
write(io,'(a)')'       LONG     Scaled fixed point format with about 15 digits.'
write(io,'(a)')'       SHORT E  Floating point format with about 5 digits.'
write(io,'(a)')'       LONG E   Floating point format with about 15 digits.'
write(io,'(a)')'       LONG Z   System dependent format, often hexadecimal.'
write(io,'(a)')''
write(io,'(a)')'short  See LONG .'
write(io,'(a)')''
write(io,'(a)')'diary  DIARY(''file'') causes a copy of all subsequent terminal input and'
write(io,'(a)')'       most of the resulting output to be written on the file. DIARY(0)'
write(io,'(a)')'       turns it off. See FILE.'
write(io,'(a)')''
write(io,'(a)')'disp  DISPLAY(X) prints X in a compact format.'
write(io,'(a)')''
write(io,'(a)')'      If a base is specified the values are printed as numeric'
write(io,'(a)')'      values in the specified base.'
write(io,'(a)')''
write(io,'(a)')'           disp(0:10,10 ) // display values in base 10'
write(io,'(a)')'           disp(0:10,16 ) // display values as hexadecimal values'
write(io,'(a)')'           disp(0:10,0 ) // display values as binary numbers'
write(io,'(a)')''
write(io,'(a)')'      If no base is specified and all the'
write(io,'(a)')'      elements of X are integers between 0 and 77, then X is'
write(io,'(a)')'      interpreted as MAT88 text and printed accordingly.'
write(io,'(a)')''
write(io,'(a)')'         <>disp(''the analysis is complete'')'
write(io,'(a)')'           the analysis is complete'
write(io,'(a)')'         disp(0:77) // print the default MAT88 character set'
write(io,'(a)')''
write(io,'(a)')'      Otherwise, + , - and blank are printed for positive,'
write(io,'(a)')'      negative and zero elements.'
write(io,'(a)')''
write(io,'(a)')'      Imaginary parts are ignored.'
write(io,'(a)')''
write(io,'(a)')'      Note that DISP(X,B) is the same as DISP(BASE(X,B)) except'
write(io,'(a)')'      it forces DISP to display numeric values.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'plot  PLOT(X,Y) produces a plot of the elements of Y against'
write(io,'(a)')'      those of X. PLOT(Y) is the same as PLOT(1:n,Y) where n is'
write(io,'(a)')'      the number of elements in Y. PLOT(X,Y,P) or'
write(io,'(a)')'      PLOT(X,Y,p1,...,pk) passes the optional parameter vector P'
write(io,'(a)')'      or scalars p1 through pk to the plot routine. The default'
write(io,'(a)')'      plot routine is a crude printer-plot. This version writes'
write(io,'(a)')'      the data as a simple X Y table into a scratch file called'
write(io,'(a)')'      "scratch.dat" and then calls the command'
write(io,'(a)')''
write(io,'(a)')'        xy scratch.dat [options]'
write(io,'(a)')''
write(io,'(a)')'      Hopefully, you have the command xy(1) in your search path.'
write(io,'(a)')'      If not, you can make one by creating a script that calls'
write(io,'(a)')'      a plotting utility.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'         t = 0:50;'
write(io,'(a)')'         plot( t.*cos(t), t.*sin(t) )'
write(io,'(a)')'         opts='' -m 1 -title test plot -d pdf'''
write(io,'(a)')'         plot( t.*cos(t), t.*sin(t),opts)'
write(io,'(a)')''
write(io,'(a)')'================================================================================'
write(io,'(a)')'PERFORMANCE INFORMATION'
write(io,'(a)')''
write(io,'(a)')'flops  Count of floating point operations.'
write(io,'(a)')'       FLOPS is a permanently defined row vector with two'
write(io,'(a)')'       elements. FLOPS(1) is the number of floating point'
write(io,'(a)')'       operations counted during the previous statement. FLOPS(2)'
write(io,'(a)')'       is a cumulative total. FLOPS can be used in the same way'
write(io,'(a)')'       as any other vector. FLOPS(2) = 0 resets the cumulative'
write(io,'(a)')'       total. In addition, FLOPS(1) will be printed whenever a'
write(io,'(a)')'       statement is terminated by an extra comma. For example,'
write(io,'(a)')''
write(io,'(a)')'         X = INV(A);,'
write(io,'(a)')''
write(io,'(a)')'       or'
write(io,'(a)')''
write(io,'(a)')'         COND(A), (as the last statement on the line).'
write(io,'(a)')''
write(io,'(a)')'       HELP FLPS gives more details.'
write(io,'(a)')''
write(io,'(a)')'flps   More detail on FLOPS.'
write(io,'(a)')'       It is not feasible to count absolutely all floating point'
write(io,'(a)')'       operations, but most of the important ones are counted.'
write(io,'(a)')'       Each multiply and add in a real vector operation such as a'
write(io,'(a)')'       dot product or a ''saxpy'' counts one flop. Each multiply'
write(io,'(a)')'       and add in a complex vector operation counts two flops.'
write(io,'(a)')'       Other additions, subtractions and multiplications count one'
write(io,'(a)')'       flop each if the result is real and two flops if it is not.'
write(io,'(a)')'       Real divisions count one and complex divisions count two.'
write(io,'(a)')'       Elementary functions count one if real and two if complex.'
write(io,'(a)')'       Some examples. If A and B are real N by N matrices, then'
write(io,'(a)')''
write(io,'(a)')'        A + B  counts N**2 flops,'
write(io,'(a)')'        A*B    counts N**3 flops,'
write(io,'(a)')'        A**100 counts 99*N**3 flops,'
write(io,'(a)')'        LU(A)  counts roughly (1/3)*N**3 flops.'
write(io,'(a)')'================================================================================'
write(io,'(a)')'MISCELLANEOUS'
write(io,'(a)')''
write(io,'(a)')'char  CHAR(K) requests an input line containing a single'
write(io,'(a)')'      character to replace MAT88 character number K in the'
write(io,'(a)')'      following table. For example, CHAR(45) replaces backslash.'
write(io,'(a)')'      CHAR(-K) replaces the alternate character number K. By default'
write(io,'(a)')'      the character set is:'
write(io,'(a)')''
write(io,'(a)')'                K  character alternate name'
write(io,'(a)')'              0 - 9   0 - 9    0 - 9   digits'
write(io,'(a)')'             10 - 35  A - Z    A - Z   letters'
write(io,'(a)')'               36                      blank'
write(io,'(a)')'               37       (        {     lparen'
write(io,'(a)')'               38       )        }     rparen'
write(io,'(a)')'               39       ;        ;     semi'
write(io,'(a)')'               40       :        |     colon'
write(io,'(a)')'               41       +        +     plus'
write(io,'(a)')'               42       -        -     minus'
write(io,'(a)')'               43       *        *     star'
write(io,'(a)')'               44       /        /     slash'
write(io,'(a)')'               45       \        $     backslash'
write(io,'(a)')'               46       =        =     equal'
write(io,'(a)')'               47       .        @     dot'
write(io,'(a)')'               48       ,        ,     comma'
write(io,'(a)')'               49       ''        "     quote'
write(io,'(a)')'               50       <        [     less'
write(io,'(a)')'               51       >        ]     great'
write(io,'(a)')'             52 - 77  a - z    a - z   letters'
write(io,'(a)')''
write(io,'(a)')'      unused: `~!#%^&_?'
write(io,'(a)')''
write(io,'(a)')'EDIT'
write(io,'(a)')'      A command line consisting of a single period (".") will cause a'
write(io,'(a)')'      small line-based editor to be called (very similar to the CDC NOS'
write(io,'(a)')'      editor "xedit") with a copy of the previous input lines. When the'
write(io,'(a)')'      editor returns control to MAT88, it will execute the edited command'
write(io,'(a)')'      (by default).'
write(io,'(a)')''
write(io,'(a)')'      In editor mode the command to be edited is shifted over one and the'
write(io,'(a)')'      first character of input determines the edit mode. The letter "c"'
write(io,'(a)')'      begins a string change (ie. "c/oldstring/newstring/").  The "l"'
write(io,'(a)')'      command lists the lines. A number goes to that command line as'
write(io,'(a)')'      listed by the "l" command. If the change command begins with a'
write(io,'(a)')'      space a letter replaces the one above it with the exception of'
write(io,'(a)')'      the special characters # (delete) & (blank out) and ^ (insert the'
write(io,'(a)')'      following string here).'
write(io,'(a)')''
write(io,'(a)')'      An editing loop is entered until a carriage return on an empty'
write(io,'(a)')'      line is entered to accept the new line or a period is entered to'
write(io,'(a)')'      cancel the editing.'
write(io,'(a)')''
write(io,'(a)')'      For example, if you had entered a line such as:'
write(io,'(a)')''
write(io,'(a)')'         <M,N>=size(A);for I = 1:M, for J = 1:N, A(I,J) = A(I,J)+3.6;'
write(io,'(a)')''
write(io,'(a)')'      Then to repeat the command changing "3.6" to "5.1" enter'
write(io,'(a)')''
write(io,'(a)')'         .'
write(io,'(a)')'      the previous command is then displayed. Now enter'
write(io,'(a)')''
write(io,'(a)')'         c/3.6/5.1'
write(io,'(a)')''
write(io,'(a)')'      and then enter a carriage return and the edited line will be'
write(io,'(a)')'      executed.'
write(io,'(a)')''
write(io,'(a)')'      The first command can appear on the same line if the line starts'
write(io,'(a)')'      with ". " (a period followed by a space). For example'
write(io,'(a)')''
write(io,'(a)')'         . /rand'
write(io,'(a)')''
write(io,'(a)')'      would take you into edit mode on the last command containing the'
write(io,'(a)')'      string "rand"'
write(io,'(a)')''
write(io,'(a)')'      Enter "?" in edit mode to display further help on editor mode.'
write(io,'(a)')''
write(io,'(a)')'eps   Floating point relative accuracy. A permanent variable'
write(io,'(a)')'      whose value is initially the distance from 1.0 to the next largest'
write(io,'(a)')'      floating point number. The value is changed by CHOP, and other'
write(io,'(a)')'      values may be assigned. EPS is used as a default tolerance by PINV'
write(io,'(a)')'      and RANK.'
write(io,'(a)')''
write(io,'(a)')'lala  A placeholder for a new command.'
write(io,'(a)')''
write(io,'(a)')'debug  DEBU(1) turns on verbose low-level debugging for the developer,'
write(io,'(a)')'       DEBU(0) turns it back off.'
write(io,'(a)')''
write(io,'(a)')'================================================================================'
end subroutine mat_make_help
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_make_manual(io)
integer,intent(in) :: io
write(io,'(a)')'6/24/81'
write(io,'(a)')''
write(io,'(a)')'                       MAT88 Users'' Guide'
write(io,'(a)')'                            May, 1981'
write(io,'(a)')'                            Dec, 2018'
write(io,'(a)')''
write(io,'(a)')'                 Based on the MATLAB package by'
write(io,'(a)')''
write(io,'(a)')'                           Cleve Moler'
write(io,'(a)')'                 Department of Computer Science'
write(io,'(a)')'                    University of New Mexico'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'          ABSTRACT. MAT88 is an interactive computer procedure'
write(io,'(a)')'          that serves as a convenient "laboratory" for'
write(io,'(a)')'          computations involving matrices. It provides easy'
write(io,'(a)')'          access to matrix software developed by the LINPACK and'
write(io,'(a)')'          EISPACK projects. The program is written in Fortran'
write(io,'(a)')'          and is designed to be readily installed under any'
write(io,'(a)')'          operating system which permits interactive execution of'
write(io,'(a)')'          Fortran programs.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'                            CONTENTS'
write(io,'(a)')''
write(io,'(a)')'          1.  Elementary operations'
write(io,'(a)')'          2.  MAT88 functions'
write(io,'(a)')'          3.  Rows, columns and submatrices'
write(io,'(a)')'          4.  FOR, WHILE and IF'
write(io,'(a)')'          5.  Commands, text, files and macros'
write(io,'(a)')'          6.  Census example'
write(io,'(a)')'          7.  Partial differential equation'
write(io,'(a)')'          8.  Eigenvalue sensitivity example'
write(io,'(a)')'          9.  Syntax diagrams'
write(io,'(a)')'         10.  The parser-interpreter'
write(io,'(a)')'         11.  The numerical algorithms'
write(io,'(a)')'         12.  FLOP and CHOP'
write(io,'(a)')'         13.  Communicating with other programs'
write(io,'(a)')'         Appendix.  The HELP file'
write(io,'(a)')''
write(io,'(a)')'6/24/81'
write(io,'(a)')''
write(io,'(a)')'   MAT88 is an interactive computer program that serves as a convenient'
write(io,'(a)')'   "laboratory" for computations involving matrices. It provides easy'
write(io,'(a)')'   access to matrix software developed by the LINPACK and EISPACK'
write(io,'(a)')'   projects [1-3]. The capabilities range from standard tasks such'
write(io,'(a)')'   as solving simultaneous linear equations and inverting matrices,'
write(io,'(a)')'   through symmetric and nonsymmetric eigenvalue problems, to fairly'
write(io,'(a)')'   sophisticated matrix tools such as the singular value decomposition.'
write(io,'(a)')''
write(io,'(a)')'   It is expected that one of MAT88''s primary uses will be in the'
write(io,'(a)')'   classroom. It should be useful in introductory courses in applied'
write(io,'(a)')'   linear algebra, as well as more advanced courses in numerical'
write(io,'(a)')'   analysis, matrix theory, statistics and applications of matrices'
write(io,'(a)')'   to other disciplines. In nonacademic settings, MAT88 can serve as a'
write(io,'(a)')'   "desk calculator" for the quick solution of small problems involving'
write(io,'(a)')'   matrices.'
write(io,'(a)')''
write(io,'(a)')'   The program is written in Fortran and is designed to be readily'
write(io,'(a)')'   installed under any operating system which permits interactive'
write(io,'(a)')'   execution of Fortran programs. The resources required are fairly'
write(io,'(a)')'   modest. There are less than 7000 lines of Fortran source code,'
write(io,'(a)')'   including the LINPACK and EISPACK subroutines used. With proper use'
write(io,'(a)')'   of overlays, it is possible run the system on a minicomputer with'
write(io,'(a)')'   only 32K bytes of memory.'
write(io,'(a)')''
write(io,'(a)')'   The size of the matrices that can be handled in MAT88 depends upon the'
write(io,'(a)')'   amount of storage that is set aside when the system is compiled on a'
write(io,'(a)')'   particular machine. We have found that an allocation of 5000 words'
write(io,'(a)')'   for matrix elements is usually quite satisfactory. This provides'
write(io,'(a)')'   room for several 20 by 20 matrices, for example. One implementation'
write(io,'(a)')'   on a virtual memory system provides 100,000 elements. Since most of'
write(io,'(a)')'   the algorithms used access memory in a sequential fashion, the large'
write(io,'(a)')'   amount of allocated storage causes no difficulties.'
write(io,'(a)')''
write(io,'(a)')'   In some ways, MAT88 resembles SPEAKEASY [4] and, to a lesser extent,'
write(io,'(a)')'   APL. All are interactive terminal languages that ordinarily accept'
write(io,'(a)')'   single-line commands or statements, process them immediately, and print'
write(io,'(a)')'   the results. All have arrays or matrices as principal data types. But'
write(io,'(a)')'   for MAT88, the matrix is the only data type (although scalars, vectors'
write(io,'(a)')'   and text are special cases), the underlying system is portable and'
write(io,'(a)')'   requires fewer resources, and the supporting subroutines are more'
write(io,'(a)')'   powerful and, in some cases, have better numerical properties.'
write(io,'(a)')''
write(io,'(a)')'   Together, LINPACK and EISPACK represent the state of the art in'
write(io,'(a)')'   software for matrix computation. EISPACK is a package of over 70'
write(io,'(a)')'   Fortran subroutines for various matrix eigenvalue computations that are'
write(io,'(a)')'   based for the most part on Algol procedures published by Wilkinson,'
write(io,'(a)')'   Reinsch and their colleagues [5]. LINPACK is a package of 40 Fortran'
write(io,'(a)')'   subroutines (in each of four data types) for solving and analyzing'
write(io,'(a)')'   simultaneous linear equations and related matrix problems. Since MAT88'
write(io,'(a)')'   is not primarily concerned with either execution time efficiency or'
write(io,'(a)')'   storage savings, it ignores most of the special matrix properties that'
write(io,'(a)')'   LINPACK and EISPACK subroutines use to advantage. Consequently, only'
write(io,'(a)')'   8 subroutines from LINPACK and 5 from EISPACK are actually involved.'
write(io,'(a)')''
write(io,'(a)')'   In more advanced applications, MAT88 can be used in conjunction with'
write(io,'(a)')'   other programs in several ways. It is possible to define new MAT88'
write(io,'(a)')'   functions and add them to the system. With most operating systems,'
write(io,'(a)')'   it is possible to use the local file system to pass matrices between'
write(io,'(a)')'   MAT88 and other programs. MAT88 command and statement input can be'
write(io,'(a)')'   obtained from a local file instead of from the terminal. The most'
write(io,'(a)')'   power and flexibility is obtained by using MAT88 as a subroutine'
write(io,'(a)')'   which is called by other programs.'
write(io,'(a)')''
write(io,'(a)')'   This document first gives an overview of MAT88 from the user''s'
write(io,'(a)')'   point of view. Several extended examples involving data fitting,'
write(io,'(a)')'   partial differential equations, eigenvalue sensitivity and other'
write(io,'(a)')'   topics are included. A formal definition of the MAT88 language and'
write(io,'(a)')'   an brief description of the parser and interpreter are given. The'
write(io,'(a)')'   system was designed and programmed using techniques described by'
write(io,'(a)')'   Wirth [6], implemented in nonrecursive, portable Fortran. There is'
write(io,'(a)')'   a brief discussion of some of the matrix algorithms and of their'
write(io,'(a)')'   numerical properties. The final section describes how MAT88 can be'
write(io,'(a)')'   used with other programs. The appendix includes the HELP documentation'
write(io,'(a)')'   available on-line.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'1.  ELEMENTARY OPERATIONS'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   MAT88 works with essentially only one kind of object, a'
write(io,'(a)')'   rectangular matrix with complex elements. If the imaginary parts'
write(io,'(a)')'   of the elements are all zero, they are not printed, but they'
write(io,'(a)')'   still occupy storage. In some situations, special meaning is'
write(io,'(a)')'   attached to 1 by 1 matrices, that is scalars, and to 1 by n and m'
write(io,'(a)')'   by 1 matrices, that is row and column vectors.'
write(io,'(a)')''
write(io,'(a)')'   Matrices can be introduced into MAT88 in four different'
write(io,'(a)')'   ways:'
write(io,'(a)')''
write(io,'(a)')'           --  Explicit list of elements,'
write(io,'(a)')'           --  Use of FOR and WHILE statements,'
write(io,'(a)')'           --  Read from an external file,'
write(io,'(a)')'           --  Execute an external Fortran program.'
write(io,'(a)')''
write(io,'(a)')'   The explicit list is surrounded by angle brackets, ''<'' and ''>'', and'
write(io,'(a)')'   uses the semicolon '';'' to indicate the ends of the rows. For example,'
write(io,'(a)')'   the input line'
write(io,'(a)')''
write(io,'(a)')'      A = <1 2 3; 4 5 6; 7 8 9>'
write(io,'(a)')''
write(io,'(a)')'   will result in the output'
write(io,'(a)')''
write(io,'(a)')'      A     ='
write(io,'(a)')''
write(io,'(a)')'          1.    2.   3.'
write(io,'(a)')'          4.    5.   6.'
write(io,'(a)')'          7.    8.   9.'
write(io,'(a)')''
write(io,'(a)')'   The matrix A will be saved for later use. The individual'
write(io,'(a)')'   elements are separated by commas or blanks and can be any MAT88'
write(io,'(a)')'   expressions, for example'
write(io,'(a)')''
write(io,'(a)')'      x = < -1.3, 4/5, 4*atan(1) >'
write(io,'(a)')''
write(io,'(a)')'   results in'
write(io,'(a)')''
write(io,'(a)')'      X     ='
write(io,'(a)')''
write(io,'(a)')'        -1.3000   0.8000   3.1416'
write(io,'(a)')''
write(io,'(a)')'   The elementary functions available include sqrt, log, exp, sin,'
write(io,'(a)')'   cos, atan, abs, round, real, imag, and conjg.'
write(io,'(a)')''
write(io,'(a)')'   Large matrices can be spread across several input lines,'
write(io,'(a)')'   with the carriage returns replacing the semicolons. The above'
write(io,'(a)')'   matrix could also have been produced by'
write(io,'(a)')''
write(io,'(a)')'      A = < 1 2 3'
write(io,'(a)')'            4 5 6'
write(io,'(a)')'            7 8 9 >'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   Matrices can be input from the local file system. Say a'
write(io,'(a)')'   file named ''xyz'' contains five lines of text,'
write(io,'(a)')''
write(io,'(a)')'      A = <'
write(io,'(a)')'      1 2 3'
write(io,'(a)')'      4 5 6'
write(io,'(a)')'      7 8 9'
write(io,'(a)')'      >;'
write(io,'(a)')''
write(io,'(a)')'   then the MAT88 statement EXEC(''xyz'') reads the matrix and'
write(io,'(a)')'   assigns it to A .'
write(io,'(a)')''
write(io,'(a)')'   The FOR statement allows the generation of matrices whose'
write(io,'(a)')'   elements are given by simple formulas. Our example matrix A'
write(io,'(a)')'   could also have been produced by'
write(io,'(a)')''
write(io,'(a)')'      for i = 1:3, for j = 1:3, a(i,j) = 3*(i-1)+j;'
write(io,'(a)')''
write(io,'(a)')'   The semicolon at the end of the line suppresses the printing,'
write(io,'(a)')'   which in this case would have been nine versions of A with'
write(io,'(a)')'   changing elements.'
write(io,'(a)')''
write(io,'(a)')'   Several statements may be given on a line, separated by'
write(io,'(a)')'   semicolons or commas.'
write(io,'(a)')''
write(io,'(a)')'   Two consecutive periods anywhere on a line indicate'
write(io,'(a)')'   continuation. The periods and any following characters are'
write(io,'(a)')'   deleted, then another line is input and concatenated onto the'
write(io,'(a)')'   previous line.'
write(io,'(a)')''
write(io,'(a)')'   Two consecutive slashes anywhere on a line cause the'
write(io,'(a)')'   remainder of the line to be ignored. This is useful for'
write(io,'(a)')'   inserting comments.'
write(io,'(a)')''
write(io,'(a)')'   Names of variables are formed by a letter, followed by any'
write(io,'(a)')'   number of letters and digits, but only the first 4 characters are'
write(io,'(a)')'   remembered.'
write(io,'(a)')''
write(io,'(a)')'   The special character prime ('') is used to denote the'
write(io,'(a)')'   transpose of a matrix, so'
write(io,'(a)')''
write(io,'(a)')'      x = x'''
write(io,'(a)')''
write(io,'(a)')'   changes the row vector above into the column vector'
write(io,'(a)')''
write(io,'(a)')'      X     ='
write(io,'(a)')''
write(io,'(a)')'        -1.3000'
write(io,'(a)')'         0.8000'
write(io,'(a)')'         3.1416'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   Individual matrix elements may be referenced by enclosing'
write(io,'(a)')'   their subscripts in parentheses. When any element is changed,'
write(io,'(a)')'   the entire matrix is reprinted. For example, using the above'
write(io,'(a)')'   matrix,'
write(io,'(a)')''
write(io,'(a)')'      a(3,3) = a(1,3) + a(3,1)'
write(io,'(a)')''
write(io,'(a)')'   results in'
write(io,'(a)')''
write(io,'(a)')'      A     ='
write(io,'(a)')''
write(io,'(a)')'          1.    2.    3.'
write(io,'(a)')'          4.    5.    6.'
write(io,'(a)')'          7.    8.   10.'
write(io,'(a)')''
write(io,'(a)')'   Addition, subtraction and multiplication of matrices are'
write(io,'(a)')'   denoted by +, -, and * . The operations are performed whenever'
write(io,'(a)')'   the matrices have the proper dimensions. For example, with the'
write(io,'(a)')'   above A and x, the expressions A + x and x*A are incorrect'
write(io,'(a)')'   because A is 3 by 3 and x is now 3 by 1. However,'
write(io,'(a)')''
write(io,'(a)')'      b = A*x'
write(io,'(a)')''
write(io,'(a)')'   is correct and results in the output'
write(io,'(a)')''
write(io,'(a)')'      B     ='
write(io,'(a)')''
write(io,'(a)')'         9.7248'
write(io,'(a)')'        17.6496'
write(io,'(a)')'        28.7159'
write(io,'(a)')''
write(io,'(a)')'   Note that both upper and lower case letters are allowed for input'
write(io,'(a)')'   (on those systems which have both).'
write(io,'(a)')''
write(io,'(a)')'   There are two "matrix division" symbols in MAT88, \ and / .'
write(io,'(a)')'   (If your terminal does not have a backslash, use $ instead, or'
write(io,'(a)')'   see CHAR.) If A and B are matrices, then A\B and B/A correspond'
write(io,'(a)')'   formally to left and right multiplication of B by the inverse of'
write(io,'(a)')'   A, that is inv(A)*B and B*inv(A), but the result is obtained'
write(io,'(a)')'   directly without the computation of the inverse. In the scalar'
write(io,'(a)')'   case, 3\1 and 1/3 have the same value, namely one-third. In'
write(io,'(a)')'   general, A\B denotes the solution X to the equation A*X = B and'
write(io,'(a)')'   B/A denotes the solution to X*A = B.'
write(io,'(a)')''
write(io,'(a)')'   Left division, A\B, is defined whenever B has as many rows'
write(io,'(a)')'   as A. If A is square, it is factored using Gaussian'
write(io,'(a)')'   elimination. The factors are used to solve the equations'
write(io,'(a)')'   A*X(:,j) = B(:,j) where B(:,j) denotes the j-th column of B. The'
write(io,'(a)')'   result is a matrix X with the same dimensions as B. If A is'
write(io,'(a)')'   nearly singular (according to the LINPACK condition estimator,'
write(io,'(a)')'   RCOND), a warning message is printed. If A is not square, it is'
write(io,'(a)')'   factored using Householder orthogonalization with column'
write(io,'(a)')'   pivoting. The factors are used to solve the under- or'
write(io,'(a)')'   overdetermined equations in a least squares sense. The result is'
write(io,'(a)')'   an m by n matrix X where m is the number of columns of A and n is'
write(io,'(a)')'   the number of columns of B . Each column of X has at most k'
write(io,'(a)')'   nonzero components, where k is the effective rank of A .'
write(io,'(a)')''
write(io,'(a)')'   Right division, B/A, can be defined in terms of left'
write(io,'(a)')'   division by B/A = (A''\B'')''.'
write(io,'(a)')''
write(io,'(a)')'   For example, since our vector b was computed as A*x, the'
write(io,'(a)')'   statement'
write(io,'(a)')''
write(io,'(a)')'      y = A\b'
write(io,'(a)')''
write(io,'(a)')'   results in'
write(io,'(a)')''
write(io,'(a)')'      Y     ='
write(io,'(a)')''
write(io,'(a)')'        -1.3000'
write(io,'(a)')'         0.8000'
write(io,'(a)')'         3.1416'
write(io,'(a)')''
write(io,'(a)')'   Of course, y is not exactly equal to x because of the'
write(io,'(a)')'   roundoff errors involved in both A*x and A\b , but we are not'
write(io,'(a)')'   printing enough digits to see the difference. The result of the'
write(io,'(a)')'   statement'
write(io,'(a)')''
write(io,'(a)')'      e = x - y'
write(io,'(a)')''
write(io,'(a)')'   depends upon the particular computer being used. In one case it'
write(io,'(a)')'   produces'
write(io,'(a)')''
write(io,'(a)')'      E     ='
write(io,'(a)')''
write(io,'(a)')'         1.0e-15 *'
write(io,'(a)')''
write(io,'(a)')'           .3053'
write(io,'(a)')'          -.2498'
write(io,'(a)')'           .0000'
write(io,'(a)')''
write(io,'(a)')'   The quantity 1.0e-15 is a scale factor which multiplies all the'
write(io,'(a)')'   components which follow. Thus our vectors x and y actually'
write(io,'(a)')'   agree to about 15 decimal places on this computer.'
write(io,'(a)')''
write(io,'(a)')'   It is also possible to obtain element-by-element'
write(io,'(a)')'   multiplicative operations. If A and B have the same dimensions,'
write(io,'(a)')'   then A .* B denotes the matrix whose elements are simply the'
write(io,'(a)')'   products of the individual elements of A and B . The expressions'
write(io,'(a)')'   A ./ B and A .\ B give the quotients of the individual elements.'
write(io,'(a)')''
write(io,'(a)')'   There are several possible output formats. The statement'
write(io,'(a)')''
write(io,'(a)')'      long, x'
write(io,'(a)')''
write(io,'(a)')'   results in'
write(io,'(a)')''
write(io,'(a)')'      X     ='
write(io,'(a)')''
write(io,'(a)')'         -1.300000000000000'
write(io,'(a)')'           .800000000000000'
write(io,'(a)')'          3.141592653589793'
write(io,'(a)')''
write(io,'(a)')'   The statement'
write(io,'(a)')''
write(io,'(a)')'      short'
write(io,'(a)')''
write(io,'(a)')'   restores the original format.'
write(io,'(a)')''
write(io,'(a)')'   The expression A**p means A to the p-th power. It is'
write(io,'(a)')'   defined if A is a square matrix and p is a scalar. If p is an'
write(io,'(a)')'   integer greater than one, the power is computed by repeated'
write(io,'(a)')'   multiplication. For other values of p the calculation involves'
write(io,'(a)')'   the eigenvalues and eigenvectors of A.'
write(io,'(a)')''
write(io,'(a)')'   Previously defined matrices and matrix expressions can be'
write(io,'(a)')'   used inside brackets to generate larger matrices, for example'
write(io,'(a)')''
write(io,'(a)')'      C = <A, b; <4 2 0>*x, x''>'
write(io,'(a)')''
write(io,'(a)')'   results in'
write(io,'(a)')''
write(io,'(a)')'      C     ='
write(io,'(a)')''
write(io,'(a)')'         1.0000   2.0000   3.0000   9.7248'
write(io,'(a)')'         4.0000   5.0000   6.0000  17.6496'
write(io,'(a)')'         7.0000   8.0000  10.0000  28.7159'
write(io,'(a)')'        -3.6000  -1.3000   0.8000   3.1416'
write(io,'(a)')''
write(io,'(a)')'   There are four predefined variables, EPS, FLOP, RAND and'
write(io,'(a)')'   EYE. The variable EPS is used as a tolerance is determining such'
write(io,'(a)')'   things as near singularity and rank. Its initial value is the'
write(io,'(a)')'   distance from 1.0 to the next largest floating point number on'
write(io,'(a)')'   the particular computer being used. The user may reset this to'
write(io,'(a)')'   any other value, including zero. EPS is changed by CHOP, which is'
write(io,'(a)')'   described in section 12.'
write(io,'(a)')''
write(io,'(a)')'   The value of RAND is a random variable, with a choice of a'
write(io,'(a)')'   uniform or a normal distribution.'
write(io,'(a)')''
write(io,'(a)')'   The name EYE is used in place of I to denote identity'
write(io,'(a)')'   matrices because I is often used as a subscript or as sqrt(-1).'
write(io,'(a)')'   The dimensions of EYE are determined by context. For example,'
write(io,'(a)')''
write(io,'(a)')'      B = A + 3*EYE'
write(io,'(a)')''
write(io,'(a)')'   adds 3 to the diagonal elements of A and'
write(io,'(a)')''
write(io,'(a)')'      X = EYE/A'
write(io,'(a)')''
write(io,'(a)')'   is one of several ways in MAT88 to invert a matrix.'
write(io,'(a)')''
write(io,'(a)')'   FLOP provides a count of the number of floating point'
write(io,'(a)')'   operations, or "flops", required for each calculation.'
write(io,'(a)')''
write(io,'(a)')'   A statement may consist of an expression alone, in which'
write(io,'(a)')'   case a variable named ANS is created and the result stored in ANS'
write(io,'(a)')'   for possible future use. Thus'
write(io,'(a)')''
write(io,'(a)')'      A\A - EYE'
write(io,'(a)')''
write(io,'(a)')'   is the same as'
write(io,'(a)')''
write(io,'(a)')'      ANS = A\A - EYE'
write(io,'(a)')''
write(io,'(a)')'   (Roundoff error usually causes this result to be a matrix of'
write(io,'(a)')'   "small" numbers, rather than all zeros.)'
write(io,'(a)')''
write(io,'(a)')'   All computations are done using either single or double'
write(io,'(a)')'   precision real arithmetic, whichever is appropriate for the'
write(io,'(a)')'   particular computer. There is no mixed-precision arithmetic.'
write(io,'(a)')'   The Fortran COMPLEX data type is not used because many systems'
write(io,'(a)')'   create unnecessary underflows and overflows with complex'
write(io,'(a)')'   operations and because some systems do not allow double precision'
write(io,'(a)')'   complex arithmetic.'
write(io,'(a)')''
write(io,'(a)')'2.  MAT88 FUNCTIONS'
write(io,'(a)')''
write(io,'(a)')'   Much of MAT88''s computational power comes from the various'
write(io,'(a)')'   matrix functions available. The current list includes:'
write(io,'(a)')''
write(io,'(a)')'      inv(A)          - Inverse.'
write(io,'(a)')'      det(A)          - Determinant.'
write(io,'(a)')'      cond(A)         - Condition number.'
write(io,'(a)')'      rcond(A)        - A measure of nearness to singularity.'
write(io,'(a)')'      eig(A)          - Eigenvalues and eigenvectors.'
write(io,'(a)')'      schur(A)        - Schur triangular form.'
write(io,'(a)')'      hess(A)         - Hessenberg or tridiagonal form.'
write(io,'(a)')'      poly(A)         - Characteristic polynomial.'
write(io,'(a)')'      svd(A)          - Singular value decomposition.'
write(io,'(a)')'      pinv(A,eps)     - Pseudoinverse with optional tolerance.'
write(io,'(a)')'      rank(A,eps)     - Matrix rank with optional tolerance.'
write(io,'(a)')'      lu(A)           - Factors from Gaussian elimination.'
write(io,'(a)')'      chol(A)         - Factor from Cholesky factorization.'
write(io,'(a)')'      qr(A)           - Factors from Householder orthogonalization.'
write(io,'(a)')'      rref(A)         - Reduced row echelon form.'
write(io,'(a)')'      orth(A)         - Orthogonal vectors spanning range of A.'
write(io,'(a)')'      exp(A)          - e to the A.'
write(io,'(a)')'      log(A)          - Natural logarithm.'
write(io,'(a)')'      sqrt(A)         - Square root.'
write(io,'(a)')'      sin(A)          - Trigonometric sine.'
write(io,'(a)')'      cos(A)          - Cosine.'
write(io,'(a)')'      atan(A)         - Arctangent.'
write(io,'(a)')'      round(A)        - Round the elements to nearest integers.'
write(io,'(a)')'      abs(A)          - Absolute value of the elements.'
write(io,'(a)')'      real(A)         - Real parts of the elements.'
write(io,'(a)')'      imag(A)         - Imaginary parts of the elements.'
write(io,'(a)')'      conjg(A)        - Complex conjugate.'
write(io,'(a)')'      sum(A)          - Sum of the elements.'
write(io,'(a)')'      prod(A)         - Product of the elements.'
write(io,'(a)')'      diag(A)         - Extract or create diagonal matrices.'
write(io,'(a)')'      tril(A)         - Lower triangular part of A.'
write(io,'(a)')'      triu(A)         - Upper triangular part of A.'
write(io,'(a)')'      norm(A,p)       - Norm with p = 1, 2 or ''Infinity''.'
write(io,'(a)')'      eye(m,n)        - Portion of identity matrix.'
write(io,'(a)')'      rand(m,n)       - Matrix with random elements.'
write(io,'(a)')'      ones(m,n)       - Matrix of all ones.'
write(io,'(a)')'      magic(n)        - Interesting test matrices.'
write(io,'(a)')'      hilbert(n)      - Inverse Hilbert matrices.'
write(io,'(a)')'      roots(C)        - Roots of polynomial with coefficients C.'
write(io,'(a)')'      display(A,p)    - Print base p representation of A.'
write(io,'(a)')'      kron(A,B)       - Kronecker tensor product of A and B.'
write(io,'(a)')'      plot(X,Y)       - Plot Y as a function of X .'
write(io,'(a)')'      rat(A)          - Find "simple" rational approximation to A.'
write(io,'(a)')'      user(A)         - Function defined by external program.'
write(io,'(a)')''
write(io,'(a)')'   Some of these functions have different interpretations when'
write(io,'(a)')'   the argument is a matrix or a vector and some of them have'
write(io,'(a)')'   additional optional arguments. Details are given in the HELP'
write(io,'(a)')'   document in the appendix.'
write(io,'(a)')''
write(io,'(a)')'   Several of these functions can be used in a generalized'
write(io,'(a)')'   assignment statement with two or three variables on the left hand'
write(io,'(a)')'   side. For example'
write(io,'(a)')''
write(io,'(a)')'      <X,D> = EIG(A)'
write(io,'(a)')''
write(io,'(a)')'   stores the eigenvectors of A in the matrix X and a diagonal'
write(io,'(a)')'   matrix containing the eigenvalues in the matrix D. The statement'
write(io,'(a)')''
write(io,'(a)')'      EIG(A)'
write(io,'(a)')''
write(io,'(a)')'   simply computes the eigenvalues and stores them in ANS.'
write(io,'(a)')''
write(io,'(a)')'   Future versions of MAT88 will probably include additional'
write(io,'(a)')'   functions, since they can easily be added to the system.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'3.  ROWS, COLUMNS AND SUBMATRICES'
write(io,'(a)')''
write(io,'(a)')'   Individual elements of a matrix can be accessed by giving'
write(io,'(a)')'   their subscripts in parentheses, eg. A(1,2), x(i), TAB(ind(k)+1).'
write(io,'(a)')'   An expression used as a subscript is rounded to the nearest'
write(io,'(a)')'   integer.'
write(io,'(a)')''
write(io,'(a)')'   Individual rows and columns can be accessed using a colon'
write(io,'(a)')'   '':'' (or a ''|'') for the free subscript. For example, A(1,:) is the'
write(io,'(a)')'   first row of A and A(:,j) is the j-th column. Thus'
write(io,'(a)')''
write(io,'(a)')'      A(i,:) = A(i,:) + c*A(k,:)'
write(io,'(a)')''
write(io,'(a)')'   adds c times the k-th row of A to the i-th row.'
write(io,'(a)')''
write(io,'(a)')'   The colon is used in several other ways in MAT88, but all'
write(io,'(a)')'   of the uses are based on the following definition.'
write(io,'(a)')''
write(io,'(a)')'      j:k    is the same as  <j, j+1, ..., k>'
write(io,'(a)')'      j:k    is empty if  j > k .'
write(io,'(a)')'      j:i:k  is the same as  <j, j+i, j+2i, ..., k>'
write(io,'(a)')'      j:i:k  is empty if  i > 0 and j > k or if i < 0 and j < k .'
write(io,'(a)')''
write(io,'(a)')'   The colon is usually used with integers, but it is possible to'
write(io,'(a)')'   use arbitrary real scalars as well. Thus'
write(io,'(a)')''
write(io,'(a)')'      1:4  is the same as  <1, 2, 3, 4>'
write(io,'(a)')'      0: 0.1: 0.5 is the same as <0.0, 0.1, 0.2, 0.3, 0.4, 0.5>'
write(io,'(a)')''
write(io,'(a)')'   In general, a subscript can be a vector. If X and V are'
write(io,'(a)')'   vectors, then X(V) is <X(V(1)), X(V(2)), ..., X(V(n))> . This can'
write(io,'(a)')'   also be used with matrices. If V has m components and W has n'
write(io,'(a)')'   components, then A(V,W) is the m by n matrix formed from the'
write(io,'(a)')'   elements of A whose subscripts are the elements of V and W.'
write(io,'(a)')'   Combinations of the colon notation and the indirect subscripting'
write(io,'(a)')'   allow manipulation of various submatrices. For example,'
write(io,'(a)')''
write(io,'(a)')'      A(<1,5>,:) = A(<5,1>,:)  interchanges rows 1 and 5 of A.'
write(io,'(a)')'      A(2:k,1:n)  is the submatrix formed from rows 2 through k'
write(io,'(a)')'         and columns 1 through n of A .'
write(io,'(a)')'      A(:,<3 1 2>)  is a permutation of the first three columns.'
write(io,'(a)')''
write(io,'(a)')'   The notation A(:) has a special meaning. On the right hand'
write(io,'(a)')'   side of an assignment statement, it denotes all the elements of'
write(io,'(a)')'   A, regarded as a single column. When an expression is assigned'
write(io,'(a)')'   to A(:), the current dimensions of A, rather than of the'
write(io,'(a)')'   expression, are used.'
write(io,'(a)')''
write(io,'(a)')'4.  FOR, WHILE AND IF'
write(io,'(a)')''
write(io,'(a)')'   The FOR clause allows statements to be repeated a specific'
write(io,'(a)')'   number of times. The general form is'
write(io,'(a)')''
write(io,'(a)')'      FOR variable = expr, statement, ..., statement, END'
write(io,'(a)')''
write(io,'(a)')'   The END and the comma before it may be omitted. In general, the'
write(io,'(a)')'   expression may be a matrix, in which case the columns are stored'
write(io,'(a)')'   one at a time in the variable and the following statements, up to'
write(io,'(a)')'   the END or the end of the line, are executed. The expression is'
write(io,'(a)')'   often of the form j:k, and its "columns" are simply the scalars'
write(io,'(a)')'   from j to k. Some examples (assume n has already been assigned a'
write(io,'(a)')'   value):'
write(io,'(a)')''
write(io,'(a)')'      for i = 1:n, for j = 1:n, A(i,j) = 1/(i+j-1);'
write(io,'(a)')''
write(io,'(a)')'   generates the Hilbert matrix.'
write(io,'(a)')''
write(io,'(a)')'      for j = 2:n-1, for i = j:n-1, ...'
write(io,'(a)')'         A(i,j) = 0; end; A(j,j) = j; end; A'
write(io,'(a)')''
write(io,'(a)')'   changes all but the "outer edge" of the lower triangle and then'
write(io,'(a)')'   prints the final matrix.'
write(io,'(a)')''
write(io,'(a)')'      for h = 1.0: -0.1: -1.0, (<h, cos(pi*h)>)'
write(io,'(a)')''
write(io,'(a)')'   prints a table of cosines.'
write(io,'(a)')''
write(io,'(a)')'      <X,D> = EIG(A); for v = X, v, A*v'
write(io,'(a)')''
write(io,'(a)')'   displays eigenvectors, one at a time.'
write(io,'(a)')''
write(io,'(a)')'        The WHILE clause allows statements to be repeated an'
write(io,'(a)')'   indefinite number of times. The general form is'
write(io,'(a)')''
write(io,'(a)')'      WHILE expr relop expr,   statement,..., statement, END'
write(io,'(a)')''
write(io,'(a)')'   where relop is =, <, >, <=, >=, or <> (not equal). The'
write(io,'(a)')'   statements are repeatedly executed as long as the indicated'
write(io,'(a)')'   comparison between the real parts of the first components of the'
write(io,'(a)')'   two expressions is true. Here are two examples. (Exercise for'
write(io,'(a)')'   the reader: What do these segments do?)'
write(io,'(a)')''
write(io,'(a)')'      eps = 1;'
write(io,'(a)')'      while 1 + eps > 1, eps = eps/2;'
write(io,'(a)')'      eps = 2*eps'
write(io,'(a)')''
write(io,'(a)')'      E = 0*A;  F = E + EYE; n = 1;'
write(io,'(a)')'      while NORM(E+F-E,1) > 0, E = E + F; F = A*F/n; n = n + 1;'
write(io,'(a)')'      E'
write(io,'(a)')''
write(io,'(a)')'   The IF clause allows conditional execution of statements.'
write(io,'(a)')'   The general form is'
write(io,'(a)')''
write(io,'(a)')'      IF expr relop expr,  statement, ..., statement,'
write(io,'(a)')'         ELSE statement, ..., statement'
write(io,'(a)')''
write(io,'(a)')'   The first group of statements are executed if the relation is'
write(io,'(a)')'   true and the second group are executed if the relation is false.'
write(io,'(a)')'   The ELSE and the statements following it may be omitted. For'
write(io,'(a)')'   example,'
write(io,'(a)')''
write(io,'(a)')'      if abs(i-j) = 2, A(i,j) = 0;'
write(io,'(a)')''
write(io,'(a)')'5.  COMMANDS, TEXT, FILES AND MACROS.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   MAT88 has several commands which control the output format'
write(io,'(a)')'   and the overall execution of the system.'
write(io,'(a)')''
write(io,'(a)')'   The HELP command allows on-line access to short portions of'
write(io,'(a)')'   text describing various operations, functions and special'
write(io,'(a)')'   characters. The entire HELP document is reproduced in an'
write(io,'(a)')'   appendix.'
write(io,'(a)')''
write(io,'(a)')'   Results are usually printed in a scaled fixed point format'
write(io,'(a)')'   that shows 4 or 5 significant figures. The commands SHORT, LONG,'
write(io,'(a)')'   SHORT E, LONG E and LONG Z alter the output format, but do not'
write(io,'(a)')'   alter the precision of the computations or the internal storage.'
write(io,'(a)')''
write(io,'(a)')'   The WHO and WHAT commands provide information about the'
write(io,'(a)')'   functions and variables that are currently defined.'
write(io,'(a)')''
write(io,'(a)')'   The CLEAR command erases all variables, except EPS, FLOP,'
write(io,'(a)')'   RAND and EYE. The statement A = <> indicates that a "0 by 0"'
write(io,'(a)')'   matrix is to be stored in A. This causes A to be erased so that'
write(io,'(a)')'   its storage can be used for other variables.'
write(io,'(a)')''
write(io,'(a)')'   The QUIT and EXIT commands cause return to the underlying'
write(io,'(a)')'   operating system through the Fortran RETURN statement.'
write(io,'(a)')''
write(io,'(a)')'   MAT88 has a limited facility for handling text. Any string'
write(io,'(a)')'   of characters delineated by quotes (with two quotes used to allow'
write(io,'(a)')'   one quote within the string) is saved as a vector of integer'
write(io,'(a)')'   values with ''1'' = 1, ''A'' = 10, '' '' = 36, etc. (The complete list'
write(io,'(a)')'   is in the appendix under CHAR.) For example'
write(io,'(a)')''
write(io,'(a)')'      ''2*A + 3''  is the same as  <2 43 10 36 41 36 3>'
write(io,'(a)')''
write(io,'(a)')'   It is possible, though seldom very meaningful, to use such'
write(io,'(a)')'   strings in matrix operations. More frequently, the text is used'
write(io,'(a)')'   as a special argument to various functions.'
write(io,'(a)')''
write(io,'(a)')'      NORM(A,''inf'')    computes the infinity norm of A .'
write(io,'(a)')'      DISPLAY(T)       prints the text stored in T .'
write(io,'(a)')'      EXEC(''file'')     obtains MAT88 input from an external file.'
write(io,'(a)')'      SAVE(''file'')     stores all the current variables in a file.'
write(io,'(a)')'      LOAD(''file'')     retrieves all the variables from a file.'
write(io,'(a)')'      PRINT(''file'',X)  prints X on a file.'
write(io,'(a)')'      DIARY(''file'')    makes a copy of the complete MAT88 session.'
write(io,'(a)')''
write(io,'(a)')'   The text can also be used in a limited string substitution'
write(io,'(a)')'   macro facility. If a variable, say T, contains the source text'
write(io,'(a)')'   for a MAT88 statement or expression, then the construction'
write(io,'(a)')''
write(io,'(a)')'      > T <'
write(io,'(a)')''
write(io,'(a)')'   causes T to be executed or evaluated. For example'
write(io,'(a)')''
write(io,'(a)')'      T = ''2*A + 3'';'
write(io,'(a)')'      S = ''B = >T< + 5'''
write(io,'(a)')'      A = 4;'
write(io,'(a)')'      > S <'
write(io,'(a)')''
write(io,'(a)')'   produces'
write(io,'(a)')''
write(io,'(a)')'      B     ='
write(io,'(a)')''
write(io,'(a)')'         16.'
write(io,'(a)')''
write(io,'(a)')'   Some other examples are given under MACRO in the appendix. This'
write(io,'(a)')'   facility is useful for fairly short statements and expressions.'
write(io,'(a)')'   More complicated MAT88 "programs" should use the EXEC facility.'
write(io,'(a)')''
write(io,'(a)')'   The operations which access external files cannot be handled'
write(io,'(a)')'   in a completely machine-independent manner by portable Fortran'
write(io,'(a)')'   code. It is necessary for each particular installation to'
write(io,'(a)')'   provide a subroutine which associates external text files with'
write(io,'(a)')'   Fortran logical unit numbers.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'6.  CENSUS EXAMPLE'
write(io,'(a)')''
write(io,'(a)')'   Our first extended example involves predicting the'
write(io,'(a)')'   population of the United States in 1980 using extrapolation of'
write(io,'(a)')'   various fits to the census data from 1900 through 1970. There'
write(io,'(a)')'   are eight observations, so we begin with the MAT88 statement'
write(io,'(a)')''
write(io,'(a)')'      n = 8'
write(io,'(a)')''
write(io,'(a)')'   The values of the dependent variable, the population in millions,'
write(io,'(a)')'   can be entered with'
write(io,'(a)')''
write(io,'(a)')'      y = < 75.995   91.972  105.711  123.203   ...'
write(io,'(a)')'           131.669  150.697  179.323  203.212>'''
write(io,'(a)')''
write(io,'(a)')'   In order to produce a reasonably scaled matrix, the independent'
write(io,'(a)')'   variable, time, is transformed from the interval [1900,1970] to'
write(io,'(a)')'   [-1.00,0.75]. This can be accomplished directly with'
write(io,'(a)')''
write(io,'(a)')'      t = -1.0:0.25:0.75'
write(io,'(a)')''
write(io,'(a)')'   or in a fancier, but perhaps clearer, way with'
write(io,'(a)')''
write(io,'(a)')'      t = 1900:10:1970;   t = (t - 1940*ones(t))/40'
write(io,'(a)')''
write(io,'(a)')'   Either of these is equivalent to'
write(io,'(a)')''
write(io,'(a)')'      t = <-1 -.75 -.50 -.25 0 .25 .50 .75>'
write(io,'(a)')''
write(io,'(a)')'   The interpolating polynomial of degree n-1 involves an'
write(io,'(a)')'   Vandermonde matrix of order n with elements that might be'
write(io,'(a)')'   generated by'
write(io,'(a)')''
write(io,'(a)')'      for i = 1:n, for j = 1:n, a(i,j) = t(i)**(j-1);'
write(io,'(a)')''
write(io,'(a)')'   However, this results in an error caused by 0**0 when i = 5 and'
write(io,'(a)')'   j = 1 . The preferable approach is'
write(io,'(a)')''
write(io,'(a)')'      A = ones(n,n);'
write(io,'(a)')'      for i = 1:n, for j = 2:n, a(i,j) = t(i)*a(i,j-1);'
write(io,'(a)')''
write(io,'(a)')'   Now the statement'
write(io,'(a)')''
write(io,'(a)')'      cond(A)'
write(io,'(a)')''
write(io,'(a)')'   produces the output'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'         1.1819E+03'
write(io,'(a)')''
write(io,'(a)')'   which indicates that transformation of the time variable has'
write(io,'(a)')'   resulted in a reasonably well conditioned matrix.'
write(io,'(a)')''
write(io,'(a)')'        The statement'
write(io,'(a)')''
write(io,'(a)')'      c = A\y'
write(io,'(a)')''
write(io,'(a)')'   results in'
write(io,'(a)')''
write(io,'(a)')'      C     ='
write(io,'(a)')''
write(io,'(a)')'        131.6690'
write(io,'(a)')'         41.0406'
write(io,'(a)')'        103.5396'
write(io,'(a)')'        262.4535'
write(io,'(a)')'       -326.0658'
write(io,'(a)')'       -662.0814'
write(io,'(a)')'        341.9022'
write(io,'(a)')'        533.6373'
write(io,'(a)')''
write(io,'(a)')'   These are the coefficients in the interpolating polynomial'
write(io,'(a)')''
write(io,'(a)')'         n-1'
write(io,'(a)')''
write(io,'(a)')'         c  + c t + ... + c t'
write(io,'(a)')'          1    2           n'
write(io,'(a)')''
write(io,'(a)')'   Our transformation of the time variable has resulted in t = 1'
write(io,'(a)')'   corresponding to the year 1980. Consequently, the extrapolated'
write(io,'(a)')'   population is simply the sum of the coefficients. This can be'
write(io,'(a)')'   computed by'
write(io,'(a)')''
write(io,'(a)')'      p = sum(c)'
write(io,'(a)')''
write(io,'(a)')'   The result is'
write(io,'(a)')''
write(io,'(a)')'      P     ='
write(io,'(a)')''
write(io,'(a)')'        426.0950'
write(io,'(a)')''
write(io,'(a)')'   which indicates a 1980 population of over 426 million. Clearly,'
write(io,'(a)')'   using the seventh degree interpolating polynomial to extrapolate'
write(io,'(a)')'   even a fairly short distance beyond the end of the data interval'
write(io,'(a)')'   is not a good idea.'
write(io,'(a)')''
write(io,'(a)')'   The coefficients in least squares fits by polynomials of'
write(io,'(a)')'   lower degree can be computed using fewer than n columns of the'
write(io,'(a)')'   matrix.'
write(io,'(a)')''
write(io,'(a)')'      for k = 1:n, c = A(:,1:k)\y,  p = sum(c)'
write(io,'(a)')''
write(io,'(a)')'   would produce the coefficients of these fits, as well as the'
write(io,'(a)')'   resulting extrapolated population. If we do not want to print'
write(io,'(a)')'   all the coefficients, we can simply generate a small table of'
write(io,'(a)')'   populations predicted by polynomials of degrees zero through'
write(io,'(a)')'   seven. We also compute the maximum deviation between the fitted'
write(io,'(a)')'   and observed values.'
write(io,'(a)')''
write(io,'(a)')'      for k = 1:n, X = A(:,1:k);  c = X\y;  ...'
write(io,'(a)')'         d(k) = k-1;  p(k) = sum(c);  e(k) = norm(X*c-y,''inf'');'
write(io,'(a)')'      <d, p, e>'
write(io,'(a)')''
write(io,'(a)')'   The resulting output is'
write(io,'(a)')''
write(io,'(a)')'         0   132.7227  70.4892'
write(io,'(a)')'         1   211.5101   9.8079'
write(io,'(a)')'         2   227.7744   5.0354'
write(io,'(a)')'         3   241.9574   3.8941'
write(io,'(a)')'         4   234.2814   4.0643'
write(io,'(a)')'         5   189.7310   2.5066'
write(io,'(a)')'         6   118.3025   1.6741'
write(io,'(a)')'         7   426.0950   0.0000'
write(io,'(a)')''
write(io,'(a)')'   The zeroth degree fit, 132.7 million, is the result of fitting a'
write(io,'(a)')'   constant to the data and is simply the average. The results'
write(io,'(a)')'   obtained with polynomials of degree one through four all appear'
write(io,'(a)')'   reasonable. The maximum deviation of the degree four fit is'
write(io,'(a)')'   slightly greater than the degree three, even though the sum of'
write(io,'(a)')'   the squares of the deviations is less. The coefficients of the'
write(io,'(a)')'   highest powers in the fits of degree five and six turn out to be'
write(io,'(a)')'   negative and the predicted populations of less than 200 million'
write(io,'(a)')'   are probably unrealistic. The hopefully absurd prediction of the'
write(io,'(a)')'   interpolating polynomial concludes the table.'
write(io,'(a)')''
write(io,'(a)')'   We wish to emphasize that roundoff errors are not'
write(io,'(a)')'   significant here. Nearly identical results would be obtained on'
write(io,'(a)')'   other computers, or with other algorithms. The results simply'
write(io,'(a)')'   indicate the difficulties associated with extrapolation of'
write(io,'(a)')'   polynomial fits of even modest degree.'
write(io,'(a)')''
write(io,'(a)')'   A stabilized fit by a seventh degree polynomial can be'
write(io,'(a)')'   obtained using the pseudoinverse, but it requires a fairly'
write(io,'(a)')'   delicate choice of a tolerance. The statement'
write(io,'(a)')''
write(io,'(a)')'      s = svd(A)'
write(io,'(a)')''
write(io,'(a)')'   produces the singular values'
write(io,'(a)')''
write(io,'(a)')'      S     ='
write(io,'(a)')''
write(io,'(a)')'         3.4594'
write(io,'(a)')'         2.2121'
write(io,'(a)')'         1.0915'
write(io,'(a)')'         0.4879'
write(io,'(a)')'         0.1759'
write(io,'(a)')'         0.0617'
write(io,'(a)')'         0.0134'
write(io,'(a)')'         0.0029'
write(io,'(a)')''
write(io,'(a)')'   We see that the last three singular values are less than 0.1 ,'
write(io,'(a)')'   consequently, A can be approximately by a matrix of rank five'
write(io,'(a)')'   with an error less than 0.1 . The Moore-Penrose pseudoinverse of'
write(io,'(a)')'   this rank five matrix is obtained from the singular value'
write(io,'(a)')'   decomposition with the following statements'
write(io,'(a)')''
write(io,'(a)')'      c = pinv(A,0.1)*y, p = sum(c), e = norm(a*c-y,''inf'')'
write(io,'(a)')''
write(io,'(a)')'   The output is'
write(io,'(a)')''
write(io,'(a)')'      C     ='
write(io,'(a)')''
write(io,'(a)')'       134.7972'
write(io,'(a)')'        67.5055'
write(io,'(a)')'        23.5523'
write(io,'(a)')'         9.2834'
write(io,'(a)')'         3.0174'
write(io,'(a)')'         2.6503'
write(io,'(a)')'        -2.8808'
write(io,'(a)')'         3.2467'
write(io,'(a)')''
write(io,'(a)')'      P     ='
write(io,'(a)')''
write(io,'(a)')'       241.1720'
write(io,'(a)')''
write(io,'(a)')'      E     ='
write(io,'(a)')''
write(io,'(a)')'         3.9469'
write(io,'(a)')''
write(io,'(a)')'   The resulting seventh degree polynomial has coefficients which'
write(io,'(a)')'   are much smaller than those of the interpolating polynomial given'
write(io,'(a)')'   earlier. The predicted population and the maximum deviation are'
write(io,'(a)')'   reasonable. Any choice of the tolerance between the fifth and'
write(io,'(a)')'   sixth singular values would produce the same results, but choices'
write(io,'(a)')'   outside this range result in pseudoinverses of different rank and'
write(io,'(a)')'   do not work as well.'
write(io,'(a)')''
write(io,'(a)')'   The one term exponential approximation'
write(io,'(a)')''
write(io,'(a)')'        y(t) = k exp(pt)'
write(io,'(a)')''
write(io,'(a)')'   can be transformed into a linear approximation by taking'
write(io,'(a)')'   logarithms.'
write(io,'(a)')''
write(io,'(a)')'        log(y(t)) = log k + pt'
write(io,'(a)')''
write(io,'(a)')'                  = c  + c t'
write(io,'(a)')'                     1    2'
write(io,'(a)')''
write(io,'(a)')'   The following segment makes use of the fact that a function of a'
write(io,'(a)')'   vector is the function applied to the individual components.'
write(io,'(a)')''
write(io,'(a)')'      X = A(:,1:2);'
write(io,'(a)')'      c = X\log(y)'
write(io,'(a)')'      p = exp(sum(c))'
write(io,'(a)')'      e = norm(exp(X*c)-y,''inf'')'
write(io,'(a)')''
write(io,'(a)')'   The resulting output is'
write(io,'(a)')''
write(io,'(a)')'      C     ='
write(io,'(a)')''
write(io,'(a)')'         4.9083'
write(io,'(a)')'         0.5407'
write(io,'(a)')''
write(io,'(a)')'      P     ='
write(io,'(a)')''
write(io,'(a)')'       232.5134'
write(io,'(a)')''
write(io,'(a)')'      E     ='
write(io,'(a)')''
write(io,'(a)')'         4.9141'
write(io,'(a)')''
write(io,'(a)')'   The predicted population and maximum deviation appear'
write(io,'(a)')'   satisfactory and indicate that the exponential model is a'
write(io,'(a)')'   reasonable one to consider.'
write(io,'(a)')''
write(io,'(a)')'   As a curiosity, we return to the degree six polynomial.'
write(io,'(a)')'   Since the coefficient of the high order term is negative and the'
write(io,'(a)')'   value of the polynomial at t = 1 is positive, it must have a root'
write(io,'(a)')'   at some value of t greater than one. The statements'
write(io,'(a)')''
write(io,'(a)')'      X = A(:,1:7);'
write(io,'(a)')'      c = X\y;'
write(io,'(a)')'      c = c(7:-1:1);  //reverse the order of the coefficients'
write(io,'(a)')'      z = roots(c)'
write(io,'(a)')''
write(io,'(a)')'   produce'
write(io,'(a)')''
write(io,'(a)')'      Z     ='
write(io,'(a)')''
write(io,'(a)')'         1.1023-  0.0000*i'
write(io,'(a)')'         0.3021+  0.7293*i'
write(io,'(a)')'        -0.8790+  0.6536*i'
write(io,'(a)')'        -1.2939-  0.0000*i'
write(io,'(a)')'        -0.8790-  0.6536*i'
write(io,'(a)')'         0.3021-  0.7293*i'
write(io,'(a)')''
write(io,'(a)')'   There is only one real, positive root. The corresponding time on'
write(io,'(a)')'   the original scale is'
write(io,'(a)')''
write(io,'(a)')'      1940 + 40*real(z(1))'
write(io,'(a)')''
write(io,'(a)')'        =  1984.091'
write(io,'(a)')''
write(io,'(a)')'   We conclude that the United States population should become zero'
write(io,'(a)')'   early in February of 1984.'
write(io,'(a)')''
write(io,'(a)')'7. PARTIAL DIFFERENTIAL EQUATION EXAMPLE'
write(io,'(a)')''
write(io,'(a)')'   Our second extended example is a boundary value problem for'
write(io,'(a)')'   Laplace''s equation. The underlying physical problem involves the'
write(io,'(a)')'   conductivity of a medium with cylindrical inclusions and is'
write(io,'(a)')'   considered by Keller and Sachs [7].'
write(io,'(a)')''
write(io,'(a)')'        Find a function  u(x,y)  satisfying Laplace''s equation'
write(io,'(a)')''
write(io,'(a)')'                  u   + u   = 0'
write(io,'(a)')'                   xx    yy'
write(io,'(a)')''
write(io,'(a)')'   The domain is a unit square with a quarter circle of radius rho'
write(io,'(a)')'   removed from one corner. There are Neumann conditions on the top'
write(io,'(a)')'   and bottom edges and Dirichlet conditions on the remainder of the'
write(io,'(a)')'   boundary.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'                            u  = 0'
write(io,'(a)')'                             n'
write(io,'(a)')''
write(io,'(a)')'                        -------------'
write(io,'(a)')'                       |             .'
write(io,'(a)')'                       |             .'
write(io,'(a)')'                       |              .'
write(io,'(a)')'                       |               .  u = 1'
write(io,'(a)')'                       |                 .'
write(io,'(a)')'                       |                    .'
write(io,'(a)')'                       |                       .'
write(io,'(a)')'                u = 0  |                        |'
write(io,'(a)')'                       |                        |'
write(io,'(a)')'                       |                        |'
write(io,'(a)')'                       |                        |  u = 1'
write(io,'(a)')'                       |                        |'
write(io,'(a)')'                       |                        |'
write(io,'(a)')'                       |                        |'
write(io,'(a)')'                        ------------------------'
write(io,'(a)')''
write(io,'(a)')'                                 u  = 0'
write(io,'(a)')'                                  n'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   The effective conductivity of an medium is then given by the'
write(io,'(a)')'   integral along the left edge,'
write(io,'(a)')''
write(io,'(a)')'                               1'
write(io,'(a)')'                    sigma = integral  u (0,y) dy'
write(io,'(a)')'                              0        n'
write(io,'(a)')''
write(io,'(a)')'   It is of interest to study the relation between the radius rho'
write(io,'(a)')'   and the conductivity sigma. In particular, as rho approaches'
write(io,'(a)')'   one, sigma becomes infinite.'
write(io,'(a)')''
write(io,'(a)')'   Keller and Sachs use a finite difference approximation. The'
write(io,'(a)')'   following technique makes use of the fact that the equation is'
write(io,'(a)')'   actually Laplace''s equation and leads to a much smaller matrix'
write(io,'(a)')'   problem to solve.'
write(io,'(a)')''
write(io,'(a)')'        Consider an approximate solution of the form'
write(io,'(a)')''
write(io,'(a)')'                    n      2j-1'
write(io,'(a)')'              u =  sum  c r    cos(2j-1)t'
write(io,'(a)')'                   j=1   j'
write(io,'(a)')''
write(io,'(a)')'   where r,t are polar coordinates (t is theta). The coefficients'
write(io,'(a)')'   are to be determined. For any set of coefficients, this function'
write(io,'(a)')'   already satisfies the differential equation because the basis'
write(io,'(a)')'   functions are harmonic; it satisfies the normal derivative'
write(io,'(a)')'   boundary condition on the bottom edge of the domain because we'
write(io,'(a)')'   used cos t in preference to sin t ; and it satisfies the'
write(io,'(a)')'   boundary condition on the left edge of the domain because we use'
write(io,'(a)')'   only odd multiples of t .'
write(io,'(a)')''
write(io,'(a)')'   The computational task is to find coefficients so that the'
write(io,'(a)')'   boundary conditions on the remaining edges are satisfied as well'
write(io,'(a)')'   as possible. To accomplish this, pick m points (r,t) on the'
write(io,'(a)')'   remaining edges. It is desirable to have m > n and in practice'
write(io,'(a)')'   we usually choose m to be two or three times as large as n .'
write(io,'(a)')'   Typical values of n are 10 or 20 and of m are 20 to 60. An'
write(io,'(a)')'   m by n matrix A is generated. The i,j element is the j-th'
write(io,'(a)')'   basis function, or its normal derivative, evaluated at the i-th'
write(io,'(a)')'   boundary point. A right hand side with m components is also'
write(io,'(a)')'   generated. In this example, the elements of the right hand side'
write(io,'(a)')'   are either zero or one. The coefficients are then found by'
write(io,'(a)')'   solving the overdetermined set of equations'
write(io,'(a)')''
write(io,'(a)')'               Ac = b'
write(io,'(a)')''
write(io,'(a)')'   in a least squares sense.'
write(io,'(a)')''
write(io,'(a)')'   Once the coefficients have been determined, the approximate'
write(io,'(a)')'   solution is defined everywhere on the domain. It is then'
write(io,'(a)')'   possible to compute the effective conductivity sigma . In fact,'
write(io,'(a)')'   a very simple formula results,'
write(io,'(a)')''
write(io,'(a)')'                        n       j-1'
write(io,'(a)')'              sigma =  sum  (-1)   c'
write(io,'(a)')'                       j=1          j'
write(io,'(a)')''
write(io,'(a)')'   To use MAT88 for this problem, the following "program" is'
write(io,'(a)')'   first stored in the local computer file system, say under the'
write(io,'(a)')'   name "PDE".'
write(io,'(a)')''
write(io,'(a)')'      //Conductivity example.'
write(io,'(a)')'      //Parameters ---'
write(io,'(a)')'         rho       //radius of cylindrical inclusion'
write(io,'(a)')'         n         //number of terms in solution'
write(io,'(a)')'         m         //number of boundary points'
write(io,'(a)')'      //initialize operation counter'
write(io,'(a)')'         flop = <0 0>;'
write(io,'(a)')'      //initialize variables'
write(io,'(a)')'         m1 = round(m/3);   //number of points on each straight edge'
write(io,'(a)')'         m2 = m - m1;       //number of points with Dirichlet conditions'
write(io,'(a)')'         pi = 4*atan(1);'
write(io,'(a)')'      //generate points in Cartesian coordinates'
write(io,'(a)')'         //right hand edge'
write(io,'(a)')'         for i = 1:m1, x(i) = 1; y(i) = (1-rho)*(i-1)/(m1-1);'
write(io,'(a)')'         //top edge'
write(io,'(a)')'         for i = m2+1:m, x(i) = (1-rho)*(m-i)/(m-m2-1); y(i) = 1;'
write(io,'(a)')'         //circular edge'
write(io,'(a)')'         for i = m1+1:m2, t = pi/2*(i-m1)/(m2-m1+1); ...'
write(io,'(a)')'            x(i) = 1-rho*sin(t);  y(i) = 1-rho*cos(t);'
write(io,'(a)')'      //convert to polar coordinates'
write(io,'(a)')'         for i = 1:m-1, th(i) = atan(y(i)/x(i));  ...'
write(io,'(a)')'            r(i) = sqrt(x(i)**2+y(i)**2);'
write(io,'(a)')'         th(m) = pi/2;  r(m) = 1;'
write(io,'(a)')'      //generate matrix'
write(io,'(a)')'         //Dirichlet conditions'
write(io,'(a)')'         for i = 1:m2, for j = 1:n, k = 2*j-1; ...'
write(io,'(a)')'            a(i,j) = r(i)**k*cos(k*th(i));'
write(io,'(a)')'         //Neumann conditions'
write(io,'(a)')'         for i = m2+1:m, for j = 1:n, k = 2*j-1; ...'
write(io,'(a)')'            a(i,j) = k*r(i)**(k-1)*sin((k-1)*th(i));'
write(io,'(a)')'      //generate right hand side'
write(io,'(a)')'         for i = 1:m2, b(i) = 1;'
write(io,'(a)')'         for i = m2+1:m, b(i) = 0;'
write(io,'(a)')'      //solve for coefficients'
write(io,'(a)')'         c = A\b'
write(io,'(a)')'      //compute effective conductivity'
write(io,'(a)')'         c(2:2:n) = -c(2:2:n);'
write(io,'(a)')'         sigma = sum(c)'
write(io,'(a)')'      //output total operation count'
write(io,'(a)')'         ops = flop(2)'
write(io,'(a)')''
write(io,'(a)')'   The program can be used within MAT88 by setting the three'
write(io,'(a)')'   parameters and then accessing the file. For example,'
write(io,'(a)')''
write(io,'(a)')'      rho = .9;'
write(io,'(a)')'      n = 15;'
write(io,'(a)')'      m = 30;'
write(io,'(a)')'      exec(''PDE'')'
write(io,'(a)')''
write(io,'(a)')'   The resulting output is'
write(io,'(a)')''
write(io,'(a)')'      RHO   ='
write(io,'(a)')''
write(io,'(a)')'         .9000'
write(io,'(a)')''
write(io,'(a)')'      N     ='
write(io,'(a)')''
write(io,'(a)')'       15.'
write(io,'(a)')''
write(io,'(a)')'      M     ='
write(io,'(a)')''
write(io,'(a)')'       30.'
write(io,'(a)')''
write(io,'(a)')'      C     ='
write(io,'(a)')''
write(io,'(a)')'         2.2275'
write(io,'(a)')'        -2.2724'
write(io,'(a)')'         1.1448'
write(io,'(a)')'         0.1455'
write(io,'(a)')'        -0.1678'
write(io,'(a)')'        -0.0005'
write(io,'(a)')'        -0.3785'
write(io,'(a)')'         0.2299'
write(io,'(a)')'         0.3228'
write(io,'(a)')'        -0.2242'
write(io,'(a)')'        -0.1311'
write(io,'(a)')'         0.0924'
write(io,'(a)')'         0.0310'
write(io,'(a)')'        -0.0154'
write(io,'(a)')'        -0.0038'
write(io,'(a)')''
write(io,'(a)')'      SIGM  ='
write(io,'(a)')''
write(io,'(a)')'         5.0895'
write(io,'(a)')''
write(io,'(a)')'      OPS   ='
write(io,'(a)')''
write(io,'(a)')'         16204.'
write(io,'(a)')''
write(io,'(a)')'   A total of 16204 floating point operations were necessary to'
write(io,'(a)')'   set up the matrix, solve for the coefficients and compute the'
write(io,'(a)')'   conductivity. The operation count is roughly proportional to'
write(io,'(a)')'   m*n**2. The results obtained for sigma as a function of rho by'
write(io,'(a)')'   this approach are essentially the same as those obtained by the'
write(io,'(a)')'   finite difference technique of Keller and Sachs, but the'
write(io,'(a)')'   computational effort involved is much less.'
write(io,'(a)')''
write(io,'(a)')'8.  EIGENVALUE SENSITIVITY EXAMPLE'
write(io,'(a)')''
write(io,'(a)')'   In this example, we construct a matrix whose eigenvalues are'
write(io,'(a)')'   moderately sensitive to perturbations and then analyze that'
write(io,'(a)')'   sensitivity. We begin with the statement'
write(io,'(a)')''
write(io,'(a)')'      B = <3 0 7; 0 2 0; 0 0 1>'
write(io,'(a)')''
write(io,'(a)')'   which produces'
write(io,'(a)')''
write(io,'(a)')'      B     ='
write(io,'(a)')''
write(io,'(a)')'          3.    0.    7.'
write(io,'(a)')'          0.    2.    0.'
write(io,'(a)')'          0.    0.    1.'
write(io,'(a)')''
write(io,'(a)')'   Obviously, the eigenvalues of B are 1, 2 and 3 . Moreover,'
write(io,'(a)')'   since B is not symmetric, these eigenvalues are slightly'
write(io,'(a)')'   sensitive to perturbation. (The value b(1,3) = 7 was chosen so'
write(io,'(a)')'   that the elements of the matrix A below are less than 1000.)'
write(io,'(a)')''
write(io,'(a)')'   We now generate a similarity transformation to disguise the'
write(io,'(a)')'   eigenvalues and make them more sensitive.'
write(io,'(a)')''
write(io,'(a)')'      L = <1 0 0; 2 1 0; -3 4 1>, M = L\L'''
write(io,'(a)')''
write(io,'(a)')'      L     ='
write(io,'(a)')''
write(io,'(a)')'          1.    0.    0.'
write(io,'(a)')'          2.    1.    0.'
write(io,'(a)')'         -3.    4.    1.'
write(io,'(a)')''
write(io,'(a)')'      M     ='
write(io,'(a)')''
write(io,'(a)')'          1.0000    2.0000   -3.0000'
write(io,'(a)')'         -2.0000   -3.0000   10.0000'
write(io,'(a)')'         11.0000   18.0000  -48.0000'
write(io,'(a)')''
write(io,'(a)')'   The matrix M has determinant equal to 1 and is moderately badly'
write(io,'(a)')'   conditioned. The similarity transformation is'
write(io,'(a)')''
write(io,'(a)')'      A = M*B/M'
write(io,'(a)')''
write(io,'(a)')'      A     ='
write(io,'(a)')''
write(io,'(a)')'        -64.0000   82.0000   21.0000'
write(io,'(a)')'        144.0000 -178.0000  -46.0000'
write(io,'(a)')'       -771.0000  962.0000  248.0000'
write(io,'(a)')''
write(io,'(a)')'   Because det(M) = 1 , the elements of A would be exact integers'
write(io,'(a)')'   if there were no roundoff. So,'
write(io,'(a)')''
write(io,'(a)')'      A = round(A)'
write(io,'(a)')''
write(io,'(a)')'      A     ='
write(io,'(a)')''
write(io,'(a)')'        -64.   82.   21.'
write(io,'(a)')'        144. -178.  -46.'
write(io,'(a)')'       -771.  962.  248.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   This, then, is our test matrix. We can now forget how it'
write(io,'(a)')'   was generated and analyze its eigenvalues.'
write(io,'(a)')''
write(io,'(a)')'      <X,D> = eig(A)'
write(io,'(a)')''
write(io,'(a)')'      D     ='
write(io,'(a)')''
write(io,'(a)')'          3.0000    0.0000    0.0000'
write(io,'(a)')'          0.0000    1.0000    0.0000'
write(io,'(a)')'          0.0000    0.0000    2.0000'
write(io,'(a)')''
write(io,'(a)')'      X     ='
write(io,'(a)')''
write(io,'(a)')'          -.0891    3.4903   41.8091'
write(io,'(a)')'           .1782   -9.1284  -62.7136'
write(io,'(a)')'          -.9800   46.4473  376.2818'
write(io,'(a)')''
write(io,'(a)')'   Since A is similar to B, its eigenvalues are also 1, 2 and 3.'
write(io,'(a)')'   They happen to be computed in another order by the EISPACK'
write(io,'(a)')'   subroutines. The fact that the columns of X, which are the'
write(io,'(a)')'   eigenvectors, are so far from being orthonormal is our first'
write(io,'(a)')'   indication that the eigenvalues are sensitive. To see this'
write(io,'(a)')'   sensitivity, we display more figures of the computed eigenvalues.'
write(io,'(a)')''
write(io,'(a)')'      long, diag(D)'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'         2.999999999973599'
write(io,'(a)')'         1.000000000015625'
write(io,'(a)')'         2.000000000011505'
write(io,'(a)')''
write(io,'(a)')'   We see that, on this computer, the last five significant figures'
write(io,'(a)')'   are contaminated by roundoff error. A somewhat superficial'
write(io,'(a)')'   explanation of this is provided by'
write(io,'(a)')''
write(io,'(a)')'      short,  cond(X)'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'         3.2216e+05'
write(io,'(a)')''
write(io,'(a)')'   The condition number of X gives an upper bound for the relative'
write(io,'(a)')'   error in the computed eigenvalues. However, this condition'
write(io,'(a)')'   number is affected by scaling.'
write(io,'(a)')''
write(io,'(a)')'      X = X/diag(X(3,:)),  cond(X)'
write(io,'(a)')''
write(io,'(a)')'      X     ='
write(io,'(a)')''
write(io,'(a)')'           .0909     .0751     .1111'
write(io,'(a)')'          -.1818    -.1965    -.1667'
write(io,'(a)')'          1.0000    1.0000    1.0000'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'         1.7692e+03'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   Rescaling the eigenvectors so that their last components are'
write(io,'(a)')'   all equal to one has two consequences. The condition of X is'
write(io,'(a)')'   decreased by over two orders of magnitude. (This is about the'
write(io,'(a)')'   minimum condition that can be obtained by such diagonal scaling.)'
write(io,'(a)')'   Moreover, it is now apparent that the three eigenvectors are'
write(io,'(a)')'   nearly parallel.'
write(io,'(a)')''
write(io,'(a)')'   More detailed information on the sensitivity of the'
write(io,'(a)')'   individual eigenvalues involves the left eigenvectors.'
write(io,'(a)')''
write(io,'(a)')'      Y = inv(X''),  Y''*A*X'
write(io,'(a)')''
write(io,'(a)')'      Y     ='
write(io,'(a)')''
write(io,'(a)')'       -511.5000  259.5000  252.0000'
write(io,'(a)')'        616.0000 -346.0000 -270.0000'
write(io,'(a)')'        159.5000  -86.5000  -72.0000'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'          3.0000     .0000     .0000'
write(io,'(a)')'           .0000    1.0000     .0000'
write(io,'(a)')'           .0000     .0000    2.0000'
write(io,'(a)')''
write(io,'(a)')'   We are now in a position to compute the sensitivities of the'
write(io,'(a)')'   individual eigenvalues.'
write(io,'(a)')''
write(io,'(a)')'      for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j)); end,  C'
write(io,'(a)')''
write(io,'(a)')'      C     ='
write(io,'(a)')''
write(io,'(a)')'        833.1092'
write(io,'(a)')'        450.7228'
write(io,'(a)')'        383.7564'
write(io,'(a)')''
write(io,'(a)')'   These three numbers are the reciprocals of the cosines of the'
write(io,'(a)')'   angles between the left and right eigenvectors. It can be shown'
write(io,'(a)')'   that perturbation of the elements of A can result in a'
write(io,'(a)')'   perturbation of the j-th eigenvalue which is c(j) times as large.'
write(io,'(a)')'   In this example, the first eigenvalue has the largest'
write(io,'(a)')'   sensitivity.'
write(io,'(a)')''
write(io,'(a)')'   We now proceed to show that A is close to a matrix with a'
write(io,'(a)')'   double eigenvalue. The direction of the required perturbation is'
write(io,'(a)')'   given by'
write(io,'(a)')''
write(io,'(a)')'      E = -1.e-6*Y(:,1)*X(:,1)'''
write(io,'(a)')''
write(io,'(a)')'      E     ='
write(io,'(a)')''
write(io,'(a)')'         1.0e-03 *'
write(io,'(a)')''
write(io,'(a)')'           .0465    -.0930     .5115'
write(io,'(a)')'          -.0560     .1120    -.6160'
write(io,'(a)')'          -.0145     .0290    -.1595'
write(io,'(a)')''
write(io,'(a)')'   With some trial and error which we do not show, we bracket the'
write(io,'(a)')'   point where two eigenvalues of a perturbed A coalesce and then'
write(io,'(a)')'   become complex.'
write(io,'(a)')''
write(io,'(a)')'      eig(A + .4*E),  eig(A + .5*E)'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'          1.1500'
write(io,'(a)')'          2.5996'
write(io,'(a)')'          2.2504'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'         2.4067 +  .1753*i'
write(io,'(a)')'         2.4067 -  .1753*i'
write(io,'(a)')'         1.1866 + 0.0000*i'
write(io,'(a)')''
write(io,'(a)')'   Now, a bisecting search, driven by the imaginary part of one of'
write(io,'(a)')'   the eigenvalues, finds the point where two eigenvalues are nearly'
write(io,'(a)')'   equal.'
write(io,'(a)')''
write(io,'(a)')'      r = .4;  s = .5;'
write(io,'(a)')''
write(io,'(a)')'      while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...'
write(io,'(a)')'        if imag(d(1))=0, r = t; else, s = t;'
write(io,'(a)')''
write(io,'(a)')'      long,  T'
write(io,'(a)')''
write(io,'(a)')'      T     ='
write(io,'(a)')''
write(io,'(a)')'           .450380734134507'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   Finally, we display the perturbed matrix, which is obviously'
write(io,'(a)')'   close to the original, and its pair of nearly equal eigenvalues.'
write(io,'(a)')'   (We have dropped a few digits from the long output.)'
write(io,'(a)')''
write(io,'(a)')'      A+t*E,  eig(A+t*E)'
write(io,'(a)')''
write(io,'(a)')'      A'
write(io,'(a)')''
write(io,'(a)')'       -63.999979057   81.999958114   21.000230369'
write(io,'(a)')'       143.999974778 -177.999949557  -46.000277434'
write(io,'(a)')'      -771.000006530  962.000013061  247.999928164'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'         2.415741150'
write(io,'(a)')'         2.415740621'
write(io,'(a)')'         1.168517777'
write(io,'(a)')''
write(io,'(a)')'   The first two eigenvectors of A + t*E are almost'
write(io,'(a)')'   indistinguishable indicating that the perturbed matrix is almost'
write(io,'(a)')'   defective.'
write(io,'(a)')''
write(io,'(a)')'      <X,D> = eig(A+t*E);  X = X/diag(X(3,:))'
write(io,'(a)')''
write(io,'(a)')'      X     ='
write(io,'(a)')''
write(io,'(a)')'          .096019578     .096019586    .071608466'
write(io,'(a)')'         -.178329614    -.178329608   -.199190520'
write(io,'(a)')'         1.000000000    1.000000000   1.000000000'
write(io,'(a)')''
write(io,'(a)')'      short,  cond(X)'
write(io,'(a)')''
write(io,'(a)')'      ANS   ='
write(io,'(a)')''
write(io,'(a)')'         3.3997e+09'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'9.  SYNTAX DIAGRAMS'
write(io,'(a)')''
write(io,'(a)')'   A formal description of the language acceptable to MAT88,'
write(io,'(a)')'   as well as a flow chart of the mat88 program, is provided by the'
write(io,'(a)')'   syntax diagrams or syntax graphs of wirth [6]. There are eleven'
write(io,'(a)')'   non-terminal symbols in the language:'
write(io,'(a)')''
write(io,'(a)')'      LINE, STATEMENT, CLAUSE, EXPRESSION, TERM,'
write(io,'(a)')'      FACTOR, NUMBER, INTEGER, NAME, COMMAND, TEXT .'
write(io,'(a)')''
write(io,'(a)')'   The diagrams define each of the non-terminal symbols using the'
write(io,'(a)')'   others and the terminal symbols:'
write(io,'(a)')''
write(io,'(a)')'      LETTER -- A THROUGH Z,'
write(io,'(a)')'      DIGIT  -- 0 THROUGH 9,'
write(io,'(a)')'      CHAR   -- ( ) ; : + - * / \ = . , < >'
write(io,'(a)')'      QUOTE  -- '''
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   LINE'
write(io,'(a)')''
write(io,'(a)')'          |-----> STATEMENT >----|'
write(io,'(a)')'          |                      |'
write(io,'(a)')'          |-----> CLAUSE >-------|'
write(io,'(a)')'          |                      |'
write(io,'(a)')'   -------|-----> EXPR >---------|------>'
write(io,'(a)')'        | |                      | |'
write(io,'(a)')'        | |-----> COMMAND >------| |'
write(io,'(a)')'        | |                      | |'
write(io,'(a)')'        | |-> > >-> EXPR >-> < >-| |'
write(io,'(a)')'        | |                      | |'
write(io,'(a)')'        | |----------------------| |'
write(io,'(a)')'        |                          |'
write(io,'(a)')'        |        |-< ; <-|         |'
write(io,'(a)')'        |--------|       |---------|'
write(io,'(a)')'                 |-< , <-|'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   STATEMENT'
write(io,'(a)')''
write(io,'(a)')'        |-> NAME >--------------------------------|'
write(io,'(a)')'        |          |                              |'
write(io,'(a)')'        |          |         |--> : >---|         |'
write(io,'(a)')'        |          |         |          |         |'
write(io,'(a)')'        |          |-> ( >---|-> EXPR >-|---> ) >-|'
write(io,'(a)')'        |                  |              |       |'
write(io,'(a)')'   -----|                  |-----< , <----|       |--> = >--> EXPR >--->'
write(io,'(a)')'        |                                         |'
write(io,'(a)')'        |       |--< , <---|                      |'
write(io,'(a)')'        |       |          |                      |'
write(io,'(a)')'        |-> < >---> NAME >---> > >----------------|'
write(io,'(a)')''
write(io,'(a)')'   CLAUSE'
write(io,'(a)')''
write(io,'(a)')'        |---> FOR   >---> NAME >---> = >---> EXPR >--------------|'
write(io,'(a)')'        |                                                        |'
write(io,'(a)')'        | |-> WHILE >-|                                          |'
write(io,'(a)')'        |-|           |-> EXPR >----------------------           |'
write(io,'(a)')'        | |-> IF    >-|          |   |   |   |   |   |           |'
write(io,'(a)')'   -----|                        <   <=  =   <>  >=  >           |---->'
write(io,'(a)')'        |                        |   |   |   |   |   |           |'
write(io,'(a)')'        |                        ----------------------> EXPR >--|'
write(io,'(a)')'        |                                                        |'
write(io,'(a)')'        |---> ELSE  >--------------------------------------------|'
write(io,'(a)')'        |                                                        |'
write(io,'(a)')'        |---> END   >--------------------------------------------|'
write(io,'(a)')''
write(io,'(a)')'   EXPR'
write(io,'(a)')''
write(io,'(a)')'          |-> + >-|'
write(io,'(a)')'          |       |'
write(io,'(a)')'   -------|-------|-------> TERM >---------->'
write(io,'(a)')'          |       |    |             |'
write(io,'(a)')'          |-> - >-|    |  |-< + <-|  |'
write(io,'(a)')'                       |  |       |  |'
write(io,'(a)')'                       |--|-< - <-|--|'
write(io,'(a)')'                          |       |'
write(io,'(a)')'                          |-< : <-|'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   TERM'
write(io,'(a)')''
write(io,'(a)')'   ---------------------> FACTOR >---------------------->'
write(io,'(a)')'           |                                   |'
write(io,'(a)')'           |             |-< * <-|             |'
write(io,'(a)')'           |  |-------|  |       |  |-------|  |'
write(io,'(a)')'           |--|       |--|-< / <-|--|       |--|'
write(io,'(a)')'              |-< . <-|  |       |  |-< . <-|'
write(io,'(a)')'                         |-< \ <-|'
write(io,'(a)')''
write(io,'(a)')'   FACTOR'
write(io,'(a)')''
write(io,'(a)')'        |----------------> NUMBER >---------------|'
write(io,'(a)')'        |                                         |'
write(io,'(a)')'        |-> NAME >--------------------------------|'
write(io,'(a)')'        |          |                              |'
write(io,'(a)')'        |          |         |--> : >---|         |'
write(io,'(a)')'        |          |         |          |         |'
write(io,'(a)')'        |          |-> ( >---|-> EXPR >-|---> ) >-|'
write(io,'(a)')'        |                  |              |       |'
write(io,'(a)')'        |                  |-----< , <----|       |'
write(io,'(a)')'        |                                         |'
write(io,'(a)')'   -----|------------> ( >-----> EXPR >-----> ) >-|-|-------|----->'
write(io,'(a)')'        |                                         | |       | |'
write(io,'(a)')'        |                  |--------------|       | |-> '' >-| |'
write(io,'(a)')'        |                  |              |       |           |'
write(io,'(a)')'        |------------> < >-|---> EXPR >---|-> > >-|           |'
write(io,'(a)')'        |                    |          |         |           |'
write(io,'(a)')'        |                    |--<   <---|         |           |'
write(io,'(a)')'        |                    |          |         |           |'
write(io,'(a)')'        |                    |--< ; <---|         |           |'
write(io,'(a)')'        |                    |          |         |           |'
write(io,'(a)')'        |                    |--< , <---|         |           |'
write(io,'(a)')'        |                                         |           |'
write(io,'(a)')'        |------------> > >-----> EXPR >-----> < >-|           |'
write(io,'(a)')'        |                                         |           |'
write(io,'(a)')'        |-----> FACTOR >---> ** >---> FACTOR >----|           |'
write(io,'(a)')'        |                                                     |'
write(io,'(a)')'        |------------> '' >-----> TEXT >-----> '' >-------------|'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   NUMBER'
write(io,'(a)')''
write(io,'(a)')'       |----------|                          |-> + >-|'
write(io,'(a)')'       |          |                          |       |'
write(io,'(a)')'   -----> INT >-----> . >---> INT >-----> E >---------> INT >---->'
write(io,'(a)')'                |                   | |      |       |        |'
write(io,'(a)')'                |                   | |      |-> - >-|        |'
write(io,'(a)')'                |                   | |                       |'
write(io,'(a)')'                |---------------------------------------------|'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   INT'
write(io,'(a)')''
write(io,'(a)')'   ------------> DIGIT >----------->'
write(io,'(a)')'             |           |'
write(io,'(a)')'             |-----------|'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   NAME'
write(io,'(a)')''
write(io,'(a)')'                     |--< LETTER <--|'
write(io,'(a)')'                     |              |'
write(io,'(a)')'   ------> LETTER >--|--------------|----->'
write(io,'(a)')'                     |              |'
write(io,'(a)')'                     |--< DIGIT  <--|'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   COMMAND'
write(io,'(a)')''
write(io,'(a)')'                           |--> NAME >--|'
write(io,'(a)')'                           |            |'
write(io,'(a)')'   --------> NAME >--------|------------|---->'
write(io,'(a)')'                           |            |'
write(io,'(a)')'                           |--> CHAR >--|'
write(io,'(a)')'                           |            |'
write(io,'(a)')'                           |---> '' >----|'
write(io,'(a)')''
write(io,'(a)')'   TEXT'
write(io,'(a)')''
write(io,'(a)')'                   |-> LETTER >--|'
write(io,'(a)')'                   |             |'
write(io,'(a)')'                   |-> DIGIT >---|'
write(io,'(a)')'   ----------------|             |-------------->'
write(io,'(a)')'               |   |-> CHAR >----|   |'
write(io,'(a)')'               |   |             |   |'
write(io,'(a)')'               |   |-> '' >-> '' >-|   |'
write(io,'(a)')'               |                     |'
write(io,'(a)')'               |---------------------|'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'10.  THE PARSER-INTERPRETER'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   The structure of the parser-interpreter is similar to that'
write(io,'(a)')'   of Wirth''s compiler [6] for his simple language, PL/0 , except'
write(io,'(a)')'   that MAT88 is programmed in Fortran, which does not have'
write(io,'(a)')'   explicit recursion. The interrelation of the primary subroutines'
write(io,'(a)')'   is shown in the following diagram.'
write(io,'(a)')''
write(io,'(a)')'         MAIN'
write(io,'(a)')'           |'
write(io,'(a)')'         MAT88     |--CLAUSE'
write(io,'(a)')'           |       |    |'
write(io,'(a)')'         PARSE-----|--EXPR----TERM----FACTOR'
write(io,'(a)')'                   |    |       |       |'
write(io,'(a)')'                   |    |-------|-------|'
write(io,'(a)')'                   |    |       |       |'
write(io,'(a)')'                   |  STACK1  STACK2  STACKG'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |--STACKP--PRINT'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |--COMAND'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |          |--CGECO'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |          |--CGEFA'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |--MATFN1--|--CGESL'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |          |--CGEDI'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |          |--CPOFA'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |          |--IMTQL2'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |          |--HTRIDI'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |--MATFN2--|--HTRIBK'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |          |--CORTH'
write(io,'(a)')'                   |          |'
write(io,'(a)')'                   |          |--COMQR3'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |--MATFN3-----CSVDC'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |          |--CQRDC'
write(io,'(a)')'                   |--MATFN4--|'
write(io,'(a)')'                   |          |--CQRSL'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |'
write(io,'(a)')'                   |          |--FILES'
write(io,'(a)')'                   |--MATFN5--|'
write(io,'(a)')'                              |--SAVLOD'
write(io,'(a)')''
write(io,'(a)')'   Subroutine PARSE controls the interpretation of each'
write(io,'(a)')'   statement. It calls subroutines that process the various'
write(io,'(a)')'   syntactic quantities such as command, expression, term and'
write(io,'(a)')'   factor. A fairly simple program stack mechanism allows these'
write(io,'(a)')'   subroutines to recursively "call" each other along the lines'
write(io,'(a)')'   allowed by the syntax diagrams. The four STACK subroutines'
write(io,'(a)')'   manage the variable memory and perform elementary operations,'
write(io,'(a)')'   such as matrix addition and transposition.'
write(io,'(a)')''
write(io,'(a)')'   The four subroutines MATFN1 though MATFN4 are called'
write(io,'(a)')'   whenever "serious" matrix computations are required. They are'
write(io,'(a)')'   interface routines which call the various LINPACK and EISPACK'
write(io,'(a)')'   subroutines. MATFN5 primarily handles the file access tasks.'
write(io,'(a)')''
write(io,'(a)')'   Two large real arrays, STKR and STKI, are used to store all'
write(io,'(a)')'   the matrices. Four integer arrays are used to store the names,'
write(io,'(a)')'   the row and column dimensions, and the pointers into the real'
write(io,'(a)')'   stacks. The following diagram illustrates this storage scheme.'
write(io,'(a)')''
write(io,'(a)')'    TOP         IDSTK     MSTK NSTK LSTK               STKR       STKI'
write(io,'(a)')'     --      -- -- -- --   --   --   --              --------   --------'
write(io,'(a)')'    |  |--->|  |  |  |  | |  | |  | |  |----------->|        | |        |'
write(io,'(a)')'     --      -- -- -- --   --   --   --              --------   --------'
write(io,'(a)')'            |  |  |  |  | |  | |  | |  |            |        | |        |'
write(io,'(a)')'             -- -- -- --   --   --   --              --------   --------'
write(io,'(a)')'                  .         .    .    .                  .          .'
write(io,'(a)')'                  .         .    .    .                  .          .'
write(io,'(a)')'                  .         .    .    .                  .          .'
write(io,'(a)')'             -- -- -- --   --   --   --              --------   --------'
write(io,'(a)')'    BOT     |  |  |  |  | |  | |  | |  |            |        | |        |'
write(io,'(a)')'     --      -- -- -- --   --   --   --              --------   --------'
write(io,'(a)')'    |  |--->| X|  |  |  | | 2| | 1| |  |----------->|  3.14  | |  0.00  |'
write(io,'(a)')'     --      -- -- -- --   --   --   --              --------   --------'
write(io,'(a)')'            | A|  |  |  | | 2| | 2| |  |---------   |  0.00  | |  1.00  |'
write(io,'(a)')'             -- -- -- --   --   --   --          \   --------   --------'
write(io,'(a)')'            | E| P| S|  | | 1| | 1| |  |-------   ->| 11.00  | |  0.00  |'
write(io,'(a)')'             -- -- -- --   --   --   --        \     --------   --------'
write(io,'(a)')'            | F| L| O| P| | 1| | 2| |  |------  \   | 21.00  | |  0.00  |'
write(io,'(a)')'             -- -- -- --   --   --   --       \  \   --------   --------'
write(io,'(a)')'            | E| Y| E|  | |-1| |-1| |  |---    \ |  | 12.00  | |  0.00  |'
write(io,'(a)')'             -- -- -- --   --   --   --    \   | |   --------   --------'
write(io,'(a)')'            | R| A| N| D| | 1| | 1| |  |-   \  | |  | 22.00  | |  0.00  |'
write(io,'(a)')'             -- -- -- --   --   --   --  \  |  \ \   --------   --------'
write(io,'(a)')'                                         |  \   \ ->| 1.E-15 | |  0.00  |'
write(io,'(a)')'                                         \   \   \   --------   --------'
write(io,'(a)')'                                          \   \   ->|  0.00  | |  0.00  |'
write(io,'(a)')'                                           \   \     --------   --------'
write(io,'(a)')'                                            \   \   |  0.00  | |  0.00  |'
write(io,'(a)')'                                             \   \   --------   --------'
write(io,'(a)')'                                              \   ->|  1.00  | |  0.00  |'
write(io,'(a)')'                                               \     --------   --------'
write(io,'(a)')'                                                --->| URAND  | |  0.00  |'
write(io,'(a)')'                                                     --------   --------'
write(io,'(a)')''
write(io,'(a)')'   The top portion of the stack is used for temporary variables'
write(io,'(a)')'   and the bottom portion for saved variables. The figure shows the'
write(io,'(a)')'   situation after the line'
write(io,'(a)')''
write(io,'(a)')'      A = <11,12; 21,22>,  x = <3.14, sqrt(-1)>'''
write(io,'(a)')''
write(io,'(a)')'   has been processed. The four permanent names, EPS, FLOP, RAND'
write(io,'(a)')'   and EYE, occupy the last four positions of the variable stacks.'
write(io,'(a)')'   RAND has dimensions 1 by 1, but whenever its value is requested,'
write(io,'(a)')'   a random number generator is used instead. EYE has dimensions -1'
write(io,'(a)')'   by -1 to indicate that the actual dimensions must be determined'
write(io,'(a)')'   later by context. The two saved variables have dimensions 2 by 2'
write(io,'(a)')'   and 2 by 1 and so take up a total of 6 locations.'
write(io,'(a)')''
write(io,'(a)')'   Subsequent statements involving A and x will result in'
write(io,'(a)')'   temporary copies being made in the top of the stack for use in'
write(io,'(a)')'   the actual calculations. Whenever the top of the stack reaches'
write(io,'(a)')'   the bottom, a message indicating memory has been exceeded is'
write(io,'(a)')'   printed, but the current variables are not affected.'
write(io,'(a)')''
write(io,'(a)')'   This modular structure makes it possible to implement MAT88'
write(io,'(a)')'   on a system with a limited amount of memory. The object code for'
write(io,'(a)')'   the MATFN''s and the LINPACK-EISPACK subroutines is rarely needed.'
write(io,'(a)')'   Although it is not standard, many Fortran operating systems'
write(io,'(a)')'   provide some overlay mechanism so that this code is brought into'
write(io,'(a)')'   the main memory only when required. The variables, which occupy'
write(io,'(a)')'   a relatively small portion of the memory, remain in place, while'
write(io,'(a)')'   the subroutines which process them are loaded a few at a time.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'11. THE NUMERICAL ALGORITHMS'
write(io,'(a)')''
write(io,'(a)')'   The algorithms underlying the basic MAT88 functions are'
write(io,'(a)')'   described in the LINPACK and EISPACK guides [1-3]. The following'
write(io,'(a)')'   list gives the subroutines used by these functions.'
write(io,'(a)')''
write(io,'(a)')'      INV(A)          - CGECO,CGEDI'
write(io,'(a)')'      DET(A)          - CGECO,CGEDI'
write(io,'(a)')'      LU(A)           - CGEFA'
write(io,'(a)')'      RCOND(A)        - CGECO'
write(io,'(a)')'      CHOL(A)         - CPOFA'
write(io,'(a)')'      SVD(A)          - CSVDC'
write(io,'(a)')'      COND(A)         - CSVDC'
write(io,'(a)')'      NORM(A,2)       - CSVDC'
write(io,'(a)')'      PINV(A,eps)     - CSVDC'
write(io,'(a)')'      RANK(A,eps)     - CSVDC'
write(io,'(a)')'      QR(A)           - CQRDC,CQRSL'
write(io,'(a)')'      ORTH(A)         - CQRDC,CSQSL'
write(io,'(a)')'      A\B and B/A     - CGECO,CGESL if A is square.'
write(io,'(a)')'                      - CQRDC,CQRSL if A is not square.'
write(io,'(a)')'      EIG(A)          - HTRIDI,IMTQL2,HTRIBK if A is Hermitian.'
write(io,'(a)')'                      - CORTH,COMQR2         if A is not Hermitian.'
write(io,'(a)')'      SCHUR(A)        - same as EIG.'
write(io,'(a)')'      HESS(A)         - same as EIG.'
write(io,'(a)')''
write(io,'(a)')'   Minor modifications were made to all these subroutines. The'
write(io,'(a)')'   LINPACK routines were changed to replace the Fortran complex'
write(io,'(a)')'   arithmetic with explicit references to real and imaginary parts.'
write(io,'(a)')'   Since most of the floating point arithmetic is concentrated in a'
write(io,'(a)')'   few low-level subroutines which perform vector operations (the'
write(io,'(a)')'   Basic Linear Algebra Subprograms), this was not an extensive'
write(io,'(a)')'   change. It also facilitated implementation of the FLOP and CHOP'
write(io,'(a)')'   features which count and optionally truncate each floating point'
write(io,'(a)')'   operation.'
write(io,'(a)')''
write(io,'(a)')'   The EISPACK subroutine COMQR2 was modified to allow access'
write(io,'(a)')'   to the Schur triangular form, ordinarily just an intermediate'
write(io,'(a)')'   result. IMTQL2 was modified to make computation of the'
write(io,'(a)')'   eigenvectors optional. Both subroutines were modified to'
write(io,'(a)')'   eliminate the machine-dependent accuracy parameter and all the'
write(io,'(a)')'   EISPACK subroutines were changed to include FLOP and CHOP.'
write(io,'(a)')''
write(io,'(a)')'   The algorithms employed for the POLY and ROOTS functions'
write(io,'(a)')'   illustrate an interesting aspect of the modern approach to'
write(io,'(a)')'   eigenvalue computation. POLY(A) generates the characteristic'
write(io,'(a)')'   polynomial of A and ROOTS(POLY(A)) finds the roots of that'
write(io,'(a)')'   polynomial, which are, of course, the eigenvalues of A . But both'
write(io,'(a)')'   POLY and ROOTS use EISPACK eigenvalues subroutines, which are'
write(io,'(a)')'   based on similarity transformations. So the classical approach'
write(io,'(a)')'   which characterizes eigenvalues as roots of the characteristic'
write(io,'(a)')'   polynomial is actually reversed.'
write(io,'(a)')''
write(io,'(a)')'   If A is an n by n matrix, POLY(A) produces the coefficients'
write(io,'(a)')'   C(1) through C(n+1), with C(1) = 1, in'
write(io,'(a)')''
write(io,'(a)')'         DET(z*EYE-A) = C(1)*z**n + ... + C(n)*z + C(n+1) .'
write(io,'(a)')''
write(io,'(a)')'   The algorithm can be expressed compactly using MAT88:'
write(io,'(a)')''
write(io,'(a)')'         Z = EIG(A);'
write(io,'(a)')'         C = 0*ONES(n+1,1);  C(1) = 1;'
write(io,'(a)')'         for j = 1:n, C(2:j+1) = C(2:j+1) - Z(j)*C(1:j);'
write(io,'(a)')'         C'
write(io,'(a)')''
write(io,'(a)')'   This recursion is easily derived by expanding the product'
write(io,'(a)')''
write(io,'(a)')'         (z - z(1))*(z - z(2))* ... * (z-z(n)) .'
write(io,'(a)')''
write(io,'(a)')'   It is possible to prove that POLY(A) produces the coefficients in'
write(io,'(a)')'   the characteristic polynomial of a matrix within roundoff error'
write(io,'(a)')'   of A. This is true even if the eigenvalues of A are badly'
write(io,'(a)')'   conditioned. The traditional algorithms for obtaining the'
write(io,'(a)')'   characteristic polynomial which do not use the eigenvalues do not'
write(io,'(a)')'   have such satisfactory numerical properties.'
write(io,'(a)')''
write(io,'(a)')'   If C is a vector with n+1 components, ROOTS(C) finds the'
write(io,'(a)')'   roots of the polynomial of degree n ,'
write(io,'(a)')''
write(io,'(a)')'          p(z) = C(1)*z**n + ... + C(n)*z + C(n+1) .'
write(io,'(a)')''
write(io,'(a)')'   The algorithm simply involves computing the eigenvalues of the'
write(io,'(a)')'   companion matrix:'
write(io,'(a)')''
write(io,'(a)')'         A = 0*ONES(n,n)'
write(io,'(a)')'         for j = 1:n, A(1,j) = -C(j+1)/C(1);'
write(io,'(a)')'         for i = 2:n, A(i,i-1) = 1;'
write(io,'(a)')'         EIG(A)'
write(io,'(a)')''
write(io,'(a)')'   It is possible to prove that the results produced are the exact'
write(io,'(a)')'   eigenvalues of a matrix within roundoff error of the companion'
write(io,'(a)')'   matrix A, but this does not mean that they are the exact roots of'
write(io,'(a)')'   a polynomial with coefficients within roundoff error of those in'
write(io,'(a)')'   C . There are more accurate, more efficient methods for finding'
write(io,'(a)')'   polynomial roots, but this approach has the crucial advantage'
write(io,'(a)')'   that it does not require very much additional code.'
write(io,'(a)')''
write(io,'(a)')'   The elementary functions EXP, LOG, SQRT, SIN, COS and ATAN'
write(io,'(a)')'   are applied to square matrices by diagonalizing the matrix,'
write(io,'(a)')'   applying the functions to the individual eigenvalues and then'
write(io,'(a)')'   transforming back. For example, EXP(A) is computed by'
write(io,'(a)')''
write(io,'(a)')'         <X,D> = EIG(A);'
write(io,'(a)')'         for j = 1:n, D(j,j) = EXP(D(j,j));'
write(io,'(a)')'         X*D/X'
write(io,'(a)')''
write(io,'(a)')'   This is essentially method number 14 out of the 19 ''dubious'''
write(io,'(a)')'   possibilities described in [8]. It is dubious because it doesn''t'
write(io,'(a)')'   always work. The matrix of eigenvectors X can be arbitrarily'
write(io,'(a)')'   badly conditioned and all accuracy lost in the computation of'
write(io,'(a)')'   X*D/X. A warning message is printed if RCOND(X) is very small,'
write(io,'(a)')'   but this only catches the extreme cases. An example of a case'
write(io,'(a)')'   not detected is when A has a double eigenvalue, but theoretically'
write(io,'(a)')'   only one linearly independent eigenvector associated with it.'
write(io,'(a)')'   The computed eigenvalues will be separated by something on the'
write(io,'(a)')'   order of the square root of the roundoff level. This separation'
write(io,'(a)')'   will be reflected in RCOND(X) which will probably not be small'
write(io,'(a)')'   enough to trigger the error message. The computed EXP(A) will be'
write(io,'(a)')'   accurate to only half precision. Better methods are known for'
write(io,'(a)')'   computing EXP(A), but they do not easily extend to the other five'
write(io,'(a)')'   functions and would require a considerable amount of additional'
write(io,'(a)')'   code.'
write(io,'(a)')''
write(io,'(a)')'   The expression A**p is evaluated by repeated multiplication'
write(io,'(a)')'   if p is an integer greater than 1. Otherwise it is evaluated by'
write(io,'(a)')''
write(io,'(a)')'         <X,D> = EIG(A);'
write(io,'(a)')'         for j = 1:n, D(j,j) = EXP(p*LOG(D(j,j)))'
write(io,'(a)')'         X*D/X'
write(io,'(a)')''
write(io,'(a)')'   This suffers from the same potential loss of accuracy if X is'
write(io,'(a)')'   badly conditioned. It was partly for this reason that the case'
write(io,'(a)')'   p = 1 is included in the general case. Comparison of A**1 with A'
write(io,'(a)')'   gives some idea of the loss of accuracy for other values of p and'
write(io,'(a)')'   for the elementary functions.'
write(io,'(a)')''
write(io,'(a)')'   RREF, the reduced row echelon form, is of some interest in'
write(io,'(a)')'   theoretical linear algebra, although it has little computational'
write(io,'(a)')'   value. It is included in MAT88 for pedagogical reasons. The'
write(io,'(a)')'   algorithm is essentially Gauss-Jordan elimination with detection'
write(io,'(a)')'   of negligible columns applied to rectangular matrices.'
write(io,'(a)')''
write(io,'(a)')'   There are three separate places in MAT88 where the rank of'
write(io,'(a)')'   a matrix is implicitly computed: in RREF(A), in A\B for non-'
write(io,'(a)')'   square A, and in the pseudoinverse PINV(A). Three different'
write(io,'(a)')'   algorithms with three different criteria for negligibility are'
write(io,'(a)')'   used and so it is possible that three different values could be'
write(io,'(a)')'   produced for the same matrix. With RREF(A), the rank of A is the'
write(io,'(a)')'   number of nonzero rows. The elimination algorithm used for RREF'
write(io,'(a)')'   is the fastest of the three rank-determining algorithms, but it'
write(io,'(a)')'   is the least sophisticated numerically and the least reliable.'
write(io,'(a)')'   With A\B, the algorithm is essentially that used by example'
write(io,'(a)')'   subroutine SQRST in chapter 9 of the LINPACK guide. With'
write(io,'(a)')'   PINV(A), the algorithm is based on the singular value'
write(io,'(a)')'   decomposition and is described in chapter 11 of the LINPACK'
write(io,'(a)')'   guide. The SVD algorithm is the most time-consuming, but the'
write(io,'(a)')'   most reliable and is therefore also used for RANK(A).'
write(io,'(a)')''
write(io,'(a)')'   The uniformly distributed random numbers in RAND are'
write(io,'(a)')'   obtained from the machine-independent random number generator'
write(io,'(a)')'   URAND described in [9]. It is possible to switch to normally'
write(io,'(a)')'   distributed random numbers, which are obtained using a'
write(io,'(a)')'   transformation also described in [9].'
write(io,'(a)')''
write(io,'(a)')'        The computation of'
write(io,'(a)')''
write(io,'(a)')'                   2    2'
write(io,'(a)')'             sqrt(a  + b )'
write(io,'(a)')''
write(io,'(a)')'   is required in many matrix algorithms, particularly those'
write(io,'(a)')'   involving complex arithmetic. A new approach to carrying out'
write(io,'(a)')'   this operation is described by Moler and Morrison [10]. It is a'
write(io,'(a)')'   cubically convergent algorithm which starts with a and b ,'
write(io,'(a)')'   rather than with their squares, and thereby avoids destructive'
write(io,'(a)')'   arithmetic underflows and overflows. In MAT88, the algorithm is'
write(io,'(a)')'   used for complex modulus, Euclidean vector norm, plane rotations,'
write(io,'(a)')'   and the shift calculation in the eigenvalue and singular value'
write(io,'(a)')'   iterations.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'12. FLOP AND CHOP'
write(io,'(a)')''
write(io,'(a)')'   Detailed information about the amount of work involved in'
write(io,'(a)')'   matrix calculations and the resulting accuracy is provided by'
write(io,'(a)')'   FLOP and CHOP. The basic unit of work is the "flop", or floating'
write(io,'(a)')'   point operation. Roughly, one flop is one execution of a Fortran'
write(io,'(a)')'   statement like'
write(io,'(a)')''
write(io,'(a)')'         S = S + X(I)*Y(I)'
write(io,'(a)')''
write(io,'(a)')'   or'
write(io,'(a)')''
write(io,'(a)')'         Y(I) = Y(I) + T*X(I)'
write(io,'(a)')''
write(io,'(a)')'   In other words, it consists of one floating point multiplication,'
write(io,'(a)')'   together with one floating point addition and the associated'
write(io,'(a)')'   indexing and storage reference operations.'
write(io,'(a)')''
write(io,'(a)')'   MAT88 will print the number of flops required for a'
write(io,'(a)')'   particular statement when the statement is terminated by an extra'
write(io,'(a)')'   comma. For example, the line'
write(io,'(a)')''
write(io,'(a)')'         n = 20;  RAND(n)*RAND(n);,'
write(io,'(a)')''
write(io,'(a)')'   ends with an extra comma. Two 20 by 20 random matrices are'
write(io,'(a)')'   generated and multiplied together. The result is assigned to'
write(io,'(a)')'   ANS, but the semicolon suppresses its printing. The only output'
write(io,'(a)')'   is'
write(io,'(a)')''
write(io,'(a)')'           8800 flops'
write(io,'(a)')''
write(io,'(a)')'   This is n**3 + 2*n**2 flops, n**2 for each random matrix and'
write(io,'(a)')'   n**3 for the product.'
write(io,'(a)')''
write(io,'(a)')'   FLOP is a predefined vector with two components. FLOP(1) is'
write(io,'(a)')'   the number of flops used by the most recently executed statement,'
write(io,'(a)')'   except that statements with zero flops are ignored. For example,'
write(io,'(a)')'   after executing the previous statement,'
write(io,'(a)')''
write(io,'(a)')'         flop(1)/n**3'
write(io,'(a)')''
write(io,'(a)')'   results in'
write(io,'(a)')''
write(io,'(a)')'         ANS   ='
write(io,'(a)')''
write(io,'(a)')'             1.1000'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   FLOP(2) is the cumulative total of all the flops used since'
write(io,'(a)')'   the beginning of the MAT88 session. The statement'
write(io,'(a)')''
write(io,'(a)')'         FLOP = <0 0>'
write(io,'(a)')''
write(io,'(a)')'   resets the total.'
write(io,'(a)')''
write(io,'(a)')'   There are several difficulties associated with keeping a'
write(io,'(a)')'   precise count of floating point operations. An addition or'
write(io,'(a)')'   subtraction that is not paired with a multiplication is usually'
write(io,'(a)')'   counted as a flop. The same is true of an isolated multiplication'
write(io,'(a)')'   that is not paired with an addition. Each floating point'
write(io,'(a)')'   division counts as a flop. But the number of operations required'
write(io,'(a)')'   by system dependent library functions such as square root cannot'
write(io,'(a)')'   be counted, so most elementary functions are arbitrarily counted'
write(io,'(a)')'   as using only one flop.'
write(io,'(a)')''
write(io,'(a)')'   The biggest difficulty occurs with complex arithmetic.'
write(io,'(a)')'   Almost all operations on the real parts of matrices are counted.'
write(io,'(a)')'   However, the operations on the complex parts of matrices are'
write(io,'(a)')'   counted only when they involve nonzero elements. This means that'
write(io,'(a)')'   simple operations on nonreal matrices require only about twice as'
write(io,'(a)')'   many flops as the same operations on real matrices. This factor'
write(io,'(a)')'   of two is not necessarily an accurate measure of the relative'
write(io,'(a)')'   costs of real and complex arithmetic.'
write(io,'(a)')''
write(io,'(a)')'   The result of each floating point operation may also be'
write(io,'(a)')'   "chopped" to simulate a computer with a shorter word length. The'
write(io,'(a)')'   details of this chopping operation depend upon the format of the'
write(io,'(a)')'   floating point word. Usually, the fraction in the floating point'
write(io,'(a)')'   word can be regarded as consisting of several octal or'
write(io,'(a)')'   hexadecimal digits. The least significant of these digits can be'
write(io,'(a)')'   set to zero by a logical masking operation. Thus the statement'
write(io,'(a)')''
write(io,'(a)')'         CHOP(p)'
write(io,'(a)')''
write(io,'(a)')'   causes the p least significant octal or hexadecimal digits in'
write(io,'(a)')'   the result of each floating point operation to be set to zero.'
write(io,'(a)')'   For example, if the computer being used has an IBM 360 long'
write(io,'(a)')'   floating point word with 14 hexadecimal digits in the fraction,'
write(io,'(a)')'   then CHOP(8) results in simulation of a computer with only 6'
write(io,'(a)')'   hexadecimal digits in the fraction, i.e. a short floating point'
write(io,'(a)')'   word. On a computer such as the CDC 6600 with 16 octal digits,'
write(io,'(a)')'   CHOP(8) results in about the same accuracy because the remaining'
write(io,'(a)')'   8 octal digits represent the same number of bits as 6 hexadecimal'
write(io,'(a)')'   digits.'
write(io,'(a)')''
write(io,'(a)')'   Some idea of the effect of CHOP on any particular system can'
write(io,'(a)')'   be obtained by executing the following statements.'
write(io,'(a)')''
write(io,'(a)')'         long,   t = 1/10'
write(io,'(a)')'         long z, t = 1/10'
write(io,'(a)')'         chop(8)'
write(io,'(a)')'         long,   t = 1/10'
write(io,'(a)')'         long z, t = 1/10'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   The following Fortran subprograms illustrate more details of'
write(io,'(a)')'   FLOP and CHOP. The first subprogram is a simplified example of a'
write(io,'(a)')'   system-dependent function used within MAT88 itself. The common'
write(io,'(a)')'   variable FLP is essentially the first component of the variable'
write(io,'(a)')'   FLOP. The common variable CHP is initially zero, but it is set'
write(io,'(a)')'   to p by the statement CHOP(p). To shorten the DATA statement,'
write(io,'(a)')'   we assume there are only 6 hexadecimal digits. We also assume an'
write(io,'(a)')'   extension of Fortran that allows .AND. to be used as a binary'
write(io,'(a)')'   operation between two real variables.'
write(io,'(a)')''
write(io,'(a)')'         REAL FUNCTION FLOP(X)'
write(io,'(a)')'         REAL X'
write(io,'(a)')'         INTEGER FLP,CHP'
write(io,'(a)')'         COMMON FLP,CHP'
write(io,'(a)')'         REAL MASK(5)'
write(io,'(a)')'         DATA MASK/ZFFFFFFF0,ZFFFFFF00,ZFFFFF000,ZFFFF0000,ZFFF00000/'
write(io,'(a)')'         FLP = FLP + 1'
write(io,'(a)')'         IF (CHP .EQ. 0) FLOP = X'
write(io,'(a)')'         IF (CHP .GE. 1 .AND. CHP .LE. 5) FLOP = X .AND. MASK(CHP)'
write(io,'(a)')'         IF (CHP .GE. 6) FLOP = 0.0'
write(io,'(a)')'         END REAL FUNCTION FLOP'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   The following subroutine illustrates a typical use of the'
write(io,'(a)')'   previous function within MAT88. It is a simplified version of'
write(io,'(a)')'   the Basic Linear Algebra Subprogram that adds a scalar multiple'
write(io,'(a)')'   of one vector to another. We assume here that the vectors are'
write(io,'(a)')'   stored with a memory increment of one.'
write(io,'(a)')''
write(io,'(a)')'         SUBROUTINE SAXPY(N,TR,TI,XR,XI,YR,YI)'
write(io,'(a)')'         REAL TR,TI,XR(N),XI(N),YR(N),YI(N),FLOP'
write(io,'(a)')'         IF (N .LE. 0) RETURN'
write(io,'(a)')'         IF (TR .EQ. 0.0 .AND. TI .EQ. 0.0) RETURN'
write(io,'(a)')'         DO I = 1, N'
write(io,'(a)')'            YR(I) = FLOP(YR(I) + TR*XR(I) - TI*XI(I))'
write(io,'(a)')'            YI(I) = YI(I) + TR*XI(I) + TI*XR(I)'
write(io,'(a)')'            IF (YI(I) .NE. 0.0D0) YI(I) = FLOP(YI(I))'
write(io,'(a)')'         enddo'
write(io,'(a)')'         END SUBROUTINE SAXPY'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   The saxpy operation is perhaps the most fundamental'
write(io,'(a)')'   operation within LINPACK. It is used in the computation of the'
write(io,'(a)')'   LU, the QR and the SVD factorizations, and in several other'
write(io,'(a)')'   places. We see that adding a multiple of one vector with n'
write(io,'(a)')'   components to another uses n flops if the vectors are real and'
write(io,'(a)')'   between n and 2*n flops if the vectors have nonzero imaginary'
write(io,'(a)')'   components.'
write(io,'(a)')''
write(io,'(a)')'   The permanent MAT88 variable EPS is reset by the statement'
write(io,'(a)')'   CHOP(p). Its new value is usually the smallest inverse power of'
write(io,'(a)')'   two that satisfies the Fortran logical test'
write(io,'(a)')''
write(io,'(a)')'               FLOP(1.0+EPS) .GT. 1.0'
write(io,'(a)')''
write(io,'(a)')'   However, if EPS had been directly reset to a larger value, the'
write(io,'(a)')'   old value is retained.'
write(io,'(a)')''
write(io,'(a)')'13. COMMUNICATING WITH OTHER PROGRAMS'
write(io,'(a)')''
write(io,'(a)')'   There are four different ways MAT88 can be used in'
write(io,'(a)')'   conjunction with other programs:'
write(io,'(a)')''
write(io,'(a)')'         -- USER,'
write(io,'(a)')'         -- EXEC,'
write(io,'(a)')'         -- SAVE and LOAD,'
write(io,'(a)')'         -- MATZ, CALL and QUIT .'
write(io,'(a)')''
write(io,'(a)')'   Let us illustrate each of these by the following simple'
write(io,'(a)')'   example.'
write(io,'(a)')''
write(io,'(a)')'         n = 6'
write(io,'(a)')'         for i = 1:n, for j = 1:n, a(i,j) = abs(i-j);'
write(io,'(a)')'         A'
write(io,'(a)')'         X = inv(A)'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   The example A could be introduced into MAT88 by writing'
write(io,'(a)')'   the following Fortran subroutine.'
write(io,'(a)')''
write(io,'(a)')'            SUBROUTINE mat88_user(A,M,N,S,T)'
write(io,'(a)')'            DOUBLEPRECISION A(*),S,T'
write(io,'(a)')'            N = IDINT(A(1))'
write(io,'(a)')'            M = N'
write(io,'(a)')'            DO J = 1, N'
write(io,'(a)')'               DO I = 1, N'
write(io,'(a)')'                  K = I + (J-1)*M'
write(io,'(a)')'                  A(K) = IABS(I-J)'
write(io,'(a)')'               enddo'
write(io,'(a)')'            enddo'
write(io,'(a)')'            END SUBROUTINE USER'
write(io,'(a)')''
write(io,'(a)')'   This subroutine should be compiled and linked into MAT88 in'
write(io,'(a)')'   place of the original version of USER. Then the MAT88'
write(io,'(a)')'   statements'
write(io,'(a)')''
write(io,'(a)')'         n = 6'
write(io,'(a)')'         A = user(n)'
write(io,'(a)')'         X = inv(A)'
write(io,'(a)')''
write(io,'(a)')'   do the job.'
write(io,'(a)')''
write(io,'(a)')'   The example A could be generated by storing the following'
write(io,'(a)')'   text in a file named, say, EXAMPLE .'
write(io,'(a)')''
write(io,'(a)')'         for i = 1:n, for j = 1:n, a(i,j) = abs(i-j);'
write(io,'(a)')''
write(io,'(a)')'   Then the MAT88 statements'
write(io,'(a)')''
write(io,'(a)')'         n = 6'
write(io,'(a)')'         exec(''EXAMPLE'',0)'
write(io,'(a)')'         X = inv(A)'
write(io,'(a)')''
write(io,'(a)')'   have the desired effect. The 0 as the optional second parameter'
write(io,'(a)')'   of exec indicates that the text in the file should not be printed'
write(io,'(a)')'   on the terminal.'
write(io,'(a)')''
write(io,'(a)')'   The matrices A and X could also be stored in files. Two'
write(io,'(a)')'   separate main programs would be involved. The first is:'
write(io,'(a)')''
write(io,'(a)')'            PROGRAM MAINA'
write(io,'(a)')'            DOUBLEPRECISION A(10,10)'
write(io,'(a)')'            N = 6'
write(io,'(a)')'            DO J = 1, N'
write(io,'(a)')'               DO I = 1, N'
write(io,'(a)')'                  A(I,J) = IABS(I-J)'
write(io,'(a)')'               enddo'
write(io,'(a)')'            enddo'
write(io,'(a)')'            OPEN(UNIT=1,FILE=''A'')'
write(io,'(a)')'            WRITE(1,101) N,N'
write(io,'(a)')'        101 FORMAT(''A   '',2I4)'
write(io,'(a)')'            DO J = 1, N'
write(io,'(a)')'               WRITE(1,102) (A(I,J),I=1,N)'
write(io,'(a)')'            enddo'
write(io,'(a)')'        102 FORMAT(4Z18)'
write(io,'(a)')'            END PROGRAM MAINA'
write(io,'(a)')''
write(io,'(a)')'   The OPEN statement may take different forms on different systems.'
write(io,'(a)')'   It attaches Fortran logical unit number 1 to the file named A.'
write(io,'(a)')'   The FORMAT number 102 may also be system dependent. This'
write(io,'(a)')'   particular one is appropriate for hexadecimal computers with an 8'
write(io,'(a)')'   byte double precision floating point word. Check, or modify,'
write(io,'(a)')'   MAT88 subroutine SAVLOD.'
write(io,'(a)')''
write(io,'(a)')'   After this program is executed, enter MAT88 and give the'
write(io,'(a)')'   following statements:'
write(io,'(a)')''
write(io,'(a)')'         load(''A'')'
write(io,'(a)')'         X = inv(A)'
write(io,'(a)')'         save(''X'',X)'
write(io,'(a)')''
write(io,'(a)')'   If all goes according to plan, this will read the matrix A from'
write(io,'(a)')'   the file A, invert it, store the inverse in X and then write the'
write(io,'(a)')'   matrix X on the file X. The following program can then access X.'
write(io,'(a)')''
write(io,'(a)')'            PROGRAM MAINX'
write(io,'(a)')'            DOUBLEPRECISION X(10,10)'
write(io,'(a)')'            OPEN(UNIT=1,FILE=''X'')'
write(io,'(a)')'            REWIND 1'
write(io,'(a)')'            READ (1,101) ID,M,N'
write(io,'(a)')'        101 FORMAT(A4,2I4)'
write(io,'(a)')'            DO J = 1, N'
write(io,'(a)')'               READ(1,102) (X(I,J),I=1,M)'
write(io,'(a)')'            ENDDO'
write(io,'(a)')'        102 FORMAT(4Z18)'
write(io,'(a)')'            ...'
write(io,'(a)')'            ...'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'   The most elaborate mechanism involves using MAT88 as a subroutine'
write(io,'(a)')'   within another program. Communication with the MAT88 stack is'
write(io,'(a)')'   accomplished using subroutine MATZ which is distributed with MAT88,'
write(io,'(a)')'   but which is not used by MAT88 itself. The preamble of MATZ is:'
write(io,'(a)')''
write(io,'(a)')'         SUBROUTINE MATZ(A,LDA,M,N,IDA,JOB,IERR)'
write(io,'(a)')'         INTEGER LDA,M,N,IDA(1),JOB,IERR'
write(io,'(a)')'         DOUBLEPRECISION A(LDA,N)'
write(io,'(a)')''
write(io,'(a)')'         ! ACCESS MAT88 VARIABLE STACK'
write(io,'(a)')'         ! A IS AN M BY N MATRIX, STORED IN AN ARRAY WITH'
write(io,'(a)')'         !     LEADING DIMENSION LDA.'
write(io,'(a)')'         ! IDA IS THE NAME OF A.'
write(io,'(a)')'         !     IF IDA IS AN INTEGER K LESS THAN 10, THEN THE NAME IS ''A''K'
write(io,'(a)')'         !     OTHERWISE, IDA(1:4) IS FOUR CHARACTERS, FORMAT 4A1.'
write(io,'(a)')'         ! JOB =  0  GET REAL A FROM MAT88,'
write(io,'(a)')'         !     =  1  PUT REAL A INTO MAT88,'
write(io,'(a)')'         !     = 10  GET IMAG PART OF A FROM MAT88,'
write(io,'(a)')'         !     = 11  PUT IMAG PART OF A INTO MAT88.'
write(io,'(a)')'         ! RETURN WITH NONZERO IERR AFTER MAT88 ERROR MESSAGE.'
write(io,'(a)')'         !'
write(io,'(a)')'         ! USES MAT88 ROUTINES STACKG, STACKP AND ERROR'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'        The preamble of subroutine MAT88 is:'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'         SUBROUTINE MAT88(INIT)'
write(io,'(a)')'         ! INIT = 0 FOR FIRST ENTRY, NONZERO FOR SUBSEQUENT ENTRIES'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'        To do our example, write the following program:'
write(io,'(a)')''
write(io,'(a)')'            DOUBLEPRECISION A(10,10),X(10,10)'
write(io,'(a)')'            INTEGER IDA(4),IDX(4)'
write(io,'(a)')'            DATA LDA/10/'
write(io,'(a)')'            DATA IDA/''A'','' '','' '','' ''/, IDX/''X'','' '','' '','' ''/'
write(io,'(a)')'            CALL M_88(0,'''')'
write(io,'(a)')'            N = 6'
write(io,'(a)')'            DO J = 1, N'
write(io,'(a)')'               DO I = 1, N'
write(io,'(a)')'                  A(I,J) = IABS(I-J)'
write(io,'(a)')'               enddo'
write(io,'(a)')'            enddo'
write(io,'(a)')'            CALL MATZ(A,LDA,N,N,IDA,1,IERR)'
write(io,'(a)')'            IF (IERR .NE. 0) GO TO ...'
write(io,'(a)')'            CALL MAT88(1,'''')'
write(io,'(a)')'            CALL MATZ(X,LDA,N,N,IDX,0,IERR)'
write(io,'(a)')'            IF (IERR .NE. 0) GO TO ...'
write(io,'(a)')'            ...'
write(io,'(a)')'            ...'
write(io,'(a)')''
write(io,'(a)')'   When this program is executed, the call to MAT88(0) produces the'
write(io,'(a)')'   MAT88 greeting, then waits for input. The command'
write(io,'(a)')''
write(io,'(a)')'            quit'
write(io,'(a)')''
write(io,'(a)')'   sends control back to our example program. The matrix A is'
write(io,'(a)')'   generated by the program and sent to the stack by the first call'
write(io,'(a)')'   to MATZ. The call to MAT88(1) produces the MAT88(1) prompt. Then'
write(io,'(a)')'   the statements'
write(io,'(a)')''
write(io,'(a)')'            X = inv(A)'
write(io,'(a)')'            quit'
write(io,'(a)')''
write(io,'(a)')'   will invert our matrix, put the result on the stack and go back'
write(io,'(a)')'   to our program. The second call to MATZ will retrieve X .'
write(io,'(a)')''
write(io,'(a)')'   By the way, this matrix X is interesting. Take a look at'
write(io,'(a)')'   round(2*(n-1)*X).'
write(io,'(a)')''
write(io,'(a)')'ACKNOWLEDGEMENT'
write(io,'(a)')''
write(io,'(a)')'   Most of the work on MAT88 has been carried out at the University'
write(io,'(a)')'   of New Mexico, where it is being supported by the National Science'
write(io,'(a)')'   Foundation. Additional work has been done during visits to Stanford'
write(io,'(a)')'   Linear Accelerator Center, Argonne National Laboratory and Los Alamos'
write(io,'(a)')'   Scientific Laboratory, where support has been provided by NSF and the'
write(io,'(a)')'   Department of Energy.'
write(io,'(a)')''
write(io,'(a)')'REFERENCES'
write(io,'(a)')''
write(io,'(a)')' [1]  J. J. Dongarra, J. R. Bunch, C. B. Moler and G. W. Stewart,'
write(io,'(a)')'      LINPACK Users'' Guide, Society for Industrial and Applied'
write(io,'(a)')'      Mathematics, Philadelphia, 1979.'
write(io,'(a)')''
write(io,'(a)')' [2]  B. T. Smith, J. M. Boyle, J. J. Dongarra, B. S. Garbow, Y.'
write(io,'(a)')'      Ikebe, V. C. Klema, C. B. Moler, Matrix Eigensystem Routines'
write(io,'(a)')'      -- EISPACK Guide, Lecture Notes in Computer Science, volume'
write(io,'(a)')'      6, second edition, Springer-Verlag, 1976.'
write(io,'(a)')''
write(io,'(a)')' [3]  B. S. Garbow, J. M. Boyle, J. J. Dongarra, C. B. Moler,'
write(io,'(a)')'      Matrix Eigensystem Routines -- EISPACK Guide Extension,'
write(io,'(a)')'      Lecture Notes in Computer Science, volume 51, Springer-'
write(io,'(a)')'      Verlag, 1977.'
write(io,'(a)')''
write(io,'(a)')' [4]  S. Cohen and S. Piper, SPEAKEASY III Reference Manual,'
write(io,'(a)')'      Speakeasy Computing Corp., Chicago, Ill., 1979.'
write(io,'(a)')''
write(io,'(a)')' [5]  J. H. Wilkinson and C. Reinsch, Handbook for Automatic'
write(io,'(a)')'      Computation, volume II, Linear Algebra, Springer-Verlag,'
write(io,'(a)')'     1971.'
write(io,'(a)')''
write(io,'(a)')' [6]  Niklaus Wirth, Algorithms + Data Structures = Programs,'
write(io,'(a)')'      Prentice-Hall, 1976.'
write(io,'(a)')''
write(io,'(a)')' [7]  H. B. Keller and D. Sachs, "Calculations of the Conductivity'
write(io,'(a)')'      of a Medium Containing Cylindrical Inclusions", J. Applied'
write(io,'(a)')'      Physics 35, 537-538, 1964.'
write(io,'(a)')''
write(io,'(a)')' [8]  C. B. Moler and C. F. Van Loan, Nineteen Dubious Ways to'
write(io,'(a)')'      Compute the Exponential of a Matrix, SIAM Review 20, 801-'
write(io,'(a)')'      836, 1979.'
write(io,'(a)')''
write(io,'(a)')' [9]  G. E. Forsythe, M. A. Malcolm and C. B. Moler, Computer'
write(io,'(a)')'      Methods for Mathematical Computations, Prentice-Hall, 1977.'
write(io,'(a)')''
write(io,'(a)')' [10] C. B. Moler and D. R. Morrison, "Replacing square roots by'
write(io,'(a)')'      Pythagorean sums", University of New Mexico, Computer'
write(io,'(a)')'      Science Department, technical report, submitted for'
write(io,'(a)')'     publication, 1980.'
write(io,'(a)')''
write(io,'(a)')'APPENDIX'
end subroutine mat_make_manual
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_matrix()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_mat88()
   call test_mat_appnum()
   call test_mat_base()
   call test_mat_buf2str()
   call test_mat_clause()
   call test_mat_comand()
   call test_mat_eqid()
   call test_mat_err()
   call test_mat_expr()
   call test_mat_factor()
   call test_mat_files()
   call test_mat_flop()
   call test_mat_formz()
   call test_mat_funs()
   call test_mat_getch()
   call test_mat_getlin()
   call test_mat_getsym()
   call test_mat_getval()
   call test_mat_hilber()
   call test_mat_iwamax()
   call test_mat_magic()
   call test_mat_make_help()
   call test_mat_make_manual()
   call test_mat_matfn1()
   call test_mat_matfn2()
   call test_mat_matfn3()
   call test_mat_matfn4()
   call test_mat_matfn5()
   call test_mat_matfn6()
   call test_mat_parse()
   call test_mat_plot()
   call test_mat_print()
   call test_mat_prntid()
   call test_mat_prompt()
   call test_mat_putid()
   call test_mat_pythag()
   call test_mat_rat()
   call test_mat_round()
   call test_mat_rref()
   call test_mat_rrot()
   call test_mat_rrotg()
   call test_mat_rset()
   call test_mat_rswap()
   call test_mat_savlod()
   call test_mat_stack1()
   call test_mat_stack2()
   call test_mat_stackg()
   call test_mat_stackp()
   call test_mat_str2buf()
   call test_mat_term()
   call test_mat_urand()
   call test_mat_wasum()
   call test_mat_watan()
   call test_mat_wcopy()
   call test_mat_wdiv()
   call test_mat_wdotci()
   call test_mat_wdotcr()
   call test_mat_wdotui()
   call test_mat_wdotur()
   call test_mat_wlog()
   call test_mat_wmul()
   call test_mat_wnrm2()
   call test_mat_wpofa()
   call test_mat_wrscal()
   call test_mat_wscal()
   call test_mat_wset()
   call test_mat_wsign()
   call test_mat_wsqrt()
   call test_mat_wswap()
   call test_matx_waxpy()
   call test_ml_comqr3()
   call test_ml_corth()
   call test_ml_htribk()
   call test_ml_htridi()
   call test_ml_imtql2()
   call test_ml_wgeco()
   call test_ml_wgedi()
   call test_ml_wgefa()
   call test_ml_wgesl()
   call test_ml_wqrdc()
   call test_ml_wqrsl()
   call test_ml_wsvdc()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat88()

   call unit_check_start('mat88',msg='')
   !!call unit_check('mat88', 0.eq.0, 'checking', 100)
   call unit_check_done('mat88',msg='')
end subroutine test_mat88
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_appnum()

   call unit_check_start('mat_appnum',msg='')
   !!call unit_check('mat_appnum', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_appnum',msg='')
end subroutine test_mat_appnum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_base()

   call unit_check_start('mat_base',msg='')
   !!call unit_check('mat_base', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_base',msg='')
end subroutine test_mat_base
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_buf2str()

   call unit_check_start('mat_buf2str',msg='')
   !!call unit_check('mat_buf2str', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_buf2str',msg='')
end subroutine test_mat_buf2str
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_clause()

   call unit_check_start('mat_clause',msg='')
   !!call unit_check('mat_clause', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_clause',msg='')
end subroutine test_mat_clause
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_comand()

   call unit_check_start('mat_comand',msg='')
   !!call unit_check('mat_comand', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_comand',msg='')
end subroutine test_mat_comand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_eqid()

   call unit_check_start('mat_eqid',msg='')
   !!call unit_check('mat_eqid', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_eqid',msg='')
end subroutine test_mat_eqid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_err()

   call unit_check_start('mat_err',msg='')
   !!call unit_check('mat_err', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_err',msg='')
end subroutine test_mat_err
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_expr()

   call unit_check_start('mat_expr',msg='')
   !!call unit_check('mat_expr', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_expr',msg='')
end subroutine test_mat_expr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_factor()

   call unit_check_start('mat_factor',msg='')
   !!call unit_check('mat_factor', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_factor',msg='')
end subroutine test_mat_factor
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_files()

   call unit_check_start('mat_files',msg='')
   !!call unit_check('mat_files', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_files',msg='')
end subroutine test_mat_files
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_flop()

   call unit_check_start('mat_flop',msg='')
   !!call unit_check('mat_flop', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_flop',msg='')
end subroutine test_mat_flop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_formz()

   call unit_check_start('mat_formz',msg='')
   !!call unit_check('mat_formz', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_formz',msg='')
end subroutine test_mat_formz
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_funs()

   call unit_check_start('mat_funs',msg='')
   !!call unit_check('mat_funs', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_funs',msg='')
end subroutine test_mat_funs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_getch()

   call unit_check_start('mat_getch',msg='')
   !!call unit_check('mat_getch', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_getch',msg='')
end subroutine test_mat_getch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_getlin()

   call unit_check_start('mat_getlin',msg='')
   !!call unit_check('mat_getlin', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_getlin',msg='')
end subroutine test_mat_getlin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_getsym()

   call unit_check_start('mat_getsym',msg='')
   !!call unit_check('mat_getsym', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_getsym',msg='')
end subroutine test_mat_getsym
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_getval()

   call unit_check_start('mat_getval',msg='')
   !!call unit_check('mat_getval', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_getval',msg='')
end subroutine test_mat_getval
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_hilber()

   call unit_check_start('mat_hilber',msg='')
   !!call unit_check('mat_hilber', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_hilber',msg='')
end subroutine test_mat_hilber
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_iwamax()

   call unit_check_start('mat_iwamax',msg='')
   !!call unit_check('mat_iwamax', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_iwamax',msg='')
end subroutine test_mat_iwamax
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_magic()

   call unit_check_start('mat_magic',msg='')
   !!call unit_check('mat_magic', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_magic',msg='')
end subroutine test_mat_magic
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_make_help()

   call unit_check_start('mat_make_help',msg='')
   !!call unit_check('mat_make_help', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_make_help',msg='')
end subroutine test_mat_make_help
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_make_manual()

   call unit_check_start('mat_make_manual',msg='')
   !!call unit_check('mat_make_manual', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_make_manual',msg='')
end subroutine test_mat_make_manual
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_matfn1()

   call unit_check_start('mat_matfn1',msg='')
   !!call unit_check('mat_matfn1', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_matfn1',msg='')
end subroutine test_mat_matfn1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_matfn2()

   call unit_check_start('mat_matfn2',msg='')
   !!call unit_check('mat_matfn2', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_matfn2',msg='')
end subroutine test_mat_matfn2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_matfn3()

   call unit_check_start('mat_matfn3',msg='')
   !!call unit_check('mat_matfn3', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_matfn3',msg='')
end subroutine test_mat_matfn3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_matfn4()

   call unit_check_start('mat_matfn4',msg='')
   !!call unit_check('mat_matfn4', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_matfn4',msg='')
end subroutine test_mat_matfn4
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_matfn5()

   call unit_check_start('mat_matfn5',msg='')
   !!call unit_check('mat_matfn5', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_matfn5',msg='')
end subroutine test_mat_matfn5
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_matfn6()

   call unit_check_start('mat_matfn6',msg='')
   !!call unit_check('mat_matfn6', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_matfn6',msg='')
end subroutine test_mat_matfn6
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_parse()

   call unit_check_start('mat_parse',msg='')
   !!call unit_check('mat_parse', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_parse',msg='')
end subroutine test_mat_parse
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_plot()

   call unit_check_start('mat_plot',msg='')
   !!call unit_check('mat_plot', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_plot',msg='')
end subroutine test_mat_plot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_print()

   call unit_check_start('mat_print',msg='')
   !!call unit_check('mat_print', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_print',msg='')
end subroutine test_mat_print
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_prntid()

   call unit_check_start('mat_prntid',msg='')
   !!call unit_check('mat_prntid', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_prntid',msg='')
end subroutine test_mat_prntid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_prompt()

   call unit_check_start('mat_prompt',msg='')
   !!call unit_check('mat_prompt', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_prompt',msg='')
end subroutine test_mat_prompt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_putid()

   call unit_check_start('mat_putid',msg='')
   !!call unit_check('mat_putid', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_putid',msg='')
end subroutine test_mat_putid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_pythag()

   call unit_check_start('mat_pythag',msg='')
   !!call unit_check('mat_pythag', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_pythag',msg='')
end subroutine test_mat_pythag
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rat()

   call unit_check_start('mat_rat',msg='')
   !!call unit_check('mat_rat', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_rat',msg='')
end subroutine test_mat_rat
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_round()

   call unit_check_start('mat_round',msg='')
   !!call unit_check('mat_round', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_round',msg='')
end subroutine test_mat_round
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rref()

   call unit_check_start('mat_rref',msg='')
   !!call unit_check('mat_rref', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_rref',msg='')
end subroutine test_mat_rref
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rrot()

   call unit_check_start('mat_rrot',msg='')
   !!call unit_check('mat_rrot', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_rrot',msg='')
end subroutine test_mat_rrot
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rrotg()

   call unit_check_start('mat_rrotg',msg='')
   !!call unit_check('mat_rrotg', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_rrotg',msg='')
end subroutine test_mat_rrotg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rset()

   call unit_check_start('mat_rset',msg='')
   !!call unit_check('mat_rset', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_rset',msg='')
end subroutine test_mat_rset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_rswap()

   call unit_check_start('mat_rswap',msg='')
   !!call unit_check('mat_rswap', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_rswap',msg='')
end subroutine test_mat_rswap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_savlod()

   call unit_check_start('mat_savlod',msg='')
   !!call unit_check('mat_savlod', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_savlod',msg='')
end subroutine test_mat_savlod
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_stack1()

   call unit_check_start('mat_stack1',msg='')
   !!call unit_check('mat_stack1', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_stack1',msg='')
end subroutine test_mat_stack1
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_stack2()

   call unit_check_start('mat_stack2',msg='')
   !!call unit_check('mat_stack2', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_stack2',msg='')
end subroutine test_mat_stack2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_stackg()

   call unit_check_start('mat_stackg',msg='')
   !!call unit_check('mat_stackg', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_stackg',msg='')
end subroutine test_mat_stackg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_stackp()

   call unit_check_start('mat_stackp',msg='')
   !!call unit_check('mat_stackp', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_stackp',msg='')
end subroutine test_mat_stackp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_str2buf()

   call unit_check_start('mat_str2buf',msg='')
   !!call unit_check('mat_str2buf', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_str2buf',msg='')
end subroutine test_mat_str2buf
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_term()

   call unit_check_start('mat_term',msg='')
   !!call unit_check('mat_term', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_term',msg='')
end subroutine test_mat_term
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_urand()

   call unit_check_start('mat_urand',msg='')
   !!call unit_check('mat_urand', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_urand',msg='')
end subroutine test_mat_urand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wasum()

   call unit_check_start('mat_wasum',msg='')
   !!call unit_check('mat_wasum', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wasum',msg='')
end subroutine test_mat_wasum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_watan()

   call unit_check_start('mat_watan',msg='')
   !!call unit_check('mat_watan', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_watan',msg='')
end subroutine test_mat_watan
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wcopy()

   call unit_check_start('mat_wcopy',msg='')
   !!call unit_check('mat_wcopy', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wcopy',msg='')
end subroutine test_mat_wcopy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdiv()

   call unit_check_start('mat_wdiv',msg='')
   !!call unit_check('mat_wdiv', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wdiv',msg='')
end subroutine test_mat_wdiv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotci()

   call unit_check_start('mat_wdotci',msg='')
   !!call unit_check('mat_wdotci', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wdotci',msg='')
end subroutine test_mat_wdotci
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotcr()

   call unit_check_start('mat_wdotcr',msg='')
   !!call unit_check('mat_wdotcr', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wdotcr',msg='')
end subroutine test_mat_wdotcr
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotui()

   call unit_check_start('mat_wdotui',msg='')
   !!call unit_check('mat_wdotui', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wdotui',msg='')
end subroutine test_mat_wdotui
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wdotur()

   call unit_check_start('mat_wdotur',msg='')
   !!call unit_check('mat_wdotur', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wdotur',msg='')
end subroutine test_mat_wdotur
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wlog()

   call unit_check_start('mat_wlog',msg='')
   !!call unit_check('mat_wlog', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wlog',msg='')
end subroutine test_mat_wlog
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wmul()

   call unit_check_start('mat_wmul',msg='')
   !!call unit_check('mat_wmul', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wmul',msg='')
end subroutine test_mat_wmul
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wnrm2()

   call unit_check_start('mat_wnrm2',msg='')
   !!call unit_check('mat_wnrm2', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wnrm2',msg='')
end subroutine test_mat_wnrm2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wpofa()

   call unit_check_start('mat_wpofa',msg='')
   !!call unit_check('mat_wpofa', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wpofa',msg='')
end subroutine test_mat_wpofa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wrscal()

   call unit_check_start('mat_wrscal',msg='')
   !!call unit_check('mat_wrscal', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wrscal',msg='')
end subroutine test_mat_wrscal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wscal()

   call unit_check_start('mat_wscal',msg='')
   !!call unit_check('mat_wscal', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wscal',msg='')
end subroutine test_mat_wscal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wset()

   call unit_check_start('mat_wset',msg='')
   !!call unit_check('mat_wset', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wset',msg='')
end subroutine test_mat_wset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wsign()

   call unit_check_start('mat_wsign',msg='')
   !!call unit_check('mat_wsign', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wsign',msg='')
end subroutine test_mat_wsign
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wsqrt()

   call unit_check_start('mat_wsqrt',msg='')
   !!call unit_check('mat_wsqrt', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wsqrt',msg='')
end subroutine test_mat_wsqrt
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_mat_wswap()

   call unit_check_start('mat_wswap',msg='')
   !!call unit_check('mat_wswap', 0.eq.0, 'checking', 100)
   call unit_check_done('mat_wswap',msg='')
end subroutine test_mat_wswap
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_matx_waxpy()

   call unit_check_start('matx_waxpy',msg='')
   !!call unit_check('matx_waxpy', 0.eq.0, 'checking', 100)
   call unit_check_done('matx_waxpy',msg='')
end subroutine test_matx_waxpy
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_comqr3()

   call unit_check_start('ml_comqr3',msg='')
   !!call unit_check('ml_comqr3', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_comqr3',msg='')
end subroutine test_ml_comqr3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_corth()

   call unit_check_start('ml_corth',msg='')
   !!call unit_check('ml_corth', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_corth',msg='')
end subroutine test_ml_corth
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_htribk()

   call unit_check_start('ml_htribk',msg='')
   !!call unit_check('ml_htribk', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_htribk',msg='')
end subroutine test_ml_htribk
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_htridi()

   call unit_check_start('ml_htridi',msg='')
   !!call unit_check('ml_htridi', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_htridi',msg='')
end subroutine test_ml_htridi
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_imtql2()

   call unit_check_start('ml_imtql2',msg='')
   !!call unit_check('ml_imtql2', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_imtql2',msg='')
end subroutine test_ml_imtql2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgeco()

   call unit_check_start('ml_wgeco',msg='')
   !!call unit_check('ml_wgeco', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_wgeco',msg='')
end subroutine test_ml_wgeco
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgedi()

   call unit_check_start('ml_wgedi',msg='')
   !!call unit_check('ml_wgedi', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_wgedi',msg='')
end subroutine test_ml_wgedi
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgefa()

   call unit_check_start('ml_wgefa',msg='')
   !!call unit_check('ml_wgefa', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_wgefa',msg='')
end subroutine test_ml_wgefa
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wgesl()

   call unit_check_start('ml_wgesl',msg='')
   !!call unit_check('ml_wgesl', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_wgesl',msg='')
end subroutine test_ml_wgesl
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wqrdc()

   call unit_check_start('ml_wqrdc',msg='')
   !!call unit_check('ml_wqrdc', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_wqrdc',msg='')
end subroutine test_ml_wqrdc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wqrsl()

   call unit_check_start('ml_wqrsl',msg='')
   !!call unit_check('ml_wqrsl', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_wqrsl',msg='')
end subroutine test_ml_wqrsl
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ml_wsvdc()

   call unit_check_start('ml_wsvdc',msg='')
   !!call unit_check('ml_wsvdc', 0.eq.0, 'checking', 100)
   call unit_check_done('ml_wsvdc',msg='')
end subroutine test_ml_wsvdc
!===================================================================================================================================
end subroutine test_suite_M_matrix
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_matrix !! subroutine
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine matX_waxpy(N,SR,SI,xr,xi,INCX,yr,yi,INCY)
use M_matrix
implicit none
integer,intent(in)         :: n
doubleprecision,intent(in) :: sr
doubleprecision,intent(in) :: si
doubleprecision,intent(in) :: xr(*)
doubleprecision,intent(in) :: xi(*)
integer,intent(in)         :: incx
integer,intent(in)         :: incy

doubleprecision :: yr(*)
doubleprecision :: yi(*)
integer         :: ix, iy

integer         :: i

   if (n .le. 0) return
   if (sr .eq. 0.0d0 .and. si .eq. 0.0d0) return
   ix = 1
   iy = 1
   if (incx.lt.0) ix = (-n+1)*incx + 1
   if (incy.lt.0) iy = (-n+1)*incy + 1
   do i = 1, n
      yr(iy) = mat_flop(yr(iy) + sr*xr(ix) - si*xi(ix))
      yi(iy) = yi(iy) + sr*xi(ix) + si*xr(ix)
      if (yi(iy) .ne. 0.0d0) yi(iy) = mat_flop(yi(iy))
      ix = ix + incx
      iy = iy + incy
   enddo
end subroutine matX_waxpy
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
      SUBROUTINE ML_WGECO(AR,AI,LDA,N,IPVT,RCOND,ZR,ZI)
      use M_matrix
      INTEGER LDA,N,IPVT(*)
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*),ZR(*),ZI(*)
      DOUBLEPRECISION RCOND
!
!     WGECO FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION
!     AND ESTIMATES THE CONDITION OF THE MATRIX.
!
!     IF  RCOND  IS NOT NEEDED, WGEFA IS SLIGHTLY FASTER.
!     TO SOLVE  A*X = B , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  INVERSE(A)*C , FOLLOW WGECO BY WGESL.
!     TO COMPUTE  DETERMINANT(A) , FOLLOW WGECO BY WGEDI.
!     TO COMPUTE  INVERSE(A) , FOLLOW WGECO BY WGEDI.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        RCOND   DOUBLEPRECISION
!                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A .
!                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS
!                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE
!                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND .
!                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION
!        1.0 + RCOND .EQ. 1.0
!                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING
!                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF
!                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE
!                UNDERFLOWS.
!
!        Z       DOUBLE-COMPLEX(N)
!                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT.
!                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS
!                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT
!                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     LINPACK WGEFA
!     BLAS WAXPY,WDOTC,mat_wasum
!     FORTRAN DABS,DMAX1
!
!     INTERNAL VARIABLES
!
      DOUBLEPRECISION EKR,EKI,TR,TI,WKR,WKI,WKMR,WKMI
      DOUBLEPRECISION ANORM,S,SM,YNORM
      INTEGER INFO,J,K,KB,KP1,L
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE 1-NORM OF A
!
      ANORM = 0.0D0
      DO J = 1, N
         ANORM = DMAX1(ANORM,mat_wasum(N,AR(1,J),AI(1,J),1))
      enddo
!
!     FACTOR
!
      CALL ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
!
!     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) .
!     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  CTRANS(A)*Y = E .
!     CTRANS(A)  IS THE CONJUGATE TRANSPOSE OF A .
!     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL
!     GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(U)*W = E .
!     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW.
!
!     SOLVE CTRANS(U)*W = E
!
      EKR = 1.0D0
      EKI = 0.0D0
      DO J = 1, N
         ZR(J) = 0.0D0
         ZI(J) = 0.0D0
      enddo
      DO 110 K = 1, N
         CALL mat_wsign(EKR,EKI,-ZR(K),-ZI(K),EKR,EKI)
         IF (CABS1(EKR-ZR(K),EKI-ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 40
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(EKR-ZR(K),EKI-ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            EKR = S*EKR
            EKI = S*EKI
   40    CONTINUE
         WKR = EKR - ZR(K)
         WKI = EKI - ZI(K)
         WKMR = -EKR - ZR(K)
         WKMI = -EKI - ZI(K)
         S = CABS1(WKR,WKI)
         SM = CABS1(WKMR,WKMI)
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 50
            CALL mat_wdiv(WKR,WKI,AR(K,K),-AI(K,K),WKR,WKI)
            CALL mat_wdiv(WKMR,WKMI,AR(K,K),-AI(K,K),WKMR,WKMI)
         GOTO 60
   50    CONTINUE
            WKR = 1.0D0
            WKI = 0.0D0
            WKMR = 1.0D0
            WKMI = 0.0D0
   60    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GOTO 100
            DO J = KP1, N
               CALL mat_wmul(WKMR,WKMI,AR(K,J),-AI(K,J),TR,TI)
               SM = mat_flop(SM + CABS1(ZR(J)+TR,ZI(J)+TI))
               CALL matX_waxpy(1,WKR,WKI,[AR(K,J)],[-AI(K,J)],1,ZR(J),ZI(J),1)
               S = mat_flop(S + CABS1(ZR(J),ZI(J)))
            enddo
            IF (S .GE. SM) GOTO 90
               TR = WKMR - WKR
               TI = WKMI - WKI
               WKR = WKMR
               WKI = WKMI
               DO J = KP1, N
                  CALL matX_waxpy(1,TR,TI,[AR(K,J)],[-AI(K,J)],1,ZR(J),ZI(J),1)
               enddo
   90       CONTINUE
  100    CONTINUE
         ZR(K) = WKR
         ZI(K) = WKI
  110 CONTINUE
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
!
!     SOLVE CTRANS(L)*Y = W
!
      DO KB = 1, N
         K = N + 1 - KB
         IF (K .GE. N) GOTO 120
            ZR(K) = ZR(K) + mat_wdotcr(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
            ZI(K) = ZI(K) + mat_wdotci(N-K,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
  120    CONTINUE
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) GOTO 130
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
  130    CONTINUE
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
      enddo
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
!
      YNORM = 1.0D0
!
!     SOLVE L*V = Y
!
      DO K = 1, N
         L = IPVT(K)
         TR = ZR(L)
         TI = ZI(L)
         ZR(L) = ZR(K)
         ZI(L) = ZI(K)
         ZR(K) = TR
         ZI(K) = TI
         IF (K .LT. N) CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,ZR(K+1),ZI(K+1),1)
         IF (CABS1(ZR(K),ZI(K)) .LE. 1.0D0) cycle
            S = 1.0D0/CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            YNORM = S*YNORM
      enddo
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
!     SOLVE  U*Z = V
!
      DO KB = 1, N
         K = N + 1 - KB
         IF (CABS1(ZR(K),ZI(K)) .LE. CABS1(AR(K,K),AI(K,K))) GOTO 170
            S = CABS1(AR(K,K),AI(K,K)) / CABS1(ZR(K),ZI(K))
            CALL mat_wrscal(N,S,ZR,ZI,1)
            YNORM = S*YNORM
  170    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .EQ. 0.0D0) GOTO 180
            CALL mat_wdiv(ZR(K),ZI(K),AR(K,K),AI(K,K),ZR(K),ZI(K))
  180    CONTINUE
         IF (CABS1(AR(K,K),AI(K,K)) .NE. 0.0D0) GOTO 190
            ZR(K) = 1.0D0
            ZI(K) = 0.0D0
  190    CONTINUE
         TR = -ZR(K)
         TI = -ZI(K)
         CALL matX_waxpy(K-1,TR,TI,AR(1,K),AI(1,K),1,ZR(1),ZI(1),1)
      enddo
!     MAKE ZNORM = 1.0
      S = 1.0D0/mat_wasum(N,ZR,ZI,1)
      CALL mat_wrscal(N,S,ZR,ZI,1)
      YNORM = S*YNORM
!
      IF (ANORM .NE. 0.0D0) RCOND = YNORM/ANORM
      IF (ANORM .EQ. 0.0D0) RCOND = 0.0D0
      END SUBROUTINE ML_WGECO
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGEFA(AR,AI,LDA,N,IPVT,INFO)
      use M_matrix
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*)
!
!     WGEFA FACTORS A DOUBLE-COMPLEX MATRIX BY GAUSSIAN ELIMINATION.
!
!     WGEFA IS USUALLY CALLED BY WGECO, BUT IT CAN BE CALLED
!     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED.
!     (TIME FOR WGECO) = (1 + 9/N)*(TIME FOR WGEFA) .
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE MATRIX TO BE FACTORED.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!     ON RETURN
!
!        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS
!                WHICH WERE USED TO OBTAIN IT.
!                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE
!                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER
!                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR.
!
!        IPVT    INTEGER(N)
!                AN INTEGER VECTOR OF PIVOT INDICES.
!
!        INFO    INTEGER
!                = 0  NORMAL VALUE.
!                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
!  CONDITION FOR THIS SUBROUTINE, BUT IT DOES
!  INDICATE THAT WGESL OR WGEDI WILL DIVIDE BY ZERO
!  IF CALLED.  USE  RCOND  IN WGECO FOR A RELIABLE
!  INDICATION OF SINGULARITY.
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WSCAL,mat_iwamax
!     FORTRAN DABS
!
!     INTERNAL VARIABLES
!
      DOUBLEPRECISION TR,TI
      INTEGER J,K,KP1,L,NM1
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
!
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GOTO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
!
!        FIND L = PIVOT INDEX
!
         L = mat_iwamax(N-K+1,AR(K,K),AI(K,K),1) + K - 1
         IPVT(K) = L
!
!        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
!
         IF (CABS1(AR(L,K),AI(L,K)) .EQ. 0.0D0) GOTO 40
!
!           INTERCHANGE IF NECESSARY
!
            IF (L .EQ. K) GOTO 10
               TR = AR(L,K)
               TI = AI(L,K)
               AR(L,K) = AR(K,K)
               AI(L,K) = AI(K,K)
               AR(K,K) = TR
               AI(K,K) = TI
   10       CONTINUE
!
!           COMPUTE MULTIPLIERS
!
            CALL mat_wdiv(-1.0D0,0.0D0,AR(K,K),AI(K,K),TR,TI)
            CALL mat_wscal(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1)
!
!           ROW ELIMINATION WITH COLUMN INDEXING
!
            DO J = KP1, N
               TR = AR(L,J)
               TI = AI(L,J)
               IF (L .EQ. K) GOTO 20
                  AR(L,J) = AR(K,J)
                  AI(L,J) = AI(K,J)
                  AR(K,J) = TR
                  AI(K,J) = TI
   20          CONTINUE
               CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,AR(K+1,J),AI(K+1,J),1)
            enddo
         GOTO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (CABS1(AR(N,N),AI(N,N)) .EQ. 0.0D0) INFO = N
      END SUBROUTINE ML_WGEFA
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGESL(AR,AI,LDA,N,IPVT,BR,BI,JOB)
use M_matrix
INTEGER LDA,N,IPVT(*),JOB
DOUBLEPRECISION AR(LDA,*),AI(LDA,*),BR(*),BI(*)
!
!     WGESL SOLVES THE DOUBLE-COMPLEX SYSTEM
!     A * X = B  OR  CTRANS(A) * X = B
!     USING THE FACTORS COMPUTED BY WGECO OR WGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE OUTPUT FROM WGECO OR WGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM WGECO OR WGEFA.
!
!        B       DOUBLE-COMPLEX(N)
!                THE RIGHT HAND SIDE VECTOR.
!
!        JOB     INTEGER
!                = 0         TO SOLVE  A*X = B ,
!                = NONZERO   TO SOLVE  CTRANS(A)*X = B  WHERE
!         CTRANS(A)  IS THE CONJUGATE TRANSPOSE.
!
!     ON RETURN
!
!        B       THE SOLUTION VECTOR  X .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
!        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
!        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
!        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE
!        CALLED CORRECTLY AND IF WGECO HAS SET RCOND .GT. 0.0
!        OR WGEFA HAS SET INFO .EQ. 0 .
!
!     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX
!     WITH  P  COLUMNS
!           CALL ML_WGECO(A,LDA,N,IPVT,RCOND,Z)
!           IF (RCOND IS TOO SMALL) GOTO ...
!           DO J = 1, P
!              CALL ML_WGESL(A,LDA,N,IPVT,C(1,J),0)
!           enddo
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WDOTC
!
!     INTERNAL VARIABLES
!
DOUBLEPRECISION TR,TI
INTEGER K,KB,L,NM1
!
   NM1 = N - 1
   IF (JOB .NE. 0) GOTO 50
!
!        JOB = 0 , SOLVE  A * X = B
!        FIRST SOLVE  L*Y = B
!
   IF (NM1 .GT. 1) then
      DO K = 1, NM1
         L = IPVT(K)
         TR = BR(L)
         TI = BI(L)
         IF (L .NE. K) then
            BR(L) = BR(K)
            BI(L) = BI(K)
            BR(K) = TR
            BI(K) = TI
         endif
         CALL matX_waxpy(N-K,TR,TI,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
      enddo
   endif
!
!        NOW SOLVE  U*X = Y
!
   DO KB = 1, N
      K = N + 1 - KB
      CALL mat_wdiv(BR(K),BI(K),AR(K,K),AI(K,K),BR(K),BI(K))
      TR = -BR(K)
      TI = -BI(K)
      CALL matX_waxpy(K-1,TR,TI,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
   enddo
   GOTO 100
50 CONTINUE
!
!  JOB = NONZERO, SOLVE  CTRANS(A) * X = B
!  FIRST SOLVE  CTRANS(U)*Y = B
!
   DO K = 1, N
      TR = BR(K) - mat_wdotcr(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
      TI = BI(K) - mat_wdotci(K-1,AR(1,K),AI(1,K),1,BR(1),BI(1),1)
      CALL mat_wdiv(TR,TI,AR(K,K),-AI(K,K),BR(K),BI(K))
   enddo
!
!        NOW SOLVE CTRANS(L)*X = Y
!
   IF (NM1 .GE. 1) then
      DO KB = 1, NM1
         K = N - KB
         BR(K) = BR(K) + mat_wdotcr(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
         BI(K) = BI(K) + mat_wdotci(N-K,AR(K+1,K),AI(K+1,K),1,BR(K+1),BI(K+1),1)
         L = IPVT(K)
         IF (L .EQ. K) cycle
         TR = BR(L)
         TI = BI(L)
         BR(L) = BR(K)
         BI(L) = BI(K)
         BR(K) = TR
         BI(K) = TI
      enddo
   endif
100 continue
end subroutine ml_wgesl
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WGEDI(ar,ai,LDA,N,ipvt,detr,deti,workr,worki,JOB)
      use M_matrix
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLEPRECISION AR(LDA,*),AI(LDA,*),DETR(2),DETI(2),WORKR(*),WORKI(*)
!
!     WGEDI COMPUTES THE DETERMINANT AND INVERSE OF A MATRIX
!     USING THE FACTORS COMPUTED BY WGECO OR WGEFA.
!
!     ON ENTRY
!
!        A       DOUBLE-COMPLEX(LDA, N)
!                THE OUTPUT FROM WGECO OR WGEFA.
!
!        LDA     INTEGER
!                THE LEADING DIMENSION OF THE ARRAY  A .
!
!        N       INTEGER
!                THE ORDER OF THE MATRIX  A .
!
!        IPVT    INTEGER(N)
!                THE PIVOT VECTOR FROM WGECO OR WGEFA.
!
!        WORK    DOUBLE-COMPLEX(N)
!                WORK VECTOR.  CONTENTS DESTROYED.
!
!        JOB     INTEGER
!                = 11   BOTH DETERMINANT AND INVERSE.
!                = 01   INVERSE ONLY.
!                = 10   DETERMINANT ONLY.
!
!     ON RETURN
!
!        A       INVERSE OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE UNCHANGED.
!
!        DET     DOUBLE-COMPLEX(2)
!                DETERMINANT OF ORIGINAL MATRIX IF REQUESTED.
!                OTHERWISE NOT REFERENCED.
!                DETERMINANT = DET(1) * 10.0**DET(2)
!                WITH  1.0 .LE. CABS1(DET(1) .LT. 10.0
!                OR  DET(1) .EQ. 0.0 .
!
!     ERROR CONDITION
!
!        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS
!        A ZERO ON THE DIAGONAL AND THE INVERSE IS REQUESTED.
!        IT WILL NOT OCCUR IF THE SUBROUTINES ARE CALLED CORRECTLY
!        AND IF WGECO HAS SET RCOND .GT. 0.0 OR WGEFA HAS SET
!        INFO .EQ. 0 .
!
!     LINPACK. THIS VERSION DATED 07/01/79 .
!     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
!
!     SUBROUTINES AND FUNCTIONS
!
!     BLAS WAXPY,WSCAL,WSWAP
!     FORTRAN DABS,MOD
!
!     INTERNAL VARIABLES
!
      DOUBLEPRECISION TR,TI
      DOUBLEPRECISION TEN
      INTEGER I,J,K,KB,KP1,L,NM1
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     COMPUTE DETERMINANT
!
      IF (JOB/10 .EQ. 0) GOTO 80
         DETR(1) = 1.0D0
         DETI(1) = 0.0D0
         DETR(2) = 0.0D0
         DETI(2) = 0.0D0
         TEN = 10.0D0
         DO 60 I = 1, N
           IF (IPVT(I) .EQ. I) GOTO 10
              DETR(1) = -DETR(1)
              DETI(1) = -DETI(1)
   10      CONTINUE
           CALL mat_wmul(AR(I,I),AI(I,I),DETR(1),DETI(1),DETR(1),DETI(1))
!          ...EXIT
!       ...EXIT
           IF (CABS1(DETR(1),DETI(1)) .EQ. 0.0D0) GOTO 70
   20      IF (CABS1(DETR(1),DETI(1)) .GE. 1.0D0) GOTO 30
              DETR(1) = TEN*DETR(1)
              DETI(1) = TEN*DETI(1)
              DETR(2) = DETR(2) - 1.0D0
              DETI(2) = DETI(2) - 0.0D0
           GOTO 20
   30      CONTINUE
   40      IF (CABS1(DETR(1),DETI(1)) .LT. TEN) GOTO 50
              DETR(1) = DETR(1)/TEN
              DETI(1) = DETI(1)/TEN
              DETR(2) = DETR(2) + 1.0D0
              DETI(2) = DETI(2) + 0.0D0
           GOTO 40
   50      CONTINUE
   60    CONTINUE
   70    CONTINUE
   80 CONTINUE
!
!     COMPUTE INVERSE(U)
!
      IF (MOD(JOB,10) .EQ. 0) GOTO 160
         DO K = 1, N
            CALL mat_wdiv(1.0D0,0.0D0,AR(K,K),AI(K,K),AR(K,K),AI(K,K))
            TR = -AR(K,K)
            TI = -AI(K,K)
            CALL mat_wscal(K-1,TR,TI,AR(1,K),AI(1,K),1)
            KP1 = K + 1
            IF (N .LT. KP1) cycle
            DO J = KP1, N
              TR = AR(K,J)
              TI = AI(K,J)
              AR(K,J) = 0.0D0
              AI(K,J) = 0.0D0
              CALL matX_waxpy(K,TR,TI,AR(1,K),AI(1,K),1,AR(1,J),AI(1,J),1)
            enddo
         enddo
!
!        FORM INVERSE(U)*INVERSE(L)
!
         NM1 = N - 1
         IF (NM1 .LT. 1) GOTO 150
         DO KB = 1, NM1
            K = N - KB
            KP1 = K + 1
            DO I = KP1, N
               WORKR(I) = AR(I,K)
               WORKI(I) = AI(I,K)
               AR(I,K) = 0.0D0
               AI(I,K) = 0.0D0
            enddo
            DO J = KP1, N
              TR = WORKR(J)
              TI = WORKI(J)
              CALL matX_waxpy(N,TR,TI,AR(1,J),AI(1,J),1,AR(1,K),AI(1,K),1)
            enddo
            L = IPVT(K)
            IF (L .NE. K)CALL mat_wswap(N,AR(1,K),AI(1,K),1,AR(1,L),AI(1,L),1)
         enddo
  150    CONTINUE
  160 CONTINUE
      END SUBROUTINE ML_WGEDI
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_HTRIDI(NM,N,AR,AI,D,E,E2,TAU)
      use M_matrix
!
      INTEGER I,J,K,L,N,II,NM,JP1
      DOUBLEPRECISION AR(NM,N),AI(NM,N),D(N),E(N),E2(N),TAU(2,N)
      DOUBLEPRECISION F,G,H,FI,GI,HH,SI,SCALE
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRED1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE REDUCES A COMPLEX HERMITIAN MATRIX
!     TO A REAL SYMMETRIC TRIDIAGONAL MATRIX USING
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX HERMITIAN INPUT MATRIX.
!          ONLY THE LOWER TRIANGLE OF THE MATRIX NEED BE SUPPLIED.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION IN THEIR FULL LOWER
!          TRIANGLES.  THEIR STRICT UPPER TRIANGLES AND THE
!          DIAGONAL OF AR ARE UNALTERED.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE THE TRIDIAGONAL MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE TRIDIAGONAL
!          MATRIX IN ITS LAST N-1 POSITIONS.  E(1) IS SET TO ZERO.
!
!        E2 CONTAINS THE SQUARES OF THE CORRESPONDING ELEMENTS OF E.
!          E2 MAY COINCIDE WITH E IF THE SQUARES ARE NOT NEEDED.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
      TAU(1,N) = 1.0D0
      TAU(2,N) = 0.0D0
!
      DO I = 1, N
         D(I) = AR(I,I)
      enddo
!     .......... FOR I=N STEP -1 UNTIL 1 DO -- ..........
      DO 300 II = 1, N
         I = N + 1 - II
         L = I - 1
         H = 0.0D0
         SCALE = 0.0D0
         IF (L .LT. 1) GOTO 130
!     .......... SCALE ROW (ALGOL TOL THEN NOT NEEDED) ..........
         DO K = 1, L
            SCALE = mat_flop(SCALE + DABS(AR(I,K)) + DABS(AI(I,K)))
         enddo
!
         IF (SCALE .NE. 0.0D0) GOTO 140
         TAU(1,L) = 1.0D0
         TAU(2,L) = 0.0D0
  130    E(I) = 0.0D0
         E2(I) = 0.0D0
         GOTO 290
!
  140    continue
         DO K = 1, L
            AR(I,K) = mat_flop(AR(I,K)/SCALE)
            AI(I,K) = mat_flop(AI(I,K)/SCALE)
            H = mat_flop(H + AR(I,K)*AR(I,K) + AI(I,K)*AI(I,K))
         enddo
!
         E2(I) = mat_flop(SCALE*SCALE*H)
         G = mat_flop(DSQRT(H))
         E(I) = mat_flop(SCALE*G)
         F = mat_pythag(AR(I,L),AI(I,L))
!     .......... FORM NEXT DIAGONAL ELEMENT OF MATRIX T ..........
         IF (F .EQ. 0.0D0) GOTO 160
         TAU(1,L) = mat_flop((AI(I,L)*TAU(2,I) - AR(I,L)*TAU(1,I))/F)
         SI = mat_flop((AR(I,L)*TAU(2,I) + AI(I,L)*TAU(1,I))/F)
         H = mat_flop(H + F*G)
         G = mat_flop(1.0D0 + G/F)
         AR(I,L) = mat_flop(G*AR(I,L))
         AI(I,L) = mat_flop(G*AI(I,L))
         IF (L .EQ. 1) GOTO 270
         GOTO 170
  160    TAU(1,L) = -TAU(1,I)
         SI = TAU(2,I)
         AR(I,L) = G
  170    F = 0.0D0
!
         DO J = 1, L
            G = 0.0D0
            GI = 0.0D0
!     .......... FORM ELEMENT OF A*U ..........
            DO K = 1, J
               G = mat_flop(G + AR(J,K)*AR(I,K) + AI(J,K)*AI(I,K))
               GI = mat_flop(GI - AR(J,K)*AI(I,K) + AI(J,K)*AR(I,K))
            enddo
!
            JP1 = J + 1
            IF (L .LT. JP1) GOTO 220
!
            DO K = JP1, L
               G = mat_flop(G + AR(K,J)*AR(I,K) - AI(K,J)*AI(I,K))
               GI = mat_flop(GI - AR(K,J)*AI(I,K) - AI(K,J)*AR(I,K))
            enddo
!     .......... FORM ELEMENT OF P ..........
  220       continue
            E(J) = mat_flop(G/H)
            TAU(2,J) = mat_flop(GI/H)
            F = mat_flop(F + E(J)*AR(I,J) - TAU(2,J)*AI(I,J))
         enddo
!
         HH = mat_flop(F/(H + H))
!     .......... FORM REDUCED A ..........
         DO J = 1, L
            F = AR(I,J)
            G = mat_flop(E(J) - HH*F)
            E(J) = G
            FI = -AI(I,J)
            GI = mat_flop(TAU(2,J) - HH*FI)
            TAU(2,J) = -GI
!
            DO K = 1, J
               AR(J,K) = mat_flop(AR(J,K) - F*E(K) - G*AR(I,K) + FI*TAU(2,K) + GI*AI(I,K))
               AI(J,K) = mat_flop(AI(J,K) - F*TAU(2,K) - G*AI(I,K) - FI*E(K) - GI*AR(I,K))
            enddo
         enddo
!
  270    continue
         DO K = 1, L
            AR(I,K) = mat_flop(SCALE*AR(I,K))
            AI(I,K) = mat_flop(SCALE*AI(I,K))
         enddo
!
         TAU(2,L) = -SI
  290    HH = D(I)
         D(I) = AR(I,I)
         AR(I,I) = HH
         AI(I,I) = mat_flop(SCALE*DSQRT(H))
  300 CONTINUE
!
END SUBROUTINE ML_HTRIDI
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_HTRIBK(NM,N,AR,AI,TAU,M,ZR,ZI)
use M_matrix
!
INTEGER I,J,K,L,M,N,NM
DOUBLEPRECISION AR(NM,N),AI(NM,N),TAU(2,N),ZR(NM,M),ZI(NM,M)
DOUBLEPRECISION H,S,SI
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE TRBAK1, NUM. MATH. 11, 181-195(1968)
!     BY MARTIN, REINSCH, AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 212-226(1971).
!
!     THIS SUBROUTINE FORMS THE EIGENVECTORS OF A COMPLEX HERMITIAN
!     MATRIX BY BACK TRANSFORMING THOSE OF THE CORRESPONDING
!     REAL SYMMETRIC TRIDIAGONAL MATRIX DETERMINED BY  HTRIDI.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        AR AND AI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  HTRIDI  IN THEIR
!          FULL LOWER TRIANGLES EXCEPT FOR THE DIAGONAL OF AR.
!
!        TAU CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
!
!        M IS THE NUMBER OF EIGENVECTORS TO BE BACK TRANSFORMED.
!
!        ZR CONTAINS THE EIGENVECTORS TO BE BACK TRANSFORMED
!          IN ITS FIRST M COLUMNS.
!
!     ON OUTPUT.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE TRANSFORMED EIGENVECTORS
!          IN THEIR FIRST M COLUMNS.
!
!     NOTE THAT THE LAST COMPONENT OF EACH RETURNED VECTOR
!     IS REAL AND THAT VECTOR EUCLIDEAN NORMS ARE PRESERVED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
   IF (M .EQ. 0) GOTO 200
!     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC
!                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN
!                TRIDIAGONAL MATRIX. ..........
   DO K = 1, N
      DO J = 1, M
         ZI(K,J) = mat_flop(-(ZR(K,J)*TAU(2,K)))
         ZR(K,J) = mat_flop(ZR(K,J)*TAU(1,K))
      enddo
   enddo
!
   IF (N .EQ. 1) GOTO 200
!     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES ..........
   DO I = 2, N
      L = I - 1
      H = AI(I,I)
      IF (H .EQ. 0.0D0) exit
      DO J = 1, M
         S = 0.0D0
         SI = 0.0D0
         DO K = 1, L
            S = mat_flop(S + AR(I,K)*ZR(K,J) - AI(I,K)*ZI(K,J))
            SI = mat_flop(SI + AR(I,K)*ZI(K,J) + AI(I,K)*ZR(K,J))
         enddo
!     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW ..........
         S = mat_flop((S/H)/H)
         SI = mat_flop((SI/H)/H)
         DO K = 1, L
            ZR(K,J) = mat_flop(ZR(K,J) - S*AR(I,K) - SI*AI(I,K))
            ZI(K,J) = mat_flop(ZI(K,J) - SI*AR(I,K) + S*AI(I,K))
         enddo
      enddo
   enddo
!
200 continue
END SUBROUTINE ML_HTRIBK
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_IMTQL2(NM,N,D,E,Z,IERR,JOB)
      use M_matrix
      IMPLICIT NONE
      INTEGER I,J,K,L,M,N,II,NM,MML,IERR
      integer :: job
      DOUBLEPRECISION D(N),E(N),Z(NM,N)
      DOUBLEPRECISION B,C,F,G,P,R,S
!
!     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE IMTQL2,
!     NUM. MATH. 12, 377-383(1968) BY MARTIN AND WILKINSON,
!     AS MODIFIED IN NUM. MATH. 15, 450(1970) BY DUBRULLE.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 241-248(1971).
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A SYMMETRIC TRIDIAGONAL MATRIX BY THE IMPLICIT QL METHOD.
!     THE EIGENVECTORS OF A FULL SYMMETRIC MATRIX CAN ALSO
!     BE FOUND IF  TRED2  HAS BEEN USED TO REDUCE THIS
!     FULL MATRIX TO TRIDIAGONAL FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        D CONTAINS THE DIAGONAL ELEMENTS OF THE INPUT MATRIX.
!
!        E CONTAINS THE SUBDIAGONAL ELEMENTS OF THE INPUT MATRIX
!          IN ITS LAST N-1 POSITIONS.  E(1) IS ARBITRARY.
!
!        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
!          REDUCTION BY  TRED2, IF PERFORMED.  IF THE EIGENVECTORS
!          OF THE TRIDIAGONAL MATRIX ARE DESIRED, Z MUST CONTAIN
!          THE IDENTITY MATRIX.
!
!      ON OUTPUT.
!
!        D CONTAINS THE EIGENVALUES IN ASCENDING ORDER.  IF AN
!          ERROR EXIT IS MADE, THE EIGENVALUES ARE CORRECT BUT
!          UNORDERED FOR INDICES 1,2,...,IERR-1.
!
!        E HAS BEEN DESTROYED.
!
!        Z CONTAINS ORTHONORMAL EIGENVECTORS OF THE SYMMETRIC
!          TRIDIAGONAL (OR FULL) MATRIX.  IF AN ERROR EXIT IS MADE,
!          Z CONTAINS THE EIGENVECTORS ASSOCIATED WITH THE STORED
!          EIGENVALUES.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER 30 ITERATIONS.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
!
!*****
!     MODIFIED BY C. MOLER TO ELIMINATE MACHEP 11/22/78
!     MODIFIED TO ADD JOB PARAMETER 08/27/79
!*****
      IERR = 0
      IF (N .EQ. 1) GOTO 1001
!
      DO I = 2, N
         E(I-1) = E(I)
      enddo
!
      E(N) = 0.0D0
!
      DO 240 L = 1, N
         J = 0
!     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    continue
         DO M = L, N
            IF (M .EQ. N) GOTO 120
!*****
            P = mat_flop(DABS(D(M)) + DABS(D(M+1)))
            S = mat_flop(P + DABS(E(M)))
            IF (P .EQ. S) GOTO 120
!*****
         enddo
!
  120    continue
         P = D(L)
         IF (M .EQ. L) GOTO 240
         IF (J .EQ. 30) GOTO 1000
         J = J + 1
!     .......... FORM SHIFT ..........
         G = mat_flop((D(L+1) - P)/(2.0D0*E(L)))
         R = mat_flop(DSQRT(G*G+1.0D0))
         G = mat_flop(D(M) - P + E(L)/(G + DSIGN(R,G)))
         S = 1.0D0
         C = 1.0D0
         P = 0.0D0
         MML = M - L
!     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = mat_flop(S*E(I))
            B = mat_flop(C*E(I))
            IF (DABS(F) .LT. DABS(G)) GOTO 150
            C = mat_flop(G/F)
            R = mat_flop(DSQRT(C*C+1.0D0))
            E(I+1) = mat_flop(F*R)
            S = mat_flop(1.0D0/R)
            C = mat_flop(C*S)
            GOTO 160
  150       S = mat_flop(F/G)
            R = mat_flop(DSQRT(S*S+1.0D0))
            E(I+1) = mat_flop(G*R)
            C = mat_flop(1.0D0/R)
            S = mat_flop(S*C)
  160       G = mat_flop(D(I+1) - P)
            R = mat_flop((D(I) - G)*S + 2.0D0*C*B)
            P = mat_flop(S*R)
            D(I+1) = G + P
            G = mat_flop(C*R - B)
            IF (JOB .EQ. 0) GOTO 185
!     .......... FORM VECTOR ..........
            DO K = 1, N
               F = Z(K,I+1)
               Z(K,I+1) = mat_flop(S*Z(K,I) + C*F)
               Z(K,I) = mat_flop(C*Z(K,I) - S*F)
            enddo
  185       CONTINUE
!
  200    CONTINUE
!
         D(L) = mat_flop(D(L) - P)
         E(L) = G
         E(M) = 0.0D0
         GOTO 105
  240 CONTINUE
!     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO II = 2, N
         I = II - 1
         K = I
         P = D(I)
!
         DO J = II, N
            IF (D(J) .GE. P) exit
            K = J
            P = D(J)
         enddo
!
         IF (K .EQ. I) exit
         D(K) = D(I)
         D(I) = P
!
         IF (JOB .EQ. 0) cycle
         DO J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
         enddo
      enddo
!
      GOTO 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 CONTINUE
      IERR = L
 1001 CONTINUE
      RETURN
      END SUBROUTINE ML_IMTQL2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_CORTH(NM,N,LOW,IGH,AR,AI,ORTR,ORTI)
use M_matrix
!
INTEGER I,J,M,N,II,JJ,LA,MP,NM,IGH,KP1,LOW
DOUBLEPRECISION AR(NM,N),AI(NM,N),ORTR(IGH),ORTI(IGH)
DOUBLEPRECISION F,G,H,FI,FR,SCALE
!
!     THIS SUBROUTINE IS A TRANSLATION OF A COMPLEX ANALOGUE OF
!     THE ALGOL PROCEDURE ORTHES, NUM. MATH. 12, 349-368(1968)
!     BY MARTIN AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
!
!     GIVEN A COMPLEX GENERAL MATRIX, THIS SUBROUTINE
!     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
!     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
!     UNITARY SIMILARITY TRANSFORMATIONS.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL.  IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX INPUT MATRIX.
!
!     ON OUTPUT.
!
!        AR AND AI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE HESSENBERG MATRIX.  INFORMATION
!          ABOUT THE UNITARY TRANSFORMATIONS USED IN THE REDUCTION
!          IS STORED IN THE REMAINING TRIANGLES UNDER THE
!          HESSENBERG MATRIX.
!
!        ORTR AND ORTI CONTAIN FURTHER INFORMATION ABOUT THE
!          TRANSFORMATIONS.  ONLY ELEMENTS LOW THROUGH IGH ARE USED.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
!
   LA = IGH - 1
   KP1 = LOW + 1
   IF (LA .LT. KP1) GOTO 200
!
   DO M = KP1, LA
      H = 0.0D0
      ORTR(M) = 0.0D0
      ORTI(M) = 0.0D0
      SCALE = 0.0D0
!     .......... SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) ..........
      DO I = M, IGH
         SCALE = mat_flop(SCALE + DABS(AR(I,M-1)) + DABS(AI(I,M-1)))
      enddo
!
      IF (SCALE .EQ. 0.0D0) cycle
      MP = M + IGH
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
      DO II = M, IGH
         I = MP - II
         ORTR(I) = mat_flop(AR(I,M-1)/SCALE)
         ORTI(I) = mat_flop(AI(I,M-1)/SCALE)
         H = mat_flop(H + ORTR(I)*ORTR(I) + ORTI(I)*ORTI(I))
      enddo
!
      G = mat_flop(DSQRT(H))
      F = mat_pythag(ORTR(M),ORTI(M))
      IF (F .EQ. 0.0D0) GOTO 103
      H = mat_flop(H + F*G)
      G = mat_flop(G/F)
      ORTR(M) = mat_flop((1.0D0 + G)*ORTR(M))
      ORTI(M) = mat_flop((1.0D0 + G)*ORTI(M))
      GOTO 105
!
103   continue
      ORTR(M) = G
      AR(M,M-1) = SCALE
!     .......... FORM (I-(U*UT)/H)*A ..........
105   continue
      DO J = M, N
         FR = 0.0D0
         FI = 0.0D0
!     .......... FOR I=IGH STEP -1 UNTIL M DO -- ..........
         DO II = M, IGH
            I = MP - II
            FR = mat_flop(FR + ORTR(I)*AR(I,J) + ORTI(I)*AI(I,J))
            FI = mat_flop(FI + ORTR(I)*AI(I,J) - ORTI(I)*AR(I,J))
         enddo
!
         FR = mat_flop(FR/H)
         FI = mat_flop(FI/H)
!
         DO I = M, IGH
            AR(I,J) = mat_flop(AR(I,J) - FR*ORTR(I) + FI*ORTI(I))
            AI(I,J) = mat_flop(AI(I,J) - FR*ORTI(I) - FI*ORTR(I))
         enddo
!
      enddo
!     .......... FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) ..........
      DO I = 1, IGH
         FR = 0.0D0
         FI = 0.0D0
!     .......... FOR J=IGH STEP -1 UNTIL M DO -- ..........
         DO JJ = M, IGH
            J = MP - JJ
            FR = mat_flop(FR + ORTR(J)*AR(I,J) - ORTI(J)*AI(I,J))
            FI = mat_flop(FI + ORTR(J)*AI(I,J) + ORTI(J)*AR(I,J))
         enddo
!
         FR = mat_flop(FR/H)
         FI = mat_flop(FI/H)
!
         DO J = M, IGH
            AR(I,J) = mat_flop(AR(I,J) - FR*ORTR(J) - FI*ORTI(J))
            AI(I,J) = mat_flop(AI(I,J) + FR*ORTI(J) - FI*ORTR(J))
         enddo
!
      enddo
!
      ORTR(M) = mat_flop(SCALE*ORTR(M))
      ORTI(M) = mat_flop(SCALE*ORTI(M))
      AR(M,M-1) = mat_flop(-(G*AR(M,M-1)))
      AI(M,M-1) = mat_flop(-(G*AI(M,M-1)))
   enddo
!
200 continue
END SUBROUTINE ML_CORTH
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine ml_comqr3(nm,n,low,igh,ortr,orti,hr,hi,wr,wi,zr,zi,ierr ,job)
!*****
!     MODIFICATION OF EISPACK COMQR2 TO ADD JOB PARAMETER
!     JOB = 0  OUTPUT H = SCHUR TRIANGULAR FORM, Z NOT USED
!         = 1  OUTPUT H = SCHUR FORM, Z = UNITARY SIMILARITY
!         = 2  SAME AS COMQR2
!         = 3  OUTPUT H = HESSENBERG FORM, Z = UNITARY SIMILARITY
!     ALSO ELIMINATE MACHEP
!     C. MOLER, 11/22/78 AND 09/14/80
!     OVERFLOW CONTROL IN EIGENVECTOR BACKSUBSTITUTION, 3/16/82
!*****
!
!
!     THIS SUBROUTINE IS A TRANSLATION OF A UNITARY ANALOGUE OF THE
!     ALGOL PROCEDURE  COMLR2, NUM. MATH. 16, 181-204(1970) BY PETERS
!     AND WILKINSON.
!     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
!     THE UNITARY ANALOGUE SUBSTITUTES THE QR ALGORITHM OF FRANCIS
!     (COMP. JOUR. 4, 332-345(1962)) FOR THE LR ALGORITHM.
!
!     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
!     OF A COMPLEX UPPER HESSENBERG MATRIX BY THE QR
!     METHOD.  THE EIGENVECTORS OF A COMPLEX GENERAL MATRIX
!     CAN ALSO BE FOUND IF  CORTH  HAS BEEN USED TO REDUCE
!     THIS GENERAL MATRIX TO HESSENBERG FORM.
!
!     ON INPUT.
!
!        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
!          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
!          DIMENSION STATEMENT.
!
!        N IS THE ORDER OF THE MATRIX.
!
!        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
!          SUBROUTINE ML_CBAL.  IF  CBAL  HAS NOT BEEN USED,
!          SET LOW=1, IGH=N.
!
!        ORTR AND ORTI CONTAIN INFORMATION ABOUT THE UNITARY TRANS-
!          FORMATIONS USED IN THE REDUCTION BY  CORTH, IF PERFORMED.
!          ONLY ELEMENTS LOW THROUGH IGH ARE USED.  IF THE EIGENVECTORS
!          OF THE HESSENBERG MATRIX ARE DESIRED, SET ORTR(J) AND
!          ORTI(J) TO 0.0D0 FOR THESE ELEMENTS.
!
!        HR AND HI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE COMPLEX UPPER HESSENBERG MATRIX.
!          THEIR LOWER TRIANGLES BELOW THE SUBDIAGONAL CONTAIN FURTHER
!          INFORMATION ABOUT THE TRANSFORMATIONS WHICH WERE USED IN THE
!          REDUCTION BY  CORTH, IF PERFORMED.  IF THE EIGENVECTORS OF
!          THE HESSENBERG MATRIX ARE DESIRED, THESE ELEMENTS MAY BE
!          ARBITRARY.
!
!     ON OUTPUT.
!
!        ORTR, ORTI, AND THE UPPER HESSENBERG PORTIONS OF HR AND HI
!          HAVE BEEN DESTROYED.
!
!        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVALUES.  IF AN ERROR
!          EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
!          FOR INDICES IERR+1,...,N.
!
!        ZR AND ZI CONTAIN THE REAL AND IMAGINARY PARTS,
!          RESPECTIVELY, OF THE EIGENVECTORS.  THE EIGENVECTORS
!          ARE UNNORMALIZED.  IF AN ERROR EXIT IS MADE, NONE OF
!          THE EIGENVECTORS HAS BEEN FOUND.
!
!        IERR IS SET TO
!          ZERO       FOR NORMAL RETURN,
!          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
!  DETERMINED AFTER A TOTAL OF 30*N ITERATIONS.
!
!     MODIFIED TO GET RID OF ALL COMPLEX ARITHMETIC, C. MOLER, 6/27/79.
!
!     QUESTIONS AND COMMENTS SHOULD BE DIRECTED TO B. S. GARBOW,
!     APPLIED MATHEMATICS DIVISION, ARGONNE NATIONAL LABORATORY
!
!     ------------------------------------------------------------------
use M_matrix
integer i,j,k,l,m,n,en,ii,ll,nm,nn,igh,ip1,itn,its,low,lp1,enm1,iend,ierr
doubleprecision hr(nm,n),hi(nm,n),wr(n),wi(n),zr(nm,n),zi(nm,n),ortr(igh),orti(igh)
doubleprecision si,sr,ti,tr,xi,xr,yi,yr,zzi,zzr,norm
integer :: job
integer :: jj
   ierr = 0
   !*****
   if (job .eq. 0) goto 150
   !*****
!     .......... INITIALIZE EIGENVECTOR MATRIX ..........
   do i = 1, n
      do j = 1, n
         zr(i,j) = 0.0d0
         zi(i,j) = 0.0d0
         if (i .eq. j) zr(i,j) = 1.0d0
      enddo
   enddo
!     .......... FORM THE MATRIX OF ACCUMULATED TRANSFORMATIONS
!                FROM THE INFORMATION LEFT BY CORTH ..........
   iend = igh - low - 1
   if (iend) 180, 150, 105
!     .......... for i=igh-1 step -1 until low+1 do -- ..........
105 continue
   do ii = 1, iend
      i = igh - ii
      if (ortr(i) .eq. 0.0d0 .and. orti(i) .eq. 0.0d0) cycle
      if (hr(i,i-1) .eq. 0.0d0 .and. hi(i,i-1) .eq. 0.0d0) cycle
!     .......... NORM BELOW IS NEGATIVE OF H FORMED IN CORTH ..........
      norm = mat_flop(hr(i,i-1)*ortr(i) + hi(i,i-1)*orti(i))
      ip1 = i + 1

      do k = ip1, igh
         ortr(k) = hr(k,i-1)
         orti(k) = hi(k,i-1)
      enddo

      do j = i, igh
         sr = 0.0d0
         si = 0.0d0

         do k = i, igh
            sr = mat_flop(sr + ortr(k)*zr(k,j) + orti(k)*zi(k,j))
            si = mat_flop(si + ortr(k)*zi(k,j) - orti(k)*zr(k,j))
         enddo

         sr = mat_flop(sr/norm)
         si = mat_flop(si/norm)

         do k = i, igh
            zr(k,j) = mat_flop(zr(k,j) + sr*ortr(k) - si*orti(k))
            zi(k,j) = mat_flop(zi(k,j) + sr*orti(k) + si*ortr(k))
         enddo

      enddo

   enddo
   !*****
   if (job .eq. 3) goto 1001
   !*****
!     .......... CREATE REAL SUBDIAGONAL ELEMENTS ..........
150 continue
   l = low + 1

   do i = l, igh
      ll = min0(i+1,igh)
      if (hi(i,i-1) .eq. 0.0d0) cycle
      norm = mat_pythag(hr(i,i-1),hi(i,i-1))
      yr = mat_flop(hr(i,i-1)/norm)
      yi = mat_flop(hi(i,i-1)/norm)
      hr(i,i-1) = norm
      hi(i,i-1) = 0.0d0

      do j = i, n
         si = mat_flop(yr*hi(i,j) - yi*hr(i,j))
         hr(i,j) = mat_flop(yr*hr(i,j) + yi*hi(i,j))
         hi(i,j) = si
      enddo

      do j = 1, ll
         si = mat_flop(yr*hi(j,i) + yi*hr(j,i))
         hr(j,i) = mat_flop(yr*hr(j,i) - yi*hi(j,i))
         hi(j,i) = si
      enddo
      !*****
      if (job .eq. 0) cycle
      !*****
      do j = low, igh
         si = mat_flop(yr*zi(j,i) + yi*zr(j,i))
         zr(j,i) = mat_flop(yr*zr(j,i) - yi*zi(j,i))
         zi(j,i) = si
      enddo

   enddo
!     .......... STORE ROOTS ISOLATED BY CBAL ..........
180 continue
   do i = 1, n
      if (i .ge. low .and. i .le. igh) cycle
      wr(i) = hr(i,i)
      wi(i) = hi(i,i)
   enddo

   en = igh
   tr = 0.0d0
   ti = 0.0d0
   itn = 30*n
!     .......... SEARCH FOR NEXT EIGENVALUE ..........
220 continue
   if (en .lt. low) goto 680
   its = 0
   enm1 = en - 1
!     .......... LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
!                FOR L=EN STEP -1 UNTIL LOW DO -- ..........
240 continue
   do ll = low, en
      l = en + low - ll
      if (l .eq. low) exit
      !*****
      xr = mat_flop(dabs(hr(l-1,l-1)) + dabs(hi(l-1,l-1)) + dabs(hr(l,l)) +dabs(hi(l,l)))
      yr = mat_flop(xr + dabs(hr(l,l-1)))
      if (xr .eq. yr) exit
      !*****
   enddo
!     .......... FORM SHIFT ..........
   if (l .eq. en) goto 660
   if (itn .eq. 0) goto 1000
   if (its .eq. 10 .or. its .eq. 20) goto 320
   sr = hr(en,en)
   si = hi(en,en)
   xr = mat_flop(hr(enm1,en)*hr(en,enm1))
   xi = mat_flop(hi(enm1,en)*hr(en,enm1))
   if (xr .eq. 0.0d0 .and. xi .eq. 0.0d0) goto 340
   yr = mat_flop((hr(enm1,enm1) - sr)/2.0d0)
   yi = mat_flop((hi(enm1,enm1) - si)/2.0d0)
   call mat_wsqrt(yr**2-yi**2+xr,2.0d0*yr*yi+xi,zzr,zzi)
   if (yr*zzr + yi*zzi .ge. 0.0d0) goto 310
   zzr = -zzr
   zzi = -zzi
310 continue
   call mat_wdiv(xr,xi,yr+zzr,yi+zzi,zzr,zzi)
   sr = mat_flop(sr - zzr)
   si = mat_flop(si - zzi)
   goto 340
!     .......... FORM EXCEPTIONAL SHIFT ..........
320 continue
   sr = mat_flop(dabs(hr(en,enm1)) + dabs(hr(enm1,en-2)))
   si = 0.0d0

340 continue
   do i = low, en
      hr(i,i) = mat_flop(hr(i,i) - sr)
      hi(i,i) = mat_flop(hi(i,i) - si)
   enddo

   tr = mat_flop(tr + sr)
   ti = mat_flop(ti + si)
   its = its + 1
   itn = itn - 1
!     .......... REDUCE TO TRIANGLE (ROWS) ..........
   lp1 = l + 1

   do i = lp1, en
      sr = hr(i,i-1)
      hr(i,i-1) = 0.0d0
      norm= mat_flop(dabs(hr(i-1,i-1)) + dabs(hi(i-1,i-1)) + dabs(sr))
      norm= mat_flop(norm*dsqrt((hr(i-1,i-1)/norm)**2 + (hi(i-1,i-1)/norm)**2 + (sr/norm)**2))
      xr = mat_flop(hr(i-1,i-1)/norm)
      wr(i-1) = xr
      xi = mat_flop(hi(i-1,i-1)/norm)
      wi(i-1) = xi
      hr(i-1,i-1) = norm
      hi(i-1,i-1) = 0.0d0
      hi(i,i-1) = mat_flop(sr/norm)

      do j = i, n
         yr = hr(i-1,j)
         yi = hi(i-1,j)
         zzr = hr(i,j)
         zzi = hi(i,j)
         hr(i-1,j) = mat_flop(xr*yr + xi*yi + hi(i,i-1)*zzr)
         hi(i-1,j) = mat_flop(xr*yi - xi*yr + hi(i,i-1)*zzi)
         hr(i,j) = mat_flop(xr*zzr - xi*zzi - hi(i,i-1)*yr)
         hi(i,j) = mat_flop(xr*zzi + xi*zzr - hi(i,i-1)*yi)
      enddo

   enddo

   si = hi(en,en)
   if (si .eq. 0.0d0) goto 540
   norm = mat_pythag(hr(en,en),si)
   sr = mat_flop(hr(en,en)/norm)
   si = mat_flop(si/norm)
   hr(en,en) = norm
   hi(en,en) = 0.0d0
   if (en .eq. n) goto 540
   ip1 = en + 1

   do j = ip1, n
      yr = hr(en,j)
      yi = hi(en,j)
      hr(en,j) = mat_flop(sr*yr + si*yi)
      hi(en,j) = mat_flop(sr*yi - si*yr)
   enddo
!     .......... INVERSE OPERATION (COLUMNS) ..........
540 continue
   do j = lp1, en
      xr = wr(j-1)
      xi = wi(j-1)

      do i = 1, j
         yr = hr(i,j-1)
         yi = 0.0d0
         zzr = hr(i,j)
         zzi = hi(i,j)
         if (i .eq. j) goto 560
         yi = hi(i,j-1)
         hi(i,j-1) = mat_flop(xr*yi + xi*yr + hi(j,j-1)*zzi)
560      continue
         hr(i,j-1) = mat_flop(xr*yr - xi*yi + hi(j,j-1)*zzr)
         hr(i,j) = mat_flop(xr*zzr + xi*zzi - hi(j,j-1)*yr)
         hi(i,j) = mat_flop(xr*zzi - xi*zzr - hi(j,j-1)*yi)
      enddo
!*****
      if (job .eq. 0) cycle
!*****
      do i = low, igh
         yr = zr(i,j-1)
         yi = zi(i,j-1)
         zzr = zr(i,j)
         zzi = zi(i,j)
         zr(i,j-1) = mat_flop(xr*yr - xi*yi + hi(j,j-1)*zzr)
         zi(i,j-1) = mat_flop(xr*yi + xi*yr + hi(j,j-1)*zzi)
         zr(i,j) = mat_flop(xr*zzr + xi*zzi - hi(j,j-1)*yr)
         zi(i,j) = mat_flop(xr*zzi - xi*zzr - hi(j,j-1)*yi)
      enddo

   enddo

   if (si .eq. 0.0d0) goto 240

   do i = 1, en
      yr = hr(i,en)
      yi = hi(i,en)
      hr(i,en) = mat_flop(sr*yr - si*yi)
      hi(i,en) = mat_flop(sr*yi + si*yr)
   enddo
!*****
   if (job .eq. 0) goto 240
!*****
   do i = low, igh
      yr = zr(i,en)
      yi = zi(i,en)
      zr(i,en) = mat_flop(sr*yr - si*yi)
      zi(i,en) = mat_flop(sr*yi + si*yr)
   enddo

   goto 240
!     .......... A ROOT FOUND ..........
660 continue
   hr(en,en) = mat_flop(hr(en,en) + tr)
   wr(en) = hr(en,en)
   hi(en,en) = mat_flop(hi(en,en) + ti)
   wi(en) = hi(en,en)
   en = enm1
   goto 220
!     .......... ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND
!                VECTORS OF UPPER TRIANGULAR FORM ..........
!
!*****  THE FOLLOWING SECTION CHANGED FOR OVERFLOW CONTROL
!       C. MOLER, 3/16/82
!
680 continue
   if (job .ne. 2) goto 1001

   norm = 0.0d0
   do i = 1, n
      do j = i, n
         tr = mat_flop(dabs(hr(i,j))) + mat_flop(dabs(hi(i,j)))
         if (tr .gt. norm) norm = tr
      enddo
   enddo
   if (n .eq. 1 .or. norm .eq. 0.0d0) goto 1001
!     .......... FOR EN=N STEP -1 UNTIL 2 DO -- ..........
   do nn = 2, n
      en = n + 2 - nn
      xr = wr(en)
      xi = wi(en)
      hr(en,en) = 1.0d0
      hi(en,en) = 0.0d0
      enm1 = en - 1
!     .......... FOR I=EN-1 STEP -1 UNTIL 1 DO -- ..........
      do ii = 1, enm1
         i = en - ii
         zzr = 0.0d0
         zzi = 0.0d0
         ip1 = i + 1
         do j = ip1, en
            zzr = mat_flop(zzr + hr(i,j)*hr(j,en) - hi(i,j)*hi(j,en))
            zzi = mat_flop(zzi + hr(i,j)*hi(j,en) + hi(i,j)*hr(j,en))
         enddo
         yr = mat_flop(xr - wr(i))
         yi = mat_flop(xi - wi(i))
         if (yr .ne. 0.0d0 .or. yi .ne. 0.0d0) goto 765
         yr = norm
760      continue
         yr = mat_flop(yr/100.0d0)
         yi = mat_flop(norm + yr)
         if (yi .ne. norm) goto 760
         yi = 0.0d0
765      continue
         call mat_wdiv(zzr,zzi,yr,yi,hr(i,en),hi(i,en))
         tr = mat_flop(dabs(hr(i,en))) + mat_flop(dabs(hi(i,en)))
         if (tr .eq. 0.0d0) cycle
         if (tr + 1.0d0/tr .gt. tr)cycle
         do j = i, en
            hr(j,en) = mat_flop(hr(j,en)/tr)
            hi(j,en) = mat_flop(hi(j,en)/tr)
         enddo
      enddo
   enddo
!*****
!     .......... END BACKSUBSTITUTION ..........
   enm1 = n - 1
!     .......... VECTORS OF ISOLATED ROOTS ..........
   do  i = 1, enm1
      if (i .ge. low .and. i .le. igh) cycle
      ip1 = i + 1

      do j = ip1, n
         zr(i,j) = hr(i,j)
         zi(i,j) = hi(i,j)
      enddo

   enddo
!     .......... MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
!                VECTORS OF ORIGINAL FULL MATRIX.
!                FOR J=N STEP -1 UNTIL LOW+1 DO -- ..........
   do jj = low, enm1
      j = n + low - jj
      m = min0(j,igh)

      do i = low, igh
         zzr = 0.0d0
         zzi = 0.0d0

         do k = low, m
            zzr = mat_flop(zzr + zr(i,k)*hr(k,j) - zi(i,k)*hi(k,j))
            zzi = mat_flop(zzi + zr(i,k)*hi(k,j) + zi(i,k)*hr(k,j))
         enddo

         zr(i,j) = zzr
         zi(i,j) = zzi
      enddo
   enddo
!
   goto 1001
!     .......... SET ERROR -- NO CONVERGENCE TO AN
!                EIGENVALUE AFTER 30 ITERATIONS ..........
1000 continue
   ierr = en
1001 continue
end subroutine ml_comqr3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WSVDC(xr,xi,LDX,N,P,sr,si,er,ei,ur,ui,LDU,vr,vi,LDV,workr,worki,JOB,INFO)
      use M_matrix
      INTEGER LDX,N,P,LDU,LDV,JOB,INFO
      DOUBLEPRECISION XR(LDX,*),XI(LDX,*),SR(*),SI(*),ER(*),EI(*), UR(LDU,*),UI(LDU,*),VR(LDV,*),VI(LDV,*), WORKR(*),WORKI(*)
!
!
!     WSVDC IS A SUBROUTINE TO REDUCE A DOUBLE-COMPLEX NXP MATRIX X BY
!     UNITARY TRANSFORMATIONS U AND V TO DIAGONAL FORM.  THE
!     DIAGONAL ELEMENTS S(I) ARE THE SINGULAR VALUES OF X.  THE
!     COLUMNS OF U ARE THE CORRESPONDING LEFT SINGULAR VECTORS,
!     AND THE COLUMNS OF V THE RIGHT SINGULAR VECTORS.
!
!     ON ENTRY
!
!         X         DOUBLE-COMPLEX(LDX,P), WHERE LDX.GE.N.
!                   X CONTAINS THE MATRIX WHOSE SINGULAR VALUE
!                   DECOMPOSITION IS TO BE COMPUTED.  X IS
!                   DESTROYED BY WSVDC.
!
!         LDX       INTEGER.
!                   LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!         N         INTEGER.
!                   N IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!         P         INTEGER.
!                   P IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!         LDU       INTEGER.
!                   LDU IS THE LEADING DIMENSION OF THE ARRAY U
!                   (SEE BELOW).
!
!         LDV       INTEGER.
!                   LDV IS THE LEADING DIMENSION OF THE ARRAY V
!                   (SEE BELOW).
!
!         WORK      DOUBLE-COMPLEX(N).
!                   WORK IS A SCRATCH ARRAY.
!
!         JOB       INTEGER.
!                   JOB CONTROLS THE COMPUTATION OF THE SINGULAR
!                   VECTORS.  IT HAS THE DECIMAL EXPANSION AB
!                   WITH THE FOLLOWING MEANING
!
!     A.EQ.0    DO NOT COMPUTE THE LEFT SINGULAR
!               VECTORS.
!     A.EQ.1    RETURN THE N LEFT SINGULAR VECTORS
!               IN U.
!     A.GE.2    RETURNS THE FIRST MIN(N,P)
!               LEFT SINGULAR VECTORS IN U.
!     B.EQ.0    DO NOT COMPUTE THE RIGHT SINGULAR
!               VECTORS.
!     B.EQ.1    RETURN THE RIGHT SINGULAR VECTORS
!               IN V.
!
!     ON RETURN
!
!         S         DOUBLE-COMPLEX(MM), WHERE MM=MIN(N+1,P).
!                   THE FIRST MIN(N,P) ENTRIES OF S CONTAIN THE
!                   SINGULAR VALUES OF X ARRANGED IN DESCENDING
!                   ORDER OF MAGNITUDE.
!
!         E         DOUBLE-COMPLEX(P).
!                   E ORDINARILY CONTAINS ZEROS.  HOWEVER SEE THE
!                   DISCUSSION OF INFO FOR EXCEPTIONS.
!
!         U         DOUBLE-COMPLEX(LDU,K), WHERE LDU.GE.N.
!                   IF JOBA.EQ.1 THEN K.EQ.N,
!                   IF JOBA.EQ.2 THEN K.EQ.MIN(N,P).
!                   U CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   U IS NOT REFERENCED IF JOBA.EQ.0.  IF N.LE.P
!                   OR IF JOBA.GT.2, THEN U MAY BE IDENTIFIED WITH X
!                   IN THE SUBROUTINE CALL.
!
!         V         DOUBLE-COMPLEX(LDV,P), WHERE LDV.GE.P.
!                   V CONTAINS THE MATRIX OF RIGHT SINGULAR VECTORS.
!                   V IS NOT REFERENCED IF JOBB.EQ.0.  IF P.LE.N,
!                   THEN V MAY BE IDENTIFIED WHTH X IN THE
!                   SUBROUTINE ML_CALL.
!
!         INFO      INTEGER.
!                   THE SINGULAR VALUES (AND THEIR CORRESPONDING
!                   SINGULAR VECTORS) S(INFO+1),S(INFO+2),...,S(M)
!                   ARE CORRECT (HERE M=MIN(N,P)).  THUS IF
!                   INFO.EQ.0, ALL THE SINGULAR VALUES AND THEIR
!                   VECTORS ARE CORRECT.  IN ANY EVENT, THE MATRIX
!                   B = CTRANS(U)*X*V IS THE BIDIAGONAL MATRIX
!                   WITH THE ELEMENTS OF S ON ITS DIAGONAL AND THE
!                   ELEMENTS OF E ON ITS SUPER-DIAGONAL (CTRANS(U)
!                   IS THE CONJUGATE-TRANSPOSE OF U).  THUS THE
!                   SINGULAR VALUES OF X AND B ARE THE SAME.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WSVDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS    matX_waxpy,mat_pythag,mat_wdotcr,mat_wdotci,mat_wscal,WSWAP,
!             mat_rrotg,mat_wnrm2
!     FORTRAN DABS,DIMAG,DMAX1
!     FORTRAN MAX0,MIN0,MOD,DSQRT
!
!     INTERNAL VARIABLES
!
      INTEGER I,ITER,J,JOBU,K,KASE,KK,L,LL,LLS,LM1,LP1,LS,LU,M,MAXIT,MM,MM1,MP1,NCT,NCTP1,NCU,NRT,NRTP1
      DOUBLEPRECISION TR,TI,RR,RI
      DOUBLEPRECISION B,C,CS,EL,EMM1,F,G,SCALE,SHIFT,SL,SM,SN,SMM1,T1,TEST,ZTEST,SMALL
      LOGICAL WANTU,WANTV
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET THE MAXIMUM NUMBER OF ITERATIONS.
!
      MAXIT = 75
!
!     SMALL NUMBER, ROUGHLY MACHINE EPSILON, USED TO AVOID UNDERFLOW
!
      SMALL = 1.D0/2.D0**48
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
      WANTU = .FALSE.
      WANTV = .FALSE.
      JOBU = MOD(JOB,100)/10
      NCU = N
      IF (JOBU .GT. 1) NCU = MIN0(N,P)
      IF (JOBU .NE. 0) WANTU = .TRUE.
      IF (MOD(JOB,10) .NE. 0) WANTV = .TRUE.
!
!     REDUCE X TO BIDIAGONAL FORM, STORING THE DIAGONAL ELEMENTS
!     IN S AND THE SUPER-DIAGONAL ELEMENTS IN E.
!
      INFO = 0
      NCT = MIN0(N-1,P)
      NRT = MAX0(0,MIN0(P-2,N))
      LU = MAX0(NCT,NRT)
      IF (LU .LT. 1) GOTO 190
      DO 180 L = 1, LU
         LP1 = L + 1
         IF (L .GT. NCT) GOTO 30
!
!           COMPUTE THE TRANSFORMATION FOR THE L-TH COLUMN AND
!           PLACE THE L-TH DIAGONAL IN S(L).
!
            SR(L) = mat_wnrm2(N-L+1,XR(L,L),XI(L,L),1)
            SI(L) = 0.0D0
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 20
               IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 10
                  CALL mat_wsign(SR(L),SI(L),XR(L,L),XI(L,L),SR(L),SI(L))
   10          CONTINUE
               CALL mat_wdiv(1.0D0,0.0D0,SR(L),SI(L),TR,TI)
               CALL mat_wscal(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
               XR(L,L) = mat_flop(1.0D0 + XR(L,L))
   20       CONTINUE
            SR(L) = -SR(L)
            SI(L) = -SI(L)
   30    CONTINUE
         IF (P .LT. LP1) GOTO 60
         DO 50 J = LP1, P
            IF (L .GT. NCT) GOTO 40
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 40
!
!              APPLY THE TRANSFORMATION.
!
               TR= -mat_wdotcr(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               TI= -mat_wdotci(N-L+1,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
               CALL mat_wdiv(TR,TI,XR(L,L),XI(L,L),TR,TI)
               CALL matX_waxpy(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J),XI(L,J),1)
   40       CONTINUE
!
!           PLACE THE L-TH ROW OF X INTO  E FOR THE
!           SUBSEQUENT CALCULATION OF THE ROW TRANSFORMATION.
!
            ER(J) = XR(L,J)
            EI(J) = -XI(L,J)
   50    CONTINUE
   60    CONTINUE
         IF (.NOT.WANTU .OR. L .GT. NCT) GOTO 80
!
!           PLACE THE TRANSFORMATION IN U FOR SUBSEQUENT BACK
!           MULTIPLICATION.
!
            DO I = L, N
               UR(I,L) = XR(I,L)
               UI(I,L) = XI(I,L)
            enddo
   80    CONTINUE
         IF (L .GT. NRT) GOTO 170
!
!           COMPUTE THE L-TH ROW TRANSFORMATION AND PLACE THE
!           L-TH SUPER-DIAGONAL IN E(L).
!
            ER(L) = mat_wnrm2(P-L,ER(LP1),EI(LP1),1)
            EI(L) = 0.0D0
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 100
               IF (CABS1(ER(LP1),EI(LP1)) .EQ. 0.0D0) GOTO 90
                  CALL mat_wsign(ER(L),EI(L),ER(LP1),EI(LP1),ER(L),EI(L))
   90          CONTINUE
               CALL mat_wdiv(1.0D0,0.0D0,ER(L),EI(L),TR,TI)
               CALL mat_wscal(P-L,TR,TI,ER(LP1),EI(LP1),1)
               ER(LP1) = mat_flop(1.0D0 + ER(LP1))
  100       CONTINUE
            ER(L) = -ER(L)
            EI(L) = +EI(L)
            IF (LP1 .GT. N .OR. CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 140
!
!              APPLY THE TRANSFORMATION.
!
               DO I = LP1, N
                  WORKR(I) = 0.0D0
                  WORKI(I) = 0.0D0
               enddo
               DO J = LP1, P
                  CALL matX_waxpy(N-L,ER(J),EI(J),XR(LP1,J),XI(LP1,J),1, WORKR(LP1),WORKI(LP1),1)
               enddo
               DO J = LP1, P
                  CALL mat_wdiv(-ER(J),-EI(J),ER(LP1),EI(LP1),TR,TI)
                  CALL matX_waxpy(N-L,TR,-TI,WORKR(LP1),WORKI(LP1),1, XR(LP1,J),XI(LP1,J),1)
               enddo
  140       CONTINUE
            IF (.NOT.WANTV) GOTO 160
!
!              PLACE THE TRANSFORMATION IN V FOR SUBSEQUENT
!              BACK MULTIPLICATION.
!
               DO I = LP1, P
                  VR(I,L) = ER(I)
                  VI(I,L) = EI(I)
               enddo
  160       CONTINUE
  170    CONTINUE
  180 CONTINUE
  190 CONTINUE
!
!     SET UP THE FINAL BIDIAGONAL MATRIX OR ORDER M.
!
      M = MIN0(P,N+1)
      NCTP1 = NCT + 1
      NRTP1 = NRT + 1
      IF (NCT .GE. P) GOTO 200
         SR(NCTP1) = XR(NCTP1,NCTP1)
         SI(NCTP1) = XI(NCTP1,NCTP1)
  200 CONTINUE
      IF (N .GE. M) GOTO 210
         SR(M) = 0.0D0
         SI(M) = 0.0D0
  210 CONTINUE
      IF (NRTP1 .GE. M) GOTO 220
         ER(NRTP1) = XR(NRTP1,M)
         EI(NRTP1) = XI(NRTP1,M)
  220 CONTINUE
      ER(M) = 0.0D0
      EI(M) = 0.0D0
!
!     IF REQUIRED, GENERATE U.
!
      IF (.NOT.WANTU) GOTO 350
         IF (NCU .LT. NCTP1) GOTO 250
         DO J = NCTP1, NCU
            DO I = 1, N
               UR(I,J) = 0.0D0
               UI(I,J) = 0.0D0
            enddo
            UR(J,J) = 1.0D0
            UI(J,J) = 0.0D0
         enddo
  250    CONTINUE
         IF (NCT .LT. 1) GOTO 340
         DO LL = 1, NCT
            L = NCT - LL + 1
            IF (CABS1(SR(L),SI(L)) .EQ. 0.0D0) GOTO 300
               LP1 = L + 1
               IF (NCU .LT. LP1) GOTO 270
               DO J = LP1, NCU
                  TR = -mat_wdotcr(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  TI = -mat_wdotci(N-L+1,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
                  CALL mat_wdiv(TR,TI,UR(L,L),UI(L,L),TR,TI)
                  CALL matX_waxpy(N-L+1,TR,TI,UR(L,L),UI(L,L),1,UR(L,J), UI(L,J),1)
               enddo
  270          CONTINUE
               CALL mat_wrscal(N-L+1,-1.0D0,UR(L,L),UI(L,L),1)
               UR(L,L) = mat_flop(1.0D0 + UR(L,L))
               LM1 = L - 1
               IF (LM1 .LT. 1) GOTO 290
               DO I = 1, LM1
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
               enddo
  290          CONTINUE
            GOTO 320
  300       CONTINUE
               DO I = 1, N
                  UR(I,L) = 0.0D0
                  UI(I,L) = 0.0D0
               enddo
               UR(L,L) = 1.0D0
               UI(L,L) = 0.0D0
  320       CONTINUE
         enddo
  340    CONTINUE
  350 CONTINUE
!
!     IF IT IS REQUIRED, GENERATE V.
!
      IF (.NOT.WANTV) GOTO 400
         DO LL = 1, P
            L = P - LL + 1
            LP1 = L + 1
            IF (L .GT. NRT) GOTO 370
            IF (CABS1(ER(L),EI(L)) .EQ. 0.0D0) GOTO 370
               DO J = LP1, P
                  TR = -mat_wdotcr(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                  TI = -mat_wdotci(P-L,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
                  CALL mat_wdiv(TR,TI,VR(LP1,L),VI(LP1,L),TR,TI)
                  CALL matX_waxpy(P-L,TR,TI,VR(LP1,L),VI(LP1,L),1,VR(LP1,J),VI(LP1,J),1)
               enddo
  370       CONTINUE
            DO I = 1, P
               VR(I,L) = 0.0D0
               VI(I,L) = 0.0D0
            enddo
            VR(L,L) = 1.0D0
            VI(L,L) = 0.0D0
         enddo
  400 CONTINUE
!
!     TRANSFORM S AND E SO THAT THEY ARE REAL.
!
      DO I = 1, M
            TR = mat_pythag(SR(I),SI(I))
            IF (TR .EQ. 0.0D0) GOTO 405
            RR = SR(I)/TR
            RI = SI(I)/TR
            SR(I) = TR
            SI(I) = 0.0D0
            IF (I .LT. M) CALL mat_wdiv(ER(I),EI(I),RR,RI,ER(I),EI(I))
            IF (WANTU) CALL mat_wscal(N,RR,RI,UR(1,I),UI(1,I),1)
  405    CONTINUE
!     ...EXIT
         IF (I .EQ. M) exit
            TR = mat_pythag(ER(I),EI(I))
            IF (TR .EQ. 0.0D0) GOTO 410
            CALL mat_wdiv(TR,0.0D0,ER(I),EI(I),RR,RI)
            ER(I) = TR
            EI(I) = 0.0D0
            CALL mat_wmul(SR(I+1),SI(I+1),RR,RI,SR(I+1),SI(I+1))
            IF (WANTV) CALL mat_wscal(P,RR,RI,VR(1,I+1),VI(1,I+1),1)
  410    CONTINUE
      enddo
!
!     MAIN ITERATION LOOP FOR THE SINGULAR VALUES.
!
      MM = M
      ITER = 0
  440 CONTINUE
!
!        QUIT IF ALL THE SINGULAR VALUES HAVE BEEN FOUND.
!
!     ...EXIT
         IF (M .EQ. 0) GOTO 700
!
!        IF TOO MANY ITERATIONS HAVE BEEN PERFORMED, SET
!        FLAG AND RETURN.
!
         IF (ITER .LT. MAXIT) GOTO 450
            INFO = M
!     ......EXIT
            GOTO 700
  450    CONTINUE
!
!        THIS SECTION OF THE PROGRAM INSPECTS FOR
!        NEGLIGIBLE ELEMENTS IN THE S AND E ARRAYS.  ON
!        COMPLETION THE VARIABLE KASE IS SET AS FOLLOWS.
!
!           KASE = 1     IF SR(M) AND ER(L-1) ARE NEGLIGIBLE AND L.LT.M
!           KASE = 2     IF SR(L) IS NEGLIGIBLE AND L.LT.M
!           KASE = 3     IF ER(L-1) IS NEGLIGIBLE, L.LT.M, AND
!     SR(L), ..., SR(M) ARE NOT NEGLIGIBLE (QR STEP).
!           KASE = 4     IF ER(M-1) IS NEGLIGIBLE (CONVERGENCE).
!
         DO LL = 1, M
            L = M - LL
!        ...EXIT
            IF (L .EQ. 0) GOTO 480
            TEST = mat_flop(DABS(SR(L)) + DABS(SR(L+1)))
            ZTEST = mat_flop(TEST + DABS(ER(L))/2.0D0)
            IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 460
               ER(L) = 0.0D0
!        ......EXIT
               GOTO 480
  460       CONTINUE
         enddo
  480    CONTINUE
         IF (L .NE. M - 1) GOTO 490
            KASE = 4
         GOTO 560
  490    CONTINUE
            LP1 = L + 1
            MP1 = M + 1
            DO LLS = LP1, MP1
               LS = M - LLS + LP1
!           ...EXIT
               IF (LS .EQ. L) GOTO 520
               TEST = 0.0D0
               IF (LS .NE. M) TEST = mat_flop(TEST + DABS(ER(LS)))
               IF (LS .NE. L + 1) TEST = mat_flop(TEST + DABS(ER(LS-1)))
               ZTEST = mat_flop(TEST + DABS(SR(LS))/2.0D0)
               IF (SMALL*ZTEST .NE. SMALL*TEST) GOTO 500
                  SR(LS) = 0.0D0
!           ......EXIT
                  GOTO 520
  500          CONTINUE
            enddo
  520       CONTINUE
            IF (LS .NE. L) GOTO 530
               KASE = 3
            GOTO 550
  530       CONTINUE
            IF (LS .NE. M) GOTO 540
               KASE = 1
            GOTO 550
  540       CONTINUE
               KASE = 2
               L = LS
  550       CONTINUE
  560    CONTINUE
         L = L + 1
!
!        PERFORM THE TASK INDICATED BY KASE.
!
         GOTO (570, 600, 620, 650), KASE
!
!        DEFLATE NEGLIGIBLE SR(M).
!
  570    CONTINUE
            MM1 = M - 1
            F = ER(M-1)
            ER(M-1) = 0.0D0
            DO KK = L, MM1
               K = MM1 - KK + L
               T1 = SR(K)
               CALL mat_rrotg(T1,F,CS,SN)
               SR(K) = T1
               IF (K .EQ. L) GOTO 580
                  F = mat_flop(-(SN*ER(K-1)))
                  ER(K-1) = mat_flop(CS*ER(K-1))
  580          CONTINUE
               IF (WANTV) CALL mat_rrot(P,VR(1,K),1,VR(1,M),1,CS,SN)
               IF (WANTV) CALL mat_rrot(P,VI(1,K),1,VI(1,M),1,CS,SN)
            enddo
         GOTO 690
!
!        SPLIT AT NEGLIGIBLE SR(L).
!
  600    CONTINUE
            F = ER(L-1)
            ER(L-1) = 0.0D0
            DO K = L, M
               T1 = SR(K)
               CALL mat_rrotg(T1,F,CS,SN)
               SR(K) = T1
               F = mat_flop(-(SN*ER(K)))
               ER(K) = mat_flop(CS*ER(K))
               IF (WANTU) CALL mat_rrot(N,UR(1,K),1,UR(1,L-1),1,CS,SN)
               IF (WANTU) CALL mat_rrot(N,UI(1,K),1,UI(1,L-1),1,CS,SN)
            enddo
         GOTO 690
!
!        PERFORM ONE QR STEP.
!
  620    CONTINUE
!
!           CALCULATE THE SHIFT.
!
            SCALE = DMAX1(DABS(SR(M)),DABS(SR(M-1)),DABS(ER(M-1)), DABS(SR(L)),DABS(ER(L)))
            SM = SR(M)/SCALE
            SMM1 = SR(M-1)/SCALE
            EMM1 = ER(M-1)/SCALE
            SL = SR(L)/SCALE
            EL = ER(L)/SCALE
            B = mat_flop(((SMM1 + SM)*(SMM1 - SM) + EMM1**2)/2.0D0)
            C = mat_flop((SM*EMM1)**2)
            SHIFT = 0.0D0
            IF (B .EQ. 0.0D0 .AND. C .EQ. 0.0D0) GOTO 630
               SHIFT = mat_flop(DSQRT(B**2+C))
               IF (B .LT. 0.0D0) SHIFT = -SHIFT
               SHIFT = mat_flop(C/(B + SHIFT))
  630       CONTINUE
            F = mat_flop((SL + SM)*(SL - SM) - SHIFT)
            G = mat_flop(SL*EL)
!
!           CHASE ZEROS.
!
            MM1 = M - 1
            DO K = L, MM1
               CALL mat_rrotg(F,G,CS,SN)
               IF (K .NE. L) ER(K-1) = F
               F = mat_flop(CS*SR(K) + SN*ER(K))
               ER(K) = mat_flop(CS*ER(K) - SN*SR(K))
               G = mat_flop(SN*SR(K+1))
               SR(K+1) = mat_flop(CS*SR(K+1))
               IF (WANTV) CALL mat_rrot(P,VR(1,K),1,VR(1,K+1),1,CS,SN)
               IF (WANTV) CALL mat_rrot(P,VI(1,K),1,VI(1,K+1),1,CS,SN)
               CALL mat_rrotg(F,G,CS,SN)
               SR(K) = F
               F = mat_flop(CS*ER(K) + SN*SR(K+1))
               SR(K+1) = mat_flop(-(SN*ER(K)) + CS*SR(K+1))
               G = mat_flop(SN*ER(K+1))
               ER(K+1) = mat_flop(CS*ER(K+1))
               IF (WANTU .AND. K .LT. N) CALL mat_rrot(N,UR(1,K),1,UR(1,K+1),1,CS,SN)
               IF (WANTU .AND. K .LT. N) CALL mat_rrot(N,UI(1,K),1,UI(1,K+1),1,CS,SN)
            enddo
            ER(M-1) = F
            ITER = ITER + 1
         GOTO 690
!
!        CONVERGENCE
!
  650    CONTINUE
!
!           MAKE THE SINGULAR VALUE  POSITIVE
!
            IF (SR(L) .GE. 0.0D0) GOTO 660
               SR(L) = -SR(L)
             IF (WANTV) CALL mat_wrscal(P,-1.0D0,VR(1,L),VI(1,L),1)
  660       CONTINUE
!
!           ORDER THE SINGULAR VALUE.
!
  670       IF (L .EQ. MM) GOTO 680
!           ...EXIT
               IF (SR(L) .GE. SR(L+1)) GOTO 680
               TR = SR(L)
               SR(L) = SR(L+1)
               SR(L+1) = TR
               IF (WANTV .AND. L .LT. P)CALL mat_wswap(P,VR(1,L),VI(1,L),1,VR(1,L+1),VI(1,L+1),1)
               IF (WANTU .AND. L .LT. N)CALL mat_wswap(N,UR(1,L),UI(1,L),1,UR(1,L+1),UI(1,L+1),1)
               L = L + 1
            GOTO 670
  680       CONTINUE
            ITER = 0
            M = M - 1
  690    CONTINUE
      GOTO 440
  700 CONTINUE
      END SUBROUTINE ML_WSVDC
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WQRDC(XR,XI,LDX,N,P,QRAUXR,QRAUXI,JPVT,WORKR,WORKI, JOB)
      use M_matrix
      INTEGER LDX,N,P,JOB
      INTEGER JPVT(*)
      DOUBLEPRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*), WORKR(*),WORKI(*)
!
!     WQRDC USES HOUSEHOLDER TRANSFORMATIONS TO COMPUTE THE QR
!     FACTORIZATION OF AN N BY P MATRIX X.  COLUMN PIVOTING
!     BASED ON THE 2-NORMS OF THE REDUCED COLUMNS MAY BE
!     PERFORMED AT THE USERS OPTION.
!
!     ON ENTRY
!
!        X       DOUBLE-COMPLEX(LDX,P), WHERE LDX .GE. N.
!                X CONTAINS THE MATRIX WHOSE DECOMPOSITION IS TO BE
!                COMPUTED.
!
!        LDX     INTEGER.
!                LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N       INTEGER.
!                N IS THE NUMBER OF ROWS OF THE MATRIX X.
!
!        P       INTEGER.
!                P IS THE NUMBER OF COLUMNS OF THE MATRIX X.
!
!        JPVT    INTEGER(P).
!                JPVT CONTAINS INTEGERS THAT CONTROL THE SELECTION
!                OF THE PIVOT COLUMNS.  THE K-TH COLUMN X(K) OF X
!                IS PLACED IN ONE OF THREE CLASSES ACCORDING TO THE
!                VALUE OF JPVT(K).
!
!                   IF JPVT(K) .GT. 0, THEN X(K) IS AN INITIAL
!                   COLUMN.
!
!                   IF JPVT(K) .EQ. 0, THEN X(K) IS A FREE COLUMN.
!
!                   IF JPVT(K) .LT. 0, THEN X(K) IS A FINAL COLUMN.
!
!                BEFORE THE DECOMPOSITION IS COMPUTED, INITIAL COLUMNS
!                ARE MOVED TO THE BEGINNING OF THE ARRAY X AND FINAL
!                COLUMNS TO THE END.  BOTH INITIAL AND FINAL COLUMNS
!                ARE FROZEN IN PLACE DURING THE COMPUTATION AND ONLY
!                FREE COLUMNS ARE MOVED.  AT THE K-TH STAGE OF THE
!                REDUCTION, IF X(K) IS OCCUPIED BY A FREE COLUMN
!                IT IS INTERCHANGED WITH THE FREE COLUMN OF LARGEST
!                REDUCED NORM.  JPVT IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        WORK    DOUBLE-COMPLEX(P).
!                WORK IS A WORK ARRAY.  WORK IS NOT REFERENCED IF
!                JOB .EQ. 0.
!
!        JOB     INTEGER.
!                JOB IS AN INTEGER THAT INITIATES COLUMN PIVOTING.
!                IF JOB .EQ. 0, NO PIVOTING IS DONE.
!                IF JOB .NE. 0, PIVOTING IS DONE.
!
!     ON RETURN
!
!        X       X CONTAINS IN ITS UPPER TRIANGLE THE UPPER
!                TRIANGULAR MATRIX R OF THE QR FACTORIZATION.
!                BELOW ITS DIAGONAL X CONTAINS INFORMATION FROM
!                WHICH THE UNITARY PART OF THE DECOMPOSITION
!                CAN BE RECOVERED.  NOTE THAT IF PIVOTING HAS
!                BEEN REQUESTED, THE DECOMPOSITION IS NOT THAT
!                OF THE ORIGINAL MATRIX X BUT THAT OF X
!                WITH ITS COLUMNS PERMUTED AS DESCRIBED BY JPVT.
!
!        QRAUX   DOUBLE-COMPLEX(P).
!                QRAUX CONTAINS FURTHER INFORMATION REQUIRED TO RECOVER
!                THE UNITARY PART OF THE DECOMPOSITION.
!
!        JPVT    JPVT(K) CONTAINS THE INDEX OF THE COLUMN OF THE
!                ORIGINAL MATRIX THAT HAS BEEN INTERCHANGED INTO
!                THE K-TH COLUMN, IF PIVOTING WAS REQUESTED.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     WQRDC USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS matX_waxpy,mat_pythag,mat_wdotcr,mat_wdotci,mat_wscal
!     blas mat_wswap ,mat_wnrm2
!     FORTRAN DABS,DIMAG,DMAX1,MIN0
!
!     INTERNAL VARIABLES
!
integer :: jj
      INTEGER J,JP,L,LP1,LUP,MAXJ,PL,PU
      DOUBLEPRECISION MAXNRM,TT
      DOUBLEPRECISION NRMXLR,NRMXLI,TR,TI
      LOGICAL NEGJ,SWAPJ
!
      DOUBLEPRECISION ZDUMR,ZDUMI
      DOUBLEPRECISION CABS1
      CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
      PL = 1
      PU = 0
      IF (JOB .EQ. 0) GOTO 60
!
!        PIVOTING HAS BEEN REQUESTED.  REARRANGE THE COLUMNS
!        ACCORDING TO JPVT.
!
         DO 20 J = 1, P
            SWAPJ = JPVT(J) .GT. 0
            NEGJ = JPVT(J) .LT. 0
            JPVT(J) = J
            IF (NEGJ) JPVT(J) = -J
            IF (.NOT.SWAPJ) GOTO 10
               IF (J .NE. PL) CALL mat_wswap(N,XR(1,PL),XI(1,PL),1,XR(1,J),XI(1,J),1)
               JPVT(J) = JPVT(PL)
               JPVT(PL) = J
               PL = PL + 1
   10       CONTINUE
   20    CONTINUE
         PU = P
         DO 50 JJ = 1, P
            J = P - JJ + 1
            IF (JPVT(J) .GE. 0) GOTO 40
               JPVT(J) = -JPVT(J)
               IF (J .EQ. PU) GOTO 30
                  CALL mat_wswap(N,XR(1,PU),XI(1,PU),1,XR(1,J),XI(1,J),1)
                  JP = JPVT(PU)
                  JPVT(PU) = JPVT(J)
                  JPVT(J) = JP
   30          CONTINUE
               PU = PU - 1
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
!
!     COMPUTE THE NORMS OF THE FREE COLUMNS.
!
      IF (PU .LT. PL) GOTO 80
      DO 70 J = PL, PU
         QRAUXR(J) = mat_wnrm2(N,XR(1,J),XI(1,J),1)
         QRAUXI(J) = 0.0D0
         WORKR(J) = QRAUXR(J)
         WORKI(J) = QRAUXI(J)
   70 CONTINUE
   80 CONTINUE
!
!     PERFORM THE HOUSEHOLDER REDUCTION OF X.
!
      LUP = MIN0(N,P)
      DO 210 L = 1, LUP
         IF (L .LT. PL .OR. L .GE. PU) GOTO 120
!
!           LOCATE THE COLUMN OF LARGEST NORM AND BRING IT
!           INTO THE PIVOT POSITION.
!
            MAXNRM = 0.0D0
            MAXJ = L
            DO J = L, PU
               IF (QRAUXR(J) .LE. MAXNRM) cycle
               MAXNRM = QRAUXR(J)
               MAXJ = J
            enddo
            IF (MAXJ .EQ. L) GOTO 110
              CALL mat_wswap(N,XR(1,L),XI(1,L),1,XR(1,MAXJ),XI(1,MAXJ),1)
              QRAUXR(MAXJ) = QRAUXR(L)
              QRAUXI(MAXJ) = QRAUXI(L)
              WORKR(MAXJ) = WORKR(L)
              WORKI(MAXJ) = WORKI(L)
              JP = JPVT(MAXJ)
              JPVT(MAXJ) = JPVT(L)
              JPVT(L) = JP
  110       CONTINUE
  120    CONTINUE
         QRAUXR(L) = 0.0D0
         QRAUXI(L) = 0.0D0
         IF (L .EQ. N) GOTO 200
!
!           COMPUTE THE HOUSEHOLDER TRANSFORMATION FOR COLUMN L.
!
            NRMXLR = mat_wnrm2(N-L+1,XR(L,L),XI(L,L),1)
            NRMXLI = 0.0D0
            IF (CABS1(NRMXLR,NRMXLI) .EQ. 0.0D0) GOTO 190
              IF (CABS1(XR(L,L),XI(L,L)) .EQ. 0.0D0) GOTO 130
              CALL mat_wsign(NRMXLR,NRMXLI,XR(L,L),XI(L,L),NRMXLR,NRMXLI)
  130         CONTINUE
              CALL mat_wdiv(1.0D0,0.0D0,NRMXLR,NRMXLI,TR,TI)
              CALL mat_wscal(N-L+1,TR,TI,XR(L,L),XI(L,L),1)
              XR(L,L) = mat_flop(1.0D0 + XR(L,L))
!
!             APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS,
!             UPDATING THE NORMS.
!
              LP1 = L + 1
              IF (P .LT. LP1) GOTO 180
              DO 170 J = LP1, P
                  TR = -mat_wdotcr(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  TI = -mat_wdotci(N-L+1,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  CALL mat_wdiv(TR,TI,XR(L,L),XI(L,L),TR,TI)
                  CALL matX_waxpy(N-L+1,TR,TI,XR(L,L),XI(L,L),1,XR(L,J), XI(L,J),1)
                  IF (J .LT. PL .OR. J .GT. PU) GOTO 160
                  IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 160
                    TT=1.0D0 - (mat_pythag(XR(L,J),XI(L,J))/QRAUXR(J))**2
                    TT=DMAX1(TT,0.0D0)
                    TR=mat_flop(TT)
                    TT=mat_flop(1.0D0+0.05D0*TT*(QRAUXR(J)/WORKR(J))**2)
                    IF (TT .EQ. 1.0D0) GOTO 140
                     QRAUXR(J) = QRAUXR(J)*DSQRT(TR)
                     QRAUXI(J) = QRAUXI(J)*DSQRT(TR)
                     GOTO 150
  140                CONTINUE
                     QRAUXR(J) = mat_wnrm2(N-L,XR(L+1,J),XI(L+1,J),1)
                     QRAUXI(J) = 0.0D0
                     WORKR(J) = QRAUXR(J)
                     WORKI(J) = QRAUXI(J)
  150                CONTINUE
  160             CONTINUE
  170          CONTINUE
  180          CONTINUE
!
!              SAVE THE TRANSFORMATION.
!
               QRAUXR(L) = XR(L,L)
               QRAUXI(L) = XI(L,L)
               XR(L,L) = -NRMXLR
               XI(L,L) = -NRMXLI
  190       CONTINUE
  200    CONTINUE
  210 CONTINUE
      END SUBROUTINE ML_WQRDC
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE ML_WQRSL(XR,XI,LDX,N,K,QRAUXR,QRAUXI,YR,YI,QYR,QYI,QTYR,QTYI,BR,BI,RSDR,RSDI,XBR,XBI,JOB,INFO)
use M_matrix
IMPLICIT NONE
INTEGER LDX,N,K,JOB,INFO
DOUBLEPRECISION XR(LDX,*),XI(LDX,*),QRAUXR(*),QRAUXI(*),YR(*),     &
   &                YI(*),QYR(*),QYI(*),QTYR(*),QTYI(*),BR(*),BI(*),   &
   &                RSDR(*),RSDI(*),XBR(*),XBI(*)
!
!     WQRSL APPLIES THE OUTPUT OF WQRDC TO COMPUTE COORDINATE
!     TRANSFORMATIONS, PROJECTIONS, AND LEAST SQUARES SOLUTIONS.
!     FOR K .LE. MIN(N,P), LET XK BE THE MATRIX
!
!            XK = (X(JPVT(1)),X(JPVT(2)), ... ,X(JPVT(K)))
!
!     FORMED FROM COLUMNS JPVT(1), ... ,JPVT(K) OF THE ORIGINAL
!     N X P MATRIX X THAT WAS INPUT TO WQRDC (IF NO PIVOTING WAS
!     DONE, XK CONSISTS OF THE FIRST K COLUMNS OF X IN THEIR
!     ORIGINAL ORDER).  WQRDC PRODUCES A FACTORED UNITARY MATRIX Q
!     AND AN UPPER TRIANGULAR MATRIX R SUCH THAT
!
!              XK = Q * (R)
!    (0)
!
!     THIS INFORMATION IS CONTAINED IN CODED FORM IN THE ARRAYS
!     X AND QRAUX.
!
!     ON ENTRY
!
!        X      DOUBLE-COMPLEX(LDX,P).
!               X CONTAINS THE OUTPUT OF WQRDC.
!
!        LDX    INTEGER.
!               LDX IS THE LEADING DIMENSION OF THE ARRAY X.
!
!        N      INTEGER.
!               N IS THE NUMBER OF ROWS OF THE MATRIX XK.  IT MUST
!               HAVE THE SAME VALUE AS N IN WQRDC.
!
!        K      INTEGER.
!               K IS THE NUMBER OF COLUMNS OF THE MATRIX XK.  K
!               MUST NNOT BE GREATER THAN MIN(N,P), WHERE P IS THE
!               SAME AS IN THE CALLING SEQUENCE TO WQRDC.
!
!        QRAUX  DOUBLE-COMPLEX(P).
!               QRAUX CONTAINS THE AUXILIARY OUTPUT FROM WQRDC.
!
!        Y      DOUBLE-COMPLEX(N)
!               Y CONTAINS AN N-VECTOR THAT IS TO BE MANIPULATED
!               BY WQRSL.
!
!        JOB    INTEGER.
!               JOB SPECIFIES WHAT IS TO BE COMPUTED.  JOB HAS
!               THE DECIMAL EXPANSION ABCDE, WITH THE FOLLOWING
!               MEANING.
!
! IF A.NE.0, COMPUTE QY.
! IF B,C,D, OR E .NE. 0, COMPUTE QTY.
! IF C.NE.0, COMPUTE B.
! IF D.NE.0, COMPUTE RSD.
! IF E.NE.0, COMPUTE XB.
!
!               NOTE THAT A REQUEST TO COMPUTE B, RSD, OR XB
!               AUTOMATICALLY TRIGGERS THE COMPUTATION OF QTY, FOR
!               WHICH AN ARRAY MUST BE PROVIDED IN THE CALLING
!               SEQUENCE.
!
!     ON RETURN
!
!        QY     DOUBLE-COMPLEX(N).
!               QY CONTAINS Q*Y, IF ITS COMPUTATION HAS BEEN
!               REQUESTED.
!
!        QTY    DOUBLE-COMPLEX(N).
!               QTY CONTAINS CTRANS(Q)*Y, IF ITS COMPUTATION HAS
!               BEEN REQUESTED.  HERE CTRANS(Q) IS THE CONJUGATE
!               TRANSPOSE OF THE MATRIX Q.
!
!        B      DOUBLE-COMPLEX(K)
!               B CONTAINS THE SOLUTION OF THE LEAST SQUARES PROBLEM
!
! MINIMIZE NORM2(Y - XK*B),
!
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  (NOTE THAT
!               IF PIVOTING WAS REQUESTED IN WQRDC, THE J-TH
!               COMPONENT OF B WILL BE ASSOCIATED WITH COLUMN JPVT(J)
!               OF THE ORIGINAL MATRIX X THAT WAS INPUT INTO WQRDC.)
!
!        RSD    DOUBLE-COMPLEX(N).
!               RSD CONTAINS THE LEAST SQUARES RESIDUAL Y - XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  RSD IS
!               ALSO THE ORTHOGONAL PROJECTION OF Y ONTO THE
!               ORTHOGONAL COMPLEMENT OF THE COLUMN SPACE OF XK.
!
!        XB     DOUBLE-COMPLEX(N).
!               XB CONTAINS THE LEAST SQUARES APPROXIMATION XK*B,
!               IF ITS COMPUTATION HAS BEEN REQUESTED.  XB IS ALSO
!               THE ORTHOGONAL PROJECTION OF Y ONTO THE COLUMN SPACE
!               OF X.
!
!        INFO   INTEGER.
!               INFO IS ZERO UNLESS THE COMPUTATION OF B HAS
!               BEEN REQUESTED AND R IS EXACTLY SINGULAR.  IN
!               THIS CASE, INFO IS THE INDEX OF THE FIRST ZERO
!               DIAGONAL ELEMENT OF R AND B IS LEFT UNALTERED.
!
!     THE PARAMETERS QY, QTY, B, RSD, AND XB ARE NOT REFERENCED
!     IF THEIR COMPUTATION IS NOT REQUESTED AND IN THIS CASE
!     CAN BE REPLACED BY DUMMY VARIABLES IN THE CALLING PROGRAM.
!     TO SAVE STORAGE, THE USER MAY IN SOME CASES USE THE SAME
!     ARRAY FOR DIFFERENT PARAMETERS IN THE CALLING SEQUENCE.  A
!     FREQUENTLY OCCURRING EXAMPLE IS WHEN ONE WISHES TO COMPUTE
!     ANY OF B, RSD, OR XB AND DOES NOT NEED Y OR QTY.  IN THIS
!     CASE ONE MAY IDENTIFY Y, QTY, AND ONE OF B, RSD, OR XB, WHILE
!     PROVIDING SEPARATE ARRAYS FOR ANYTHING ELSE THAT IS TO BE
!     COMPUTED.  THUS THE CALLING SEQUENCE
!
!          CALL ML_WQRSL(X,LDX,N,K,QRAUX,Y,DUM,Y,B,Y,DUM,110,INFO)
!
!     WILL RESULT IN THE COMPUTATION OF B AND RSD, WITH RSD
!     OVERWRITING Y.  MORE GENERALLY, EACH ITEM IN THE FOLLOWING
!     LIST CONTAINS GROUPS OF PERMISSIBLE IDENTIFICATIONS FOR
!     A SINGLE CALLING SEQUENCE.
!
!          1. (Y,QTY,B) (RSD) (XB) (QY)
!
!          2. (Y,QTY,RSD) (B) (XB) (QY)
!
!          3. (Y,QTY,XB) (B) (RSD) (QY)
!
!          4. (Y,QY) (QTY,B) (RSD) (XB)
!
!          5. (Y,QY) (QTY,RSD) (B) (XB)
!
!          6. (Y,QY) (QTY,XB) (B) (RSD)
!
!     IN ANY GROUP THE VALUE RETURNED IN THE ARRAY ALLOCATED TO
!     THE GROUP CORRESPONDS TO THE LAST MEMBER OF THE GROUP.
!
!     LINPACK. THIS VERSION DATED 07/03/79 .
!     G.W. STEWART, UNIVERSITY OF MARYLAND, ARGONNE NATIONAL LAB.
!
!     ML_WQRSL USES THE FOLLOWING FUNCTIONS AND SUBPROGRAMS.
!
!     BLAS matX_waxpy,WCOPY,mat_wdotcr,mat_wdotci
!     FORTRAN DABS,DIMAG,MIN0,MOD
!
!     INTERNAL VARIABLES
!
   INTEGER I,J,JJ,JU,KP1
   DOUBLEPRECISION TR,TI,TEMPR,TEMPI
   LOGICAL CB,CQY,CQTY,CR,CXB
!
   DOUBLEPRECISION ZDUMR,ZDUMI
   DOUBLEPRECISION CABS1
   CABS1(ZDUMR,ZDUMI) = DABS(ZDUMR) + DABS(ZDUMI)
!
!     SET INFO FLAG.
!
   INFO = 0
!
!     DETERMINE WHAT IS TO BE COMPUTED.
!
   CQY = JOB/10000 .NE. 0
   CQTY = MOD(JOB,10000) .NE. 0
   CB = MOD(JOB,1000)/100 .NE. 0
   CR = MOD(JOB,100)/10 .NE. 0
   CXB = MOD(JOB,10) .NE. 0
   JU = MIN0(K,N-1)
!
!     SPECIAL ACTION WHEN N=1.
!
   IF (JU .NE. 0) GOTO 80
   IF (.NOT.CQY) GOTO 10
   QYR(1) = YR(1)
   QYI(1) = YI(1)
10 CONTINUE
   IF (.NOT.CQTY) GOTO 20
   QTYR(1) = YR(1)
   QTYI(1) = YI(1)
20 CONTINUE
   IF (.NOT.CXB) GOTO 30
   XBR(1) = YR(1)
   XBI(1) = YI(1)
30 CONTINUE
   IF (.NOT.CB) GOTO 60
   IF (CABS1(XR(1,1),XI(1,1)) .NE. 0.0D0) GOTO 40
   INFO = 1
   GOTO 50
40 CONTINUE
   CALL mat_wdiv(YR(1),YI(1),XR(1,1),XI(1,1),BR(1),BI(1))
50 CONTINUE
60 CONTINUE
   IF (.NOT.CR) GOTO 70
   RSDR(1) = 0.0D0
   RSDI(1) = 0.0D0
70 CONTINUE
   GOTO 290
80 CONTINUE
!
!        SET UP TO COMPUTE QY OR QTY.
!
   IF (CQY) CALL mat_wcopy(N,YR,YI,1,QYR,QYI,1)
   IF (CQTY) CALL mat_wcopy(N,YR,YI,1,QTYR,QTYI,1)
   IF (.NOT.CQY) GOTO 110
!
!           COMPUTE QY.
!
   DO JJ = 1, JU
      J = JU - JJ + 1
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 90
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      TR=-mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
      TI=-mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,QYR(J),QYI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QYR(J), QYI(J),1)
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
90    CONTINUE
   enddo
110 CONTINUE
   IF (.NOT.CQTY) GOTO 140
!
!           COMPUTE CTRANS(Q)*Y.
!
   DO J = 1, JU
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 120
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,QTYR(J), QTYI(J),1)
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
120   CONTINUE
   enddo
140 CONTINUE
!
!        SET UP TO COMPUTE B, RSD, OR XB.
!
   IF (CB) CALL mat_wcopy(K,QTYR,QTYI,1,BR,BI,1)
   KP1 = K + 1
   IF (CXB) CALL mat_wcopy(K,QTYR,QTYI,1,XBR,XBI,1)
   IF (CR .AND. K .LT. N)CALL mat_wcopy(N-K,QTYR(KP1),QTYI(KP1),1,RSDR(KP1),RSDI(KP1),1)
   IF (.NOT.CXB .OR. KP1 .GT. N) GOTO 160
   DO I = KP1, N
      XBR(I) = 0.0D0
      XBI(I) = 0.0D0
   enddo
160 CONTINUE
   IF (.NOT.CR) GOTO 180
   DO I = 1, K
      RSDR(I) = 0.0D0
      RSDI(I) = 0.0D0
   enddo
180 CONTINUE
   IF (.NOT.CB) GOTO 230
!
!           COMPUTE B.
!
   DO JJ = 1, K
      J = K - JJ + 1
      IF (CABS1(XR(J,J),XI(J,J)) .NE. 0.0D0) GOTO 190
      INFO = J
!                 ......EXIT
!           ......EXIT
      GOTO 220
190   CONTINUE
      CALL mat_wdiv(BR(J),BI(J),XR(J,J),XI(J,J),BR(J),BI(J))
      IF (J .EQ. 1) GOTO 200
      TR = -BR(J)
      TI = -BI(J)
      CALL matX_waxpy(J-1,TR,TI,XR(1,J),XI(1,J),1,BR,BI,1)
200   CONTINUE
   enddo
220 CONTINUE
230 CONTINUE
   IF (.NOT.CR .AND. .NOT.CXB) GOTO 280
!
!           COMPUTE RSD OR XB AS REQUIRED.
!
   DO JJ = 1, JU
      J = JU - JJ + 1
      IF (CABS1(QRAUXR(J),QRAUXI(J)) .EQ. 0.0D0) GOTO 260
      TEMPR = XR(J,J)
      TEMPI = XI(J,J)
      XR(J,J) = QRAUXR(J)
      XI(J,J) = QRAUXI(J)
      IF (.NOT.CR) GOTO 240
      TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
      TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,RSDR(J), RSDI(J),1)
240   CONTINUE
      IF (.NOT.CXB) GOTO 250
      TR = -mat_wdotcr(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
      TI = -mat_wdotci(N-J+1,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
      CALL mat_wdiv(TR,TI,XR(J,J),XI(J,J),TR,TI)
      CALL matX_waxpy(N-J+1,TR,TI,XR(J,J),XI(J,J),1,XBR(J), XBI(J),1)
250   CONTINUE
      XR(J,J) = TEMPR
      XI(J,J) = TEMPI
260   CONTINUE
   enddo
280 CONTINUE
290 CONTINUE
END SUBROUTINE ML_WQRSL
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

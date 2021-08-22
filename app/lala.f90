!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program bigmat
use M_matrix, only  : lala
use M_CLI2, only : set_args, lget, sget, iget, expressions=>unnamed          ! add command-line parser module
implicit none
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
integer                      :: i
logical                      :: echo
logical                      :: markdown
   call setup()
   ! define command arguments,default values and crack command line
   call set_args('lala --markdown F -noecho F',help_text,version_text )
   echo=.not.lget('noecho')
   if(size(expressions).eq.0)then
      call lala(echo=echo)                           ! CALL LALA interactively with default scratch space
   else
      call lala(200000,echo=echo)                    ! CALL LALA to initialize it and set scratch space size
      do i=1,size(expressions)
         call lala(expressions(i))                   ! CALL LALA
      enddo
   endif
   stop
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   lala(1f) - interpret matrix expressions using a shell-like interface',&
'',&
'SYNOPSIS',&
'    lala [expression(s)] | [ --help| --version]',&
'',&
'DESCRIPTION',&
'   lala(1) is an interactive computer program that serves as a convenient',&
'   "laboratory" for computations involving matrices. It provides easy',&
'   access to matrix software developed by the LINPACK and EISPACK',&
'   projects. The capabilities range from standard tasks such as solving',&
'   simultaneous linear equations and inverting matrices, through symmetric',&
'   and nonsymmetric eigenvalue problems, to fairly sophisticated matrix',&
'   tools such as the singular value decomposition.',&
'',&
'OPTIONS',&
'    --help         display this help and exit',&
'    --version      output version information and exit',&
'    expression(s)  if expressions are supplied they are evaluated and the',&
'                   program terminates.',&
'',&
'AUTHOR',&
'    This is heavily based on a program from the Department of Computer',&
'    Science, University of New Mexico, by Cleve Moler.',&
'',&
'EXAMPLES',&
'  Sample commands',&
'',&
'    # Example 1: introductory usage:',&
'    lala',&
'    a=<1 2 3;5 4 6;7 8 9>',&
'    b=<5;6;7>',&
'    a*b',&
'    b*a',&
'    det(a)',&
'    quit',&
'',&
'   An explanation of Example 1:',&
'',&
'    // For this session the <> character is the LALA prompt.',&
'     <> A=<1 2 3;5 4 6;7 8 9>            <---  you enter this',&
'     A     =                             <---  LALA response',&
'         1.    2.    3.',&
'         5.    4.    6.',&
'         7.    8.    9.',&
'     <> b=<5;6;7>',&
'     b     =',&
'         5.',&
'         6.',&
'         7.',&
'',&
'     <> A*b             <--- you enter "multiply A and b"',&
'',&
'     ANS   =            <--- LALA response',&
'        38.',&
'        91.',&
'       146.',&
'',&
'     <> b*A             <---you enter "multiply b and A"',&
'        /--ERROR                         <--- LALA response',&
'     INCOMPATIBLE FOR MULTIPLICATION',&
'',&
'     <> det(A)         <--- Take the determinant of A',&
'',&
'     ANS   =           <---LALA response',&
'',&
'        18.',&
'',&
'     <> quit           <--- you quit LALA',&
'',&
'     total flops        34',&
'     ADIOS',&
'    // --------------------------------------',&
'',&
'   Example 2: Simple looping and conditionals are also available',&
'',&
'    lala',&
'    //Eigenvalue sensitivity example. See section 8 of the Users'' Guide.',&
'    B = <3 0 7; 0 2 0; 0 0 1>',&
'    L = <1 0 0; 2 1 0; -3 4 1>,  M = L\L''',&
'    A = M*B/M',&
'    A = round(A)',&
'    <X,D> = eig(A)',&
'    long,  diag(D),  short',&
'    cond(X)',&
'    X = X/diag(X(3,:)),  cond(X)',&
'    Y = inv(X''),  Y''*A*X',&
'    for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j));',&
'    C',&
'    E = -1.e-6*Y(:,1)*X(:,1)''',&
'    eig(A + .4*E),  eig(A + .5*E)',&
'    r = .4;  s = .5;',&
'    while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...',&
'      if imag(d(1))=0, r = t; else, s = t;',&
'    long,  t = r',&
'    A+t*e,  eig(A+t*E)',&
'    <X,D> = eig(A+t*E);  X = X/diag(X(3,:))',&
'    short,  cond(X)',&
'    // --------------------------------------',&
'',&
'   Use the HELP command for further information.',&
'   For example, to enter HELP on the entire manual,',&
'   display directions for using HELP and',&
'   place a User manual in the file "lala.userguide.txt",',&
'   enter',&
'',&
'    lala',&
'    <>help manual',&
'    continue ...',&
'    h // show directions for using "help"',&
'    w lala.userguide.txt',&
'    continue ...',&
'    q',&
'    quit',&
'']
!>
!!##NAME
!!    lala(1f) - interpret matrix expressions using a shell-like interface
!!
!!##SYNOPSIS
!!
!!     lala [expression(s)] | [ --help| --version]
!!
!!##DESCRIPTION
!!    lala(1) is an interactive computer program that serves as a convenient
!!    "laboratory" for computations involving matrices. It provides easy
!!    access to matrix software developed by the LINPACK and EISPACK
!!    projects. The capabilities range from standard tasks such as solving
!!    simultaneous linear equations and inverting matrices, through symmetric
!!    and nonsymmetric eigenvalue problems, to fairly sophisticated matrix
!!    tools such as the singular value decomposition.
!!
!!##OPTIONS
!!     --help         display this help and exit
!!     --version      output version information and exit
!!     expression(s)  if expressions are supplied they are evaluated and the
!!                    program terminates.
!!
!!##AUTHOR
!!     This is heavily based on a program from the Department of Computer
!!     Science, University of New Mexico, by Cleve Moler.
!!
!!##EXAMPLES
!!
!!   Sample commands
!!
!!     # Example 1: introductory usage:
!!     lala
!!     a=<1 2 3;5 4 6;7 8 9>
!!     b=<5;6;7>
!!     a*b
!!     b*a
!!     det(a)
!!     quit
!!
!!    An explanation of Example 1:
!!
!!     // For this session the <> character is the LALA prompt.
!!      <> A=<1 2 3;5 4 6;7 8 9>            <---  you enter this
!!      A     =                             <---  LALA response
!!          1.    2.    3.
!!          5.    4.    6.
!!          7.    8.    9.
!!      <> b=<5;6;7>
!!      b     =
!!          5.
!!          6.
!!          7.
!!
!!      <> A*b             <--- you enter "multiply A and b"
!!
!!      ANS   =            <--- LALA response
!!         38.
!!         91.
!!        146.
!!
!!      <> b*A             <---you enter "multiply b and A"
!!         /--ERROR                         <--- LALA response
!!      INCOMPATIBLE FOR MULTIPLICATION
!!
!!      <> det(A)         <--- Take the determinant of A
!!
!!      ANS   =           <---LALA response
!!
!!         18.
!!
!!      <> quit           <--- you quit LALA
!!
!!      total flops        34
!!      ADIOS
!!     // --------------------------------------
!!
!!    Example 2: Simple looping and conditionals are also available
!!
!!     lala
!!     //Eigenvalue sensitivity example. See section 8 of the Users' Guide.
!!     B = <3 0 7; 0 2 0; 0 0 1>
!!     L = <1 0 0; 2 1 0; -3 4 1>,  M = L\L'
!!     A = M*B/M
!!     A = round(A)
!!     <X,D> = eig(A)
!!     long,  diag(D),  short
!!     cond(X)
!!     X = X/diag(X(3,:)),  cond(X)
!!     Y = inv(X'),  Y'*A*X
!!     for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j));
!!     C
!!     E = -1.e-6*Y(:,1)*X(:,1)'
!!     eig(A + .4*E),  eig(A + .5*E)
!!     r = .4;  s = .5;
!!     while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...
!!       if imag(d(1))=0, r = t; else, s = t;
!!     long,  t = r
!!     A+t*e,  eig(A+t*E)
!!     <X,D> = eig(A+t*E);  X = X/diag(X(3,:))
!!     short,  cond(X)
!!     // --------------------------------------
!!
!!    Use the HELP command for further information.
!!    For example, to enter HELP on the entire manual,
!!    display directions for using HELP and
!!    place a User manual in the file "lala.userguide.txt",
!!    enter
!!
!!     lala
!!     <>help manual
!!     continue ...
!!     h // show directions for using "help"
!!     w lala.userguide.txt
!!     continue ...
!!     q
!!     quit
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        lala(1)',&
'DESCRIPTION:    interpret matrix operations using a shell-like interface',&
'VERSION:        1.0, 19910403',&
'AUTHOR:         John S. Urban; heavily based on the original by Cleve Moler',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html',&
'']
end subroutine setup
end program bigmat
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

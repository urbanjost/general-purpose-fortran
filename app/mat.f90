!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   mat(1f) - interpret matrix expressions using a shell-like interface          ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    mat [ --help| --version]                                                    ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   mat(1) is an interactive computer program that serves as a convenient        ',&
'   "laboratory" for computations involving matrices. It provides easy access    ',&
'   to matrix software developed by the LINPACK and EISPACK projects. The        ',&
'   capabilities range from standard tasks such as solving simultaneous linear   ',&
'   equations and inverting matrices, through symmetric and nonsymmetric         ',&
'   eigenvalue problems, to fairly sophisticated matrix tools such as the        ',&
'   singular value decomposition.                                                ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    --help     display this help and exit                                       ',&
'    --version  output version information and exit                              ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'    This is heavily based on a program from the Department of Computer Science, ',&
'    University of New Mexico, by Cleve Moler.                                   ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'  Sample commands                                                               ',&
'                                                                                ',&
'   # Example 1: introductory usage:                                             ',&
'   mat                                                                          ',&
'   a=<1 2 3;5 4 6;7 8 9>                                                        ',&
'   b=<5;6;7>                                                                    ',&
'   a*b                                                                          ',&
'   b*a                                                                          ',&
'   det(a)                                                                       ',&
'   quit                                                                         ',&
'                                                                                ',&
'  An explanation of Example 1:                                                  ',&
'                                                                                ',&
'   // For this session the <> character is the MATLAB prompt.                   ',&
'    <> a=<1 2 3;5 4 6;7 8 9>            <---  you enter this                    ',&
'                                                                                ',&
'    A     =                             <---  MATLAB response                   ',&
'                                                                                ',&
'         1.    2.    3.                                                         ',&
'        5.    4.    6.                                                          ',&
'        7.    8.    9.                                                          ',&
'                                                                                ',&
'    <> b=<5;6;7>                        <--- you enter this                     ',&
'                                                                                ',&
'    B     =                             <--- MATLAB response                    ',&
'                                                                                ',&
'        5.                                                                      ',&
'        6.                                                                      ',&
'        7.                                                                      ',&
'                                                                                ',&
'    <> a*b             <--- you enter "multiply a and b"                        ',&
'                                                                                ',&
'    ANS   =            <--- MATLAB response                                     ',&
'                                                                                ',&
'       38.                                                                      ',&
'       91.                                                                      ',&
'      146.                                                                      ',&
'                                                                                ',&
'    <> b*a             <---you enter "multiply b and a"                         ',&
'       /--ERROR                         <--- MATLAB response                    ',&
'    INCOMPATIBLE FOR MULTIPLICATION                                             ',&
'                                                                                ',&
'    <> det(a)         <--- Take the determinant of a                            ',&
'                                                                                ',&
'    ANS   =           <---MATLAB response                                       ',&
'                                                                                ',&
'       18.                                                                      ',&
'                                                                                ',&
'    <> quit           <--- you quit MATLAB                                      ',&
'                                                                                ',&
'    total flops        34                                                       ',&
'    ADIOS                                                                       ',&
'   // ----------------------------------------------------------------------------',&
'                                                                                ',&
' Example 2: Simple looping and conditionals are also available                  ',&
'                                                                                ',&
'   mat                                                                          ',&
'   //Eigenvalue sensitivity example. See section 8 of the Users'' Guide.        ',&
'   B = <3 0 7; 0 2 0; 0 0 1>                                                    ',&
'   L = <1 0 0; 2 1 0; -3 4 1>,  M = L\L''                                       ',&
'   A = M*B/M                                                                    ',&
'   A = round(A)                                                                 ',&
'   <X,D> = eig(A)                                                               ',&
'   long,  diag(D),  short                                                       ',&
'   cond(X)                                                                      ',&
'   X = X/diag(X(3,:)),  cond(X)                                                 ',&
'   Y = inv(X''),  Y''*A*X                                                       ',&
'   for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j));                               ',&
'   C                                                                            ',&
'   E = -1.e-6*Y(:,1)*X(:,1)''                                                   ',&
'   eig(A + .4*E),  eig(A + .5*E)                                                ',&
'   r = .4;  s = .5;                                                             ',&
'   while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...                         ',&
'     if imag(d(1))=0, r = t; else, s = t;                                       ',&
'   long,  t = r                                                                 ',&
'   A+t*e,  eig(A+t*E)                                                           ',&
'   <X,D> = eig(A+t*E);  X = X/diag(X(3,:))                                      ',&
'   short,  cond(X)                                                             ',&
'   // ----------------------------------------------------------------------------',&
'                                                                                ',&
'   Use the HELP command and the DOC command for further information.            ',&
'   For example:                                                                 ',&
'                                                                                ',&
'      mat                                                                       ',&
'      <>doc(''mat.txt'')                                                        ',&
'      <>quit                                                                    ',&
'                                                                                ',&
'   will place a User manual in the file "mat.txt".                              ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    mat(1f) - interpret matrix expressions using a shell-like interface
!!
!!##SYNOPSIS
!!
!!     mat [ --help| --version]
!!
!!##DESCRIPTION
!!    mat(1) is an interactive computer program that serves as a convenient
!!    "laboratory" for computations involving matrices. It provides easy access
!!    to matrix software developed by the LINPACK and EISPACK projects. The
!!    capabilities range from standard tasks such as solving simultaneous linear
!!    equations and inverting matrices, through symmetric and nonsymmetric
!!    eigenvalue problems, to fairly sophisticated matrix tools such as the
!!    singular value decomposition.
!!
!!##OPTIONS
!!     --help     display this help and exit
!!     --version  output version information and exit
!!
!!##AUTHOR
!!     This is heavily based on a program from the Department of Computer Science,
!!     University of New Mexico, by Cleve Moler.
!!
!!##EXAMPLES
!!
!!   Sample commands
!!
!!    # Example 1: introductory usage:
!!    mat
!!    a=<1 2 3;5 4 6;7 8 9>
!!    b=<5;6;7>
!!    a*b
!!    b*a
!!    det(a)
!!    quit
!!
!!   An explanation of Example 1:
!!
!!    // For this session the <> character is the MATLAB prompt.
!!     <> a=<1 2 3;5 4 6;7 8 9>            <---  you enter this
!!
!!     A     =                             <---  MATLAB response
!!
!!          1.    2.    3.
!!         5.    4.    6.
!!         7.    8.    9.
!!
!!     <> b=<5;6;7>                        <--- you enter this
!!
!!     B     =                             <--- MATLAB response
!!
!!         5.
!!         6.
!!         7.
!!
!!     <> a*b             <--- you enter "multiply a and b"
!!
!!     ANS   =            <--- MATLAB response
!!
!!        38.
!!        91.
!!       146.
!!
!!     <> b*a             <---you enter "multiply b and a"
!!        /--ERROR                         <--- MATLAB response
!!     INCOMPATIBLE FOR MULTIPLICATION
!!
!!     <> det(a)         <--- Take the determinant of a
!!
!!     ANS   =           <---MATLAB response
!!
!!        18.
!!
!!     <> quit           <--- you quit MATLAB
!!
!!     total flops        34
!!     ADIOS
!!    // ----------------------------------------------------------------------------
!!
!!  Example 2: Simple looping and conditionals are also available
!!
!!    mat
!!    //Eigenvalue sensitivity example. See section 8 of the Users' Guide.
!!    B = <3 0 7; 0 2 0; 0 0 1>
!!    L = <1 0 0; 2 1 0; -3 4 1>,  M = L\L'
!!    A = M*B/M
!!    A = round(A)
!!    <X,D> = eig(A)
!!    long,  diag(D),  short
!!    cond(X)
!!    X = X/diag(X(3,:)),  cond(X)
!!    Y = inv(X'),  Y'*A*X
!!    for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j));
!!    C
!!    E = -1.e-6*Y(:,1)*X(:,1)'
!!    eig(A + .4*E),  eig(A + .5*E)
!!    r = .4;  s = .5;
!!    while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...
!!      if imag(d(1))=0, r = t; else, s = t;
!!    long,  t = r
!!    A+t*e,  eig(A+t*E)
!!    <X,D> = eig(A+t*E);  X = X/diag(X(3,:))
!!    short,  cond(X)
!!    // ----------------------------------------------------------------------------
!!
!!    Use the HELP command and the DOC command for further information.
!!    For example:
!!
!!       mat
!!       <>doc('mat.txt')
!!       <>quit
!!
!!    will place a User manual in the file "mat.txt".
subroutine help_version(l_version)
implicit none
! @(#)help_version(3f): prints version information
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        mat(1)>',&
'@(#)DESCRIPTION:    interpret matrix operations using a shell-like interface>',&
'@(#)VERSION:        1.0, 19910403>',&
'@(#)AUTHOR:         John S. Urban; heavily based on the original by Cleve Moler>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Tue, Mar 9th, 2021 8:04:09 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
PROGRAM BIGMAT
use M_matrix, only  : mat88
use M_kracken, only : kracken, lget, sget, iget                  ! add command-line parser module
implicit none
   call kracken('mat','-help .false. -version .false.' )            ! define command arguments,default values and crack command line
   call help_usage(lget('mat_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('mat_version'))                           ! if -version option is present, display version text and exit
!   CALL MATLAB
   CALL mat88(0,' ')
   STOP
END PROGRAM BIGMAT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

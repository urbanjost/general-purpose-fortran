!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program bigmat
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64
use,intrinsic :: iso_fortran_env, only : sp=>real32, dp=>real64
use M_matrix, only  : lala
use M_CLI2, only : set_args, lget, sget, iget, expressions=>unnamed, specified          ! add command-line parser module
implicit none
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
character(len=256)           :: line
character(len=256)           :: topic
integer                      :: i
integer                      :: storage
logical                      :: echo
logical                      :: markdown
   call setup()
   ! define command arguments,default values and crack command line
   call set_args('--markdown F --topic:t " " --noecho F --storage:s 400000',help_text,version_text )
   echo=.not.lget('noecho')
   storage=iget('storage')
   topic=sget('topic')

   if(specified('topic').and.topic.eq.'')topic='help'

   if(topic.ne.' ')then
           write(line,'(*(g0))')"help ",trim(topic),";"
      call lala([character(len=256) ::'lines(9999999);',line ],echo=.FALSE.)
      stop
   endif
   if(size(expressions).eq.0)then
      call lala(storage)                   ! CALL LALA interactively
      call lala()                          ! CALL LALA interactively
   else
      call lala(storage,echo=echo)         ! CALL LALA to initialize it and set scratch space size
      do i=1,size(expressions)
         call lala(expressions(i))         ! CALL LALA
      enddo
   endif
   stop
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   lala(1f) - interpret matrix expressions using a shell-like interface',&
'                                                                       ',&
'SYNOPSIS                                                               ',&
'    lala [expression(s)] [--storage values]|--topic TOPIC|[--help|--version]',&
'                                                                            ',&
'DESCRIPTION                                                                 ',&
'   lala(1) is an interactive computer program that serves as a convenient   ',&
'   "laboratory" for computations involving matrices. It provides easy       ',&
'   access to matrix software developed by the LINPACK and EISPACK           ',&
'   projects. The capabilities range from standard tasks such as solving     ',&
'   simultaneous linear equations and inverting matrices, through symmetric  ',&
'   and nonsymmetric eigenvalue problems, to fairly sophisticated matrix     ',&
'   tools such as the singular value decomposition.                          ',&
'                                                                            ',&
'OPTIONS                                                                     ',&
'   expression(s)     if expressions are supplied they are evaluated         ',&
'                     and the program terminates.                            ',&
'   -s,--storage NNN  number of 64-bit words to use for storage. Defaults    ',&
'                     to 400000.                                             ',&
'   -t,--topic TOPIC  display help for specified topic and exit              ',&
'   -h,--help         display this help and exit                             ',&
'   -v,--version      output version information and exit                    ',&
'                                                                            ',&
'AUTHOR                                                                      ',&
'   This is heavily based on a program from the Department of Computer       ',&
'   Science, University of New Mexico, by Cleve Moler.                       ',&
'                                                                            ',&
'EXAMPLES                                                                    ',&
'  Sample commands                                                           ',&
'                                                                            ',&
'    lala --topic manual # display manual and exit                           ',&
'                                                                            ',&
'    # Example 1: introductory usage:                                        ',&
'    lala                                                                    ',&
'    A=<1 2 3;5 4 6;7 8 9>                                                   ',&
'    b=<5;6;7>                                                               ',&
'    A*b                                                                     ',&
'    b*A                                                                     ',&
'    det(A)                                                                  ',&
'    quit                                                                    ',&
'                                                                            ',&
'   An explanation of Example 1:                                             ',&
'                                                                            ',&
'    // For this session the <> character is the LALA prompt.                ',&
'     <> A=<1 2 3;5 4 6;7 8 9>            <---  you enter this               ',&
'     A     =                             <---  LALA response                ',&
'         1.    2.    3.                                                     ',&
'         5.    4.    6.                                                     ',&
'         7.    8.    9.                                                     ',&
'     <> b=<5;6;7>                                                           ',&
'     b     =                                                                ',&
'         5.                                                                 ',&
'         6.                                                                 ',&
'         7.                                                                 ',&
'                                                                            ',&
'     <> A*b             <--- you enter "multiply A and b"                   ',&
'                                                                            ',&
'     ANS   =            <--- LALA response                                  ',&
'        38.                                                                 ',&
'        91.                                                                 ',&
'       146.                                                                 ',&
'                                                                            ',&
'     <> b*A             <---you enter "multiply b and A"                    ',&
'        /--ERROR                         <--- LALA response                 ',&
'     INCOMPATIBLE FOR MULTIPLICATION                                        ',&
'                                                                            ',&
'     <> det(A)         <--- Take the determinant of A                       ',&
'                                                                            ',&
'     ANS   =           <---LALA response                                    ',&
'                                                                            ',&
'        18.                                                                 ',&
'                                                                            ',&
'     <> quit           <--- you quit LALA                                   ',&
'                                                                            ',&
'     total flops        34                                                  ',&
'     ADIOS                                                                  ',&
'    // --------------------------------------                               ',&
'                                                                            ',&
'   Example 2: Simple looping and conditionals are also available            ',&
'                                                                            ',&
'    lala                                                                    ',&
'    //Eigenvalue sensitivity example. See section 8 of the Users'' Guide.   ',&
'    B = <3 0 7; 0 2 0; 0 0 1>                                               ',&
'    L = <1 0 0; 2 1 0; -3 4 1>,  M = L\L''                                  ',&
'    A = M*B/M                                                               ',&
'    A = round(A)                                                            ',&
'    <X,D> = eig(A)                                                          ',&
'    long,  diag(D),  short                                                  ',&
'    cond(X)                                                                 ',&
'    X = X/diag(X(3,:)),  cond(X)                                            ',&
'    Y = inv(X''),  Y''*A*X                                                  ',&
'    for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j));                          ',&
'    C                                                                       ',&
'    E = -1.e-6*Y(:,1)*X(:,1)''                                              ',&
'    eig(A + .4*E),  eig(A + .5*E)                                           ',&
'    r = .4;  s = .5;                                                        ',&
'    while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...                    ',&
'      if imag(d(1))=0, r = t; else, s = t;                                  ',&
'    long,  t = r                                                            ',&
'    A+t*e,  eig(A+t*E)                                                      ',&
'    <X,D> = eig(A+t*E);  X = X/diag(X(3,:))                                 ',&
'    short,  cond(X)                                                         ',&
'    // --------------------------------------                               ',&
'                                                                            ',&
'   Use the HELP command for further information.                            ',&
'   For example, to enter HELP on the entire manual,                         ',&
'   display directions for using HELP and                                    ',&
'   place a User manual in the file "lala.userguide.txt",                    ',&
'   enter                                                                    ',&
'                                                                            ',&
'    lala                                                                    ',&
'    <>help manual                                                           ',&
'    continue ...                                                            ',&
'    h // show directions for using "help"                                   ',&
'    w lala.userguide.txt                                                    ',&
'    continue ...                                                            ',&
'    q                                                                       ',&
'    quit                                                                    ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        lala(1)                                             ',&
'DESCRIPTION:    interpret matrix operations using a shell-like interface',&
'VERSION:        1.1, 2025-04-14                                         ',&
'AUTHOR:         John S. Urban; heavily based on the original by Cleve Moler',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html             ',&
'']
end subroutine setup
end program bigmat
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!

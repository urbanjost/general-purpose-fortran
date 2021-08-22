










!>
!!##NAME
!!    M_matrix(3f) - [M_matrix] The Los Alamos-inspired Linear Algebra Fortran Facility (LALA)
!!    LICENSE(MIT)
!!
!!##DESCRIPTION
!! The M_matrix module contains the Linear Algebra Fortran Facility (LALA)
!! which allows for interacting with a Fortran program using Matlab
!! or Octave-like commands.  LALA is also usable as a simple one-line
!! language. It is a WIP (Work In Progress) but is already useful.
!!
!!   * You can pass intrinsic-type data easily between your Fortran
!!     program and the LALA utility.
!!   * blocks of LALA commands may be passed to lala(3f) as well.
!!   * external files containing lala(3f) commands may be read to create
!!     data or as configuration files.
!!   * LALA commands may be recorded and played back.
!!   * a command-line based command history allowed for recalling and editing
!!     input.
!!   * a stand-alone program lets you create and test LALA files. It is
!!     a flexible calculator utility all by itself.
!!   * a built-in help command describes the many functions and commands
!!   * a user-added Fortran routine may be called via the USER() function.
!!
!! All together, this allows lala(3f) to be used for self-describing
!! configuration and data files, inspecting data in existing programs,
!! transferring small amounts of data between programs or assisting in
!! debugging and development, unit testing and macro-level timing.
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_M_matrix
!!     use M_matrix, only : lala, put_into_lala, get_from_lala, ifin_lala
!!     !real,allocatable             :: r
!!     !complex,allocatable          :: cvec(:)
!!     integer,allocatable          :: iarr(:,:)
!!     character(len=:),allocatable :: t(:)
!!     integer                      :: ierr
!!
!!     ! store some data into lala(3)
!!     call put_into_lala('A',[1,2,3,4,5]*10.5,ierr)
!!     write(*,*)'is A defined in LALA?',ifin_lala('A')
!!     call lala('A/2.0')
!!
!!     ! pass some commands to lala(3f)
!!     call lala([character(len=80) :: &
!!     &'PI=atan(1)*4               ', &
!!     &"mytitle='this is my title';", &
!!     &'littlearray=<              ', &
!!     &'   1 2 3;                  ', &
!!     &'   4 5 6;                  ', &
!!     &'   7 8 9;                  ', &
!!     &'>                          ', &
!!     &'S=sum(A)                   ', &
!!     &'I=inv(littlearray);        ', &
!!     &'B=littlearray*sin(PI/3)    ', &
!!     &"save('keepB',B)            ", &
!!     &''])
!!
!!     ! read a file containing lala(3f) commands
!!     call lala("exec('mycommands');")
!!
!!     ! interactively interact with lala(3f) interpreter
!!     call lala()
!!
!!     ! get some data from LALA into the calling program
!!     call get_from_lala('littlearray',iarr,ierr)
!!     write(*,'(a)')'IN CALLING PROGRAM IARR='
!!     write(*,'(1x,*(g0,1x))')(IARR(i,:),new_line('A'),i=1,size(iarr,dim=1))
!!
!!     call get_from_lala('mytitle',t,ierr)
!!     write(*,*)'IN CALLING PROGRAM T=',t
!!
!!     end program demo_M_matrix
module M_matrix

use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

use M_strings, only     : value_to_string, lower, v2s, s2v
use M_journal, only     : journal
use M_help, only        : help_command
use M_history, only     : redo
use M_list, only        : insert, locate, replace, remove
use M_io, only          : lookfor
use M_intrinsics, only  : help_intrinsics

use M_LA, only : mat_flop,   mat_inverse_hilbert,  mat_iwamax,  mat_magic,   mat_pythag,  mat_rat,    mat_round,  mat_rref
use M_LA, only : mat_rrot,   mat_rrotg,            mat_rset,    mat_rswap,   mat_urand,   mat_wasum,  mat_wcopy
use M_LA, only : mat_wdotci,           mat_wdotcr,  mat_wdotui,  mat_wdotur, mat_wmul,   mat_wnrm2
use M_LA, only : mat_wpofa,  mat_wrscal,           mat_wscal,   mat_wset,    mat_wsign,   mat_wsqrt,  mat_wswap

!use M_LA, only : mat_wdiv,   mat_wlog, mat_watan

!matx_waxpy,ml_comqr3,ml_corth,ml_htribk,ml_htridi,ml_imtql2,ml_wgeco,ml_wgedi,ml_wgefa,ml_wgesl,ml_wqrdc,ml_wqrsl,ml_wsvdc_
!>
!!##SYNTAX DIAGRAMS (9)
!!
!!    A formal description of the language acceptable to LALA, as well as
!!    a flow chart of the lala program, is provided by the syntax diagrams
!!    or syntax graphs of wirth [6]. There are eleven non-terminal symbols
!!    in the language:
!!
!!       LINE, STATEMENT, CLAUSE, EXPRESSION, TERM,
!!       FACTOR, NUMBER, INTEGER, NAME, COMMAND, TEXT .
!!
!!    The diagrams define each of the non-terminal symbols using the others
!!    and the terminal symbols:
!!
!!       LETTER -- A THROUGH Z,
!!       DIGIT  -- 0 THROUGH 9,
!!       CHAR   -- ( ) ; : + - * / \ = . , < >
!!       QUOTE  -- '
!!
!!    LINE
!!
!!           |-----> STATEMENT >----|
!!           |                      |
!!           |-----> CLAUSE >-------|
!!           |                      |
!!    -------|-----> EXPR >---------|------>
!!         | |                      | |
!!         | |-----> COMMAND >------| |
!!         | |                      | |
!!         | |-> > >-> EXPR >-> < >-| |
!!         | |                      | |
!!         | |----------------------| |
!!         |                          |
!!         |        |-< ; <-|         |
!!         |--------|       |---------|
!!                  |-< , <-|
!!
!!    STATEMENT
!!
!!         |-> NAME >--------------------------------|
!!         |          |                              |
!!         |          |         |--> : >---|         |
!!         |          |         |          |         |
!!         |          |-> ( >---|-> EXPR >-|---> ) >-|
!!         |                  |              |       |
!!    -----|                  |-----< , <----|       |--> = >--> EXPR >--->
!!         |                                         |
!!         |       |--< , <---|                      |
!!         |       |          |                      |
!!         |-> < >---> NAME >---> > >----------------|
!!
!!    CLAUSE
!!
!!         |---> FOR   >---> NAME >---> = >---> EXPR >--------------|
!!         |                                                        |
!!         | |-> WHILE >-|                                          |
!!         |-|           |-> EXPR >----------------------           |
!!         | |-> IF    >-|          |   |   |   |   |   |           |
!!    -----|                        <   <=  =   <>  >=  >           |---->
!!         |                        |   |   |   |   |   |           |
!!         |                        ----------------------> EXPR >--|
!!         |                                                        |
!!         |---> ELSE  >--------------------------------------------|
!!         |                                                        |
!!         |---> END   >--------------------------------------------|
!!
!!    EXPR
!!
!!           |-> + >-|
!!           |       |
!!    -------|-------|-------> TERM >---------->
!!           |       |    |             |
!!           |-> - >-|    |  |-< + <-|  |
!!                        |  |       |  |
!!                        |--|-< - <-|--|
!!                           |       |
!!                           |-< : <-|
!!
!!    TERM
!!
!!    ---------------------> FACTOR >---------------------->
!!            |                                   |
!!            |             |-< * <-|             |
!!            |  |-------|  |       |  |-------|  |
!!            |--|       |--|-< / <-|--|       |--|
!!               |-< . <-|  |       |  |-< . <-|
!!                          |-< \ <-|
!!
!!    FACTOR
!!
!!         |----------------> NUMBER >---------------|
!!         |                                         |
!!         |-> NAME >--------------------------------|
!!         |          |                              |
!!         |          |         |--> : >---|         |
!!         |          |         |          |         |
!!         |          |-> ( >---|-> EXPR >-|---> ) >-|
!!         |                  |              |       |
!!         |                  |-----< , <----|       |
!!         |                                         |
!!    -----|------------> ( >-----> EXPR >-----> ) >-|-|-------|----->
!!         |                                         | |       | |
!!         |                  |--------------|       | |-> ' >-| |
!!         |                  |              |       |           |
!!         |------------> < >-|---> EXPR >---|-> > >-|           |
!!         |                    |          |         |           |
!!         |                    |--<   <---|         |           |
!!         |                    |          |         |           |
!!         |                    |--< ; <---|         |           |
!!         |                    |          |         |           |
!!         |                    |--< , <---|         |           |
!!         |                                         |           |
!!         |------------> > >-----> EXPR >-----> < >-|           |
!!         |                                         |           |
!!         |-----> FACTOR >---> ** >---> FACTOR >----|           |
!!         |                                                     |
!!         |------------> ' >-----> TEXT >-----> ' >-------------|
!!
!!    NUMBER
!!
!!        |----------|                          |-> + >-|
!!        |          |                          |       |
!!    -----> INT >-----> . >---> INT >-----> E >---------> INT >---->
!!                 |                   | |      |       |        |
!!                 |                   | |      |-> - >-|        |
!!                 |                   | |                       |
!!                 |---------------------------------------------|
!!
!!    INT
!!
!!    ------------> DIGIT >----------->
!!              |           |
!!              |-----------|
!!
!!    NAME
!!
!!                      |--< LETTER <--|
!!                      |              |
!!    ------> LETTER >--|--------------|----->
!!                      |              |
!!                      |--< DIGIT  <--|
!!
!!    COMMAND
!!
!!                            |--> NAME >--|
!!                            |            |
!!    --------> NAME >--------|------------|---->
!!                            |            |
!!                            |--> CHAR >--|
!!                            |            |
!!                            |---> ' >----|
!!
!!    TEXT
!!
!!                    |-> LETTER >--|
!!                    |             |
!!                    |-> DIGIT >---|
!!    ----------------|             |-------------->
!!                |   |-> CHAR >----|   |
!!                |   |             |   |
!!                |   |-> ' >-> ' >-|   |
!!                |                     |
!!                |---------------------|
!>
!! Originally based on a routine called MATLAB, although heavily modified
!! since. The original stated ...
!!
!!    MATLAB stands for MATrix LABoratory. It is a FORTRAN package
!!    developed by Argonne National Laboratories for in-house use. It
!!    provides comprehensive vector and tensor operations in a package
!!    which may be programmed, either through a macro language or through
!!    execution of script files.
!!
!!    Matlab is reentrant and recursive. Functions supported include (but
!!    are not by any means limited to) sin, cos, tan, arc functions, upper
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
!!    cost money. Believe me, this package is a bargain at the price.
!!    Please feel free to distribute the package.
!!
!!    The source was taken off a VAX 11/780. It ran without modification
!!    (except the file handler and some minor error handling) on an Amiga
!!    1000 using ABSoft Fortran v2.2.  It will run in 512K environment.
!!    I have seen it on IBM mainframes and IBM PCs.
!!
!!    Subsequent changes per John S. Urban: see change log and git(1) history
implicit none
!private

public lala
public get_from_lala  ! get_a_lala   ! ??? maybe a function too with a second parameter and returned value is of same type(?)
public put_into_lala  ! give_a_lala
public :: ifin_lala   ! lalain
public :: printit
!!public :: size_lala

! for other routines
public mat_flop
public mat_wasum
public mat_wdotcr
public mat_wdotci
! till get rid of type mismatches, the following are public

integer,parameter,private:: sp=kind(1.0),dp=kind(1.0d0)
character(len=*),parameter :: gen0='(*(g0))'
character(len=*),parameter :: gen1='(*(g0,1x))'
!==================================================================================================================================!
! program limits
integer,parameter        :: GG_LINELEN=1024
integer,parameter        :: GG_MAX_NUMBER_OF_NAMES=480
integer,parameter        :: GG_MAX_NAME_LENGTH=63
integer,parameter        :: GG_EOL=99999           ! make > 2256

!==================================================================================================================================!
character(len=GG_LINELEN),allocatable,save :: G_PSEUDO_FILE(:) ! allow for input to be passed from program instead of from file
logical                  :: G_PROMPT              ! companion for G_PSEUDO_FILE
logical,save             :: G_ECHO=.false.        ! echo input lines

integer                  :: G_LIN(GG_LINELEN)

integer                  :: G_LHS ! number of arguments on LHS
integer                  :: G_RHS ! number of arguments on RHS
integer                  :: G_FIN
integer                  :: G_FUN
integer                  :: G_FMT

integer                  :: G_RIO
integer                  :: G_INPUT_LUN

integer                  :: G_PTZ
integer                  :: G_SYM
integer                  :: G_SYN(GG_MAX_NAME_LENGTH)
!==================================================================================================================================!
integer                  :: G_CURRENT_RANDOM_SEED
integer                  :: G_CURRENT_RANDOM_TYPE     ! [0] uniform distribution
                                                      ! [*] normal distribution
integer                  :: G_FLOP_COUNTER(2)
integer                  :: G_DEBUG_LEVEL             ! select which debug messages to display. zero (0) is off
logical                  :: G_FILE_OPEN_ERROR         ! flag whether file open error occurred or not
integer                  :: G_ERR
integer                  :: G_LINECOUNT(4)            ! [1] lines displayed since count started
                                                      ! [2] line limit before warning (ie. page length+1)
                                                      ! [3] 0 or 1 for "semi" mode to be on or off
                                                      ! [4] flag from "exec" command, and ...

integer                  :: G_BUF(GG_LINELEN)
!==================================================================================================================================!
! PARSING
integer,parameter        :: G_PSIZE=32                        ! stack size for pseudo-recursion
integer                  :: G_IDS(GG_MAX_NAME_LENGTH,G_PSIZE)
integer                  :: G_PSTK(G_PSIZE)
integer                  :: G_RSTK(G_PSIZE)
integer                  :: G_PT

integer                  :: G_CHRA ! current character in line
integer                  :: G_LINE_POINTER(6) ! [1] first character to process in current line
                                              ! [2] last character to process in current line
                                              ! [3]
                                              ! [4] pointer into current character in current line being processed
                                              ! [5]
                                              ! [6]
!==================================================================================================================================!
integer,save                   :: GM_BIGMEM=-1                           ! allocated size of data storage
doubleprecision,allocatable    :: GM_REALS(:), GM_IMAGS(:)               ! set to size of GM_BIGMEM

integer                        :: G_VAR_IDS(GG_MAX_NAME_LENGTH, GG_MAX_NUMBER_OF_NAMES)
integer                        :: G_VAR_DATALOC(GG_MAX_NUMBER_OF_NAMES)
integer                        :: G_VAR_ROWS(GG_MAX_NUMBER_OF_NAMES)
integer                        :: G_VAR_COLS(GG_MAX_NUMBER_OF_NAMES)

type vctr
   integer :: rows
   integer :: cols
   doubleprecision,allocatable :: re(:)
   doubleprecision,allocatable :: im(:)
endtype vctr

character(len=:),allocatable   :: keywords(:)
integer,allocatable            :: locs(:)
integer,allocatable            :: rows(:)
integer,allocatable            :: cols(:)
type(vctr),allocatable         :: vals(:)

character(len=:),allocatable   :: scr_keywords(:)
integer,allocatable            :: scr_locs(:)
integer,allocatable            :: scr_rows(:)
integer,allocatable            :: scr_cols(:)

integer                     :: G_TOP_OF_SAVED, G_ARGUMENT_POINTER

!   Two large real arrays, GM_REALS and GM_IMAGS (for real and imaginary parts), are used to store all
!   the matrices. Four integer arrays (G_VAR_IDS, G_VAR_ROWS, G_VAR_COLS, G_VAR_DATALOC) are used to store the names,
!   the row and column dimensions, and the pointers into the real stacks. The following diagram illustrates this storage scheme.
!
!                    TOP        IDS       ROWS COLS LOCS              GM_REALS    GM_IMAGS
!                     --      -- -- -- --   --   --   --              --------   --------    <<== G_ARGUMENT_POINTER
!                    |  |--->|  |  |  |  | |  | |  | |  |----------->|        | |        |
!                     --      -- -- -- --   --   --   --              --------   --------
!                            |  |  |  |  | |  | |  | |  |            |        | |        |
!                             -- -- -- --   --   --   --              --------   --------
!                                  .         .    .    .                  .          .
!                                  .         .    .    .                  .          .
!                                  .         .    .    .                  .          .
!                             -- -- -- --   --   --   --              --------   --------
!                    BOT     |  |  |  |  | |  | |  | |  |            |        | |        |
!                     --      -- -- -- --   --   --   --              --------   --------
!                    |  |--->| X|  |  |  | | 2| | 1| |  |----------->|  3.14  | |  0.00  |  <<== G_TOP_OF_SAVED
!                     --      -- -- -- --   --   --   --              --------   --------
!                            | A|  |  |  | | 2| | 2| |  |---------   |  0.00  | |  1.00  |
!                             -- -- -- --   --   --   --          \   --------   --------
!                            | E| P| S|  | | 1| | 1| |  |-------   ->| 11.00  | |  0.00  |
!                             -- -- -- --   --   --   --        \     --------   --------
!                            | F| L| O| P| | 1| | 2| |  |------  \   | 21.00  | |  0.00  |
!                             -- -- -- --   --   --   --       \  \   --------   --------
!                            | E| Y| E|  | |-1| |-1| |  |---    \ |  | 12.00  | |  0.00  |
!                             -- -- -- --   --   --   --    \   | |   --------   --------
!   GG_MAX_NUMBER_OF_NAMES-> | R| A| N| D| | 1| | 1| |  |-   \  | |  | 22.00  | |  0.00  |
!                             -- -- -- --   --   --   --  \  |  \ \   --------   --------
!                                                         |  \   \ ->| 1.E-15 | |  0.00  |
!                                                         \   \   \   --------   --------
!                                                          \   \   ->|  0.00  | |  0.00  |
!                                                           \   \     --------   --------
!                                                            \   \   |  0.00  | |  0.00  |
!                                                             \   \   --------   --------
!                                                              \   ->|  1.00  | |  0.00  |
!                                                               \     --------   --------
!                                                                --->| URAND  | |  0.00  |  GM_BIGMEM
!                                                                     --------   --------
!
!   The top portion of the stack is used for temporary variables and the
!   bottom portion for saved variables. The figure shows the situation
!   after the line
!
!      A = [11,12; 21,22],  x = [3.14, sqrt(-1)]'
!
!   has been processed. The four permanent names, "eps", "flop", "rand"
!   and "eye", occupy the last four positions of the variable stacks.
!   RAND has dimensions 1 by 1, but whenever its value is requested,
!   a random number generator is used instead. "eye" has dimensions -1
!   by -1 to indicate that the actual dimensions must be determined
!   later by context. The two saved variables have dimensions 2 by 2
!   and 2 by 1 and so take up a total of 6 locations.
!
!   Subsequent statements involving A and x will result in
!   temporary copies being made in the top of the stack for use in
!   the actual calculations. Whenever the top of the stack reaches
!   the bottom, a message indicating memory has been exceeded is
!   printed, but the current variables are not affected.
!
!   This modular structure makes it possible to implement LALA
!   on a system with a limited amount of memory, as this can easily be
!   implemented with a direct-access file as well.
!==================================================================================================================================!
interface put_into_lala
   module procedure store_array_into_lala
   module procedure store_vector_into_lala
   module procedure store_scalar_into_lala
end interface put_into_lala

interface get_from_lala
   module procedure get_fixed_array_from_lala_dpcmplx
   module procedure get_fixed_array_from_lala_cmplx
   module procedure get_fixed_array_from_lala_real32
   module procedure get_fixed_array_from_lala_real64
   module procedure get_fixed_array_from_lala_real128
   module procedure get_fixed_array_from_lala_int8
   module procedure get_fixed_array_from_lala_int16
   module procedure get_fixed_array_from_lala_int32
   module procedure get_fixed_array_from_lala_int64
   module procedure get_fixed_array_from_lala_logical
   !module procedure get_fixed_array_from_lala_character !???? hmmm, does not meet current lala model

   module procedure get_fixed_vector_from_lala_dpcmplx
   module procedure get_fixed_vector_from_lala_cmplx
   module procedure get_fixed_vector_from_lala_real32
   module procedure get_fixed_vector_from_lala_real64
   module procedure get_fixed_vector_from_lala_real128
   module procedure get_fixed_vector_from_lala_int8
   module procedure get_fixed_vector_from_lala_int16
   module procedure get_fixed_vector_from_lala_int32
   module procedure get_fixed_vector_from_lala_int64
   module procedure get_fixed_vector_from_lala_logical
   module procedure get_fixed_vector_from_lala_character

   module procedure get_fixed_scalar_from_lala_character

   module procedure get_array_from_lala_dpcmplx
   module procedure get_array_from_lala_cmplx
   module procedure get_array_from_lala_real32
   module procedure get_array_from_lala_real64
   module procedure get_array_from_lala_real128
   module procedure get_array_from_lala_int8
   module procedure get_array_from_lala_int16
   module procedure get_array_from_lala_int32
   module procedure get_array_from_lala_int64
   module procedure get_array_from_lala_logical
   !module procedure get_array_from_lala_character !???? hmmm, does not meet current lala model

   module procedure get_vector_from_lala_dpcmplx
   module procedure get_vector_from_lala_cmplx
   module procedure get_vector_from_lala_real32
   module procedure get_vector_from_lala_real64
   module procedure get_vector_from_lala_real128
   module procedure get_vector_from_lala_int8
   module procedure get_vector_from_lala_int16
   module procedure get_vector_from_lala_int32
   module procedure get_vector_from_lala_int64
   module procedure get_vector_from_lala_logical
   module procedure get_vector_from_lala_character

   module procedure get_scalar_from_lala_dpcmplx
   module procedure get_scalar_from_lala_cmplx
   module procedure get_scalar_from_lala_real32
   module procedure get_scalar_from_lala_real64
   module procedure get_scalar_from_lala_real128
   module procedure get_scalar_from_lala_int8
   module procedure get_scalar_from_lala_int16
   module procedure get_scalar_from_lala_int32
   module procedure get_scalar_from_lala_int64
   module procedure get_scalar_from_lala_logical
   module procedure get_scalar_from_lala_character

end interface get_from_lala

interface lala
   module procedure lala_init
   module procedure lala_cmd
   module procedure lala_cmds
end interface lala

character(len=:),allocatable :: G_HELP_TEXT(:)
character(len=:),allocatable :: G_FORTRAN_TEXT(:)

!==================================================================================================================================!
! CHARACTER SET
integer,parameter        :: G_CHARSET_SIZE=256      ! number of characters in character set

! unused: `~!#%^&_?
! now allow all characters, using !#
! thinking ! for comments, & for continue like Fortran
! use % for shell commands?

character(len=*),parameter :: digit='0123456789'
character(len=*),parameter :: little='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter :: big='ABCDEFGHIJKLMNOPQRSTUVWXYZ'

integer,parameter :: dstar=3042

integer,parameter :: isname=0 ! -1000
integer,parameter :: isnum=1  ! -1001

integer,parameter :: blank=32 ! blank
!integer,parameter ::  =33 !  !
!integer,parameter :: doublequote=34 ! "
!integer,parameter ::  =35 ! #
!integer,parameter ::  =36 ! $
!integer,parameter ::  =37 ! %
!integer,parameter ::  =38 ! &
integer,parameter ::  quote=39  ! '
integer,parameter ::  lparen=40 ! (
integer,parameter ::  rparen=41 ! )
integer,parameter ::  star=42   ! *
integer,parameter ::  plus=43   ! +
integer,parameter ::  comma=44  ! ,
integer,parameter ::  minus=45  ! -
integer,parameter ::  dot=46    ! .
integer,parameter ::  slash=47  ! /
integer,parameter ::  zero=48   ! 0
!integer,parameter ::  =49 ! 1
!integer,parameter ::  =50 ! 2
!integer,parameter ::  =51 ! 3
!integer,parameter ::  =52 ! 4
!integer,parameter ::  =53 ! 5
!integer,parameter ::  =54 ! 6
!integer,parameter ::  =55 ! 7
!integer,parameter ::  =56 ! 8
!integer,parameter ::  =57 ! 9
integer,parameter ::  colon=58  ! :
integer,parameter ::  semi=59   ! ;
integer,parameter ::  less=60   ! <
integer,parameter ::  equal=61  ! =
integer,parameter ::  great=62  ! >
!integer,parameter ::  =63 ! ?
!integer,parameter ::  =64 ! @
integer,parameter ::  a_up=65   ! A
!integer,parameter ::  =66 ! B
!integer,parameter ::  =67 ! C
integer,parameter :: d_up=68   ! D
integer,parameter ::  e_up=69   ! E
!integer,parameter ::  =70 ! F
!integer,parameter ::  =71 ! G
!integer,parameter ::  =72 ! H
!integer,parameter ::  =73 ! I
!integer,parameter ::  =74 ! J
!integer,parameter ::  =75 ! K
!integer,parameter ::  =76 ! L
!integer,parameter ::  =77 ! M
!integer,parameter ::  =78 ! N
!integer,parameter ::  =79 ! O
!integer,parameter ::  =80 ! P
!integer,parameter ::  =81 ! Q
!integer,parameter ::  =82 ! R
!integer,parameter ::  =83 ! S
!integer,parameter ::  =84 ! T
!integer,parameter ::  =85 ! U
!integer,parameter ::  =86 ! V
!integer,parameter ::  =87 ! W
!integer,parameter ::  =88 ! X
!integer,parameter ::  =89 ! Y
integer,parameter ::  z_up=90   ! Z
integer,parameter ::  lbracket=91 ! [
integer,parameter ::  bslash=92 ! backslash
integer,parameter ::  rbracket=93 ! ]
!integer,parameter ::  =94 ! ^
integer,parameter ::  score=95  ! _
!integer,parameter ::  =96 ! `
integer,parameter ::  a_low=97  ! a
!integer,parameter ::  =98 ! b
!integer,parameter ::  =99 ! c
integer,parameter ::  d_low=100 ! d
integer,parameter ::  e_low=101 ! e
!integer,parameter ::  =102 ! f
!integer,parameter ::  =103 ! g
!integer,parameter ::  =104 ! h
!integer,parameter ::  =105 ! i
!integer,parameter ::  =106 ! j
!integer,parameter ::  =107 ! k
!integer,parameter ::  =108 ! l
!integer,parameter ::  =109 ! m
!integer,parameter ::  =110 ! n
!integer,parameter ::  =111 ! o
!integer,parameter ::  =112 ! p
!integer,parameter ::  =113 ! q
!integer,parameter ::  =114 ! r
!integer,parameter ::  =115 ! s
!integer,parameter ::  =116 ! t
!integer,parameter ::  =117 ! u
!integer,parameter ::  =118 ! v
!integer,parameter ::  =119 ! w
!integer,parameter ::  =120 ! x
!integer,parameter ::  =121 ! y
integer,parameter ::  z_low=122 ! z
integer,parameter ::  lbrace=123 ! {
!integer,parameter ::  =124 ! |
integer,parameter ::  rbrace=125 ! }
!integer,parameter ::  =126 ! ~

integer,parameter        :: GG_PAD(63)=blank
!==================================================================================================================================!
! allow for a user-defined subroutine.
! ??? expand this to allow for multiple routines and a user-specified name for the procedure
! ??? and a variable specifically for returning a user error
private :: usersub_placeholder

abstract interface
   subroutine usersub_interface(a,m,n,s,t)
      import dp
      integer :: m,n
      doubleprecision :: a(:)
      doubleprecision :: s,t
   end subroutine usersub_interface
end interface

public usersub_interface

procedure(usersub_interface),pointer :: usersub => usersub_placeholder
!==================================================================================================================================!
!==================================================================================================================================!

contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine set_usersub(proc)
procedure(usersub_interface) :: proc
   usersub => proc
end subroutine set_usersub
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine usersub_placeholder(a,m,n,s,t)  ! sample usersub_placeholder routine
implicit none
integer                    :: m,n
doubleprecision            :: a(:)
doubleprecision            :: s,t
integer                    :: i, j, k
!  allowing for m and n to be changed complicates dimensioning a(m,n)
!  on most compilers overindexing would probably not be a problem in actuality
!  and dimensioning would not be either but not standard unless make allocatable.
!  See RESHAPE() and PACK() if passing to other routines
   write(*,*)'M=',m
   write(*,*)'N=',n
   write(*,*)'S=',s
   write(*,*)'T=',t
   k=0
   do i = 1, m
      do j = 1, n
         k=k+1
         write(*,*)i,j,a(k)
      enddo
   enddo
   k=0
   if(s.eq.0)s=1
   do i = 1, m
      do j = 1, n
         k=k+1
         a(k)=a(k)*s+t
      enddo
   enddo
end subroutine usersub_placeholder
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    LALA(3f) - [M_matrix] initialize and/or pass commands to matrix
!!    laboratory interpreter
!!    LICENSE(MIT)
!!##SYNOPSIS
!!
!!
!!     subroutine lala(init,cmd)
!!
!!      integer,intent(in),optional :: init
!!      character(len=*),intent(in),optional :: cmd
!!         or
!!      character(len=*),intent(in),optional :: cmd(:)
!!
!!##DESCRIPTION
!!    LALA(3f) is modeled on MATLAB(3f) (MATrix LABoratory), a FORTRAN
!!    package developed by Argonne National Laboratories for in-house use.
!!    It provides comprehensive vector and tensor operations in a package
!!    which may be programmed, either through a macro language or through
!!    execution of script files.
!!
!!    LALA(3f) Functions supported include (but are not by any means limited
!!    to) sin, cos, tan, arcfunctions, upper triangular, lower triangular,
!!    determinants, matrix multiplication, identity, Hilbert matrices,
!!    eigenvalues and eigenvectors, matrix roots and products, inversion
!!    and so on and so forth.
!!
!!    LALA() can be used
!!       + as a stand-alone utility for working with lala() files and
!!         for basic computations.
!!       + embedded in a Fortran program, passing variables back and forth
!!         between the calling program and the utility.
!!       + to read configuration and data files that contain expressions
!!         and conditionally selected values.
!!       + for interactively inspecting data generated by the calling program.
!!       + for creating unit tests that allow for further interactive examination.
!!
!!    The HELP command describes using the interpreter.
!!
!!##OPTIONS
!!    INIT    indicate size of scratch space to allocate and (re)initialize
!!            LALA.
!!
!!    CMD     LALA command(s) to perform. May be CHARACTER scalar or vector
!!
!!    INIT and CMD cannot be combined on a single call.
!!
!!    The first call may be an initialization declaring the number of
!!    doubleprecision complex values to allocate for the combined scratch
!!    and variable storage area. This form may be repeated and reinitializes
!!    the utility at each call. A size of zero will deallocate any allocated
!!    storage (after which the routine cannot be called with commands until
!!    reallocated by another call to lala()).
!!
!!    If no parameters are supplied interactive mode is entered.
!!
!!    If a CMD is passed and no previous initialization call was made the
!!    scratch space will be allocated to 200000.
!!
!!##EXAMPLE
!!
!!
!!   Example 1:
!!
!!       program demo_LALA
!!       use M_matrix, only : lala
!!
!!          write(*,'(a)')'optionally initialize scratch area size'
!!          call LALA(20000)
!!
!!          write(*,'(a)')'do some commands'
!!          call LALA([character(len=80) :: &
!!          & 'semi;                         ',&
!!          & 'a=magic(4),b=-a               ',&
!!          & 'a+b;a;b                       ',&
!!          & "display('That is all Folks!') "])
!!
!!          write(*,'(a)')'do a single command'
!!          call LALA('who')
!!
!!          write(*,'(a)')'enter interactive mode'
!!          call LALA()
!!
!!          write(*,'(a)')'ending program'
!!       end program demo_LALA
!!
!!   Example 2:
!!
!!    program bigmat
!!    use M_matrix, only : lala
!!       ! pass strings to LALA but do not enter interactive mode
!!       call lala(20000)                  ! initialize silently
!!       call lala( 'a=[1 2 3 4; 5 6 7 8]')
!!       call lala( [character(len=80) :: &
!!        & 'semi;lines(999999)                                    ',&
!!        & '// create a magic square and add 100 to all the values',&
!!        & 'A=magic(4),<X,Y>=shape(A)                             ',&
!!        & 'B=A+ones(X,Y)*100                                     ',&
!!        & '// save all current values to a file                  ',&
!!        & "save('sample.laf')                                    ",&
!!        & '// clear all user values                              ',&
!!        & 'clear                                                 ',&
!!        & '// show variable names, load values from file         ',&
!!        & '// and show again to show the variables are restored  ',&
!!        & "who;load('sample.laf');who                            "])
!!    end program bigmat
!!
!!   Example 3: Sample program with custom user function
!!
!!       program custom_user
!!       use M_matrix
!!       implicit none
!!       call set_usersub(lala_user)
!!       call lala()
!!       contains
!!       !-------------------------------------------------------------
!!       subroutine lala_user(a,m,n,s,t)  ! sample user routine
!!       ! Allows personal  Fortran  subroutines  to  be  linked  into
!!       ! LALA. The subroutine should have the heading
!!       !
!!       !    subroutine name(a,m,n,s,t)
!!       !    integer :: m,n
!!       !    doubleprecision a(:),s,t
!!       !
!!       ! The LALA statement Y = USER(X,s,t) results in a call to
!!       ! the subroutine with a copy of the matrix X stored in the
!!       ! argument A, its column and row dimensions in M and N,
!!       ! and the scalar parameters S and T stored in S and T.
!!       ! If S and T are omitted, they are set to 0.0. After
!!       ! the return, A is stored in Y. The dimensions M and
!!       ! N may be reset within the subroutine. The statement Y =
!!       ! USER(K) results in a call with M = 1, N = 1 and A(1,1) =
!!       ! FLOAT(K). After the subroutine has been written, it must
!!       ! be compiled and linked to the LALA object code within the
!!       ! local programming environment.
!!       !
!!       implicit none
!!       integer                    :: m,n
!!       doubleprecision            :: a(:)
!!       doubleprecision            :: s,t
!!       integer                    :: i, j, k
!!          write(*,*)'MY ROUTINE'
!!          write(*,*)'M=',m
!!          write(*,*)'N=',n
!!          write(*,*)'S=',s
!!          write(*,*)'T=',t
!!          k=0
!!          do i = 1, m
!!             do j = 1, n
!!                k=k+1
!!                write(*,*)i,j,a(k)
!!             enddo
!!          enddo
!!          k=0
!!          if(s.eq.0)s=1
!!          do i = 1, m
!!             do j = 1, n
!!                k=k+1
!!                a(k)=a(k)*s+t
!!             enddo
!!          enddo
!!       end subroutine lala_user
!!       end program custom_user
!!
!!  Example inputs
!!
!!      >:avg:
!!
!!      >for i = 2:2:n, for j = 2:2:n, t = (a(i-1,j-1)+a(i-1,j)+a(i,j-1)+a(i,j))/4; ...
!!      >a(i-1,j-1) = t; a(i,j-1) = t; a(i-1,j) = t; a(i,j) = t;
!!
!!      >:cdiv:
!!
!!      >// ======================================================
!!      >// cdiv
!!      >a=sqrt(random(8))
!!      >ar = real(a); ai = imag(a); br = real(b); bi = imag(b);
!!      >p = bi/br;
!!      >t = (ai - p*ar)/(br + p*bi);
!!      >cr = p*t + ar/br;
!!      >ci = t;
!!      >p2 = br/bi;
!!      >t2 = (ai + p2*ar)/(bi + p2*br);
!!      >ci2 = p2*t2 - ar/bi;
!!      >cr2 = t2;
!!      >s = abs(br) + abs(bi);
!!      >ars = ar/s;
!!      >ais = ai/s;
!!      >brs = br/s;
!!      >bis = bi/s;
!!      >s = brs**2 + bis**2;
!!      >cr3 = (ars*brs + ais*bis)/s;
!!      >ci3 = (ais*brs - ars*bis)/s;
!!      >[cr ci; cr2 ci2; cr3 ci3]
!!      >// ======================================================
!!
!!      >:exp:
!!
!!      >t = 0*x + eye; s = 0*eye(x); n = 1;
!!      >while abs(s+t-s) > 0, s = s+t, t = x*t/n, n = n + 1
!!
!!      >:four:
!!      > n
!!      > pi = 4*atan(1);
!!      > i = sqrt(-1);
!!      > w = exp(2*pi*i/n);
!!      > F = [];
!!      > for k = 1:n, for j = 1:n, F(k,j) = w**((j-1)*(k-1));
!!      > F = F/sqrt(n);
!!      > alpha = r*pi;
!!      > rho = exp(i*alpha);
!!      > S = log(rho*F)/i - alpha*eye;
!!      > serr = norm(imag(S),1);
!!      > S = real(S);
!!      > serr = serr + norm(S-S',1)
!!      > S = (S + S')/2;
!!      > ferr = norm(F-exp(i*S),1)
!!
!!      > :gs:
!!      > for k = 1:n, for j = 1:k-1, d = x(k,:)*x(j,:)'; x(k,:) = x(k,:) - d*x(j,:); ...
!!      > end, s = norm(x(k,:)), x(k,:) = x(k,:)/s;
!!
!!      > :jacobi:
!!      > [n, n] = shape(A);
!!      > X = eye(n);
!!      > anorm = norm(A,'fro');
!!      > cnt = 1;
!!      > while cnt > 0, ...
!!      >   cnt = 0; ...
!!      >   for p = 1:n-1, ...
!!      >     for q = p+1:n, ...
!!      >       if anorm + abs(a(p,q)) > anorm, ...
!!      >         cnt = cnt + 1; ...
!!      >         exec('jacstep'); ...
!!      >       end, ...
!!      >     end, ...
!!      >   end, ...
!!      >   display(rat(A)), ...
!!      > end
!!
!!      > :jacstep:
!!
!!      > d = (a(q,q)-a(p,p))*0.5/a(p,q);
!!      > t = 1/(abs(d)+sqrt(d*d+1));
!!      > if d < 0, t = -t; end;
!!      > c = 1/sqrt(1+t*t);  s = t*c;
!!      > R = eye(n); r(p,p)=c; r(q,q)=c; r(p,q)=s; r(q,p)=-s;
!!      > X = X*R;
!!      > A = R'*A*R;
!!
!!      > :kron:
!!
!!      > //  C = Kronecker product of A and B
!!      > [m, n] = shape(A);
!!      > for i = 1:m, ...
!!      >    ci = a(i,1)*B; ...
!!      >    for j = 2:n, ci = [ci a(i,j)*B]; end ...
!!      >    if i = 1, C = ci; else, C = [C; ci];
!!
!!      > :lanczos:
!!
!!      > [n,n] = shape(A);
!!      > q1 = rand(n,1);
!!      > ort
!!      > alpha = []; beta = [];
!!      > q = q1/norm(q1); r = A*q(:,1);
!!      > for j = 1:n, exec('lanstep',0);
!!
!!      > :lanstep:
!!
!!      > alpha(j) = q(:,j)'*r;
!!      > r = r - alpha(j)*q(:,j);
!!      > if ort <> 0, for k = 1:j-1, r = r - r'*q(:,k)*q(:,k);
!!      > beta(j) = norm(r);
!!      > q(:,j+1) = r/beta(j);
!!      > r = A*q(:,j+1) - beta(j)*q(:,j);
!!      > if j > 1, T = diag(beta(1:j-1),1); T = diag(alpha) + T + T'; eig(T)
!!
!!      > :mgs:
!!
!!      > for k = 1:n, s = norm(x(k,:)), x(k,:) = x(k,:)/s; ...
!!      >    for j = k+1:n, d = x(j,:)*x(k,:)'; x(j,:) = x(j,:) - d*x(k,:);
!!
!!      > :net:
!!
!!      > C = [
!!      > 1   2   15  .   .   .
!!      > 2   1   3   .   .   .
!!      > 3   2   4   11  .   .
!!      > 4   3   5   .   .   .
!!      > 5   4   6   7   .   .
!!      > 6   5   8   .   .   .
!!      > 7   5   9   30  .   .
!!      > 8   6   9   10  11  .
!!      > 9   7   8   30  .   .
!!      > 10  8   12  30  31  34
!!      > 11  3   8   12  13  .
!!      > 12  10  11  34  36  .
!!      > 13  11  14  .   .   .
!!      > 14  13  15  16  38  .
!!      > 15  1   14  .   .   .
!!      > 16  14  17  20  35  37
!!      > 17  16  18  .   .   .
!!      > 18  17  19  .   .   .
!!      > 19  18  20  .   .   .
!!      > 20  16  19  21  .   .
!!      > 21  20  22  .   .   .
!!      > 22  21  23  .   .   .
!!      > 23  22  24  35  .   .
!!      > 24  23  25  39  .   .
!!      > 25  24  .   .   .   .
!!      > 26  27  33  39  .   .
!!      > 27  26  32  .   .   .
!!      > 28  29  32  .   .   .
!!      > 29  28  30  .   .   .
!!      > 30  7   9   10  29  .
!!      > 31  10  32  .   .   .
!!      > 32  27  28  31  34  .
!!      > 33  26  34  .   .   .
!!      > 34  10  12  32  33  35
!!      > 35  16  23  34  36  .
!!      > 36  12  35  38  .   .
!!      > 37  16  38  .   .   .
!!      > 38  14  36  37  .   .
!!      > 39  24  26  .   .   .
!!      > ];
!!      > [n, m] = shape(C);
!!      > A = 0*ones(n,n);
!!      > for i=1:n, for j=2:m, k=c(i,j); if k>0, a(i,k)=1;
!!      > check = norm(A-A',1), if check > 0, quit
!!      > [X,D] = eig(A+eye);
!!      > D = diag(D);  D = D(n:-1:1)
!!      > X = X(:,n:-1:1);
!!      > [x(:,1)/sum(x(:,1)) x(:,2) x(:,3) x(:,19)]
!!
!!      > :pascal:
!!
!!      > //Generate next Pascal matrix
!!      > [k,k] = shape(L);
!!      > k = k + 1;
!!      > L(k,1:k) = [L(k-1,:) 0] + [0 L(k-1,:)];
!!
!!      > :pdq:
!!
!!      > alpha = []; beta = 0; q = []; p = p(:,1)/norm(p(:,1));
!!      > t = A'*p(:,1);
!!      > alpha(1) = norm(t);
!!      > q(:,1) = t/alpha(1);
!!      > X = p(:,1)*(alpha(1)*q(:,1))'
!!      > e(1) = norm(A-X,1)
!!      > for j = 2:r, exec('pdqstep',ip); ...
!!      >    X = X + p(:,j)*(alpha(j)*q(:,j)+beta(j)*q(:,j-1))', ...
!!      >    e(j) = norm(A-X,1)
!!
!!      > :pdqstep:
!!
!!      > t = A*q(:,j-1) - alpha(j-1)*p(:,j-1);
!!      >    if ort>0, for i = 1:j-1, t = t - t'*p(:,i)*p(:,i);
!!      > beta(j) = norm(t);
!!      > p(:,j) = t/beta(j);
!!      > t = A'*p(:,j) - beta(j)*q(:,j-1);
!!      >    if ort>0, for i = 1:j-1, t = t - t'*q(:,i)*q(:,i);
!!      > alpha(j) = norm(t);
!!      > q(:,j) = t/alpha(j);
!!
!!      > :pop:
!!
!!      > y = [ 75.995   91.972  105.711  123.203   ...
!!      >      131.669  150.697  179.323  203.212]'
!!      > t = [ 1900:10:1970 ]'
!!      > t = (t - 1940*ones(t))/40;   [t y]
!!      > n = 8;  A(:,1) = ones(t);  for j = 2:n, A(:,j) = t .* A(:,j-1);
!!      > A
!!      > c = A\y
!!
!!      > :qr:
!!
!!      > scale = s(m);
!!      > sm = s(m)/scale; smm1 = s(m-1)/scale; emm1 = e(m-1)/scale;
!!      > sl = s(l)/scale; el = e(l)/scale;
!!      > b = ((smm1 + sm)*(smm1 - sm) + emm1**2)/2;
!!      > c = (sm*emm1)**2;
!!      > shift = sqrt(b**2+c); if b < 0, shift = -shift;
!!      > shift = c/(b + shift)
!!      > f = (sl + sm)*(sl-sm) - shift
!!      > g = sl*el
!!      > for k = l: m-1, exec('qrstep',ip)
!!      > e(m-1) = f
!!
!!      > :qrstep:
!!
!!      > exec('rot');
!!      > if k <> l, e(k-1) = f
!!      > f = cs*s(k) + sn*e(k)
!!      > e(k) = cs*e(k) - sn*s(k)
!!      > g = sn*s(k+1)
!!      > s(k+1) = cs*s(k+1)
!!      > exec('rot');
!!      > s(k) = f
!!      > f = cs*e(k) + sn*s(k+1)
!!      > s(k+1) = -sn*e(k) + cs*s(k+1)
!!      > g = sn*e(k+1)
!!      > e(k+1) = cs*e(k+1)
!!
!!      > :rho:
!!
!!      > //Conductivity example.
!!      > //Parameters ---
!!      >    rho       //radius of cylindrical inclusion
!!      >    n         //number of terms in solution
!!      >    m         //number of boundary points
!!      > //initialize operation counter
!!      >    flop = [0 0];
!!      > //initialize variables
!!      >    m1 = round(m/3);   //number of points on each straight edge
!!      >    m2 = m - m1;       //number of points with Dirichlet conditions
!!      >    pi = 4*atan(1);
!!      > //generate points in Cartesian coordinates
!!      >    //right hand edge
!!      >    for i = 1:m1, x(i) = 1; y(i) = (1-rho)*(i-1)/(m1-1);
!!      >    //top edge
!!      >    for i = m2+1:m, x(i) = (1-rho)*(m-i)/(m-m2-1); y(i) = 1;
!!      >    //circular edge
!!      >    for i = m1+1:m2, t = pi/2*(i-m1)/(m2-m1+1); ...
!!      >       x(i) = 1-rho*sin(t);  y(i) = 1-rho*cos(t);
!!      > //convert to polar coordinates
!!      >    for i = 1:m-1, th(i) = atan(y(i)/x(i));  ...
!!      >       r(i) = sqrt(x(i)**2+y(i)**2);
!!      >    th(m) = pi/2;  r(m) = 1;
!!      > //generate matrix
!!      >    //Dirichlet conditions
!!      >    for i = 1:m2, for j = 1:n, k = 2*j-1; ...
!!      >       a(i,j) = r(i)**k*cos(k*th(i));
!!      >    //Neumann conditions
!!      >    for i = m2+1:m, for j = 1:n, k = 2*j-1; ...
!!      >       a(i,j) = k*r(i)**(k-1)*sin((k-1)*th(i));
!!      > //generate right hand side
!!      >    for i = 1:m2, b(i) = 1;
!!      >    for i = m2+1:m, b(i) = 0;
!!      > //solve for coefficients
!!      >    c = A\b
!!      > //compute effective conductivity
!!      >    c(2:2:n) = -c(2:2:n)
!!      >    sigma = sum(c)
!!      > //output total operation count
!!      >    ops = flop(2)
!!
!!      > :rogers.exec:
!!
!!      > exec('d.boug');                // reads data
!!      > [g,k] = shape(p);              // p is matrix of gene frequencies
!!      > wv = ncen/sum(ncen);           // ncen contains population sizes
!!      > pbar = wv*p;                   // weighted average of p
!!      > p = p - ones(g,1)*pbar;        // deviations from mean
!!      > p = sqrt(diag(wv)) * p;        // weight rows of p by sqrt of pop size
!!      > h = diag(pbar); h = h*(eye-h); // diagonal contains binomial variance: p*(1-p)
!!      > r = p*inv(h)*p'/k;             // normalized covariance matrix
!!      > eig(r)'
!!
!!      > :rosser:
!!
!!      > A  = [
!!      >   611.  196. -192.  407.   -8.  -52.  -49.   29.
!!      >   196.  899.  113. -192.  -71.  -43.   -8.  -44.
!!      >  -192.  113.  899.  196.   61.   49.    8.   52.
!!      >   407. -192.  196.  611.    8.   44.   59.  -23.
!!      >    -8.  -71.   61.    8.  411. -599.  208.  208.
!!      >   -52.  -43.   49.   44. -599.  411.  208.  208.
!!      >   -49.   -8.    8.   59.  208.  208.   99. -911.
!!      >    29.  -44.   52.  -23.  208.  208. -911.   99.  ];
!!
!!      > :rot:
!!
!!      > // subexec rot(f,g,cs,sn)
!!      >    rho = g; if abs(f) > abs(g), rho = f;
!!      >    cs = 1.0; sn = 0.0; z = 1.0;
!!      >    r = norm([f g]); if rho < 0, r = -r; r
!!      >    if r <> 0.0, cs = f/r
!!      >    if r <> 0.0, sn = g/r
!!      >    if abs(f) > abs(g), z = sn;
!!      >    if abs(g) >= abs(f), if cs <> 0, z = 1/cs;
!!      >    f = r;
!!      >    g = z;
!!
!!      > :rqi:
!!
!!      > rho = (x'*A*x)
!!      > x = (A-rho*eye)\x;
!!      > x = x/norm(x)
!!
!!      > :setup:
!!
!!      > diary('xxx')
!!      > !tail -f xxx > /dev/tty1 &
!!      > !tail -f xxx > /dev/tty2 &
!!
!!      > :sigma:
!!
!!      > RHO = .5  M = 20  N = 10   SIGMA =  1.488934271883534
!!      > RHO = .5  M = 40  N = 20   SIGMA =  1.488920312974229
!!      > RHO = .5  M = 60  N = 30   SIGMA =  1.488920697912116
!!
!!      > :strut.laf:
!!
!!      > // Structure problem, Forsythe, Malcolm and Moler, p. 62
!!      > s =  sqrt(2)/2;
!!      > A = [
!!      > -s  .  .  1  s   .  .  .  .  .  .  .  .  .  .  .  .
!!      > -s  . -1  . -s   .  .  .  .  .  .  .  .  .  .  .  .
!!      >  . -1  .  .  .   1  .  .  .  .  .  .  .  .  .  .  .
!!      >  .  .  1  .  .   .  .  .  .  .  .  .  .  .  .  .  .
!!      >  .  .  . -1  .   .  .  1  .  .  .  .  .  .  .  .  .
!!      >  .  .  .  .  .   . -1  .  .  .  .  .  .  .  .  .  .
!!      >  .  .  .  . -s -1  .  .  s  1  .  .  .   .  .  .  .
!!      >  .  .  .  .  s   .  1  .  s  .  .  .  .  .  .  .  .
!!      >  .  .  .  .  .   .  . -1 -s  .  .  1  s  .  .  .  .
!!      >  .  .  .  .  .   .  .  . -s  . -1  . -s  .  .  .  .
!!      >  .  .  .  .  .   .  .  .  . -1  .  .  .  1  .  .  .
!!      >  .  .  .  .  .   .  .  .  .  .  1  .  .  .  .  .  .
!!      >  .  .  .  .  .   .  .  .  .  .  . -1  .  .  .  s  .
!!      >  .  .  .  .  .   .  .  .  .  .  .  .  .  . -1 -s  .
!!      >  .  .  .  .  .   .  .  .  .  .  .  . -s -1  .  .  1
!!      >  .  .  .  .  .   .  .  .  .  .  .  .  s  .  1  .  .
!!      >  .  .  .  .  .   .  .  .  .  .  .  .  .  .  . -s -1];
!!      > b = [
!!      >  .  .  . 10  .   .  . 15  .  .  .  .  .  .  . 10  .]';
!!
!!      > :test1:
!!
!!      > // -----------------------------------------------------------------
!!      > // start a new log file
!!      > sh rm -fv log.txt
!!      > diary('log.txt')
!!      > // -----------------------------------------------------------------
!!      > titles=['GNP deflator'
!!      >  'GNP         '
!!      >  'Unemployment'
!!      >  'Armed Force '
!!      >  'Population  '
!!      >  'Year        '
!!      >  'Employment  '];
!!      > data = ...
!!      > [ 83.0  234.289  235.6  159.0  107.608  1947  60.323
!!      >   88.5  259.426  232.5  145.6  108.632  1948  61.122
!!      >   88.2  258.054  368.2  161.6  109.773  1949  60.171
!!      >   89.5  284.599  335.1  165.0  110.929  1950  61.187
!!      >   96.2  328.975  209.9  309.9  112.075  1951  63.221
!!      >   98.1  346.999  193.2  359.4  113.270  1952  63.639
!!      >   99.0  365.385  187.0  354.7  115.094  1953  64.989
!!      >  100.0  363.112  357.8  335.0  116.219  1954  63.761
!!      >  101.2  397.469  290.4  304.8  117.388  1955  66.019
!!      >  104.6  419.180  282.2  285.7  118.734  1956  67.857
!!      >  108.4  442.769  293.6  279.8  120.445  1957  68.169
!!      >  110.8  444.546  468.1  263.7  121.950  1958  66.513
!!      >  112.6  482.704  381.3  255.2  123.366  1959  68.655
!!      >  114.2  502.601  393.1  251.4  125.368  1960  69.564
!!      >  115.7  518.173  480.6  257.2  127.852  1961  69.331
!!      >  116.9  554.894  400.7  282.7  130.081  1962  70.551];
!!      > short
!!      > X = data;
!!      > [n,p] = shape(X)
!!      > mu = ones(1,n)*X/n
!!      > X = X - ones(n,1)*mu;  X = X/diag(sqrt(diag(X'*X)))
!!      > corr = X'*X
!!      > y = data(:,p); X = [ones(y) data(:,1:p-1)];
!!      > long e
!!      > beta = X\y
!!      > expected = [ ...
!!      >    -3.482258634594421D+03
!!      >     1.506187227124484D-02
!!      >    -3.581917929257409D-02
!!      >    -2.020229803816908D-02
!!      >    -1.033226867173703D-02
!!      >    -5.110410565317738D-02
!!      >     1.829151464612817D+00
!!      > ]
!!      > display('EXPE and BETA should be the same')
!!
!!      > :tryall:
!!
!!      > diary('log.txt')
!!      > a=magic(8)
!!      > n=3
!!      > exec('avg')
!!      > b=random(8,8)
!!      > exec('cdiv')
!!      > exec('exp')
!!      > exec('four')
!!      > exec('gs')
!!      > exec('jacobi')
!!      > // jacstep
!!      > exec('kron')
!!      > exec('lanczos')
!!      > // lanstep
!!      > exec('longley')
!!      > exec('mgs')
!!      > exec('net')
!!      > exec('pascal')
!!      > exec('pdq')
!!      > // pdqstep
!!      > exec('pop')
!!      > exec('qr')
!!      > // qrstep
!!      > exec('rho')
!!      > exec('rosser')
!!      > // rot
!!      > exec('rqi')
!!      > exec('setup')
!!      > exec('sigma')
!!      > exec('strut.laf')
!!      > exec('w5')
!!      > exec('rogers.exec
!!      > exec('rogers.load
!!
!!      > :w5:
!!
!!      > w5 = [
!!      >         1.     1.      0.      0.      0.
!!      >       -10.     1.      1.      0.      0.
!!      >        40.     0.      1.      1.      0.
!!      >       205.     0.      0.      1.      1.
!!      >       024.     0.      0.      0.     -4.
!!      >      ]
subroutine LALA_init(init,echo)

! ident_1="@(#)M_matrix::lala(3f): initialize and/or pass commands to matrix laboratory interpreter"

integer,intent(in)          :: init
logical,intent(in),optional :: echo
doubleprecision             :: s,t
integer,parameter           :: EPS(GG_MAX_NAME_LENGTH)=   [iachar(['e','p','s',' ',' ']),GG_PAD(6:)]
integer,parameter           :: FLOPS(GG_MAX_NAME_LENGTH)= [iachar(['f','l','o','p','s']),GG_PAD(6:)]
integer,parameter           :: EYE(GG_MAX_NAME_LENGTH)=   [iachar(['e','y','e',' ',' ']),GG_PAD(6:)]
integer,parameter           :: RAND(GG_MAX_NAME_LENGTH)=  [iachar(['r','a','n','d',' ']),GG_PAD(6:)]

   if(present(echo)) G_ECHO=echo

   G_PROMPT=.true.
   G_ERR=0

   if(allocated(G_PSEUDO_FILE))deallocate(G_PSEUDO_FILE)
   allocate(G_PSEUDO_FILE(0))

   G_LIN=blank
   G_VAR_IDS=blank

   GM_BIGMEM=INIT
   if(GM_BIGMEM.lt.0)GM_BIGMEM=200000
   if(allocated(GM_REALS) )deallocate(GM_REALS)
   if(allocated(GM_IMAGS) )deallocate(GM_IMAGS)
   allocate(GM_REALS(GM_BIGMEM),GM_IMAGS(GM_BIGMEM))                      ! set to size of GM_BIGMEM

   G_INPUT_LUN = STDIN                                                    ! unit number for terminal input
   call mat_files(G_INPUT_LUN,G_BUF)
   G_RIO = G_INPUT_LUN                                                    ! current file to read commands from
   call mat_files(STDOUT,G_BUF)

   call mat_help_text()                                                   ! initialize help text
   G_CURRENT_RANDOM_SEED = 0                                              ! random number seed
   G_CURRENT_RANDOM_TYPE = 0                                              ! set the type of random numbers to compute
   G_LINECOUNT(2) = 23                                                    ! initial line limit for paging output

   G_TOP_OF_SAVED = GG_MAX_NUMBER_OF_NAMES-3  ! move up to allow room for the built-in values eps, flops, eye, rand

   call mat_wset(5,0.0D0,0.0d0,GM_REALS(GM_BIGMEM-4),GM_IMAGS(GM_BIGMEM-4),1)

   call update('eps',1,1,GM_BIGMEM-4)
      !=============================================================
      call mat_copyid(G_VAR_IDS(1,GG_MAX_NUMBER_OF_NAMES-3),EPS)
      G_VAR_DATALOC(GG_MAX_NUMBER_OF_NAMES-3) = GM_BIGMEM-4
      G_VAR_ROWS(GG_MAX_NUMBER_OF_NAMES-3) = 1
      G_VAR_COLS(GG_MAX_NUMBER_OF_NAMES-3) = 1
      !=============================================================

   ! interesting way to calculate the epsilon value of a machine
   s = 1.0d0
   SET_ST: do
      s = s/2.0D0
      t = 1.0d0 + s
      if (t .LE. 1.0d0) exit
   enddo SET_ST

   GM_REALS(GM_BIGMEM-4) = 2.0d0*s

   call update('flops',1,2,GM_BIGMEM-3)
      !=============================================================
      call mat_copyid(G_VAR_IDS(1,GG_MAX_NUMBER_OF_NAMES-2),flops)
      G_VAR_DATALOC(GG_MAX_NUMBER_OF_NAMES-2) = GM_BIGMEM-3
      G_VAR_ROWS(GG_MAX_NUMBER_OF_NAMES-2) = 1
      G_VAR_COLS(GG_MAX_NUMBER_OF_NAMES-2) = 2
      !=============================================================

   call update('eye',-1,-1,GM_BIGMEM-1)
      !=============================================================
      call mat_copyid(G_VAR_IDS(1,GG_MAX_NUMBER_OF_NAMES-1), eye)
      G_VAR_DATALOC(GG_MAX_NUMBER_OF_NAMES-1) = GM_BIGMEM-1
      G_VAR_ROWS(GG_MAX_NUMBER_OF_NAMES-1) = -1
      G_VAR_COLS(GG_MAX_NUMBER_OF_NAMES-1) = -1
      !=============================================================

   GM_REALS(GM_BIGMEM-1) = 1.0D0

   call update('rand',1,1,GM_BIGMEM)
      !=============================================================
      call mat_copyid(G_VAR_IDS(1,GG_MAX_NUMBER_OF_NAMES), rand)
      G_VAR_DATALOC(GG_MAX_NUMBER_OF_NAMES) = GM_BIGMEM
      G_VAR_ROWS(GG_MAX_NUMBER_OF_NAMES) = 1
      G_VAR_COLS(GG_MAX_NUMBER_OF_NAMES) = 1
      !=============================================================

   G_FMT = 1
   G_FLOP_COUNTER(1) = 0
   G_FLOP_COUNTER(2) = 0
   G_DEBUG_LEVEL = 0
   G_PTZ = 0
   G_PT = G_PTZ

   G_FORTRAN_TEXT=help_intrinsics('manual',m_help=.true.)  ! load Fortran documentation

end subroutine LALA_init
!==================================================================================================================================
subroutine LALA_cmd(input_string,echo)

! ident_2="@(#)M_matrix::lala(3f): run a single command in matrix laboratory interpreter and return to calling program"

character(len=*),intent(in) :: input_string
logical,intent(in),optional :: echo

   call lala_cmds( [input_string],echo=echo)

end subroutine LALA_cmd
!==================================================================================================================================
subroutine LALA_cmds(pseudo_file,echo)

! ident_3="@(#)M_matrix::lala(3f): run an array of commands in matrix laboratory interpreter and return to calling program"

character(len=*),intent(in),optional :: pseudo_file(:)
logical,intent(in),optional          :: echo

   if(present(echo)) G_ECHO=echo

   if(GM_BIGMEM.LT.0)then
      call lala_init(200000)
   else
      G_INPUT_LUN = STDIN                                                    ! unit number for terminal input
      G_RIO = G_INPUT_LUN                                                    ! current file to read commands from
      G_PROMPT=.true.
   endif

   if(present(pseudo_file))then
      G_PSEUDO_FILE=[character(len=GG_LINELEN) :: pseudo_file,'quit;']
      G_PROMPT=.false.
   else
      if(allocated(G_PSEUDO_FILE))deallocate(G_PSEUDO_FILE)
      allocate(G_PSEUDO_FILE(0))
   endif

   PARSE_LINE : do
      call mat_parse()
      select case(G_FUN)
      case(1) ; call mat_matfn1()
      case(2) ; call mat_matfn2()
      case(3) ; call mat_matfn3()
      case(4) ; call mat_matfn4()
      case(5) ; call mat_matfn5()
      case(6) ; call mat_matfn6()
      case(21); call mat_matfn1()
      case(99); exit PARSE_LINE
      case default
      end select
   enddo PARSE_LINE

end subroutine LALA_cmds
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_err(n)

! ident_4="@(#)M_matrix::mat_err(3fp): given error number, write associated error message and set G_ERR"

integer,intent(in)   :: n

integer              :: i
integer              :: k
integer              :: lb
integer              :: lt
character(len=255)   :: msg

   G_ERR = n
   select case(n)
    case(1);  msg='Improper multiple assignment'
    case(2);  msg='Improper factor'
    case(3);  msg='Expected right parenthesis'
    case(4);  msg='Undefined variable: '//ade2str(G_IDS(:,G_PT+1)) ! extract variable name into buffer
    case(5);  msg='Column lengths do not match'
    case(6);  msg='Row lengths do not match'
    case(7);  msg='Text too long'
    case(8);  msg='Incompatible for ADDITION'
    case(9);  msg='Incompatible for SUBTRACTION'
    case(10); msg='Incompatible for MULTIPLICATION'
    case(11); msg='Incompatible for RIGHT DIVISION'
    case(12); msg='Incompatible for LEFT DIVISION'
    case(13); msg='Improper assignment to PERMANENT VARIABLE'
    case(14); msg='EYE-dentity undefined by CONTEXT'
    case(15); msg='Improper assignment to submatrix'
    case(16); msg='Improper command'
    case(17)
      lb = GM_BIGMEM - G_VAR_DATALOC(G_TOP_OF_SAVED) + 1
      lt = g_err + G_VAR_DATALOC(G_TOP_OF_SAVED)
      call journal(' Too much memory required')
      write(msg,'(1X,I7,'' Variables,'',I7,'' Temporaries,'',I7,'' Available.'')') lb,lt,GM_BIGMEM
    case(18); msg='Too many names'
    case(19); msg='Matrix is singular to working precision'
    case(20); msg='Matrix must be square'
    case(21); msg='Subscript out of range'
    case(22); write(msg, '(1x,"Recursion difficulties",*(i4))') (G_RSTK(i),i=1,G_PT)
    case(23); msg='Only 1, 2 or INF norm of matrix'
    case(24); msg='No convergence'
    case(25); msg='Can not use function name as variable'
    case(26); msg='Too complicated (STACK OVERFLOW)'
    case(27); msg='Division by zero is a NO-NO'
    case(28); msg='Empty macro'
    case(29); msg='Not positive definite'
    case(30); msg='Improper exponent'
    case(31); msg='Improper string'
    case(32); msg='Singularity of LOG or ATAN'
    case(33); msg='Too many colons'
    case(34); msg='Improper FOR clause'
    case(35); msg='Improper WHILE or IF clause'
    case(36); msg='Argument out of range'
    case(37); msg='Improper MACROS'
    case(38); msg='Improper file name'
    case(39); msg='Incorrect number of arguments'
    case(40); msg='Expecting statement terminator'
    case default
       call journal('sc','*mat_err* internal error: unknown error code=',n)
       return
   end select

   k = max(1,G_LINE_POINTER(2) - G_LINE_POINTER(1)) ! number of spaces to shift arrow by
   call journal(' '//repeat(' ',k)//'/\--ERROR:'//msg)

end subroutine mat_err
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_files(lunit,iname,status)
integer                      :: lunit             ! logical unit number
                                                  ! if LUNIT is zero, return
                                                  ! if LUNIT = standard input, return
                                                  ! if LUNIT = standard output, return
                                                  ! if LUNIT is positive, open the unit to file name INAME
                                                  ! if LUNIT is negative, close the unit number
integer                      :: iname(GG_LINELEN) ! INAME = FILE NAME, 1 character per word
                                                  ! how to know length of iname?
character(len=1024)          :: name
character(len=*),optional    :: status
character(len=20)            :: status_local
integer                      :: ios
   if(present(status))then
      status_local=status
   else
      status_local='UNKNOWN'
   endif

   G_FILE_OPEN_ERROR=.false.
   select case(lunit)
    case(0)      ! error catcher
    case(stdin)  ! if unit is standard input return
    case(stdout) ! if unit is standard output return
    case(8)      ! diary file
       call mat_buf2str(name,iname,GG_LINELEN)
       call journal('O',trim(name)) ! open up trail file
    case(:-1)
      if(lunit.eq.-8)then
         call journal('O','')                                        ! close trail file
      else                                                           ! if LUNIT is negative, close the unit
         ios=0
         flush(unit=-lunit,iostat=ios)
         if(-lunit.ne.STDIN)then
            close(unit=-lunit,iostat=ios)
         endif
      endif
    case default                                                     !  ALL OTHER FILES
      call mat_buf2str(name,iname,GG_LINELEN)
      if(lunit.ne.STDIN)then
         open(unit=lunit,file=name,status=status_local,iostat=ios)      ! open a file
         if(ios.ne.0)then                                               ! general file open failure
            call journal('OPEN failed on file '//name)
            G_FILE_OPEN_ERROR=.true.                                    ! set the flag a file error occurred
            G_RIO=G_INPUT_LUN                                           ! set current file to read input lines from/to G_INPUT_LUN
         else
            G_FILE_OPEN_ERROR=.false.                                   ! set the flag a file error did not occur
         endif
      endif
   end select
end subroutine mat_files
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getsym()

! ident_5="@(#)M_matrix::mat_getsym(3fp): get a symbol"

doubleprecision   :: syv
doubleprecision   :: s

integer      :: sign
integer      :: chcnt
integer      :: ss
integer      :: i
!.......................................................................
   INFINITE : do
      if (G_CHRA .ne. blank) exit INFINITE
      call mat_getch() ! get next character
   enddo INFINITE
!.......................................................................
   G_LINE_POINTER(2) = G_LINE_POINTER(3)
   G_LINE_POINTER(3) = G_LINE_POINTER(4)
   if ( verify(achar(G_CHRA),digit) == 0) then
      call mat_getval(syv)
      if (G_CHRA .ne. dot) goto 60
      call mat_getch() ! get next character
   elseif (verify(achar(G_CHRA),digit//big//little//achar(score))== 0) then ! alphameric (0-9a-zA-Z_)
      ! name
      G_SYM = isname
      G_SYN=blank
      G_SYN(1) = G_CHRA
      do i=2,GG_MAX_NAME_LENGTH
         call mat_getch() ! get next character
         ! if not alphanumeric and not special like eol
         if (verify(achar(G_CHRA),digit//big//little//achar(score))== 0 ) then
            G_SYN(i) = G_CHRA
         else
            exit
         endif
      enddo

      goto 90
   else ! special character
      ss = G_SYM
      G_SYM = G_CHRA
      call mat_getch() ! get next character
      if (G_SYM .ne. dot) goto 90
      ! is dot part of number or operator
      syv = 0.0d0
      if (.not.(verify(achar(G_CHRA),digit)== 0) ) then ! not a number character
         if (G_CHRA.eq.star.or.G_CHRA.eq.slash.or.G_CHRA.eq.bslash) goto 90
         if (ss.eq.star .or. ss.eq.slash .or. ss.eq.bslash) goto 90
      endif
   endif

   ! number
   chcnt = G_LINE_POINTER(4)
   call mat_getval(s)
   chcnt = G_LINE_POINTER(4) - chcnt
   if (G_CHRA .eq. GG_EOL) chcnt = chcnt+1
   syv = syv + s/10.0d0**chcnt
   goto 60

60 continue

   if (.not.(G_CHRA.ne.d_low .and. G_CHRA.ne.e_low .and. G_CHRA.ne.d_up .and. G_CHRA.ne.e_up) )then
      call mat_getch() ! get next character
      sign = G_CHRA
      if (sign.eq.minus .or. sign.eq.plus) call mat_getch() ! get next character
      call mat_getval(s)
      if (sign .ne. minus) syv = syv*10.0d0**s
      if (sign .eq. minus) syv = syv/10.0d0**s
   endif
   GM_IMAGS(GM_BIGMEM) = mat_flop(syv)
   G_SYM = isnum
   goto 90

90 continue

   if (G_CHRA .eq. blank) then
      call mat_getch() ! get next character till a non-blank is found
      goto 90
   endif

end subroutine mat_getsym
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_str2buf(string,buf,lrecl)

! ident_6="@(#)M_matrix::mat_str2buf(3fp): convert string to hollerith"

! g95 compiler does not support Hollerith, this is a KLUDGE to give time to think about it

character(len=*),intent(in) :: string
integer,intent(in)          :: lrecl
integer,intent(out)         :: buf(:)
integer                     :: i

   buf=iachar(' ')
   do i=1,min(lrecl,len_trim(string),size(buf))
      buf(i)=iachar(string(i:i))
   enddo

end subroutine mat_str2buf
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function str2ade(string) result(vec)

! ident_7="@(#)M_matrix::mat_str2buf(3fp): convert CHARACTER TO ADE array vector"

character(len=*),intent(in)  :: string
integer,allocatable          :: vec(:)
integer                      :: i
   allocate(vec(len(string)))
   do i=1,len(string)
      vec(i)=iachar(string(i:i))
   enddo
end function str2ade
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function ade2str(buf) result(string)

! ident_8="@(#)M_matrix::mat_str2buf(3fp): convert ADE array to CHARACTER"

character(len=:),allocatable :: string
integer,intent(in)           :: buf(:)
integer                      :: i
   string=repeat(' ',size(buf))
   do i=1,size(buf)
      if(buf(i).ge.0 .or. buf(i).lt.255)then
         string(i:i)=achar(buf(i))
      else
         call journal('sc','ADE2STR:string contains unacceptable characters, position=',i,'ADE=',buf(i))
      endif
   enddo
end function ade2str
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_buf2str(string,buf,lrecl)

! ident_9="@(#)M_matrix::mat_buf2string(3fp): convert hollerith to string"

integer,intent(in)     :: lrecl
integer,intent(in)     :: buf(:)
character(len=*)       :: string
integer                :: i
integer                :: ilen
   string(:)=' '
   ilen=len(string)
   do i=1,min(lrecl,ilen,size(buf))
      string(i:i)=achar(buf(i))
   enddo
end subroutine mat_buf2str
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine ints2str(ints,string,ierr)

! ident_10="@(#)M_matrix::ints2str(3f): convert lala integers to a character variable"

! temporary procedure while writing ASCII-based upgrade

integer,intent(in)                       :: ints(:)
character(len=:),allocatable,intent(out) :: string
integer,intent(out)                      :: ierr
integer                                  :: i

   ierr=0
   if(allocated(string))deallocate(string)
   allocate(character(len=size(ints)) :: string)
   string(:)=' '
   do i=1,size(ints)
      if( ints(i).lt.G_CHARSET_SIZE .and. ints(i).ge.0 )then
         string(i:i)=achar(ints(i))
      else
         call journal('sc',' function name contains unacceptable characters:',ints(i))
         ierr=ierr+1
      endif
   enddo

end subroutine ints2str
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn6()
!
! ident_11="@(#)M_matrix::mat_matfn6(3f):evaluate utility functions"
!
integer :: i, j, k
integer :: ia
integer :: ib
integer :: ja
integer :: jb
integer :: location
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
integer,parameter :: unifor(GG_MAX_NAME_LENGTH) =  [iachar(['u','n','i','f','o','r','m']),GG_PAD(8:)]
integer,parameter :: normal(GG_MAX_NAME_LENGTH) =  [iachar(['n','o','r','m','a','l',' ']),GG_PAD(8:)]
integer,parameter :: seed(GG_MAX_NAME_LENGTH)   =  [iachar(['s','e','e','d',' ',' ',' ']),GG_PAD(8:)]
integer           :: id(GG_MAX_NAME_LENGTH)
doubleprecision   :: eps0,eps,s,sr,si,t
character(len=80) :: message

   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   m = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n = G_VAR_COLS(G_ARGUMENT_POINTER)

!  functions/G_FIN
!  magi diag sum  prod user eye  rand ones chop shape kron  tril triu zeros
!    1    2    3    4    5    6    7    8    9   10   11-13  14   15   16

   FUN6: select case(G_FIN)
!===================================================================================================================================
   case(1) ! COMMAND::MAGIC
      N = MAX(int(GM_REALS(location)),0)
      IF (N .EQ. 2) N = 0
      IF (N .GT. 0) call mat_magic(GM_REALS(location),N,N)
      call mat_rset(N*N,0.0D0,GM_IMAGS(location),1)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = N
      G_VAR_COLS(G_ARGUMENT_POINTER) = N
!===================================================================================================================================
   case(11,12,13) !  COMMAND::KRONECKER PRODUCT
      if (G_RHS .ne. 2) then
         call mat_err(39) ! Incorrect number of arguments
         return
      endif
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 1
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      MA = G_VAR_ROWS(G_ARGUMENT_POINTER)
      NA = G_VAR_COLS(G_ARGUMENT_POINTER)
      LA = location + MAX(M*N*MA*NA,M*N+MA*NA)
      LB = LA + MA*NA

      if(too_much_memory(LB + M*N - G_VAR_DATALOC(G_TOP_OF_SAVED)) )return

!     MOVE A AND B ABOVE RESULT
      call mat_wcopy(MA*NA+M*N,GM_REALS(location),GM_IMAGS(location),1,GM_REALS(LA),GM_IMAGS(LA),1)
      DO JA = 1, NA
        DO J = 1, N
          LJ = LB + (J-1)*M
          DO IA = 1, MA
!           GET J-TH COLUMN OF B
            call mat_wcopy(M,GM_REALS(LJ),GM_IMAGS(LJ),1,GM_REALS(location),GM_IMAGS(location),1)
!           ADDRESS OF A(IA,JA)
            LS = LA + IA-1 + (JA-1)*MA
            DO I = 1, M
!             A(IA,JA) OP B(I,J)
              IF (G_FIN .EQ. 11) &
              call mat_wmul( GM_REALS(LS), GM_IMAGS(LS), &
                             GM_REALS(location),  GM_IMAGS(location),  &
                             GM_REALS(location),  GM_IMAGS(location))
              IF (G_FIN .EQ. 12) &
              call mat_wdiv( GM_REALS(LS), GM_IMAGS(LS), &
                             GM_REALS(location),  GM_IMAGS(location),  &
                             GM_REALS(location),  GM_IMAGS(location))
              IF (G_FIN .EQ. 13)  &
              call mat_wdiv( GM_REALS(location),  GM_IMAGS(location),  &
                             GM_REALS(LS), GM_IMAGS(LS), &
                             GM_REALS(location),  GM_IMAGS(location))
              IF (G_ERR .GT. 0) return
              location = location + 1
            enddo
          enddo
        enddo
      enddo
      G_VAR_ROWS(G_ARGUMENT_POINTER) = M*MA
      G_VAR_COLS(G_ARGUMENT_POINTER) = N*NA
!===================================================================================================================================
   case(9) ! COMMAND::CHOP

      eps0 = 1.0d0
      do                                                                  ! recalculate epsilon
         eps0 = eps0/2.0d0
         t = mat_flop(1.0d0 + eps0)
         if (t .le. 1.0d0) exit
      enddo
      eps0 = 2.0d0*eps0

      G_FLOP_COUNTER(2) = int(GM_REALS(location))
      if (G_SYM .ne. SEMI) then
         write(message,'(''CHOP '',I2,'' PLACES.'')') G_FLOP_COUNTER(2)
         call journal(message)
      endif

      eps = 1.0d0
      do                                                                  ! recalculate epsilon
         eps = eps/2.0d0
         t = mat_flop(1.0d0 + eps)
         if (t .le. 1.0d0) exit
      enddo
      eps = 2.0d0*eps

      t = GM_REALS(GM_BIGMEM-4)
      if (t.lt.eps .or. t.eq.eps0) GM_REALS(GM_BIGMEM-4) = eps
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
   case(3) ! COMMAND::SUM
      sr = 0.0d0
      si = 0.0d0
      mn = m*n
      do i = 1, mn
         ls = location+i-1
         sr = mat_flop(SR+GM_REALS(LS))
         si = mat_flop(SI+GM_IMAGS(LS))
      enddo
      GM_REALS(location) = sr
      GM_IMAGS(location) = si
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
!===================================================================================================================================
   case(4) ! COMMAND::PROD
      SR = 1.0D0
      SI = 0.0D0
      MN = M*N
      DO I = 1, MN
         LS = location+I-1
         call mat_wmul(GM_REALS(LS),GM_IMAGS(LS),SR,SI,SR,SI)
      enddo
      GM_REALS(location) = SR
      GM_IMAGS(location) = SI
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
!===================================================================================================================================
   case(5) ! COMMAND::USER
      ! The LALA statement "Y = user(X,s,t)" results in a call to the
      ! subroutine with a copy of the matrix X stored in the argument A,
      ! its column and row dimensions in M and N, and the scalar parameters
      ! s and t stored in S and T. If s and t are omitted, they are set
      ! to 0.0. After the return, A is stored in Y. The dimensions M and
      ! N may be reset within the subroutine. The statement Y = user(K)"
      ! results in a call with M = 1, N = 1 and A(1,1) = "float(K)".

      ! all of the arguments are in a vector that is part of the stack.
      ! the location points to the last value and M and N are set to the
      ! the row and column size of the last argument. G_RHS is the number
      ! of arguments.
      s = 0.0d0
      t = 0.0d0
      if (G_RHS .eq. 2) then
         s = GM_REALS(location)
         ! back up the stack one argument
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)   ! the end of argument X
         m = G_VAR_ROWS(G_ARGUMENT_POINTER)            ! the size of X(M,N)
         n = G_VAR_COLS(G_ARGUMENT_POINTER)
      elseif(G_RHS.gt.2)then
         t = GM_REALS(location)
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1       ! back up to s
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
         s = GM_REALS(location)
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1       ! back up to X
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
         m = G_VAR_ROWS(G_ARGUMENT_POINTER)
         n = G_VAR_COLS(G_ARGUMENT_POINTER)
      else  ! if not 1,2,3 should it be an error???
      endif
      ! ??? if user routine changes size of array and/or should pass vector instead of address ???
      ! ??? user routine cannot do complex values? Just REAL ???
      call usersub(GM_REALS(location:),m,n,s,t)
      call mat_rset(m*n,0.0d0,GM_IMAGS(location),1)      ! set the imaginary values to zero
      G_VAR_COLS(G_ARGUMENT_POINTER) = n               ! store the possibly new size
      G_VAR_ROWS(G_ARGUMENT_POINTER) = m
!===================================================================================================================================
   case(10) ! COMMAND::SHAPE
      ! store the two output values onto stack
      GM_REALS(location) = M
      GM_IMAGS(location) = 0.0D0
      GM_REALS(location+1) = N
      GM_IMAGS(location+1) = 0.0D0
      if(G_LHS.eq.1)then
         ! output is a 1x2 array so store values indicating the shape of the new stack value
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
         G_VAR_COLS(G_ARGUMENT_POINTER) = 2
      else
         ! output is two scalars
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
         G_VAR_COLS(G_ARGUMENT_POINTER) = 1

         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
         G_VAR_DATALOC(G_ARGUMENT_POINTER) = location+1
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
         G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      endif
!===================================================================================================================================
   case(2,14,15) ! COMMAND::DIAG=2
                 ! COMMAND::TRIL=14
                 ! COMMAND::TRIU=15
      k = 0
      if (G_RHS .eq. 2) then
         k = int(GM_REALS(location))
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
         m = G_VAR_ROWS(G_ARGUMENT_POINTER)
         n = G_VAR_COLS(G_ARGUMENT_POINTER)
      endif

      if (G_FIN .ge. 14) then ! COMMAND::TRIL, COMMAND::TRIU
            do j = 1, n
               ld = location + j - k - 1 + (j-1)*m
               select case(G_FIN)
               case(14)
                        ll = j - k - 1
                        ls = ld - ll
               case(15)
                        ll = m - j + k
                        ls = ld + 1
               end select
               if (ll .gt. 0) call mat_wset(ll, 0.0d0, 0.0d0, GM_REALS(ls), GM_IMAGS(ls), 1)
            enddo
      elseif (m .eq. 1 .or. n .eq. 1) then
         n = max(m,n)+iabs(k)

         if(too_much_memory( location+n*n - G_VAR_DATALOC(G_TOP_OF_SAVED)) )return

         G_VAR_ROWS(G_ARGUMENT_POINTER) = n
         G_VAR_COLS(G_ARGUMENT_POINTER) = n
         do jb = 1, n
            do ib = 1, n
               j = n+1-jb
               i = n+1-ib
               sr = 0.0d0
               si = 0.0d0
               if (k.ge.0) ls = location+i-1
               if (k.lt.0) ls = location+j-1
               ll = location+i-1+(j-1)*n
               if (j-i .eq. k) sr = GM_REALS(ls)
               if (j-i .eq. k) si = GM_IMAGS(ls)
               GM_REALS(LL) = sr
               GM_IMAGS(LL) = si
            enddo
         enddo
      else
         if (k.ge.0) mn=min(m,n-k)
         if (k.lt.0) mn=min(m+k,n)
         G_VAR_ROWS(G_ARGUMENT_POINTER) = max(mn,0)
         G_VAR_COLS(G_ARGUMENT_POINTER) = 1
         if (mn .le. 0) exit FUN6
         do i = 1, mn
            if (k.ge.0) ls = location+(i-1)+(i+k-1)*m
            if (k.lt.0) ls = location+(i-k-1)+(i-1)*m
            ll = location+i-1
            GM_REALS(ll) = GM_REALS(ls)
            GM_IMAGS(ll) = GM_IMAGS(ls)
         enddo
      endif
      exit FUN6
!-----------------------------------------------------------------------------------------------------------------------------------
   case(6,7,8,16) ! COMMAND::EYE,
                  ! COMMAND::RAND,
                  ! COMMAND::ONES,
                  ! COMMAND::ZEROS
      if (.not.(m.gt.1 .or. G_RHS.eq.0)) then

         if (G_RHS .eq. 2) then
            nn = int(GM_REALS(location))
            G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
            location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
            n = G_VAR_COLS(G_ARGUMENT_POINTER)
         endif

         if (G_FIN.eq.7.and.n.lt.GG_MAX_NAME_LENGTH)then        ! a call to RAND might be RAND('UNIFORM'|'SEED'|'NORMAL')
            id=blank
            do i = 1, min(GG_MAX_NAME_LENGTH,n)  ! in case it is one of these words store it in the ID array to test if it matches
               ls = location+i-1
               id(i) = int(GM_REALS(ls))
            enddo
            if(mat_eqid(id,unifor).or.mat_eqid(id,normal))then ! SWITCH UNIFORM AND NORMAL(if a matrix just happens to match, a bug)
               G_CURRENT_RANDOM_TYPE = id(1) - unifor(1)        ! set random type to generate by seeing if first letter is a "u"
               G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
               exit FUN6
            elseif (mat_eqid(id,seed)) then                     ! if a matrix just happens to match "seed" , a bug)
               if (G_RHS .eq. 2) G_CURRENT_RANDOM_SEED = nn
               GM_REALS(location) = G_CURRENT_RANDOM_SEED
               G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
               if (G_RHS .eq. 2) G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
               G_VAR_COLS(G_ARGUMENT_POINTER) = 1
               exit FUN6
            endif
         endif

         if (n .le. 1) then
            m = max(int(GM_REALS(location)),0)
            if (G_RHS .eq. 2) n = max(nn,0)
            if (G_RHS .ne. 2) n = m

            if(too_much_memory( location+m*n - G_VAR_DATALOC(G_TOP_OF_SAVED))) return

            G_VAR_ROWS(G_ARGUMENT_POINTER) = m
            G_VAR_COLS(G_ARGUMENT_POINTER) = n
            if (m*n .eq. 0) exit FUN6
         endif

      endif

      do j = 1, n
         do i = 1, m

           ll = location+i-1+(j-1)*m             ! location to place value

           GM_IMAGS(ll) = 0.0d0      ! all of these functions set imaginary values to zero

           select case(G_FIN)
           case( 6 ) !::EYE
              if(i.eq.j)then               ! on the diagonal
                 GM_REALS(ll) = 1.0d0
              else
                 GM_REALS(ll) = 0.0d0
              endif
           case( 7 ) !::RAND
              IF(G_CURRENT_RANDOM_TYPE.EQ.0) then
                 GM_REALS(ll)=mat_flop(mat_urand(G_CURRENT_RANDOM_SEED))
              else
                 do
                    sr = 2.0d0*mat_urand(G_CURRENT_RANDOM_SEED)-1.0d0
                    si = 2.0d0*mat_urand(G_CURRENT_RANDOM_SEED)-1.0d0
                    t = sr*sr + si*si
                    if (t .le. 1.0d0) exit
                 enddo

                 GM_REALS(ll) = mat_flop(sr*dsqrt((-(2.0d0*dlog(t)))/t))
              endif
           case( 8 ) !::ONES
              GM_REALS(ll) = 1.0d0
           case( 16) !::ZEROS
              GM_REALS(ll) = 0.0d0
           case default
              call journal('should not get here: internal error')
           end select
         enddo
      enddo
      exit FUN6
!===================================================================================================================================
   case(17) ! COMMAND::GETENV JSU
      GETENV : block
      character(len=:),allocatable :: answers(:)
      character(len=GG_LINELEN)    :: varname
      character(len=:),allocatable :: env_value
      allocate(character(len=0)    :: answers(0) )
      ! sort out what to do with an array of input later, for now concatenating into one string
      if (m.lt.1 .or. G_RHS.eq.0)then
         call journal('sc','<ERROR>getenv:needs an argument:rows=',m,' arg_count=',G_RHS)
         G_ERR=999
         return
      endif
      if (G_RHS.gt.1)then
         call journal('sc','<ERROR>getenv:too many arguments:arg_count=',G_RHS)
         G_ERR=999
         return
      endif

      ll=location
      do j=1,m
         varname=ade2str( int(GM_REALS(ll:ll+n-1)) )
         if(.not.mat_is_name(varname))then
               call journal('sc',' function name contains unacceptable characters')
               return
         endif
         ll=ll+n
         env_value=system_getenv(varname)
         ! do not leave it undefined or any variable on LHS will not be defined so make sure at least 1
         answers=[character(len=max(len(answers),len_trim(env_value),1)) :: answers,env_value]
      enddo

      m=size(answers,dim=1)
      n=len(answers)
      if(too_much_memory( location+m*n - G_VAR_DATALOC(G_TOP_OF_SAVED)) )return
      G_VAR_ROWS(G_ARGUMENT_POINTER) = m
      G_VAR_COLS(G_ARGUMENT_POINTER) = n
      if (m*n .eq. 0) exit FUN6

      ! so starting at GM_REALS(location) convert the characters to numbers and store the M x N number of characters
      do j = 1, n
         do i = 1, m
           ll = location+i-1+(j-1)*m             ! location to place value
           GM_IMAGS(ll) = 0.0d0             ! all of these functions set imaginary values to zero
           nn=iachar(answers(m)(j:j))
           if(nn.gt.0)then
              GM_REALS(ll) = real(nn)
           else
              call journal('sc','bad character')
              GM_REALS(ll) = 0.0d0
           endif
         enddo
      enddo
      endblock GETENV
      exit FUN6
!===================================================================================================================================
   case(18) ! COMMAND::DAT
      DATETIME: block
      integer :: time_values(8)
      ! store the two output values onto stack
      call date_and_time(values=time_values)
      GM_REALS(location:location+8-1) = dble(time_values)
      GM_IMAGS(location:location+8-1) = 0.0D0
      ! output is a 1x8 array so store values indicating the size of the new stack value
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 8
      endblock DATETIME
!===================================================================================================================================
   end select FUN6
end subroutine mat_matfn6
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_funs(id)

! ident_12="@(#)M_matrix::ml_funcs(3fp):scan function list and set G_FUN and G_FIN"

integer,intent(in)                :: id(GG_MAX_NAME_LENGTH)
integer                           :: selector
character(len=GG_MAX_NAME_LENGTH) :: name
integer                           :: i

   name=' '
   do i=1,size(id)
      if(id(i).le.0)exit
      if(id(i).le.G_CHARSET_SIZE)then
         name(i:i)=achar(id(i))
      else
         call journal('sc',' function name contains unacceptable characters:',name,'... ADE=',id(i),'position=',i)
         G_FIN = 0
         return
      endif
   enddo
   !
   !  find value for given function name to determine what to call for each name.
   !     o first digit indicates which routine to call (SUBROUTINE MAT_MATFN[1-6])
   !     o remaining digits indicate nth number in computed goto in called routine
   select case(name)
   case('eps');             selector=000
   case('flop');            selector=000

   case('inv');             selector=101
   case('det');             selector=102
   case('rcond');           selector=103
   case('lu');              selector=104
   case('invh','inverse_hilbert','invhilb');  selector=105
   case('chol');            selector=106
   case('rref');            selector=107

   case('sin');             selector=201
   case('cos');             selector=202
   case('atan');            selector=203
   case('exp');             selector=204
   case('sqrt');            selector=205
   case('log');             selector=206
   case('eig');             selector=211
   case('schur');           selector=212
   case('hess');            selector=213
   case('poly');            selector=214
   case('roots');           selector=215
   case('abs');             selector=221  !  calling  codes  corresponding  to  the  function  names
   case('round');           selector=222
   case('real');            selector=223
   case('imag','aimag');    selector=224
   case('conjg');           selector=225

   case('svd');             selector=301
   case('pinv');            selector=302
   case('cond');            selector=303
   case('norm');            selector=304
   case('rank');            selector=305

   case('qr');              selector=401
   case('orth');            selector=402

   case('exec','include','source','script');  selector=501
   case('save');            selector=502
   case('load');            selector=503
   case('print');           selector=504
   case('diary');           selector=505
   case('disp','display','echo');  selector=506
   case('base');            selector=507
   case('lines');           selector=508
   case('char');            selector=509
   case('plot');            selector=510
   case('rat');             selector=511
   case('debug');           selector=512
   case('show');            selector=513
   case('delete');          selector=514

   case('magic');           selector=601
   case('diag');            selector=602
   case('sum');             selector=603
   case('prod');            selector=604
   case('user');            selector=605
   case('eye');             selector=606
   case('rand','random');   selector=607
   case('ones');            selector=608
   case('chop');            selector=609
   case('shape');           selector=610
   case('kron');            selector=611
   case('tril');            selector=614
   case('triu');            selector=615
   case('zeros');           selector=616
   case('getenv');          selector=617
   case('dat','date_and_time');   selector=618

   case default !  function name was not found
      G_FIN = 0
      return
   end select

!  found name so get G_FIN and G_FUN value from corresponding code

   G_FIN = mod(selector,100) ! which case to select in called procedure
   G_FUN = selector/100      ! which routine to call (SUBROUTINE MAT_MATFN[1-6])

   if (G_RHS.eq.0 .and. selector.eq.606) G_FIN = 0
   if (G_RHS.eq.0 .and. selector.eq.607) G_FIN = 0
end subroutine mat_funs
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_copyid(x,y)

! ident_13="@(#)M_matrix::mat_copyid(3fp): copy a name to allow an easy way to store a name"

integer,intent(out) :: x(GG_MAX_NAME_LENGTH)
integer,intent(in)  :: y(GG_MAX_NAME_LENGTH)
integer             :: i
      do i = 1, GG_MAX_NAME_LENGTH
         x(i) = y(i)
      enddo
end subroutine mat_copyid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getval(s)

! ident_14="@(#)M_matrix::mat_getval(3fp): form numerical value from string of integer characters"

doubleprecision,intent(out) :: s
      s = 0.0d0
      INFINITE: do
         select case(G_CHRA)
         case(iachar('0')); s = 10.0d0*s + 0.0d0
         case(iachar('1')); s = 10.0d0*s + 1.0d0
         case(iachar('2')); s = 10.0d0*s + 2.0d0
         case(iachar('3')); s = 10.0d0*s + 3.0d0
         case(iachar('4')); s = 10.0d0*s + 4.0d0
         case(iachar('5')); s = 10.0d0*s + 5.0d0
         case(iachar('6')); s = 10.0d0*s + 6.0d0
         case(iachar('7')); s = 10.0d0*s + 7.0d0
         case(iachar('8')); s = 10.0d0*s + 8.0d0
         case(iachar('9')); s = 10.0d0*s + 9.0d0
         case default
            exit INFINITE
         end select
         call mat_getch() ! get next character
      enddo INFINITE
end subroutine mat_getval
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getch()

! ident_15="@(#)M_matrix::mat_getch(3f): get next character from input line into G_CHRA"

   G_CHRA = G_LIN(G_LINE_POINTER(4))
   if (G_CHRA .ne. GG_EOL) G_LINE_POINTER(4) = G_LINE_POINTER(4) + 1

end subroutine mat_getch
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_base(x,base,eps,s,n)

! ident_16="@(#)M_matrix::mat_base(3fp): store representation of x in s(1:n) using specified base"

doubleprecision            :: x
doubleprecision,intent(in) :: base
doubleprecision,intent(in) :: eps
doubleprecision            :: s(*)
integer                    :: n

doubleprecision :: t

integer      :: l
integer      :: j
integer      :: k
integer      :: m

   l = 1
   if (x .ge. 0.0d0)then
      s(l) = plus
   else
      s(l) = minus
   endif
   s(l+1) = zero
   s(l+2) = dot
   x = dabs(x)
   if (x .ne. 0.0d0) then
      k = dlog(x)/dlog(base)
   else
      k = 0
   endif
   if (x .gt. 1.0d0) k = k + 1
   x = x/base**k
   if (base*x .ge. base) k = k + 1
   if (base*x .ge. base) x = x/base
   if (eps .ne. 0.0d0)then
      m = (-1)*dlog(eps)/dlog(base) + 4
   else
      m = 54
   endif
   do l = 4, m
      x = base*x
      j = int(x)
      s(l) = dble(j)
      x = x - s(l)
      s(l)=s(l)+48
   enddo
   s(m+1) = comma
   if (k .ge. 0) s(m+2) = plus
   if (k .lt. 0) s(m+2) = minus
   t = dabs(dble(k))
   n = m + 3
   if (t .ge. base) n = n + int(dlog(t)/dlog(base))
   l = n
   INFINITE: do
      j = int(dmod(t,base))
      s(l) = dble(j+48)
      l = l - 1
      t = t/base
      if (l .lt. m+3) exit
   enddo INFINITE
end subroutine mat_base
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_print(ID,K)

! ident_17="@(#)M_matrix::mat_print(3fp): primary output routine"

integer           :: id(GG_MAX_NAME_LENGTH)
integer           :: k

character(len=81) :: message
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
integer           :: location,m,n,mn
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
integer,save      :: fno(11)= [11,12,21,22,23,24,31,32,33,34,-1]
integer,save      :: fnl(11)= [12, 6, 8, 4, 6, 3, 4, 2, 3, 1, 1]
integer           :: itype

! FORMAT NUMBERS AND LENGTHS
! G_FMT   1       2       3       4       5
!       SHORT   LONG   SHORT E  LONG E    Z
! TYP   1       2       3
!    INTEGER  REAL   COMPLEX
!.......................................................................
   if (G_LINECOUNT(1) .lt. 0) goto 99
!.......................................................................
   location = G_VAR_DATALOC(k)
   m = G_VAR_ROWS(k)
   n = G_VAR_COLS(k)
   mn = m*n
   typ = 1
   s = 0.0d0
   itype=-9999
   do i = 1, mn
      ls = location+i-1
      tr = GM_REALS(ls)
      ti = GM_IMAGS(ls)
      s = dmax1(s,dabs(tr),dabs(ti))
      if (mat_round(tr) .ne. tr) typ = max(2,typ)
      if (ti .ne. 0.0d0) typ = 3
   enddo
   if (s .ne. 0.0d0) s = dlog10(s)
   ks = int(s)
   if (-2 .le. ks .and. ks .le. 1) ks = 0
   if (ks .eq. 2 .and. G_FMT .eq. 1 .and. typ .eq. 2) ks = 0

   f=0                          ! initialize to bad value
   if (typ .eq. 1 )then         ! if output type is integer
      if( ks .le. 2 )then
         f = 1
      else
         f = 2
      endif
   endif
   if (typ .eq. 1 .and. ks .gt. 9) typ = 2  !change type from integer to real

   if (typ .eq. 2) f = G_FMT + 2   ! if type is real
   if (typ .eq. 3) f = G_FMT + 6   ! if type is complex
   if(f.eq.0)then
      call journal('*mat_print* internal error - bad type')
      goto 99
   endif

   if (mn.eq.1 .and. ks.ne.0 .and. G_FMT.lt.3 .and. typ.ne.1) f = f+2

   if (G_FMT .eq. 5) f = 11

   jinc = fnl(f)
   f = fno(f)

   s = 1.0d0
   if (f.eq.21 .or. f.eq.22 .or. f.eq.31 .or. f.eq.32) s = 10.0D0**ks
   ls = ((n-1)/jinc+1)*m + 2
!.......................................................................
   IF (G_LINECOUNT(1) + LS .gt. G_LINECOUNT(2)) then
      G_LINECOUNT(1) = 0

      if(G_PROMPT)then
         WRITE(message, "(' AT LEAST ',I5,' MORE LINES.','  ENTER BLANK LINE TO CONTINUE OUTPUT.')") LS
         call journal(message)

         READ(G_INPUT_LUN,'(a1)',END=19) LS_CHAR  ! read response to pause from standard input
         IF (LS_CHAR .EQ. ' ') goto 20      ! if blank or a return display the values
         G_LINECOUNT(1) = -1
         goto 99
      else
         LS_CHAR = ' '
         goto 20
      endif
   19 continue
      call mat_files(-G_INPUT_LUN,G_BUF)
   endif
   20 continue
!.......................................................................
   call journal(' ')
   call mat_print_id(ID,-1)
   G_LINECOUNT(1) = G_LINECOUNT(1)+2
   if (s .ne. 1.0d0)then
      write(message,'(''  '',1PD9.1," *")') s
      call journal(message)
   endif
   do j1 = 1, n, jinc
      j2 = min(n, j1+jinc-1)
      if (n .gt. jinc)then
         write(message,'(''     COLUMNS'',I6,'' THRU'',I6)') j1,j2
         call journal(message)
      endif
      do i = 1, m
         jm = j2-j1+1
         do j = 1, jm
            ls = location+i-1+(j+j1-2)*m
            pr(j) = GM_REALS(ls)/s
            pi(j) = dabs(GM_IMAGS(ls)/s)
            sig(j) = plus
            if (GM_IMAGS(ls) .lt. 0.0d0) sig(j) = minus
         enddo

         select case(F)
         case(11)
            form='(1X,12F6.0)'          ! integer
            istep=12
            itype= 777
         case(12)
            form='(1X,6F12.0)'          ! integer
            istep=6
            itype= 777
         case(21)
            form='(1X,F9.4,7F10.4)'     ! 8 numbers
            istep=8
            itype= 999
         case(22)
            form='(1X,F19.15,3F20.15)'  ! 4 numbers
            istep=4
            itype= 999
         case(23)
            form='(1X,1P6D13.4)'        ! 6 numbers
            istep=6
            itype= 999
         case(24)
            form='(1X,1P3D24.15)'       ! 3 numbers
            istep=3
            itype= 999
         case(31)
            form='(1X,4(F9.4,1X,A1,F7.4,''i''))'                       ! 4x3
            istep=12
            itype= 888
         case(32)
            form='(1X,F19.15,A1,F18.15,''i'',F20.15,A1,F18.15,''i'')'  ! 6
            istep=6
            itype= 888
         case(33)
            form='(1X,3(1PD13.4,1X,A1,1PD10.4,''i''))'                 ! 9
            istep=9
            itype= 888
         case(34)
            form='(1X,1PD24.15,1X,A1,1PD21.15,''i'')'                  ! 3
            istep=3
            itype= 888
         case(-1)
            call mat_formz(GM_REALS(ls),GM_IMAGS(ls))
            istep=-1
            itype=-1
         case default
            call journal('*internal error*')
            goto 99
         end select

         ! print data based on type
         if(itype.gt.0)then
            do j3=1,jm,istep
               select case(itype)
               case(777); write(message,form)(pr(j),j=j3,min(j3+istep-1,jm))
               case(999); write(message,form)(pr(j),j=j3,min(j3+istep,jm))
               case(888); write(message,form)(pr(j),sig(j),pi(j),j=j3,min(j3+istep-1,jm))
               end select
               call journal(message)
            enddo
         endif

         G_LINECOUNT(1) = G_LINECOUNT(1)+1
      enddo
   enddo

99 continue
   flush(unit=STDOUT,iostat=ios)

end subroutine mat_print
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_formz(x,y)

! ident_18="@(#)M_matrix::mat_formz: system dependent routine to print with z format"

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

! ident_19="@(#)M_matrix::mat_prompt(3f): issue interactive prompt with optional pause"

integer,intent(in) :: pause
character(len=1)   :: dummy

   if(.not.G_PROMPT)return ! in batch mode
   ! write prompt using format that stays on current line
   if(G_INPUT_LUN.eq.STDIN)then
     WRITE(STDOUT,'(''<>'')',advance='no')   ! write prompt to interactive input
     if (pause .eq. 1) read(G_INPUT_LUN,'(a1)') dummy
   endif

end subroutine mat_prompt
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack1(op)

! ident_20="@(#)M_matrix::mat_stack1(3f): Unary Operations"

integer           :: op
integer           :: i
integer           :: j
integer           :: location
integer           :: ll
integer           :: ls
integer           :: m
integer           :: mn
integer           :: n

   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   m = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n = G_VAR_COLS(G_ARGUMENT_POINTER)
   mn = m*n
   if (mn .eq. 0) then
   elseif (op .ne. quote) then                                 ! unary minus
      call mat_wrscal(MN,-1.0D0,GM_REALS(location),GM_IMAGS(location),1)
   else                                                        ! transpose
      ll = location + mn

      if(too_much_memory( ll+mn - G_VAR_DATALOC(G_TOP_OF_SAVED)) )return

      call mat_wcopy(MN,GM_REALS(location),GM_IMAGS(location),1,GM_REALS(ll),GM_IMAGS(ll),1)
      M = G_VAR_COLS(G_ARGUMENT_POINTER)
      N = G_VAR_ROWS(G_ARGUMENT_POINTER)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = m
      G_VAR_COLS(G_ARGUMENT_POINTER) = n
      do i = 1, m
         do j = 1, n
            ls = location+mn+(j-1)+(i-1)*n
            ll = location+(i-1)+(j-1)*m
            GM_REALS(ll) = GM_REALS(ls)
            GM_IMAGS(ll) = -GM_IMAGS(ls)
         enddo
      enddo
   endif
end subroutine mat_stack1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_print_id(id,argcnt)

! ident_21="@(#)M_matrix::mat_print_id(3fp): print table of variable id names (up to) eight per line"

!     ID     Is array of GG_MAX_NAME_LENGTH character IDs to print
!     ARGCNT is number of IDs to print
!            If = -1, print one ID with an "  =" suffix
!
integer            :: id(GG_MAX_NAME_LENGTH,*)
integer            :: argcnt
integer            :: id_counter                               !
integer            :: i, j, k
integer            :: line_position                            ! pointer into output line being built
integer            :: linebuf(8*GG_MAX_NAME_LENGTH+2*8+1)      ! scratch buffer for building up line
character(len=(8*GG_MAX_NAME_LENGTH+2*8+1)) :: mline           ! scratch space for building line to print

   id_counter = 1                                         ! which ID to start the line with
   INFINITE : do
      linebuf(1)=blank                                    ! put a space at beginning of line
      line_position = 2
      do j = id_counter,min(id_counter+7,iabs(argcnt))    ! copy up to eight names into buffer
         do i = 1, GG_MAX_NAME_LENGTH                     ! copy one name into buffer
            k = id(i,j)                                   ! this is the kth letter of the set
            linebuf(line_position) = k
            if(linebuf(line_position).ne.blank)line_position = line_position+1   ! increment pointer into output
         enddo
         linebuf(line_position+0)=blank         ! put two spaces between names
         linebuf(line_position+1)=blank
         line_position=line_position+2
      enddo
      if (argcnt .eq. -1) then                            ! special flag to print one word and  =
         linebuf(line_position) = equal                   ! put value for equal sign into buffer
      else
         line_position=line_position-3                    ! was prepared for another ID with two blanks
      endif

      call mat_buf2str(mline,linebuf,line_position)       ! write LINEBUF(1:line_position) line to a character variable
      call journal(mline)                                 ! print the line to stdout

      id_counter = id_counter+8                           ! prepare to get up to eight more IDs
      if (id_counter .gt. iabs(argcnt)) exit INFINITE     ! if not done do another line
   enddo INFINITE
end subroutine mat_print_id
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack_put(id)

! ident_22="@(#)M_matrix::mat_stack_put(3fp): put variables into storage"

integer  :: id(GG_MAX_NAME_LENGTH)
integer  :: i, j, k
integer  :: ib
integer  :: km1
integer  :: location
integer  :: l1,l2, li,lj,lk, ll,ls,lt
integer  :: m, m1,m2, mk
integer  :: mn, mn1, mn2, mnk
integer  :: mt
integer  :: n, nk, nt

   if (G_ARGUMENT_POINTER .le. 0) then
      call mat_err(1)  ! Improper multiple assignment
      return
   endif

   call mat_funs(id)
   if (G_FIN .ne. 0) then
      call mat_err(25) ! Can not use function name as variable
      return
   endif

   m = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n = G_VAR_COLS(G_ARGUMENT_POINTER)
   if (m .gt. 0) then
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   elseif(m.lt.0) then
      call mat_err(14) ! EYE-dentity undefined by CONTEXT
      return
   elseif (m .eq. 0 .and. n .ne. 0) then
      goto 99
   else  ! what about m zero and n not zero???
   endif

   mn = m*n
   lk = 0
   mk = 1
   nk = 0
   lt = 0
   mt = 0
   nt = 0

   ! unconditionally add name to end of list
   call mat_copyid(G_VAR_IDS(1,G_TOP_OF_SAVED-1),id)

   ! did variable already exist (knowning name is there at least once)
   do k=GG_MAX_NUMBER_OF_NAMES,1,-1
      if (mat_eqid(G_VAR_IDS(1:,k),id)) exit
   enddo

   if (k .ne. G_TOP_OF_SAVED-1) then        ! variable exists
      lk = G_VAR_DATALOC(k)
      mk = G_VAR_ROWS(k)
      nk = G_VAR_COLS(k)
      mnk = mk*nk
      if (G_RHS .gt. 2) then
         call mat_err(15)                   ! Improper assignment to submatrix
         return
      elseif (G_RHS .ne. 0) then
         mt = mk
         nt = nk
         lt = location + mn
         if(too_much_memory( lt + mnk - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )then
            return
         endif
         call mat_wcopy(mnk,GM_REALS(lk),GM_IMAGS(lk),1,GM_REALS(lt),GM_IMAGS(lt),1)
      endif

      ! does it fit
      if (G_RHS.eq.0 .and. mn.eq.mnk) then    ! size of existing array did not change
         goto 40
      endif

      if (k .ge. GG_MAX_NUMBER_OF_NAMES-3) then
         call mat_err(13) ! Improper assignment to PERMANENT VARIABLE
         return
      endif

      if (k .ne. G_TOP_OF_SAVED) then
         ! shift storage
         ls = G_VAR_DATALOC(G_TOP_OF_SAVED)
         ll = ls + mnk
         call mat_wcopy(lk-ls,GM_REALS(ls),GM_IMAGS(ls),-1,GM_REALS(ll),GM_IMAGS(ll),-1)
         km1 = k-1
         do ib = G_TOP_OF_SAVED, km1
            i = G_TOP_OF_SAVED+km1-ib
            call mat_copyid(G_VAR_IDS(1,i+1),G_VAR_IDS(1,i))
            G_VAR_ROWS(i+1) = G_VAR_ROWS(i)
            G_VAR_COLS(i+1) = G_VAR_COLS(i)
            G_VAR_DATALOC(i+1) = G_VAR_DATALOC(i)+mnk
         enddo
      endif

      ! destroy old variable
      G_TOP_OF_SAVED = G_TOP_OF_SAVED+1
   endif
!
   ! create new variable
   if (mn .eq. 0) then
      goto 99
   endif

   if (G_TOP_OF_SAVED-2 .le. G_ARGUMENT_POINTER) then
      call mat_err(18) ! Too many names
      return
   endif

   k = G_TOP_OF_SAVED-1
   call mat_copyid(G_VAR_IDS(1,k), id)

   if (G_RHS .eq. 1) then
      !  vect(arg)
      if (G_VAR_ROWS(G_ARGUMENT_POINTER-1) .lt. 0) then
         goto 59
      endif
      mn1 = 1
      mn2 = 1
      l1 = 0
      l2 = 0
      if (n.ne.1 .or. nk.ne.1) then
         if (m.ne.1 .or. mk.ne.1) then
            call mat_err(15) ! Improper assignment to submatrix
            return
         endif
         l2 = G_VAR_DATALOC(G_ARGUMENT_POINTER-1)
         m2 = G_VAR_ROWS(G_ARGUMENT_POINTER-1)
         mn2 = m2*G_VAR_COLS(G_ARGUMENT_POINTER-1)
         m1 = -1
         goto 60
      endif
      l1 = G_VAR_DATALOC(G_ARGUMENT_POINTER-1)
      m1 = G_VAR_ROWS(G_ARGUMENT_POINTER-1)
      mn1 = m1*G_VAR_COLS(G_ARGUMENT_POINTER-1)
      m2 = -1
      goto 60
   elseif (G_RHS .eq. 2)then
      ! matrix(arg,arg)
      if (G_VAR_ROWS(G_ARGUMENT_POINTER-1).lt.0 .and. G_VAR_ROWS(G_ARGUMENT_POINTER-2).lt.0) then
         goto 59
      endif
      l2 = G_VAR_DATALOC(G_ARGUMENT_POINTER-1)
      m2 = G_VAR_ROWS(G_ARGUMENT_POINTER-1)
      mn2 = m2*G_VAR_COLS(G_ARGUMENT_POINTER-1)
      if (m2 .lt. 0) mn2 = n
      l1 = G_VAR_DATALOC(G_ARGUMENT_POINTER-2)
      m1 = G_VAR_ROWS(G_ARGUMENT_POINTER-2)
      mn1 = m1*G_VAR_COLS(G_ARGUMENT_POINTER-2)
      if (m1 .lt. 0) mn1 = m
      goto 60
   endif
!
!  STORE
40 continue
   if (k .lt. GG_MAX_NUMBER_OF_NAMES) G_VAR_DATALOC(k) = G_VAR_DATALOC(k+1) - mn
   G_VAR_ROWS(k) = m
   G_VAR_COLS(k) = n

   lk = G_VAR_DATALOC(k)
   call mat_wcopy(mn,GM_REALS(location),GM_IMAGS(location),-1,GM_REALS(lk),GM_IMAGS(lk),-1)
   goto 90
!===================================================================================================================================
59 continue
   if (mn .ne. mnk) then
      call mat_err(15) ! Improper assignment to submatrix
      return
   endif

   lk = G_VAR_DATALOC(k)
   call mat_wcopy(mn,GM_REALS(location),GM_IMAGS(location),-1,GM_REALS(lk),GM_IMAGS(lk),-1)
   goto 90
!===================================================================================================================================
60 continue
   if (mn1.ne.m .or. mn2.ne.n) then
      call mat_err(15) ! Improper assignment to submatrix
      return
   endif
   ll = 1
   if (m1 .ge. 0) then
      do i = 1, mn1
         ls = l1+i-1
         mk = max(mk,int(GM_REALS(ls)))
         ll = min(ll,int(GM_REALS(ls)))
      enddo
   endif

   mk = max(mk,m)
   if (m2 .ge. 0) then
      do i = 1, mn2
         ls = l2+i-1
         nk = max(nk,int(GM_REALS(ls)))
         ll = min(ll,int(GM_REALS(ls)))
      enddo
   endif
   nk = max(nk,n)
   if (ll .lt. 1) then
      call mat_err(21) ! Subscript out of range
      return
   endif
   mnk = mk*nk
   lk = G_VAR_DATALOC(k+1) - mnk

   if(too_much_memory( lt + mt*nt - lk) )return

   G_VAR_DATALOC(k) = lk
   G_VAR_ROWS(k) = mk
   G_VAR_COLS(k) = nk
   call mat_wset(mnk,0.0d0,0.0d0,GM_REALS(lk),GM_IMAGS(lk),1)
   if (nt .ge. 1) then
      do j = 1, nt
         ls = lt+(j-1)*mt
         ll = lk+(j-1)*mk
         call mat_wcopy(mt,GM_REALS(ls),GM_IMAGS(ls),-1,GM_REALS(ll),GM_IMAGS(ll),-1)
      enddo
   endif

   do j = 1, n
      do i = 1, m
         li = l1+i-1
         if (m1 .gt. 0) li = l1 + int(GM_REALS(li)) - 1
         lj = l2+j-1
         if (m2 .gt. 0) lj = l2 + int(GM_REALS(lj)) - 1
         ll = lk+li-l1+(lj-l2)*mk
         ls = location+i-1+(j-1)*m
         GM_REALS(ll) = GM_REALS(ls)
         GM_IMAGS(ll) = GM_IMAGS(ls)
      enddo
   enddo
   goto 90
!===================================================================================================================================
! print if desired and pop stack
90 continue
   if (G_SYM.ne.semi .and. G_LINECOUNT(3).eq.0) call mat_print(id,k) ! if not a semi-colon and "semi" mode print
   if (G_SYM.eq.semi .and. G_LINECOUNT(3).eq.1) call mat_print(id,k) ! if a semi-colon and "semi" mode off print
   if (k .eq. G_TOP_OF_SAVED-1) G_TOP_OF_SAVED = G_TOP_OF_SAVED-1

99 continue
   if (m .eq. 0) then
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 1
   else
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 1 - G_RHS
   endif
end subroutine MAT_STACK_PUT
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##THE PARSER-INTERPRETER (10)
!!
!!    The structure of the parser-interpreter is similar to that of Wirth's
!!    compiler [6] for his simple language, PL/0 , except that LALA is
!!    programmed in Fortran, which does not have explicit recursion. The
!!    interrelation of the primary subroutines is shown in the following
!!    diagram.
!!
!!          MAIN
!!            |
!!          LALA     |--CLAUSE
!!            |       |    |
!!          PARSE-----|--EXPR----TERM----FACTOR
!!                    |    |       |       |
!!                    |    |-------|-------|
!!                    |    |       |       |
!!                    |  STACK1  STACK2  STACKG
!!                    |
!!                    |--STACKP--PRINT
!!                    |
!!                    |--COMAND
!!                    |
!!                    |
!!                    |          |--CGECO
!!                    |          |
!!                    |          |--CGEFA
!!                    |          |
!!                    |--MATFN1--|--CGESL
!!                    |          |
!!                    |          |--CGEDI
!!                    |          |
!!                    |          |--CPOFA
!!                    |
!!                    |
!!                    |          |--IMTQL2
!!                    |          |
!!                    |          |--HTRIDI
!!                    |          |
!!                    |--MATFN2--|--HTRIBK
!!                    |          |
!!                    |          |--CORTH
!!                    |          |
!!                    |          |--COMQR3
!!                    |
!!                    |
!!                    |--MATFN3-----CSVDC
!!                    |
!!                    |
!!                    |          |--CQRDC
!!                    |--MATFN4--|
!!                    |          |--CQRSL
!!                    |
!!                    |
!!                    |          |--FILES
!!                    |--MATFN5--|
!!                               |--SAVLOD
!!
!!    Subroutine MAT_PARSE controls the interpretation of each statement. It
!!    calls subroutines that process the various syntactic quantities such
!!    as command, expression, term and factor. A fairly simple program
!!    stack mechanism allows these subroutines to recursively "call"
!!    each other along the lines allowed by the syntax diagrams. The four
!!    STACK subroutines manage the variable memory and perform elementary
!!    operations, such as matrix addition and transposition.
!!
!!    The four subroutines MATFN1 though MATFN4 are called whenever "serious"
!!    matrix computations are required. They are interface routines which
!!    call the various LINPACK and EISPACK subroutines. MATFN5 primarily
!!    handles the file access tasks.
SUBROUTINE mat_parse()
integer            :: id(GG_MAX_NAME_LENGTH)
integer            :: excnt
integer            :: pts
integer,parameter  :: ans(GG_MAX_NAME_LENGTH)  = [iachar(['a','n','s',' ',' ',' ',' ']),GG_PAD(8:)]
integer,parameter  :: ennd(GG_MAX_NAME_LENGTH) = [iachar(['e','n','d',' ',' ',' ',' ']),GG_PAD(8:)]
integer,parameter  :: else(GG_MAX_NAME_LENGTH) = [iachar(['e','l','s','e',' ',' ',' ']),GG_PAD(8:)]
integer            :: p
integer            :: r
integer            :: i5
integer            :: ierr
integer            :: j
integer            :: k
integer            :: location
integer            :: ls
integer            :: n
character(len=:),allocatable :: symbol
!
   01 continue
      r = 0
      if (G_ERR .gt. 0) G_PTZ = 0
      if (G_ERR.le.0 .and. G_PT.gt.G_PTZ) r = G_RSTK(G_PT)

      if (r.eq.15) goto 93
      if (r.eq.16 .or. r.eq.17) goto 94
      G_SYM = GG_EOL
      G_ARGUMENT_POINTER = 0
      if (G_RIO .ne. G_INPUT_LUN) call mat_files(-G_RIO,G_BUF)
      G_RIO = G_INPUT_LUN
      G_LINECOUNT(3) = 0
      G_LINECOUNT(4) = 2
      G_LINE_POINTER(1) = 1
   10 continue  ! get a new line if the current line has ended
      if (G_SYM.eq.GG_EOL.and.mod(G_LINECOUNT(4)/2,2).eq.1) call mat_prompt(G_LINECOUNT(4)/4)
      if (G_SYM .eq. GG_EOL) call mat_getlin()
      G_ERR = 0
      G_PT = G_PTZ

   15 continue   ! (continue) processing current line
      excnt = 0
      G_LHS = 1
      call mat_copyid(id,ans) ! copy ans to id
      call mat_getsym()

      if (G_SYM .eq. colon) then
         call mat_getsym()
      endif

      if (G_SYM.eq.SEMI .or. G_SYM.eq.COMMA .or. G_SYM.eq.GG_EOL) goto 80

      if (G_SYM .eq. isname) then
         ! lhs begins with name
         call ints2str(G_SYN,symbol,ierr)              ! convert ID to a character variable
         call mat_comand(symbol)
         IF (G_ERR .GT. 0) goto 01
         IF (G_FUN .EQ. 99) goto 95
         IF (G_FIN .EQ. -15) goto 80
         IF (G_FIN .LT. 0) goto 91
         IF (G_FIN .GT. 0) goto 70
         ! if name is a function, must be rhs
         G_RHS = 0
         call mat_funs(G_SYN)
         IF (G_FIN .NE. 0)then
            goto 50
         endif
         ! peek one character ahead
         IF (G_CHRA.EQ.SEMI .OR. G_CHRA.EQ.COMMA .OR. G_CHRA.EQ.GG_EOL) call mat_copyid(ID,G_SYN)
         IF (G_CHRA .EQ. EQUAL) then
            ! lhs is simple variable
            call mat_copyid(ID,G_SYN)
            call mat_getsym()
            call mat_getsym()
            goto 50
         endif
         IF (G_CHRA .EQ. LPAREN .or. G_CHRA .EQ. LBRACE) then
            ! lhs is name(...)
            G_LINE_POINTER(5) = G_LINE_POINTER(4)
            call mat_copyid(ID,G_SYN)
            call mat_getsym()
            goto 32
         endif
         goto 50
      endif
      if (G_SYM .eq. less .or. G_SYM .eq. lbracket) goto 40
      if (G_SYM .eq. great .or. G_SYM .eq. rbracket) goto 45
      goto 50
!.......................................................................
!     lhs is name(...)
   32 continue
      call mat_getsym()
      excnt = excnt+1
      G_PT = G_PT+1
      call mat_copyid(G_IDS(1,G_PT), id)
      G_PSTK(G_PT) = excnt
      G_RSTK(G_PT) = 1
!     *call* expr
      goto 92
!.......................................................................
   35 continue
      call mat_copyid(id,G_IDS(1,G_PT))
      excnt = G_PSTK(G_PT)
      G_PT = G_PT-1
      if (G_SYM .eq. comma) goto 32
      if ((G_SYM .ne. rparen) .and. (G_SYM.ne.rbrace)) then
         call mat_err(3)
         goto 01
         return  ! ???? cannot unconditionally goto and return
      endif
      if ((G_SYM .eq. rparen) .or. (G_SYM.eq.rbrace)) call mat_getsym()
      if (G_SYM .eq. equal) goto 50
!     lhs is really rhs, forget scan just done
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - excnt
      G_LINE_POINTER(4) = G_LINE_POINTER(5)
      G_CHRA = lparen
      G_SYM = isname
      call mat_copyid(G_SYN,id)
      call mat_copyid(id,ans)
      excnt = 0
      goto 50
!.......................................................................
!     multiple lhs
   40 continue
      G_LINE_POINTER(5) = G_LINE_POINTER(4)
      pts = G_PT
      call mat_getsym()
   41 continue
      if (G_SYM .ne. isname)then
         goto 43
      endif
      call mat_copyid(id,G_SYN)
      call mat_getsym()
      if (G_SYM .eq. great.or. G_SYM.eq.rbracket)then
         call mat_getsym()
         if (G_SYM .eq. equal) goto 50
         goto 43
      endif
      if (G_SYM .eq. comma) call mat_getsym()
      G_PT = G_PT+1
      G_LHS = G_LHS+1
      G_PSTK(G_PT) = 0
      call mat_copyid(G_IDS(1,G_PT),id)
      goto 41
!.......................................................................
   43 continue
      G_LINE_POINTER(4) = G_LINE_POINTER(5)
      G_PT = pts
      G_LHS = 1
      G_SYM = less
      G_CHRA = G_LIN(G_LINE_POINTER(4)-1)
      call mat_copyid(id,ans)
      goto 50
!.......................................................................
!     macros string
   45 continue
      call mat_getsym()
      if ((G_SYM.eq.less .or. G_SYM.eq.lbracket) .and. G_CHRA.eq.GG_EOL) then
         call mat_err(28) ! Empty macro
         goto 01
      endif
      G_PT = G_PT+1
      G_RSTK(G_PT) = 20
!     *call* expr
      goto 92
!.......................................................................
   46 continue
      G_PT = G_PT-1
      if ((G_SYM.ne.less .and. G_SYM.ne.lbracket) .and. G_SYM.ne.GG_EOL) then
         call mat_err(37) ! Improper MACROS
         goto 01
      endif
      if (G_SYM .eq. less .or. G_SYM.eq. lbracket) call mat_getsym()
      k = G_LINE_POINTER(6)
      G_LIN(k+1) = G_LINE_POINTER(1)
      G_LIN(k+2) = G_LINE_POINTER(2)
      G_LIN(k+3) = G_LINE_POINTER(6)
      G_LINE_POINTER(1) = k + 4
!     transfer stack to input line
      k = G_LINE_POINTER(1)
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      n = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
      do j = 1, n
         ls = location + j-1
         G_LIN(k) = int(GM_REALS(ls))
         if (G_LIN(k).lt.0 .or. G_LIN(k).ge.G_CHARSET_SIZE) then
            call mat_err(37) ! improper MACROS
            return
         endif
         if (k.lt.1024) k = k+1
         if (k.eq.1024) then
            call journal('sc',' input buffer limit is',k,'characters')
          endif
      enddo
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
      G_LIN(K) = GG_EOL; G_LIN(K+1:)=blank
      G_LINE_POINTER(6) = k
      G_LINE_POINTER(4) = G_LINE_POINTER(1)
      G_LINE_POINTER(3) = 0
      G_LINE_POINTER(2) = 0
      G_LINECOUNT(1) = 0
      G_CHRA = blank
      G_PT = G_PT+1
      G_PSTK(G_PT) = G_LINE_POINTER(1)
      G_RSTK(G_PT) = 21
!     *call* parse
      goto 15
!.......................................................................
   49 continue
      G_PT = G_PT-1
      k = G_LINE_POINTER(1) - 4
      G_LINE_POINTER(1) = G_LIN(K+1)
      G_LINE_POINTER(4) = G_LIN(K+2)
      G_LINE_POINTER(6) = G_LIN(K+3)
      G_CHRA = BLANK
      call mat_getsym()
      goto 80
!.......................................................................
!     lhs finished, start rhs
   50 continue
      if (G_SYM .eq. equal) call mat_getsym()
      G_PT = G_PT+1
      call mat_copyid(G_IDS(1,G_PT),id)
      G_PSTK(G_PT) = excnt
      G_RSTK(G_PT) = 2
!     *call* expr
      goto 92
!.......................................................................
!     store results
   60 continue
      G_RHS = G_PSTK(G_PT)
      call MAT_STACK_PUT(G_IDS(1,G_PT))
      if (G_ERR .gt. 0) goto 01
      G_PT = G_PT-1
      G_LHS = G_LHS-1
      if (G_LHS .gt. 0) goto 60
      goto 70
!.......................................................................
!     update and possibly print operation counts
   70 continue
      k = G_FLOP_COUNTER(1)
      if (K .ne. 0) GM_REALS(GM_BIGMEM-3) = dble(k)
      GM_REALS(GM_BIGMEM-2) = GM_REALS(GM_BIGMEM-2) + dble(K)
      G_FLOP_COUNTER(1) = 0
      if (.not.(G_CHRA.eq.comma .or. (G_SYM.eq.comma .and. G_CHRA.eq.GG_EOL)))goto 80
      call mat_getsym()
      i5 = 10**5

      if (k .eq. 0) then
         call journal('   no flops')
      elseif (k .EQ. 1) then
         call journal('    1 flop')
      else
         call journal('sc','',k,' flops')
      endif
      goto 80
!.......................................................................
!     finish statement
   80 continue
      G_FIN = 0
      p = 0
      r = 0
      if (G_PT .gt. 0) p = G_PSTK(G_PT)
      if (G_PT .gt. 0) r = G_RSTK(G_PT)
      if (G_SYM.eq.comma .or. G_SYM.eq.semi) goto 15
      if (r.eq.21 .and. p.eq.G_LINE_POINTER(1)) goto 49
      if (G_PT .gt. G_PTZ) goto 91
      goto 10
!.......................................................................
!     simulate recursion
!.......................................................................
   91 continue
      call mat_clause()
      if (G_ERR .gt. 0) goto 01
      if (G_PT .le. G_PTZ) goto 15
      r = G_RSTK(G_PT)
      select case(R)
      case(3:5);   goto 92
      case(13:14); goto 15
      case(21);    goto 49
      case default
         write(*,*)'INTERNAL ERROR 91'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
   92 CONTINUE
      call mat_expr()
      if (G_ERR .gt. 0) goto 01
      r = G_RSTK(G_PT)
      select case(r)
      case(1);     goto 35
      case(2)
         if (G_SYM.eq.semi .or. G_SYM.eq.comma .or. G_SYM.eq.GG_EOL) goto 60
         if (G_SYM.eq.isname .and. mat_eqid(G_SYN,else)) goto 60
         if (G_SYM.eq.isname .and. mat_eqid(G_SYN,ennd)) goto 60
         call mat_err(40)
         if (G_ERR .gt. 0) goto 01
         goto 60
      case(3:5);   goto 91
      case(6:7);   goto 93
      case(10:11); goto 94
      case(18:19); goto 94
      case(20);    goto 46
      case default
         write(*,*)'Internal error 92'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
   93 continue
      call mat_term()
      if (G_ERR .gt. 0) goto 01
      r = G_RSTK(G_PT)
      select case(R)
      case(6:7);   goto 92
      case(8:9);   goto 94
      case(15);    goto 95
      case default
         write(*,*)'INTERNAL ERROR 93'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
   94 continue
      call mat_factor()
      if (G_ERR .gt. 0) goto 01
      r = G_RSTK(G_PT)
      select case(R)
      case(8:9);   goto 93
      case(10:11); goto 92
      case(12);    goto 94
      case(16:17); goto 95
      case(18:19); goto 92
      case default
         write(*,*)'INTERNAL ERROR 94'
         call mat_err(22) ! recursion difficulties
         goto 01
      end select
!.......................................................................
!     call mat_matfns by returning to LALA
   95 continue
      if(G_ARGUMENT_POINTER.lt.1)then
         !call journal('sc','*mat_parse* stack emptied',G_ARGUMENT_POINTER)
      else
         if (G_FIN.gt.0 .and. G_VAR_ROWS(G_ARGUMENT_POINTER).lt.0) call mat_err(14)
      endif
      if (G_ERR .gt. 0) goto 01
      return
!.......................................................................
end subroutine mat_parse
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_comand(id)

character(len=*),intent(in)  :: id
integer                      :: chr
integer                      :: i, k
integer                      :: l

! a list of names this procedure matches to use for some preliminary tests
character(len=10),parameter :: cmd(*)=[ character(len=10) :: &
 & 'clear', 'else',  'end',      'exit',   'for',  &
 & 'help',  'if',    'long',     'quit',   'semi', &
 & 'short', 'what',  'while',    'who',    'sh',   &
 & 'lala',  'shell', 'continue', 'return', 'fhelp'   &
 & ]

FINISHED: block
   G_FUN = 0

   do k = size(cmd),0,-1
     if(k.eq.0)then                          ! did not match anything
        G_FIN = 0
        return
     elseif (id.eq.cmd(k))then               ! found match to command

        select case(G_CHRA)                  ! check next character
        case(comma,semi,GG_EOL)               ! next character is end of a command so good to go
           exit
        case(iachar('0'):iachar('9'),iachar('a'):iachar('z'),iachar('A'):iachar('Z'),score) ! alphanumeric or a HELP command so good to go
           exit
        end select

        if (id.eq.'help')then                ! special case where anything after the help could be a topic
           exit
        elseif(id.eq.'fhelp')then
           exit
        else
           call mat_err(16)                  ! improper command
           return
        endif

     endif
   enddo

   G_FIN = 1                                 ! found a match and next character passed tests
!===================================================================================================================================
   COMAND : select case(id)
!===================================================================================================================================
   case('clear')
   ! alphameric character
      if(verify(achar(G_CHRA),big//little//digit)==0)then ! is alphanumeric so good to go by name
         call mat_getsym()
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
         G_VAR_COLS(G_ARGUMENT_POINTER) = 0
         G_RHS = 0
         call mat_stack_put(G_SYN)
         if (G_ERR .gt. 0) return
         G_FIN = 1
      else
         G_TOP_OF_SAVED = GG_MAX_NUMBER_OF_NAMES-3
      endif
!===================================================================================================================================
   case('for')
      G_FIN = -11
      exit FINISHED
   case('while')
      G_FIN = -12
      exit FINISHED
   case('if')
      G_FIN = -13
      exit FINISHED
   case('else')
      G_FIN = -14
      exit FINISHED
   case('end')
      G_FIN = -15
      exit FINISHED
!===================================================================================================================================
   case('exit')
      IF (G_PT .GT. G_PTZ)then
         G_FIN = -16
         exit COMAND
      endif
      K = int(GM_REALS(GM_BIGMEM-2))
      call journal('sc',' total flops ',k)

      select case( int(mat_urand(G_CURRENT_RANDOM_SEED)*9) )    ! for serendipity's sake randomly pick a sign-off
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

      G_FUN = 99
!===================================================================================================================================
   case('quit','return')
      K = G_LINE_POINTER(1) - 7
      IF (K .LE. 0)then
         G_FUN = 99
         exit COMAND
      endif
      call mat_files(-G_RIO,G_BUF)
      G_LINE_POINTER(1) = G_LIN(K+1)
      G_LINE_POINTER(4) = G_LIN(K+2)
      G_LINE_POINTER(6) = G_LIN(K+3)
      G_PTZ = G_LIN(K+4)
      G_RIO = G_LIN(K+5)
      G_LINECOUNT(4) = G_LIN(K+6)
      G_CHRA = BLANK
      G_SYM = COMMA
      exit FINISHED
!===================================================================================================================================
   case('continue')
      G_FUN = 99
      exit FINISHED
!===================================================================================================================================
   case('lala')
      call journal('QUIT SINGING AND GET BACK TO WORK.')
!===================================================================================================================================
   case('shell')
      call journal(' Your place or mine?')
!===================================================================================================================================
   case('short','long')
      if(k.eq.11)then
         G_FMT = 1
      else
         G_FMT = 2
      endif
      if (G_CHRA.eq.e_low .or. G_CHRA.eq.d_low .or. G_CHRA.eq.e_up .or. chr.eq.d_up ) G_FMT = G_FMT+2
      if (G_CHRA .eq. z_low) G_FMT = 5
      if (G_CHRA.eq.e_low .or. G_CHRA.eq.d_low .or. G_CHRA.eq.z_low) call mat_getsym()
      if (G_CHRA.eq.e_UP .or. G_CHRA.eq.d_up .or. G_CHRA.eq.z_up ) call mat_getsym()
!===================================================================================================================================
   case('semi')
      G_LINECOUNT(3) = 1 - G_LINECOUNT(3)  ! toggle "semi" mode
!===================================================================================================================================
   case('who')
      call journal(' Your current variables are...')
      call mat_print_id(G_VAR_IDS(1,G_TOP_OF_SAVED),GG_MAX_NUMBER_OF_NAMES-G_TOP_OF_SAVED+1)
      !x!do i=1,size(keywords)
      !x!   write(*,*)keywords(i),rows(i),cols(i),locs(i)
      !x!enddo
      l = GM_BIGMEM-G_VAR_DATALOC(G_TOP_OF_SAVED)+1
      call journal('sc','using',l,'out of',GM_BIGMEM,'elements')
!===================================================================================================================================
   case('what')
!===================================================================================================================================
   case('sh')
      call sh_command()
!===================================================================================================================================
   case('help','fhelp')
      HELP_ : block
      character(len=GG_LINELEN) :: topic_name
         G_BUF=blank
         if (G_CHRA .eq. GG_EOL) then                                ! if no topic
            topic_name= ' '
         else
            call mat_getsym()                                       ! get next symbol or name
            if (G_SYM .eq. isname)then                              ! use next word on line as topic
                 G_BUF(:GG_MAX_NAME_LENGTH) = G_SYN
            else                                                    ! use next non-blank character as topic
               if (G_SYM .eq. 0) G_SYM = dot
               G_BUF(1)  = G_SYM
               G_BUF(2:) = blank
            endif
            call mat_buf2str(topic_name,G_BUF,len(topic_name))      ! convert ADE array to string
         endif
         if(topic_name.eq.'search')then
            topic_name=ade2str(pack(G_LIN,G_LIN.gt.0.and.G_LIN.lt.255))
            i=index(topic_name,'search')                            ! assuming help command on line by itself to some extent
            if(i.ne.0)topic_name=topic_name(i:)
         endif
         if(id.eq.'help')then
            call help_command(G_HELP_TEXT,trim(topic_name),&
            & merge(G_LINECOUNT(:2),[0,huge(0)],&                      ! page length
            & G_PROMPT))
         else
            call help_command(G_FORTRAN_TEXT,trim(topic_name),&
            & merge(G_LINECOUNT(:2),[0,huge(0)],&                      ! page length
            & G_PROMPT))
         endif
      endblock HELP_
!===================================================================================================================================
   case default ! did not find a match
      G_FIN = 0
      return
!===================================================================================================================================
   end select COMAND
!===================================================================================================================================
   call mat_getsym()
endblock FINISHED
end subroutine mat_comand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine sh_command()

! ident_23="@(#)M_matrix::sh_command(3f): start system shell interactively"

character(len=GG_LINELEN) :: line
integer                   :: istat

   call get_environment_variable('SHELL',line)               ! get command to execute
   IF (G_CHRA .eq. GG_EOL )then                               ! if next character on stack is end-of-line call interactive shell
      call execute_command_line(line,cmdstat=istat)          ! call shell interactively
   else                                                      ! there were characters after SH on the line
      call execute_command_line(line,cmdstat=istat)          ! call shell interactively
   endif

end subroutine sh_command
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_plot(lplot,x,y,n,p,k)

! ident_24="@(#)M_matrix::mat_plot(3fp): Plot X vs. Y on LPLOT.  If K is nonzero, then P(1),...,P(K) are extra parameters"

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
!!   01 FORMAT('Extra parameters',*(f5.1,/))

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
         jmax = max(jmax,j)
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
      if ((ch.ge.0) .and. (ch.lt.G_CHARSET_SIZE)) then
         lets(i) = ch
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

! ident_25="@(#)M_matrix::mat_matfn1(3fp): evaluate functions involving gaussian elimination"

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
integer           :: location
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
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   M = G_VAR_ROWS(G_ARGUMENT_POINTER)
   N = G_VAR_COLS(G_ARGUMENT_POINTER)
!===================================================================================================================================
   select case(G_FIN)
!===================================================================================================================================
    case(-1) ! MATRIX RIGHT DIVISION, A/A2
      l2 = G_VAR_DATALOC(G_ARGUMENT_POINTER+1)
      m2 = G_VAR_ROWS(G_ARGUMENT_POINTER+1)
      n2 = G_VAR_COLS(G_ARGUMENT_POINTER+1)
      if (m2 .ne. n2) then
         call mat_err(20)
         return
      endif
      if (m*n .ne. 1) then
         if (n .ne. n2) then
            call mat_err(11)
            return
         endif
         l3 = l2 + m2*n2

         if(too_much_memory( l3+n2 - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

         call ml_wgeco(GM_REALS(l2),GM_IMAGS(l2),m2,n2,G_BUF,rcond,GM_REALS(l3),GM_IMAGS(l3))
         if (rcond .eq. 0.0d0) then
            call mat_err(19)
            return
         endif
         t = mat_flop(1.0d0 + rcond)
         if (t.eq.1.0d0 .and. G_FUN.ne.21)then
            call journal('WARNING:')
            call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
            WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
            call journal(mline)
         endif
         if (t.eq.1.0d0 .and. G_FUN.eq.21)then
            call journal('WARNING')
            call journal('EIGENVECTORS ARE BADLY CONDITIONED.')
            WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
            call journal(mline)
         endif
         do i = 1, m
            do j = 1, n
               ls = location+i-1+(j-1)*m
               ll = l3+j-1
               GM_REALS(ll) = GM_REALS(ls)
               GM_IMAGS(ll) = -GM_IMAGS(ls)
            enddo
            call ml_wgesl(GM_REALS(l2),GM_IMAGS(l2),m2,n2,G_BUF,GM_REALS(l3),GM_IMAGS(l3),1)
            do j = 1, n
               ll = location+i-1+(j-1)*m
               ls = l3+j-1
               GM_REALS(ll) = GM_REALS(ls)
               GM_IMAGS(ll) = -GM_IMAGS(ls)
            enddo
         enddo
         if (G_FUN .ne. 21) goto 99
   !
   !     CHECK FOR IMAGINARY ROUNDOFF IN MATRIX FUNCTIONS
         sr(1) = mat_wasum(n*n,GM_REALS(location),GM_REALS(location),1)
         si(1) = mat_wasum(n*n,GM_IMAGS(location),GM_IMAGS(location),1)
         eps = GM_REALS(GM_BIGMEM-4)
         t = eps*sr(1)
         if (si(1) .le. eps*sr(1)) call mat_rset(n*n,0.0d0,GM_IMAGS(location),1)
         goto 99
   !
      endif

      sr(1) = GM_REALS(location)
      si(1) = GM_IMAGS(location)
      n = n2
      m = n
      G_VAR_ROWS(G_ARGUMENT_POINTER) = n
      G_VAR_COLS(G_ARGUMENT_POINTER) = n
      call mat_wcopy(n*n,GM_REALS(l2),GM_IMAGS(l2),1,GM_REALS(location),GM_IMAGS(location),1)
!===================================================================================================================================
    case(-2) ! MATRIX LEFT DIVISION A BACKSLASH A2
      l2 = G_VAR_DATALOC(G_ARGUMENT_POINTER+1)
      m2 = G_VAR_ROWS(G_ARGUMENT_POINTER+1)
      n2 = G_VAR_COLS(G_ARGUMENT_POINTER+1)
      if (m .ne. n) then
         call mat_err(20)
         return
      endif
      if (m2*n2 .ne. 1) then
         l3 = l2 + m2*n2

         if(too_much_memory( l3+n - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

         call ml_wgeco(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,rcond,GM_REALS(l3),GM_IMAGS(l3))
         if (rcond .eq. 0.0d0) then
            call mat_err(19)
            return
         endif
         t = mat_flop(1.0d0 + rcond)
         if (t .eq. 1.0d0) then
            call journal('WARNING:')
            call journal('MATRIX IS CLOSE TO SINGULAR OR BADLY SCALED.')
            WRITE(mline,'(''RESULTS MAY BE INACCURATE. RCOND='',1PD13.4)') RCOND
            call journal(mline)
         endif
         if (m2 .ne. n) then
            call mat_err(12)
            return
         endif
         do j = 1, n2
            lj = l2+(j-1)*m2
            call ml_wgesl(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,GM_REALS(lj),GM_IMAGS(lj),0)
         enddo
         G_VAR_COLS(G_ARGUMENT_POINTER) = n2
         call mat_wcopy(m2*n2,GM_REALS(l2),GM_IMAGS(l2),1,GM_REALS(location),GM_IMAGS(location),1)
         goto 99
      endif
      sr(1) = GM_REALS(l2)
      si(1) = GM_IMAGS(l2)
!===================================================================================================================================
   end select
!===================================================================================================================================
   select case(G_FIN)
!===================================================================================================================================
    case(1) ! COMMAND::INV
      if (m .ne. n) then
         call mat_err(20)
         return
      endif
      do j = 1, n
         do i = 1, n
            ls = location+i-1+(j-1)*n
            t0 = GM_REALS(ls)
            t1 = mat_flop(1.0d0/(dble(i+j-1)))
            if (t0 .ne. t1) goto 32
         enddo
      enddo
      call mat_inverse_hilbert(GM_REALS(location),n,n)
      call mat_rset(n*n,0.0d0,GM_IMAGS(location),1)
      if (G_FIN .lt. 0) call mat_wscal(n*n,sr(1),si(1),GM_REALS(location),GM_IMAGS(location),1)
      goto 99
32    continue
      l3 = location + n*n

      if(too_much_memory( l3+n - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      call ml_wgeco(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,rcond,GM_REALS(l3),GM_IMAGS(l3))
      if (rcond .eq. 0.0d0) then
         call mat_err(19)
         return
      endif
      t = mat_flop(1.0d0 + rcond)
      if (t .eq. 1.0d0) then
         call journal('warning:')
         call journal('matrix is close to singular or badly scaled.')
         write(mline,'(''results may be inaccurate. rcond='',1pd13.4)') rcond
         call journal(mline)
      endif
      call ml_wgedi(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,dtr,dti,GM_REALS(l3),GM_IMAGS(l3),1)
      if (G_FIN .lt. 0) call mat_wscal(n*n,sr(1),si(1),GM_REALS(location),GM_IMAGS(location),1)
!===================================================================================================================================
    case (2) ! COMMAND::DET
      if (m .ne. n) then
         call mat_err(20)
         return
      endif
      call ml_wgefa(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,info)
      !SUBROUTINE ML_WGEDI(ar,ai,LDA,N,ipvt,detr,deti,workr,worki,JOB)
      call ml_wgedi(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,dtr,dti,sr(1),si(1),10)
      k = int(dtr(2))
      ka = iabs(k)+2
      t = 1.0d0
      do i = 1, ka
         t = t/10.0d0
         if (t .ne. 0.0d0) goto 42
      enddo
      GM_REALS(location) = dtr(1)*10.d0**k
      GM_IMAGS(location) = dti(1)*10.d0**k
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      goto 99
42    continue
      if (dti(1) .eq. 0.0d0)then
         write(mline,43) dtr(1),k
         call journal(mline)
      else
         write(mline,44) dtr(1),dti(1),k
         call journal(mline)
      endif
      GM_REALS(location) = dtr(1)
      GM_IMAGS(location) = dti(1)
      GM_REALS(location+1) = dtr(2)
      GM_IMAGS(location+1) = 0.0d0
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 2
43    format(' det =  ',f7.4,7h * 10**,i4)
44    format(' det =  ',f7.4,' + ',f7.4,' i ',7h * 10**,i4)
!===================================================================================================================================
    case(3) ! COMMAND::RCOND
      if (m .ne. n) then
         call mat_err(20)
         return
      endif
      l3 = location + n*n

      if(too_much_memory( l3+n - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      call ml_wgeco(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,rcond,GM_REALS(l3),GM_IMAGS(l3))
      GM_REALS(location) = rcond
      GM_IMAGS(location) = 0.0d0
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      if (G_lhs .ne. 1)then
         location = location + 1
         call mat_wcopy(n,GM_REALS(l3),GM_IMAGS(l3),1,GM_REALS(location),GM_IMAGS(location),1)
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
         G_VAR_DATALOC(G_ARGUMENT_POINTER) = location
         G_VAR_ROWS(G_ARGUMENT_POINTER) = n
         G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      endif
!===================================================================================================================================
    case(4) ! COMMAND::LU
      if (m .ne. n) then
         call mat_err(20)
         return
      endif
      call ml_wgefa(GM_REALS(location),GM_IMAGS(location),m,n,G_BUF,info)
      if (G_lhs .ne. 2) goto 99
      nn = n*n
      if (G_ARGUMENT_POINTER+1 .ge. G_TOP_OF_SAVED) then
         call mat_err(18)
         return
      endif
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
      G_VAR_DATALOC(G_ARGUMENT_POINTER) = location + nn
      G_VAR_ROWS(G_ARGUMENT_POINTER) = n
      G_VAR_COLS(G_ARGUMENT_POINTER) = n

      if(too_much_memory( location+nn+nn - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      do kb = 1, n
         k = n+1-kb
         do i = 1, n
            ll = location+i-1+(k-1)*n
            lu = ll + nn
            if (i .le. k) GM_REALS(lu) = GM_REALS(ll)
            if (i .le. k) GM_IMAGS(lu) = GM_IMAGS(ll)
            if (i .gt. k) GM_REALS(lu) = 0.0d0
            if (i .gt. k) GM_IMAGS(lu) = 0.0d0
            if (i .lt. k) GM_REALS(ll) = 0.0d0
            if (i .lt. k) GM_IMAGS(ll) = 0.0d0
            if (i .eq. k) GM_REALS(ll) = 1.0d0
            if (i .eq. k) GM_IMAGS(ll) = 0.0d0
            if (i .gt. k) GM_REALS(ll) = -GM_REALS(ll)
            if (i .gt. k) GM_IMAGS(ll) = -GM_IMAGS(ll)
         enddo
         i = G_BUF(k)
         if (i .eq. k) cycle
         li = location+i-1+(k-1)*n
         lk = location+k-1+(k-1)*n
         call mat_wswap(n-k+1,GM_REALS(li),GM_IMAGS(li),n,GM_REALS(lk),GM_IMAGS(lk),n)
      enddo
!===================================================================================================================================
    case(5) ! COMMAND::inverse_hilbert
      n = int(GM_REALS(location))
      G_VAR_ROWS(G_ARGUMENT_POINTER) = n
      G_VAR_COLS(G_ARGUMENT_POINTER) = n
      call mat_inverse_hilbert(GM_REALS(location),n,n)
      call mat_rset(n*n,0.0d0,GM_IMAGS(location),1)
      if (G_FIN .lt. 0) call mat_wscal(n*n,sr(1),si(1),GM_REALS(location),GM_IMAGS(location),1)
!===================================================================================================================================
    case(6) ! COMMAND::CHOLESKY
      if (m .ne. n) then
         call mat_err(20)
         return
      endif
      call mat_wpofa(GM_REALS(location),GM_IMAGS(location),m,n,G_err)
      if (G_err .ne. 0) then
         call mat_err(29)
         return
      endif
      do j = 1, n
         ll = location+j+(j-1)*m
         call mat_wset(m-j,0.0d0,0.0d0,GM_REALS(ll),GM_IMAGS(ll),1)
      enddo
!===================================================================================================================================
    case(7) ! COMMAND::RREF
      if (G_RHS .ge. 2)then
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
         if (G_VAR_ROWS(G_ARGUMENT_POINTER) .ne. m) then
            call mat_err(5)
            return
         endif
         n = n + G_VAR_COLS(G_ARGUMENT_POINTER)
      endif
      call mat_rref(GM_REALS(location),GM_IMAGS(location),m,m,n,GM_REALS(GM_BIGMEM-4))
      G_VAR_COLS(G_ARGUMENT_POINTER) = n
!===================================================================================================================================
   end select
!
99 continue
end subroutine mat_matfn1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn2()
integer          :: i
integer          :: inc
integer          :: j
integer          :: job
integer          :: k
integer          :: location
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
!     evaluate elementary functions and functions involving eigenvalues and eigenvectors
!
      doubleprecision tr(1),ti(1),sr,si,powr,powi
      logical herm,schur,vect,hess
!
!     functions/G_FIN
!     **   SIN  COS ATAN  EXP  SQRT LOG
!      0    1    2    3    4    5    6
!    EIG  SCHU HESS POLY ROOT
!     11   12   13   14   15
!    ABS  ROUN REAL IMAG CONJ
!     21   22   23   24   25
      if (G_FIN .ne. 0) goto 05
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER+1)
         powr = GM_REALS(location)
         powi = GM_IMAGS(location)
   05 continue
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      m = G_VAR_ROWS(G_ARGUMENT_POINTER)
      n = G_VAR_COLS(G_ARGUMENT_POINTER)
      if (G_FIN .ge. 11 .and. G_FIN .le. 13) goto 10
      if (G_FIN .eq. 14 .and. (m.eq.1 .or. n.eq.1))then
         goto 50
      endif
      if (G_FIN .eq. 14) goto 10
      if (G_FIN .eq. 15) goto 60

      if (G_FIN .gt. 20) goto 40
      if (m .eq. 1 .or. n .eq. 1) goto 40
      ! what about fall-though?
!===================================================================================================================================
!     EIGENVALUES AND VECTORS
   10 continue
      IF (M .NE. N) then
         call mat_err(20)
         return
      endif
      SCHUR = G_FIN .EQ. 12
      HESS = G_FIN .EQ. 13
      VECT = G_LHS.EQ.2 .OR. G_FIN.LT.10
      NN = N*N
      L2 = location + NN
      LD = L2 + NN
      LE = LD + N
      LW = LE + N

      if(too_much_memory( LW+N - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      call mat_wcopy(NN,GM_REALS(location),GM_IMAGS(location),1,GM_REALS(L2),GM_IMAGS(L2),1)
!
!     CHECK IF HERMITIAN
      HERM=.FALSE.
      DO J = 1, N
         DO I = 1, J
            LS = location+I-1+(J-1)*N
            LL = location+(I-1)*N+J-1
            HERM = GM_REALS(LL).EQ.GM_REALS(LS) .AND. GM_IMAGS(LL).EQ.-GM_IMAGS(LS)
            IF (.NOT. HERM) goto 30
         enddo
      enddo
!
!     HERMITIAN EIGENVALUE PROBLEM
      call mat_wset(NN,0.0D0,0.0D0,GM_REALS(location),GM_IMAGS(location),1)
      call mat_wset(N,1.0D0,0.0D0,GM_REALS(location),GM_IMAGS(location),N+1)
      call mat_wset(N,0.0D0,0.0D0,GM_IMAGS(LD),GM_IMAGS(LE),1)
      job = 0
      IF (VECT) JOB = 1
      call ML_HTRIDI(N,N, &
      GM_REALS(L2),GM_IMAGS(L2), &
      GM_REALS(LD),GM_REALS(LE), &
      GM_REALS(LE),GM_REALS(LW))
      IF(.NOT.HESS)call ML_IMTQL2(N,N,GM_REALS(LD),GM_REALS(LE),GM_REALS(location),G_ERR,JOB)
      IF (G_ERR .GT. 0) then
         call mat_err(24)
         return
      endif
      IF (JOB .NE. 0) call ML_HTRIBK(N,N,GM_REALS(L2),GM_IMAGS(L2), &
                                         GM_REALS(LW),N,GM_REALS(location), &
                                         GM_IMAGS(location))
      goto 31
!
!     NON-HERMITIAN EIGENVALUE PROBLEM
   30 continue
      call ML_CORTH(N,N,1,N,GM_REALS(L2),GM_IMAGS(L2), &
                            GM_REALS(LW),GM_IMAGS(LW))
      IF (.NOT.VECT .AND. HESS) goto 31
      JOB = 0
      IF (VECT) JOB = 2
      IF (VECT .AND. SCHUR) JOB = 1
      IF (HESS) JOB = 3
      call ML_COMQR3(N,N,1,N,GM_REALS(LW),GM_IMAGS(LW), &
                             GM_REALS(L2),GM_IMAGS(L2),  &
                             GM_REALS(LD),GM_IMAGS(LD), &
                             GM_REALS(location),GM_IMAGS(location), &
                             G_ERR,JOB)
      IF (G_ERR .GT. 0) then
         call mat_err(24)
         return
      endif
!
!     VECTORS
   31 continue
      IF (.NOT.VECT) goto 34
      IF (G_ARGUMENT_POINTER+1 .GE. G_TOP_OF_SAVED) then
         call mat_err(18)
         return
      endif
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
      G_VAR_DATALOC(G_ARGUMENT_POINTER) = L2
      G_VAR_ROWS(G_ARGUMENT_POINTER) = N
      G_VAR_COLS(G_ARGUMENT_POINTER) = N
!
!     DIAGONAL OF VALUES OR CANONICAL FORMS
   34 continue
      IF (.NOT.VECT .AND. .NOT.SCHUR .AND. .NOT.HESS) goto 37
      DO J = 1, N
         LJ = L2+(J-1)*N
         IF (SCHUR .AND. (.NOT.HERM)) LJ = LJ+J
         IF (HESS .AND. (.NOT.HERM)) LJ = LJ+J+1
         LL = L2+J*N-LJ
         call mat_wset(LL,0.0D0,0.0D0,GM_REALS(LJ),GM_IMAGS(LJ),1)
      enddo
      IF (.NOT.HESS .OR. HERM) call mat_wcopy(N,GM_REALS(LD),GM_IMAGS(LD),1,GM_REALS(L2),GM_IMAGS(L2),N+1)
      LL = L2+1
      IF (HESS .AND. HERM)call mat_wcopy(N-1,GM_REALS(LE+1),GM_IMAGS(LE+1),1,GM_REALS(LL),GM_IMAGS(LL),N+1)
      LL = L2+N
      IF (HESS .AND. HERM)call mat_wcopy(N-1,GM_REALS(LE+1),GM_IMAGS(LE+1),1,GM_REALS(LL),GM_IMAGS(LL),N+1)
      IF (G_FIN .LT. 10) goto 42
      IF (VECT .OR. .NOT.(SCHUR.OR.HESS)) goto 99
      call mat_wcopy(NN,GM_REALS(L2),GM_IMAGS(L2),1,GM_REALS(location),GM_IMAGS(location),1)
      goto 99
!
!     VECTOR OF EIGENVALUES
   37 continue
      IF (G_FIN .EQ. 14) goto 52
      call mat_wcopy(N,GM_REALS(LD),GM_IMAGS(LD),1,GM_REALS(location),GM_IMAGS(location),1)
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      goto 99
!===================================================================================================================================
!     elementary functions
!     for matrices.. x,d = eig(a), fun(a) = x*fun(d)/x
   40 continue
      inc = 1
      n = m*n
      l2 = location
      goto 44

   42 continue
      INC = N+1

   44 continue
      do j = 1, n
        ls = l2+(j-1)*inc
        sr = GM_REALS(ls)
        si = GM_IMAGS(ls)
        ti = 0.0d0
        if (G_FIN .eq. 0) then
          call mat_wlog(sr,si,sr,si)
          call mat_wmul(sr,si,powr,powi,sr,si)
          tr(1) = dexp(sr)*dcos(si)
          ti(1) = dexp(sr)*dsin(si)
        endif

        select case(G_FIN)
        case( 1)                                      ! sin
                 tr(1) = dsin(sr)*dcosh(si)
                 ti(1) = dcos(sr)*dsinh(si)
        case( 2)                                      ! cos
                 tr(1) = dcos(sr)*dcosh(si)
                 ti(1) = (-dsin(sr))*dsinh(si)
        case( 3)                                      ! atan
                 call mat_watan(sr,si,tr(1),ti(1))
        case( 4)                                      ! exp
                 tr(1) = dexp(sr)*dcos(si)
                 ti(1) = dexp(sr)*dsin(si)
        case( 5)                                      ! sqrt
                 call mat_wsqrt(sr,si,tr(1),ti(1))
        case( 6)                                      ! log
                 call mat_wlog(sr,si,tr(1),ti(1))
        case( 21)
                 tr(1) = mat_pythag(sr,si)
        case( 22)
                 tr(1) = mat_round(sr)
        case( 23)
                 tr(1) = sr
        case( 24)
                 tr(1) = si
        case( 25)
                 tr(1) = sr
                 ti(1) = -si
        end select

        if (G_ERR .gt. 0) return
        GM_REALS(ls) = mat_flop(tr(1))
        GM_IMAGS(ls) = 0.0d0
        if (ti(1) .ne. 0.0d0) GM_IMAGS(ls) = mat_flop(ti(1))
      enddo
      if (inc .eq. 1) goto 99
      do j = 1, n
        ls = l2+(j-1)*inc
        sr = GM_REALS(ls)
        si = GM_IMAGS(ls)
        ls = location+(j-1)*n
        ll = l2+(j-1)*n
        call mat_wcopy(n,GM_REALS(ls),GM_IMAGS(ls),1,GM_REALS(ll),GM_IMAGS(ll),1)
        call mat_wscal(n,sr,si,GM_REALS(ls),GM_IMAGS(ls),1)
      enddo
      ! signal matfn1 to divide by eigenvectors
      G_FUN = 21
      G_FIN = -1
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
      goto 99
!===================================================================================================================================
!     POLY
      ! form polynomial with given vector as roots
   50 continue
      N = MAX(M,N)
      LD = location+N+1
      call mat_wcopy(N,GM_REALS(location),GM_IMAGS(location),1,GM_REALS(LD),GM_IMAGS(LD),1)
      goto 52
!===================================================================================================================================
!     FORM CHARACTERISTIC POLYNOMIAL
   52 continue
      call mat_wset(N+1,0.0D0,0.0D0,GM_REALS(location),GM_IMAGS(location),1)
      GM_REALS(location) = 1.0D0
      DO J = 1, N
         call matX_waxpy(J,-GM_REALS(LD),-GM_IMAGS(LD), &
                            GM_REALS(location),GM_IMAGS(location), &
                            -1,  &
                            GM_REALS(location+1),GM_IMAGS(location+1), &
                            -1)
         LD = LD+1
      enddo
      G_VAR_ROWS(G_ARGUMENT_POINTER) = N+1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      goto 99
!===================================================================================================================================
!     ROOTS
   60 continue
      LL = location+M*N
      GM_REALS(LL) = -1.0D0
      GM_IMAGS(LL) = 0.0D0
      K = -1
   61 continue
      K = K+1
      L1 = location+K
      IF (DABS(GM_REALS(L1))+DABS(GM_IMAGS(L1)) .EQ. 0.0D0) goto 61
      N = MAX(M*N - K-1, 0)
      IF (N .LE. 0) goto 65
      L2 = L1+N+1
      LW = L2+N*N

      if(too_much_memory( LW+N - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      call mat_wset(N*N+N,0.0D0,0.0D0,GM_REALS(L2),GM_IMAGS(L2),1)
      DO J = 1, N
         LL = L2+J+(J-1)*N
         GM_REALS(LL) = 1.0D0
         LS = L1+J
         LL = L2+(J-1)*N
         call mat_wdiv(-GM_REALS(LS),-GM_IMAGS(LS), &
                        GM_REALS(L1),GM_IMAGS(L1),  &
                        GM_REALS(LL),GM_IMAGS(LL))
         IF (G_ERR .GT. 0) return
      enddo
      call ML_COMQR3(N,N,1,N,GM_REALS(LW),GM_IMAGS(LW), &
                             GM_REALS(L2),GM_IMAGS(L2), &
                             GM_REALS(location),GM_IMAGS(location), &
                             TR,TI,G_ERR,0)
      IF (G_ERR .GT. 0) then
         call mat_err(24)
         return
      endif
   65 continue
      G_VAR_ROWS(G_ARGUMENT_POINTER) = N
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      goto 99
!===================================================================================================================================
   99 continue
end subroutine mat_matfn2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn3()

! ident_26="@(#)M_matrix::mat_matfn3(3fp): evaluate functions involving singular value decomposition"

integer         :: i
integer         :: j
integer         :: jb
integer         :: job
integer         :: k
integer         :: location
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
   if (G_FIN.eq.1 .and. G_RHS.eq.2) G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   m = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n = G_VAR_COLS(G_ARGUMENT_POINTER)
   mn = m*n
   !      SVD PINV COND NORM RANK
   !        1    2    3    4    5
   FUN3: select case(G_FIN)
!===================================================================================================================================
    case(3) ! COMMAND::COND
      ld = location + m*n
      l1 = ld + min(m+1,n)
      l2 = l1 + n

      if(too_much_memory( l2+min(m,n) - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      call ml_wsvdc(GM_REALS(location),GM_IMAGS(location),   &
                  & m,m,n,                               &
                  & GM_REALS(ld),GM_IMAGS(ld), &
                  & GM_REALS(l1),GM_IMAGS(l1), &
                  & t,t,1,t,t,1,                         &
                  & GM_REALS(l2),GM_IMAGS(l2), &
                  & 0,G_err)
      if (G_err .ne. 0) then
         call mat_err(24)
         return
      endif
      s = GM_REALS(ld)
      ld = ld + min(m,n) - 1
      t(1,1) = GM_REALS(ld)
      if (t(1,1) .ne. 0.0d0) then
         GM_REALS(location) = mat_flop(s/t(1,1))
         GM_IMAGS(location) = 0.0d0
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
         G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      else
         call journal(' CONDITION IS INFINITE')
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
      endif
!===================================================================================================================================
    case(4) ! command::norm

      p = 2.0d0
      inf = .false.

      if (G_RHS .eq. 2)then
         fro = int(GM_REALS(location)).eq.iachar('f') .and. mn.gt.1
         inf = int(GM_REALS(location)).eq.iachar('i') .and. mn.gt.1
         if (.not. fro) then
            p = GM_REALS(location)
         endif
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
         m = G_VAR_ROWS(G_ARGUMENT_POINTER)
         n = G_VAR_COLS(G_ARGUMENT_POINTER)
         mn = m*n
         if (fro) then
            m = mn
            n = 1
         endif
      endif

      if (m .gt. 1 .and. n .gt. 1) then
         ! matrix norm

         if (inf)then
            s = 0.0d0
            do i = 1, m
               li = location+i-1
               t(1,1) = mat_wasum(n,GM_REALS(LI),GM_IMAGS(li),m)
               s = dmax1(s,t(1,1))
            enddo
         elseif (p .eq. 1.0d0) then
            s = 0.0d0
            do j = 1, n
               lj = location+(j-1)*m
               t(1,1) = mat_wasum(m,GM_REALS(LJ),GM_IMAGS(lj),1)
               s = dmax1(s,t(1,1))
            enddo
         elseif (p .ne. 2.0d0) then
            call mat_err(23) ! Only 1, 2 or INF norm of matrix
            return
         else
            ld = location + m*n
            l1 = ld + min(m+1,n)
            l2 = l1 + n

            if(too_much_memory( l2+min(m,n) - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )then
               return
            endif

            call ml_wsvdc(GM_REALS(location),GM_IMAGS(location), &
                        & m,m,n, &
                        & GM_REALS(ld),GM_IMAGS(ld), &
                        & GM_REALS(l1),GM_IMAGS(l1), &
                        & t,t,1,t,t,1, &
                        & GM_REALS(l2),GM_IMAGS(l2), &
                        & 0,G_err)

            if (G_ERR .ne. 0)then
               call mat_err(24)
               return
            endif

            s = GM_REALS(LD)
         endif

      elseif (p .eq. 1.0d0)then
         s = mat_wasum(MN,GM_REALS(location),GM_IMAGS(location),1)
      elseif (p .eq. 2.0d0) then
         s = mat_wnrm2(MN,GM_REALS(location),GM_IMAGS(location),1)
      else
         i = mat_iwamax(mn,GM_REALS(location),GM_IMAGS(location),1) + location - 1
         s = dabs(GM_REALS(i)) + dabs(GM_IMAGS(i))

         if (.not.(inf .or. s .eq. 0.0d0))then
            t(1,1) = 0.0d0
            do i = 1, mn
               ls = location+i-1
               t(1,1) = mat_flop(t(1,1) + (mat_pythag(GM_REALS(ls),GM_IMAGS(ls))/s)**p)
            enddo
            if (p .ne. 0.0d0) then
               p = 1.0d0/p
            endif
            s = mat_flop(s*t(1,1)**p)
         endif
      endif

      GM_REALS(location) = s
      GM_IMAGS(location) = 0.0d0
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
!===================================================================================================================================
    case(1) !     COMMAND::SVD
      IF (G_LHS .EQ. 3)then
         K = M
         IF (G_RHS .EQ. 2) K = MIN(M,N)
         LU = location + M*N
         LD = LU + M*K
         LV = LD + K*N
         L1 = LV + N*N
         L2 = L1 + N

         if(too_much_memory( L2+MIN(M,N) - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

         JOB = 11
         IF (G_RHS .EQ. 2) JOB = 21
         call ml_wsvdc(GM_REALS(location),GM_IMAGS(location), &
         & m,m,n, &
         & GM_REALS(ld),GM_IMAGS(ld), &
         & GM_REALS(l1),GM_IMAGS(l1), &
         & GM_REALS(lu),GM_IMAGS(lu), &
         & m, &
         & GM_REALS(lv),GM_IMAGS(lv), &
         & n, &
         & GM_REALS(l2),GM_IMAGS(l2), &
         & job,G_err)
         DO JB = 1, N
            DO I = 1, K
               J = N+1-JB
               LL = LD+I-1+(J-1)*K
               IF (I.NE.J) GM_REALS(LL) = 0.0D0
               GM_IMAGS(LL) = 0.0D0
               LS = LD+I-1
               IF (I.EQ.J) GM_REALS(LL) = GM_REALS(LS)
               LS = L1+I-1
               IF (G_ERR.NE.0 .AND. I.EQ.J-1) GM_REALS(LL) = GM_REALS(LS)
            enddo
         enddo
         IF (G_ERR .NE. 0) call mat_err(24)
         G_ERR = 0
         call mat_wcopy(M*K+K*N+N*N, &
                      & GM_REALS(LU),GM_IMAGS(LU), &
                      & 1, &
                      & GM_REALS(location),GM_IMAGS(location), &
                      & 1)
         G_VAR_ROWS(G_ARGUMENT_POINTER) = M
         G_VAR_COLS(G_ARGUMENT_POINTER) = K
         IF (G_ARGUMENT_POINTER+1 .GE. G_TOP_OF_SAVED) then
            call mat_err(18)
            return
         endif
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
         G_VAR_DATALOC(G_ARGUMENT_POINTER) = location + M*K
         G_VAR_ROWS(G_ARGUMENT_POINTER) = K
         G_VAR_COLS(G_ARGUMENT_POINTER) = N
         IF (G_ARGUMENT_POINTER+1 .GE. G_TOP_OF_SAVED) then
            call mat_err(18)
            return
         endif
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
         G_VAR_DATALOC(G_ARGUMENT_POINTER) = location + M*K + K*N
         G_VAR_ROWS(G_ARGUMENT_POINTER) = N
         G_VAR_COLS(G_ARGUMENT_POINTER) = N
      else
         LD = location + M*N
         L1 = LD + MIN(M+1,N)
         L2 = L1 + N

         if(too_much_memory( L2+MIN(M,N) - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

         call ml_wsvdc(GM_REALS(location),GM_IMAGS(location),m,m,n, &
         & GM_REALS(ld),GM_IMAGS(ld),GM_REALS(l1),GM_IMAGS(l1), &
         & t,t,1,t,t,1,GM_REALS(l2),GM_IMAGS(l2),0,G_err)
         IF (G_ERR .NE. 0) then
            call mat_err(24)
            return
         endif
         K = MIN(M,N)
         call mat_wcopy(K,GM_REALS(LD),GM_IMAGS(LD),1,GM_REALS(location),GM_IMAGS(location),1)
         G_VAR_ROWS(G_ARGUMENT_POINTER) = K
         G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      endif
!===================================================================================================================================
    case(2,5) ! COMMAND::PINV AND RANK
      TOL = -1.0D0
      IF (G_RHS .EQ. 2) then
         TOL = GM_REALS(location)
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
         M = G_VAR_ROWS(G_ARGUMENT_POINTER)
         N = G_VAR_COLS(G_ARGUMENT_POINTER)
      endif
      LU = location + M*N
      LD = LU + M*M
      IF (G_FIN .EQ. 5) LD = location + M*N
      LV = LD + M*N
      L1 = LV + N*N
      IF (G_FIN .EQ. 5) L1 = LD + N
      L2 = L1 + N

      if(too_much_memory( L2+MIN(M,N) - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      IF (G_FIN .EQ. 2) JOB = 11
      IF (G_FIN .EQ. 5) JOB = 0
      call ML_WSVDC(GM_REALS(location),GM_IMAGS(location),M,M,N, &
                  & GM_REALS(LD),GM_IMAGS(LD), &
                  & GM_REALS(L1),GM_IMAGS(L1), &
                  & GM_REALS(LU),GM_IMAGS(LU), &
                  & M, &
                  & GM_REALS(LV),GM_IMAGS(LV), &
                  & N, &
                  & GM_REALS(L2),GM_IMAGS(L2), &
                  & JOB,G_ERR)
      IF (G_ERR .NE. 0) then
         call mat_err(24)
         return
      endif
      EPS = GM_REALS(GM_BIGMEM-4)
      IF (TOL .LT. 0.0D0) TOL = mat_flop(dble(MAX(M,N))*EPS*GM_REALS(LD))
      MN = MIN(M,N)
      K = 0
      DO J = 1, MN
         LS = LD+J-1
         S = GM_REALS(LS)
         IF (S .LE. TOL) exit
         K = J
         LL = LV+(J-1)*N
         IF (G_FIN .EQ. 2) call mat_wrscal(N,1.0D0/S,GM_REALS(LL),GM_IMAGS(LL),1)
      enddo
      if (G_FIN .ne. 5) then
         do j = 1, m
            do i = 1, n
               ll = location+i-1+(j-1)*n
               l1 = lv+i-1
               l2 = lu+j-1
               GM_REALS(ll) = mat_wdotcr(k,GM_REALS(l2),GM_IMAGS(l2),m,GM_REALS(l1),GM_IMAGS(l1),n)
               GM_IMAGS(ll) = mat_wdotci(k,GM_REALS(l2),GM_IMAGS(l2),m,GM_REALS(l1),GM_IMAGS(l1),n)
            enddo
         enddo
         G_VAR_ROWS(G_ARGUMENT_POINTER) = n
         G_VAR_COLS(G_ARGUMENT_POINTER) = m
      else
         GM_REALS(location) = dble(k)
         GM_IMAGS(location) = 0.0d0
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
         G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      endif
!===================================================================================================================================
   end select FUN3
!
end subroutine mat_matfn3
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
SUBROUTINE mat_matfn4()

! ident_27="@(#)M_matrix::mat_matfn4(3fp): evaluate functions involving qr decomposition (least squares)"

integer           :: info
integer           :: j
integer           :: jb
integer           :: job
integer           :: k
integer           :: location
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
character(len=81) :: message
DOUBLEPRECISION   :: T(1),TOL,EPS
!
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      M = G_VAR_ROWS(G_ARGUMENT_POINTER)
      N = G_VAR_COLS(G_ARGUMENT_POINTER)

      IF (G_FIN .EQ. -1) then
         goto 10
      elseIF (G_FIN .EQ. -2) then
         goto 20
      else
         goto 40
      endif
!
!     RECTANGULAR MATRIX RIGHT DIVISION, A/A2
   10 continue
      L2 = G_VAR_DATALOC(G_ARGUMENT_POINTER+1)
      M2 = G_VAR_ROWS(G_ARGUMENT_POINTER+1)
      N2 = G_VAR_COLS(G_ARGUMENT_POINTER+1)
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
      IF (N.GT.1 .AND. N.NE.N2) then
         call mat_err(11)
         return
      endif
      call mat_stack1(QUOTE)
      IF (G_ERR .GT. 0) return
      LL = L2+M2*N2
      call mat_wcopy(M*N,GM_REALS(location),GM_IMAGS(location),1,GM_REALS(LL),GM_IMAGS(LL),1)
      call mat_wcopy(M*N+M2*N2,GM_REALS(L2),GM_IMAGS(L2),1,GM_REALS(location),GM_IMAGS(location),1)
      G_VAR_DATALOC(G_ARGUMENT_POINTER) = location+M2*N2
      G_VAR_ROWS(G_ARGUMENT_POINTER) = M
      G_VAR_COLS(G_ARGUMENT_POINTER) = N
      call mat_stack1(QUOTE)
      IF (G_ERR .GT. 0) return
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 1
      M = N2
      N = M2
      goto 20
!
!     RECTANGULAR MATRIX LEFT DIVISION A BACKSLASH A2
!
   20 continue
      L2 = G_VAR_DATALOC(G_ARGUMENT_POINTER+1)
      M2 = G_VAR_ROWS(G_ARGUMENT_POINTER+1)
      N2 = G_VAR_COLS(G_ARGUMENT_POINTER+1)
      IF (M2*N2 .GT. 1) goto 21
        M2 = M
        N2 = M

        if(too_much_memory( L2+M*M - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

        call mat_wset(M*M-1,0.0D0,0.0D0,GM_REALS(L2+1),GM_IMAGS(L2+1),1)
        call mat_wcopy(M,GM_REALS(L2),GM_IMAGS(L2),0,GM_REALS(L2),GM_IMAGS(L2),M+1)
   21 continue
      IF (M2 .NE. M) then
         call mat_err(12)
         return
      endif
      L3 = L2 + MAX(M,N)*N2
      L4 = L3 + N

      if(too_much_memory( L4 + N - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      IF (M .GT. N) goto 23
      DO JB = 1, N2
        J = N+1-JB
        LS = L2 + (J-1)*M
        LL = L2 + (J-1)*N
        call mat_wcopy(M,GM_REALS(LS),GM_IMAGS(LS),-1,GM_REALS(LL),GM_IMAGS(LL),-1)
      enddo
   23 continue
      DO J = 1, N
        G_BUF(J) = 0
      enddo
      call ML_WQRDC(GM_REALS(location),GM_IMAGS(location), &
                  & M,M,N, &
                  & GM_REALS(L4),GM_IMAGS(L4), &
                  & G_BUF, &
                  & GM_REALS(L3),GM_IMAGS(L3), &
                  & 1)
      K = 0
      EPS = GM_REALS(GM_BIGMEM-4)
      T(1) = DABS(GM_REALS(location))+DABS(GM_IMAGS(location))
      TOL = mat_flop(dble(MAX(M,N))*EPS*T(1))
      MN = MIN(M,N)
      DO J = 1, MN
        LS = location+J-1+(J-1)*M
        T(1) = DABS(GM_REALS(LS)) + DABS(GM_IMAGS(LS))
        IF (T(1) .GT. TOL) K = J
      enddo
      IF (K .LT. MN) then
         WRITE(message,'(" RANK DEFICIENT,  RANK =",I4,",  TOL =",1PD13.4)') K,TOL
         call journal(message)
      endif
      MN = MAX(M,N)
      DO J = 1, N2
        LS = L2+(J-1)*MN
        call ML_WQRSL(GM_REALS(location),GM_IMAGS(location), &
                        & M,M,K, &
                        & GM_REALS(L4),GM_IMAGS(L4), &
                        & GM_REALS(LS),GM_IMAGS(LS), &
                        & T,T, &
                        & GM_REALS(LS),GM_IMAGS(LS), &
                        & GM_REALS(LS),GM_IMAGS(LS), &
                        & T,T,T,T,100,INFO)
        LL = LS+K
        call mat_wset(N-K,0.0D0,0.0D0,GM_REALS(LL),GM_IMAGS(LL),1)
      enddo
      DO J = 1, N
        G_BUF(J) = -G_BUF(J)
      enddo
      DO J = 1, N
        IF (G_BUF(J) .GT. 0) cycle
        K = -G_BUF(J)
        G_BUF(J) = K
   33   CONTINUE
          IF (K .EQ. J) cycle
          LS = L2+J-1
          LL = L2+K-1
          call mat_wswap(N2,GM_REALS(LS),GM_IMAGS(LS),MN,GM_REALS(LL),GM_IMAGS(LL),MN)
          G_BUF(K) = -G_BUF(K)
          K = G_BUF(K)
          goto 33
      enddo
      DO J = 1, N2
        LS = L2+(J-1)*MN
        LL = location+(J-1)*N
        call mat_wcopy(N,GM_REALS(LS),GM_IMAGS(LS),1,GM_REALS(LL),GM_IMAGS(LL),1)
      enddo
      G_VAR_ROWS(G_ARGUMENT_POINTER) = N
      G_VAR_COLS(G_ARGUMENT_POINTER) = N2
      IF (G_FIN .EQ. -1) call mat_stack1(QUOTE)
      IF (G_ERR .GT. 0) return
      goto 99
!===================================================================================================================================
!     QR
!
   40 continue
      mm = max(m,n)
      ls = location + mm*mm
      if (G_LHS.eq.1 .and. G_FIN.eq.1) ls = location
      le = ls + m*n
      l4 = le + mm

      if(too_much_memory( l4+mm - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      if (ls.ne.location) then
         call mat_wcopy(m*n,GM_REALS(location),GM_IMAGS(location),1,GM_REALS(ls),GM_IMAGS(ls),1)
      endif
      job = 1
      if (G_LHS.lt.3) job = 0
      do j = 1, n
        G_BUF(j) = 0
      enddo
      call ml_wqrdc(GM_REALS(ls),GM_IMAGS(ls), &
       & m,m,n, &
       & GM_REALS(l4),GM_IMAGS(l4), &
       & G_BUF, &
       & GM_REALS(le),GM_IMAGS(le), &
       & job)
      if (G_LHS.eq.1 .and. G_FIN.eq.1) goto 99
      call mat_wset(m*m,0.0d0,0.0d0,GM_REALS(location),GM_IMAGS(location),1)
      call mat_wset(m,1.0d0,0.0d0,GM_REALS(location),GM_IMAGS(location),m+1)
      do j = 1, m
        ll = location+(j-1)*m
        call ml_wqrsl(GM_REALS(ls),GM_IMAGS(ls),m,m,n,GM_REALS(l4),GM_IMAGS(l4),   &
     &             GM_REALS(ll),GM_IMAGS(ll),GM_REALS(ll),GM_IMAGS(ll),t,t,        &
     &             t,t,t,t,t,t,10000,info)
      enddo
      if (G_FIN .eq. 2) goto 99
      G_VAR_COLS(G_ARGUMENT_POINTER) = M
      do j = 1, n
        ll = ls+j+(j-1)*m
        call mat_wset(m-j,0.0d0,0.0d0,GM_REALS(ll),GM_IMAGS(ll),1)
      enddo
      if (G_ARGUMENT_POINTER+1 .ge. G_TOP_OF_SAVED) then
         call mat_err(18)
         return
      endif
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
      G_VAR_DATALOC(G_ARGUMENT_POINTER) = ls
      G_VAR_ROWS(G_ARGUMENT_POINTER) = m
      G_VAR_COLS(G_ARGUMENT_POINTER) = n
      if (G_LHS .eq. 2) goto 99
      call mat_wset(N*N,0.0D0,0.0D0,GM_REALS(le),GM_IMAGS(le),1)
      do j = 1, n
        ll = le+G_BUF(j)-1+(j-1)*n
        GM_REALS(ll) = 1.0d0
      enddo
      if (G_ARGUMENT_POINTER+1 .ge. G_TOP_OF_SAVED) then
         call mat_err(18)
         return
      endif
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
      G_VAR_DATALOC(G_ARGUMENT_POINTER) = le
      G_VAR_ROWS(G_ARGUMENT_POINTER) = n
      G_VAR_COLS(G_ARGUMENT_POINTER) = n
      goto 99
!===================================================================================================================================
!
   99 continue
END SUBROUTINE mat_matfn4
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_matfn5()

! ident_28="@(#)M_matrix::mat_matfn5(3fp):file handling and other I/O"

character(len=GG_LINELEN)  :: mline
character(len=256)         :: errmsg
integer,save               :: flag=0  ! should be saved or set at each call?
integer,save               :: lrat=5
integer,save               :: mrat=100
integer                    :: ch,top2
integer                    :: id(GG_MAX_NAME_LENGTH)
doubleprecision            :: eps,b,s,t,tdum(2)
logical                    :: text
integer                    :: i, j, k, location, m, n
integer                    :: img
integer                    :: space_left
integer                    :: l2
integer                    :: ll
integer                    :: ls
integer                    :: lun
integer                    :: lunit
integer                    :: lw
integer                    :: lx
integer                    :: ly
integer                    :: mn
!
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   m = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n = G_VAR_COLS(G_ARGUMENT_POINTER)

   !  functions/G_FIN
   !  exec save load prin diar disp base line char plot rat  debu doc  delete
   !    1    2    3    4    5    6    7    8    9   10   11   12   13      14

   select case(G_FIN)
      case(:5,13,14) ! setup for filename parameter

         mn = m*n

         if (G_SYM .eq. semi)then
            flag = 0
         else
            flag = 3
         endif

         if (G_RHS .ge. 2) then            ! if more than one parameter on exec('filename',flag) get value of FLAG
            flag = int(GM_REALS(location))
            top2 = G_ARGUMENT_POINTER
            G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
            location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
            mn = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
         endif

         ! if a single character and a digit set LUN to that so exec(0) works
         if (mn.eq.1 .and. GM_REALS(location).LT.10.0d0)then
            lun = int(GM_REALS(location))
         else
            lun = -1
            do j = 1, GG_LINELEN
               ls = location+j-1
               if (j .le. mn) ch = int(GM_REALS(ls))
               if (j .gt. mn) ch = blank
               if (ch.lt.0 .or. ch.ge.g_charset_size) then
                  call mat_err(38)
                  return
               endif
               G_BUF(j) = ch
            enddo
         endif
      end select
!===================================================================================================================================
      FUN5 : select case(G_FIN)
!===================================================================================================================================
      case(1)                                               ! command::exec
      EXEC_CMD : block
      character(len=:),allocatable :: filename
      if (lun .eq. 0) then                                  ! exec(0)
         G_RIO = G_INPUT_LUN
         G_ERR = 99
      else
         k = G_LINE_POINTER(6)
         G_LIN(k+1) = G_LINE_POINTER(1)
         G_LIN(k+2) = G_LINE_POINTER(3)
         G_LIN(k+3) = G_LINE_POINTER(6)
         G_LIN(k+4) = G_PTZ
         G_LIN(k+5) = G_RIO
         G_LIN(k+6) = G_LINECOUNT(4)
         G_LINE_POINTER(1) = k + 7
         G_LINECOUNT(4) = flag
         G_PTZ = G_PT - 4

         if (G_RIO .eq. G_INPUT_LUN)then
            G_RIO = 12
         endif

         G_RIO = G_RIO + 1

         filename=find_exec_file(ade2str(G_BUF))
         call mat_str2buf(filename,G_BUF,GG_LINELEN)    ! convert input line to ADE buffer
         call mat_files(G_RIO,G_BUF,status='old')
         if(G_FILE_OPEN_ERROR)then
            G_RIO = G_INPUT_LUN
            G_ERR = 99
         endif

         if (flag .ge. 4)then
            call journal(' PAUSE MODE. Enter blank lines.')
         endif

         G_SYM = GG_EOL
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
      endif
      endblock EXEC_CMD
!===================================================================================================================================
      case(2) ! COMMAND::SAVE
      lunit = 1
      call mat_files(lunit,G_BUF)
      k = GG_MAX_NUMBER_OF_NAMES-4
      if (k .lt. G_TOP_OF_SAVED) k = GG_MAX_NUMBER_OF_NAMES
      if (G_RHS .eq. 2) k = top2
      if (G_RHS .eq. 2) call mat_copyid(G_VAR_IDS(1,k),G_SYN)
      do
         location = G_VAR_DATALOC(k)
         m = G_VAR_ROWS(k)
         n = G_VAR_COLS(k)
         do i = 1, GG_MAX_NAME_LENGTH
            j = G_VAR_IDS(i,k)
            G_BUF(i) = j
         enddo
         img = 0
         if (mat_wasum(m*n,GM_IMAGS(location),GM_IMAGS(location),1) .ne. 0.0d0) img = 1
         if(.not.G_FILE_OPEN_ERROR)call mat_savlod(lunit,G_BUF,m,n,img,0,GM_REALS(location),GM_IMAGS(location))
         k = k-1
         if (k .lt. G_TOP_OF_SAVED) exit
      enddo
      call mat_files(-lunit,G_BUF) ! close unit
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0  ! do not set "ans" to filename
!===================================================================================================================================
      case(14) ! COMMAND::DELETE
         DELETE_IT: block
         integer :: templun
         integer :: ios
         call mat_buf2str(mline,G_BUF,GG_LINELEN)
         open(file=mline,newunit=templun,iostat=ios,iomsg=errmsg,status='old')
         if(ios.ne.0)then
            call journal('sc','ERROR:',errmsg)
            G_ERR=999
            exit FUN5
         endif
         close(unit=templun,iostat=ios,iomsg=errmsg,status='delete')
         if(ios.ne.0)then
            call journal('sc','ERROR:',errmsg)
            G_ERR=999
            exit FUN5
         endif
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 0  ! do not set "ans" to filename
         endblock DELETE_IT
!===================================================================================================================================
      case(3) ! command::load
      call mat_buf2str(mline,G_BUF,GG_LINELEN)

      lunit = 2
      call mat_files(LUNIT,G_BUF) ! open the unit
      call mat_buf2str(mline,G_BUF,GG_LINELEN)

      do
         space_left = G_VAR_DATALOC(G_TOP_OF_SAVED) - location
         IF(.not.G_FILE_OPEN_ERROR)then
            call mat_savlod(lunit, &
                & id, &
                & G_VAR_ROWS(G_ARGUMENT_POINTER), &
                & G_VAR_COLS(G_ARGUMENT_POINTER), &
                & img, &
                & space_left, &
                & GM_REALS(location), &
                & GM_IMAGS(location))
         endif

         mn = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)

         if (mn .ne. 0)then
            if (img .eq. 0) call mat_rset(mn,0.0d0,GM_IMAGS(location),1)

            !do i = 1, GG_MAX_NAME_LENGTH
            !   do j=1,G_CHARSET_SIZE
            !      if(id(i).eq.blank)then
            !         id(i) = blank
            !         exit
            !      elseif (id(i).ne.J)then
            !         cycle
            !      else
            !         id(i) = j-1  ! ????
            !         exit
            !      endif
            !   enddo
            !enddo

            G_SYM = semi
            G_RHS = 0
            call MAT_STACK_PUT(ID)
            G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
         else
            exit
         endif

      enddo

      call mat_files(-lunit,G_BUF) ! close unit

      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      case(4) ! command::print
      call mat_files(7,G_BUF)

      location = G_LINECOUNT(2)                       ! hold
      G_LINECOUNT(2) = 999999                         ! turn off paging of output
      if (G_RHS .gt. 1) call mat_print(G_SYN,top2)

      G_LINECOUNT(2) = location                       ! restore

      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      case(5) ! command::diary
      call mat_files(8,G_BUF)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      case(6,7) !     COMMAND::DISPLAY
60    continue
      if (G_FIN.eq.7)goto 65
      if (G_RHS .ge. 2)then
         if (G_RHS .ne. 2) call mat_err(39)           ! Incorrect number of arguments
         if (GM_REALS(location) .lt. 1.0d0)then       ! test if base is 0
            call mat_err(36)                          ! Argument out of range
            exit FUN5
         endif
         b = GM_REALS(location)
         if(b.gt.1)then
            goto 65
         endif
      else
         b=10
      endif
      mn = m*n
      text = .true.
      do i = 1, mn
        ls = location+i-1
        ch = int(GM_REALS(LS))
        text = text .and. (ch.ge.0) .and. (ch.lt.G_CHARSET_SIZE)
        text = text .and. (dble(ch).eq.GM_REALS(ls) )
      enddo
      if(b.le.1)text=.false. ! for forcing non-text display when values are in range of text

      do i = 1, m
         do j = 1, n
           ls = location+i-1+(j-1)*m
           if (GM_REALS(ls) .eq. 0.0d0) ch = blank
           if (GM_REALS(ls) .gt. 0.0d0) ch = plus
           if (GM_REALS(ls) .lt. 0.0d0) ch = minus
           if (text) ch = int(GM_REALS(ls))
           G_BUF(j) = ch
         enddo
         call mat_buf2str(mline,G_BUF,n)
         call journal(mline)
      enddo
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
      exit FUN5
!. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
!     command::base
   65 CONTINUE
      if (G_RHS .ne. 2) then
         call mat_err(39)                         ! Incorrect number of arguments
         exit FUN5
      endif
      if (GM_REALS(location) .le. 1.0d0) then     ! test if base is <= 0
         call mat_err(36)                         ! Argument out of range
         exit FUN5
      endif
      b = GM_REALS(location)
      l2 = location
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
      G_RHS = 1
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      m = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
      eps = GM_REALS(GM_BIGMEM-4)
      do i = 1, m
         ls = l2+(i-1)*n
         ll = location+i-1
         call mat_base(GM_REALS(ll),b,eps,GM_REALS(ls),n)
      enddo
      call mat_rset(m*n,0.0d0,GM_IMAGS(l2),1)
      call mat_wcopy(m*n,GM_REALS(l2),GM_IMAGS(l2),1,GM_REALS(location),GM_IMAGS(location),1)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = n
      G_VAR_COLS(G_ARGUMENT_POINTER) = m
      call mat_stack1(quote)
      if (G_FIN .eq. 6) goto 60
!===================================================================================================================================
      case(8)
!     command::lines
      G_LINECOUNT(2) = int(GM_REALS(location))
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      !!! BROKEN BY GOING TO ASCII. ELIMINATE OR CORRECT
      case(9) !     COMMAND::CHAR                   ! does currently not do anything
      K = IABS(int(GM_REALS(location)))
      IF (M*N.NE.1 .OR. K.GT.G_CHARSET_SIZE-1) then
         call mat_err(36) ! Argument out of range
         exit FUN5
      endif
      CH = K
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      case(10) !     COMMAND::PLOT
      IF (G_RHS .GE. 2) goto 82
      N = M*N
      DO I = 1, N
         LL = location+I-1
         GM_IMAGS(LL) = dble(I)
      enddo
      call mat_plot(STDOUT,GM_IMAGS(location),GM_REALS(location),N,TDUM,0)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
      exit FUN5
!. . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

   82 continue
      IF (G_RHS .EQ. 2) K = 0
      IF (G_RHS .EQ. 3) K = M*N
      IF (G_RHS .GT. 3) K = G_RHS - 2
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - (G_RHS - 1)
      N = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
      IF (G_VAR_ROWS(G_ARGUMENT_POINTER+1)*G_VAR_COLS(G_ARGUMENT_POINTER+1) .NE. N) then
         call mat_err(5)
         exit FUN5
      endif
      LX = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      LY = G_VAR_DATALOC(G_ARGUMENT_POINTER+1)
      IF (G_RHS .GT. 3) location = G_VAR_DATALOC(G_ARGUMENT_POINTER+2)
      call mat_plot(STDOUT,GM_REALS(LX),GM_REALS(LY),N,GM_REALS(location),K)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      case(11) ! COMMAND::RAT
      if (G_RHS .ne. 2) then
         mn = m*n
         l2 = location
         if (G_lhs .eq. 2) l2 = location + mn
         lw = l2 + mn

         if(too_much_memory( lw + lrat - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

         if (G_lhs .eq. 2) G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
         G_VAR_DATALOC(G_ARGUMENT_POINTER) = l2
         G_VAR_ROWS(G_ARGUMENT_POINTER) = m
         G_VAR_COLS(G_ARGUMENT_POINTER) = n
         call mat_rset(G_lhs*mn,0.0d0,GM_IMAGS(location),1)
         do i = 1, mn
            call mat_rat(GM_REALS(location),lrat,mrat,s,t,GM_REALS(lw))
            GM_REALS(location) = s
            GM_REALS(l2) = t
            if (G_lhs .eq. 1) GM_REALS(location) = mat_flop(s/t)
            location = location + 1
            l2 = l2 + 1
         enddo
      else
         mrat = int(GM_REALS(location))
         lrat = int(GM_REALS(location-1))
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 1
         G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
      endif
!===================================================================================================================================
      case(12) !     COMMAND::DEBUG
      G_DEBUG_LEVEL = int(GM_REALS(location))
      call journal('sc',' DEBUG ',G_DEBUG_LEVEL)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      case(13) !     COMMAND::SHOW
      call printit()
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!===================================================================================================================================
      end select FUN5
!===================================================================================================================================
end subroutine mat_matfn5
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack_get(id)

! ident_29="@(#)M_matrix::mat_stack_get(3fp): get variables from storage"

integer,intent(in)  :: id(GG_MAX_NAME_LENGTH)
integer             :: i
integer             :: j
integer             :: k
integer             :: location
integer             :: l2
integer             :: l3
integer             :: li
integer             :: lj
integer             :: current_location
integer             :: ll
integer             :: ls
integer             :: m
integer             :: mk
integer             :: mn
integer             :: mnk
integer             :: n

   call mat_copyid(G_VAR_IDS(1,G_TOP_OF_SAVED-1), ID)    ! copy ID to next blank entry in G_VAR_IDS in case it is not there(?)

   do k=GG_MAX_NUMBER_OF_NAMES,1,-1                      ! start at bottom and search up through names till find the name
      if (mat_eqid(G_VAR_IDS(1:,k), id))exit             ! if found name exit loop
   enddo
   ! if (?)
   ! or if matched the name inserted above did not find it.
   if ( (k .ge. GG_MAX_NUMBER_OF_NAMES-1 .and. G_RHS .gt. 0) .or. (k .eq. G_TOP_OF_SAVED-1) ) then
      G_FIN = 0
      return
   endif

   current_location = G_VAR_DATALOC(K)                               ! found it, so this is the location where the data begins
   IF (G_RHS .EQ. 1) then                                             ! VECT(ARG)
      IF (G_VAR_ROWS(G_ARGUMENT_POINTER) .EQ. 0) goto 99
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      MN = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
      MNK = G_VAR_ROWS(K)*G_VAR_COLS(K)                            ! number of values in this variable
      IF (G_VAR_ROWS(G_ARGUMENT_POINTER) .LT. 0) MN = MNK
      DO I = 1, MN
        LL = location+I-1
        LS = current_location+I-1
        IF (G_VAR_ROWS(G_ARGUMENT_POINTER) .GT. 0) LS = current_location + int(GM_REALS(LL)) - 1
        IF (LS .LT. current_location .OR. LS .GE. current_location+MNK) then
           call mat_err(21)          ! Subscript out of range
           return
        endif
        GM_REALS(LL) = GM_REALS(LS)
        GM_IMAGS(LL) = GM_IMAGS(LS)
      enddo
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      IF (G_VAR_ROWS(K) .GT. 1) G_VAR_ROWS(G_ARGUMENT_POINTER) = MN
      IF (G_VAR_ROWS(K) .EQ. 1) G_VAR_COLS(G_ARGUMENT_POINTER) = MN
      goto 99
   elseif (G_RHS .EQ. 2) then                                              ! MATRIX(ARG,ARG)
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
      IF (G_VAR_ROWS(G_ARGUMENT_POINTER+1) .EQ. 0) G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
      IF (G_VAR_ROWS(G_ARGUMENT_POINTER) .EQ. 0) goto 99
      L2 = G_VAR_DATALOC(G_ARGUMENT_POINTER+1)
      M = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
      IF (G_VAR_ROWS(G_ARGUMENT_POINTER) .LT. 0) M = G_VAR_ROWS(K)
      N = G_VAR_ROWS(G_ARGUMENT_POINTER+1)*G_VAR_COLS(G_ARGUMENT_POINTER+1)
      IF (G_VAR_ROWS(G_ARGUMENT_POINTER+1) .LT. 0) N = G_VAR_COLS(K)
      L3 = L2 + N
      MK = G_VAR_ROWS(K)
      MNK = G_VAR_ROWS(K)*G_VAR_COLS(K)
      DO J = 1, N
         DO I = 1, M
           LI = location+I-1
           IF (G_VAR_ROWS(G_ARGUMENT_POINTER) .GT. 0) LI = location + int(GM_REALS(LI)) - 1
           LJ = L2+J-1
           IF (G_VAR_ROWS(G_ARGUMENT_POINTER+1) .GT. 0) LJ = L2 + int(GM_REALS(LJ)) - 1
           LS = current_location + LI-location + (LJ-L2)*MK
           IF (LS.LT.current_location .OR. LS.GE.current_location+MNK) then
              call mat_err(21)
              return
           endif
           LL = L3 + I-1 + (J-1)*M
           GM_REALS(LL) = GM_REALS(LS)
           GM_IMAGS(LL) = GM_IMAGS(LS)
         enddo
      enddo
      MN = M*N
      call mat_wcopy(MN,GM_REALS(L3),GM_IMAGS(L3),1,GM_REALS(location),GM_IMAGS(location),1)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = M
      G_VAR_COLS(G_ARGUMENT_POINTER) = N
      goto 99
   elseif (G_RHS .GT. 2) then
      call mat_err(21)                                                     ! Subscript out of range
      return
   else                                                                    ! SCALAR
      location = 1
      IF (G_ARGUMENT_POINTER .GT. 0) &
        & location = G_VAR_DATALOC(G_ARGUMENT_POINTER) + &
        & G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
      IF (G_ARGUMENT_POINTER+1 .GE. G_TOP_OF_SAVED) then
         call mat_err(18)  ! Too many names
         return
      endif

      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1

      !  LOAD VARIABLE TO TOP OF STACK
      G_VAR_DATALOC(G_ARGUMENT_POINTER) = location
      G_VAR_ROWS(G_ARGUMENT_POINTER) = G_VAR_ROWS(K)
      G_VAR_COLS(G_ARGUMENT_POINTER) = G_VAR_COLS(K)
      MN = G_VAR_ROWS(K)*G_VAR_COLS(K)

      if(too_much_memory( location+MN - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

      !  IF RAND, MATFN6 GENERATES RANDOM NUMBER
      IF (K .EQ. GG_MAX_NUMBER_OF_NAMES) then
         G_FIN = 7
         G_FUN = 6
         return
      endif
      call mat_wcopy(MN,GM_REALS(current_location),   &
                        & GM_IMAGS(current_location), &
                        & 1,                               &
                        & GM_REALS(location),         &
                        & GM_IMAGS(location),         &
                        & 1)
   endif

99 continue
   G_FIN = -1
   G_FUN = 0

END SUBROUTINE MAT_STACK_GET
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_stack2(op)

! ident_30="@(#)M_matrix::ml_stackp(3fp): binary and ternary operations"

integer           :: op
doubleprecision   :: sr,si,e1,st,e2

integer           ::  i
integer           ::  j
integer           ::  k
integer           ::  k1
integer           ::  k2
integer           ::  kexp
integer           ::  location
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
integer           :: op_select

   l2 = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   m2 = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n2 = G_VAR_COLS(G_ARGUMENT_POINTER)
   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   m = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n = G_VAR_COLS(G_ARGUMENT_POINTER)
   G_FUN = 0

   if(op.eq.DSTAR)then
      op_select=-op
   else
      op_select=op
   endif
   DO_OP: select case(op_select)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (PLUS) ! ADDITION
      if (m .lt. 0) then
         if (m2 .ne. n2) then
            call mat_err(8)
            exit DO_OP
         endif
         m = m2
         n = n2
         G_VAR_ROWS(G_ARGUMENT_POINTER) = m
         G_VAR_COLS(G_ARGUMENT_POINTER) = n
         sr = GM_REALS(location)
         si = GM_IMAGS(location)
         call mat_wcopy(m*n,GM_REALS(location+1),GM_IMAGS(location+1),1,GM_REALS(location),GM_IMAGS(location),1)
         call finish()
         exit DO_OP
      endif
      if (m2 .lt. 0)then
         if (m .ne. n) then
            call mat_err(8)
            exit DO_OP
         endif
         sr = GM_REALS(l2)
         si = GM_IMAGS(l2)
         call finish()
         exit DO_OP
      endif
      if (m .ne. m2) then
         call mat_err(8)
         exit DO_OP
      endif
      if (n .ne. n2) then
         call mat_err(8)
         exit DO_OP
      endif
      call matX_waxpy(m*n,1.0d0,0.0d0,GM_REALS(l2),GM_IMAGS(l2),1,GM_REALS(location),GM_IMAGS(location),1)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (MINUS) ! SUBTRACTION
      if (m .lt. 0) then
         if (m2 .ne. n2)then
            call mat_err(9)
            exit do_op
         endif
         m = m2
         n = n2
         G_VAR_ROWS(G_ARGUMENT_POINTER) = m
         G_VAR_COLS(G_ARGUMENT_POINTER) = n
         sr = GM_REALS(location)
         si = GM_IMAGS(location)
         call mat_wcopy(m*n,GM_REALS(location+1),GM_IMAGS(location+1),1,GM_REALS(location),GM_IMAGS(location),1)
         call mat_wrscal(m*n,-1.0d0,GM_REALS(location),GM_IMAGS(location),1)
         call finish()
         exit DO_OP
      endif
      if (m2 .lt. 0) then
         ! add or subtract scalar
         if (m .ne. n) then
            call mat_err(9)
            exit DO_OP
         endif
         sr = -GM_REALS(l2)
         si = -GM_IMAGS(l2)
         call finish()
         exit DO_OP
      endif
      if (m .ne. m2)then
         call mat_err(9)
         exit DO_OP
      endif
      if (n .ne. n2) then
         call mat_err(9)
         exit DO_OP
      endif
      call matX_waxpy(M*N,-1.0D0,0.0D0,GM_REALS(L2),GM_IMAGS(L2),1,GM_REALS(location),GM_IMAGS(location),1)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (STAR) ! MULTIPLICATION
      if (m2*m2*n2 .eq. 1) goto 10
      if (m*n .eq. 1) goto 11
      if (m2*n2 .eq. 1) goto 10
      if (n .ne. m2) then
         call mat_err(10)
         exit do_op
      endif
      mn = m*n2
      ll = location + mn

      if(too_much_memory( ll+m*n+m2*n2 - G_VAR_DATALOC(G_TOP_OF_SAVED)) ) exit do_op

      call mat_wcopy(m*n+m2*n2,GM_REALS(location),GM_IMAGS(location),-1,GM_REALS(ll),GM_IMAGS(ll),-1)
      do j = 1, n2
         do i = 1, m
            k1 = location + mn + (i-1)
            k2 = l2 + mn + (j-1)*m2
            k = location + (i-1) + (j-1)*m
            GM_REALS(k) = mat_wdotur(N,GM_REALS(k1),GM_IMAGS(k1),m,GM_REALS(k2),GM_IMAGS(k2),1)
            GM_IMAGS(k) = mat_wdotui(N,GM_REALS(k1),GM_IMAGS(k1),m,GM_REALS(k2),GM_IMAGS(k2),1)
         enddo
      enddo
      G_VAR_COLS(G_ARGUMENT_POINTER) = n2
      exit do_op
!-----------------------------------------------------------------------------------------------------------------------------------
   ! multiplication by scalar
   10 continue
      sr = GM_REALS(l2)
      si = GM_IMAGS(l2)
      l1 = location
      goto 13
   11 continue
      sr = GM_REALS(location)
      si = GM_IMAGS(location)
      l1 = location+1
      G_VAR_ROWS(G_ARGUMENT_POINTER) = m2
      G_VAR_COLS(G_ARGUMENT_POINTER) = n2
   13 continue
      mn = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
      call mat_wscal(mn,sr,si,GM_REALS(l1),GM_IMAGS(l1),1)
      if (l1.ne.location) call mat_wcopy(mn,GM_REALS(l1),GM_IMAGS(l1),1,GM_REALS(location),GM_IMAGS(location),1)
!-----------------------------------------------------------------------------------------------------------------------------------
   case (-DSTAR) ! POWER
      IF (M2*N2 .NE. 1) then
         call mat_err(30)
         exit do_op
      endif
      IF (M .NE. N) then
         call mat_err(20)
         exit do_op
      endif
      NEXP = int(GM_REALS(L2))

      IF ( (GM_REALS(L2) .NE. dble(NEXP)) .or. (GM_IMAGS(L2) .NE. 0.0D0) .or. (NEXP .LT. 2) )then
         ! NONINTEGER OR NONPOSITIVE POWER, USE EIGENVECTORS
         G_FUN = 2
         G_FIN = 0
         exit DO_OP
      endif

      MN = M*N

      if(too_much_memory( L2+MN+N - G_VAR_DATALOC(G_TOP_OF_SAVED)) ) exit do_op

      call mat_wcopy(MN,GM_REALS(location),GM_IMAGS(location),1,GM_REALS(L2),GM_IMAGS(L2),1)
      L3 = L2+MN
      DO KEXP = 2, NEXP
         DO J = 1, N
            LS = location+(J-1)*N
            call mat_wcopy(N,GM_REALS(LS),GM_IMAGS(LS),1,GM_REALS(L3),GM_IMAGS(L3),1)
            DO I = 1, N
               LS = L2+I-1
               LL = location+I-1+(J-1)*N
               GM_REALS(LL)=mat_wdotur(N,GM_REALS(LS),GM_IMAGS(LS),N,GM_REALS(L3),GM_IMAGS(L3),1)
               GM_IMAGS(LL)=mat_wdotui(N,GM_REALS(LS),GM_IMAGS(LS),N,GM_REALS(L3),GM_IMAGS(L3),1)
            enddo
         enddo
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (SLASH) ! right division
      if (m2*n2 .ne. 1) then
         if (m2 .eq. n2) G_FUN = 1
         if (m2 .ne. n2) G_FUN = 4
         G_FIN = -1
         G_RHS = 2
         exit DO_OP
      endif
      sr = GM_REALS(l2)
      si = GM_IMAGS(l2)
      mn = m*n
      do i = 1, mn
         ll = location+i-1
         call mat_wdiv(GM_REALS(ll),GM_IMAGS(ll),sr,si,GM_REALS(ll),GM_IMAGS(ll))
         if (G_ERR .gt. 0) exit
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (BSLASH) ! LEFT DIVISION
      if (m*n .ne. 1) then
         if (m .eq. n) G_FUN = 1
         if (m .ne. n) G_FUN = 4
         G_FIN = -2
         G_RHS = 2
         exit DO_OP
      endif
      SR = GM_REALS(location)
      SI = GM_IMAGS(location)
      G_VAR_ROWS(G_ARGUMENT_POINTER) = M2
      G_VAR_COLS(G_ARGUMENT_POINTER) = N2
      MN = M2*N2
      DO I = 1, MN
         LL = location+I-1
         call mat_wdiv(GM_REALS(LL+1),GM_IMAGS(LL+1),SR,SI,GM_REALS(LL),GM_IMAGS(LL))
         IF (G_ERR .GT. 0) exit
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (COLON) ! COLON
      E2 = GM_REALS(L2)
      ST = 1.0D0
      N = 0
      IF (G_RHS .GE. 3) then
         ST = GM_REALS(location)
         G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
         location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
         IF (ST .EQ. 0.0D0) goto 63
      endif

      E1 = GM_REALS(location)
      ! CHECK FOR CLAUSE
      IF (G_RSTK(G_PT) .EQ. 3) then
   !     FOR CLAUSE
         GM_REALS(location) = E1
         GM_REALS(location+1) = ST
         GM_REALS(location+2) = E2
         G_VAR_ROWS(G_ARGUMENT_POINTER) = -3
         G_VAR_COLS(G_ARGUMENT_POINTER) = -1
         exit DO_OP
      endif

      if(too_much_memory( location + MAX(3,int((E2-E1)/ST)) - G_VAR_DATALOC(G_TOP_OF_SAVED) ) ) exit do_op

      do
         IF (ST .GT. 0.0D0 .AND. GM_REALS(location) .GT. E2) exit
         IF (ST .LT. 0.0D0 .AND. GM_REALS(location) .LT. E2) exit
         N = N+1
         location = location+1
         GM_REALS(location) = E1 + dble(N)*ST
         GM_IMAGS(location) = 0.0D0
      enddo

   63 continue
      G_VAR_COLS(G_ARGUMENT_POINTER) = N
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      IF (N .EQ. 0) G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
!-----------------------------------------------------------------------------------------------------------------------------------
   case (1000:2000-1) ! element-wise operations
      op = op -1000
      if (m.ne.m2 .or. n.ne.n2) then
         call mat_err(10)
         exit do_op
      endif
      mn = m*n
      do i = 1, mn
         j = location+i-1
         k = l2+i-1
         select case(op)
         case(STAR)
         call mat_wmul(GM_REALS(J),GM_IMAGS(J), &
                                        GM_REALS(K),GM_IMAGS(K), &
                                        GM_REALS(J),GM_IMAGS(J))
         case(SLASH)
         call mat_wdiv(GM_REALS(J),GM_IMAGS(J), &
                                        GM_REALS(K),GM_IMAGS(K), &
                                        GM_REALS(J),GM_IMAGS(J))
         case(BSLASH)
         call mat_wdiv(GM_REALS(K),GM_IMAGS(K), &
                                        GM_REALS(J),GM_IMAGS(J), &
                                        GM_REALS(J),GM_IMAGS(J))
         end select
         IF (G_ERR .GT. 0) exit
      enddo
!-----------------------------------------------------------------------------------------------------------------------------------
   case (2000:) ! kronecker
      G_FIN = op - 2000 - star + 11
      G_FUN = 6
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
      G_RHS = 2
!-----------------------------------------------------------------------------------------------------------------------------------
   case default
      write(*,*)'<ERROR> unknown operator ',op
      stop
   end select DO_OP
!-----------------------------------------------------------------------------------------------------------------------------------
contains
subroutine finish()
   do i = 1, n
      ll = location + (i-1)*(n+1)
      GM_REALS(ll) = mat_flop(GM_REALS(LL)+sr)
      GM_IMAGS(ll) = mat_flop(GM_IMAGS(LL)+si)
   enddo
end subroutine finish
end subroutine mat_stack2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_getlin() ! get a new input line

character(len=GG_LINELEN) :: mline
character(len=GG_LINELEN) :: shift_mline

integer                   :: istat
integer,parameter         :: retu(GG_MAX_NAME_LENGTH) =  [iachar(['q','u','i','t',' ',' ',' ']),GG_PAD(8:)]
integer                   :: i, j, k
integer                   :: l
integer                   :: n
integer                   :: ios
!.......................................................................
   l = G_LINE_POINTER(1)
!.......................................................................
   11 continue

      G_BUF(:GG_LINELEN)= blank      ! blank out buffer before reading into it
      n = GG_LINELEN+1

      ! get line of input and place it in line buffer
      if(size(G_PSEUDO_FILE).eq.1.and.G_RIO.eq.STDIN)then
         mline=get_pseudo_line()
         G_RIO = G_INPUT_LUN
      elseif(size(G_PSEUDO_FILE).ne.0.and.G_RIO.eq.STDIN)then
         mline=get_pseudo_line()
      else
         mline(:)=' '
         read(G_RIO,'(a)',iostat=ios) mline       ! read input line from file
         if( (ios.ne.0) ) then
             if(is_iostat_end(ios))then           ! hit end of file
                call mat_copyid(G_LIN(l),retu) ! store QUIT onto G_LIN(L) to simulate RETURN command
                l = l + 4
                goto 45
             else
                goto 15
             endif
         endif
      endif
      if(G_ECHO)write(*,'(*(g0))')'',trim(mline)
      shift_mline=adjustl(mline)
      if(shift_mline(1:2).eq.'??')then            ! edit command line history
         mline='. '//mline(3:)
      endif

      if(G_RIO.eq.stdin)then
         call journal('t',mline)   ! reading from standard input, so copy to trail file
      else
         call journal('c',mline)   ! reading from an exec() command, so write as a comment
      endif
      call redo(mline,'.')         ! pass line to REDO(3f). This is a no-op except for storing the line into the input history
                                   ! (unless the input line is the "r" command)

      ! look for other lines to immediately process and then ignore
      shift_mline=adjustl(mline)
      if(shift_mline(1:1).eq.'#')then
         mline=''                                                      ! ignore lines with a # as first non-blank character
      elseif(shift_mline(1:1).eq.'!')then
         if(shift_mline.eq.'!')then
            call get_environment_variable('SHELL',shift_mline)         ! get command to execute
            call execute_command_line(shift_mline,cmdstat=istat)       ! call system shell
         else
            call execute_command_line(shift_mline(2:),cmdstat=istat)   ! call system shell
         endif
         mline=''
      endif

      call mat_str2buf(mline,G_BUF,GG_LINELEN)    ! convert input line to "Hollerith" buffer
!.......................................................................
   15 continue
      n = n-1
      if(n.lt.1)then
         n=1
      elseif (G_BUF(n) .eq. blank)then
         goto 15 ! trim off trailing spaces
      endif

      if (mod(G_LINECOUNT(4),2) .eq. 1) then
              call mat_buf2str(mline,G_BUF,n) ! convert ADE buffer to character
              call journal('s',mline) ! just to standard output
      endif
!.......................................................................
      do j = 1, n
         do k = 1, G_CHARSET_SIZE  ! make sure this letter is in set of LALA characters and get its LALA number
           if (G_BUF(j).eq.k ) goto 30
         enddo
         call journal('sc','Unknown character at column ',j) ! this is not a known character
         k = GG_EOL+1
         if (k .gt. GG_EOL) then
            l = G_LINE_POINTER(1)
            goto 11   ! Unknown character , K not changed. get new line
         endif
         if (k .eq. GG_EOL) exit
         if (k .eq. -1) l = l-1
         if (k .le. 0) cycle
!
   30    continue
         if (k.eq.slash .and. G_BUF(j+1).eq.G_BUF(j)) exit     ! if // rest is comment
         if (k.eq.dot .and. G_BUF(j+1).eq.G_BUF(j)) goto 11    ! if .. line continuation
         if (k.eq.bslash .and. n.eq.1) then                    ! if \ in column 1
            n = G_LINE_POINTER(6) - G_LINE_POINTER(1)
            do i = 1, n
               k = G_LIN(l+i-1)
               G_BUF(i) = k
            enddo
            goto 15
         endif
         G_LIN(l) = k
         if (l.lt.1024) l = l+1
         if (l.eq.1024) call journal('sc','input buffer limit exceeded=',l)
      enddo
!.......................................................................
   45 CONTINUE      ! line is ready, reset line pointers
      G_LIN(l) = GG_EOL;G_LIN(l+1:)=blank
      G_LINE_POINTER(6) = l
      G_LINE_POINTER(4) = G_LINE_POINTER(1)
      G_LINE_POINTER(3) = 0
      G_LINE_POINTER(2) = 0
      G_LINECOUNT(1) = 0
      call mat_getch() ! load first character onto G_CHRA

contains

function get_pseudo_line() result(line)
character(len=GG_LINELEN) :: line
! reallocating all the time is inefficient
   line=G_PSEUDO_FILE(1)
   if(size(G_PSEUDO_FILE).gt.1)then
      G_PSEUDO_FILE=G_PSEUDO_FILE(2:)
   else
      G_PSEUDO_FILE=[character(len=GG_LINELEN) :: ]
   endif
end function get_pseudo_line

end subroutine mat_getlin
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_clause()
doubleprecision    :: e1,e2
integer            :: op
integer            :: r
integer,parameter  :: for(GG_MAX_NAME_LENGTH)   =  [iachar(['f','o','r',' ',' ',' ',' ']),GG_PAD(8:)]
integer,parameter  :: while(GG_MAX_NAME_LENGTH) =  [iachar(['w','h','i','l','e',' ',' ']),GG_PAD(8:)]
integer,parameter  :: iff(GG_MAX_NAME_LENGTH)   =  [iachar(['i','f',' ',' ',' ',' ',' ']),GG_PAD(8:)]
integer,parameter  :: else(GG_MAX_NAME_LENGTH)  =  [iachar(['e','l','s','e',' ',' ',' ']),GG_PAD(8:)]
integer,parameter  :: ennd(GG_MAX_NAME_LENGTH)  =  [iachar(['e','n','d',' ',' ',' ',' ']),GG_PAD(8:)]
integer,parameter  :: do(GG_MAX_NAME_LENGTH)    =  [iachar(['d','o',' ',' ',' ',' ',' ']),GG_PAD(8:)]
integer,parameter  :: thenn(GG_MAX_NAME_LENGTH) =  [iachar(['t','h','e','n',' ',' ',' ']),GG_PAD(8:)]

integer            :: j
integer            :: kount
integer            :: location
integer            :: l2
integer            :: lj
integer            :: m
integer            :: n

   r = -G_FIN-10
   G_FIN = 0
   if (r.lt.1 .or. r.gt.6) goto 01
   goto (02,30,30,80,99,90),R
01 continue
   r = G_RSTK(G_PT)
   goto (99,99,05,40,45,99,99,99,99,99,99,99,15,55,99,99,99),R
   call journal('*mat_clause* -- internal error')
   goto 99
!.......................................................................
!     FOR
02 continue
   call mat_getsym()
   if (G_SYM .ne. isname) then
      call mat_err(34) ! improper for clause
      return
   endif
   G_PT = G_PT+2
   call mat_copyid(G_IDS(1,G_PT),G_SYN)
   call mat_getsym()
   if (G_SYM .ne. equal) then
      call mat_err(34) ! improper for clause
      return
   endif
   call mat_getsym()
   G_RSTK(G_PT) = 3
   ! *call* expr
   return
05 continue
   G_PSTK(G_PT-1) = 0
   G_PSTK(G_PT) = G_LINE_POINTER(4) - 1
   if (mat_eqid(G_SYN,DO)) G_SYM = semi
   if (G_SYM .eq. comma) G_SYM = semi
   if (G_SYM .ne. semi) then
      call mat_err(34) ! improper for clause
      return
   endif
10 continue
   j = G_PSTK(G_PT-1)
   G_LINE_POINTER(4) = G_PSTK(G_PT)
   G_SYM = semi
   G_CHRA = blank
   j = j+1
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   m = G_VAR_ROWS(G_ARGUMENT_POINTER)
   n = G_VAR_COLS(G_ARGUMENT_POINTER)
   lj = location+(j-1)*m
   l2 = location + m*n
   if (m .ne. -3) goto 12
   lj = location+3
   l2 = lj
   GM_REALS(lj) = GM_REALS(location) + dble(j-1)*GM_REALS(location+1)
   GM_IMAGS(lj) = 0.0d0
   if (GM_REALS(location+1).gt.0.0d0 .and. GM_REALS(lj).gt.GM_REALS(location+2)) goto 20
   if (GM_REALS(location+1).lt.0.0d0 .and. GM_REALS(lj).lt.GM_REALS(location+2)) goto 20
   m = 1
   n = j
12 continue
   if (j .gt. n) goto 20
   if (G_ARGUMENT_POINTER+1 .ge. G_TOP_OF_SAVED) then
      call mat_err(18) ! too many names
      return
   endif
   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
   G_VAR_DATALOC(G_ARGUMENT_POINTER) = l2
   G_VAR_ROWS(G_ARGUMENT_POINTER) = m
   G_VAR_COLS(G_ARGUMENT_POINTER) = 1

   if(too_much_memory( l2+m - G_VAR_DATALOC(G_TOP_OF_SAVED) ) )return

   call mat_wcopy(m,GM_REALS(lj),GM_IMAGS(lj),1,GM_REALS(l2),GM_IMAGS(l2),1)
   G_RHS = 0
   call mat_stack_put(G_IDS(1,G_PT))
   if (G_ERR .gt. 0) return
   G_PSTK(G_PT-1) = j
   G_PSTK(G_PT) = G_LINE_POINTER(4)
   G_RSTK(G_PT) = 13
!     *call* PARSE
   return
15 continue
   goto 10
20 continue
   G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
   G_VAR_COLS(G_ARGUMENT_POINTER) = 0
   G_RHS = 0
   call mat_stack_put(G_IDS(1,G_PT))
   if (G_ERR .gt. 0) return
   G_PT = G_PT-2
   goto 80
!.......................................................................
!
!     WHILE OR IF
!
30 continue
   G_PT = G_PT+1
   call mat_copyid(G_IDS(1,G_PT),G_SYN)
   G_PSTK(G_PT) = G_LINE_POINTER(4)-1
35 continue
   G_LINE_POINTER(4) = G_PSTK(G_PT)
   G_CHRA = blank
   call mat_getsym()
   G_RSTK(G_PT) = 4
!     *call* EXPR
   return
40 continue
   if (G_SYM.ne.equal .and. (G_SYM.NE.LESS.and.G_SYM.ne.lbracket) .and. (G_SYM.NE.GREAT.and.G_SYM.ne.rbracket))then
      call mat_err(35)    ! improper WHILE or IF clause
      return
   endif
   op = G_SYM
   call mat_getsym()
   if (G_SYM.EQ.equal .or. (G_SYM.EQ.great)) op = op + G_SYM
   if (op .gt. great) call mat_getsym()
   G_PSTK(G_PT) = 256*G_PSTK(G_PT) + op
   G_RSTK(G_PT) = 5
!     *call* EXPR
   return
45 continue
   op = mod(G_PSTK(G_PT),256)
   G_PSTK(G_PT) = G_PSTK(G_PT)/256
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER-1)
   e1 = GM_REALS(location)
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   e2 = GM_REALS(location)
   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 2
   if (mat_eqid(G_SYN,do) .or. mat_eqid(G_SYN,thenn)) G_SYM = semi
   if (G_SYM .EQ. COMMA) G_SYM = SEMI
   if (G_SYM .NE. SEMI) then
      call mat_err(35) ! improper WHILE or IF clause
      return
   endif
   if (op.eq.equal .and. e1.eq.e2) goto 50
   if ((op.eq.less) .and. e1.lt.e2) goto 50
   if (op.eq.great         .and. e1.gt.e2) goto 50
   if (op.eq.(less+equal)  .and. e1.le.e2) goto 50
   if (op.eq.(great+equal) .and. e1.ge.e2) goto 50
   if (op.eq.(less+great)  .and. e1.ne.e2) goto 50
   G_PT = G_PT-1
   goto 80
50 continue
   G_RSTK(G_PT) = 14
!     *call* PARSE
   return
55 continue
   IF (mat_eqid(G_IDS(1:,G_PT),while)) goto 35
   G_PT = G_PT-1
   if (mat_eqid(G_SYN,else)) goto 80
   return
!.......................................................................
!     SEARCH FOR MATCHING END OR ELSE
80 continue
   kount = 0
   call mat_getsym()
82 continue
   if (G_SYM .eq. GG_EOL) return
   if (G_SYM .ne. isname) goto 83
   if (mat_eqid(G_SYN,ennd) .and. kount.eq.0) return
   if (mat_eqid(G_SYN,else) .and. kount.eq.0) return
   if (mat_eqid(G_SYN,ennd) .or. mat_eqid(G_SYN,else))kount = kount-1
   if (mat_eqid(G_SYN,for) .or. mat_eqid(G_SYN,while).or.mat_eqid(G_SYN,iff)) kount = kount+1
83 continue
   call mat_getsym()
   goto 82
!.......................................................................
!     EXIT FROM LOOP
90 continue

   if (G_RSTK(G_PT) .eq. 14) G_PT = G_PT-1
   if (G_PT .le. G_PTZ) return

   if (G_RSTK(G_PT) .eq. 14) G_PT = G_PT-1
   if (G_PT-1 .le. G_PTZ) return

   if (G_RSTK(G_PT) .eq. 13) G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
   if (G_RSTK(G_PT) .eq. 13) G_PT = G_PT-2
   goto 80
!.......................................................................
!
99 continue
   call mat_err(22)    ! recursion difficulties
end subroutine mat_clause
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_expr()
integer           :: r
integer           :: sign
integer,parameter :: eye(GG_MAX_NAME_LENGTH) =  [iachar(['e','y','e',' ',' ',' ',' ']),GG_PAD(8:)]
integer           :: kount
integer           :: ls
integer           :: op

   r = G_RSTK(G_pt)
!===================================================================================================================================
!        1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 16 18 19 20
   goto (01,01,01,01,01,05,25,99,99,01,01,99,99,99,99,99,99,01,01,01),R
!  what about drop-though???
!===================================================================================================================================
01 continue
   if (G_SYM .eq. colon) call mat_copyid(G_SYN,eye)
   if (G_SYM .eq. colon) G_SYM = isname
   kount = 1
02 continue
   sign = plus
   if (G_SYM .eq. minus) sign = minus
   if (G_SYM.eq.plus .or. G_SYM.eq.minus) call mat_getsym()
   G_pt = G_pt+1
   if (G_pt .gt. G_PSIZE-1) then
      call mat_err(26) ! too complicated (stack overflow)
      return
   endif
   G_PSTK(G_pt) = sign + 256*kount
   G_RSTK(G_pt) = 6
   ! *call* term
   return
!===================================================================================================================================
05 continue
   sign = mod(G_PSTK(G_pt),256)
   kount = G_PSTK(G_pt)/256
   G_pt = G_pt-1
   if (sign .eq. minus) call mat_stack1(minus)
   if (G_err .gt. 0) return
10 continue
   if (G_SYM.eq.plus .or. G_SYM.eq.minus) goto 20
   goto 50
!===================================================================================================================================
20 continue
   if (G_RSTK(G_pt) .eq. 10) then
      ! blank is delimiter inside angle brackets
      ls = G_LINE_POINTER(3) - 2
      if (G_LIN(ls) .eq. blank) goto 50
   endif
   op = G_SYM
   call mat_getsym()
   G_PT = G_PT+1
   G_PSTK(G_PT) = op + 256*kount
   G_RSTK(G_PT) = 7
!     *call* term
   return
!===================================================================================================================================
25 continue
   op = mod(G_PSTK(G_pt),256)
   kount = G_PSTK(G_pt)/256
   G_PT = G_PT-1
   call mat_stack2(op)
   if (G_ERR .gt. 0) return
   goto 10
!===================================================================================================================================
50 continue
   if (G_SYM .ne. colon) goto 60
   call mat_getsym()
   kount = kount+1
   goto 02
!===================================================================================================================================
60 continue
   if (kount .gt. 3) then
      call mat_err(33)  ! too many colons
      return
   endif
   G_RHS = kount
   if (kount .gt. 1) call mat_stack2(colon)
   if (G_err .gt. 0) return
   return
!===================================================================================================================================
99 continue
   call mat_err(22)     ! recursion difficulties
   return
!===================================================================================================================================
end subroutine mat_expr
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_factor()
integer           :: r
integer           :: id(gg_max_name_length)
integer           :: excnt
integer           :: i, j, k
integer           :: location
integer           :: ln
integer           :: ls
integer           :: n

   r = G_RSTK(G_PT)
   !      1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19
   goto (99,99,99,99,99,99,99,01,01,25,45,65,99,99,99,55,75,32,37),r
01 continue
   if (.not.(G_SYM.eq.isnum .or. G_SYM.eq.quote .or.  (G_SYM.EQ.less.or.G_SYM.eq.lbracket))) then

      if (G_SYM .eq. great.or.G_SYM.eq.rbracket)then
         !  MACROS STRING
            call mat_getsym()
            if ((G_SYM .eq. less.or.G_SYM.eq.lbracket) .and. G_CHRA.EQ.GG_EOL) then
            call mat_err(28) ! Empty macro
            return
         endif
            G_PT = G_PT+1
            G_RSTK(G_PT) = 18
            ! *call* EXPR
            return
      endif

      excnt = 0
      if (G_SYM .eq. isname)then
         ! FUNCTION OR MATRIX ELEMENT
         call mat_copyid(id,G_SYN)
         call mat_getsym()
         if (G_SYM .eq. lparen .or. G_SYM.eq. lbrace) goto 42
         G_RHS = 0
         call mat_funs(ID)
         if (G_FIN .ne. 0) then
            call mat_err(25) ! Can not use function name as variable
            return
         endif
         call mat_stack_get(id)
         if (G_ERR .gt. 0) return
         if (G_FIN .eq. 7) goto 50
         if (G_FIN .eq. 0) call mat_copyid(G_IDS(1,G_PT+1),id)

         if (G_FIN .eq. 0) then
            call mat_err(4) ! undefined variable
            return
         endif
         goto 60
      endif
      id(1) = BLANK
      if (G_SYM .eq. lparen .or. G_SYM.eq. lbrace) goto 42
      call mat_err(2)
      return
   endif
!======================================================================
   ! put something on the stack
   location = 1
   if (G_ARGUMENT_POINTER .gt. 0) then
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER) &
       & + G_VAR_ROWS(G_ARGUMENT_POINTER) &
       & * G_VAR_COLS(G_ARGUMENT_POINTER)
   endif
   if (G_ARGUMENT_POINTER+1 .ge. G_TOP_OF_SAVED) then
      call mat_err(18)
      return
   endif

   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER+1
   G_VAR_DATALOC(G_ARGUMENT_POINTER) = location
   if (G_SYM .ne. quote) then
      if (G_SYM .eq. less.or.G_SYM.eq.lbracket) goto 20
      ! single number, getsym stored it in GM_IMAGS
      G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
      G_VAR_COLS(G_ARGUMENT_POINTER) = 1
      GM_REALS(location) = GM_IMAGS(GM_BIGMEM)
      GM_IMAGS(location) = 0.0D0
      call mat_getsym()
      goto 60
      ! string
   endif

   n = 0
   G_LINE_POINTER(4) = G_LINE_POINTER(3)
   call mat_getch()  ! get next character

!==================================
16 continue
   if (G_CHRA .eq. QUOTE) goto 18
17 continue
   ln = location+n
   if (G_CHRA .eq. GG_EOL) then
      call mat_err(31) ! Improper string
      return
   endif
   GM_REALS(LN) = dble(G_CHRA)
   GM_IMAGS(LN) = 0.0d0
   n = n+1
   call mat_getch()  ! get next character
   goto 16

18 continue
   call mat_getch()  ! get next character
   if (G_CHRA .eq. QUOTE) goto 17
!==================================

   if (n .le. 0) then
      call mat_err(31) ! Improper string
      return
   endif
   G_VAR_ROWS(G_ARGUMENT_POINTER) = 1
   G_VAR_COLS(G_ARGUMENT_POINTER) = n
   call mat_getsym()
   goto 60
!==================================================================================================================================!
!  explicit matrix
20 continue
   G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
   G_VAR_COLS(G_ARGUMENT_POINTER) = 0

21 continue
   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
   G_VAR_DATALOC(G_ARGUMENT_POINTER) = &
      &   G_VAR_DATALOC(G_ARGUMENT_POINTER-1) &
      & + G_VAR_ROWS(G_ARGUMENT_POINTER-1)&
      & * G_VAR_COLS(G_ARGUMENT_POINTER-1)
   G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
   G_VAR_COLS(G_ARGUMENT_POINTER) = 0
   call mat_getsym()

22 continue
   if (G_SYM.eq.semi .or. (G_SYM.eq.great.or.G_SYM.eq.rbracket) .or. G_SYM.eq.GG_EOL) then
      if (G_SYM.eq.semi .and. G_CHRA.eq.GG_EOL) call mat_getsym()
      call mat_stack1(quote)
      if (G_ERR .gt. 0) return
      G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 1
      if (G_VAR_ROWS(G_ARGUMENT_POINTER) .eq. 0)  &
         & G_VAR_ROWS(G_ARGUMENT_POINTER) = G_VAR_ROWS(G_ARGUMENT_POINTER+1)
      if (G_VAR_ROWS(G_ARGUMENT_POINTER) .ne. G_VAR_ROWS(G_ARGUMENT_POINTER+1) &
         & .and. G_VAR_ROWS(G_ARGUMENT_POINTER+1) .gt. 0) then
         call mat_err(6)
         return
      endif
      G_VAR_COLS(G_ARGUMENT_POINTER) = G_VAR_COLS(G_ARGUMENT_POINTER) &
         & + G_VAR_COLS(G_ARGUMENT_POINTER+1)
      if (G_SYM .eq. GG_EOL) call mat_getlin()
      if (G_SYM .ne. great.and. G_SYM.ne.rbracket) goto 21
      call mat_stack1(quote)
      if (G_ERR .gt. 0) return
      call mat_getsym()
      goto 60
   endif
   if (G_SYM .eq. comma) call mat_getsym()
   G_PT = G_PT+1
   G_RSTK(G_PT) = 10
   ! *call* EXPR
   return
!==================================================================================================================================!
25 continue
   G_PT = G_PT-1
   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER - 1
   if (G_VAR_ROWS(G_ARGUMENT_POINTER) .eq. 0) then
      G_VAR_ROWS(G_ARGUMENT_POINTER) = G_VAR_ROWS(G_ARGUMENT_POINTER+1)
   endif

   if (G_VAR_ROWS(G_ARGUMENT_POINTER) .ne. G_VAR_ROWS(G_ARGUMENT_POINTER+1))then
      call mat_err(5)
      return
   endif
   G_VAR_COLS(G_ARGUMENT_POINTER) =  &
      & G_VAR_COLS(G_ARGUMENT_POINTER) + G_VAR_COLS(G_ARGUMENT_POINTER+1)
   goto 22
!==================================================================================================================================!
32 continue
   G_PT = G_PT-1
   if ((G_SYM.ne.less.or.G_SYM.eq.lbracket) .and. G_SYM.NE.GG_EOL) then
      call mat_err(37) ! Improper MACROS
      return
   endif
   if (G_SYM .EQ. LESS.or.G_SYM.eq.lbracket) call mat_getsym()
   k = G_LINE_POINTER(6)
   G_LIN(k+1) = G_LINE_POINTER(1)
   G_LIN(k+2) = G_LINE_POINTER(2)
   G_LIN(k+3) = G_LINE_POINTER(6)
   G_LINE_POINTER(1) = k + 4
!     transfer stack to input line
   k = G_LINE_POINTER(1)
   location = G_VAR_DATALOC(G_ARGUMENT_POINTER)
   n = G_VAR_ROWS(G_ARGUMENT_POINTER)*G_VAR_COLS(G_ARGUMENT_POINTER)
   do j = 1, n
      ls = location + j-1
      G_LIN(k) = int(GM_REALS(ls))
      if (G_LIN(k).lt.0 .or. G_LIN(k).ge.G_CHARSET_SIZE) then
         call mat_err(37) ! Improper MACROS
         return
      endif
      if (k.lt.1024) k = k+1
      if (k.eq.1024)call journal('sc','Input buffer char limit exceeded=',K)
   enddo
   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER-1
   G_LIN(k) = GG_EOL;G_LIN(k+1:)=blank
   G_LINE_POINTER(6) = k
   G_LINE_POINTER(4) = G_LINE_POINTER(1)
   G_LINE_POINTER(3) = 0
   G_LINE_POINTER(2) = 0
   G_LINECOUNT(1) = 0
   G_CHRA = blank
   call mat_getsym()
   G_PT = G_PT+1
   G_RSTK(G_PT) = 19
!     *call* EXPR
   return
!==================================================================================================================================!
37 continue
   G_PT = G_PT-1
   k = G_LINE_POINTER(1) - 4
   G_LINE_POINTER(1) = G_LIN(K+1)
   G_LINE_POINTER(4) = G_LIN(K+2)
   G_LINE_POINTER(6) = G_LIN(K+3)
   G_CHRA = BLANK
   call mat_getsym()
   goto 60
!==================================================================================================================================!
42 continue
   call mat_getsym()
   excnt = excnt+1
   G_PT = G_PT+1
   G_PSTK(G_PT) = excnt
   call mat_copyid(G_IDS(1,G_PT),id)
   G_RSTK(G_PT) = 11
   ! *call* expr
   return
!==================================================================================================================================!
45 continue
   call mat_copyid(id,G_IDS(1,G_PT))
   excnt = G_PSTK(G_PT)
   G_PT = G_PT-1
   if (G_SYM .eq. comma) goto 42
   if ((G_SYM .ne. rparen) .and. (G_SYM.ne.rbrace)) then
      call mat_err(3)
      return
   endif
   if ((G_SYM .eq. rparen) .or. (G_SYM .eq. rbrace)) call mat_getsym()
   if (id(1) .eq. blank) goto 60
   G_RHS = excnt
   call MAT_STACK_GET(id)
   if (G_ERR .gt. 0) return
   if (G_FIN .eq. 0) call mat_funs(ID)
   if (G_FIN .eq. 0) then
      call mat_err(4) ! undefined variable
      return
   endif
   ! evaluate matrix function
50 continue
   G_PT = G_PT+1
   G_RSTK(G_PT) = 16
   ! *call* matfn
   return
!==================================================================================================================================!
55 continue
   G_PT = G_PT-1
   goto 60
!==================================================================================================================================!
!  check for quote (transpose) and ** (power)
60 continue
   if (G_SYM .eq. quote) then
      i = G_LINE_POINTER(3) - 2
      if (G_LIN(i) .eq. blank) goto 90
      call mat_stack1(quote)
      if (G_ERR .gt. 0) return
      call mat_getsym()
   endif
   if (G_SYM.ne.star .or. G_CHRA.ne.star) goto 90
   call mat_getsym()
   call mat_getsym()
   G_PT = G_PT+1
   G_RSTK(G_PT) = 12
   ! *call* factor
   goto 01
!==================================================================================================================================!
65 continue
   G_PT = G_PT-1
   call mat_stack2(DSTAR)
   if (G_ERR .gt. 0) return
   if (G_FUN .ne. 2) goto 90
   !  matrix power, use eigenvectors
   G_PT = G_PT+1
   G_RSTK(G_PT) = 17
   ! *call* matfn
   return
!==================================================================================================================================!
75 continue
   G_PT = G_PT-1
90 continue
   return
!==================================================================================================================================!
99 continue
   call mat_err(22) ! recursion difficulties
   return
end subroutine mat_factor
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_term()
integer                   :: op

   select case( G_RSTK(G_PT) )
   case(6,7)
      G_PT = G_PT+1
      G_RSTK(G_PT) = 8
      ! *call* factor
      return
   case(8)
      G_PT = G_PT-1
   case(9)
      op = G_PSTK(G_PT)
      G_PT = G_PT-1
      call mat_stack2(op)
      if (G_ERR .gt. 0)then
         return
      endif
      ! some binary ops done in matfns
      if (G_FUN .ne. 0) then
         G_PT = G_PT+1
         G_RSTK(G_PT) = 15
         ! *call* matfn
         return
      endif
   case(15)
      G_PT = G_PT-1
   case default
      call mat_err(22)
      return
   end select

   op = 0
   if (G_SYM .eq. dot) then
      op = dot
      call mat_getsym()
   endif
   if (.not.(G_SYM.eq.star .or. G_SYM.eq.slash .or. G_SYM.eq.bslash)) then
      return
   endif

   if(op.eq.0)then ! make a special code out of two characters ie. "./" or just set to last symbol found if op=0
      op = G_SYM
   else
      op = G_SYM + 1000
   endif

   call mat_getsym()

   if (G_SYM .eq. dot)then
      op = op + 1000  ! now holds three characters
      call mat_getsym()
   endif

   G_PT = G_PT+1
   G_PSTK(G_PT) = op
   G_RSTK(G_PT) = 9
   ! *call* factor

end subroutine mat_term
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_savlod(lun,id,m,n,img,space_left,xreal,ximag)

! ident_31="@(#)M_matrix::mat_savlod(3fp): read next variable from a save file or write next variable to it"

integer,intent(in)                :: lun                                       ! logical unit number
integer                           :: id(GG_MAX_NAME_LENGTH)                    ! name, format 32a1
integer                           :: m, n                                      ! dimensions
integer                           :: img                                       ! nonzero if ximag is nonzero.  returned on a load
integer                           :: space_left                                ! 0 for save, = space available for load
doubleprecision                   :: xreal(*), ximag(*)                        ! real and optional imaginary parts
character(len=GG_MAX_NAME_LENGTH) :: cid
integer                           :: j,k,l
integer                           :: ios
character(len=256)                :: message
                                                                               ! system dependent formats
character(len=*),parameter        :: f101 ='(A,3I9)'                           ! ID, MxN dimensions of ID, imaginary or real flag
character(len=*),parameter        :: f102 ='(4Z18)'                            ! format for data
      if (space_left .le. 0) then                                              ! save
         call mat_buf2str(cid,id,GG_MAX_NAME_LENGTH)                           ! convert ID to a character string
         write(lun,f101) cid,m,n,img
         do j = 1, n
            k = (j-1)*m+1
            l = j*m
            write(lun,f102) xreal(k:l)                                         ! real
            if (img .ne. 0) write(lun,f102) ximag(k:l)                         ! imaginary
         enddo
      else                                                                     ! load
         read(lun,f101,iostat=ios,iomsg=message) cid,m,n,img
         if(ios.ne.0)then
            call journal(message)
            m=0
            n=0
         else
            call mat_str2buf(cid,id,GG_MAX_NAME_LENGTH)                        ! convert character string to an ID
            if (m*n .gt. space_left) then
               m=0
               n=0
            else
               do j = 1, n
                  k = (j-1)*m+1
                  l = j*m
                  read(lun,f102,iostat=ios,iomsg=message) xreal(k:l)           ! real
                  if(ios.ne.0)then
                     call journal(message)
                     m=0
                     n=0
                     exit
                  elseif (img .ne. 0) then
                     read(lun,f102,iostat=ios,iomsg=message) ximag(k:l)        ! imaginary
                     if(ios.ne.0)then
                        m=0
                        n=0
                        exit
                     endif
                  endif
               enddo
            endif
         endif
      endif
end subroutine mat_savlod
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
logical function mat_eqid(x,y)

!     check for equality of two integer arrays

integer,intent(in) :: x(GG_MAX_NAME_LENGTH)
integer,intent(in) :: y(GG_MAX_NAME_LENGTH)

integer            :: i

   mat_eqid = .true.

   do i = 1, GG_MAX_NAME_LENGTH
      mat_eqid = mat_eqid .and. (x(i).eq.y(i))
   enddo

end function mat_eqid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    ifin_lala(3f) - [M_matrix] test if variable name exists in lala()
!!    LICENSE(MIT)
!!##SYNOPSIS
!!
!!      logical function ifin_lala(varname)
!!
!!      character(len=*),intent(in) :: varname
!!##DESCRIPTION
!!    Determine if a variable name currently exists in lala().
!!
!!##RETURNS
!!     ifin_lala  TRUE if varname exists in lala, else FALSE.
!!##EXAMPLE
!!
!!   sample program:
!!
!!      program demo_ifin_lala
!!      use M_matrix, only : ifin_lala
!!      implicit none
!!         write(*,*)'eps ',ifin_lala('eps')
!!         write(*,*)'unknown ',ifin_lala('unknown')
!!      end program demo_ifin_lala
!!
!!   Results:
!!
!!     eps  T
!!     unknown  F
function ifin_lala(varname)

! ident_32="@(#)M_matrix::ifin_lala(3f) :: access LALA variable stack and see if a variable name exists"

character(len=*),intent(in)        :: varname
integer                            :: id(GG_MAX_NAME_LENGTH)
logical                            :: ifin_lala
integer                            :: k

   ifin_lala=.true.
   if(GM_BIGMEM.LT.0) call lala_init(200000) ! if not initialized initialize
   if( .not.mat_is_name(varname))then
      call journal('sc',varname,'is not a valid variable name')
      ifin_lala=.false.
   endif

   ! convert character name to lala character set
   id=iachar(' ')
   call mat_str2buf(varname,id,len(varname))
   call mat_copyid(G_VAR_IDS(1,G_TOP_OF_SAVED-1), ID)   ! copy ID to next blank entry in G_VAR_IDS for messages(?)

   do k=GG_MAX_NUMBER_OF_NAMES,1,-1                       ! start at bottom and search up through names till find the name
      if (mat_eqid(G_VAR_IDS(1:,k), id))exit            ! if found name exit loop
   enddo

   ! if matched the name inserted above did not find it.
   if ( (k .ge. GG_MAX_NUMBER_OF_NAMES-1) .or.  (k .eq. G_TOP_OF_SAVED-1)) then
      ifin_lala=.false.                              ! unknown variable name
   endif

end function ifin_lala
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!     get_from_lala(3f) - [M_matrix] return data from lala(3f) to calling program
!!     LICENSE(MIT)
!!##SYNOPSIS
!!
!!     subroutine get_from_lala(varname,A,IERR,fixed)
!!
!!      character(len=*),intent(in)               :: varname
!!      [INTRINSIC_TYPE],allocatable,intent(out)  :: a(:,:)
!!      integer,intent(out)                       :: ierr
!!      logical,intent(in),optional               :: fixed
!!
!!##DESCRIPTION
!!    Given the name of a variable defined with lala(3f) commands return
!!    the values to the calling program.
!!
!!##OPTIONS
!!    VARNAME Name of lala(3f) variable to retrieve
!!
!!    FIXED   If .true., A is assumed to be a fixed size. It should only
!!            be specified if the value is .true.! It is up to the user
!!            at this point to ensure the size is correct at this point.
!!##RETURNS
!!      A    May be of TYPE INTEGER, REAL, CHARACTER, LOGICAL or COMPLEX.
!!           May be a scalar, vector, or MxN matrix.
!!    IERR   Zero if no error occurred
!!
!!##EXAMPLE
!!
!!   sample program:
!!
!!    program demo_get_from_lala
!!    use M_matrix, only : lala, get_from_lala, put_into_lala
!!    implicit none
!!    doubleprecision,allocatable :: darr(:,:)
!!    real,allocatable            :: rarr(:,:)
!!    integer,allocatable         :: ivec(:)
!!    integer                     :: ierr
!!    integer                     :: i
!!    character(len=*),parameter  :: gen='(*(g0,1x))'
!!
!!       ! create an array in LALA so have something to get
!!       call lala('A=rand(4,5)*10.5,long,A')
!!
!!       ! get the array as a REAL array
!!       call get_from_lala('A',rarr,ierr)
!!       write(*,gen)'in calling program RARR=',shape(rarr)
!!       write(*,gen)(rarr(i,:),new_line('A'),i=1,size(rarr,dim=1))
!!
!!       ! get the array as a DOUBLEPRECISION  array
!!       call get_from_lala('A',darr,ierr)
!!       write(*,gen)'in calling program darr=',shape(darr)
!!       write(*,gen)(darr(i,:),new_line('A'),i=1,size(darr,dim=1))
!!
!!       ! get the array as an INTEGER vector, much like the
!!       ! PUSH(3f) intrinsic
!!       call get_from_lala('A',ivec,ierr)
!!       write(*,gen)'in calling program ivec=',shape(ivec)
!!       write(*,gen)ivec
!!
!!    end program demo_get_from_lala
!!
!!   Results:
!!
!!    >A  =
!!    >   2.2189  6.9865  9.2213  7.6267  2.4278
!!    >   7.9385  6.5981  0.7179  2.0844  2.2729
!!    >   0.0023  8.9223  5.8889  5.7147  9.2756
!!    >   3.4684  7.2002  6.9547  2.4368  6.8514
!!
!!    >A  =
!!    >    COLUMNS     1 THRU     4
!!    >  2.218911087373272 6.986501594306901 9.221273053670302 7.626682105707005
!!    >  7.938460468780249 6.598113777581602 0.717927386518568 2.084401034284383
!!    >  0.002321913605556 8.922324976650998 5.888910365989432 5.714701820863411
!!    >  3.468434463255107 7.200175708159804 6.954747841693461 2.436785291880369
!!    >    COLUMNS     5 THRU     5
!!    >  2.427849056432024
!!    >  2.272864263039082
!!    >  9.275582205271348
!!    >  6.851391694508493
!!    >in calling program RARR= 4 5
!!    > 2.21891117 6.98650169 9.22127342 7.62668228 2.42784905
!!    > 7.93846035 6.59811401 0.717927396 2.08440113 2.27286434
!!    > 0.232191361E-2 8.92232513 5.88891029 5.71470165 9.27558231
!!    > 3.46843457 7.20017576 6.95474768 2.43678522 6.85139179
!!
!!    >in calling program darr= 4 5
!!    > 2.2189110873732716 6.9865015943069011 9.2212730536703020 ..
!!    > 7.6266821057070047 2.4278490564320236
!!    > 7.9384604687802494 6.5981137775816023 0.71792738651856780 ..
!!    > 2.0844010342843831 2.2728642630390823
!!    > 0.23219136055558920E-2 8.9223249766509980 5.8889103659894317 ..
!!    > 5.7147018208634108 9.2755822052713484
!!    > 3.4684344632551074 7.2001757081598043 6.9547478416934609 ..
!!    > 2.4367852918803692 6.8513916945084929
!!
!!    >in calling program ivec= 20
!!    > 2 8 0 3 7 7 9 7 9 1 6 7 8 2 6 2 2 2 9 7
subroutine get_double_from_lala(varname,A,type,IERR)

! ident_33="@(#)M_matrix::get_double_from_lala(3f) :: access LALA variable stack and get a variable by name and its data from the stack"

character(len=*),intent(in)              :: varname    ! the name of A.
integer,intent(in)                       :: type       ! type =  0  get REAL A from LALA, type  = 1  get IMAGINARY A into LALA,
integer,INTENT(OUT)                      :: ierr       ! return with nonzero IERR after LALA error message.
doubleprecision,allocatable,intent(out)  :: a(:,:)     ! A is an M by N matrix
integer                                  :: id(GG_MAX_NAME_LENGTH)
integer                                  :: i,j,k,location,m,n

   if(GM_BIGMEM.LT.0) then
      call lala_init(200000) ! if not initialized initialize
   endif
   ierr=0

   ! convert character name to lala character set
   id=iachar(' ')
   call mat_str2buf(varname,id,len(varname))
   ! ??? make sure this letter is in set of LALA characters and get its LALA number
   call mat_copyid(G_VAR_IDS(1,G_TOP_OF_SAVED-1), ID)   ! copy ID to next blank entry in G_VAR_IDS for messages(?)

   do k=GG_MAX_NUMBER_OF_NAMES,1,-1                       ! start at bottom and search up through names till find the name
      if (mat_eqid(G_VAR_IDS(1:,k), id))exit            ! if found name exit loop
   enddo

   ! if matched the name inserted above did not find it.
   if ( (k .ge. GG_MAX_NUMBER_OF_NAMES-1 .and. G_RHS .gt. 0) .or. (k .eq. G_TOP_OF_SAVED-1) ) then
      call journal('sc','<ERROR>get_double_from_lala: unknown variable name',varname)
      IERR=4
      if(allocated(a))deallocate(a)
      allocate(a(0,0))
   else
      if(allocated(a))deallocate(a)
      M=G_VAR_ROWS(k)
      N=G_VAR_COLS(k)
      allocate(a(m,n))
      location=G_VAR_DATALOC(k)
      do j=1,n
         do i=1,m
            if(type.eq.0)then
               a(i,j)=GM_REALS(location)       ! type =  0  GET REAL A FROM LALA,
            else
               a(i,j)=GM_IMAGS(location)       ! type =  1  GET IMAGINARY A FROM LALA,
            endif
            location=location+1
         enddo
      enddo
   endif

end subroutine get_double_from_lala
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function rowpack(arr) result(vec)
doubleprecision,intent(in)  :: arr(:,:)
doubleprecision,allocatable :: vec(:)
integer                     :: i
if(allocated(vec))deallocate(vec)
vec=[(arr(:,i),i=1,size(arr,dim=2))]
end function rowpack
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!     put_from_lala(3f) - [M_matrix] return data from lala(3f) to calling program
!!     LICENSE(MIT)
!!##SYNOPSIS
!!
!!   subroutine put_into_lala(varname,A,IERR)
!!
!!    character(len=*),intent(in)              :: varname
!!    [INTRINSIC_TYPE],allocatable,intent(in)  :: a(:,:)
!!    integer,intent(out)                      :: ierr
!!
!!##DESCRIPTION
!!    Define a variable in the lala(3f) utility with a variable declared
!!    in the calling program.
!!
!!##OPTIONS
!!    VARNAME Name of lala(3f) variable to retrieve
!!      A     May be of TYPE INTEGER, REAL, CHARACTER, LOGICAL or COMPLEX.
!!            May be a scalar, vector, or MxN matrix.
!!
!!##RETURNS
!!    IERR   Zero if no error occurred
!!
!!##EXAMPLE
!!
!!   sample program:
!!
!!    program demo_put_into_lala
!!    use M_matrix, only : lala, get_from_lala, put_into_lala
!!    implicit none
!!    integer :: ierr
!!
!!       ! store some data from the program into lala(3f)
!!       call put_into_lala('A',[1,2,3,4,5,6,7,8,9],ierr)
!!       call put_into_lala('B',[1.1,2.2,3.3],ierr)
!!       call put_into_lala('C',"This is my title",ierr)
!!
!!       ! call lala(3f) and display the values
!!       call lala([character(len=80) :: &
!!       & 'who,A,B', &
!!       & 'display(C);', &
!!       & '', &
!!       & ''])
!!
!!    end program demo_put_into_lala
!!
!!   Results:
!!
!!      > Your current variables are...
!!      > C  B  A  eps  flops  eye  rand
!!      >using 33 out of 200000 elements
!!      >
!!      > A  =
!!      >     1.    2.    3.    4.    5.    6.    7.    8.    9.
!!      >
!!      > B  =
!!      >    1.1000    2.2000    3.3000
!!      >This is my title
subroutine store_double_into_lala(varname,realxx,imagxx,ierr)

! ident_34="@(#)M_matrix:: _store_double_into_lala(3f): put a variable name and its data onto LALA stack"

character(len=*),intent(in)          :: varname                ! the name of realxx.
doubleprecision,intent(in)           :: realxx(:,:)            ! inputarray is an M by N matrix
doubleprecision,intent(in),optional  :: imagxx(:,:)            ! inputarray is an M by N matrix
integer,intent(out)                  :: ierr                   ! return with nonzero ierr after LALA error message.

integer                              :: img
integer                              :: space_left
integer                              :: id(GG_MAX_NAME_LENGTH) ! ID = name, in numeric format
integer                              :: location
integer                              :: m,n                    ! m, n = dimensions

   if(GM_BIGMEM.LT.0) then
      call lala_init(200000) ! if not initialized initialize
   else
   endif

   ierr=0
   if(present(imagxx))then
      img=1
      if(size(realxx,dim=1).ne.size(imagxx,dim=1).or.size(realxx,dim=2).ne.size(imagxx,dim=2))then
         call journal('sc','<ERROR>*lala_put* real and imaginary parts have different sizes')
         ierr=-1
         return
      endif
   else
      img=0
   endif

   if(G_ARGUMENT_POINTER.ne.0)then
      location = G_VAR_DATALOC(G_ARGUMENT_POINTER) ! location of bottom of used scratch space
   else
     !call journal('sc','<WARNING>G_ARGUMENT_POINTER=',G_ARGUMENT_POINTER)
     G_ARGUMENT_POINTER= 1
     G_VAR_DATALOC(G_ARGUMENT_POINTER)=1
     location=1
   endif
   space_left = G_VAR_DATALOC(G_TOP_OF_SAVED) - location
   !! assume input arrays can be one or two dimension but lala stores everything as a vector and store m and n
   m=size(realxx,dim=1)
   n=size(realxx,dim=2)
   if (m*n .gt. space_left) then
      call journal('sc','<ERROR>*lala_put* insufficient space to save data to LALA')
      ierr=-2
      return
   elseif(m*n.eq.0)then ! check for zero-size input array
      call journal('sc','<ERROR>*lala_put* cannot save empty arrays to LALA')
      ierr=-3
      return
   else
      if (img .eq. 0)then
         call mat_rset(m*n,0.0d0,GM_IMAGS(location),1) ! set imaginary values to zero
      else
         GM_IMAGS(location:location+m*n-1)=rowpack(imagxx)
      endif
      GM_REALS(location:location+m*n-1)=rowpack(realxx)
   endif
   G_VAR_ROWS(G_ARGUMENT_POINTER)=m
   G_VAR_COLS(G_ARGUMENT_POINTER)=n
   G_SYM = semi   !! ??? why
   G_RHS = 0      !! ??? why
   call mat_str2buf(varname,id,GG_MAX_NAME_LENGTH)                        ! convert character string to an ID
   !! ???? if(G_ERR.ne.0)
   !! ???? check if varname is an acceptable name
   call mat_stack_put(id)
   !! ???? if(G_ERR.ne.0)
   G_ARGUMENT_POINTER = G_ARGUMENT_POINTER + 1
   G_VAR_ROWS(G_ARGUMENT_POINTER) = 0
   G_VAR_COLS(G_ARGUMENT_POINTER) = 0
end subroutine store_double_into_lala
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine store_array_into_lala(varname,anything,ierr)
character(len=*),intent(in)  :: varname
class(*)                     :: anything(:,:)
integer,intent(out)          :: ierr
   select type(anything)
!!    type is (character(len=*));
!!       call store_double_into_lala(varname,
!!       reshape(real(str2ade(anything),kind=dp),[1,len(anything)])
!!       ,ierr=ierr)
!!       call store_double_into_lala(varname,reshape(real(str2ade(anything),kind=dp),[1,len(anything)]),ierr=ierr)
   type is (complex);              call store_double_into_lala(varname,real(anything,kind=dp), &
                                                                     & real(aimag(anything),kind=dp),ierr=ierr)
   type is (complex(kind=dp));     call store_double_into_lala(varname,real(anything),aimag(anything),ierr=ierr)
   type is (integer(kind=int8));   call store_double_into_lala(varname,real(anything,kind=dp),ierr=ierr)
   type is (integer(kind=int16));  call store_double_into_lala(varname,real(anything,kind=dp),ierr=ierr)
   type is (integer(kind=int32));  call store_double_into_lala(varname,real(anything,kind=dp),ierr=ierr)
   type is (integer(kind=int64));  call store_double_into_lala(varname,real(anything,kind=dp),ierr=ierr)
   type is (real(kind=real32));    call store_double_into_lala(varname,real(anything,kind=dp),ierr=ierr)
   type is (real(kind=real64));    call store_double_into_lala(varname,real(anything,kind=dp),ierr=ierr)
   type is (real(kind=real128));   call store_double_into_lala(varname,real(anything,kind=dp),ierr=ierr)
   ! arbitrarily, 0 is false and not 0 is true, although I prefer the opposite
   type is (logical);              call store_double_into_lala(varname,merge(0.1d0,0.0d0,anything),ierr=ierr)
   class default
      stop 'crud. store_array_into_lala(1) does not know about this type'
   end select
end subroutine store_array_into_lala
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine store_vector_into_lala(varname,anything,ierr)
character(len=*),intent(in)  :: varname
class(*)                     :: anything(:)
integer,intent(out)          :: ierr
integer                      :: i
   select type(anything)
    type is (character(len=*));
       associate ( &
                   & letters  => [( real(str2ade(anything(i)),kind=dp),i=1,size(anything,dim=1))] , &
                   & r=> size(anything), &
                   & c=> len(anything) &
                 )
          call store_double_into_lala(varname,reshape(letters,[r,c],order=[2,1]),ierr=ierr)
       end associate
    type is (complex)
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]), &
                                          & reshape(real(aimag(anything),kind=dp),[1,size(anything)]), ierr=ierr)
    type is (complex(kind=dp))
       call store_double_into_lala(varname,reshape(real(anything),[1,size(anything)]), &
                                          & reshape(aimag(anything),[1,size(anything)]), ierr=ierr)
    type is (integer(kind=int8))
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]),ierr=ierr)
    type is (integer(kind=int16))
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]),ierr=ierr)
    type is (integer(kind=int32))
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]),ierr=ierr)
    type is (integer(kind=int64))
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]),ierr=ierr)
    type is (real(kind=real32))
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]),ierr=ierr)
    type is (real(kind=real64))
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]),ierr=ierr)
    type is (real(kind=real128))
       call store_double_into_lala(varname,reshape(real(anything,kind=dp),[1,size(anything)]),ierr=ierr)
    type is (logical)
       call store_double_into_lala(varname,reshape(merge(0.1d0,0.0d0,anything),[1,size(anything)]),ierr=ierr)
    class default
      stop 'crud. store_vector_into_lala(1) does not know about this type'
      ierr=-20
   end select
end subroutine store_vector_into_lala
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine store_scalar_into_lala(varname,anything,ierr)
character(len=*),intent(in)  :: varname
class(*)                     :: anything
integer,intent(out)          :: ierr
logical,parameter            :: T=.true.
   select type(anything)
    type is (character(len=*))
       call store_double_into_lala(varname,reshape(real(str2ade(anything),kind=dp),[1,len(anything)]),ierr=ierr)
    type is (complex)
       call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]), &
                                          & reshape([real(aimag(anything),kind=dp)],[1,1]), ierr=ierr)
    type is (complex(kind=dp))
             call store_double_into_lala(varname,reshape([real(anything)],[1,1]), reshape([aimag(anything)],[1,1]), ierr=ierr)
    type is (integer(kind=int8));  call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]),ierr=ierr)
    type is (integer(kind=int16)); call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]),ierr=ierr)
    type is (integer(kind=int32)); call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]),ierr=ierr)
    type is (integer(kind=int64)); call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]),ierr=ierr)
    type is (real(kind=real32));   call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]),ierr=ierr)
    type is (real(kind=real64));   call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]),ierr=ierr)
    type is (real(kind=real128));  call store_double_into_lala(varname,reshape([real(anything,kind=dp)],[1,1]),ierr=ierr)
    ! arbitrarily, 0 is false and not 0 is true, although I prefer the opposite
    type is (logical);             call store_double_into_lala(varname,reshape([merge(1.0d0,0.0d0,anything)],[1,1]),ierr=ierr)
    class default
      stop 'crud. store_scalar_into_lala(1) does not know about this type'
   end select
end subroutine store_scalar_into_lala
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine get_fixed_array_from_lala_int8(varname,out,ierr,fixed)
character(len=*),intent(in)                :: varname
integer(kind=int8),intent(out) :: out(:,:)
doubleprecision,allocatable                :: double(:,:)
integer,intent(out)                        :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(double,kind=int8)
end subroutine get_fixed_array_from_lala_int8
!===================================================================================================================================
subroutine get_fixed_array_from_lala_int16(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
integer(kind=int16),intent(out) :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(double,kind=int16)
end subroutine get_fixed_array_from_lala_int16
!===================================================================================================================================
subroutine get_fixed_array_from_lala_int32(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
integer(kind=int32),intent(out) :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(double,kind=int32)
end subroutine get_fixed_array_from_lala_int32
!===================================================================================================================================
subroutine get_fixed_array_from_lala_int64(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
integer(kind=int64),intent(out) :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=int64)
end subroutine get_fixed_array_from_lala_int64
!===================================================================================================================================
subroutine get_fixed_array_from_lala_real32(varname,out,ierr,fixed)
character(len=*),intent(in)               :: varname
real(kind=real32),intent(out) :: out(:,:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=real32)
end subroutine get_fixed_array_from_lala_real32
!===================================================================================================================================
subroutine get_fixed_array_from_lala_real64(varname,out,ierr,fixed)
character(len=*),intent(in)               :: varname
real(kind=real64),intent(out) :: out(:,:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=real64)
end subroutine get_fixed_array_from_lala_real64
!===================================================================================================================================
subroutine get_fixed_array_from_lala_real128(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
real(kind=real128),intent(out)  :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=real128)
end subroutine get_fixed_array_from_lala_real128
!===================================================================================================================================
subroutine get_fixed_array_from_lala_logical(varname,out,ierr,fixed)
character(len=*),intent(in)      :: varname
logical,intent(out)  :: out(:,:)
doubleprecision,allocatable      :: double(:,:)
integer,intent(out)              :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=merge(.false.,.true.,nint(double).eq.0)
end subroutine get_fixed_array_from_lala_logical
!===================================================================================================================================
subroutine get_fixed_array_from_lala_cmplx(varname,out,ierr,fixed)
character(len=*),intent(in)      :: varname
complex,intent(out)  :: out(:,:)
doubleprecision,allocatable      :: double(:,:), doublei(:,:)
integer,intent(out)              :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(double,doublei,kind=sp)
end subroutine get_fixed_array_from_lala_cmplx
!===================================================================================================================================
subroutine get_fixed_array_from_lala_dpcmplx(varname,out,ierr,fixed)
character(len=*),intent(in)               :: varname
complex(kind=dp),intent(out)  :: out(:,:)
doubleprecision,allocatable               :: double(:,:), doublei(:,:)
integer,intent(out)                       :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(double,doublei,kind=sp)
end subroutine get_fixed_array_from_lala_dpcmplx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_character(varname,out,ierr,fixed)
character(len=*),intent(in)              :: varname
character(len=*),intent(out) :: out(:)
doubleprecision,allocatable              :: double(:,:)
integer,intent(out)                      :: ierr
integer                                  :: i,j
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   do i=1,size(double,dim=1)
      do j=1,size(double,dim=2)
         out(i)(j:j)=achar(nint(double(i,j)))
      enddo
   enddo
end subroutine get_fixed_vector_from_lala_character
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_int8(varname,out,ierr,fixed)
character(len=*),intent(in)                :: varname
integer(kind=int8),intent(out) :: out(:)
doubleprecision,allocatable                :: double(:,:)
integer,intent(out)                        :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(rowpack(double),kind=int8)
end subroutine get_fixed_vector_from_lala_int8
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_int16(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
integer(kind=int16),intent(out) :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(rowpack(double),kind=int16)
end subroutine get_fixed_vector_from_lala_int16
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_int32(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
integer(kind=int32),intent(out) :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(rowpack(double),kind=int32)
end subroutine get_fixed_vector_from_lala_int32
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_int64(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
integer(kind=int64),intent(out) :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=int64)
end subroutine get_fixed_vector_from_lala_int64
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_real32(varname,out,ierr,fixed)
character(len=*),intent(in)               :: varname
real(kind=real32),intent(out) :: out(:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=real32)
end subroutine get_fixed_vector_from_lala_real32
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_real64(varname,out,ierr,fixed)
character(len=*),intent(in)               :: varname
real(kind=real64),intent(out) :: out(:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=real64)
end subroutine get_fixed_vector_from_lala_real64
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_real128(varname,out,ierr,fixed)
character(len=*),intent(in)                 :: varname
real(kind=real128),intent(out)  :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=real128)
end subroutine get_fixed_vector_from_lala_real128
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_logical(varname,out,ierr,fixed)
character(len=*),intent(in)      :: varname
logical,intent(out)  :: out(:)
doubleprecision,allocatable      :: double(:,:)
integer,intent(out)              :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=merge(.false.,.true.,nint(rowpack(double)).eq.0)
end subroutine get_fixed_vector_from_lala_logical
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_cmplx(varname,out,ierr,fixed)
character(len=*),intent(in)      :: varname
complex,intent(out)  :: out(:)
doubleprecision,allocatable      :: double(:,:), doublei(:,:)
integer,intent(out)              :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(rowpack(double),rowpack(doublei),kind=sp)
end subroutine get_fixed_vector_from_lala_cmplx
!===================================================================================================================================
subroutine get_fixed_vector_from_lala_dpcmplx(varname,out,ierr,fixed)
character(len=*),intent(in)               :: varname
complex(kind=dp),intent(out)  :: out(:)
doubleprecision,allocatable               :: double(:,:), doublei(:,:)
integer,intent(out)                       :: ierr
logical,intent(in)                         :: fixed
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(rowpack(double),rowpack(doublei),kind=dp)
end subroutine get_fixed_vector_from_lala_dpcmplx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine get_fixed_scalar_from_lala_character(varname,out,ierr,fixed)
character(len=*),intent(in)              :: varname
character(len=*),intent(out) :: out
doubleprecision,allocatable              :: double(:,:)
integer,intent(out)                      :: ierr
logical,intent(in)                         :: fixed
integer                                  :: i,j,k
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   k=0
   do i=1,size(double,dim=1)
      do j=1,size(double,dim=2)
         k=k+1
         out(k:k)=achar(nint(double(i,j)))
      enddo
   enddo
end subroutine get_fixed_scalar_from_lala_character
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine get_array_from_lala_int8(varname,out,ierr)
character(len=*),intent(in)                :: varname
integer(kind=int8),allocatable,intent(out) :: out(:,:)
doubleprecision,allocatable                :: double(:,:)
integer,intent(out)                        :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(double,kind=int8)
end subroutine get_array_from_lala_int8
!===================================================================================================================================
subroutine get_array_from_lala_int16(varname,out,ierr)
character(len=*),intent(in)                 :: varname
integer(kind=int16),allocatable,intent(out) :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(double,kind=int16)
end subroutine get_array_from_lala_int16
!===================================================================================================================================
subroutine get_array_from_lala_int32(varname,out,ierr)
character(len=*),intent(in)                 :: varname
integer(kind=int32),allocatable,intent(out) :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(double,kind=int32)
end subroutine get_array_from_lala_int32
!===================================================================================================================================
subroutine get_array_from_lala_int64(varname,out,ierr)
character(len=*),intent(in)                 :: varname
integer(kind=int64),allocatable,intent(out) :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=int64)
end subroutine get_array_from_lala_int64
!===================================================================================================================================
subroutine get_array_from_lala_real32(varname,out,ierr)
character(len=*),intent(in)               :: varname
real(kind=real32),allocatable,intent(out) :: out(:,:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=real32)
end subroutine get_array_from_lala_real32
!===================================================================================================================================
subroutine get_array_from_lala_real64(varname,out,ierr)
character(len=*),intent(in)               :: varname
real(kind=real64),allocatable,intent(out) :: out(:,:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=real64)
end subroutine get_array_from_lala_real64
!===================================================================================================================================
subroutine get_array_from_lala_real128(varname,out,ierr)
character(len=*),intent(in)                 :: varname
real(kind=real128),allocatable,intent(out)  :: out(:,:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(double,kind=real128)
end subroutine get_array_from_lala_real128
!===================================================================================================================================
subroutine get_array_from_lala_logical(varname,out,ierr)
character(len=*),intent(in)      :: varname
logical,allocatable,intent(out)  :: out(:,:)
doubleprecision,allocatable      :: double(:,:)
integer,intent(out)              :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=merge(.false.,.true.,nint(double).eq.0)
end subroutine get_array_from_lala_logical
!===================================================================================================================================
subroutine get_array_from_lala_cmplx(varname,out,ierr)
character(len=*),intent(in)      :: varname
complex,allocatable,intent(out)  :: out(:,:)
doubleprecision,allocatable      :: double(:,:), doublei(:,:)
integer,intent(out)              :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(double,doublei,kind=sp)
end subroutine get_array_from_lala_cmplx
!===================================================================================================================================
subroutine get_array_from_lala_dpcmplx(varname,out,ierr)
character(len=*),intent(in)               :: varname
complex(kind=dp),allocatable,intent(out)  :: out(:,:)
doubleprecision,allocatable               :: double(:,:), doublei(:,:)
integer,intent(out)                       :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(double,doublei,kind=sp)
end subroutine get_array_from_lala_dpcmplx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine get_vector_from_lala_character(varname,out,ierr)
character(len=*),intent(in)              :: varname
character(len=:),allocatable,intent(out) :: out(:)
doubleprecision,allocatable              :: double(:,:)
integer,intent(out)                      :: ierr
integer                                  :: i,j
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(allocated(out))deallocate(out)
   allocate(character(len=size(double,dim=2)) :: out(size(double,dim=1)))
   do i=1,size(double,dim=1)
      do j=1,size(double,dim=2)
         out(i)(j:j)=achar(nint(double(i,j)))
      enddo
   enddo
end subroutine get_vector_from_lala_character
!===================================================================================================================================
subroutine get_vector_from_lala_int8(varname,out,ierr)
character(len=*),intent(in)                :: varname
integer(kind=int8),allocatable,intent(out) :: out(:)
doubleprecision,allocatable                :: double(:,:)
integer,intent(out)                        :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(rowpack(double),kind=int8)
end subroutine get_vector_from_lala_int8
!===================================================================================================================================
subroutine get_vector_from_lala_int16(varname,out,ierr)
character(len=*),intent(in)                 :: varname
integer(kind=int16),allocatable,intent(out) :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(rowpack(double),kind=int16)
end subroutine get_vector_from_lala_int16
!===================================================================================================================================
subroutine get_vector_from_lala_int32(varname,out,ierr)
character(len=*),intent(in)                 :: varname
integer(kind=int32),allocatable,intent(out) :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=nint(rowpack(double),kind=int32)
end subroutine get_vector_from_lala_int32
!===================================================================================================================================
subroutine get_vector_from_lala_int64(varname,out,ierr)
character(len=*),intent(in)                 :: varname
integer(kind=int64),allocatable,intent(out) :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=int64)
end subroutine get_vector_from_lala_int64
!===================================================================================================================================
subroutine get_vector_from_lala_real32(varname,out,ierr)
character(len=*),intent(in)               :: varname
real(kind=real32),allocatable,intent(out) :: out(:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=real32)
end subroutine get_vector_from_lala_real32
!===================================================================================================================================
subroutine get_vector_from_lala_real64(varname,out,ierr)
character(len=*),intent(in)               :: varname
real(kind=real64),allocatable,intent(out) :: out(:)
doubleprecision,allocatable               :: double(:,:)
integer,intent(out)                       :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=real64)
end subroutine get_vector_from_lala_real64
!===================================================================================================================================
subroutine get_vector_from_lala_real128(varname,out,ierr)
character(len=*),intent(in)                 :: varname
real(kind=real128),allocatable,intent(out)  :: out(:)
doubleprecision,allocatable                 :: double(:,:)
integer,intent(out)                         :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=real(rowpack(double),kind=real128)
end subroutine get_vector_from_lala_real128
!===================================================================================================================================
subroutine get_vector_from_lala_logical(varname,out,ierr)
character(len=*),intent(in)      :: varname
logical,allocatable,intent(out)  :: out(:)
doubleprecision,allocatable      :: double(:,:)
integer,intent(out)              :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   out=merge(.false.,.true.,nint(rowpack(double)).eq.0)
end subroutine get_vector_from_lala_logical
!===================================================================================================================================
subroutine get_vector_from_lala_cmplx(varname,out,ierr)
character(len=*),intent(in)      :: varname
complex,allocatable,intent(out)  :: out(:)
doubleprecision,allocatable      :: double(:,:), doublei(:,:)
integer,intent(out)              :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(rowpack(double),rowpack(doublei),kind=sp)
end subroutine get_vector_from_lala_cmplx
!===================================================================================================================================
subroutine get_vector_from_lala_dpcmplx(varname,out,ierr)
character(len=*),intent(in)               :: varname
complex(kind=dp),allocatable,intent(out)  :: out(:)
doubleprecision,allocatable               :: double(:,:), doublei(:,:)
integer,intent(out)                       :: ierr
   if(allocated(out))deallocate(out)
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   out=cmplx(rowpack(double),rowpack(doublei),kind=dp)
end subroutine get_vector_from_lala_dpcmplx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine get_scalar_from_lala_character(varname,out,ierr)
character(len=*),intent(in)              :: varname
character(len=:),allocatable,intent(out) :: out
doubleprecision,allocatable              :: double(:,:)
integer,intent(out)                      :: ierr
integer                                  :: i,j,k
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(allocated(out))deallocate(out)
   allocate(character(len=size(double)) :: out)
   k=0
   do i=1,size(double,dim=1)
      do j=1,size(double,dim=2)
         k=k+1
         out(k:k)=achar(nint(double(i,j)))
      enddo
   enddo
end subroutine get_scalar_from_lala_character
!===================================================================================================================================
subroutine get_scalar_from_lala_int8(varname,out,ierr)
character(len=*),intent(in)    :: varname
integer(kind=int8),intent(out) :: out
doubleprecision,allocatable    :: double(:,:)
integer,intent(out)            :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=nint(double(1,1),kind=int8)
end subroutine get_scalar_from_lala_int8
!===================================================================================================================================
subroutine get_scalar_from_lala_int16(varname,out,ierr)
character(len=*),intent(in)     :: varname
integer(kind=int16),intent(out) :: out
doubleprecision,allocatable     :: double(:,:)
integer,intent(out)             :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=nint(double(1,1),kind=int16)
end subroutine get_scalar_from_lala_int16
!===================================================================================================================================
subroutine get_scalar_from_lala_int32(varname,out,ierr)
character(len=*),intent(in)     :: varname
integer(kind=int32),intent(out) :: out
doubleprecision,allocatable     :: double(:,:)
integer,intent(out)             :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=nint(double(1,1),kind=int32)
end subroutine get_scalar_from_lala_int32
!===================================================================================================================================
subroutine get_scalar_from_lala_int64(varname,out,ierr)
character(len=*),intent(in)     :: varname
integer(kind=int64),intent(out) :: out
doubleprecision,allocatable     :: double(:,:)
integer,intent(out)             :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=real(double(1,1),kind=int64)
end subroutine get_scalar_from_lala_int64
!===================================================================================================================================
subroutine get_scalar_from_lala_real32(varname,out,ierr)
character(len=*),intent(in)   :: varname
real(kind=real32),intent(out) :: out
doubleprecision,allocatable   :: double(:,:)
integer,intent(out)           :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=real(double(1,1),kind=real32)
end subroutine get_scalar_from_lala_real32
!===================================================================================================================================
subroutine get_scalar_from_lala_real64(varname,out,ierr)
character(len=*),intent(in)   :: varname
real(kind=real64),intent(out) :: out
doubleprecision,allocatable   :: double(:,:)
integer,intent(out)           :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=real(double(1,1),kind=real64)
end subroutine get_scalar_from_lala_real64
!===================================================================================================================================
subroutine get_scalar_from_lala_real128(varname,out,ierr)
character(len=*),intent(in)    :: varname
real(kind=real128),intent(out) :: out
doubleprecision,allocatable    :: double(:,:)
integer,intent(out)            :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=real(double(1,1),kind=real128)
end subroutine get_scalar_from_lala_real128
!===================================================================================================================================
subroutine get_scalar_from_lala_logical(varname,out,ierr)
character(len=*),intent(in)   :: varname
logical,intent(out)           :: out
doubleprecision,allocatable   :: double(:,:)
integer,intent(out)           :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=merge(.false.,.true.,nint(double(1,1)).eq.0)
end subroutine get_scalar_from_lala_logical
!===================================================================================================================================
subroutine get_scalar_from_lala_cmplx(varname,out,ierr)
character(len=*),intent(in)   :: varname
complex,intent(out)           :: out
doubleprecision,allocatable   :: double(:,:), doublei(:,:)
integer,intent(out)           :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=cmplx(double(1,1),doublei(1,1),kind=sp)
end subroutine get_scalar_from_lala_cmplx
!===================================================================================================================================
subroutine get_scalar_from_lala_dpcmplx(varname,out,ierr)
character(len=*),intent(in)   :: varname
complex(kind=dp),intent(out)  :: out
doubleprecision,allocatable   :: double(:,:), doublei(:,:)
integer,intent(out)           :: ierr
   call get_double_from_lala(varname,double,type=0,ierr=ierr)
   call get_double_from_lala(varname,doublei,type=1,ierr=ierr)
   if(ierr.ne.0)return
   if(size(double).ne.1)call journal('sc','warning: returned scalar does not have size 1 but size',size(double))
   out=cmplx(double(1,1),doublei(1,1),kind=dp)
end subroutine get_scalar_from_lala_dpcmplx
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function too_much_memory(expression)
integer,intent(in) :: expression
logical            :: too_much_memory

! ident_35="@(#)too much memory required"

   G_ERR=expression
   if(G_ERR.gt.0)then
      call mat_err(17)
      too_much_memory=.true.
   else
      too_much_memory=.false.
   endif

end function too_much_memory
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function system_getenv(name,default) result(value)

! ident_36="@(#)M_system::system_getenv(3f): call get_environment_variable as a function with a default value(3f)"

character(len=*),intent(in)          :: name
character(len=*),intent(in),optional :: default
integer                              :: howbig
integer                              :: stat
character(len=:),allocatable         :: value

   if(NAME.ne.'')then
      call get_environment_variable(name, length=howbig, status=stat, trim_name=.true.)  ! get length required to hold value
      if(howbig.ne.0)then
         select case (stat)
         case (1)     ! print *, NAME, " is not defined in the environment. Strange..."
            value=''
         case (2)     ! print *, "This processor doesn't support environment variables. Boooh!"
            value=''
         case default ! make string to hold value of sufficient size and get value
            if(allocated(value))deallocate(value)
            allocate(character(len=max(howbig,1)) :: VALUE)
            call get_environment_variable(name,value,status=stat,trim_name=.true.)
            if(stat.ne.0)VALUE=''
         end select
      else
         value=''
      endif
   else
      value=''
   endif
   if(value.eq.''.and.present(default))value=default

end function system_getenv
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function mat_is_name(line) result (lout)
! determine if a string is a valid Fortran name ignoring trailing spaces
! (but not leading spaces)
character(len=*),parameter   :: int='0123456789'
character(len=*),parameter   :: lower='abcdefghijklmnopqrstuvwxyz'
character(len=*),parameter   :: upper='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*),parameter   :: allowed=upper//lower//int//'_'
character(len=*),intent(in)  :: line
character(len=:),allocatable :: name
logical                      :: lout
        name=trim(line)
        if(len(name).ne.0)then
            lout = .true.                                  &
             & .and. verify(name(1:1), lower//upper) == 0  &
             & .and. verify(name,allowed) == 0             &
             & .and. len(name) <= 33
        else
            lout = .false.
        endif
end function mat_is_name
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine update(key,valin1,valin2,valin3)
character(len=*),intent(in)      :: key
integer,optional                 :: valin1,valin2,valin3
integer                          :: place
   if(present(valin1))then
      ! find where string is or should be
      call locate(keywords,key,place)
      ! if string was not found insert it
      if(place.lt.1)then
         call insert(keywords,key,iabs(place))
         call insert(rows,valin1,iabs(place))
         call insert(cols,valin2,iabs(place))
         call insert(locs,valin3,iabs(place))
      else
         call replace(rows,valin1,place)
         call replace(cols,valin2,place)
         call replace(locs,valin3,place)
      endif
   else
      call locate(keywords,key,place)
      if(place.gt.0)then
         call remove(keywords,place)
         call remove(rows,place)
         call remove(cols,place)
         call remove(locs,place)
      endif
   endif
end subroutine update

subroutine get(key,valout1,valout2,valout3)
character(len=*),intent(in)   :: key
integer                       :: valout1, valout2, valout3
integer                       :: place
! find where string is or should be
   call locate(keywords,key,place)
   if(place.lt.1)then
      valout1=-99999
      valout2=-99999
      valout3=-99999
   else
      valout1=rows(place)
      valout2=cols(place)
      valout3=locs(place)
   endif
end subroutine get
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_help_text()
G_HELP_TEXT=[ CHARACTER(LEN=128) :: &
'================================================================================',&
'LALA USERS'' GUIDE',&
'',&
'                        LALA (May, 1981 - Apr, 2021)',&
'',&
'   The Linear Algebra Fortran Facility (LALA) is a collection of Fortran',&
'   procedures that serves as a convenient tool for Fortran programs to',&
'   interact with their data (interactively or in batch mode) with a',&
'   tool that acts as a basic "laboratory" for computations involving',&
'   matrices.',&
'',&
'   It provides easy access to matrix software developed by the LINPACK',&
'   and EISPACK projects.',&
'',&
'   It is based on the Los Alamos procedure MATLAB, and owes much to',&
'   Cleve Moler, Department of Computer Science, University of New Mexico.',&
'',&
'                            CONTENTS',&
'',&
'          -  Elementary operations',&
'          -  LALA functions',&
'          -  Rows, columns and submatrices',&
'          -  "for", "while" and "if"',&
'          -  Characters, text, files and macros',&
'          -  The numerical algorithms',&
'          -  "flop" and "chop"',&
'          -  Census example',&
'          -  Partial differential equation example',&
'          -  Eigenvalue sensitivity example',&
'          -  Communicating with other programs',&
'          -  Appendix  (The HELP file)',&
'',&
'   The capabilities range from standard tasks such as solving simultaneous',&
'   linear equations and inverting matrices, through symmetric and',&
'   nonsymmetric eigenvalue problems, to fairly sophisticated matrix',&
'   tools such as the singular value decomposition.',&
'',&
'   LALA should be useful in applied linear algebra, as well as more',&
'   general numerical analysis, matrix theory, statistics and applications',&
'   of matrices to other disciplines.',&
'',&
'   LALA can serve as a "desk calculator" for the quick solution of small',&
'   problems involving matrices.',&
'',&
'   The program is written in Fortran and is designed to be readily',&
'   installed under any operating system which permits interactive',&
'   execution of Fortran programs. The resources required are fairly',&
'   modest.',&
'',&
'   The size of the matrices that can be handled in LALA depends upon the',&
'   amount of storage available on the supporting platform and the optional',&
'   word count that can be supplied on an initial call to LALA(3f).',&
'',&
'   In some ways, LALA resembles SPEAKEASY [4] and, to a lesser extent,',&
'   APL. All are interactive terminal languages that ordinarily accept',&
'   single-line commands or statements, process them immediately, and print',&
'   the results. All have arrays or matrices as principal data types. But',&
'   for LALA, the matrix is the only data type (although scalars, vectors',&
'   and text are special cases), the underlying system is portable and',&
'   requires fewer resources, and the supporting subroutines are more',&
'   powerful and in some cases, have better numerical properties.',&
'',&
'   Together, LINPACK and EISPACK provide for powerful matrix',&
'   computation. EISPACK is a package of over 70 Fortran subroutines for',&
'   various matrix eigenvalue computations that are based for the most',&
'   part on Algol procedures published by Wilkinson, Reinsch and their',&
'   colleagues [5]. LINPACK is a package of 40 Fortran subroutines (in',&
'   each of four data types) for solving and analyzing simultaneous linear',&
'   equations and related matrix problems. Since LALA is not primarily',&
'   concerned with either execution time efficiency or storage savings,',&
'   it ignores most of the special matrix properties that LINPACK and',&
'   EISPACK subroutines use to advantage. Consequently, only 8 subroutines',&
'   from LINPACK and 5 from EISPACK are actually involved.',&
'',&
'   In more advanced applications, LALA can be used in conjunction with',&
'   other programs in several ways. It is possible to define new LALA',&
'   functions and add them to the system.  it is possible to use the local',&
'   file system to pass matrices between LALA and other programs. LALA',&
'   command and statement input can be obtained from a local file instead',&
'   of from the terminal. The most power and flexibility is obtained by',&
'   using LALA as a subroutine which is called by other programs.',&
'',&
'   This document first gives an overview of LALA from the user''s',&
'   point of view. Several extended examples involving data fitting,',&
'   partial differential equations, eigenvalue sensitivity and other',&
'   topics are included.  The system was designed and programmed using',&
'   techniques described by Wirth [6], implemented in nonrecursive,',&
'   portable Fortran. There is a brief discussion of some of the matrix',&
'   algorithms and of their numerical properties. A final section describes',&
'   how LALA can be used with other programs. The appendix includes the',&
'   HELP documentation available on-line.',&
'',&
'================================================================================',&
'ELEMENTARY OPERATIONS',&
'',&
'   LALA works with essentially only one kind of object, a rectangular',&
'   matrix with complex elements. If the imaginary parts of the elements',&
'   are all zero, they are not printed, but they still occupy storage. In',&
'   some situations, special meaning is attached to 1 by 1 matrices,',&
'   that is scalars, and to 1 by n and m by 1 matrices, that is row and',&
'   column vectors.',&
'',&
'   Matrices can be introduced into LALA in four different',&
'   ways:',&
'',&
'           --  Explicit list of elements,',&
'           --  Use of "for" and "while" statements,',&
'           --  Read from an external file,',&
'           --  Execute an external Fortran program.',&
'',&
'   The explicit list is surrounded by angle brackets, ''<'' and ''>'' or',&
'   braces ''['' and '']'', and uses the semicolon '';'' to indicate the ends',&
'   of the rows. For example, the input line',&
'',&
'      A = <1 2 3; 4 5 6; 7 8 9>',&
'',&
'   will result in the output',&
'',&
'      A     =',&
'',&
'          1.    2.   3.',&
'          4.    5.   6.',&
'          7.    8.   9.',&
'',&
'   The matrix A will be saved for later use. The individual elements',&
'   are separated by commas or blanks and can be any LALA expressions,',&
'   for example',&
'',&
'      x = < -1.3, 4/5, 4*atan(1) >',&
'',&
'   results in',&
'',&
'      x     =',&
'',&
'        -1.3000   0.8000   3.1416',&
'',&
'   The elementary functions available include sqrt, log, exp, sin, cos,',&
'   atan, abs, round, real, imag, and conjg.',&
'',&
'   Large matrices can be spread across several input lines, with the',&
'   carriage returns replacing the semicolons. The above matrix could',&
'   also have been produced by',&
'',&
'      A = < 1 2 3',&
'            4 5 6',&
'            7 8 9 >',&
'',&
'   Matrices can be input from the local file system. Say a file named',&
'   ''xyz'' contains five lines of text,',&
'',&
'      A = <',&
'      1 2 3',&
'      4 5 6',&
'      7 8 9',&
'      >;',&
'',&
'   then the LALA statement exec(''xyz'') reads the matrix and assigns it',&
'   to A .',&
'',&
'   The "for" statement allows the generation of matrices whose elements',&
'   are given by simple formulas. Our example matrix A could also have',&
'   been produced by',&
'',&
'      for i = 1:3, for j = 1:3, A(i,j) = 3*(i-1)+j;',&
'',&
'   The semicolon at the end of the line suppresses the printing, which',&
'   in this case would have been nine versions of A with changing elements.',&
'',&
'   Several statements may be given on a line, separated by semicolons',&
'   or commas.',&
'',&
'   Two consecutive periods anywhere on a line indicate continuation. The',&
'   periods and any following characters are deleted, then another line',&
'   is input and concatenated onto the previous line.',&
'',&
'   Two consecutive slashes anywhere on a line cause the remainder of',&
'   the line to be ignored. This is useful for inserting comments.',&
'',&
'   Names of variables are formed by a letter, followed by any of',&
'   letters, digits and underscores, up to 63 characters in length.',&
'',&
'   The special character prime ('') is used to denote the transpose of',&
'   a matrix, so',&
'',&
'      X = X''',&
'',&
'   changes the row vector above into the column vector',&
'',&
'      X     =',&
'',&
'        -1.3000',&
'         0.8000',&
'         3.1416',&
'',&
'   Individual matrix elements may be referenced by enclosing their',&
'   subscripts in parentheses. When any element is changed, the entire',&
'   matrix is reprinted. For example, using the above matrix,',&
'',&
'      B(3,3) = B(1,3) + B(3,1)',&
'',&
'   results in',&
'',&
'      B     =',&
'',&
'          1.    2.    3.',&
'          4.    5.    6.',&
'          7.    8.   10.',&
'',&
'   Addition, subtraction and multiplication of matrices are denoted by',&
'   +, -, and * . The operations are performed whenever the matrices',&
'   have the proper dimensions. For example, with the above A and x,',&
'   the expressions A + X and X*A are incorrect because A is 3 by 3 and',&
'   X is now 3 by 1. However,',&
'',&
'      B = A*B',&
'',&
'   is correct and results in the output',&
'',&
'      B     =',&
'',&
'         9.7248',&
'        17.6496',&
'        28.7159',&
'',&
'   Note that both upper and lower case letters are allowed for input',&
'   (on those systems which have both).',&
'',&
'   There are two "matrix division" symbols in LALA, \ and / .  If A and',&
'   B are matrices, then A\B and B/A correspond formally to left and right',&
'   multiplication of B by the inverse of A, that is inv(A)*B and B*inv(A),',&
'   but the result is obtained directly without the computation of the',&
'   inverse. In the scalar case, 3\1 and 1/3 have the same value, namely',&
'   one-third. In general, A\B denotes the solution X to the equation A*X =',&
'   B and B/A denotes the solution to X*A = B.',&
'',&
'   Left division, A\B, is defined whenever B has as many rows as A. If A',&
'   is square, it is factored using Gaussian elimination. The factors are',&
'   used to solve the equations A*X(:,j) = B(:,j) where B(:,j) denotes the',&
'   j-th column of B. The result is a matrix X with the same dimensions',&
'   as B. If A is nearly singular (according to the LINPACK condition',&
'   estimator, RCOND(3f)), a warning message is printed. If A is not',&
'   square, it is factored using Householder orthogonalization with column',&
'   pivoting. The factors are used to solve the under- or overdetermined',&
'   equations in a least squares sense. The result is an M by N matrix X',&
'   where M is the number of columns of A and N is the number of columns',&
'   of B . Each column of X has at most K nonzero components, where K is',&
'   the effective rank of A .',&
'',&
'   Right division, B/A, can be defined in terms of left division by B/A =',&
'   (A''\B'')''.',&
'',&
'   For example, since our vector b was computed as A*X, the statement',&
'',&
'      Y = A\B',&
'',&
'   results in',&
'',&
'      Y     =',&
'',&
'        -1.3000',&
'         0.8000',&
'         3.1416',&
'',&
'   Of course, Y is not exactly equal to X because of the roundoff errors',&
'   involved in both A*X and A\B , but we are not printing enough digits',&
'   to see the difference. The result of the statement',&
'',&
'      E = X - Y',&
'',&
'   depends upon the particular computer being used. In one case it',&
'   produces',&
'',&
'      E     =',&
'',&
'         1.0e-15 *',&
'',&
'           .3053',&
'          -.2498',&
'           .0000',&
'',&
'   The quantity 1.0e-15 is a scale factor which multiplies all the',&
'   components which follow. Thus our vectors X and Y actually',&
'   agree to about 15 decimal places on this computer.',&
'',&
'   It is also possible to obtain element-by-element',&
'   multiplicative operations. If A and B have the same dimensions,',&
'   then A .* B denotes the matrix whose elements are simply the',&
'   products of the individual elements of A and B . The expressions',&
'   A ./ B and A .\ B give the quotients of the individual elements.',&
'',&
'   There are several possible output formats. The statement',&
'',&
'      long, X',&
'',&
'   results in',&
'',&
'      X     =',&
'',&
'         -1.300000000000000',&
'           .800000000000000',&
'          3.141592653589793',&
'',&
'   The statement',&
'',&
'      short',&
'',&
'   restores the original format.',&
'',&
'   The expression A**p means A to the p-th power. It is',&
'   defined if A is a square matrix and p is a scalar. If p is an',&
'   integer greater than one, the power is computed by repeated',&
'   multiplication. For other values of p the calculation involves',&
'   the eigenvalues and eigenvectors of A.',&
'',&
'   Previously defined matrices and matrix expressions can be',&
'   used inside brackets to generate larger matrices, for example',&
'',&
'      C = <A, B; <4 2 0>*X, X''>',&
'',&
'   results in',&
'',&
'      C     =',&
'',&
'         1.0000   2.0000   3.0000   9.7248',&
'         4.0000   5.0000   6.0000  17.6496',&
'         7.0000   8.0000  10.0000  28.7159',&
'        -3.6000  -1.3000   0.8000   3.1416',&
'',&
'   There are four predefined variables, "eps", "flop", "rand" and',&
'   "eye". The variable "eps" is used as a tolerance is determining such',&
'   things as near singularity and rank. Its initial value is the distance',&
'   from 1.0 to the next largest floating point number on the particular',&
'   computer being used. The user may reset this to any other value,',&
'   including zero. "eps" is changed by "chop", which is described later',&
'   in this manual.',&
'',&
'   The value of "rand" is a random variable, with a choice of a uniform',&
'   or a normal distribution.',&
'',&
'   The name "eye" is used in place of "i" to denote identity matrices',&
'   because "i" is often used as a subscript or as sqrt(-1).  The dimensions',&
'   of "eye" are determined by context. For example,',&
'',&
'      B = A + 3*eye',&
'',&
'   adds 3 to the diagonal elements of A and',&
'',&
'      X = eye/A',&
'',&
'   is one of several ways in LALA to invert a matrix.',&
'',&
'   "flop" provides a measure of the number of floating point operations,',&
'   or "flops", required for each calculation by reporting the CPU time',&
'   consumed.',&
'',&
'   A statement may consist of an expression alone, in which case a',&
'   variable named "ans" is created and the result stored in "ans" for',&
'   possible future use. Thus',&
'',&
'      A\A - eye',&
'',&
'   is the same as',&
'',&
'      ans = A\A - eye',&
'',&
'   (Roundoff error usually causes this result to be a matrix of "small"',&
'   numbers, rather than all zeros.)',&
'',&
'   All computations are done using double precision real arithmetic. There',&
'   is no mixed-precision arithmetic.  The Fortran COMPLEX data type',&
'   is not used because many systems create unnecessary underflows and',&
'   overflows with complex operations.',&
'',&
'================================================================================',&
'FUNCTIONS',&
'',&
'   Much of LALA''s computational power comes from the various',&
'   matrix functions available. The current list includes:',&
'',&
'      inv(A)          - Inverse.',&
'      det(A)          - Determinant.',&
'      cond(A)         - Condition number.',&
'      rcond(A)        - A measure of nearness to singularity.',&
'      eig(A)          - Eigenvalues and eigenvectors.',&
'      schur(A)        - Schur triangular form.',&
'      hess(A)         - Hessenberg or tridiagonal form.',&
'      poly(A)         - Characteristic polynomial.',&
'      svd(A)          - Singular value decomposition.',&
'      pinv(A,eps)     - Pseudo-inverse with optional tolerance.',&
'      rank(A,eps)     - Matrix rank with optional tolerance.',&
'      lu(A)           - Factors from Gaussian elimination.',&
'      chol(A)         - Factor from Cholesky factorization.',&
'      qr(A)           - Factors from Householder orthogonalization.',&
'      rref(A)         - Reduced row echelon form.',&
'      orth(A)         - Orthogonal vectors spanning range of A.',&
'      exp(A)          - e to the A.',&
'      log(A)          - Natural logarithm.',&
'      sqrt(A)         - Square root.',&
'      sin(A)          - Trigonometric sine.',&
'      cos(A)          - Cosine.',&
'      atan(A)         - Arctangent.',&
'      round(A)        - Round the elements to nearest integers.',&
'      abs(A)          - Absolute value of the elements.',&
'      real(A)         - Real parts of the elements.',&
'      imag(A)         - Imaginary parts of the elements.',&
'      conjg(A)        - Complex conjugate.',&
'      sum(A)          - Sum of the elements.',&
'      prod(A)         - Product of the elements.',&
'      diag(A)         - Extract or create diagonal matrices.',&
'      tril(A)         - Lower triangular part of A.',&
'      triu(A)         - Upper triangular part of A.',&
'      norm(A,p)       - Norm with p = 1, 2 or ''Infinity''.',&
'      eye(m,n)        - Portion of identity matrix.',&
'      rand(m,n)       - Matrix with random elements.',&
'      ones(m,n)       - Matrix of all ones.',&
'      magic(n)        - Interesting test matrices.',&
'      invh(n)         - Inverse Hilbert matrices.',&
'      roots(C)        - Roots of polynomial with coefficients C.',&
'      display(A,p)    - Print base p representation of A.',&
'      kron(A,B)       - Kronecker tensor product of A and B.',&
'      plot(X,Y)       - Plot Y as a function of X .',&
'      rat(A)          - Find "simple" rational approximation to A.',&
'      user(A)         - Function defined by external program.',&
'',&
'   Some of these functions have different interpretations when the',&
'   argument is a matrix or a vector and some of them have additional',&
'   optional arguments. Details are given in the HELP document in the',&
'   appendix.',&
'',&
'   Several of these functions can be used in a generalized assignment',&
'   statement with two or three variables on the left hand side. For',&
'   example',&
'',&
'      <X,D> = eig(A)',&
'',&
'   stores the eigenvectors of A in the matrix X and a diagonal matrix',&
'   containing the eigenvalues in the matrix D. The statement',&
'',&
'      eig(A)',&
'',&
'   simply computes the eigenvalues and stores them in "ans".',&
'',&
'   Future versions of LALA will probably include additional functions,',&
'   since they can easily be added to the system.',&
'',&
'================================================================================',&
'ROWS COLUMNS AND SUBMATRICES',&
'',&
'   Individual elements of a matrix can be accessed by giving their',&
'   subscripts in parentheses, eg. A(1,2), x(i), TAB(ind(k)+1).',&
'   An expression used as a subscript is rounded to the nearest integer.',&
'',&
'   Individual rows and columns can be accessed using a colon '':'' (or a',&
'   ''|'') for the free subscript. For example, A(1,:) is the first row of',&
'   A and A(:,j) is the j-th column. Thus',&
'',&
'      A(i,:) = A(i,:) + c*A(k,:)',&
'',&
'   adds c times the k-th row of A to the i-th row.',&
'',&
'   The colon is used in several other ways in LALA, but all of the uses',&
'   are based on the following definition.',&
'',&
'      j:k    is the same as  <j, j+1, ..., k>',&
'      j:k    is empty if  j > k .',&
'      j:i:k  is the same as  <j, j+i, j+2i, ..., k>',&
'      j:i:k  is empty if  i > 0 and j > k or if i < 0 and j < k .',&
'',&
'   The colon is usually used with integers, but it is possible to',&
'   use arbitrary real scalars as well. Thus',&
'',&
'      1:4  is the same as  <1, 2, 3, 4>',&
'      0: 0.1: 0.5 is the same as <0.0, 0.1, 0.2, 0.3, 0.4, 0.5>',&
'',&
'   In general, a subscript can be a vector. If X and V are vectors,',&
'   then X(V) is <X(V(1)), X(V(2)), ..., X(V(n))> . This can also be',&
'   used with matrices. If V has m components and W has n components,',&
'   then A(V,W) is the m by n matrix formed from the elements of A whose',&
'   subscripts are the elements of V and W.  Combinations of the colon',&
'   notation and the indirect subscripting allow manipulation of various',&
'   submatrices. For example,',&
'',&
'      A(<1,5>,:) = A(<5,1>,:)  interchanges rows 1 and 5 of A.',&
'      A(2:k,1:n)  is the submatrix formed from rows 2 through k',&
'         and columns 1 through n of A .',&
'      A(:,<3 1 2>)  is a permutation of the first three columns.',&
'',&
'   The notation A(:) has a special meaning. On the right hand side of an',&
'   assignment statement, it denotes all the elements of A, regarded as',&
'   a single column. When an expression is assigned to A(:), the current',&
'   dimensions of A, rather than of the expression, are used.',&
'',&
'================================================================================',&
'FOR WHILE AND IF',&
'',&
'   The "for" clause allows statements to be repeated a specific',&
'   number of times. The general form is',&
'',&
'      for variable = expr, statement, ..., statement, end',&
'',&
'   The "end" and the comma before it may be omitted. In general, the',&
'   expression may be a matrix, in which case the columns are stored one',&
'   at a time in the variable and the following statements, up to the',&
'   "end" or the end of the line, are executed. The expression is often',&
'   of the form j:k, and its "columns" are simply the scalars from j to',&
'   k. Some examples (assume n has already been assigned a value):',&
'',&
'      for i = 1:n, for j = 1:n, A(i,j) = 1/(i+j-1);',&
'',&
'   generates the Hilbert matrix.',&
'',&
'      for j = 2:n-1, for i = j:n-1, ...',&
'         A(i,j) = 0; end; A(j,j) = j; end; A',&
'',&
'   changes all but the "outer edge" of the lower triangle and then',&
'   prints the final matrix.',&
'',&
'      for h = 1.0: -0.1: -1.0, (<h, cos(pi*h)>)',&
'',&
'   prints a table of cosines.',&
'',&
'      <X,D> = eig(A); for v = X, v, A*v',&
'',&
'   displays eigenvectors, one at a time.',&
'',&
'        The "while" clause allows statements to be repeated an',&
'   indefinite number of times. The general form is',&
'',&
'      while expr relop expr,   statement,..., statement, end',&
'',&
'   where relop is =, <, >, <=, >=, or <> (not equal). The statements are',&
'   repeatedly executed as long as the indicated comparison between the',&
'   real parts of the first components of the two expressions is true. Here',&
'   are two examples. (Exercise for the reader: What do these segments do?)',&
'',&
'      eps = 1;',&
'      while 1 + eps > 1, eps = eps/2;',&
'      eps = 2*eps',&
'',&
'      E = 0*A;  F = E + eye; n = 1;',&
'      while norm(E+F-E,1) > 0, E = E + F; F = A*F/n; n = n + 1;',&
'      E',&
'',&
'   The IF clause allows conditional execution of statements.  The general',&
'   form is',&
'',&
'      if expr relop expr,  statement, ..., statement,',&
'         else statement, ..., statement',&
'',&
'   The first group of statements are executed if the relation is true and',&
'   the second group are executed if the relation is false.  The "else"',&
'   and the statements following it may be omitted. For example,',&
'',&
'      if abs(i-j) = 2, A(i,j) = 0;',&
'',&
'================================================================================',&
'CHARACTERS AND TEXTFILES AND MACROS',&
'',&
'   LALA has several commands which control the output format and the',&
'   overall execution of the system.',&
'',&
'   The "help" command allows on-line access to short portions of text',&
'   describing various operations, functions and special characters. The',&
'   entire "help" document is reproduced in an appendix.',&
'',&
'   Results are usually printed in a scaled fixed point format that shows',&
'   4 or 5 significant figures. The commands "short", "long", "short e",',&
'   "long e" and "long z" alter the output format, but do not alter the',&
'   precision of the computations or the internal storage.',&
'',&
'   The "who" command provides information about the functions and',&
'   variables that are currently defined.',&
'',&
'   The "clear" command erases all variables, except "eps", "flop",',&
'   "rand" and "eye". The statement A = <> indicates that a "0 by 0"',&
'   matrix is to be stored in A. This causes A to be erased so that its',&
'   storage can be used for other variables.',&
'',&
'   The "quit" and "exit" commands cause return to the underlying operating',&
'   system through the Fortran RETURN statement.',&
'',&
'   LALA has a limited facility for handling text. Any string of characters',&
'   delineated by quotes (with two quotes used to allow one quote within',&
'   the string) is saved as a vector of integer values that are the ADE',&
'   (Ascii Decimal Equivalent) value of the character, with special',&
'   equivalencing of the characters {}[]" into ()<>'' in expressions. It',&
'   is important to know you use those characters as part of an expression',&
'   or command without treating them as equivalent outside of strings.',&
'',&
'   (The complete list is in the appendix under "CHAR".) For example',&
'',&
'      ''2*A + 3''  is the same as  < 50 42 65 32 43 32 51 >.',&
'',&
'   It is possible, though seldom very meaningful, to use such',&
'   strings in matrix operations. More frequently, the text is used',&
'   as a special argument to various functions.',&
'',&
'      norm(A,''inf'')    computes the infinity norm of A .',&
'      display(T)       prints the text stored in T .',&
'      exec(''file'')     obtains LALA input from an external file.',&
'      save(''file'')     stores all the current variables in a file.',&
'      load(''file'')     retrieves all the variables from a file.',&
'      print(''file'',X)  prints X on a file.',&
'      diary(''file'')    makes a copy of the complete LALA session.',&
'',&
'   The text can also be used in a limited string substitution',&
'   macro facility. If a variable, say T, contains the source text',&
'   for a LALA statement or expression, then the construction',&
'',&
'      > T <',&
'',&
'   causes T to be executed or evaluated. For example',&
'',&
'      T = ''2*A + 3'';',&
'      S = ''B = >T< + 5''',&
'      A = 4;',&
'      > S <',&
'',&
'   produces',&
'',&
'      B     =',&
'',&
'         16.',&
'',&
'   Some other examples are given under MACROS in the appendix. This',&
'   facility is useful for fairly short statements and expressions.',&
'   More complicated LALA "programs" should use the "exec" facility.',&
'',&
'================================================================================',&
'NUMERICAL ALGORITHMS',&
'',&
'   The algorithms underlying the basic LALA functions are described in',&
'   the LINPACK and EISPACK guides [1-3]. The following list gives the',&
'   subroutines used by these functions.',&
'',&
'      inv(A)          - CGECO,CGEDI',&
'      det(A)          - CGECO,CGEDI',&
'      lu(A)           - CGEFA',&
'      rcond(A)        - CGECO',&
'      chol(A)         - CPOFA',&
'      svd(A)          - CSVDC',&
'      cond(A)         - CSVDC',&
'      norm(A,2)       - CSVDC',&
'      pinv(A,eps)     - CSVDC',&
'      rank(A,eps)     - CSVDC',&
'      qr(A)           - CQRDC,CQRSL',&
'      orth(A)         - CQRDC,CSQSL',&
'      A\B and B/A     - CGECO,CGESL if A is square.',&
'                      - CQRDC,CQRSL if A is not square.',&
'      eig(A)          - HTRIDI,IMTQL2,HTRIBK if A is Hermitian.',&
'                      - CORTH,COMQR2         if A is not Hermitian.',&
'      schur(A)        - same as EIG.',&
'      hess(A)         - same as EIG.',&
'',&
'   Minor modifications were made to all these subroutines. The LINPACK',&
'   routines were changed to replace the Fortran complex arithmetic',&
'   with explicit references to real and imaginary parts.  Since most',&
'   of the floating point arithmetic is concentrated in a few low-level',&
'   subroutines which perform vector operations (the Basic Linear Algebra',&
'   Subprograms), this was not an extensive change. It also facilitated',&
'   implementation of the "flop" and "chop" features which count and',&
'   optionally truncate each floating point operation.',&
'',&
'   The EISPACK subroutine COMQR2 was modified to allow access to the',&
'   Schur triangular form, ordinarily just an intermediate result. IMTQL2',&
'   was modified to make computation of the eigenvectors optional. Both',&
'   subroutines were modified to eliminate the machine-dependent accuracy',&
'   parameter and all the EISPACK subroutines were changed to include',&
'   "flop" and "chop".',&
'',&
'   The algorithms employed for the "poly" and "roots" functions',&
'   illustrate an interesting aspect of the modern approach to eigenvalue',&
'   computation. "poly(A)" generates the characteristic polynomial of',&
'   A and "roots(poly(A))" finds the roots of that polynomial, which',&
'   are, of course, the eigenvalues of A . But both "poly" and "roots"',&
'   use EISPACK eigenvalues subroutines, which are based on similarity',&
'   transformations. So the classical approach which characterizes',&
'   eigenvalues as roots of the characteristic polynomial is actually',&
'   reversed.',&
'',&
'   If A is an n by n matrix, "poly(A)" produces the coefficients C(1)',&
'   through C(n+1), with C(1) = 1, in',&
'',&
'         det(z*eye-A) = C(1)*z**n + ... + C(n)*z + C(n+1) .',&
'',&
'   The algorithm can be expressed compactly using LALA:',&
'',&
'         Z = eig(A);',&
'         C = 0*ones(n+1,1);  C(1) = 1;',&
'         for j = 1:n, C(2:j+1) = C(2:j+1) - Z(j)*C(1:j);',&
'         C',&
'',&
'   This recursion is easily derived by expanding the product',&
'',&
'         (z - z(1))*(z - z(2))* ... * (z-z(n)) .',&
'',&
'   It is possible to prove that "poly(A)" produces the coefficients in',&
'   the characteristic polynomial of a matrix within roundoff error of',&
'   A. This is true even if the eigenvalues of A are badly conditioned. The',&
'   traditional algorithms for obtaining the characteristic polynomial',&
'   which do not use the eigenvalues do not have such satisfactory',&
'   numerical properties.',&
'',&
'   If C is a vector with n+1 components, "roots(C)" finds the roots of',&
'   the polynomial of degree n ,',&
'',&
'          p(z) = C(1)*z**n + ... + C(n)*z + C(n+1) .',&
'',&
'   The algorithm simply involves computing the eigenvalues of the',&
'   companion matrix:',&
'',&
'         A = 0*ones(n,n)',&
'         for j = 1:n, A(1,j) = -C(j+1)/C(1);',&
'         for i = 2:n, A(i,i-1) = 1;',&
'         eig(A)',&
'',&
'   It is possible to prove that the results produced are the exact',&
'   eigenvalues of a matrix within roundoff error of the companion matrix',&
'   A, but this does not mean that they are the exact roots of a polynomial',&
'   with coefficients within roundoff error of those in C . There are',&
'   more accurate, more efficient methods for finding polynomial roots,',&
'   but this approach has the crucial advantage that it does not require',&
'   very much additional code.',&
'',&
'   The elementary functions "exp", "log", "sqrt", "sin", "cos" and "atan"',&
'   are applied to square matrices by diagonalizing the matrix, applying',&
'   the functions to the individual eigenvalues and then transforming',&
'   back. For example, "exp(A)" is computed by',&
'',&
'         <X,D> = eig(A);',&
'         for j = 1:n, D(j,j) = exp(D(j,j));',&
'         X*D/X',&
'',&
'   This is essentially method number 14 out of the 19 ''dubious''',&
'   possibilities described in [8]. It is dubious because it doesn''t always',&
'   work. The matrix of eigenvectors X can be arbitrarily badly conditioned',&
'   and all accuracy lost in the computation of X*D/X. A warning message',&
'   is printed if "rcond(X)" is very small, but this only catches the',&
'   extreme cases. An example of a case not detected is when A has a double',&
'   eigenvalue, but theoretically only one linearly independent eigenvector',&
'   associated with it.  The computed eigenvalues will be separated by',&
'   something on the order of the square root of the roundoff level. This',&
'   separation will be reflected in "rcond(X)" which will probably not',&
'   be small enough to trigger the error message. The computed "exp(A)"',&
'   will be accurate to only half precision. Better methods are known for',&
'   computing "exp(A)", but they do not easily extend to the other five',&
'   functions and would require a considerable amount of additional code.',&
'',&
'   The expression A**p is evaluated by repeated multiplication if p is',&
'   an integer greater than 1. Otherwise it is evaluated by',&
'',&
'         <X,D> = eig(A);',&
'         for j = 1:n, D(j,j) = exp(p*log(D(j,j)))',&
'         X*D/X',&
'',&
'   This suffers from the same potential loss of accuracy if X is',&
'   badly conditioned. It was partly for this reason that the case p =',&
'   1 is included in the general case. Comparison of A**1 with A gives',&
'   some idea of the loss of accuracy for other values of p and for the',&
'   elementary functions.',&
'',&
'   "rref", the reduced row echelon form, is of some interest in',&
'   theoretical linear algebra, although it has little computational',&
'   value. It is included in LALA for pedagogical reasons. The algorithm',&
'   is essentially Gauss-Jordan elimination with detection of negligible',&
'   columns applied to rectangular matrices.',&
'',&
'   There are three separate places in LALA where the rank of a matrix',&
'   is implicitly computed: in rref(A), in A\B for non-square A, and',&
'   in the pseudoinverse pinv(A). Three different algorithms with three',&
'   different criteria for negligibility are used and so it is possible',&
'   that three different values could be produced for the same matrix. With',&
'   rref(A), the rank of A is the number of nonzero rows. The elimination',&
'   algorithm used for "rref" is the fastest of the three rank-determining',&
'   algorithms, but it is the least sophisticated numerically and the',&
'   least reliable.  With A\B, the algorithm is essentially that used',&
'   by example subroutine SQRST in chapter 9 of the LINPACK guide. With',&
'   pinv(A), the algorithm is based on the singular value decomposition',&
'   and is described in chapter 11 of the LINPACK guide. The SVD algorithm',&
'   is the most time-consuming, but the most reliable and is therefore',&
'   also used for rank(A).',&
'',&
'   The uniformly distributed random numbers in "rand" are obtained from',&
'   the machine-independent random number generator URAND described in',&
'   [9]. It is possible to switch to normally distributed random numbers,',&
'   which are obtained using a transformation also described in [9].',&
'',&
'        The computation of',&
'',&
'                   2    2',&
'             sqrt(a  + b )',&
'',&
'   is required in many matrix algorithms, particularly those involving',&
'   complex arithmetic. A new approach to carrying out this operation is',&
'   described by Moler and Morrison [10]. It is a cubically convergent',&
'   algorithm which starts with a and b , rather than with their squares,',&
'   and thereby avoids destructive arithmetic underflows and overflows. In',&
'   LALA, the algorithm is used for complex modulus, Euclidean vector',&
'   norm, plane rotations, and the shift calculation in the eigenvalue',&
'   and singular value iterations.',&
'',&
'================================================================================',&
'FLOP AND CHOP',&
'',&
'   Detailed information about the amount of work involved in matrix',&
'   calculations and the resulting accuracy is provided by "flop" and',&
'   "chop". The basic unit of work is the "flop", or floating point',&
'   operation. Roughly, one flop is one execution of a Fortran statement',&
'   like',&
'',&
'         S = S + X(I)*Y(I)',&
'',&
'   or',&
'',&
'         Y(I) = Y(I) + T*X(I)',&
'',&
'   In other words, it consists of one floating point multiplication,',&
'   together with one floating point addition and the associated',&
'   indexing and storage reference operations.',&
'',&
'   LALA will print the CPU time required for a particular',&
'   statement when the statement is terminated by an extra comma. For',&
'   example, the line',&
'',&
'         n = 20;  rand(n)*rand(n);,',&
'',&
'   ends with an extra comma. Two 20 by 20 random matrices are generated',&
'   and multiplied together. The result is assigned to "ans", but the',&
'   semicolon suppresses its printing. The only output is',&
'',&
'           8800 flops',&
'',&
'   This is n**3 + 2*n**2 flops, n**2 for each random matrix and n**3',&
'   for the product.',&
'',&
'   "flop" is a predefined vector with two components. "flop(1)" is',&
'   the number of flops used by the most recently executed statement,',&
'   except that statements with zero flops are ignored. For example,',&
'   after executing the previous statement,',&
'',&
'         flop(1)/n**3',&
'',&
'   results in',&
'',&
'         ans   =',&
'',&
'             1.1000',&
'',&
'   "flop(2)" is the cumulative total of all the flops used since',&
'   the beginning of the LALA session. The statement',&
'',&
'         flop = <0 0>',&
'',&
'   resets the total.',&
'',&
'   There are several difficulties associated with keeping a',&
'   precise count of floating point operations.',&
'',&
'   As the program generally uses complex values but only performs',&
'   operations on the real matrices in many cases where all the imaginary',&
'   values are zero it may not provide an accurate measure of the relative',&
'   costs of real and complex arithmetic.',&
'',&
'   The result of each floating point operation may also be "chopped"',&
'   to simulate a computer with a shorter word length. The details',&
'   of this chopping operation depend upon the format of the floating',&
'   point word. Usually, the fraction in the floating point word can be',&
'   regarded as consisting of several octal or hexadecimal digits. The',&
'   least significant of these digits can be set to zero by a logical',&
'   masking operation. Thus the statement',&
'',&
'         chop(p)',&
'',&
'   causes the p least significant octal or hexadecimal digits in',&
'   the result of each floating point operation to be set to zero.',&
'   For example, if the computer being used has an IBM 360 long floating',&
'   point word with 14 hexadecimal digits in the fraction, then "chop(8)"',&
'   results in simulation of a computer with only 6 hexadecimal digits',&
'   in the fraction, i.e. a short floating point word. On a computer such',&
'   as the CDC 6600 with 16 octal digits, "chop(8)" results in about the',&
'   same accuracy because the remaining 8 octal digits represent the same',&
'   number of bits as 6 hexadecimal digits.',&
'',&
'   Some idea of the effect of "chop" on any particular system can',&
'   be obtained by executing the following statements.',&
'',&
'         long,   t = 1/10',&
'         long z, t = 1/10',&
'         chop(8)',&
'         long,   t = 1/10',&
'         long z, t = 1/10',&
'',&
'   The following Fortran subprograms illustrate more details of',&
'   "flop" and "chop". The first subprogram is a simplified example of a',&
'   system-dependent function used within LALA itself. The common variable',&
'   G_FLOP_COUNTER is essentially the first component of the variable',&
'   FLOP. The common variable CHP is initially zero, but it is set to p',&
'   by the statement "chop(p)". To shorten the DATA statement, we assume',&
'   there are only 6 hexadecimal digits. We also assume an extension of',&
'   Fortran that allows .AND. to be used as a binary operation between',&
'   two real variables.',&
'',&
'         REAL FUNCTION FLOP(X)',&
'         REAL X',&
'         INTEGER G_FLOP_COUNTER,CHP',&
'         COMMON G_FLOP_COUNTER,CHP',&
'         REAL MASK(5)',&
'         DATA MASK/ZFFFFFFF0,ZFFFFFF00,ZFFFFF000,ZFFFF0000,ZFFF00000/',&
'         G_FLOP_COUNTER = G_FLOP_COUNTER + 1',&
'         IF (CHP .EQ. 0) FLOP = X',&
'         IF (CHP .GE. 1 .AND. CHP .LE. 5) FLOP = X .AND. MASK(CHP)',&
'         IF (CHP .GE. 6) FLOP = 0.0',&
'         END REAL FUNCTION FLOP',&
'',&
'   The following subroutine illustrates a typical use of the',&
'   previous function within LALA. It is a simplified version of',&
'   the Basic Linear Algebra Subprogram that adds a scalar multiple',&
'   of one vector to another. We assume here that the vectors are',&
'   stored with a memory increment of one.',&
'',&
'         SUBROUTINE SAXPY(N,TR,TI,XR,XI,YR,YI)',&
'         REAL TR,TI,XR(N),XI(N),YR(N),YI(N),FLOP',&
'         IF (N .LE. 0) return',&
'         IF (TR .EQ. 0.0 .AND. TI .EQ. 0.0) return',&
'         DO I = 1, N',&
'            YR(I) = FLOP(YR(I) + TR*XR(I) - TI*XI(I))',&
'            YI(I) = YI(I) + TR*XI(I) + TI*XR(I)',&
'            IF (YI(I) .NE. 0.0D0) YI(I) = FLOP(YI(I))',&
'         enddo',&
'         END SUBROUTINE SAXPY',&
'',&
'   The saxpy operation is perhaps the most fundamental',&
'   operation within LINPACK. It is used in the computation of the',&
'   LU, the QR and the SVD factorizations, and in several other',&
'   places. We see that adding a multiple of one vector with n',&
'   components to another uses n flops if the vectors are real and',&
'   between n and 2*n flops if the vectors have nonzero imaginary',&
'   components.',&
'',&
'   The permanent LALA variable "eps" is reset by the statement',&
'   CHOP(p). Its new value is usually the smallest inverse power of',&
'   two that satisfies the Fortran logical test',&
'',&
'               FLOP(1.0+eps) .GT. 1.0',&
'',&
'   However, if "eps" had been directly reset to a larger value, the',&
'   old value is retained.',&
'',&
'================================================================================',&
'CENSUS EXAMPLE',&
'',&
'   Our first extended example involves predicting the population of the',&
'   United States in 1980 using extrapolation of various fits to the',&
'   census data from 1900 through 1970. There are eight observations,',&
'   so we begin with the LALA statement',&
'',&
'      n = 8',&
'',&
'   The values of the dependent variable, the population in millions,',&
'   can be entered with',&
'',&
'      y = < 75.995   91.972  105.711  123.203   ...',&
'           131.669  150.697  179.323  203.212>''',&
'',&
'   In order to produce a reasonably scaled matrix, the independent',&
'   variable, time, is transformed from the interval [1900,1970] to',&
'   [-1.00,0.75]. This can be accomplished directly with',&
'',&
'      t = -1.0:0.25:0.75',&
'',&
'   or in a fancier, but perhaps clearer, way with',&
'',&
'      t = 1900:10:1970;   t = (t - 1940*ones(t))/40',&
'',&
'   Either of these is equivalent to',&
'',&
'      t = <-1 -.75 -.50 -.25 0 .25 .50 .75>',&
'',&
'   The interpolating polynomial of degree n-1 involves an Vandermonde',&
'   matrix of order n with elements that might be generated by',&
'',&
'      for i = 1:n, for j = 1:n, a(i,j) = t(i)**(j-1);',&
'',&
'   However, this results in an error caused by 0**0 when i = 5 and',&
'   j = 1 . The preferable approach is',&
'',&
'      A = ones(n,n);',&
'      for i = 1:n, for j = 2:n, a(i,j) = t(i)*a(i,j-1);',&
'',&
'   Now the statement',&
'',&
'      cond(A)',&
'',&
'   produces the output',&
'',&
'      ans  =',&
'',&
'         1.1819E+03',&
'',&
'   which indicates that transformation of the time variable has resulted',&
'   in a reasonably well conditioned matrix.',&
'',&
'        The statement',&
'',&
'      c = A\y',&
'',&
'   results in',&
'',&
'      C     =',&
'',&
'        131.6690',&
'         41.0406',&
'        103.5396',&
'        262.4535',&
'       -326.0658',&
'       -662.0814',&
'        341.9022',&
'        533.6373',&
'',&
'   These are the coefficients in the interpolating polynomial',&
'',&
'         n-1',&
'',&
'         c  + c t + ... + c t',&
'          1    2           n',&
'',&
'   Our transformation of the time variable has resulted in t = 1',&
'   corresponding to the year 1980. Consequently, the extrapolated',&
'   population is simply the sum of the coefficients. This can be',&
'   computed by',&
'',&
'      p = sum(c)',&
'',&
'   The result is',&
'',&
'      P     =',&
'',&
'        426.0950',&
'',&
'   which indicates a 1980 population of over 426 million. Clearly, using',&
'   the seventh degree interpolating polynomial to extrapolate even a',&
'   fairly short distance beyond the end of the data interval is not a',&
'   good idea.',&
'',&
'   The coefficients in least squares fits by polynomials of lower degree',&
'   can be computed using fewer than n columns of the matrix.',&
'',&
'      for k = 1:n, c = A(:,1:k)\y,  p = sum(c)',&
'',&
'   would produce the coefficients of these fits, as well as the',&
'   resulting extrapolated population. If we do not want to print all the',&
'   coefficients, we can simply generate a small table of populations',&
'   predicted by polynomials of degrees zero through seven. We also',&
'   compute the maximum deviation between the fitted and observed values.',&
'',&
'      for k = 1:n, X = A(:,1:k);  c = X\y;  ...',&
'         d(k) = k-1;  p(k) = sum(c);  e(k) = norm(X*c-y,''inf'');',&
'      <d, p, e>',&
'',&
'   The resulting output is',&
'',&
'         0   132.7227  70.4892',&
'         1   211.5101   9.8079',&
'         2   227.7744   5.0354',&
'         3   241.9574   3.8941',&
'         4   234.2814   4.0643',&
'         5   189.7310   2.5066',&
'         6   118.3025   1.6741',&
'         7   426.0950   0.0000',&
'',&
'   The zeroth degree fit, 132.7 million, is the result of fitting a',&
'   constant to the data and is simply the average. The results obtained',&
'   with polynomials of degree one through four all appear reasonable. The',&
'   maximum deviation of the degree four fit is slightly greater than the',&
'   degree three, even though the sum of the squares of the deviations',&
'   is less. The coefficients of the highest powers in the fits of degree',&
'   five and six turn out to be negative and the predicted populations of',&
'   less than 200 million are probably unrealistic. The hopefully absurd',&
'   prediction of the interpolating polynomial concludes the table.',&
'',&
'   We wish to emphasize that roundoff errors are not significant',&
'   here. Nearly identical results would be obtained on other computers,',&
'   or with other algorithms. The results simply indicate the difficulties',&
'   associated with extrapolation of polynomial fits of even modest degree.',&
'',&
'   A stabilized fit by a seventh degree polynomial can be obtained using',&
'   the pseudoinverse, but it requires a fairly delicate choice of a',&
'   tolerance. The statement',&
'',&
'      s = svd(A)',&
'',&
'   produces the singular values',&
'',&
'      S     =',&
'',&
'         3.4594',&
'         2.2121',&
'         1.0915',&
'         0.4879',&
'         0.1759',&
'         0.0617',&
'         0.0134',&
'         0.0029',&
'',&
'   We see that the last three singular values are less than 0.1 ,',&
'   consequently, A can be approximately by a matrix of rank five with an',&
'   error less than 0.1 . The Moore-Penrose pseudoinverse of this rank',&
'   five matrix is obtained from the singular value decomposition with',&
'   the following statements',&
'',&
'      c = pinv(A,0.1)*y, p = sum(c), e = norm(a*c-y,''inf'')',&
'',&
'   The output is',&
'',&
'      C     =',&
'',&
'       134.7972',&
'        67.5055',&
'        23.5523',&
'         9.2834',&
'         3.0174',&
'         2.6503',&
'        -2.8808',&
'         3.2467',&
'',&
'      P     =',&
'',&
'       241.1720',&
'',&
'      E     =',&
'',&
'         3.9469',&
'',&
'   The resulting seventh degree polynomial has coefficients which are much',&
'   smaller than those of the interpolating polynomial given earlier. The',&
'   predicted population and the maximum deviation are reasonable. Any',&
'   choice of the tolerance between the fifth and sixth singular values',&
'   would produce the same results, but choices outside this range result',&
'   in pseudoinverses of different rank and do not work as well.',&
'',&
'   The one term exponential approximation',&
'',&
'        y(t) = k exp(pt)',&
'',&
'   can be transformed into a linear approximation by taking logarithms.',&
'',&
'        log(y(t)) = log k + pt',&
'',&
'                  = c  + c t',&
'                     1    2',&
'',&
'   The following segment makes use of the fact that a function of a',&
'   vector is the function applied to the individual components.',&
'',&
'      X = A(:,1:2);',&
'      c = X\log(y)',&
'      p = exp(sum(c))',&
'      e = norm(exp(X*c)-y,''inf'')',&
'',&
'   The resulting output is',&
'',&
'      C     =',&
'',&
'         4.9083',&
'         0.5407',&
'',&
'      P     =',&
'',&
'       232.5134',&
'',&
'      E     =',&
'',&
'         4.9141',&
'',&
'   The predicted population and maximum deviation appear satisfactory and',&
'   indicate that the exponential model is a reasonable one to consider.',&
'',&
'   As a curiosity, we return to the degree six polynomial.  Since the',&
'   coefficient of the high order term is negative and the value of the',&
'   polynomial at t = 1 is positive, it must have a root at some value',&
'   of t greater than one. The statements',&
'',&
'      X = A(:,1:7);',&
'      c = X\y;',&
'      c = c(7:-1:1);  //reverse the order of the coefficients',&
'      z = roots(c)',&
'',&
'   produce',&
'',&
'      Z     =',&
'',&
'         1.1023-  0.0000*i',&
'         0.3021+  0.7293*i',&
'        -0.8790+  0.6536*i',&
'        -1.2939-  0.0000*i',&
'        -0.8790-  0.6536*i',&
'         0.3021-  0.7293*i',&
'',&
'   There is only one real, positive root. The corresponding time on the',&
'   original scale is',&
'',&
'      1940 + 40*real(z(1))',&
'',&
'        =  1984.091',&
'',&
'   We conclude that the United States population should become zero',&
'   early in February of 1984.',&
'',&
'================================================================================',&
'PARTIAL DIFFERENTIAL EQUATION EXAMPLE',&
'',&
'   Our second extended example is a boundary value problem for Laplace''s',&
'   equation. The underlying physical problem involves the conductivity',&
'   of a medium with cylindrical inclusions and is considered by Keller',&
'   and Sachs [7].',&
'',&
'        Find a function  u(x,y)  satisfying Laplace''s equation',&
'',&
'                  u   + u   = 0',&
'                   xx    yy',&
'',&
'   The domain is a unit square with a quarter circle of radius rho removed',&
'   from one corner. There are Neumann conditions on the top and bottom',&
'   edges and Dirichlet conditions on the remainder of the boundary.',&
'',&
'                            u  = 0',&
'                             n',&
'',&
'                        -------------',&
'                       |             .',&
'                       |             .',&
'                       |              .',&
'                       |               .  u = 1',&
'                       |                 .',&
'                       |                    .',&
'                       |                       .',&
'                u = 0  |                        |',&
'                       |                        |',&
'                       |                        |',&
'                       |                        |  u = 1',&
'                       |                        |',&
'                       |                        |',&
'                       |                        |',&
'                        ------------------------',&
'',&
'                                 u  = 0',&
'                                  n',&
'',&
'   The effective conductivity of an medium is then given by the integral',&
'   along the left edge,',&
'',&
'                               1',&
'                    sigma = integral  u (0,y) dy',&
'                              0        n',&
'',&
'   It is of interest to study the relation between the radius rho and',&
'   the conductivity sigma. In particular, as rho approaches one, sigma',&
'   becomes infinite.',&
'',&
'   Keller and Sachs use a finite difference approximation. The following',&
'   technique makes use of the fact that the equation is actually Laplace''s',&
'   equation and leads to a much smaller matrix problem to solve.',&
'',&
'        Consider an approximate solution of the form',&
'',&
'                    n      2j-1',&
'              u =  sum  c r    cos(2j-1)t',&
'                   j=1   j',&
'',&
'   where r,t are polar coordinates (t is theta). The coefficients are',&
'   to be determined. For any set of coefficients, this function already',&
'   satisfies the differential equation because the basis functions are',&
'   harmonic; it satisfies the normal derivative boundary condition on',&
'   the bottom edge of the domain because we used cos t in preference to',&
'   sin t ; and it satisfies the boundary condition on the left edge of',&
'   the domain because we use only odd multiples of t .',&
'',&
'   The computational task is to find coefficients so that the boundary',&
'   conditions on the remaining edges are satisfied as well as possible. To',&
'   accomplish this, pick m points (r,t) on the remaining edges. It is',&
'   desirable to have m > n and in practice we usually choose m to be two',&
'   or three times as large as n .  Typical values of n are 10 or 20 and',&
'   of m are 20 to 60. An m by n matrix A is generated. The i,j element',&
'   is the j-th basis function, or its normal derivative, evaluated at',&
'   the i-th boundary point. A right hand side with m components is also',&
'   generated. In this example, the elements of the right hand side are',&
'   either zero or one. The coefficients are then found by solving the',&
'   overdetermined set of equations',&
'',&
'               Ac = b',&
'',&
'   in a least squares sense.',&
'',&
'   Once the coefficients have been determined, the approximate solution',&
'   is defined everywhere on the domain. It is then possible to compute the',&
'   effective conductivity sigma . In fact, a very simple formula results,',&
'',&
'                        n       j-1',&
'              sigma =  sum  (-1)   c',&
'                       j=1          j',&
'',&
'   To use LALA for this problem, the following "program" is first stored',&
'   in the local computer file system, say under the name "PDE".',&
'',&
'      //Conductivity example.',&
'      //Parameters ---',&
'         rho       //radius of cylindrical inclusion',&
'         n         //number of terms in solution',&
'         m         //number of boundary points',&
'      //initialize operation counter',&
'         flop = <0 0>;',&
'      //initialize variables',&
'         m1 = round(m/3);   //number of points on each straight edge',&
'         m2 = m - m1;       //number of points with Dirichlet conditions',&
'         pi = 4*atan(1);',&
'      //generate points in Cartesian coordinates',&
'         //right hand edge',&
'         for i = 1:m1, x(i) = 1; y(i) = (1-rho)*(i-1)/(m1-1);',&
'         //top edge',&
'         for i = m2+1:m, x(i) = (1-rho)*(m-i)/(m-m2-1); y(i) = 1;',&
'         //circular edge',&
'         for i = m1+1:m2, t = pi/2*(i-m1)/(m2-m1+1); ...',&
'            x(i) = 1-rho*sin(t);  y(i) = 1-rho*cos(t);',&
'      //convert to polar coordinates',&
'         for i = 1:m-1, th(i) = atan(y(i)/x(i));  ...',&
'            r(i) = sqrt(x(i)**2+y(i)**2);',&
'         th(m) = pi/2;  r(m) = 1;',&
'      //generate matrix',&
'         //Dirichlet conditions',&
'         for i = 1:m2, for j = 1:n, k = 2*j-1; ...',&
'            a(i,j) = r(i)**k*cos(k*th(i));',&
'         //Neumann conditions',&
'         for i = m2+1:m, for j = 1:n, k = 2*j-1; ...',&
'            a(i,j) = k*r(i)**(k-1)*sin((k-1)*th(i));',&
'      //generate right hand side',&
'         for i = 1:m2, b(i) = 1;',&
'         for i = m2+1:m, b(i) = 0;',&
'      //solve for coefficients',&
'         c = A\b',&
'      //compute effective conductivity',&
'         c(2:2:n) = -c(2:2:n);',&
'         sigma = sum(c)',&
'      //output total operation count',&
'         ops = flop(2)',&
'',&
'   The program can be used within LALA by setting the three parameters',&
'   and then accessing the file. For example,',&
'',&
'      rho = .9;',&
'      n = 15;',&
'      m = 30;',&
'      exec(''PDE'')',&
'',&
'   The resulting output is',&
'',&
'      rho   =',&
'',&
'         .9000',&
'',&
'      n     =',&
'',&
'       15.',&
'',&
'      m     =',&
'',&
'       30.',&
'',&
'      c     =',&
'',&
'         2.2275',&
'        -2.2724',&
'         1.1448',&
'         0.1455',&
'        -0.1678',&
'        -0.0005',&
'        -0.3785',&
'         0.2299',&
'         0.3228',&
'        -0.2242',&
'        -0.1311',&
'         0.0924',&
'         0.0310',&
'        -0.0154',&
'        -0.0038',&
'',&
'      sigm  =',&
'',&
'         5.0895',&
'',&
'      ops   =',&
'',&
'         16204.',&
'',&
'   A total of 16204 floating point operations were necessary to set up the',&
'   matrix, solve for the coefficients and compute the conductivity. The',&
'   operation count is roughly proportional to m*n**2. The results obtained',&
'   for sigma as a function of rho by this approach are essentially the',&
'   same as those obtained by the finite difference technique of Keller',&
'   and Sachs, but the computational effort involved is much less.',&
'',&
'================================================================================',&
'EIGENVALUE SENSITIVITY EXAMPLE',&
'',&
'   In this example, we construct a matrix whose eigenvalues are moderately',&
'   sensitive to perturbations and then analyze that sensitivity. We',&
'   begin with the statement',&
'',&
'      B = <3 0 7; 0 2 0; 0 0 1>',&
'',&
'   which produces',&
'',&
'      B     =',&
'',&
'          3.    0.    7.',&
'          0.    2.    0.',&
'          0.    0.    1.',&
'',&
'   Obviously, the eigenvalues of B are 1, 2 and 3 . Moreover, since',&
'   B is not symmetric, these eigenvalues are slightly sensitive to',&
'   perturbation. (The value b(1,3) = 7 was chosen so that the elements',&
'   of the matrix A below are less than 1000.)',&
'',&
'   We now generate a similarity transformation to disguise the eigenvalues',&
'   and make them more sensitive.',&
'',&
'      L = <1 0 0; 2 1 0; -3 4 1>, M = L\L''',&
'',&
'      L     =',&
'',&
'          1.    0.    0.',&
'          2.    1.    0.',&
'         -3.    4.    1.',&
'',&
'      M     =',&
'',&
'          1.0000    2.0000   -3.0000',&
'         -2.0000   -3.0000   10.0000',&
'         11.0000   18.0000  -48.0000',&
'',&
'   The matrix M has determinant equal to 1 and is moderately badly',&
'   conditioned. The similarity transformation is',&
'',&
'      A = M*B/M',&
'',&
'      A     =',&
'',&
'        -64.0000   82.0000   21.0000',&
'        144.0000 -178.0000  -46.0000',&
'       -771.0000  962.0000  248.0000',&
'',&
'   Because det(M) = 1 , the elements of A would be exact integers',&
'   if there were no roundoff. So,',&
'',&
'      A = round(A)',&
'',&
'      A     =',&
'',&
'        -64.   82.   21.',&
'        144. -178.  -46.',&
'       -771.  962.  248.',&
'',&
'   This, then, is our test matrix. We can now forget how it',&
'   was generated and analyze its eigenvalues.',&
'',&
'      <X,D> = eig(A)',&
'',&
'      D     =',&
'',&
'          3.0000    0.0000    0.0000',&
'          0.0000    1.0000    0.0000',&
'          0.0000    0.0000    2.0000',&
'',&
'      X     =',&
'',&
'          -.0891    3.4903   41.8091',&
'           .1782   -9.1284  -62.7136',&
'          -.9800   46.4473  376.2818',&
'',&
'   Since A is similar to B, its eigenvalues are also 1, 2 and 3.  They',&
'   happen to be computed in another order by the EISPACK subroutines. The',&
'   fact that the columns of X, which are the eigenvectors, are so far',&
'   from being orthonormal is our first indication that the eigenvalues',&
'   are sensitive. To see this sensitivity, we display more figures of',&
'   the computed eigenvalues.',&
'',&
'      long, diag(D)',&
'',&
'      ans   =',&
'',&
'         2.999999999973599',&
'         1.000000000015625',&
'         2.000000000011505',&
'',&
'   We see that, on this computer, the last five significant figures are',&
'   contaminated by roundoff error. A somewhat superficial explanation',&
'   of this is provided by',&
'',&
'      short,  cond(X)',&
'',&
'      ans   =',&
'',&
'         3.2216e+05',&
'',&
'   The condition number of X gives an upper bound for the relative',&
'   error in the computed eigenvalues. However, this condition',&
'   number is affected by scaling.',&
'',&
'      X = X/diag(X(3,:)),  cond(X)',&
'',&
'      X     =',&
'',&
'           .0909     .0751     .1111',&
'          -.1818    -.1965    -.1667',&
'          1.0000    1.0000    1.0000',&
'',&
'      ans   =',&
'',&
'         1.7692e+03',&
'',&
'   Rescaling the eigenvectors so that their last components are all',&
'   equal to one has two consequences. The condition of X is decreased',&
'   by over two orders of magnitude. (This is about the minimum condition',&
'   that can be obtained by such diagonal scaling.)  Moreover, it is now',&
'   apparent that the three eigenvectors are nearly parallel.',&
'',&
'   More detailed information on the sensitivity of the individual',&
'   eigenvalues involves the left eigenvectors.',&
'',&
'      Y = inv(X''),  Y''*A*X',&
'',&
'      Y     =',&
'',&
'       -511.5000  259.5000  252.0000',&
'        616.0000 -346.0000 -270.0000',&
'        159.5000  -86.5000  -72.0000',&
'',&
'      ans   =',&
'',&
'          3.0000     .0000     .0000',&
'           .0000    1.0000     .0000',&
'           .0000     .0000    2.0000',&
'',&
'   We are now in a position to compute the sensitivities of the individual',&
'   eigenvalues.',&
'',&
'      for j = 1:3, c(j) = norm(Y(:,j))*norm(X(:,j)); end,  C',&
'',&
'      C     =',&
'',&
'        833.1092',&
'        450.7228',&
'        383.7564',&
'',&
'   These three numbers are the reciprocals of the cosines of the',&
'   angles between the left and right eigenvectors. It can be shown that',&
'   perturbation of the elements of A can result in a perturbation of',&
'   the j-th eigenvalue which is c(j) times as large.  In this example,',&
'   the first eigenvalue has the largest sensitivity.',&
'',&
'   We now proceed to show that A is close to a matrix with a double',&
'   eigenvalue. The direction of the required perturbation is given by',&
'',&
'      E = -1.e-6*Y(:,1)*X(:,1)''',&
'',&
'      E     =',&
'',&
'         1.0e-03 *',&
'',&
'           .0465    -.0930     .5115',&
'          -.0560     .1120    -.6160',&
'          -.0145     .0290    -.1595',&
'',&
'   With some trial and error which we do not show, we bracket the',&
'   point where two eigenvalues of a perturbed A coalesce and then',&
'   become complex.',&
'',&
'      eig(A + .4*E),  eig(A + .5*E)',&
'',&
'      ans   =',&
'',&
'          1.1500',&
'          2.5996',&
'          2.2504',&
'',&
'      ans   =',&
'',&
'         2.4067 +  .1753*i',&
'         2.4067 -  .1753*i',&
'         1.1866 + 0.0000*i',&
'',&
'   Now, a bisecting search, driven by the imaginary part of one of',&
'   the eigenvalues, finds the point where two eigenvalues are nearly',&
'   equal.',&
'',&
'      r = .4;  s = .5;',&
'',&
'      while s-r > 1.e-14, t = (r+s)/2; d = eig(A+t*E); ...',&
'        if imag(d(1))=0, r = t; else, s = t;',&
'',&
'      long,  T',&
'',&
'      T     =',&
'',&
'           .450380734134507',&
'',&
'   Finally, we display the perturbed matrix, which is obviously close',&
'   to the original, and its pair of nearly equal eigenvalues.  (We have',&
'   dropped a few digits from the long output.)',&
'',&
'      A+t*E,  eig(A+t*E)',&
'',&
'      A',&
'',&
'       -63.999979057   81.999958114   21.000230369',&
'       143.999974778 -177.999949557  -46.000277434',&
'      -771.000006530  962.000013061  247.999928164',&
'',&
'      ans   =',&
'',&
'         2.415741150',&
'         2.415740621',&
'         1.168517777',&
'',&
'   The first two eigenvectors of A + t*E are almost indistinguishable',&
'   indicating that the perturbed matrix is almost defective.',&
'',&
'      <X,D> = eig(A+t*E);  X = X/diag(X(3,:))',&
'',&
'      X     =',&
'',&
'          .096019578     .096019586    .071608466',&
'         -.178329614    -.178329608   -.199190520',&
'         1.000000000    1.000000000   1.000000000',&
'',&
'      short,  cond(X)',&
'',&
'      ans   =',&
'',&
'         3.3997e+09',&
'',&
'================================================================================',&
'COMMUNICATING WITH OTHER PROGRAMS',&
'',&
'   There are four different ways LALA can be used in',&
'   conjunction with other programs:',&
'',&
'      -- user() - a user-supplied subroutine',&
'      -- exec() - reading commands from a file',&
'      -- save() and load() -- reading specially formatted data files.',&
'      -- lala() - call the interpreter with a CHARACTER array of',&
'                  commands or interactively.',&
'',&
'   Let us illustrate each of these by equivalents of the following',&
'   simple example.',&
'',&
'   You can start the lala(1) program up and simply enter:',&
'',&
'         n = 6',&
'         for i = 1:n, for j = 1:n, a(i,j) = abs(i-j);',&
'         a',&
'         x = inv(a)',&
'',&
'   An example user routine could be introduced into LALA that',&
'   does the same thing as the "for" statement by compiling and',&
'   linking the following subroutine into the calling program.',&
'',&
'         program demo_user',&
'         implicit none',&
'         use M_matrix',&
'         call set_usersub(lala_user)',&
'         call lala()',&
'         subroutine lala_user(a,m,n,s,t)',&
'            implicit none',&
'            doubleprecision a(:),s,t',&
'            integer m,n',&
'            n = int(a(1))',&
'            m = n',&
'            do j = 1, n',&
'               do i = 1, n',&
'                  k = i + (j-1)*m',&
'                  a(k) = iabs(i-j)',&
'               enddo',&
'            enddo',&
'            end subroutine lala_user',&
'         end program demo_user',&
'',&
'   A user-defined function can then be registered with the program',&
'   with',&
'',&
'           call set_usersub(SUBROUTINE_NAME)',&
'',&
'   Note the routine must be defined with an explicit interface',&
'   available in the calling unit.',&
'',&
'   Then the LALA statements',&
'',&
'         n = 6',&
'         a = user(n)',&
'         x = inv(a)',&
'',&
'   do the job.',&
'',&
'   The example procedure could be called by storing the following',&
'   text in a file named, say, EXAMPLE.',&
'',&
'         for i = 1:n, for j = 1:n, a(i,j) = abs(i-j);',&
'',&
'   Then the LALA statements',&
'',&
'         n = 6',&
'         exec(''EXAMPLE'',0)',&
'         x = inv(a)',&
'',&
'   have the desired effect. The 0 as the optional second parameter',&
'   of exec indicates that the text in the file should not be printed',&
'   on the terminal.',&
'',&
'   The matrices A and X could also be stored in files. Two',&
'   separate main programs would be involved. The first is:',&
'',&
'            program maina',&
'            doubleprecision a(10,10)',&
'            n = 6',&
'            do j = 1, n',&
'               do i = 1, n',&
'                  a(i,j) = iabs(i-j)',&
'               enddo',&
'            enddo',&
'            OPEN(UNIT=1,FILE=''A'')',&
'            write(1,''(a32,2i4)'') ''a'', n,n',&
'            do j = 1, n',&
'               write(1,102) (a(i,j),i=1,n)',&
'            enddo',&
'        102 format(4z18)',&
'            end program maina',&
'',&
'   The OPEN statement may take different forms on different systems.',&
'   It attaches Fortran logical unit number 1 to the file named A.',&
'',&
'   The FORMAT number 102 may also be system dependent. This',&
'   particular one is appropriate for hexadecimal computers with an 8',&
'   byte double precision floating point word. Check, or modify,',&
'   LALA subroutine SAVLOD.',&
'',&
'   After this program is executed, enter LALA and give the',&
'   following statements:',&
'',&
'         load(''A'')',&
'         X = inv(a)',&
'         save(''X'',X)',&
'',&
'   If all goes according to plan, this will read the matrix "a" from',&
'   the file A, invert it, store the inverse in X and then write the',&
'   matrix X on the file X. The following program can then access X.',&
'',&
'            program mainx',&
'            doubleprecision x(10,10)',&
'            open(unit=1,file=''x'')',&
'            rewind 1',&
'            read (1, ''(a32,2i4)'') id,m,n',&
'            do j = 1, n',&
'               read(1,''(4z18)'') (x(i,j),i=1,m)',&
'            enddo',&
'            ...',&
'            ...',&
'',&
'',&
'   The most elaborate mechanism involves using LALA as a subroutine',&
'   within another program. Communication with the LALA stack is',&
'   accomplished using subroutine lala().',&
'    The preamble of MATZ is:',&
'',&
'         SUBROUTINE MATZ(A,LDA,M,N,ID,JOB,IERR)',&
'         INTEGER LDA,M,N,JOB,IERR',&
'         character(len=*) :: id',&
'         DOUBLEPRECISION A(LDA,N)',&
'',&
'         ! ACCESS LALA VARIABLE STACK',&
'         ! A IS AN M BY N MATRIX, STORED IN AN ARRAY WITH',&
'         !     LEADING DIMENSION LDA.',&
'         ! ID IS THE NAME OF A. ID IS UP TO FOUR CHARACTERS.',&
'         ! JOB =  0  GET REAL A FROM LALA,',&
'         !     =  1  PUT REAL A INTO LALA,',&
'         !     = 10  GET IMAG PART OF A FROM LALA,',&
'         !     = 11  PUT IMAG PART OF A INTO LALA.',&
'         ! RETURN WITH NONZERO IERR AFTER LALA ERROR MESSAGE.',&
'         !',&
'         ! USES LALA ROUTINES STACKG, STACKP AND ERROR',&
'',&
'        The preamble of subroutine LALA is:',&
'',&
'         SUBROUTINE LALA(INIT)',&
'         ! INIT = 0 FOR FIRST ENTRY, NONZERO FOR SUBSEQUENT ENTRIES',&
'',&
'        To do our example, write the following program:',&
'',&
'            DOUBLEPRECISION A(10,10),X(10,10)',&
'            DATA LDA/10/',&
'            call M_88(0,'''')',&
'            N = 6',&
'            DO J = 1, N',&
'               DO I = 1, N',&
'                  A(I,J) = IABS(I-J)',&
'               enddo',&
'            enddo',&
'            call MATZ(A,LDA,N,N,''A'',1,IERR)',&
'            IF (IERR .NE. 0) GO TO ...',&
'            call LALA(1,'''')',&
'            call MATZ(X,LDA,N,N,''X'',0,IERR)',&
'            IF (IERR .NE. 0) GO TO ...',&
'            ...',&
'            ...',&
'',&
'   When this program is executed, the call to LALA(0) produces the',&
'   LALA greeting, then waits for input. The command',&
'',&
'            quit',&
'',&
'   sends control back to our example program. The matrix A is',&
'   generated by the program and sent to the stack by the first call',&
'   to MATZ. The call to LALA(1) produces the LALA(1) prompt. Then',&
'   the statements',&
'',&
'            X = inv(A)',&
'            quit',&
'',&
'   will invert our matrix, put the result on the stack and go back',&
'   to our program. The second call to MATZ will retrieve X .',&
'',&
'   By the way, this matrix X is interesting. Take a look at',&
'   round(2*(n-1)*X).',&
'',&
'================================================================================',&
'ACKNOWLEDGEMENT',&
'',&
'   LALA was inspired by the MATLAB subroutine.  Most of the work on',&
'   MATLAB was carried out at the University of New Mexico, where it was',&
'   being supported by the National Science Foundation. Additional work',&
'   has been done during visits to Stanford Linear Accelerator Center,',&
'   Argonne National Laboratory and Los Alamos Scientific Laboratory,',&
'   where support has been provided by NSF and the Department of Energy.',&
'',&
'================================================================================',&
'REFERENCES FOR THE MATLAB ROUTINE',&
'',&
' [1]  J. J. Dongarra, J. R. Bunch, C. B. Moler and G. W. Stewart,',&
'      LINPACK Users'' Guide, Society for Industrial and Applied',&
'      Mathematics, Philadelphia, 1979.',&
'',&
' [2]  B. T. Smith, J. M. Boyle, J. J. Dongarra, B. S. Garbow, Y.',&
'      Ikebe, V. C. Klema, C. B. Moler, Matrix Eigensystem Routines',&
'      -- EISPACK Guide, Lecture Notes in Computer Science, volume',&
'      6, second edition, Springer-Verlag, 1976.',&
'',&
' [3]  B. S. Garbow, J. M. Boyle, J. J. Dongarra, C. B. Moler,',&
'      Matrix Eigensystem Routines -- EISPACK Guide Extension,',&
'      Lecture Notes in Computer Science, volume 51, Springer-',&
'      Verlag, 1977.',&
'',&
' [4]  S. Cohen and S. Piper, SPEAKEASY III Reference Manual,',&
'      Speakeasy Computing Corp., Chicago, Ill., 1979.',&
'',&
' [5]  J. H. Wilkinson and C. Reinsch, Handbook for Automatic',&
'      Computation, volume II, Linear Algebra, Springer-Verlag,',&
'     1971.',&
'',&
' [6]  Niklaus Wirth, Algorithms + Data Structures = Programs,',&
'      Prentice-Hall, 1976.',&
'',&
' [7]  H. B. Keller and D. Sachs, "Calculations of the Conductivity',&
'      of a Medium Containing Cylindrical Inclusions", J. Applied',&
'      Physics 35, 537-538, 1964.',&
'',&
' [8]  C. B. Moler and C. F. Van Loan, Nineteen Dubious Ways to',&
'      Compute the Exponential of a Matrix, SIAM Review 20, 801-',&
'      836, 1979.',&
'',&
' [9]  G. E. Forsythe, M. A. Malcolm and C. B. Moler, Computer',&
'      Methods for Mathematical Computations, Prentice-Hall, 1977.',&
'',&
' [10] C. B. Moler and D. R. Morrison, "Replacing square roots by',&
'      Pythagorean sums", University of New Mexico, Computer',&
'      Science Department, technical report, submitted for',&
'     publication, 1980.',&
'',&
'================================================================================',&
'SUMMARY    A list of basic (case-sensitive) section and topic names',&
'   .______________._________________________________________________________.',&
'   |SYNTAX        | [ ] < > ( ) = .  , !  ; \ / '''' + - * : semi ?           |',&
'   |______________._________________________________________________________|',&
'   |VARIABLES     | ans    clear who                                        |',&
'   |______________._________________________________________________________|',&
'   |BASIC         | atan   cos   exp    log    sin      sqrt                |',&
'   |______________._________________________________________________________|',&
'   |HIGH          | abs    base  chol   chop   cond     conjg  det    diag  |',&
'   |              | eig    eye   hess   invh   imag     inv    kron   lu    |',&
'   |              | magic  norm  ones   orth   pinv     poly   prod   qr    |',&
'   |              | rand   rank  rcond  rat    real     rref   roots  round |',&
'   |              | schur  shape sum    svd    tril     triu   user   zeros |',&
'   |______________._________________________________________________________|',&
'   |FLOW control  | else   end   if     for    while    exit   quit         |',&
'   |______________._________________________________________________________|',&
'   |FILE access   | exec   load  print  save   delete                       |',&
'   |______________._________________________________________________________|',&
'   |OUTPUT options| lines  long  short  diary  display  plot                |',&
'   |______________._________________________________________________________|',&
'   |ENVIRONMENT   | getenv                                                  |',&
'   |______________._________________________________________________________|',&
'   |DOCUMENTATION | help   fhelp  NEWS                                      |',&
'   |______________._________________________________________________________|',&
'   |MISCELLANEOUS | eps    debug  flops sh     MACROS   EDIT   CHARS        |',&
'   |______________._________________________________________________________|',&
'================================================================================',&
'SAMPLE',&
'      Here are a few sample statements:',&
'',&
'       A = <1 2; 3 4>',&
'       b = <5 6>''',&
'       x = A\b',&
'       <V,D> = eig(A),  norm(A-V*D/V)',&
'       help \ , help eig',&
'       exec(''demo'',7)',&
'',&
'      For more information, generate the LALA Users'' Guide',&
'      using',&
'',&
'        help manual',&
'        w help.txt',&
'        q',&
'================================================================================',&
'DOCUMENTATION',&
'fhelp topic|SECTION_NAME',&
'',&
'      "fhelp" is identical in usage to "help" except that it searches a',&
'      collection of descriptions of Fortran intrinsics.',&
'',&
'        fhelp verify',&
'        fhelp pack',&
'',&
'      See "help"',&
'',&
'help  topic|SECTION_NAME',&
'',&
'      "help" gives assistance. It is equivalent to "help SUMMARY"',&
'      by default.',&
'',&
'      o  "help" with no options lists common topic and section names.',&
'      o  The special topic "topics" shows all topic lines.',&
'      o  The special topic "manual" displays all the help text.',&
'      o  The special topic "search" shows lines from the manual',&
'         containing the subsequent string',&
'',&
'         Enter "h" at the "continue ..." prompt for additional options.',&
'',&
'      For example:',&
'',&
'         help        // a list of common topics and section names',&
'         help topics // a list of topics including the first line of',&
'                     // the topic.',&
'         help abs    // produces help on the function "abs".',&
'         help FLOW   // the entire section on flow control is displayed.',&
'         help manual // show all the help text',&
'         help help   // obviously prints this message.',&
'         help search factor // show all lines containing "factor".',&
'',&
'      Alternatively, To place all the documenation in a file, use',&
'      "help manual" and enter "w help.txt" at the "continue .." prompt.',&
'NEWS',&
'      LALA is intended to be used primarily by families of FORTRAN',&
'      programs that wish to add a consistent interactive "calculator"',&
'      mode for interactively inspecting and modifying data.',&
'',&
'      May, 1981.',&
'',&
'      This is a port of the Argonne National Lab. FORTRAN 77 MATLAB',&
'      routine circa 1981.',&
'',&
'      Mar, 1990.',&
'',&
'      Input lines can now be recalled and edited.  A "??" on a line by',&
'      itself calls the command history mode. Enter "?" after entering',&
'      the mode for details.',&
'',&
'      Apr, 2021.',&
'',&
'      Rewritten but largely true to the original documentation.',&
'',&
'what  does nothing for now',&
'',&
'sh    Starts the command shell interactively, using the command defined by',&
'      the environment variable SHELL. Note that in addition any line',&
'      starting with an exclamation (!) is passed to the system for',&
'      execution.',&
'================================================================================',&
'SYNTAX',&
'[     See "<"',&
']     See "<"',&
'>     See "<" . Also see MACROS.',&
'<     < > or [ ] are brackets used in forming vectors and matrices.',&
'      "<6.9 9.64 sqrt(-1)>" is a vector with three elements separated by',&
'      blanks. "[1+I 2-I 3]" and "[1 +I 2 -I 3]" are not the same. The',&
'      first has three elements, the second has five.  <11 12 13; 21 22',&
'      23> is a 2 by 3 matrix. The semicolon ends the first row.',&
'',&
'      Vectors and matrices can be used inside < > brackets.  <A B; C>',&
'      is allowed if the number of rows of A equals the number of rows',&
'      of B and the number of columns of A plus the number of columns of',&
'      B equals the number of columns of C. This rule generalizes in a',&
'      hopefully obvious way to allow fairly complicated constructions.',&
'',&
'      A = < > stores an empty matrix in A, thereby removing it from the',&
'      list of current variables.',&
'',&
'      For the use of < and > on the left of the = in multiple assignment',&
'      statements, See "lu", "eig", "svd" and so on.',&
'',&
'      In "while" and "if" clauses, "<>" means less than or greater than,',&
'      i.e. not equal, "<" means less than, ">" means greater than,',&
'      "<=" means less than or equal, ">=" means greater than or equal.',&
'',&
'      For the use of ">" and "<" to delineate macros, see MACROS.',&
'',&
'{     see "(".',&
'}     see "(".',&
')     See "(" .',&
'(     ( ) or { } are used to indicate precedence in arithmetic expressions',&
'      and to enclose arguments of functions in the usual way. They are',&
'      also used to enclose subscripts of vectors and matrices in a manner',&
'      somewhat more general than the usual way. If X and V are vectors,',&
'      then X(V) is <X(V(1)), X(V(2)), ..., X(V(N))>. The components of V',&
'      are rounded to nearest integers and used as subscripts. An error',&
'      occurs if any such subscript is less than 1 or greater than the',&
'      dimension of X. Some examples:',&
'',&
'         X(3) is the third element of X .',&
'         X([1 2 3]) is the first three elements of X. So is',&
'         X([sqrt(2), sqrt(3), 4*atan(1)]) .',&
'         If X has N components, X(N:-1:1) reverses them.',&
'',&
'      The same indirect subscripting is used in matrices. If V has',&
'      M components and W has N components, then A(V,W) is the M by N',&
'      matrix formed from the elements of A whose subscripts are the',&
'      elements of V and W. For example...  A(<1,5>,:) = A(<5,1>,:)',&
'      interchanges rows 1 and 5 of A.',&
'',&
'=     Used in assignment statements and to mean equality in "while"',&
'      and "if" clauses.',&
'',&
'.     Decimal point. 314/100, 3.14 and .314E1 are all the',&
'      same.',&
'',&
'      Element-by-element multiplicative operations are obtained',&
'      using .* , ./ , or .\ . For example, C = A ./ B is the',&
'      matrix with elements c(i,j) = a(i,j)/b(i,j) .',&
'',&
'      Kronecker tensor products and quotients are obtained with',&
'      .*. , ./. and .\. . See "kron".',&
'',&
'      Two or more points at the end of the line indicate',&
'      continuation. The total line length limit is 1024',&
'      characters.',&
'',&
',     Used to separate matrix subscripts and function arguments.',&
'      Used at the end of "for", "while" and "if" clauses. Used to',&
'      separate statements in multi-statement lines. In this',&
'      situation, it may be replaced by a semicolon to suppress',&
'      printing.',&
'',&
'!     If an exclamation is the first character of a line the',&
'      rest of the line is passed to the system to be executed.',&
'',&
'      Examples:',&
'',&
'         // enter command history mode and change all occurrences of',&
'         // "abc" to "123" on the last command entered.',&
'         !!c/abc/123',&
'',&
'         // pass command to system',&
'         !ls -ltrasd',&
'',&
'      see "EDIT"',&
'',&
';     Used inside brackets to end rows.',&
'',&
'      Used after an expression or statement to suppress printing.',&
'      See "semi".',&
'',&
'\     Backslash or matrix left division. A\B is roughly the',&
'      same as "inv(A)*B", except it is computed in a different',&
'      way. If A is an N by N matrix and B is a column vector',&
'      with N components, or a matrix with several such columns,',&
'      then X = A\B is the solution to the equation A*X = B',&
'      computed by Gaussian elimination. A warning message is',&
'      printed if A is badly scaled or nearly singular.',&
'      A\eye produces the inverse of A .',&
'',&
'      If A is an M by N matrix with M < or > N and B is a',&
'      column vector with M components, or a matrix with several',&
'      such columns, then X = A\B is the solution in the least',&
'      squares sense to the under- or overdetermined system of',&
'      equations A*X = B. The effective rank, K, of A is',&
'      determined from the QR decomposition with pivoting. A',&
'      solution X is computed which has at most K nonzero',&
'      components per column. If K < N this will usually not be',&
'      the same solution as pinv(A)*B .',&
'      A\eye produces a generalized inverse of A.',&
'',&
'      If A and B have the same dimensions, then A .\ B has',&
'      elements a(i,j)\b(i,j) .',&
'',&
'      Also, see "edit".',&
'',&
'/     Slash or matrix right division. B/A is roughly the same',&
'      as B*inv(A) . More precisely, B/A = (A''\B'')'' . See \ .',&
'',&
'      IF A and B have the same dimensions, then A ./ B has',&
'      elements a(i,j)/b(i,j) .',&
'',&
'      Two or more slashes together on a line indicate a logical end of',&
'      line. Any following text is ignored.',&
'',&
'''     Transpose. X'' is the complex conjugate transpose of X .',&
'',&
'      A quote is also use to delmit text. ''ANY TEXT'' is a vector whose',&
'      components are the LALA internal codes for the characters. A',&
'      quote within the text is indicated by two quotes. See "display"',&
'      and "FILE" .',&
'',&
'+     Addition. X + Y . X and Y must have the same dimensions.',&
'',&
'-     Subtraction. X - Y . X and Y must have the same',&
'      dimensions.',&
'',&
'*     Matrix multiplication, X*Y . Any scalar (1 by 1 matrix)',&
'      may multiply anything. Otherwise, the number of columns of',&
'      X must equal the number of rows of Y .',&
'',&
'      Element-by-element multiplication is obtained with X .* Y .',&
'',&
'      The Kronecker tensor product is denoted by X .*. Y .',&
'',&
'      Powers. X**p is X to the p power. p must be a',&
'      scalar. If X is a matrix, see "HIGH" .',&
'',&
':     Colon. Used in subscripts, "for" iterations and possibly',&
'      elsewhere.',&
'',&
'        j:k   is the same as  <j, j+1, ..., k>',&
'              is empty if  j > k .',&
'        j:i:k is the same as [j, j+i,j+2*i, ..., k]',&
'              (Fortran DO loop users beware of the unusual order!)',&
'',&
'         j:i:k  is the same as  <j, j+i, j+2i, ..., k>',&
'         j:i:k  is empty if  i > 0 and j > k or if i < 0 and j < k .',&
'',&
'      The colon notation can be used to pick out selected rows,',&
'      columns and elements of vectors and matrices.',&
'',&
'         A(:)    is all the elements of A, regarded as a single column.',&
'                 However, used on the left side of an assignment, A(:)',&
'                 fills A, but preserves its shape.',&
'        A(:,j)   is the j-th column of A',&
'        A(j:k)   is A(j), A(j+1), ... , A(k)',&
'        A(:,j:k) is A(:,j), A(:,j+1), ... ,A(:,k) and so on.',&
'        A(:,:)   is the same as A.',&
'',&
'      For the use of the colon in the "for" statement, See "for" .',&
'',&
'semi  "semi" toggles the action of semicolons at the end of lines.',&
'      It will make semicolons cause rather than suppress printing.',&
'      A second "semi" restores the initial interpretation.',&
'================================================================================',&
'VARIABLES',&
'',&
'ans   Variable created automatically when expressions are not',&
'      assigned to anything else.',&
'',&
'clear  Erases all variables, except "eps", "flop", "eye" and "rand".',&
'       X = <> erases only variable X . So does "clear X".',&
'',&
'who   Lists current variables.',&
'================================================================================',&
'MACROS',&
'',&
'       The macro facility involves text and inward pointing angle',&
'       brackets. If "STRING" is the source text for any LALA',&
'       expression or statement, then',&
'',&
'             t = ''STRING'';',&
'       encodes the text as a vector of integers and stores that',&
'       vector in t. "display(t)" will print the text and',&
'',&
'             >t<',&
'       causes the text to be interpreted, either as a statement or',&
'       as a factor in an expression. For example',&
'',&
'             t = ''1/(i+j-1)'';',&
'             display(t)',&
'             for i = 1:n, for j = 1:n, a(i,j) = >t<;',&
'',&
'       generates the Hilbert matrix of order n.',&
'       Another example showing indexed text,',&
'',&
'             S = <''x = 3            ''',&
'                  ''y = 4            ''',&
'                  ''z = sqrt(x*x+y*y)''>',&
'             for k = 1:3, >S(k,:)<',&
'',&
'       It is necessary that the strings making up the "rows" of',&
'       the "matrix" S have the same lengths.',&
'',&
'================================================================================',&
'BASIC FUNCTIONS',&
'',&
'      For matrix arguments X , the functions "sin", "cos", "atan",',&
'      "sqrt", "log", "exp" and X**p are computed using eigenvalues D',&
'      and eigenvectors V . If <V,D> = eig(X) then f(X) = V*f(D)/V . This',&
'      method may give inaccurate results if V is badly conditioned. Some',&
'      idea of the accuracy can be obtained by comparing X**1 with X .',&
'      For vector arguments, the function is applied to each component.',&
'',&
'atan  atan(X) is the arctangent of X . See "BASIC" .',&
'',&
'cos   cos(X) is the cosine of X . See "BASIC" .',&
'',&
'exp   exp(X) is the exponential of X , e to the X . See "BASIC".',&
'',&
'log   log(X) is the natural logarithm of X.',&
'',&
'      Complex results are produced if X is not positive, or has',&
'      nonpositive eigenvalues.',&
'',&
'      See "BASIC".',&
'',&
'sin   sin(X) is the sine of X. See "BASIC".',&
'',&
'sqrt  sqrt(X) is the square root of X. See "BASIC". Complex',&
'      results are produced if X is not positive, or has',&
'      nonpositive eigenvalues.',&
'================================================================================',&
'HIGH LEVEL FUNCTIONS',&
'',&
'abs   abs(X) is the absolute value, or complex modulus,',&
'      of the elements of X .',&
'',&
'base  base(X,B) is a vector containing the base B representation',&
'      of X. This is often used in conjunction with "display".',&
'      "display(X,B)" is the same as "display(base(X,B))". For example,',&
'      "display(4*atan(1),16)" prints the hexadecimal representation of pi.',&
'',&
'chol  Cholesky factorization. "chol(X)" uses only the diagonal',&
'      and upper triangle of X. The lower triangular is assumed to be',&
'      the (complex conjugate) transpose of the upper. If X is positive',&
'      definite, then "R = chol(X)" produces an upper triangular R so',&
'      that R''*R = X . If X is not positive definite, an error message',&
'      is printed.',&
'',&
'chop  Truncate arithmetic. "chop(P)" causes P places to be chopped',&
'      off after each arithmetic operation in subsequent computations. This',&
'      means P hexadecimal digits on some computers and P octal digits',&
'      on others. "chop(0)" restores full precision.',&
'',&
'cond  Condition number in 2-norm. "cond(X)" is the ratio of the',&
'      largest singular value of X to the smallest.',&
'',&
'conjg  "conjg(X)" is the complex conjugate of X .',&
'',&
'det   "det(X)" is the determinant of the square matrix X .',&
'',&
'diag  If V is a row or column vector with N components,',&
'      "diag(V,K)" is a square matrix of order "N+abs(K)" with the',&
'      elements of V on the K-th diagonal. K = 0 is the main diagonal,',&
'      K > 0 is above the main diagonal and K < 0 is below the main',&
'      diagonal. "diag(V)" simply puts V on the main diagonal. eg.',&
'',&
'         diag(-M:M) + diag(ones(2*M,1),1) + diag(ones(2*M,1),-1)',&
'',&
'      produces a tridiagonal matrix of order 2*M+1 .',&
'',&
'      If X is a matrix, "diag(X,K)" is a column vector formed from the',&
'      elements of the K-th diagonal of X.  "diag(X)" is the main diagonal',&
'      of X.  "diag(diag(X))" is a diagonal matrix .',&
'',&
'eig   Eigenvalues and eigenvectors.',&
'      "eig(X)" is a vector containing the eigenvalues of a square',&
'      matrix X.',&
'      "<V,D> = eig(X)" produces a diagonal matrix D of',&
'      eigenvalues and a full matrix V whose columns are the',&
'      corresponding eigenvectors so that X*V = V*D .',&
'',&
'eye   Identity matrix. "eye(N)" is the N by N identity matrix.',&
'      "eye(M,N)" is an M by N matrix with 1''s on the diagonal and',&
'      zeros elsewhere. "eye(A)" is the same size as A. "eye"',&
'      with no arguments is an identity matrix of whatever order',&
'      is appropriate in the context. For example "A + 3*eye"',&
'      adds 3 to each diagonal element of A.',&
'',&
'hess  Hessenberg form. The Hessenberg form of a matrix is zero',&
'      below the first subdiagonal. If the matrix is symmetric or',&
'      Hermitian, the form is tridiagonal. <P,H> = "hess(A)" produces a',&
'      unitary matrix P and a Hessenberg matrix H so that A = P*H*P''. By',&
'      itself, "hess(A)" returns H.',&
'',&
'invh  Inverse Hilbert matrix. "invh(N)" is the inverse of a N_by_N',&
'      Hilbert matrix (which is a famous example of a badly conditioned',&
'      matrix). The result is exact for N less than about 15, depending',&
'      upon the computer.',&
'',&
'         for i = 1:N, for j = 1:N, A(i,j) = 1/(i+j-1);',&
'',&
'      generates the NxN Hilbert matrix.',&
'',&
'      "invh" has an alias of "inverse_hilbert" and "invhilb".',&
'',&
'aimag see "imag"',&
'imag  "imag(X)" is the imaginary part of X .',&
'',&
'inv   "inv(X)" is the inverse of the square matrix X . A warning',&
'      message is printed if X is badly scaled or nearly',&
'      singular.',&
'',&
'kron  "kron(X,Y)" is the Kronecker tensor product of X and Y. It',&
'      is also denoted by X .*. Y . The result is a large matrix',&
'      formed by taking all possible products between the elements',&
'      of X and those of Y . For example, if X is 2 by 3, then',&
'      X .*. Y is',&
'',&
'            < x(1,1)*Y  x(1,2)*Y  x(1,3)*Y',&
'              x(2,1)*Y  x(2,2)*Y  x(2,3)*Y >',&
'',&
'      The five-point discrete Laplacian for an n-by-n grid can be',&
'      generated by',&
'',&
'            T = diag(ones(n-1,1),1);  T = T + T'';  I = eye(T);',&
'            A = T.*.I + I.*.T - 4*eye;',&
'',&
'      Just in case they might be useful, LALA includes',&
'      constructions called Kronecker tensor quotients, denoted by',&
'      X ./. Y and X .\. Y . They are obtained by replacing the',&
'      element-wise multiplications in X .*. Y with divisions.',&
'',&
'lu    Factors from Gaussian elimination. <L,U> = LU(X) stores a',&
'      upper triangular matrix in U and a ''psychologically lower',&
'      triangular matrix'', i.e. a product of lower triangular and',&
'      permutation matrices, in L , so that X = L*U . By itself,',&
'      "lu(X)" returns the output from CGEFA .',&
'',&
'magic  Magic square. "magic(N)" is an N by N matrix constructed',&
'       from the integers 1 through N**2 with equal row, column and',&
'       diagonal sums. N must be a positive whole number not equal to two.',&
'',&
'norm  computes the norm or P-norm of X',&
'',&
'      norm(X,P) computes the P-norm of X. P=2 is the default, which defines',&
'      the standard norm.',&
'',&
'      For matrices..',&
'          norm(X,1)      is the 1-norm of X; ie. the largest column sum',&
'                         of X.',&
'',&
'          norm(X,2)      the largest singular value of X.',&
'          or norm(X)',&
'',&
'          norm(X,''inf'')  is the infinity norm of X; ie. the largest row',&
'                         sum of X.',&
'',&
'          norm(X,''fro'')  is the F-norm, i.e. "sqrt(sum(diag(X''*X)))" .',&
'',&
'      For vectors..',&
'          norm(V,P)      the same as sum(V(I)**P)**(1/P) .',&
'                         ??? what about negative values of (I) and odd P? abs() or not',&
'',&
'          norm(V,2)      the square root of the sum of the squares of',&
'          or norm(V)     the entries of V.',&
'',&
'          norm(V,''inf'')  the value is max(abs(V)) .',&
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<',&
'!!          If X is a vector, then',&
'!!',&
'!!            norm(x,p) = sum(abs(x) .^ p) ^ (1/p)',&
'!!            norm(x,1) is the sum of the absolute values of X.',&
'!!            norm(x)/sqrt(n) is the root-mean-square value.',&
'!!            norm(x,-inf)=min(abs(x))',&
'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<',&
'',&
'ones  All ones. "ones(N)" is an N by N matrix of ones. "ones(M,N)"',&
'      is an M by N matrix of ones . "ones(A)" is the same size as A and',&
'      all ones .',&
'',&
'         a=magic(4)',&
'         a=a+ones(a)*3 // Add 3 to each element of "a"',&
'',&
'orth  Orthogonalization. "Q = orth(X)" is a matrix with',&
'      orthonormal columns, i.e. Q''*Q = eye, which span the same',&
'      space as the columns of X .',&
'',&
'pinv  Pseudoinverse.',&
'',&
'      "X = pinv(A)" produces a matrix X of the same dimensions as A''',&
'      so that A*X*A = A , X*A*X = X and AX and XA are Hermitian . The',&
'      computation is based on "svd(A)" and any singular values less',&
'      than a tolerance are treated as zero. The default tolerance is',&
'      "norm(shape(A),''inf'')*norM(A)*eps". This tolerance may be overridden',&
'      with "X = pinv(A,tol)". See "rank".',&
'',&
'poly  Characteristic polynomial.',&
'',&
'      If A is an N by N matrix, "poly(A)" is a column vector with',&
'      N+1 elements which are the coefficients of the characteristic',&
'      polynomial, "det(lambda*eye - A)" .',&
'',&
'      If V is a vector, "poly(V)" is a vector whose elements are the',&
'      coefficients of the polynomial whose roots are the elements of V',&
'      . For vectors, "roots" and "poly" are inverse functions of each',&
'      other, up to ordering, scaling, and roundoff error.',&
'',&
'      "roots(poly(1:20))" generates Wilkinson''s famous example.',&
'',&
'prod  "prod(X)" is the product of all the elements of X .',&
'',&
'qr    Orthogonal-triangular decomposition.  "<Q,R> = qr(X)" produces an',&
'      upper triangular matrix R of the same',&
'      dimension as X and a unitary matrix Q so that X = Q*R .',&
'',&
'      "<Q,R,E> = qr(X)" produces a permutation matrix E, an upper',&
'      triangular R with decreasing diagonal elements and a unitary Q',&
'      so that X*E = Q*R .  By itself, "qr(X)" returns the output of',&
'      "cqrdc". "triu(qr(X))" is R .',&
'',&
'rand  Random numbers and matrices. "rand(N)" is an N by N matrix',&
'      with random entries. "rand(M,N)" is an M by N matrix with',&
'      random entries. "rand(A)" is the same size as A. "rand"',&
'      with no arguments is a scalar whose value changes each time',&
'      it is referenced.',&
'',&
'      Ordinarily, random numbers are uniformly distributed in',&
'      the interval "(0.0,1.0). rand(''normal'')" switches to a',&
'      normal distribution with mean 0.0 and variance 1.0.',&
'      "rand(''uniform'')" switches back to the uniform distribution.',&
'      "rand(''seed'')" returns the current value of the seed for the',&
'      generator. "rand(''seed'',n)" sets the seed to n.',&
'      "rand(''seed'',0)" resets the seed to 0, its value when LALA',&
'      is first entered.',&
'',&
'rank  Rank. "K = rank(X)" is the number of singular values of X',&
'      that are larger than "norm(shape(X),''inf'')*norm(X)*eps".',&
'      "K = rank(X,tol)" is the number of singular values of X that',&
'      are larger than tol.',&
'',&
'rcond  "rcond(X)" is an estimate for the reciprocal of the',&
'       condition of X in the 1-norm obtained by the LINPACK',&
'       condition estimator. If X is well conditioned, rcond(X)',&
'       is near 1.0. If X is badly conditioned, rcond(X) is',&
'       near 0.0.',&
'       <R, Z> = rcond(A) sets R to rcond(A) and also produces a',&
'       vector Z so that',&
'',&
'                 norm(A*Z,1) = R*norm(A,1)*norm(Z,1)',&
'',&
'       So, if rcond(A) is small, then Z is an approximate null',&
'       vector.',&
'',&
'rat   An experimental function which attempts to remove the',&
'      roundoff error from results that should be "simple"',&
'      rational numbers.',&
'      "rat(X)" approximates each element of X by a continued',&
'      fraction of the form',&
'',&
'                a/b = d1 + 1/(d2 + 1/(d3 + ... + 1/dk))',&
'',&
'      with k <= len, integer di and abs(di) <= max . The default',&
'      values of the parameters are len = 5 and max = 100.',&
'      "rat(len,max)" changes the default values. Increasing either',&
'      len or max increases the number of possible fractions.',&
'      "<A,B> = rat(X)" produces integer matrices A and B so that',&
'',&
'                A ./ B  =  rat(X)',&
'',&
'      Some examples:',&
'',&
'            long',&
'            T = invh(6), X = inv(T)',&
'            <A,B> = rat(X)',&
'            H = A ./ B, S = inv(H)',&
'',&
'            short e',&
'            d = 1:8,  e = ones(d),  A = abs(d''*e - e''*d)',&
'            X = inv(A)',&
'            rat(X)',&
'            display(ans)',&
'',&
'real  "real(X)" is the real part of X.',&
'',&
'rref  "rref(A)" is the reduced row echelon form of the rectangular',&
'      matrix. rref(A,B) is the same as rref(<A,B>) .',&
'',&
'roots  Find polynomial roots. "roots(C)" computes the roots of the',&
'       polynomial whose coefficients are the elements of the vector C.',&
'       If C has N+1 components, the polynomial is',&
'',&
'          C(1)*X**N + ... + C(N)*X + C(N+1)',&
'',&
'       See "poly".',&
'',&
'round  "round(X)" rounds the elements of X to the nearest integers.',&
'',&
'schur  Schur decomposition. "<U,T> = schur(X)" produces an upper',&
'       triangular matrix T , with the eigenvalues of X on the',&
'       diagonal, and a unitary matrix U so that X = U*T*U'' and',&
'       U''*U = eye . By itself, "schur(X)" returns T .',&
'',&
'shape  If X is an M by N matrix, then shape(X) is <M, N> .',&
'       Can also be used with a multiple assignment,',&
'            <M, N> = shape(X) .',&
'',&
'sum   "sum(X)" is the sum of all the elements of X.',&
'      "sum(diag(X))" is the trace of X.',&
'',&
'svd   Singular value decomposition. "<U,S,V> = svd(X)" produces a',&
'      diagonal matrix S , of the same dimension as X and with',&
'      nonnegative diagonal elements in decreasing order, and',&
'      unitary matrices U and V so that X = U*S*V'' .',&
'',&
'      By itself, "svd(X)" returns a vector containing the singular',&
'      values.',&
'',&
'      "<U,S,V> = svd(X,0)" produces the "economy size"',&
'      decomposition. If X is m by n with m > n, then only the',&
'      first n columns of U are computed and S is n by n .',&
'',&
'tril  Lower triangle. "tril(X)" is the lower triangular part of X.',&
'      "tril(X,K)" is the elements on and below the K-th diagonal of',&
'      X. K = 0 is the main diagonal, K > 0 is above the main',&
'      diagonal and K < 0 is below the main diagonal.',&
'',&
'triu  Upper triangle. "triu(X)" is the upper triangular part of X.',&
'      "triu(X,K)" is the elements on and above the K-th diagonal of X. K',&
'      = 0 is the main diagonal, K > 0 is above the main diagonal and K <',&
'      0 is below the main diagonal.',&
'',&
'user  Allows personal Fortran subroutines to be linked into',&
'      LALA. The subroutine should have the heading',&
'',&
'         SUBROUTINE USER(A,M,N,S,T)',&
'         REAL or DOUBLEPRECISION A(M,N),S,T',&
'',&
'      The LALA statement "Y = user(X,s,t)" results in a call to the',&
'      subroutine with a copy of the matrix X stored in the argument A,',&
'      its column and row dimensions in M and N, and the scalar parameters',&
'      s and t stored in S and T. If s and t are omitted, they are set',&
'      to 0.0. After the return, A is stored in Y. The dimensions M and',&
'      N may be reset within the subroutine. The statement Y = "user(K)"',&
'      results in a call with M = 1, N = 1 and A(1,1) = "float(K)". After',&
'      the subroutine has been written, it must be compiled and linked',&
'      to the LALA object code within the local operating system.',&
'',&
'zeros',&
'      Returns a matrix of all zeros.',&
'',&
'         zeros(N)    returns an N by N matrix of zeroes.',&
'         zeros(M,N)  returns an M by N matrix of zeroes.',&
'         zeros(X)    returns a matrix of zeroes of the same order as X.',&
'================================================================================',&
'FLOW CONTROL',&
'',&
'else  Used with "if".',&
'',&
'end   Terminates the scope of "for", "while" and "if" statements.',&
'      Without "end"s, "for" and "while" repeat all statements up to',&
'      the end of the line. Each "end" is paired with the closest',&
'      previous unpaired "for" or "while" and serves to terminate its',&
'      scope. The line',&
'',&
'         for I=1:N, for J=1:N, A(I,J)=1/(I+J-1); A',&
'',&
'      would cause A to be printed N**2 times, once for each new',&
'      element. On the other hand, the line',&
'',&
'         for I=1:N, for J=1:N, A(I,J)=1/(I+J-1); end, end, A',&
'',&
'      will lead to only the final printing of A.',&
'      Similar considerations apply to "while".',&
'',&
'      See "exit" (terminates execution of loops or of LALA itself).',&
'',&
'if    Conditionally execute statements',&
'',&
'      SIMPLE FORM',&
'       Enter',&
'',&
'         if expression rop expression, statements',&
'',&
'      where rop is =, <, >, <=, >=, or <> (not equal). The',&
'      statements are executed once if the indicated comparison',&
'      between the real parts of the first components of the two',&
'      expressions is true, otherwise the statements are skipped.',&
'',&
'      EXAMPLE',&
'        Enter',&
'',&
'         if abs(i-j) = 1, a(i,j) = -1;',&
'',&
'      More complicated forms use "end" in the same way it is used with',&
'      "for" and "while" and use "else" as an abbreviation for "end",',&
'',&
'         if expression not rop expression',&
'',&
'      EXAMPLE',&
'        Enter',&
'',&
'         for i = 1:n, for j = 1:n, ...',&
'            if i = j, a(i,j) = 2; else if abs(i-j) = 1, a(i,j) = -1; ...',&
'            else a(i,j) = 0;',&
'',&
'      An easier way to accomplish the same thing is',&
'',&
'         a = 2*eye(n);',&
'         for i = 1:n-1, a(i,i+1) = -1; a(i+1,i) = -1;',&
'',&
'for   Repeat statements a specific number of times.',&
'',&
'         for variable = expr, statement, ..., statement, end',&
'',&
'      The "end" at the end of a line may be omitted. The comma before the',&
'      "end" may also be omitted. The columns of the expression are stored',&
'      one at a time in the variable and then the following statements,',&
'      up to the "end", are executed.  The expression is often of the form',&
'      X:Y, in which case its columns are simply scalars. Some examples',&
'      (assume N has already been assigned a value).',&
'',&
'       for I = 1:N, for J = 1:N, A(I,J) = 1/(I+J-1);',&
'       for J = 2:N-1, A(J,J) = J; end; A',&
'       for S = 1.0: -0.1: 0.0, ... steps S with increments of -0.1 .',&
'       for E = eye(N), ... sets E to the unit N-vectors.',&
'       for V = A, ... has the same effect as',&
'       for J = 1:N, V = A(:,J); ... except J is also set here.',&
'',&
'while  Repeat statements an indefinite number of times.',&
'',&
'          while expr rop expr, statement, ..., statement, end',&
'',&
'       where rop is =, <, >, <=, >=, or <> (not equal). The "end"',&
'       at the end of a line may be omitted. The comma before the',&
'       "end" may also be omitted. The commas may be replaced by',&
'       semicolons to avoid printing. The statements are',&
'       repeatedly executed as long as the indicated comparison',&
'       between the real parts of the first components of the two',&
'       expressions is true.',&
'',&
'       EXAMPLE',&
'       (assume a matrix A is already defined).',&
'',&
'        E = 0*A; F = E + eye; N = 1;',&
'        while norm(E+F-E,1) > 0, E = E + F; F = A*F/N; N = N + 1;',&
'        E',&
'',&
'exit  Causes termination of a "for" or "while" loop.',&
'      If not in a loop, terminates execution of LALA.',&
'      Also see "quit".',&
'',&
'quit  From the terminal, causes return to the operating system',&
'      or other program which invoked LALA. From inside an',&
'      "exec", causes return to the invoking "exec", or to the',&
'      terminal.',&
'================================================================================',&
'FILE ACCESS',&
'',&
'      The "exec", "save", "delete", "load", "diary", and "print"',&
'      functions access files.  The ''file'' parameter takes different',&
'      forms for different operating systems. On most systems, ''file''',&
'      may be a string of up to 1024 characters in quotes. For example,',&
'      "save(''A'')" or "exec(''LALA/demo.exec'')" . The string will be used',&
'      as the name of a file in the local operating system.',&
'',&
'      Check your local installation for details.  The filename must be',&
'      composed of recognized characters. See "char".',&
'',&
'      Also see "quit" and "exit".',&
'',&
'delete  "delete(''filename'')" deletes the given file.',&
'',&
'exec  "exec(''file'',k)" obtains subsequent LALA input from an',&
'      external file. The printing of input is controlled by the',&
'      optional parameter k .',&
'',&
'      Files are searched for by the given name. If not found, it is searched',&
'      for in the colon-separated directory names in the environment variable',&
'      LALA_PATH. It is looked for first literally by the given name, and then',&
'      by the name suffixed with ".la".',&
'',&
'      "include" is an alias for "exec".',&
'',&
'         If k = 0 , there is no echo, prompt or pause. This is the',&
'                    default if the exec command is followed by a semicolon.',&
'         If k = 1 , the input is echoed.',&
'         If k = 2 , the LALA prompt <> is printed.',&
'         If k = 3 , there will be echos and prompts, but no pauses.',&
'                    This is the the default if the exec command is not',&
'                    followed by a semicolon.',&
'         If k = 4 , LALA pauses before each prompt and waits for a',&
'                    null line to continue.',&
'         If k = 7 , there will be echos, prompts and pauses. This is',&
'                    useful for demonstrations on video terminals.',&
'',&
'      "exec"''s may be nested, i.e. the text in the file may contain',&
'      "exec" of another file.',&
'',&
'      "exec" may not be recursive, as Fortran (currently) does not allow',&
'      for multiple opens of a file.',&
'',&
'      "exec"s may also be driven by "for" and "while" loops.',&
'',&
'include  "include" is an alias for "exec".',&
'',&
'load  "load(''file'')" retrieves all the variables from the file .',&
'      See FILE and "save" for more details. To prepare your own',&
'      file for "load"ing, change the "read" to "write" in the code',&
'      given under "save".',&
'',&
'print  "print(''file'',X)" prints X on the file using the current',&
'       format determined by "short", "long z", etc. See FILE.',&
'',&
'doc   does nothing at the moment',&
'',&
'save  "save(''file'')" stores all the current variables in a file.',&
'      "save(''file'',X)" saves only X . See FILE .',&
'',&
'      The variables may be retrieved later by "load(''file'')" or by your',&
'      own program using the following code for each matrix.  The lines',&
'      involving "ximag" may be eliminated if everything is known to',&
'      be real.',&
'',&
'        > ! attach LUN to ''file'', then ...',&
'        > doubleprecision :: xreal(mmax,nmax)',&
'        > doubleprecision :: ximag(mmax,nmax)',&
'        > character(len=32) :: id',&
'        > read(LUN,''(a32,3i9)'') id,m,n,img',&
'        > do j = 1, n',&
'        >    read(LUN,''(4z18)'') (xreal(i,j), i=1,m)',&
'        >    if (img .ne. 0) read(LUN,102) (ximag(i,j),i=1,m)',&
'        > enddo',&
'        > ! The formats used are system dependent. These are typical.',&
'        > ! See SUBROUTINE mat_savlod(3f) in your local implementation',&
'        > ! of LALA.',&
'',&
'================================================================================',&
'OUTPUT OPTIONS',&
'      ( Also see "FILE" ("exec", "load", "print", "save" ))',&
'',&
'lines  An internal count is kept of the number of lines of output',&
'       since the last input. Whenever this count approaches a',&
'       limit, the user is asked whether or not to suppress',&
'       printing until the next input. Initially the limit is 21.',&
'       "lines(N)" resets the limit to N .',&
'',&
'long   See "short" also.',&
'',&
'       Determine output format. All computations are done in',&
'       complex arithmetic and double precision if it is available.',&
'       "short" and "long" merely switch between different output',&
'       formats.',&
'',&
'        long     // Scaled fixed point format with about 15 digits.',&
'        long e   // Floating point format with about 15 digits.',&
'        long z   // System dependent format, often hexadecimal.',&
'',&
'short  See "long" also.',&
'       Determine output format. All computations are done in',&
'       complex arithmetic and double precision if it is available.',&
'       "short" and "long" merely switch between different output',&
'       formats.',&
'',&
'        short    // Scaled fixed point format with about 5 digits.',&
'        short e  // Floating point format with about 5 digits.',&
'',&
'diary  "diary(''file'')" causes a copy of all subsequent terminal input and',&
'       most of the resulting output to be written on the file. "diary(0)"',&
'       turns it off. See "FILE".',&
'',&
'display  "display(X)" prints X in a compact format.',&
'',&
'      If base >= 2 is specified the values are printed as numeric',&
'      values in the specified base.',&
'',&
'           display(0:10,10 ) // display values in base 10',&
'           display(0:10,16 ) // display values as hexadecimal values',&
'           display(0:10,2 )  // display values as binary numbers',&
'',&
'      If no base is specified and all the elements of X are integers',&
'      between 0 and 255, then X is interpreted as LALA text and printed',&
'      accordingly.',&
'',&
'         <>display(''the analysis is complete'')',&
'           the analysis is complete',&
'         display(32:126) // print the printable default LALA characters',&
'',&
'      Otherwise or if the base is one, + , - and blank are printed for',&
'      positive, negative and zero elements.',&
'',&
'         display(rand(24,80)-rand(24,80))',&
'',&
'      Imaginary parts are ignored.',&
'',&
'      Note that "display(X,B)" is the same as "display(base(X,B))" except',&
'      for base 1 except it forces "display" to display numeric values.',&
'',&
'      "display" has an alias of "disp".',&
'',&
'plot  "plot(X,Y)" produces a plot of the elements of Y against',&
'      those of X. plot(Y) is the same as plot(1:n,Y) where n is the number',&
'      of elements in Y. plot(X,Y,P) or "plot(X,Y,p1,...,pk)" passes the',&
'      optional parameter vector P or scalars p1 through pk to the plot',&
'      routine. The default plot routine is a crude printer-plot. This',&
'      version writes the data as a simple X Y table into a scratch file',&
'      called "scratch.dat" and then calls the command',&
'',&
'        xy scratch.dat [options]',&
'',&
'      Hopefully, you have the command xy(1) in your search path.',&
'      If not, you can make one by creating a script that calls',&
'      a plotting utility.',&
'',&
'         t = 0:50;',&
'         plot( t.*cos(t), t.*sin(t) )',&
'         opts='' -m -1 -title test plot -d pdf''',&
'         plot( t.*cos(t), t.*sin(t),opts)',&
'================================================================================',&
'ENVIRONMENT',&
'',&
'getenv   get environment variable or return a space',&
'',&
'            // read commands from a file if an environment variable is set.',&
'            MATRC=getenv(''MATRC'');',&
'            if MATRC <> '' '', exec(''MATRC'');',&
'================================================================================',&
'PERFORMANCE INFORMATION',&
'',&
'flops  Count of floating point operations.',&
'',&
'       "flops" is a permanently defined row vector with two elements.',&
'       "flops(1)" is the cpu time consumed by the the previous',&
'       statement. "flops(2)" is a cumulative total. "flops" can be used',&
'       in the same way as any other vector. "flops(2) = 0" resets the',&
'       cumulative total. In addition, "flops(1)" will be printed whenever',&
'       a statement is terminated by an extra comma. For example,',&
'',&
'         X = inv(A);,',&
'',&
'       or',&
'',&
'         cond(A), (as the last statement on the line).',&
'================================================================================',&
'MISCELLANEOUS',&
'',&
'CHAR  special issues regarding strings',&
'',&
'   LALA has a limited facility for handling text. Any string of',&
'   characters delineated by quotes (with two quotes used to allow one',&
'   quote within the string) is saved as a vector of integer values that',&
'   are the ADE (Ascii Decimal Equivalent) value of the character.',&
'',&
'   In commands { and } are equivalent to ( and )',&
'',&
'   When defining an array [ and ] or < and > may be used as the delimiters.',&
'',&
'   lala(3f)  is too flexible about that and lets them be interchanged freely',&
'   instead of being matched but that will probably change to be more strictly',&
'   enforced.',&
'',&
'   Currently " is not a special character but will probably be allowed as a',&
'   string quoting character in the future.',&
'',&
'   For example',&
'',&
'      ''2*A + 3''        //  is the same as  < 50 42 65 32 43 32 51 >.',&
'      display(32:126)  //  display the basic visible ASCII characters',&
'',&
'',&
'   So if you wanted to home the cursor and clear the screen on an',&
'   ANSI-compatible terminal and entered',&
'',&
'       display(<27,''[H'',27,''[2J''>)',&
'',&
'   The terminal screen would clear. More usefully, if you define the',&
'   string',&
'',&
'       clr=''display([27,91,''''H'''',27,91,''''2J''''])''',&
'',&
'   Then entering',&
'',&
'       >clr',&
'',&
'   will clear the screen on ANSI terminals and emulators.',&
'',&
'DECIMAL ADE TABLE',&
'      The ASCII Decimal Equivalents',&
'      *-------*-------*-------*-------*-------*-------*-------*-------*',&
'      | 00 nul| 01 soh| 02 stx| 03 etx| 04 eot| 05 enq| 06 ack| 07 bel|',&
'      | 08 bs | 09 ht | 10 nl | 11 vt | 12 np | 13 cr | 14 so | 15 si |',&
'      | 16 dle| 17 dc1| 18 dc2| 19 dc3| 20 dc4| 21 nak| 22 syn| 23 etb|',&
'      | 24 can| 25 em | 26 sub| 27 esc| 28 fs | 29 gs | 30 rs | 31 us |',&
'      | 32 sp | 33  ! | 34  " | 35  # | 36  $ | 37  % | 38  & | 39  '' |',&
'      | 40  ( | 41  ) | 42  * | 43  + | 44  , | 45  - | 46  . | 47  / |',&
'      | 48  0 | 49  1 | 50  2 | 51  3 | 52  4 | 53  5 | 54  6 | 55  7 |',&
'      | 56  8 | 57  9 | 58  : | 59  ; | 60  < | 61  = | 62  > | 63  ? |',&
'      | 64  @ | 65  A | 66  B | 67  C | 68  D | 69  E | 70  F | 71  G |',&
'      | 72  H | 73  I | 74  J | 75  K | 76  L | 77  M | 78  N | 79  O |',&
'      | 80  P | 81  Q | 82  R | 83  S | 84  T | 85  U | 86  V | 87  W |',&
'      | 88  X | 89  Y | 90  Z | 91  [ | 92  \ | 93  ] | 94  ^ | 95  _ |',&
'      | 96  ` | 97  a | 98  b | 99  c |100  d |101  e |102  f |103  g |',&
'      |104  h |105  i |106  j |107  k |108  l |109  m |110  n |111  o |',&
'      |112  p |113  q |114  r |115  s |116  t |117  u |118  v |119  w |',&
'      |120  x |121  y |122  z |123  { |124  | |125  } |126  ~ |127 del|',&
'      *-------*-------*-------*-------*-------*-------*-------*-------*',&
'',&
'??    Two exclamation marks beginning a line enters command history mode.',&
'      The rest of the line is treated as an optional initial history',&
'      edit command. Enter "???" to enter history mode and then display',&
'      additional instructions.',&
'      see "EDIT" for further details.',&
'',&
'EDIT',&
'      A command line consisting of two question marks("??") will cause a',&
'      small line-based editor to be called (very similar to the CDC NOS',&
'      editor "xedit") with a copy of the previous input lines. When the',&
'      editor returns control to LALA, it will execute the edited command',&
'      (by default).',&
'',&
'      In editor mode the command to be edited is shifted over one and the',&
'      first character of input determines the edit mode. The letter "c"',&
'      begins a string change (ie. "c/oldstring/newstring/").  The "l"',&
'      command lists the lines. A number goes to that command line as',&
'      listed by the "l" command. If the change command begins with a',&
'      space a letter replaces the one above it with the exception of',&
'      the special characters # (delete) & (blank out) and ^ (insert the',&
'      following string here).',&
'',&
'      An editing loop is entered until a carriage return on an empty',&
'      line is entered to accept the new line or a period is entered to',&
'      cancel the editing.',&
'',&
'      For example, if you had entered a line such as:',&
'',&
'         <M,N>=shape(A);for I = 1:M, for J = 1:N, A(I,J) = A(I,J)+3.6;',&
'',&
'      Then to repeat the command changing "3.6" to "5.1" enter',&
'',&
'         ??',&
'      the previous command is then displayed. Now enter',&
'',&
'         c/3.6/5.1',&
'',&
'      and then enter a carriage return and the edited line will be',&
'      executed.',&
'',&
'      The first command can appear on the same line if the line starts',&
'      with "?? " (two question marks followed by a space). For example',&
'',&
'         ?? /rand',&
'',&
'      would take you into edit mode on the last command containing the',&
'      string "rand"',&
'',&
'      Enter "?" in edit mode to display further help on editor mode.',&
'',&
'eps   Floating point relative accuracy. A permanent variable',&
'      whose value is initially the distance from 1.0 to the next largest',&
'      floating point number. The value is changed by "chop", and other',&
'      values may be assigned. "eps" is used as a default tolerance by "pinv"',&
'      and "rank".',&
'',&
'lala  A placeholder for a new command.',&
'',&
'debug  "debu(1)" turns on verbose low-level debugging for the developer,',&
'       "debu(0)" turns it back off.',&
'',&
'================================================================================',&
'']
end subroutine mat_help_text
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function find_exec_file(filename) result(returned)
! look for file. If not found look for file.la. If not found, repeat using directories in MATRIX_PATH=DIR1:DIR2:DIR3...
character(len=:),allocatable,intent(in) :: filename
character(len=:),allocatable            :: returned
   if(exists(filename))then
      returned=filename
   elseif(exists(trim(filename)//'.la'))then
      returned=filename//'.la'
   else
      returned=lookfor(filename,'LALA_PATH')
      if(returned.eq.'')then
         returned=lookfor(filename//'.la','LALA_PATH')
      endif
      if(returned.eq.'')returned=filename
   endif
end function find_exec_file
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
logical function exists(filename) result(r)
character(len=*), intent(in) :: filename
    inquire(file=filename, exist=r)
end function
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine printit()
integer :: i
integer :: m,n
integer :: l
if(allocated(G_PSEUDO_FILE)) write(*,*)'G_PSEUDO_FILE:SIZE:',size(G_PSEUDO_FILE)
write(*,gen1)'G_PROMPT:',G_PROMPT,':G_ECHO:',G_ECHO
write(*,gen1)'G_LIN:',trim(ade2str(G_LIN))
write(*,gen1)'G_LINE_POINTER:',G_LINE_POINTER
!                                              ! [1] first character to process in current line
!                                              ! [2] last character to process in current line
!                                              ! [3]
!                                              ! [4] pointer into current character in current line being processed
!                                              ! [5]
!                                              ! [6]
write(*,gen1)'G_LHS:',G_LHS,':G_RHS:',G_RHS
write(*,gen1)'G_FIN:',G_FIN,':G_FUN:',G_FUN,':G_FMT:',G_FMT
write(*,gen1)'G_RIO:',G_RIO,':G_INPUT_LUN:',G_INPUT_LUN
write(*,gen1)'G_PTZ:',G_PTZ,':G_SYM:',G_SYM,':G_SYN:',trim(ade2str(G_SYN))
write(*,gen1)'G_CURRENT_RANDOM_SEED:',G_CURRENT_RANDOM_SEED,':G_CURRENT_RANDOM_TYPE:',G_CURRENT_RANDOM_TYPE
write(*,gen1)'G_FLOP_COUNTER:',G_FLOP_COUNTER
write(*,gen1)'G_DEBUG_LEVEL:',G_DEBUG_LEVEL
write(*,gen1)'G_FILE_OPEN_ERROR:',G_FILE_OPEN_ERROR,':G_ERR:',G_ERR
write(*,gen1)'G_LINECOUNT:',G_LINECOUNT
!                                    ! [1] lines displayed since count started
!                                    ! [2] line limit before warning (ie. page length+1)
!                                    ! [3] 0 or 1 for "semi" mode to be on or off
!                                    ! [4] flag from "exec" command, and ...
write(*,gen1)'G_BUF:',trim(ade2str(G_BUF))
write(*,gen1)'GM_BIGMEM:',GM_BIGMEM
write(*,gen1)'G_TOP_OF_SAVED:',G_TOP_OF_SAVED,':G_ARGUMENT_POINTER:',G_ARGUMENT_POINTER
do i=1,GG_MAX_NUMBER_OF_NAMES
   m=G_VAR_ROWS(i)
   n=G_VAR_COLS(i)
   l=G_VAR_DATALOC(i)
   if(.not.(ade2str(G_VAR_IDS(:,i)).eq.''.and.l.eq.0.and.m.eq.0.and.n.eq.0))then
      write(*,*)i,ade2str(G_VAR_IDS(:,i)),l,m,n,'VALS=',real(GM_REALS(l:l+m*n-1))
   endif
enddo
!==================================================================================================================================!
! PARSING
!integer,parameter        :: G_PSIZE=32                        ! stack size for pseudo-recursion
!integer                  :: G_IDS(GG_MAX_NAME_LENGTH,G_PSIZE)
!integer                  :: G_PSTK(G_PSIZE)
!integer                  :: G_RSTK(G_PSIZE)
!integer                  :: G_PT
!
!integer                  :: G_CHRA ! current character in line
!==================================================================================================================================!
!doubleprecision,allocatable    :: GM_REALS(:), GM_IMAGS(:)               ! set to size of GM_BIGMEM
!==================================================================================================================================!
end subroutine printit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine mat_wdiv(ar,ai,br,bi,cr,ci)

! ident_17="@(#)M_LA::mat_wdiv(3fp): c = a/b"

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
   if (s .eq. 0.0d0) then
      call mat_err(27)
      return
   endif
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
subroutine mat_wlog(in_real,in_imag,out_real,out_imag)

! ident_22="@(#)M_LA::mat_wlog(3fp): y = log(x)"

doubleprecision :: in_real, in_imag
doubleprecision :: out_real, out_imag
doubleprecision :: t
doubleprecision :: r
   r = mat_pythag(in_real,in_imag)

   if (r .eq. 0.0d0) then
      call mat_err(32) !  Singularity of LOG or ATAN
   else
      t = datan2(in_imag,in_real)
      if (in_imag.eq.0.0d0 .and. in_real.lt.0.0d0) t = dabs(t)
      out_real = dlog(r)
      out_imag = t
   endif

end subroutine mat_wlog
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine mat_watan(xr,xi,yr,yi)

! ident_37="@(#)M_LA::mat_watan(3fp): y = atan(x) = (i/2)*log((i+x)/(i-x))"

doubleprecision,intent(in)  :: xr, xi
doubleprecision,intent(out) :: yr, yi
doubleprecision             :: tr, ti

   if (xi .eq. 0.0d0) then
      yr = datan2(xr,1.0d0)
      yi = 0.0d0
   elseif (xr.ne.0.0d0 .or. dabs(xi).ne.1.0d0) then
      call mat_wdiv(xr,1.0d0+xi,-xr,1.0d0-xi,tr,ti)
      call mat_wlog(tr,ti,tr,ti)
      yr = -(ti/2.0d0)
      yi = tr/2.0d0
   else
      call mat_err(32) ! Singularity of LOG or ATAN
   endif

end subroutine mat_watan
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_matrix
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
